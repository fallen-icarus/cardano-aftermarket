use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/transaction.{Datum, Output, InlineDatum, OutputReference} as tx
use aiken/transaction/credential.{Address, StakeCredential, Inline}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_aftermarket/data/core.{Asset,KeyHash}
use cardano_aftermarket/data/datums.{AcceptedBidDatum}
use cardano_aftermarket/utils.{error_if_false}

// The information needed to validate the claim payment output.
pub type RequiredClaimOutput {
  beacon_id: PolicyId,
  bid: List<(Asset,Int)>,
  seller_deposit: Int,
  claim_ref: OutputReference,
  req_address: Address
}

// Determine what outputs and burning are required based on the AcceptedBid UTxO being claim.
pub fn process_claim_input(
  input_ref: OutputReference,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>,
  old_datum: Datum, 
  old_value: Value,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (RequiredClaimOutput,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    AcceptedBidDatum{
      beacon_policy_id,
      payment_address,
      seller_deposit,
      bidder_credential,
      bid
      ..
    } -> {
      expect True = error_if_false(
        @"Bidder credential did not approve", 
        utils.staking_credential_approves(
          Some(Inline(bidder_credential)),
          withdrawals, 
          extra_signatories
        ))

      // Bid UTxOs should have three beacons. All of them must be burned. 
      let new_burn = when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2),(tn3,n3)] -> 
          acc_beacon_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid AcceptedBid UTxO found"
      }

      let new_claim_output = RequiredClaimOutput {
        beacon_id: beacon_policy_id,
        bid: bid,
        seller_deposit: seller_deposit,
        req_address: payment_address,
        claim_ref: input_ref
      }

      (new_claim_output, new_burn)
    }
    _ -> error @"Input is not an AcceptedBid UTxO"
  }
}

fn is_next_claim_output(req_claim_output: RequiredClaimOutput, output: Output) -> Bool {
  let Output(addr,val,d,_) = output
  let RequiredClaimOutput{
    beacon_id,
    bid,
    seller_deposit,
    req_address,
    claim_ref
  } = req_claim_output

  if addr == req_address && d == InlineDatum((beacon_id,claim_ref)) {
    // This has its own error messages.
    check_claim_payment_output(
      seller_deposit,
      bid,
      value.flatten(val)
    )
  } else { False }
}

fn check_claim_payment_output(
  seller_deposit: Int,
  req_assets: List<(Asset,Int)>,
  actual_assets: List<(PolicyId,AssetName,Int)>
) -> Bool {
  when (req_assets,actual_assets) is {
    ([_next_req,..],[]) -> error @"Claim payment UTxO has wrong value"

    ([],_) -> True

    ([next_req,..rest_req],[next_actual,..rest_actual]) -> {
      let ((req_pol, req_name), req_val) = next_req
      let (actual_pol, actual_name, actual_val) = next_actual

      // Ada needs to be handled separately.
      if actual_pol == #"" {
        if req_pol == #"" {
          expect True = error_if_false(
            @"Claim payment UTxO has wrong value",
            // Can be greater in case minUTxOValue is larger than expected.
            actual_val >= req_val + seller_deposit
          )

          check_claim_payment_output(
            seller_deposit,
            rest_req,
            rest_actual
          )

        } else {
          expect True = error_if_false(
            @"Claim payment UTxO missing sale deposit",
            actual_val >= seller_deposit
          )

          check_claim_payment_output(
            seller_deposit,
            req_assets, // This asset still needs to be found.
            rest_actual
          )
        }
      } else {
        if actual_pol == req_pol && actual_name == req_name {
          expect True = error_if_false(
            @"Claim payment UTxO has wrong value",
            actual_val == req_val
          )

          check_claim_payment_output(
            seller_deposit,
            rest_req,
            rest_actual
          )
        } else {
          check_claim_payment_output(
            seller_deposit,
            req_assets, // This asset still needs to be found.
            rest_actual
          )
        }
      }
    }
  }
}

pub fn missing_claim_payment_outputs(
  req: List<RequiredClaimOutput>, 
  output: Output
) -> List<RequiredClaimOutput> {
  when req is {
    [] -> []
    [x, ..rest] -> if is_next_claim_output(x, output) { rest } else { req }
  }
}
