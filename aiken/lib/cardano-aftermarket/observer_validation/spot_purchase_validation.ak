use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/transaction.{Datum, Output, InlineDatum, OutputReference} as tx
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_aftermarket/data/core.{Asset}
use cardano_aftermarket/data/datums.{SpotDatum}
use cardano_aftermarket/utils.{error_if_false}

// The information needed to validate the spot payment output.
pub type RequiredSpotOutput {
  beacon_id: PolicyId,
  sale_price: List<(Asset,Int)>,
  sale_deposit: Int,
  sale_ref: OutputReference,
  req_address: Address
}

// Determine what outputs and burning are required based on the Spot UTxO being purchased.
pub fn process_spot_purchase_input(
  input_ref: OutputReference,
  old_datum: Datum, 
  old_value: Value,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (RequiredSpotOutput,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    SpotDatum{
      beacon_policy_id,
      payment_address,
      sale_deposit,
      sale_price
      ..
    } -> {
      // Spot UTxOs should have two beacons. Both of them must be burned. 
      let new_burn = when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2)] -> 
          acc_beacon_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Spot UTxO found"
      }

      let new_spot_output = RequiredSpotOutput {
        beacon_id: beacon_policy_id,
        sale_price: sale_price,
        sale_deposit: sale_deposit,
        req_address: payment_address,
        sale_ref: input_ref
      }

      (new_spot_output, new_burn)
    }
    _ -> error @"Input is not a spot UTxO"
  }
}

fn is_next_spot_payment_output(req_spot_output: RequiredSpotOutput, output: Output) -> Bool {
  let Output(addr,val,d,_) = output
  let RequiredSpotOutput{
    beacon_id,
    sale_price,
    sale_deposit,
    req_address,
    sale_ref
  } = req_spot_output

  if addr == req_address && d == InlineDatum((beacon_id,sale_ref)) {
    // This has its own error messages.
    check_spot_payment_output(
      sale_deposit,
      sale_price,
      value.flatten(val)
    )
  } else { False }
}

fn check_spot_payment_output(
  sale_deposit: Int,
  req_assets: List<(Asset,Int)>,
  actual_assets: List<(PolicyId,AssetName,Int)>
) -> Bool {
  when (req_assets,actual_assets) is {
    ([_next_req,..],[]) -> error @"Spot payment UTxO has wrong value"

    ([],_) -> True

    ([next_req,..rest_req],[next_actual,..rest_actual]) -> {
      let ((req_pol, req_name), req_val) = next_req
      let (actual_pol, actual_name, actual_val) = next_actual

      // Ada needs to be handled separately.
      if actual_pol == #"" {
        if req_pol == #"" {
          expect True = error_if_false(
            @"Spot payment UTxO has wrong value",
            // Can be greater in case minUTxOValue is larger than expected.
            actual_val >= req_val + sale_deposit
          )

          check_spot_payment_output(
            sale_deposit,
            rest_req,
            rest_actual
          )

        } else {
          expect True = error_if_false(
            @"Spot payment UTxO missing sale deposit",
            actual_val >= sale_deposit
          )

          check_spot_payment_output(
            sale_deposit,
            req_assets, // This asset still needs to be found.
            rest_actual
          )
        }
      } else {
        if actual_pol == req_pol && actual_name == req_name {
          expect True = error_if_false(
            @"Spot payment UTxO has wrong value",
            actual_val == req_val
          )

          check_spot_payment_output(
            sale_deposit,
            rest_req,
            rest_actual
          )
        } else {
          check_spot_payment_output(
            sale_deposit,
            req_assets, // This asset still needs to be found.
            rest_actual
          )
        }
      }
    }
  }
}

pub fn missing_spot_purchase_outputs(
  req: List<RequiredSpotOutput>, 
  output: Output
) -> List<RequiredSpotOutput> {
  when req is {
    [] -> []
    [x, ..rest] -> if is_next_spot_payment_output(x, output) { rest } else { req }
  }
}
