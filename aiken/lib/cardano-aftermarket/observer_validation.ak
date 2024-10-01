use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.
  { Output
  , Input
  , ScriptPurpose
  , Redeemer
  , Spend
  , Mint
  , ValidityRange
  , WithdrawFrom
  } as tx
use aiken/transaction/credential.{Address, Credential, ScriptCredential, StakeCredential, Inline}
use aiken/transaction/value.{PolicyId, AssetName}

use cardano_aftermarket/data/core.{KeyHash, ScriptHash}
use cardano_aftermarket/data/datums
use cardano_aftermarket/observer_validation/claim_bid_acceptance_validation.
  {RequiredAcceptanceOutput} as accept_bid
use cardano_aftermarket/observer_validation/claim_validation.
  {RequiredClaimOutput} as claim
use cardano_aftermarket/observer_validation/spot_bid_acceptance_validation.
  {RequiredSpotAuctionOutput} as spot_bid
use cardano_aftermarket/observer_validation/spot_purchase_validation.
  {RequiredSpotOutput} as spot_purchase
use cardano_aftermarket/observer_validation/unlock_validation as unlock
use cardano_aftermarket/data/redeemers.{MarketRedeemer,BeaconsRedeemer}
use cardano_aftermarket/utils.{error_if_false}

// Check all inputs that should compose.
pub fn valid_payment(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_burned: Dict<AssetName,Int>,
  beacon_id: PolicyId,
  aftermarket_script_credential: Credential,
  validity_range: ValidityRange,
  proxy_hash: ScriptHash,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>
) -> Bool {
  let (
    req_spot_outputs,
    req_auction_outputs,
    req_acceptance_outputs,
    req_claim_outputs,
    req_beacon_burn,
    beacon_redeemer
  ) = get_input_info(
    beacon_id,
    aftermarket_script_credential,
    validity_range,
    proxy_hash,
    withdrawals,
    extra_signatories,
    inputs,
    dict.to_list(redeemers),
    ([],[],[],[],dict.new(),None)
  )

  // Only check the proper beacons are burned if CreateCloseOrUpdateMarketUTxOs is not used in this
  // transaction. Accepting Claim bids does not involve burning so the beacons script should not
  // need to be executed in this case.
  expect True = dict.is_empty(req_beacon_burn) || when beacon_redeemer is {
    // The beacon script will check the proper beacons are burned.
    Some(redeemers.CreateCloseOrUpdateMarketUTxOs) -> True

    // This script must check the beacons were properly burned.
    Some(redeemers.BurnBeacons) -> error_if_false(
      @"The wrong beacons were burned",
      beacons_burned == req_beacon_burn
    )

    _ -> error @"Beacon script not executed with proper redeemer"
  }

  // This either returns True or fails with an error.
  check_outputs(
    aftermarket_script_credential,
    req_spot_outputs, 
    req_auction_outputs, 
    req_acceptance_outputs, 
    req_claim_outputs, 
    outputs
  )
}

// Compare the inputs to the redeemer map to get all inputs that need to be validated. It returns
// the required information for checking the outputs as well as mint/burn.
fn get_input_info(
  beacon_policy_id: PolicyId,
  aftermarket_script_credential: Credential,
  validity_range: ValidityRange,
  proxy_hash: ScriptHash,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (
    List<RequiredSpotOutput>,
    List<RequiredSpotAuctionOutput>,
    List<RequiredAcceptanceOutput>,
    List<RequiredClaimOutput>,
    Dict<AssetName,Int>, // Actual beacons burned.
    Option<BeaconsRedeemer>
  )
) -> (
  List<RequiredSpotOutput>,
  List<RequiredSpotAuctionOutput>,
  List<RequiredAcceptanceOutput>,
  List<RequiredClaimOutput>,
  Dict<AssetName,Int>, // Actual beacons burned.
  Option<BeaconsRedeemer>
) {
  let (
    req_spot_outputs,
    req_auction_outputs,
    req_acceptance_outputs,
    req_claim_outputs,
    req_beacon_burn,
    beacon_redeemer
  ) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as seller_address,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is a market's script input, do all of the checks.
      if pay_cred == aftermarket_script_credential {
        // This is probably not necessary for now but there are CIPs to enable specifying different
        // orderings. This is here mostly as a reminder to consider it in the future.
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: MarketRedeemer = used_redeemer
        when used_redeemer_ is {
          redeemers.PurchaseSpot -> {
            let (new_spot_output, new_burn) = spot_purchase.process_spot_purchase_input(
              in_ref,
              datum,
              val,
              req_beacon_burn
            )

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              validity_range,
              proxy_hash,
              withdrawals,
              extra_signatories,
              rest_ins,
              rest_reds,
              ( list.concat(req_spot_outputs,[new_spot_output])
              , req_auction_outputs
              , req_acceptance_outputs
              , req_claim_outputs
              , new_burn
              , beacon_redeemer
              )
            )
          }

          redeemers.AcceptSpotBid -> {
            let (maybe_new_auction_output,new_burn) = spot_bid.process_spot_bid_acceptance_input(
              in_ref,
              datum,
              val,
              req_beacon_burn
            )

            let new_auction_outputs = when maybe_new_auction_output is {
              None -> req_auction_outputs
              Some(new_auction_output) -> list.concat(req_auction_outputs,[new_auction_output])
            }

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              validity_range,
              proxy_hash,
              withdrawals,
              extra_signatories,
              rest_ins,
              rest_reds,
              ( req_spot_outputs
              , new_auction_outputs
              , req_acceptance_outputs
              , req_claim_outputs
              , new_burn
              , beacon_redeemer
              )
            )
          }

          redeemers.AcceptClaimBid{seller_deposit,payment_address} -> {
            let (maybe_new_accepted_output,new_burn) = accept_bid.process_claim_bid_acceptance_input(
              seller_deposit,
              payment_address,
              seller_address,
              datum,
              val,
              utils.get_upper_bound(validity_range),
              proxy_hash,
              req_beacon_burn
            )

            let new_acceptance_outputs = when maybe_new_accepted_output is {
              None -> req_acceptance_outputs
              Some(new_accepted_output) -> list.concat(req_acceptance_outputs,[new_accepted_output])
            }

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              validity_range,
              proxy_hash,
              withdrawals,
              extra_signatories,
              rest_ins,
              rest_reds,
              ( req_spot_outputs
              , req_auction_outputs
              , new_acceptance_outputs
              , req_claim_outputs
              , new_burn
              , beacon_redeemer
              )
            )
          }

          redeemers.ClaimAcceptedBid -> {
            let (new_claim_output, new_burn) = claim.process_claim_input(
              in_ref,
              withdrawals,
              extra_signatories,
              datum,
              val,
              req_beacon_burn
            )

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              validity_range,
              proxy_hash,
              withdrawals,
              extra_signatories,
              rest_ins,
              rest_reds,
              ( req_spot_outputs
              , req_auction_outputs
              , req_acceptance_outputs
              , list.concat(req_claim_outputs,[new_claim_output])
              , new_burn
              , beacon_redeemer
              )
            )
          }

          redeemers.UnlockUnclaimedAcceptedBid -> {
            let new_burn = unlock.process_unlock_input(
              datum,
              val,
              utils.get_lower_bound(validity_range),
              req_beacon_burn
            )

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              validity_range,
              proxy_hash,
              withdrawals,
              extra_signatories,
              rest_ins,
              rest_reds,
              ( req_spot_outputs
              , req_auction_outputs
              , req_acceptance_outputs
              , req_claim_outputs
              , new_burn
              , beacon_redeemer
              )
            )
          }

          // If the input is using a different market redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ -> get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
            validity_range,
            proxy_hash,
            withdrawals,
            extra_signatories,
            rest_ins,
            rest_reds,
            result
          )
        }

      // If it is not a market script input, check if it is a pubkey input. Pubkey inputs don't
      // use redeemers so they need to be handled differently.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_input_info(
          beacon_policy_id,
          aftermarket_script_credential,
          validity_range,
          proxy_hash,
          withdrawals,
          extra_signatories,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          result
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not an options' input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          beacon_policy_id,
          aftermarket_script_credential,
          validity_range,
          proxy_hash,
          withdrawals,
          extra_signatories,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          result
        )
      }
    }

    // If the redeemer is not a Spend purpose, check what redeemer was used for the
    // beacon_script.
    (_ , [(purpose,used_redeemer), ..rest_reds]) -> when beacon_redeemer is {
      // The beacon_script redeemer was already found so skip this redeemer.
      Some(_) -> get_input_info(
        beacon_policy_id,
        aftermarket_script_credential,
        validity_range,
        proxy_hash,
        withdrawals,
        extra_signatories,
        inputs,
        rest_reds,
        result
      )

      // The beacon_script redeemer still needs to be found.
      None -> {
        // These helper variables are because the actual code is long.
        let stake_cred = Inline(ScriptCredential(beacon_policy_id))

        // The beacon_script can be used as either a minting policy or a staking script.
        if purpose == Mint(beacon_policy_id) || purpose == WithdrawFrom(stake_cred) {
          expect used_redeemer_: BeaconsRedeemer = used_redeemer
          get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
            validity_range,
            proxy_hash,
            withdrawals,
            extra_signatories,
            inputs,
            rest_reds,
            ( req_spot_outputs
            , req_auction_outputs
            , req_acceptance_outputs
            , req_claim_outputs
            , req_beacon_burn
            , Some(used_redeemer_)
            )
          )
        } else {
          // This redeemer is not for the beacon_script so skip it.
          get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
            validity_range,
            proxy_hash,
            withdrawals,
            extra_signatories,
            inputs,
            rest_reds,
            result
          )
        }
      }
    }

    // There are no more redeemers to check.
    _ -> result
  }
}

// Go through the outputs and check that all required outputs are satisfied. The required outputs
// must be in the same order as the corresponding inputs.
fn check_outputs(
  aftermarket_script_credential: Credential,
  req_spot_outputs: List<RequiredSpotOutput>,
  req_auction_outputs: List<RequiredSpotAuctionOutput>,
  req_acceptance_outputs: List<RequiredAcceptanceOutput>,
  req_claim_outputs: List<RequiredClaimOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_spot_outputs, req_auction_outputs, req_acceptance_outputs, req_claim_outputs, outputs) is {
    ([], [], [], [], remaining_outputs) -> {
      // The rest of the outputs need to be checked to ensure there are no excess AcceptedBid
      // UTxOs in order to prevent the minting of improper BidderIds.
      list.all(
        remaining_outputs,
        fn(output) {
          let Output(Address(pay_cred,_),_,d,_) = output

          // This checks the address first to not try parsing unrelated datums. I am assuming
          // checking the address is cheaper than checking the datum.
          if pay_cred == aftermarket_script_credential {
            when utils.parse_datum(d) is {
              datums.AcceptedBidDatum{..} -> error @"Extra AcceptedBid UTxO found"
              _ -> True
            }
          } else {
            True
          }
        }
      )
    }

    ([_,..], _, _, _, []) -> error "Not all required spot payments found"

    (_, [_,..], _, _, []) -> error "Not all required spot auction payments found"

    (_, _, [_,..], _, []) -> error "Not all required accepted bid outputs found"

    (_, _, _, [_,..], []) -> error "Not all required claim payment outputs found"

    (_, _, _, _, [output, ..rest_outputs]) -> check_outputs(
      aftermarket_script_credential,
      spot_purchase.missing_spot_purchase_outputs(req_spot_outputs, output),
      spot_bid.missing_spot_bid_outputs(req_auction_outputs, output),
      accept_bid.missing_acceptance_outputs(aftermarket_script_credential, req_acceptance_outputs, output),
      claim.missing_claim_payment_outputs(req_claim_outputs, output),
      rest_outputs
    )
  }
}
