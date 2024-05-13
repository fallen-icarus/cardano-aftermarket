use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.
  { Datum
  , InlineDatum
  , Output
  , OutputReference
  , Input
  , ScriptPurpose
  , Redeemer
  , Spend
  , Mint
  , WithdrawFrom
  } as tx
use aiken/transaction/credential.{Address, Credential, ScriptCredential, Inline}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_aftermarket/data/core.
  { BeaconId
  , Asset
  }
use cardano_aftermarket/data/datums.{SpotDatum,AuctionDatum,BidDatum}
use cardano_aftermarket/data/redeemers.{MarketRedeemer,BeaconsRedeemer}
use cardano_aftermarket/utils.{error_if_false}

// The information needed to validate the spot payment output.
type RequiredSpotOutput {
  beacon_id: PolicyId,
  sale_price: List<(Asset,Int)>,
  sale_deposit: Int,
  sale_ref: OutputReference,
  req_address: Address
}

// The information needed to validate the auction payment output. The information comes from the
// Bid UTxO.
type RequiredAuctionOutput {
  beacon_id: PolicyId,
  nft_policy_id: PolicyId,
  nft_names: List<AssetName>,
  bid_deposit: Int,
  bid_ref: OutputReference,
  req_address: Address
}

// Check all inputs that should compose.
pub fn valid_payment(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_burned: Dict<AssetName,Int>,
  beacon_id: BeaconId,
  aftermarket_script_credential: Credential,
) -> Bool {
  let (
    req_spot_outputs,
    req_auction_outputs,
    req_beacon_burn,
    beacon_redeemer
  ) = get_input_info(
    beacon_id,
    aftermarket_script_credential,
    inputs,
    dict.to_list(redeemers),
    ([],[],dict.new(),None)
  )

  // Only check the proper beacons are burned if CreateCloseOrUpdateMarketUTxOs is not used in this
  // transaction.
  expect True = when beacon_redeemer is {
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
  check_outputs(req_spot_outputs, req_auction_outputs, outputs)
}

// Compare the inputs to the redeemer maps to get all inputs that need to be validated. It returns
// the required information for checking the outputs as well as mint/burn.
fn get_input_info(
  beacon_policy_id: BeaconId,
  aftermarket_script_credential: Credential,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (
    List<RequiredSpotOutput>,
    List<RequiredAuctionOutput>,
    Dict<AssetName,Int>, // Actual beacons burned.
    Option<BeaconsRedeemer>
  )
) -> (
  List<RequiredSpotOutput>,
  List<RequiredAuctionOutput>,
  Dict<AssetName,Int>, // Actual beacons burned.
  Option<BeaconsRedeemer>
) {
  let (
    req_spot_outputs,
    req_auction_outputs,
    req_beacon_burn,
    beacon_redeemer
  ) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_),val,datum,_)), ..rest_ins ]
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
            let (
              new_spot_output,
              new_burn,
            ) = process_spot_purchase_input(
              in_ref,
              datum,
              val,
              req_beacon_burn
            )

            get_input_info(
              beacon_policy_id,
              aftermarket_script_credential,
              rest_ins,
              rest_reds,
              ( list.concat(req_spot_outputs,[new_spot_output])
              , req_auction_outputs
              , new_burn
              , beacon_redeemer
              )
            )
          }

          redeemers.AcceptBid -> {
            let (maybe_new_auction_output,new_burn) = process_bid_acceptance_input(
              in_ref,
              datum,
              val,
              req_beacon_burn
            )

            when maybe_new_auction_output is {
              None -> {
                get_input_info(
                  beacon_policy_id,
                  aftermarket_script_credential,
                  rest_ins,
                  rest_reds,
                  ( req_spot_outputs
                  , req_auction_outputs
                  , new_burn
                  , beacon_redeemer
                  )
                )
              }
              Some(new_auction_output) -> {
                get_input_info(
                  beacon_policy_id,
                  aftermarket_script_credential,
                  rest_ins,
                  rest_reds,
                  ( req_spot_outputs
                  , list.concat(req_auction_outputs,[new_auction_output])
                  , new_burn
                  , beacon_redeemer
                  )
                )
              }
            }
          }

          // If the input is using a different market redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ -> get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
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
        inputs,
        rest_reds,
        result
      )

      // The beacon_script redeemer still needs to be found.
      None -> {
        // These helper variables are because the actual code is long.
        let beacon_id = core.from_beacon_id(beacon_policy_id)
        let stake_cred = Inline(ScriptCredential(beacon_id))

        // The beacon_script can be used as either a minting policy or a staking script.
        if purpose == Mint(beacon_id) || purpose == WithdrawFrom(stake_cred) {
          expect used_redeemer_: BeaconsRedeemer = used_redeemer
          get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
            inputs,
            rest_reds,
            ( req_spot_outputs
            , req_auction_outputs
            , req_beacon_burn
            , Some(used_redeemer_)
            )
          )
        } else {
          // This redeemer is not for the beacon_script so skip it.
          get_input_info(
            beacon_policy_id,
            aftermarket_script_credential,
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

// Determine what outputs and burning are required based on the Spot UTxO being purchased.
fn process_spot_purchase_input(
  input_ref: OutputReference,
  old_datum: Datum, 
  old_value: Value,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (RequiredSpotOutput,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    // The datum must be a SpotDatum.
    BidDatum{..} -> error @"Input is not a Spot UTxO"
    AuctionDatum{..} -> error @"Input is not a Spot UTxO"

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
  }
}

// Determine what outputs and burning are required based on the Bid UTxO or Auction UTxO.
fn process_bid_acceptance_input(
  input_ref: OutputReference,
  old_datum: Datum, 
  old_value: Value,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (Option<RequiredAuctionOutput>,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    // The datum must be either a BidDatum or an AuctionDatum.
    SpotDatum{..} -> error @"Input is not either a BidDatum or an AuctionDatum"

    BidDatum{
      beacon_policy_id,
      nft_policy_id,
      nft_names,
      payment_address,
      bid_deposit,
      ..
    } -> {
      // Bid UTxOs should have three beacons. All of them must be burned. 
      let new_burn = when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2),(tn3,n3)] -> 
          acc_beacon_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Bid UTxO found"
      }

      let new_auction_output = RequiredAuctionOutput {
        beacon_id: beacon_policy_id,
        nft_policy_id: nft_policy_id,
        nft_names: nft_names,
        bid_deposit: bid_deposit,
        req_address: payment_address,
        bid_ref: input_ref
      }

      (Some(new_auction_output), new_burn)
    }

    AuctionDatum{
      beacon_policy_id,
      ..
    } -> {
      // Auction UTxOs should have two beacons. Both of them must be burned. 
      let new_burn = when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2)] -> 
          acc_beacon_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Auction UTxO found"
      }

      (None, new_burn)
    }
  }
}

// Go through the outputs and check that all required outputs are satisfied. The required outputs
// must be in the same order as the corresponding inputs.
fn check_outputs(
  req_spot_outputs: List<RequiredSpotOutput>,
  req_auction_outputs: List<RequiredAuctionOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_spot_outputs, req_auction_outputs, outputs) is {
    // Only auction payment outputs still need to be found
    ( []
    , [ auction_output, ..rest_auction_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredAuctionOutput{
        beacon_id,
        nft_policy_id,
        nft_names,
        bid_deposit,
        bid_ref,
        req_address
      } = auction_output

      // Check if it is the next required auction output.
      if addr == req_address && d == InlineDatum((beacon_id,bid_ref)) {
        // This has its own error messages.
        expect True = check_auction_output(
          bid_deposit,
          nft_policy_id,
          nft_names,
          value.flatten(val)
        )

        check_outputs(
          [],
          rest_auction_outputs, // Move on to the next auction output.
          rest_outs // Move on to the next output.
        )
      } else {
        check_outputs(
          [], 
          req_auction_outputs, // The current auction output still needs to be found.
          rest_outs
        )
      }
    }

    // Only spot payment outputs still need to be found
    ( [ spot_output, ..rest_spot_outputs ]
    , []
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredSpotOutput{
        beacon_id,
        sale_price,
        sale_deposit,
        sale_ref,
        req_address
      } = spot_output

      // Check if it is the next required spot output.
      if addr == req_address && d == InlineDatum((beacon_id,sale_ref)) {
        // This has its own error messages.
        expect True = check_spot_output(
          sale_deposit,
          sale_price,
          value.flatten(val)
        )

        check_outputs(
          rest_spot_outputs, // Move on to the next spot output.
          [],
          rest_outs // Move on to the next output.
        )
      } else {
        check_outputs(
          req_spot_outputs, // The current spot output still needs to be found.
          [],
          rest_outs
        )
      }
    }

    // Both outputs still need to be found.
    ( [ spot_output, ..rest_spot_outputs ]
    , [ auction_output, ..rest_auction_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredAuctionOutput{
        beacon_id,
        nft_policy_id,
        nft_names,
        bid_deposit,
        bid_ref,
        req_address: req_auction_address
      } = auction_output

      let RequiredSpotOutput{
        sale_price,
        sale_deposit,
        sale_ref,
        req_address: req_spot_address,
        ..
      } = spot_output

      // Check if it is the next required spot output.
      if addr == req_spot_address && d == InlineDatum((beacon_id,sale_ref)) {
        // This has its own error messages.
        expect True = check_spot_output(
          sale_deposit,
          sale_price,
          value.flatten(val)
        )

        check_outputs(
          rest_spot_outputs, // Move on to the next spot output.
          req_auction_outputs, // Keep the current auction output.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required auction output.
      } else if addr == req_auction_address && d == InlineDatum((beacon_id,bid_ref)) {
        // This has its own error messages.
        expect True = check_auction_output(
          bid_deposit,
          nft_policy_id,
          nft_names,
          value.flatten(val)
        )

        check_outputs(
          req_spot_outputs, // Keep the current spot output.
          rest_auction_outputs, // Move on to the next auction output.
          rest_outs // Move on to the next output.
        )

      // These outputs still need to be found.
      } else {
        check_outputs(
          req_spot_outputs, // Keep the current spot output.
          req_auction_outputs, // Keep the current auction output.
          rest_outs // Move on to the next output.
        )
      }
    }

    ([], [], _) -> True

    ([_,..], _, []) -> error "Not all required spot payments found"

    (_, [_,..], []) -> error "Not all required auction payments found"
  }
}

fn check_spot_output(
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

          check_spot_output(
            sale_deposit,
            rest_req,
            rest_actual
          )

        } else {
          expect True = error_if_false(
            @"Spot payment UTxO missing sale deposit",
            actual_val >= sale_deposit
          )

          check_spot_output(
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

          check_spot_output(
            sale_deposit,
            rest_req,
            rest_actual
          )
        } else {
          check_spot_output(
            sale_deposit,
            req_assets, // This asset still needs to be found.
            rest_actual
          )
        }
      }
    }
  }
}

fn check_auction_output(
  bid_deposit: Int,
  nft_policy_id: PolicyId,
  req_nfts: List<AssetName>,
  actual_assets: List<(PolicyId,AssetName,Int)>
) -> Bool {
  when (req_nfts,actual_assets) is {
    ([_next_req,..],[]) -> error @"Auction payment UTxO has wrong value"

    ([],_) -> True

    ([next_nft,..rest_req],[next_actual,..rest_actual]) -> {
      let (actual_pol, actual_name, actual_val) = next_actual

      // Ada needs to be handled separately.
      if actual_pol == #"" {
        expect True = error_if_false(
          @"Auction payment UTxO missing bid deposit",
          actual_val >= bid_deposit
        )

        check_auction_output(
          bid_deposit,
          nft_policy_id,
          req_nfts, // This asset still needs to be found.
          rest_actual
        )
      } else {
        if actual_pol == nft_policy_id && actual_name == next_nft {
          // The fact the NFT is present among the actual value means the balance is
          // greater than 0.

          check_auction_output(
            bid_deposit,
            nft_policy_id,
            rest_req,
            rest_actual
          )
        } else {
          check_auction_output(
            bid_deposit,
            nft_policy_id,
            req_nfts, // This asset still needs to be found.
            rest_actual
          )
        }
      }
    }
  }
}
