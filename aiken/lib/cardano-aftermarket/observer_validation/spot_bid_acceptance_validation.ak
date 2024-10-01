use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/transaction.{Datum, Output, InlineDatum, OutputReference} as tx
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_aftermarket/data/datums.{AuctionDatum, SpotBidDatum}
use cardano_aftermarket/utils.{error_if_false}

// The information needed to validate the auction payment output. The information comes from the
// SpotBid UTxO.
pub type RequiredSpotAuctionOutput {
  beacon_id: PolicyId,
  nft_policy_id: PolicyId,
  nft_names: List<AssetName>,
  bid_deposit: Int,
  bid_ref: OutputReference,
  req_address: Address
}

// Determine what outputs and burning are required based on the SpotBid UTxO or Auction UTxO.
pub fn process_spot_bid_acceptance_input(
  input_ref: OutputReference,
  old_datum: Datum, 
  old_value: Value,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (Option<RequiredSpotAuctionOutput>,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    SpotBidDatum{
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

        _ -> error @"Invalid SpotBid UTxO found"
      }

      let new_auction_output = RequiredSpotAuctionOutput {
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

    // The datum must be either a SpotBidDatum or an AuctionDatum.
    _ -> error @"Input is not either a SpotBidDatum or an AuctionDatum"
  }
}

fn is_next_spot_bid_payment_output(
  auction_output: RequiredSpotAuctionOutput, 
  output: Output
) -> Bool {
  let Output(addr,val,d,_) = output
  let RequiredSpotAuctionOutput{
    beacon_id,
    nft_policy_id,
    nft_names,
    bid_deposit,
    bid_ref,
    req_address
  } = auction_output

  if addr == req_address && d == InlineDatum((beacon_id,bid_ref)) {
    // This has its own error messages.
    check_spot_bid_payment_output(
      bid_deposit,
      nft_policy_id,
      nft_names,
      value.flatten(val)
    )
  } else { False }
}

fn check_spot_bid_payment_output(
  bid_deposit: Int,
  nft_policy_id: PolicyId,
  req_nfts: List<AssetName>,
  actual_assets: List<(PolicyId,AssetName,Int)>
) -> Bool {
  when (req_nfts,actual_assets) is {
    ([_next_req,..],[]) -> error @"Spot auction payment UTxO has wrong value"

    ([],_) -> True

    ([next_nft,..rest_req],[next_actual,..rest_actual]) -> {
      let (actual_pol, actual_name, actual_val) = next_actual

      // Ada needs to be handled separately.
      if actual_pol == #"" {
        expect True = error_if_false(
          @"Spot auction payment UTxO missing bid deposit",
          actual_val >= bid_deposit
        )

        check_spot_bid_payment_output(
          bid_deposit,
          nft_policy_id,
          req_nfts, // This asset still needs to be found.
          rest_actual
        )
      } else {
        if actual_pol == nft_policy_id && actual_name == next_nft {
          // The fact the NFT is present among the actual value means the balance is
          // greater than 0.

          check_spot_bid_payment_output(
            bid_deposit,
            nft_policy_id,
            rest_req,
            rest_actual
          )
        } else {
          check_spot_bid_payment_output(
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

pub fn missing_spot_bid_outputs(
  req: List<RequiredSpotAuctionOutput>, 
  output: Output
) -> List<RequiredSpotAuctionOutput> {
  when req is {
    [] -> []
    [x, ..rest] -> if is_next_spot_bid_payment_output(x, output) { rest } else { req }
  }
}
