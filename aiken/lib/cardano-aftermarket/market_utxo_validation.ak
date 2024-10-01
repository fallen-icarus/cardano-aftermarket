use aiken/dict.{Dict}
use aiken/list
use aiken/option
use aiken/time.{PosixTime}
use aiken/transaction.{Datum,Output}
use aiken/transaction/credential.{Address, ScriptCredential, StakeCredential, Inline}
use aiken/transaction/value.{AssetName,PolicyId,Value}

use cardano_aftermarket/data/core.
  { ScriptHash
  , BeaconId
  , PolicyBeacon
  , Asset
  , KeyHash
  }
use cardano_aftermarket/data/datums.
  { MarketDatum
  , SpotDatum
  , AuctionDatum
  , SpotBidDatum
  , ClaimBidDatum
  , AcceptedBidDatum
  }
use cardano_aftermarket/utils.{error_if_false,trace_if_false,parse_datum}

// Check if the beacons are locked at the proper address and the market UTxOs are configured properly.
// This function can return False.
pub fn market_beacons_destination_check(
  market_script_hash: ScriptHash,
  beacon_id: BeaconId,
  proxy_hash: ScriptHash,
  aftermarket_observer_hash: ScriptHash,
  outputs: List<Output>,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>,
  invalid_hereafter: Option<PosixTime>
) -> Bool {
  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val, core.from_beacon_id(beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has three types of beacon. This is a SpotBid UTxO, a ClaimBid UTxO, or
      // an AcceptedBid UTxO.
      [(_,_),(_,_),(_,_)] -> {
        and {
          // All previous outputs must be valid.
          acc,

          // The output must be at a DApp address with a valid staking credential.
          trace_if_false( 
            @"Beacons must go to a DApp address using a valid staking credential",
            ScriptCredential(market_script_hash) == pay && option.is_some(stake)
          ),

          when parse_datum(d) is {
            ClaimBidDatum{..} as claimBidDatum -> {
              // The ClaimBid UTxO must be properly configured. This has its own trace messages.
              valid_claim_bid_utxo(
                beacon_id,
                aftermarket_observer_hash,
                withdrawals,
                extra_signatories,
                invalid_hereafter,
                val,
                claimBidDatum
              )
            }

            SpotBidDatum{..} as spotBidDatum -> {
              // The SpotBid UTxO must be properly configured. This has its own trace messages.
              valid_spot_bid_utxo(
                beacon_id,
                proxy_hash,
                aftermarket_observer_hash,
                withdrawals,
                extra_signatories,
                val,
                spotBidDatum
              )
            }

            AcceptedBidDatum{..} -> {
              // This is an edge case since these are not meant to be created directly. Instead,
              // they are meant to be created by evolving a ClaimBid UTxO. This UTxO can't just be
              // ignored since it could result in creating the BidderIds for another user without
              // their approval which could lead to a DOS attack. This script does not directly
              // check minting. Instead, the observer script will ensure these UTxOs are handled
              // correctly.
              trace_if_false(
                @"Observer script not executed", 
                utils.staking_credential_approves(
                  Some(Inline(ScriptCredential(aftermarket_observer_hash))),
                  withdrawals, 
                  []
                )
              )
            }

            _ -> error @"Beacons stored with invalid datum"
          }
        }
      }

      // The UTxO has two types of beacon. This can either be an Auction UTxO or a Spot UTxO.
      [(tn1,_),(tn2,_)] -> {
        and {
          // All previous outputs must be valid.
          acc,

          // The output must be at a DApp address with a valid staking credential.
          trace_if_false( 
            @"Beacons must go to a DApp address using a valid staking credential",
            ScriptCredential(market_script_hash) == pay && option.is_some(stake)
          ),

          if tn1 == "Auction" || tn2 == "Auction" {
            // The Auction UTxO must be properly configured. This has its own trace messages.
            valid_auction_utxo(beacon_id, aftermarket_observer_hash, val, d)
          } else if tn1 == "Spot" || tn2 == "Spot" {
            // The Spot UTxO must be properly configured. This has its own trace messages.
            valid_spot_utxo(beacon_id, proxy_hash, aftermarket_observer_hash, val, d)
          } else {
            error @"Beacons not stored with a valid Market UTxO"
          }
        }
      }

      // This path can only be taken if the above matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Beacons not stored with a valid Market UTxO"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl(outputs, True, foo)
}

// This checks the datum and the value for Auction UTxOs.
fn valid_auction_utxo(
  expected_beacon_id: BeaconId,
  expected_aftermarket_observer_hash: ScriptHash,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the AuctionDatum. Crash the script if it is not an AuctionDatum.
  expect AuctionDatum{
    beacon_policy_id,
    aftermarket_observer_hash,
    nft_policy_id,
    nft_names,
    starting_price
  } = when parse_datum(output_datum) is {
    AuctionDatum{..} as datum -> datum
    _ -> error @"Auction UTxO not stored with an AuctionDatum"
  }

  let expected_policy_beacon = core.make_policy_beacon(nft_policy_id)

  // Get the nfts from the target policy id present and the amount of ada in the UTxO. 
  let (actual_nfts,_) = 
    extract_seller_utxo_quantity(
      output_value,
      expected_beacon_id,
      nft_policy_id,
      expected_policy_beacon,
      "Auction"
    )

  // All of the following must be true.
  and {
    // Datum checks:
    trace_if_false(
      @"Datum has wrong beacon id", 
      expected_beacon_id == core.to_beacon_id(beacon_policy_id)),
    trace_if_false(
      @"Datum has wrong aftermarket observer hash", 
      expected_aftermarket_observer_hash == aftermarket_observer_hash),
    trace_if_false(
      @"NFTs cannot be protocol beacons", 
      beacon_policy_id != nft_policy_id),
    trace_if_false(
      @"NFT names cannot be empty",
      nft_names != []),
    trace_if_false(
      @"Invalid starting price",
      starting_price != []),
    trace_if_false(
      @"Invalid starting price",
      list.all(
        starting_price,
        fn(x) { 
          let (_,price) = x
          price > 0
        }
      )
    ),

    // Value checks:
    trace_if_false(
      @"NFT names don't match Auction UTxO value",
      // This also ensures the NFT names are sorted and contain no duplicates.
      actual_nfts == nft_names
    )
  }
}

// This checks the datum and the value for Spot UTxOs.
fn valid_spot_utxo(
  expected_beacon_id: BeaconId,
  proxy_hash: ScriptHash,
  expected_aftermarket_observer_hash: ScriptHash,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the SpotDatum. Crash the script if it is not a SpotDatum.
  expect SpotDatum{
    beacon_policy_id,
    aftermarket_observer_hash,
    nft_policy_id,
    nft_names,
    payment_address,
    sale_deposit,
    sale_price
  } = when parse_datum(output_datum) is {
    SpotDatum{..} as datum -> datum
    _ -> error @"Spot UTxO not stored with a SpotDatum"
  }

  let expected_policy_beacon = core.make_policy_beacon(nft_policy_id)

  // Get the nfts from the target policy id present and the amount of ada in the UTxO. 
  let (actual_nfts,ada_quantity) = 
    extract_seller_utxo_quantity(
      output_value,
      expected_beacon_id,
      nft_policy_id,
      expected_policy_beacon,
      "Spot"
    )

  // All of the following must be true.
  and {
    // Datum checks:
    trace_if_false(
      @"Datum has wrong beacon id", 
      expected_beacon_id == core.to_beacon_id(beacon_policy_id)),
    trace_if_false(
      @"Datum has wrong aftermarket observer hash", 
      expected_aftermarket_observer_hash == aftermarket_observer_hash),
    trace_if_false(
      @"NFTs cannot be protocol beacons", 
      beacon_policy_id != nft_policy_id),
    trace_if_false(
      @"NFT names cannot be empty",
      nft_names != []),
    trace_if_false(
      @"Invalid payment address",
      utils.valid_payment_address(proxy_hash,payment_address)),
    trace_if_false(
      @"Sale price cannot be empty",
      sale_price != []),
    trace_if_false(
      @"Sale price must be sorted",
      sale_price == list.sort(sale_price,utils.compare_asset_no_dups)),
    trace_if_false(
      @"All prices must be > 0",
      list.all(
        sale_price,
        fn(x) { 
          let (_,price) = x
          price > 0
        }
      )
    ),

    // Value checks:
    trace_if_false(
      @"NFT names don't match Spot UTxO value",
      // This also ensures the NFT names are sorted and contain no duplicates.
      actual_nfts == nft_names
    ),
    trace_if_false(
      @"Spot UTxO has wrong deposit",
      // This also ensures the deposit cannot be zero or negative.
      ada_quantity == sale_deposit
    ),
  }
}

// This checks the datum and the value for SpotBid UTxOs.
fn valid_spot_bid_utxo(
  expected_beacon_id: BeaconId,
  proxy_hash: ScriptHash,
  expected_aftermarket_observer_hash: ScriptHash,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>,
  output_value: Value,
  spotBidDatum: MarketDatum
) -> Bool {
  // Extract the info from the SpotBidDatum.
  expect SpotBidDatum{
    beacon_policy_id,
    aftermarket_observer_hash,
    nft_policy_id,
    bidder_credential,
    nft_names,
    payment_address,
    bid_deposit,
    bid
  } = spotBidDatum

  let expected_policy_beacon = core.make_policy_beacon(nft_policy_id)

  // Get the name of the BidderId beacon for this bidder's credential.
  let bidder_id = 
    core.to_bidder_id(bidder_credential)
    |> core.get_bidder_id_name(_)

  // Add the beacons to the bid amount so that the utxo value can be checked against it. Ada does
  // not need to be added, even if the bid list does not contain ada; ada will be handled
  // separately. Since this function will throw an error if there are duplicates, none of the
  // beacons can be in the bid list.
  let expected_total_value =
    list.push(bid, ((beacon_policy_id,"Bid"),1))
    |> list.push(_, ((beacon_policy_id,core.from_policy_beacon(expected_policy_beacon)),1))
    |> list.push(_, ((beacon_policy_id,bidder_id),1))
    |> list.sort(_, utils.compare_asset_no_dups)

  and {
    // Approval checks:
    // This is redundantly checked per Bid UTxOs but it enables flexibility in the BidderIds used.
    // When only one BidderId is used, the withdrawals and signatures could only have a few entries
    // which means the performance hit should be mostly negligible.
    trace_if_false(
      @"Bidder credential did not approve", 
      utils.staking_credential_approves(
        Some(Inline(bidder_credential)),
        withdrawals, 
        extra_signatories
      )
    ),

    // Datum checks:
    trace_if_false(
      @"Datum has wrong beacon id", 
      expected_beacon_id == core.to_beacon_id(beacon_policy_id)),
    trace_if_false(
      @"Datum has wrong aftermarket observer hash", 
      expected_aftermarket_observer_hash == aftermarket_observer_hash),
    trace_if_false(
      @"NFTs cannot be protocol beacons", 
      beacon_policy_id != nft_policy_id),
    trace_if_false(
      @"NFT names cannot be empty", 
      nft_names != []),
    trace_if_false(
      @"NFT names must be sorted", 
      nft_names == list.sort(nft_names,utils.compare_names_no_dups)),
    trace_if_false(
      @"Invalid payment address",
      utils.valid_payment_address(proxy_hash,payment_address)),
    trace_if_false(
      @"Invalid bid deposit", 
      // This must be checked because a negative bid value could be taken from the ada amount in the
      // bid.
      bid_deposit > 0),
    trace_if_false(
      @"Bid cannot be empty", 
      bid != []),
    trace_if_false(
      @"Bid assets must be sorted", 
      bid == list.sort(bid,utils.compare_asset_no_dups)),
    trace_if_false(
      @"All prices must be > 0",
      list.all(
        bid,
        fn(x) { 
          let (_,price) = x
          price > 0
        }
      )
    ),

    // Value checks:
    // This has its own trace messages.
    has_proper_buyer_value(
      bid_deposit,
      expected_total_value,
      value.flatten(output_value)
    )
  }
}

// This checks the datum and and value for ClaimBid UTxOs.
fn valid_claim_bid_utxo(
  expected_beacon_id: BeaconId,
  expected_aftermarket_observer_hash: ScriptHash,
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>,
  invalid_hereafter: Option<PosixTime>,
  output_value: Value,
  claimBidDatum: MarketDatum
) -> Bool {
  // Extract the info from the ClaimBidDatum.
  expect ClaimBidDatum{
    beacon_policy_id,
    aftermarket_observer_hash,
    nft_policy_id,
    bidder_credential,
    nft_names,
    bid_deposit,
    bid,
    bid_expiration,
    claim_expiration
  } = claimBidDatum

  let future_time = when invalid_hereafter is {
    None -> error @"invalid-hereafter not specified"
    Some(x) -> x
  }

  let expected_policy_beacon = core.make_policy_beacon(nft_policy_id)

  // Get the name of the BidderId beacon for this bidder's credential.
  let bidder_id = 
    core.to_bidder_id(bidder_credential)
    |> core.get_bidder_id_name(_)

  // This utxo should just have ada and the beacons. The bid does not need to be present. Ada will
  // also be handled separately so it does not need to be included here.
  let expected_total_value = [((beacon_policy_id,"Bid"),1)]
    |> list.push(_, ((beacon_policy_id,core.from_policy_beacon(expected_policy_beacon)),1))
    |> list.push(_, ((beacon_policy_id,bidder_id),1))
    |> list.sort(_, utils.compare_asset_no_dups)

  and {
    // Approval checks:
    // This is redundantly checked per Bid UTxOs but it enables flexibility in the BidderIds used.
    // When only one BidderId is used, the withdrawals and signatures could only have a few entries
    // which means the performance hit should be mostly negligible.
    trace_if_false(
      @"Bidder credential did not approve", 
      utils.staking_credential_approves(
        Some(Inline(bidder_credential)),
        withdrawals, 
        extra_signatories
      )
    ),

    // Datum checks:
    trace_if_false(
      @"Datum has wrong beacon id", 
      expected_beacon_id == core.to_beacon_id(beacon_policy_id)),
    trace_if_false(
      @"Datum has wrong aftermarket observer hash", 
      expected_aftermarket_observer_hash == aftermarket_observer_hash),
    trace_if_false(
      @"NFTs cannot be protocol beacons", 
      beacon_policy_id != nft_policy_id),
    trace_if_false(
      @"NFT names cannot be empty", 
      nft_names != []),
    trace_if_false(
      @"NFT names must be sorted", 
      nft_names == list.sort(nft_names,utils.compare_names_no_dups)),
    trace_if_false(
      @"Invalid bid expiration",
      when bid_expiration is {
        None -> True
        Some(expr) -> expr >= future_time
      }),
    trace_if_false(
      @"Invalid claim expiration",
      claim_expiration >= future_time),
    trace_if_false(
      @"Invalid bid deposit", 
      bid_deposit > 0),
    trace_if_false(
      @"Bid cannot be empty", 
      bid != []),
    trace_if_false(
      @"Bid assets must be sorted", 
      bid == list.sort(bid,utils.compare_asset_no_dups)),
    list.all(
      bid,
      fn(x) { 
        let ((pol,_),price) = x
        trace_if_false(@"All prices must be > 0", price > 0) &&
        trace_if_false(@"Protocol beacons cannot be bid", pol != beacon_policy_id)
      }
    ),

    // Value checks:
    // This has its own trace messages.
    has_proper_buyer_value(
      bid_deposit,
      expected_total_value,
      value.flatten(output_value)
    )
  }
}

// Return the actual NFTs present as well as the total amount of ada present.
fn extract_seller_utxo_quantity(
  val: Value,
  beacon_id: BeaconId,
  nft_policy_id: PolicyId,
  policy_beacon: PolicyBeacon,
  category_beacon: AssetName, // Either "Auction" or "Spot". This is not used with Bid UTxOs.
) -> (List<AssetName>,Int) {
  // A function to map over the value of this UTxO. It will return the NFTs present that are
  // associated with the policy_beacon and the total amount of ada.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (List<AssetName>,Int)) {
    let (cn,tns_dict) = x
    let (actual_nfts,ada_quantity) = acc

    // Check the beacons.
    if cn == core.from_beacon_id(beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool)) {
        let (tn,n) = y
        let (policy_check,category_check) = inner_acc
        if tn == core.from_policy_beacon(policy_beacon) {
          // There must only be one Policy beacon. If n was zero, tn could not have matched.
          ( error_if_false(@"UTxO has too many Policy beacons", n == 1)
          , category_check
          )
        } else if tn == category_beacon {
          // There must only be one category beacon. If n was zero, tn could not have matched.
          ( policy_check
          , error_if_false(@"UTxO has too many category beacons", n == 1)
          )
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False),beacon_check) is {
        (True,True) -> (actual_nfts,ada_quantity)

        // This path should never be taken since the script should fail earlier if the UTxO has 
        // too few or too many types of beacons.
        _ -> error @"E4"
      }
    } 

    // Check that the asset is one of the NFTs. 
    else if cn == nft_policy_id {
      ( dict.to_list(tns_dict) |> 
          list.map(
            _, fn(x) {
              let (name,_) = x
              name
            }
          )
      , ada_quantity
      )
    }

    // Check that the asset is ada. Ada must be explicitly allowed to satisfy the 
    // minUTxOValue requirement.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      (actual_nfts, ada_quantity+n)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  list.foldl(value.to_dict(val) |> dict.to_list(_), ([],0), foo)
}

// Check that the Bid UTxO has the specified deposit and bid amount. ClaimBid UTxOs should not be
// stored with the bid amount. When ada is part of the bid, the amount that must be present is
// deposit + bid amount. The beacons are added to the `bid` list and are therefore also checked
// with this function.
fn has_proper_buyer_value(
  bid_deposit: Int,
  bid: List<(Asset,Int)>,
  actual_assets: List<(PolicyId,AssetName,Int)>
) -> Bool {
  when (bid,actual_assets) is {
    ([_next_req,..],[]) -> error @"Bid UTxO has wrong value"

    ([],[]) -> True

    ([],[next_actual,..rest_actual]) -> {
      let (actual_pol, _, actual_val) = next_actual

      if actual_pol == #"" {
        expect True = error_if_false(
          @"Bid UTxO missing bid deposit",
          actual_val == bid_deposit
        )

        has_proper_buyer_value(
          bid_deposit,
          [],
          rest_actual
        )
      } else {
        error @"Bid UTxO has wrong value"
      }
    }

    ([next_req,..rest_req],[next_actual,..rest_actual]) -> {
      let ((req_pol, req_name), req_val) = next_req
      let (actual_pol, actual_name, actual_val) = next_actual

      // Ada needs to be handled separately.
      if actual_pol == #"" {
        // If ada is also part of the bid.
        if req_pol == #"" {
          expect True = error_if_false(
            @"Bid UTxO has wrong value",
            actual_val == req_val + bid_deposit
          )

          has_proper_buyer_value(
            bid_deposit,
            rest_req,
            rest_actual
          )

        // If ada is not part of the bid.
        } else {
          expect True = error_if_false(
            @"Bid UTxO missing bid deposit",
            actual_val == bid_deposit
          )

          has_proper_buyer_value(
            bid_deposit,
            bid, // This bid asset still needs to be found.
            rest_actual
          )
        }

      // This is a bid asset.
      } else if actual_pol == req_pol && actual_name == req_name {
          expect True = error_if_false(
            @"Bid UTxO has wrong value",
            actual_val == req_val
          )

          has_proper_buyer_value(
            bid_deposit,
            rest_req,
            rest_actual
          )

      // This is either an extra asset or a required asset was skipped.
      } else {
        error @"Bid UTxO has wrong value"
      }
    }
  }
}
