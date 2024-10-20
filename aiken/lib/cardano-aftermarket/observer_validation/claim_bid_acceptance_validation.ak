use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{Datum, Output, InlineDatum} as tx
use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_aftermarket/data/core.{ScriptHash}
use cardano_aftermarket/data/datums.{AcceptedBidDatum, AuctionDatum, ClaimBidDatum, MarketDatum}
use cardano_aftermarket/utils.{error_if_false}

// The information needed to validate the claim acceptance output. The information comes from the
// Bid UTxO.
pub type RequiredAcceptanceOutput {
  beacon_id: PolicyId,
  beacons: List<(AssetName,Int)>,
  nft_policy_id: PolicyId,
  nft_names: List<AssetName>,
  bid_deposit: Int,
  seller_deposit: Int,
  new_datum: MarketDatum,
  seller_address: Address
}

// Determine what outputs and burning are required based on the ClaimBid UTxO or Auction UTxO.
pub fn process_claim_bid_acceptance_input(
  seller_deposit: Int, // This should come from redeemer.
  payment_address: Address, // This should come from redeemer.
  seller_address: Address, // This should be address where input is from.
  old_datum: Datum, 
  old_value: Value,
  invalid_hereafter: Option<PosixTime>,
  proxy_hash: ScriptHash,
  acc_beacon_burn: Dict<AssetName,Int>
) -> (Option<RequiredAcceptanceOutput>,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    ClaimBidDatum{
      beacon_policy_id,
      aftermarket_observer_hash,
      nft_policy_id,
      nft_names,
      bid_deposit,
      bidder_credential,
      bid,
      bid_expiration,
      claim_expiration
    } -> {
      // Bid UTxOs should have three beacons. All of them must be found in the AcceptedBid UTxO. 
      let beacons = 
        when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
          [_,_,_] as beacons -> beacons
          _ -> error @"Invalid ClaimBid UTxO found"
        }

      // The seller_deposit must be > 0.
      expect True = error_if_false(
        @"Invalid seller deposit",
        seller_deposit >= 0
      )

      // The new payment_address must be a valid address.
      expect True = error_if_false(
        @"Payment address is not a valid address",
        utils.valid_payment_address(proxy_hash, payment_address)
      )

      // The bid must not be expired.
      expect True = 
        error_if_false(@"Bid is expired",
          when bid_expiration is {
            None -> True
            Some(expiration) -> 
              when invalid_hereafter is {
                None -> error @"invalid-hereafter not specified"
                Some(future_time) -> future_time <= expiration
              }
          }
        )

      let new_datum = AcceptedBidDatum
        { beacon_policy_id: beacon_policy_id
        , aftermarket_observer_hash: aftermarket_observer_hash
        , nft_policy_id: nft_policy_id
        , bidder_credential: bidder_credential
        , nft_names: nft_names
        , bid_deposit: bid_deposit
        , seller_deposit: seller_deposit
        , payment_address: payment_address
        , bid: bid
        , claim_expiration: claim_expiration
        }

      let new_accepted_output = RequiredAcceptanceOutput {
        beacon_id: beacon_policy_id,
        beacons: beacons,
        nft_policy_id: nft_policy_id,
        nft_names: nft_names,
        bid_deposit: bid_deposit,
        seller_deposit: seller_deposit,
        new_datum: new_datum,
        seller_address: seller_address
      }

      (Some(new_accepted_output), acc_beacon_burn)
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
    _ -> error @"Input is not either a ClaimBidDatum or an AuctionDatum"
  }
}

fn is_next_acceptance_output(
  acceptance_output: RequiredAcceptanceOutput, 
  output: Output
) -> Bool {
  let Output(addr,val,d,_) = output
  let RequiredAcceptanceOutput{
    beacon_id,
    beacons,
    nft_policy_id,
    nft_names,
    bid_deposit,
    seller_deposit,
    new_datum,
    seller_address
  } = acceptance_output

  if addr == seller_address && d == InlineDatum(new_datum) {
    // This has its own error messages.
    has_proper_acceptance_value(
      beacon_id,
      beacons,
      bid_deposit,
      seller_deposit,
      nft_policy_id,
      nft_names,
      val
    )
  } else { False }
}

fn has_proper_acceptance_value(
  beacon_id: PolicyId,
  beacons: List<(AssetName,Int)>,
  bid_deposit: Int,
  seller_deposit: Int,
  nft_policy_id: PolicyId,
  nft_names: List<AssetName>,
  val: Value
) -> Bool {
  // A function to map over the value of this UTxO. It can't actually return False.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Bool,Bool,Bool)) {
    let (cn,tns_dict) = x
    let (has_beacons,has_nfts,has_deposits) = acc

    // Check the beacons.
    if cn == beacon_id {
      ( error_if_false(
          @"AcceptedBid UTxO has wrong beacons",
          dict.to_list(tns_dict) == beacons
        )
      , has_nfts
      , has_deposits
      )
    }

    // Check that the asset is one of the NFTs. 
    else if cn == nft_policy_id {
      ( has_beacons
      , error_if_false(
          @"AcceptedBid UTxO has wrong NFTs",
          nft_names == 
            (dict.to_list(tns_dict) |> 
              list.map(
                _, fn(x) {
                  let (name,_) = x
                  name
                }
              )
            )
        )
      , has_deposits
      )
    }

    // Check that the asset is ada. Ada must be explicitly allowed to satisfy the minUTxOValue
    // requirement.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      ( has_beacons
      , has_nfts
      , error_if_false(
          @"AcceptedBid UTxO has wrong value of ADA",
          n == bid_deposit + seller_deposit
        )
      )
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  when list.foldl(value.to_dict(val) |> dict.to_list(_), (False,False,False), foo) is {
    (True, True, True) -> True
    _ -> error @"AcceptedBid UTxO has wrong value"
  }
}

pub fn missing_acceptance_outputs(
  aftermarket_script_credential: Credential,
  req: List<RequiredAcceptanceOutput>, 
  output: Output
) -> List<RequiredAcceptanceOutput> {
  when req is {
    [] -> {
      // This needs to ensure the output is not an unnecessary AcceptedBid UTxO since it can result
      // in improper minting of BidderIds.
      let Output(Address(pay_cred,_),_,d,_) = output

      // This checks the address first to not try parsing unrelated datums. I am assuming
      // checking the address is cheaper than checking the datum.
      if pay_cred == aftermarket_script_credential {
        when utils.parse_datum(d) is {
          datums.AcceptedBidDatum{..} -> error @"Extra AcceptedBid UTxO found"
          _ -> []
        }
      } else {
        []
      }
    }

    [x, ..rest] -> if is_next_acceptance_output(x, output) { rest } else { req }
  }
}
