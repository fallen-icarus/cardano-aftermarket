use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/time.{PosixTime}
use aiken/transaction.{Datum} as tx
use aiken/transaction/value.{Value, AssetName}

use cardano_aftermarket/data/datums.{AcceptedBidDatum}
use cardano_aftermarket/utils.{error_if_false}

// Determine what burning is required based on the AcceptedBidDatum.
pub fn process_unlock_input(
  old_datum: Datum, 
  old_value: Value,
  invalid_before: Option<PosixTime>,
  acc_beacon_burn: Dict<AssetName,Int>
) -> Dict<AssetName,Int> {
  when utils.parse_datum(old_datum) is {
    AcceptedBidDatum{beacon_policy_id, claim_expiration,..} -> {
      let current_time = when invalid_before is {
        None -> error @"invalid-before not specified"
        Some(x) -> x
      }

      // The claim period must have passed.
      expect True = error_if_false(
        @"Claim period has not expired",
        claim_expiration <= current_time
      )

      // Bid UTxOs should have three beacons. All of them must be burned. 
      when value.tokens(old_value,beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2),(tn3,n3)] -> 
          acc_beacon_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Bid UTxO found"
      }
    }

    // The datum must be an AcceptedBidDatum.
    _ -> error @"Input is not an AcceptedBidDatum."
  }
}
