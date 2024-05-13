use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/transaction.
  { Datum
  , InlineDatum
  , Redeemer
  , ScriptPurpose
  } as tx
use aiken/transaction/credential.
  { Address
  , Credential
  , Inline
  , StakeCredential
  , ScriptCredential
  , VerificationKeyCredential
  }
use aiken/transaction/value.{AssetName, PolicyId}

use cardano_aftermarket/data/core.{Asset, ScriptHash, KeyHash}
use cardano_aftermarket/data/datums.{MarketDatum}

// If a test fails, provide a user friendly error message. This is useful
// when chaining if-then-elses.
pub fn trace_if_false(msg: String, predicate: Bool) {
  if !predicate {
    trace msg
    predicate
  } else {
    predicate
  }
}

// If a test fails, immediately crash the script with an error message. This is useful
// when some side-effect needs to be verified before continuing with the main function.
pub fn error_if_false(msg: String, predicate: Bool) {
  if !predicate {
    error msg
  } else {
    predicate
  }
}

// This parses the datums attached to market UTxOs. It will fail with an error unless
// the datum is an inline MarketDatum.
pub fn parse_datum(raw_datum: Datum) -> MarketDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect market_datum: MarketDatum = d
      market_datum
    }
    _ -> error @"All datums must be inline datums"
  }
}

// Check that either the staking pubkey signed or the staking script was executed.
pub fn staking_credential_approves(
  staking_cred: Option<StakeCredential>, 
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>
) -> Bool {
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> list.has(extra_signatories,skey)

    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> dict.has_key(withdrawals,svh)
  }
}

// Check if the address is using a pubkey or the proxy script for the payment credential.
// If the proxy script is used for the payment credential, the address MUST have a staking
// credential.
pub fn valid_payment_address(proxy_hash: ScriptHash, addr: Address) -> Bool {
  when addr is {
    // Any payment pubkey can be used.
    Address(VerificationKeyCredential(_),_) -> True

    // The only payment script supported is the proxy script.
    Address(ScriptCredential(sh),stake) -> sh == proxy_hash && option.is_some(stake)
  }
}

// Check if a credential is a pubkey credential.
pub fn is_pub_key_credential(cred: Credential) -> Bool {
  when cred is {
    VerificationKeyCredential(_) -> True
    _ -> False
  }
}

// A helper function for adding tokens to a Value.
pub fn sum_token_values(_key: AssetName, v1: Int, v2: Int) -> Option<Int> {
  Some(v1 + v2)
}

// Check if the target script was executed using either a minting execution or a staking
// execution, and with the proper redeemer.
pub fn script_executed_with_redeemer(
  redeemers: Dict<ScriptPurpose, Redeemer>,
  req_redeemer: Data, 
  req_script_id: PolicyId
) -> Bool {
  let req_mint = tx.Mint(req_script_id)
  let req_stake = tx.WithdrawFrom(Inline(ScriptCredential(req_script_id)))
  let foo = fn(red: (ScriptPurpose, Redeemer)) -> Bool {
    let (purpose,red_used) = red
    and {
      purpose == req_mint || purpose == req_stake,
      red_used == req_redeemer
    }
  }
  list.find(dict.to_list(redeemers),foo) |> option.is_some(_)
}

// Compare two assets. Fail if a duplicate is found.
pub fn compare_asset_no_dups(asset1: (Asset,Int), asset2: (Asset,Int)) -> Ordering {
  let ((pol1,tok1),_) = asset1
  let ((pol2,tok2),_) = asset2

  when byte.compare(pol1,pol2) is {
    Less -> Less
    Greater -> Greater
    Equal -> {
      when byte.compare(tok1,tok2) is {
        Less -> Less
        Greater -> Greater
        Equal -> error @"Duplicate asset found"
      }
    }
  }
}

// Compare two asset names. Fail if a duplicate is found.
pub fn compare_names_no_dups(name1: AssetName, name2: AssetName) -> Ordering {
  when byte.compare(name1,name2) is {
    Less -> Less
    Greater -> Greater
    Equal -> error @"Duplicate asset found"
  }
}
