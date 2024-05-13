use aiken/bytearray as byte
use aiken/hash.{Blake2b_224,Hash}
use aiken/transaction/credential.
  { Credential
  , Script
  , ScriptCredential
  , VerificationKey
  , VerificationKeyCredential
  }
use aiken/transaction/value.{PolicyId, AssetName}

//-----------------------------------------------
// Type Synonyms
//-----------------------------------------------
pub type Asset = (PolicyId,AssetName)
pub type ScriptHash = Hash<Blake2b_224,Script>
pub type KeyHash = Hash<Blake2b_224,VerificationKey>

//-----------------------------------------------
// Beacon Id
//-----------------------------------------------
// A zero cost type representing the beacon script's policy id.
pub opaque type BeaconId {
  inner: PolicyId
}

// Cast a policy id to a BeaconId.
pub fn to_beacon_id(policy_id: PolicyId) -> BeaconId {
  BeaconId(policy_id)
}

// Get the policy id from a BeaconId.
pub fn from_beacon_id(beacon: BeaconId) -> PolicyId {
  beacon.inner
}

//-----------------------------------------------
// Policy Beacon
//-----------------------------------------------
// A zero cost type representing the policy beacon.
pub opaque type PolicyBeacon {
  inner: AssetName
}

// Convert a policy id to a PolicyBeacon. It is prefixed with "00" before hashing to help guarantee
// uniqueness from BidderIds.
pub fn make_policy_beacon(policy_id: PolicyId) -> PolicyBeacon {
  hash.sha2_256(
    byte.concat(#"00", policy_id) // Prefix it with "00".
  ) |> PolicyBeacon
}

pub fn unsafe_to_policy_beacon(policy_id: AssetName) -> PolicyBeacon {
  PolicyBeacon(policy_id)
}

// Get the asset name from a PolicyBeacon.
pub fn from_policy_beacon(beacon: PolicyBeacon) -> AssetName {
  beacon.inner
}

//-----------------------------------------------
// Bidder Id
//-----------------------------------------------
// A type representing the bidder's staking credential id.
pub opaque type BidderId {
  inner: AssetName
}

// Convert a pubkey hash or script hash to a BidderId. It is prefixed with "01" to
// help guarantee uniqueness from PolicyBeacons.
pub fn to_bidder_id(bidder_cred: Credential) -> BidderId {
  when bidder_cred is {
    VerificationKeyCredential(pkh) -> {
      // Prefix it with "01".
      byte.concat(#"01", pkh) |> BidderId
    }
    ScriptCredential(sh) -> {
      // Prefix it with "01".
      byte.concat(#"01", sh) |> BidderId
    }
  }
}

// Get the inner AssetName from the BidderId.
pub fn get_bidder_id_name(bidder_id: BidderId) -> AssetName {
  bidder_id.inner
}
