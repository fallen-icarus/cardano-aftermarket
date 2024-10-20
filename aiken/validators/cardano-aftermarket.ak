use aiken/dict
use aiken/transaction.{ScriptContext} as tx
use aiken/transaction/certificate.{CredentialRegistration}
use aiken/transaction/credential as cred
use aiken/transaction/value

use cardano_aftermarket/data/core.{ScriptHash}
use cardano_aftermarket/data/datums.{MarketDatum}
use cardano_aftermarket/data/redeemers.{MarketRedeemer, BeaconsRedeemer, MarketObserverRedeemer}

use cardano_aftermarket/market_utxo_validation.{market_beacons_destination_check}
use cardano_aftermarket/observer_validation.{valid_payment}
use cardano_aftermarket/utils.{trace_if_false}

// The Aftermarket is a secondary market for selling NFTs. It makes certain design choices based on
// the assumption that NFTs are unique! Therefore, using this market place with FTs can
// have result in unintended behavior.
//
// The aftermarket supports two types of trades:
// 1) Spot trades: trades that can be immediately executed and fulfilled by a buyer without any
// further input from the seller.
// 2) Auction trades: The seller puts up the NFTs and signals a starting price, and buyers can
// places bid.
//
// The auctions are further divided into two types:
// 1) Auctions where the seller finalizes the deal.
// 2) Auctions where the buyer finalizes the deal.
//
// The two types of auctions are required to support compositions with other DApps. For some DApps,
// like cardano-loans, the buyer must be able to change the lender address in the same transaction
// where the bond is purchased on the secondary market. This requires the buyer to finalize the
// deal. (The buyer already finalizes the deal with spot trades.) However, there may be other DApps
// where the seller must finalize in order to safely compose with other DApps. Therefore, support
// for both types of auctions was added. 
//
// For the seller finalized auctions, the seller must send the NFTs to the buyer's specified
// address in order to claim the bid amount.
//
// For the buyer finalized auctions, buyers do not actually store the bid amount in their Bid UTxO.
// Instead, the datum says how much they are willing to pay. The seller can accept this bid by
// evolving the Bid UTxO into an AcceptedBid UTxO with the NFTs to be claimed. The buyer has a
// certain amount of time to claim the NFTs before the seller can take them back. If the buyer backs
// out (ie, does not claim the NFTs), the deposit the buyer used for the Bid UTxO's minUTxOValue
// will be claimed by the seller. This "deposit" helps compensate the seller for any lost
// opportunity from accepting one bid over another. The buyers can store extra ADA (above the
// required minUTxOValue) to make their bid more attractive since it means the seller has less risk
// from accepting their bid over others.

// ----------------------------------------------
// Aftermarket Validator Script
// ----------------------------------------------
// This smart contract is the payment credential for all aftermarket addresses. To minimize
// redundant executions, it will delegate the checks to one of the other protocol scripts. This one
// payment script can manage the sales of all possible NFTs.
validator() {
  fn aftermarket_script(datum: MarketDatum, redeemer: MarketRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.CloseOrUpdateSellerUTxO -> {
        // Make changes to either a Spot UTxO or an Auction UTxO. This delegates the checks to the
        // beacon policy using CreateCloseOrUpdateMarketUTxOs, but also directly checks for the
        // seller's approval.

        // Extract the required information from the script context.
        expect ScriptContext(
          tx.Transaction{inputs,redeemers,withdrawals,extra_signatories..}, 
          tx.Spend(out_ref)
        ) = ctx

        // Get this input being spent to extract the borrower's staking credential.
        expect Some(tx.Input(_, tx.Output(cred.Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id from the datum.
        let beacon_policy_id = when datum is {
          datums.SpotDatum{beacon_policy_id,..} -> beacon_policy_id
          datums.AuctionDatum{beacon_policy_id,..} -> beacon_policy_id
          _ -> error @"UTxO is not a seller's UTxO"
        }

        let req_redeemer: Data = redeemers.CreateCloseOrUpdateMarketUTxOs // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          // If there are beacons present, the beacon script must be executed as either a staking
          // script or a minting policy.
          when value.tokens(val,beacon_policy_id) |> dict.to_list(_) is {
            [] -> True
            _ -> trace_if_false(
              @"Beacon script not executed with proper redeemer",
              utils.script_executed_with_redeemer(
                redeemers,
                req_redeemer,
                beacon_policy_id
              )
            )
          }
        }
      }

      redeemers.CloseOrUpdateBidderUTxO-> {
        // Make changes to a Bid UTxO. This delegates the checks to the beacon policy using
        // CreateCloseOrUpdateMarketUTxOs, but also directly checks for the bidder's approval.
        // Invalid Bid UTxOs (ie, those with a BidDatum but now beacons) belong to the address
        // owner.

        // Extract the required information from the script context.
        expect ScriptContext(
          tx.Transaction{inputs,redeemers,withdrawals,extra_signatories..}, 
          tx.Spend(out_ref)
        ) = ctx

        // Get this input being spent to extract the borrower's staking credential.
        expect Some(tx.Input(_, tx.Output(cred.Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id and bidder credential from the bid datums.
        let (beacon_policy_id,bidder_credential) = when datum is {
          datums.SpotBidDatum{beacon_policy_id,bidder_credential,..} -> (beacon_policy_id, bidder_credential)
          datums.ClaimBidDatum{beacon_policy_id,bidder_credential,..} -> (beacon_policy_id, bidder_credential)
          _ -> error @"UTxO is not a Bid UTxO"
        }

        when value.tokens(val,beacon_policy_id) |> dict.to_list(_) is {
          [] -> {
            // The address owner has custody of invalid UTxOs.
            trace_if_false( 
              @"Address owner did not approve",
              utils.staking_credential_approves(stake,withdrawals,extra_signatories)
            )
          }

          _ -> {
            // If the Bid beacon is present, the bidder has custody and the logic must be
            // delegated to the beacon script with the CreateCloseOrUpdateMarketUTxOs redeemer.
            let req_redeemer: Data = redeemers.CreateCloseOrUpdateMarketUTxOs // Cast to Data type.

            and{
              trace_if_false(
                @"Beacon script not executed with proper redeemer",
                utils.script_executed_with_redeemer(
                  redeemers,
                  req_redeemer,
                  beacon_policy_id
                )
              ),
              trace_if_false( 
                @"Bidder did not approve",
                utils.staking_credential_approves(
                  Some(cred.Inline(bidder_credential)),
                  withdrawals,
                  extra_signatories
                )
              )
            }
          }
        }
      }

      redeemers.PurchaseSpot -> {
        // Purchase a Spot UTxO. This delegates the checks to the aftermarket observer script.

        // Extract out the required information from the script context.
        expect ScriptContext(
          tx.Transaction{redeemers,..},
          tx.Spend(_)
        ) = ctx

        // Get the aftermarket_observer_hash from the SpotDatum. All valid Spot UTxOs are 
        // guaranteed to have the proper aftermarket_observer_hash in the datum.
        let (aftermarket_observer_hash,beacon_policy_id) = when datum is {
          datums.SpotDatum{aftermarket_observer_hash,beacon_policy_id,..} -> 
            (aftermarket_observer_hash,beacon_policy_id)
          _ -> error @"UTxO is not a Spot UTxO"
        }

        let req_redeemer: Data = redeemers.ObserveAftermarket(beacon_policy_id) // Cast to Data type.

        // The aftermarket_observer_script must be executed with the ObserveAftermarket redeemer. This
        // redeemer can only be used with a staking execution. The aftermarket_observer_script only has
        // one possible redeemer that can be used.
        trace_if_false(
          @"Aftermarket observer script not executed with proper redeemer",
          utils.script_executed_with_redeemer(redeemers, req_redeemer, aftermarket_observer_hash)
        )
      }

      redeemers.AcceptSpotBid -> {
        // Accept a Spot Bid UTxO. This delegates the checks to the aftermarket observer script, but
        // also directly checks for the seller's approval. For convenience, this redeemer can also
        // be used to close the associated Auction UTxOs.

        // Extract out the required information from the script context.
        expect ScriptContext(
          tx.Transaction{inputs,withdrawals,extra_signatories,redeemers,..},
          tx.Spend(out_ref)
        ) = ctx

        // Get the address of the input being spent.
        expect Some(tx.Input(_,tx.Output(cred.Address(_,stake),..))) = tx.find_input(inputs,out_ref)

        // Get the aftermarket_observer_hash from either the SpotBidDatum or the AuctionDatum. All
        // valid Auction UTxOs and SpotBid UTxOs are guaranteed to have the proper
        // aftermarket_observer_hash in the datum.
        let (aftermarket_observer_hash, beacon_policy_id) = when datum is {
          datums.SpotBidDatum{aftermarket_observer_hash,beacon_policy_id,..} -> 
            (aftermarket_observer_hash, beacon_policy_id)
          datums.AuctionDatum{aftermarket_observer_hash,beacon_policy_id,..} ->
            (aftermarket_observer_hash, beacon_policy_id)
          _ -> error @"UTxO is not an Auction UTxO or a SpotBid UTxO"
        }
        
        let req_redeemer: Data = redeemers.ObserveAftermarket(beacon_policy_id) // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          // The aftermarket_observer_script must be executed with the ObserveAftermarket redeemer. This
          // redeemer can only be used with a staking execution. The aftermarket_observer_script only has
          // one possible redeemer that can be used.
          trace_if_false(
            @"Aftermarket observer script not executed with proper redeemer",
            utils.script_executed_with_redeemer(redeemers, req_redeemer, aftermarket_observer_hash)
          )
        }
      }

      redeemers.AcceptClaimBid{..} -> {
        // Accept a ClaimBid UTxO. This delegates the checks to the aftermarket observer script, but
        // also directly checks for the seller's approval. For convenience, this redeemer can also
        // be used to close the associated Auction UTxOs.

        // Extract out the required information from the script context.
        expect ScriptContext(
          tx.Transaction{inputs,withdrawals,extra_signatories,redeemers,..},
          tx.Spend(out_ref)
        ) = ctx

        // Get the address and value of the input being spent.
        expect Some(tx.Input(_,tx.Output(cred.Address(_,stake),..))) = tx.find_input(inputs,out_ref)

        // Get the aftermarket_observer_hash from either the BidDatum or the AuctionDatum. All valid
        // Auction UTxOs and ClaimBid UTxOs are guaranteed to have the proper
        // aftermarket_observer_hash in the datum.
        let (aftermarket_observer_hash, beacon_policy_id) = when datum is {
          datums.ClaimBidDatum{aftermarket_observer_hash,beacon_policy_id,..} -> 
            (aftermarket_observer_hash, beacon_policy_id)
          datums.AuctionDatum{aftermarket_observer_hash,beacon_policy_id,..} ->
            (aftermarket_observer_hash, beacon_policy_id)
          _ -> error @"UTxO is not an Auction UTxO or a ClaimBid UTxO"
        }
        
        let req_redeemer: Data = redeemers.ObserveAftermarket(beacon_policy_id) // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          // The aftermarket_observer_script must be executed with the ObserveAftermarket redeemer. This
          // redeemer can only be used with a staking execution. The aftermarket_observer_script only has
          // one possible redeemer that can be used.
          trace_if_false(
            @"Aftermarket observer script not executed with proper redeemer",
            utils.script_executed_with_redeemer(redeemers, req_redeemer, aftermarket_observer_hash)
          )
        }
      }

      redeemers.ClaimAcceptedBid -> {
        // Claim an AcceptedBid UTxO. This delegates the checks to the aftermarket observer script,
        // but also directly checks for the seller's approval. For convenience, this redeemer can
        // also be used to close the associated Auction UTxOs.

        // Extract out the required information from the script context.
        expect ScriptContext(
          tx.Transaction{redeemers,..},
          tx.Spend(_)
        ) = ctx

        // Get the aftermarket_observer_hash from the AcceptedBidDatum. All valid AcceptedBid UTxOs
        // are guaranteed to have the proper aftermarket_observer_hash in the datum.
        let (aftermarket_observer_hash, beacon_policy_id) = when datum is {
          datums.AcceptedBidDatum{aftermarket_observer_hash,beacon_policy_id,..} -> 
            (aftermarket_observer_hash, beacon_policy_id)
          _ -> error @"UTxO is not an AcceptedBidDatum"
        }
        
        let req_redeemer: Data = redeemers.ObserveAftermarket(beacon_policy_id) // Cast to Data type.

        // The aftermarket_observer_script must be executed with the ObserveAftermarket redeemer. This
        // redeemer can only be used with a staking execution. The aftermarket_observer_script only has
        // one possible redeemer that can be used.
        trace_if_false(
          @"Aftermarket observer script not executed with proper redeemer",
          utils.script_executed_with_redeemer(redeemers, req_redeemer, aftermarket_observer_hash)
        )
      }

      redeemers.UnlockUnclaimedAcceptedBid -> {
        // Unlock an AcceptedBid UTxO whose claim period has passed. This delegates the checks to
        // the aftermarket observer script, but also directly checks for the seller's approval. 

        // Extract out the required information from the script context.
        expect ScriptContext(
          tx.Transaction{inputs,withdrawals,extra_signatories,redeemers,..},
          tx.Spend(out_ref)
        ) = ctx

        // Get the address and value of the input being spent.
        expect Some(tx.Input(_, tx.Output(cred.Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the aftermarket_observer_hash from the AcceptedBidDatum. All valid AcceptedBid UTxOs
        // are guaranteed to have the proper aftermarket_observer_hash in the datum.
        let (aftermarket_observer_hash, beacon_policy_id) = when datum is {
          datums.AcceptedBidDatum{aftermarket_observer_hash,beacon_policy_id,..} -> 
            (aftermarket_observer_hash, beacon_policy_id)
          _ -> error @"UTxO is not an AcceptedBidDatum"
        }
        
        let req_redeemer: Data = redeemers.ObserveAftermarket(beacon_policy_id) // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          when value.tokens(val,beacon_policy_id) |> dict.to_list(_) is {
            [] -> True
            _ -> 
              // The aftermarket_observer_script must be executed with the ObserveAftermarket redeemer. This
              // redeemer can only be used with a staking execution. The aftermarket_observer_script only has
              // one possible redeemer that can be used.
              trace_if_false(
                @"Aftermarket observer script not executed with proper redeemer",
                utils.script_executed_with_redeemer(redeemers, req_redeemer, aftermarket_observer_hash)
              )
          }
        }
      }
    }
  }
}

// ----------------------------------------------
// Aftermarket Observer Script
// ----------------------------------------------
// This script is used as a staking observer script to oversee all purchases, and acceptances.
// NOTE: This could possibly be broken up to decrease fees since reference script sizes impact the
// calculated fee. This could decrease the fee whenever only one action is required, but could
// increase the fee if all actions are required.
//
// It must ensure:
// 1) Buyers give the sellers the sale price + the seller's deposit back for spot purchases. 
// 2) Sellers give the buyer the required NFTs + the buyer's deposit back for SpotBids. 
// 3) Sellers properly create an AcceptedBid UTxO with the NFTs + seller's deposit + buyer's 
// deposit from the associated ClaimBid UTxO.
// 4) Buyers give the seller the bid amount + the seller's deposit for AcceptedBid UTxOs.
// 5) Unnecessary AcceptedBid UTxOs are *never* created. This prevents the seller from being able to
// mint BidderIds for buyers.
// 6) If the aftermarket beacon policy is executed with BurnBeacons, then the proper amount of
// beacons must be burned. If the beacon policy is executed with CreateCloseOrUpdateMarketUTxOs,
// then the actual beacons minted/burned are left to the beacon policy to enforce. Since this script
// always checks that no excess AcceptedBid UTxOs are ever created, the beacon policy cannot
// accidentally allow the minting of BidderIds without the consent from the buyer.
validator(proxy_hash: ScriptHash, aftermarket_script_hash: ScriptHash) {
  fn aftermarket_observer_script(redeemer: MarketObserverRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.ObserveAftermarket{beacon_policy_id} -> {
        // Extract the required information from the script context.
        expect ScriptContext( 
          tx.Transaction{inputs,outputs,redeemers,mint,validity_range,withdrawals,extra_signatories,..}, 
          tx.WithdrawFrom(_)
        ) = ctx

        // This has its own error messages.
        valid_payment(
          inputs,
          outputs,
          redeemers,
          value.from_minted_value(mint) |> value.tokens(_, beacon_policy_id),
          beacon_policy_id,
          cred.ScriptCredential(aftermarket_script_hash),
          validity_range,
          proxy_hash,
          withdrawals,
          extra_signatories
        )
      }

      redeemers.RegisterAftermarketObserverScript -> {
        // Register the observer script for use as a staking script. It cannot be 
        // delegated or deregistered.
        when ctx is {
          ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
          _ -> error "This redeemer can only be used to register the aftermarket observer script"
        }
      }
    }
  }
}

// ----------------------------------------------
// Aftermarket Beacon Script
// ----------------------------------------------
// There are five possible beacons:
// 1) a "Spot" beacon - representing a Spot UTxO.
// 2) a "Auction" beacon - representing an Auction UTxO.
// 3) a "Bid" beacon - representing a Bid UTxO. This beacon is used for all Bid UTxO types.
// 4) a Policy beacon - representing the policy id for the nfts being sold.
// 5) a BidderId beacon - representing the credential for the bidder.
validator(
  proxy_hash: ScriptHash, 
  aftermarket_script_hash: ScriptHash, 
  aftermarket_observer_hash: ScriptHash
) {
  fn beacon_script(redeemer: BeaconsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.CreateCloseOrUpdateMarketUTxOs -> {
        // Create any kind of market UTxO. This all-in-one redeemer allows composing actions on
        // market UTxOs. For example, a bidder on an auction can also create a new Spot UTxO in the
        // same transaction.
        //
        // This redeemer can also be used with a staking execution in case market outputs need to 
        // be checked without minting/burning beacons.
        //
        // By explicitly stating what valid UTxOs with beacons are, it implicitly enforces that the
        // proper beacons are minted/burned. This design allows for closing and updating Market
        // UTxOs in the same transaction while also improving performance. This means converting
        // Market UTxOs in place is supported. The one case that makes this difficult is with
        // AcceptedBid UTxOs; they are created by the seller, but include BidderId beacons. If the
        // seller could mint BidderId beacons for buyers, querying the buyer's UTxOs could be DOS
        // attacked. To prevent the seller from minting BidderId's, this script will require the
        // aftermarket_observer_script to be executed in any transaction where AcceptedBid outputs
        // are created. The observer script will check that no excess AcceptedBid UTxOs are created
        // (ie, they all have a corresponding Bid UTxO).
        //
        // This script will check Auction UTxOs, Spot UTxOs, and Bid UTxOs separately. All checks 
        // on all market UTxOs must be successfull for this redeemer to successfully execute.
        //
        //---------------------------------------
        // Universal Checks
        //---------------------------------------
        // 1) All beacons must be locked at a market address with a valid staking credential.
        // 2) All outputs with beacons must either be a Spot UTxO, Auction UTxO,
        //    a SpotBid UTxO, or a ClaimBid UTxO. AcceptedBid UTxOs can be among the outputs, but
        //    only if the observer script is executed in the same transaction. The observer script
        //    checks that bidder ids are not improperly minted.
        //
        //---------------------------------------
        // Spot UTxO Checks
        //---------------------------------------
        // 1) All Spot UTxOs must have exactly two beacons, one unit each:
        //    a) A "Spot" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        // 2) All Spot UTxOs must have the proper inline SpotDatum:
        //    a) beacon_id == this policy id
        //    b) aftermarket_observer_hash == hard-coded aftermarket_observer_hash
        //    c) nft_policy_id == policy id for nfts being sold in the UTxO and cannot be the beacon
        //       policy id
        //    d) nft_names == token names for nfts being sold in the UTxO and cannot be empty
        //    e) payment_address must either use a payment pubkey, or the proxy script as the
        //       payment credential and a valid staking credential
        //    f) sale_deposit > 0
        //    g) sale_price must not be empty, must not use any protocol beacons as the assets,
        //       all prices must be > 0, and the list must be sorted.
        // 3) All Spot UTxOs must have the NFTs being sold and the sale_deposit amount
        //    of ada.
        //
        //---------------------------------------
        // Auction UTxO Checks
        //---------------------------------------
        // 1) All Auction UTxOs must have exactly two beacons, one unit each:
        //    a) An "Auction" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        // 2) All Auction UTxOs must have the proper inline AuctionDatum:
        //    a) beacon_id == this policy id
        //    b) aftermarket_observer_hash == hard-coded aftermarket_observer_hash
        //    c) nft_policy_id == policy id for nfts being auctioned in the UTxO
        //    d) nft_names == token names for nfts being auctioned in the UTxO
        //    e) starting_price must not be empty, must not use any protocol beacons as the assets,
        //       and all prices must be > 0
        // 3) All Auction UTxOs must have the NFTs being auctioned.
        //
        //---------------------------------------
        // SpotBid UTxO Checks - a seller finalized auction
        //---------------------------------------
        // 1) All SpotBid UTxOs must have exactly three beacons, one unit each:
        //    a) A "Bid" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        //    c) The BidderId beacon corresponding to the bidder_credential in the BidDatum
        // 2) All SpotBid UTxOs must have the proper inline SpotBidDatum:
        //    a) beacon_id == this policy id
        //    b) aftermarket_observer_hash == hard-coded aftermarket_observer_hash
        //    c) nft_policy_id == policy id for nfts being auctioned in the UTxO
        //    d) bidder_credential == credential for the BidderId in this Bid UTxO
        //    e) nft_names == token names for nfts being auctioned in the UTxO
        //    f) payment_address must either use a payment pubkey, or the proxy script as the
        //       payment credential and a valid staking credential
        //    g) bid_deposit > 0
        //    h) bid must not be empty, must not use any protocol beacons as the assets,
        //       all prices must be > 0, and the list must be sorted
        // 3) All SpotBid UTxOs must have exactly the bid amount and the bid_deposit amount
        //    of ada.
        // 4) The credential used for the BidderId must approve the transaction.
        //
        //---------------------------------------
        // ClaimBid UTxO Checks - a buyer finalized auction
        //---------------------------------------
        // 1) All ClaimBid UTxOs must have exactly three beacons, one unit each:
        //    a) A "Bid" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        //    c) The BidderId beacon corresponding to the bidder_credential in the BidDatum
        // 2) All ClaimBid UTxOs must have the proper inline ClaimBidDatum:
        //    a) beacon_id == this policy id
        //    b) aftermarket_observer_hash == hard-coded aftermarket_observer_hash
        //    c) nft_policy_id == policy id for nfts being auctioned in the UTxO
        //    d) bidder_credential == credential for the BidderId in this Bid UTxO
        //    e) nft_names == token names for nfts being auctioned in the UTxO
        //    f) bid_deposit > 0
        //    g) bid must not be empty, must not use any protocol beacons as the assets,
        //       all prices must be > 0, and the list must be sorted
        //    h) bid_expiration must be <= invalid_hereafter
        //    i) claim_expiration must be <= invalid_hereafter
        // 3) All ClaimBid UTxOs must have exactly the bid_deposit amount
        //    of ada. The bid should not be included.
        // 4) The credential used for the BidderId must approve the transaction.

        // Extract out the required information from the script context.
        let ScriptContext(
          tx.Transaction{outputs,withdrawals,extra_signatories,validity_range,..}, 
          purpose
        ) = ctx

        // This redeemer must be used with either a staking execution or a minting execution. Get
        // the beacon_id from the purpose while checking the type of execution used.
        let beacon_id = when purpose is {
          tx.Mint(beacon_id) -> beacon_id
          tx.WithdrawFrom(cred.Inline(cred.ScriptCredential(beacon_id))) -> beacon_id
          _ -> error @"This redeemer must be used with minting or staking execution"
        }

        // Check the newly created UTxOs.
        market_beacons_destination_check(
          aftermarket_script_hash,
          core.to_beacon_id(beacon_id),
          proxy_hash,
          aftermarket_observer_hash,
          outputs,
          withdrawals,
          extra_signatories,
          utils.get_upper_bound(validity_range)
        )
      }

      redeemers.BurnBeacons -> {
        // This can only be used to burn beacons. The only time it can be used is when the payment
        // observer script is executed.

        // Get the required information from the script context. This redeemer can only be used
        // with minting executions.
        expect ScriptContext(tx.Transaction{mint,..}, tx.Mint(beacon_id)) = ctx

        // Check that beacons are only burned.
        trace_if_false( 
          @"This redeemer can only be used to burn beacons",
          dict.foldl( 
            value.from_minted_value(mint) |> value.tokens(_,beacon_id),
            True, 
            fn(_,v,r) {r && v < 0} // Burn check.
          )
        )
      }

      redeemers.RegisterBeaconsScript -> {
        // Register the script for use as a staking script. It cannot be delegated or deregistered.
        when ctx is {
          ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
          _ -> error "This redeemer can only be used to register the market beacon script"
        }
      }
    }
  }
}
