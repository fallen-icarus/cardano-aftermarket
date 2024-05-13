use aiken/dict
use aiken/transaction.{Transaction,Input,Output,ScriptContext} as tx
use aiken/transaction/certificate.{CredentialRegistration}
use aiken/transaction/credential.
  { Address
  , Inline
  , ScriptCredential
  }
use aiken/transaction/value

use cardano_aftermarket/data/core.{ScriptHash}
use cardano_aftermarket/data/datums.
  { MarketDatum
  , SpotDatum
  , AuctionDatum
  , BidDatum
  }
use cardano_aftermarket/data/redeemers.{MarketRedeemer, BeaconsRedeemer, PaymentObserverRedeemer}
use cardano_aftermarket/utils.{trace_if_false}

use cardano_aftermarket/market_utxo_validation.{market_beacons_destination_check}
use cardano_aftermarket/payment_validation.{valid_payment}

// ----------------------------------------------
// Aftermarket Validator Script
// ----------------------------------------------
// This smart contract is the payment credential for all aftermarket addresses. To minimize
// redundant executions, it will delegate the checks to one of the other protocol scripts.
validator() {
  fn aftermarket_script(datum: MarketDatum, redeemer: MarketRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.CloseOrUpdateSellerUTxO -> {
        // Make changes to either a Spot UTxO or an Auction UTxO. This delegates the checks to the
        // beacon policy using CreateCloseOrUpdateMarketUTxOs, but also directly checks for the
        // seller's approval.

        // Extract the required information from the script context.
        expect ScriptContext(
          Transaction{inputs,redeemers,withdrawals,extra_signatories..}, 
          tx.Spend(out_ref)
        ) = ctx

        // Get this input being spent to extract the borrower's staking credential.
        expect Some(Input(_, Output(Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id from the datum.
        let beacon_policy_id = when datum is {
          SpotDatum{beacon_policy_id,..} -> beacon_policy_id
          AuctionDatum{beacon_policy_id,..} -> beacon_policy_id
          _ -> error @"UTxO is not a seller's UTxO"
        }

        let req_redeemer: Data = redeemers.CreateCloseOrUpdateMarketUTxOs // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

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
          Transaction{inputs,redeemers,withdrawals,extra_signatories..}, 
          tx.Spend(out_ref)
        ) = ctx

        // Get this input being spent to extract the borrower's staking credential.
        expect Some(Input(_, Output(Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id and bidder credential from the BidDatum.
        let (beacon_policy_id,bidder_credential) = when datum is {
          BidDatum{beacon_policy_id,bidder_credential,..} -> (beacon_policy_id, bidder_credential)
          _ -> error @"UTxO is not a Bid UTxO"
        }

        when value.tokens(val,beacon_policy_id) |> dict.to_list(_) is {
          [] -> {
            // The address owner has custody.
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
                  Some(Inline(bidder_credential)),
                  withdrawals,
                  extra_signatories
                )
              )
            }
          }
        }
      }

      redeemers.PurchaseSpot -> {
        // Purchase a Spot UTxO. This delegates the checks to the payment observer script.

        // Extract out the required information from the script context.
        expect ScriptContext(
          Transaction{redeemers,..},
          tx.Spend(_)
        ) = ctx

        // Get the payment_observer_hash from the SpotDatum. All valid Spot UTxOs are 
        // guaranteed to have the proper payment_observer_hash in the datum.
        let (payment_observer_hash,beacon_policy_id) = when datum is {
          SpotDatum{payment_observer_hash,beacon_policy_id,..} -> 
            (payment_observer_hash,beacon_policy_id)
          _ -> error @"UTxO is not a Spot UTxO"
        }

        let req_redeemer: Data = redeemers.ObservePayment(beacon_policy_id) // Cast to Data type.

        // The payment_observer_script must be executed with the ObservePayment redeemer. This
        // redeemer can only be used with a staking execution. The payment_observer_script only has
        // one possible redeemer that can be used.
        trace_if_false(
          @"Payment observer script not executed with proper redeemer",
          utils.script_executed_with_redeemer(redeemers, req_redeemer, payment_observer_hash)
        )
      }

      redeemers.AcceptBid -> {
        // Accept a Bid UTxO. This delegates the checks to the payment observer script, but also
        // directly checks for the seller's approval. For convenience, this redeemer can also be
        // used to close the associated Auction UTxOs.

        // Extract out the required information from the script context.
        expect ScriptContext(
          Transaction{inputs,withdrawals,extra_signatories,redeemers,..},
          tx.Spend(out_ref)
        ) = ctx

        // Get the address and value of the input being spent.
        expect Some(Input(_,Output(Address(_,stake),..))) = tx.find_input(inputs,out_ref)

        // Get the payment_observer_hash from either the BidDatum or the AuctionDatum. All valid
        // Auction UTxOs and Bid UTxOs are guaranteed to have the proper payment_observer_hash in
        // the datum.
        let (payment_observer_hash, beacon_policy_id) = when datum is {
          BidDatum{payment_observer_hash,beacon_policy_id,..} -> 
            (payment_observer_hash, beacon_policy_id)
          AuctionDatum{payment_observer_hash,beacon_policy_id,..} ->
            (payment_observer_hash, beacon_policy_id)
          _ -> error @"UTxO is not an Auction UTxO or a Bid UTxO"
        }
        
        let req_redeemer: Data = redeemers.ObservePayment(beacon_policy_id) // Cast to Data type.

        and {
          trace_if_false( 
            @"Seller did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          // The payment_observer_script must be executed with the ObserveAddressUpdate redeemer. This
          // redeemer can only be used with a staking execution. The payment_observer_script only has
          // one possible redeemer that can be used.
          trace_if_false(
            @"Payment observer script not executed with proper redeemer",
            utils.script_executed_with_redeemer(redeemers, req_redeemer, payment_observer_hash)
          )
        }
      }
    }
  }
}

// ----------------------------------------------
// Payment Observer Script
// ----------------------------------------------
// This script is used as a staking observer script to oversee all purchases.
validator(aftermarket_script_hash: ScriptHash) {
  fn payment_observer_script(redeemer: PaymentObserverRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.ObservePayment{beacon_policy_id} -> {
        // Observer payments for both spot purchases and bid acceptances. For convenience, this
        // script will also validate Auction UTxOs spent with the AcceptBid spending redeemer since
        // this may be cheaper than using CloseOrUpdateSellerUTxO.
        //
        // To figure out which action is meant for each UTxO, the inputs are compared against the
        // redeemers map. The spending redeemer used determines which action is taking place for
        // that UTxO:
        // - AcceptBid -> accepting a Bid UTxO or closing an Auction UTxO
        // - PurchaseSpot -> buying a Spot UTxO
        //
        // At a high level, every input either has required outputs and/or required beacon token
        // mints/burns. So while matching up inputs and redeemers, the smart contract will build up
        // a list of required outputs and required mints/burns. Then, it will check the outputs and
        // tx mint to see if the requirements are met for the given inputs.
        //
        // To improve performance, the required outputs are assumed to be in the same order as the 
        // inputs. The outputs do not need to be grouped together and there can be unrelated outputs
        // mixed in between the required outputs. You can think of it like this:
        // - There are two types of required outputs: spot payment output, and auction payment
        // output.
        // - These output types are treated separately.
        // - Filter all inputs for a given action. Then, filter all outputs for a specific type that
        // corresponds to that action. As long as the inputs and outputs are in the same order, the
        // transaction will succeed.
        //
        // For example, if you have this input ordering:
        // 1) spotPurchase1 input
        // 2) bidAcceptance1 input
        // 3) spotPurchase2 input
        // 4) bidAcceptance2 input
        //
        // Then these are some of the possible valid output orderings:
        // 1) bid1 payment output
        // 2) spot1 payment output
        // 3) spot2 payment output
        // 4) bid2 payment output
        //
        // or
        //
        // 1) spot1 payment output
        // 2) bid1 payment output
        // 3) bid2 payment output
        // 4) spot2 payment output
        //
        // These are the key requirements for the above example's output ordering:
        // 1) spot1 payment output MUST appear before spot1 payment output
        // 2) bid1 payment output MUST appear before bid2 payment output
        // Any ordering that satisfies these two requirements is valid.
        //
        //---------------------------------------
        // Spot Purchase Validation
        //---------------------------------------
        // In order to purchase a Spot UTxO, all of the following must be true for all
        // spot inputs:
        // 1) The input must be spent using PurchaseSpot.
        // 2) The input must have a SpotDatum.
        // 3) The input must have the required beacons.
        // 4) There must be a corresponding spot payment output with the following characteristics:
        //    a) It must locked at the payment_address from the SpotDatum.
        //    b) Contains the sale_price assets from the SpotDatum + the sale_deposit amount of ada.
        //    c) Contains the proper hard-coded inline datum: 
        //       (beacon_policy_id, spot_utxo_output_ref)
        // 5) Either all beacons attached to this input must be burned or the
        //    beacon_script must be executed using CreateCloseOrUpdateMarketUTxOs.
        //
        //---------------------------------------
        // Bid Acceptance Validation
        //---------------------------------------
        // In order to accept a Bid UTxO, all of the following must be true for all
        // bid inputs:
        // 1) The input must be spent using AcceptBid.
        // 2) The input must have a BidDatum.
        // 3) The input must have the required beacons.
        // 4) There must be a corresponding auction payment output with the following characteristics:
        //    a) It must locked at the payment_address from the BidDatum.
        //    b) Contains the NFTs from the BidDatum + the bid_deposit amount of ada.
        //    c) Contains the proper hard-coded inline datum: 
        //       (beacon_policy_id, bid_utxo_output_ref)
        // 5) Either all beacons attached to this input must be burned or the
        //    beacon_script must be executed using CreateCloseOrUpdateMarketUTxOs.
        //
        //---------------------------------------
        // Closing Auction UTxO Validation
        //---------------------------------------
        // In order to close an Auction UTxO, all of the following must be true for all
        // auction inputs:
        // 1) The input must be spent using AcceptBid.
        // 2) The input must have a AuctionDatum.
        // 3) The input must have the required beacons.
        // 4) Either all beacons attached to this input must be burned or the
        //    beacon_script must be executed using CreateCloseOrUpdateMarketUTxOs.

        // Extract the required information from the script context.
        expect ScriptContext( 
          Transaction{inputs,outputs,redeemers,mint,..}, 
          tx.WithdrawFrom(_)
        ) = ctx

        // This has its own error messages.
        valid_payment(
          inputs,
          outputs,
          redeemers,
          value.from_minted_value(mint) |> value.tokens(_, beacon_policy_id),
          core.to_beacon_id(beacon_policy_id),
          ScriptCredential(aftermarket_script_hash),
        )
      }

      redeemers.RegisterPaymentObserverScript -> {
        // Register the observer script for use as a staking script. It cannot be 
        // delegated or deregistered.
        when ctx is {
          ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
          _ -> error "This redeemer can only be used to register the payment observer script"
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
// 3) a "Bid" beacon - representing a Bid UTxO.
// 4) a Policy beacon - representing the policy id for the nfts being sold.
// 5) a BidderId beacon - representing the credential for the bidder.
validator(proxy_hash: ScriptHash, aftermarket_script_hash: ScriptHash, payment_observer_hash: ScriptHash) {
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
        // By explicitly stating what valid UTxOs with beacons are, it implicitly enforces
        // that the proper beacons are minted/burned. This design allows for closing and updating
        // Market UTxOs in the same transaction while also improving performance. This means
        // converting Market UTxOs in place is supported.
        //
        // This script will Auction UTxOs, Spot UTxOs, and Bid UTxOs separately. All checks on all
        // market UTxOs must be successfull for this redeemer to successfully execute.
        //
        //---------------------------------------
        // Universal Checks
        //---------------------------------------
        // 1) All beacons must be locked at a market address with a valid staking credential.
        // 2) All outputs with beacons must either be a Spot UTxO, Auction UTxO, or Bid UTxO.
        //
        //---------------------------------------
        // Spot UTxO Checks
        //---------------------------------------
        // 1) All Spot UTxOs must have exactly two beacons, one unit each:
        //    a) A "Spot" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        // 2) All Spot UTxOs must have the proper inline SpotDatum:
        //    a) beacon_id == this policy id
        //    b) payment_observer_hash == hard-coded payment_observer_hash
        //    c) nft_policy_id == policy id for nfts being sold in the UTxO and cannot be the beacon
        //       policy id
        //    d) nft_names == token names for nfts being sold in the UTxO and cannot be empty
        //    f) payment_address must either use a payment pubkey, or the proxy script as the
        //       payment credential and a valid staking credential
        //    g) sale_deposit > 0
        //    h) sale_price must not be empty, must not use any protocol beacons as the assets,
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
        //    b) payment_observer_hash == hard-coded payment_observer_hash
        //    c) nft_policy_id == policy id for nfts being auctioned in the UTxO
        //    d) nft_names == token names for nfts being auctioned in the UTxO
        //    f) starting_price must not be empty, must not use any protocol beacons as the assets,
        //       and all prices must be > 0
        // 3) All Auction UTxOs must have the NFTs being auctioned.
        //
        //---------------------------------------
        // Bid UTxO Checks
        //---------------------------------------
        // 1) All Bid UTxOs must have exactly three beacons, one unit each:
        //    a) A "Bid" Beacon
        //    b) The Policy Beacon for the NFTs being auctioned
        //    c) The BidderId beacon corresponding to the bidder_credential in the BidDatum
        // 2) All Bid UTxOs must have the proper inline BidDatum:
        //    a) beacon_id == this policy id
        //    b) payment_observer_hash == hard-coded payment_observer_hash
        //    c) nft_policy_id == policy id for nfts being auctioned in the UTxO
        //    d) bidder_credential == credential for the BidderId in this Bid UTxO
        //    e) nft_names == token names for nfts being auctioned in the UTxO
        //    g) payment_address must either use a payment pubkey, or the proxy script as the
        //       payment credential and a valid staking credential
        //    h) bid_deposit > 0
        //    i) bid must not be empty, must not use any protocol beacons as the assets,
        //       all prices must be > 0, and the list must be sorted
        // 3) All Bid UTxOs must have exactly the bid amount and the bid_deposit amount
        //    of ada.
        // 4) The credential used for the BidderId must approve the transaction.

        // Extract out the required information from the script context.
        let ScriptContext(Transaction{outputs,withdrawals,extra_signatories,..}, purpose) = ctx

        // This redeemer must be used with either a staking execution or a minting execution. Get
        // the beacon_id from the purpose while checking the type of execution used.
        let beacon_id = when purpose is {
          tx.Mint(beacon_id) -> beacon_id
          tx.WithdrawFrom(Inline(ScriptCredential(beacon_id))) -> beacon_id
          _ -> error @"This redeemer must be used with minting or staking execution"
        }

        market_beacons_destination_check(
          aftermarket_script_hash,
          core.to_beacon_id(beacon_id),
          proxy_hash,
          payment_observer_hash,
          outputs,
          withdrawals,
          extra_signatories
        )
      }

      redeemers.BurnBeacons -> {
        // This can only be used to burn beacons. The only time it can be used is when the payment
        // observer script is executed.

        // Get the required information from the script context. This redeemer can only be used
        // with minting executions.
        expect ScriptContext(Transaction{mint,..}, tx.Mint(beacon_id)) = ctx

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
