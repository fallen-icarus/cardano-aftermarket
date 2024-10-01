# Benchmarks (YMMV)

All benchmarks were done using reference scripts and the cardano-node emulator
([github](https://github.com/IntersectMBO/cardano-node-emulator)). The scripts for this protocol are
too large to be used locally so it is required that reference scripts are used. The emulator uses
the same parameters as the mainnet.

> [!WARNING]
> The transaction fee and collateral estimations are not accuration since the tests do not account
> for the reference script sizes in the calculations. However, the maximum number of UTxOs that can be
> processed in each scenario *is* accurate.

## Table of Contents 
- [Required Deposits for Reference Scripts](#required-deposits-for-reference-scripts)
- [Creating Spot UTxOs](#creating-spot-utxos)
- [Closing Spot UTxOs](#closing-spot-utxos)
- [Updating Spot UTxOs](#updating-spot-utxos)
- [Purchasing Spot UTxOs](#purchasing-spot-utxos)
- [Creating Auction UTxOs](#creating-auction-utxos)
- [Closing Auction UTxOs](#closing-auction-utxos)
- [Updating Auction UTxOs](#updating-auction-utxos)
- [Creating SpotBid UTxOs](#creating-spotbid-utxos)
- [Closing SpotBid UTxOs](#closing-spotbid-utxos)
- [Updating SpotBid UTxOs](#updating-spotbid-utxos)
- [Accepting SpotBid UTxOs](#accepting-spotbid-utxos)
- [Creating ClaimBid UTxOs](#creating-claimbid-utxos)
- [Closing ClaimBid UTxOs](#closing-claimbid-utxos)
- [Updating ClaimBid UTxOs](#updating-claimbid-utxos)
- [Accepting ClaimBid UTxOs](#accepting-claimbid-utxos)
- [Claiming AcceptedBid UTxOs](#claiming-acceptedbid-utxos)
- [Unlocking Unclaimed AcceptedBid UTxOs](#unlocking-unclaimed-acceptedbid-utxos)

## Required Deposits for Reference Scripts

| Script | Deposit |
|:------:|:-------:|
| aftermarket spending script | 20 ADA |
| beacon script | 35 ADA |
| aftermarket observer script | 43 ADA |

## Creating Spot UTxOs

#### Each Spot UTxO has three NFTs for sale.
| Spots Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.236636 ADA | 0.354954 ADA |
| 5 | 0.426404 ADA | 0.639606 ADA |
| 10 | 0.664054 ADA | 0.996081 ADA |
| 15 | 0.901704 ADA | 1.352556 ADA |
| 20 | 1.139354 ADA | 1.709031 ADA |
| 25 | 1.377136 ADA | 2.065704 ADA |
| 30 | 1.614787 ADA | 2.422181 ADA |
| 32 | 1.722147 ADA | 2.583221 ADA |

Max: 32 Spots  
Bottleneck: Tx Size

## Closing Spot UTxOs

#### Each Spot UTxO has three NFTs for sale.
| Spots Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.204058 ADA | 0.306087 ADA |
| 5 | 0.294361 ADA | 0.441542 ADA |
| 10 | 0.450584 ADA | 0.675876 ADA |
| 15 | 0.652118 ADA | 0.978177 ADA |
| 20 | 0.903370 ADA | 1.355055 ADA |
| 25 | 1.202709 ADA | 1.804064 ADA |
| 30 | 1.549914 ADA | 2.324871 ADA |
| 31 | 1.625099 ADA | 2.437649 ADA |

Max: 32 Spots  
Bottleneck: Memory

## Updating Spot UTxOs

#### Each Spot UTxO has three NFTs for sale. No beacons need to be changed.
| Spots Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.253392 ADA | 0.380088 ADA |
| 5 | 0.527801 ADA | 0.791702 ADA |
| 10 | 0.914332 ADA | 1.371498 ADA |
| 15 | 1.345055 ADA | 2.017583 ADA |
| 20 | 1.828274 ADA | 2.742411 ADA |
| 21 | 1.930661 ADA | 2.895992 ADA |

Max: 21 Spots  
Bottleneck: Memory

#### Each Spot UTxO has three NFTs for sale. The Spot UTxOs are converted to Auction UTxOs.
| Spots Converted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.385531 ADA | 0.578297 ADA |
| 5 | 0.614590 ADA | 0.921885 ADA |
| 10 | 0.944213 ADA | 1.416320 ADA |
| 15 | 1.321703 ADA | 1.982555 ADA |
| 20 | 1.763912 ADA | 2.645868 ADA |
| 23 | 2.152565 ADA | 3.228848 ADA |

Max: 23 Spots  
Bottleneck: Memory

## Purchasing Spot UTxOs

#### Each Spot UTxO has three NFTs for sale, and three assets are used for the sale price.
| Spots Purchased | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.258371 ADA | 0.387557 ADA |
| 5 | 0.473405 ADA | 0.710108 ADA |
| 10 | 0.779456 ADA | 1.169184 ADA |
| 15 | 1.126814 ADA | 1.690221 ADA |
| 20 | 1.515215 ADA | 2.272823 ADA |
| 22 | 1.682365 ADA | 2.523548 ADA |

Max: 22 Spots  
Bottleneck: Memory

## Creating Auction UTxOs

#### Each Auction UTxO has three NFTs for sale.
| Auctions Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.227490 ADA | 0.341235 ADA |
| 5 | 0.380150 ADA | 0.570225 ADA |
| 10 | 0.571415 ADA | 0.857123 ADA |
| 15 | 0.762679 ADA | 1.144019 ADA |
| 20 | 0.953944 ADA | 1.430916 ADA |
| 25 | 1.145340 ADA | 1.718010 ADA |
| 30 | 1.336605 ADA | 2.004908 ADA |
| 35 | 1.528574 ADA | 2.292861 ADA |

Max: 35 Auctions  
Bottleneck: Tx Size

## Closing Auction UTxOs

#### Each Auction UTxO has three NFTs for sale.
| Auctions Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.202912 ADA | 0.304368 ADA |
| 5 | 0.287751 ADA | 0.431627 ADA |
| 10 | 0.436616 ADA | 0.654924 ADA |
| 15 | 0.634096 ADA | 0.951144 ADA |
| 20 | 0.877547 ADA | 1.316321 ADA |
| 25 | 1.170496 ADA | 1.755744 ADA |
| 30 | 1.511312 ADA | 2.266968 ADA |
| 32 | 1.661041 ADA | 2.491562 ADA |

Max: 32 Auctions  
Bottleneck: Memory

## Updating Auction UTxOs

#### Each Auction UTxO has three NFTs for sale. No beacons need to be changed.
| Auctions Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.238713 ADA | 0.358070 ADA |
| 5 | 0.453700 ADA | 0.680550 ADA |
| 10 | 0.764897 ADA | 1.147346 ADA |
| 15 | 1.125545 ADA | 1.688318 ADA |
| 20 | 1.529841 ADA | 2.294762 ADA |
| 24 | 1.987749 ADA | 2.981624 ADA |

Max: 24 Auctions  
Bottleneck: Memory

#### Each Auction UTxO has three NFTs for sale. The Auction UTxOs are converted to Spot UTxOs.
| Auctions Converted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.393530 ADA | 0.590295 ADA |
| 5 | 0.654586 ADA | 0.981879 ADA |
| 10 | 1.024205 ADA | 1.536308 ADA |
| 15 | 1.449623 ADA | 2.174435 ADA |
| 20 | 1.925830 ADA | 2.888745 ADA |
| 22 | 2.129716 ADA | 3.194574 ADA |

Max: 22 Auctions  
Bottleneck: Memory

## Creating SpotBid UTxOs

#### Each SpotBid UTxO uses three assets for the bid.
| Bids Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.280369 ADA | 0.420554 ADA |
| 5 | 0.616470 ADA | 0.924705 ADA |
| 10 | 1.036926 ADA | 1.555389 ADA |
| 15 | 1.457382 ADA | 2.186073 ADA |
| 20 | 1.878014 ADA | 2.817021 ADA |
| 21 | 1.970657 ADA | 2.955986 ADA |

Max: 21 Bids  
Bottleneck: Memory

## Closing SpotBid UTxOs

#### Each SpotBid UTxO uses three assets for the bid.
| Bids Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.206458 ADA | 0.309687 ADA |
| 5 | 0.301127 ADA | 0.451691 ADA |
| 10 | 0.462752 ADA | 0.694128 ADA |
| 15 | 0.672200 ADA | 1.008300 ADA |
| 20 | 0.929910 ADA | 1.394865 ADA |
| 25 | 1.236015 ADA | 1.854023 ADA |
| 30 | 1.587071 ADA | 2.380607 ADA |
| 31 | 1.663468 ADA | 2.495202 ADA |

Max: 31 Bids  
Bottleneck: Memory

## Updating SpotBid UTxOs

#### Each SpotBid UTxO uses three assets for the bid. No beacons need to be changed.
| Bids Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.290902 ADA | 0.436353 ADA |
| 5 | 0.715437 ADA | 1.073156 ADA |
| 10 | 1.289516 ADA | 1.934274 ADA |
| 14 | 1.783243 ADA | 2.674865 ADA |

Max: 14 Bids  
Bottleneck: Memory

#### Each SpotBid UTxO uses three assets for the bid. The Bids are changed to a new BidderId.
| Bids Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.296901 ADA | 0.445352 ADA |
| 5 | 0.713880 ADA | 1.070820 ADA |
| 10 | 1.278513 ADA | 1.917770 ADA |
| 14 | 1.764683 ADA | 2.647025 ADA |

Max: 14 Bids  
Bottleneck: Memory

## Accepting SpotBid UTxOs

If the associated Auction UTxOs are closed separately, throughtput could double.

#### Each SpotBid UTxO uses three assets for the bid. The associated Auction UTxOs were closed in the same transaction.
| Bids Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.293522 ADA | 0.440283 ADA |
| 2 | 0.394616 ADA | 0.591924 ADA |
| 3 | 0.503474 ADA | 0.755211 ADA |
| 4 | 0.620186 ADA | 0.930279 ADA |
| 5 | 0.744750 ADA | 1.117125 ADA |
| 6 | 0.877168 ADA | 1.315752 ADA |
| 7 | 1.017438 ADA | 1.526157 ADA |
| 8 | 1.165561 ADA | 1.748342 ADA |
| 9 | 1.321537 ADA | 1.982306 ADA |
| 10 | 1.485366 ADA | 2.228049 ADA |

Max: 10 Bids  
Bottleneck: Memory

## Creating ClaimBid UTxOs

#### Each ClaimBid UTxO uses three assets for the bid.
| Bids Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.257597 ADA | 0.386396 ADA |
| 5 | 0.521586 ADA | 0.782379 ADA |
| 10 | 0.851737 ADA | 1.277606 ADA |
| 15 | 1.181888 ADA | 1.772832 ADA |
| 20 | 1.512039 ADA | 2.268059 ADA |
| 25 | 1.842542 ADA | 2.763813 ADA |
| 27 | 1.974603 ADA | 2.961905 ADA |

Max: 27 Bids  
Bottleneck: Memory

## Closing Claim UTxOs

#### Each Claim UTxO uses three assets for the bid.
| Bids Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.206458 ADA | 0.309687 ADA |
| 5 | 0.301127 ADA | 0.451691 ADA |
| 10 | 0.462752 ADA | 0.694128 ADA |
| 15 | 0.672200 ADA | 1.008300 ADA |
| 20 | 0.929910 ADA | 1.394865 ADA |
| 25 | 1.236015 ADA | 1.854023 ADA |
| 30 | 1.587071 ADA | 2.380607 ADA |
| 31 | 1.663468 ADA | 2.495202 ADA |

Max: 31 Bids  
Bottleneck: Memory

## Updating ClaimBid UTxOs

#### Each ClaimBid UTxO uses three assets for the bid. No beacons need to be changed.
| Bids Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.290902 ADA | 0.436353 ADA |
| 5 | 0.715437 ADA | 1.073156 ADA |
| 10 | 1.289516 ADA | 1.934274 ADA |
| 15 | 1.634585 ADA | 2.451878 ADA |
| 16 | 1.746029 ADA | 2.619044 ADA |

Max: 16 Bids  
Bottleneck: Memory

#### Each ClaimBid UTxO uses three assets for the bid. The Bids are changed to a new BidderId.
| Bids Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.296901 ADA | 0.445352 ADA |
| 5 | 0.713880 ADA | 1.070820 ADA |
| 15 | 1.278513 ADA | 1.917770 ADA |
| 16 | 1.756974 ADA | 2.635461 ADA |

Max: 16 Bids  
Bottleneck: Memory

## Accepting ClaimBid UTxOs

If the associated Auction UTxOs are closed separately, throughtput could double.

#### Each ClaimBid UTxO uses three assets for the bid. The associated Auction UTxOs were closed in the same transaction.
| Bids Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.293522 ADA | 0.440283 ADA |
| 2 | 0.394616 ADA | 0.591924 ADA |
| 3 | 0.503474 ADA | 0.755211 ADA |
| 4 | 0.620186 ADA | 0.930279 ADA |
| 5 | 0.744750 ADA | 1.117125 ADA |
| 6 | 0.877168 ADA | 1.315752 ADA |
| 7 | 1.017438 ADA | 1.526157 ADA |
| 8 | 1.165561 ADA | 1.748342 ADA |
| 9 | 1.321537 ADA | 1.982306 ADA |
| 10 | 1.485366 ADA | 2.228049 ADA |
| 11 | 1.837718 ADA | 2.756577 ADA |

Max: 11 Bids  
Bottleneck: Memory

## Claiming AcceptedBid UTxOs

#### Each AcceptedBid UTxO uses three assets for the bid.
| Bids Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.280867 ADA | 0.421301 ADA |
| 5 | 0.546612 ADA | 0.819918 ADA |
| 10 | 0.915911 ADA | 1.373867 ADA |
| 15 | 1.326208 ADA | 1.989312 ADA |
| 19 | 1.684358 ADA | 2.526537 ADA |

Max: 19 Bids  
Bottleneck: Memory

## Unlocking Unclaimed AcceptedBid UTxOs

#### Each AcceptedBid UTxO uses three assets for the bid.
| Bids Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.246290 ADA | 0.369435 ADA |
| 5 | 0.431081 ADA | 0.646622 ADA |
| 10 | 0.705150 ADA | 1.057725 ADA |
| 15 | 1.027086 ADA | 1.540629 ADA |
| 20 | 1.396888 ADA | 2.095332 ADA |
| 21 | 1.474370 ADA | 2.211555 ADA |

Max: 21 Bids  
Bottleneck: Memory
