# Ipncore

## Installation

```bash
apt-get install erlang erlang-src elixir
```

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `ipncore` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ipncore, "~> 0.1.0"}
  ]
end
```
## Work status
|Status|Description|
|-|-|
|✅|Done|
|❌|Pending|
|⚒️|In progress|
|🐞|Debug|

## Assets
It is the set of objects that is part of the Blockchain

|Asset|Description|
|-|-|
|```Tokens```|Payment currencies exchangeable in the Blockchain ecosystem|
|```Validators```|These are the nodes in charge of performing the Proof of Validation (PoV) applied to all events of the block that are processed on the platform|
|```Domains```|Identification record that can be used to shorten an address to receive payments, navigate to a social profile or website|

## Events
Events are requests made by users and automatic operations by the system that affect the Blockchain.

|Code|Event|Description|Status|
|-|-|-|-|
|100|```validator.new```|Create a new validator|✅|
|101|```validator.update```|Update a validator|✅|
|102|```validator.delete```|Delete a validator|✅|
|200|```token.new```|Create a new token|✅|
|201|```token.update```|Update a token|✅|
|202|```token.delete```|Delete a token|✅|
|210|```tx.coinbase```|Creation of coins by the token owner|✅|
|211|```tx.send```|Send a simple payment|✅|
|212|```tx.sendmulti```|Send payment to multiple recipients|❌|
|213|```tx.refund```|Return a transaction without paying a fee|❌|
|214|```tx.jackpot```|Jackpot event|❌|
|215|```tx.reward```|Reward core validators|❌|
|216|```tx.burn```|Coin destruction|❌|
|250|```balance.lock```|balance lock|✅|
|400|```domain.new```|Register a new domain|✅|
|401|```domain.update```|Update a domain|✅|
|402|```domain.delete```|Delete a domain|✅|
|403|```domain.renew```|Renew a domain|❌|
|410|```dns.set```|Create or replace a DNS Record|✅|
|411|```dns.push```|Put a DNS Record|✅|
|412|```dns.drop```|Drop a DNS Record|✅|
|1000|```pubkey.new```|Public key registration|✅|

## Network server

|Name|Protocol|Port|Status|
|-|-|-|-|
|Explorer API|HTTP|80, 443|✅
|RealTime API|WebSocket|80, 443|❌|
|DNS Service|DNS|53|⚒️ 80%|
|PubSub Service|IMP|8484|⚒️ 70%|

## Explorer API

|Method|Route|Description|Status|
|-|-|-|-|
|```GET```|```/blockchain/blocks```|List of blocks|✅
|```GET```|```/blockchain/block/:hash16```|Details of the block|✅
|```GET```|```/blockchain/block/height/:height```|Details of the block queried by height|✅
|```GET```|```/blockchain/events```|List of events|✅
|```GET```|```/blockchain/event/:hash16```|Event detail|✅
|```GET```|```/blockchain/txs```|List of transactions|✅
|```GET```|```/blockchain/tx/:hash16```|Details transaction queried by hash|✅
|```GET```|```/txo```|List of output transactions|✅
|```GET```|```/blockchain/tokens```|List of tokens|✅
|```GET```|```/blockchain/token/:token/:channel```|Token details|✅
|```GET```|```/blockchain/validators```|List of validators|✅
|```GET```|```/blockchain/validators/:hostname/:channel```|Validator details|✅
|```GET```|```/blockchain/balance/:address58```|Balance of an address|✅
|```GET```|```/blockchain/activity/:address58```|List of events of an address|✅
<!-- |```GET```|```/blockchain/channel```|List of channel|❌ -->
<!-- |```GET```|```/blockchain/channel/:channel_id```|Details of a channel|❌ -->
|```GET```|```/blockchain/status```|Blockchain status|✅
|```GET```|```/blockchain/domains```|List of domains|✅
|```GET```|```/blockchain/domain/:name```|Details domain|✅
|```GET```|```/blockchain/dns```|List of DNS records|✅
|```GET```|```/blockchain/dns/:domain/:type```|Show a DNS records|✅
<!-- |```GET```|```/blockchain/dns/:domain```|List of dns records by domain|❌
|```GET```|```/blockchain/dns/:domain/:type```|Deatil of dns records|❌ -->
|```GET```|```/blockchain/search```|Search result|❌
|```POST```|```/event```|Create an event|✅

## DNS Record Type support

|Type|Status|
|-|-|
|A|✅|
|AAAA|✅|
|CNAME|✅|
|MX|❌|
|TXT|✅|
|SRV|❌|
|CAA|❌|
|NS|❌|
|SOA|❌|

### Domain Prices
|Characters|Price (nIPN)|
|-|-|
|```Less than 6```|100.000|
|```Less than 9```|75.000|
|```Rest```|5.000|

### Validator fee types
|||
|-|-|
|0|```by size```|
|1|```percent```|
|2|```fixed price```|

### Updating operations
- All fee prices to update 1.000 nIPN
- Time to wait update again (20 minutes)
- DNS Record update 500 nIPN

## Mempool
**Status: ⚒️**

List of events stored in volatile memory, categorized by CPU threads that process the events, waiting to be verified for registration if approved.


## Byzantine Fault Tolerance Consensus Algorithms (BFTCA)
**Status: ❌**

## Blockchain Settings
|Setting|Value
|-|-|
|Block time|5 seconds|
|Time out of refund|72 hours|
|Event max size|8.192 bytes|
|Tx memo max size|255 bytes|
|Unit time|millisecond|
|Default token|IPN|
|Maximum domain renewal time|Two years|
|Imposible Address|1x1|
