# Ipncore

## Installation

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
|🐞|Testing|

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
|201|```token.update```|Update a token|⚒️|
|202|```token.delete```|Delete a token|⚒️|
|210|```tx.coinbase```|Creation of coins by the token owner|🐞|
|211|```tx.send```|Send a simple payment|🐞|
|212|```tx.sendmulti```|Send payment to multiple recipients|❌|
|213|```tx.refund```|Returns a transaction made by the payer|❌|
|214|```tx.jackpot```|Jackpot event|❌|
|215|```tx.reward```|Reward core validators|❌|
|216|```tx.burned```|Coin destruction|❌|
|400|```domain.new```|Register a new domain|⚒️|
|401|```domain.update```|Update a domain|⚒️|
|402|```domain.delete```|Delete a domain|⚒️|
|410|```dns.new```|Register a new DNS Record|❌|
|411|```dns.update```|Update a DNS Record|❌|
|412|```dns.delete```|Delete a DNS Record|❌|
|1000|```pubkey.new```|Public key registration|✅|

## Services

|Name|Protocol|Port|Status|
|-|-|-|-|
|Explorer API|HTTP|80, 443|✅
|RealTime API|WebSocket|80, 443|❌|
|DNS Service|DNS|53|❌|
|PubSub Service|IMP|8484|⚒️ 70%|

## Explorer API

|Method|Route|Description|Status|
|-|-|-|-|
|```GET```|```/blocks```|List of blocks|✅
|```GET```|```/channel```|List of channel|✅
|```GET```|```/txs```|List of transactions|✅
|```GET```|```/txo```|List of output transactions|✅
|```GET```|```/txi```|List of input transactions|✅
|```GET```|```/tokens```|List of tokens|✅
|```GET```|```/token/:token/:channel```|Token details|✅
|```GET```|```/validators```|List of validators|✅
|```GET```|```/validators/:hostname/:channel```|Validator details|✅
|```GET```|```/search```|Search result|✅
|```GET```|```/balance/:address58```|Balance of an address|✅
|```GET```|```/activity/:address58```|List of events of an address|✅
|```GET```|```/channel/:channel_id```|Details of a channel|✅
|```GET```|```/status/:channel_id```|Channel status|✅
|```GET```|```/block/:hash16```|Details of the block queried by hash|✅
|```GET```|```/block/height/:height```|Details of the block queried by height|✅
|```GET```|```/tx/:hash16```|Details transaction queried by hash|✅
|```POST```|```/event```|Create an event|✅

## DNS Record Type support

|Type|Status|
|-|-|
|A|❌|
|AAAA|❌|
|CNAME|❌|
|MX|❌|
|TXT|❌|
|SRV|❌|
|CAA|❌|
|NS|❌|

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
