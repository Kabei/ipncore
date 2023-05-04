## IPNCORE

##### Version 0.1.0

#

## Installation

```bash
apt-get install erlang erlang-src elixir
mix deps.get
```

## Run

```bash
elixir -S mix run --no-halt --no-compile
```

#

## Work status

|Status|Description|
|-|-|
|✅|Done|
|❌|Pending|
|⚒️|In progress|
|🐞|Debug|

## Blockchain assets

It is the set of objects that is part of the Blockchain

|Asset|Description|
|-|-|
| `Tokens` |Payment currencies exchangeable in the Blockchain ecosystem|
| `Validators` |These are the nodes in charge of performing the Proof of Validation (PoV) applied to all events of the block that are processed on the platform|
| `Domains` |Identification record that can be used to shorten an address to receive payments, navigate to a social profile or website|

<!-- ## Special Operations

* Transaction
* Exchange -->

## Events

Events are requests made by users or automatic operations by the system that affect the Blockchain.

|Code|Event|Description|Status|
|-|-|-|-|
|0| `account.new` |Account registration|✅|
|1| `account.validator` |Update a account's validator|✅|
|2| `account.recover` |account recover|✅|
|3| `account.update` |Update a account|❌|
|50| `env.set` |Set environment variable|❌|
|51| `env.delete` |Delete environment variable|❌|
|100| `validator.new` |Create a new validator|❌|
|101| `validator.update` |Update a validator|❌|
|102| `validator.delete` |Delete a validator|❌|
|200| `token.new` |Create a new token|❌|
|201| `token.update` |Update a token|❌|
|202| `token.delete` |Delete a token|❌|
|250| `balance.lock` |Balance lock|❌|
|251| `balance.unlock` |Balance unlock|❌|
|300| `tx.coinbase` |Creation of coins by the token owner|❌|
|301| `tx.send` |Send a simple payment|❌|
|302| `tx.burn` |Coin destruction|❌|
|303| `tx.refund` |Return a transaction without paying a fee|❌|
|400| `domain.new` |Register a new domain|❌|
|401| `domain.update` |Update a domain|❌|
|402| `domain.delete` |Delete a domain|❌|
|403| `domain.renew` |Renew a domain|❌|
|404| `domain.expiry` |Remove all domains expired|❌|
|500| `dns.new` |Create a DNS Record|❌|
|501| `dns.update` |Update a DNS Record|❌|
|502| `dns.delete` |Delete a DNS Record|❌|
|900| `block.new` |A new block is published by a validator|❌|
|901| `block.received` |Replication of metadata received by block owner|❌|
|990| `round.start` |A validator starts a round|❌|
|999| `round.end` |A validator ends a round|❌|

### Body of the event

```json
[ // start
    0, // version
    "tx.send", // name
    1670285730448, // timestamp
    [ // body start
        "IPN", // token
        "1x7RKhLSoPHnP1bSx9J4apFkwb5ow", // send to
        50000, // amount
        "ippan.red", // validator
        "shopping" // note
    ], // body end
    "1x3dcvB6nq1uPqwG3W6njMhNwGmhrg", // from
    "otjBcCaM+KWTiRnI7rRQy8/CZPT/W2YJ0Ji9iUGSdq..." // signature base64
] // end
```

## Network servers

|Name|Protocol|Port|Status|
|-|-|-|-|
|Explorer API|HTTPS|80, 443|✅
|RealTime API|WebSockets|80, 443|❌|
|DNS over UDP|UDP|53|🐞|
|DNS over TLS|TLS|853|🐞|
|DNS over HTTPS|HTTPS|80, 443|⚒️ 70%|
|PubSub Service|IMP|8484|⚒️ 70%|

## Blockchain Explorer API

|Method|Route|Description|Status|
|-|-|-|-|
| `GET` | `/blockchain/blocks` |List of blocks|✅
| `GET` | `/blockchain/block/:hash16` |Details of the block|✅
| `GET` | `/blockchain/block/height/:height` |Details of the block queried by height|✅
| `GET` | `/blockchain/events` |List of events|✅
| `GET` | `/blockchain/event/:hash16` |Event detail|✅
| `GET` | `/blockchain/txs` |List of transactions|✅
| `GET` | `/blockchain/tx/:hash16` |Details transaction queried by hash|✅
| `GET` | `/txo` |List of output transactions|✅
| `HEAD` | `/blockchain/token/:name` |409 token not exists, 200 token exists|✅
| `GET` | `/blockchain/tokens` |List of tokens|✅
| `GET` | `/blockchain/token/:token/:channel` |Token details|✅
| `HEAD` | `/blockchain/validator/:name` |409 validator not exists, 200 validator exists|✅
| `GET` | `/blockchain/validators` |List of validators|✅
| `GET` | `/blockchain/validators` |Validator details|✅
| `GET` | `/blockchain/balance/:address58` |Balance of an address|✅
| `GET` | `/blockchain/activity/:address58` |List of events of an address|✅
| `GET` | `/blockchain/status` |Blockchain status|✅
| `HEAD` | `/blockchain/domain/:name` |409 domain not exists, 200 domain exists|✅
| `GET` | `/blockchain/domains` |List of domains|✅
| `GET` | `/blockchain/domain/:name` |Details domain|✅
| `GET` | `/blockchain/dns` |List of DNS records|✅
| `GET` | `/blockchain/dns/:domain/:type` |Show a DNS records|✅
| `GET` | `/blockchain/search` |Search result|❌
| `POST` | `/event` |Create an event|✅
| `POST` | `/dns-query` |DNS Wireformat|✅

## DNS Record Type support

|Code|Type|Status|
|-|-|-|
| `1` |A|✅|
| `2` |NS|✅|
| `5` |CNAME|✅|
| `6` |SOA|✅|
| `11` |WKS|🐞|
| `12` |PTR|🐞|
| `13` |HINFO|🐞|
| `15` |MX|✅|
| `16` |TXT|✅|
| `28` |AAAA|✅|
| `33` |SRV|🐞|
| `43` |DS|🐞|
| `44` |SSHFP|🐞|
| `46` |RRSIG|🐞|
| `47` |NSEC|🐞|
| `48` |DNSKEY|🐞|
| `99` |SPF|🐞|
| `255` |ALL|🐞|
| `256` |URI|🐞|
| `257` |CAA|🐞|

### Domain Prices

|Characters|Price (nIPN)|
|-|-|
| `Less than 6` |100.000|
| `Less than 9` |75.000|
| `Rest` |5.000|

### Validator fee types

|||
|-|-|
|0| `by size` |
|1| `percent` |
|2| `fixed price` |

### Updating operations

* All fee prices to update 1.000 nIPN
* Time to wait update again (20 minutes)
* DNS Record update 500 nIPN

## Mempool

**Status: 🐞**

List of events stored in volatile memory, categorized by CPU threads that process the events, waiting to be verified for registration if approved.

## Byzantine Fault Tolerance Consensus Algorithms (BFTCA)

**Status: ❌**

## Blockchain Settings

|Setting|Value
|-|-|
|Block Time|5 seconds|
|Time out of refund|72 hours|
|Event Max Size|8.192 bytes|
|Tx note Max Size|255 bytes|
|Unit Time|millisecond|
|Native Token|IPN|
|Maximum domain renewal time|Two years|
|Imposible Address|1x1|
