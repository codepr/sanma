Sanma
=====

Playing around with Erlang and C, low-latency trading platform, learning something about HFTs
and scratching the surface of quant development.

- An Order Execution Manager, An OTP application that handles the I/O portion
  of the platform, probably via a REST api, including:
    - A Session Manager to handle authentication
    - A Risk Manager to track positions in real-time
    - Market Data Feed for consumption (real-time price quotes, last trades etc)
    - The main interface with the Matching Engine that allows to insert/cancel orders
    - An audit logger
- A Matching Engine, this part is the most latency-critical part and requires low
  overhead and predictable performance to match lowest bids with highest asks. It's
  gonna be made in C and communicate with the Order Execution Manager via an Unix
  socket.

The Matching Engine is ideally built and supervised by the Erlang main application.

## Why

I believe a trading platform is an awesome project to explore several CS
topics, at its simplest it's a matching engine which can be implemented in any
language, but it can also be extended into something more interesting to a
pretty much endless degree, involving system programming, memory management,
protocols, IPC, RPC, backend technologies, database, authentications and so on.

It was also a good chance to give an honest try to Erlang, I spent a
considerable amount of time writing Elixir in the last few years but never had
a true shot at the original BEAM language. I think it fits the architecture
wonderfully as the core engine in C should be pretty performant, enough for the
whole platform to be I/O bound, where Erlang shines, allowing for simple
horizontal scaling if needed.

As a final note, this is meant to be a **purely fun and learning experiment**,
I am totally **ignorant** on how HFTs should work in the real world and it's
likely that to achieve microseconds level latencies, many critical choices and
expedients must be adopted, likely writing the whole system in highly optimized
C++ (custom network stack?) down to the hardware level. I do belive though,
that while Erlang may be too slow for the core matching logic, it is indeed the
best tool for building a highly available, scalable, fault-tolerant gateway and
supervision layer.

## Build

```bash
rebar3 compile
```
