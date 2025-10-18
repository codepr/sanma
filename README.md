Sanma
=====

Playing around with Erlang and C, low-latency trading platform, composed by two macro components

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

Build
-----

    $ rebar3 compile
