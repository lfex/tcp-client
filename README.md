# tcp-client

*An LFE TCP client connection manager*

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

## About

This library provides a simple TCP client for use in non-critical BEAM
applications such as command line tools and REPLs. That being said, it is
intended for use from OTP release apps, making use of `./config/sys.config`.

## Configuration

Update your applications `sys.config` to include the following:

``` erlang
[
 %% any other confit entries ...
 {'tcp-client', [
     {server, [
         %% where to connect the client
         {host, "localhost"},
         {port, 7099},
         {options, [
             %% gen_tcp options
             {tcp, [binary, {active, true}, {packet, 0}]},
             %% The M/F responseible for parsing the packet; Func is arity 2,
             %% taking the packet data as the first argument and a tuple of
             %% the reporter {Mod, Func} as the second argument. This M/F is
             %% called by `connect` when a TCP packet is received.
             {parser, {Mod, Func}},
             %% It is up to the parser to call this next M/F, but the reporter
             %% tuple is what gets passed to the paser M/F.
             {reporter, {Mod, Func}}
         ]}
     ]}
 ]}
].
```

## Usage

Start up the connection manager:

``` lisp
lfe> (application:start 'tcp-client)
```

Send a message that will wait for a response ("call"):

``` lisp
lfe> (tcp-client:call-msg data)
```

Send a message that will return immediately ("cast"):

``` lisp
lfe> (tcp-client:cast-msg data)
```

## Licence

Copyright © 2015, Carlos Andres Bolaños

Copyright © 2019, Andrea Leopardi

Copyright © 2020, Duncan McGreggor


[//]: ---Named-Links---

[logo]: priv/images/logo.png
[logo-large]: priv/images/logo.svg
[github]: https://github.com/lfex/tcp-client
[gh-actions-badge]: https://github.com/lfex/tcp-client/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/tcp-client/actions
[lfe]: https://github.com/rvirding/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/lfex/tcp-client/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/lfex/tcp-client/tags
[github-tag-badge]: https://img.shields.io/github/tag/lfex/tcp-client.svg
[github-downloads]: https://img.shields.io/github/downloads/lfex/tcp-client/total.svg
