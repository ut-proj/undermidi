# undermidi

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]

[![][logo]][logo-large]

*An LFE MIDI Port Server*

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* GNU `make`
* A modern install of Erlang (v20+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)
* Golang

This project's `rebar.config.script` will set the required Go environment
variables.

## Build & Run

Build the required Go MIDI server and compile the LFE:

```shell
$ make
```

Start up the LFE REPL:

``` shell
$ rebar3 lfe repl
```

Start the app:

```cl
lfe> (application:ensure_all_started 'undermidi)
```

See the running `gen_server`s for the Go MIDI server:

```lisp
lfe> (undermidi.app:children)
```
```lisp
(#(ports.lisp.server #Pid<0.366.0> worker (ports.lisp.server))
 #(ports.go.server #Pid<0.365.0> worker (ports.go.server)))
```

## API

```lisp
(ports.go.server:send #(command echo))
```
```lisp
#(result "echo")
```

```lisp
(ports.lisp.server:send #(command echo))
```
```lisp
#(result "echo")
```

[//]: ---Named-Links---

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[github]: https://github.com/ut-proj/undermidi
[gh-actions-badge]: https://github.com/ut-proj/undermidi/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/ut-proj/undermidi/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-21%20to%2024-blue.svg
[versions]: https://github.com/ut-proj/undermidi/blob/master/.github/workflows/cicd.yml
