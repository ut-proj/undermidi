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
$ make clean && make
```

Start up the LFE REPL:

``` shell
$ rebar3 lfe repl
```

Start the app:

```cl
lfe> (undermidi:start)
```

Depending upon the configured log level, you may see a fair amount of output, including the Go MIDI server being started.

## API

```lisp
lfe> (undermidi:ping)
pong
```

Play the example MIDI code (requires the first MIDI device on your system, index 0, to be connected to a MIDI device, hardware or software, listening on channel 1):

```lisp
lfe> (undermidi:example)
ok
```

## Macros

The `midi` macros is provided as a typing convenience for generating MIDI messages:

``` lisp
lfe> (include-lib "undermidi/include/macros.lfe")
|-- loaded include: macros --|
lfe> (midi (midimsg:device 0)
           (midimsg:channel 0))
#(midi
  #(batch
    (#(device 0) 
    #(channel 0))))
```

The `send` macro from the same include saves typing when sending many messages to the Go MIDI server. Additionally, macros for MIDI notes are provided via another include:

``` lisp
lfe> (include-lib "undermidi/include/notes.lfe")
|-- loaded include: notes --|
lfe> (set volume 40)
lfe> (progn
       (send (midimsg:device 0)
             (midimsg:channel 0))
       (send (midimsg:note-on (Bb1) volume))
       (timer:sleep 2000)
       (send (midimsg:note-off (Bb1))))
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
