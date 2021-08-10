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

Various application-level functions (i.e., "admin stuff") is available in the `undermidi*` modules, e.g.:

```lisp
lfe> (undermidi:ping)
pong
```

Play the example MIDI code (requires the first MIDI device on your system, index 0, to be connected to a MIDI device, hardware or software, listening on channel 1):

```lisp
lfe> (undermidi:example)
ok
```

For actually using undermidi to generate music, the `um*` modules are used:

``` lisp
lfe> (include-lib "undermidi/include/notes.lfe")
lfe> (progn
       (um:set-device 0)
       (um:set-channel 0)
       (set bpm 66)
       (set veloc 50)
       (set dur (trunc (* 4 (/ 60 bpm) 1000)))
       (set ch1 (list (Bb2) (F3) (Db4)))
       (um:play-chord ch1 veloc dur))
```

A few more:

``` lisp
lfe> (progn
      (set ch2 (list (Ab2) (F3) (C4)))
      (set ch3 (list (Db2) (Db3) (Ab3)))
      (set ch4 (list (Gb2) (Gb3) (Bb3)))
      (set ch5 (list (C3) (Ab3) (Eb4)))
      (set ch6 (list (Db3) (Ab3) (Eb4)))
      (set ch7 (list (C3) (Ab3) (F4)))
      (set ch8 (list (F2) (Db3) (Ab3)))
      (set ch9 (list (Ab2) (F3) (C4)))
      (set ch10 (list (Gb2) (Eb3) (Bb3)))
      (set ch11 (list (Eb2) (Gb3) (C4)))
      'ok)
lfe> (list-comp ((<- ch (list ch1 ch2 ch3 ch4
                              ch1 ch2 ch3 ch4
                              ch1 ch5 ch6 ch7
                              ch1 ch5 ch6 ch2
                              ch1 ch8 ch4 ch9
                              ch1 ch8 ch10 ch11)))
       (um:play-chord ch veloc dur)
       'ok)
```

## Macros

The `midi` macros is provided as a typing convenience for generating MIDI messages:

``` lisp
lfe> (include-lib "undermidi/include/macros.lfe")
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
lfe> (set veloc 40)
lfe> (progn
       (send (midimsg:device 0)
             (midimsg:channel 0))
       (send (midimsg:note-on (Bb1) veloc))
       (timer:sleep 2000)
       (send (midimsg:note-off (Bb1))))
```

By default, batched messages sent to the MIDI server will be executed serially, in the order they are defined in the batch.

To execute these in parallel, one must annotate the messages appropriately:

``` lisp
lfe> (midi-parallel (midimsg:note-on (Bb2) veloc)
                    (midimsg:note-on (F3) veloc)
                    (midimsg:note-on (Db4) veloc))
#(midi
  #(batch
    (#(id #B(1 5 108 38 116 193 73 249 156 64 81 98 200 199 193 170))
     #(parallel? true)
     #(messages
       (#(note_on (#(pitch 46) #(velocity 40)))
        #(note_on (#(pitch 53) #(velocity 40)))
        #(note_on (#(pitch 61) #(velocity 40))))))))
```

You may send this message with either the `send-parallel` or the shorter `cast` macro:

``` lisp
lfe> (progn 
       (cast (midimsg:note-on (Bb2) volume)
             (midimsg:note-on (F3) volume)
             (midimsg:note-on (Db4) volume))
       (timer:sleep 4000)
       (cast (midimsg:note-off (Bb2))
             (midimsg:note-off (F3))
             (midimsg:note-off (Db4))))
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
