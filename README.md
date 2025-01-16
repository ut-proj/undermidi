# undermidi

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github-tags-badge]][github-tags]
[![Downloads][hex-downloads]][hex-package]

*A set of OTP servers that faciliate live MIDI composition and performance*

[![][logo]][logo-large]

## About

undermidi supports two use cases, both of which utilise the Erlang term MIDI message formats defined in [midilib](https://github.com/erlsci/midilib):

* direct calls to MIDI devices, each call requiring the use of a device and MIDI channel
* a MIDI device manager that allows calls to be made without explicitly passing the device and MIDI channel every time

Note that the calls made to midilib use the `midibin` module for binary MIDI messages, which in turn uses the [Erlang MIDI NIF](https://github.com/sonic-pi-net/sonic-pi/tree/dev/app/external/sp_midi) provided by the [Sonic Pi project](https://github.com/sonic-pi-net/sonic-pi).

**Update**: This use of an Erlang NIF is new in 0.3.0! As part of that change, we made _significant_ and **breaking** changes to the `undermidi` API.

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* `cmake`, GNU `make`, OS-specific dev libraries that support MIDI
* A modern install of Erlang (v25+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)

## Build & Run

The required sources for buidling the Erlang NIF will be downloaded and compiled, and then the Erlang and LFE for undermidi will be compiled, all with the following:

```shell
$ rebar3 compile
```

Then start the LFE REPL with prefined options for undermidi:

```shell
$ rebar3 as undermidi repl
```

**IMPORTANT!!**: the command given above automatically starts undermidi. If you do not use that command, you will need to manually start undermidi. Not doing so will result in many commands causing a segmentation fault of the Erlang VM due to the MIDI NIF not being initialised!

If you find yourself tiring of typing the above command, you can use the simple bash script that does the same:

``` shell
$ ./priv/scripts/run.sh
```

Once the LFE REPL is ready, you can start the app:

```cl
(undermidi@local)lfe> (undermidi:start)
```

Note that, depending upon the configured log level, you may see a fair amount of output.

## API

There are two ways to use this library:

1. Stateless: Make direct calls to the undermidi wrappers for the MIDI Erlang NIF, passing device and channel with every call
1. Stateful: Create a managed connection to the MIDI device, passing only the data you need to make music

Each of these are demonstrated below. The stateful approach is preferred and encouraged, as it makes code easier to read and helps one organise workflows when coding for multiple MIDI devices at once.

The `midilib` code used by undermidi utilises the same means as the Erlang NIF for referencing MIDI devices: their system names. The full set known by the system can be displayed with the following:

``` lisp
lfe> (undermidi:list-devices)

inputs
  1. network_session_1
  2. core_midi_general
  3. core_midi_keyboards
  4. komplete_kontrol_s88_mk2_port_1
  5. komplete_kontrol_s88_mk2_port_2
  6. komplete_kontrol_daw_-_1
  7. model_15
  8. model_d

outputs
  1. network_session_1
  2. core_midi_general
  3. core_midi_keyboards
  4. komplete_kontrol_s88_mk2_port_1
  5. komplete_kontrol_s88_mk2_port_2
  6. komplete_kontrol_daw_-_1
  7. model_15
  8. model_d
ok
```

The output of this display function will vary, depending upon system and connected/configured MIDI devices.

We'll use one of these names in the examples below, Moog's `"model_15"`.

### Stateful

Get a managed MIDI device connection:

``` lisp
lfe> (set `#(ok ,d) (undermidi.devices:new "model_15"))
#(ok #Pid<0.1140.0>)
```

Note that the name may be passed as either an atom or a string (list), but that the name used by the NIF is a string, and as such, undermidi ensures a name passed as an atom is converted when calling `new`.

#### Notes

The `undermidi` project represents notes as a data structure of pitch, velocity, and duration. However, it provides some defaults to make that a little easier to work with, as well as a means of easily referencing MIDI pitch values using note names.

Play a single note:

``` lisp
lfe> (undermidi:play-note d 'C3)
```

Play a series of notes:

``` lisp
lfe> (undermidi:play-notes d '(C3 C3 Eb3 C3 C3 Bb3 C3 C4))
```

The `play-notes` function also accepts optional arguments for changing the time between the notes as well as the ability to repeat the series.

#### Chords

#### Sequences

### Stateless

Some variables for the MIDI device and the MIDI channel we're going to use for most calls:

``` lisp
lfe> (set device "")
lfe> (set channel 1)
```

#### Notes

``` lisp
lfe> (um.note:play device channel 'C3)
lfe> (um.note:play device channel '(C3 C3 Eb3 C3 C3 Bb3 C3 C4))
```

#### Chords

#### Sequences

## Licenses

undermidi: BSD 2-Clause

Sonic Pi's Erlang NIF: MIT

RtMIDI: MIT-like (optional notification)

[//]: ---Named-Links---

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[github]: https://github.com/ut-proj/undermidi
[gh-actions-badge]: https://github.com/ut-proj/undermidi/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/ut-proj/undermidi/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-25%20to%2027-blue.svg
[versions]: https://github.com/ut-proj/undermidi/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/ut-proj/undermidi/tags
[github-tags-badge]: https://img.shields.io/github/tag/ut-proj/undermidi.svg
[github-downloads]: https://img.shields.io/github/downloads/ut-proj/undermidi/total.svg
[hex-badge]: https://img.shields.io/hexpm/v/undermidi.svg?maxAge=2592000
[hex-package]: https://hex.pm/packages/undermidi
[hex-downloads]: https://img.shields.io/hexpm/dt/undermidi.svg
