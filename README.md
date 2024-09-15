# undermidi

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]

[![][logo]][logo-large]

*An NIF-based LFE MIDI wrapper and server*

## About

undermidi supports two use cases, both of which utilise the Erlang term MIDI message formats defined in [midilib](https://github.com/erlsci/midilib):

* direct calls to MIDI devices, each call requiring the use of a device and MIDI channel
* a MIDI device manager that allows calls to be made without explicitly passing the device and MIDI channel every time

Note that the calls made to midilib use the `midibin` module for binary MIDI messages, which in turn uses the [Erlang MIDI NIF](https://github.com/sonic-pi-net/sonic-pi/tree/dev/app/external/sp_midi) provided by the [Sonic Pi project](https://github.com/sonic-pi-net/sonic-pi).

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* `cmake`, GNU `make`, OS-specific dev libraries that support MIDI
* A modern install of Erlang (v21+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)

## Build & Run

Once you've selected how pull in the Go `midiserver`, you can build and run the
LFE code:

```shell
$ rebar3 compile
```

If you're using the downloaded binary, you don't need anything special, just run
this:

``` shell
$ rebar3 undermidi
```

Once the LFE REPL is ready, you can start the app:

```cl
(undermidi@local)lfe> (undermidi:start)
```

Depending upon the configured log level, you may see a fair amount of output, including the Go MIDI server being started.

## API

For actually using undermidi to generate music, the `um*` modules are used. Notes, chords, and controls are all supported. Before playing, though, the device and MIDI channel must be set:

``` lisp
(undermidi@local)lfe> (progn
                        (um:set-device 0)
                        (um:set-channel 0)
                        'ok)
```

### Notes

Individual notes may be played in any of the following ways:

``` lisp
(undermidi@local)lfe> (progn
                        (set veloc 80)
                        (set dur 3000))
(undermidi@local)lfe> (um:play-note 'Ab3 veloc dur)
(undermidi@local)lfe> (um:play-notes '(Ab3 F4 C5) veloc dur)
```

### Chords

Here's how to set up for playing a chord (this was created for a sampled MIDI piano on the OS' MIDI channel 1, which maps to the MIDI server's channel 0) :

``` lisp
(undermidi@local)lfe> (progn
                        (set bpm 66)
                        (set pedal-gap 500)
                        (set dur (trunc (- (* 4 (/ 60 bpm) 1000) pedal-gap)))
                        (set ch1 '(Bb2 F3 Db4))
                        (um:soft-pedal-on)
                        (um:play-chord ch1 veloc dur)
                        'ok)
```

And a few more chords ;-)

``` lisp
(undermidi@local)lfe> (progn
                        (set ch2 '(Ab2 F3 C4))
                        (set ch3 '(Db2 Db3 Ab3))
                        (set ch4 '(Gb2 Gb3 Bb3))
                        (set ch5 '(C3 Ab3 Eb4))
                        (set ch6 '(Db3 Ab3 Eb4))
                        (set ch7 '(C3 Ab3 F4))
                        (set ch8 '(F2 Db3 Ab3))
                        (set ch9 '(Ab2 F3 C4))
                        (set ch10 '(Gb2 Eb3 Bb3))
                        (set ch11 '(Eb2 Gb3 C4))
                        'ok)
(undermidi@local)lfe> (progn
                        (list-comp ((<- ch (list ch1 ch2 ch3 ch4
                                                 ch1 ch2 ch3 ch4
                                                 ch1 ch5 ch6 ch7
                                                 ch1 ch5 ch6 ch2
                                                 ch1 ch8 ch4 ch9
                                                 ch1 ch8 ch10 ch11)))
                          ;; tweak the velocity for some dynamics
                          (let ((veloc (+ veloc (trunc (* 3 (- 3 (* 8 (rand:uniform))))))))
                            (um:sustain-pedal-off)
                            (timer:sleep pedal-gap)
                            (um:sustain-pedal-on)
                            (um:play-chord ch veloc dur)))
                        (um:sustain-pedal-off)
                        (um:soft-pedal-off)
                        'ok)
```

Fans of Max Richter will recognise this immediately :-)

The above example defines custom chords with an explicit set of notes. You may also use named chords and their inversions:

``` lisp
(undermidi@local)lfe> (progn
                        (set octave 2)
                        (set ch1-a (um.chord:create 'Bb 'minor octave))
                        (set ch1-b (um.chord:create 'Bb 'minor octave #m(inversion 2)))
                        (set ch1-c (um.chord:create 'Bb 'minor octave #m(inversion 3)))
                        (list-comp ((<- ch (list ch1-a ch1-b ch1-c)))
                          (um:play-chord ch veloc dur)))
```

Or:

``` lisp
(undermidi@local)lfe> (list-comp ((<- ch (list ch1-a
                                               (um.chord:invert ch1-a 2)
                                               (um.chord:invert ch1-a 3))))
                        (um:play-chord ch veloc dur))
```

Chords by roman numeral are also supported:

``` lisp
(undermidi@local)lfe> (progn
                        (set key 'C#)
                        (set ch1-d (um.chord:create key 'ionian 'vi octave))
                        (set ch1-e (um.chord:create key 'aeolian 'i octave))
                        (list-comp ((<- ch (list ch1-d ch1-e)))
                          (um:play-chord ch veloc dur)))
```

Since there is no overlap in chord function name between the Ionian and Aeolian modes, and those are the two most common, the following shorter syntax for those two is also supported:

``` lisp
(undermidi@local)lfe> (progn
                        (set ch1-f (um.chord:create key 'vi octave))
                        (set ch1-g (um.chord:create key 'i octave))
                        (list-comp ((<- ch (list ch1-f ch1-g)))
                          (um:play-chord ch veloc dur)))
```

### Controls 

In addition to notes and chords, undermidi can control the knobs on a synthesizer via MIDI CC messages.
Here's a setup for the Minimoog in the Luna DAW (for Tangerine Dream fans):

``` lisp
(undermidi@local)lfe> (include-lib "undermidi/include/luna/minimoog.lfe")

(undermidi@local)lfe> (progn
                        (um:set-device 0)
                        (um:set-channel 0)

                        (set velocity 80)
                        (set bpm 250)
                        (set dur (trunc (* (/ 60 bpm) 1000)))
                        (set pat1 '(C4 C4 Bb3 G3))
                        (set pat2 '(C4 C4 C4 Bb3 G3))
                        (set pat3 '(C4 C4 C4 C4 Bb3 G3))
                        (set pat4 '(C4 C3 C4 C4 Bb3 G3))
                        (set pat5 '(C4 C3 C4 C3 Bb3 G3))

                        (set seq1 (um.notes:duplicate pat1 15))
                        (set seq2 (um.notes:duplicate pat2 12))
                        (set seq3 (um.notes:duplicate pat3 10))
                        (set seq4 (um.notes:duplicate pat4 10))
                        (set seq5 (um.notes:duplicate pat5 10))

                        (set all (lists:append (list seq1 seq2 seq3 seq4 seq5)))
                        (um:set-cc (filter-cutoff-frequency) 16)
                        (um:cycle-cc (filter-cutoff-frequency) 16 70 68)
                        (um:play-notes all velocity dur))
```

You can do more than that at once, however you may experience MIDI or timing jitter (that will go away once the beat tracking is implemented). If you'd like to experiment with more, try this (using the same notes as above):

``` lisp
(progn
  (timer:sleep 1500)
  (um:set-cc (filter-cutoff-frequency) 16)
  (um:cycle-cc (filter-cutoff-frequency) 16 70 68)
  (um:cycle-cc (filter-emphasis) 0 58 68)
  (um:cycle-cc (osc-3-volume) 127 16 10)
  (um:cycle-cc (noise-volume) 0 28 20)
  (timer:sleep 500)
  (um:play-notes all velocity dur))
```


### Testing / Debugging

Various application-level functions (i.e., "admin stuff") is available in the `undermidi*` modules, e.g.:

```lisp
(undermidi@local)lfe> (undermidi:ping)
pong
```

Play the example MIDI code (requires the first MIDI device on your system, index 0, to be connected to a MIDI device, hardware or software, listening on channel 1):

```lisp
(undermidi@local)lfe> (undermidi:example)
ok
```


## Macros

The `midi` macros is provided as a typing convenience for generating MIDI messages:

``` lisp
(undermidi@local)lfe> (include-lib "undermidi/include/macros.lfe")
(undermidi@local)lfe> (midi (midimsg:device 0)
                            (midimsg:channel 0))
#(midi
  #(batch
    (#(device 0) 
    #(channel 0))))
```

The `send` macro from the same include saves typing when sending many messages to the Go MIDI server. Additionally, macros for MIDI notes are provided via another include:

``` lisp
(undermidi@local)lfe> (include-lib "undermidi/include/notes.lfe")
(undermidi@local)lfe> (set veloc 40)
(undermidi@local)lfe> (progn
                        (send (midimsg:device 0)
                        (midimsg:channel 0))
                        (send (midimsg:note-on (Bb1) veloc))
                        (timer:sleep 2000)
                        (send (midimsg:note-off (Bb1))))
```

By default, batched messages sent to the MIDI server will be executed serially, in the order they are defined in the batch.

To execute these in parallel, one must annotate the messages appropriately:

``` lisp
(undermidi@local)lfe> (midi-parallel (midimsg:note-on (Bb2) veloc)
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
(undermidi@local)lfe> (progn 
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
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-25%20to%2027-blue.svg
[versions]: https://github.com/ut-proj/undermidi/blob/master/.github/workflows/cicd.yml
