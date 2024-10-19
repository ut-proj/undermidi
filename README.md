# undermidi

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]

[![][logo]][logo-large]

*A set of OTP servers that faciliate live MIDI composition and performance*

## About

undermidi supports two use cases, both of which utilise the Erlang term MIDI message formats defined in [midilib](https://github.com/erlsci/midilib):

* direct calls to MIDI devices, each call requiring the use of a device and MIDI channel
* a MIDI device manager that allows calls to be made without explicitly passing the device and MIDI channel every time

Note that the calls made to midilib use the `midibin` module for binary MIDI messages, which in turn uses the [Erlang MIDI NIF](https://github.com/sonic-pi-net/sonic-pi/tree/dev/app/external/sp_midi) provided by the [Sonic Pi project](https://github.com/sonic-pi-net/sonic-pi).

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

**IMPORTANT!!**: the command given above automatically starts undermidid. If you do not use that command, you will need to manually start undermidi. Not doing so will result in many commands cusing a segmentation fault of the Erlang VM due to the MIDI NIF not being initialised!

Once the LFE REPL is ready, you can start the app:

```cl
(undermidi@local)lfe> (undermidi:start)
```

Note that, depending upon the configured log level, you may see a fair amount of output.

## Scratch (temporary)

``` lisp
(undermidi:start)
(undermidi:list-devices)
(set device "model_15")
(set channel 1)
(set term (midimsg:note-on channel 48 64))
(um.ml:send device term)
(set term (midimsg:note-off channel 48 64))
(um.ml:send device term)

(undermidi:list-devices)
(set device "model_15")
(set channel 1)
(um.note:play device channel (um.note:make 'C3))

(undermidi:list-devices)
(set device "model_15")
(set channel 1)
(um.note:play-notes device channel (um.note:make '(C3 C3 C4 C3)) 500)
(set notes (um.note:make '(C3 C3 Eb3 C3 C3 Bb3 C4 C3)))
(um.note:play-notes device channel notes 250 8)
(set notes (um.note:make '(C3 C3 Eb3 C3 Eb4 Bb3 C4 C3)))
(um.note:play-notes device channel notes 250 8)

(undermidi:list-devices)
(set device "model_15")
(set `#(ok ,d) (undermidi.devices:new device))
(undermidi.device.conn:echo d "testing ...")
(undermidi:play-note d 'C3)
(undermidi:play-notes d '(C3 C3 Eb3 C3 C3 C3 C3 C4) 250 8)
(include-lib "priv/seqs/basic.lfe")
(undermidi:play-notes d (seq-1a) 250 8)
(undermidi:play-notes d '(seq-1b) 250 8)
(undermidi:play-notes d '(seq-2a) 250 8)
(undermidi:play-notes d '(seq-3a) 250 8)

(undermidi:list-devices)
(set device "midi_bus_1")
(set device "provs-mini_provs-mini_midi_1_24_0")
(set `#(ok ,d) (undermidi.devices:new device))
(include-lib "priv/seqs/basic.lfe")
(set notes (um.note:lengthen (um.chord:make-fuzzy (seq-3a)) 80))
(undermidi:play-notes d notes 8200 1)

(set device "midi_bus_1")
(set `#(ok ,d) (undermidi.devices:new device))
(set cmaj7 (um.chord:lengthen (um.chord:make-fuzzy '(C4 E4 G4 B4)) 40))
(set am7 (um.chord:lengthen (um.chord:make-fuzzy '(C4 E4 G4 A4)) 40))
(set fmaj7 (um.chord:lengthen (um.chord:make-fuzzy '(C4 E4 F4 A4)) 40))
(set dm7 (um.chord:lengthen (um.chord:make-fuzzy '(C4 D4 F4 A4)) 40))
(set bdim7 (um.chord:lengthen (um.chord:make-fuzzy '(B3 D4 F4 A4)) 40))
(undermidi:play-chords d (list cmaj7 am7 fmaj7 dm7 bdim7) 4200 0)

;; Experiment in adding a playlist to the playlist gen_server

(set device "model_15")
(set `#(ok ,d) (undermidi.devices:new device))
(priv.seqs.basic:play d)

(undermidi.player.queue:dump)

rebar3 as playlist-add lfe run -- name:seq1 type:mod source:priv.seqs.basic

(undermidi.player.queue:dump)
(undermidi.player.queue:play-next "model_15")

;; new sequence

(set device "provs-mini_provs-mini_midi_1_24_0")
(set device "model_15")
(set device "midi_bus_1")
(set `#(ok ,d) (undermidi.devices:new device))
(priv.seqs.rhythmic01:play d)

;; |1      2       3       4       |1      2       3       4       
;; '   '   '   '   '   '   '   '   '   '   '   '   '   '   '   '   
;; 1/4 -  1/8 1/4  -  1/4  -  1/8  1/4 -   .   .   .   .   .   .   

(set d1 400)
(set p1 50)
(set n1 (um.note:make 'E4 64 (- (* 2 d1) p1)))
(set n2 (um.note:make 'E4 64 (- d1 p1)))
(set n3 (um.note:make 'E5 64 (- d1 p1)))
(set n4 (um.note:make 'B4 64 (- (* 2 d1) p1)))
(set n5 (um.note:make 'A4 64 (- (* 2 d1) p1)))

(set r1 (um.note:make 'E0 0 (* 2 d1)))
(set r2 (um.note:make 'E0 0 p1))

(set n6 (um.note:make 'F#4 64 (- (* 2 d1) p1)))
(set n7 (um.note:make 'F#4 64 (- d1 p1)))
(set n8 (um.note:make 'F#5 64 (- d1 p1)))
(set n9 (um.note:make 'C#5 64 (- (* 2 d1) p1)))
(set n10 (um.note:make 'D#5 64 (- (* 2 d1) p1)))

(set n11 (um.note:make 'B3 64 (- (* 2 d1) p1)))
(set n12 (um.note:make 'C#4 64 (- d1 p1)))
(set n13 (um.note:make 'B4 64 (- d1 p1)))
(set n14 (um.note:make 'F#4 64 (- (* 2 d1) p1)))
(set n15 (um.note:make 'D#5 64 (- (* 2 d1) p1)))

(set motive1 (list n1  r2 n2  r2 n4  r2 n1  r2 n3  r2 n1  r1 r1 r1))
(set motive2 (list n1  r2 n2  r2 n5  r2 n1  r2 n3  r2 n1  r1 r1 r1))
(set motive3 (list n6  r2 n7  r2 n9  r2 n6  r2 n8  r2 n6  r1 r1 r1))
(set motive4 (list n6  r2 n7  r2 n10 r2 n6  r2 n8  r2 n6  r1 r1 r1))
(set motive5 (list n11 r2 n12 r2 n14 r2 n11 r2 n13 r2 n11 r1 r1 r1))
(set motive6 (list n11 r2 n12 r2 n15 r2 n11 r2 n13 r2 n11 r1 r1 r1))

(set phrase1 (list motive1 motive1 motive2 motive1))
(set phrase2 (list motive3 motive3 motive4 motive3))
(set phrase3 (list motive5 motive5 motive6 motive5))
(set phrases (++ phrase1 phrase2 phrase1 phrase3 phrase1))

(defun play (phrases)
  (list-comp ((<- phrase phrases))
    (list-comp ((<- n phrase))
        (progn 
          (undermidi:play-note d n)
          (timer:sleep (mref n 'duration))))))

(play phrases)
  
(defun repeat
  ((_ 0)
   'ok)
  ((phrases count)
   (play phrases)
   (repeat phrases (- count 1))))

(repeat phrases 2)

;; Ambient chord progression 1

(set device "provs-mini_provs-mini_midi_1_24_0")
(set `#(ok ,d) (undermidi.devices:new device))

(set cd1 (um.chord:lengthen (um.chord:make-fuzzy '(A3 C4 E4 A4)) 40))
(set cd2 (um.chord:lengthen (um.chord:make-fuzzy '(A3 C4 F4 A4)) 40))
(set cd3 (um.chord:lengthen (um.chord:make-fuzzy '(G3 C4 F4 G4)) 40))
(set cd4 (um.chord:lengthen (um.chord:make-fuzzy '(G3 C4 E4 G4)) 40))
(set cd5 (um.chord:lengthen (um.chord:make-fuzzy '(F3 C4 E4 F4)) 40))
(set cd6 (um.chord:lengthen (um.chord:make-fuzzy '(F3 C4 D4 F4)) 40))
(set cd7 (um.chord:lengthen (um.chord:make-fuzzy '(G3 C4 D4 G4)) 40))
(set cd8 (um.chord:lengthen (um.chord:make-fuzzy '(G3 B3 A4)) 40))
(set cd9 (um.chord:lengthen (um.chord:make-fuzzy '(A3 C4 B4)) 40))
(set cd10 (um.chord:lengthen (um.chord:make-fuzzy '(A3 C4 A4 E5)) 40))
(undermidi:play-chords d (list cd1 cd2 cd3 cd4 cd5 cd6 cd7 cd8 cd9 cd10) 12800 0)

;; Ambient chord progression 2

(set device "provs-mini_provs-mini_midi_1_24_0")
(set `#(ok ,d) (undermidi.devices:new device))

(set cd1 (um.chord:lengthen (um.chord:make-fuzzy '(D3 F3 A3)) 40))
(set cd2 (um.chord:lengthen (um.chord:make-fuzzy '(D3 G3 B3)) 40))
(set cd3 (um.chord:lengthen (um.chord:make-fuzzy '(E3 G3 C4)) 40))
(set cd4 (um.chord:lengthen (um.chord:make-fuzzy '(F3 A3 C4)) 40))
(set cd5 (um.chord:lengthen (um.chord:make-fuzzy '(E3 A3 C4)) 40))
(set cd6 (um.chord:lengthen (um.chord:make-fuzzy '(F3 A3 D4)) 40))
(set cd7 (um.chord:lengthen (um.chord:make-fuzzy '(G3 D4 B4)) 40))
(set cd8 (um.chord:lengthen (um.chord:make-fuzzy '(A3 E4 C5)) 40))
(set cd9 (um.chord:lengthen (um.chord:make-fuzzy '(A3 F4 C5)) 40))
(set cd10 (um.chord:lengthen (um.chord:make-fuzzy '(A3 D4 F5)) 40))
(set cd11 (um.chord:lengthen (um.chord:make-fuzzy '(G3 D4 B5)) 40))
(set cd12 (um.chord:lengthen (um.chord:make-fuzzy '(E3 C4 A5)) 40))
(set cd13 (um.chord:lengthen (um.chord:make-fuzzy '(F3 D4 A5)) 40))
(set cd14 (um.chord:lengthen (um.chord:make-fuzzy '(F3 D4 A4)) 40))
(set cd15 (um.chord:lengthen (um.chord:make-fuzzy '(B2 D4 G4)) 40))
(set cd16 (um.chord:lengthen (um.chord:make-fuzzy '(E2 C4 G4)) 40))
(set cd17 (um.chord:lengthen (um.chord:make-fuzzy '(A1 C4 F4)) 40))
(set cd18 (um.chord:lengthen (um.chord:make-fuzzy '(D1 A2 F3 D4)) 40))
(undermidi:play-chords d (++ (list cd1 cd2 cd3 cd4)
                             (list cd1 cd2 cd5 cd4)
                             (list cd6 cd7 cd8 cd9)
                             (list cd10 cd11 cd12 cd13)
                             (list cd14 cd15 cd16 cd17 cd18))
                         8400 0)

```

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

#### Scales

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

## OLD -- will be (re)moved

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
