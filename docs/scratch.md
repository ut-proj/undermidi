## Scratch

### Working with the new NIF-based API

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
```

### Working with Patches

``` lisp
;; Basic sequence

(undermidi:list-devices)
(set device "model_15")
(set `#(ok ,d) (undermidi.devices:new device))
(patches.seqs.basic:play d)

;; Rhythmic sequence

(set device "provs-mini_provs-mini_midi_1_24_0")
(set device "model_15")
(set device "midi_bus_1")
(set `#(ok ,d) (undermidi.devices:new device))

(patches.seqs.rhythmic01:play d)

;; Ambient chord progression 1

(set device "provs-mini_provs-mini_midi_1_24_0")
(set device "core_midi_general")
(set `#(ok ,d) (undermidi.devices:new device))

(patches.progs.slow-chords01:play d)

;; Ambient chord progression 2

(set device "provs-mini_provs-mini_midi_1_24_0")
(set device "core_midi_general")
(set `#(ok ,d) (undermidi.devices:new device))

(patches.progs.slow-chords02:play d)

;; More radnom chords playing
(set variances #m(velocity 80
                delay 4000))
(set chrds (patches.progs.slow-chords02:all-chords))
(set preped (patches.common:prep-chords chrds 60 variances))
```

### Working with Playlists

``` lisp
;; Experiment in adding a playlist to the playlist gen_server

(undermidi.player.queue:dump)

rebar3 as playlist-add lfe run -- name:seq1 type:mod source:patches.progs.slow-chords01

(undermidi.player.queue:dump)
(undermidi.player.queue:play-next "model_15")
```
