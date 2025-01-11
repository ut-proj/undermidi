(defmodule patches.seqs.richter-01
  (export all))

#|
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

|#

