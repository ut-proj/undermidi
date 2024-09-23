(defun um.tempo
  (export all))

(defun tempos ()
  '(#(prestissimo 200)
    #(presto 184)
    #(vivace 166)
    #(allegro 138)
    #(allegretto 116)
    #(moderato 108)
    #(andante 92)
    #(adagio 72)
    #(lento 60)
    #(largo 48)
    #(grave 36)))

(defun bpms ()
  (list-comp ((<- `#(,k ,v) (tempos))) `#(,v ,k)))

(defun to-bpm (name)
  (proplists:get_value name (tempos)))

(defun from-bpm (bpm)
  (proplists:get_value bpm (bpms)))

#| To get the nearest tempo for a given BPM, did the following in the LFE REPL:

lfe> (set bpms (list-comp ((<- `#(,k ,v) (tempos))) v)))
(200 184 166 138 116 108 92 72 60 48 36)
lfe> (list-comp ((<- `#(,hi ,lo) (lists:zip bpms (cdr bpms) 'trim))) (round (+ lo (/ (- hi lo) 2))))
(192 175 152 127 112 100 82 66 54 42)

|#
(defun nearest-tempo
  ((bpm) (when (=< bpm 42))
   'grave)
  ((bpm) (when (=< bpm 54))
   'largo)
  ((bpm) (when (=< bpm 66))
   'lento)
  ((bpm) (when (=< bpm 82))
   'adagio)
  ((bpm) (when (=< bpm 100))
   'andante)
  ((bpm) (when (=< bpm 112))
   'moderato)
  ((bpm) (when (=< bpm 127))
   'allegretto)
  ((bpm) (when (=< bpm 152))
   'allegro)
  ((bpm) (when (=< bpm 175))
   'vivace)
  ((bpm) (when (=< bpm 199))
   'presto)
  ((bpm) (when (> bpm 199))
   'prestissimo))

(defun fuzzy-bpm (name)
  (fuzzy-bpm name 5))

(defun fuzzy-bpm
  ((name variance) (when (is_atom name))
   (fuzzy-bpm (to-bpm name) variance))
  ((bpm variance)
   (round (rand:normal bpm variance))))
