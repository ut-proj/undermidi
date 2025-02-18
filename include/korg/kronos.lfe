(defun -bank-lookup ()
  #m("INT-A" (0 0)
     "INT-B" (0 1)
     "INT-C" (0 2)
     "INT-D" (0 3)
     "INT-E" (0 4)
     "INT-F" (0 5)
     "INT-G" (0 6)
     ;;
     "GM" (79 0)
     "g(1)" (79 1)
     "g(2)" (79 2)
     "g(3)" (79 3)
     "g(4)" (79 4)
     "g(5)" (79 5)
     "g(6)" (79 6)
     "g(7)" (79 7)
     "g(8)" (79 8)
     "g(9)" (79 9)
     "g(d)" (78 0)
     ;;
     "USER-A" (0 8)
     "USER-B" (0 9)
     "USER-C" (0 #x0a)
     "USER-D" (0 #x0b)
     "USER-E" (0 #x0c)
     "USER-F" (0 #x0d)
     "USER-G" (0 #x0e)
     ;;
     "USER-AA" (0 #x0f)
     "USER-BB" (0 #x10)
     "USER-CC" (0 #x11)
     "USER-DD" (0 #x12)
     "USER-EE" (0 #x13)
     "USER-FF" (0 #x14)
     "USER-GG" (0 #x15)))

;; Note that in order for bank selection to work on the Korg (in particular,
;; using the Kronos for testing), the "Prog" bank/button needs to be selected
;; on the face of the synthesizer.
(defun bank-select (program)
  (um:program-change program))

(defun bank-select (bank program)
  (um:bank-select (mref (-bank-lookup) bank) program))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: korg/kronos --| ()
  'ok)
