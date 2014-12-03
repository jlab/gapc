
          ********************************************
          *               Intloop21                  *
          *                                          *
          *    Special thermodynamic parameters      *
          *     for 2x1 and 1x2 internal loops       *
          ********************************************

> module Intloop21 where

> import Foldingspace
> import Intloop
> import Data.Array

Data arrangement: internal loop formed by b,e framed by pairs (a,f) and (c,d)

     5'.... a b   c.......
            |     |      . 
     3'.... g f e d ......

      ___________
     |    _      |
     |   | |     |
     a b c d e f g 

> il12_energy:: Num a => RNAInput -> Int -> Int -> a
> il12_energy inp lb rb = intloop21 (inp!(lb))         (inp!(lb+1))      (inp!(lb+2))
>				       (inp!(rb-3)) (inp!(rb-2)) (inp!(rb-1)) (inp!(rb))

> il21_energy:: Num a => RNAInput -> Int -> Int -> a
> il21_energy inp lb rb = intloop21 (inp!(rb-2)) (inp!(rb-1)) (inp!(rb))
>				       (inp!(lb)) (inp!(lb+1)) (inp!(lb+2)) (inp!(lb+3))

> intloop21 :: Num a => Ebase -> Ebase -> Ebase -> Ebase -> Ebase -> Ebase -> Ebase -> a
> intloop21 C A G C A A G = 240
> intloop21 C A G C A C G = 220
> intloop21 C A G C A G G = 160
> intloop21 C A G C A U G = 400

> intloop21 C A G C C A G = 210
> intloop21 C A G C C C G = 170
> intloop21 C A G C C G G = 160
> intloop21 C A G C C U G = 400

> intloop21 C A G C G A G = 100
> intloop21 C A G C G C G = 60
> intloop21 C A G C G G G = 40
> intloop21 C A G C G U G = 400

> intloop21 C A G C U A G = 400
> intloop21 C A G C U C G = 400
> intloop21 C A G C U G G = 400
> intloop21 C A G C U U G = 400


> intloop21 C C G C A A G = 230
> intloop21 C C G C A C G = 220
> intloop21 C C G C A G G = 400
> intloop21 C C G C A U G = 220

> intloop21 C C G C C A G = 220
> intloop21 C C G C C C G = 250
> intloop21 C C G C C G G = 400
> intloop21 C C G C C U G = 220

> intloop21 C C G C G A G = 400
> intloop21 C C G C G C G = 400
> intloop21 C C G C G G G = 400
> intloop21 C C G C G U G = 400

> intloop21 C C G C U A G = 250
> intloop21 C C G C U C G = 190
> intloop21 C C G C U G G = 400
> intloop21 C C G C U U G = 220


> intloop21 C G G C A A G = 170
> intloop21 C G G C A C G = 400
> intloop21 C G G C A G G = 80
> intloop21 C G G C A U G = 400

> intloop21 C G G C C A G = 400
> intloop21 C G G C C C G = 400
> intloop21 C G G C C G G = 400
> intloop21 C G G C C U G = 400

> intloop21 C G G C G A G = 80
> intloop21 C G G C G C G = 400
> intloop21 C G G C G G G = 220
> intloop21 C G G C G U G = 400

> intloop21 C G G C U A G = 400
> intloop21 C G G C U C G = 400
> intloop21 C G G C U G G = 400
> intloop21 C G G C U U G = 400


> intloop21 C U G C A A G = 400
> intloop21 C U G C A C G = 400
> intloop21 C U G C A G G = 400
> intloop21 C U G C A U G = 400

> intloop21 C U G C C A G = 400
> intloop21 C U G C C C G = 220
> intloop21 C U G C C G G = 400
> intloop21 C U G C C U G = 130

> intloop21 C U G C G A G = 400
> intloop21 C U G C G C G = 400
> intloop21 C U G C G G G = 400
> intloop21 C U G C G U G = 400

> intloop21 C U G C U A G = 400
> intloop21 C U G C U C G = 170
> intloop21 C U G C U G G = 400
> intloop21 C U G C U U G = 120


> intloop21 C A C G A A G = 230
> intloop21 C A C G A C G = 220
> intloop21 C A C G A G G = 110
> intloop21 C A C G A U G = 400

> intloop21 C A C G C A G = 210
> intloop21 C A C G C C G = 170
> intloop21 C A C G C G G = 160
> intloop21 C A C G C U G = 400

> intloop21 C A C G G A G = 80
> intloop21 C A C G G C G = 60
> intloop21 C A C G G G G = 40
> intloop21 C A C G G U G = 400

> intloop21 C A C G U A G = 400
> intloop21 C A C G U C G = 400
> intloop21 C A C G U G G = 400
> intloop21 C A C G U U G = 400


> intloop21 C C C G A A G = 230
> intloop21 C C C G A C G = 220
> intloop21 C C C G A G G = 400
> intloop21 C C C G A U G = 220

> intloop21 C C C G C A G = 220
> intloop21 C C C G C C G = 250
> intloop21 C C C G C G G = 400
> intloop21 C C C G C U G = 220

> intloop21 C C C G G A G = 400
> intloop21 C C C G G C G = 400
> intloop21 C C C G G G G = 400
> intloop21 C C C G G U G = 400

> intloop21 C C C G U A G = 250
> intloop21 C C C G U C G = 190
> intloop21 C C C G U G G = 400
> intloop21 C C C G U U G = 220


> intloop21 C G C G A A G = 170
> intloop21 C G C G A C G = 400
> intloop21 C G C G A G G = 80
> intloop21 C G C G A U G = 400

> intloop21 C G C G C A G = 400
> intloop21 C G C G C C G = 400
> intloop21 C G C G C G G = 400
> intloop21 C G C G C U G = 400

> intloop21 C G C G G A G = 80
> intloop21 C G C G G C G = 400
> intloop21 C G C G G G G = 220
> intloop21 C G C G G U G = 400

> intloop21 C G C G U A G = 400
> intloop21 C G C G U C G = 400
> intloop21 C G C G U G G = 400
> intloop21 C G C G U U G = 400


> intloop21 C U C G A A G = 400
> intloop21 C U C G A C G = 400
> intloop21 C U C G A G G = 400
> intloop21 C U C G A U G = 400

> intloop21 C U C G C A G = 400
> intloop21 C U C G C C G = 220
> intloop21 C U C G C G G = 400
> intloop21 C U C G C U G = 150

> intloop21 C U C G G A G = 400
> intloop21 C U C G G C G = 400
> intloop21 C U C G G G G = 400
> intloop21 C U C G G U G = 400

> intloop21 C U C G U A G = 400
> intloop21 C U C G U C G = 170
> intloop21 C U C G U G G = 400
> intloop21 C U C G U U G = 120


> intloop21 C A U G A A G = 320
> intloop21 C A U G A C G = 300
> intloop21 C A U G A G G = 240
> intloop21 C A U G A U G = 480

> intloop21 C A U G C A G = 290
> intloop21 C A U G C C G = 250
> intloop21 C A U G C G G = 240
> intloop21 C A U G C U G = 480

> intloop21 C A U G G A G = 180
> intloop21 C A U G G C G = 140
> intloop21 C A U G G G G = 120
> intloop21 C A U G G U G = 480

> intloop21 C A U G U A G = 480
> intloop21 C A U G U C G = 480
> intloop21 C A U G U G G = 480
> intloop21 C A U G U U G = 480


> intloop21 C C U G A A G = 310
> intloop21 C C U G A C G = 300
> intloop21 C C U G A G G = 480
> intloop21 C C U G A U G = 300

> intloop21 C C U G C A G = 300
> intloop21 C C U G C C G = 330
> intloop21 C C U G C G G = 480
> intloop21 C C U G C U G = 300

> intloop21 C C U G G A G = 480
> intloop21 C C U G G C G = 480
> intloop21 C C U G G G G = 480
> intloop21 C C U G G U G = 480

> intloop21 C C U G U A G = 330
> intloop21 C C U G U C G = 270
> intloop21 C C U G U G G = 480
> intloop21 C C U G U U G = 300


> intloop21 C G U G A A G = 250
> intloop21 C G U G A C G = 480
> intloop21 C G U G A G G = 160
> intloop21 C G U G A U G = 480

> intloop21 C G U G C A G = 480
> intloop21 C G U G C C G = 480
> intloop21 C G U G C G G = 480
> intloop21 C G U G C U G = 480

> intloop21 C G U G G A G = 160
> intloop21 C G U G G C G = 480
> intloop21 C G U G G G G = 300
> intloop21 C G U G G U G = 480

> intloop21 C G U G U A G = 480
> intloop21 C G U G U C G = 480
> intloop21 C G U G U G G = 480
> intloop21 C G U G U U G = 480


> intloop21 C U U G A A G = 480
> intloop21 C U U G A C G = 480
> intloop21 C U U G A G G = 480
> intloop21 C U U G A U G = 480

> intloop21 C U U G C A G = 480
> intloop21 C U U G C C G = 300
> intloop21 C U U G C G G = 480
> intloop21 C U U G C U G = 210

> intloop21 C U U G G A G = 480
> intloop21 C U U G G C G = 480
> intloop21 C U U G G G G = 480
> intloop21 C U U G G U G = 480

> intloop21 C U U G U A G = 480
> intloop21 C U U G U C G = 250
> intloop21 C U U G U G G = 480
> intloop21 C U U G U U G = 200


> intloop21 C A G U A A G = 320
> intloop21 C A G U A C G = 300
> intloop21 C A G U A G G = 240
> intloop21 C A G U A U G = 480

> intloop21 C A G U C A G = 290
> intloop21 C A G U C C G = 250
> intloop21 C A G U C G G = 240
> intloop21 C A G U C U G = 480

> intloop21 C A G U G A G = 180
> intloop21 C A G U G C G = 140
> intloop21 C A G U G G G = 120
> intloop21 C A G U G U G = 480

> intloop21 C A G U U A G = 480
> intloop21 C A G U U C G = 480
> intloop21 C A G U U G G = 480
> intloop21 C A G U U U G = 480


> intloop21 C C G U A A G = 310
> intloop21 C C G U A C G = 300
> intloop21 C C G U A G G = 480
> intloop21 C C G U A U G = 300

> intloop21 C C G U C A G = 300
> intloop21 C C G U C C G = 330
> intloop21 C C G U C G G = 480
> intloop21 C C G U C U G = 300

> intloop21 C C G U G A G = 480
> intloop21 C C G U G C G = 480
> intloop21 C C G U G G G = 480
> intloop21 C C G U G U G = 480

> intloop21 C C G U U A G = 330
> intloop21 C C G U U C G = 270
> intloop21 C C G U U G G = 480
> intloop21 C C G U U U G = 300


> intloop21 C G G U A A G = 250
> intloop21 C G G U A C G = 480
> intloop21 C G G U A G G = 160
> intloop21 C G G U A U G = 480

> intloop21 C G G U C A G = 480
> intloop21 C G G U C C G = 480
> intloop21 C G G U C G G = 480
> intloop21 C G G U C U G = 480

> intloop21 C G G U G A G = 160
> intloop21 C G G U G C G = 480
> intloop21 C G G U G G G = 300
> intloop21 C G G U G U G = 480

> intloop21 C G G U U A G = 480
> intloop21 C G G U U C G = 480
> intloop21 C G G U U G G = 480
> intloop21 C G G U U U G = 480


> intloop21 C U G U A A G = 480
> intloop21 C U G U A C G = 480
> intloop21 C U G U A G G = 480
> intloop21 C U G U A U G = 480

> intloop21 C U G U C A G = 480
> intloop21 C U G U C C G = 300
> intloop21 C U G U C G G = 480
> intloop21 C U G U C U G = 210

> intloop21 C U G U G A G = 480
> intloop21 C U G U G C G = 480
> intloop21 C U G U G G G = 480
> intloop21 C U G U G U G = 480

> intloop21 C U G U U A G = 480
> intloop21 C U G U U C G = 250
> intloop21 C U G U U G G = 480
> intloop21 C U G U U U G = 200


> intloop21 C A U A A A G = 320
> intloop21 C A U A A C G = 300
> intloop21 C A U A A G G = 240
> intloop21 C A U A A U G = 480

> intloop21 C A U A C A G = 290
> intloop21 C A U A C C G = 250
> intloop21 C A U A C G G = 240
> intloop21 C A U A C U G = 480

> intloop21 C A U A G A G = 180
> intloop21 C A U A G C G = 140
> intloop21 C A U A G G G = 120
> intloop21 C A U A G U G = 480

> intloop21 C A U A U A G = 480
> intloop21 C A U A U C G = 480
> intloop21 C A U A U G G = 480
> intloop21 C A U A U U G = 480


> intloop21 C C U A A A G = 310
> intloop21 C C U A A C G = 300
> intloop21 C C U A A G G = 480
> intloop21 C C U A A U G = 300

> intloop21 C C U A C A G = 300
> intloop21 C C U A C C G = 330
> intloop21 C C U A C G G = 480
> intloop21 C C U A C U G = 300

> intloop21 C C U A G A G = 480
> intloop21 C C U A G C G = 480
> intloop21 C C U A G G G = 480
> intloop21 C C U A G U G = 480

> intloop21 C C U A U A G = 330
> intloop21 C C U A U C G = 270
> intloop21 C C U A U G G = 480
> intloop21 C C U A U U G = 300


> intloop21 C G U A A A G = 250
> intloop21 C G U A A C G = 480
> intloop21 C G U A A G G = 160
> intloop21 C G U A A U G = 480

> intloop21 C G U A C A G = 480
> intloop21 C G U A C C G = 480
> intloop21 C G U A C G G = 480
> intloop21 C G U A C U G = 480

> intloop21 C G U A G A G = 160
> intloop21 C G U A G C G = 480
> intloop21 C G U A G G G = 300
> intloop21 C G U A G U G = 480

> intloop21 C G U A U A G = 480
> intloop21 C G U A U C G = 480
> intloop21 C G U A U G G = 480
> intloop21 C G U A U U G = 480


> intloop21 C U U A A A G = 480
> intloop21 C U U A A C G = 480
> intloop21 C U U A A G G = 480
> intloop21 C U U A A U G = 480

> intloop21 C U U A C A G = 480
> intloop21 C U U A C C G = 300
> intloop21 C U U A C G G = 480
> intloop21 C U U A C U G = 210

> intloop21 C U U A G A G = 480
> intloop21 C U U A G C G = 480
> intloop21 C U U A G G G = 480
> intloop21 C U U A G U G = 480

> intloop21 C U U A U A G = 480
> intloop21 C U U A U C G = 250
> intloop21 C U U A U G G = 480
> intloop21 C U U A U U G = 200


> intloop21 C A A U A A G = 320
> intloop21 C A A U A C G = 300
> intloop21 C A A U A G G = 240
> intloop21 C A A U A U G = 480

> intloop21 C A A U C A G = 290
> intloop21 C A A U C C G = 250
> intloop21 C A A U C G G = 240
> intloop21 C A A U C U G = 480

> intloop21 C A A U G A G = 180
> intloop21 C A A U G C G = 140
> intloop21 C A A U G G G = 120
> intloop21 C A A U G U G = 480

> intloop21 C A A U U A G = 480
> intloop21 C A A U U C G = 480
> intloop21 C A A U U G G = 480
> intloop21 C A A U U U G = 480


> intloop21 C C A U A A G = 310
> intloop21 C C A U A C G = 300
> intloop21 C C A U A G G = 480
> intloop21 C C A U A U G = 300

> intloop21 C C A U C A G = 300
> intloop21 C C A U C C G = 330
> intloop21 C C A U C G G = 480
> intloop21 C C A U C U G = 300

> intloop21 C C A U G A G = 480
> intloop21 C C A U G C G = 480
> intloop21 C C A U G G G = 480
> intloop21 C C A U G U G = 480

> intloop21 C C A U U A G = 330
> intloop21 C C A U U C G = 270
> intloop21 C C A U U G G = 480
> intloop21 C C A U U U G = 300


> intloop21 C G A U A A G = 250
> intloop21 C G A U A C G = 480
> intloop21 C G A U A G G = 160
> intloop21 C G A U A U G = 480

> intloop21 C G A U C A G = 480
> intloop21 C G A U C C G = 480
> intloop21 C G A U C G G = 480
> intloop21 C G A U C U G = 480

> intloop21 C G A U G A G = 160
> intloop21 C G A U G C G = 480
> intloop21 C G A U G G G = 300
> intloop21 C G A U G U G = 480

> intloop21 C G A U U A G = 480
> intloop21 C G A U U C G = 480
> intloop21 C G A U U G G = 480
> intloop21 C G A U U U G = 480


> intloop21 C U A U A A G = 480
> intloop21 C U A U A C G = 480
> intloop21 C U A U A G G = 480
> intloop21 C U A U A U G = 480

> intloop21 C U A U C A G = 480
> intloop21 C U A U C C G = 300
> intloop21 C U A U C G G = 480
> intloop21 C U A U C U G = 210

> intloop21 C U A U G A G = 480
> intloop21 C U A U G C G = 480
> intloop21 C U A U G G G = 480
> intloop21 C U A U G U G = 480

> intloop21 C U A U U A G = 480
> intloop21 C U A U U C G = 250
> intloop21 C U A U U G G = 480
> intloop21 C U A U U U G = 200


> intloop21 G A G C A A C = 250
> intloop21 G A G C A C C = 220
> intloop21 G A G C A G C = 210
> intloop21 G A G C A U C = 400

> intloop21 G A G C C A C = 210
> intloop21 G A G C C C C = 170
> intloop21 G A G C C G C = 160
> intloop21 G A G C C U C = 400

> intloop21 G A G C G A C = 120
> intloop21 G A G C G C C = 60
> intloop21 G A G C G G C = 40
> intloop21 G A G C G U C = 400

> intloop21 G A G C U A C = 400
> intloop21 G A G C U C C = 400
> intloop21 G A G C U G C = 400
> intloop21 G A G C U U C = 400


> intloop21 G C G C A A C = 230
> intloop21 G C G C A C C = 220
> intloop21 G C G C A G C = 400
> intloop21 G C G C A U C = 220

> intloop21 G C G C C A C = 220
> intloop21 G C G C C C C = 250
> intloop21 G C G C C G C = 400
> intloop21 G C G C C U C = 220

> intloop21 G C G C G A C = 400
> intloop21 G C G C G C C = 400
> intloop21 G C G C G G C = 400
> intloop21 G C G C G U C = 400

> intloop21 G C G C U A C = 250
> intloop21 G C G C U C C = 190
> intloop21 G C G C U G C = 400
> intloop21 G C G C U U C = 220


> intloop21 G G G C A A C = 170
> intloop21 G G G C A C C = 400
> intloop21 G G G C A G C = 80
> intloop21 G G G C A U C = 400

> intloop21 G G G C C A C = 400
> intloop21 G G G C C C C = 400
> intloop21 G G G C C G C = 400
> intloop21 G G G C C U C = 400

> intloop21 G G G C G A C = 80
> intloop21 G G G C G C C = 400
> intloop21 G G G C G G C = 220
> intloop21 G G G C G U C = 400

> intloop21 G G G C U A C = 400
> intloop21 G G G C U C C = 400
> intloop21 G G G C U G C = 400
> intloop21 G G G C U U C = 400


> intloop21 G U G C A A C = 400
> intloop21 G U G C A C C = 400
> intloop21 G U G C A G C = 400
> intloop21 G U G C A U C = 400

> intloop21 G U G C C A C = 400
> intloop21 G U G C C C C = 220
> intloop21 G U G C C G C = 400
> intloop21 G U G C C U C = 120

> intloop21 G U G C G A C = 400
> intloop21 G U G C G C C = 400
> intloop21 G U G C G G C = 400
> intloop21 G U G C G U C = 400

> intloop21 G U G C U A C = 400
> intloop21 G U G C U C C = 170
> intloop21 G U G C U G C = 400
> intloop21 G U G C U U C = 120


> intloop21 G A C G A A C = 240
> intloop21 G A C G A C C = 220
> intloop21 G A C G A G C = 160
> intloop21 G A C G A U C = 400

> intloop21 G A C G C A C = 210
> intloop21 G A C G C C C = 170
> intloop21 G A C G C G C = 160
> intloop21 G A C G C U C = 400

> intloop21 G A C G G A C = 100
> intloop21 G A C G G C C = 60
> intloop21 G A C G G G C = 40
> intloop21 G A C G G U C = 400

> intloop21 G A C G U A C = 400
> intloop21 G A C G U C C = 400
> intloop21 G A C G U G C = 400
> intloop21 G A C G U U C = 400


> intloop21 G C C G A A C = 230
> intloop21 G C C G A C C = 220
> intloop21 G C C G A G C = 400
> intloop21 G C C G A U C = 220

> intloop21 G C C G C A C = 220
> intloop21 G C C G C C C = 250
> intloop21 G C C G C G C = 400
> intloop21 G C C G C U C = 220

> intloop21 G C C G G A C = 400
> intloop21 G C C G G C C = 400
> intloop21 G C C G G G C = 400
> intloop21 G C C G G U C = 400

> intloop21 G C C G U A C = 250
> intloop21 G C C G U C C = 190
> intloop21 G C C G U G C = 400
> intloop21 G C C G U U C = 220


> intloop21 G G C G A A C = 170
> intloop21 G G C G A C C = 400
> intloop21 G G C G A G C = 80
> intloop21 G G C G A U C = 400

> intloop21 G G C G C A C = 400
> intloop21 G G C G C C C = 400
> intloop21 G G C G C G C = 400
> intloop21 G G C G C U C = 400

> intloop21 G G C G G A C = 80
> intloop21 G G C G G C C = 400
> intloop21 G G C G G G C = 220
> intloop21 G G C G G U C = 400

> intloop21 G G C G U A C = 400
> intloop21 G G C G U C C = 400
> intloop21 G G C G U G C = 400
> intloop21 G G C G U U C = 400


> intloop21 G U C G A A C = 400
> intloop21 G U C G A C C = 400
> intloop21 G U C G A G C = 400
> intloop21 G U C G A U C = 400

> intloop21 G U C G C A C = 400
> intloop21 G U C G C C C = 220
> intloop21 G U C G C G C = 400
> intloop21 G U C G C U C = 130

> intloop21 G U C G G A C = 400
> intloop21 G U C G G C C = 400
> intloop21 G U C G G G C = 400
> intloop21 G U C G G U C = 400

> intloop21 G U C G U A C = 400
> intloop21 G U C G U C C = 170
> intloop21 G U C G U G C = 400
> intloop21 G U C G U U C = 120


> intloop21 G A U G A A C = 320
> intloop21 G A U G A C C = 300
> intloop21 G A U G A G C = 240
> intloop21 G A U G A U C = 480

> intloop21 G A U G C A C = 290
> intloop21 G A U G C C C = 250
> intloop21 G A U G C G C = 240
> intloop21 G A U G C U C = 480

> intloop21 G A U G G A C = 180
> intloop21 G A U G G C C = 140
> intloop21 G A U G G G C = 120
> intloop21 G A U G G U C = 480

> intloop21 G A U G U A C = 480
> intloop21 G A U G U C C = 480
> intloop21 G A U G U G C = 480
> intloop21 G A U G U U C = 480


> intloop21 G C U G A A C = 310
> intloop21 G C U G A C C = 300
> intloop21 G C U G A G C = 480
> intloop21 G C U G A U C = 300

> intloop21 G C U G C A C = 300
> intloop21 G C U G C C C = 330
> intloop21 G C U G C G C = 480
> intloop21 G C U G C U C = 300

> intloop21 G C U G G A C = 480
> intloop21 G C U G G C C = 480
> intloop21 G C U G G G C = 480
> intloop21 G C U G G U C = 480

> intloop21 G C U G U A C = 330
> intloop21 G C U G U C C = 270
> intloop21 G C U G U G C = 480
> intloop21 G C U G U U C = 300


> intloop21 G G U G A A C = 250
> intloop21 G G U G A C C = 480
> intloop21 G G U G A G C = 160
> intloop21 G G U G A U C = 480

> intloop21 G G U G C A C = 480
> intloop21 G G U G C C C = 480
> intloop21 G G U G C G C = 480
> intloop21 G G U G C U C = 480

> intloop21 G G U G G A C = 160
> intloop21 G G U G G C C = 480
> intloop21 G G U G G G C = 300
> intloop21 G G U G G U C = 480

> intloop21 G G U G U A C = 480
> intloop21 G G U G U C C = 480
> intloop21 G G U G U G C = 480
> intloop21 G G U G U U C = 480


> intloop21 G U U G A A C = 480
> intloop21 G U U G A C C = 480
> intloop21 G U U G A G C = 480
> intloop21 G U U G A U C = 480

> intloop21 G U U G C A C = 480
> intloop21 G U U G C C C = 300
> intloop21 G U U G C G C = 480
> intloop21 G U U G C U C = 210

> intloop21 G U U G G A C = 480
> intloop21 G U U G G C C = 480
> intloop21 G U U G G G C = 480
> intloop21 G U U G G U C = 480

> intloop21 G U U G U A C = 480
> intloop21 G U U G U C C = 250
> intloop21 G U U G U G C = 480
> intloop21 G U U G U U C = 200


> intloop21 G A G U A A C = 320
> intloop21 G A G U A C C = 300
> intloop21 G A G U A G C = 240
> intloop21 G A G U A U C = 480

> intloop21 G A G U C A C = 290
> intloop21 G A G U C C C = 250
> intloop21 G A G U C G C = 240
> intloop21 G A G U C U C = 480

> intloop21 G A G U G A C = 180
> intloop21 G A G U G C C = 140
> intloop21 G A G U G G C = 120
> intloop21 G A G U G U C = 480

> intloop21 G A G U U A C = 480
> intloop21 G A G U U C C = 480
> intloop21 G A G U U G C = 480
> intloop21 G A G U U U C = 480


> intloop21 G C G U A A C = 310
> intloop21 G C G U A C C = 300
> intloop21 G C G U A G C = 480
> intloop21 G C G U A U C = 300

> intloop21 G C G U C A C = 300
> intloop21 G C G U C C C = 330
> intloop21 G C G U C G C = 480
> intloop21 G C G U C U C = 300

> intloop21 G C G U G A C = 480
> intloop21 G C G U G C C = 480
> intloop21 G C G U G G C = 480
> intloop21 G C G U G U C = 480

> intloop21 G C G U U A C = 330
> intloop21 G C G U U C C = 270
> intloop21 G C G U U G C = 480
> intloop21 G C G U U U C = 300


> intloop21 G G G U A A C = 250
> intloop21 G G G U A C C = 480
> intloop21 G G G U A G C = 160
> intloop21 G G G U A U C = 480

> intloop21 G G G U C A C = 480
> intloop21 G G G U C C C = 480
> intloop21 G G G U C G C = 480
> intloop21 G G G U C U C = 480

> intloop21 G G G U G A C = 160
> intloop21 G G G U G C C = 480
> intloop21 G G G U G G C = 300
> intloop21 G G G U G U C = 480

> intloop21 G G G U U A C = 480
> intloop21 G G G U U C C = 480
> intloop21 G G G U U G C = 480
> intloop21 G G G U U U C = 480


> intloop21 G U G U A A C = 480
> intloop21 G U G U A C C = 480
> intloop21 G U G U A G C = 480
> intloop21 G U G U A U C = 480

> intloop21 G U G U C A C = 480
> intloop21 G U G U C C C = 300
> intloop21 G U G U C G C = 480
> intloop21 G U G U C U C = 210

> intloop21 G U G U G A C = 480
> intloop21 G U G U G C C = 480
> intloop21 G U G U G G C = 480
> intloop21 G U G U G U C = 480

> intloop21 G U G U U A C = 480
> intloop21 G U G U U C C = 250
> intloop21 G U G U U G C = 480
> intloop21 G U G U U U C = 200


> intloop21 G A U A A A C = 320
> intloop21 G A U A A C C = 300
> intloop21 G A U A A G C = 240
> intloop21 G A U A A U C = 480

> intloop21 G A U A C A C = 290
> intloop21 G A U A C C C = 250
> intloop21 G A U A C G C = 240
> intloop21 G A U A C U C = 480

> intloop21 G A U A G A C = 180
> intloop21 G A U A G C C = 140
> intloop21 G A U A G G C = 120
> intloop21 G A U A G U C = 480

> intloop21 G A U A U A C = 480
> intloop21 G A U A U C C = 480
> intloop21 G A U A U G C = 480
> intloop21 G A U A U U C = 480


> intloop21 G C U A A A C = 310
> intloop21 G C U A A C C = 300
> intloop21 G C U A A G C = 480
> intloop21 G C U A A U C = 300

> intloop21 G C U A C A C = 300
> intloop21 G C U A C C C = 330
> intloop21 G C U A C G C = 480
> intloop21 G C U A C U C = 300

> intloop21 G C U A G A C = 480
> intloop21 G C U A G C C = 480
> intloop21 G C U A G G C = 480
> intloop21 G C U A G U C = 480

> intloop21 G C U A U A C = 330
> intloop21 G C U A U C C = 270
> intloop21 G C U A U G C = 480
> intloop21 G C U A U U C = 300


> intloop21 G G U A A A C = 250
> intloop21 G G U A A C C = 480
> intloop21 G G U A A G C = 160
> intloop21 G G U A A U C = 480

> intloop21 G G U A C A C = 480
> intloop21 G G U A C C C = 480
> intloop21 G G U A C G C = 480
> intloop21 G G U A C U C = 480

> intloop21 G G U A G A C = 160
> intloop21 G G U A G C C = 480
> intloop21 G G U A G G C = 300
> intloop21 G G U A G U C = 480

> intloop21 G G U A U A C = 480
> intloop21 G G U A U C C = 480
> intloop21 G G U A U G C = 480
> intloop21 G G U A U U C = 480


> intloop21 G U U A A A C = 480
> intloop21 G U U A A C C = 480
> intloop21 G U U A A G C = 480
> intloop21 G U U A A U C = 480

> intloop21 G U U A C A C = 480
> intloop21 G U U A C C C = 300
> intloop21 G U U A C G C = 480
> intloop21 G U U A C U C = 210

> intloop21 G U U A G A C = 480
> intloop21 G U U A G C C = 480
> intloop21 G U U A G G C = 480
> intloop21 G U U A G U C = 480

> intloop21 G U U A U A C = 480
> intloop21 G U U A U C C = 250
> intloop21 G U U A U G C = 480
> intloop21 G U U A U U C = 200


> intloop21 G A A U A A C = 320
> intloop21 G A A U A C C = 300
> intloop21 G A A U A G C = 240
> intloop21 G A A U A U C = 480

> intloop21 G A A U C A C = 290
> intloop21 G A A U C C C = 250
> intloop21 G A A U C G C = 240
> intloop21 G A A U C U C = 480

> intloop21 G A A U G A C = 180
> intloop21 G A A U G C C = 140
> intloop21 G A A U G G C = 120
> intloop21 G A A U G U C = 480

> intloop21 G A A U U A C = 480
> intloop21 G A A U U C C = 480
> intloop21 G A A U U G C = 480
> intloop21 G A A U U U C = 480


> intloop21 G C A U A A C = 310
> intloop21 G C A U A C C = 300
> intloop21 G C A U A G C = 480
> intloop21 G C A U A U C = 300

> intloop21 G C A U C A C = 300
> intloop21 G C A U C C C = 330
> intloop21 G C A U C G C = 480
> intloop21 G C A U C U C = 300

> intloop21 G C A U G A C = 480
> intloop21 G C A U G C C = 480
> intloop21 G C A U G G C = 480
> intloop21 G C A U G U C = 480

> intloop21 G C A U U A C = 330
> intloop21 G C A U U C C = 270
> intloop21 G C A U U G C = 480
> intloop21 G C A U U U C = 300


> intloop21 G G A U A A C = 250
> intloop21 G G A U A C C = 480
> intloop21 G G A U A G C = 160
> intloop21 G G A U A U C = 480

> intloop21 G G A U C A C = 480
> intloop21 G G A U C C C = 480
> intloop21 G G A U C G C = 480
> intloop21 G G A U C U C = 480

> intloop21 G G A U G A C = 160
> intloop21 G G A U G C C = 480
> intloop21 G G A U G G C = 300
> intloop21 G G A U G U C = 480

> intloop21 G G A U U A C = 480
> intloop21 G G A U U C C = 480
> intloop21 G G A U U G C = 480
> intloop21 G G A U U U C = 480


> intloop21 G U A U A A C = 480
> intloop21 G U A U A C C = 480
> intloop21 G U A U A G C = 480
> intloop21 G U A U A U C = 480

> intloop21 G U A U C A C = 480
> intloop21 G U A U C C C = 300
> intloop21 G U A U C G C = 480
> intloop21 G U A U C U C = 210

> intloop21 G U A U G A C = 480
> intloop21 G U A U G C C = 480
> intloop21 G U A U G G C = 480
> intloop21 G U A U G U C = 480

> intloop21 G U A U U A C = 480
> intloop21 G U A U U C C = 250
> intloop21 G U A U U G C = 480
> intloop21 G U A U U U C = 200


> intloop21 G A G C A A U = 320
> intloop21 G A G C A C U = 300
> intloop21 G A G C A G U = 240
> intloop21 G A G C A U U = 480

> intloop21 G A G C C A U = 290
> intloop21 G A G C C C U = 250
> intloop21 G A G C C G U = 240
> intloop21 G A G C C U U = 480

> intloop21 G A G C G A U = 180
> intloop21 G A G C G C U = 140
> intloop21 G A G C G G U = 120
> intloop21 G A G C G U U = 480

> intloop21 G A G C U A U = 480
> intloop21 G A G C U C U = 480
> intloop21 G A G C U G U = 480
> intloop21 G A G C U U U = 480


> intloop21 G C G C A A U = 310
> intloop21 G C G C A C U = 300
> intloop21 G C G C A G U = 480
> intloop21 G C G C A U U = 300

> intloop21 G C G C C A U = 300
> intloop21 G C G C C C U = 330
> intloop21 G C G C C G U = 480
> intloop21 G C G C C U U = 300

> intloop21 G C G C G A U = 480
> intloop21 G C G C G C U = 480
> intloop21 G C G C G G U = 480
> intloop21 G C G C G U U = 480

> intloop21 G C G C U A U = 330
> intloop21 G C G C U C U = 270
> intloop21 G C G C U G U = 480
> intloop21 G C G C U U U = 300


> intloop21 G G G C A A U = 250
> intloop21 G G G C A C U = 480
> intloop21 G G G C A G U = 160
> intloop21 G G G C A U U = 480

> intloop21 G G G C C A U = 480
> intloop21 G G G C C C U = 480
> intloop21 G G G C C G U = 480
> intloop21 G G G C C U U = 480

> intloop21 G G G C G A U = 160
> intloop21 G G G C G C U = 480
> intloop21 G G G C G G U = 300
> intloop21 G G G C G U U = 480

> intloop21 G G G C U A U = 480
> intloop21 G G G C U C U = 480
> intloop21 G G G C U G U = 480
> intloop21 G G G C U U U = 480


> intloop21 G U G C A A U = 480
> intloop21 G U G C A C U = 480
> intloop21 G U G C A G U = 480
> intloop21 G U G C A U U = 480

> intloop21 G U G C C A U = 480
> intloop21 G U G C C C U = 300
> intloop21 G U G C C G U = 480
> intloop21 G U G C C U U = 210

> intloop21 G U G C G A U = 480
> intloop21 G U G C G C U = 480
> intloop21 G U G C G G U = 480
> intloop21 G U G C G U U = 480

> intloop21 G U G C U A U = 480
> intloop21 G U G C U C U = 250
> intloop21 G U G C U G U = 480
> intloop21 G U G C U U U = 200


> intloop21 G A C G A A U = 320
> intloop21 G A C G A C U = 300
> intloop21 G A C G A G U = 240
> intloop21 G A C G A U U = 480

> intloop21 G A C G C A U = 290
> intloop21 G A C G C C U = 250
> intloop21 G A C G C G U = 240
> intloop21 G A C G C U U = 480

> intloop21 G A C G G A U = 180
> intloop21 G A C G G C U = 140
> intloop21 G A C G G G U = 120
> intloop21 G A C G G U U = 480

> intloop21 G A C G U A U = 480
> intloop21 G A C G U C U = 480
> intloop21 G A C G U G U = 480
> intloop21 G A C G U U U = 480


> intloop21 G C C G A A U = 310
> intloop21 G C C G A C U = 300
> intloop21 G C C G A G U = 480
> intloop21 G C C G A U U = 300

> intloop21 G C C G C A U = 300
> intloop21 G C C G C C U = 330
> intloop21 G C C G C G U = 480
> intloop21 G C C G C U U = 300

> intloop21 G C C G G A U = 480
> intloop21 G C C G G C U = 480
> intloop21 G C C G G G U = 480
> intloop21 G C C G G U U = 480

> intloop21 G C C G U A U = 330
> intloop21 G C C G U C U = 270
> intloop21 G C C G U G U = 480
> intloop21 G C C G U U U = 300


> intloop21 G G C G A A U = 250
> intloop21 G G C G A C U = 480
> intloop21 G G C G A G U = 160
> intloop21 G G C G A U U = 480

> intloop21 G G C G C A U = 480
> intloop21 G G C G C C U = 480
> intloop21 G G C G C G U = 480
> intloop21 G G C G C U U = 480

> intloop21 G G C G G A U = 160
> intloop21 G G C G G C U = 480
> intloop21 G G C G G G U = 300
> intloop21 G G C G G U U = 480

> intloop21 G G C G U A U = 480
> intloop21 G G C G U C U = 480
> intloop21 G G C G U G U = 480
> intloop21 G G C G U U U = 480


> intloop21 G U C G A A U = 480
> intloop21 G U C G A C U = 480
> intloop21 G U C G A G U = 480
> intloop21 G U C G A U U = 480

> intloop21 G U C G C A U = 480
> intloop21 G U C G C C U = 300
> intloop21 G U C G C G U = 480
> intloop21 G U C G C U U = 210

> intloop21 G U C G G A U = 480
> intloop21 G U C G G C U = 480
> intloop21 G U C G G G U = 480
> intloop21 G U C G G U U = 480

> intloop21 G U C G U A U = 480
> intloop21 G U C G U C U = 250
> intloop21 G U C G U G U = 480
> intloop21 G U C G U U U = 200


> intloop21 G A U G A A U = 390
> intloop21 G A U G A C U = 370
> intloop21 G A U G A G U = 310
> intloop21 G A U G A U U = 550

> intloop21 G A U G C A U = 360
> intloop21 G A U G C C U = 320
> intloop21 G A U G C G U = 310
> intloop21 G A U G C U U = 550

> intloop21 G A U G G A U = 250
> intloop21 G A U G G C U = 210
> intloop21 G A U G G G U = 190
> intloop21 G A U G G U U = 550

> intloop21 G A U G U A U = 550
> intloop21 G A U G U C U = 550
> intloop21 G A U G U G U = 550
> intloop21 G A U G U U U = 550


> intloop21 G C U G A A U = 380
> intloop21 G C U G A C U = 370
> intloop21 G C U G A G U = 550
> intloop21 G C U G A U U = 370

> intloop21 G C U G C A U = 370
> intloop21 G C U G C C U = 400
> intloop21 G C U G C G U = 550
> intloop21 G C U G C U U = 370

> intloop21 G C U G G A U = 550
> intloop21 G C U G G C U = 550
> intloop21 G C U G G G U = 550
> intloop21 G C U G G U U = 550

> intloop21 G C U G U A U = 400
> intloop21 G C U G U C U = 340
> intloop21 G C U G U G U = 550
> intloop21 G C U G U U U = 370


> intloop21 G G U G A A U = 320
> intloop21 G G U G A C U = 550
> intloop21 G G U G A G U = 230
> intloop21 G G U G A U U = 550

> intloop21 G G U G C A U = 550
> intloop21 G G U G C C U = 550
> intloop21 G G U G C G U = 550
> intloop21 G G U G C U U = 550

> intloop21 G G U G G A U = 230
> intloop21 G G U G G C U = 550
> intloop21 G G U G G G U = 370
> intloop21 G G U G G U U = 550

> intloop21 G G U G U A U = 550
> intloop21 G G U G U C U = 550
> intloop21 G G U G U G U = 550
> intloop21 G G U G U U U = 550


> intloop21 G U U G A A U = 550
> intloop21 G U U G A C U = 550
> intloop21 G U U G A G U = 550
> intloop21 G U U G A U U = 550

> intloop21 G U U G C A U = 550
> intloop21 G U U G C C U = 370
> intloop21 G U U G C G U = 550
> intloop21 G U U G C U U = 280

> intloop21 G U U G G A U = 550
> intloop21 G U U G G C U = 550
> intloop21 G U U G G G U = 550
> intloop21 G U U G G U U = 550

> intloop21 G U U G U A U = 550
> intloop21 G U U G U C U = 320
> intloop21 G U U G U G U = 550
> intloop21 G U U G U U U = 270


> intloop21 G A G U A A U = 390
> intloop21 G A G U A C U = 370
> intloop21 G A G U A G U = 310
> intloop21 G A G U A U U = 550

> intloop21 G A G U C A U = 360
> intloop21 G A G U C C U = 320
> intloop21 G A G U C G U = 310
> intloop21 G A G U C U U = 550

> intloop21 G A G U G A U = 250
> intloop21 G A G U G C U = 210
> intloop21 G A G U G G U = 190
> intloop21 G A G U G U U = 550

> intloop21 G A G U U A U = 550
> intloop21 G A G U U C U = 550
> intloop21 G A G U U G U = 550
> intloop21 G A G U U U U = 550


> intloop21 G C G U A A U = 380
> intloop21 G C G U A C U = 370
> intloop21 G C G U A G U = 550
> intloop21 G C G U A U U = 370

> intloop21 G C G U C A U = 370
> intloop21 G C G U C C U = 400
> intloop21 G C G U C G U = 550
> intloop21 G C G U C U U = 370

> intloop21 G C G U G A U = 550
> intloop21 G C G U G C U = 550
> intloop21 G C G U G G U = 550
> intloop21 G C G U G U U = 550

> intloop21 G C G U U A U = 400
> intloop21 G C G U U C U = 340
> intloop21 G C G U U G U = 550
> intloop21 G C G U U U U = 370


> intloop21 G G G U A A U = 320
> intloop21 G G G U A C U = 550
> intloop21 G G G U A G U = 230
> intloop21 G G G U A U U = 550

> intloop21 G G G U C A U = 550
> intloop21 G G G U C C U = 550
> intloop21 G G G U C G U = 550
> intloop21 G G G U C U U = 550

> intloop21 G G G U G A U = 230
> intloop21 G G G U G C U = 550
> intloop21 G G G U G G U = 370
> intloop21 G G G U G U U = 550

> intloop21 G G G U U A U = 550
> intloop21 G G G U U C U = 550
> intloop21 G G G U U G U = 550
> intloop21 G G G U U U U = 550


> intloop21 G U G U A A U = 550
> intloop21 G U G U A C U = 550
> intloop21 G U G U A G U = 550
> intloop21 G U G U A U U = 550

> intloop21 G U G U C A U = 550
> intloop21 G U G U C C U = 370
> intloop21 G U G U C G U = 550
> intloop21 G U G U C U U = 280

> intloop21 G U G U G A U = 550
> intloop21 G U G U G C U = 550
> intloop21 G U G U G G U = 550
> intloop21 G U G U G U U = 550

> intloop21 G U G U U A U = 550
> intloop21 G U G U U C U = 320
> intloop21 G U G U U G U = 550
> intloop21 G U G U U U U = 270


> intloop21 G A U A A A U = 390
> intloop21 G A U A A C U = 370
> intloop21 G A U A A G U = 310
> intloop21 G A U A A U U = 550

> intloop21 G A U A C A U = 360
> intloop21 G A U A C C U = 320
> intloop21 G A U A C G U = 310
> intloop21 G A U A C U U = 550

> intloop21 G A U A G A U = 250
> intloop21 G A U A G C U = 210
> intloop21 G A U A G G U = 190
> intloop21 G A U A G U U = 550

> intloop21 G A U A U A U = 550
> intloop21 G A U A U C U = 550
> intloop21 G A U A U G U = 550
> intloop21 G A U A U U U = 550


> intloop21 G C U A A A U = 380
> intloop21 G C U A A C U = 370
> intloop21 G C U A A G U = 550
> intloop21 G C U A A U U = 370

> intloop21 G C U A C A U = 370
> intloop21 G C U A C C U = 400
> intloop21 G C U A C G U = 550
> intloop21 G C U A C U U = 370

> intloop21 G C U A G A U = 550
> intloop21 G C U A G C U = 550
> intloop21 G C U A G G U = 550
> intloop21 G C U A G U U = 550

> intloop21 G C U A U A U = 400
> intloop21 G C U A U C U = 340
> intloop21 G C U A U G U = 550
> intloop21 G C U A U U U = 370


> intloop21 G G U A A A U = 320
> intloop21 G G U A A C U = 550
> intloop21 G G U A A G U = 230
> intloop21 G G U A A U U = 550

> intloop21 G G U A C A U = 550
> intloop21 G G U A C C U = 550
> intloop21 G G U A C G U = 550
> intloop21 G G U A C U U = 550

> intloop21 G G U A G A U = 230
> intloop21 G G U A G C U = 550
> intloop21 G G U A G G U = 370
> intloop21 G G U A G U U = 550

> intloop21 G G U A U A U = 550
> intloop21 G G U A U C U = 550
> intloop21 G G U A U G U = 550
> intloop21 G G U A U U U = 550


> intloop21 G U U A A A U = 550
> intloop21 G U U A A C U = 550
> intloop21 G U U A A G U = 550
> intloop21 G U U A A U U = 550

> intloop21 G U U A C A U = 550
> intloop21 G U U A C C U = 370
> intloop21 G U U A C G U = 550
> intloop21 G U U A C U U = 280

> intloop21 G U U A G A U = 550
> intloop21 G U U A G C U = 550
> intloop21 G U U A G G U = 550
> intloop21 G U U A G U U = 550

> intloop21 G U U A U A U = 550
> intloop21 G U U A U C U = 320
> intloop21 G U U A U G U = 550
> intloop21 G U U A U U U = 270


> intloop21 G A A U A A U = 390
> intloop21 G A A U A C U = 370
> intloop21 G A A U A G U = 310
> intloop21 G A A U A U U = 550

> intloop21 G A A U C A U = 360
> intloop21 G A A U C C U = 320
> intloop21 G A A U C G U = 310
> intloop21 G A A U C U U = 550

> intloop21 G A A U G A U = 250
> intloop21 G A A U G C U = 210
> intloop21 G A A U G G U = 190
> intloop21 G A A U G U U = 550

> intloop21 G A A U U A U = 550
> intloop21 G A A U U C U = 550
> intloop21 G A A U U G U = 550
> intloop21 G A A U U U U = 550


> intloop21 G C A U A A U = 380
> intloop21 G C A U A C U = 370
> intloop21 G C A U A G U = 550
> intloop21 G C A U A U U = 370

> intloop21 G C A U C A U = 370
> intloop21 G C A U C C U = 400
> intloop21 G C A U C G U = 550
> intloop21 G C A U C U U = 370

> intloop21 G C A U G A U = 550
> intloop21 G C A U G C U = 550
> intloop21 G C A U G G U = 550
> intloop21 G C A U G U U = 550

> intloop21 G C A U U A U = 400
> intloop21 G C A U U C U = 340
> intloop21 G C A U U G U = 550
> intloop21 G C A U U U U = 370


> intloop21 G G A U A A U = 320
> intloop21 G G A U A C U = 550
> intloop21 G G A U A G U = 230
> intloop21 G G A U A U U = 550

> intloop21 G G A U C A U = 550
> intloop21 G G A U C C U = 550
> intloop21 G G A U C G U = 550
> intloop21 G G A U C U U = 550

> intloop21 G G A U G A U = 230
> intloop21 G G A U G C U = 550
> intloop21 G G A U G G U = 370
> intloop21 G G A U G U U = 550

> intloop21 G G A U U A U = 550
> intloop21 G G A U U C U = 550
> intloop21 G G A U U G U = 550
> intloop21 G G A U U U U = 550


> intloop21 G U A U A A U = 550
> intloop21 G U A U A C U = 550
> intloop21 G U A U A G U = 550
> intloop21 G U A U A U U = 550

> intloop21 G U A U C A U = 550
> intloop21 G U A U C C U = 370
> intloop21 G U A U C G U = 550
> intloop21 G U A U C U U = 280

> intloop21 G U A U G A U = 550
> intloop21 G U A U G C U = 550
> intloop21 G U A U G G U = 550
> intloop21 G U A U G U U = 550

> intloop21 G U A U U A U = 550
> intloop21 G U A U U C U = 320
> intloop21 G U A U U G U = 550
> intloop21 G U A U U U U = 270


> intloop21 U A G C A A G = 320
> intloop21 U A G C A C G = 300
> intloop21 U A G C A G G = 240
> intloop21 U A G C A U G = 480

> intloop21 U A G C C A G = 290
> intloop21 U A G C C C G = 250
> intloop21 U A G C C G G = 240
> intloop21 U A G C C U G = 480

> intloop21 U A G C G A G = 180
> intloop21 U A G C G C G = 140
> intloop21 U A G C G G G = 120
> intloop21 U A G C G U G = 480

> intloop21 U A G C U A G = 480
> intloop21 U A G C U C G = 480
> intloop21 U A G C U G G = 480
> intloop21 U A G C U U G = 480


> intloop21 U C G C A A G = 310
> intloop21 U C G C A C G = 300
> intloop21 U C G C A G G = 480
> intloop21 U C G C A U G = 300

> intloop21 U C G C C A G = 300
> intloop21 U C G C C C G = 330
> intloop21 U C G C C G G = 480
> intloop21 U C G C C U G = 300

> intloop21 U C G C G A G = 480
> intloop21 U C G C G C G = 480
> intloop21 U C G C G G G = 480
> intloop21 U C G C G U G = 480

> intloop21 U C G C U A G = 330
> intloop21 U C G C U C G = 270
> intloop21 U C G C U G G = 480
> intloop21 U C G C U U G = 300


> intloop21 U G G C A A G = 250
> intloop21 U G G C A C G = 480
> intloop21 U G G C A G G = 160
> intloop21 U G G C A U G = 480

> intloop21 U G G C C A G = 480
> intloop21 U G G C C C G = 480
> intloop21 U G G C C G G = 480
> intloop21 U G G C C U G = 480

> intloop21 U G G C G A G = 160
> intloop21 U G G C G C G = 480
> intloop21 U G G C G G G = 300
> intloop21 U G G C G U G = 480

> intloop21 U G G C U A G = 480
> intloop21 U G G C U C G = 480
> intloop21 U G G C U G G = 480
> intloop21 U G G C U U G = 480


> intloop21 U U G C A A G = 480
> intloop21 U U G C A C G = 480
> intloop21 U U G C A G G = 480
> intloop21 U U G C A U G = 480

> intloop21 U U G C C A G = 480
> intloop21 U U G C C C G = 300
> intloop21 U U G C C G G = 480
> intloop21 U U G C C U G = 210

> intloop21 U U G C G A G = 480
> intloop21 U U G C G C G = 480
> intloop21 U U G C G G G = 480
> intloop21 U U G C G U G = 480

> intloop21 U U G C U A G = 480
> intloop21 U U G C U C G = 250
> intloop21 U U G C U G G = 480
> intloop21 U U G C U U G = 200


> intloop21 U A C G A A G = 320
> intloop21 U A C G A C G = 300
> intloop21 U A C G A G G = 240
> intloop21 U A C G A U G = 480

> intloop21 U A C G C A G = 290
> intloop21 U A C G C C G = 250
> intloop21 U A C G C G G = 240
> intloop21 U A C G C U G = 480

> intloop21 U A C G G A G = 180
> intloop21 U A C G G C G = 140
> intloop21 U A C G G G G = 120
> intloop21 U A C G G U G = 480

> intloop21 U A C G U A G = 480
> intloop21 U A C G U C G = 480
> intloop21 U A C G U G G = 480
> intloop21 U A C G U U G = 480


> intloop21 U C C G A A G = 310
> intloop21 U C C G A C G = 300
> intloop21 U C C G A G G = 480
> intloop21 U C C G A U G = 300

> intloop21 U C C G C A G = 300
> intloop21 U C C G C C G = 330
> intloop21 U C C G C G G = 480
> intloop21 U C C G C U G = 300

> intloop21 U C C G G A G = 480
> intloop21 U C C G G C G = 480
> intloop21 U C C G G G G = 480
> intloop21 U C C G G U G = 480

> intloop21 U C C G U A G = 330
> intloop21 U C C G U C G = 270
> intloop21 U C C G U G G = 480
> intloop21 U C C G U U G = 300


> intloop21 U G C G A A G = 250
> intloop21 U G C G A C G = 480
> intloop21 U G C G A G G = 160
> intloop21 U G C G A U G = 480

> intloop21 U G C G C A G = 480
> intloop21 U G C G C C G = 480
> intloop21 U G C G C G G = 480
> intloop21 U G C G C U G = 480

> intloop21 U G C G G A G = 160
> intloop21 U G C G G C G = 480
> intloop21 U G C G G G G = 300
> intloop21 U G C G G U G = 480

> intloop21 U G C G U A G = 480
> intloop21 U G C G U C G = 480
> intloop21 U G C G U G G = 480
> intloop21 U G C G U U G = 480


> intloop21 U U C G A A G = 480
> intloop21 U U C G A C G = 480
> intloop21 U U C G A G G = 480
> intloop21 U U C G A U G = 480

> intloop21 U U C G C A G = 480
> intloop21 U U C G C C G = 300
> intloop21 U U C G C G G = 480
> intloop21 U U C G C U G = 210

> intloop21 U U C G G A G = 480
> intloop21 U U C G G C G = 480
> intloop21 U U C G G G G = 480
> intloop21 U U C G G U G = 480

> intloop21 U U C G U A G = 480
> intloop21 U U C G U C G = 250
> intloop21 U U C G U G G = 480
> intloop21 U U C G U U G = 200


> intloop21 U A U G A A G = 390
> intloop21 U A U G A C G = 370
> intloop21 U A U G A G G = 310
> intloop21 U A U G A U G = 550

> intloop21 U A U G C A G = 360
> intloop21 U A U G C C G = 320
> intloop21 U A U G C G G = 310
> intloop21 U A U G C U G = 550

> intloop21 U A U G G A G = 250
> intloop21 U A U G G C G = 210
> intloop21 U A U G G G G = 190
> intloop21 U A U G G U G = 550

> intloop21 U A U G U A G = 550
> intloop21 U A U G U C G = 550
> intloop21 U A U G U G G = 550
> intloop21 U A U G U U G = 550


> intloop21 U C U G A A G = 380
> intloop21 U C U G A C G = 370
> intloop21 U C U G A G G = 550
> intloop21 U C U G A U G = 370

> intloop21 U C U G C A G = 370
> intloop21 U C U G C C G = 400
> intloop21 U C U G C G G = 550
> intloop21 U C U G C U G = 370

> intloop21 U C U G G A G = 550
> intloop21 U C U G G C G = 550
> intloop21 U C U G G G G = 550
> intloop21 U C U G G U G = 550

> intloop21 U C U G U A G = 400
> intloop21 U C U G U C G = 340
> intloop21 U C U G U G G = 550
> intloop21 U C U G U U G = 370


> intloop21 U G U G A A G = 320
> intloop21 U G U G A C G = 550
> intloop21 U G U G A G G = 230
> intloop21 U G U G A U G = 550

> intloop21 U G U G C A G = 550
> intloop21 U G U G C C G = 550
> intloop21 U G U G C G G = 550
> intloop21 U G U G C U G = 550

> intloop21 U G U G G A G = 230
> intloop21 U G U G G C G = 550
> intloop21 U G U G G G G = 370
> intloop21 U G U G G U G = 550

> intloop21 U G U G U A G = 550
> intloop21 U G U G U C G = 550
> intloop21 U G U G U G G = 550
> intloop21 U G U G U U G = 550


> intloop21 U U U G A A G = 550
> intloop21 U U U G A C G = 550
> intloop21 U U U G A G G = 550
> intloop21 U U U G A U G = 550

> intloop21 U U U G C A G = 550
> intloop21 U U U G C C G = 370
> intloop21 U U U G C G G = 550
> intloop21 U U U G C U G = 280

> intloop21 U U U G G A G = 550
> intloop21 U U U G G C G = 550
> intloop21 U U U G G G G = 550
> intloop21 U U U G G U G = 550

> intloop21 U U U G U A G = 550
> intloop21 U U U G U C G = 320
> intloop21 U U U G U G G = 550
> intloop21 U U U G U U G = 270


> intloop21 U A G U A A G = 390
> intloop21 U A G U A C G = 370
> intloop21 U A G U A G G = 310
> intloop21 U A G U A U G = 550

> intloop21 U A G U C A G = 360
> intloop21 U A G U C C G = 320
> intloop21 U A G U C G G = 310
> intloop21 U A G U C U G = 550

> intloop21 U A G U G A G = 250
> intloop21 U A G U G C G = 210
> intloop21 U A G U G G G = 190
> intloop21 U A G U G U G = 550

> intloop21 U A G U U A G = 550
> intloop21 U A G U U C G = 550
> intloop21 U A G U U G G = 550
> intloop21 U A G U U U G = 550


> intloop21 U C G U A A G = 380
> intloop21 U C G U A C G = 370
> intloop21 U C G U A G G = 550
> intloop21 U C G U A U G = 370

> intloop21 U C G U C A G = 370
> intloop21 U C G U C C G = 400
> intloop21 U C G U C G G = 550
> intloop21 U C G U C U G = 370

> intloop21 U C G U G A G = 550
> intloop21 U C G U G C G = 550
> intloop21 U C G U G G G = 550
> intloop21 U C G U G U G = 550

> intloop21 U C G U U A G = 400
> intloop21 U C G U U C G = 340
> intloop21 U C G U U G G = 550
> intloop21 U C G U U U G = 370


> intloop21 U G G U A A G = 320
> intloop21 U G G U A C G = 550
> intloop21 U G G U A G G = 230
> intloop21 U G G U A U G = 550

> intloop21 U G G U C A G = 550
> intloop21 U G G U C C G = 550
> intloop21 U G G U C G G = 550
> intloop21 U G G U C U G = 550

> intloop21 U G G U G A G = 230
> intloop21 U G G U G C G = 550
> intloop21 U G G U G G G = 370
> intloop21 U G G U G U G = 550

> intloop21 U G G U U A G = 550
> intloop21 U G G U U C G = 550
> intloop21 U G G U U G G = 550
> intloop21 U G G U U U G = 550


> intloop21 U U G U A A G = 550
> intloop21 U U G U A C G = 550
> intloop21 U U G U A G G = 550
> intloop21 U U G U A U G = 550

> intloop21 U U G U C A G = 550
> intloop21 U U G U C C G = 370
> intloop21 U U G U C G G = 550
> intloop21 U U G U C U G = 280

> intloop21 U U G U G A G = 550
> intloop21 U U G U G C G = 550
> intloop21 U U G U G G G = 550
> intloop21 U U G U G U G = 550

> intloop21 U U G U U A G = 550
> intloop21 U U G U U C G = 320
> intloop21 U U G U U G G = 550
> intloop21 U U G U U U G = 270


> intloop21 U A U A A A G = 390
> intloop21 U A U A A C G = 370
> intloop21 U A U A A G G = 310
> intloop21 U A U A A U G = 550

> intloop21 U A U A C A G = 360
> intloop21 U A U A C C G = 320
> intloop21 U A U A C G G = 310
> intloop21 U A U A C U G = 550

> intloop21 U A U A G A G = 250
> intloop21 U A U A G C G = 210
> intloop21 U A U A G G G = 190
> intloop21 U A U A G U G = 550

> intloop21 U A U A U A G = 550
> intloop21 U A U A U C G = 550
> intloop21 U A U A U G G = 550
> intloop21 U A U A U U G = 550


> intloop21 U C U A A A G = 380
> intloop21 U C U A A C G = 370
> intloop21 U C U A A G G = 550
> intloop21 U C U A A U G = 370

> intloop21 U C U A C A G = 370
> intloop21 U C U A C C G = 400
> intloop21 U C U A C G G = 550
> intloop21 U C U A C U G = 370

> intloop21 U C U A G A G = 550
> intloop21 U C U A G C G = 550
> intloop21 U C U A G G G = 550
> intloop21 U C U A G U G = 550

> intloop21 U C U A U A G = 400
> intloop21 U C U A U C G = 340
> intloop21 U C U A U G G = 550
> intloop21 U C U A U U G = 370


> intloop21 U G U A A A G = 320
> intloop21 U G U A A C G = 550
> intloop21 U G U A A G G = 230
> intloop21 U G U A A U G = 550

> intloop21 U G U A C A G = 550
> intloop21 U G U A C C G = 550
> intloop21 U G U A C G G = 550
> intloop21 U G U A C U G = 550

> intloop21 U G U A G A G = 230
> intloop21 U G U A G C G = 550
> intloop21 U G U A G G G = 370
> intloop21 U G U A G U G = 550

> intloop21 U G U A U A G = 550
> intloop21 U G U A U C G = 550
> intloop21 U G U A U G G = 550
> intloop21 U G U A U U G = 550


> intloop21 U U U A A A G = 550
> intloop21 U U U A A C G = 550
> intloop21 U U U A A G G = 550
> intloop21 U U U A A U G = 550

> intloop21 U U U A C A G = 550
> intloop21 U U U A C C G = 370
> intloop21 U U U A C G G = 550
> intloop21 U U U A C U G = 280

> intloop21 U U U A G A G = 550
> intloop21 U U U A G C G = 550
> intloop21 U U U A G G G = 550
> intloop21 U U U A G U G = 550

> intloop21 U U U A U A G = 550
> intloop21 U U U A U C G = 320
> intloop21 U U U A U G G = 550
> intloop21 U U U A U U G = 270


> intloop21 U A A U A A G = 390
> intloop21 U A A U A C G = 370
> intloop21 U A A U A G G = 310
> intloop21 U A A U A U G = 550

> intloop21 U A A U C A G = 360
> intloop21 U A A U C C G = 320
> intloop21 U A A U C G G = 310
> intloop21 U A A U C U G = 550

> intloop21 U A A U G A G = 250
> intloop21 U A A U G C G = 210
> intloop21 U A A U G G G = 190
> intloop21 U A A U G U G = 550

> intloop21 U A A U U A G = 550
> intloop21 U A A U U C G = 550
> intloop21 U A A U U G G = 550
> intloop21 U A A U U U G = 550


> intloop21 U C A U A A G = 380
> intloop21 U C A U A C G = 370
> intloop21 U C A U A G G = 550
> intloop21 U C A U A U G = 370

> intloop21 U C A U C A G = 370
> intloop21 U C A U C C G = 400
> intloop21 U C A U C G G = 550
> intloop21 U C A U C U G = 370

> intloop21 U C A U G A G = 550
> intloop21 U C A U G C G = 550
> intloop21 U C A U G G G = 550
> intloop21 U C A U G U G = 550

> intloop21 U C A U U A G = 400
> intloop21 U C A U U C G = 340
> intloop21 U C A U U G G = 550
> intloop21 U C A U U U G = 370


> intloop21 U G A U A A G = 320
> intloop21 U G A U A C G = 550
> intloop21 U G A U A G G = 230
> intloop21 U G A U A U G = 550

> intloop21 U G A U C A G = 550
> intloop21 U G A U C C G = 550
> intloop21 U G A U C G G = 550
> intloop21 U G A U C U G = 550

> intloop21 U G A U G A G = 230
> intloop21 U G A U G C G = 550
> intloop21 U G A U G G G = 370
> intloop21 U G A U G U G = 550

> intloop21 U G A U U A G = 550
> intloop21 U G A U U C G = 550
> intloop21 U G A U U G G = 550
> intloop21 U G A U U U G = 550


> intloop21 U U A U A A G = 550
> intloop21 U U A U A C G = 550
> intloop21 U U A U A G G = 550
> intloop21 U U A U A U G = 550

> intloop21 U U A U C A G = 550
> intloop21 U U A U C C G = 370
> intloop21 U U A U C G G = 550
> intloop21 U U A U C U G = 280

> intloop21 U U A U G A G = 550
> intloop21 U U A U G C G = 550
> intloop21 U U A U G G G = 550
> intloop21 U U A U G U G = 550

> intloop21 U U A U U A G = 550
> intloop21 U U A U U C G = 320
> intloop21 U U A U U G G = 550
> intloop21 U U A U U U G = 270


> intloop21 A A G C A A U = 320
> intloop21 A A G C A C U = 300
> intloop21 A A G C A G U = 240
> intloop21 A A G C A U U = 480

> intloop21 A A G C C A U = 290
> intloop21 A A G C C C U = 250
> intloop21 A A G C C G U = 240
> intloop21 A A G C C U U = 480

> intloop21 A A G C G A U = 180
> intloop21 A A G C G C U = 140
> intloop21 A A G C G G U = 120
> intloop21 A A G C G U U = 480

> intloop21 A A G C U A U = 480
> intloop21 A A G C U C U = 480
> intloop21 A A G C U G U = 480
> intloop21 A A G C U U U = 480


> intloop21 A C G C A A U = 310
> intloop21 A C G C A C U = 300
> intloop21 A C G C A G U = 480
> intloop21 A C G C A U U = 300

> intloop21 A C G C C A U = 300
> intloop21 A C G C C C U = 330
> intloop21 A C G C C G U = 480
> intloop21 A C G C C U U = 300

> intloop21 A C G C G A U = 480
> intloop21 A C G C G C U = 480
> intloop21 A C G C G G U = 480
> intloop21 A C G C G U U = 480

> intloop21 A C G C U A U = 330
> intloop21 A C G C U C U = 270
> intloop21 A C G C U G U = 480
> intloop21 A C G C U U U = 300


> intloop21 A G G C A A U = 250
> intloop21 A G G C A C U = 480
> intloop21 A G G C A G U = 160
> intloop21 A G G C A U U = 480

> intloop21 A G G C C A U = 480
> intloop21 A G G C C C U = 480
> intloop21 A G G C C G U = 480
> intloop21 A G G C C U U = 480

> intloop21 A G G C G A U = 160
> intloop21 A G G C G C U = 480
> intloop21 A G G C G G U = 300
> intloop21 A G G C G U U = 480

> intloop21 A G G C U A U = 480
> intloop21 A G G C U C U = 480
> intloop21 A G G C U G U = 480
> intloop21 A G G C U U U = 480


> intloop21 A U G C A A U = 480
> intloop21 A U G C A C U = 480
> intloop21 A U G C A G U = 480
> intloop21 A U G C A U U = 480

> intloop21 A U G C C A U = 480
> intloop21 A U G C C C U = 300
> intloop21 A U G C C G U = 480
> intloop21 A U G C C U U = 210

> intloop21 A U G C G A U = 480
> intloop21 A U G C G C U = 480
> intloop21 A U G C G G U = 480
> intloop21 A U G C G U U = 480

> intloop21 A U G C U A U = 480
> intloop21 A U G C U C U = 250
> intloop21 A U G C U G U = 480
> intloop21 A U G C U U U = 200


> intloop21 A A C G A A U = 320
> intloop21 A A C G A C U = 300
> intloop21 A A C G A G U = 240
> intloop21 A A C G A U U = 480

> intloop21 A A C G C A U = 290
> intloop21 A A C G C C U = 250
> intloop21 A A C G C G U = 240
> intloop21 A A C G C U U = 480

> intloop21 A A C G G A U = 180
> intloop21 A A C G G C U = 140
> intloop21 A A C G G G U = 120
> intloop21 A A C G G U U = 480

> intloop21 A A C G U A U = 480
> intloop21 A A C G U C U = 480
> intloop21 A A C G U G U = 480
> intloop21 A A C G U U U = 480


> intloop21 A C C G A A U = 310
> intloop21 A C C G A C U = 300
> intloop21 A C C G A G U = 480
> intloop21 A C C G A U U = 300

> intloop21 A C C G C A U = 300
> intloop21 A C C G C C U = 330
> intloop21 A C C G C G U = 480
> intloop21 A C C G C U U = 300

> intloop21 A C C G G A U = 480
> intloop21 A C C G G C U = 480
> intloop21 A C C G G G U = 480
> intloop21 A C C G G U U = 480

> intloop21 A C C G U A U = 330
> intloop21 A C C G U C U = 270
> intloop21 A C C G U G U = 480
> intloop21 A C C G U U U = 300


> intloop21 A G C G A A U = 250
> intloop21 A G C G A C U = 480
> intloop21 A G C G A G U = 160
> intloop21 A G C G A U U = 480

> intloop21 A G C G C A U = 480
> intloop21 A G C G C C U = 480
> intloop21 A G C G C G U = 480
> intloop21 A G C G C U U = 480

> intloop21 A G C G G A U = 160
> intloop21 A G C G G C U = 480
> intloop21 A G C G G G U = 300
> intloop21 A G C G G U U = 480

> intloop21 A G C G U A U = 480
> intloop21 A G C G U C U = 480
> intloop21 A G C G U G U = 480
> intloop21 A G C G U U U = 480


> intloop21 A U C G A A U = 480
> intloop21 A U C G A C U = 480
> intloop21 A U C G A G U = 480
> intloop21 A U C G A U U = 480

> intloop21 A U C G C A U = 480
> intloop21 A U C G C C U = 300
> intloop21 A U C G C G U = 480
> intloop21 A U C G C U U = 210

> intloop21 A U C G G A U = 480
> intloop21 A U C G G C U = 480
> intloop21 A U C G G G U = 480
> intloop21 A U C G G U U = 480

> intloop21 A U C G U A U = 480
> intloop21 A U C G U C U = 250
> intloop21 A U C G U G U = 480
> intloop21 A U C G U U U = 200


> intloop21 A A U G A A U = 390
> intloop21 A A U G A C U = 370
> intloop21 A A U G A G U = 310
> intloop21 A A U G A U U = 550

> intloop21 A A U G C A U = 360
> intloop21 A A U G C C U = 320
> intloop21 A A U G C G U = 310
> intloop21 A A U G C U U = 550

> intloop21 A A U G G A U = 250
> intloop21 A A U G G C U = 210
> intloop21 A A U G G G U = 190
> intloop21 A A U G G U U = 550

> intloop21 A A U G U A U = 550
> intloop21 A A U G U C U = 550
> intloop21 A A U G U G U = 550
> intloop21 A A U G U U U = 550


> intloop21 A C U G A A U = 380
> intloop21 A C U G A C U = 370
> intloop21 A C U G A G U = 550
> intloop21 A C U G A U U = 370

> intloop21 A C U G C A U = 370
> intloop21 A C U G C C U = 400
> intloop21 A C U G C G U = 550
> intloop21 A C U G C U U = 370

> intloop21 A C U G G A U = 550
> intloop21 A C U G G C U = 550
> intloop21 A C U G G G U = 550
> intloop21 A C U G G U U = 550

> intloop21 A C U G U A U = 400
> intloop21 A C U G U C U = 340
> intloop21 A C U G U G U = 550
> intloop21 A C U G U U U = 370


> intloop21 A G U G A A U = 320
> intloop21 A G U G A C U = 550
> intloop21 A G U G A G U = 230
> intloop21 A G U G A U U = 550

> intloop21 A G U G C A U = 550
> intloop21 A G U G C C U = 550
> intloop21 A G U G C G U = 550
> intloop21 A G U G C U U = 550

> intloop21 A G U G G A U = 230
> intloop21 A G U G G C U = 550
> intloop21 A G U G G G U = 370
> intloop21 A G U G G U U = 550

> intloop21 A G U G U A U = 550
> intloop21 A G U G U C U = 550
> intloop21 A G U G U G U = 550
> intloop21 A G U G U U U = 550


> intloop21 A U U G A A U = 550
> intloop21 A U U G A C U = 550
> intloop21 A U U G A G U = 550
> intloop21 A U U G A U U = 550

> intloop21 A U U G C A U = 550
> intloop21 A U U G C C U = 370
> intloop21 A U U G C G U = 550
> intloop21 A U U G C U U = 280

> intloop21 A U U G G A U = 550
> intloop21 A U U G G C U = 550
> intloop21 A U U G G G U = 550
> intloop21 A U U G G U U = 550

> intloop21 A U U G U A U = 550
> intloop21 A U U G U C U = 320
> intloop21 A U U G U G U = 550
> intloop21 A U U G U U U = 270


> intloop21 A A G U A A U = 390
> intloop21 A A G U A C U = 370
> intloop21 A A G U A G U = 310
> intloop21 A A G U A U U = 550

> intloop21 A A G U C A U = 360
> intloop21 A A G U C C U = 320
> intloop21 A A G U C G U = 310
> intloop21 A A G U C U U = 550

> intloop21 A A G U G A U = 250
> intloop21 A A G U G C U = 210
> intloop21 A A G U G G U = 190
> intloop21 A A G U G U U = 550

> intloop21 A A G U U A U = 550
> intloop21 A A G U U C U = 550
> intloop21 A A G U U G U = 550
> intloop21 A A G U U U U = 550


> intloop21 A C G U A A U = 380
> intloop21 A C G U A C U = 370
> intloop21 A C G U A G U = 550
> intloop21 A C G U A U U = 370

> intloop21 A C G U C A U = 370
> intloop21 A C G U C C U = 400
> intloop21 A C G U C G U = 550
> intloop21 A C G U C U U = 370

> intloop21 A C G U G A U = 550
> intloop21 A C G U G C U = 550
> intloop21 A C G U G G U = 550
> intloop21 A C G U G U U = 550

> intloop21 A C G U U A U = 400
> intloop21 A C G U U C U = 340
> intloop21 A C G U U G U = 550
> intloop21 A C G U U U U = 370


> intloop21 A G G U A A U = 320
> intloop21 A G G U A C U = 550
> intloop21 A G G U A G U = 230
> intloop21 A G G U A U U = 550

> intloop21 A G G U C A U = 550
> intloop21 A G G U C C U = 550
> intloop21 A G G U C G U = 550
> intloop21 A G G U C U U = 550

> intloop21 A G G U G A U = 230
> intloop21 A G G U G C U = 550
> intloop21 A G G U G G U = 370
> intloop21 A G G U G U U = 550

> intloop21 A G G U U A U = 550
> intloop21 A G G U U C U = 550
> intloop21 A G G U U G U = 550
> intloop21 A G G U U U U = 550


> intloop21 A U G U A A U = 550
> intloop21 A U G U A C U = 550
> intloop21 A U G U A G U = 550
> intloop21 A U G U A U U = 550

> intloop21 A U G U C A U = 550
> intloop21 A U G U C C U = 370
> intloop21 A U G U C G U = 550
> intloop21 A U G U C U U = 280

> intloop21 A U G U G A U = 550
> intloop21 A U G U G C U = 550
> intloop21 A U G U G G U = 550
> intloop21 A U G U G U U = 550

> intloop21 A U G U U A U = 550
> intloop21 A U G U U C U = 320
> intloop21 A U G U U G U = 550
> intloop21 A U G U U U U = 270


> intloop21 A A U A A A U = 390
> intloop21 A A U A A C U = 370
> intloop21 A A U A A G U = 310
> intloop21 A A U A A U U = 550

> intloop21 A A U A C A U = 360
> intloop21 A A U A C C U = 320
> intloop21 A A U A C G U = 310
> intloop21 A A U A C U U = 550

> intloop21 A A U A G A U = 250
> intloop21 A A U A G C U = 210
> intloop21 A A U A G G U = 190
> intloop21 A A U A G U U = 550

> intloop21 A A U A U A U = 550
> intloop21 A A U A U C U = 550
> intloop21 A A U A U G U = 550
> intloop21 A A U A U U U = 550


> intloop21 A C U A A A U = 380
> intloop21 A C U A A C U = 370
> intloop21 A C U A A G U = 550
> intloop21 A C U A A U U = 370

> intloop21 A C U A C A U = 370
> intloop21 A C U A C C U = 400
> intloop21 A C U A C G U = 550
> intloop21 A C U A C U U = 370

> intloop21 A C U A G A U = 550
> intloop21 A C U A G C U = 550
> intloop21 A C U A G G U = 550
> intloop21 A C U A G U U = 550

> intloop21 A C U A U A U = 400
> intloop21 A C U A U C U = 340
> intloop21 A C U A U G U = 550
> intloop21 A C U A U U U = 370


> intloop21 A G U A A A U = 320
> intloop21 A G U A A C U = 550
> intloop21 A G U A A G U = 230
> intloop21 A G U A A U U = 550

> intloop21 A G U A C A U = 550
> intloop21 A G U A C C U = 550
> intloop21 A G U A C G U = 550
> intloop21 A G U A C U U = 550

> intloop21 A G U A G A U = 230
> intloop21 A G U A G C U = 550
> intloop21 A G U A G G U = 370
> intloop21 A G U A G U U = 550

> intloop21 A G U A U A U = 550
> intloop21 A G U A U C U = 550
> intloop21 A G U A U G U = 550
> intloop21 A G U A U U U = 550


> intloop21 A U U A A A U = 550
> intloop21 A U U A A C U = 550
> intloop21 A U U A A G U = 550
> intloop21 A U U A A U U = 550

> intloop21 A U U A C A U = 550
> intloop21 A U U A C C U = 370
> intloop21 A U U A C G U = 550
> intloop21 A U U A C U U = 280

> intloop21 A U U A G A U = 550
> intloop21 A U U A G C U = 550
> intloop21 A U U A G G U = 550
> intloop21 A U U A G U U = 550

> intloop21 A U U A U A U = 550
> intloop21 A U U A U C U = 320
> intloop21 A U U A U G U = 550
> intloop21 A U U A U U U = 270


> intloop21 A A A U A A U = 390
> intloop21 A A A U A C U = 370
> intloop21 A A A U A G U = 310
> intloop21 A A A U A U U = 550

> intloop21 A A A U C A U = 360
> intloop21 A A A U C C U = 320
> intloop21 A A A U C G U = 310
> intloop21 A A A U C U U = 550

> intloop21 A A A U G A U = 250
> intloop21 A A A U G C U = 210
> intloop21 A A A U G G U = 190
> intloop21 A A A U G U U = 550

> intloop21 A A A U U A U = 550
> intloop21 A A A U U C U = 550
> intloop21 A A A U U G U = 550
> intloop21 A A A U U U U = 550


> intloop21 A C A U A A U = 380
> intloop21 A C A U A C U = 370
> intloop21 A C A U A G U = 550
> intloop21 A C A U A U U = 370

> intloop21 A C A U C A U = 370
> intloop21 A C A U C C U = 400
> intloop21 A C A U C G U = 550
> intloop21 A C A U C U U = 370

> intloop21 A C A U G A U = 550
> intloop21 A C A U G C U = 550
> intloop21 A C A U G G U = 550
> intloop21 A C A U G U U = 550

> intloop21 A C A U U A U = 400
> intloop21 A C A U U C U = 340
> intloop21 A C A U U G U = 550
> intloop21 A C A U U U U = 370


> intloop21 A G A U A A U = 320
> intloop21 A G A U A C U = 550
> intloop21 A G A U A G U = 230
> intloop21 A G A U A U U = 550

> intloop21 A G A U C A U = 550
> intloop21 A G A U C C U = 550
> intloop21 A G A U C G U = 550
> intloop21 A G A U C U U = 550

> intloop21 A G A U G A U = 230
> intloop21 A G A U G C U = 550
> intloop21 A G A U G G U = 370
> intloop21 A G A U G U U = 550

> intloop21 A G A U U A U = 550
> intloop21 A G A U U C U = 550
> intloop21 A G A U U G U = 550
> intloop21 A G A U U U U = 550


> intloop21 A U A U A A U = 550
> intloop21 A U A U A C U = 550
> intloop21 A U A U A G U = 550
> intloop21 A U A U A U U = 550

> intloop21 A U A U C A U = 550
> intloop21 A U A U C C U = 370
> intloop21 A U A U C G U = 550
> intloop21 A U A U C U U = 280

> intloop21 A U A U G A U = 550
> intloop21 A U A U G C U = 550
> intloop21 A U A U G G U = 550
> intloop21 A U A U G U U = 550

> intloop21 A U A U U A U = 550
> intloop21 A U A U U C U = 320
> intloop21 A U A U U G U = 550
> intloop21 A U A U U U U = 270


> intloop21 U A G C A A A = 320
> intloop21 U A G C A C A = 300
> intloop21 U A G C A G A = 240
> intloop21 U A G C A U A = 480

> intloop21 U A G C C A A = 290
> intloop21 U A G C C C A = 250
> intloop21 U A G C C G A = 240
> intloop21 U A G C C U A = 480

> intloop21 U A G C G A A = 180
> intloop21 U A G C G C A = 140
> intloop21 U A G C G G A = 120
> intloop21 U A G C G U A = 480

> intloop21 U A G C U A A = 480
> intloop21 U A G C U C A = 480
> intloop21 U A G C U G A = 480
> intloop21 U A G C U U A = 480


> intloop21 U C G C A A A = 310
> intloop21 U C G C A C A = 300
> intloop21 U C G C A G A = 480
> intloop21 U C G C A U A = 300

> intloop21 U C G C C A A = 300
> intloop21 U C G C C C A = 330
> intloop21 U C G C C G A = 480
> intloop21 U C G C C U A = 300

> intloop21 U C G C G A A = 480
> intloop21 U C G C G C A = 480
> intloop21 U C G C G G A = 480
> intloop21 U C G C G U A = 480

> intloop21 U C G C U A A = 330
> intloop21 U C G C U C A = 270
> intloop21 U C G C U G A = 480
> intloop21 U C G C U U A = 300


> intloop21 U G G C A A A = 250
> intloop21 U G G C A C A = 480
> intloop21 U G G C A G A = 160
> intloop21 U G G C A U A = 480

> intloop21 U G G C C A A = 480
> intloop21 U G G C C C A = 480
> intloop21 U G G C C G A = 480
> intloop21 U G G C C U A = 480

> intloop21 U G G C G A A = 160
> intloop21 U G G C G C A = 480
> intloop21 U G G C G G A = 300
> intloop21 U G G C G U A = 480

> intloop21 U G G C U A A = 480
> intloop21 U G G C U C A = 480
> intloop21 U G G C U G A = 480
> intloop21 U G G C U U A = 480


> intloop21 U U G C A A A = 480
> intloop21 U U G C A C A = 480
> intloop21 U U G C A G A = 480
> intloop21 U U G C A U A = 480

> intloop21 U U G C C A A = 480
> intloop21 U U G C C C A = 300
> intloop21 U U G C C G A = 480
> intloop21 U U G C C U A = 210

> intloop21 U U G C G A A = 480
> intloop21 U U G C G C A = 480
> intloop21 U U G C G G A = 480
> intloop21 U U G C G U A = 480

> intloop21 U U G C U A A = 480
> intloop21 U U G C U C A = 250
> intloop21 U U G C U G A = 480
> intloop21 U U G C U U A = 200


> intloop21 U A C G A A A = 320
> intloop21 U A C G A C A = 300
> intloop21 U A C G A G A = 240
> intloop21 U A C G A U A = 480

> intloop21 U A C G C A A = 290
> intloop21 U A C G C C A = 250
> intloop21 U A C G C G A = 240
> intloop21 U A C G C U A = 480

> intloop21 U A C G G A A = 180
> intloop21 U A C G G C A = 140
> intloop21 U A C G G G A = 120
> intloop21 U A C G G U A = 480

> intloop21 U A C G U A A = 480
> intloop21 U A C G U C A = 480
> intloop21 U A C G U G A = 480
> intloop21 U A C G U U A = 480


> intloop21 U C C G A A A = 310
> intloop21 U C C G A C A = 300
> intloop21 U C C G A G A = 480
> intloop21 U C C G A U A = 300

> intloop21 U C C G C A A = 300
> intloop21 U C C G C C A = 330
> intloop21 U C C G C G A = 480
> intloop21 U C C G C U A = 300

> intloop21 U C C G G A A = 480
> intloop21 U C C G G C A = 480
> intloop21 U C C G G G A = 480
> intloop21 U C C G G U A = 480

> intloop21 U C C G U A A = 330
> intloop21 U C C G U C A = 270
> intloop21 U C C G U G A = 480
> intloop21 U C C G U U A = 300


> intloop21 U G C G A A A = 250
> intloop21 U G C G A C A = 480
> intloop21 U G C G A G A = 160
> intloop21 U G C G A U A = 480

> intloop21 U G C G C A A = 480
> intloop21 U G C G C C A = 480
> intloop21 U G C G C G A = 480
> intloop21 U G C G C U A = 480

> intloop21 U G C G G A A = 160
> intloop21 U G C G G C A = 480
> intloop21 U G C G G G A = 300
> intloop21 U G C G G U A = 480

> intloop21 U G C G U A A = 480
> intloop21 U G C G U C A = 480
> intloop21 U G C G U G A = 480
> intloop21 U G C G U U A = 480


> intloop21 U U C G A A A = 480
> intloop21 U U C G A C A = 480
> intloop21 U U C G A G A = 480
> intloop21 U U C G A U A = 480

> intloop21 U U C G C A A = 480
> intloop21 U U C G C C A = 300
> intloop21 U U C G C G A = 480
> intloop21 U U C G C U A = 210

> intloop21 U U C G G A A = 480
> intloop21 U U C G G C A = 480
> intloop21 U U C G G G A = 480
> intloop21 U U C G G U A = 480

> intloop21 U U C G U A A = 480
> intloop21 U U C G U C A = 250
> intloop21 U U C G U G A = 480
> intloop21 U U C G U U A = 200


> intloop21 U A U G A A A = 390
> intloop21 U A U G A C A = 370
> intloop21 U A U G A G A = 310
> intloop21 U A U G A U A = 550

> intloop21 U A U G C A A = 360
> intloop21 U A U G C C A = 320
> intloop21 U A U G C G A = 310
> intloop21 U A U G C U A = 550

> intloop21 U A U G G A A = 250
> intloop21 U A U G G C A = 210
> intloop21 U A U G G G A = 190
> intloop21 U A U G G U A = 550

> intloop21 U A U G U A A = 550
> intloop21 U A U G U C A = 550
> intloop21 U A U G U G A = 550
> intloop21 U A U G U U A = 550


> intloop21 U C U G A A A = 380
> intloop21 U C U G A C A = 370
> intloop21 U C U G A G A = 550
> intloop21 U C U G A U A = 370

> intloop21 U C U G C A A = 370
> intloop21 U C U G C C A = 400
> intloop21 U C U G C G A = 550
> intloop21 U C U G C U A = 370

> intloop21 U C U G G A A = 550
> intloop21 U C U G G C A = 550
> intloop21 U C U G G G A = 550
> intloop21 U C U G G U A = 550

> intloop21 U C U G U A A = 400
> intloop21 U C U G U C A = 340
> intloop21 U C U G U G A = 550
> intloop21 U C U G U U A = 370


> intloop21 U G U G A A A = 320
> intloop21 U G U G A C A = 550
> intloop21 U G U G A G A = 230
> intloop21 U G U G A U A = 550

> intloop21 U G U G C A A = 550
> intloop21 U G U G C C A = 550
> intloop21 U G U G C G A = 550
> intloop21 U G U G C U A = 550

> intloop21 U G U G G A A = 230
> intloop21 U G U G G C A = 550
> intloop21 U G U G G G A = 370
> intloop21 U G U G G U A = 550

> intloop21 U G U G U A A = 550
> intloop21 U G U G U C A = 550
> intloop21 U G U G U G A = 550
> intloop21 U G U G U U A = 550


> intloop21 U U U G A A A = 550
> intloop21 U U U G A C A = 550
> intloop21 U U U G A G A = 550
> intloop21 U U U G A U A = 550

> intloop21 U U U G C A A = 550
> intloop21 U U U G C C A = 370
> intloop21 U U U G C G A = 550
> intloop21 U U U G C U A = 280

> intloop21 U U U G G A A = 550
> intloop21 U U U G G C A = 550
> intloop21 U U U G G G A = 550
> intloop21 U U U G G U A = 550

> intloop21 U U U G U A A = 550
> intloop21 U U U G U C A = 320
> intloop21 U U U G U G A = 550
> intloop21 U U U G U U A = 270


> intloop21 U A G U A A A = 390
> intloop21 U A G U A C A = 370
> intloop21 U A G U A G A = 310
> intloop21 U A G U A U A = 550

> intloop21 U A G U C A A = 360
> intloop21 U A G U C C A = 320
> intloop21 U A G U C G A = 310
> intloop21 U A G U C U A = 550

> intloop21 U A G U G A A = 250
> intloop21 U A G U G C A = 210
> intloop21 U A G U G G A = 190
> intloop21 U A G U G U A = 550

> intloop21 U A G U U A A = 550
> intloop21 U A G U U C A = 550
> intloop21 U A G U U G A = 550
> intloop21 U A G U U U A = 550


> intloop21 U C G U A A A = 380
> intloop21 U C G U A C A = 370
> intloop21 U C G U A G A = 550
> intloop21 U C G U A U A = 370

> intloop21 U C G U C A A = 370
> intloop21 U C G U C C A = 400
> intloop21 U C G U C G A = 550
> intloop21 U C G U C U A = 370

> intloop21 U C G U G A A = 550
> intloop21 U C G U G C A = 550
> intloop21 U C G U G G A = 550
> intloop21 U C G U G U A = 550

> intloop21 U C G U U A A = 400
> intloop21 U C G U U C A = 340
> intloop21 U C G U U G A = 550
> intloop21 U C G U U U A = 370


> intloop21 U G G U A A A = 320
> intloop21 U G G U A C A = 550
> intloop21 U G G U A G A = 230
> intloop21 U G G U A U A = 550

> intloop21 U G G U C A A = 550
> intloop21 U G G U C C A = 550
> intloop21 U G G U C G A = 550
> intloop21 U G G U C U A = 550

> intloop21 U G G U G A A = 230
> intloop21 U G G U G C A = 550
> intloop21 U G G U G G A = 370
> intloop21 U G G U G U A = 550

> intloop21 U G G U U A A = 550
> intloop21 U G G U U C A = 550
> intloop21 U G G U U G A = 550
> intloop21 U G G U U U A = 550


> intloop21 U U G U A A A = 550
> intloop21 U U G U A C A = 550
> intloop21 U U G U A G A = 550
> intloop21 U U G U A U A = 550

> intloop21 U U G U C A A = 550
> intloop21 U U G U C C A = 370
> intloop21 U U G U C G A = 550
> intloop21 U U G U C U A = 280

> intloop21 U U G U G A A = 550
> intloop21 U U G U G C A = 550
> intloop21 U U G U G G A = 550
> intloop21 U U G U G U A = 550

> intloop21 U U G U U A A = 550
> intloop21 U U G U U C A = 320
> intloop21 U U G U U G A = 550
> intloop21 U U G U U U A = 270


> intloop21 U A U A A A A = 390
> intloop21 U A U A A C A = 370
> intloop21 U A U A A G A = 310
> intloop21 U A U A A U A = 550

> intloop21 U A U A C A A = 360
> intloop21 U A U A C C A = 320
> intloop21 U A U A C G A = 310
> intloop21 U A U A C U A = 550

> intloop21 U A U A G A A = 250
> intloop21 U A U A G C A = 210
> intloop21 U A U A G G A = 190
> intloop21 U A U A G U A = 550

> intloop21 U A U A U A A = 550
> intloop21 U A U A U C A = 550
> intloop21 U A U A U G A = 550
> intloop21 U A U A U U A = 550


> intloop21 U C U A A A A = 380
> intloop21 U C U A A C A = 370
> intloop21 U C U A A G A = 550
> intloop21 U C U A A U A = 370

> intloop21 U C U A C A A = 370
> intloop21 U C U A C C A = 400
> intloop21 U C U A C G A = 550
> intloop21 U C U A C U A = 370

> intloop21 U C U A G A A = 550
> intloop21 U C U A G C A = 550
> intloop21 U C U A G G A = 550
> intloop21 U C U A G U A = 550

> intloop21 U C U A U A A = 400
> intloop21 U C U A U C A = 340
> intloop21 U C U A U G A = 550
> intloop21 U C U A U U A = 370


> intloop21 U G U A A A A = 320
> intloop21 U G U A A C A = 550
> intloop21 U G U A A G A = 230
> intloop21 U G U A A U A = 550

> intloop21 U G U A C A A = 550
> intloop21 U G U A C C A = 550
> intloop21 U G U A C G A = 550
> intloop21 U G U A C U A = 550

> intloop21 U G U A G A A = 230
> intloop21 U G U A G C A = 550
> intloop21 U G U A G G A = 370
> intloop21 U G U A G U A = 550

> intloop21 U G U A U A A = 550
> intloop21 U G U A U C A = 550
> intloop21 U G U A U G A = 550
> intloop21 U G U A U U A = 550


> intloop21 U U U A A A A = 550
> intloop21 U U U A A C A = 550
> intloop21 U U U A A G A = 550
> intloop21 U U U A A U A = 550

> intloop21 U U U A C A A = 550
> intloop21 U U U A C C A = 370
> intloop21 U U U A C G A = 550
> intloop21 U U U A C U A = 280

> intloop21 U U U A G A A = 550
> intloop21 U U U A G C A = 550
> intloop21 U U U A G G A = 550
> intloop21 U U U A G U A = 550

> intloop21 U U U A U A A = 550
> intloop21 U U U A U C A = 320
> intloop21 U U U A U G A = 550
> intloop21 U U U A U U A = 270


> intloop21 U A A U A A A = 390
> intloop21 U A A U A C A = 370
> intloop21 U A A U A G A = 310
> intloop21 U A A U A U A = 550

> intloop21 U A A U C A A = 360
> intloop21 U A A U C C A = 320
> intloop21 U A A U C G A = 310
> intloop21 U A A U C U A = 550

> intloop21 U A A U G A A = 250
> intloop21 U A A U G C A = 210
> intloop21 U A A U G G A = 190
> intloop21 U A A U G U A = 550

> intloop21 U A A U U A A = 550
> intloop21 U A A U U C A = 550
> intloop21 U A A U U G A = 550
> intloop21 U A A U U U A = 550


> intloop21 U C A U A A A = 380
> intloop21 U C A U A C A = 370
> intloop21 U C A U A G A = 550
> intloop21 U C A U A U A = 370

> intloop21 U C A U C A A = 370
> intloop21 U C A U C C A = 400
> intloop21 U C A U C G A = 550
> intloop21 U C A U C U A = 370

> intloop21 U C A U G A A = 550
> intloop21 U C A U G C A = 550
> intloop21 U C A U G G A = 550
> intloop21 U C A U G U A = 550

> intloop21 U C A U U A A = 400
> intloop21 U C A U U C A = 340
> intloop21 U C A U U G A = 550
> intloop21 U C A U U U A = 370


> intloop21 U G A U A A A = 320
> intloop21 U G A U A C A = 550
> intloop21 U G A U A G A = 230
> intloop21 U G A U A U A = 550

> intloop21 U G A U C A A = 550
> intloop21 U G A U C C A = 550
> intloop21 U G A U C G A = 550
> intloop21 U G A U C U A = 550

> intloop21 U G A U G A A = 230
> intloop21 U G A U G C A = 550
> intloop21 U G A U G G A = 370
> intloop21 U G A U G U A = 550

> intloop21 U G A U U A A = 550
> intloop21 U G A U U C A = 550
> intloop21 U G A U U G A = 550
> intloop21 U G A U U U A = 550


> intloop21 U U A U A A A = 550
> intloop21 U U A U A C A = 550
> intloop21 U U A U A G A = 550
> intloop21 U U A U A U A = 550

> intloop21 U U A U C A A = 550
> intloop21 U U A U C C A = 370
> intloop21 U U A U C G A = 550
> intloop21 U U A U C U A = 280

> intloop21 U U A U G A A = 550
> intloop21 U U A U G C A = 550
> intloop21 U U A U G G A = 550
> intloop21 U U A U G U A = 550

> intloop21 U U A U U A A = 550
> intloop21 U U A U U C A = 320
> intloop21 U U A U U G A = 550
> intloop21 U U A U U U A = 270
> intloop21 _ _ _ _ _ _ _ = 550

