> module RandomNumber where

> import System.Random
> import Control.Monad
> import System.IO.Unsafe

An easy, but maybe dirty way of getting some random Values
We use the global random generator to produce a random int, which is used as seed
for a new generator from which we draw random doubles.

> getRandomDouble :: Double -> Double
> getRandomDouble x = head (getRandomDoubles 1 x)

> getRandomDoubles :: Int -> Double -> [Double]
> getRandomDoubles n l = take n (unsafePerformIO((getARange 0 l) >>= return))
       
> getARange :: Double -> Double -> IO [Double]
> getARange x y  = liftM (randomRs (x,y)) (liftM mkStdGen (randomIO))



> getIntRange:: Int ->IO [Int]
> getIntRange  x =   liftM (randomRs (0,x)) (liftM mkStdGen (randomIO))

> getRandomInt :: Int -> Int
> getRandomInt x = head (getRandomInts 1 x)

> getRandomInts :: Int -> Int -> [Int]
> getRandomInts n l = take n (unsafePerformIO((getIntRange l) >>= return))
  
> pick:: [a] ->[a]
> pick   xs = [xs!!(getRandomInt (length xs -1))]