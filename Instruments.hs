{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Instruments where

import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe

import Wave
import Sound

-- from https://stackoverflow.com/a/4236284
memo :: Ord a => (a -> b) -> (a -> b)
memo f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y


leadSynth :: Instrument
leadSynth = memo leadSynth'
  where
    leadSynth' (freq, noteLength) = 0.5 * base * hull
      where
        base = 2 * abs (modulatedSine freq (sine (2*freq))) - 1
        hull = hullCurve 0.01 0.1 0.3 noteLength 0.05

bass :: Instrument
bass = memo bass'
  where
    bass' (freq, noteLength) = 0.5 * base * hull
      where
        base = modulatedSine (2*freq) (sine (3*freq))
        hull = hullCurve 0.01 0.05 0.3 noteLength 0.5

bass2 :: Instrument
bass2 = memo bass2'
  where
    bass2' (freq, noteLength) = 0.5 * base * hull
      where
        base = modulatedSine (2*freq) (sine (3*freq))
        hull = hullCurve 0.01 0.1 0.3 noteLength 0.5

bass3 :: Instrument
bass3 = memo bass3'
  where
    bass3' (freq, noteLength) = 0.5 * base * hull
      where
        base = modulatedSine (2*freq) (sine (3*freq))
        hull = hullCurve 0.005 0.1 0.3 noteLength 0.1

guitar :: Instrument
guitar = memo guitar'
  where
    guitar' (freq, _) = 0.5 * base * hull
      where
        base = modulatedSine freq (sine freq)
        hull = exponentialHull (2.0 * 6/50)

exponentialHull :: Double -> Signal
exponentialHull halflife = Signal (map scaledExp [0..]) `trim` (5*halflife)
  where
    scaledExp t = exp (λ*t)
    λ = (log 0.5)/(halflife*sampleRate)

