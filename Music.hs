module Music where

import Wave

double2Int :: Double -> Int
double2Int = fromInteger . floor

constant :: Double -> Signal
constant d = Signal l
	where l = d:l 
 
silence :: Signal
silence = constant 0

sine :: Double -> Signal
sine f = Signal $ [sin (f*2*pi*x/sampleRate) | x <- [0..]] 

trim :: Signal -> Double -> Signal
trim (Signal d) t = Signal $ take (double2Int $ sampleRate*t)  d
