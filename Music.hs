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

instance Num Signal where
	(+) (Signal a) (Signal b) = Signal $ zipWith (+) a b
	(-) (Signal a) (Signal b) = Signal $ zipWith (-) a b  
	(*) (Signal a) (Signal b) = Signal $ zipWith (*) a b 
	negate x = 0-x
	fromInteger = constant . fromInteger 
instance Fractional Signal where
	(/) (Signal a) (Signal b) = Signal $ zipWith f a b
		where f x y = 
			if y /= 0 then x/y 
			else 0 
	fromRational =  constant . fromRational 
