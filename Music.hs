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

integrate :: Signal -> Signal
integrate (Signal a) = Signal $ sum (zipWith area a (drop 1 a)) 0
	where 	sum (x:xs) a = (a+x):(sum xs (a+x))
		area a b = 1/2*a+1/2*b

modulatedSine :: Double -> Signal -> Signal
modulatedSine c m = Signal $ u (integrate m)
	where u (Signal i) = map (\x -> sin (2*pi*c+2*pi*x)) i

rampUp :: Double -> Signal
rampUp t = Signal [1/(t*sampleRate)*x | x <-[0..]]

append :: Signal -> Signal -> Signal
append (Signal a) (Signal b) = Signal $ a++b

hullCurve :: Double -> Double -> Double -> Double -> Double -> Signal
hullCurve attack decay decayLevel duration release 
	= append at (append de (append con re))
	where 
		at = rampUp attack
		de = 1-((constant (1-decayLevel))*rampUp (decay-attack))
		con = trim (constant decayLevel) (duration - attack- decay)
		re = (constant decayLevel)*(rampUp release)

synthLead :: (Double, Double) -> Signal
synthLead (freq,length) = base * hull
	where 
		base = modulatedSine freq (sine freq)
		hull = hullCurve 0.004 0.2 0.625 length 2.0

type Instrument = (Double, Double) -> Signal

play :: Instrument -> [(Double, Double)] -> Signal
play instrument ((freqSeq, lenSeq):[]) = instrument (freqSeq, lenSeq)
play instrument ((freqSeq, lenSeq):xs) = append (instrument (freqSeq, lenSeq)) (play instrument xs)
