module Song where

import Wave
import Notes

import Sound -- Das Modul mit Ihrer Implementierung

leadPattern0 = [
  ("g4", 2),
  ("f4", 2),
  ("g4", 2),
  ("d4", 2),
  ("a#3", 1),
  ("d4", 2),
  ("g3", 2),
  ("", 3)
  ]
leadPattern1 = [
  ("g4", 2),
  ("a4", 2),
  ("a#4", 2),
  ("a4", 1),
  ("a#4", 2),
  ("a#4", 2),
  ("g4", 1),
  ("a4", 2),
  ("g4", 1),
  ("a4", 2),
  ("a4", 2),
  ("f4", 1),
  ("g4", 2),
  ("f4", 1),
  ("g4", 2),
  ("g4", 2),
  ("f4", 1),
  ("g4", 4)
  ]

bassPattern = [
  ("g2", 2),
  ("g2", 2),
  ("d2", 2),
  ("f2", 1),
  ("g2", 2),
  ("g2", 2),
  ("g2", 1),
  ("d2", 2),
  ("f2", 2)
  ]

completeBass = bassPattern ++ bassPattern ++ bassPattern ++ bassPattern ++ bassPattern ++ bassPattern
completeLead = [("", 12)] ++ leadPattern0 ++ leadPattern0 ++ leadPattern1

bass :: Instrument
bass (freq, noteLength) = base * hull
  where
    base = modulatedSine (2*freq) (sine (3*freq))
    hull = hullCurve 0.01 0.05 0.3 noteLength 0.5

leadSynth :: Instrument
leadSynth (freq, noteLength) = base * hull
  where
    base = abs (modulatedSine freq (sine (2*freq)))
    hull = hullCurve 0.01 0.1 0 noteLength 0.0001

speed = 6/50
song = 0.2*(play bass (pattern2Notes speed completeBass)) + 0.4*(play leadSynth (pattern2Notes speed completeLead))
