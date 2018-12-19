module Song3 where

import Wave
import Notes

import Sound -- Das Modul mit Ihrer Implementierung
import Instruments

leadPattern0 = [
  ("", 4),
  ("c4", 4),
  ("e4", 4),
  ("a4", 4),
  ("b4", 4),
  ("e4", 4),
  ("c4", 4),
  ("b4", 4),
  ("c5", 4),
  ("e4", 4),
  ("c4", 4),
  ("c5", 4),
  ("f#4",4),
  ("d4", 4),
  ("a3", 4),
  ("f#4",4),
  ("e4",4),
  ("c4",4),
  ("a3",4),
  ("c4",4),
  ("", 4),
  ("e4",4),
  ("c4",4),
  ("a3",4),
  ("b3",4),
  ("c4",4),
  ("c4",4)
  ]

leadPattern1 = [
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("",4),
  ("", 4),
  ("", 4),
  ("", 4),
  ("",4),
  ("",4),
  ("",4),
  ("",4),
  ("",4),
  ("",4),
  ("",4),
  ("",4),
  ("g3",4),
  ("a3",4),
  ("a3",4)
  ]

bassPattern = [
  ("g3",  4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("g#3", 4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("g3",  4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("f#3", 4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("f3",  4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("",    4),
  ("f#3", 4),
  ("",    4),
  ("",    4),
  ("b2",  4),
  ("a2",  4),
  ("a2",  4)]

completeBass = bassPattern
completeLead = leadPattern0  ++ leadPattern1

speed = 6/50
song = 0.33*(play guitar (pattern2Notes speed completeBass)) +
       0.33*(play guitar (pattern2Notes speed leadPattern0)) +
       0.33*(play guitar (pattern2Notes speed leadPattern1))
