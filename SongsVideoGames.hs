module SongsVideoGames where

import Wave
import Notes

import Sound -- Das Modul mit Ihrer Implementierung
import Instruments
import SongCollection

songSuperMario = 0.33*(play bass (pattern2Notes speed sm_bass_pattern))
                + 0.15*(play bass (pattern2Notes speed sm_steel_drums1))
                + 0.15*(play bass (pattern2Notes speed sm_steel_drums2))
                + 0.25*(play leadSynth (pattern2Notes speed sm_marimba))
    where speed = 24/50
   
songLegendOfZelda = play leadSynth (pattern2Notes speed loz)
    where speed = 24/50
   
songMegaMan = play leadSynth (pattern2Notes speed mm)
    where speed = 24/50
   
songTetris = play leadSynth (pattern2Notes speed tetris)
    where speed = 60/140
          tetris = tetris_pattern1 ++ tetris_pattern1 ++ tetris_pattern2

songNecrodancerLevel4_3 = 0.5*(play leadSynth (pattern2Notes speed wight))
                            + 0.3*(play bass2 (pattern2Notes speed wight_bass))
    where speed = 60/160

songNecrodancerDeathMetal = 0.32*(play bass3 (pattern2Notes speed dm1))
                            + 0.32*(play bass3 (pattern2Notes speed dm2))
                            + 0.32*(play leadSynth (pattern2Notes speed dm3))
    where speed = 120/175

songMetroidRidley = 1/6 * sum (map (play leadSynth . pattern2Notes speed) super_metroid_ridley)
    where speed = 60/115

songUndertaleMegalovania = 0.23 * play leadSynth (pattern2Notes speed undertale_1)
                            + 0.23 * (1/6 * sum (map (play leadSynth . pattern2Notes speed) undertale_2))
                            + 0.23 * play leadSynth (pattern2Notes speed undertale_3)
                            + 0.17 * play leadSynth (pattern2Notes speed undertale_4)
    where speed = 60/120
