module Main where

import Graphics.Gloss

main = display disp color pic
  where disp = (InWindow "Nice Window" (200, 200) (10, 10))
        color = white
        pic = pictures [Circle 80, Circle 40]
