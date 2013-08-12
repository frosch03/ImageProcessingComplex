module Test where

import Picture
import Masks

-- Image operators
mirrorHorizontal :: Picture a -> Picture a
mirrorHorizontal = toPict . reverse . toList

mirrorVertical :: Picture a -> Picture a
mirrorVertical = toPict . map reverse . toList

-- Filter operator
weichzeichner :: Picture Int -> Picture Int
weichzeichner = applyFilter gaussTP

-- Morph operators
open :: Mask Bool -> Picture Bool -> Picture Bool
open m p  = applyDilatation m . applyErosion m    $ p

close :: Mask Bool -> Picture Bool -> Picture Bool
close m p = applyErosion m    . applyDilatation m $ p



test42 :: Picture Bool
test42 = toPict $ 
         [empty]
      ++ [empty]
      ++ [empty]
      ++ [[False, True, False, False, False, True, False, False, False, False, True, True, True, True, False, False]]
      ++ [[False, True, False, False, False, True, False, False, False, True, False, False, False, False, True, False]]
      ++ [[False, True, False, False, False, True, False, False, False, True, False, False, False, False, True, False]]
      ++ [[False, True, False, False, False, True, False, False, False, False, False, False, False, False, True, False]] 
      ++ [[False, True, True, True, True, True, False, False, False, False, False, False, False, True, False, False]]
      ++ [[False, False, False, False, False, True, False, False, False, False, False, False, True, True, False, False]]
      ++ [[False, False, False, False, False, True, False, False, False, False, False, True, True, False, False, False]]
      ++ [[False, False, False, False, False, True, False, False, False, False, True, True, False, False, False, False]]
      ++ [[False, False, False, False, False, True, False, False, False, True, True, False, False, False, False, False]]
      ++ [[False, False, False, False, False, True, False, False, False, True, True, True, True, True, True, False]]
      ++ [empty]
      ++ [empty]
      ++ [empty]
    where empty = (replicate 16 False)
-- +----------------+
-- |                |
-- |                |
-- |                |
-- | #   #    ####  |
-- | #   #   #    # |
-- | #   #   #    # |
-- | #   #        # |
-- | #####       #  |
-- |     #      ##  |
-- |     #     ##   |
-- |     #    ##    |
-- |     #   ##     |
-- |     #   ###### |
-- |                |
-- |                |
-- |                |
-- +----------------+
                

testimage1 :: Picture Int
testimage1 = toPict $
                         [empty]
          ++ (replicate 5 form1)
          ++ (replicate 2 empty)
          ++ (replicate 2 form2)
          ++ (replicate 8 form3)
          ++ (replicate 2 form2)
          ++ (replicate 2 empty)
          ++             [form4]
          ++             [form5]
          ++             [form6]
          ++             [form6]
          ++             [form5]
          ++             [form4]
          ++ (replicate 4 empty)
    where empty = (replicate 32 0)
          form1 = [0,0] ++ (replicate 10 255) ++ (replicate 20 0)
        
          form2 = (replicate 9 0) ++ (replicate 12 255) ++ (replicate 11 0)
          form3 = (replicate 9 0) ++ 255 : (replicate 9 0) ++ [255,255] ++ (replicate 11 0)
        
          form4 = (replicate 6 0) ++ 255 : (replicate 4 0) ++ 255 : (replicate 20 0)
          form5 = (replicate 7 0) ++ 255 : (replicate 2 0) ++ 255 : (replicate 21 0)
          form6 = (replicate 8 0) ++ [255, 255] ++ (replicate 22 0)
-- |           testimage1           |
-- +--------------------------------+
-- |                                | <- empty
-- |  ##########                    | <- form1
-- |  ##########                    |
-- |  ##########                    |
-- |  ##########                    |
-- |  ##########                    |
-- |                                |
-- |                                |
-- |         ############           | <- form2
-- |         ############           |
-- |         #         ##           | <- form3
-- |         #         ##           |
-- |         #         ##           |
-- |         #         ##           |
-- |         #         ##           |
-- |         #         ##           |
-- |         #         ##           |
-- |         #         ##           |
-- |         ############           |
-- |         ############           |
-- |                                |
-- |                                |
-- |      #    #                    | <- form4
-- |       #  #                     | <- form5
-- |        ##                      | <- form6
-- |        ##                      |
-- |       #  #                     |
-- |      #    #                    |
-- |                                |
-- |                                |
-- |                                |
-- |                                |
-- +--------------------------------+
