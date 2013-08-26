{-# LANGUAGE FlexibleInstances #-}

module Morph where

type Position = (Int, Int)

data Picture a 
    = Picture { unPicture :: [[a]] 
              } deriving (Eq)

instance Show (Picture Bool) where
    show (Picture (row:[]))   = toTxt row ++ "\n"
    show (Picture (row:rows)) = toTxt row ++ show (Picture rows)
    show otherwise            = error $ "empty picture"
        
toTxt []         = "\n"
toTxt (True :xs) = 'O' : toTxt xs
toTxt (False:xs) = '.' : toTxt xs

data Mask a 
    = Mask { maskPicture  :: Picture a
           , maskRefPoint :: Position 
           }
           deriving (Eq)

section :: (Int, Int) -> [a] -> [a]
section (start, len) = (take len) . (drop start)

groupInto :: Int -> [a] -> [[a]]
groupInto i [] = [[]]
groupInto i xs = (take i xs) : (groupInto i (drop i xs))

spacer :: Mask a -> (Int, Int, Int, Int)
spacer (Mask (Picture m) (mX, mY)) = (north, east, south, west)
    where north = mY
          east  = lX - mX - 1
          south = lY - mY - 1
          west  = mX
          lX = length . head $ m
          lY = length        $ m

prepare :: Picture a -> Mask a -> Picture (a, Maybe [[a]])
prepare (Picture p) mask@(Mask (Picture m) _)
    = Picture $ groupInto piclX $ zip (concat p) env
    where env     = [ if (  ((x - offsetW) < 0) 
                         || ((y - offsetN) < 0)
                         || ((x + offsetE) >= piclX)
                         || ((y + offsetS) >= piclY)
                         ) 
                         then Nothing
                         else Just (map (section ((x - offsetW), masklX)) $ (section ((y - offsetN), masklY)) $ p) 
                      | y <- [0..(piclY-1)], x <- [0..(piclX-1)]
                    ]
          piclX   = length . head $ p 
          piclY   = length        $ p
          masklX  = length . head $ m
          masklY  = length        $ m
          (offsetN, offsetE, offsetS, offsetW) = spacer mask
          

apply :: (([a] -> [a] -> [b]), ([b] -> a)) -> Picture a -> Mask a -> Picture a
apply (zipit, foldit) pict@(Picture p) mask@(Mask (Picture m) _)
    = Picture $ map (map calcNewPixel) prepared
    where (Picture prepared) = prepare pict mask
          calcNewPixel (pixel, Nothing)  = pixel
          calcNewPixel (_    , Just env) = foldit $ zipit (concat env) (concat m)


type MaskBit    = Bool
type PictureBit = Bool
mergeTr :: PictureBit -> MaskBit -> Maybe Bool
mergeTr True  True = Just True
mergeTr False True = Just False
mergeTr _     _    = Nothing

reduceTr :: (a -> a -> a) -> a -> Maybe a -> a 
reduceTr op done  (Just next) = op done next
reduceTr _  done   Nothing    = done

erode :: Picture Bool -> Mask Bool -> Picture Bool
erode = apply (zipWith mergeTr, foldl (reduceTr (&&)) True)

dilate :: Picture Bool -> Mask Bool -> Picture Bool
dilate = apply (zipWith mergeTr, foldl (reduceTr (||)) False)
















dot :: Mask Bool
dot = Mask (Picture [[True]]) (0,0)

senkrecht :: Mask Bool
senkrecht = Mask (Picture senkrecht') (0,1)

senkrecht' :: [[Bool]] 
senkrecht' = [ [True]
             , [True]
             , [True]
             ]

waagerecht :: Mask Bool
waagerecht = Mask (Picture waagerecht') (1,0)

waagerecht' :: [[Bool]]
waagerecht' = [[True, True, True]]

slash :: Mask Bool
slash = Mask (Picture slash') (1,1)

slash' :: [[Bool]]
slash' = [ [False, False, True]
         , [False, True, False]
         , [True, False, False]
         ]

testSlash :: Picture Bool
testSlash = Picture slash'

testPict :: Picture Bool
testPict = Picture $ [ [False, False, False, False, False]
                     , [False, False, False, True,  False]
                     , [False, False, True,  True,  False]
                     , [False, True,  True,  False, False]
                     , [False, True,  True,  False, False]
                     ]

test42 :: Picture Bool
test42 = Picture $ 
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

tstMsk :: Mask Bool
tstMsk = Mask (Picture tstMsk') (0,1)

tstMsk' :: [[Bool]]
tstMsk' = [ [False]
          , [True]
          ]

tstPct :: Picture Bool
tstPct = Picture $
        [ [False, False, True,  False]
        , [False, False, True,  False]
        , [False, True,  False, False]
        , [False, False, False, False]
        ]
