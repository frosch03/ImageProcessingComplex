{-# LANGUAGE FlexibleInstances #-}

module Picture ( Picture(..)
               , Mask
               , applyFilter
               , applyErosion
               , applyDilatation
               , toPict
               , toList
               )
where

data Picture a = Picture { unPicture :: [Row a] } deriving (Eq)
data Row     a = Row     { unRow     :: [Pixel a] } deriving (Eq)
data Pixel   a = Pixel   { unPixel   :: a } deriving (Eq)

type Mask = Picture

toList :: Picture a -> [[a]]
toList pic = map (map unPixel . unRow) $ unPicture pic

toPict :: [[a]] -> Picture a
toPict lst = Picture $ map (Row . map Pixel) lst


instance Show (Picture Int) where
    show (Picture [])     = []
    show (Picture (x:xs)) = show x ++ (show (Picture xs))
instance Show (Row Int) where
    show (Row [])     = "\n"
    show (Row (x:xs)) = show x ++ show (Row xs)
instance Show (Pixel Int) where
    show (Pixel x) | x >=   0 && x <=  41 = " "
                   | x >=  42 && x <=  83 = "."
                   | x >=  84 && x <= 125 = "o"
                   | x >= 126 && x <= 167 = "O"
                   | x >= 168 && x <= 209 = "#"
                   | x >= 210 && x <= 255 = "@"
                   | otherwise            = "E"

instance Show (Picture Bool) where
    show (Picture [])     = []
    show (Picture (x:xs)) = show x ++ (show (Picture xs))
instance Show (Row Bool) where
    show (Row [])     = "\n"
    show (Row (x:xs)) = show x ++ show (Row xs)
instance Show (Pixel Bool) where
    show (Pixel x) | x == True  = "O"
                   | x == False = "."
                   | otherwise  = "E"



applyFilter :: (Integral a) => Mask a -> Picture a -> Picture a
applyFilter = walkPic newFilterPix

applyErosion :: Mask Bool -> Picture Bool -> Picture Bool
applyErosion = walkPic newErosionPix

applyDilatation :: Mask Bool -> Picture Bool -> Picture Bool
applyDilatation = walkPic newDilatationPix




cropPic :: (Int, Int) -> Picture a -> Picture a
cropPic (x, y) pic = pic'
    where lst  = toList pic
          sp   = (map (take x)) . (take y) $ lst
          pic' = toPict sp

newPix :: (a -> a -> a) -> ([a] -> a) -> Mask a -> Picture a -> Pixel a
newPix op merge m sp = Pixel np
    where lstSP = concat $ toList sp
          lstM  = concat $ toList  m
          np    = merge  $ zipWith op lstM lstSP


newFilterPix :: (Integral a) => Mask a -> Picture a -> Pixel a
newFilterPix m sp = Pixel np
    where (Pixel tmp) = newPix (*) sum m sp
          lstM        = concat $ toList m
          sumM        = sum lstM
          np          = tmp `div` sumM

newErosionPix :: Mask Bool -> Picture Bool -> Pixel Bool
newErosionPix m sp = newPix (&&) minimum m sp

newDilatationPix :: Mask Bool -> Picture Bool -> Pixel Bool
newDilatationPix m sp = newPix (&&) maximum m sp


walkRow :: (Eq a) => (Mask a -> Picture a -> Pixel a) -> Mask a -> Picture a -> Row a
walkRow f m p = Row $ front ++ (walkRow' f m p) ++ back
    where lstP  = toList p
          lstM  = toList m

          maskProp  = (head $ map length lstM, length lstM)
          maskXSize = fst maskProp
          maskYSize = snd maskProp
          row   = head . (drop (maskYSize `div` 2)) . (take ((maskYSize `div` 2) + 1)) $ lstP
          front =           (map Pixel) . (take (maskXSize `div` 2)) $ row
          back  = reverse . (map Pixel) . (take (maskXSize `div` 2)) $ (reverse row)
          

walkRow' :: (Eq a) => (Mask a -> Picture a -> Pixel a) -> Mask a -> Picture a -> [Pixel a]
walkRow' f m p = if (drop maskXSize (head lstP)) == []
                    then f m subPic : []
                    else f m subPic : walkRow' f m nxtPic
    where lstP      = toList p
          lstM      = toList m
          maskProp  = (head $ map length lstM, length lstM) 
          maskXSize = fst maskProp 
          subPic    = cropPic maskProp p
          nxtPic    = toPict $ map (drop 1) lstP



apply :: (Eq a) => (Mask a -> Picture a -> Pixel a) -> Mask a -> Picture a -> Picture a
apply = walkPic

walkPic :: (Eq a) => (Mask a -> Picture a -> Pixel a) -> Mask a -> Picture a -> Picture a
walkPic f m p = Picture $ front ++ (walkPic' f m p) ++ back
    where rws   = unPicture p
          lstM  = toList m

          maskProp  = (head $ map length lstM, length lstM) -- Maybe instead of head the maximum is better? DRY
          maskYSize = snd maskProp

          front =           (take (maskYSize `div` 2)) $ rws
          back  = reverse . (take (maskYSize `div` 2)) $ (reverse rws)
          

walkPic' :: (Eq a) => (Mask a -> Picture a -> Pixel a) -> Mask a -> Picture a -> [Row a]
walkPic' f m p = if (drop maskYSize lstP) == []
                    then walkRow f m p : []
                    else walkRow f m p : walkPic' f m nxtP
    where lstP      = toList p
          lstM      = toList m
          nxtP      = toPict $ drop 1 lstP
          maskProp  = (head $ map length lstM, length lstM)-- Maybe instead of head the maximum is better?  DRY
          maskYSize = snd maskProp




-- [ ]   0 - 41
-- [.]  42 - 83
-- [o]  84 - 125
-- [O] 126 - 167
-- [X] 168 - 209
-- [#] 210 - 255
