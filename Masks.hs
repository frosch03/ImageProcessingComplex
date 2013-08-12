module Masks where  

import Picture (toPict, Mask)

mittelwert :: Mask Int
mittelwert = toPict $ 
    [ [1,1,1]
    , [1,1,1]
    , [1,1,1]
    ]

gaussTP :: (Integral a) => Mask a
gaussTP = toPict $ 
    [ [1,2,1]
    , [2,4,2]
    , [1,2,1]
    ]

nothing :: Mask Int 
nothing = toPict $ 
    [ [0, 0, 0]
    , [0, 1, 0]
    , [0, 0, 0]
    ]

waagerecht :: Mask Bool
waagerecht = toPict $ 
    [[True,  True,  True]]

senkrecht :: Mask Bool
senkrecht = toPict $ 
    [ [True]
    , [True]
    , [True]
    ]

slash :: Mask Bool
slash = toPict $ 
    [ [False, False, True]
    , [False, True, False]
    , [True, False, False]
    ]
