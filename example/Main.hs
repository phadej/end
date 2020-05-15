{-# LANGUAGE DataKinds #-}
module Main (
    main,
    -- * Finnish Randonneur results
    route1, route2, route3, openRouteService,
) where

import Data.List          (intercalate)
import Data.Maybe         (fromMaybe)
import Data.Word          (Word64)
import Numeric            (showFFloat)
import System.Environment (getArgs)
import Text.Read          (readMaybe)

import qualified Data.Set as Set

import END
import END.Example.FinnishRandonneur
import END.Example.Ramp
import END.Example.SortingNetwork

main :: IO ()
main = do
    args <- getArgs
    case args of
        "ramp9"      : args' -> ramp9      (seed args')
        "ramp19"     : args' -> ramp19     (seed args')
        "sn7"        : args' -> sn7        (seed args')
        "sn8"        : args' -> sn8        (seed args')
        "sn9"        : args' -> sn9        (seed args')
        "sn10"       : args' -> sn10       (seed args')
        "sn12"       : args' -> sn12       (seed args')
        "frando5"    : args' -> frando5    (seed args')
        "frando9"    : args' -> frando9    (seed args')
        "frando-all" : args' -> frandoAll  (seed args')

        _ -> ramp9 42
  where
    seed :: [String] -> Word64
    seed []    = 42
    seed (s:_) = fromMaybe 42 (readMaybe s)


-------------------------------------------------------------------------------
-- Ramps
-------------------------------------------------------------------------------

ramp9 :: Word64 -> IO ()
ramp9 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 16 (Threshold 16 20) 50 500 Neighbors1)
    seed
    (rampProblem 9)


ramp19 :: Word64 -> IO ()
ramp19 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 64 (Threshold 64 15) 50 500 NeighborsMax2)
    seed
    (rampProblem 19)

-------------------------------------------------------------------------------
-- Sorting Networks
-------------------------------------------------------------------------------

sn7 :: Word64 -> IO ()
sn7 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 64 (Threshold 64 15) 50 500 NeighborsMax1)
    seed
    (sortingNetworkProblem 7 $ Metric 16 6)

sn8 :: Word64 -> IO ()
sn8 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 64 (Threshold 64 15) 50 500 NeighborsMax2)
    seed
    (sortingNetworkProblem 8 $ Metric 19 6)

sn9 :: Word64 -> IO ()
sn9 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 128 (Threshold 128 15) 50 500 NeighborsMax2)
    seed
    (sortingNetworkProblem 9 $ Metric 25 7)

sn10 :: Word64 -> IO ()
sn10 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 128 (Threshold 128 15) 50 500 NeighborsMax2)
    seed
    ((sortingNetworkProblem 10 (Metric 29 8))
        { initialCommitment = 4
        , initialSolution   = makeSolution $ concat
            [ [(0,5),(1,6),(2,7),(3,8),(4,9)]
            , [(0,3),(1,4),(5,8),(6,9)]
            , [(0,2),(3,6),(7,9)]
            ]
        })

sn12 :: Word64 -> IO ()
sn12 seed = print =<< evolvingNonDetermism
    (Config :: Config 32 128 (Threshold 128 15) 50 500 NeighborsMax2)
    seed
    ((sortingNetworkProblem 12 (Metric 39 9))
        { initialCommitment = 4
        , initialSolution   = makeSolution $ concat
            [ [(0,1),(2,3),(4,5),(6,7),(8,9),(10,11)]
            , [(0,2),(1,3),(4,6),(5,7),(8,10),(9,11)]
            , [(1,2),(5,6),(9,10)]
            ]
        })

-------------------------------------------------------------------------------
-- Sorting Network results
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Finnish Travelling Salesman
-------------------------------------------------------------------------------

-- [Helsinki,Turku,Tampere,Oulu,Jyvaskyla]
-- 1238 vs 1431km
frando5 :: Word64 -> IO ()
frando5 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 16 (Threshold 16 20) 50 500 Neighbors1)
    seed
    (finnishRandonneurProblem 0 $ Set.fromList [Helsinki, Tampere, Turku, Oulu, Jyvaskyla])

-- [Helsinki,Turku,Pori,Tampere,Jyvaskyla,Kuopio,Joensuu,Kouvola,Lahti]
-- 1150 vs 1362km
frando9 :: Word64 -> IO ()
frando9 seed = print =<< evolvingNonDetermism
    (Config :: Config 16 16 (Threshold 16 20) 50 500 Neighbors1)
    seed
    (finnishRandonneurProblem 0
    $ Set.fromList
        [ Helsinki, Tampere, Turku, Jyvaskyla
        , Lahti, Kuopio, Pori, Kouvola, Joensuu
        ])

frandoAll :: Word64 -> IO ()
frandoAll seed = print =<< evolvingNonDetermism
    (Config :: Config 16 128 (Threshold 128 8) 300 3000 NeighborsMax1)
    seed
    (finnishRandonneurProblem 2445 $ Set.fromList [minBound .. maxBound])

-------------------------------------------------------------------------------
-- Finnish Travelling Salesman results
-------------------------------------------------------------------------------

-- fitness 2446
--
-- OSM: 2375km (without Oulu and Rovaniemi)
route1 :: [City]
route1 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Lahti,Jyvaskyla,Seinajoki,Vaasa,Kokkola
    ,Oulu,Rovaniemi,Kajaani,Kuopio,Joensuu
    ,Savonlinna,Mikkeli,Lappeenranta,Kouvola,Kotka
    ,Porvoo,Hyvinkaa,Jarvenpaa,Kerava,Vantaa
    ,Helsinki]

-- fitness 2449
--
-- OSM: 2376km (without Oulu and Rovaniemi)
route2 :: [City]
route2 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Lahti,Mikkeli,Jyvaskyla,Seinajoki,Vaasa
    ,Kokkola,Oulu,Rovaniemi,Kajaani,Kuopio
    ,Joensuu,Savonlinna,Lappeenranta,Kouvola,Kotka
    ,Porvoo,Hyvinkaa,Jarvenpaa,Kerava,Vantaa
    ]

-- fitness 2460
--
-- OSM: 2363km (without Oulu and Rovaniemi)
route3 :: [City]
route3 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Seinajoki
    ,Vaasa,Kokkola,Oulu,Rovaniemi,Kajaani
    ,Kuopio,Joensuu,Savonlinna,Jyvaskyla,Mikkeli
    ,Lappeenranta,Kotka,Kouvola,Lahti,Hameenlinna
    ,Hyvinkaa,Jarvenpaa,Porvoo,Kerava,Vantaa]

openRouteService :: [City] -> String
openRouteService cs = concat $
    [ "https://maps.openrouteservice.org/directions?n1=62.410729&n2=26.438599&n3=7&"
    , "a="
    ] ++
    [ intercalate ","
        [ showFFloat (Just 6) x "" ++ "," ++ showFFloat (Just 6) y ""
        | c <- cs
        , let (x,y) = cityWGS84 c
        ]
    ] ++
    [ "&b=0&c=1&k1=en-US&k2=km"
    ]
