{-# OPTIONS_GHC -Wall #-}

import Data.Geo.Jord
import Data.Foldable (for_)

main :: IO ()
main = do
    for_ [minBound..maxBound] $ \i -> for_ [minBound..maxBound] $ \j ->
        if i == j
        then putStrLn $ unwords ["cityDistance", leftpad i, leftpad j, "=", "0"]
        else putStrLn $ unwords ["cityDistance", leftpad i, leftpad j, "=", show $ cityDistance i j ]

  where
    leftpad x = s ++ replicate (11 - length s) ' ' where s = show x

-------------------------------------------------------------------------------
-- Cities
-------------------------------------------------------------------------------

data City
    = Helsinki
    | Espoo
    | Tampere
    | Vantaa
    | Oulu

    | Turku
    | Jyvaskyla
    | Lahti
    | Kuopio
    | Pori

    | Kouvola
    | Joensuu
    | Lappeenranta
    | Hameenlinna
    | Vaasa

    | Seinajoki
    | Rovaniemi
    | Mikkeli
    | Kotka
    | Salo

    | Porvoo
    | Kokkola
    | Hyvinkaa
    | Lohja
    | Jarvenpaa

    | Rauma
    | Kajaani
    | Kerava
    | Savonlinna
    | Nokia
  deriving (Eq, Ord, Show, Enum, Bounded)

cityDistance :: City -> City -> Double
cityDistance a b = case surfaceDistanceE (cityPosition a) (cityPosition b) of
    Nothing -> error $ "Cannot calculate distance between " ++ show (a,b)
    Just d  -> toKilometres d

cityPosition :: City -> Position WGS84
cityPosition c = case cityWGS84 c of 
    (x, y) -> wgs84Pos x y zero

cityWGS84 :: City -> (Double, Double)
cityWGS84 Helsinki     = (60.166641, 24.943537)
cityWGS84 Espoo        = (60.206376, 24.656729)
cityWGS84 Tampere      = (61.497743, 23.76129)
cityWGS84 Vantaa       = (60.298134, 25.006641)
cityWGS84 Oulu         = (65.013785, 25.472099)

cityWGS84 Turku        = (60.45169, 22.266867)
cityWGS84 Jyvaskyla    = (62.241678, 25.749498)
cityWGS84 Lahti        = (60.980381, 25.654988)
cityWGS84 Kuopio       = (62.892983, 27.688935)
cityWGS84 Pori         = (61.483726, 21.7959)

cityWGS84 Kouvola      = (60.866825, 26.705598)
cityWGS84 Lappeenranta = (61.05875, 28.18769)
cityWGS84 Joensuu      = (62.602079, 29.759679)
cityWGS84 Hameenlinna  = (60.996174, 24.464425)
cityWGS84 Vaasa        = (63.092589, 21.615874)

cityWGS84 Seinajoki    = (62.786663, 22.84228)
cityWGS84 Rovaniemi    = (66.50279, 25.728479)
cityWGS84 Mikkeli      = (61.687727, 27.273224)
cityWGS84 Kotka        = (60.465521, 26.941153)
cityWGS84 Salo         = (60.384374, 23.126727)

cityWGS84 Porvoo       = (60.395372, 25.66656)
cityWGS84 Kokkola      = (63.837583, 23.131962)
cityWGS84 Hyvinkaa     = (60.631017, 24.861124)
cityWGS84 Lohja        = (60.250916, 24.065782)
cityWGS84 Jarvenpaa    = (60.481098, 25.100747)

cityWGS84 Rauma        = (61.128738, 21.511127)
cityWGS84 Kajaani      = (64.226734, 27.728047)
cityWGS84 Kerava       = (60.404869, 25.103549)
cityWGS84 Savonlinna   = (61.869803, 28.878498)
cityWGS84 Nokia        = (61.478774, 23.508499)
