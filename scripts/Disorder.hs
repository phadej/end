import Control.Monad (when)
import Numeric (showFFloat)
import Data.Foldable (for_)
import Data.IORef

drawDisorder :: Int -> String -> IO Int
drawDisorder size input = do
    ref <- newIORef 0

    when (length input /= size2) $ fail $ "Invalid length " ++ show (length input)
    putStrLn "\\begin{tikzpicture}"

    for_ [0..size-1] $ \x -> for_ [0..size-1] $ \y -> do
        let c = index x y
        putStrLn $ concat
            [ "\\draw ("
            , scaleCoord x 0.5, ",", scaleCoord y 0.5
            , ") node {", [c] , "};"
            ]

        let x' = (x + 1) `mod` size
        let y' = (y + 1) `mod` size

        let yy = scaleCoord y 0.75
        when (c /= index x y') $ do
            modifyIORef' ref succ
            putStrLn $ concat
                [ "\\draw[thick] ("
                , scaleCoord x 0.25, ",", yy
                , ") -- ("
                , scaleCoord x 0.75, ",", yy
                , ");"
                ]

        let xx = scaleCoord x 0.75
        when (c /= index x' y) $ do
            modifyIORef' ref succ
            putStrLn $ concat
                [ "\\draw[thick] ("
                , xx, ",", scaleCoord y 0.25
                , ") -- ("
                , xx, ",", scaleCoord y 0.75
                , ");"
                ]

    putStrLn "\\end{tikzpicture}"
    readIORef ref
  where
    size2 = size * size

    scaleCoord :: Int -> Double -> String
    scaleCoord n off = showFFloat (Just 2) (off + fromIntegral n / 2 :: Double) ""

    index :: Int -> Int -> Char
    index x y = input !! (y * size + x)

-- drawDisorder 16 "3333653531111133333355553111163330300255111149233111222041444223311122000444402321142210424000073144211112400003144411112100004351411112211310455511777221131155532111112113155552224112210333352224447300033333222243300111333262223333111111166632633911111111"
-- drawDisorder 16 "3032633311111233333303001111222330110000111122221111000001142222221100004144422222111114441000231241111041100003111411112110000111111111211100011111117122133011121111122133333322211113111133332222133221113333223223332111111233323303111111133332233331111113"
-- drawDisorder 16 "2211101011122222121111001112202211111110011000021111111001000000111111110000000002111101000000002221100000000000021111110000000001111110100000000111111110000000111111111100000001111111111000000111111111100000111111111111010001111111111110000211011111111000"
-- drawDisorder 16 "0000000000000000000000000000000000100000000000000010000000000000001100000000000000110000000000000001001000000000000001100000000000000010000000000000111000000000000111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
