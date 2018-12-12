import Lib
import System.Random
import Control.Exception
import Text.Printf

histogram :: [Double] -> [Item String] -> [Double]
histogram results [] = results
histogram [r, y, b] (Item "A" _ : xs) = histogram [r + 1, y, b] xs
histogram [r, y, b] (Item "B" _ : xs) = histogram [r, y + 1, b] xs
histogram [r, y, b] (Item "C" _ : xs) = histogram [r, y, b + 1] xs

main :: IO ()
main = do
    let samples = 1000
    let xs = [ Item "A" 7.0
             , Item "B" 2.0
             , Item "C" 1.0
             ]
    let g = mkStdGen 3
    let wheel = RWheel 0.0 0.0 [] :: RWheel String
    let queue = foldl enqueue wheel xs
    let got = applyRs samples g queue
    let results = histogram [0, 0, 0] got
    let [x, y, z] = map (/fromIntegral samples) results
    let err = abs (0.7 - x + 0.2 - y + 0.1 - z)  
    putStrLn "\n----------------"
    putStrLn $ printf "Sampling error: %.2f" $ assert (err < 0.1) err
    putStrLn "----------------"

applyRs :: Eq a => Int -> StdGen -> RWheel a -> [Item a]
applyRs 0 _ _ = []
applyRs n g queue =
    let (got, g') = get queue g 0
     in [got] ++ applyRs (n-1) g' queue
