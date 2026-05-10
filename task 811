import Data.Bits
import Data.List

modulo :: Integer
modulo = 1000062031

t :: Integer
t = 10^14 + 31

r :: Integer
r = 62

binomials :: Integer -> [Integer]
binomials n = scanl next 1 [0 .. n - 1]
  where
    next c k = c * (n - k) `div` (k + 1)

bitPositions :: Integer -> Integer -> [Integer]
bitPositions shift power =
    sort
    [ k * shift + bit
    | (k, c) <- zip [0..] (binomials power)
    , bit <- bitsOf c
    ]

bitsOf :: Integer -> [Integer]
bitsOf 0 = []
bitsOf x = go x 0
  where
    go 0 _ = []
    go n i
        | testBit n 0 = i : go (shiftR n 1) (i + 1)
        | otherwise   = go (shiftR n 1) (i + 1)

aFromPositions :: [Integer] -> Integer
aFromPositions positions =
    foldl step 1 gaps
  where
    desc = reverse positions
    onesCount = length positions

    vList = take onesCount $ iterate (\x -> (5 * x + 3) `mod` modulo) 1

    gaps =
        [ (fromIntegral i + 1, desc !! i - desc !! (i + 1) - 1)
        | i <- [0 .. length desc - 2]
        ]

    step acc (idx, gap)
        | gap <= 0  = acc
        | otherwise =
            let base = vList !! idx
            in acc * powMod base gap modulo `mod` modulo

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod a e m
    | even e    = half * half `mod` m
    | otherwise = a * powMod a (e - 1) m `mod` m
  where
    half = powMod a (e `div` 2) m

main :: IO ()
main = do
    let positions = bitPositions t r
    print (aFromPositions positions)
