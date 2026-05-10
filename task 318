import Text.Printf

limit :: Int
limit = 2011

target :: Double
target = 2011 * log 10

valid :: Int -> Int -> Bool
valid p q =
    let sp = sqrt (fromIntegral p)
        sq = sqrt (fromIntegral q)
    in sq - sp < 1.0

nValue :: Int -> Int -> Int
nValue p q =
    let sp = sqrt (fromIntegral p)
        sq = sqrt (fromIntegral q)
        beta = (sq - sp) ^ 2
    in ceiling (target / (- log beta))

main :: IO ()
main = do
    let pairs =
            [ (p, q)
            | p <- [1 .. limit]
            , q <- [p + 1 .. limit - p]
            , valid p q
            ]

    let ans = sum [nValue p q | (p, q) <- pairs]

    print ans
