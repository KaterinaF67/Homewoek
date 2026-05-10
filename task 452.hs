module Main where

modulo :: Integer
modulo = 1234567891

mLimit :: Integer
mLimit = 10^9

nLimit :: Integer
nLimit = 10^9

maxLen :: Int
maxLen = 30

fact :: [Integer]
fact = scanl (*) 1 [1..fromIntegral maxLen]

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b =
    let (g, x, y) = egcd b (a `mod` b)
    in (g, y, x - (a `div` b) * y)

modInv :: Integer -> Integer
modInv a =
    let (_, x, _) = egcd a modulo
    in x `mod` modulo

invFact :: [Integer]
invFact = map modInv fact

isqrt :: Integer -> Integer
isqrt x = floor (sqrt (fromIntegral x :: Double))

addFactor :: Integer -> Integer -> [Int] -> [Int]
addFactor x lastVal patternCounts
    | x == lastVal =
        let before = init patternCounts
            lastCount = last patternCounts
        in before ++ [lastCount + 1]
    | otherwise = patternCounts ++ [1]

falling :: Integer -> Int -> Integer
falling n r =
    product [ (n - fromIntegral i) `mod` modulo | i <- [0..r-1] ] `mod` modulo

ways :: Integer -> [Int] -> Integer
ways n patternCounts =
    let totalLen = sum patternCounts
        numerator = falling n totalLen
        denominatorInv =
            product [ invFact !! c | c <- patternCounts ] `mod` modulo
    in numerator * denominatorInv `mod` modulo

dfs :: Integer -> Integer -> Integer -> Integer -> [Int] -> Integer
dfs n rem start lastVal patternCounts =
    let current = ways n patternCounts
        s = isqrt rem

        smallPart =
            sum
            [ dfs n (rem `div` x) x x (addFactor x lastVal patternCounts)
            | x <- [start..s]
            ] `mod` modulo

        largeStart = max start (s + 1)

        largePart
            | largeStart > rem = 0
            | otherwise =
                let countAll = rem - largeStart + 1

                    sameContribution =
                        if lastVal >= largeStart && lastVal <= rem
                        then ways n (addFactor lastVal lastVal patternCounts)
                        else 0

                    countDifferent =
                        if lastVal >= largeStart && lastVal <= rem
                        then countAll - 1
                        else countAll

                    differentContribution =
                        if countDifferent > 0
                        then (countDifferent `mod` modulo) * ways n (patternCounts ++ [1]) `mod` modulo
                        else 0

                in (sameContribution + differentContribution) `mod` modulo

    in (current + smallPart + largePart) `mod` modulo

solve :: Integer -> Integer -> Integer
solve m n = dfs n m 2 0 []

main :: IO ()
main = do
    print (solve 10 10)


    print (solve (10^6) (10^6))

    print (solve mLimit nLimit)
