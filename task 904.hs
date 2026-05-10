import Data.List (nub)
import Data.Ratio
import Data.Bits

limitN :: Int
limitN = 45000

bigL :: Integer
bigL = 10^10

deg2rad :: Double
deg2rad = pi / 180.0

t0 :: Double
t0 = sqrt 2.0 - 1.0

gFun :: Double -> Double
gFun t =
    let tt = t * t
        d = 1.0 + tt
    in 3.0 * t * (1.0 - tt) / (d * d)

rootLeft :: Double -> Double
rootLeft y = go 0.0 t0 80
  where
    go lo hi 0 = (lo + hi) / 2.0
    go lo hi k =
        let mid = (lo + hi) / 2.0
        in if gFun mid < y
           then go mid hi (k - 1)
           else go lo mid (k - 1)

rootRight :: Double -> Double
rootRight y = go t0 1.0 80
  where
    go lo hi 0 = (lo + hi) / 2.0
    go lo hi k =
        let mid = (lo + hi) / 2.0
        in if gFun mid > y
           then go mid hi (k - 1)
           else go lo mid (k - 1)

isqrt :: Integer -> Integer
isqrt n = floor (sqrt (fromIntegral n :: Double))

cfCandidates :: Double -> Integer -> [(Integer, Integer)]
cfCandidates x l = nub (go x 0 1 1 0 80)
  where
    go frac p0 q0 p1 q1 0 = []
    go frac p0 q0 p1 q1 iter =
        let a = floor frac :: Integer
            p2 = a * p1 + p0
            q2 = a * q1 + q0
        in if p2*p2 + q2*q2 > l
           then
                let aa = p1*p1 + q1*q1
                    bb = 2 * (p0*p1 + q0*q1)
                    cc = p0*p0 + q0*q0 - l
                    disc = bb*bb - 4*aa*cc
                    s = if disc > 0 then isqrt disc else 0
                    kmax0 = if aa > 0 then (-bb + s) `div` (2*aa) else 0
                    kmax = min kmax0 (a - 1)
                    ks = [max 1 (kmax - 3) .. min (a - 1) (kmax + 3)]
                in [ (ps, qs)
                   | k <- ks
                   , let ps = k*p1 + p0
                   , let qs = k*q1 + q0
                   , ps > 0
                   , qs > 0
                   , ps*ps + qs*qs <= l
                   ]
           else
                let current =
                        if p2 > 0 && q2 > 0
                        then [(p2, q2)]
                        else []
                in if frac == fromIntegral a
                   then current
                   else current ++ go (1.0 / (frac - fromIntegral a))
                                        p1 q1 p2 q2 (iter - 1)

triangle :: Integer -> Integer -> (Integer, Integer, Integer)
triangle m n =
    let a = m*m - n*n
        b = 2*m*n
        c = m*m + n*n
        d = gcd a (gcd b c)
    in (a `div` d, b `div` d, c `div` d)

tanTheta :: Integer -> Integer -> Double
tanTheta a b =
    let aa = fromIntegral (a*a) :: Double
        bb = fromIntegral (b*b) :: Double
        ab = fromIntegral (a*b) :: Double
    in 3.0 * ab / (2.0 * (aa + bb))

fVal :: Double -> Integer -> Integer
fVal alphaDeg l =
    let alpha = alphaDeg * deg2rad
        y = tan alpha
        roots = [rootLeft y, rootRight y]
        candidates =
            [ chooseTriangle n m
            | r <- roots
            , (n, m) <- cfCandidates r l
            , 0 < n
            , n < m
            ]
        valid = [x | Just x <- candidates]
        best = foldl1 better valid
    in third best
  where
    chooseTriangle n m =
        let (a,b,c) = triangle m n
        in if c > l
           then Nothing
           else
                let k = l `div` c
                    theta = atan (tanTheta a b)
                    alpha = alphaDeg * deg2rad
                    diff = abs (theta - alpha)
                    area = k*k*a*b
                    perim = k*(a+b+c)
                in Just (diff, area, perim)

    better x@(d1,a1,_) y@(d2,a2,_)
        | d2 + 1e-16 < d1 = y
        | abs (d2 - d1) <= 1e-16 && a2 > a1 = y
        | otherwise = x

    third (_,_,p) = p

fSum :: Int -> Integer -> Integer
fSum n l =
    sum [ fVal ((fromIntegral i) ** (1.0 / 3.0)) l | i <- [1..n] ]

main :: IO ()
main = do
    print (fVal 30 (10^2))      
    print (fVal 10 (10^6))      
    print (fSum 10 (10^6))      
    print (fSum 45000 bigL)     
