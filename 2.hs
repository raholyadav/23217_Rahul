maxFg :: (Double -> Double) -> (Double -> Double) -> Double -> Double
maxFg f g p = max (f p) (g p)



maxExpF :: (Double -> Double) -> Double -> Double
maxExpF f p = max (exp p) (f p)


applyNTimes :: (Double -> Double) -> Double -> Int -> Double
applyNTimes f p n
    | n < 0 = error "Invalid value for N"
    | n == 0 = p
    | otherwise = applyNTimes f (f p) (n - 1)
