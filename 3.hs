fibMod5 :: [Int]
fibMod5 = filter (\x -> x `mod` 5 == 0) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

perimeter :: [Point] -> Double
perimeter [p1, p2, p3] = distance p1 p2 + distance p2 p3 + distance p3 p1


checkAllEq :: Eq a => [a] -> Bool
checkAllEq [] = True
checkAllEq [_] = True
checkAllEq (x:y:rest) = x == y && checkAllEq (y:rest)



minDistance :: [Point] -> Double
minDistance points = minimum [distance p1 p2 | p1 <- points, p2 <- points, p1 /= p2]


computeProgram :: [String] -> Double -> Double
computeProgram [] num = num
computeProgram ("inc":rest) num = computeProgram rest (num + 1)
computeProgram ("dec":rest) num = computeProgram rest (num - 1)
computeProgram ("double":rest) num = computeProgram rest (num * 2)
computeProgram ("sqrt":rest) num = computeProgram rest (sqrt num)
computeProgram ("halveIfPositive":rest) num
    | num > 0 = computeProgram rest (num / 2)
    | otherwise = computeProgram rest num
computeProgram _ num = num 



