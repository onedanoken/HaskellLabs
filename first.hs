-- Задача 1
combinationsWithRepetitions :: Int -> Int -> Int
combinationsWithRepetitions n m
	| n < m = 0
	| otherwise = a `div` b
	where
		a = ( product [1..n+m-1] ) 
		b = ( product [1..n - 1] * product [1..m] )

-- Задача 4
quadratic ::  Double -> Double -> Double -> [Double]
quadratic a b c
	| a == 0 = if b == 0
	           then []
			   else [-c / b]
	| d > 0 = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
	| d == 0 =  [(-b) / (2 * a)]
	| otherwise = []
	where
		d = b^2 - 4 * a * c

test1 :: Bool
test1 = 
    let n = 10
        m = 5
        n1 = 15
        m1 = 6
        n2 = 4
        m2 = 10
    in combinationsWithRepetitions n m == 2002 &&
       combinationsWithRepetitions n1 m1 == 38760 &&
       combinationsWithRepetitions n2 m2 == 0
	   
test4 :: Bool
test4 = 
    let a = 1
        b = 4
        c = 3
        a1 = 0
        b1 = 12
        c1 = 6
        a2 = 10
        b2 = 1
        c2 = 2
        a3 = 1
        b3 = -2
        c3 = 1
    in quadratic a b c == [-1,-3] &&
       quadratic a1 b1 c1 == [-0.5] &&
       quadratic a2 b2 c2 == [] &&
       quadratic a3 b3 c3 == [1]	   