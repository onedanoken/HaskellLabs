import Data.Char (digitToInt)

-- Задача 1
descendants :: String -> [(String, String)] -> [String]
descendants person couplesFatherSon =
 let children = [child | (father, child) <- couplesFatherSon, person == father]
 in children ++ concatMap (\child -> descendants child couplesFatherSon) children
 
 -- Задача 2
lastManStanding :: [a] -> a
lastManStanding persons = case persons of
 [person] -> person
 _ -> lastManStanding $ take (length persons - 1) $ drop 5 $ cycle persons
 
 -- Задача 5
isSelfDual :: [Int] -> Bool
isSelfDual vector = and [vector !! i /= vector !! complement i | i <- [0..(length vector - 1)]]
 where complement i = length vector - 1 - i
 
-- Задача 6
isCorrectBankNumber :: String -> Bool
isCorrectBankNumber bankNumber =
  let reverseBankNumber = reverse bankNumber
      digitBankNumber = map digitToInt reverseBankNumber
      doubleDigitBankNumber = zipWith (*) digitBankNumber (cycle [1, 2])
      splitDigits = concatMap split doubleDigitBankNumber
  in sum splitDigits `mod` 10 == 0
  where split r = [r `div` 10, r `mod` 10]
 
-- Задача 4
anyPairAttacks :: [(Int, Int)] -> Bool
anyPairAttacks queens = any (\(q1, q2) -> attack q1 q2) (pairs queens)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

attack :: (Int, Int) -> (Int, Int) -> Bool
attack (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)

-- Tests Task1
test1 :: Bool
test1 = 
   let a = [("John", "David"), ("John", "Georgy"), ("Georgy","Viktor"), ("Viktor", "Valerii")] -- Output for "John":: ["David", "Georgy", "Viktor", "Valerii"]
       b = [("David", "Egor"), ("Egor", "Sasha"), ("Sasha", "Pasha")] -- Output for "David" :: ["Egor", "Sasha", "Pasha"]; for "Egor" :: ["Sasha", "Pasha"]; for "Sasha" :: ["Pasha"]
       c = [("2", "4"), ("1", "2"), ("1", "3")] --Output for "1" :: ["2", "3", "4"]; for "3" :: []; for "2" :: ["4"]
   in descendants "John" a == ["David","Georgy","Viktor","Valerii"] &&
      descendants "David" b == ["Egor","Sasha","Pasha"] &&
	  descendants "Egor" b == ["Sasha","Pasha"] &&
	  descendants "Sasha" b == ["Pasha"] &&
	  descendants "1" c == ["2","3", "4"] &&
	  descendants "3" c == [] &&
	  descendants "2" c == ["4"]

-- Tests Task2
test2 :: Bool
test2 = 
   let d = ["1", "2", "3", "4", "5"] -- Output :: "2"
       e = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"] -- Output :: "3"
   in lastManStanding d == "2" &&
      lastManStanding e == "3"
	  
-- Tests Task5
test5 :: Bool
test5 =
   let f = [1, 0, 0, 1] -- Output :: "False"
       g = [1, 0, 1, 0] -- Output :: "True"
   in isSelfDual f == False &&
      isSelfDual g == True
	  
-- Tests Task6
test6 :: Bool
test6 = 
   let h = "5469490019673975" -- Output :: "True"
       m = "4561261212345464" -- Output :: "False"
   in isCorrectBankNumber h == True &&
      isCorrectBankNumber m == False
	    
-- Tests Task4
test4 :: Bool
test4 =
   let i = [(1, 3), (10, 50), (2, 3)] -- Output :: "True"
       j = [(1, 3), (10, 50), (1, 2)] -- Output :: "True"
       k = [(8, 45), (10, 50), (2, 3)] -- Output :: "False"
   in anyPairAttacks i == True &&
      anyPairAttacks j == True &&
	  anyPairAttacks k == False
