import Data.Char (isDigit)

data BinTree = 
    None
  | Tree String BinTree BinTree

-- Показ дерева
instance Show BinTree where
  show None = ""
  show (Tree x None None) = show x
  show (Tree x None right) = show x ++ "(" ++ show right ++ ")"
  show (Tree x left None) = "(" ++ show left ++ ")" ++ show x
  show (Tree x left right) = "(" ++ show left ++ ")" ++ show x ++ "(" ++ show right ++ ")"  
  
-- Проверка, что строка является числом
isNumber :: String -> Bool
isNumber x = all isDigit x 

-- Приоритет оператора
operatorPriority :: String -> Int
operatorPriority op = case op of
 "*" -> 2
 "/" -> 2
 "-" -> 1
 "+" -> 1
  
-- Преобразование арифметического выражения в ОПЗ
toRPN :: String -> String
toRPN ariph = make (words ariph) [] []
  where
      make [] [] out = out
      make [] (x:xs) out = make [] xs (out ++ x ++ " ")
      make (x:xs) stack out
         | isNumber	x = make xs stack (out ++ x ++ " ")
         | otherwise = let (higherOps, lowerOps) = span (\op -> operatorPriority op >= operatorPriority x) stack
                       in make xs (x:lowerOps) (out ++ concatMap add_space higherOps)
           where add_space x = x ++ " "

-- Обход дерева		  
lrn :: BinTree -> String
lrn None = ""
lrn (Tree x None None) = x
lrn (Tree x left None) = lrn left ++ " " ++ x
lrn (Tree x None right) = x ++ " " ++ lrn right
lrn (Tree x left right) = lrn left ++ " " ++ lrn right ++ " " ++ x	
 
-- Создание дерева по ОПЗ 
createBinTree :: String -> BinTree
createBinTree expr = head (foldl foldingFunction [] (words (toRPN expr)))
   where
      foldingFunction(x:y:ys) "*" = (Tree "*" y x):ys
      foldingFunction(x:y:ys) "+" = (Tree "+" y x):ys
      foldingFunction(x:y:ys) "-" = (Tree "-" y x):ys
      foldingFunction(x:y:ys) "/" = (Tree "/" y x):ys
      foldingFunction xs numberString = (Tree numberString None None):xs

-- Вычисление выражения по ОПЗ
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
   where
      foldingFunction (x:y:ys) "*" = (x * y):ys
      foldingFunction (x:y:ys) "+" = (x + y):ys
      foldingFunction (x:y:ys) "-" = (y - x):ys
      foldingFunction (x:y:ys) "/" = (y / x):ys
      foldingFunction xs numberString = read numberString:xs
	  
calculation :: String -> Double
calculation x = solveRPN (lrn (createBinTree x))
	  
	  
-- Тесты
test :: Bool
test = let a = createBinTree "22 * 3 - 22 * 2 - 1"
           b = createBinTree "12 / 4 - 3"
	   in lrn a == "22 3 * 22 2 * - 1 -" &&
	      lrn b == "12 4 / 3 -" &&
		  solveRPN (lrn a) == 21 &&
		  solveRPN (lrn b) == 0