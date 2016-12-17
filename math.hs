module Math where
import Data.Char

data Expr =	Const Double
			| Add Expr Expr
			| Sub Expr Expr
			| Mul Expr Expr
			| Div Expr Expr
			| Mod Expr Expr
	deriving Show

eval :: Expr -> Double
eval (Const x)			= x
eval (Add x y)			= eval x + eval y
eval (Sub x y)			= eval x - eval y
eval (Mul x y)			= eval x * eval y
eval (Div x y)			= eval x / eval y

isNumChar c				=  isDigit c || c == '.' || c == 'e'

readNum :: String -> (Double, String)
readNum str				=
	if length valstr > 0
		then (val, drop (length valstr) str)
		else error ("Expected number got " ++ show (take 10 str))
	where
		valstr			= takeWhile isNumChar str
		val				= read valstr

readOp :: Char -> Expr -> Expr -> Expr
readOp '+' x y			= Add x y
readOp '-' x y			= Sub x y
readOp '*' x y			= Mul x y
readOp '/' x y			= Div x y
readOp x _ _			= error ("Unknown operator " ++ show x)

parseBinary :: String -> (Expr, String)
parseBinary str1		=
	if hasOp
		then ((readOp op left right), str3)
		else (left, "")
 	where
		(num, str2)		= readNum str1
		left			= Const num
		hasOp			= length str2 > 0
		op				= head str2
		(right, str3)	= parseBinary (tail str2)

parse :: String -> Expr
parse str				= fst $ parseBinary str

{-
TODO
precendece '+'			= 1
precendece '+'			= 1
precendece '*'			= 2
precendece '/'			= 2
-}
