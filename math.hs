module Math where
import Data.Char

data Expr =   Const Double
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
    deriving Show

eval :: Expr -> Double
eval (Const x)          = x
eval (Add x y)          = eval x + eval y
eval (Sub x y)          = eval x - eval y
eval (Mul x y)          = eval x * eval y
eval (Div x y)          = eval x / eval y

isNumChar c             = isDigit c || c == '.' || c == 'e'

skipSpaces str          = drop (length (takeWhile isSpace str)) str

readNum :: String -> (Expr, String)
readNum str             =
    if length valstr > 0
        then (Const val, drop (length valstr) str)
        else error ("Expected number got " ++ show (take 10 str))
    where
        valstr          = takeWhile isNumChar (skipSpaces str)
        val             = read valstr

readOp :: Char -> Expr -> Expr -> Expr
readOp '+' x y          = Add x y
readOp '-' x y          = Sub x y
readOp '*' x y          = Mul x y
readOp '/' x y          = Div x y
readOp x _ _            = error ("Unknown operator " ++ show x)

precendence :: Char -> Int
precendence '+'         = 1
precendence '-'         = 1
precendence '*'         = 2
precendence '/'         = 2
precendence x           = error ("Unknown operator " ++ show x)

parseBinary :: Expr -> String -> Int -> (Expr, String)
parseBinary left str prec =
    if hasOp
        then if opPrec > prec
            then ((readOp op left result), str3)
            else (left, str1)
        else (left, "")
     where
        str1            = skipSpaces str
        hasOp           = length str1 > 0
        op              = head str1
        opPrec          = precendence op
        (right, str2)   = readNum (skipSpaces $ tail str1)
        (result, str3)  = parseBinary right str2 opPrec

parseR :: (Expr, String) -> (Expr, String)
parseR x                =
    if length str > 0
        then parseR $ parseBinary val str 0
        else (val, str)
    where
        (val, str)      = parseBinary (fst x) (snd x) 0

parse :: String -> Expr
parse str               = fst $ parseR $ readNum str
