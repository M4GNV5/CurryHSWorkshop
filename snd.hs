readRevPrint = do
    getLine >>= (putStrLn . reverse)

readMod3D = do
    x <- fmap read getLine
    if mod x 3 == 0 then
        return x
    else
        readMod3D
{-}
readMod3 =
    let x = fmap read getLine
    in
        if fmap (\x -> (mod x 3) == 0) x then
            x
        else
            readMod3
-}

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 val        = return []
replicateM count mval   = do
    val <- mval
    rest <- replicateM (count - 1) mval
    return (val : rest)

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM [] _       = return []
forM (x:xs) func    = do
    val <- func x
    rest <- forM xs func
    return (val : rest)

cp fin fout = readFile fin >>= writeFile fout

join :: (Monad m) => m (m a) -> m a
join x  = x >>= id

{-

do x <- foo
   bar ... x ...

   =====

foo >>= (\x -> bar ... x ...)

-}

whileM cond val =
    cond >>= \x ->
        if x then
            val >>= \y ->
                whileM cond val >>= \z ->
                    return (y : z)
        else
            return []

guessNumR :: Integer -> Integer -> IO Integer
guessNumR n step =
    putStrLn (show n) >>= \_ ->
        getLine >>= \line ->
            case line of
                "="    -> return n
                "<"    -> guessNumR (n - step) (max 1 (div step 2))
                ">"    -> guessNumR (n + step) (max 1 (div step 2))
                _      -> guessNumR n step

guessNum = do
    x <- guessNumR 50 25
    putStrLn $ "You number was: " ++ (show x)

allSumsOf :: Int -> [[Int]]
allSumsOf 0 = return []
allSumsOf n = do
    x <- [1 .. n]
    if x == n then
        return [x]
    else
        (map (x:) (allSumsOf (n - x)))

data Tree a   = Fork a (Tree a) (Tree a)
                | Nil

instance Show a => Show(Tree a) where
    show Nil = "E"
    show (Fork val left right) = "(T " ++ (show val) ++ " " ++ (show left) ++ " " ++ (show right) ++ ")"


insert :: Ord a => a -> Tree a -> Tree a
insert val Nil = Fork val Nil Nil
insert val (Fork v left right)
    | v == val  = Fork v left right
    | v > val   = Fork v (insert val left) right
    | v < val   = Fork v left (insert val right)

delete :: Ord a => a -> Tree a -> Tree a
delete val Nil = undefined
delete val (Fork v left right)
    | v == val  = Nil --TODO dont cut off
    | v > val   = Fork v (delete val left) right
    | v < val   = Fork v left (delete val right)
