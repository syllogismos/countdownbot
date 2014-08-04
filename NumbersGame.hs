{-# LANGUAGE ScopedTypeVariables #-}

-- http://www.cs.nott.ac.uk/~gmh/countdown.hs
-----------------------------------------------------------------------------
--
--                           The Countdown Problem
--
--                               Graham Hutton
--			                    University of Nottingham
--
--                               November 2001
--
-----------------------------------------------------------------------------

import System.IO()
import System.Environment

-----------------------------------------------------------------------------
-- Formally specifying the problem
-----------------------------------------------------------------------------

data Op               = Add | Sub | Mul | Div

valid                :: Op -> Int -> Int -> Bool
valid Add _ _         = True
valid Sub x y         = x > y
valid Mul _ _         = True
valid Div x y         = x `mod` y == 0
 
apply                :: Op -> Int -> Int -> Int
apply Add x y         = x + y
apply Sub x y         = x - y
apply Mul x y         = x * y
apply Div x y         = x `div` y

data Expr             = Val Int | App Op Expr Expr

values               :: Expr -> [Int]
values (Val n)        = [n]
values (App _ l r)    = values l ++ values r

eval                 :: Expr -> [Int]
eval (Val n)          = [n | n > 0]
eval (App o l r)      = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subbags              :: [a] -> [[a]]
subbags xs            = [zs | ys <- subs xs, zs <- perms ys]

subs                 :: [a] -> [[a]]
subs []               = [[]]
subs (x:xs)           = ys ++ map (x:) ys
                        where
                           ys = subs xs

perms                :: [a] -> [[a]]
perms []              = [[]]
perms (x:xs)          = concatMap (interleave x) (perms xs)

interleave           :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y:ys)   = (x:y:ys) : map (y:) (interleave x ys)

solution             :: Expr -> [Int] -> Int -> Bool
solution e ns n       = elem (values e) (subbags ns) && eval e == [n]

-----------------------------------------------------------------------------
-- Brute force implementation
-----------------------------------------------------------------------------

split                :: [a] -> [([a],[a])]
split []              = [([],[])]
split (x:xs)          = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- split xs]

nesplit              :: [a] -> [([a],[a])]
nesplit               = filter ne . split

ne                   :: ([a],[b]) -> Bool
ne (xs,ys)            = not (null xs || null ys)

exprs                :: [Int] -> [Expr]
exprs []              = []
exprs [n]             = [Val n]
exprs ns              = [e | (ls,rs) <- nesplit ns
                           , l       <- exprs ls
                           , r       <- exprs rs
                           , e       <- combine l r]

combine              :: Expr -> Expr -> [Expr]
combine l r           = [App o l r | o <- ops]
 
ops                  :: [Op]
ops                   = [Add,Sub,Mul,Div]

solutions            :: [Int] -> Int -> [Expr]
solutions ns n        = [e | ns' <- subbags ns, e <- exprs ns', eval e == [n]]

-----------------------------------------------------------------------------
-- Fusing generation and evaluation
-----------------------------------------------------------------------------

type Result           = (Expr,Int)

results              :: [Int] -> [Result]
results []            = []
results [n]           = [(Val n,n) | n > 0]
results ns            = [res | (ls,rs) <- nesplit ns
                             , lx      <- results ls
                             , ry      <- results rs
                             , res     <- combine' lx ry]

combine'             :: Result -> Result -> [Result]
combine' (l,x) (r,y)  = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions'           :: [Int] -> Int -> [Expr]
solutions' ns n       = [e | ns' <- subbags ns, (e,m) <- results ns', m == n]

-----------------------------------------------------------------------------
-- Exploiting arithmetic properties
-----------------------------------------------------------------------------

valid'               :: Op -> Int -> Int -> Bool
valid' Add x y        = x <= y
valid' Sub x y        = x > y
valid' Mul x y        = x /= 1 && y /= 1 && x <= y
valid' Div x y        = y /= 1 && x `mod` y == 0

eval'                :: Expr -> [Int]
eval' (Val n)         = [n | n > 0]
eval' (App o l r)     = [apply o x y | x <- eval' l, y <- eval' r, valid' o x y]

solution'            :: Expr -> [Int] -> Int -> Bool
solution' e ns n      = elem (values e) (subbags ns) && eval' e == [n]

results'             :: [Int] -> [Result]
results' []           = []
results' [n]          = [(Val n,n) | n > 0]
results' ns           = [res | (ls,rs) <- nesplit ns
                             , lx      <- results' ls
                             , ry      <- results' rs
                             , res     <- combine'' lx ry]

combine''            :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions''          :: [Int] -> Int -> [Expr]
solutions'' ns n      = [e | ns' <- subbags ns, (e,m) <- results' ns', m == n]

-----------------------------------------------------------------------------
-- Interactive version for testing
-----------------------------------------------------------------------------

instance Show Op where
   show Add           = "+"
   show Sub           = "-"
   show Mul           = "*"
   show Div           = "/"

instance Show Expr where
   show (Val n)       = show n
   show (App o l r)   = bracket l ++ show o ++ bracket r
                        where
                           bracket (Val n) = show n
                           bracket e       = "(" ++ show e ++ ")"

display              :: [Expr] -> Int -> IO ()
display [] _         = putStr "\nThere are no solutions.\n\n"
display (e:_) n      = do putStr $ "\nOne possible solution for " ++ show n ++ " is: "
                          putStr (show e)
	                         

main :: IO ()
main = do
    args <- getArgs
    let
        ints = map (read :: String -> Int) args
        ns = tail ints
        n = head ints
    if (length ns > 10) || n > 100000 -- just a silly restriction
      then ioError (userError "Target too big or too many numbers") 
      else display (solutions'' ns n) n
-----------------------------------------------------------------------------