{-# LANGUAGE BlockArguments #-}

module Main where

factorial :: Int -> Int
factorial 1 = 1
factorial a = a *(factorial (a-1))

data Tree a = Node a (Maybe (Tree a)) deriving Show 
data Sequence = Seq Int (Maybe Sequence) deriving Show

intTotree :: Int -> Maybe (Tree Int)
intTotree 0 = Nothing
intTotree a = Just $ Node a (intTotree (a-1))

theTree =  intTotree 10
derive :: Maybe (Tree Int) -> [Int]
derive t = case t of 
		Just (Node a b) -> a : derive b
		Nothing -> [0]

sizeTree :: Maybe (Tree Int) -> Int
sizeTree t = case t of 
		Just (Node a b) -> 1 + sizeTree b
		Nothing -> 0
secTree :: Int -> Maybe (Tree Int) -> Int
secTree a t = a^(2*sizeTree t) `div` factorial 2*sizeTree t

sec u n  | u/=n =  (u^(2*n) `div` factorial (2*n)) + sec u (n+1)  | u==n = 0

treeSecCoeff a b = secTree a (intTotree 10) `div` sec b 1
listToTree [] =  Nothing
listToTree (x:xs) = Just $ Node x (listToTree xs)

mkSeq [] = Seq 0 Nothing
mkSeq (x:xs) =  Seq x (Just $ mkSeq xs)
interNode :: Maybe (Tree Int) -> Int -> Int
interNode t i =  if((sizeTree t)==i) then case t of Just (Node a b) -> a else case t of Just (Node a b) -> interNode b i

interiorNode t = interNode t (((sizeTree t)-2)+1) -- can be -1 but just to express it
main :: IO ()
main = print $  interiorNode (listToTree [1,69,1,3,1,3]) 
