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

(+++) :: [Int] -> Sequence -> Sequence 
(+++) [] s = Seq 0 Nothing
(+++)  (x:xs) s = Seq x (Just $ xs +++ s)
(|++) :: Sequence -> Sequence -> Sequence
(|++) (Seq 0 Nothing) (Seq 0 Nothing) = Seq 0 Nothing
(|++) a (Seq 0 Nothing) = case a of Seq x (Just y) -> Seq x (Just (y |++ (Seq 0 Nothing)))
(|++) (Seq 0 Nothing) b = case b of Seq x (Just y) -> Seq x (Just (b |++ y))
(|++) a b = case a of Seq x (Just y) -> Seq x (Just (y |++ b))
sizeSeq :: Sequence -> Int
sizeSeq (Seq 0 Nothing) = 0
sizeSeq s = 1 + case s of Seq x (Just y) -> sizeSeq y

crtSet :: Maybe (Tree Int) -> Maybe (Tree Int) -> Sequence
crtSet t1 Nothing = mkSeq  []
crtSet Nothing t2 = mkSeq  []
crtSet t1 t2 = (derive t1 +++ case t1 of Just (Node a b) -> crtSet b t2) |++ (derive t2 +++ case t2 of Just (Node a b) -> crtSet t1 b)

seq2n l = crtSet (listToTree l) (listToTree l)
estSize x = (x + 1)^2
estSizeL :: [Int] -> [Int]
estSizeL [] = []
estSizeL (x:xs) = case seq2n xs of 
		Seq x (Just y) -> (estSize (sizeSeq y) - sizeSeq y) : estSizeL xs
		Seq 0 Nothing -> []
estSizeMat x | x>0 = estSizeL [1..x] : estSizeMat (x-1) | otherwise = [[]]
eoc :: [[Int]] -> Int
eoc [[]] = 0
eoc (x:xs) = sum x + eoc xs
pom :: [[Int]] -> Int
pom [[]] = 0
pom (x:xs) = (x !! 0) + pom xs
diag f 0 = 0
diag (x:xs) size = (x !! size) + diag xs (size-1)
interiorNode t = interNode t (((sizeTree t)-2)+1) -- can be -1 but just to express it
main :: IO ()
main = print $ diag (estSizeMat 100) 10
