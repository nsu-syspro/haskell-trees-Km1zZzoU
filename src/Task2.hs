{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y
  | x < y     = LT
  | x == y    = EQ
  | otherwise = GT

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ [] = Leaf
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)-- | Conversion from binary search tree to list

--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--

bstToList :: Tree a -> [a]
bstToList Leaf = []
bstToList (Branch x left right) = bstToList left ++ [x] ++ bstToList right

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--

isBST :: Cmp a -> Tree a -> Bool
isBST _ Leaf = True
isBST cmp (Branch x left right) =
  allLeft cmp x left && allRight cmp x right && isBST cmp left && isBST cmp right
  where
    allLeft :: Cmp a -> a -> Tree a -> Bool
    allLeft _ _ Leaf = True
    allLeft cmpFn val (Branch nodeVal leftSub rightSub) = 
      cmpFn nodeVal val == LT && 
      allLeft cmpFn val leftSub && 
      allLeft cmpFn val rightSub
    
    allRight :: Cmp a -> a -> Tree a -> Bool
    allRight _ _ Leaf = True
    allRight cmpFn val (Branch nodeVal leftSub rightSub) = 
      cmpFn nodeVal val == GT && 
      allRight cmpFn val leftSub && 
      allRight cmpFn val rightSub

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp x (Branch y left right) =
  case cmp x y of
    LT -> tlookup cmp x left
    EQ -> Just y
    GT -> tlookup cmp x right

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ x Leaf = Branch x Leaf Leaf
tinsert cmp x (Branch y left right) =
  case cmp x y of
    LT -> Branch y (tinsert cmp x left) right
    EQ -> Branch x left right
    GT -> Branch y left (tinsert cmp x right)

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--

tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp x (Branch y left right) =
  case cmp x y of
    LT -> Branch y (tdelete cmp x left) right
    GT -> Branch y left (tdelete cmp x right)
    EQ -> deleteNode left right
  where
    deleteNode :: Tree a -> Tree a -> Tree a
    deleteNode l Leaf = l
    deleteNode Leaf r = r
    deleteNode l r =
      let (minVal, newRight) = extractMin r
      in Branch minVal l newRight
    
    extractMin :: Tree a -> (a, Tree a)
    extractMin (Branch val Leaf rightSubtree) = (val, rightSubtree)
    extractMin (Branch val leftSubtree rightSubtree) = 
      let (minVal, newLeft) = extractMin leftSubtree
      in (minVal, Branch val newLeft rightSubtree)
    extractMin Leaf = error "empty tree"
