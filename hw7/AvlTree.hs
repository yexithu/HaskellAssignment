module AvlTree where

import Data.Maybe

-- 二叉树是一类树。为此定义一个类型类
class BinaryTree t where
    -- 取得键、取得左子树、取得右子树。返回 Maybe a 而非 a，可以应对当前结点为 Nil 的情形。
    key :: t a -> Maybe a
    left :: t a -> Maybe (t a)
    right :: t a -> Maybe (t a)    
    height :: t a -> Int
    size :: t a -> Int
    -- collect 返回中序遍历这棵树的结果。
    collect :: t a -> [a]
    contains :: Eq a => a -> t a -> Bool

-- 有序的二叉树也是一类树。它们首先要是二叉树。
class BinaryTree t => SortedBinaryTree t where
    search :: (Ord a) => a -> t a -> Maybe (t a)
    insert :: (Ord a) => a -> t a -> t a
    remove :: (Ord a) => a -> t a -> t a

-- Node k h l r 中，k 为该结点的键，h 为以该结点为根的子树的高度，l 和 r 为左右子树。
data AvlTree a = Nil | Node a Int (AvlTree a) (AvlTree a)
    deriving(Show, Read, Eq)

-- 用 leaf 函数可以在构造 AvlTree 时少打几个字
leaf :: a -> AvlTree a
leaf k = Node k 0 Nil Nil

-- 要成为 BinaryTree 类型类的实例，AvlTree 得实现这些函数
instance BinaryTree AvlTree where
    -- 把这些名字绑定到 undefined，使得在开始改动任何东西之前，本文件就能通过编译。
    key Nil = Nothing
    key (Node k _ _ _) = Just k

    left Nil = Nothing
    left (Node _ _ l _) = Just l

    right Nil = Nothing
    right (Node _ _ _ r) = Just r
    
    height Nil = -1
    height (Node _ h _ _) = h

    size Nil = 0
    size (Node _ _ l r) = (size l) + (size r) + 1

    collect Nil = []
    collect (Node k _ l r) = (collect l) ++ [k] ++ (collect r) 
    
    contains _ Nil = False
    contains v (Node k _ l r)
        | v == k = True
        | contains v l = True
        | otherwise = contains v r

-- 成为 SortedBinaryTree 类型类的实例
instance SortedBinaryTree AvlTree where
    -- 把这些名字绑定到 undefined，使得在开始改动任何东西之前，本文件就能通过编译。
    -- 把这些 undefined 改成真正的函数实现，就像以前实现函数那样。
    search _ Nil = Nothing
    search v t@(Node k _ l r)
        | v < k = search v l 
        | v > k = search v r
        | otherwise = Just t
        
    -- insert = undefined
    insert v t = rotate $ insertToNode v t
    
    remove v t = rotate $ removeFromNode v t

-- AvlTree 独有的、不属于某个类型类的函数，定义在外面
-- 当然不必叫 rotateLL 这个名字……
rotateLL :: AvlTree a -> AvlTree a
rotateLL (Node k h (Node lk lh ll lr) r) = let newr = (Node k (parentHeight lr r) lr r)
                                           in (Node lk (parentHeight ll newr) ll newr)  
rotateRR :: AvlTree a -> AvlTree a
rotateRR (Node k h l (Node rk rh rl rr)) = let newl = (Node k (parentHeight l rl) l rl)
                                           in (Node rk (parentHeight newl rr) newl rr)

rotateLR :: AvlTree a -> AvlTree a
rotateLR (Node k h l r) = rotateLL (Node k h (rotateRR l) r)
rotateRL :: AvlTree a -> AvlTree a
rotateRL (Node k h l r) = rotateRR (Node k h l (rotateLL r))

parentHeight :: AvlTree a -> AvlTree a -> Int
parentHeight l r = 1 + (max (height l) (height r))

insertToNode :: (Ord a) => a -> AvlTree a -> AvlTree a
insertToNode v Nil = leaf v
insertToNode v origin@(Node k h l r)
    | v < k = let newl = insertToNode v l
              in rotate $ (Node k (parentHeight newl r) newl r) 
    | v > k = let newr = insertToNode v r
              in rotate $ (Node k (parentHeight l newr) l newr) 
    | otherwise = origin

removeFromNode :: (Ord a) => a -> AvlTree a -> AvlTree a
removeFromNode v Nil = Nil
removeFromNode v origin@(Node k h l r)
    | v < k = let newl = removeFromNode v l
              in rotate $ (Node k (parentHeight newl r) newl r)
    | v > k = let newr = removeFromNode v r
              in rotate $ (Node k (parentHeight l newr) l newr) 
    | otherwise = removeNode origin

removeNode :: AvlTree a -> AvlTree a
removeNode origin@(Node k h l r)
    | (height l) == (-1) = r
    | otherwise = let (newl, newk) = removeRightMost l
                  in (Node newk (parentHeight newl r) newl r)

removeRightMost :: AvlTree a -> (AvlTree a, a)
removeRightMost origin@(Node k h l r)
    | (height r) == (-1) = (Nil, k)
    | otherwise = let (newr, returnk) = removeRightMost r
                  in ((Node k (parentHeight l newr) l newr), returnk)

rotate :: AvlTree a -> AvlTree a
rotate t@(Node k h l r)
    | (abs $ hL - hR) < 2 = t
    | otherwise = (getRotateFunc t) t  
    where hL = height l
          hR = height r
 
getRotateFunc :: AvlTree a -> (AvlTree a -> AvlTree a)
getRotateFunc t@(Node k h l r)
    | leftTaller t = if leftTaller l then rotateLL else rotateLR
    | otherwise = if leftTaller r then rotateRL else rotateRR

leftTaller :: AvlTree a -> Bool
leftTaller (Node k h l r) = (height l) > (height r) 

-- 成为 Functor 类型类的实例
instance Functor AvlTree where
    fmap f Nil = Nil
    fmap f (Node k h l r) = Node (f k) h (fmap f l) (fmap f r)

-- 成为 Foldable 类型类的实例
instance Foldable AvlTree where
    foldr f v t = foldr f v (collect t)
