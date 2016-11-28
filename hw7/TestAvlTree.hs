module TestAvlTree where

import Test.QuickCheck
-- 可能带来便利的重命名 https://wiki.haskell.org/Import
import AvlTree as AvlTree
-- import Solution as AvlTree

-- 几个可以用来测试的数据
a1 = Node 7 4
        (Node 4 2
            (Node 2 1
                (leaf 1)
                (leaf 3))
            (Node 6 1
                (leaf 5)
                Nil))
        (Node 13 3
            (Node 11 2 
                (Node 10 1
                    (leaf 8)
                    Nil)        
                (leaf 12))
            (Node 15 1
                (leaf 14)
                (leaf 16)))

aRBeforeInsert = Node 3 2
                    (leaf 1)
                    (Node 7 1 
                        (leaf 5)
                        (leaf 9))

aLBeforeInsert = Node 9 2
                    (Node 5 1 
                        (leaf 3)
                        (leaf 7))
                    (leaf 11)

aLLAfterRotate = Node 5 2 (Node 3 1 (leaf 2) Nil) (Node 9 1 (leaf 7) (leaf 11))
aLRAfterRotate = Node 7 2 (Node 5 1 (leaf 3) (leaf 6)) (Node 9 1 Nil (leaf 11))
aRRAfterRotate = Node 7 2 (Node 3 1 (leaf 1) (leaf 5)) (Node 9 1 (leaf 8) Nil)
aRLAfterRotate = Node 5 2 (Node 3 1 (leaf 1) (leaf 4)) (Node 7 1 Nil (leaf 9))


p11 = search 9 a1 == Nothing
p12 = search 8 a1 == Just (leaf 8)
p13 = search 10 a1 == Just (Node 10 1 (leaf 8) Nil)

p21 = AvlTree.collect (insert 2 aLBeforeInsert) == AvlTree.collect aLLAfterRotate
p22 = AvlTree.collect (insert 6 aLBeforeInsert) == AvlTree.collect aLRAfterRotate
p23 = AvlTree.collect (insert 8 aRBeforeInsert) == AvlTree.collect aRRAfterRotate
p24 = AvlTree.collect (insert 4 aRBeforeInsert) == AvlTree.collect aRLAfterRotate
-- 下面四个属性即使不满足，AvlTree 的实现也可能正确（正确 = 满足不变式 && 满足API约定）
p25 = (insert 2 aLBeforeInsert) == aLLAfterRotate
p26 = (insert 6 aLBeforeInsert) == aLRAfterRotate
p27 = (insert 8 aRBeforeInsert) == aRRAfterRotate
p28 = (insert 4 aRBeforeInsert) == aRLAfterRotate

p31 = AvlTree.collect (remove 9 aLBeforeInsert) == AvlTree.collect (Node 5 2 (Node 3 0 Nil Nil) (Node 11 1 (Node 7 0 Nil Nil) Nil))
-- 即使不满足，AvlTree 的实现也可能正确（正确 = 满足不变式 && 满足API约定）
p32 = (remove 9 aLBeforeInsert) == Node 5 2 (Node 3 0 Nil Nil) (Node 11 1 (Node 7 0 Nil Nil) Nil)


p41 = fmap (+1) aLBeforeInsert == Node 10 2 (Node 6 1 (Node 4 0 Nil Nil) (Node 8 0 Nil Nil)) (Node 12 0 Nil Nil)

p51 = (foldr (:) [] aLBeforeInsert) == [3,5,7,9,11]
p52 = (foldr (:) [] aLBeforeInsert) == AvlTree.collect aLBeforeInsert


-- pBalance :: AvlTree a -> Property
-- pBalance Nil = True ==> True
-- pBalance (Node k h l r) = True ==> (abs $ height l - height r) < 2

-- *Main> let except x xs = (takeWhile (/=x) xs) ++ (tail $ dropWhile (/=x) xs)
-- *Main> except 9 [1..16]
-- [1,2,3,4,5,6,7,8,10,11,12,13,14,15,16]
-- *Main> except 13 $ except 9 [1..16]
-- [1,2,3,4,5,6,7,8,10,11,12,14,15,16]