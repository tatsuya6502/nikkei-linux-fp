-- -*- coding: utf-8 -*-

-- | QueensモジュールはN Queensパズルの解を求めるための関数を提供します。 
module Queens (queens) where

import Data.List (permutations)


-- | queens関数はN Queensパズルの解を求めます。

-- queens関数の型定義。整数を受け取り、整数のリストのリストを返します。
-- Haskellは静的型付けですが、コンパイラが自動的に型を推測できるので、
-- 型定義は省略可能です。
queens :: Int -> [[Int]]

-- queens関数の本体
queens n = filter (safe . zip [1..]) $ permutations [1..n]

-- 上の式の (safe . zip ...) は「関数合成」といい、ラムダ式よりも簡潔に記述できます。


-- --------------------------------------------------------------------
-- プライベート関数（moduleで指定しなかった関数。モジュールの外からは呼べません）
-- --------------------------------------------------------------------

-- | safe関数は全ての駒が互いに相手を攻撃できないことを検査します。
--   攻撃できないときは安全という意味のTrueを、攻撃できる時はFalseを返します。
safe :: [(Int, Int)] -> Bool
safe []     = True
safe (x:xs) = all (safe2 x) xs && safe xs
  -- where句でローカル関数を定義できます。
  where safe2 (r1,c1) (r2,c2) = r1 + c1 /= r2 + c2 && r1 - c1 /= r2 - c2

