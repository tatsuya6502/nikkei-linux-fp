-- -*- coding: utf-8 -*-

-- | Queens.Mathモジュールは汎用的な数学関数を提供します。
module Queens.Math
       ( factorial
       , combinations
       ) where


-- | factorial関数はnの「階乗」を求めます。
factorial :: Integer -> Integer
factorial 0 = 1
factorial n
  | n > 0 = n * factorial (n - 1)


-- | combinations関数はn個の要素からm個を取り出す時の「組み合わせ」の数を求めます。
combinations :: Integer -> Integer -> Integer
combinations n m
  | n >= m = factorial n `div` (factorial m * factorial (n - m))


-- | 「順列組み合わせ」の一覧を作るpermutations関数は標準モジュールの
--   Data.Listで定義されています。

