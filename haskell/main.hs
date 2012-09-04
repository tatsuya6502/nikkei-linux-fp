-- -*- coding: utf-8 -*-

{- 
   このモジュールを使うとqueens関数をCPUのネイティブコードにコンパイルし、
   Linuxのシェルから実行できるようになります。おそらく、対話環境（ghci）で
   実行する時の数十倍の速度で動作するでしょう。

   以下のコマンドでコンパイルします。

   $ ghc --make -O3 -o queens main.hs qu.hs qu_math.hs qu_utils.hs

   以下のように実行します。

   $ ./queens 10
-}

module Main (main) where

import Queens (queens)
import Queens.Utils (printBoards)
import System.Environment (getArgs)

main :: IO ()
main = do
  n <- fmap (read . head) getArgs
  main1 n


-- --------------------------------------------------------------------
-- プライベート関数（moduleで指定しなかった関数。モジュールの外からは呼べません）
-- --------------------------------------------------------------------

-- | ディフォルトの実装。解を列番号のリストで表示します。
main1 :: Int -> IO ()
main1 n = mapM_ print $ queens n

-- | 解をチェス盤の形で表示します。
--   main関数3行目の「main1 n」を「main2 n」に書き換えて再度コンパイルしてください。
main2 :: Int -> IO ()
main2 n = printBoards n $ queens n
