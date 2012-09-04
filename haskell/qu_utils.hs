-- -*- coding: utf-8 -*-

-- | qu_utilsモジュールはN Queensパズルのチェス版を表示する関数を提供します。
module Queens.Utils
       ( printBoard
       , printBoards
       ) where

-- | print_board関数はN Queensパズルの解を1つ受け取り、チェス盤の形で表示します。
printBoard :: Int -> [Int] -> IO ()
printBoard n cols = do
  mapM_ putStrLn $ (toStr n cols)
  putStrLn ""


-- | print_boards関数はN Queensパズルの解を複数受け取り、チェス盤の形で連続して表示します。
printBoards :: Int -> [[Int]] -> IO ()
printBoards n boards = mapM_ (printBoard n) boards


-- --------------------------------------------------------------------
-- プライベート関数（moduleで指定しなかった関数。モジュールの外からは呼べません）
-- --------------------------------------------------------------------

toStr :: Int -> [Int] -> [[Char]]
toStr n cols = map (toStr' n "") cols
  where toStr' 0 line _ = line
        toStr' i line col
          | col == i = toStr' (i - 1) ('Q':line) col
          | col /= i = toStr' (i - 1) ('.':line) col

