%% -*- coding: utf-8 -*-

%%% @doc quモジュールはN Queensパズルの解を求めるための関数を提供します。

%% Erlangについてもっと知りたいときは、こちらを参照してください。
%%
%% 「Learn you some Erlang for great good!」 日本語翻訳版
%% http://ymotongpoo.appspot.com/lyse_ja/index.html


%%%
%%% コンパイラ向けの設定
%%%

-module(qu).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

%% デバッグ用：本モジュールで定義した全ての関数が外部から参照できます。
-compile([export_all]).

%% 他のモジュールで定義されている関数をモジュール名抜きで参照します。
-import(qu_math,  [permutations/1]).
-import(qu_utils, [print_boards/2]).
-import(lists,    [all/2, filter/2, seq/2, zip/2]).


%%%
%%% 型定義
%%% Erlangは動的型付け言語ですが、-typeと-specを定義すると
%%% 静的コード解析ツールの「Dialyzer」で型情報が検証できるようになります。
%%%

-type cols() :: [integer()]. %% 全ての駒の位置を列で表します。


%%%
%%% 関数定義
%%% ここからがErlangプログラムです。
%%%

%% @doc queens関数はN Queensパズルの解を求めます。
%%      整数を引数に取り、解をcols型のリストで返します。
-spec queens(integer()) -> [cols()].
queens(N) ->
    L = seq(1, N),
    filter(fun(Cols) -> safe(zip(L, Cols)) end, permutations(L)).

%% @doc safe関数は駒が互いに相手を攻撃できないことを検査します。
%%      今は仮の実装として常にtrueを返すようにしています。
-spec safe(term()) -> true.
safe(_) ->
    true.

