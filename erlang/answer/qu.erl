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

-export([queens/1]).

-import(qu_math,  [permutations/1]).
-import(qu_utils, [print_boards/2]).
-import(lists,    [all/2, filter/2, seq/2, zip/2]).


%%%
%%% 型定義
%%%

-type cols() :: [integer()].                %% 全ての駒の位置を列で表します。
-type position() :: {integer(), integer()}. %% 駒の位置を {行番号,列番号} で表します。


%%%
%%% 関数定義
%%%

%% @doc queens関数はN Queensパズルの解を求めます。
%%      整数を引数に取り、解をcols型のリストで返します。
-spec queens(integer()) -> [cols()].
queens(N) ->
    L = seq(1, N),
    filter(fun(Cols) -> safe(zip(L, Cols)) end, permutations(L)).


%%
%% プライベート関数（-exportで指定しなかった関数。モジュールの外からは呼べません）
%%

%% @doc safe関数は全ての駒が互いに相手を攻撃できないことを検査します。
%%      攻撃できないときは安全という意味のtrueを、攻撃できる時はfalseを返します。
-spec safe([position()]) -> boolean().
safe([]) ->
    true;
safe([X|Xs]) ->
    all(fun(Xn) -> safe2(X, Xn) end, Xs)
        andalso safe(Xs).

%% @doc safe2関数は2つの駒が互いに相手を攻撃できないことを検査します。
%%      攻撃できないときは安全という意味のtrueを、攻撃できる時はfalseを返します。
-spec safe2(position(), position()) -> boolean().
safe2({R1,C1}, {R2,C2}) ->
    R1 + C1 =/= R2 + C2 andalso R1 - C1 =/= R2 - C2.

