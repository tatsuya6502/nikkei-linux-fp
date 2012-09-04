%% -*- coding: utf-8 -*-

%%% @doc qu_mathモジュールは汎用的な数学関数を提供します。

%%%
%%% コンパイラ向けの設定
%%%

-module(qu_math).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

-export([factorial/1, combinations/2, permutations/1]).
-import(lists, [map/2]).


%%%
%%% 関数定義
%%%

%% @doc factorial関数はNの「階乗」を求めます。
-spec factorial(integer()) -> integer().
factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

%% @doc combinations関数はN個の要素からM個を取り出す時の「組み合わせ」の数を求めます。
-spec combinations(integer(), integer()) -> integer().
combinations(N, M) when N >= M ->
    factorial(N) div (factorial(M) * factorial(N - M)).  %% divは整数の割り算

%% @doc permutations関数は「順列組み合わせ」の一覧を作ります。
%%      順列組み合わせは、与えられた要素を横一列に並べるときに可能なすべての並び順です。
%%      permutations([1,2]) は [1,2] と [2,1] です。
-spec permutations([term()]) -> [[term()]].
permutations([]) ->
    [[]];
permutations(L) ->
    [ [H|T] || H <- L, T <- permutations(L -- [H]) ].

    %% 上の式はリスト内包表記で書かれています。
    %% リスト内包表記についてはこちらを参照してください。
    %% 「Learn you some Erlang for great good!」 日本語翻訳版
    %% http://ymotongpoo.appspot.com/lyse_ja/ja/starting_out_for_real.html#id8

