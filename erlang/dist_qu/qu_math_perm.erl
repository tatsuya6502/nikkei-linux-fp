%% -*- coding: utf-8 -*-

%%% @doc qu_math_permモジュールは機能拡張版のpermutatios関数を提供します。

%%%
%%% コンパイラ向けの設定
%%%

-module(qu_math_perm).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

-export([all/1, all/2, new/1, new/2, next/1]).
-import(lists, [map/2, reverse/1]).


%%%
%%% 型定義
%%%

-type seed() :: [term()].                         %% 順列組み合わせの元となるデータ
-type perm() :: [term()].                         %% 順列組み合わせ
-type state() :: [[term()]].                      %% 現在の状態
-type perm_record() :: {atom(), seed(), state()}. %% 順列組み合わせの生成に必要な情報


%%%
%%% 関数定義
%%%

%% @doc all関数は与えられたSeedを元に「順列組み合わせ」の一覧を作ります。
%%      順列組み合わせは、与えられた要素を横一列に並べるときに可能なすべての並び順です。
%%      all([1,2]) は [1,2] と [2,1] です。
-spec all(seed()) -> perm().
all(Seed) ->
    all0(new(Seed), []).

%% @doc all関数は与えられたSeedの中から一度にLen個の要素だけを使って
%%      「順列組み合わせ」の一覧を作ります。
-spec all(seed(), integer()) -> perm().
all(Seed, Len) ->
    all0(new(Seed, Len), []).

%% @doc new関数は与えられたSeedに基づき、perm_recordを作ります。
%%      next関数にperm_recordを渡すことで、順列組み合わせを1つずつ得られます。
-spec new(seed()) -> perm_record().
new(Seed) ->
    {?MODULE, Seed, reverse(new0(length(Seed), Seed))}.

%% @doc new関数は与えられたSeedとLenに基づき、perm_recordを作ります。
%%      next関数にperm_recordを渡すことで、順列組み合わせを1つずつ得られます。
-spec new(seed(), integer()) -> perm_record().
new(Seed, Len) ->
    {?MODULE, Seed, reverse(new0(Len, Seed))}.

%% @doc next関数はperm_recordを元に順列組み合わせを1つだけ作り、
%%      順列組み合わせと更新されたperm_recordのタプルを返します。
%%      全ての順列組み合わせを返した後はfalseを返します。
-spec next(perm_record()) -> false | {perm(), perm_record()}.
next({?MODULE, _, []}) ->
    false;
next({?MODULE, Seed, State}) ->
    {head_all(State), {?MODULE, Seed, next_state(Seed, State)}}.


%%
%% プライベート関数（-exportで指定しなかった関数。モジュールの外からは呼べません）
%%

-spec all0(perm_record(), [perm()]) -> [perm()].
all0(Generator, Acc) ->
    case Generator:next() of
        false ->
            reverse(Acc);
        {Permutation, NewGenerator} ->
            all0(NewGenerator, [Permutation | Acc])
    end.

-spec new0(integer(), seed()) -> state().
new0(0, _) ->
    [];
new0(Len, [_|T]=Seed) ->
    [ Seed | new0(Len - 1, T) ].

-spec next_state(seed(), state()) -> state().
next_state(_, []) ->
    [];
next_state(Seed, [[_|HT]|T ]) when HT =:= [] ->
    case next_state(Seed, T) of
        [] ->
            [];
        S ->
            [ Seed -- head_all(S) | S ]
    end;
next_state(_, [[_|HT]|T ]) ->
    [HT|T].

-spec head_all(state()) -> perm().
head_all(State) ->
    reverse([ H || [H|_] <- State ]).

