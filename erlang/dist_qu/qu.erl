%% -*- coding: utf-8 -*-

%%% @doc quモジュールはN Queensパズルの解を求めるための関数を提供します。
%%%      qu_math_permの機能拡張により、オリジナルのN Queensよりもメモリーの
%%%      使用量が少なくなっています。

%%%
%%% コンパイラ向けの設定
%%%

-module(qu).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

-export([queens/1, queens/2]).

-import(qu_math,  [permutations/1]).
-import(qu_utils, [print_boards/2]).
-import(lists,    [all/2, filter/2, map/2, seq/2, zip/2]).


%%%
%%% 型定義
%%%

-type rows() :: [integer()].                 %% 行番号
-type cols() :: [integer()].                 %% 全ての駒の位置を列で表します。
-type position() :: {integer(), integer()}.  %% 駒の位置を {行番号,列番号} で表します。
-type perm_record() :: tuple().              %% qu_math_perm:new()などが返す順列組み合わせの生成に必要な情報です。


%%%
%%% 関数定義
%%%

%% @doc queens関数はN Queensパズルの解を求めます。
%%      整数を引数に取り、解をcols型のリストで返します。
-spec queens(integer()) -> [cols()].
queens(N) ->
    queens([], seq(1, N)).

%% @doc queens関数はN Queensパズルの解を求めます。
%%      引数に整数をリストを2つ取り、それを元に部分的な駒の配置のみを検証します。
%%      解はcols型のリストで返します。
-spec queens([integer()], [integer()]) -> [cols()].
queens(Prefix, Rest) ->
    Rows = seq(1, length(Prefix) + length(Rest)),
    queens1(Rows, Prefix, qu_math_perm:new(Rest), []).


%%
%% プライベート関数（-exportで指定しなかった関数。モジュールの外からは呼べません）
%%

%% @doc queens1関数はオリジナルのqueens関数のfilterにあたる処理を行います。
-spec queens1(rows(), [integer()], perm_record(), [cols()]) -> [cols()].
queens1(Rows, Prefix, Generator, Results) ->
    %% Generator:next()は qu_math_perm:next(Generator) の省略形で
    %% Generatorの情報を元に、次の順列組み合わせを返します。
    case Generator:next() of
        false ->
            %% 組み合わせがもう無ければ蓄積した解のリストを返す。
            Results;
        {Perm, NewGenerator} ->
            %% 組み合わせがまだあるならsafeで検証し、trueなら解のリストに追加する。
            Cols = Prefix ++ Perm,
            NewResults = case safe(zip(Rows, Cols)) of
                true ->  [Cols | Results];
                false -> Results
            end,
            %% 再帰呼び出しで次の順列組み合わせを検証。
            queens1(Rows, Prefix, NewGenerator, NewResults)
	end.

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

