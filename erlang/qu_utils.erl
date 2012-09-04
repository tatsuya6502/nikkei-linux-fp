%% -*- coding: utf-8 -*-

%%% @doc qu_utilsモジュールはN Queensパズルのチェス版を表示する関数を提供します。

%%%
%%% コンパイラ向けの設定
%%%

-module(qu_utils).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

-export([print_board/2, print_boards/2]).
-import(lists, [foreach/2, map/2, seq/2]).


%%%
%%% 型定義
%%%

-type cols() :: [integer()]. %% 全ての駒の位置を列で表します。


%%%
%%% 関数定義
%%%

%% @doc print_board関数はN Queensパズルの解を1つ受け取り、チェス盤の形で表示します。
-spec print_board(integer(), cols()) -> ok.
print_board(N, Cols) when is_list(Cols) ->
    foreach(fun(Line) ->  io:format("~s~n", [Line]) end,
            to_str(N, Cols) ),
    io:format("~n").

%% @doc print_boards関数はN Queensパズルの解を複数受け取り、チェス盤の形で連続して表示します。
-spec print_boards(integer(), [cols()]) -> ok.
print_boards(N, Boards) ->
    foreach(fun(Board) -> print_board(N, Board) end, Boards).


%%
%% プライベート関数（-exportで指定しなかった関数。モジュールの外からは呼べません）
%%

%% @doc to_str関数はチェス盤の1行を表す文字列を返します。
-spec to_str(integer(), cols()) -> string().
to_str(N, Cols) ->
    [ to_str0(N, [], Col) || Col <- Cols ].

-spec to_str0(integer(), string(), integer()) -> string().
to_str0(0, Line, _) ->
    Line;
to_str0(I, Line, Col) when Col =:= I ->
    to_str0(I - 1, [$Q | Line], Col);
to_str0(I, Line, Col) ->
    to_str0(I - 1, [$. | Line], Col).

