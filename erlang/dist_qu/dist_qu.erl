%% -*- coding: utf-8 -*-

%%% @doc dist_quモジュールは並列プログラミングによるN Queensプログラムです。

%% このプログラムの実行方法と仕組みについては、
%% 同じディレクトリ（erlang/dist_qu/）にある「README.md」を
%% 参照してください。
%%
%% Erlangについてもっと知りたいときは、こちらを参照してください。
%%
%% 「Learn you some Erlang for great good!」 日本語翻訳版
%% http://ymotongpoo.appspot.com/lyse_ja/index.html


%%%
%%% コンパイラ向けの設定
%%%

-module(dist_qu).
-compile([native, {hipe, ['O3']}]).  %% 高速化のためにネイティブコードにコンパイル

-export([queens/1, queens/2, run_queens/4]).
-import(lists, [foldl/3, map/2, seq/2, zip/2]).


%%%
%%% 型定義
%%% Erlangは動的型付け言語ですが、-typeと-specを定義すると
%%% 静的コード解析ツールの「Dialyzer」で型情報が検証できるようになります。
%%%

-type cols() :: [integer()]. %% 全ての駒の位置を列で表します。


%%%
%%% 関数定義
%%%

%% @doc queens関数は1台のコンピュータで多数の軽量プロセスを起動し、
%%      並列プログラミングでN Queensパズルの解を求めます。
%%      整数を引数に取り、解をcols型のリストで返します。
-spec queens(integer()) -> [cols()].
queens(N) ->
    queens(N, []).

%% @doc queens関数は2台以上のコンピュータで多数の軽量プロセスを起動し、
%%      並列プログラミングでN Queensパズルの解を求めます。
%%      第2引数にErlang仮想マシンのノード名を取り、解をcols型のリストで返します。
-spec queens(integer(), [node()]) -> [cols()].
queens(N, Nodes) when N >= 4 ->
    Seed = seq(1, N),
    Prefixes = qu_math_perm:all(Seed, 2), %% 駒の配置を小分けにする。
    Ref = erlang:make_ref(),
    Pids = start_workers(Seed, Prefixes, self(), Ref, Nodes),
    io:format("Started ~p workers.~n", [length(Pids)]),
    gather_results(Pids, Ref).


%%
%% プライベート関数（-exportで指定しなかった関数。モジュールの外からは呼べません）
%%

%% @doc start_workers関数はメインプロセスにより実行され、Erlangの軽量プロセスを起動します。
%%      起動したプロセスを、ここでは「ワーカープロセス」と呼びます。
-spec start_workers([integer()], [integer()], pid(), reference(), [node()]) -> [pid()].
start_workers(Seed, Prefixes, Self, Ref, []) ->
    map(fun(Prefix) -> spawn_link(?MODULE, run_queens, [Self, Ref, Prefix, Seed -- Prefix]) end,
	Prefixes);
start_workers(Seed, Prefixes, Self, Ref, Nodes) ->
    NumNodes = length(Nodes),
    NumberedPrefixes = zip(seq(1, length(Prefixes)), Prefixes),
    map(fun({I, Prefix}) ->
                Node = lists:nth(I rem NumNodes + 1, Nodes),
                spawn_link(Node, ?MODULE, run_queens, [Self, Ref, Prefix, Seed -- Prefix])
        end, NumberedPrefixes).

%% @doc run_queens関数はワーカープロセスにより実行され、指定された範囲の駒の配置について解を求めます。
-spec run_queens(pid(), reference(), [integer()], [integer()]) -> {pid(), reference(), [cols()]}.
run_queens(Parent, Ref, Prefix, Seed) ->
    %% メッセージ交換機能を使い、親プロセス（メインプロセス）へ解を送信。
    %% メッセージは親の受信ボックスに入ります。
    Parent ! {self(), Ref, (catch qu:queens(Prefix, Seed))}.

%% @doc gather_results関数はメインプロセスにより実行され、ワーカープロセスが求めた解を収集します。
-spec gather_results([pid()], reference()) -> [cols()].
gather_results(Pids, Ref) ->
    foldl(fun(Pid, Acc) ->
                  %% 受信ボックスに溜まったメッセージから解を取り出し、Accに蓄積。
                  receive
                      {Pid, Ref, Result} -> Result ++ Acc
                  end
          end, [], Pids).

