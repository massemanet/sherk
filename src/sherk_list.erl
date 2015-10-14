%%%-------------------------------------------------------------------
%%% File    : sherk_list.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : generates list view of a proc
%%%
%%% Created : 21 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_list).

-export([go/1]).

-define(LOG(T), sherk:log(process_info(self()),T)).

go({call,PidStr}) ->
  case sherk_ets:lup(sherk_prof,PidStr) of
    [] -> [];
    Pid -> get_list(Pid)
  end.

get_list(Pid) ->
  Tot = sherk_ets:lup(sherk_prof,{{pid,time}, Pid}),
  TMFAs = lists:reverse(
            lists:sort(
              ets:match(sherk_prof,{{{func,time},Pid,'$2'},'$1'}))),
  [[str(MFA),calls(Pid,MFA),percent(T,Tot)] || [T,MFA] <- TMFAs].

str(X) -> lists:flatten(io_lib:fwrite("~p",[X])).

percent(_,0) -> 0;
percent(A,B) -> round(100*A/B).

calls(Pid,MFA) ->
  case sherk_ets:lup(sherk_prof,{{func, calls}, Pid, MFA}) of
    [] -> 0;
    N -> N
  end.
