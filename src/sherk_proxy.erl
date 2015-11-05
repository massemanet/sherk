%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Nov 2015 by masse <masse@klarna.com>

%% @doc
%% @end

-module('sherk_proxy').
-author('masse').
-export([init/0]).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the proxy process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  sherk_target:self_register(sherk_proxy),
  process_flag(trap_exit,true),
  receive
    {init,LD} ->
      Targs = dict:fetch(targs,LD),
      sherk_netload:assert(Targs,sherk_target),
      Pids = [spawn_link(T, fun sherk_target:init/0) || T <- Targs],
      [ P ! {init,dict:store(daddy,self(),LD)} || P <- Pids],
      Timer = erlang:start_timer(dict:fetch(time,LD),self(),{die}),
      loop(dict:store(pids,Pids,dict:store(timer,Timer,LD)))
  end.

loop(LD) ->
  receive
    {timeout,_,{die}} ->
      ?log({timed_out}),
      stop(LD);
    stop ->
      stop(LD);
    {info,P,D} ->
      I = [{K,V}||{K,V}<-dict:to_list(D),lists:member(K,[procs,flags,time])],
      ?log([{target,node(P)},{info,I}]),
      loop(LD);
    {'EXIT',P,R} ->
      ?log([got_exit,{from,node(P)},{reason,R}]),
      case dict:fetch(pids,LD) of
        [P] -> ?log(all_clients_dead);
        Ps ->
          case lists:member(P,Ps) of
            true -> loop(dict:store(pids,Ps--[P],LD));
            false->
              ?log({weird_exit,node(P)}),
              exit({got_weird_exit,node(P)})
          end
      end
  end.

stop(LD) ->
  Pids = dict:fetch(pids,LD),
  ?log({stopping,[node(P) || P <- Pids]}),
  [P ! stop || P <- Pids],
  recv(Pids,dict:fetch(daddy,LD),0).

recv(Pids,Daddy,N) ->
  receive
    {'EXIT',P,R}        -> recv(bye(P,R,Pids,N),Daddy,N);
    {P,chunk,eof}       -> recv(bye(P,eof,Pids,N),Daddy,N);
    {P,chunk,{error,R}} -> recv(bye(P,R,Pids,N),Daddy,N);
    {P,chunk,B}         -> Daddy ! {P,B},recv(Pids,Daddy,N+1)
  end.

bye(P,R,Pids,N) ->
  ?log([{client_finished,node(P)},{reason,R}]),
  case [P] of
    Pids -> exit({done,N});
    Pid  -> Pids--Pid
  end.
