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
  ?log({initing}),
  sherk_target:self_register(sherk_proxy),
  process_flag(trap_exit,true),
  receive
    {init,LD} ->
      Targs = dict:fetch(targs,LD),
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
      ?log({stopping}),
      stop(LD);
    {info,P,D} ->
      ?log({infoed}),
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
  [P ! stop || P <- Pids],
  recv(Pids,dict:fetch(daddy,LD)).

recv({ip,_},_) -> ok;  % not yet implemented
recv(Pids,Daddy) ->
  receive
    {'EXIT',P,R}        -> recv(bye(P,R,Pids),Daddy);
    {P,chunk,eof}       -> recv(bye(P,eof,Pids),Daddy);
    {P,chunk,{error,R}} -> recv(bye(P,R,Pids),Daddy);
    {P,chunk,B}         -> Daddy ! {P,B},recv(Pids,Daddy)
  end.

bye(P,R,Pids) ->
  ?log([{client_finished,node(P)},{reason,R}]),
  case [P] of
    Pids -> exit(done);
    Pid  -> Pids--Pid
  end.
