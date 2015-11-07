%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Nov 2015 by masse <masse@klarna.com>

%% @doc
%% @end

-module('sherk_proxy').
-author('masse').
-export([init/0,
         get_target_nodes/0,get_target_nodes/1,
         ping/1]).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% called from sherk_host

get_target_nodes() ->
  case code:get_object_code(sherk_target) of
    {_,TargetBeamCode,_} -> get_target_nodes(TargetBeamCode);
    Err -> exit({getting_target_beam_code,sherk_target,Err})
  end.

get_target_nodes(TargetBeamCode) ->
  [N || {_,N} <- get_targets(sherk_target:get_nodes(),TargetBeamCode)].

get_targets(HostNodes,TargetBeamCode) when is_list(HostNodes) ->
  HNs = filter_hosts(HostNodes),
  HostNodes++lists:flatten([get_targets(HN,TargetBeamCode) || HN <- HNs]);
get_targets({_,Node},TargetBeamCode) ->
  sherk_netload:assert(Node,TargetBeamCode),
  [N || {_,N} <- rpc:call(Node,sherk_target,get_nodes,[])].

% we want one erlang node per host
filter_hosts(HostNodes) ->
  [_,H] = string:tokens(atom_to_list(node()),"@"),
  Host = list_to_atom(H),
  F = fun(A,B) -> one_node_per_host(A,B,Host) end,
  lists:foldl(F,[],HostNodes).

one_node_per_host({Host,_},R,Host)     -> R;
one_node_per_host({H,_},[{H,_}|_]=R,_) -> R;
one_node_per_host(HN,R,_)              -> [HN|R].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% called from sherk_host
ping(T) ->
  net_adm:ping(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the proxy process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  sherk_target:self_register(sherk_proxy),
  process_flag(trap_exit,true),
  receive
    {init,LD} ->
      Targs = dict:fetch(targs,LD),
      sherk_netload:assert(Targs,dict:fetch(target_beam_code,LD)),
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
