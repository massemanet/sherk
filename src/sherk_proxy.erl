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
  sherk_target:self_register(sherk_host),
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
      stop(LD);
    {info,P,D} ->
      I = [{K,V}||{K,V}<-dict:to_list(D),lists:member(K,[procs,flags,time])],
      ?log([{target,node(P)},{info,I}]),
      loop(LD);
    {'EXIT',P,R} ->
      ?log([got_exit,{from,node(P)},{reason,R}]),
      case dict:fetch(pids,LD) of
        [P] -> ?log(all_clients_dead);
        Ps -> loop(dict:store(pids,Ps--[P],LD))
      end
  end.

stop(LD) ->
  Pids = dict:fetch(pids,LD),
  [P ! stop || P <- Pids],
  recv(Pids,dict:fetch(dest,LD),dict:new()).

recv(_,{ip,_},_) -> ok;
recv(Pids,{file,Dir},FDs) -> recv(Pids,Dir,FDs);
recv([],_,FDs) ->
  case dict:fold(fun(P,_,A)->[node(P)|A] end,[],FDs) of
    [] -> ok;
    X -> ?log({fds_still_open,X})
  end;
recv(Pids,Dir,FDs) ->
  receive
    {'EXIT',P,R}                  -> recv(bye(P,R,Pids),Dir,close(P,FDs));
    {P,chunk,eof}                 -> recv(bye(P,eof,Pids),Dir,close(P,FDs));
    {P,chunk,{error,R}}           -> recv(bye(P,R,Pids),Dir,close(P,FDs));
    {P,chunk,B} when is_binary(B) -> recv(Pids,Dir,stuff(P,B,Dir,FDs))
  end.

stuff(P,B,Dir,FDs) ->
  case dict:find(P,FDs) of
    {ok,FD} ->
      file:write(FD,B),
      FDs;
    error ->
      File = filename:join(Dir,atom_to_list(node(P)))++".trz",
      filelib:ensure_dir(File),
      {ok,FD} = file:open(File,[raw,write,compressed]),
      ?log({opened,File}),
      stuff(P,B,Dir,dict:store(P,FD,FDs))
  end.

close(P,FDs) ->
  try
    FD = dict:fetch(P,FDs),
    file:close(FD),
    dict:erase(P,FDs)
  catch
    _:_ -> FDs
  end.

bye(P,R,Pids) ->
  ?log([{client_finished,node(P)},{reason,R}]),
  Pids--[P].
