%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Nov 2015 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('sherk_host').
-author('mats cronqvist').

-export([init/0,
         ping/2]).

-include("log.hrl").

ping(Proxy,Targ) ->
  sherk_netload:assert(Proxy,[sherk_proxy]),
  rpc:call(Proxy,sherk_proxy,ping,[Targ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the proxy process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  sherk_target:self_register(sherk_host),
  process_flag(trap_exit,true),
  receive
    {init,LD} ->
      Proxy = dict:fetch(proxy,LD),
      sherk_netload:assert(Proxy,[sherk_target,sherk_netload,sherk_proxy]),
      ProxyPid = spawn_link(Proxy,fun sherk_proxy:init/0),
      ProxyPid ! {init,dict:store(daddy,self(),LD)},
      {file,Dir} = dict:fetch(dest,LD),
      recv(ProxyPid,Dir,dict:new(),0)
  end.

recv(Proxy,Dir,FDs,N) ->
  receive
    {'EXIT',Proxy,R} -> ?log({quitting,N,R});
    {Pid,Chunk}      -> recv(Proxy,Dir,stuff(Pid,Chunk,Dir,FDs),N+1);
    X                -> ?log({weird_msg,X})
  end.

stuff(Pid,Chunk,Dir,FDs) ->
  case dict:find(Pid,FDs) of
    {ok,FD} ->
      file:write(FD,Chunk),
      FDs;
    error ->
      File = filename:join(Dir,atom_to_list(node(Pid)))++".trz",
      filelib:ensure_dir(File),
      {ok,FD} = file:open(File,[raw,write,compressed]),
      ?log({opened,File}),
      stuff(Pid,Chunk,Dir,dict:store(Pid,FD,FDs))
  end.
