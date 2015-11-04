%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Nov 2015 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('sherk_host').
-author('mats cronqvist').

-export([init/0]).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the proxy process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  ?log({initing}),
  sherk_target:self_register(sherk_host),
  process_flag(trap_exit,true),
  receive
    {init,LD} ->
      Proxy = dict:fetch(proxy,LD),
      ?log({spawning}),
      ProxyPid = spawn(Proxy,fun sherk_proxy:init/0),
      ProxyPid ! {init,LD},
      {file,Dir} = dict:fetch(dest,LD),
      recv(ProxyPid,Dir,dict:new())
  end.

recv(Proxy,Dir,FDs) ->
  receive
    {'EXIT',Proxy,done} -> ?log({quitting,done});
    {Pid,Chunk}         -> recv(Proxy,Dir,stuff(Pid,Chunk,Dir,FDs));
    X                   -> ?log({weird_msg,X})
  end.

stuff(Pid,Chunk,Dir,FDs) ->
  ?log({chunking,node(Pid)}),
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
