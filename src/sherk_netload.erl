%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  5 Nov 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module('sherk_netload').
-author('masse').
-export([assert/2]).

assert(Node,Mods) when is_atom(Node)  -> assert([Node],Mods);
assert(Nodes,Mod) when is_atom(Mod)   -> assert(Nodes,[Mod]);
assert(Nodes,Bin) when is_binary(Bin) -> assert(Nodes,[Bin]);
assert(Nodes,Mods) ->
  [assrt(Node,Mod) || Node <- Nodes, Mod <- Mods].

assrt(Node,Bin) when is_binary(Bin) ->
  {ok,{Mod,[{"CInf",_}]}} = beam_lib:chunks(Bin,["CInf"]),
  netload(Node,Mod,Bin);
assrt(Node,Mod) when is_atom(Mod) ->
  case rpc:call(Node,Mod,module_info,[compile]) of
    {badrpc,{'EXIT',{undef,_}}} ->          %no code
      netload(Node,Mod),
      assert(Node,Mod);
    {badrpc,_} ->
      exit({no_connection,Node});
    CompInfo when is_list(CompInfo) ->
      case {ftime(CompInfo),ftime(Mod:module_info(compile))} of
        {interpreted,_} ->
          exit({target_has_interpreted_code,Mod});
        {TargT,HostT} when TargT < HostT -> %old code on target
          netload(Node,Mod),
          assert(Node,Mod);
        _ ->
          Node                              % we're golden
      end
  end.

netload(Node,Mod) ->
  {Mod,Bin,_} = code:get_object_code(Mod),
  netload(Node,Mod,Bin).

netload(Node,Mod,Bin) ->
  case rpc:call(Node,code,load_binary,[Mod,"netloaded",Bin]) of
    {module,Mod} -> ok;
    {badrpc,_} ->
      exit({no_connection,Node});
    {error,badfile} ->
      I = (catch rpc:call(Node,erlang,system_info,[otp_release])),
      exit({target_emulator_too_old,Node,I})
  end.

ftime([])           -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T])        -> ftime(T).
