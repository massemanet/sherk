%%%-------------------------------------------------------------------
%%% File     : sherk_aquire.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description :
%%%
%%% Created : 16 Aug 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(sherk_aquire).

-export([go/0,go/1,go/2,go/3,go/4,go/5]).
-export([stop/1]).
-export([ass_loaded/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
%%%
%%% Eg;
%%% sherk_aquire:go(1000,call,foo,[mwux005@mwux005],{file,"/tmp"}).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go() ->
  go(5000).

go(Time) ->
  go(Time,{file,"/tmp"}).

go(Time,Dest) ->
  go(Time,call,Dest).

go(Time,Flavor,Dest) ->
  go(Time,Flavor,[node()],Dest).

go(Time,Flavor,Targ,Dest) ->
  go(Time,Flavor,all,Targ,Dest).

go(Time,Flavor,Procs,Targ,Dest) ->
  go(Time,Flavor,Procs,Targ,Dest,node()).

go(Time,Flavor,Procs,Targs,Dest,Proxy) ->
  check_and_spawn(Time,Flavor,Procs,Targs,Dest,Proxy).

stop(Pid) ->
  catch (Pid ! stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% most argument checking is done here. some pid-related checking has
%% to be deferred to the target

check_and_spawn(Time,Flavor,Procs,Targs,Dest,Proxy) ->
  chk_flavor(Flavor),
  LD = dict:from_list(
         [{time  ,chk_time(Time)},
          {flags ,flags(Flavor)},
          {rtps  ,rtps(Flavor)},
          {procs ,chk_procs(Procs)},
          {dest  ,chk_dest(Dest)},
          {targs ,chk_targs(Targs)},
          {proxy ,Proxy},
          {daddy ,self()}]),

  Pid = spawn(fun sherk_host:init/0),
  Pid ! {init,LD},
  Pid.

chk_flavor(call) -> ok;
chk_flavor(proc) -> ok;
chk_flavor(Flav) -> error({bad_flavor,Flav}).

flags(proc) -> [procs,running,garbage_collection,set_on_spawn];
flags(call) -> [call,return_to,arity|flags(proc)].

rtps(call) -> [{{'_','_','_'},[],[local]}];
rtps(proc) -> [].

chk_targs(Targs) ->
  lists:map(fun(T)->chk_conn(T) end,Targs).

chk_conn(T) when T==node() -> T;
chk_conn(T) ->
  case net_adm:ping(T) of
    pong -> ass_loaded(T,sherk_target);
    pang -> exit({connection_failed,T})
  end.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_procs(X)  when all==X; existing==X; new==X -> [X];
chk_procs(Ps) when is_list(Ps)                 -> lists:map(fun chk_proc/1, Ps);
chk_procs(X)                                   -> exit({bad_proc_spec,X}).

chk_proc(X)         when X==all; X==existing; X==new  -> exit({bad_proc,X});
chk_proc(Pid)       when is_pid(Pid)                  -> Pid;
chk_proc(Atom)      when is_atom(Atom)                -> Atom;
chk_proc({pid,I,J}) when is_integer(I), is_integer(J) -> {pid,I,J};
chk_proc(X)                                           -> exit({bad_proc,X}).

chk_dest({ip,Port})  when is_integer(Port) -> {ip,Port};
chk_dest({file,Dir}) when $/==hd(Dir)      -> {file,Dir};
chk_dest(X)                                -> exit({bad_dest,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ass_loaded(Node, Mod) ->
  case rpc:call(Node,Mod,module_info,[compile]) of
    {badrpc,{'EXIT',{undef,_}}} ->          %no code
      netload(Node, Mod),
      ass_loaded(Node, Mod);
    {badrpc,_} ->
      exit({no_connection,Node});
    CompInfo when is_list(CompInfo) ->
      case {ftime(CompInfo), ftime(Mod:module_info(compile))} of
        {interpreted,_} ->
          exit({target_has_interpreted_code,Mod});
        {TargT, HostT} when TargT < HostT -> %old code on target
          netload(Node, Mod),
          ass_loaded(Node, Mod);
        _ ->
          Node
      end
  end.

netload(Node, Mod) ->
  {Mod, Bin, Fname} = code:get_object_code(Mod),
  case rpc:call(Node, code, load_binary, [Mod, Fname, Bin]) of
    {module, Mod} -> ok;
    {error,badfile} ->
      I = (catch rpc:call(Node, erlang, system_info, [otp_release])),
      exit({target_emulator_too_old,Node,I})
  end.

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).
