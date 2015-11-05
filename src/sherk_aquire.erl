%%%-------------------------------------------------------------------
%%% File     : sherk_aquire.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description :
%%%
%%% Created : 16 Aug 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(sherk_aquire).

-export([go/0,go/1,go/2,go/3,go/4,go/5,go/6]).
-export([stop/1]).

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
  go(Time,Dest,call).

go(Time,Dest,Flavor) ->
  go(Time,Dest,Flavor,[node()]).

go(Time,Dest,Flavor,Targ) ->
  go(Time,Dest,Flavor,Targ,node()).

go(Time,Dest,Flavor,Targ,Proxy) ->
  go(Time,Dest,Flavor,Targ,Proxy,all).

go(Time,Dest,Flavor,Targs,Proxy,Procs) ->
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
    pong -> T;
    pang -> exit({connection_failed,T})
  end.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_procs(X)  when all==X;existing==X;new==X -> [X];
chk_procs(Ps) when is_list(Ps)               -> lists:map(fun chk_proc/1, Ps);
chk_procs(X)                                 -> exit({bad_proc_spec,X}).

chk_proc(X)         when X==all; X==existing; X==new  -> exit({bad_proc,X});
chk_proc(Pid)       when is_pid(Pid)                  -> Pid;
chk_proc(Atom)      when is_atom(Atom)                -> Atom;
chk_proc({pid,I,J}) when is_integer(I), is_integer(J) -> {pid,I,J};
chk_proc(X)                                           -> exit({bad_proc,X}).

chk_dest({ip,Port})  when is_integer(Port) -> {ip,Port};
chk_dest({file,Dir}) when $/==hd(Dir)      -> {file,Dir};
chk_dest(X)                                -> exit({bad_dest,X}).
