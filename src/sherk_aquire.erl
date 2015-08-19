%%%-------------------------------------------------------------------
%%% File     : sherk_aquire.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description :
%%%
%%% Created : 16 Aug 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(sherk_aquire).

-export([go/0,go/1,go/2,go/3,go/4,go/5]).
-export([stop/0,kill/0]).
-export([ass_loaded/2]).

-include("log.hrl").

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

go(Time,Flavor,Procs,Targs,Dest) ->
  check_and_spawn(Time,Flavor,Procs,Targs,Dest).

stop() ->
  catch (sherk_host ! stop).

kill() ->
  catch exit(erlang:whereis(sherk_host),kill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% most argument checking is done here. some pid-related checking has
%% to be deferred to the target

check_and_spawn(Time,Flavor,Procs,Targs,Dest) ->
  LD = dict:from_list(
         [{time  ,chk_time(Time)},
          {flags ,flags(Flavor)},
          {rtps  ,rtps(Flavor)},
          {procs ,chk_procs(Procs)},
          {dest  ,chk_dest(Dest)},
          {targs ,chk_targs(Targs)},
          {daddy ,self()}]),

  Pid = spawn(fun init/0),
  Pid ! {init,LD},
  Pid.

flags(proc) -> [procs,running,garbage_collection,set_on_spawn];
flags(call) -> [call,return_to,arity|flags(proc)];
flags(Flavor) -> error({bad_flavor,Flavor}).

rtps(call) -> [{{'_','_','_'},[],[local]}];
rtps(proc) -> [];
rtps(Flav) -> error({bad_flavor,Flav}).

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
%%% the host process
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
  case dict:fetch(P,FDs) of
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
