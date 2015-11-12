%%%-------------------------------------------------------------------
%%% File    : sherk_target.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created :  5 Sep 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_target).

-export([init/0,self_register/1]).
-export([get_nodes/0]).

-include_lib("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  the control process
%%%  tracing can go to a file, or to a local consumer process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  self_register(sherk_control),
  receive
    {init,LD} -> loop(start(check_dest(check_procs(LD))))
  end.

loop(LD) ->
  receive
    stop -> ok
  end,
  stop_trace(LD),
  send_files(LD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(LD) ->
  unset_tps(),
  set_tps(dict:fetch(rtps,LD)),
  start_trace(LD).

start_trace(LD) ->
  Procs     = dict:fetch(procs,LD),
  TimeStamp = timestamp(Procs),
  Consumer  = consumer(dict:fetch(dest,LD)),
  Flags     = [{tracer,Consumer},TimeStamp|dict:fetch(flags,LD)],
  send2port(Consumer,{trace_info,dict:to_list(LD)}),
  send2port(Consumer,{start_time,os:timestamp()}),
  lists:foreach(fun(P) -> erlang:trace(P,true,Flags) end,Procs),
  dict:fetch(daddy,LD) ! {info,self(),LD},
  dict:store(consumer,Consumer,LD).

consumer(Dest) ->
  Port = mk_port(Dest),
  send2port(Port,{proc_info,[{P,pi(P)} || P <- processes()]}),
  send2port(Port,{port_info,[{P,pi(P)} || P <- erlang:ports()]}),
  Port.

mk_port({file,{0,File}}) ->
  (dbg:trace_port(file,File))();
mk_port({file,{Size,File}}) ->
  (dbg:trace_port(file,{File, wrap, ".trc", Size, 8}))();
mk_port({ip,Port,QueSize}) ->
  (dbg:trace_port(ip,{Port, QueSize}))().

set_tps(TPs) -> lists:foreach(fun set_tps_f/1,TPs).

set_tps_f({MFA,MS,Fs}) -> erlang:trace_pattern(MFA,MS,Fs).

timestamp(Procs) ->
  case cpu_timestamp_works() andalso Procs =:= [all] of
    true  -> cpu_timestamp;
    false -> timestamp
  end.

cpu_timestamp_works() ->
  try
    erlang:trace(all,true,[cpu_timestamp]),
    erlang:trace(all,false,[cpu_timestamp]),
    true
  catch
    _:_ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_trace(LD) ->
  Consumer = dict:fetch(consumer,LD),
  erlang:trace(all,false,dict:fetch(flags,LD)),
  unset_tps(),
  send2port(Consumer,{stop_time,os:timestamp()}),
  consumer_stop(Consumer).

consumer_stop(Port) -> erlang:port_close(Port).%%dbg:flush_trace_port().

unset_tps() ->
  erlang:trace_pattern({'_','_','_'},false,[local]),
  erlang:trace_pattern({'_','_','_'},false,[global]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_dest(LD) ->
  dict:store(dest,mk_dest(dict:fetch(dest,LD)),LD).

mk_dest({ip,IP})    -> {ip,IP};
mk_dest({file,Dir}) -> {file,{0,mk_file(Dir)}};
mk_dest(Dest)       -> error({bad_dest,Dest}).

mk_file(Dir) ->
  File = filename:join([Dir,"sherk","sherk"++timestring()++".trc"]),
  try
    ok = filelib:ensure_dir(File),
    File
  catch
    _:Error -> error({file_create_fail,{Error,File}})
  end.

timestring() ->
  {{Y,M,D},{Ho,Mi,Se}} = calendar:local_time(),
  Format = "~4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
  lists:flatten(io_lib:format(Format,[Y,M,D,Ho,Mi,Se])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_procs(LD) ->
  dict:store(procs,[check_proc(P) || P <- dict:fetch(procs,LD)],LD).

check_proc(X) ->
  case mk_prc(X) of
    A when is_atom(A) -> chk_tracer(new),A;
    Pid when is_pid(Pid) -> chk_tracer(Pid),Pid
  end.

chk_tracer(P) ->
  case erlang:trace_info(P,tracer) of
    {tracer,[]}         -> ok;
    {tracer,Tracer} -> exit({already_traced,tracer_info(Tracer)})
  end.

tracer_info(Tracer) when is_pid(Tracer) ->  process_info(Tracer);
tracer_info(Tracer) when is_port(Tracer) -> erlang:port_info(Tracer).

mk_prc(all) -> all;
mk_prc(Pid) when is_pid(Pid) -> Pid;
mk_prc({pid,P1,P2}) when is_integer(P1), is_integer(P2) -> c:pid(0,P1,P2);
mk_prc(Reg) when is_atom(Reg) ->
  case whereis(Reg) of
    undefined -> exit({no_such_process, Reg});
    Pid when is_pid(Pid) -> Pid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(CHUNKSIZE,8192).

send_files(LD) ->
  case dict:fetch(dest,LD) of
    {ip,_} -> ok;
    {file,{0,Tmp}} -> send_chunks(Tmp,dict:fetch(daddy,LD)), rm(Tmp);
    {file,{_,_}} -> exit({many_files,not_yet_implemented})
  end.

send_chunks(File, Daddy) ->
  {ok,FD} = file:open(File, [read, raw, binary]),
  Get = fun() -> file:read(FD, ?CHUNKSIZE) end,
  Put = fun(X)-> Daddy ! {self(), chunk, X} end,
  send_chunks(Get(), Get, Put),
  file:close(FD).

send_chunks({ok, Bin}, Get, Put) ->
  Put(Bin),
  send_chunks(Get(),Get,Put);
send_chunks(eof, _, Put) ->
  Put(eof);
send_chunks({error, Reason}, _, Put) ->
  Put({error, Reason}).

rm(File) ->
  %%io:fwrite("delete ~p~n",[File]),
  file:delete(File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pi(P) when is_pid(P) ->
  case process_info(P, registered_name) of
    [] ->
      case process_info(P, initial_call) of
        {_, {proc_lib,init_p,_}} -> proc_lib:translate_initial_call(P);
        {_,MFA} -> MFA;
        undefined -> dead
      end;
    {_,Nam} -> Nam;
    undefined -> dead
  end;
pi(P) when is_port(P) ->
  case erlang:port_info(P,name) of
    {name,N} ->
      [Hd|_] = string:tokens(N," "),
      lists:reverse(hd(string:tokens(lists:reverse(Hd),"/")));
    undefined ->
      "dead"
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
self_register(Name) ->
  try register(Name,self())
  catch error:badarg -> exit({already_running})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send2port(Port, Bin) when is_port(Port), is_binary(Bin) ->
  erlang:port_command(Port, Bin);
send2port(Port, Bin) when is_binary(Bin) ->
  ?log({bad_port, {Port}});
send2port(Port, Term) ->
  send2port(Port, term_to_binary(Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_nodes() ->
  case filelib:wildcard(filename:join([code:root_dir(),'erts*',bin,epmd])) of
    [Epmd|_] -> parse_epmd(os:cmd(Epmd++" -names"));
    []       -> []
  end.

parse_epmd(EpmdStr) ->
  [_,Host] = string:tokens(atom_to_list(node()),"@"),
  Lines = string:tokens(EpmdStr,"\n"),
  CPs = [N || ["name",N|_] <- [string:tokens(Line," ") || Line <- Lines]],
  [{list_to_atom(Host),list_to_atom(CP++"@"++Host)} || CP <-CPs].
