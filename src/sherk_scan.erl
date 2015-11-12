%%%----------------------------------------------------------------------
%%% File    : shrerk_scan.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose :
%%% Created : 27 Feb 2006 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(sherk_scan).
-author('etxmacr@avc386').

-include_lib("kernel/include/file.hrl").

-export([fold/3,fold/4]).

-include("log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  fold(Filename,Fun,Acc0,Opts).
%%%  Opts - [Opt] - defaults to []
%%%  Opt - {pattern,Patt} | {min,Min} | {max,Max} | {raw,boolean(Raw)}
%%%  folds Fun over trace messages in a trace file.
%%%  prefilters on sequence number (Min and Max) and a pattern Patt.
%%%
%%%  Filename - string()
%%%  Patt - list(term(P))|term(P) - all terms P must exist in the message.
%%%    funs, ports, refs and pids ar turned into atoms.
%%%  Fun - fun(Msg,Acc0) -> Acc1.
%%%  Msg - the trace message
%%%  Raw - boolean() - if true, trace messages are normalized to sherk form.
%%%  Min - integer() - min sequence number
%%%  Max - integer() - max sequence number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state,{fd,
               seq = 1,
               hits = 0,
               eof = false,
               cb,
               pattern,
               raw,
               min,
               max}).

fold(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[]).
fold(Filename,Fun,Acc0,Opts) ->
  sherk_ets:new(?MODULE),
  {ok,FD} = file:open(Filename,[read,raw,binary,compressed]),
  try
    {_,Acc} = (file_action(mk_state(Fun,Acc0,FD,Opts)))#state.cb,
    Acc
  after
    file:close(FD)
  end.

mk_state(CB,Acc,FD,Opts) ->
  #state{fd      = FD,
         cb      = {CB,Acc},
         pattern = getv(pattern,Opts,''),
         raw     = getv(raw,Opts,false),
         min     = getv(min,Opts,0),
         max     = getv(max,Opts,'')}.

getv(K,PL,Def) ->
  proplists:get_value(K,PL,Def).

-define(CHUNKSIZE,100000).
file_action(S) ->
  file_action(make_chunk(S#state.fd,<<>>),S).

-spec file_action(eof | {any(),binary()},#state{}) -> #state{}.
file_action(eof,S) ->
  do(eof,S);
file_action({Term,Rest},S) ->
  NS = do(Term,S),
  case NS#state.eof of
    false -> file_action(make_chunk(S#state.fd,Rest),NS);
    true  -> NS
  end.

-spec make_chunk(file:fd(),eof | binary()) -> eof | {any(),binary()}.
make_chunk(_,eof) ->
  eof;
make_chunk(FD,Bin) ->
  case Bin of
    <<0,Size:32,T:Size/binary,Tail/binary>> -> {binary_to_term(T),Tail};
    _ -> make_chunk(FD,get_more_bytes(FD,Bin))
  end.

get_more_bytes(FD,Rest) ->
  case file:read(FD,?CHUNKSIZE) of
    {ok,Bin} -> <<Rest/binary,Bin/binary>>;
    _ -> eof
  end.

-spec do(any(),#state{}) -> #state{}.
do(M,S) when M =:= eof orelse (S#state.max < S#state.seq) ->
  NS = call_cb(eof,S),
  NS#state{eof=true,hits=S#state.hits};
do(M,S) when S#state.seq < S#state.min ->
  case is_meta(M) of
    true -> S;
    false-> bump_seq(S)
  end;
do(M,S) ->
  case {S#state.raw, is_meta(M)} of
    {true ,true } -> call_cb(M,S);
    {true ,false} -> bump_seq(call_cb(M,S));
    {false,true } -> mass(M),S;
    {false,false} -> bump_seq(call_cb(mass(M),S))
  end.

bump_seq(S) ->
  S#state{seq = S#state.seq+1}.

-spec call_cb(any(),#state{}) -> #state{}.
call_cb(M,S) ->
  case M =:= eof orelse grep(S#state.pattern,M) of
    false-> S;
    true -> S#state{hits = S#state.hits+1,cb = safe_cb(M,S)}
  end.

safe_cb(M,#state{cb = {Fun,Acc},seq = Seq}) ->
  case M of
    eof -> {Fun,Fun(eof,Acc)};
    _   -> {Fun,Fun({Seq,M},Acc)}
  end.

-spec grep(any(),any()) -> boolean().
grep('',_) -> true;
grep(P,T) when not is_list(P) -> grep([P],T);
grep(P,T) ->
  case grp(P,T) of
    [] -> true;
    _  -> false
  end.

grp([],_) -> [];
grp(P,[]) -> P;
grp(P,Fun) when is_function(Fun) ->
  grp(P,list_to_atom(erlang:fun_to_list(Fun)));
grp(P,Port) when is_port(Port) ->
  grp(P,list_to_atom(erlang:port_to_list(Port)));
grp(P,Ref) when is_reference(Ref) ->
  grp(P,list_to_atom(erlang:ref_to_list(Ref)));
grp(P,Pid) when is_pid(Pid) ->
  grp(P,pid_to_atom(Pid));
grp(P,T) when is_tuple(T) ->
  case lists:member(T,P) of
    true -> grp(P--[T],[]);
    false -> grp(P,tuple_to_list(T))
  end;
grp(P,L) when is_list(L) ->
  case lists:member(L,P) of
    true -> grp(P--[L],[]);
    false -> grp(grp(P,hd(L)),tl(L))
  end;
grp(P,T) ->
  grp(P--[T],[]).

pid_to_atom(Pid) ->
  [_,A,B] = string:tokens(pid_to_list(Pid),"<.>"),
  list_to_atom(lists:flatten(["<0.",A,".",B,">"])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format is {Tag,PI,Data,Timestamp}
%%% PI is {pid(),Info}
%%% Info is atom()|tuple()
%%% Timestamp is no_time|now()
%%% Data is;
%%%
%%% send,                         {PI2,Msg}
%%% send_to_non_existing_process, {PI2,Msg}
%%% 'receive',                    Message
%%% call,                         {M,F,A}
%%% return_to,                    {M,F,A}
%%% return_from,                  {{M,F,A},ReturnValue}
%%% spawn,                        {PI2,{M,F,A}}
%%% exit,                         Reason
%%% link,                         PI2
%%% unlink,                       PI2
%%% getting_linked,               PI2
%%% getting_unlinked,             PI2
%%% register,                     registered_name
%%% unregister,                   registered_name
%%% in,                           {M,F,A}
%%% out,                          {M,F,A}
%%% gc_start,                     Info
%%% gc_end,                       Info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_meta(M) ->
  case element(1,M) of
    trace    -> false;
    trace_ts -> false;
    _        -> true
  end.

mass({port_info,Info})  -> handle_porti(Info),[];
mass({proc_info,Info})  -> handle_proci(Info),[];
mass({trace_info,Info}) -> handle_traci(Info),[];

mass({trace,A,B,C})         -> mass(A,B,C,ts());
mass({trace,A,B,C,D})       -> mass(A,B,{C,D},ts());
mass({trace_ts,A,B,C,TS})   -> mass(A,B,C,ts(TS));
mass({trace_ts,A,B,C,D,TS}) -> mass(A,B,{C,D},ts(TS));
mass(X)                     -> ?log({unrec_msg,X}),[].

mass(Pid,T=send,X,TS) ->                         mass_send(Pid,T,X,TS);
mass(Pid,T=send_to_non_existing_process,X,TS) -> mass_send(Pid,T,X,TS);
mass(Pid,T='receive',Message,TS) ->              {T,pi(Pid),Message,TS};
mass(Pid,T=call,MFA,TS) ->                       {T,pi(Pid),MFA,TS};
mass(Pid,T=return_to,MFA,TS) ->                  {T,pi(Pid),MFA,TS};
mass(Pid,T=return_from,{MFA,R},TS) ->            {T,pi(Pid),{MFA,R},TS};
mass(Pid,T=spawn,{P2,MFA},TS) ->   ins({P2,MFA}),{T,pi(Pid),{pi(P2),MFA},TS};
mass(Pid,T=exit,Reason,TS) ->                    {T,pi(Pid),Reason,TS};
mass(Pid,T=link,Pd,TS) ->                        {T,pi(Pid),pi(Pd),TS};
mass(Pid,T=unlink,Pd,TS) when is_pid(Pd) ->      {T,pi(Pid),pi(Pd),TS};
mass(Pid,T=unlink,_Crap,TS) ->                   {T,pi(Pid),unknown,TS};
mass(Pid,T=getting_linked,Pd,TS) ->              {T,pi(Pid),pi(Pd),TS};
mass(Pid,T=getting_unlinked,Pd,TS) ->            {T,pi(Pid),pi(Pd),TS};
mass(Pid,T=register,Nm,TS) ->      ins({Pid,Nm}),{T,pi(Pid),Nm,TS};
mass(Pid,T=unregister,Name,TS) ->      del(Name),{T,pi(Pid),Name,TS};
mass(Pid,T=in,MFA,TS) ->                         {T,pi(Pid),MFA,TS};
mass(Pid,T=out,MFA,TS) ->                        {T,pi(Pid),MFA,TS};
mass(Pid,T=gc_start,Info,TS) ->                  {T,pi(Pid),Info,TS};
mass(Pid,T=gc_end,Info,TS) ->                    {T,pi(Pid),Info,TS}.

mass_send(Pid,T,{Msg,To},TS) when is_pid(To); is_port(To) ->
  {T,pi(Pid),{pi(To),Msg},TS};
mass_send(Pid,T,{Msg,To},TS) when is_atom(To) ->
  {T,pi(Pid),{{find_pid(To),To},Msg},TS};
mass_send(Pid,T,{Msg,{To,Node}},TS) when is_atom(To),Node==node(Pid) ->
  {T,pi(Pid),{{find_pid(To),To},Msg},TS};
mass_send(Pid,T,{Msg,{To,Node}},TS) when is_atom(To),is_atom(Node) ->
  {T,pi(Pid),{{remote,{To,Node}},Msg},TS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert to {Second,MilliSecond}
ts() -> ts({0,0,0}).

ts({0,S,Ms})  -> {S,Ms};
ts({MS,S,Ms}) -> {MS*1000000+S,Ms}.

pi(file_driver) -> {trace,file_driver};
pi(Port) when is_port(Port) ->
  case ets_lup(Port) of
    [] -> {Port,unknown};
    Name -> {Port,Name}
  end;
pi(Pid) when is_pid(Pid) ->
  case ets_lup(Pid) of
    [] -> {Pid,unknown};
    {M,F,A} when is_list(A) -> {Pid,{M,F,length(A)}};
    Name -> {Pid,Name}
  end.

find_pid(Name) -> ets_lup(Name).

handle_porti(Is) -> lists:foreach(fun(I)->ins(I) end,Is).

handle_proci(Is) -> lists:foreach(fun(I)->ins(I) end,Is).

handle_traci(_I) -> ok.

%%% sherk_scan table translates Pid -> Tag,where Tag can be;
%%% atom() -  (a registered name)
%%% tuple(atom(),atom(),integer()) - an initial call (we missed the spawn)
%%% tuple(atom(),atom(),list()) - info from spawn
%%%
%%% it also translates RegisteredName -> Pid

ins({Pid,Reg}) when is_atom(Reg) -> ets_ins({Pid,Reg}),ets_ins({Reg,Pid});
ins({Port,Desc}) when is_port(Port) -> ets_ins({Port,Desc});
ins({Pid,{M,F,A}}) when is_integer(A) -> ets_ins({Pid,{M,F,A}});
ins({Pid,Fun}) when is_function(Fun) -> ets_ins({Pid,funi(Fun)});
ins({Pid,{M,F,As}}) when is_list(As) -> ets_ins({Pid,mangle_ic({M,F,As})}).

del(Reg) when is_atom(Reg) -> ?log({unregistered,Reg}),ets_del(Reg).

mangle_ic(MFA) ->
  case MFA of
    {proc_lib,init_p,[_,_,M,F,A]} ->
      {proc_lib,trans_init(M,F,A)};
    {file,file,[_,FileName,_]} ->
      {file,{atomize(FileName)}};
    {dets,do_open_file,[Tab,_FileName,_,_,_,_,_,_Ram,_,_,_]} ->
      {dets,{Tab}};
    {application_master,start_it,[_,{state,_,ApplD,_,_,_},_,_]} ->
      {appl_data,App,_,_,_,_,_,_,_} = ApplD,
      {application_master,{App}};
    {erlang,apply,[Fun,[]]} when is_function(Fun) ->
      funi(Fun);
    {M,F,As} ->
      {M,F,As}
  end.

atomize(FileName) ->
  list_to_atom(hd(lists:reverse(string:tokens(FileName,"/")))).

funi(Fun) ->
  case erlang:fun_info(Fun,module) of
    {_,rpc} ->
      case erlang:fun_info(Fun,env) of
        {_,[_,_,Pid,A,_F,_M]} when is_pid(Pid),is_list(A) ->
          {rpc,{call_dummy,node(Pid)}};
        {_,[_,Pid,A,F,M]} when is_pid(Pid)->
          {rpc,{call,node(Pid)},{M,F,length(A)}};
        {_,[Pid,A,F,M]} when is_pid(Pid),is_list(A) ->
          {rpc,{cast,node(Pid)},{M,F,length(A)}};
        _X ->
          {rpc}
      end;
    {_,Mod} ->
      {new_index,Idx} = erlang:fun_info(Fun,new_index),
      {'fun',{Mod,Idx}}
  end.

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
  {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
  {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
  {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
  {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
  {gen_server,Module};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
  {gen_server,Module};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
  {gen_fsm,Module};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
  {gen_fsm,Module};
trans_init(gen,init_it,[gen_event|_]) ->
  {gen_event};
trans_init(M,F,A) ->
  {M,F,length(A)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ets_ins(Rec) -> ets_ins(?MODULE,Rec).
ets_ins(Tab,Rec) -> ets:insert(Tab,Rec).

ets_lup(Key) -> ets_lup(?MODULE,Key).
ets_lup(Tab,Key) ->
  try ets:lookup(Tab,Key) of
      [{Key,R}] -> R;
      R -> R
  catch _:_ -> []
  end.

ets_del(Key) -> ets:delete(?MODULE,Key).
