%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 30 Sep 2015 by masse <masse@klarna.com>

%% @doc
%% dumps a trace file to a formatted text file
%% @end

-module(sherk_dump).
-author('masse').
-export([go/0,go/1,go/2]).

go() ->
  go("/Users/masse/kred@kred-verif-app1.trz",[{pattern,'<0.1629.0>'}]).
go(Filename) ->
  go(Filename,[]).
go(Filename,Opts) ->
  sherk_scan:fold(Filename,fun handler/2,[],Opts).

handler(eof,{_,_,A}) ->
  out(A);
handler({_,{O,_,M,T}},A) ->
  case A of
    [] -> {T,T,format(O,M,T,T,T,A)};
    {T0,T1,Acc} -> {T0,T,format(O,M,T,T0,T1,Acc)}
  end.

format(O,M,T,T0,T1,Acc) ->
  [{diff(T,T0),diff(T,T1),O,M}|Acc].

diff({S0,N0},{S1,N1}) ->
  (S0-S1)*1000000+(N0-N1)/1000.

out(A)->
  FN = "foox",
  {ok,FD} = file:open(FN,[write]),
  [write(FD,T0,T1,O,M) || {T0,T1,O,M} <- lists:reverse(A)],
  file:close(FD),
  FN.

write(FD,T0,T1,O,M) ->
  io:fwrite(FD,"~10w ~8w ~10w ~p~n",[round(T0),round(T1),O,M]).
