%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 11 Nov 2015 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('sherk_net').
-author('mats cronqvist').
-export([names/0]).

names() ->
  {ok,NamePorts} = net_adm:names(),
  [name(Port) || {_,Port} <- NamePorts].

name(Port) ->
  {ok,P} = gen_tcp:connect("localhost",Port,[binary,{packet,2},{active,false}]),
  ok = gen_tcp:send(P,<<$n,5:16, 16#FFF:32,"erl@localhost">>),
  {ok,<<"sok">>} = gen_tcp:recv(P,0),
  {ok,<<$n,5:16,_Flags:32,_Challenge:32,Name/binary>>} = gen_tcp:recv(P,0),
  gen_tcp:close(P),
  Name.
