%%%-------------------------------------------------------------------
%%% File    : sherk.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 14 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk).

-export([go/0]).  % interactive
-export([ni/0]).  % non-interactive
-export([fold/3,fold/4]).

%% sherk-specific export
-export([to_str/1]).

%% internal exports
-export([log/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go() -> spawn_link(fun sherk_gtk:init/0).

ni() -> sherk_gtk:init().

fold(File,Fun,Acc)      -> fold(File,Fun,Acc,[]).
fold(File,Fun,Acc,Opts) -> sherk_scan:fold(File,Fun,Acc,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
  error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).

to_str(X) when is_pid(X) -> pid_to_list(X);
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_float(X) ->  float_to_list(X);
to_str(X) when is_list(X) ->
  case is_string(X) of
    true -> X;
    false -> lists:flatten(io_lib:fwrite("~p",[X]))
  end;
to_str(X) -> lists:flatten(io_lib:fwrite("~p",[X])).

is_string(X) when not is_list(X) -> false;
is_string([]) -> true;
is_string([H|T]) when is_integer(H), H >= $ , H =< $~ -> is_string(T);
is_string(_) -> false.
