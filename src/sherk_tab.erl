%%%-------------------------------------------------------------------
%%% File    : sherk_tab.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : open the trace file, and generate the sherk_prof table
%%%               (by running sherk_scan:fold(fun sherk_prof:go...))
%%%
%%% Created : 21 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_tab).

-export([assert/1,check_file/1]).

-include_lib("kernel/include/file.hrl").

-include("log.hrl").

assert(File) ->
  TFile = filename:dirname(File)++"/."++filename:basename(File,".trz")++".etz",
  {ok,#file_info{mtime = MT}} = file:read_file_info(File),
  case file:read_file_info(TFile) of
    {ok,#file_info{mtime = TabMT}} when MT < TabMT ->
      %% the tab file exists and is up-to-date
      case sherk_ets:lup(sherk_prof,file) of
        File -> ?log({is_cached,TFile});
        _ ->
          ?log({restoring_tab,sherk_prof}),
          try sherk_ets:f2t(TFile)
          catch
            _:X ->
              ?log({deleting_bad_tab_file,X}),
              file:delete(TFile),
              assert(File)
          end
      end;
    _ ->
      %% make tab and save it
      ?log([creating_tab]),
      sherk_scan:fold(File,fun sherk_prof:go/2,[]),
      ets:insert(sherk_prof, {file, File}),
      try
        ?log(storing_tab),
        sherk_ets:t2f([sherk_prof,sherk_scan],TFile)
      catch
        _:_ -> ?log({creation_failed,TFile})
      end
  end,
  ?log([folding_pids]),
  ets:foldl(fun store_pid/2, [], sherk_prof),
  ?log([done]).

store_pid({{{pid,time},P},_},_) -> ets:insert(sherk_prof,{sherk:to_str(P),P});
store_pid(_,_) -> ok.

check_file(File) -> ".trz" = filename:extension(File).
