-module(gperf).
-export([ni/0,ni/1,start/1,start/0,log/2]).

ni() -> ni([]).
ni(Args) -> self() ! {args, Args}, gperfGtk:init().

start() -> start(['']).

start(Args) -> gperfGtk:start() ! {args,Args}.

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
