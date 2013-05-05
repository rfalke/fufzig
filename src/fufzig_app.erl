%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(fufzig_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, main/0]).

-include("cli_options.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(context,{acceptUrlTest=unset,
		 log,
		 fileStoragePid,
		 workerPoolPid,
		 parallel}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fuf_sup:start_link().

stop(_State) ->
    ok.

main()->
    inets:start(),
    Args=init:get_plain_arguments(),
    Options=cli_options:parse_options(Args),
    Log = case Options#options.parallel>1 of
	      true-> fun parallelLog/3;
	      false->fun seqLog/3
	  end,
    WorkerPoolPid = case Options#options.parallel>1 of
			true-> semaphore:start(Options#options.parallel);
			false->unset
		    end,
    FileStoragePid = spawn(fun() -> write_response_loop(Log, Options#options.basedir) end),
    Context = #context{log=Log, 
		       parallel=Options#options.parallel, 
		       fileStoragePid=FileStoragePid, 
		       workerPoolPid=WorkerPoolPid,
		       acceptUrlTest=Options#options.acceptUrlTest},
    driver(Context, url:make_full_url(Options#options.seedurl)),
    halt(0).

%% ===================================================================
%% Implementation
%% ===================================================================
write_response_loop(Log, Dir)->
    receive
	{save, Url, Body, Pid} ->
	    Fname = file_support:write_response_to_file(Dir, Body, Url),
	    Pid!{writtenTo, Fname},
	    write_response_loop(Log, Dir)
    end.

driver(Context, Url) ->
    driver(Context, 1, [Url], sets:new()).

driver(Context, BatchNumber, Todo, DoneSet)->
    io:format("driver: start batch ~B with ~B urls (~B urls already done)~n",
	      [BatchNumber, length(Todo), sets:size(DoneSet)]),
    NewDoneSet = sets:union(DoneSet, sets:from_list(Todo)),

    NewTodoList = sets:to_list(
		    case Context#context.parallel>1 of
			true->
			    doOneBatchParallel(Context, Todo, length(Todo));
			false->
			    doOneBatchSequential(Context, Todo, 0, length(Todo), sets:new())
		    end
		    ),
    NewTodoListFiltered = [X || X <- NewTodoList, not sets:is_element(X, NewDoneSet)],
    NewTodoListSorted = ordsets:to_list(ordsets:from_list(NewTodoListFiltered)),
    case NewTodoListSorted of
        [] -> io:format("Finished downloading~n");
	_ -> driver(Context, BatchNumber+1, NewTodoListSorted, NewDoneSet)
    end.

seqLog(What, Msg, Args)->
    case What of
	start -> 
	    io:format("  "++Msg, Args);
	finished -> 
	    io:format(" "++Msg++"~n", Args);
	other ->
	    io:format(Msg++"~n", Args)
    end.

doOneBatchSequential(Context, Todo, NumDone, NumTotal, AlreadyKnownNewUrlsSet)->
    case Todo of
        [] -> AlreadyKnownNewUrlsSet;
        [Url | Todo2] ->
	    Prefix = lists:flatten(io_lib:format("~B/~B", [NumDone+1, NumTotal])),
            NewUrls0 = handle_one_url(Context, Url, Prefix),
            TestFun = Context#context.acceptUrlTest,
            NewUrls = [X || X <- NewUrls0, TestFun(X) andalso (not sets:is_element(X, AlreadyKnownNewUrlsSet))],
            case NewUrls of
                [_H | _T] -> io:format("    found ~B new urls to crawl: ~p ~n", [length(NewUrls), NewUrls]);
                [] -> ok
            end,
	    AllNewUrls = sets:union(AlreadyKnownNewUrlsSet, sets:from_list(NewUrls)),
            doOneBatchSequential(Context, Todo2, NumDone+1, NumTotal, AllNewUrls)
    end.

parallelLog(What, Msg, Args)->
    case What of
	start -> 
	    io:format("  ~p  "++Msg++"~n", [self()]++Args);
	finished -> 
	    io:format("  ~p    "++Msg++"~n", [self()]++Args);
	other ->
	    io:format("  ~p  "++Msg++"~n", [self()]++Args)
    end.

doOneBatchParallel(Context, Todo, NumTotal)->
    Urls = support:add_index(Todo),
    Processes = Context#context.parallel,
    WorkerPoolPid=Context#context.workerPoolPid,
    ListOfNewUrls = putil:pmap(
      fun({No,Url}) ->
	      semaphore:obtain(WorkerPoolPid),
	      Prefix = lists:flatten(io_lib:format("~B/~B", [No, NumTotal])),
	      NewUrls = handle_one_url(Context, Url, Prefix),
	      semaphore:release(WorkerPoolPid),
	      NewUrls
      end,
      Urls, 100*Processes),
    AllAsSet = lists:foldl(fun(X, Set) -> sets:union(Set, sets:from_list(X)) end, sets:new(), ListOfNewUrls),
    TestFun = Context#context.acceptUrlTest,
    FilteredSet = sets:filter(TestFun, AllAsSet),
    FilteredSet.

% From http://blog.jebu.net/2009/09/erlang-tap-to-the-twitter-stream/
receive_chunk(TimeoutInSec, RequestId, DataParts) ->
    receive
	{http, {RequestId, {error, Reason}}} when (Reason =:= etimedout) orelse (Reason =:= timeout) ->
	    {error, timeout};
	{http, {RequestId, {{_, ErrorCode, _}, _Headers, _}}} ->
	    {error, httpError, ErrorCode};
	{http, {RequestId, Result}} ->
	    {error, other, Result};

	{http,{RequestId, stream_start, _Headers}} ->
	    receive_chunk(TimeoutInSec, RequestId, DataParts);
	{http,{RequestId, stream, Data}} ->
	    receive_chunk(TimeoutInSec, RequestId,DataParts++[Data]);
	{http,{RequestId, stream_end, _Headers}} ->
	    {ok, binary_to_list(support:binary_join(DataParts))}
    after TimeoutInSec * 1000 ->
	    {error, timeout}
    end.

request(Url, TimeoutInSec)->
    case httpc:request(get, {Url, []}, [], [{sync,false},{stream,self}]) of
	{ok, RequestId} ->
	    receive_chunk(TimeoutInSec, RequestId, []);
	Other ->
	    {error, requestError, Other}
    end.

downloadWithRetry(_Url, WhichTry, TotalTries, _Prefix, _Log) when WhichTry > TotalTries ->
    failed;
downloadWithRetry(Url, WhichTry, TotalTries, Prefix, Log)  ->
    Log(start, "~s downloading '~s' ~B/~B ...", [Prefix, Url, WhichTry, TotalTries]),
    Start = support:timestamp(),
    Resp = request(Url, 6),
    Time = support:timestamp() - Start,
    case Resp of
	{ok, Body} ->
	    Bytes = length(Body),
	    Log(finished, "got ~s in ~.1f seconds (~s)", [support:format_bytes(Bytes), Time, support:format_rate(Bytes, Time)]),
	    {ok,Body};
	{error, httpError, ErrorCode} ->
	    Log(finished, "got ~B after ~.1f seconds", [ErrorCode, Time]),
	    case ErrorCode==403 orelse ErrorCode==404 of
		true -> failed;
		false -> downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix, Log)
	    end;
	_ -> Log(finished, "got ~p after ~.1f seconds", [Resp, Time]),
	     downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix, Log)
    end.

handle_one_url(Context, Url, Prefix) ->
    TotalTries = 5,
    Log=Context#context.log,
    case downloadWithRetry(Url, 1, TotalTries, Prefix, Log) of
	{ok, Body} ->
	    Context#context.fileStoragePid!{save, Url, Body, self()},
	    receive
		{writtenTo, Fname}-> 
		    Log(other, "    saved to ~s", [Fname])
	    end,
	    Links = extraction:extract_links(Url, Body),
	    Links;
	failed ->
	    Log(other, "    *** failed to download '~s'", [Url]),
	    []
    end.

-ifdef(TEST).

simple_test() ->
    ?assert(length([1,2,3]) =:= 3).
-endif.
