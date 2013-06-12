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
		 urlCollectorPid,
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
    SeedUrl = url:make_full_url(Options#options.seedurl),
    ShowAddedUrls = Options#options.parallel =< 1,
    UrlCollectorPid = spawn(fun() -> collect_urls_loop(sets:from_list([SeedUrl]), ShowAddedUrls) end),
    Context = #context{log=Log,
		       parallel=Options#options.parallel,
		       fileStoragePid=FileStoragePid,
		       workerPoolPid=WorkerPoolPid,
		       urlCollectorPid=UrlCollectorPid,
		       acceptUrlTest=Options#options.acceptUrlTest},
    spawn(fun() -> support:repeat_call(fun()->watcher(WorkerPoolPid, UrlCollectorPid) end, 15000) end),
    driver(Context),
    halt(0).

%% ===================================================================
%% Implementation
%% ===================================================================
get_download_status(Pid)->
    Pid!{queryStats, self()},
    receive_download_status(Pid).

receive_download_status(Pid)->
    receive
	{statsFor, Pid2, Url, StartTime, Size, Downloaded}->
	    case Pid == Pid2 of
		false->
		    receive_download_status(Pid);
		true->
		    Time=support:timestamp()-StartTime,
		    case Size of
			notStarted->
			    lists:flatten(io_lib:format("~.1fs ~p '~s'", [Time, Size, Url]));
			unknownSize->
			    lists:flatten(io_lib:format("~.1fs ~B/~p ~s '~s'", [Time, Downloaded,Size, support:format_rate(Downloaded, Time), Url]));
			_ ->
			    Percent=(Downloaded*100.0)/Size,
			    lists:flatten(io_lib:format("~.1fs ~B/~p ~.1f% ~s '~s'", [Time, Downloaded, Size, Percent, support:format_rate(Downloaded, Time), Url]))
		    end
	    end
    after 100 -> "unknown"
    end.

watcher(WorkerPoolPid, UrlCollectorPid)->
    gc_all_processes(),
    %support:dump_processes(),
    UrlCollectorPid!{queryStats,self()},
    receive
	{numUrls,N}->
	    Now=support:now_as_string(),
	    io:format("~s:  there are ~B urls gathered for the next batch~n", [Now,N])
    end,
    case WorkerPoolPid of
	unset->ok;
	_->dump_active_downloads(WorkerPoolPid)
    end.

gc_all_processes()->
    [erlang:garbage_collect(X) || X<-processes()].

dump_active_downloads(WorkerPoolPid)->
    Pids=support:add_index(semaphore:stats(WorkerPoolPid)),
    N = length(Pids),
    Now=support:now_as_string(),
    io:format("~s:  dumping ~B active downloads~n", [Now,N]),
    lists:foreach(fun({No, P})->io:format("    ~2B/~2B: ~p ~s~n",[No, N, P, get_download_status(P)]) end, Pids).

write_response_loop(Log, Dir)->
    receive
	{save, Url, Body, Pid} ->
	    Fname = file_support:write_response_to_file(Dir, Body, Url),
	    Pid!{writtenTo, Fname},
	    write_response_loop(Log, Dir)
    end.

collect_urls_loop(Urls, Verbose)->
    receive
	{add, _BaseUrl, NewUrls, Pid} ->
	    case Verbose of
		true ->
		    ReallyNewUrls = [X || X <- sets:to_list(NewUrls), (not sets:is_element(X, Urls))],
		    case ReallyNewUrls of
			[_H | _T] ->
			    Sorted = ordsets:to_list(ordsets:from_list(ReallyNewUrls)),
			    io:format("    found ~B new urls to crawl: ~p ~n", [length(Sorted), Sorted]);
			[] -> ok
		    end;
		false->ok
	    end,
	    NewSet = sets:union(Urls, NewUrls),
	    Pid!added,
	    collect_urls_loop(NewSet, Verbose);
	{retrieve,Pid} ->
	    Pid!{knownUrls, ordsets:from_list(sets:to_list(Urls))},
	    collect_urls_loop(sets:new(), Verbose);
	{queryStats,Pid}->
	    Pid!{numUrls, sets:size(Urls)},
	    collect_urls_loop(Urls, Verbose)
    end.

driver(Context) ->
    driver(Context, 1, sets:new()).

driver(Context, BatchNumber, DoneSet)->
    Context#context.urlCollectorPid!{retrieve,self()},
    receive
	{knownUrls,RawTodoList}->ok
    end,

    TodoList = [X || X <- RawTodoList, not sets:is_element(X, DoneSet)],
    case TodoList of
        [] -> io:format("Finished downloading~n");
	_ ->
	    io:format("driver: start batch ~B with ~B urls (~B urls already done)~n",
		      [BatchNumber, length(TodoList), sets:size(DoneSet)]),
	    NewDoneSet = sets:union(DoneSet, sets:from_list(TodoList)),
	    NumTodos=length(TodoList),
	    TodoListWithPrefix = [{Url, lists:flatten(io_lib:format("~B/~B", [No, NumTodos]))} || {No, Url}<-support:add_index(TodoList)],
	    case Context#context.parallel>1 of
		true-> doOneBatchParallel(Context, TodoListWithPrefix);
		false-> doOneBatchSequential(Context, TodoListWithPrefix)
	    end,
	    driver(Context, BatchNumber+1, NewDoneSet)
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

doOneBatchSequential(Context, Todo)->
    case Todo of
        [] -> ok;
        [{Url,Prefix} | Tail] ->
            handle_one_url(Context, Url, Prefix),
            doOneBatchSequential(Context, Tail)
    end.

parallelLog(What, Msg, Args)->
    Now = support:now_as_string(),
    case What of
	start ->
	    io:format("~s:  ~p  "++Msg++"~n", [Now,self()]++Args);
	finished ->
	    io:format("~s:  ~p    "++Msg++"~n", [Now,self()]++Args);
	other ->
	    io:format("~s:  ~p  "++Msg++"~n", [Now,self()]++Args)
    end.

doOneBatchParallel(Context, Urls)->
    Processes = Context#context.parallel,
    WorkerPoolPid = Context#context.workerPoolPid,
    putil:pmap(
      fun({Url, Prefix}) ->
	      semaphore:obtain(WorkerPoolPid),
	      handle_one_url(Context, Url, Prefix),
	      semaphore:release(WorkerPoolPid),
	      ok
      end,
      Urls, 100*Processes).

getContentLengthFromHeaders([H|T]) ->
    case H of
        {"content-length",Str}->
            {N,[]} = string:to_integer(Str),
            N;
        _ -> getContentLengthFromHeaders(T)
    end;
getContentLengthFromHeaders([])->
    unknownSize.

% From http://blog.jebu.net/2009/09/erlang-tap-to-the-twitter-stream/
receive_chunk(TimeoutInSec, RequestId, DataParts, Url, StartTime, Size, Downloaded) ->
    receive
	{http, {RequestId, {error, Reason}}} when (Reason =:= etimedout) orelse (Reason =:= timeout) ->
	    {error, timeout};
	{http, {RequestId, {{_, ErrorCode, _}, _Headers, _}}} ->
	    {error, httpError, ErrorCode};
	{http, {RequestId, Result}} ->
	    {error, other, Result};

	{http,{RequestId, stream_start, Headers}} ->
	    receive_chunk(TimeoutInSec, RequestId, DataParts, Url, StartTime, getContentLengthFromHeaders(Headers), Downloaded);
	{http,{RequestId, stream, Data}} ->
        receive_chunk(TimeoutInSec, RequestId, DataParts ++ [Data], Url, StartTime, Size, Downloaded+byte_size(Data));
	{http,{RequestId, stream_end, _Headers}} ->
	    {ok, binary_to_list(support:binary_join(DataParts))};

	{queryStats, Pid}->
	    Pid!{statsFor, self(), Url, StartTime, Size, Downloaded},
	    receive_chunk(TimeoutInSec, RequestId, DataParts, Url, StartTime, Size, Downloaded);
	{http,_Msg} ->
	    % ignore messages arriving from an old canceled request
	    receive_chunk(TimeoutInSec, RequestId, DataParts, Url, StartTime, Size, Downloaded);
	Msg ->
	    io:format("proccess ~p got an unknown message ~p~n", [self(), Msg]),
	    halt()
    after TimeoutInSec * 1000 ->
	    httpc:cancel_request(RequestId),
	    {error, timeout}
    end.

request(Url, TimeoutInSec)->
    case httpc:request(get, {Url, []}, [{version, "HTTP/1.0"}], [{sync,false},{stream,self}]) of
	{ok, RequestId} ->
	    receive_chunk(TimeoutInSec, RequestId, [], Url, support:timestamp(), notStarted, 0);
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
	    AllUrls = extraction:extract_links(Url, Body),
	    TestFun = Context#context.acceptUrlTest,
            AcceptedUrls = [X || X <- AllUrls, TestFun(X)],
	    Context#context.urlCollectorPid!{add, Url, sets:from_list(AcceptedUrls), self()},
	    receive
		added->ok
	    end;
	failed ->
	    Log(other, "    *** failed to download '~s'", [Url])
    end.

-ifdef(TEST).

simple_test() ->
    ?assert(length([1,2,3]) =:= 3).
-endif.
