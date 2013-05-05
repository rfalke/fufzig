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
    driver(Options, url:make_full_url(Options#options.seedurl)),
    halt(0).

%% ===================================================================
%% Implementation
%% ===================================================================

driver(Options, Url) ->
    driver(Options, 1, [Url], sets:new()).

driver(Options, BatchNumber, Todo, DoneSet)->
    io:format("driver: start batch ~B with ~B urls (~B urls already done)~n",
	      [BatchNumber, length(Todo), sets:size(DoneSet)]),
    NewDoneSet = sets:union(DoneSet, sets:from_list(Todo)),

    NewTodoList = sets:to_list(
		    case Options#options.parallel>1 of
			true->
			    doOneBatchParallel(Options, Todo, length(Todo));
			false->
			    doOneBatchSequential(Options, Todo, 0, length(Todo), sets:new())
		    end
		    ),
    NewTodoListFiltered = [X || X <- NewTodoList, not sets:is_element(X, NewDoneSet)],
    NewTodoListSorted = ordsets:to_list(ordsets:from_list(NewTodoListFiltered)),
    case NewTodoListSorted of
        [] -> io:format("Finished downloading~n");
	_ -> driver(Options, BatchNumber+1, NewTodoListSorted, NewDoneSet)
    end.

seqLog(Msg, Args)->
    io:format(Msg++"~n", Args).
seqLogStart(Msg, Args)->
    io:format("  "++Msg, Args).
seqLogFinished(Msg, Args)->
    io:format(" "++Msg++"~n", Args).

doOneBatchSequential(Options, Todo, NumDone, NumTotal, AlreadyKnownNewUrlsSet)->
    case Todo of
        [] -> AlreadyKnownNewUrlsSet;
        [Url | Todo2] ->
	    Prefix = lists:flatten(io_lib:format("~B/~B", [NumDone+1, NumTotal])),
            NewUrls0 = handle_one_url(Options, Url, Prefix, fun seqLog/2, fun seqLogStart/2, fun seqLogFinished/2),
            TestFun = Options#options.acceptUrlTest,
            NewUrls = [X || X <- NewUrls0, TestFun(X) andalso (not sets:is_element(X, AlreadyKnownNewUrlsSet))],
            case NewUrls of
                [_H | _T] -> io:format("    found ~B new urls to crawl: ~p ~n", [length(NewUrls), NewUrls]);
                [] -> ok
            end,
	    AllNewUrls = sets:union(AlreadyKnownNewUrlsSet, sets:from_list(NewUrls)),
            doOneBatchSequential(Options, Todo2, NumDone+1, NumTotal, AllNewUrls)
    end.

parallelLog(Msg, Args)->
    io:format("  ~p  "++Msg++"~n", [self()]++Args).
parallelLogStart(Msg, Args)->
    io:format("  ~p  "++Msg++"~n", [self()]++Args).
parallelLogFinished(Msg, Args)->
    io:format("  ~p    "++Msg++"~n", [self()]++Args).

doOneBatchParallel(Options, Todo, NumTotal)->
    Urls = support:add_index(Todo),
    Processes = Options#options.parallel,
    ListOfNewUrls = putil:pmap(
      fun({No,Url}) ->
	      Prefix = lists:flatten(io_lib:format("~B/~B", [No, NumTotal])),
	      NewUrls = handle_one_url(Options, Url, Prefix,
				       fun parallelLog/2, fun parallelLogStart/2, fun parallelLogFinished/2),
	      NewUrls
      end,
      Urls, Processes),
    AllAsSet = lists:foldl(fun(X, Set) -> sets:union(Set, sets:from_list(X)) end, sets:new(), ListOfNewUrls),
    TestFun = Options#options.acceptUrlTest,
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

downloadWithRetry(_Url, WhichTry, TotalTries, _Prefix, _Log, _LogStart, _LogFinished) when WhichTry > TotalTries ->
    failed;
downloadWithRetry(Url, WhichTry, TotalTries, Prefix, Log, LogStart, LogFinished)  ->
    LogStart("~s downloading '~s' ~B/~B ...", [Prefix, Url, WhichTry, TotalTries]),
    Start = support:timestamp(),
    Resp = request(Url, 6),
    Time = support:timestamp() - Start,
    case Resp of
	{ok, Body} ->
	    Bytes = length(Body),
	    LogFinished("got ~s in ~.1f seconds (~s)", [support:format_bytes(Bytes), Time, support:format_rate(Bytes, Time)]),
	    {ok,Body};
	{error, httpError, ErrorCode} ->
	    LogFinished("got ~B after ~.1f seconds", [ErrorCode, Time]),
	    case ErrorCode==403 orelse ErrorCode==404 of
		true -> failed;
		false -> downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix, Log, LogStart, LogFinished)
	    end;
	_ -> LogFinished("got ~p after ~.1f seconds", [Resp, Time]),
	     downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix, Log, LogStart, LogFinished)
    end.

handle_one_url(Options, Url, Prefix, Log, LogStart, LogFinished) ->
    TotalTries = 5,
    case downloadWithRetry(Url, 1, TotalTries, Prefix, Log, LogStart, LogFinished) of
	{ok, Body} ->
	    Fname = file_support:write_response_to_file(Options#options.basedir, Body, Url),
	    Log("    saved to ~s", [Fname]),
	    Links = extraction:extract_links(Url, Body),
	    Links;
	failed ->
	    Log("    *** failed to download '~s'", [Url]),
	    []
    end.

-ifdef(TEST).

simple_test() ->
    ?assert(length([1,2,3]) =:= 3).
-endif.
