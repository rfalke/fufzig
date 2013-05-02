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

    NewTodoList = sets:to_list(doOneBatch(Options, Todo, 0, length(Todo), sets:new())),
    NewTodoListFiltered = [X || X <- NewTodoList, not sets:is_element(X, NewDoneSet)],
    NewTodoListSorted = ordsets:to_list(ordsets:from_list(NewTodoListFiltered)),
    case NewTodoListSorted of
        [] -> io:format("Finished downloading~n");
	_ -> driver(Options, BatchNumber+1, NewTodoListSorted, NewDoneSet)
    end.

doOneBatch(Options, Todo, NumDone, NumTotal, AlreadyKnownNewUrlsSet)->
    case Todo of
        [] -> AlreadyKnownNewUrlsSet;
        [Url | Todo2] ->
	    Prefix = lists:flatten(io_lib:format("~B/~B", [NumDone+1, NumTotal])),
            NewUrls0 = handle_one_url(Options, Url, Prefix),
            TestFun = Options#options.acceptUrlTest,
            NewUrls = [X || X <- NewUrls0, TestFun(X) andalso (not sets:is_element(X, AlreadyKnownNewUrlsSet))],
            case NewUrls of
                [_H | _T] -> io:format("    found ~B new urls to crawl: ~p ~n", [length(NewUrls), NewUrls]);
                [] -> ok
            end,
	    AllNewUrls = sets:union(AlreadyKnownNewUrlsSet, sets:from_list(NewUrls)),
            doOneBatch(Options, Todo2, NumDone+1, NumTotal, AllNewUrls)
    end.

format_bytes(N) ->
    support:format_int_with_thousand_separator(N, ",") ++ " bytes".

downloadWithRetry(Url, WhichTry, TotalTries, Prefix) ->
    case WhichTry>TotalTries of
	true -> failed;
	false ->
	    io:format("  ~s downloading '~s' ~B/~B ...", [Prefix, Url, WhichTry, TotalTries]),
	    Start = support:timestamp(),
	    Resp = httpc:request(get, {Url, []}, [{timeout,90 * 1000}], []),
	    Time = support:timestamp() - Start,
	    case Resp of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
		    io:format(" got ~s in ~.1f seconds~n", [format_bytes(length(Body)), Time]),
		    {ok,Body};
		{ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, _Body}} ->
		    io:format(" got ~B after ~.1f seconds~n", [ErrorCode, Time]),
		    case ErrorCode==403 orelse ErrorCode==404 of
			true -> failed;
			false -> downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix)
		    end;
		_ -> io:format(" got ~p after ~.1f seconds~n", [Resp, Time]),
		     downloadWithRetry(Url, WhichTry+1, TotalTries, Prefix)
	    end
    end.

handle_one_url(Options, Url, Prefix) ->
    TotalTries = 5,
    case downloadWithRetry(Url, 1, TotalTries, Prefix) of
	{ok, Body} ->
	    file_support:write_response_to_file(Options#options.basedir, Body, Url),
	    Links = extraction:extract_links(Url, Body),
	    Links;
	failed ->
	    io:format("  *** failed to download '~s' ~n", [Url]),
	    []
    end.

-ifdef(TEST).

simple_test() ->
    ?assert(length([1,2,3]) =:= 3).
-endif.
