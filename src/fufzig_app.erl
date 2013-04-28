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
    driver(Options, [Url], []).

driver(Options, Todo, Done) ->
    io:format("driver: ~B urls done and ~B urls todo~n", [length(Done), length(Todo)]),
    case Todo of
        [] -> io:format("Finished downloading ~n");
        [Url | Todo2] ->
            NewUrls0 = filter_urls(handle_one_url(Options, Url), Todo, Done),
            TestFun = Options#options.acceptUrlTest,
            NewUrls = [X || X <- NewUrls0, TestFun(X)],
            case NewUrls of
                [_H | _T] -> io:format("  found ~B new urls to crawl: ~p ~n", [length(NewUrls), NewUrls]);
                [] -> ok
            end,
            driver(Options, Todo2 ++ NewUrls, [Url | Done])
    end.

filter_urls(NewUrls, Todo, Done) ->
    Known = sets:union(sets:from_list(Todo), sets:from_list(Done)),
    [X || X <- NewUrls, not sets:is_element(X, Known)].

format_bytes(N) ->
    support:format_int_with_thousand_separator(N, ",") ++ " bytes".

downloadWithRetry(Url, WhichTry, TotalTries) ->
    case WhichTry>TotalTries of
	true -> failed;
	false ->
	    io:format("  Downloading '~s' ~B/~B ...", [Url, WhichTry, TotalTries]),
	    Start = support:timestamp(),
	    Resp = httpc:request(get, {Url, []}, [{timeout,10*1000}], []),
	    Time = support:timestamp() - Start,
	    case Resp of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
		    io:format(" got ~s in ~.1f seconds~n", [format_bytes(length(Body)), Time]),
		    {ok,Body};
		{ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, _Body}} ->
		    io:format(" got ~B after ~.1f seconds~n", [ErrorCode, Time]),
		    case ErrorCode==403 orelse ErrorCode==404 of
			true -> failed;
			false -> downloadWithRetry(Url, WhichTry+1, TotalTries)
		    end;
		_ -> io:format(" got ~p after ~.1f seconds~n", [Resp, Time]),
		     downloadWithRetry(Url, WhichTry+1, TotalTries)
	    end
    end.

handle_one_url(Options, Url) ->
    TotalTries = 5,
    case downloadWithRetry(Url, 1, TotalTries) of
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
