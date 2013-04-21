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
    io:format("Args: ~p~n",[Args]),
    Options=cli_options:parse_options(Args),
    driver(Options, url:make_full_url(Options#options.seedurl)),
    halt(0).

%% ===================================================================
%% Implementation
%% ===================================================================

driver(Options, Url)->
    driver(Options, [Url], []).

driver(Options, Todo, Done)->
    io:format("driver: ~B urls done and ~B urls todo~n", [length(Done), length(Todo)]),
    case Todo of
	[] ->
	    io:format("Finished downloading ~n");
	[Url|Todo2] ->
	    NewUrls0 = filter_urls(handle_one_url(Options, Url), Todo, Done),
	    TestFun = Options#options.acceptUrlTest,
	    NewUrls = [X || X <- NewUrls0, TestFun(X)],
	    case NewUrls of
		[_H|_T] -> io:format("  found ~B new urls to crawl: ~p ~n", [length(NewUrls), NewUrls]);
		[] -> ok
	    end,
	    driver(Options, Todo2++NewUrls, [Url|Done])
    end.

filter_urls(NewUrls, Todo, Done)->    
    Known = sets:union(sets:from_list(Todo), sets:from_list(Done)),
    [X || X<-NewUrls,not sets:is_element(X, Known)].

timestamp() ->
    {Mega,Sec,Micro} = os:timestamp(),
    (Mega*1000000+Sec)+Micro/1000000.0.

add_thousand_separator(Str, Sep)->
    List=lists_support:n_length_chunks_back(Str,3),
    lists_support:join(List, Sep).

format_bytes(N) ->
    S=integer_to_list(N),
    S2=add_thousand_separator(S, ","),
    S2++" bytes".
  
download(Url)->
    case httpc:request(Url) of
	{ok, {{_Version, 403, _ReasonPhrase}, _Headers, _Body}}-> {httpError,403};
	{ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}}-> {httpError,404};
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}-> {ok,Body}
    end.

handleOk(Options,Url, Body, Time)->
    io:format(" got ~s in ~.1f seconds~n", [format_bytes(length(Body)), Time]),
    file_support:write_response_to_file(Options#options.basedir, Body, Url),
    Links=extraction:extract_links(Url, Body),
    %io:format("Links ~p~n", [Links]),
    Links.

handleHttpError(_Url, Code, Time)->
    io:format(" got ~B after ~.1f seconds~n", [Code, Time]),
    [].

handle_one_url(Options,Url) ->
    io:format("  Downloading '~s' ...", [Url]),
    Start=timestamp(),
    case download(Url) of 
	{ok, Body} -> handleOk(Options,Url, Body, timestamp() - Start);
	{httpError, Code} -> handleHttpError(Url, Code, timestamp() - Start)
    end.

-ifdef(TEST).

simple_test() ->
    ?assert(length([1,2,3]) =:= 3).
-endif.
