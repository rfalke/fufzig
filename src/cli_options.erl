-module(cli_options).

-export([parse_options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("cli_options.hrl").

is_in_subdir(Seedurl, Url) ->
    {ok, SProto, SHost, SPort, SPath, _SQuery} = url:split_full(Seedurl),
    {ok, Proto, Host, Port, Path, _Query} = url:split_full(Url),
    SBase = SProto ++ SHost ++ SPort ++ filename:dirname(SPath),
    Base = Proto ++ Host ++ Port ++ filename:dirname(Path),
    lists:prefix(SBase, Base).

is_same_host(Seedurl, Url) ->
    {ok, SProto, SHost, SPort, _SPath, _SQuery} = url:split_full(Seedurl),
    case url:split_full(Url) of
	{ok, Proto, Host, Port, _Path, _Query} -> 
	    SBase = SProto ++ SHost ++ SPort,
	    Base = Proto ++ Host ++ Port,
	    SBase == Base;
	_ -> error("Have a problem with "++Url)
    end.

usage() ->
    io:format("fufzig [-o DIR] [-r|-r+|-r++] [-i PATTERNS] URL~n"),
    io:format("  -o DIR       set the output directory [defaults to '.']~n"),
    io:format("  -r           limit recursive download to the sub directory of the initial URL~n"),
    io:format("  -r+          limit recursive download to host of the initial URL~n"),
    io:format("  -r++         do not limit the recursive download~n"),
    io:format("  -i PATTERNS  only visit urls which match PATTERNS~n"),
    io:format("~n"),
    io:format("PATTERNS consists of a delimeter char and a list of positive and negative~n"),
    io:format("patterns separated by the delimter char. The url is matched against each~n"),
    io:format("pattern and the first match decided (i.e. the url is accepted or rejected).~n"),
    io:format("Empty parts mean no match.~n"),
    io:format("Example: -i ',/good,/subdir/bad,/subdir'~n"),
    io:format("         means accept urls containing '/good' and '/subdir' but not if they ~n"),
    io:format("         also contain '/subdir/bad'.~n"),
    halt(1).

parse_options(Args) ->
    case Args of
        [] -> usage();
        _ ->
            Parsed = parse_options(Args, #options{}),
            Seedurl = Parsed#options.seedurl,
            case Seedurl of
                [] -> usage();
                _ ->
                    Fun1 = case Parsed#options.recurse of
                        none -> fun(_Url) -> false end;
                        subdirs -> fun(Url) -> is_in_subdir(Seedurl, Url) end;
                        sameHost -> fun(Url) -> is_same_host(Seedurl, Url) end;
                        internet -> fun(_Url) -> true end
                    end,
		    Fun2 = get_pattern_function(Parsed#options.includePattern),
		    Fun = fun(Url) -> Fun1(Url) andalso Fun2(Url) end,
                    Parsed#options{acceptUrlTest = Fun}
            end
    end.

parse_options(Args, Options) ->
    case Args of
        ["-i", X | T] ->
            parse_options(T, Options#options{includePattern = X});
        ["-o", X | T] ->
            parse_options(T, Options#options{basedir = X});
        ["-r" | T] ->
            parse_options(T, Options#options{recurse = subdirs});
        ["-r+" | T] ->
            parse_options(T, Options#options{recurse = sameHost});
        ["-r++" | T] ->
            parse_options(T, Options#options{recurse = internet});
        [H | T] ->
            parse_options(T, Options#options{seedurl = H});
        [] -> Options
    end.

helper(Url, Patterns, Accept)->
    %io:format(user,"Match ~p against ~p ~p~n",[Url, Patterns, Accept]),
    case Patterns of
	[] -> Accept;
	[H|T] -> 
	    Nomatch = (H==[]) orelse (re:run(Url, H) == nomatch),
	    case Nomatch of
		true -> helper(Url,T, not Accept);
		false -> Accept
	    end
    end.
	     
get_pattern_function(PatternsPara)->
    {Sep,Patterns}=lists:split(1,PatternsPara),
    Parts=re:split(Patterns, "\\Q"++Sep++"\\E",[{return,list}]),
    fun(Url) -> helper(Url, Parts,true)
    end.
    
-ifdef(TEST).

evalPattern(Pattern, Sample)->
    Fun = get_pattern_function(Pattern),
    Fun(Sample).

get_pattern_function_test_() ->
    Pat=",^a$,^bc$,^b.*\$",
    [?_assertEqual(true, evalPattern(Pat, "a")),
     ?_assertEqual(false, evalPattern(Pat, "bc")),
     ?_assertEqual(true, evalPattern(Pat, "bx")),
     ?_assertEqual(true, evalPattern(Pat, "b")),

     ?_assertEqual(false, evalPattern(Pat, "x")),
     ?_assertEqual(false, evalPattern(Pat, "aa")),

     ?_assertEqual(false, evalPattern(",", "bb")),
     ?_assertEqual(true, evalPattern(",,", "cc")),

     ?_assertEqual(true, evalPattern(".^a\$.^b\$", "a")),
     ?_assertEqual(false, evalPattern(".^a\$.^b\$", "b")),

     ?_assertEqual(true, evalPattern(",a,bc,b", "a")),
     ?_assertEqual(false, evalPattern(",a,bc,b", "bc")),
     ?_assertEqual(true, evalPattern(",a,bc,b", "xby")),
     ?_assertEqual(false, evalPattern(",a,bc,b", "xy"))
    ].


parse_options_test() ->
    ?_assertEqual(#options{seedurl = "url4", basedir = "dir2"},
        parse_options(["url1", "url2", "-o", "dir1", "url3", "-o", "dir2", "url4"])).

is_in_subdir_test_() ->
    Host="http://www.example.com",
    [?_assertEqual(true, is_in_subdir(Host++"/", Host++"/")),
     ?_assertEqual(true, is_in_subdir(Host++"/", Host++"/abc/def")),
     ?_assertEqual(true, is_in_subdir(Host++"/abc/def", Host++"/abc/def")),
     ?_assertEqual(true, is_in_subdir(Host++"/abc/def", Host++"/abc/def/ghi")),
     ?_assertEqual(false, is_in_subdir(Host++"/abc/def/ghi", Host++"/abc/def")),

     ?_assertEqual(false, is_in_subdir("http://www.example.net/abc", "https://www.example.net/abc")),
     ?_assertEqual(false, is_in_subdir("http://www.example.net:80/abc", "http://www.example.net:8080/abc"))
    ].

is_same_host_test_() ->
    Host="http://www.example.com",
    [?_assertEqual(true, is_same_host(Host++"/", Host++"/")),
     ?_assertEqual(true, is_same_host(Host++"/", Host++"/abc/def")),
     ?_assertEqual(true, is_same_host(Host++"/abc/def", Host++"/abc/def")),
     ?_assertEqual(true, is_same_host(Host++"/abc/def", Host++"/abc/def/ghi")),
     ?_assertEqual(true, is_same_host(Host++"/abc/def/ghi", Host++"/abc/def")),

     ?_assertEqual(false, is_same_host("http://www.example.net/abc", "https://www.example.net/abc")),
     ?_assertEqual(false, is_same_host("http://www.example.net:80/abc", "http://www.example.net:8080/abc"))
    ].

-endif.
