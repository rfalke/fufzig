-module(cli_options).

-export([parse_options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("cli_options.hrl").

is_in_subdir(Seedurl, Url) ->
    {ok, SProto, SHost, SPort, SPath} = url:split_full(Seedurl),
    {ok, Proto, Host, Port, Path} = url:split_full(Url),
    SBase = SProto ++ SHost ++ SPort ++ filename:dirname(SPath),
    Base = Proto ++ Host ++ Port ++ filename:dirname(Path),
    lists:prefix(SBase, Base).

is_same_host(Seedurl, Url) ->
    {ok, SProto, SHost, SPort, _SPath} = url:split_full(Seedurl),
    {ok, Proto, Host, Port, _Path} = url:split_full(Url),
    SBase = SProto ++ SHost ++ SPort,
    Base = Proto ++ Host ++ Port,
    SBase == Base.

usage() ->
    io:format("fufzig [-o DIR] [-r|-r+|-r++] URL~n"),
    io:format("  -o   DIR to set the output directory~n"),
    io:format("  -r   limit recursive download to the sub directory of the initial URL~n"),
    io:format("  -r+  limit recursive download to host of the initial URL~n"),
    io:format("  -r++ do not limit the recursive download~n"),
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
                    Fun = case Parsed#options.recurse of
                        none -> fun(_Url) -> false end;
                        subdirs -> fun(Url) -> is_in_subdir(Seedurl, Url) end;
                        sameHost -> fun(Url) -> is_same_host(Seedurl, Url) end;
                        internet -> fun(_Url) -> true end
                    end,
                    Parsed#options{acceptUrlTest = Fun}
            end
    end.

parse_options(Args, Options) ->
    case Args of
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

-ifdef(TEST).

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
