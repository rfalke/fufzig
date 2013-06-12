%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(url).

-export([split_full/1, remove_anchor/1, make_link_absolute/2, make_full_url/1,is_supported_url/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

is_mailto(Url) -> lists:prefix("mailto:", Url).

is_javascript(Url) -> lists:prefix("javascript:", Url).

is_supported_url(Url) when is_list(Url) ->
    case Url of
        "#" -> false;
        _ -> not(is_mailto(Url) orelse is_javascript(Url) orelse lists:prefix("http:///", Url))
    end.

remove_anchor(Url) when is_list(Url) ->
    case Url == "#" of
        true -> Url;
        false -> [H | _] = string:tokens(Url, "#"),
            H
    end.

rsplit(Input, Char) ->
    Index = string:rchr(Input, Char),
    {string:substr(Input, 1, Index), string:substr(Input, Index + 1)}.

replace_dot_at_end(Url)->
    case lists:reverse(Url) of
	[$.,$/|T] ->
	    lists:reverse(T)++"/DOT";
	_ -> Url
    end.

% http://tools.ietf.org/html/rfc3986#section-5.2.4
removeDotSegments(In, Out)->
    case In of
	[] -> lists:reverse(Out);
	["." | T] -> removeDotSegments(T, Out);
	[".." | T] ->
	    case Out of
		[] -> removeDotSegments(T, Out);
		[_|T2] -> removeDotSegments(T, T2)
	    end;
	[H | T] ->removeDotSegments(T, [H|Out])
    end.

join(List, Sep)->
    case List of
	[] -> "";
	[H1,H2|T]-> H1++Sep++join([H2|T],Sep);
	[H]-> H
    end.

normalize_url(UrlPara) ->
    Url = replace_dot_at_end(UrlPara),
    case split_full(Url) of
	nofullurl -> Url;
	{ok, Proto, Host, Port, Path, Query} ->
	    PathParts=re:split(Path, "/",[{return,list}]),
	    Cleaned = removeDotSegments(PathParts,[]),
	    NewPath=join(Cleaned,"/"),
	    Proto++Host++Port++NewPath++Query
    end.

get_proto(Url) ->
    {match, [Proto]} = re:run(Url, "^(https?:).*\$", [{capture, all_but_first, list}]),
    Proto.

split_full(Url) when is_list(Url) ->
    %io:format("url is ~p ~n", [Url]),
    case re:run(Url, "^(https?://)([a-zA-Z0-9-.]+)(:[0-9]+)?([^?]*)(.*)\$", [{capture, all_but_first, list}]) of
        {match, [Proto, Host, Port, Path0, Query]} ->
            Path = case Path0 of
                [] -> "/";
                _ -> Path0
            end,
            Result = {ok, Proto, Host, Port, Path, Query},
            %io:format("result of url splitting: ~p ~n", [Result]),
            Result;
        nomatch ->
            %io:format("is not a full url~n"),
            nofullurl
    end.

split_url(Url) when is_list(Url) ->
    case split_full(Url) of
        {ok, Proto, Host, Port, AllPath, Query} ->
            {Path, Name} = rsplit(AllPath, $/),
            {full, Proto ++ Host ++ Port, Path, Name++Query};
        nofullurl ->
            case lists:prefix("//", Url) of
                true ->
                    case re:run(Url, "^//[a-zA-Z0-9-.]+(:[0-9]+)?/.*\$", [{capture, all_but_first, list}]) of
                        {match, _} -> {withoutproto, Url};
                        nomatch -> {withoutproto, Url ++ "/"}
                    end;
                false ->
                    case lists:prefix("/", Url) of
                        true -> absolute;
                        false -> relative
                    end
            end
    end.

make_link_absolute(Base, UrlPara) when is_list(Base) and is_list(UrlPara)->
    Url = remove_anchor(UrlPara),
    {full, Host, Path, _} = split_url(Base),
    AbsoluteUrl = case split_url(remove_anchor(Url)) of
        {full, Host2, Path2, Name2} -> Host2 ++ Path2 ++ Name2;
        {withoutproto, Url2} -> get_proto(Base) ++ Url2;
        absolute -> Host ++ Url;
        relative -> Host ++ Path ++ Url
    end,
    normalize_url(AbsoluteUrl).

make_full_url(UrlPara) when is_list(UrlPara) ->
    Url = remove_anchor(UrlPara),
    {match, [Proto, Host, Port, Path]} = re:run(Url, "^([a-z]+://)?([a-zA-Z0-9-.]+)(:[0-9]+)?(.*)\$",
        [{capture, all_but_first, list}]),
    Proto2 = case Proto of
        [] -> "http://";
        _ -> Proto
    end,
    Path2 = case Path of
        [] -> "/";
        _ -> Path
    end,
    Proto2 ++ Host ++ Port ++ Path2.

-ifdef(TEST).
split_full_test_() ->
    [?_assertEqual({ok,"http://", "www.example.com", "", "/abc/dec", ""}, split_full("http://www.example.com/abc/dec")),
     ?_assertEqual({ok,"http://", "www.example.com", "", "/abc/dec", "?foo=bar"}, split_full("http://www.example.com/abc/dec?foo=bar")),
     ?_assertEqual({ok,"http://", "www.example.com", ":123", "/abc/dec", ""}, split_full("http://www.example.com:123/abc/dec")),
     ?_assertEqual({ok,"http://", "www.example.com", "", "/", ""}, split_full("http://www.example.com/")),
     ?_assertEqual({ok,"http://", "www.example.com", "", "/", ""}, split_full("http://www.example.com")),
     ?_assertEqual(nofullurl, split_full("/abc/def")),
     ?_assertEqual(nofullurl, split_full("abc/def"))
    ].

split_url_test_() ->
    [?_assert(split_url("http://www.example.com/abc/dec") =:= {full,"http://www.example.com", "/abc/","dec"}),
     ?_assert(split_url("/abc/dec") =:= absolute),
     ?_assert(split_url("dec") =:= relative)
       ].

make_link_absolute_test_() ->
    Full="http://www.example.com/abc/dec",
    [?_assertEqual("http://www.otherhost/foo", make_link_absolute(Full, "http://www.otherhost/foo")),
     ?_assertEqual("http://www.example.com/foo/bar", make_link_absolute(Full, "/foo/bar")),
     ?_assertEqual("http://www.example.com/abc/foo/bar", make_link_absolute(Full, "foo/bar")),

     ?_assertEqual("http://www.otherhost/foo", make_link_absolute(Full, "http://www.otherhost/foo#xyz")),
     ?_assertEqual("http://www.example.com/foo/bar", make_link_absolute(Full, "/foo/bar#xyz")),
     ?_assertEqual("http://www.example.com/abc/foo/bar", make_link_absolute(Full, "foo/bar#xyz")),

     ?_assertEqual("http://www.example.com/foo/bar", make_link_absolute(Full, "/foo/././bar")),
     ?_assertEqual("http://www.example.com/foo/DOT", make_link_absolute(Full, "/foo/.")),
     ?_assertEqual("http://otherhost/", make_link_absolute(Full, "http://otherhost")),

     ?_assertEqual("http://newhost/some/path", make_link_absolute(Full, "//newhost/some/path")),
     ?_assertEqual("http://newhost/", make_link_absolute(Full, "//newhost")),

     ?_assertEqual("http://www.example.com/a/g?foo=..", make_link_absolute(Full, "/a/b/c/./../../g?foo=..")),
     ?_assertEqual("http://www.example.com/abc/mid/6", make_link_absolute(Full, "mid/content=5/../6"))
    ].

make_full_url_test_() ->
    [?_assertEqual("http://sample.com/", make_full_url("sample.com")),
     ?_assertEqual("http://sample.com:88/", make_full_url("sample.com:88")),
     ?_assertEqual("http://sample.com/", make_full_url("sample.com/")),
     ?_assertEqual("http://sample.com/abc", make_full_url("sample.com/abc")),
     ?_assertEqual("http://sample.com:88/abc", make_full_url("sample.com:88/abc")),

     ?_assertEqual("https://sample.com/", make_full_url("https://sample.com")),
     ?_assertEqual("https://sample.com:88/", make_full_url("https://sample.com:88")),
     ?_assertEqual("https://sample.com/", make_full_url("https://sample.com/")),
     ?_assertEqual("https://sample.com/abc", make_full_url("https://sample.com/abc")),
     ?_assertEqual("https://sample.com:88/abc", make_full_url("https://sample.com:88/abc")),

     ?_assertEqual("https://sample.com/abc", make_full_url("https://sample.com/abc#def"))
    ].

is_supported_url_test_() ->
    [?_assertEqual(false, is_supported_url("#")),
     ?_assertEqual(false, is_supported_url("mailto:abc")),
     ?_assertEqual(false, is_supported_url("javascript:abc")),
     ?_assertEqual(false, is_supported_url("http:///www.regulations.gov/")),

     ?_assertEqual(true, is_supported_url("/abc")),
     ?_assertEqual(true, is_supported_url("http://www.example.com/abc"))
    ].

-endif.
