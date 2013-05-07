%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(extraction).

-export([extract_links/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

extract_links(Url, Body) ->
    Inner = fun (Pattern) ->
		    case re:run(Body, Pattern, [global, caseless, {capture, all_but_first, list}]) of
		      {match, Offsets} ->
			    lists:append(Offsets);
		      nomatch -> []
		    end
	    end,
    Links = (Inner("(?:src)=\"([^\"]+)\"") ++
		 Inner("(?:href)=\"([^\"]+)\"") ++
		 Inner("(?:background)=\"([^\"]+)\"")),
    AbsoluteLinks = [url:make_link_absolute(Url, url:remove_anchor(X))
		     || X <- Links, url:is_supported_url(X)],
    {GoodLinks, BadLinks} = lists:partition(fun(X) -> not (url:split_full(X)==nofullurl) end, AbsoluteLinks),
    case BadLinks of
	[] -> ok;
	_ -> io:format("Ignore links which are not valid ~p~n", [BadLinks])
    end,
    ordsets:to_list(ordsets:from_list(GoodLinks)).

-ifdef(TEST).
extract_links_test_() ->
    Url="http://www.example.com/abc/dec",
    [?_assertEqual(sets:from_list(["http://www.example.com/abc/link1", "http://www.foobar.com/pixel.gif", "http://www.example.com/background.png"]),
		  sets:from_list(extract_links(Url, "HREF=\"link1\" href=\"mailto:foo@bar.com\" <img src=\"http://www.foobar.com/pixel.gif\" /> <body background=\"/background.png\""))),
     ?_assertEqual(["http://www.example.com/abc/link1"], extract_links(Url, "HREF=\"link1\" href=\"link1\"")),
     ?_assertEqual([], extract_links(Url, "href=\"mailto:foo@bar.com\"")),
     ?_assertEqual([], extract_links(Url, "href=\"javascript:void(0)\"")),
     ?_assertEqual([], extract_links(Url, "href=\"#\"")),
     ?_assertEqual([], extract_links(Url, "some text hr_ef=\"link1\"")),
     ?_assertEqual([], extract_links(Url, "<img src=\"///C:\\Users\\foo\\bar.gif\" />"))
    ].

-endif.
