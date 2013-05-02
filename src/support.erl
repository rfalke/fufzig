%% @author Raimar Falke
%% @copyright 2012 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(support).

-export([timestamp/0, format_int_with_thousand_separator/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) + Micro / 1000000.0.

helper(List, Sep) ->
    case List of
        [H1, H2, H3, H4 | T] -> [H1, H2, H3] ++ Sep ++ helper([H4 | T], Sep);
        _ -> List
    end.

format_int_with_thousand_separator(N, Sep) ->
    S = integer_to_list(N),
    lists:reverse(helper(lists:reverse(S), Sep)).

-ifdef(TEST).

format_int_with_thousand_separator_test_() ->
    [?_assertEqual("0", format_int_with_thousand_separator(0,",")),
     ?_assertEqual("1", format_int_with_thousand_separator(1,",")),
     ?_assertEqual("12", format_int_with_thousand_separator(12,",")),
     ?_assertEqual("123", format_int_with_thousand_separator(123,",")),
     ?_assertEqual("1,234", format_int_with_thousand_separator(1234,",")),
     ?_assertEqual("12,345", format_int_with_thousand_separator(12345,",")),
     ?_assertEqual("123,456", format_int_with_thousand_separator(123456,",")),
     ?_assertEqual("1,234,567", format_int_with_thousand_separator(1234567,","))
    ].
-endif.
