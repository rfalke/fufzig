%% @author Seth Falcon
% from http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/

-module(putil).

-export([ensure_list/1, pmap/2, pmap/3, pfilter/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAKE_P_FUN(F, L, Eval, Gather),
        S = self(), 
        Ref = erlang:make_ref(), 
        Pids = lists:map(
                 fun(I) -> 
                         spawn(fun() -> Eval(S, Ref, F, I) end)
                 end, L),
        Gather(Pids, Ref)
       ).

%% @spec ensure_list(L) -> list(any())
%%
%% @doc If L is a list, it is returned unchanged, otherwise a length
%% one list containing L is returned.
ensure_list(L) when is_list(L) ->
    L;
ensure_list(L) ->
    [L].

%% @spec pmap(F, L) -> list(any())
%%
%% @doc A parallelized version of lists:map/2, but each call to F is
%% executed in its own spawned process.
pmap(F, L) ->
    do_pmap(F, L).

%% @spec pmap(F, L, N) -> list(any())
%%
%% @doc A parallelized version of lists:map/2 that executes at most N
%% concurrent processes.  This will perform much better than pmap/2
%% for long lists.
pmap(F, L, N) ->
    pmap(F, L, N, []).

%% @spec pfilter(F, L, N) -> list(any())
%%
%% @doc A parallelized version of lists:filter/2 that executes at most
%% N concurrent processes.
pfilter(P, L, N) ->
    pfilter(P, L, N, []).

%% --- private ----

pmap(_F, [], _N, Acc) ->
    ungather(Acc);
pmap(F, L, N, Acc) ->
    partition_call(F, L, N, Acc, fun pmap/4, fun do_pmap/2).

pfilter(_P, [], _N, Acc) ->
    ungather(Acc);
pfilter(P, L, N, Acc) ->
    partition_call(P, L, N, Acc, fun pfilter/4, fun do_pfilter/2).

partition_call(F, L, N, Acc, Call, Gather) ->
    {L1, Rest} = if
                     N > length(L) -> {L, []};
                     true          -> lists:split(N, L)
                 end,
    Call(F, Rest, N, [Gather(F, L1)|Acc]).

ungather(Acc) ->
    lists:foldl(
      fun(L1, A1) ->
              lists:foldl(fun(L2, A2) -> [L2|A2] end, A1, lists:reverse(L1))
      end, [], Acc).

do_pmap(F, L) ->
    ?MAKE_P_FUN(F, L, do_f, gather).

do_pfilter(P, L) ->
    ?MAKE_P_FUN(P, L, do_p, gather_filt).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

do_p(Parent, Ref, P, I) ->
    Parent ! {self(), Ref, {(catch P(I)), I}}.

gather([Pid|T], Ref) -> 
    receive 
        {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end; 
gather([], _) -> 
    []. 

gather_filt([Pid|T], Ref) ->
    receive 
        {Pid, Ref, {true, Ret}} -> [Ret|gather_filt(T, Ref)];
        {Pid, Ref, {false, _}}  -> gather_filt(T, Ref)
    end;
gather_filt([], _) ->
    [].

%% --- tests ---

-ifdef(TEST).
pmap3_test_() ->
    Tests = [
             {{fun(X) -> 2*X end, [1,2,3,4,5], 3}, [2,4,6,8,10]},
             {{fun(X) -> {x, X} end, [1,2,3], 1}, [{x, 1}, {x, 2}, {x, 3}]},
             {{fun(X) -> [x, X] end, [1,2,3], 1}, [[x, 1], [x, 2], [x, 3]]}
            ],
    [ ?_assertMatch(Want, pmap(F, I, N)) || {{F, I, N}, Want} <- Tests ].

pfilter3_test_() ->
    Tests = [
             {{fun(X) -> X rem 2 == 1 end, [1,2,3,4,5], 3}, [1,3,5]},
             {{fun([V, _X]) -> V =:= a end, [[a, 1], [b, 2], [a, 3]], 2},
              [[a, 1], [a, 3]]}
            ],
    [ ?_assertMatch(Want, pfilter(F, I, N)) || {{F, I, N}, Want} <- Tests ].
-endif.
