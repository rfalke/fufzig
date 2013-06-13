%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(putil).

-export([pexec/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

pexec(Fun, List, MaxParallel) ->
    PoolPid = spawn(fun() -> runPexec(Fun, List, MaxParallel, MaxParallel) end),
    PoolPid!{waitForFinish, self()},
    receive
        finished -> ok
    end.

% All slots are used, wait for one free slot
runPexec(Fun, List, MaxParallel, 0) ->
    receive
        childFinished -> runPexec(Fun, List, MaxParallel, 1)
    end;

% No more things to start and no ongoing task
runPexec(_Fun, [], MaxParallel, MaxParallel) ->
    receive
        {waitForFinish,Pid} -> Pid!finished
    end;

% No more things to start but ongoing tasks
runPexec(Fun, [], MaxParallel, Free) ->
    receive
        childFinished -> runPexec(Fun, [], MaxParallel, Free+1)
    end;

% Something to start and at least one free slot
runPexec(Fun, [H|T], MaxParallel, Free) ->
    PoolPid=self(),
    spawn(fun() -> runChild(Fun, H, PoolPid) end),
    runPexec(Fun, T, MaxParallel, Free-1).

runChild(Fun, Arg, PoolPid) ->
    Fun(Arg),
    PoolPid!childFinished.


%% --- tests ---

-ifdef(TEST).

sampleFun({Id, Delay, Pid})->
    receive
    after
	Delay -> Pid!{ok,Id}
    end.

collectIds(Timeout, List)->
    receive
	{ok,Id}->
	    collectIds(Timeout,[Id|List])
    after Timeout->
	    lists:reverse(List)
    end.

pexec_seq_test() ->
    Tests = [{1,1,self()},
	     {2,1,self()},
	     {3,1,self()}],
    pexec(fun sampleFun/1, Tests, 1),
    ?assertEqual([1, 2, 3], collectIds(100,[])).

pexec_parallel_with_2_test() ->
    Tests = [{1,1,self()},
	     {2,20,self()},
	     {3,1,self()}],
    pexec(fun sampleFun/1, Tests, 2),
    ?assertEqual([1, 3, 2], collectIds(100,[])).

pexec_parallel_with_3_test() ->
    Tests = [{1,100,self()},
	     {2,20,self()},
	     {3,1,self()}],
    pexec(fun sampleFun/1, Tests, 3),
    ?assertEqual([3, 2, 1], collectIds(100,[])).

pexec_more_parallel_than_input_size_test() ->
    Tests = [{1,100,self()},
	     {2,20,self()},
	     {3,1,self()}],
    pexec(fun sampleFun/1, Tests, 10),
    ?assertEqual([3, 2, 1], collectIds(100,[])).

-endif.
