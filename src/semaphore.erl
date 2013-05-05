%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2
% Based on http://stackoverflow.com/questions/3685987/erlang-semaphore-mutex

-module(semaphore).

-export([start/1, obtain/1, release/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(Max)->
    spawn(fun() -> run(Max, Max) end).

obtain(SemaPid)->
    SemaPid!{obtain,self()},
    receive
	granted -> ok
    end.

release(SemaPid)->
    SemaPid!{release,self()},
    receive
	granted-> ok
    end.
    
run(0, Max)->
    receive
	{release, Pid} ->
	    Pid!granted,
	    run(1, Max)
    end;
run(Max, Max)->
    receive
	{obtain, Pid} ->
	    Pid!granted,
	    run(Max-1, Max)
    end;
run(Free, Max)->
    receive
	{obtain, Pid} ->
	    Pid!granted,
	    run(Free-1, Max);
	{release, Pid} ->
	    Pid!granted,
	    run(Free+1, Max)
    end.

-ifdef(TEST).

a_test() ->
    Pid=start(2),
    ok=obtain(Pid),
    ok=obtain(Pid),
    ok=release(Pid),
    ok=release(Pid).

-endif.
