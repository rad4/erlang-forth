-module(digio).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

start({Tag,Pin})->
    Id = spawn_link(?MODULE, loop, [Tag,Pin]),
    register(Tag, Id).

on(Tag)->
    Tag ! {self(),on}.

off(Tag)->
    Tag ! {self(),off}.

done(Tag)->
    Tag ! done.

loop(Tag,Pin) ->
    receive 
	{Pid,on} ->
	    io:format("Turn on ~w~n",[Pin]),
	    Pid ! {Tag,on},
	    loop(Tag,Pin);
	{Pid,off} ->
	    io:format("Turn off ~w~n",[Pin]),
	    Pid ! {Tag,off},
	    loop(Tag,Pin);
	done ->
	    done
    end.	       
	    
