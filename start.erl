-module(start).

-export([letrip/0]).

letrip()->
    Pid = spawn_link(fun x2/0),
    register(topdog,Pid).


x2()->
    process_flag(trap_exit, true),
    serial:start(),
    erltk:start(),
    serial:subscribe(self(),raw),
    mcp3424:send_file(),

    mcp3424:send_file('cooler.fth'),
    heater:start(),
    pump:start(),
    receive
	quit ->
	    exit(shuterdown);

	{'EXIT', Id, Reason} ->
	    io:format('Top got exit message ~w from ~w~n',
		      [Reason, Id]) 
    end.		
	
    
    
