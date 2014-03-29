-module(poller).
-compile(export_all).

start(Time,Fun) ->
    Pid = spawn_link(?MODULE, poller, [Time,Fun]),
    register(poller,Pid).

stop() ->
    poller ! pollstop.

poller(Time,Fun) ->
    receive
	pollstop ->
	    done
    after Time ->
	    Fun(),
	    poller(Time,Fun)
    end.

    
    
    
