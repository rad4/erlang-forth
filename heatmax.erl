-module(heatmax).
-compile(export_all).

start(StopTemp,Timeout)->
    Id = spawn_link(?MODULE, heatmax_init, [self(),StopTemp,Timeout]).

heatmax_init(Starter,StopTemp,Timeout) ->
    pump:on(),
    timer:sleep(100),
    case pump:status() of
	{ok, on} ->
	    ok;
	_TOther ->
	    exit(pumpnostart)
    end,		
    heater:on(),
    timer:sleep(100),

    case heater:status() of
	{ok, on} ->
	    ok;
	_Other ->
	    exit(heaternostart)
    end,
% subscribe to temp1
    heatmax(Starter,StopTemp,Timeout).

heatmax(Starter,StopTemp,Timeout) ->
    receive
	{dataman,tehot,Temp} ->
	    case Temp of 
		N when N >= StopTemp ->
		    Starter ! {heatmax, tempreach};
		_Other ->
		    heatmax(Starter, StopTemp,Timeout)
	    end;

	quit ->
	    heater:off(),
	    pump:off(),
	    {heatmax,terminated}
	
	after Timeout ->
		Starter ! {heatmax, timeout}
	end.
		

				     
	    
		  
			 
