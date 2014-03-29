-module(heater).
-compile(export_all).

start()->
    Id = spawn_link(?MODULE, heater_init, [undef]),
    register(heater, Id).
    

on()->
    heater ! {self(),on}.

off() ->
    heater ! {self(),off}.

status() ->
    heater ! {self(),status},

    receive
	{heater, status, Status} ->
	    {ok, Status}
    after 500 ->
	{error, noheaterstatus}
    end.	      

heater_init(State)->
    serial:subscribe(heater,heater),
    heater(State).

heater(State) ->
    receive
	{dataman,heater,Data} ->
	    io:format("Heater got Tag: heater Data: ~p~n",[Data]),
	    heater(Data);
	{Rqstr,status} ->
	    Rqstr ! {heater,status, State},
	     heater(State);
	{Rqstr, on} ->
	    serial:send_cmd(["HEATERON",10,13]),
	    heater(State);
	{Rqstr, off} ->
	    serial:send_cmd(["HEATEROFF",10,13]),
	    heater(State);
	quit  ->
	    exit(heaterdone);

	
	_Other ->
	    heater(State)
    end.
