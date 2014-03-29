-module(heater_fsm).
-compile(export_all).

start()->
    Id = spawn_link(?MODULE, init, [undef]),
    register(?MODULE, Id).
    

on()->
    ?MODULE ! {self(),on}.

off() ->
    ?MODULE ! {self(),off}.

status() ->
    ?MODULE ! {self(),status},

    receive
	{?MODULE, status, Status} ->
	    {ok, Status}
    after 500 ->
	{error, noheaterstatus}
    end.	      

init(State)->
    serial:subscribe(?MODULE,heater),
    idle(State).

idle(State) ->
    receive
	{dataman,heater,Data} ->
	    io:format("Heater got Tag: heater Data: ~p~n",[Data]),
	    heater(Data);
	{Rqstr,status} ->
	    Rqstr ! {?MODULE,status, State},
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
