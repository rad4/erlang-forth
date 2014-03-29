-module(pump).
-compile(export_all).

start()->
    Id = spawn_link(?MODULE, pump_init, [undef]),
    %register(pump,Id)
    register(?MODULE, Id).
    

on()->

    ?MODULE ! {self(),on}.

off() ->
    ?MODULE ! {self(),off}.

status() ->
    ?MODULE ! {self(),status},
    receive
	{pump, status, Status} ->
	    {ok, Status}
    after 500 ->
	{error, nopumpstatus}
    end.	      
	

pump_init(State)->
    serial:subscribe(?MODULE,?MODULE),
    %serial:subscribe(pump,pump),
    pump(State).

pump(State) ->
    receive
	{dataman,pump,Data} ->
	    io:format("Pump got Tag: pump Data: ~p~n",[Data]),
	    pump(Data);
	{Rqstr,status} ->
	    Rqstr ! {?MODULE,status, State},
	     pump(State);
	{Rqstr, on} ->
	    serial:send_cmd(["PUMPON",10,13]),
	    pump(State);
	{Rqstr, off} ->
	    serial:send_cmd(["PUMPOFF",10,13]),
	    pump(State);
	quit  ->
	    pump:off(),
	    exit(pumpdone);

	
	_Other ->
	    pump(State)
     end.
