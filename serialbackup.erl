-module(serial).
-compile(export_all).

%%% start up a tail -f erlpipe
%%% CR LF = [13,10]
start()->
    Id = spawn_link(?MODULE, driver1, [self()]),
    register(serial, Id),
    receive
	serial_started ->
	    true
    end.
%wrong place    process_flag(trap_exit,true).

%driver(Starter)->
 %   loop(Port,Starter).

driver1(Starter)->
    process_flag(trap_exit,true),
    Port = open_port({spawn, "./erltty -pipe"},[{packet,2},binary,exit_status]),
    PInfo = erlang:port_info(Port),
    io:format("Port: ~p   PInfo: ~p~n", [Port, PInfo]),
    restart_forth(Port,5),
    Starter ! serial_started,
    io:format('Hw started ~w~n',[Starter]),
    loop1(Port,Starter,<<>>).

% Send Ctrl-C ~ 3 ~ to forth wait 5 second and flush process inbox
% Then call restart_forth wich sends tests the forth for a good response

reset_flush()->
    receive
	_flush -> 
	    reset_flush()
    after 0 ->
	    ok
    end.
   
	      
restart_forth(_Port,0) ->
    io:format('Forth Conection Failed',[]),
    exit('no forthchip');

restart_forth(Port,Tries) ->
     io:format('Restart serial port: ~w~n',[Tries]),
     Port ! {self(), {command, [<<3>>]}},
     timer:sleep(3000),
     reset_flush(),
     Port ! {self(), {command, <<"3 4 + .",10,13>>}},
     RetMsg = get_forth(Port,<<>>),
     case RetMsg == [<<"3 4 + . 7 ok">>] of
	     	true ->
	 	    io:format('Forth started~n');
	 	_Other ->    
	 	    restart_forth(Port,Tries-1)
     end.

% get one complete message only
get_forth(Port,Mesg)->

        receive
	{Port, {data, SerData}} ->
	    case bframe(SerData, Mesg) of

		{complete, CompMesgs}  ->
		    CompMesgs;
		  
		{partial, _PartialMesg, CompMesgs} ->
		    CompMesgs;
		 
		{none, PartialMesg} ->
		    get_forth(Port,PartialMesg)
	    end

	after 2000 ->
		io:format("Trying to establish connection No:~n"),
		exit(noforthconnect)
	end.

	    
close()->
    serial ! close.

send_cmd(Cmd)->
    serial ! {send_cmd,Cmd}.

subscribe(Pid)->
    Ref = erlang:make_ref(),
    ?MODULE ! {self, Ref, {subscribe, Pid}},
    receive
	{Ref, ok} ->
	    true
    after 5000 ->
	    {error, subtimeout}
	      
    end.

forth_mult([]) ->
    monser_mult(1000);

forth_mult([Fth|Rest])->
    send_cmd([Fth,10,13]),
    timer:sleep(2),
    
    monser_mult(50),
    forth_mult(Rest).


forth(Cmd)->    
    send_cmd([Cmd,10,13]),
    monser().

monser()->
    
    receive
	{seriald, Data}->
	 
	    io:format("Fth: ~s~n",[convert:decode_serial(Data)]),
	    monser();

	{_, _Other} ->
	    false,
	    monser()
	
	after 1000   ->
		done
	end.
		  
monser_mult(Time)->
    
    receive
	{seriald, Data}->
	    io:format("Fth: ~s ~n",[convert:decode(Data)]),
	    monser_mult(Time);

	{_, _Other} ->
	    false,
	    monser_mult(Time)
	
	after Time  ->
		done
	end.

%% loop(Port,Starter)->
%%     receive
%% 	{Port, {data, Data}} ->
%% 	    Starter ! {seriald, Data},
%% 	    loop(Port,Starter);
%% 	{send_cmd, Cmd} ->
%% 	    Port ! {self(),{command, Cmd}},
%% 	    loop(Port,Starter);
%% 	close ->
%% 	    Port ! {self(), close},
%% 	    receive
%% 		{Port, closed} ->
%% 		    exit(normal)
%% 	    end;
%% 	{'EXIT', Port, Reason} ->
%% 	    exit({port_terminated,Reason})
%% end.	

		
	  
%- build up byte list only send when cr lf received [10,13|Rest]
% assume message is fragmented.  Build mesg in Mesg
%
loop1(Port,Starter,Mesg)->
    receive
	{Port, {data, SerData}} ->
	    case bframe(SerData, Mesg) of
		{complete, CompMesgs} ->   
		    Starter ! {seriald, CompMesgs},
		    loop1(Port,Starter,<<>>);
		{partial, PartialMesg, CompMesgs} ->
		   %%  io:format("Partial ok ~n"),
		    Starter ! {seriald, CompMesgs},
		    loop1(Port, Starter, PartialMesg);
		{none, PartialMesg} ->
		    loop1(Port, Starter, PartialMesg);
		
		A ->
		    io:format("Exit is ~p.", [A]),
		    loop1(Port,Starter, Mesg)
		    
	    end;

	{Pid,MsgRef,{subscribe,Client,Tags}} ->
	    
	    Pid ! {MsgRef,ok},
	    loop1(Port,Starter,<<>>);
	
		

	{send_cmd, Cmd} ->
	    Port ! {self(),{command, Cmd}},
	    loop1(Port,Starter,Mesg);

	
	    
	close ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("Port exit~n"),
	    exit({port_terminated,Reason})
end.		
		

%%% binary splitting with no cr,lf returns length of 1
%%% binary splitting with cr,lf at the end leaves empty binary list <<>>
%%% binary splitting without an empty binary at the end has incomplete data

bframe(NewBin,OldBin) ->
    SplitBin =  (binary:split(NewBin,<<13,10>>,[global])),
    [Rem|_Rest] = SplitBin,
    RSplitBin = lists:reverse(SplitBin),
    [Left|Termd] = RSplitBin,  %% Leftover, Terminaded
   
  
    Len = length(SplitBin),
    
    if Len == 1 -> 
	    {none,<<OldBin/binary,NewBin/binary>>};
       Left == <<>>, OldBin == <<>> ->
	    {complete,Termd};
       Left == <<>>, OldBin /= <<>> ->
	  %  [TB | Rest]= SplitBin,
	    [_Empty|RTermd] = lists:reverse(Termd),
	    {complete,[<<OldBin/binary,Rem/binary>>|RTermd]};
       true -> %partialy complete
	   % [TB | Resta] = Termd,
	    [_Empty|RTermd] = lists:reverse(Termd),
	    {partial, Left, [<<OldBin/binary,Rem/binary>>|RTermd]}
    end.

send_to_clients([],_Data) ->
    [];

send_to_clients([Client|Clients],Data) ->
    Client ! Data,
    send_to_clients(Clients,Data).

dist_data({Tag,Val},Dict) ->
    case orddict:find(Tag,Dict) of
	{ok,[]} ->
	    skip;
	{ok,Clients} ->
	    send_to_clients(Clients,Val);
	error ->
	    skip
    end.
	


		 
	    




				     
   
    
