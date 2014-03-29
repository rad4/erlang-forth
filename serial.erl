-module(serial).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

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
    Port = open_port({spawn, "./erltty -baud 230400 -pipe"},[{packet,2},binary,exit_status]),
    PInfo = erlang:port_info(Port),
    io:format("Port: ~p   PInfo: ~p~n", [Port, PInfo]),
    restart_forth(Port,5),
    Starter ! serial_started,
    io:format('Hw started ~w~n',[Starter]),
    Clients1 = orddict:from_list(convert:clients_ord()),
    Clients = orddict:store(raw,[],Clients1),
    Port ! {self(), {command, <<"0 1 I2CPINS DECIMAL",10,13>>}},
    loop1(Port,Clients,<<>>).

% Send Ctrl-C  <<3>>  to forth wait 5 second and flush process inbox
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
    exit(noforthchip);

restart_forth(Port,Tries) ->
     io:format('Restart serial port: ~w~n',[Tries]),
     Port ! {self(), {command, [<<3>>]}},
     timer:sleep(3000),
     reset_flush(),
     Port ! {self(), {command, <<"3 4 + .",13,10>>}},
     RetMsg = get_forth(Port,<<>>),
     case RetMsg == [<<"3 4 + . 7 ok">>] of
	     	true ->
	 	    io:format('Forth started~n');
	 	_Other ->    
	            io:format('Returned binary~s~n',[RetMsg]),	     
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

subscribe(Pid,Tag)->
    Ref = erlang:make_ref(),
    ?MODULE ! {self(), Ref, {subscribe, Pid,Tag}},
    receive
	{Ref, ok} ->
	    true
    after 5000 ->
	    {error, subtimeout}
	      
    end.

subscribers()->
    Ref = erlang:make_ref(),
    ?MODULE ! {self(), Ref, subscribers},
    receive
	{Clients,Ref, ok} ->
	    Clients
    after 5000 ->
	    {error, subtimeout}
	      
    end.

forth_mult([]) ->
    monser_mult(1000);

forth_mult([Fth|Rest])->
    send_cmd([Fth,10,13]),
    timer:sleep(20),
    
    monser_mult(50),
    forth_mult(Rest).


forth(Cmd)->    
    send_cmd([Cmd,10,13]),
    monser().

monser()->
    
    receive
	
	{dataman,raw, Data} ->
	    io:format("Forth: ~s~n",[Data]),
	    monser();
	
	{dataman,Tag,Data} ->
	    io:format("Decoded data -> Tag: ~p Data: ~p~n",[Tag,Data]),
	    monser();

	_Other ->
	    
	    monser()
	
	after 1000   ->
		done
	end.
		  
monser_mult(Time)->
        
    receive
	
	{dataman,raw, Data} ->
	    io:format("Forth: ~s~n",[Data]),
	    monser_mult(Time);
	
	{dataman,Tag,Data} ->
	    io:format("Decoded data -> Tag: ~p Data: ~p~n",[Tag,Data]),
	    monser_mult(Time);

	_Other ->
	    
	    monser_mult(Time)
	
	after Time   ->
		done
	end.

		
	  
%- build up byte list only send when cr lf received [10,13|Rest]
% assume message is fragmented.  Build mesg in Mesg
%
loop1(Port,Clients,Mesg)->
    receive
	{Port, {data, SerData}} ->
	    DistFunc = fun(X) ->
			       dist_data(X,Clients)
			       end,
				   
	    case bframe(SerData, Mesg) of
		{complete, CompMesgs} -> 
		    DecodedSerial = convert:decode_serial(CompMesgs),
		    lists:map(DistFunc,DecodedSerial),
		    loop1(Port,Clients,<<>>);

		{partial, PartialMesg, CompMesgs} ->
		    DecodedSerial = convert:decode_serial(CompMesgs),
		    lists:map(DistFunc,DecodedSerial),
		    loop1(Port, Clients, PartialMesg);

		{none, PartialMesg} ->
		    loop1(Port, Clients, PartialMesg);
		
		A ->
		    io:format("Unknown message from Port ~p.", [A]),
		    loop1(Port,Clients, Mesg)
		    
	    end;

	{Pid,MsgRef,{subscribe,Client,Tag}} ->
	    NewClients = orddict:append(Tag,Client,Clients),
	    link(Pid),
	    Pid ! {MsgRef,ok},
	    loop1(Port,NewClients,Mesg);
	
	{Pid,MsgRef,subscribers} ->
	    Pid ! {Clients,MsgRef, ok},
	    loop1(Port,Clients,Mesg);

	{send_cmd, Cmd} ->
	    Port ! {self(),{command, Cmd}},
	    loop1(Port,Clients,Mesg);

        die ->
	    exit(crapped);
	    
	close ->
	    Port ! {self(), close},
	    loop1(Port,Clients,Mesg);

	{Port, closed} ->
		    exit({portdown,closed});

	{'EXIT', Port, Reason} ->
	    io:format("Port exit~n"),
	    exit({portdown,Reason}); % exit/1 will send error signals to linked.

	{'EXIT', _Pid, shuterdown} ->
	    io:format('System shudown~n'),
	    exit(normal);

	{'EXIT', Id, Reason} ->
	    io:format('Serial got exit message ~w from ~w~n',
		      [Reason, Id]), 
	    loop1(Port,Clients,Mesg)
	

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

send_to_clients([],_Tag,_Data) ->
    [];

send_to_clients([Client|Clients],Tag,Val) ->
    Client ! {dataman,Tag,Val},
    %io:format("Send client: ~w data: ~w~n",[Client,{dataman,Tag,Val}]),
    send_to_clients(Clients,Tag,Val).

dist_data({Tag,Val},Dict) ->
    case orddict:find(Tag,Dict) of
	{ok,[]} ->
	    skip;
	{ok,Clients} ->
	    send_to_clients(Clients,Tag,Val);
	error ->
	    skip
    end.
	
write_to_clients([],_Data) ->
    [];

write_to_clients([Client|Clients],Data) ->
    io:format("Test write to client Client:~w Data:~w~n",[Client,Data]),
    write_to_clients(Clients,Data).


write_data({Tag,Val},Dict) ->
    case orddict:find(Tag,Dict) of
	{ok,[]} ->
	    skip;
	{ok,Clients} ->
	    write_to_clients(Clients,Val);
	error ->
	    skip
    end.		 
	    
serial_test()->
    G3 = orddict:new(),
    G = orddict:store(tehot,[],G3),
    V = [{tehot,5},{junctemp1,63}],
    G1 = orddict:append(tehot,rat,G),
    G2 = orddict:append(tehot,cat,G1),
    G4 = orddict:store(junctemp1,[cat],G2),
    Gx = G4,
 %   write_data(hd(V),G2),
    FuncG = fun(X) -> write_data(X,Gx) end,
    lists:map(FuncG,V),
		      
    ok.


    
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

    


				     
   
    
