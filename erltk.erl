-module(erltk).
-compile(export_all).


start()->
    Id = spawn_link(?MODULE, tkdriver, [self()]),
    register(tk, Id).
    

tkdriver(Starter)->
    process_flag(trap_exit,true),
    Tk = open_port({spawn, "wish"},[binary, {line, 255}]),
    io:format('Gui port opened ~w~n',[Starter]),
    {ok, Bin} = file:read_file("erlanglog.tcl"),
    Tk ! {self(), {command, Bin}},
    serial:subscribe(tk,raw),
    loop(Tk).

loop(Tk)->
    receive
	{Tk, {eof,Ret}}->
	    io:format('Tk returned ~s~n',[Ret]),
	    loop(Tk);
	{send, Cmd} ->
	    Tk ! {self(),{command, [Cmd,"\n"]}},
	    loop(Tk);
	{dataman,raw,Data} ->
	    %io:format("Tk: ~p~n",[Data]),
	    log(Data),
	    loop(Tk);
	
	close ->
	    erltk:send("destroy ."),
	    Tk ! {self(), close},
	    exit(normal);
	{'EXIT',Tk,Reason} ->
	    exit({tk_terminated,Reason})
end.
	    


send(Cmd)->
     tk ! {send,Cmd}.

log(Logtext)->
    L = ["writeToLog \"",Logtext,"\""],
    send(L).

%% proc retval {puts stdout "cat dog"}
%% button .b -text "Help" -command retval
%% button .a -text "done" -command retval

%% Tk ! {self(), {command, <<"proc retval {} {puts stdout \"cat dog\"}\n">>}}.

%% Tk ! {self(), {command, <<"button .a -text \"done\" -command retval\n">>}}.
%% Tk ! {self(), {command, <<"button .b -text \"exit\" -command retval\n">>}}.

%% Tk ! {self(), {command, <<"pack .a .b\n">>}}.
