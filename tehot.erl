-module(tehot).
-compile(export_all).

start() ->
    Pid = spawn(?MODULE,loop,[none,[],idle]),
    register(tehot,Pid),
    serial:subscribe(Pid,tehot),
    serial:subscribe(Pid,junctemp1).

stop() ->
    tehot ! close.


loop(Current,Last8,State) ->
    receive
	{dataman,tehot,Val} ->
	    NewLast8 = append8(Val,Last8),
	    Avg = NewLast8 / length(Last8),
	    loop(Val,NewLast8,State);
	_Other ->
	    loop(Current,Last8,State)
		end.
	    
    
append8(Val,List) ->
    case length(List) of
	8 ->
	   [_H|Tail] = lists:reverse(List),
	   [Val|lists:reverse(Tail)];
	_Oth ->
	    [Val|List]
	end.
	    
    
