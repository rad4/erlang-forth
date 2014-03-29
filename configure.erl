-module(configure).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-record(tag, {tagid,units,desc,type,convfunc,args}).

tags()->
    Tehot = #tag{tagid=tehot,units=degC,desc="Hot Water Brd 1 Chn 1 ",
		 type=mcp3424,args=[8,16,1]},
    Tecld = #tag{tagid=tecold,units=degC,desc="Cold Water Brd 1 Chn 2",
		 type=mcp3424,args=[8,16,2]},
    Tejunc1 = #tag{tagid=junctemp1,units=degC,desc="Brd 1 Junc Temp",
		   type=mcp9800},
    [Tehot,Tecld,Tejunc1].

clients_ord()->
    [{X#tag.tagid,[]} || X <-tags()].

tagdata_ord()->
    [{X#tag.tagid,[]} || X <-tags()].

tags_mcp3424()->
    [ X#tag.tagid || X <- configure:tags(), X#tag.type =:= mcp3424].


configure_test()->
    Z = [{junctemp1,degC,20.5}, {tehot,degC,35.86},
	 {tecold,degC,33.94}, {tex,degC,26.26}],
    G1 = fun(Key,_Val) -> 
		 {_A,{_B,_C,NewVal}} = lists:keysearch(Key,1,Z), NewVal end,
     X = orddict:from_list(configure:tagdata_ord()),
     ?assertEqual(orddict:map(G1,X),[{junctemp1,20.5},{tecold,33.94},
				     {tehot,35.86}]),
    ok.
