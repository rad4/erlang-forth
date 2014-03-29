-module(convert).

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
    Pump = #tag{tagid=pump,units=nil,desc="Circulating Pump",type=do_low},
    Heater = #tag{tagid=heater,units=nil,desc="Heater Element",type=do_low},
    [Tehot,Tecld,Tejunc1,Pump,Heater].

clients_ord()->
    [{X#tag.tagid,[]} || X <-tags()].

tagdata_ord()->
    [{X#tag.tagid,[]} || X <-tags()].

tags_mcp3424()->
    [ X#tag.tagid || X <- convert:tags(), X#tag.type =:= mcp3424].

twos_comp(Num,Bits)->
    BitMask = trunc(math:pow(2,Bits)-1),
    Neg = Num bsr (Bits-1),
    case Neg of
	0 ->
	    TwoComp = Num;
	1 ->
	    TwoComp = -((Num bxor BitMask)+1)
	end,

    %io:format('Bit Mask: ~.2B ~nOriginal Num: ~.2B ~n New Num: ~.2B ~n Dec ~.10B~n',
%	      [BitMask,Num,TwoComp,TwoComp]),
    TwoComp.

    
mcp9800_convfunc()->
    %mcp9800 MSB 
    DecRes = 7, FracRes = 4,
    FracCnt = 1/math:pow(2,FracRes),
    BitMask = 2#11111111,

    fun(Dec,Frac) ->
	    Neg = Dec bsr DecRes, 
	    FracD = (Frac bsr 4) * FracCnt,
	    case Neg of
		0 ->
		    Dec + FracD;
		1 ->
		    -(((Dec bxor BitMask) + 1) + FracD)
		end
    end.

mcp3424_convfunc(Gain,BitRes) ->    
    Vref = 2.048,
    LSB = (2 * Vref)/math:pow(2,BitRes)*1000000, % uB
    TypeK = 40.69, % uV/C
%   BitMask = trunc(math:pow(2,BitRes+1)-1),
    
    fun(Count) ->
	    TWCount = twos_comp(Count,BitRes),
	    (TWCount / Gain) * (LSB / TypeK)
		       
	
    end.

decode_serial(ListBins)->
    decode_serial(ListBins,[]).

decode_serial([],DecodeList)->
    %io:format("Decode serial ~w~n",[DecodeList]),
    lists:flatten(DecodeList);

decode_serial([H|Tail],Acc) ->
    case decode(H) of
	{error,_X} ->
	    decode_serial(Tail,Acc);
	Val ->
	    decode_serial(Tail,[Val|Acc])
    end.
	  

decode(<<"data: ",Rest/binary>>)->
    try data_decode(Rest) of
	Val ->
	    Val
    catch	
	error:X ->
	    {error,X}
    end;


decode(<<"cmd: ",Rest/binary>>)->
    cmd_decode(Rest);

decode(Data) ->
    {raw,Data}.
    
data_decode(<<"TE1 ",Args/binary>>)->
    [A,B|_R] = decode_ints(Args),
    {te1int, A, te1bits, B};

data_decode(<<"temp_board1 ",Args/binary>>) ->
    MCP9800 = mcp9800_convfunc(),
    MCP3424 = mcp3424_convfunc(8,16),
    [Fra,Dec,TH,TC,TX|_R] = decode_ints(Args),
    Junc = round(MCP9800(Dec,Fra),2),
    Tehot = round(Junc + MCP3424(TH),2),
    Tecold = round(Junc + MCP3424(TC),2),
    Tx = round(Junc + MCP3424(TX),2),
    [{junctemp1,Junc}, {tehot,Tehot}, {tecold,Tecold}, {tex,Tx}];

data_decode(<<"pumpoff">>)->
    {pump, off};

data_decode(<<"pumpon">>) ->
    {pump, on};

data_decode(<<"heateroff">>) ->
    {heater, off};

data_decode(<<"heateron">>) ->
    {heater, on};
   

data_decode(<<"TE2 ",Args/binary>>)->
    {te2, decode_ints(Args)}.
    
cmd_decode(<<"exit">>) ->
    {command,exit}.

decode_ints(Args) ->
    X=string:tokens(binary_to_list(Args)," ok"), %gets rid of ok at end
    ArgsInt = lists:map(fun erlang:list_to_integer/1, X),
    ArgsInt.

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

decode_test()->
    ?assertEqual(decode(<<"data: TE1 50 60 ok">>),{te1int, 50, te1bits, 60}),
    ?assertEqual(decode(<<"data: TE2 70 80 90">>),{te2,[70,80,90]}),

    JuncFunc = mcp9800_convfunc(),
    ?assertEqual(20.5,JuncFunc(2#00010100,2#10000000)),
    ?assertEqual(20.5,JuncFunc(20,128)),

    TwoC20 = (20 bxor 2#11111111) + 1,
    ?assertEqual(-20.5, JuncFunc(TwoC20,128)),

    TeFunc = mcp3424_convfunc(8,16),
    TwoC6077 = (6077 bxor 2#11111111111111111) + 1,
    ?assertEqual(TeFunc(6077),-TeFunc(TwoC6077)),
    ?assertEqual(decode(<<"data: temp_board1 128 20 80 70 30">>),
			[{junctemp1,20.5}, {tehot,35.86},
			 {tecold,33.94}, {tex,26.26}]),
    Z = [{junctemp1,degC,20.5}, {tehot,degC,35.86},
	 {tecold,degC,33.94}, {tex,degC,26.26}],
    G1 = fun(Key,_Val) -> {_A,{_B,_C,NewVal}} = lists:keysearch(Key,1,Z), NewVal end,
     X = orddict:from_list(convert:tagdata_ord()),
     ?assertEqual(orddict:map(G1,X),[{junctemp1,20.5},{tecold,33.94},
				     {tehot,35.86}]),
   % ?assertEqual(convert:decode_serial([<<"1 3 4">>,<<"data: TE1 50 60 ok">>]),
%		 [{te1int,50,te1bits,60},{raw,<<"1 3 4">>}]),

    X1=convert:decode_serial([<<"data: temp_board1 128 20 80 70 30">>,<<"cat">>]),
    
     X2=[{raw,<<"cat">>},{junctemp1,20.5}, {tehot,35.86}, {tecold,33.94},
	 {tex,26.26}],
 %   ?assertEqual(X1,X2),

    ok.


  
		



    
