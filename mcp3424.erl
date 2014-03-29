-module(mcp3424).
-include_lib("eunit/include/eunit.hrl").
-import(lists, [reverse/1,map/2,foreach/2]).
-compile(export_all).

% Describes a 3424 channel
% Tag will be used to identify in a server
-record(tecoup,{tag,name,gain=8,res=16,chan,addr=16#68}).
-record(junctemp,{tag,name,res,addr}).

% MCP3424 has 4 channels, 1 to 4

byte_str(Bin,Base)->
   Int = hd(binary_to_list(Bin)),
   integer_to_list(Int,Base).

i2cstart()->
    I1 = "0 1 I2CPINS",
    I2 = "DECIMAL",
    I3 = "I2CBUS",
    I4 = ": spc .\"  \" ;",
    [I1,I2,I3,I4].

mcp9800_config()->
    I1 = "%0001_0000 CONSTANT config9800",
    I2 = ": !AMBCONFIG I2CSTART $90 I2C! 1 I2C! config9800 I2C! I2CSTOP ; ",
    I3 = ": @AMBCONFIG I2CSTART $90 I2C! 1 I2C! I2CSTOP 
	     I2CSTART $91 I2C! 1 I2C@ I2CSTOP ;",
    I4 = ": @junctemp I2CSTART $90 I2C! 0 I2C! I2CSTOP \\  ( emp - deg frac)
             I2CSTART $91 I2C! 0 I2C@ 1 I2C@ I2CSTOP ; ",

    [I1,I2,I3,I4].
	
mcp9800_convfunc()->
    %mcp9800 MSB 
    DecRes = 7,
    FracRes = 4,
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
    BitMask = trunc(math:pow(2,BitRes+1)-1),
    
    ConvFunc = fun(Count) ->
		       Neg = Count bsr BitRes,
		       case Neg of
			   0 -> (Count / Gain) * (LSB / TypeK) ;
			   1 ->  -(((Count bxor BitMask) + 1) / Gain * (LSB / TypeK))
			   end
	       end,
    ConvFunc.

mcp_test() ->	
    % test mcp9800 conversion functions
    G = mcp9800_convfunc(),
    ?assertEqual(20.5,G(2#00010100,2#10000000)),
    TwoC20 = (20 bxor 2#11111111) + 1,  
    ?assertEqual(20.5,G(20,128)),
    ?assertEqual(-20.5, G(TwoC20,128)),
    %E = G(84,64),
    TwoC84 = (84 bxor 2#11111111) + 1,
    %F = G(TwoC84,64),

    % test mcp3424 conversion functions
    [F3424|_Other] = config_3424chan(8,16,1), 
    %H = F3424(6077),
    TwoC77 = (6077 bxor 2#11111111111111111) + 1,
    %I = F3424(TwoC77),
    %?assertEqual(H,-I),
    ok.
	
% Given the inputs generate list with a function that can convert to temperature
% and a string that can be used to configure the chanel
config_3424chan(Gain,BitRes,Chan)->
    % Function Conversion
    Vref = 2.048,
    LSB = (2 * Vref)/math:pow(2,BitRes)*1000000, % uB
    TypeK = 40.69, % uV/C
    BitMask = trunc(math:pow(2,BitRes+1)-1),
    
    ConvFunc = fun(Count) ->
		       Neg = Count bsr BitRes,
		       case Neg of
			   0 -> (Count / Gain) * (LSB / TypeK) ;
			   1 ->  -(((Count bxor BitMask) + 1) / Gain * (LSB / TypeK))
			   end
		       end,
   
			   
    % Bit configuration
    Bit7 = 1, % Initiate 1 shot conversion
    Chanbits = Chan - 1, 
    Bit4 = 0,  % Conversion mode, 0 = Oneshot

    case BitRes of
	12 -> Res = 2#00;
	14 -> Res = 2#01;
	16 -> Res = 2#10;
	18 -> Res = 2#11
    end,

    case Gain of 
	1 -> Gbits = 2#00; %Gain bits
	2 -> Gbits = 2#01;
	4 -> Gbits = 2#10;
	8 -> Gbits = 2#11
    end,   
    
    ConfigByte = <<Bit7:1,Chanbits:2,Bit4:1,Res:2,Gbits:2>>,
    ConfigInt = hd(binary_to_list(ConfigByte)),
    ConfigStrBin = ["%"|integer_to_list(ConfigInt, 2)],
    %ConfigStrDec = integer_to_list(ConfigInt, 10),
    [ConvFunc,ConfigStrBin,ConfigByte].



% used to descripe chanels on 3424 board #1   
temp_board1() ->
    
    TE1 = #tecoup{tag=tehot,name="Hot Water",gain=8,res=16,chan=1},
    TE2 = #tecoup{tag=tecld,name="Cld Water",gain=8,res=16,chan=2}, 
    TE3 = #tecoup{tag=tex,name="Tex",gain=8,res=16,chan=3},
    [TE1,TE2,TE3].
       
% generate code to read from a 3424 board
% create a line of forth that cnfg the chanel for a read
% create a line of forth that read the chanel this assumes a maximum 
% two byte read good up to 16 bit res
% create a line of forth that combine the two byte read and does the bit shifting

config_chan(TEChan) ->
    [_Func,ByteStr,_Byte] = config_3424chan(TEChan#tecoup.gain,TEChan#tecoup.res,
				   TEChan#tecoup.chan),
    WriteAddr = TEChan#tecoup.addr*2,
    ReadAddr = WriteAddr+1,
    Tag = atom_to_list(TEChan#tecoup.tag),
    C1 = ": CFG",
    C2 = Tag,
    C3 = " I2CSTART $",
    C4 = integer_to_list(WriteAddr,16),
    C5 = " I2C! ",
    C6 = ByteStr,
    C7 = " I2C! I2CSTOP ;",
    C8 = "\n",

    CfgStr = [C1,C2,C3,C4,C5,C6,C7,C8],
    
    R1 = ": READ",
    R2 = Tag,
    R3 = " I2CSTART $",
    R4 = integer_to_list(ReadAddr,16),
    R5 = " I2C! 0 I2C@ 1 I2C@ I2CSTOP ;",
    R6 = "\n",   
    ReadStr = [R1,R2,R3,R4,R5,R6],
    
    G1 = ": GET",
    G2 = Tag,
    G3 = " CFG",
    G4 = R2,
    G5 = " 70 ms READ",
    G6 = R2,
    G7 = " SWAP 8 SHL OR ;",
    G8 = "\n",
    GetStr = [G1,G2,G3,G4,G5,G6,G7,G8],

    F1 = ": @", F2 = G2, F3 = "CR .\" data: ", F4=F2,
    F5 = " \" GET", F6 = G2, F7 = " . CR ; \n\n",
    FetchStr = [F1,F2,F3,F4,F5,F6,F7],

    [CfgStr,ReadStr,GetStr,FetchStr].
	
	
create_file() ->
    {ok,F} = file:open("temp_board1.fth",write),
    Fx = fun(X) -> io:format(F,"~s~n~n",[X]) end,
    I2Csetup = i2cstart(),
    AMBsetup = mcp9800_config(),
    lists:foreach(Fx,I2Csetup),
    lists:foreach(Fx,AMBsetup),

    MCPsetup = lists:map(fun config_chan/1,temp_board1()),
    io:format(F,"~s",[MCPsetup]),
   

    % Create the final string
    % Helper function to get tags from temp_board
    Gx = fun(TEChan)-> atom_to_list(TEChan#tecoup.tag) end,
    % Map over chanels is temp_board
    FetchList = map(Gx,temp_board1()),
    % create a fetch string
    % : getboard1 ." tempboard1 " @ambtemp . ." tehot "  @tehot @ tecld
    GetBoard1Setup = [": getboard1 CR  .\" data: temp_board1 \" @junctemp . spc . spc \t\t"],
    % Write static part to file
    io:format(F,"~s~n",GetBoard1Setup),
    G1x = fun(X) -> io:format(F," GET~s . spc ",[X]) end,
    foreach(G1x,FetchList),
    io:format(F," ~s",["CR  ;"]),
    file:close(F).

send_file() ->
    {ok, Bin} = file:read_file("temp_board1.fth"),
    Bins = binary:split(Bin,<<10>>,[global]),
    serial:forth_mult(Bins).

send_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bins = binary:split(Bin,<<10>>,[global]),
    serial:forth_mult(Bins).
