-module(driver_fth).
-compile(export_all).

%% char 47 is the escape characte
%% 45 is start, 46 is end.

%%% Packet definition

% 45 | Packet Length max 255 | Command max 255 | Data Bytes ... | 47
    
%% Add byte 45 to start of byte, 46 to end.
%% if data stream has 45,46,47 it is xored with 32 and escape byte
%% 47 is inserted befor xored byte.  

bytestuff(Data) ->
    bytestuff(Data,[]).

bytestuff([],Acc) ->
    Len = length(Acc),
    [45,Len|lists:reverse([46|Acc])]; %% 45 start byte 46 stop byte
    
bytestuff([H|T],Acc) when H >= 45, H =< 47 ->
    N = H bxor 16#20,
    bytestuff(T,[N | [47 | Acc]]);

bytestuff([H|T],Acc) ->
    bytestuff(T, [H|Acc]).

%%% -------

byteunstuff(Stuffed) ->
    byteunstuff(Stuffed,[]).

byteunstuff([],Unstuffed) ->
    lists:reverse(Unstuffed);

byteunstuff([Esc,Val|T],Unstuffed) when Esc == 47 ->
    A = Val bxor 16#20,
    byteunstuff(T, [A|Unstuffed]);
		    
byteunstuff([Val|T],Unstuffed) ->
    byteunstuff(T,[Val|Unstuffed]).



	

%%    io:format("~8.16.0B~n", [42]).
%%    0000002A
%% basically, it's ~F.P.Pad where:

%    F = field width
%    P = base
%    Pad = pad character

bin_disp(Bytes) ->
    io:format("~8.2.0B~n", Bytes).
    
%% helper to understand xor encoding    
xor_encode_disp(Byte) ->  
    A = Byte bxor 16#20,
    B = A bxor 16#20,
    bin_disp([Byte]),
    bin_disp([16#20]),
    bin_disp([A]),
    bin_disp([16#20]),
    bin_disp([B]).

     
encode({sp1,A}) ->
    B = <<A:16>>,
    C = binary_to_list(B),
    bytestuff([0|C]).


forth(A) ->
    [A,10,13].

