-module(thermocouple).
-compile(export_all).

byte_str(Bin,Base)->
   Int = hd(binary_to_list(Bin)),
   integer_to_list(Int,Base).

config9800()->
    C1 = "%0001_0000 CONSTANT config9800",
    C2 = ": @AMBCONFIG I2CSTART $90 I2C! 1 I2C! I2CSTOP
	     I2CSTART $91 I2C! 1 I2C@ I2CSTOP ;",
    C3 = ": !AMBCONFIG I2CSTART $90 I2C! 1 I2C! config9800 I2C! I2CSTOP ;",
    C4 = ": @AMBTEMP I2CSTART $90 I2C! 0 I2C! I2CSTOP 
	   I2CSTART $91 I2C! 0 I2C@ 1 I2C@ I2CSTOP ;",  % \  ( emp - deg frac)
    C5 = ": @AMBTEMP12BIT @AMBTEMP SWAP 4 SHL SWAP 4 SHR OR ;",
    [C1,C2,C3,C4,C5].
       
