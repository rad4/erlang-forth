-module(rrdtool).
-compile(export_all).


epoch()->
    UnixEpoch={{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds(UnixEpoch).

now()->
    Now = calendar:local_time(),
    NowGregSecs = calendar:datetime_to_gregorian_seconds(Now),
    NowGregSecs - epoch().
    
