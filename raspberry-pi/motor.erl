-module(motor).
-export([start/1, stop/0, loop/1]).

start({PinDir1, PinDir2}) ->
	Pins = {gpio:init(PinDir1, out), gpio:init(PinDir2, out)},
	Pid = spawn(?MODULE, loop, [Pins]),
	Pid.

stop() ->
	self() ! stop.

loop({PinDir1, PinDir2}) ->
    receive
        M when M == cw;
               M == ccw;
               M == halt ->
            gpio:write_set([PinDir1, PinDir2], pin_settings(M)),
            loop({PinDir1, PinDir2});
        stop ->
            gpio:write_set([PinDir1, PinDir2],
                           pin_settings(stop)),
                                                %gpio:release(PinDir1),
                                                %gpio:release(PinDir2),
            exit(terminated)
	end.
 
pin_settings(cw)   -> [0, 1];
pin_settings(ccw)  -> [1, 0];
pin_settings(halt) -> [0, 0];
pin_settings(stop) -> [0, 0]. 
    
