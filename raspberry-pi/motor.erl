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
		cw ->
			gpio:write_set([PinDir1, PinDir2], [0, 1]),
			loop({PinDir1, PinDir2});

		ccw ->
			gpio:write_set([PinDir1, PinDir2], [1, 0]),
			loop({PinDir1, PinDir2});

		halt ->
			gpio:write_set([PinDir1, PinDir2], [0, 0]),
			loop({PinDir1, PinDir2});

		stop ->
			gpio:write_set([PinDir1, PinDir2], [0, 0]),
			%gpio:release(PinDir1),
			%gpio:release(PinDir2),
			exit(terminated)
	end.
 
