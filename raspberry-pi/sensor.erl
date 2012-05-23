-module(sensor).
-export([start/3, stop/0, loop/3]).

start(Id, Pin, PidController) ->
	FdPin = gpio:init(Pin, in),
	Pid = spawn(?MODULE, loop, [Id, FdPin, PidController]),
	Pid.

stop() ->
	%gpio:release(FdPin),
	exit(stopped).

loop(Id, FdPin, PidController) ->
	PinState = gpio:read(FdPin),
	case PinState of
		"1" -> PidController ! {state, Id, 1};
		"0" -> PidController ! {state, Id, 0}
	end,
	timer:sleep(2000),
	loop(Id, FdPin, PidController).

