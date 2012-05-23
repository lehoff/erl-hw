-module(motor_controller).
-export([start/3, stop/0, loop/3]).

start(MotorLeft, MotorRight, PwmController) ->
	Pid = spawn(?MODULE, loop, [MotorLeft, MotorRight, PwmController]),
	Pid.

stop() ->
	self() ! stop.

loop(MotorLeft, MotorRight, PwmController) ->
	receive
		{speed, Val} ->
			pwm:value(Val);

		forward ->
			MotorLeft ! cw,
			MotorRight ! cw,
			loop(MotorLeft, MotorRight, PwmController);			

		backward ->
			MotorLeft ! ccw,
			MotorRight ! ccw,
			loop(MotorLeft, MotorRight, PwmController);

		left ->
			MotorLeft ! halt,
			MotorRight ! cw,
			timer:sleep(2000),
			self() ! forward,
			loop(MotorLeft, MotorRight, PwmController);

		right ->
			MotorRight ! halt,
			MotorLeft ! cw,
			timer:sleep(2000),
			self() ! forward,
			loop(MotorLeft, MotorRight, PwmController);

		halt ->
			MotorLeft ! halt,
			MotorRight ! halt,
			loop(MotorLeft, MotorRight, PwmController);		

		stop ->
			MotorLeft ! stop,
			MotorRight ! stop,
			PwmController ! stop
	end.

