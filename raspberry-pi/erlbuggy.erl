-module(erlbuggy).
-export([start/2, stop/0, loop/2, navigate/2]).

start(PinSensors, PinMotors) ->
	{PinSensorL, PinSensorR, PinSensorT} = PinSensors,
	{{PinMotorL1, PinMotorL2}, {PinMotorR1, PinMotorR2}} = PinMotors,

	MotorL = motor:start({PinMotorL1, PinMotorL2}),
	MotorR = motor:start({PinMotorR1, PinMotorR2}),
	MotorController = motor_controller:start(MotorL, MotorR, not_implemented),

	Pid = spawn(?MODULE, loop, [{0, 1, 0}, MotorController]),

	sensor:start("left", PinSensorL, Pid),
	sensor:start("right", PinSensorR, Pid),
	sensor:start("top", PinSensorT, Pid),

	Pid.

stop() ->
	ok.

loop(State, MotorController) ->
	receive
		{state, "left", CurL} ->
			{_, CurR, CurT} = State,
			NewState = {CurL, CurR, CurT},
			navigate(NewState, MotorController),
			loop(NewState, MotorController);
		{state, "right", CurR} ->
			{CurL, _, CurT} = State,
			NewState = {CurL, CurR, CurT},
			navigate(NewState, MotorController),
			loop(NewState, MotorController);
		{state, "top", CurT} ->
			{CurL, CurR, _} = State,
			NewState = {CurL, CurR, CurT},
			navigate(NewState, MotorController),
			loop(NewState, MotorController)
	end.

navigate(State, MotorController) ->
    %%io:format("L:~s T:~s R:~s~n", tuple_to_list(State)),
    MotorController ! state_direction(State).

-type state_direction({0|1, 0|1, 0|1}) -> 'left' | 'right' | 'forward'.
state_direction({0, _, 1}) -> right;
state_direction(State) when State == {0, 1, 0};
                            State == {1, 1, 1} -> forward;
state_direction(_) -> left.
    

