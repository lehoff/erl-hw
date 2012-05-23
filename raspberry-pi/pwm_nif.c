/* 
 * PWM NIF support for Erlang on Raspberry Pi
 * Uses the wiringPi library: https://projects.drogon.net/raspberry-pi/wiringpi
*/

#include <erl_nif.h>
#include <wiringPi.h>

extern int value(int val);

static ERL_NIF_TERM
value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

	int val;
	if (!enif_get_int(env, argv[0], &val)) {
		return enif_make_badarg(env);
	}

	if (wiringPiSetup() == -1){
		/* FIXME: return error - can't init wiringPi */
	}

	pinMode(1, PWM_OUTPUT);

	pwmWrite(1, val);

	/* FIXME: return something sensible? */
	return enif_make_int(env, 1);
}


static ErlNifFunc nif_funcs[] = {
    {"value", 1, value_nif},
};

ERL_NIF_INIT(pwm, nif_funcs, NULL, NULL, NULL, NULL)

