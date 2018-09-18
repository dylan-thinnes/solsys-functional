#include <stdio.h>
#include <stdint.h>
#include <msieve.h>
#include <malloc.h>
#include <string.h>

void get_random_seeds(uint32 *seed1, uint32 *seed2) {

	uint32 tmp_seed1, tmp_seed2;

	/* In a multithreaded program, every msieve object
	   should have two unique, non-correlated seeds
	   chosen for it */

#if !defined(WIN32) && !defined(_WIN64)

	FILE *rand_device = fopen("/dev/urandom", "r");

	if (rand_device != NULL) {

		/* Yay! Cryptographic-quality nondeterministic randomness! */

		fread(&tmp_seed1, sizeof(uint32), (size_t)1, rand_device);
		fread(&tmp_seed2, sizeof(uint32), (size_t)1, rand_device);
		fclose(rand_device);
	}
	else

#endif
	{
		/* <Shrug> For everyone else, sample the current time,
		   the high-res timer (hopefully not correlated to the
		   current time), and the process ID. Multithreaded
		   applications should fold in the thread ID too */

		uint64 high_res_time = read_clock();
		tmp_seed1 = ((uint32)(high_res_time >> 32) ^
			     (uint32)time(NULL)) * 
			    (uint32)getpid();
		tmp_seed2 = (uint32)high_res_time;
	}

	/* The final seeds are the result of a multiplicative
	   hash of the initial seeds */

	(*seed1) = tmp_seed1 * ((uint32)40499 * 65543);
	(*seed2) = tmp_seed2 * ((uint32)40499 * 65543);
}

char * factor_integer(char *buf) {
	
	msieve_obj *obj;
	msieve_obj *g_curr_factorization;
	msieve_factor *factor;

    uint32 seed1, seed2;
    get_random_seeds(&seed1, &seed2);
    enum cpu_type cpu = get_cpu_type();

    uint32 cache_size1, cache_size2;
    get_cache_sizes(&cache_size1, &cache_size2);

	g_curr_factorization = msieve_obj_new(buf, 0,
					NULL, NULL,
					NULL,
					seed1, seed2, 0,
					cpu, cache_size1, cache_size2,
					0, 0,
                    NULL);
	if (g_curr_factorization == NULL) {
		printf("factoring initialization failed\n");
		return;
	}

	msieve_run(g_curr_factorization);

	if (!(g_curr_factorization->flags & MSIEVE_FLAG_FACTORIZATION_DONE)) {
		printf("\ncurrent factorization was interrupted\n");
		exit(0);
	}

    factor = g_curr_factorization->factors;
    char * factors_list = "[";
    while (factor != NULL) {
        char * number = factor->number;
        char * new_factors_list = malloc(strlen(factors_list) + strlen(number) + 2);
        new_factors_list[0] = '\0';
        strcat(new_factors_list, factors_list);
        strcat(new_factors_list, number);

        if (factor->next != NULL) strcat(new_factors_list, ",");
        //if (factors_list != NULL) free(factors_list);
        factors_list = new_factors_list;

        factor = factor->next;
    }
    strcat(factors_list, "]");

	/* free the current factorization struct. The following
	   avoids a race condition in the signal handler */

	obj = g_curr_factorization;
	g_curr_factorization = NULL;
	if (obj)
		msieve_obj_free(obj);

    return factors_list;
}
