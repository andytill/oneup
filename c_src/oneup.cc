#include <iostream>
#include <atomic>
#include <erl_nif.h>

ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;

ErlNifResourceType* ONEUP_RESOURCE_TYPE;

// holds the value of all oneup counters that have been created
// and not yet garbage collected.
std::atomic<long> num_counters_v;

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

void
free_resource(ErlNifEnv* env, void* obj)
{
    num_counters_v.operator--();
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "oneup";
    const char* name = "oneup";
    ErlNifResourceFlags flags = ErlNifResourceFlags(
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER
    );

    ONEUP_RESOURCE_TYPE = enif_open_resource_type(
        env, mod, name, free_resource, flags, NULL
    );
    if(ONEUP_RESOURCE_TYPE == NULL)
        return -1;

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");

    return 0;
}

static ERL_NIF_TERM
new_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    std::atomic<long> *value;

    value = (std::atomic<long>*)enif_alloc_resource(
        ONEUP_RESOURCE_TYPE, sizeof(std::atomic<long>)
    );
    if(value == NULL)
        return enif_make_badarg(env);
    value->store(0);

    ret = enif_make_resource(env, value);
    enif_release_resource(value);

    num_counters_v.operator++();

    return ret;
}

static ERL_NIF_TERM
inc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;

    if(argc != 1) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    value->operator++();

    return ATOM_OK;
}

static ERL_NIF_TERM
inc2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int inc;

    if(argc != 2) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &inc)) {
        return enif_make_badarg(env);
    }
    value->operator+=((long)inc);

    return ATOM_OK;
}

static ERL_NIF_TERM
inc_and_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;

    if(argc != 1) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }

    return enif_make_long(env, value->operator++());
}

static ERL_NIF_TERM
inc_and_get2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int inc;

    if(argc != 2) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &inc)) {
        return enif_make_badarg(env);
    }

    return enif_make_long(env, value->operator+=((long)inc));
}


static ERL_NIF_TERM
set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int new_value;

    if(argc != 2) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &new_value)) {
        return enif_make_badarg(env);
    }

    return enif_make_long(env, value->exchange((long) new_value));
}

static ERL_NIF_TERM
set_min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int new_value;

    if(argc != 2) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &new_value)) {
        return enif_make_badarg(env);
    }

    while (true) {
        long int current = value->load();
        if (current > new_value) {
            if (value->compare_exchange_strong(current, new_value)) {
                return enif_make_long(env, (long) new_value);
            }
        }
        else {
            return enif_make_long(env, current);
        }
    }

    return enif_make_long(env, (long) value->load());
}

static ERL_NIF_TERM
set_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int new_value;

    if(argc != 2) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &new_value)) {
        return enif_make_badarg(env);
    }

    while (true) {
        long int current = value->load();
        if (current < new_value) {
            if (value->compare_exchange_strong(current, new_value)) {
                return enif_make_long(env, (long) new_value);
            }
        }
        else {
            return enif_make_long(env, current);
        }
    }

    return enif_make_long(env, (long) value->load());
}

static ERL_NIF_TERM
inc_if_less_than(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;
    long int inc, threshold;

    if(argc != 3) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[1], &inc)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_long(env, argv[2], &threshold)) {
        return enif_make_badarg(env);
    }

    // implementation nabbed from the gridgain blog, even better atomic itegers.
    // it uses spin lock style operation to check the value is below the
    // threshold then do a compare-and-swap to make sure it has not changed in
    // between
    // http://gridgain.blogspot.co.uk/2011/06/even-better-atomicinteger-and.html
    while (true) {
        long int current = value->load();
        if (threshold > current) {
            // according to the docs, a weak exchange can spuriously fail
            // http://en.cppreference.com/w/cpp/atomic/atomic/compare_exchange
            if (value->compare_exchange_strong(current, current + inc)) {
               return ATOM_TRUE;
            }
        }
        else {
            return ATOM_FALSE;
        }
    }

    return ATOM_FALSE;
}

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value;

    if(argc != 1) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[0], ONEUP_RESOURCE_TYPE, (void**) &value)) {
        return enif_make_badarg(env);
    }

    return enif_make_long(env, value->load());
}

static ERL_NIF_TERM
is_lock_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> value;
    return value.is_lock_free() ? ATOM_TRUE : ATOM_FALSE;
}

static ERL_NIF_TERM
num_counters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_long(env, num_counters_v.load());
}

static ErlNifFunc nif_funcs[] = {
    {"get", 1, get},
    {"inc", 1, inc},
    {"inc2", 2, inc2},
    {"inc_and_get", 1, inc_and_get},
    {"inc_and_get2", 2, inc_and_get2},
    {"set", 2, set},
    {"set_min",2, set_min},
    {"set_max",2, set_max},
    {"inc_if_less_than", 3, inc_if_less_than},
    {"is_lock_free", 0, is_lock_free},
    {"new_counter", 0, new_counter},
    {"num_counters", 0, num_counters}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(oneup, nif_funcs, &load, NULL, NULL, NULL);

// Or if you don't need reload, upgrade, or unload.
// ERL_NIF_INIT(skeleton, nif_funcs, &load, NULL, NULL, NULL);