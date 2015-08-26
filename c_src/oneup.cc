#include <iostream>
#include <atomic>
#include <erl_nif.h>

ERL_NIF_TERM ATOM_OK;

ErlNifResourceType* ONEUP_RESOURCE_TYPE;

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

    return 0;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

static ERL_NIF_TERM
new_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
/*    std::atomic<long> *value = new std::atomic<long>;
*/
    std::atomic<long> *value;

    value = (std::atomic<long>*)enif_alloc_resource(
        ONEUP_RESOURCE_TYPE, sizeof(std::atomic<long>)
    );
    if(value == NULL)
        return enif_make_badarg(env);
    value->store(0);

    ret = enif_make_resource(env, value);
    enif_release_resource(value);

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

static ErlNifFunc nif_funcs[] = {
    {"get", 1, get},
    {"inc", 1, inc},
    {"new_counter", 0, new_counter}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(oneup, nif_funcs, &load, NULL, NULL, NULL);

// Or if you don't need reload, upgrade, or unload.
// ERL_NIF_INIT(skeleton, nif_funcs, &load, NULL, NULL, NULL);