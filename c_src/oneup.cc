#include <iostream>
#include <atomic>
#include <erl_nif.h>

ERL_NIF_TERM ATOM_OK;

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ATOM_OK = enif_make_atom(env, "ok");

    return 0;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

static ERL_NIF_TERM
new_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::atomic<long> *value = new std::atomic<long>;
    value->store(0);
    long pointer = reinterpret_cast<long>(value);

    return enif_make_long(env, pointer);
}

static ERL_NIF_TERM
inc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long pointer;
    if (!enif_get_long(env, argv[0], &pointer)) {
        return enif_make_badarg(env);
    }

    std::atomic<long> *value = (std::atomic<long>*)pointer;
    value->operator++();

    return ATOM_OK;
}

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long pointer;
    if (!enif_get_long(env, argv[0], &pointer)) {
        return enif_make_badarg(env);
    }

    std::atomic<long> *value = (std::atomic<long>*)pointer;

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