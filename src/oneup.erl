-module(oneup).

-export([inc/1]).
-export([inc2/2]).
-export([get/1]).
-export([new_counter/0]).

-on_load(init/0).

-define(APPNAME, oneup).
-define(LIBNAME, oneup).

%% API

% NIF functions end up overriding the functions defined in this module. But
% this module must define the functions we want the NIF to implement.
% Theoretically this won't ever get called as out on_load function init/0
% should raise an error if we have issues.
%
% A really nice person would make a pure Erlang fallback incase a NIF was
% unable to load for a specific platform.

new_counter() ->
    not_loaded(?LINE).

inc(_) ->
    not_loaded(?LINE).

inc2(_,_) ->
    not_loaded(?LINE).

get(_) ->
    not_loaded(?LINE).


%% internal functions

% Since we used init/0 in our -on_load() preprocessor directive, this
% function will get called as the module is loaded. This is the perfect
% place to load up our NIF shared library. Handily, the response of
% erlang:load_nif/2 matches the return specification for -on_load()
% functions.

init() ->
    SoName = 
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).


-ifdef(PERF).

horse_oneup_inc() ->
    Counter = oneup:new_counter(),
    spam_inc(Counter,1000000),
    1000000 = oneup:get(Counter).

spam_inc(_,0) ->
    ok;
spam_inc(Counter,N) ->
    oneup:inc(Counter),
    spam_inc(Counter,N-1).

horse_oneup_concurrent_inc() ->
    Self = self(),
    Counter = oneup:new_counter(),
    Num_procs = 8,
    Seq = lists:seq(1, Num_procs),
    [spawn_link(fun() -> spam_inc_conc(Self,Counter,1000000) end) || _ <- Seq],
    [begin receive
        test_done ->
            ok
    end end || _ <- Seq],
    Expected = (1000000 * Num_procs),
    Expected = oneup:get(Counter).

spam_inc_conc(Self,_,0) ->
    Self ! test_done;
spam_inc_conc(Self,Counter,N) ->
    oneup:inc(Counter),
    spam_inc_conc(Self,Counter,N-1).

horse_update_counter() ->
    ets:new(tab, [named_table, set]),
    ets:insert(tab, {counter, 0}),
    horse:repeat(1000000,
        ets:update_counter(tab, counter, 1)
    ).

horse_update_counter_concurrent_10_percent_iterations() ->
    ets:new(tab, [named_table, set, public, {write_concurrency, true}]),
    ets:insert(tab, {counter, 0}),

    Self = self(),
    Num_procs = 8,
    Seq = lists:seq(1, Num_procs),
    % NOTE THAT ETS 
    [spawn_link(fun() -> spam_update_counter(Self,100000) end) || _ <- Seq],
    [begin receive
        test_done ->
            ok
    end end || _ <- Seq].

spam_update_counter(Self,0) ->
    Self ! test_done;
spam_update_counter(Self,N) ->
    ets:update_counter(tab, counter, 1),
    spam_update_counter(Self,N-1).

-endif.