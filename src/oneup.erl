-module(oneup).

-export([get/1]).
-export([inc/1]).
-export([inc2/2]).
-export([inc_if_less_than/3]).
-export([is_lock_free/0]).
-export([new_counter/0]).
-export([num_counters/0]).

-ifdef(TEST).
-export([infinite_create_oneup/0]).
-endif.

-on_load(init/0).

-define(APPNAME, oneup).
-define(LIBNAME, oneup).

-type oneup() :: term().

%%% ============================================================================
%%% API
%%% ============================================================================

new_counter() ->
    erlang:nif_error(?LINE).

-spec inc(Oneup::oneup()) -> ok.
inc(_) ->
    erlang:nif_error(?LINE).

-spec inc2(Oneup::oneup(), Increment::integer()) -> ok.
inc2(_,_) ->
    erlang:nif_error(?LINE).

%% Increment this counter by `Inc' if the current value is less than
%% the value of `Threshold'.
%%
%% This function is not pure CAS, since CAS operations do not allow
%% greater and less than checks, at least in cpp stl.   
-spec inc_if_less_than(Oneup::oneup(), 
                       Increment::integer(),
                       Threshold::integer()) -> boolean().
inc_if_less_than(_,_,_) ->
    erlang:nif_error(?LINE).

%%
-spec get(Oneup::oneup()) -> integer().
get(_) ->
    erlang:nif_error(?LINE).

%% Check if the underlying atomic implementation for this platform
%% is actually lock free.
-spec is_lock_free() -> boolean().
is_lock_free() ->
    erlang:nif_error(?LINE).

%% Return the counter for the total number oneup counters, that are active
%% i.e. created and have not yet been garbage collected.
-spec num_counters() -> integer().
num_counters() ->
    erlang:nif_error(?LINE).

-ifdef(TEST).
%% Infinitely create oneup counters. Leave this running to test for counters
%% not being garbage collected causing memory leaks.
infinite_create_oneup() ->
    C = oneup:new_counter(),oneup:inc(C),
    infinite_create_oneup().
-endif.

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

% Since we used init/0 in our -on_load() preprocessor directive, this
% function will get called as the module is loaded. This is the perfect
% place to load up our NIF shared library. Handily, the response of
% erlang:load_nif/2 matches the return specification for -on_load()
% functions.

init() ->
    SoName =
        case priv_path_env() of
            undefined ->
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
                end;
            SoNameFromEnv when is_list(SoNameFromEnv) -> SoNameFromEnv
        end,
    io:format("Loading ~p~n", [SoName]),
    erlang:load_nif(SoName, 0).

priv_path_env()->
  case application:get_env(oneup, priv_path) of
    undefined -> undefined;
    {ok, PrivPath} when is_list(PrivPath) ->
      case filelib:find_file("oneup.so", PrivPath) of
        {ok, SoNameFull} ->
          [SoNamePrefix, _Rest] = string:split(SoNameFull, ".so"),
          SoNamePrefix;
        _NotFound -> undefined
      end;
    {ok, InvalidPrivPath} ->
      io:format("Expected priv_path env as string got ~p~n", [InvalidPrivPath]),
      undefined
  end.


%%% ============================================================================
%%% Benchmarks
%%% ============================================================================

-ifdef(PERF).

-define(ITERATIONS, 100000).
-define(ITERATIONS_QUICK, ?ITERATIONS).

horse_oneup_inc() ->
    Counter = oneup:new_counter(),
    spam_inc(Counter,?ITERATIONS),
    ?ITERATIONS = oneup:get(Counter).

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
    [spawn_link(fun() -> spam_inc_conc(Self,Counter,?ITERATIONS) end) || _ <- Seq],
    [begin receive
        test_done ->
            ok
    end end || _ <- Seq],
    Expected = (?ITERATIONS * Num_procs),
    Expected = oneup:get(Counter).

spam_inc_conc(Self,_,0) ->
    Self ! test_done;
spam_inc_conc(Self,Counter,N) ->
    oneup:inc(Counter),
    spam_inc_conc(Self,Counter,N-1).

horse_oneup_inc_if_less_than() ->
    Counter = oneup:new_counter(),
    spam_inc_if_less_than_conc(self(),Counter,?ITERATIONS,?ITERATIONS),
    receive
        test_done ->
            ok
    end,
    ?ITERATIONS = oneup:get(Counter).

horse_oneup_concurrent_inc_if_less_than() ->
    Self = self(),
    Counter = oneup:new_counter(),
    Num_procs = 8,
    Seq = lists:seq(1, Num_procs),
    Threshold = (?ITERATIONS * Num_procs),
    [spawn_link(
        fun() ->
            spam_inc_if_less_than_conc(Self,Counter,?ITERATIONS,Threshold)
        end) || _ <- Seq],

    [begin receive
        test_done ->
            ok
    end end || _ <- Seq],
    Threshold = oneup:get(Counter).

spam_inc_if_less_than_conc(Self,_,0,_) ->
    Self ! test_done;
spam_inc_if_less_than_conc(Self,Counter,N,Threshold) ->
    oneup:inc_if_less_than(Counter,1,Threshold),
    spam_inc_if_less_than_conc(Self,Counter,N-1,Threshold).

horse_ets_update_counter() ->
    ets:new(tab, [named_table, set]),
    ets:insert(tab, {counter, 0}),
    horse:repeat(?ITERATIONS,
        ets:update_counter(tab, counter, 1)
    ).

horse_ets_update_counter_with_with_write_concurrency_opt() ->
    ets:new(tab, [named_table, set, {write_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    horse:repeat(?ITERATIONS,
        ets:update_counter(tab, counter, 1)
    ).

horse_ets_update_counter_with_with_read_concurrency_opt() ->
    ets:new(tab, [named_table, set, {read_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    horse:repeat(?ITERATIONS,
        ets:update_counter(tab, counter, 1)
    ).

horse_ets_update_counter_with_with_read_write_concurrency_opt() ->
    ets:new(tab, [named_table, set, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    horse:repeat(?ITERATIONS,
        ets:update_counter(tab, counter, 1)
    ).

horse_ets_concurrent_update_counter() ->
    ets:new(tab, [named_table, set, public]),
    ets:insert(tab, {counter, 0}),
    ets_concurrent_update_counter().

horse_ets_concurrent_update_counter_with_write_concurrency_opt() ->
    ets:new(tab, [named_table, set, public, {write_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    ets_concurrent_update_counter().

horse_ets_concurrent_update_counter_with_read_concurrency_opt() ->
    ets:new(tab, [named_table, set, public, {read_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    ets_concurrent_update_counter().

horse_ets_concurrent_update_counter_with_read_write_concurrency_options() ->
    ets:new(tab, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(tab, {counter, 0}),
    ets_concurrent_update_counter().

ets_concurrent_update_counter() ->
    Self = self(),
    Num_procs = 8,
    Seq = lists:seq(1, Num_procs),
    [spawn_link(fun() -> spam_update_counter(Self,?ITERATIONS_QUICK) end) || _ <- Seq],
    [begin receive
        test_done ->
            ok
    end end || _ <- Seq].

spam_update_counter(Self,0) ->
    Self ! test_done;
spam_update_counter(Self,N) ->
    ets:update_counter(tab, counter, 1),
    spam_update_counter(Self,N-1).

horse_ets_update_counter_with_threshold() ->
    ets:new(tab, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(tab, {counter, 0}),

    Self = self(),
    Num_procs = 8,
    Seq = lists:seq(1, Num_procs),
    Threshold = (?ITERATIONS_QUICK * Num_procs),
    [spawn_link(
        fun() ->
            spam_update_counter_with_threshold(Self,?ITERATIONS_QUICK,Threshold)
        end) || _ <- Seq],
    [begin receive
        test_done ->
            ok
    end end || _ <- Seq].

spam_update_counter_with_threshold(Self,0,_) ->
    Self ! test_done;
spam_update_counter_with_threshold(Self,N, Threshold) ->
    ets:update_counter(tab, counter, {2,1,Threshold,Threshold}),
    spam_update_counter_with_threshold(Self,N-1, Threshold).


-endif.