-module(erlmonads_state).
-compile({parse_transform, erlmonads_expr_do}).

-behaviour(erlmonads).

-export([
    '>>='/2,
    return/1,
    fail/1,
    get/0,
    put/1,
    modify/1,
    eval/2,
    exec/2,
    run/2,
    sequence/1]).

-ifdef(use_specs).
-type(monad(A) :: fun ((S) -> {A, S})).
-include("monad_specs.hrl").
-endif.


'>>='(X, Fun) ->
    fun (S) ->
        {A, S1} = X(S),
        G = Fun(A),
        G(S1)
    end.

return(A) ->
    fun (S) -> {A, S} end.

fail(Str) ->
    fun (_) -> throw({error, Str}) end.

get() ->
    fun (S) -> {S, S} end.

put(S) ->
    fun (_) -> {ok, S} end.

modify(Fun) ->
    fun (S) -> {ok, Fun(S)} end.

eval(Monad, S) ->
    {A, _S1} = Monad(S),
    A.

exec(Monad, S) ->
    {_A, S1} = Monad(S),
    S1.

run(Monad, S) ->
    Monad(S).

sequence(Xs) ->
    erlmonads:sequence(erlmonads_state, Xs).