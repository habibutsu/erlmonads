%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc; Eduard Sergeev.
%% All rights reserved.
%%

-module(erlmonads_expr_do_test).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, erlmonads_expr_do}).

-export([
    maybe/1,
    fib_m/1,
    fib_m_step/1,
    fib_rec/1,
    fib_rec/3
]).

sequence_test() ->
    List = lists:seq(1,5),
    ListM = [do([erlmonads_maybe || return(N)]) || N <- List],
    {just, List} = erlmonads:sequence(erlmonads_maybe, ListM).

join_test() ->
    {just, 5} = erlmonads:join(
        erlmonads_maybe,
        erlmonads_maybe:return(erlmonads_maybe:return(5))),
    {just, 5} = erlmonads:join(
        erlmonads_maybe,
        do([erlmonads_maybe || return(erlmonads_maybe:return(5))])),
    {just, 5} = erlmonads:join(
        erlmonads_maybe,
        do([erlmonads_maybe || return(do([erlmonads_maybe || return(5)]))])).

maybe_test() ->
    nothing = maybe(atom),
    {just, 9} = maybe(3).

maybe(Arg) ->
    do([erlmonads_maybe
        || erlmonads_plus:guard(erlmonads_maybe, is_number(Arg)),
           return(Arg*Arg)]).

fib_test() ->
    true = lists:all(fun ({X, Y}) -> X =:= Y end,
                     [{fib_m(N), fib_rec(N)} || N <- lists:seq(0, 20)]).

%% Classic monadic implementation of fibonnaci
fib_m(N) ->
    StateT = erlmonads_state_t:new(erlmonads_identity),
    {_, R} = StateT:exec(
        erlmonads:sequence(
            StateT,
            lists:duplicate(N, fib_m_step(StateT))), {0, 1}),
    R.

fib_m_step(StateT) -> StateT:modify(fun ({X, Y}) -> {Y, X+Y} end).

%% Classic recursive implementation of fibonnaci
fib_rec(N) when N >= 0 -> fib_rec(N, 0, 1).
fib_rec(0, _X, Y) -> Y;
fib_rec(N,  X, Y) -> fib_rec(N-1, Y, X+Y).

list_test() ->
    %% Demonstrate equivalence of list comprehensions and list monad
    A = [{X,Y} || X <- "abcd",
                  Y <- [1,2]],
    A = do([erlmonads_list || X <- "abcd",
                      Y <- [1,2],
                      return({X,Y})]),
    %% Classic pythagorean triples
    P = [{X, Y, Z} || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)],
    P = do([erlmonads_list || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      erlmonads_plus:guard(
                        erlmonads_list, math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)),
                      return({X,Y,Z})]).

omega_test() ->
    A = [{X,Y,Z} || X <- "abcd",
                    Y <- lists:seq(1,5),
                    Z <- lists:seq(11,15)],
    B = do([erlmonads_omega || X <- "abcd",
                       Y <- lists:seq(1,5),
                       Z <- lists:seq(11,15),
                       return({X,Y,Z})]),
    ?assert(A =/= B),
    ?assert(A =:= lists:usort(B)).

error_t_list_test() ->
    M = erlmonads_error_t:new(erlmonads_list),
    R = M:run(do([M || E1 <- M:lift([1, 2, 3]),
                       E2 <- M:lift([4, 5, 6]),
                       case (E1 * E2) rem 2 of
                           0 -> return({E1, E2});
                           _ -> fail(not_even_product)
                       end])),
    R = [{ok, {1, 4}}, {error, not_even_product}, {ok, {1, 6}},
         {ok, {2, 4}}, {ok, {2, 5}},              {ok, {2, 6}},
         {ok, {3, 4}}, {error, not_even_product}, {ok, {3, 6}}],

    %% Compare with the non-error_t version, which will remove failures:
    S = do([erlmonads_list || E1 <- [1, 2, 3],
                      E2 <- [4, 5, 6],
                      case (E1 * E2) rem 2 of
                          0 -> return({E1, E2});
                          _ -> fail(not_even_product)
                      end]),
    S = [{1, 4}, {1, 6}, {2, 4}, {2, 5}, {2, 6}, {3, 4}, {3, 6}].

%% Tests for 'let-match binding' (a-la 'let' in Haskell's 'do'
%% expression) But instead of 'let' here we use 'match' (=) expression
%% in 'do([])':
let_match_test() ->
    T1 = do([erlmonads_maybe || R <- return(2),
                        R2 = R*R,
                        return(R2*R2)]),
    ?assertEqual({just, 16}, T1),
    T1 = do([erlmonads_maybe || R <- return(2),
                        return(R*R*R*R)]),
    ?assertEqual({just, 16}, T1),

    %% Failure test
    T2 = do([erlmonads_error || A <- return(42),
                        {B,C} <- fail(test),
                        BC = B*C,
                        return(BC+A)]),
    ?assertEqual({error, test}, T2),
    T2 = do([erlmonads_error || A <- return(42),
                        {B,C} <- fail(test),
                        return(B*C+A)]),
    ?assertEqual({error, test}, T2),

    Fun = fun({X,Y}) -> {Y,X} end, %% Mysterious function
    T3 = do([erlmonads_error || R <- return({1,42}),
                        {R1,R2} = Fun(R),
                        return(R1+R2)]),
    ?assertEqual({ok,43}, T3),
    T3 = do([erlmonads_error || R <- return({1,42}),
                        %% No better way without 'let'?
                        %% Well, only via extra 'return'
                        return(element(1,Fun(R)) + element(2,Fun(R)))]),
    ?assertEqual({ok,43}, T3),

    DivRem = fun(N,M) -> {N div M,N rem M} end,
    T4 = do([erlmonads_error || {N,M} <- return({42,3}),
                        {D,R} = DivRem(N,M),
                        E <- T3,
                        S = D+R+E,
                        return({D,R,S})]),
    ?assertEqual({ok,{14,0,57}}, T4),
    T4 = do([erlmonads_error || {N,M} <- return({42,3}),
                        %% Can hack it with extra 'return' (and '>>='
                        %% as result)
                        {D,R} <- return(DivRem(N,M)),
                        E <- T3,
                        return({D,R,D+R+E})]),
    ?assertEqual({ok,{14,0,57}}, T4).
    % FIXME
    % T5 = do([erlmonads_error || X <- [1,2,3],
    %                    X2 = X*X,
    %                    Y <- lists:seq(1,X2),
    %                    Y2 = {Y,X2},
    %                    Z = Y + X2,
    %                    return({X2,Y,Y2,Z})]),
    % T5 = do([erlmonads_error || X <- [1,2,3],
    %                    Y <- lists:seq(1,X*X),
    %                    return({X*X,Y,{Y,X*X},Y+X*X})]).

let_first_test() ->
    M = do([erlmonads_list || A = 3,
                      X <- [1,2,A],
                      Y <- [A,A+1],
                      return({X,Y})]),
    M = fun() ->
                A = 3,
                do([erlmonads_list || X <- [1,2,A],
                              Y <- [A,A+1],
                              return({X,Y})])
        end().

let_escapes_test() ->
    M1 = do([erlmonads_maybe || A = 5,
                        return(A)]),
    M2 = do([erlmonads_maybe || A = 6,
                        return(A)]),
    M1 = do([erlmonads_maybe || return(5)]),
    M2 = do([erlmonads_maybe || return(6)]),

    %% Demonstrate that bindings do not escape.
    M3 = do([erlmonads_maybe || return(_A = 5)]),
    M3 = do([erlmonads_maybe || return((_A = 7) - 2)]),
    _A = 6.
