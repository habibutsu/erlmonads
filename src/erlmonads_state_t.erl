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
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(erlmonads_state_t).
-compile({parse_transform, erlmonads_expr_do}).

-behaviour(erlmonads).

-export([new/1]).

% is required by behaviour
-export(['>>='/2, return/1, fail/1]).

-export(['>>='/3, return/2, fail/2]).
-export([get/1, put/2, eval/3, exec/3, run/3,
         modify/2, modify_and_return/2, lift/2]).

-ifdef(use_specs).
-type(monad(A) :: fun ((S) -> {A, S})).
-include("monad_specs.hrl").
-endif.

new(InnerMonad) ->
    {erlmonads_state_t, InnerMonad}.

'>>='(_, _) ->
    throw("Monad transformer is should be parametrized").

'>>='(X, Fun, {_, InnerMonad} = _THIS) ->
    fun (S) -> do([InnerMonad ||
        {A, S1} <- X(S),
        (Fun(A))(S1)]) end.

return(_) ->
    throw("Monad transformer is should be parametrized").

return(A, {_, InnerMonad} = _THIS) ->
    fun (S) -> InnerMonad:return({A, S}) end.

fail(_) ->
    throw("Monad transformer is should be parametrized").

fail(Str, {_, InnerMonad} = _THIS) ->
    fun (_) -> InnerMonad:fail(Str) end.

get({_, InnerMonad} = _THIS) ->
    fun (S) -> InnerMonad:return({S, S}) end.

put(S, {_, InnerMonad} = _THIS) ->
    fun (_) -> InnerMonad:return({ok, S}) end.

eval(Monad, S, {_, InnerMonad} = _THIS) ->
    do([InnerMonad ||
        {A, _S1} <- Monad(S),
        return(A)]).

exec(Monad, S, {_, InnerMonad} = _THIS) ->
    do([InnerMonad ||
        {_A, S1} <- Monad(S),
        return(S1)]).

run(Monad, S, {_, _InnerMonad} = _THIS) ->
    do([_InnerMonad || Monad(S)]).

modify(Fun, {_, InnerMonad} = _THIS) ->
    fun (S) -> InnerMonad:return({ok, Fun(S)}) end.

modify_and_return(Fun, {_, InnerMonad} = _THIS) ->
    fun (S) -> InnerMonad:return(Fun(S)) end.

lift(X, {_, InnerMonad} = _THIS) ->
    fun (S) -> do([InnerMonad || A <- X, return({A, S})]) end.
