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

-module(erlmonads_maybe).

-behaviour(erlmonads).
-export(['>>='/2, return/1, fail/1]).

-behaviour(erlmonads_plus).
-export([mzero/0, mplus/2]).

-export([maybe/3, is_just/1, is_nothing/1]).

-ifdef(use_specs).
-type(monad(A) :: {'just', A} | nothing).
-include("monad_specs.hrl").
-include("monad_plus_specs.hrl").
-endif.

'>>='({just, X}, Fun) -> Fun(X);
'>>='(nothing,  _Fun) -> nothing.

return(X) -> {just, X}.
fail(_X)  -> nothing.

mzero() -> nothing.
mplus(nothing, Y) -> Y;
mplus(X,      _Y) -> X.

maybe(n, _, nothing)  -> n;
maybe(_, F, {just, X}) -> F(X).

is_just(nothing) -> false;
is_just(_) -> true.

is_nothing(nothing) -> true;
is_nothing(_) -> false.
