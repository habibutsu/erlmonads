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

-module(erlmonads_identity).

-behaviour(erlmonads).
-export(['>>='/2, return/1, fail/1]).

-ifdef(use_specs).
-type(monad(A) :: A).
-include("monad_specs.hrl").
-endif.

'>>='(X, Fun) -> Fun(X).

return(X) -> X.
fail(X)   -> throw({error, X}).
