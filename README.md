# Erlmonads

Fork of [erlando](https://github.com/rabbitmq/erlando) without parameterized
modules that are no longer supported starting with version OTP 16.

## Introduction

Erlmonads is a set of syntax extensions for Erlang. Currently it
consists of three syntax extensions, all of which take the form of
[parse-transformers](http://www.erlang.org/doc/man/erl_id_trans.html).

* **Cut**: This adds support for *cut*s to Erlang. These are
  inspired by the
  [Scheme form of cuts](http://srfi.schemers.org/srfi-26/srfi-26.html). *Cut*s
  can be thought of as a light-weight form of abstraction, with
  similarities to partial application (or currying).

* **Do**: This adds support for *do*-syntax and monads to
  Erlang. These are heavily inspired by [Haskell](http://haskell.org),
  and the monads and libraries are near-mechanical translations from
  the Haskell GHC libraries.

* **Import As**: This adds support for importing remote functions to
  the current module namespace with explicit control of the local
  function names.



## Use

To use any of these parse-transformers, you must add the necessary
`-compile` attributes to your Erlang source files. For example:

    -module(test).
    -compile({parse_transform, erlmonads_expr_cut}).
    -compile({parse_transform, erlmonads_expr_do}).
    -compile({parse_transform, erlmonads_expr_import_as}).
    ...

Then, when compiling `test.erl`, you must ensure `erlc` can locate
`cut.beam` or `do.beam` or `import_as.beam` by passing the suitable
path to `erlc` with a `-pa` or `-pz` argument. For example:

    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/cut.erl
    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/do.erl
    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/import_as.erl
    erlc -Wall +debug_info -I ./include -pa test/ebin -pa ./ebin -o test/ebin test/src/test.erl

*Note*: If you're using QLC, you may find you need to be careful as to
the placement of the parse-transformer attributes. For example, I've
found that `-compile({parse_transform, cut}).` must occur before
`-include_lib("stdlib/include/qlc.hrl").`



## Cut

### Motivation

The *cut* parse-transformer is motivated by the frequency with which simple
function abstractions are used in Erlang, and the relatively noisy
nature of declaring `fun`s. For example, it's quite common to see code
like:

    with_resource(Resource, Fun) ->
        case lookup_resource(Resource) of
            {ok, R}          -> Fun(R);
            {error, _} = Err -> Err
        end.

    my_fun(A, B, C) ->
        with_resource(A, fun (Resource) ->
                             my_resource_modification(Resource, B, C)
                         end).

That is, a `fun` is created in order to perform variable capture
from the surrounding scope but to leave holes for further
arguments to be provided. Using a *cut*, the function `my_fun` can be
rewritten as:

    my_fun(A, B, C) ->
        with_resource(A, my_resource_modification(_, B, C)).


### Definition

Normally, the variable `_` can only occur in patterns: that is, where a
match occurs. This can be in assignment, in cases, and in function
heads. For example:

    {_, bar} = {foo, bar}.

*Cut* uses `_` in expressions to indicate where abstraction should
occur. Abstraction from *cut*s is **always** performed on the
*shallowest* enclosing expression. For example:

    list_to_binary([1, 2, math:pow(2, _)]).

will create the expression

    list_to_binary([1, 2, fun (X) -> math:pow(2, X) end]).

and not

    fun (X) -> list_to_binary([1, 2, math:pow(2, X)]) end.

It is fine to use multiple *cut*s in the same expression, and the
arguments to the created abstraction will match the order in which the
`_` var is found in the expression. For example:

    assert_sum_3(X, Y, Z, Sum) when X + Y + Z == Sum -> ok;
    assert_sum_3(_X, _Y, _Z, _Sum) -> {error, not_sum}.
    
    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        ok = Equals12(9, 2, 1).

It is perfectly legal to take *cut*s of *cut*s as the abstraction created
by the *cut* is a normal `fun` expression and thus can be re-*cut* as
necessary:

    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        Equals5 = Equals12(_, _, 7),
        ok = Equals5(2, 3).

Note that because a simple `fun` is being constructed by the *cut*, the
arguments are evaluated prior to the *cut* function. For example:

    f1(_, _) -> io:format("in f1~n").

    test() ->
        F = f1(io:format("test line 1~n"), _),
        F(io:format("test line 2~n")).

will print out

    test line 2
    test line 1
    in f1

This is because the *cut* creates `fun (X) -> f1(io:format("test line
1~n"), X) end`. Thus it is clear that `X` must be evaluated first,
before the `fun` can be invoked.

Of course, no one would be crazy enough to have side-effects in
function argument expressions, so this will never cause any issues!

*Cut*s are not limited to function calls. They can be used in any
expression where they make sense:


#### Tuples

    F = {_, 3},
    {a, 3} = F(a).


#### Lists

    dbl_cons(List) -> [_, _ | List].
    
    test() ->
        F = dbl_cons([33]),
        [7, 8, 33] = F(7, 8).

Note that if you nest a list as a list tail in Erlang, it's still
treated as one expression. For example:

    A = [a, b | [c, d | [e]]]

is exactly the same (right from the Erlang parser onwards) as:

    A = [a, b, c, d, e]

That is, those sub-lists, when they're in the tail position, **do not**
form sub-expressions. Thus:

    F = [1, _, _, [_], 5 | [6, [_] | [_]]],
    %% This is the same as:
    %%  [1, _, _, [_], 5, 6, [_], _]
    [1, 2, 3, G, 5, 6, H, 8] = F(2, 3, 8),
    [4] = G(4),
    [7] = H(7).

However, be very clear about the difference between `,` and `|`: the
tail of a list is **only** defined following a `|`. Following a `,`,
you're just defining another list element.

    F = [_, [_]],
    %% This is **not** the same as [_, _] or its synonym: [_ | [_]]
    [a, G] = F(a),
    [b] = G(b).


#### Records

    -record(vector, { x, y, z }).
    
    test() ->
        GetZ = _#vector.z,
        7 = GetZ(#vector { z = 7 }),
        SetX = _#vector{x = _},
        V = #vector{ x = 5, y = 4 } = SetX(#vector{ y = 4 }, 5).


#### Case

    F = case _ of
            N when is_integer(N) -> N + N;
            N                    -> N
        end,
    10 = F(5),
    ok = F(ok).


See
[erlmonads_expr_cut_test.erl](https://github.com/habibutsu/erlmonads/blob/master/test/erlmonads_expr_cut_test.erl)
for more examples, including the use of *cut*s in list comprehensions and
binary construction.

Note that *cut*s are not allowed where the result of the *cut* can only be
useful by interacting with the evaluation scope. For example:

    F = begin _, _, _ end.

This is not allowed, because the arguments to `F` would have to be
evaluated before the invocation of its body, which would then have no
effect, as they're already fully evaluated by that point.



## Do

The *do* parse-transformer permits Haskell-style *do-notation* in
Erlang, which makes using monads, and monad transformers possible and
easy. (Without *do-notation*, monads tend to look like a lot of line
noise.)


### The Inevitable Monad Tutorial

#### The Mechanics of a Comma

What follows is a brief and mechanical introduction to monads. It
differs from a lot of the Haskell monad tutorials, because they tend
to view monads as a means of achieving sequencing of operations in
Haskell, which is challenging because Haskell is a lazy
language. Erlang is not a lazy language, but the abstractions
possible from using monads are still worthwhile.

Let's say we have the three lines of code:

    A = foo(),
    B = bar(A, dog),
    ok.

They are three, simple statements, which are evaluated
consecutively. What a monad gives you is control over what happens
between the statements: in Erlang, it is a programmatic comma.

If you wanted to implement a programmatic comma, how would you do it?
You might start with something like:

    A = foo(),
    comma(),
    B = bar(A, dog),
    comma(),
    ok.

But that's not quite powerful enough, because unless `comma/0` throws
some sort of exception, it can't actually stop the subsequent
expression from being evaluated. Most of the time we'd probably like
the `comma/0` function to be able to act on some variables which are
currently in scope, and that's not possible here either. So we should
extend the function `comma/0` so that it takes the result of the
preceding expression, and can choose whether or not the subsequent
expressions should be evaluated:

    comma(foo(),
          fun (A) -> comma(bar(A, dog),
                           fun (B) -> ok end)
          end).

Thus the function `comma/2` takes all results from the previous
expression, and controls how and whether they are passed to the next
expression.

As defined, the `comma/2` function is the monadic function `'>>='/2`.

Now it's pretty difficult to read the program with the `comma/2`
function (especially as Erlang annoyingly doesn't allow us to define
new *infix* functions), which is why some special syntax is
desirable. Haskell has its *do-notation*, and so we've borrowed from
that and abused Erlang's list comprehensions. Haskell also has lovely
type-classes, which we've sort of faked specifically for monads. So,
with the *do* parse-transformer, you can write in Erlang:

    do([Monad ||
        A <- foo(),
        B <- bar(A, dog),
        ok]).

which is readable and straightforward, and this is transformed into:

    Monad:'>>='(foo(),
                fun (A) -> Monad:'>>='(bar(A, dog),
                                       fun (B) -> ok end)
                end).

There is no intention that this latter form is any more readable than
the `comma/2` form - it is not. However, it should be clear that the
function `Monad:'>>='/2` now has *complete* control over what happens:
whether the `fun` on the right hand side ever gets invoked (and how often);
and if so, with what parameter values.


#### Lots of different types of Monads

So now that we have some relatively nice syntax for using monads, what
can we do with them? Also, in the code

    do([Monad ||
        A <- foo(),
        B <- bar(A, dog),
        ok]).

what are the possible values of `Monad`?

The answer to the former question is *almost anything*; and to the
latter question, is *any module name that implements the monad
behaviour*.

Above, we covered one of the three monadic operators, `'>>='/2`. The
others are:

* `return/1`: This *lifts* a value into the monad. We'll see examples
  of this shortly.

* `fail/1`: This takes a term describing the error encountered, and
  informs whichever monad currently in use that some sort of error has
  occurred.

Note that within *do-notation*, any function call to functions named
`return` or `fail`, are automatically rewritten to invoke `return` or
`fail` within the current monad.

> Some people familiar with Haskell's monads may be expecting to see a
fourth operator, `'>>'/2`. Interestingly, it turns out that you can't
implement `'>>'/2` in a strict language unless all your monad types are
built on functions. This is because in a strict language,
arguments to functions are evaluated before the function is
invoked. For `'>>='/2`, the second argument is only reduced to a function
prior to invocation of `'>>='/2`. But the second argument to `'>>'/2` is not
a function, and so in strict languages, will be fully reduced prior to
`'>>'/2` being invoked. This is problematic because the `'>>'/2` operator
is meant to be in control of whether or not subsequent expressions are
evaluated. The only solution here would be to make the basic monad
type a function, which would then mean that the second argument to
`'>>='/2` would become a function to a function to a result!

> However, it is required that `'>>'(A, B)` behaves identically to
`'>>='(A, fun (_) -> B end)`, and so that is what we do: whenever we come to a
`do([Monad || A, B ])`, we rewrite it to `'>>='(A, fun (_) -> B end)`
rather than `'>>'(A, B)`. There is no `'>>'/2` operator in our Erlang monads.

The simplest monad possible is the Identity-monad:

    -module(erlmonads_identity).
    -behaviour(erlmonads).
    -export(['>>='/2, return/1, fail/1]).

    '>>='(X, Fun) -> Fun(X).
    return(X)     -> X.
    fail(X)       -> throw({error, X}).

This makes our programmatic comma behave just like Erlang's comma
normally does. The **bind** operator (that's the Haskell term for the
`'>>='/2` monadic operator) does not inspect the
values passed to it, and always invokes the subsequent expression function.

What could we do if we did inspect the values passed to the sequencing
combinators? One possibility results in the Maybe-monad:

    -module(erlmonads_maybe).
    -behaviour(erlmonads).
    -export(['>>='/2, return/1, fail/1]).
    
    '>>='({just, X}, Fun) -> Fun(X);
    '>>='(nothing,  _Fun) -> nothing.
    
    return(X) -> {just, X}.
    fail(_X)  -> nothing.

Thus if the result of the preceding expression is `nothing`, the
subsequent expressions are *not* evaluated. This means that we can write
very neat looking code which immediately stops should any failure be
encountered.

    if_safe_div_zero(X, Y, Fun) ->
        do([erlmonads_maybe ||
            Result <- case Y == 0 of
                          true  -> fail("Cannot divide by zero");
                          false -> return(X / Y)
                      end,
            return(Fun(Result))]).

If `Y` is equal to 0, then `Fun` will not be invoked, and the result
of the `if_safe_div_zero` function call will be `nothing`. If `Y` is
not equal to 0, then the result of the `if_safe_div_zero` function
call will be `{just, Fun(X / Y)}`.

We see here that within the *do*-block, there is no mention of `nothing`
or `just`: they are abstracted away by the Maybe-monad. As a result,
it is possible to change the monad in use, without having to rewrite
any further code.

One common place to use a monad like the Maybe-monad is where you'd
otherwise have a lot of nested case statements in order to detect
errors. For example:

    write_file(Path, Data, Modes) ->
        Modes1 = [binary, write | (Modes -- [binary, write])],
        case make_binary(Data) of
            Bin when is_binary(Bin) ->
                case file:open(Path, Modes1) of
                    {ok, Hdl} ->
                        case file:write(Hdl, Bin) of
                            ok ->
                                case file:sync(Hdl) of
                                    ok ->
                                        file:close(Hdl);
                                    {error, _} = E ->
                                        file:close(Hdl),
                                        E
                                end;
                            {error, _} = E ->
                                file:close(Hdl),
                                E
                        end;
                    {error, _} = E -> E
                end;
            {error, _} = E -> E
        end.

    make_binary(Bin) when is_binary(Bin) ->
        Bin;
    make_binary(List) ->
        try
            iolist_to_binary(List)
        catch error:Reason ->
                {error, Reason}
        end.

can be transformed into the much shorter

    write_file(Path, Data, Modes) ->
        Modes1 = [binary, write | (Modes -- [binary, write])],
        do([erlmonads_error ||
            Bin <- make_binary(Data),
            Hdl <- file:open(Path, Modes1),
            Result <- return(do([erlmonads_error ||
                                 file:write(Hdl, Bin),
                                 file:sync(Hdl)])),
            file:close(Hdl),
            Result]).
    
    make_binary(Bin) when is_binary(Bin) ->
        erlmonads_error:return(Bin);
    make_binary(List) ->
        try
            erlmonads_error:return(iolist_to_binary(List))
        catch error:Reason ->
                erlmonads_error:fail(Reason)
        end.

Note that we have a nested *do*-block so, as with the non-monadic
code, we ensure that once the file is opened, we always call
`file:close/1` even if an error occurs in a subsequent operation. This
is achieved by wrapping the nested *do*-block with a `return/1` call:
even if the inner *do*-block errors, the error is *lifted* to a
non-error value in the outer *do*-block, and thus execution continues to
the subsequent `file:close/1` call.

Here we are using an Error-monad which is remarkably similar to the
Maybe-monad, but matches the typical Erlang practice of indicating
errors by an `{error, Reason}` tuple:

    -module(erlmonads_error).
    -behaviour(erlmonads).
    -export(['>>='/2, return/1, fail/1]).
    
    '>>='({error, _Err} = Error, _Fun) -> Error;
    '>>='({ok, Result},           Fun) -> Fun(Result);
    '>>='(ok,                     Fun) -> Fun(ok).
    
    return(X) -> {ok,    X}.
    fail(X)   -> {error, X}.


#### Monad Transformers

Monads can be *nested* by having *do*-blocks inside *do*-blocks, and
*parameterized* by defining a monad as a transformation of another, inner,
monad. The State Transform is a very commonly used monad transformer,
and is especially relevant for Erlang. Because Erlang is a
single-assignment language, it's very common to end up with a lot of
code that incrementally numbers variables:

    State1 = init(Dimensions),
    State2 = plant_seeds(SeedCount, State1),
    {DidFlood, State3} = pour_on_water(WaterVolume, State2),
    State4 = apply_sunlight(Time, State3),
    {DidFlood2, State5} = pour_on_water(WaterVolume, State4),
    {Crop, State6} = harvest(State5),
    ...

This is doubly annoying, not only because it looks awful, but also
because you have to re-number many variables and references whenever a
line is added or removed. Wouldn't it be nice if we could abstract out the
`State`? We could then have a monad encapsulate the state and provide
it to (and collect it from) the functions we wish to run.


The State-transform can be applied to any monad. If we apply it to the
Identity-monad then we get what we're looking for. The key extra
functionality that the State transformer provides us with is the
ability to `get` and `set` (or just plain `modify`) state from within
the inner monad. If we use both the *do* and *cut* parse-transformers, we
can write:

    StateT = erlmonads_state_t:new(erlmonads_identity),
    SM = StateT:modify(_),
    SMR = StateT:modify_and_return(_),
    StateT:exec(
      do([StateT ||

          StateT:put(init(Dimensions)),
          SM(plant_seeds(SeedCount, _)),
          DidFlood <- SMR(pour_on_water(WaterVolume, _)),
          SM(apply_sunlight(Time, _)),
          DidFlood2 <- SMR(pour_on_water(WaterVolume, _)),
          Crop <- SMR(harvest(_)),
          ...

          ]), undefined).

We began by creating a State-transform over the Identity-monad:

    StateT = erlmonads_state_t:new(erlmonads_identity),
    ...

> This is the syntax for *instantiating* parameterized modules. `StateT` is a
variable referencing a module instance which, in this case, is a monad.

and we define two shorthands for running functions that either just
modify the state, or modify the state *and* return a result:

    SM = StateT:modify(_),
    SMR = StateT:modify_and_return(_),
    ...

There's a bit of bookkeeping required but we achieve our goal: there are no
state variables now to renumber whenever we make a change. We used *cut*s
to leave holes in the functions where State should be fed in; and we
obeyed the protocol that if a function returns both a result and a state, it
is in the form of a `{Result, State}` tuple. The State-transform does the rest.


### Beyond Monads

There are some standard monad functions such as `join/2` and
`sequence/2` available in the `monad` module. We have also implemented
`monad_plus` which works for monads where there's an obvious sense of
*zero*  and *plus* (currently Maybe-monad, List-monad, and Omega-monad).
The associated functions `guard`, `msum` and `mfilter` are available
in the `monad_plus` module.

In many cases, a fairly mechanical translation from Haskell to Erlang
is possible, so converting other monads or combinators should mostly
be straightforward. However, the lack of type classes in Erlang is
limiting.



## Import As

For cosmetic reasons, it is sometimes desirable to import a remote
function into the current module's namespace. This eliminates the need
to continuously prefix calls to that function with its module
name. Erlang can already do this by using the
[`-import` attribute](http://www.erlang.org/doc/reference_manual/modules.html).
However, this always uses the same function name locally as remotely
which can either lead to misleading function names or even
collisions. Consider, for example, wishing to import `length`
functions from two remote modules. Aliasing of the functions is one
solution to this.

For example:

    -import_as({lists, [{duplicate/2, dup}]}).
    
    test() ->
        [a, a, a, a] = dup(4, a).

As with `-import`, the left of the tuple is the module name, but the
right of the tuple is a list of pairs, with the left being the
function to import from the module (including arity) and the right
being the local name by which the function is to be known--the
*alias*. The implementation creates a local function, so the alias is
safe to use in, for example, `Var = fun dup/2` expressions.
