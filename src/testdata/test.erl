-module(test).

-compile(debug_info).
-foo_attribute(bar). % An original attribute

-behaviour(test).
-behavior(test2).

-export([literals/0]).
-export([hello/1]).
-export([map_fun/2]).
-export([cons/2]).
-export([to_my_list/1]).
-export([my_record/0]).
-export([guard/1]).

-export_type([my_list/1]).
-export_type([my_cons/2]).

-callback hello(Name :: binary()) -> ok | {error, Reason :: term()}.

-opaque my_list(E) :: my_cons(E, my_list(E)) | nil.
-type my_cons(H, T) :: {H, T}.

-record(my_record,
        {
          a,
          b = 10 :: integer(),
          c :: pid(),
          d = foo
        }).

-spec literals() -> {integer(), neg_integer(), float(), atom(), list(), binary(), bitstring(), map(), pid(), reference()}.
literals() ->
    {
      123,
      -123,
      12.3,
      foo,
      [1,2,3],
      <<"123">>,
      <<"123", 2:2>>,
      #{123 => abc},
      self(),
      make_ref()
    }.

hello(<<Name/binary>>) ->
    io:format("Hello ~s\n", [Name]),
    ok.

-spec map_fun(Fun, List) -> Result when
      Fun :: fun ((Input) -> Result),
      Input :: term(),
      List :: [Input],
      Result :: term().
map_fun(Fun, List) ->
    [Fun(X) || X <- List].

-spec cons(H, T) -> my_cons(H, T) when
      H :: term(),
      T :: term().
cons(H, T) ->
    {H, T}.

-spec to_my_list([E]) -> my_list(E).
to_my_list([])      -> nil;
to_my_list([H | T]) -> cons(H, to_my_list(T)).

-spec my_record() -> #my_record{}.
my_record() ->
    #my_record{
       c = self()
      }.

-spec guard(term()) -> term().
guard(X) when is_integer(X); is_atom(X) -> X;
guard(X) when is_integer(X), X > 0 -> 10 / X;
guard(#{hello := X}) when is_atom(X) orelse (is_integer(X) andalso X < 0)-> X;
guard({_, #{}, <<10, Bin/binary>>}) ->  Bin.
