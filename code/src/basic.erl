-module(basic).

-export([f/1,g/1,safe/1,preferred/1]).

-export([factorial/1,guard/2,guard2/2]).
-export([even/1, number/1]).
-export([a/2, b/1]).

a(X, Y) -> c(X) + a(Y).
a(X) -> 2 * X.
b(X) -> X * X.
c(X) -> 3 * X.

factorial(0) -> 1;
factorial(N) ->
  N * factorial(N-1).

%% Guards
factorial2(N) when N > 0 ->
  N * factorial2(N - 1);
factorial2(0) -> 1.

guard(X,Y) when not(((X>Y) or not(is_atom(X)) ) and (is_atom(Y) or (X==3.4))) ->
  X+Y.

guard2(X,Y) when not(X>Y) , is_atom(X) ; not(is_atom(Y)) , X=/=3.4 ->
  X+Y.

even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false.

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num)   -> float;
number(_Other)                   -> false.
