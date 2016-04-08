-module(hof).

-export([one/0,two/0,add/2,add2/2]).
-export([increment/1,decrement/1]).
-export([map/2,incr/1,decr/1]).
-export([sum/1,product/1]).

-export([silva/1,joel/1]).

-export([filter/2,foldr/3,mayor_two/1]).

silva(X) -> X*X.

one() -> 1.
two() -> 2. 
add(X,Y) -> X() + Y().

add2(A, B) ->
  F = fun() -> A + B end,
  F().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].
 
decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

product([]) -> 1;
product([H|T]) -> H * product(T).

mayor_two(X) -> X>2.

joel(X) -> X>5.

filter(Pred, L) -> foldr(Pred, L,[]).

foldr(_, [], Acc) -> Acc;
foldr(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> foldr(Pred, T, [H|Acc]);
    false -> foldr(Pred, T, Acc)
  end.
