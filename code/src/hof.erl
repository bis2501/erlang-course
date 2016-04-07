-module(hof).

-export([one/0,two/0,add/2]).
-export([increment/1,decrement/1]).
-export([map/2,incr/1,decr/1]).
-export([sum/1,product/1]).

-export([filter/2,filter/3,mayor_two/1]).

one() -> 1.
two() -> 2. 
add(X,Y) -> X() + Y().

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

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.
