-module(calculus).

-export([f/1]).
-export([area/1,area2/2,factorial/1]).

-import(math, [sqrt/1]).

f(X) -> Y=X+1,Y*X.

% calculo del area de una figura
area({square, Side}) ->
  Side * Side ;
area({circle, Radius}) ->
  math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
  S = (A + B + C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C));
area(_) ->
  {error, invalid_object}.


area2(Type, Data) ->
  case Type of
    square   -> Data * Data;
    circle   -> math:pi() * Data * Data;
    triangle -> triangle_area(Data);
    _        -> {error, invalid_object}
  end.

triangle_area({A,B,C}) ->
  area({triangle, A, B, C}).

% el famoso factorial
factorial(N) when N > 0 ->
  N * factorial(N - 1);
factorial(0) -> 1.
