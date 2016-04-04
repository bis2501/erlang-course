-module(listas).

-export([list_len/1,list_len2/1]).
-export([index/1,index/2,index2/2,index3/2]).

-export([bump/1,average/1,sum/1,len/1]).
-export([member/2]).
-export([sum_acc/2,sum2/1,bump_acc/2,bump2/1]).
-export([reverse/1,reverse_acc/2]).
-export([merge/2,mergeL/3,mergeR/3]).
-export([average_acc/3,average2/1]).
-export([sum_acc/3,sum3/1]).

-export([qsort1/1, qsort2/1, qsort3/1]).

list_len([])     -> 0;
list_len([_|Xs]) -> 1 + list_len(Xs).

list_len2(Y) ->
  case Y of
    []     -> 0;
    [_|Xs] -> 1 + list_len2(Xs)
  end.

index(0,[X|_]) -> X;
index(N,[_|Xs]) when N>0 -> index(N-1,Xs).

index2(X,Y) ->
   index({X,Y}).

index(Z) ->
  case Z of
    {0,[X|_]}            -> X;
    {N,[_|Xs]} when N>0  -> index2(N-1,Xs)
  end.

index3(X,Y) ->
  case X of
    0 ->
      case Y of
        [Z|_]    -> Z
      end;
    N when N>0 ->
      case Y of
        [_|Zs]   -> index3(N-1,Zs)
      end
  end.

%% Recursion
bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].

% Finding the average value in a numeric list.
average(List) -> sum(List) / len(List).

sum([]) -> 0;
sum([Head | Tail]) -> Head + sum(Tail).

len([]) -> 0;
len([_ | Tail]) -> 1 + len(Tail).

% Is the first argument a memeer of the second argument (a list)?

member(_, [])      -> false;
member(H, [H | _]) -> true;
member(H, [_ | T]) -> member(H, T).

% Summing a list using tail recursion.

sum_acc([],Sum) -> Sum;
sum_acc([Head|Tail], Sum) -> sum_acc(Tail, Head+Sum).

sum2(List) -> sum_acc(List,0).

% Bumping every element in a list using an accumulator.

bump2(List) -> bump_acc(List, []).

bump_acc([], Acc)            -> reverse(Acc);
bump_acc([Head | Tail], Acc) -> bump_acc(Tail, [Head + 1 | Acc]).

% Reversing a list.

reverse(List) -> reverse_acc(List, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([H | T], Acc) -> reverse_acc(T, [H | Acc]).

% Merging the elements of two lists.

merge(Xs,Ys) ->
 lists:reverse(mergeL(Xs,Ys,[])).

mergeL([X|Xs],Ys,Zs) ->
  mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs) ->
  Zs.

mergeR(Xs,[Y|Ys],Zs) ->
  mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) ->
  Zs.

% Average revisited, this time using two accumulators.

average2(List) -> average_acc(List, 0,0).

average_acc([], Sum, Length) ->
  Sum / Length;
average_acc([H | T], Sum, Length) ->
  average_acc(T, Sum + H, Length + 1).

% Iterative version of sum

sum3(Boundary) -> sum_acc(1, Boundary, 0).

sum_acc(Index, Boundary, Sum) when Index =< Boundary ->
  sum_acc(Index + 1, Boundary, Sum + Index);
sum_acc(_I, _B, Sum)->
   Sum.


qsort1([]) -> [];
qsort1([H | T]) ->
   qsort1([ X || X <- T, X < H ]) ++ [H] ++ qsort1([ X || X <- T, X >= H ]).

qsort2([]) -> [];
qsort2([H | T]) ->
   {Less, Equal, Greater} = part(H, T, {[], [H], []}),
   qsort2(Less) ++ Equal ++ qsort2(Greater).

part(_, [], {L, E, G}) -> {L, E, G};
part(X, [H | T], {L, E, G}) ->
   if
       H < X ->
           part(X, T, {[H | L], E, G});
       H > X ->
           part(X, T, {L, E, [H | G]});
       true ->
           part(X, T, {L, [H | E], G})
   end.


qsort3([]) -> [];
qsort3([H | T]) -> qsort3_acc([H | T], []).

qsort3_acc([], Acc) -> Acc;
qsort3_acc([H | T], Acc) -> part_acc(H, T, {[], [H], []}, Acc).

part_acc(_, [], {L, E, G}, Acc) -> qsort3_acc(L, (E ++ qsort3_acc(G, Acc)));
part_acc(X, [H | T], {L, E, G}, Acc) ->
   if
       H < X ->
           part_acc(X, T, {[H | L], E, G}, Acc);
       H > X ->
           part_acc(X, T, {L, E, [H | G]}, Acc);
       true ->
           part_acc(X, T, {L, [H | E], G}, Acc)
   end.
