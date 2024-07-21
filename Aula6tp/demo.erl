%
%-module(demo).
%-export([fact/1]).
%
%fact(0) -> 1;
%fact(N) -> N*fact(N-1).
%
%
%
%%%%%%%%%
-module(demo).
-export([create/0,enqueue/2,dequeue/1,teste/0]).

create() -> [].

%enqueue(Queue, Item) -> ok.
enqueue(Queue, Item) -> Queue ++ [Item].

% outro exc
% enqueue({In Out},Item) -> {[Item | In], Out}.


%dequeue(Queue) -> ok.
dequeue([]) -> empty;
dequeue([H|T]) -> {T, H}.

% outro exc
% dequeue({In, [H|T]}) -> {{In, T}, H};
% dequeue({In, []}) -> dequeue({[], lists:reverse(In)}).

%
%resverse(L) -> reverse(L,[]).
%
%resverse([], L) -> L;
%resverse([H|T], L) -> reverse(T, [H|L]).
%


teste() ->
    Q1 = create(),
    empty = dequeue(Q1),
    Q2 = enqueue(Q1,1),
    Q3 = enqueue(Q2,2),
    {_, 1} = dequeue(Q2),
    {Q4, 1} = dequeue(Q3),
    {Q5, 2} = dequeue(Q4),
    empty = dequeue(Q5),
    ok.