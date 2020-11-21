-module(quizfive).
-compile(export_all).

allEmpty(Queue) -> 
    {Item, NextQueue} = queue:out(Queue),
    case Item of
        empty -> true;
        {value, {empty}} -> allEmpty(NextQueue);
        {value, {node,_,_,_}} -> false
    end.

isComplete({empty}) -> true;
isComplete(Node) ->
    Q = queue:new(),
    Q1 = queue:in(Node, Q),
    isCompleteHelper(Q1).

isCompleteHelper(Queue) ->
    {Item, NextQueue} = queue:out(Queue),
    case Item of
        empty -> true;
        {value, {empty}} -> allEmpty(NextQueue);
        {value, {node, _, Left, Right}} -> isCompleteHelper(queue:in(Right, queue:in(Left, NextQueue)))
    end.

t1() -> {node, 1, {node, 2, {empty}, {empty}}, {node, 3, {empty}, {empty}}}.
t2() -> {node, 1,
            {node, 2, {empty}, {empty}},
            {node, 3, {empty},
                {node, 3, {empty}, {empty}}
            }
        }.