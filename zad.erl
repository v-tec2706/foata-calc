-module(zad).
-export([createPermutations/1,createDependancies/0,checkDependance/3,checkPair/3,parseWord/2,checkAllDependancies/1,
splitWord/3,findNormalForm/3,findNormalFoataForm/3,isEmpty/1,hasseFunction/2,performNodes/6,checkIfDependent/2,subPerformNodes/5,findFoataFromHasse/3,findEdges/4,examples/0]).

createPermutations(List1)->
  A = [{X,Y} || X <- List1, Y <- List1].

createDependancies()->
    D = dict:new(),
    D1 = dict:append('a', {x,[x,y]}, D),
    D2 = dict:append('b', {y,[y,z]}, D1),
    D3 = dict:append('c', {x,[x,z]}, D2),
    D4 = dict:append('d', {z,[y,z]}, D3).

checkDependance(NonDependant,Dependant,Pair)->
  A = createDependancies(),
  {Elem1,Elem2} = Pair,
  [{Var1,Exp1}|Tail] = dict:fetch(Elem1,A),
  [{Var2,Exp2}|Tail] = dict:fetch(Elem2,A),
  case lists:member(Var1,Exp2) or lists:member(Var2,Exp1) of
    true -> {NonDependant,[{Elem1,Elem2}|Dependant]};
    false -> {[{Elem1,Elem2}|NonDependant],Dependant}
  end.

checkPair(NonDependant,Dependancies,[]) ->
  {NonDependant,Dependancies};

checkPair(NonDependant,Dependancies,[H|T])->
  {NonDependant1,Dependancies1} = checkDependance(NonDependant,Dependancies,H),
  checkPair(NonDependant1,Dependancies1,T).

checkAllDependancies(A)->
 B = createPermutations(A),
 checkPair([],[],B).

addToDict(D,[]) -> D;
addToDict(D,[H|T]) ->
  D1 = dict:append(H,'*',D),
  addToDict(D1,T).

isEmpty(List)->
  case lists:filter(fun(X)-> case X == [] of true -> true;_->false end end,List) of
    [] -> false;
    _ -> true
  end.

isEmpty2(List)->
  case lists:filter(fun(X)-> case X /= [] of true -> true;_->false end end,List) of
    [] -> true;
    _ -> false
  end.

findNormalForm(_,true,Nf) -> lists:flatten(Nf);
findNormalForm(A,false,List) ->
  A1 = lengthSort(A),
  [H|T] = A1,
  MaxLen = length(H),
  A2 = [X || X <- A1, length(X) == MaxLen],
  A3 = lists:map(fun(A) -> lists:last(A) end,A2),
  A4 = lists:filter(fun(X) -> case X == '*' of true -> false;_->true end end,A3),
  A5 = lists:sort(A4),
  A6 = lists:map(fun(X) -> case length(X) == MaxLen of
    true -> lists:droplast(X);
    false -> X
  end end,A1),
  findNormalForm(A6,isEmpty(A6),[List|A5]).

findNormalFoataForm(_,true,Nf) -> lists:flatten(Nf);
findNormalFoataForm(A,false,List) ->
  A1 = lengthSort(A),
  [H|T] = A1,
  MaxLen = length(H),
  A2 = lists:map(fun(X) -> case length(X) > 0 of
    true ->
      case lists:last(X) == '*' of
        false -> lists:last(X);
        true -> []
      end;
    false -> []
  end end,A1),
  A3 = lists:flatten(A2),
  A4 = lists:map(fun(X) -> case length(X) > 0 of
    true ->
      case lists:last(X) == '*' of
        false -> lists:droplast(X);
        true -> X
      end;
    false -> []
  end end,A1),
  A5 = lists:map(fun(X) -> case length(X) > 0 of
    true ->
      case lists:last(X) == '*' of
        true -> lists:droplast(X);
        false -> X
      end;
    false -> []
  end end,A4),
  findNormalFoataForm(A5,isEmpty2(A5),[lists:flatten(List)|[A3,'|']]).

splitWord(Dict,_,[]) -> Dict;
splitWord(Dict,Dependant,[H|T]) ->
  D1 = dict:append(H,H,Dict),
  L = lists:filtermap( fun({X,Y}) -> case X==H andalso X/=Y of true -> {true,Y}; _ -> false end end,Dependant),
  D2 = addToDict(D1,L),
  splitWord(D2,Dependant,T).

lengthSort(List)->
  lists:sort(fun(A,B) -> case length(A) > length(B) of true -> true;_->false end end,List).

parseWord(Word,Alphabet)->
  {_,Dependant} = checkAllDependancies(Alphabet),
  D = dict:new(),
  D1 = splitWord(D,Dependant,lists:reverse(Word)),
  L = dict:to_list(D1),

  L1 = [ B || {_,B} <- L],
  findNormalFoataForm(L1,isEmpty(L1),[]).

checkIfDependent(Y,List) ->
  case lists:any(fun(X)-> case X == Y of true -> true;_ -> false end end,List) of
    false -> false;
    true -> true
  end.

subPerformNodes([],_,Min,H,_) -> {Min,H};
subPerformNodes([Head|T],NewNode,Min1,H,Dependancies)->
  {I,Ai} = NewNode,
  {Ix,Ax} = Head,
  case Head /= NewNode of
    true -> case checkIfDependent({Ax,Ai},Dependancies) of
      true -> H1 = [{I,Ix}|H],
              Min2 = lists:delete(Head, Min1);
      false -> H1 = H,Min2 = Min1
    end;
    false -> H1 = H,Min2 = Min1
  end,
  subPerformNodes(T,NewNode,Min2,H1,Dependancies).

performNodes([Ai|Tail], H, Min,I,T,Dependancies)->
  NewNode = {I,Ai},
  Min1 = [NewNode|Min],
  T1 = [NewNode|T],
  {Min2,H1} = subPerformNodes(Min1,NewNode,Min1,H,Dependancies),
  performNodes(Tail,H1,Min2,I-1,T1,Dependancies);
performNodes([],H,Min,_,T,_) -> {H,Min,lists:flatten(T)}.

hasseFunction(T,Alphabet)->
  H = [],
  Min = [],
  N = length(T),
  TRev = lists:reverse(T),
  {_,Dependancies} = checkAllDependancies(Alphabet),
  performNodes(TRev,H,Min,N,[],Dependancies).

%to nie jest ważne, a właśnie że jest, no to jest czy nie jest?halooooo :o xD 

findEdges([],_,FoataForm,Labels) -> lists:flatten(FoataForm);
findEdges(Nodes,Edges,FoataForm,Labels)->
  MatchingNodes = lists:filter(fun({X,_})-> case lists:any(fun(Y)-> case Y == X of true -> true; _ -> false end end,Nodes) of true -> true;_->false end end,Edges),
  MatchingNodes1 = [X || {_,X} <- MatchingNodes],
  RemainingEdges = lists:filter(fun({_,X})-> case lists:any(fun(Y) -> case Y == X of true -> true; _ -> false end end,MatchingNodes1) of true -> false;_->true end end,Edges),
  MatchingNodesLabels = lists:filter(fun({X,_})-> case lists:any(fun(Y)-> case Y == X of true -> true; _ -> false end end,MatchingNodes1) of true -> true;_->false end end,Labels),
  MatchingNodesLabels2 = [X||{_,X} <- MatchingNodesLabels],
  MatchingNodes2 = ['-'|MatchingNodesLabels2],
  findEdges(MatchingNodes1,RemainingEdges,[FoataForm| MatchingNodes2],Labels).

findFoataFromHasse(Edges,Min,Labels) ->
  [{First,Label}|_] = Min,
  findEdges([First],Edges,[Label],Labels).



%%%%%% EXAMPLES
examples()->
% wyznacza relację zależności i niezależności:
checkAllDependancies(['a','b','c','d']),

% wyznacza postać normalną Foaty FNF[w]:
parseWord(['b','a','a','d','c','b'],['a','b','c','d']),

% wyznacza krawędzie grafu zależności:
{A,B,C} = hasseFunction(['b','a','a','d','c','b'],['a','b','c','d']),

%wyznacza postać normalną Foaty na podstawie grafu:
findFoataFromHasse(A,B,C).
