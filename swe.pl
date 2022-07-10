% programming in logic
reload:- [swe].
open(F):- [F].
:- initialization(main).
main :- write('--- File is loaded! ---'), nl, start.
% touch swe.pl gprolog

:- use_module(library(lists)).

:- dynamic(money/1).
money(100).
setMoney(X) :- retract(money(T)),asserta(money(X)).
addMoney(X) :- money(T), N is T + X, setMoney(N).
removeMoney(X) :- money(T), N is T - X, setMoney(N).

:- dynamic(health/1).
health(100).
setHealth(X) :- retract(health(T)),asserta(health(X)).
addHealth(X) :- health(T), N is T + X, setHealth(N).
removeHealth(X) :- health(T), N is T - X, setHealth(N).

checkDie :- health(H), H =< 0 -> die ; fail.
checkDie :- health(H), H > 100 -> die ; fail.

:- dynamic(day/1).
day(1).
setDay(X) :- retract(day(T)),asserta(day(X)).
addDay(X) :- day(T), N is T + X, setDay(N).
maxDay(100).

:- dynamic(currentOrt/1).
currentOrt(erfurt).


ware(topf, 0, 2, 10).
ware(axt,1, 2, 15).
ware(schaufel,2, 2, 14).
ware(hacke,3, 2, 18).

randomWare(R) :- random(0,3, X), ware(Y,X,_,_), R = Y,!.

food(brot,10, 0.5, 5).
food(apfel,11, 1, 3).
food(kuchen,12, 1, 10).
food(kartoffel,13, 1, 2).

food(speck,14, 1, 10).

randomFood(R) :- random(0,2, X), food(Y,X,_,_), R = Y,!.
%randomItems(Items) :-

:- dynamic(toBuy/2).
toBuy(apfel, 5).
toBuy(kuchen, 8).
toBuy(brot, 12).
toBuy(axt, 2).

generateMarktCity :- 
    random(0,4,I).

loo(X) :-
    X =\= 0,
    Y is X - 1,
    loo(Y).

:- dynamic(slot/2).
slot(apfel, 2).
slot(brot, 2).
slot(hacke, 1).

count(Item, R) :- slot(Item,R),!.

canBuy(Item, Count) :-
    toBuy(Item, C),
    C >= Count, 
    money(M),
    isFood(Item) ->
    food(Item,_,_,P),
    X is P * Count,
    M >= X,!;
    ware(Item,_,_,P),
    X is P * Count,
    M >= X,!.

buy(Item, Count):-
    canBuy(Item,Count),
    toBuy(Item, C),
    addItem(Item,Count),
    N is C - Count,
    removeToBuy(Item,C,N),
    isFood(Item) ->
    food(Item,_,_,P),
    M is P * Count,
    removeMoney(M),
    handeln,!;
    ware(Item,_,_,P),
    M is P * Count,
    removeMoney(M),!.

buy(_, _) :- handeln.

removeToBuy(Item,C,N):-
    N =< 0 ->
    retract(toBuy(Item,C))
    ;
    retract(toBuy(Item,C)),
    assertz(toBuy(Item,N)).

buy(_) :- handeln.

canSell(Item, Count) :-
    slot(Item, C),
    C >= Count, !.

sell(Item,Count):-
    canSell(Item,Count),
    isInInventory(Item),
    isFood(Item) ->
    food(Item,_,_,Preis),
    P is Preis * Count,
    addMoney(P),
    removeItem(Item,Count), 
    handeln,! ;
    ware(Item,_,_,Preis),
    P is Preis * Count,
    addMoney(P),
    removeItem(Item,Count),
    handeln, ! .

%sell(_,_):- handeln.


addItem(Item,Count) :- 
    itemExists(Item),
    isInInventory(Item),
    addItemCount(Item,Count),!.

addItem(Item,Count) :- 
    itemExists(Item),
    \+ isInInventory(Item),
    assertz(slot(Item,Count)).

removeItem(Item,Count):-
    itemExists(Item),
    isInInventory(Item),
    removeItemCount(Item,Count),!.

itemExists(Item) :-
    food(Item,_,_,_),!.
itemExists(Item) :-
    ware(Item,_,_,_),!.
itemExists(_) :- fail.


addItemCount(Item,Count) :- 
    isInInventory(Item),
    slot(Item, C),
    NewCount is C + Count,
    updateItemData(Item, NewCount).

removeItemCount(Item,Count) :- 
    isInInventory(Item),
    slot(Item, C),
    C =< 1,
    retract(slot(Item, C)).

removeItemCount(Item,Count) :- 
    isInInventory(Item),
    slot(Item, C),
    C > 1,
    NewCount is C - Count,
    updateItemData(Item, NewCount).

updateItemData(Item, New):-
    slot(Item, Count),    
    New > 0 ->
    retract(slot(Item, Count)),
    asserta(slot(Item, New));
    retract(slot(Item, Count)).


isInInventory(Item) :- 
    slot(Item,_),!.

isFood(Item):- food(Item,_,_,_).

ort(erfurt).
ort(schmalkalden).
ort(eisenach).
ort(ilmenau).
ort(weimar).

city(erfurt).

weg(erfurt, schmalkalden).
weg(erfurt, eisenach).
weg(erfurt, ilmenau).
weg(erfurt, weimar).
weg(schmalkalden, eisenach).
weg(schmalkalden, ilmenau).
weg(ilmenau, weimar).

connect(X,Y) :- weg(X,Y).
connect(X,Y) :- weg(Y,X).


start :- 
    write('[   Game started   ]'), nl,
    setHealth(100),
    setMoney(100),
    setDay(1),
    look.

showInfo :- 
    write('\33\[2J'),
    write('---------------------------------------------------'), nl,
    money(M),health(H),day(D),
    format('    [ Lebenspunkte: ~w  Geld: ~w Tag: ~w ]~n', [H,M,D]),
    write('---------------------------------------------------'), nl.
showOptionen :-
    write('---------------------------------------------------'), nl,
    write('Optionen: <essen>, <arbeiten>, <handeln>, <warten>'), nl,
    write('---------------------------------------------------'), nl.

showOptionenHandeln :-
    write('------------------------------------------------------'), nl,
    write('Optionen: <exit>, <buy(item,count)>, <sell(item,count)>'), nl,
    write('------------------------------------------------------'), nl.

look :-
    showInfo,
    write('Aktueller Ort: '), currentOrt(X),
    write(X),nl,nl,
    write('        --------------------------'), nl,
    write('        Du kannst besuchen <goto>: '),nl,
    write('        --------------------------'), nl,nl,
    write('            |---------------|'), nl,
    listConnections(X),
    write('            |---------------|'), nl, nl,
    showOptionen.


canGo(Ort):-
    currentOrt(X),
    connect(X, Ort),!.
canGo(Ort):-
    write('Du kannst nicht hinkommen!'),nl,
    fail.

goto(Ort) :-
    canGo(Ort),
    move(Ort),
    removeHealth(20),
    addDay(1),
    \+ checkDie ->
    look ; fail.


move(Ort) :-
    retract(currentOrt(X)),
    asserta(currentOrt(Ort)).

listConnections(Ort) :-
    connect(Ort, X),
    tab(14),
    write(X),
    nl, fail.

listConnections(Ort).

checkDay :- day(X), maxDay(Y), X >= Y, win.

win :- write('[ End! ]'), nl, result.
die :- write('[ Du bist gestorben! ]'), nl, result.

result :-
    money(M), day(D),
    format('[ Geld: ~w Tag: ~w ] ~n', [M,D]).


handeln :-
    showInfo,
    write('             Your inventory: \n'),
    write('            |---------------|'), nl,
    showInventory,
    write('            |---------------|'), nl,nl,
    write('               You can buy: \n'),
    write('            |---------------|'), nl,
    showToBuy,
    write('            |---------------|'), nl,
    showOptionenHandeln.

showInventory:-
    slot(X,Y),
    isFood(X),
    food(X,_,_,P),
    format('              ~w (~w)  ~w€ ~n', [X,Y,P]),
    fail.

showInventory:-
    slot(X,Y),
    \+ isFood(X),
    ware(X,_,_,P),
    format('              ~w (~w)  ~w€ ~n', [X,Y,P]),
    fail.

showInventory.

showToBuy:-
    toBuy(X,Y),
    isFood(X),
    food(X,_,_,P),
    format('              ~w (~w)  ~w€ ~n', [X,Y,P]),
    fail.

showToBuy:-
    toBuy(X,Y),
    \+ isFood(X),
    ware(X,_,_,P),
    format('              ~w (~w)  ~w€ ~n', [X,Y,P]),
    fail.

showToBuy.

getGesamtGewicht(Item, R) :-
    slot(Item,G),
    isFood(Item),
    food(Item,_,Y,_),
    R is G * Y.

getGesamtGewicht(Item, R) :-
    slot(Item,G),
    \+ isFood(Item),
    ware(Item,_,Y,_),
    R is G * Y.
getGesamtGewicht(_,_).

exit :-
    addDay(1),
    removeHealth(20),
    \+ checkDie ->
    look ; fail.

arbeiten :-
    removeHealth(20),
    addDay(1),
    addMoney(5),
    \+ checkDie ->
    look ; fail.

essen :-
    addHealth(20),
    removeMoney(2),
    addDay(1),
    \+ checkDie ->
    look ; fail.

warten :-
    removeHealth(20),
    addDay(1),
    \+ checkDie ->
    look ; fail.



