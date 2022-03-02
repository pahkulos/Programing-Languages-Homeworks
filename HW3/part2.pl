% knowledge base
flight(istanbul,izmir,2).
flight(istanbul,ankara,1).
flight(istanbul,rize,4).
flight(ankara,rize,5).
flight(ankara,izmir,6).
flight(ankara,van,4).
flight(ankara,diyarbakır,7).
flight(van,gaziantep,5).
flight(izmir,antalya,2).
flight(canakkale,erzincan,6).
flight(antalya,erzincan,3).
flight(antalya,diyarbakır,4).


% rules
route_path(X,Y,C) :-
    flight(X,Y,C) ;
    flight(Y,X,C) .

route(X, Y):-
    search_route(X, Y, 0, []).


search_route(X, Y,R, List):-
    route_path(X, Z,C1),
    C2 is C1+R,
    format("distance ~w", C2),
    not(member(Z, List)),
    (Y = Z; search_route(Z, Y,C2, [X | List])).
