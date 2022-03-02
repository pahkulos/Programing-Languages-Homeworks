
roomCap(z10,20).
roomCap(z6,15).
roomCap(z11,15).
roomCap(z12,10).

occupancy(z10,math,8).
occupancy(z10,math,9).
occupancy(z10,algo,9).
occupancy(z06,pl,10).
occupancy(z06,pl,11).


roomEquipment(z10,projector).
roomEquipment(z10,smartboard).
roomEquipment(z6,projector).
roomEquipment(z11,access).
roomEquipment(z11,projector).
roomEquipment(z11,smartboard).
courseCap(math,10).
courseCap(pl,15).
courseCap(music,5).
courseIns(math,ahmet).
courseIns(pl,hasan).
courseIns(music,ali).

courseID(math).
courseID(pl).
courseID(music).
courseID(algo).
courseID(organization).

courseRoom(math,z10).
courseRoom(pl,z06).

courseSpecial(math,projector).
courseSpecial(math,smartboard).
courseSpecial(math,access).
courseSpecial(pl,projector).
courseSpecial(pl,smartboard).
courseSpecial(pl,access).
courseSpecial(music,access).
courseSpecial(algo,smartboard).
courseSpecial(organization,projector).

courseHour(math,8).
courseHour(pl,10).
courseHour(music,11).
courseHour(algo,15).
courseHour(organization,15).

instCourse(ahmet,math).
instCourse(hasan,pl).
instCourse(mehmet,algo).
instCourse(ali,music).

instPreference(ahmet,projector).
instPreference(ahmet,smartboard).   
instPreference(hasan,projector).
instPreference(hasan,smartboard).
instPreference(ali,access).

studentCourse(ali,math).
studentCourse(ali,algo).
studentCourse(ayse,algo).
studentCourse(berk,organization).
studentID(ayse).
studentID(ali).
studentID(berk).
studentHandicapped(ali,0).
studentHandicapped(ayşe,0).
studentHandicapped(berk,1).
confilict():-
    occupancy(X,L1,Z),
    occupancy(X,L2,Z), (L1\==L2),
    format("course1:~w course2:~w room:~w time:~w", [L1, L2,X, Z]).


assign(X):-
    courseSpecial(X,Courneed),
    courseCap(X,Cour_cap),
    roomCap(Roomname,Roomcap),
    roomEquipment(Roomname,Roomequ),
    instCourse(Instname,X),
    instPreference(Instname,Inspreference),
    Courneed==Roomequ,
    Inspreference==Roomequ,
    Roomcap >= Cour_cap,
    format("~w can be assign ~w ",[X, Roomname]).

assign():-
    courseID(X),
    assign(X).
    
enrolled(X):-
    studentHandicapped(StuName,Check),
    courseSpecial(X,Courneed),
    (Check==1,
    Courneed==access,
    format("~w can be assign ~w ",[StuName, X]);Check==0,format("~w can be assign ~w ",[StuName, X])).

enrolled():-
    courseID(X),
    enrolled(X).