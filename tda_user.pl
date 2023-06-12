:- module(tda_user, [create_user/2, set_user/3, username_exist/2, login_exist/1, get_user/2, get_login_user/2,add_user_to_users/3, add_login/3]).

%-------------------Constructores-----------------------
%Descripcion: Crea un usuario
%tipo de algoritmo: No aplicado
%Dom: string
%Rec: list
create_user(Name, [Name]).

%Descripcion: Actualiza el sistema con los nuevos usuarios
%tipo de algoritmo: No aplicado
%Dom: list - list
%Rec: list
set_user(System, UpdateUsers, UpdateSystem) :-
    filesystem(Name, Drives, _, Path, Rb, Date, System),
    filesystem(Name, Drives, UpdateUsers, Path, Rb, Date, UpdateSystem).

%-------------------Pertenencia-----------------------
%Descripcion: Verifica si el usuario existe 
%tipo de algoritmo: Backtracking
%Dom: string - list
%Rec: bool
username_exist(User, [User | _]) :- true.

username_exist(NewUser, [_ | Rest]) :-
    username_exist(NewUser, Rest).

%Descripcion: Verifica si alguien esta logeado
%tipo de algoritmo: Backtracking
%Dom: list
%Rec: bool
login_exist([true | _]).

login_exist([_ | Rest]) :-
    login_exist(Rest).  

%-------------------Selectores-----------------------
%Descripcion: Obtiene el usuario del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_user(System, Users) :-
    filesystem(_, _, Users, _, _, _, System).

%Descripcion: Obtiene los usuarios logeados
%tipo de algoritmo: No aplica
%Dom: list 
%Rec: string
get_login_user(Users, LoginUser) :-
    get_first_element(Users, LoginUser).

%-------------------Modificadores-----------------------
%Descripcion: Añade un nuevo usuario a la lista de usuarios
%tipo de algoritmo: No aplicado
%Dom: string - list
%Rec: list
add_user_to_users(NewUser, Users, UpdateUsers) :-
    append(Users, NewUser, UpdateUsers).

%Descripcion: Logea al usuario entregado
%tipo de algoritmo: Bakctracking
%Dom: string - list
%Rec: list
add_login(User, [User| Rest], [User, true | Rest]).

add_login(User, [OtherUser | Rest], [OtherUser, UpdateUsers]) :-
    add_login(User, Rest, UpdateUsers).