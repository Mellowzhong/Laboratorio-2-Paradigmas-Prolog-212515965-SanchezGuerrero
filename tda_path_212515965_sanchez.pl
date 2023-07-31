:- module(tda_path_212515965_sanchez, [set_path/3, set_path_return/2, set_path_root/2, get_path/2, switch_path/2, update_path/3]).

%-----------------------Representacion-----------------------
%Se presenta el TDA Path, el cual corresponde a tal y como indica su nombre a una representacion del path,
%el cual es la direccion hacia donde uno se debe mover dentro del sistema. Esta respresentación esta dada
%por una serie de funciones las cuales permitiran conocer las rutas por donde se debera mover en el 
%sistema pasando de un string (ruta dada por el usuario) a una lista para poder indendtidicar

%-------------------Selectores-----------------------
%Descripcion: Entrega el path del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_path(System, Path) :-
    filesystem(_, _, _, Path, _, _, System).
%-------------------Modificadores-----------------------
%Descripcion: Borra el primer elemento del path entregado
%tipo de algoritmo: No aplicada
%Dom: list
%Rec: list
set_path_return(Path, UpdatePath) :-
    del_first_element(Path, UpdatePath).    

%Descripcion: Actualiza el path a la raiz
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
set_path_root(Path, [RootPath]) :-
    get_first_element(Path, RootPath).

%Descripcion: Actualiza el sistema con el nuevo path
%tipo de algoritmo: No aplicada
%Dom: list - list
%Rec: list
set_path(System, NewPath, UpdateSystem):-
    filesystem(Name, Drives, User, _, Rb, Date, System),
    filesystem(Name, Drives, User, NewPath, Rb, Date, UpdateSystem).

%Descripcion: Setea un nuevo path, dando un nuevo origen
%tipo de algoritmo: No aplicada
%Dom: string
%Rec: list
switch_path(Letter, [Letter]).

%Descripcion: Actualiza el path dando un un path nuevo
%tipo de algoritmo: Backtracking
%Dom: list - list
%Rec: list
update_path([], Acum, Acum).

update_path([FirstListDirectory | RestListDirectory], Path, UpdatePath) :-
    equal_elements(FirstListDirectory, ""),
    reverse_list(Path, ReversePath),
    set_path_root(ReversePath, NewPath),
    update_path(RestListDirectory, NewPath, UpdatePath).

update_path([FirstListDirectory | RestListDirectory], Path, UpdatePath) :-
    equal_elements(FirstListDirectory, ".."),
    set_path_return(Path, NewPath),
    update_path(RestListDirectory, NewPath, UpdatePath).

update_path([FirstListDirectory | RestListDirectory], Path, UpdatePath) :-
    equal_elements(FirstListDirectory, "."),
    update_path(RestListDirectory, Path, UpdatePath).

update_path([FirstListDirectory | RestListDirectory], Path, UpdatePath) :-
    update_path(RestListDirectory, [FirstListDirectory | Path], UpdatePath).
    