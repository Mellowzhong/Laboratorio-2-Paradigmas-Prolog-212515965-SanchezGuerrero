:- module(tda_path, [set_path/3, set_path_return/2, set_path_root/2, set_add_path/3, get_path/2, get_origin_path/2, switch_path(Letter, [Letter]).
/2, add_path/3, update_path/3]).

%-------------------Constructores-----------------------
%Descripcion: Actualiza el sistema con el nuevo path
%tipo de algoritmo: No aplicada
%Dom: list - list
%Rec: list
set_path(System, NewPath, UpdateSystem):-
    filesystem(Name, Drives, User, _, Rb, Date, System),
    filesystem(Name, Drives, User, NewPath, Rb, Date, UpdateSystem).

%Descripcion: Borra el primer elemento del path entregado
%tipo de algoritmo: No aplicada
%Dom: list
%Rec: list
set_path_return(Path, UpdatePath) :-
    del_first_element(Path, UpdatePath).    

%Descripcion: Actualiza el drive a la raiz
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
set_path_root(Path, [RootPath]) :-
    get_first_element(Path, RootPath).

%Descripcion: Añade una nueva ruta al path entregado
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
set_add_path(ReversePath, FirstListDirectory, NewPath) :-
    append(FirstListDirectory, [ReversePath], NewPath).

%-------------------Selectores-----------------------
%Descripcion: Entrega el path del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_path(System, Path) :-
    filesystem(_, _, _, Path, _, _, System).

%Descripcion: Obtiene el origen dle path entregado
%tipo de algoritmo: No aplicado
%Dom: list
%Rec: string
get_origin_path([Letter | _], Letter).
%-------------------Modificadores-----------------------
%Descripcion: Setea un nuevo path, dando un nuevo origen
%tipo de algoritmo: No aplicada
%Dom: string
%Rec: list
switch_path(Letter, [Letter]).

%Descripcion: Añade una nueva ruta al path
%tipo de algoritmo: No aplicada
%Dom: string - list
%Rec: list
add_path(DirectoryCd, Path, [DirectoryCd | Path]).

%Descripcion: Actualiza el path dando un un path nuevo
%tipo de algoritmo: Backtracking
%Dom: list - list
%Rec: list
update_path([], Acum, Acum).

update_path([_ | RestListDirectory], Path, UpdatePath) :-
    path_get_origin(Path),
    set_path_origin(Path, NewPath),
    update_path(RestListDirectory, NewPath, UpdatePath).

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

%-------------------Otras funciones-----------------------
%Descripcion: Verifica si es primer elemento es una raiz o un folder
%tipo de algoritmo: No aplicada
%Dom: list
%Rec: bool
path_get_origin([Path | _]) :-
    split_string(Path, "/", "", ListPath),
    get_origin_path(ListPath, OriginPath),
    atom_chars(OriginPath, ListOriginPath),
    length_list(ListOriginPath, Length),
    Length =< 1.
    