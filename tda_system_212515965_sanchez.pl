:- use_module(tda_drive_212515965_sanchez).
:- use_module(tda_path_212515965_sanchez).
:- use_module(tda_user_212515965_sanchez).
:- use_module(tda_fecha_212515965_sanchez).
:- use_module(tda_rb_212515965_sanchez).

%-----------------------Representacion-----------------------
%Se presenta el TDA System, el cual corresponde tal y como indica su nombre a una representacion
%del system, el cual contiene la fecha, el nombre del sistema, los drives,los usuarios, el path y la papelera de reciclaje. Esta representacion
%esta dada por una lista con estos elementos los cuales se ven en el constructor demonimado system y set-system.

%-------------------Constructores-----------------------
%Descripcion: Crea un nuevo sistema
%tipo de algoritmo: No aplica
%Dom: string
%Rec:list
system(SystemName, System) :-
    get_current_date_time(Date),
    filesystem(SystemName,[] ,[], [], [], Date, System).

%Descripcion: Crea un sistema con un nombre, drives, users, path, rb, date
%tipo de algoritmo: No aplica
%Dom: string - list - list - list - list - string
%Rec: list
filesystem(Name, Drives, User, Path, Rb, Date, [Name, Drives, User, Path, Rb, Date]).

%Descripcion: Crea una lista con el nombre del directorio, fecha de creacion y el usuario logeado
%tipo de algoritmo:No aplica
%Dom: string - string - string
%Rec: list
set_directory(Name, Type, Date, LoginUser, Content, [Name, Type, Date, LoginUser, Content]).

%Descripcion: Crea un file con el nombre, tipo y un contenido
%tipo de algoritmo: No aplica
%Dom: string - list
%Rec: list
file(NameAndType, Content, [NameAndType, "file", Content]).

%Descripcion: Crea un file con las caracteristicas del file pasado, la fecha de creacion y usuario logeado
%tipo de algoritmo: No aplica
%Dom: list - string - list - 
%Rec: list
set_file([NameAndType, Type, Content], Date, LoginUser, [NameAndType, Type, Date, LoginUser, Content]).

%-------------------Selectores-----------------------
%Descripcion: Obtiene el primer elemento de la lista
%tipo de algoritmo: No aplica
%Dom: list 
%Rec: string
get_first_element([Element | _], Element).

%Descripcion: Obtiene todos los nombres de un contenido dado
%tipo de algoritmo: Backtracking
%Dom: list - list
%Rec: list
get_all_names([], []) :- !.

get_all_names([First | Rest], [FirstName | Acum]) :-
    get_first_element(First, FirstName),
    get_all_names(Rest, Acum).
%-------------------Modificadores-----------------------
%Descripcion: Agrega un nuevo drive al sistema
%tipo de algoritmo: No aplica 
%Dom: list - string - string - int
%Rec:list
systemAddDrive(System, Letter, Name, Capacity, UpdateSystem) :-
    create_drive(Letter, Name, Capacity, [], NewDrive),
    get_drives(System, Drives),
    \+drive_exist(Letter, Drives),
    add_drives_to_drives(NewDrive, Drives, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%Descripcion: Registra a un usuario en el sistema
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec:list
systemRegister(System, Name, UpdateSystem) :-
    create_user(Name, NewUser), 
    get_user(System, Users),
    \+username_exist(Name, Users), 
    add_user_to_users(NewUser, Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).

%Descripcion: Logea a un usuario en el sistema
%tipo de algoritmo: No aplica
%Dom: list - user
%Rec: list
systemLogin(System, User, UpdateSystem) :-
    get_user(System, Users),
    \+login_exist(Users),
    add_login(User, Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).

%Descripcion: Desloguea a los usuarios del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
systemLogout(System, UpdateSystem) :-
    get_user(System, Users),
    systemLogoutAux(Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).

%Descripcion: Cambia el path a al drive 
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
systemSwitchDrive(System, Letter, UpdateSystem) :-
    get_user(System, Users),
    login_exist(Users),
    get_drives(System, Drives),
    drive_exist(Letter, Drives),
    switch_path(Letter, UpdateDrivePath),
    set_path(System, UpdateDrivePath, UpdateSystem).
    
%Descripcion: Agrega el directorio dado al sistema
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
systemMkdir(System, Directory, UpdateSystem) :-
    get_user(System, Users),
    login_exist(Users),
    get_drives(System, Drives),
    get_path(System, Path),
    get_current_date_time(Date),
    isLogged(Users, LoginUser),
    set_directory(Directory, "folder", Date, LoginUser, [], NewDirectory),
    update_drives(Drives, Path, NewDirectory, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%Descripcion: Cambia el path dado uno nuevo
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
systemCd(System, DirectoryCd, UpdateSystem) :-
    get_user(System, Users),
    get_path(System, Path),
    login_exist(Users),
    split_string(DirectoryCd, "/", "", ListDirectory),
    reverse_list(Path, ReversePath),
    update_path(ListDirectory, ReversePath, UpdatePath),
    reverse_list(UpdatePath, NewUpdatePath),
    set_path(System, NewUpdatePath, UpdateSystem).

%Descripcion: Agrega un file al sistema
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: No aplica
systemAddFile(System, File, UpdateSystem) :-
    get_user(System, Users),
    login_exist(Users),
    get_drives(System, Drives),
    get_path(System, Path),
    get_current_date_time(Date),
    isLogged(Users, LoginUser),
    set_file(File, Date, LoginUser,NewFile),
    update_drives(Drives, Path, NewFile, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%Descripcion: Borra un archivo o folder del sistema
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
systemDel(System, DelFile, UpdateSystem) :-
    get_path(System, Path),
    get_drives(System, Drives),
    get_from_drives(Drives, Path, DelFile, Directorys),
    update_rb(Drives, Path, DelFile, UpdateDrives),
    set_drive(System, UpdateDrives, NewUpdateSystem),
    set_rb(NewUpdateSystem, Directorys, UpdateSystem).

%Descripcion: Copia un archivo del sistema a una ruta especifica
%tipo de algoritmo: No aplica
%Dom: list - string - string - list
%Rec: list
systemCopy(System, CopyFile, TargetPath, UpdateSystem) :-
    get_drives(System, Drives),
    get_path(System, Path),
    get_from_drives(Drives, Path, CopyFile, Directorys),
    split_string(TargetPath, "/", "", ListTargetPath),
    update_drives_by_directorys(Drives, ListTargetPath, Directorys, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%Descripcion: Mueve un archivo del sistema a una ruta especifica
%tipo de algoritmo: No aplica
%Dom: list - string - string - list
%Rec: list
systemMove(System, MoveFile, TargetPath, UpdateSystem) :-
    get_path(System, Path),
    get_drives(System, Drives),    
    get_from_drives(Drives, Path, MoveFile, Directorys),
    \+empty_List(Directorys),
    split_string(TargetPath, "/", "", ListTargetPath),
    update_drives_by_directorys(Drives, ListTargetPath, Directorys, UpdateDrives),
    update_rb(UpdateDrives, Path, MoveFile, NewDrives),
    set_drive(System, NewDrives, UpdateSystem).

%Descripcion: Renombra el archivo dado con el nombre dado
%tipo de algoritmo: No aplica
%Dom: list - string - string - list
%Rec: list
systemRen(System, RenFile, NewName, UpdateSystem):-
    get_path(System, Path),
    get_drives(System, Drives),
    rename(Drives, Path, RenFile, NewName, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%Descripcion: Muestra informacion dependiendo de los parametros dados
%tipo de algoritmo: No aplica
%Dom: list - list - list
%Rec: list
systemDir(_, ActionList, String) :-
    equal_elements(ActionList, ["/?"]),
    list_to_string(["[]", "/a"], String).

systemDir(System, ActionList, String) :-
    empty_List(ActionList),
    get_path(System, Path),
    get_drives(System, Drives),
    get_from_drives(Drives, Path, "*", Directorys),
    get_all_names(Directorys, Names),
    filter_by_security(Names, FilterDirectorys),
    list_to_string(FilterDirectorys, String).

systemDir(System, ActionList, String) :-
    get_path(System, Path),
    get_drives(System, Drives),
    get_from_drives(Drives, Path, "*", Directorys),
    get_all_names(Directorys, Names),
    equal_elements(ActionList, ["/a"]),
    list_to_string(Names, String).

%Descripcion: Formatea una unidad con un nuevo nombre
%tipo de algoritmo: No aplica
%Dom: list - string - string - list
%Rec: list
systemFormat(System, Letter, NewName, UpdateSystem) :-
    get_drives(System, Drives),
    format_drive(Drives, Letter, NewName, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).
    
%-------------------Otras funciones-----------------------
%Descripcion: Auxiliar del logout
%tipo de algoritmo: Backtracking
%Dom: list
%Rec: list
systemLogoutAux(Users, UpdateUsers) :-
    systemLogoutAux_2(Users, [], UpdateUsers).

systemLogoutAux_2([], Acum, Acum).

systemLogoutAux_2([Head | Tail], Acum, UpdateUsers) :-
    string(Head),
    systemLogoutAux_2(Tail, [Head | Acum], UpdateUsers).

systemLogoutAux_2([_ | Tail], Acum, UpdateUsers) :-
    systemLogoutAux_2(Tail, Acum, UpdateUsers).

%Descripcion: Invirte una lista
%tipo de algoritmo: Backtracking
%Dom: list
%Rec: list
reverse_list(Lista, ReverseList) :-
    reverse_list_aux(Lista, [], ReverseList).

reverse_list_aux([], Acum, Acum).

reverse_list_aux([Head|Cola], Acum, ReverseList) :-
    reverse_list_aux(Cola, [Head|Acum], ReverseList).

%Descripcion: Borra el primer elemento de la lista entregada
%tipo de algoritmo: No aplica
%Dom: list
%Rec: string
del_first_element([_ | Rest], Rest).

%Descripcion: Verifica que la lista entregada sea vacia
%tipo de algoritmo: No aplica
%Dom: list
%Rec: bool
empty_List([]) :- true.

%Descripcion: Verifica si dos elementos son iguales
%tipo de algoritmo: No aplica
%Dom: list or string
%Rec: bool
equal_elements(Element, Element).

%Descripcion: Comprueba si el nombre dado tiene el atributo de hide
%tipo de algoritmo: No aplica
%Dom: string
%Rec: boolean
is_hide(Name) :-
    atom_chars(Name, ['.' | _]).

%Descripcion: Filtra los nombres para agregar obtener solos los que no sean hide
%tipo de algoritmo: Backtracking
%Dom: list - list
%Rec: list
filter_by_security([], []) :- !.

filter_by_security([First | Rest], [First | Acum]) :-
    \+is_hide(First),
    filter_by_security(Rest, Acum).

filter_by_security([_ | Rest], FilterDirectorys) :-
    filter_by_security(Rest, FilterDirectorys).

%Descripcion: Convierte una lista a un string uniendola con ","
%tipo de algoritmo: recursion
%Dom: list - string
%Rec: string
list_to_string([], '').

list_to_string([First | RestList], String) :-
    list_to_string(RestList, Rest),
    string_concat(First, ", ", NewString),
    string_concat(NewString, Rest, String).