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
%Descripcion: Crea un sistema con un nombre, drives, users, path, rb, date
%tipo de algoritmo: No aplica
%Dom: string - list - list - list - list - string
%Rec: list
filesystem(Name, Drives, User, Path, Rb, Date, [Name, Drives, User, Path, Rb, Date]).

%Descripcion: Crea una lista con el nombre del directorio, fecha de creacion y el usuario logeado
%tipo de algoritmo:No aplica
%Dom: string - string - string
%Rec: list
set_directory(Directory, Date, LoginUser, [Directory, Date, LoginUser, []]).

%Descripcion: Crea un file con el nombre, tipo y un contenido
%tipo de algoritmo: No aplica
%Dom: string - list
%Rec: list
file(NameAndType, Content, [NameAndType, Type, Content]) :-
    split_string(NameAndType, ".", "", [ _| ListType]),
    get_first_element(ListType, Type).

%Descripcion: Crea un file con las caracteristicas del file pasado, la fecha de creacion y usuario logeado
%tipo de algoritmo: No aplica
%Dom: list - string - list - 
%Rec: list
set_file(File, Date, LoginUser, List) :-
    append(File, [LoginUser, Date], List).

%-------------------Selectores-----------------------
%Descripcion: Obtiene el primer elemento de la lista
%tipo de algoritmo: No aplica
%Dom: list 
%Rec: string
get_first_element([Element | _], Element).

%Descripcion: Obtiene el resto de una lista
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_rest_list([_ |Rest], Rest).
%-------------------Modificadores-----------------------
%Descripcion: Agrega un nuevo contenido al anterior
%tipo de algoritmo: No aplicado
%Dom: list - list
%Rec: list
add_content_to_content(NewContent, RestContentDrive, NewUpdateContent) :-
    append(NewContent, [RestContentDrive], NewUpdateContent).

%Descripcion: Crea un nuevo sistema
%tipo de algoritmo: No aplica
%Dom: string
%Rec:list
system(SystemName, System) :-
    get_current_date_time(Date),
    filesystem(SystemName,[] ,[], [], [], Date, System).

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
    get_login_user(Users, LoginUser),
    set_directory(Directory, Date, LoginUser,NewDirectory),
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
systemAddFile(System, _, UpdateSystem) :-
    get_user(System, Users),
    login_exist(Users),
    get_drives(System, Drives),
    get_path(System, Path),
    length_list(Path, Length),
    length_path(Length),
    set_drive(System, Drives, UpdateSystem).

systemAddFile(System, File, UpdateSystem) :-
    get_user(System, Users),
    login_exist(Users),
    get_drives(System, Drives),
    get_path(System, Path),
    get_current_date_time(Date),
    get_login_user(Users, LoginUser),
    set_file(File, Date, LoginUser,NewFile),
    update_drives(Drives, Path, NewFile, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

systemAddFile(System, _, UpdateSystem) :-
    get_drives(System, Drives),
    set_drive(System, Drives, UpdateSystem).

%Descripcion: Borra un archivo o folder del sistema
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
systemDel(System, Delfile, UpdateSystem) :-
    get_path(System, Path),
    get_drives(System, Drives),
    update_rb(Drives, Path, Delfile, UpdateDrives),
    set_drive(System, UpdateDrives, NewUpdateSystem),
    get_rb(NewUpdateSystem, Rb),
    move_element_to_rb(Rb, Delfile, UpdateRb),
    set_rb(NewUpdateSystem, UpdateRb, UpdateSystem).

%Descripcion: Copia un archivo del sistema
%tipo de algoritmo: No aplica
%Dom: list - string - string
%Rec: list
systemCopy(System, CopyFile, TargetPath, UpdateSystem) :-
    get_drives(System, Drives),
    get_path(System, Path),
    get_user(System, Users),
    get_current_date_time(Date),
    get_login_user(Users, LoginUser),
    set_directory(CopyFile, Date, LoginUser, NewDirectory),
    split_string(TargetPath, "/", "", ListDirectory),
    reverse_list(Path, ReversePath),
    update_path(ListDirectory, ReversePath, UpdatePath),
    update_drives(Drives, UpdatePath, NewDirectory, UpdateDrives),
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

%Descripcion: Verifica que el largo dado sea menos a uno
%tipo de algoritmo: No aplica
%Dom: int
%Rec: bool
length_path(Length) :-
    Length < 1.

%Descripcion: Entrega el largo de una lista
%tipo de algoritmo: Backtracking
%Dom: list
%Rec: int
length_list([], 0).

length_list([_ | Rest], Length) :- 
    length(Rest, LengthTail), 
    Length is LengthTail + 1.

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