:- use_module(tda_drive).
:- use_module(tda_path).
:- use_module(tda_user).
:- use_module(tda_fecha).
:- use_module(tda_rb).

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

%Descripcion:
%tipo de algoritmo:
%Dom:
%Rec:
add_content_to_content(NewContent, RestContentDrive, NewUpdateContent) :-
    append(NewContent, [RestContentDrive], NewUpdateContent).

system(SystemName, System) :-
    get_current_date_time(Date),
    filesystem(SystemName,[] ,[], [], [], Date, System).

systemAddDrive(System, Letter, Name, Capacity, UpdateSystem) :-
    create_drive(Letter, Name, Capacity, [], NewDrive),
    get_drives(System, Drives),
    add_drives_to_drives(NewDrive, Drives, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

systemRegister(System, Name, UpdateSystem) :-
    create_user(Name, NewUser),
    get_user(System, Users),
    \+username_exist(NewUser, Users),
    set_user(System, Users, UpdateSystem).

systemRegister(System, Name, UpdateSystem) :-
    create_user(Name, NewUser),
    get_user(System, Users),
    add_user_to_users(NewUser, Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).
    
systemLogin(System, User, UpdateSystem) :-
    get_user(System, Users),
    \+login_exist(Users),
    add_login(User, Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).

systemLogoutAux(Users, UpdateUsers) :-
    systemLogoutAux(Users, [], UpdateUsers).

systemLogoutAux([], Acum, Acum).
systemLogoutAux([Head | Tail], Acum, UpdateUsers) :-
    atom(Head),
    systemLogoutAux(Tail, Acum, UpdateUsers).

systemLogoutAux([Head | Tail], Acum, UpdateUsers) :-
    systemLogoutAux(Tail, [Head | Acum], UpdateUsers).
systemLogout(System, UpdateSystem) :-
    get_user(System, Users),
    systemLogoutAux(Users, UpdateUsers),
    set_user(System, UpdateUsers, UpdateSystem).

systemSwitchDrive(System, Letter, UpdateSystem) :-
    get_drives(System, Drives),
    drive_exist(Letter, Drives),
    add_drive_path(Letter, UpdateDrivePath),
    set_path(System, UpdateDrivePath, UpdateSystem).
    
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
    
systemCd(System, DirectoryCd, UpdateSystem) :-
    get_user(System, Users),
    get_path(System, Path),
    login_exist(Users),
    split_string(DirectoryCd, "/", "", ListDirectory),
    reverse_list(Path, ReversePath),
    update_path(ListDirectory, ReversePath, UpdatePath),
    reverse_list(UpdatePath, NewUpdatePath),
    set_path(System, NewUpdatePath, UpdateSystem).

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

systemDel(System, Delfile, UpdateSystem) :-
    get_path(System, Path),
    get_drives(System, Drives),
    update_rb(Drives, Path, Delfile, UpdateDrives),
    set_drive(System, UpdateDrives, NewUpdateSystem),
    get_rb(NewUpdateSystem, Rb),
    move_element_to_rb(Rb, Delfile, UpdateRb),
    set_rb(NewUpdateSystem, UpdateRb, UpdateSystem).

systemCopy(System, CopyFile, TargetPath, UpdateSystem) :-
    get_drives(System, Drives),
    get_path(System, Path),
    get_user(System, Users),
    get_current_date_time(Date),
    get_login_user(Users, LoginUser),
    set_directory(CopyFile, Date, LoginUser,NewDirectory),
    split_string(TargetPath, "/", "", ListDirectory),
    reverse_list(Path, ReversePath),
    update_path(ListDirectory, ReversePath, UpdatePath),
    update_drives(Drives, UpdatePath, NewDirectory, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

%-------------------Otras funciones-----------------------

length_path(Length) :-
    Length < 1.

length_list([], 0).

length_list([_ | Rest], Length) :- 
    length(Rest, LengthTail), 
    Length is LengthTail + 1.

reverse_list(Lista, ReverseList) :-
    reverse_list_aux(Lista, [], ReverseList).

reverse_list_aux([], Acum, Acum).

reverse_list_aux([Head|Cola], Acum, ReverseList) :-
    reverse_list_aux(Cola, [Head|Acum], ReverseList).

del_first_element([_ | Rest], Rest).

empty_List([]) :- true.

equal_elements(Element, Element).