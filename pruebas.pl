filesystem(Name, Drives, User, Path, Rb, [Name, Drives, User, Path, Rb]).
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).
create_user(Name, [Name]).
set_directory(Directory, Date, [Directory, Date, [], []]).

get_content_drive([_, _, _, Content], Content).
get_current_date_time(Date) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(date, DateTime, Date).
get_drives(System, Drives) :-
    filesystem(_ , Drives, _, _, _, System).
get_user(System, Users) :-
    filesystem(_, _, Users, _, _, System).
get_path(System, Path) :-
    filesystem(_, _, _, Path, _, _, System).
get_origin_path([Letter | _], Letter).

username_exist(NewUser, Users) :-
    member(NewUser, [Users]).

drive_exist(_, []) :- false.

drive_exist(Letter, [[Letter |_] | _]).

drive_exist(Letter, [_ | Rest]) :-
    drive_exist(Letter, Rest).

login_exist([true | _]).

login_exist([_ | Rest]) :-
    login_exist(Rest).   

add_content(Drive, UpdateContent, UpdateDrive) :-
    create_drive(Letter, Name, Capacity, _, Drive),
    create_drive(Letter, Name, Capacity, UpdateContent, UpdateDrive).

add_directory(Content, Directory, [Directory | Content]).

add_drive_path(Letter, [Letter]).

add_login(User, [User| Rest], [User, true | Rest]).

add_login(User, [OtherUser | Rest], [OtherUser, UpdateUsers]) :-
    add_login(User, Rest, UpdateUsers).

add_drives_to_drives(NewDrive, Drives, UpdateDrives) :-
    append(Drives, [NewDrive], UpdateDrives).

add_user_to_users(NewUser, Users, UpdateUsers) :-
    append(Users, NewUser, UpdateUsers).

set_drive(System, UpdateDrives, UpdateSystem) :-
    filesystem(Name, _, User, Path, Rb, System),
    filesystem(Name, UpdateDrives, User, Path, Rb, UpdateSystem).

set_user(System, UpdateUsers, UpdateSystem) :-
    filesystem(Name, Drives, _, Path, Rb, System),
    filesystem(Name, Drives, UpdateUsers, Path, Rb, UpdateSystem).

set_path(System, NewPath, UpdateSystem):-
    filesystem(Name, Drives, User, _, Rb, Date, System),
    filesystem(Name, Drives, User, NewPath, Rb, Date, UpdateSystem).

system(SystemName, System) :-
    filesystem(SystemName,[] ,[], [], [], System).

systemAddDrive(System, Letter, Name, Capacity, UpdateSystem) :-
    create_drive(Letter, Name, Capacity, [], NewDrive),
    get_drives(System, Drives),
    add_drives_to_drives(NewDrive, Drives, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

systemRegister(System, Name, UpdateSystem) :-
    create_user(Name, NewUser),
    get_user(System, Users),
    add_user_to_users(NewUser, Users, UpdateUsers),
    username_exist(NewUser, Users),
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
    get_drives(System, Drives),
    get_path(System, Path),
    length_list(Path, Length),
    length_path(Length),
    set_drive(System, Drives, UpdateSystem).

systemMkdir(System, Directory, UpdateSystem) :-
    get_drives(System, Drives),
    get_path(System, Path),
    update_drives(Drives, Path, Directory, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).

update_drives(Drives, Path, Directory, UpdateDrives):-
    update_drives_aux(Drives, Path, Directory, [], UpdateDrives).

update_drives_aux([], _, _, Acum, Acum).

update_drives_aux([Drive | Rest], Path, Directory, Acum, UpdateDrives) :-
    get_origin_path(Path, OriginPath),
    equal(Drive, OriginPath),
    get_content_drive(Drive, ContentDrive),
    add_directory(ContentDrive, Directory, UpdateContent),
    add_content(Drive, UpdateContent, UpdateDrive),
    add_drives_to_drives(UpdateDrive, Rest, UpdateDrives).

update_drives_aux([Drive | Rest], Path, Directory, Acum, UpdateDrives) :-
    update_drives_aux(Rest, Path, Directory, [Drive | Acum], UpdateDrives).

length_path(Length) :-
    Length < 1.

length_list([], 0).

length_list([_ | Rest], Length) :- 
    length(Rest, LengthTail), 
    Length is LengthTail + 1.

equal(Element, Element).