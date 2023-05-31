filesystem(Name, Drives, User, Path, Rb, [Name, Drives, User, Path, Rb]).
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).
create_user(Name, [Name]).

get_drives(System, Drives) :-
    filesystem(_ , Drives, _, _, _, System).
get_user(System, Users) :-
    filesystem(_, _, Users, _, _, System).

username_exist(NewUser, Users) :-
    member(NewUser, [Users]).

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
    (username_exist(NewUser, Users) ->
        set_user(System, Users, UpdateSystem)
        ;
        set_user(System, UpdateUsers, UpdateSystem)).