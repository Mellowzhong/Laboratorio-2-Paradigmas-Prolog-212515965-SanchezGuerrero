filesystem(Name, Drives, User, Path, Rb, [Name, Drives, User, Path, Rb]).
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).

get_drives(System, Drives) :-
    filesystem(_ , Drives, _, _, _, System).

add_drives_to_drives(NewDrive, Drives, UpdateDrives) :-
    append(Drives, [NewDrive], UpdateDrives).

set_drive(System, UpdateDrives, UpdateSystem) :-
    filesystem(Name, _, User, Path, Rb, System),
    filesystem(Name, UpdateDrives, User, Path, Rb, UpdateSystem).

system(SystemName, System) :-
    filesystem(SystemName,[] ,[], [], [], System).

systemAddDrive(System, Letter, Name, Capacity, UpdateSystem) :-
    create_drive(Letter, Name, Capacity, [], NewDrive),
    get_drives(System, Drives),
    add_drives_to_drives(NewDrive, Drives, UpdateDrives),
    set_drive(System, UpdateDrives, UpdateSystem).