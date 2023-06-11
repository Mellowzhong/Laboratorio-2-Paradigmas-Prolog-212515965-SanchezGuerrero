filesystem(Name, Drives, User, Path, Rb, [Name, Drives, User, Path, Rb]).
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).
create_user(Name, [Name]).
set_directory(Directory, Date, [Directory, Date, [], []]).
file(NameAndType, Content, [NameAndType, Type, Content]) :-
    split_string(NameAndType, ".", "", [ _| ListType]),
    get_first_element(ListType, Type).

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
get_login_user(Users, LoginUser) :-
    get_first_element(Users, LoginUser).

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

update_drives(Drives, Path, Directory, UpdateDrives) :-
    update_drives_aux(Drives, Path, Directory, [], UpdateDrives).

update_drives_aux([], _, _, Acum, Acum).

update_drives_aux([Drive | RestDrive], Path, Directory, Acum, UpdateDrives) :-
    get_rest_list(Path, RestPath),
    \+empty_List(RestPath),
    get_content_drive(Drive, ContentDrive),
    update_content(ContentDrive, RestPath, Directory, UpdateContent),
    add_content(Drive, UpdateContent, UpdateDrive),
    update_drives_aux(RestDrive, Path, Directory, [UpdateDrive | Acum], UpdateDrives).

update_drives_aux([Drive | RestDrive], Path, Directory, Acum, UpdateDrives) :-
    get_first_element(Path, LetterPath),
    get_first_element(Drive, LetterDrive),
    equal_elements(LetterDrive, LetterPath),
    get_content_drive(Drive, ContentDrive),
    add_directory(ContentDrive, Directory, UpdateContent),
    add_content(Drive, UpdateContent, UpdateDrive),
    update_drives_aux(RestDrive, Path, Directory, [UpdateDrive | Acum], UpdateDrives).

update_content(ContentDrive, RestPath, Directory, UpdateContent) :-
    update_content_aux(ContentDrive, RestPath, Directory, [],UpdateContent).

update_content_aux(Acum, [], _, _, Acum).

update_content_aux([], [_ | RestPath], Directory, Acum, UpdateContent) :-
    update_content_aux(Acum, RestPath, Directory, [], UpdateContent).

update_content_aux([ContentDrive | RestContentDrive], Path, Directory, Acum, UpdateContent) :-
    get_first_element(ContentDrive, FolderName),
    get_first_element(Path, OriginPath),
    equal_elements(FolderName, OriginPath),
    get_rest_list(Path, RestPath),
    empty_List(RestPath),
    get_content_drive(ContentDrive, Content),
    add_directory(Content, Directory, NewContent),
    add_content(ContentDrive, NewContent, NewUpdateContent),
    update_content_aux(RestContentDrive, Path, Directory, [NewUpdateContent | Acum], UpdateContent).

update_content_aux([ContentDrive | RestContentDrive], Path, Directory, Acum, UpdateContent) :-
    get_first_element(ContentDrive, FolderName),
    get_first_element(Path, OriginPath),
    equal_elements(FolderName, OriginPath),
    get_content_drive(ContentDrive, Content),
    get_rest_list(Path, RestPath),
    update_content(Content, RestPath, Directory, NewUpdateContent),
    add_content(ContentDrive, NewUpdateContent, NewNewUpdateContent),
    update_content_aux(RestContentDrive, Path, Directory, [NewNewUpdateContent | Acum], UpdateContent).

update_content_aux([ContentDrive | RestContentDrive], Path, Directory, Acum, UpdateContent) :-
    update_content_aux(RestContentDrive, Path, Directory, [ContentDrive | Acum], UpdateContent).

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

length_path(Length) :-
    Length < 1.

length_list([], 0).

length_list([_ | Rest], Length) :- 
    length(Rest, LengthTail), 
    Length is LengthTail + 1.

equal(Element, Element).

reverse_list(Lista, ReverseList) :-
    reverse_list_aux(Lista, [], ReverseList).

reverse_list_aux([], Acum, Acum).

reverse_list_aux([Head|Cola], Acum, ReverseList) :-
    reverse_list_aux(Cola, [Head|Acum], ReverseList).