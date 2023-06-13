:- module(tda_drive_212515965_sanchez, [create_drive/5,set_drive/3, drive_exist/2, get_drives/2, get_content_drive/2, add_directory/3, add_folder/3, add_content/3, add_drives_to_drives/3, update_drives/4, update_content/4]).
%-----------------------Representacion-----------------------
%Se presenta el TDA Drives, el cual corresponde a tal y como indica su nombre a una representacion del drive,
%el drive es el lugar donde se almacenan las unidades, las carpetas o archivos varios. Esta representacion esta dada
%por una serie de funciones las cuales tendrian el fin de encontrar o modificar cosas de los drives.

%-------------------Constructores-----------------------
%Descripcion: Crea un drive con la informacion dada
%tipo de algoritmo: No aplicada
%Dom: string - string - int - list
%Rec: list
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).

%-------------------Pertenencia-----------------------
%Descripcion: Verifica si el drive existe
%tipo de algoritmo: Backtracking
%Dom: string - list
%Rec: bool
drive_exist(_, []) :- false.

drive_exist(Letter, [[Letter |_] | _]) :- true.

drive_exist(Letter, [_ | Rest]) :-
    drive_exist(Letter, Rest).

%Descripcion: Verifica si el directorio existe
%tipo de algoritmo: Bakctracking
%Dom: list - string
%Rec: bool
directory_exist([], _) :- false.

directory_exist([FirstContentDrive | _], Directory) :-
    get_first_element(FirstContentDrive, FolderName),
    get_first_element(Directory, DirectoryName),
    equal_elements(FolderName, DirectoryName).

directory_exist([_ | RestContentDrive], Directory) :-
    directory_exist(RestContentDrive, Directory).

%-------------------Selectores-----------------------
%Descripcion: Obtiene los drives del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_drives(System, Drives) :-
    filesystem(_ , Drives, _, _, _, _, System).

%Descripcion: Obtiene el contenido del drive 
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_content_drive([_, _, _, Content], Content).
%-------------------Modificadores-----------------------
%Descripcion: Actualiza el sistema con el nuevo drive dado
%tipo de algoritmo: No aplicado
%Dom: list - list
%Rec: list
set_drive(System, UpdateDrives, UpdateSystem) :-
    filesystem(Name, _, User, Path, Rb, Date, System),
    filesystem(Name, UpdateDrives, User, Path, Rb, Date, UpdateSystem).

%Descripcion: Agrega un nuevo directorio al contenido
%tipo de algoritmo: No aplicado
%Dom: list - string
%Rec: list
add_directory(Content, Directory, [Directory | Content]).

%Descripcion: Agrega un folder a los drives 
%tipo de algoritmo: No aplica
%Dom: list - list
%Rec: list
add_folder(Directory, Drives, UpdateDrives) :-
    append(Drives, [Directory],UpdateDrives).

%Descripcion: Actualiza el sistema con el nuevo contenido
%tipo de algoritmo: No aplica
%Dom: list - list
%Rec: list
add_content(Drive, UpdateContent, UpdateDrive) :-
    create_drive(Letter, Name, Capacity, _, Drive),
    create_drive(Letter, Name, Capacity, UpdateContent, UpdateDrive).

%Descripcion: Agrega un nuevo drive a los drives
%tipo de algoritmo: No aplicada
%Dom: list - list
%Rec: list
add_drives_to_drives(NewDrive, Drives, UpdateDrives) :-
    append(Drives, [NewDrive], UpdateDrives).

%Descripcion: Agrega un el directorio dado a los drives a travez del path
%tipo de algoritmo: Backtracking
%Dom: list - list - list
%Rec: list
update_drives(Drives, Path, Directory, UpdateDrives) :-
    update_drives_aux(Drives, Path, Directory, [], UpdateDrives).

update_drives_aux([], _, _, Acum, Acum).

update_drives_aux([Drive | RestDrive], Path, Directory, Acum, UpdateDrives) :-
    get_first_element(Path, LetterPath),
    get_first_element(Drive, LetterDrive),
    equal_elements(LetterDrive, LetterPath),
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
    \+directory_exist(ContentDrive, Directory),
    add_directory(ContentDrive, Directory, UpdateContent),
    add_content(Drive, UpdateContent, UpdateDrive),
    update_drives_aux(RestDrive, Path, Directory, [UpdateDrive | Acum], UpdateDrives).

update_drives_aux([Drive | RestDrive], Path, Directory, Acum, UpdateDrives) :-
    update_drives_aux(RestDrive, Path, Directory, [Drive | Acum], UpdateDrives).
%Descripcion: Agrega el directorio al contenido del drive a travez del path
%tipo de algoritmo: Backtracking
%Dom: list - list - list
%Rec: list
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
    \+directory_exist(Content, Directory),
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

