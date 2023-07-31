:- module(tda_drive_212515965_sanchez, [add_drives_to_drives/3, get_content_folder/2, update_drives_by_directorys/4, compare_pattern/2, set_by_origin_path/4, set_content_drive/3, format_drive/4, rename/5, create_drive/5,set_drive/3, drive_exist/2, get_drives/2, get_drive_by_letter/3, get_from_folder/4, get_from_content/3, get_from_drives/4, get_from_specific_drive/4, get_by_name/3, get_content_drive/2, add_directory/3, update_drives/4]).
%-----------------------Representacion-----------------------
%Se presenta el TDA Drives, el cual corresponde a tal y como indica su nombre a una representacion del drive,
%el drive es el lugar donde se almacenan las unidades, las carpetas o archivos varios. Esta representacion esta dada
%por una serie de funciones las cuales tendrian el fin de encontrar o modificar cosas de los drives.

%Descripcion: Regla la cual ayudara a verificar si es un file o no
%tipo de algoritmo: No aplica
%Dom: No aplica
%Rec: No aplica
if_file("file").

%-------------------Constructores-----------------------
%Descripcion: Crea un drive con la informacion dada
%tipo de algoritmo: No aplicada
%Dom: string - string - int - list
%Rec: list
create_drive(Letter, Name, Capacity, Content, [Letter, Name, Capacity, Content]).

%Descripcion: Crea un folder con el contenido actualizado
%tipo de algoritmo: No aplicada
%Dom: Folder - List - Folder
%Rec: Folder
set_folder_content(Folder, NewContent, NewUpdateContent) :-
    set_directory(Name, Type, Date, LoginUser, _, Folder),
    set_directory(Name, Type, Date, LoginUser, NewContent, NewUpdateContent).

%Descripcion: Crea un folder con el nombre actualizado
%tipo de algoritmo: No aplicada
%Dom: Folder - List - Folder
%Rec: Folder
set_folder_name(Folder, NewName, NewFolder) :-
    set_directory(_, Type, Date, LoginUser, Content, Folder),
    set_directory(NewName, Type, Date, LoginUser, Content, NewFolder).

%Descripcion: Crea un sistema con el nuevo drive dado
%tipo de algoritmo: No aplicado
%Dom: list - list - list
%Rec: list
set_drive(System, UpdateDrives, UpdateSystem) :-
    filesystem(Name, _, User, Path, Rb, Date, System),
    filesystem(Name, UpdateDrives, User, Path, Rb, Date, UpdateSystem).

%Descripcion: Crea un drive con el nuevo contenido dado
%tipo de algoritmo: No aplicado
%Dom: list - list - list
%Rec: list
set_content_drive(Drive, UpdateContent, UpdateDrive) :-
    create_drive(Letter, Name, Capacity, _, Drive),
    create_drive(Letter, Name, Capacity, UpdateContent, UpdateDrive).

%Descripcion: Actualiza el contenido de un folder a traves de un nombre
%tipo de algoritmo: Backtracking
%Dom: list - folder - list
%Rec: list
set_by_name(_, [], []) :- !.

set_by_name([Name, Type, Date, LoginUser, Content], [[Name, _, _, _, _] | Rest], [[Name, Type, Date, LoginUser, Content] | NewContent]) :-
    set_by_name([Name, Type, Date, LoginUser, Content], Rest, NewContent).

set_by_name(NewElement, [FirstElement | Rest], [FirstElement | NewContent]):-
    set_by_name(NewElement, Rest, NewContent).

%Descripcion: Actualiza el drive a traves de una letra dada
%tipo de algoritmo: Backtracking
%Dom: list - drive - string - list
%Rec: list
set_by_origin_path([], _, _, []):- !.

set_by_origin_path([[OriginPath, _, _, _] | Rest], NewDrive, OriginPath, [NewDrive | UpdateDrives]):-
    set_by_origin_path(Rest, NewDrive, OriginPath, UpdateDrives).

set_by_origin_path([FirstDrive | Rest], NewDrive, OriginPath, [FirstDrive | UpdateDrives]) :-
    set_by_origin_path(Rest, NewDrive, OriginPath, UpdateDrives).
%-------------------Pertenencia-----------------------
%Descripcion: Verifica si el drive existe
%tipo de algoritmo: Recursion
%Dom: string - list
%Rec: bool
drive_exist(_, []) :- false.

drive_exist(Letter, [[Letter |_] | _]) :- true.

drive_exist(Letter, [_ | Rest]) :-
    drive_exist(Letter, Rest).

%Descripcion: Verifica si el directorio existe
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: bool
directory_exist(Content, Directory) :- 
    get_first_element(Directory, Name),
    get_all_names(Content, AllNames),
    member(Name, AllNames).
%-------------------Selectores-----------------------
%Descripcion: Obtiene los drives del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_drives(System, Drives) :-
    filesystem(_, Drives, _, _, _, _, System).

%Descripcion: Obtiene el tipo de un elemento
%tipo de algoritmo: No aplica
%Dom: Element - String
%Rec: String
get_type(Element,Type) :-
    set_directory(_, Type, _, _, _, Element).

%Descripcion: Obtiene el nombre de un elemento
%tipo de algoritmo: No aplica
%Dom: Element - String
%Rec: String
get_name(Element, Name) :-
    set_directory(Name, _,_, _, _, Element).

%Descripcion: Obtiene un elemento a traves del nombre dado
%tipo de algoritmo: Recursion
%Dom: List - String - Element
%Rec: Element
get_by_name([], _, _) :- false.

get_by_name([[Name, Type, Date, LoginUser, Content] | _ ], Name, [Name, Type, Date, LoginUser, Content]).

get_by_name([ _ | Rest], Name, Element) :-
  get_by_name(Rest, Name, Element).

%Descripcion: Obtiene el contenido del drive 
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_content_drive([_, _, _, Content], Content).

%Descripcion: Obtiene el contenido del folder
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_content_folder([_, _, _, _, Content], Content).

%Descripcion: Obtiene los elementos a traves de los drives
%tipo de algoritmo: No aplica
%Dom: List - String - List - List
%Rec: List
get_from_drives(Drives, [OriginPath | RestPath], Pattern, Directorys) :-
    get_drive_by_letter(Drives, OriginPath, Drive),
    get_from_specific_drive(Drive, RestPath, Pattern, Directorys).

%Descripcion: Obtiene los elementos a traves de un drive especifico
%tipo de algoritmo: No aplica
%Dom: List - String - List - List
%Rec: List
get_from_specific_drive(Drive, Path, Pattern, Directorys) :-
    empty_List(Path),
    get_content_drive(Drive, ContentDrive),
    get_from_content(ContentDrive, Pattern, Directorys).
  
get_from_specific_drive(Drive, [Head | Tail], Pattern, Directorys) :-
    get_content_drive(Drive, ContentDrive),
    get_by_name(ContentDrive, Head, Element),
    get_from_folder(Element, Tail, Pattern, Directorys).
    
%Descripcion: Obtiene los elementos a traves de un folder
%tipo de algoritmo: Recursion
%Dom: List - String - List - List
%Rec: List
get_from_folder(Element, [], Pattern, Directorys) :-
    get_content_folder(Element, Content),
    get_from_content(Content, Pattern, Directorys).
  
get_from_folder(Element, [Head | Rest], Pattern, Directorys) :-
    get_content_folder(Element, Content),
    get_by_name(Content, Head, GetterFolder),
    get_from_folder(GetterFolder, Rest, Pattern, Directorys).

%Descripcion: Obtiene los elementos que cumplan cierto patron en especifico
%tipo de algoritmo: Backtracking
%Dom: List - String - List
%Rec: List
get_from_content([], _, []):- !.

get_from_content([Element | Rest], Pattern, [Element | Acum]) :-
  compare_pattern(Element, Pattern),
  get_from_content(Rest, Pattern, Acum).

get_from_content([_ | Rest], Pattern, Acum) :-
  get_from_content(Rest, Pattern, Acum).

%Descripcion: Obtiene un drive a traves de la letra dada
%tipo de algoritmo: Recursion
%Dom: List - String - List
%Rec: List
get_drive_by_letter([[Letter, Name, Capacity, Content] | _], Letter, Drive) :-
  create_drive(Letter, Name, Capacity, Content, Drive).

get_drive_by_letter([_ | Rest], Letter, Drive) :-
  get_drive_by_letter(Rest, Letter, Drive).
%-------------------Modificadores-----------------------
%Descripcion: Agrega un nuevo drive a los drives
%tipo de algoritmo: No aplicada
%Dom: list - list
%Rec: list
add_drives_to_drives(NewDrive, Drives, UpdateDrives) :-
    append(Drives, [NewDrive], UpdateDrives).

%Descripcion: Agrega un nuevo directorio al contenido
%tipo de algoritmo: No aplicado
%Dom: list - string
%Rec: list
add_directory(Content, Directory, NewContent) :- 
    get_type(Directory, Type),
    if_file(Type),
    directory_exist(Content, Directory),
    set_by_name(Directory, Content, NewContent).

add_directory(Content, Directory, NewContent) :- 
    get_type(Directory, Type),
    if_file(Type),
    append([Directory], Content, NewContent).

add_directory(Content, Directory, NewContent) :- 
    \+ directory_exist(Content, Directory),
    append([Directory], Content, NewContent).

%Descripcion: Agrega un el directorio dado a los drives a traves del path
%tipo de algoritmo: Backtracking
%Dom: list - list - list
%Rec: list
update_drives(Drives, [OriginPath | RestPath], Directory, UpdateDrives):-
    get_drive_by_letter(Drives, OriginPath, Drive),
    add_directory_to_drive(Drive, RestPath, Directory, NewDrive),
    set_by_origin_path(Drives, NewDrive, OriginPath, UpdateDrives).

%Descripcion: Agrega unos directorios a los drives a traves del path
%tipo de algoritmo: Backtracking
%Dom: list - list - list
%Rec: list
update_drives_by_directorys(Drives, [OriginPath | Rest], Directorys, UpdateDrives) :-
    get_drive_by_letter(Drives, OriginPath, Drive),
    add_directorys_by_specific_drive(Drive, Rest, Directorys, NewDrive),
    set_by_origin_path(Drives, NewDrive, OriginPath, UpdateDrives).

%Descripcion: Agrega un nuevo directorio a un drive a traves de un path dado
%tipo de algoritmo: No aplica
%Dom: List - list - String - List
%Rec: List
add_directory_to_drive(Drive, [], Directory, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    add_directory(ContentDrive, Directory, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

add_directory_to_drive(Drive, [First | Rest], Directory, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    get_by_name(ContentDrive, First, SubGetterFolder),
    add_by_rest_path(SubGetterFolder, Rest, Directory, NewFolder),
    set_by_name(NewFolder, ContentDrive, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

%Descripcion: Agrega un nuevo directorio a tarves de un path dado
%tipo de algoritmo: No aplica
%Dom: List - list - String - List
%Rec: List
add_by_rest_path(GetterFolder, [], Directory, NewFolder) :-
    get_content_folder(GetterFolder, ContentFolder),
    add_directory(ContentFolder, Directory, NewFolderContent),
    set_folder_content(GetterFolder, NewFolderContent, NewFolder).

add_by_rest_path(GetterFolder, [First | Rest], Directory, NewFolder) :-
    get_content_folder(GetterFolder, ContentFolder),
    get_by_name(ContentFolder, First, SubGetterFolder),
    add_by_rest_path(SubGetterFolder, Directory, Rest, NewGetterFolder),
    set_by_name(NewGetterFolder, ContentFolder, NewContent),
    set_folder_content(GetterFolder, NewContent, NewFolder).

%Descripcion: Agrega unos directorios a un drive especifico a traves del path
%tipo de algoritmo: No aplica
%Dom: list - list - list
%Rec: list
add_directorys_by_specific_drive(Drive, [], Directorys, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    add_directorys(ContentDrive, Directorys, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

add_directorys_by_specific_drive(Drive, [First | Rest], Directorys, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    get_by_name(ContentDrive, First, Folder),
    add_directorys_by_folder(Folder, Rest, Directorys, NewFolder),
    set_by_name(NewFolder, ContentDrive, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

%Descripcion: Agrega los directorios al contenido
%tipo de algoritmo: Backtracking
%Dom: list - list - list
%Rec: list
add_directorys(ContentDrive, [], ContentDrive).

add_directorys(ContentDrive, [Element | Rest], NewContent) :-
    append_directory(ContentDrive, Element, NewInnerContent),
    add_directorys(NewInnerContent, Rest, NewContent).

%Descripcion: Agrega unos directorios a un folder a traves del path
%tipo de algoritmo: No aplica
%Dom: list - list - string - list
%Rec: list
add_directorys_by_folder(Folder, [], Directorys, NewFolder) :-
    get_content_folder(Folder, Content),
    add_directorys(Content, Directorys, NewContent),
    set_folder_content(Folder, NewContent, NewFolder).

add_directorys_by_folder(Folder, [First | Rest], Directorys, NewFolder) :-
    get_content_folder(Folder, Content),
    get_by_name(Content, First, GetterFolder),
    add_directorys_by_folder(GetterFolder, Rest, Directorys, NewGetterFolder),
    set_by_name(NewGetterFolder, Content, NewContent),
    set_folder_content(Folder, NewContent, NewFolder).

%Descripcion: Renombra un archivo a traves de los drives
%tipo de algoritmo: No aplica
%Dom: list - list - string - string - list
%Rec: list
rename(Drives, [OriginPath | RestPath], RenFile, NewName, UpdateDrives) :-
    get_drive_by_letter(Drives, OriginPath, Drive),
    rename_from_drive(Drive, RestPath, RenFile, NewName, NewDrive),
    set_by_origin_path(Drives, NewDrive, OriginPath, UpdateDrives).

%Descripcion: Renombra un archivo a traves de un drive especifico
%tipo de algoritmo: No aplica
%Dom: list - list - string - string - list
%Rec: list
rename_from_drive(Drive, [], RenFile, NewName,  NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    rename_from_drive_content(ContentDrive, RenFile, NewName, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

rename_from_drive(Drive, [First | Rest], RenFile, NewName, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    get_by_name(ContentDrive, First, Folder),
    rename_from_folder(Folder, Rest, RenFile, NewName, NewFolder),
    set_by_name(NewFolder, ContentDrive, NewContent),
    set_content_drive(Drive, NewContent, NewDrive).

%Descripcion: Renonmbra el elemento buscado dentro del contenido
%tipo de algoritmo: Backtracking
%Dom: list - string - string - list
%Rec: list
rename_from_drive_content([], _, _, []):- !.

rename_from_drive_content([First | Rest], RenFile, NewName, [NewElement | Acum]) :-
    get_first_element(First, Name),
    equal_elements(RenFile, Name),
    set_folder_name(First, NewName, NewElement),
    rename_from_drive_content(Rest, RenFile, NewName, Acum).

rename_from_drive_content([First | Rest], RenFile, NewName, [First | Acum]) :-
    rename_from_drive_content(Rest, RenFile, NewName, Acum).

%Descripcion: Renonmbra el elemento buscado dentro del folder a tarvez del path
%tipo de algoritmo: No aplica
%Dom: folder - list - string - string - list
%Rec: list
rename_from_folder(Folder, [], RenFile, NewName, NewFolder) :-
    get_content_folder(Folder, Content),
    rename_from_drive_content(Content, RenFile, NewName, NewContent),
    set_folder_content(Folder, NewContent, NewFolder).

rename_from_folder(Folder, [First | Rest], RenFile, NewName, NewFolder) :-
    get_content_folder(Folder, Content),
    get_by_name(Content, First, GetterFolder),
    rename_from_folder(GetterFolder, RenFile, NewName, Rest, NewGetterFolder),
    set_by_name(NewGetterFolder, Content, NewContent),
    set_folder_content(Folder, NewContent, NewFolder).

%Descripcion: Formatea un el drive de la letra dada cambiando el nombre al nuevo dado
%tipo de algoritmo: No aplica
%Dom: List - String - String - List
%Rec: List
format_drive(Drives, Letter, NewName, UpdateDrives) :-
    get_drive_by_letter(Drives, Letter, Drive),
    format_element(Drive, NewName, FormatedDrive),
    set_by_origin_path(Drives, FormatedDrive, Letter, UpdateDrives).

%Descripcion: Formatea el drive con el nuevo nombre
%tipo de algoritmo: No aplica
%Dom: List - String - List
%Rec: List
format_element(Drive, NewName, FormatedDrive) :-
    create_drive(Letter, _, Capacity, _, Drive),
    create_drive(Letter, NewName, Capacity, [], FormatedDrive).
%-------------------Otras funciones-----------------------
%Descripcion: Compara si dos listas son iguales
%tipo de algoritmo: Recursion
%Dom: List - List
%Rec: boolean
compare([], []).

compare([Char | Word], [Char | Pattern]) :-
    compare(Word, Pattern).

compare(_, [*]).

%Descripcion: Compara si el nombre de elemento cumple con algun patron especifico.
%tipo de algoritmo: No aplica
%Dom: Element - String
%Rec: boolean
compare_pattern(_, "*").

compare_pattern(Element, Pattern) :-
    get_type(Element, Type),
    if_file(Type),
    get_name(Element, Name),
    split_string(Name, ".", "", [Filename, Extension]),
    split_string(Pattern, ".", "", [FilenamePattern, ExtensionPattern]),
    atom_chars(FilenamePattern, FilePatternChars),
    atom_chars(Filename, NameFileChars),
    compare(NameFileChars, FilePatternChars),
    atom_chars(ExtensionPattern, ExtensionPatternChars),
    atom_chars(Extension, TypeChars),
    compare(TypeChars, ExtensionPatternChars).

compare_pattern(Element, Pattern) :-
    get_type(Element, Type),
    \+if_file(Type),
    get_name(Element, Name),
    split_string(Name, ".", "", [NameFile]),
    split_string(Pattern, ".", "", [FilePattern]),
    atom_chars(FilePattern, FilePatternChars),
    atom_chars(NameFile, NameFileChars),
    compare(NameFileChars, FilePatternChars).

%Descripcion: Verfica si se debe agregar o reemplazar el directorio dado
%tipo de algoritmo: No aplica
%Dom: list - list - list
%Rec: list
append_directory(ContentDrive, Element, NewContent) :-
    directory_exist(ContentDrive, Element),
    set_by_name(Element, ContentDrive, NewContent).

append_directory(ContentDrive, Element, NewContent) :-
    append([Element], ContentDrive, NewContent).