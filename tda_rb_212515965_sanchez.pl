:- module(tda_rb_212515965_sanchez, [get_rb/2, set_rb/3, move_element_to_rb/3, update_rb/4, type_of_file/1]).
:- use_module(tda_drive_212515965_sanchez).

%-----------------------Representacion-----------------------
%Se presenta el TDA Recycle bin, el cual corresponde tal y como indica su nombre a una representacion
%de la papelera de reciclaje, el cual contiene los archivos borrados del sistema. Esta representacion
%esta dada por una serie de funciones las cuales tienen como objetivo mover archivos hacia la papelera de reciclaje.

%-------------------Selectores-----------------------
%Descripcion: Obtiene el rb del sistema
%tipo de algoritmo: No aplica
%Dom: list
%Rec: list
get_rb([_, _, _, _, Rb, _], Rb).

%-------------------Modificadores-----------------------
%Descripcion: Actualiza el sistema con el nuevo rb
%tipo de algoritmo: No aplica
%Dom: list - list
%Rec: list
set_rb(System, UpdateRb, UpdateSystem) :-
    filesystem(Name, Drives, Users, Path, _, Date, System),
    filesystem(Name, Drives, Users, Path, UpdateRb, Date, UpdateSystem).

%Descripcion: Mueve el file dado al rb
%tipo de algoritmo: No aplica
%Dom: list - string
%Rec: list
move_element_to_rb(Rb, Delfile, UpdateRb) :-
    add_directory(Rb, Delfile, UpdateRb).

%Descripcion: Acutaliza los drives borrando el file 
%tipo de algoritmo: Backtracking
%Dom: list - list - string
%Rec: list
update_rb(Drives, Path, Delfile, UpdateRb) :-
    update_rb_aux(Drives, Path, Delfile, [], UpdateRb).
    
update_rb_aux([], _, _, Acum, Acum).

update_rb_aux([Drive | RestDrive], Path, Delfile, Acum, UpdateRb) :-
    get_first_element(Drive, LetterDrive),
    get_first_element(Path, OriginPath),
    equal_elements(LetterDrive, OriginPath),
    get_first_element(Path, OriginPath),
    get_rest_list(Path, RestPath),
    \+empty_List(RestPath),
    \+type_of_file(Delfile),
    get_content_drive(Drive, ContentDrive),
    update_content_rb(ContentDrive, RestPath, Delfile, UpdateContentRb),
    add_content(Drive, UpdateContentRb, NewUpdateContentRb),
    update_rb_aux(RestDrive, Path, Delfile, [NewUpdateContentRb | Acum], UpdateRb).

update_rb_aux([Drive | RestDrive], Path, Delfile, Acum, UpdateRb) :-
    get_first_element(Drive, LetterDrive),
    get_first_element(Path, OriginPath),
    equal_elements(LetterDrive, OriginPath),
    get_first_element(Path, OriginPath),
    get_rest_list(Path, RestPath),
    \+empty_List(RestPath),
    get_content_drive(Drive, ContentDrive),
    del_file(ContentDrive, Delfile, UpdateContentRb),
    add_content(Drive, UpdateContentRb, NewUpdateContentRb),
    update_rb_aux(RestDrive, Path, Delfile, [NewUpdateContentRb | Acum], UpdateRb).

update_rb_aux([Drive | RestDrive], Path, Delfile, Acum, UpdateRb) :-
    get_first_element(Drive, LetterDrive),
    get_first_element(Path, OriginPath),
    equal_elements(LetterDrive, OriginPath),
    get_content_drive(Drive, ContentDrive),
    del_directory(ContentDrive, Delfile, UpdateContentRb),
    add_content(Drive, UpdateContentRb, NewUpdateContentRb),
    update_rb_aux(RestDrive, Path, Delfile, [NewUpdateContentRb | Acum], UpdateRb).

%Descripcion: Actualiza el contenido dado a travez del path
%tipo de algoritmo: Backtracking
%Dom: list - list - string
%Rec: list
update_content_rb(ContentDrive, RestPath, Delfile, UpdateContentRb) :-
    update_content_rb_aux(ContentDrive, RestPath, Delfile, [], UpdateContentRb).

update_content_rb_aux(Acum, [], _, _, Acum). 

update_content_rb_aux([], [_ | RestPath], Directory, Acum, UpdateContentRb) :-
    update_content_rb_aux(Acum, RestPath, Directory, [], UpdateContentRb).

update_content_rb_aux([ContentDrive | RestContentDrive], Path, Delfile, Acum, UpdateContentRb) :-
    get_first_element(ContentDrive, FolderName),
    get_first_element(Path, OriginPath),
    equal_elements(FolderName, OriginPath),
    get_rest_list(Path, RestPath),
    empty_List(RestPath),
    get_content_drive(ContentDrive, Content),
    del_directory(Content, Delfile, NewUpdateContentRb),
    add_content(ContentDrive, NewUpdateContentRb, NewNewUpdateContentRb),
    update_content_rb_aux(RestContentDrive, Path, Delfile, [NewNewUpdateContentRb | Acum], UpdateContentRb).

update_content_rb_aux([ContentDrive | RestContentDrive], Path, Delfile, Acum, UpdateContentRb) :-
    get_first_element(ContentDrive, FolderName),
    get_first_element(Path, OriginPath),
    equal_elements(FolderName, OriginPath),
    get_content_drive(ContentDrive, Content),
    get_rest_list(Path, RestPath),
    update_content_rb(Content, RestPath, Delfile, NewUpdateContent),
    add_content(ContentDrive, NewUpdateContent, NewNewUpdateContent),
    update_content_rb_aux(RestContentDrive, Path, Delfile, [NewNewUpdateContent | Acum], UpdateContentRb).

update_content_rb_aux([ContentDrive | RestContentDrive], Path, Directory, Acum, UpdateContentRb) :-
    update_content_rb_aux(RestContentDrive, Path, Directory, [ContentDrive | Acum], UpdateContentRb).

%Descripcion: Borra un directory del contenido dado
%tipo de algoritmo: Bcktracking
%Dom: list - string
%Rec: list
del_directory(ContentDrive, Delfile, UpdateContentRb) :-
    del_directory_aux(ContentDrive, Delfile, [], UpdateContentRb).

del_directory_aux([], _, Acum, Acum).

del_directory_aux([FirstFolder | RestFolder], Delfile, Acum, UpdateContentRb) :-
    get_first_element(FirstFolder, FolderName),
    equal_elements(FolderName, Delfile),
    del_directory_aux(RestFolder, Delfile, Acum, UpdateContentRb).

del_directory_aux([FirstFolder | RestFolder], Delfile, Acum, UpdateContentRb) :-
    del_directory_aux(RestFolder, Delfile, [FirstFolder | Acum], UpdateContentRb).

%Descripcion: Borra un file del contenido dado
%tipo de algoritmo: Backtracking
%Dom: list - string
%Rec: list
del_file(ContentDrive, Delfile, UpdateContentRb) :-
    del_file_aux(ContentDrive, Delfile, [], UpdateContentRb).

del_file_aux([], _, Acum, Acum).

del_file_aux([FirstContentDrive | RestContentDrive], Delfile, Acum, UpdateContentRb) :-
    get_first_element(FirstContentDrive, FolderName),
    equal_elements(FolderName, Delfile),
    del_file_aux(RestContentDrive, Delfile, Acum, UpdateContentRb).

del_file_aux([FirstContentDrive | RestContentDrive], Delfile, Acum, UpdateContentRb) :-
    del_file_aux(RestContentDrive, Delfile, [FirstContentDrive | Acum], UpdateContentRb).

%-----------------------------otras funciones-----------------
%Descripcion: Verifica que el delfile es un file o folder
%tipo de algoritmo: No aplica
%Dom: list
%Rec: bool
type_of_file(Delfile) :-
    split_string(Delfile, ".", "", List),
    length_list(List, Length),
    Length =< 1.
    