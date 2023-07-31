:- module(tda_rb_212515965_sanchez, [set_rb/3, update_rb/4]).
:- use_module(tda_drive_212515965_sanchez).

%-----------------------Representacion-----------------------
%Se presenta el TDA Recycle bin, el cual corresponde tal y como indica su nombre a una representacion
%de la papelera de reciclaje, el cual contiene los archivos borrados del sistema. Esta representacion
%esta dada por una serie de funciones las cuales tienen como objetivo mover archivos hacia la papelera de reciclaje.
%-------------------Constructores-----------------------
%Descripcion: Actualiza el sistema con el nuevo rb
%tipo de algoritmo: No aplica
%Dom: list - list
%Rec: list
set_rb(System, UpdateRb, UpdateSystem) :-
    filesystem(Name, Drives, Users, Path, _, Date, System),
    filesystem(Name, Drives, Users, Path, UpdateRb, Date, UpdateSystem).
%-------------------Modificadores-----------------------
%Descripcion: Acutaliza los drives borrando el file 
%tipo de algoritmo: No aplica
%Dom: list - list - string - list
%Rec: list
update_rb(Drives, [OriginPath | Rest], DelFile, UpdateRb) :-
    get_drive_by_letter(Drives, OriginPath, Drive),
    update_rb_from_specific_drive(Drive, Rest, DelFile, NewDrive),
    set_by_origin_path(Drives, NewDrive, OriginPath, UpdateRb).

%Descripcion: Acutaliza un drive especifico borrando el file 
%tipo de algoritmo: No aplica
%Dom: list - list - string - list
%Rec: list
update_rb_from_specific_drive(Drive, [], DelFile, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    remove_directories(ContentDrive , DelFile, UpdatedContentDrive),
    set_content_drive(Drive, UpdatedContentDrive, NewDrive).

update_rb_from_specific_drive(Drive, [First | Rest], DelFile, NewDrive) :-
    get_content_drive(Drive, ContentDrive),
    get_by_name(ContentDrive, First, GetterFolder),
    delete_from_folder(GetterFolder, Rest, DelFile, NewGetterFolder),
    set_by_name(ContentDrive, NewGetterFolder, NewContent),
    set_content_drive(ContentDrive, NewContent, NewDrive). 

%Descripcion: Borra el file de la carpeta a traves de un path
%tipo de algoritmo: Recursion
%Dom: list - list - string - list
%Rec: list
delete_from_folder(GetterFolder, [], DelFile, SetterFolder) :-
    get_content_folder(GetterFolder, Content),
    remove_directories(DelFile, Content, NewContent),
    set_folder_content(Content, NewContent, SetterFolder).

delete_from_folder(GetterFolder, [First | Rest], DelFile, SetterFolder) :-
    get_content_folder(GetterFolder, Content),
    get_by_name(Content, First, SubGetterFolder),
    delete_from_folder(SubGetterFolder, Rest, DelFile, NewSubGetterFolder),
    set_by_name(Content, NewSubGetterFolder, NewContent),
    set_folder_content(Content, NewContent, SetterFolder).

%Descripcion: Borra un directory del contenido dado
%tipo de algoritmo: Bcktracking
%Dom: list - string
%Rec: list
remove_directories([], _, []).

remove_directories([Element | Rest], Pattern, [Element | NewList]) :-
  \+compare_pattern(Element, Pattern),
  remove_directories(Rest, Pattern, NewList).

remove_directories([_ | Rest], Pattern, NewList) :-
    remove_directories(Rest, Pattern, NewList).
    