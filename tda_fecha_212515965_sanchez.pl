:- module(tda_fecha_212515965_sanchez, [get_current_date_time/1]).

%-----------------------Representacion-----------------------
%Se presenta el TDA Fecha el cual corresponde tal y como indica su nombre a una representación
%de fecha en forma estructurada. Esta representacón esta dada por una serie de funciones las
%cuales tienen como objetivo obtener la fecha actual.
%-------------------Selectores-----------------------
%Descripcion: Obtiene la fecha actual
%tipo de algoritmo: No aplicada
%Dom: string
%Rec: string
get_current_date_time(Date) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(date, DateTime, Date).