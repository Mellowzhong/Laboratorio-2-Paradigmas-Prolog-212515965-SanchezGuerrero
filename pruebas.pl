% Caso que debe retornar true:
% Creando un sistema, con el disco C, dos usuarios: “user1” y “user2”, 
% se hace login con "user1”, se utiliza el disco "C", se crea la carpeta “folder1”, 
% “folder2”, se cambia al directorio actual “folder1", 
% se crea la carpeta "folder11" dentro de "folder1", 
% se hace logout del usuario "user1", se hace login con “user2”, 
% se crea el archivo "foo.txt" dentro de “folder1”, se acceder a la carpeta c:/folder2, 
% se crea el archivo "ejemplo.txt" dentro de c:/folder2
system("newSystem", S1), systemAddDrive(S1, "C", "OS", 10000000000, S2), systemRegister(S2, "user1", S3), systemRegister(S3, "user2", S4), systemLogin(S4, "user1", S5), systemSwitchDrive(S5, "C", S6), systemMkdir(S6, "folder1", S7), systemMkdir(S7, "folder2", S8), systemCd(S8, "folder1", S9), systemMkdir(S9, "folder11", S10), systemLogout(S10, S11), systemLogin(S11, "user2", S12), file( "foo.txt", "hello world", F1), systemAddFile(S12, F1, S13), systemCd(S13, "/folder2", S14),  file( "ejemplo.txt", "otro archivo", F2), systemAddFile(S14, F2, S15).

% Casos que deben retornar false:
% si se intenta añadir una unidad con una letra que ya existe
system("newSystem", S1), systemRegister(S1, "user1", S2), systemAddDrive(S2, "C", "OS", 1000000000, S3), systemAddDrive(S3, "C", "otra etiqueta", 1000000000, S4).

% si se intenta hacer login con otra sesión ya iniciada por este usuario u otro
system("newSystem", S1), systemRegister(S1, "user1", S2), systemRegister(S2, "user2", S3), systemLogin(S3, "user1", S4), systemLogin(S4, "user2", S5).

% si se intenta usar una unidad inexistente
system("newSystem", S1), systemRegister(S1, "user1", S2), systemLogin(S2, "user1", S3), systemSwitchDrive(S3, "K", S4).


%Script de pruebas propio
%Falla debido a que no se "logeo".
system("system", S1), systemAddDrive(S1, "D", "OS", 1000, S2), systemRegister(S2 , "user1", S3), systemSwitchDrive(S3, "D", S4).

%Falla debido a que se intenta "logear" mientras hay un usuario "logeado"
system("system", S1), systemAddDrive(S1, "D", "OS", 1000, S2), systemAddDrive(S2, "C", "OS", 2000, S3), systemRegister(S3, "user1", S4), systemRegister(S4, "user2", S5), systemLogin(S5, "user2", S6), systemLogin(S6, "user1", S7).

%Falla debido a que se intento agregar un drive cuando se "deslogueo"
system("system", S1), systemAddDrive(S1, "D", "OS", 1000, S2), systemAddDrive(S2, "C", "OS", 2000, S3), systemRegister(S3, "user1", S4), systemRegister(S4, "user2", S5), systemLogin(S5, "user2", S6), systemSwitchDrive( S6, "D", S7), systemLogout(S7, S8), systemSwitchDrive(S8, "C", S9).

%Caso en que no falla:
system("system", S1), systemAddDrive(S1, "D", "OS", 1000, S2), systemAddDrive(S2, "C", "OS2", 200, S3), systemRegister(S3, "user1", S4), systemRegister(S4, "user2", S5), systemLogin(S5, "user2", S6), systemSwitchDrive(S6, "D", S7), systemLogout(S7, S8), systemLogin(S8, "user1", S9), systemMkdir(S9, "folder1", S10), 
systemSwitchDrive(S10, "C", S11), systemMkdir(S11, "folder2", S12), systemRegister(S12, "user3", S13), systemLogout(S13, S14), systemLogin(S14, "user3", S15), systemCd(S15, "folder2", S16), file("foo.txt" , "hola mundo", F1), systemAddFile(S16, F1, S17), systemMkdir(S17, "folder3", S18), systemAddDrive(S18, "A", "OS3", 30000, S19), systemLogout(S19, S20), 
systemLogin(S20, "user3", S21), systemSwitchDrive(S21, "A", S22), systemMkdir(S22, "folder4", S23), systemCd(S23, "folder4", S25), file("text.txt", "hace frio", F2), systemAddFile(S25, F2, S26), systemCd(S26, "/", S27), systemMkdir(S27, "folder5", S28), file("mensaje.txt", "hay contenido", F3), systemAddFile(S28, F3, S29), systemCd(S29, "./folder4", S30),
 systemMkdir(S30, "folder6", S31), systemCd(S31, ".." , S32), systemDel(S32, "folder5", S33), systemCopy(S33, "folder4", "C", S34), systemDel(S34, "folder4", S35), systemFormat(S35, "A", "Formated",S36), systemSwitchDrive(S36, "C",S37), systemFormat(S37, "C", "NewOs", S38), file("move.txt", "contenido", F4), systemAddFile(S38, F4, S39), systemMkdir(S39, "folderMove", S40),
 file(".move.jpg", "nuevamente contenido", F5), systemAddFile(S40, F5, S41), systemDir(S41, [], String1), systemRen(S41, ".move.jpg", "move.jpg", S42), systemDir(S42, [], String2), systemMove(S42, "move.jpg", "C/folderMove", S43), systemRen(S43, "folderMove", ".folder", S44), systemDir(S44, ["/a"], String3), systemDir(S44, ["/?"], String4), systemSwitchDrive(S44, "D", S45), systemMkdir(S45, "folderMove", S46), systemMove(S46, "folderMove", "D/folder1", S47), file("text.txt", "voy a moverlo a foldermove", F6), file("mellow.txt","voy a moverlo a foldermove",  F7), systemAddFile(S47, F6, S48), systemAddFile(S48, F7, S49), systemMove(S49, "*.txt", "D/folder1/folderMove", S50), 
systemRen(S50, "folder1", "folderFinal", S51), systemDir(S51, [], String5), systemCopy(S51, "folderFinal", "C", S52), systemDir(S52, ["/a"], String6), systemFormat(S52, "C", "Formated", S53), systemCopy(S53, "folderFinal", "A", S54), systemDel(S54, "folderFinal",S55).