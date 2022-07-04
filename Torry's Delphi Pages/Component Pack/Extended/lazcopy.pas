{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
Cette source est seulement employée pour compiler et installer le paquet.
 }

unit lazcopy; 

interface

uses
  U_ExtFileCopy, u_traducefile, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('U_ExtFileCopy', @U_ExtFileCopy.Register); 
  RegisterUnit('u_traducefile', @u_traducefile.Register); 
end; 

initialization
  RegisterPackage('lazcopy', @Register); 
end.
