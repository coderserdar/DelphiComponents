{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  Cette source est seulement employée pour compiler et installer le paquet.
 }

unit lazfonctions; 

interface

uses
    unite_messages, fonctions_erreurs, fonctions_db, fonctions_images, 
  fonctions_init, fonctions_numedit, fonctions_proprietes, fonctions_string, 
  fonctions_variant, fonctions_web, fonctions_array, u_zconnection, 
  fonctions_dbcomponents, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('lazfonctions', @Register); 
end.
