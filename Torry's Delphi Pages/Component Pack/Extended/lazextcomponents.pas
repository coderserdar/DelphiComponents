{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  Cette source est seulement employée pour compiler et installer le paquet.
 }

unit lazextcomponents; 

interface

uses
    U_OnFormInfoIni, PDBCheck, PCheck, U_ExtColorCombos, u_regfwbuttons, 
  u_buttons_appli, u_extcomponent, u_framework_components, U_DBListView, 
  U_ExtNumEdits, u_regframeworkcomponents, U_ExtDBNavigator, U_FormMainIni, 
  U_ExtDBImage, u_framework_dbcomponents, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('PDBCheck', @PDBCheck.Register); 
  RegisterUnit('PCheck', @PCheck.Register); 
  RegisterUnit('u_regfwbuttons', @u_regfwbuttons.Register); 
  RegisterUnit('u_regframeworkcomponents', @u_regframeworkcomponents.Register); 
end; 

initialization
  RegisterPackage('lazextcomponents', @Register); 
end.
