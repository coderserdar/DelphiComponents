{ Этот файл был автоматически создан Lazarus. Не редактировать!
Исходный код используется только для компиляции и установки пакета.
 }

unit UIBEditors; 

interface

uses
  jvuibdatabaseedit, jvuibtransactionedit, laz_editors_register, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('laz_editors_register', @laz_editors_register.Register); 
end; 

initialization
  RegisterPackage('UIBEditors', @Register); 
end.
