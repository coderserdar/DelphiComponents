{ Этот файл был автоматически создан Lazarus. Не редактировать!
Исходный код используется только для компиляции и установки пакета.
 }

unit dcl_fb_id_dataset; 

interface

uses
  fb_ib_edt_ins_master_field_unit, fbcustomdatasetpropeditorl, 
    fbcustomdatasetautoupdateoptionseditorl, fbcustomdatasetsqleditorl, 
    FBCustomDataSetSQLEditorTestL, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('fbcustomdatasetpropeditorl', 
    @fbcustomdatasetpropeditorl.Register); 
end; 

initialization
  RegisterPackage('dcl_fb_id_dataset', @Register); 
end.
