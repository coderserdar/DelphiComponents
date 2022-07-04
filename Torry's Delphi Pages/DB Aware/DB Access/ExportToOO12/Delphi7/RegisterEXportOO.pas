unit RegisterEXportOO;

interface

uses
  DesignEditors,Classes,Dialogs,Forms,FileCtrl,DesignIntf,ExportOOSheet,CO_ExportOOSheet;


implementation

procedure Register;
begin
  RegisterComponents('My Components', [TExportOOSheet]);
  RegisterComponentEditor(TExportOOSheet, TExportOOEditor);
end;


end.
