unit DTablesEditor;

interface

uses
  Windows, Classes, Messages, Dialogs, SysUtils,
  {$IFDEF MSWINDOWS}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  DTables;


type
  TDTableNameProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

procedure Register;
begin
     RegisterPropertyEditor(TypeInfo(TFileName), TDTable, 'TableName', TDTableNameProperty);
end;


function TDTableNameProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paValueList, paSortList, paAutoUpdate];
end;


procedure TDTableNameProperty.GetValues(Proc: TGetStrProc);
var
   i   : Integer;
   oTmp: TStringList;
begin
     with TDTable(GetComponent(0)) do
     begin
          if Assigned(Master) then Connection := Master.Connection;
          if Connection <> '' then
          begin
               oTmp := TStringList.Create;
               try
               begin
                    GetADOTableNames(oTmp);
                    for i := 0 to oTmp.Count-1 do
                    begin
                         Proc(oTmp.Strings[i]);
                    end;
               end;
               finally
                    oTmp.Free;
               end;
          end;
     end;
     Designer.Modified;
end;

end.
