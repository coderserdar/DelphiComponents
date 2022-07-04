unit DDBEditor;

interface

uses
  Windows, Classes, Messages, Dialogs, SysUtils,
  {$IFDEF MSWINDOWS}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  DUtils, DDB;

type
  TDADOConnectionProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

type
  TDADOPrimaryKeyProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

type
  TDADOMarkerFieldProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

type
  TDADOFileNameProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

procedure Register;
begin
     RegisterPropertyEditor(TypeInfo(String), TDADODataSet, 'Connection' , TDADOConnectionProperty);
     RegisterPropertyEditor(TypeInfo(String), TDADODataSet, 'PrimaryKey' , TDADOPrimaryKeyProperty);
     RegisterPropertyEditor(TypeInfo(String), TDADODataSet, 'MarkerField', TDADOMarkerFieldProperty);
     RegisterPropertyEditor(TypeInfo(String), TDADODataSet, 'FileName'   , TDADOFileNameProperty);
end;

function TDADOConnectionProperty.GetAttributes:TPropertyAttributes;
begin
     Result := [paDialog];
end;


procedure TDADOConnectionProperty.Edit;
begin
     SetStrValue(TDADODataSet(GetComponent(0)).ConnectionEditor);
     Modified;
end;


function TDADOPrimaryKeyProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paValueList, paSortList, paAutoUpdate];
end;


procedure TDADOPrimaryKeyProperty.GetValues(Proc: TGetStrProc);
var
   i    : Integer;
   oTmp : TStringList;
   sTab : String;
begin
     with TDADODataSet(GetComponent(0)) do
     begin
          sTab := InternalTable(Command);
          if (Connection <> '') and (sTab <> '') then
          begin
               oTmp := TStringList.Create;
               try
               begin
                    GetADOFieldNames(sTab, oTmp);
                    for i := 0 to oTmp.Count-1 do
                    begin
                         if oTmp.Strings[i] <> MarkerField then Proc(oTmp.Strings[i]);
                    end;
               end;
               finally
                    oTmp.Free;
               end;
          end;
     end;
     Designer.Modified;
end;


function TDADOMarkerFieldProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paValueList, paSortList, paAutoUpdate];
end;


procedure TDADOMarkerFieldProperty.GetValues(Proc: TGetStrProc);
var
   i    : Integer;
   oTmp : TStringList;
   sTab : String;
begin
     with TDADODataSet(GetComponent(0)) do
     begin
          sTab := InternalTable(Command);
          if (Connection <> '') and (sTab <> '') then
          begin
               oTmp := TStringList.Create;
               try
               begin
                    GetADOFieldNames(sTab, oTmp);
                    for i := 0 to oTmp.Count-1 do
                    begin
                         if oTmp.Strings[i] <> PrimaryKey then Proc(oTmp.Strings[i]);
                    end;
               end;
               finally
                    oTmp.Free;
               end;
          end;
     end;
     Designer.Modified;
end;


function TDADOFileNameProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paDialog];
end;


procedure TDADOFileNameProperty.Edit;
var
   dlgSave : TSaveDialog;
   sFile   : String;
begin
     dlgSave := TSaveDialog.Create(nil);
     try
     begin
          sFile := TDADODataSet(GetComponent(0)).FileName;
          if sFile = '' then
          begin
               dlgSave.FileName   := cnsResName;
               dlgSave.InitialDir := IIF((csDesigning in TDADODataSet(GetComponent(0)).ComponentState), GetWorkDir, GetExeDir);
          end
          else
          begin
               dlgSave.FileName   := ExtractFileName(sFile);
               dlgSave.InitialDir := ExtractFileDir(sFile);
          end;
          dlgSave.Filter     := 'DataSet Resource Files (*' + cnsResExt + ') |*' + cnsResExt;
          dlgSave.DefaultExt := cnsResExt;
          if dlgSave.Execute then SetStrValue(dlgSave.FileName);
     end;
     finally
          dlgSave.Free;
     end;
     Modified;
end;

end.
