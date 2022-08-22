unit TinyDBReg;

{$I TinyDB.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, TinyDB;

procedure Register;
  
{$R TinyDB.dcr}
  
implementation

uses
{$IFDEF DELPHI_6_UP}
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

type

{ Property Editor }

  TTinyStringsProperty = class(TStringProperty)
  protected
    procedure GetValueList(List: TStrings); virtual; 
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TTinyTableNameProperty = class(TTinyStringsProperty)
  protected
    procedure GetValueList(List: TStrings); override;
  end;

  TTinyIndexNameProperty = class(TTinyStringsProperty)
  protected
    procedure GetValueList(List: TStrings); override;
  end;

  TTinyDatabaseNameProperty = class(TStringProperty)
  private
    function GetMediumType: TTinyDBMediumType;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TTinyAboutProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    //procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
  end;
  
{ TTinyStringsProperty }

function TTinyStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TTinyStringsProperty.GetValueList(List: TStrings);
begin
end;

procedure TTinyStringsProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TTinyTableNameProperty }

procedure TTinyTableNameProperty.GetValueList(List: TStrings);
var
  Table: TTinyTable;
begin
  Table := GetComponent(0) as TTinyTable;
  Table.DBSession.GetTableNames(Table.DatabaseName, List);
end;

{ TTinyIndexNameProperty }

procedure TTinyIndexNameProperty.GetValueList(List: TStrings);
var
  Table: TTinyTable;
begin
  Table := GetComponent(0) as TTinyTable;
  Table.DBSession.GetIndexNames(Table.DatabaseName, Table.TableName, List);
end;

{ TTinyDatabaseNameProperty }

function TTinyDatabaseNameProperty.GetMediumType: TTinyDBMediumType;
var
  AComponent: TComponent;
begin
  AComponent := GetComponent(0) as TComponent;
  if AComponent is TTDEDataSet then
    Result := (AComponent as TTDEDataSet).MediumType
  else if AComponent is TTinyDatabase then
    Result := (AComponent as TTinyDatabase).MediumType
  else
    Result := mtDisk;
end;

function TTinyDatabaseNameProperty.GetAttributes: TPropertyAttributes;
begin
  if GetMediumType = mtDisk then
    Result := [paDialog]
  else
    Result := [];
end;

procedure TTinyDatabaseNameProperty.Edit;
var
  OpenDialog: TOpenDialog;
begin
  if GetMediumType = mtDisk then
  begin
    OpenDialog := TOpenDialog.Create(Application);
    try
      with OpenDialog do
      begin                            
        Options := Options + [ofFileMustExist];
        InitialDir := ExtractFilePath(GetValue);
        Filter := 'TinyDB Files(*' + tdbDBFileExt + ')|*' + tdbDBFileExt + '|All Files(*.*)|*.*';
        DefaultExt := tdbDBFileExt;
      end;
      if OpenDialog.Execute then
      begin
        SetValue(OpenDialog.FileName);
      end;
    finally
      OpenDialog.Free;
    end;
  end;
end;

{ TTinyAboutProperty }

procedure TTinyAboutProperty.Edit;
var
  Msg: string;
begin
  Msg := 'TinyDB Database Engine' + #13 +
         'Version ' + tdbSoftVer + #13 +
         'Copyright(c) 2000-2006 DayDream Software' + #13 +
         'URL: ' + tdbWebsite + #13 +
         'Email: ' + tdbSupportEmail;

  MessageBox(Application.Handle, PChar(Msg), 'About TinyDB', MB_OK + MB_ICONINFORMATION);
end;

function TTinyAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paFullWidthName];
end;

function TTinyAboutProperty.GetValue: string;
begin
  Result := '(About)';
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TTinyAboutBox), nil, '', TTinyAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TTinyTable, 'TableName', TTinyTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTinyTable, 'IndexName', TTinyIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTDBDataSet, 'DatabaseName', TTinyDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTinyDatabase, 'DatabaseName', TTinyDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTinyDatabase, 'FileName', TTinyDatabaseNameProperty);

  RegisterComponents('TinyDB', [TTinyTable]);
  //RegisterComponents('TinyDB', [TTinyQuery]);
  RegisterComponents('TinyDB', [TTinyDatabase]);
  RegisterComponents('TinyDB', [TTinySession]);
end;

end.
