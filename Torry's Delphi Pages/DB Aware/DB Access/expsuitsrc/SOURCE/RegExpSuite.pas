{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit RegExpSuite;

interface

{$I DELPHI.VER}

{$DEFINE DATATODBF}
{$DEFINE DATATOASCII}
{$DEFINE DATATOXLS}
{$DEFINE DATATOWK1}
{$DEFINE DATATOHTML}
{.$DEFINE EXPORDIALOG}

uses
  Classes, Db, DataExport,
  {$IFDEF LESS140}DsgnIntf{$ELSE}
  DesignIntf, DesignEditors{$ENDIF};

Type
  TExportFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); {virtual;}
    procedure GetValues(Proc: TGetStrProc); override;
    function GetDataSet: TDataSet; {virtual;}
  end;

  TDataExportMenu = class(TComponentEditor)
  protected
    function GetCollection: TExportFields;
    function GetCollectionPropertyName: string; virtual; abstract;
    function GetFileFilter: string; virtual; abstract;
    procedure TestComponent;
    procedure FindDBGrid;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IFDEF DATATODBF}
  TDataToDBFMenu = class(TDataExportMenu)
  protected
    function GetCollectionPropertyName: string; override;
    function GetFileFilter: string; override;
  end;
  {$ENDIF}

  {$IFDEF DATATOASCII}
  TDataToAsciiMenu = class(TDataExportMenu)
  protected
    function GetCollectionPropertyName: string; override;
    function GetFileFilter: string; override;
  end;
  {$ENDIF}

  {$IFDEF DATATOXLS}
  TDataToXLSMenu = class(TDataExportMenu)
  protected
    function GetCollectionPropertyName: string; override;
    function GetFileFilter: string; override;
  end;
  {$ENDIF}

  {$IFDEF DATATOWK1}
  TDataToWK1Menu = class(TDataExportMenu)
  protected
    function GetCollectionPropertyName: string; override;
    function GetFileFilter: string; override;
  end;
  {$ENDIF}

  {$IFDEF DATATOHTML}
  TDataToHTMLMenu = class(TDataExportMenu)
  protected
    function GetCollectionPropertyName: string; override;
    function GetFileFilter: string; override;
  end;
  {$ENDIF}

procedure Register;

implementation

uses
  {$IFDEF DATATODBF}DataToDBF,{$ENDIF}
  {$IFDEF DATATOASCII}DataToAscii,{$ENDIF}
  {$IFDEF DATATOXLS}DataToXLS,{$ENDIF}
  {$IFDEF DATATOWK1}DataToWK1,{$ENDIF}
  {$IFDEF DATATOHTML}DataToHTML,{$ENDIF}
  {$IFDEF EXPORDIALOG}ExpDlg,{$ENDIF}
  DbConsts, SysUtils, Dialogs, Controls, Windows,
  DBGrids, unClassLister, ShellApi, ColnEdit;

resourcestring
  SPalette = 'FireSoft';

  {$IFDEF SPANISH}
  SDeleteOldFields     = 'Esta operación eliminará los campos antiguos'#13#10'¿Desea continuar?';
  SMnuAbout            = 'Acerca de...';
  SMnuTest             = 'Probar exportación...';
  SMnuFieldsEditor     = 'Editor de Campos...';
  SMnuRetriveFields    = 'Cargar Campos...';
  SMnuRetriveDBColumns = 'Cargar TDBColumns...';
  {$ELSE}
  SDeleteOldFields     = 'This operation delete old fields'#13#10'Continue?';
  SMnuAbout            = 'About...';
  SMnuTest             = 'Test to File...';
  SMnuFieldsEditor     = 'Fields Editor...';
  SMnuRetriveFields    = 'Retrive Fields...';
  SMnuRetriveDBColumns = 'Retrive TDBColumns...';
  {$ENDIF}


procedure Register;
begin
  {$IFDEF DATATODBF}
  RegisterComponents(SPalette, [TDataToDBF]);
  RegisterComponentEditor(TDataToDBF, TDataToDBFMenu);
  {$ENDIF}

  {$IFDEF DATATOASCII}
  RegisterComponents(SPalette, [TDataToAscii]);
  RegisterComponentEditor(TDataToAscii, TDataToAsciiMenu);
  {$ENDIF}

  {$IFDEF DATATOXLS}
  RegisterComponents(SPalette, [TDataToXLS]);
  RegisterComponentEditor(TDataToXLS, TDataToXLSMenu);
  {$ENDIF}

  {$IFDEF DATATOWK1}
  RegisterComponents(SPalette, [TDataToWK1]);
  RegisterComponentEditor(TDataToWK1, TDataToWK1Menu);
  {$ENDIF}

  {$IFDEF DATATOHTML}
  RegisterComponents(SPalette, [TDataToHTML]);
  RegisterComponentEditor(TDataToHTML, TDataToHTMLMenu);
  {$ENDIF}

  {$IFDEF EXPORTDIALOG}
  RegisterComponents(SPalette, [TExportDialog]);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TExportField, 'DataField', TExportFieldProperty);
end;

{ TExportFieldProperty }

function TExportFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

function TExportFieldProperty.GetDataSet: TDataSet;
begin
  if GetComponent(0) is TExportField Then
    Result := TExportField(GetComponent(0)).DataSet
  else
    Result := nil;
end;

procedure TExportFieldProperty.GetValueList(List: TStrings);
var
  DataSet: TDataSet;
begin
  DataSet := GetDataSet;
  if Assigned(DataSet) then
    DataSet.GetFieldNames(List)
  else
    raise Exception.Create(SDataSetEmpty);
end;

procedure TExportFieldProperty.GetValues(Proc: TGetStrProc);
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

{ TDataExportMenu }

procedure TDataExportMenu.Edit;
begin
  ExecuteVerb(3);
end;

procedure TDataExportMenu.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShellExecute(0, 'open', 'http://www.firesoft.com.ar/delphi/fses.html', nil, nil, SW_SHOW);
    2: TestComponent;
    3: ShowCollectionEditor(Designer, Component, GetCollection, GetCollectionPropertyName);
    4: if (GetCollection.Count = 0) or
          (MessageDlg( SDeleteOldFields, mtConfirmation, [mbYes, mbNo], 0)= mrYes) then
       begin
         GetCollection.BuildFields;
         Designer.Modified
       end;
    5: FindDbGrid;
  end;
end;

procedure TDataExportMenu.FindDBGrid;
var
  ADBGrid: TComponent;
begin
  ADBGrid := SelectComponentByClass(Component.Owner, TDBGrid);
  if ADBGrid is TDBGrid then
    GetCollection.Assign(TDBGrid(ADBGrid).Columns);
end;

function TDataExportMenu.GetCollection: TExportFields;
begin
  Result := TDataExport(Component).GetFields
end;

function TDataExportMenu.GetVerb(Index: Integer): string;
begin
  Case Index of
    0: Result := SMnuAbout;
    1: Result := '-';
    2: Result := SMnuTest;
    3: Result := SMnuFieldsEditor;
    4: Result := SMnuRetriveFields;
    5: Result := SMnuRetriveDBColumns;
  else
  end;
end;

function TDataExportMenu.GetVerbCount: Integer;
begin
  Result := 6;
end;

procedure TDataExportMenu.TestComponent;
begin
  with TSaveDialog.Create(nil) do
    try
      Title := 'Export File';
      Filter := GetFileFilter;
      Options := [ofOverwritePrompt, ofHideReadOnly{$IFNDEF LESS110}, ofEnableSizing{$ENDIF}];
      if Execute Then
        {$IFDEF LESS140}
        TDataExport(Component).SaveToFile(FileName);
        {$ELSE}
        TDataExport(GetComponent).SaveToFile(FileName);
        {$ENDIF}
    finally
      Free;
    end;
end;

{ TDataToDBFMenu }

{$IFDEF DATATODBF}
function TDataToDBFMenu.GetCollectionPropertyName: String;
begin
  Result := 'Fields';
end;

function TDataToDBFMenu.GetFileFilter: string;
begin
  Result := 'DBase Files|*.dbf';
end;
{$ENDIF}

{ TDataToAsciiMenu }

{$IFDEF DATATOASCII}
function TDataToAsciiMenu.GetCollectionPropertyName: String;
begin
  Result := 'Fields';
end;

function TDataToAsciiMenu.GetFileFilter: string;
begin
  Result := 'Text Files|*.txt|CSV Files|*.csv|All Files|*.*';
end;
{$ENDIF}

{ TDataToXLSMenu }

{$IFDEF DATATOXLS}
function TDataToXLSMenu.GetCollectionPropertyName: String;
begin
  Result := 'Columns';
end;

function TDataToXLSMenu.GetFileFilter: string;
begin
  Result := 'Excel Files|*.xls';
end;
{$ENDIF}

{ TDataToWK1Menu }

{$IFDEF DATATOWK1}
function TDataToWK1Menu.GetCollectionPropertyName: String;
begin
  Result := 'Columns';
end;

function TDataToWK1Menu.GetFileFilter: string;
begin
  Result := 'Lotus Files|*.wk?';
end;
{$ENDIF}

{ TDataToHTMLMenu }

{$IFDEF DATATOHTML}
function TDataToHTMLMenu.GetCollectionPropertyName: String;
begin
  Result := 'Fields';
end;

function TDataToHTMLMenu.GetFileFilter: string;
begin
  Result := 'HTML Files|*.htm;*.html';
end;
{$ENDIF}

end.
