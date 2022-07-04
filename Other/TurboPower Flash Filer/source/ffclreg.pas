{*********************************************************}
{* FlashFiler: Property Editors for FF Client Components *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffclreg;

interface

procedure Register;

implementation

uses
  {$IFDEF Delphi3}
  Dialogs,
  {$ENDIF}
  {$IFDEF CBuilder3}
  Dialogs,
  {$ENDIF}
  SysUtils,
  Classes,
  Controls,
  Forms,
  DB,
  {$IFNDEF DCC4OrLater}
  DBTables,
  {$ENDIF}
  {$IFDEF DCC6OrLater}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ExptIntf,
  ffclcoln,
  ffclreng,
  ffclsqle,
  ffclbase,
  ffconst,
  ffdbbase,
  ffdb,
  ffllbase,
  ffllgrid,
  fflllgcy,
  fflllog,
  ffsreng,
  ffclfldg,
  ffclver,
  ffsrcmd,
  ffsrsec,
  ffllthrd,
  ffclexpt,
  fflleng,
  ffllcomm,
  ffsqleng,
  ffllcomp;

{ TffFieldLinkProperty }
type
  TffFieldLinkProperty = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
  end;

procedure TffFieldLinkProperty.Edit;
var
  Table         : TffTable;
  lMasterTable  : TDataset;
  lDetailIndex  : TffShStr;
  lDetailFields : TffShStr;
  lMasterFields : TffShStr;
begin
  Table := GetComponent(0) as TffTable;
  with Table do begin
    if not Assigned(MasterSource) then                                          {begin !!.06}
      {$IFDEF Delphi3}
      begin
        ShowMessageFmt('The MasterSource property of ''%s'' must be linked to a DataSource', [Name]);
        Exit;
      end;
      {$ENDIF}
      {$IFDEF CBuilder3}
      begin
        ShowMessageFmt('The MasterSource property of ''%s'' must be linked to a DataSource', [Name]);
        Exit;
      end;
      {$ENDIF}
      RaiseFFErrorObjFmt(Table, ffccDesign_SLinkMasterSource, [Name]);
    if not Assigned(MasterSource.DataSet) then
      {$IFDEF Delphi3}
      begin
        ShowMessage('Unable to open the MasterSource Table');
        Exit;
      end;
      {$ENDIF}
      {$IFDEF CBuilder3}
      begin
        ShowMessage('Unable to open the MasterSource Table');
        Exit;
      end;
      {$ENDIF}
      RaiseFFErrorObj(Table, ffccDesign_SLinkMaster);                           {end !!.06}
    lMasterTable := MasterSource.DataSet;
    lDetailIndex := IndexName;
    lDetailFields := IndexFieldNames;
    lMasterFields := GetValue;
  end;
  if ShowFieldLinkDesigner(lMasterTable,
                           Table,
                           lDetailIndex,
                           lDetailFields,
                           lMasterFields) = mrOK then
    with Table do begin
      if lDetailIndex <> '' then
        IndexName := lDetailIndex
      else
        IndexFieldNames := lDetailFields;
      SetValue(lMasterFields);
    end;
end;

function TffFieldLinkProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{ TffDBStringProperty }

type
  TffDBStringProperty = class(TStringProperty)
    protected
      procedure GetValueList(List: TStrings); virtual;
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TffDBStringProperty.GetValueList(List: TStrings);
begin
  { Do nothing - avoid compiler hint }
end;

function TffDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TffDBStringProperty.GetValues(Proc: TGetStrProc);
var
  i      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  try
    Values.BeginUpdate;
    try
      GetValueList(Values);
      for i := 0 to Pred(Values.Count) do
        Proc(Values[i]);
    finally
      Values.EndUpdate;
    end;
  finally
    Values.Free;
  end;
end;

{ TffClientNameProperty }

type
  TffClientNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffClientNameProperty.GetValueList(List: TStrings);
begin
  GetFFClientNames(List);
end;

{ TffSessionNameProperty }

type
  TffSessionNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffSessionNameProperty.GetValueList(List: TStrings);
begin
  GetFFSessionNames(List);
end;

{ TffDatabaseNameProperty }

type
  TffDatabaseNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffDatabaseNameProperty.GetValueList(List: TStrings);
var
  S : TffSession;
begin
  S := (GetComponent(0) as TffDataset).Session;
  if Assigned(S) then
    GetFFDatabaseNames(S, List);
end;

{ TffAliasNameProperty }

type
  TffAliasNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffAliasNameProperty.GetValueList(List: TStrings);
var
  S : TffSession;
begin
  S := (GetComponent(0) as TffDatabase).Session;
  if Assigned(S) then
    S.GetAliasNames(List);
end;

{ TffTableNameProperty }

type
  TffTableNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffTableNameProperty.GetValueList(List: TStrings);
var
  DB : TffDatabase;
begin
  DB := TffDatabase((GetComponent(0) as TffTable).Database);
  if Assigned(DB) then
    DB.GetTableNames(List);
end;


{ TffIndexNameProperty }

type
  TffIndexNameProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffIndexNameProperty.GetValueList(List: TStrings);
var
  Table : TffTable;
begin
  Table := GetComponent(0) as TffTable;
  if Assigned(Table) then
    Table.GetIndexNames(List);
end;

{ TffIndexFieldNamesProperty }

type
  TffIndexFieldNamesProperty = class(TffDBStringProperty)
    public
      procedure GetValueList(List: TStrings); override;
  end;

procedure TffIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  Table : TffTable;
  i     : Integer;
begin
  Table := GetComponent(0) as TffTable;
  if Assigned(Table) then
    with Table do begin
      IndexDefs.Update;
      for i := 0 to Pred(IndexDefs.Count) do
        with IndexDefs[i] do
          if not (ixExpression in Options) then
            List.Add(Fields);
    end;
end;

//{ TffDataSourceProperty }                                            {!!.06 - Deleted - Start}
//
//type
//  TffDataSourceProperty = class(TffDBStringProperty)
//    public
//      function GetValue : string; override;
//      procedure GetValueList(List: TStrings); override;
//      procedure SetValue(const aValue : string); override;
//  end;
//
//function TffDataSourceProperty.GetValue : string;
//var
//  i, j  : integer;
//  Table : TffTable;
//  MrSrc : TDataSource;
//  Cmpnt : TComponent;
//  DataModule : TDataModule;
//  Form : TForm;
//begin
//  Result := '';
//  Table := GetComponent(0) as TffTable;
//  if (Table <> nil) and (Table.MasterSource <> nil) then begin
//    MrSrc := Table.MasterSource;
//    {is the master source on the table's form? if so just return the
//     data source's name}
//    for i := 0 to pred(Table.Owner.ComponentCount) do begin
//      if (Table.Owner.Components[i] = MrSrc) then begin
//        Result := MrSrc.Name;
//        Exit;
//      end;
//    end;
//    {is the master source on one of the project's data modules? if so
//     return the data module name, period, and the data source's name}
//    for j := 0 to pred(Screen.DataModuleCount) do begin
//      DataModule := Screen.DataModules[j];
//      for i := 0 to pred(DataModule.ComponentCount) do begin
//        Cmpnt := DataModule.Components[i];
//        if (Cmpnt = MrSrc) {and
//           Designer.IsComponentLinkable(Cmpnt)} then begin
//          Result := DataModule.Name + '.' + MrSrc.Name;
//          Exit;
//        end;
//      end;
//    end;
//    {is the master source on one of the project's forms? if so return the form
//     name, period, and the data source's name}
//    for j := 0 to pred(Screen.FormCount) do begin
//      Form := Screen.Forms[j];
//      for i := 0 to pred(Form.ComponentCount) do begin
//        Cmpnt := Form.Components[i];
//        if (Cmpnt = MrSrc) {and
//           Designer.IsComponentLinkable(Cmpnt)} then begin
//          Result := Form.Name + '.' + MrSrc.Name;
//          Exit;
//        end;
//      end;
//    end;
//
//  end;
//end;
//
//procedure TffDataSourceProperty.GetValueList(List: TStrings);
//var
//  i, j  : integer;
//  Table : TffDataset;
//  Cmpnt : TComponent;
//  DataModule : TDataModule;
//  Form : TForm;
//begin
//  Table := GetComponent(0) as TffDataset;
//  if (Table <> nil) and (Table.Owner <> nil) then begin
//    {first add all the names of the data sources on the table's owner}
//    for i := 0 to pred(Table.Owner.ComponentCount) do begin
//      Cmpnt := Table.Owner.Components[i];
//      if (Cmpnt is TDataSource) and
//         not Table.IsLinkedTo(TDataSource(Cmpnt)) and
//         (Cmpnt.Name <> '') then
//        List.Add(Cmpnt.Name);
//    end;
//    {then add all the names of the data sources on the project's data
//     modules, at least those that can be linked; prefix with the data
//     module name plus a period}
//    for j := 0 to pred(Screen.DataModuleCount) do begin
//      DataModule := Screen.DataModules[j];
//      for i := 0 to pred(DataModule.ComponentCount) do begin
//        if DataModule = Table.Owner then
//          Continue;
//        Cmpnt := DataModule.Components[i];
//        if (Cmpnt is TDataSource) and
//           not Table.IsLinkedTo(TDataSource(Cmpnt)) and
//           Designer.IsComponentLinkable(Cmpnt) and
//           (Cmpnt.Name <> '') then begin
//          List.Add(DataModule.Name + '.' + Cmpnt.Name);
//        end;
//      end;
//    end;
//
//    for j := 0 to pred(Screen.FormCount) do begin
//      Form := Screen.Forms[j];
//      for i := 0 to pred(Form.ComponentCount) do begin
//        if Form = Table.Owner then
//          Continue;
//        Cmpnt := Form.Components[i];
//        if (Cmpnt is TDataSource) and
//           not Table.IsLinkedTo(TDataSource(Cmpnt)) and
//           Designer.IsComponentLinkable(Cmpnt) and
//           (Cmpnt.Name <> '') then begin
//          List.Add(Form.Name + '.' + Cmpnt.Name);
//        end;
//      end;
//    end;
//
//  end;
//end;
//
//procedure TffDataSourceProperty.SetValue(const aValue : string);
//var
//  i, j  : integer;
//  PosDot: integer;
//  Table : TffTable;
//  Cmpnt : TComponent;
//  DataModule : TDataModule;
//  DataModName: string;
//  DataSrcName: string;
//begin
//  Table := GetComponent(0) as TffTable;
//  if (Table <> nil) and (Table.Owner <> nil) then begin
//    {assume we won't find the name; set the master source property
//     to nil}
//    Table.MasterSource := nil;
//    if (aValue <> '') then begin
//      {find the period in the master source name: its presence will
//       indicate whether the component is on the same form or a
//       separate data module}
//      PosDot := Pos('.', aValue);
//      if (PosDot = 0) {there is no period} then begin
//        {find the data source on this form}
//        for i := 0 to pred(Table.Owner.ComponentCount) do begin
//          Cmpnt := Table.Owner.Components[i];
//          if (Cmpnt is TDataSource) and
//             not Table.IsLinkedTo(TDataSource(Cmpnt)) and
//             (CompareText(Cmpnt.Name, aValue) = 0) then begin
//            Table.MasterSource := TDataSource(Cmpnt);
//            Exit;
//          end;
//        end;
//      end
//      else {there is a period} begin
//        DataModName := Copy(aValue, 1, pred(PosDot));
//        DataSrcName := Copy(aValue, succ(PosDot), length(aValue));
//        for j := 0 to pred(Screen.DataModuleCount) do begin
//          DataModule := Screen.DataModules[j];
//          if (CompareText(DataModule.Name, DataModName) = 0) then begin
//            for i := 0 to pred(DataModule.ComponentCount) do begin
//              Cmpnt := DataModule.Components[i];
//              if (Cmpnt is TDataSource) and
//                 not Table.IsLinkedTo(TDataSource(Cmpnt)) and
//                 Designer.IsComponentLinkable(Cmpnt) and
//                 (CompareText(Cmpnt.Name, DataSrcName) = 0) then begin
//                Table.MasterSource := TDataSource(Cmpnt);
//                Exit;
//              end;
//            end;
//          end;
//        end;
//      end;
//    end;
//  end;
//end;                                                                 {!!.06 - Deleted - End}

{ TffServerEngineProperty}
type
  TffServerEngineProperty = class(TffDBStringProperty)
    public
      function GetValue : string; override;
      procedure GetValueList(List: TStrings); override;
      procedure SetValue(const aValue : string); override;
  end;

function TffServerEngineProperty.GetValue : string;
var
  i, j  : integer;
  Client : TffBaseClient;                                              {!!.03}
  SvrEng : TffBaseServerEngine;
  Cmpnt : TComponent;
  DataModule : TDataModule;
  Form : TForm;
begin
  Result := '';
  Client := GetComponent(0) as TffBaseClient;                         {!!.03}
  if Assigned(Client) and Assigned(Client.ServerEngine) then begin
    if Client.OwnServerEngine then
      Exit;

    SvrEng := Client.ServerEngine;
    {is the server engine on the table's form? if so just return the
     data source's name}
    for i := 0 to Pred(Client.Owner.ComponentCount) do
      if (Client.Owner.Components[i] = SvrEng) then begin
        Result := SvrEng.Name;
        Exit;
      end;


    {is the master source on one of the project's data modules? if so
     return the data module name, period, and the data source's name}
    for j := 0 to Pred(Screen.DataModuleCount) do begin
      DataModule := Screen.DataModules[j];
      for i := 0 to pred(DataModule.ComponentCount) do begin
        Cmpnt := DataModule.Components[i];
        if (Cmpnt = SvrEng) {and
           Designer.IsComponentLinkable(Cmpnt)} then begin
          Result := DataModule.Name + '.' + SvrEng.Name;
          Exit;
        end;
      end;
    end;

    {is the master source on one of the project's forms? if so return the form
     name, period, and the data source's name}
    for j := 0 to pred(Screen.FormCount) do begin
      Form := Screen.Forms[j];
      for i := 0 to pred(Form.ComponentCount) do begin
        Cmpnt := Form.Components[i];
        if (Cmpnt = SvrEng) {and
           Designer.IsComponentLinkable(Cmpnt)} then begin
          Result := Form.Name + '.' + SvrEng.Name;
          Exit;
        end;
      end;
    end;

  end;
end;

procedure TffServerEngineProperty.GetValueList(List: TStrings);
var
  i, j  : integer;
  Client : TffBaseClient;
  Cmpnt : TComponent;
  DataModule : TDataModule;
begin
  Client := GetComponent(0) as TffBaseClient;
  if (Client <> nil) and (Client.Owner <> nil) then begin
    {first add all the names of the data sources on the table's owner}
    for i := 0 to pred(Client.Owner.ComponentCount) do begin
      Cmpnt := Client.Owner.Components[i];
      if (Cmpnt is TffBaseServerEngine) and
         (Cmpnt.Name <> '') then
        List.Add(Cmpnt.Name);
    end;

    {then add all the names of the data sources on the project's data
     modules, at least those that can be linked; prefix with the data
     module name plus a period}
    for j := 0 to pred(Screen.DataModuleCount) do begin
      DataModule := Screen.DataModules[j];
      for i := 0 to pred(DataModule.ComponentCount) do begin
        Cmpnt := DataModule.Components[i];
        if (Cmpnt is TffBaseServerEngine) and
           Designer.IsComponentLinkable(Cmpnt) and
           (Cmpnt.Name <> '') then begin
          List.Add(DataModule.Name + '.' + Cmpnt.Name);
        end;
      end;
    end;
  end;
end;

procedure TffServerEngineProperty.SetValue(const aValue : string);
var
  i, j  : integer;
  PosDot: integer;
  Client : TffBaseClient;
  Cmpnt : TComponent;
  DataModule : TDataModule;
  DataModName: string;
  SvrEngName: string;
begin
  Client := GetComponent(0) as TffBaseClient;
  if (Client <> nil) and (Client.Owner <> nil) then begin
    {assume we won't find the name; set the master source property
     to nil}
    Client.ServerEngine := nil;
    if (aValue <> '') then begin
      {find the period in the master source name: its presence will
       indicate whether the component is on the same form or a
       separate data module}
      PosDot := Pos('.', aValue);
      if (PosDot = 0) {there is no period} then begin
        {find the data source on this form}
        for i := 0 to pred(Client.Owner.ComponentCount) do begin
          Cmpnt := Client.Owner.Components[i];
          if (Cmpnt is TffBaseServerEngine) and
             (CompareText(Cmpnt.Name, aValue) = 0) then begin
            Client.ServerEngine := TffBaseServerEngine(Cmpnt);
            Exit;
          end;
        end;
      end
      else {there is a period} begin
        DataModName := Copy(aValue, 1, pred(PosDot));
        SvrEngName := Copy(aValue, succ(PosDot), length(aValue));
        for j := 0 to pred(Screen.DataModuleCount) do begin
          DataModule := Screen.DataModules[j];
          if (CompareText(DataModule.Name, DataModName) = 0) then begin
            for i := 0 to pred(DataModule.ComponentCount) do begin
              Cmpnt := DataModule.Components[i];
              if (Cmpnt is TffBaseServerEngine) and
                 Designer.IsComponentLinkable(Cmpnt) and
                 (CompareText(Cmpnt.Name, SvrEngName) = 0) then begin
                Client.ServerEngine := TffBaseServerEngine(Cmpnt);
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TffStringListProperty }
type
  TffStringListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TffStringListProperty.Edit;
begin
  with TffSQLEditor.Create(Application) do
    try
      SQLLines := GetOrdValue;
      ShowModal;
      if ModalResult = mrOK then
        SetOrdValue(SQLLines);
    finally
      Free;
    end;
end;

function TffStringListProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;


{ TffCollectionProperty }
type
  TffCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

procedure TffCollectionProperty.Edit;
begin
  FFShowParamEditor(Designer, TComponent(GetComponent(0)), GetName, GetOrdValue);
end;

function TffCollectionProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog];
end;

{TffServerEngineComponentEditor }
type
  TffServerEngineComponentEditor = class(TComponentEditor)
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
  end;

function TffServerEngineComponentEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TffServerEngineComponentEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Shutdown server engine';
  else
    Result := 'ERROR!';
  end;
end;

procedure TffServerEngineComponentEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: TffStateComponent(Component).Shutdown;
  else
    Assert(False);
  end;
end;

(*
{ TffDatabaseEditor }

type
  TffDatabaseEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

procedure TffDatabaseEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: if EditDatabase(TffDatabase(Component)) then Designer.Modified;
    1: ExploreDatabase(TffDatabase(Component));
  end;
end;

function TffDatabaseEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := LoadStr(SDatabaseEditor);
    1: Result := LoadStr(SExplore);
  end;
end;

function TffDatabaseEditor.GetVerbCount: integer;
begin
  Result := 2;
end;
*)

procedure Register;
begin
  { Register FlashFiler Client components }
  RegisterComponents('FlashFiler Client', [
                                    TffClient,
                                    TffCommsEngine,
                                    TffSession,
                                    TffDatabase,
                                    TffTable,
                                    TffQuery,
                                    TffStringGrid
                                    ]);

  { Register FlashFiler Server components }
  RegisterComponents('FlashFiler Server', [
                                    TffServerEngine,
                                    TffRemoteServerEngine,
                                    TffSQLEngine,
                                    TffServerCommandHandler,
                                    TffLegacyTransport,
                                    TffEventLog,
                                    TffSecurityMonitor,
                                    TffThreadPool
                                    ]);

  {register the experts}
  RegisterCustomModule(TffBaseEngineManager, TCustomModule);
  RegisterLibraryExpert(TffEngineManagerWizard.Create);

  {register the property editors...}
  {...for clients}
  RegisterPropertyEditor(TypeInfo(AnsiString),                         {!!.05}
                         TffBaseClient,
                         'ServerEngine',
                         TffServerEngineProperty);
  {...for sessions}
  RegisterPropertyEditor(TypeInfo(AnsiString), TffSession, 'CommsEngineName', TffClientNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffSession, 'ClientName', TffClientNameProperty);
  {...for databases}
  RegisterPropertyEditor(TypeInfo(AnsiString), TffDatabase, 'AliasName', TffAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffDatabase, 'SessionName', TffSessionNameProperty);
  {...for tables}
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'SessionName', TffSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'DatabaseName', TffDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'TableName', TffTableNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'IndexName', TffIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'IndexFieldNames', TffIndexFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'MasterFields', TffFieldLinkProperty);
//  RegisterPropertyEditor(TypeInfo(TDataSource), TffTable, 'MasterSource', TffDataSourceProperty); {!!.06}
  {...for queries}
  RegisterPropertyEditor(TypeInfo(AnsiString), TffQuery, 'DatabaseName', TffDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TffQuery, 'Params', TffCollectionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffQuery, 'SessionName', TffSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TffQuery, 'SQL', TffStringListProperty);
  {..for version number property}
  RegisterPropertyEditor(TypeInfo(AnsiString), TffClient, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffCommsEngine, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffSession, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffDatabase, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffTable, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffQuery, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffServerEngine, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffRemoteServerEngine, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffSQLEngine, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffServerCommandHandler, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffLegacyTransport, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffEventLog, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffSecurityMonitor, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffThreadPool, 'Version', TffVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TffStringGrid, 'Version', TffVersionProperty);

  {register the component editors...}
  RegisterComponentEditor(TffServerEngine, TffServerEngineComponentEditor);
//  RegisterComponentEditor(TffDatabase, TffDatabaseEditor);
end;

end.
