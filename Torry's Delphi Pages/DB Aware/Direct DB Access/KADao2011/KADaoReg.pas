unit KADaoReg; 
{$I KADaoCommonDirectives.pas}
interface
Uses Classes {$IFNDEF D6UP}, DsgnIntf{$ENDIF}
             {$IFDEF D6UP},  DesignIntf, DesignWindows,  DesignEditors{$ENDIF};

Type
//******************************************************************************
//                           EDITOR DEFINITIONS - KADaoDBEngine
//******************************************************************************
  TEngEngineTypeEditor = class(TIntegerProperty)
    Public
     function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     procedure SetValue(const Value: string); override;                                               
     Function  GetAttributes: TPropertyAttributes; override;
    End;

  TEngSystemDatabaseNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

  {$IFDEF DYNADAO}
  TEngDaoVersionEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
  {$ENDIF}
//******************************************************************************
//                           EDITOR DEFINITIONS - KADaoWorkspace
//******************************************************************************
  TWorkEngineTypeEditor = class(TIntegerProperty)
    Public
     function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

  TWorkDefaultCursorDriverTypeEditor = class(TIntegerProperty)
    Public
     function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;
//******************************************************************************
//                           EDITOR DEFINITIONS - KADaoDatabase
//******************************************************************************
TDBDatabaseTypeEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TDBDatabaseNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TDBSystemDatabaseNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TDBEngineTypeEditor = class(TIntegerProperty)
    Public
     function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TDBDefaultCursorDriverTypeEditor = class(TIntegerProperty)
    Public
     function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;


TDBWorkspaceEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

{$IFDEF DYNADAO}
TDBDaoVersionEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
{$ENDIF}
//******************************************************************************
//                           EDITOR DEFINITIONS - KADaoTable
//******************************************************************************
TTableNameEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TQueryNameEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TIndexNameEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

TTableTypeEditor = class(TIntegerProperty)
    Public
     Function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     Procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TLockTypeEditor = class(TIntegerProperty)
    Public
     Function  GetValue: string; override;
     Procedure GetValues( Proc: TGetStrProc);override;
     Procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TSortByEditor = class(TStringProperty)
    Public
     Function  GetValue: string; override;
     Procedure Edit;override;
     Procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TQueryDefParamsEditor = class(TStringProperty)
    Public
     Function  GetValue: string; override;
     Procedure Edit;override;
     Procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TMasterFieldsEditor = class(TStringProperty)
    Public
     Procedure Edit;override;
     Function GetValue: string; override;
     Procedure SetValue(const Value: string); override;
     Function  GetAttributes: TPropertyAttributes; override;
    End;

TEncrypterEditor= class(TComponentProperty)
    Public
      procedure GetValues(Proc: TGetStrProc);override;
    End;
//******************************************************************************
//                           EDITOR DEFINITIONS - KADaoInfo
//******************************************************************************
  TDIDatabaseNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;

  TDIDLLNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
//******************************************************************************

procedure Register;

Implementation
Uses SysUtils, Forms, Dialogs, DB, KDaoDBEngine, KDaoWorkspace, KDaoDatabase,
     KDaoTable, KADaoInfo, DaoApi, KADaoEncrypter, TypInfo,
     SortByDialog, MasterDetailFormUnit;

//******************************************************************************
//                           EDITOR CODE - KADaoDBEngine
//******************************************************************************
//******************************************************** KADaoDBEngine Editors
//*********************************************************** Engine Type Editor
Function TEngEngineTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TEngEngineTypeEditor.GetValue: string;
Begin
 if GetComponent(0) is TKADaoDBEngine then
    Begin
      if TKADaoDBEngine(GetComponent(0)).EngineType=DaoApi.dbUseODBC then
         Result:='dbUseODBC'
      Else
         Result:='dbUseJet';
    End;
End;

Procedure TEngEngineTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoDBEngine then
     Begin
       Proc('dbUseODBC');
       Proc('dbUseJet');
     End;
End;


procedure TEngEngineTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoDBEngine then
       Begin
       if Value='dbUseODBC' Then
          Dat:=DaoApi.dbUseODBC
       Else
          Dat:=DaoApi.dbUseJet;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;


//******************************************************* System Database Editor
Function TEngSystemDatabaseNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TEngSystemDatabaseNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoDBEngine then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TEngSystemDatabaseNameEditor.Edit;
var
   DlgChooseDatabase : TOpenDialog;
   DBase             : TKADaoDBEngine;
   Filter            : String;
Begin
   DBase:=TKADaoDBEngine(GetComponent(0));
   DlgChooseDatabase := TOpenDialog.Create(Nil);
   DlgChooseDatabase.FilterIndex:=1;
   DlgChooseDatabase.DefaultExt:='mdw';
   DlgChooseDatabase.InitialDir:= ExtractFilePath(DBase.SystemDatabase);
   DlgChooseDatabase.FileName  := ExtractFileName(DBase.SystemDatabase);
   if DlgChooseDatabase.FileName <> '' Then Filter := AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName));
   System.Delete(Filter,1,1);
   DlgChooseDatabase.DefaultExt:= Filter;
   if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mda' Then DlgChooseDatabase.FilterIndex:=1
   Else
     if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mdw' Then DlgChooseDatabase.FilterIndex:=1
   Else
     if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mdb' Then DlgChooseDatabase.FilterIndex:=2
   Else
     DlgChooseDatabase.FilterIndex:=3;
   Filter:='Microsoft Access security files (*.mda *.mdw)|*.mda;*.mdw';
   Filter:=Filter+'|Microsoft Access (*.mdb)|*.mdb';
   Filter:=Filter+'|All files (*.*)|*.*';
   DlgChooseDatabase.Title:='Choose System Database:';
   DlgChooseDatabase.Options:=[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
   DlgChooseDatabase.Filter :=Filter;
   if DlgChooseDatabase.Execute then SetStrValue(DlgChooseDatabase.FileName);
   DlgChooseDatabase.Free;
   Modified;
End;

//*********************************************************** DAO Version Editor
{$IFDEF DYNADAO}
Procedure TEngDaoVersionEditor.GetValues( Proc: TGetStrProc);
Var
  DBase : TKADaoDBEngine;
  X     : Integer;
Begin
  if GetComponent(0) is TKADaoDBEngine then
  Begin
    DBase := TKADaoDBEngine(GetComponent(0));
    Try
      For X := 0 to DBase.F_DaoVersionList.Count-1 do Proc(DBase.F_DaoVersionList[X]);
    Finally
    End;
  End;
End;

Procedure TEngDaoVersionEditor.SetValue(const Value: string);
Begin
 if GetComponent(0) is TKADaoDBEngine then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Function TEngDaoVersionEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;
{$ENDIF}

//******************************************************************************
//                           EDITOR CODE - KADaoWorkspace
//******************************************************************************
//******************************************************* KADaoWorkspace Editors
//*********************************************************** Engine Type Editor
Function TWorkEngineTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TWorkEngineTypeEditor.GetValue: string;
Begin
 if GetComponent(0) is TKADaoWorkspace then
    Begin
      if TKADaoWorkspace(GetComponent(0)).EngineType=DaoApi.dbUseODBC then
         Result:='dbUseODBC'
      Else
         Result:='dbUseJet';
    End;
End;

Procedure TWorkEngineTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoWorkspace then
     Begin
       Proc('dbUseODBC');
       Proc('dbUseJet');
     End;
End;


procedure TWorkEngineTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoWorkspace then
       Begin
       if Value='dbUseODBC' Then
          Dat:=DaoApi.dbUseODBC
       Else
          Dat:=DaoApi.dbUseJet;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;

//*************************************************** DefaultCursorDriver Editor
Function TWorkDefaultCursorDriverTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TWorkDefaultCursorDriverTypeEditor.GetValue: string;
Var
 DB : TKADaoWorkspace;
Begin
 if GetComponent(0) is TKADaoWorkspace then
    Begin
     DB := TKADaoWorkspace(GetComponent(0));
     Result:='dbUseDefaultCursor';
     if DB.DefaultCursorDriver=DaoApi.dbUseDefaultCursor     then Result:='dbUseDefaultCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseODBCCursor        then Result:='dbUseODBCCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseServerCursor      then Result:='dbUseServerCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseClientBatchCursor then Result:='dbUseClientBatchCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseNoCursor          then Result:='dbUseNoCursor';
    End;
End;

Procedure TWorkDefaultCursorDriverTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoWorkspace then
     Begin
       Proc('dbUseDefaultCursor');
       Proc('dbUseODBCCursor');
       Proc('dbUseServerCursor');
       Proc('dbUseClientBatchCursor');
       Proc('dbUseNoCursor');
     End;
End;


procedure TWorkDefaultCursorDriverTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoWorkspace then
       Begin
       Dat:=DaoApi.dbUseDefaultCursor;
       if Value='dbUseDefaultCursor' Then Dat:=DaoApi.dbUseDefaultCursor
       Else
       if Value='dbUseODBCCursor' Then Dat:=DaoApi.dbUseODBCCursor
       Else
       if Value='dbUseServerCursor' Then Dat:=DaoApi.dbUseServerCursor
       Else
       if Value='dbUseClientBatchCursor' Then Dat:=DaoApi.dbUseClientBatchCursor
       Else
       if Value='dbUseNoCursor' Then Dat:=DaoApi.dbUseNoCursor;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;
//******************************************************************************
//                           EDITOR CODE - KADaoDatabase
//******************************************************************************
//******************************************************** KADaoDatabase Editors
Procedure TDBDatabaseTypeEditor.GetValues( Proc: TGetStrProc);
Var
  DBase : TKADaoDatabase;
  X     : Integer;
Begin
  if GetComponent(0) is TKADaoDatabase then
  Begin
    DBase := TKADaoDatabase(GetComponent(0));
    Try
      For X := 0 to DBase.F_DBTypesList.Count-1 do Proc(DBase.F_DBTypesList[X]);
    Finally
    End;
  End;
End;

Procedure TDBDatabaseTypeEditor.SetValue(const Value: string);
Begin
 if GetComponent(0) is TKADaoDatabase then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Function TDBDatabaseTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;


//************************************************************** Database Editor
Function TDBDatabaseNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDBDatabaseNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoDatabase then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDBDatabaseNameEditor.Edit;
var
   FileName : String;
   DBase    : TKADaoDatabase;
Begin
   DBase:=TKADaoDatabase(GetComponent(0));
   Filename := DBase.F_ChooseDatabase;
   if Filename <> '' then begin
      SetStrValue(Filename);
      Modified;
   end;
End;


//******************************************************* System Database Editor
Function TDBSystemDatabaseNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDBSystemDatabaseNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoDatabase then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDBSystemDatabaseNameEditor.Edit;
var
   DBase               : TKADaoDatabase;
   Filter              : String;
   DlgChooseDatabase   : TOpenDialog;
Begin
   DBase:=TKADaoDatabase(GetComponent(0));
   DlgChooseDatabase := TOpenDialog.Create(Nil);
   DlgChooseDatabase.FilterIndex:=1;
   DlgChooseDatabase.DefaultExt:='mdw';
   DlgChooseDatabase.InitialDir:= ExtractFilePath(DBase.SystemDatabase);
   DlgChooseDatabase.FileName  := ExtractFileName(DBase.SystemDatabase);
   if DlgChooseDatabase.FileName <> '' Then Filter := AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName));
   System.Delete(Filter,1,1);
   DlgChooseDatabase.DefaultExt:= Filter;
   if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mda' Then DlgChooseDatabase.FilterIndex:=1
   Else
     if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mdw' Then DlgChooseDatabase.FilterIndex:=1
   Else
     if AnsiLowerCase(ExtractFileExt(DlgChooseDatabase.FileName))='.mdb' Then DlgChooseDatabase.FilterIndex:=2
   Else
     DlgChooseDatabase.FilterIndex:=3;
   Filter:='Microsoft Access security files (*.mda *.mdw)|*.mda;*.mdw';
   Filter:=Filter+'|Microsoft Access (*.mdb)|*.mdb';
   Filter:=Filter+'|All files (*.*)|*.*';
   DlgChooseDatabase.Title:='Choose System Database:';
   DlgChooseDatabase.Options:=[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
   DlgChooseDatabase.Filter :=Filter;
   if DlgChooseDatabase.Execute then SetStrValue(DlgChooseDatabase.FileName);
   DlgChooseDatabase.Free;
   Modified;
End;


//*********************************************************** Engine Type Editor
Function TDBEngineTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TDBEngineTypeEditor.GetValue: string;
Begin
 if GetComponent(0) is TKADaoDatabase then
    Begin
      if TKADaoDatabase(GetComponent(0)).EngineType=DaoApi.dbUseODBC then
         Result:='dbUseODBC'
      Else
         Result:='dbUseJet';
    End;
End;

Procedure TDBEngineTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoDatabase then
     Begin
       Proc('dbUseODBC');
       Proc('dbUseJet');
     End;
End;


procedure TDBEngineTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoDatabase then
       Begin
       if Value='dbUseODBC' Then
          Dat:=DaoApi.dbUseODBC
       Else
          Dat:=DaoApi.dbUseJet;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;
//*************************************************** DefaultCursorDriver Editor
Function TDBDefaultCursorDriverTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TDBDefaultCursorDriverTypeEditor.GetValue: string;
Var
 DB : TKADaoDatabase;
Begin
 if GetComponent(0) is TKADaoDatabase then
    Begin
     DB := TKADaoDatabase(GetComponent(0));
     Result:='dbUseDefaultCursor';
     if DB.DefaultCursorDriver=DaoApi.dbUseDefaultCursor     then Result:='dbUseDefaultCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseODBCCursor        then Result:='dbUseODBCCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseServerCursor      then Result:='dbUseServerCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseClientBatchCursor then Result:='dbUseClientBatchCursor'
     Else
     if DB.DefaultCursorDriver=DaoApi.dbUseNoCursor          then Result:='dbUseNoCursor';
    End;
End;

Procedure TDBDefaultCursorDriverTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoDatabase then
     Begin
       Proc('dbUseDefaultCursor');
       Proc('dbUseODBCCursor');
       Proc('dbUseServerCursor');
       Proc('dbUseClientBatchCursor');
       Proc('dbUseNoCursor');
     End;
End;


procedure TDBDefaultCursorDriverTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoDatabase then
       Begin
       Dat:=DaoApi.dbUseDefaultCursor;
       if Value='dbUseDefaultCursor'     Then Dat:=DaoApi.dbUseDefaultCursor
       Else
       if Value='dbUseODBCCursor'        Then Dat:=DaoApi.dbUseODBCCursor
       Else
       if Value='dbUseServerCursor'      Then Dat:=DaoApi.dbUseServerCursor
       Else
       if Value='dbUseClientBatchCursor' Then Dat:=DaoApi.dbUseClientBatchCursor
       Else
       if Value='dbUseNoCursor'          Then Dat:=DaoApi.dbUseNoCursor;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;


//************************************************************* Workspase Editor
Procedure TDBWorkspaceEditor.GetValues( Proc: TGetStrProc);
Var
  DBase : TKADaoDatabase;
  X     : Integer;
Begin
  if GetComponent(0) is TKADaoDatabase then
  Begin
    DBase := TKADaoDatabase(GetComponent(0));
    Try
      For X := 0 to DBase.CoreDBEngine.Workspaces.Count-1 do
          Begin
            if AnsiCompareText(DBase.CoreDBEngine.Workspaces.Item[X].Name,
                               '#Default Workspace#') <> 0 Then
            Proc(DBase.CoreDBEngine.Workspaces.Item[X].Name);
          End;
    Finally
    End;
  End;
End;

Function TDBWorkspaceEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;

//*********************************************************** DAO Version Editor
{$IFDEF DYNADAO}
Procedure TDBDaoVersionEditor.GetValues( Proc: TGetStrProc);
Var
  DBase : TKADaoDatabase;
  X     : Integer;
Begin
  if GetComponent(0) is TKADaoDatabase then
  Begin
    DBase := TKADaoDatabase(GetComponent(0));
    Try
      For X := 0 to DBase.F_DaoVersionList.Count-1 do Proc(DBase.F_DaoVersionList[X]);
    Finally
    End;
  End;
End;

Procedure TDBDaoVersionEditor.SetValue(const Value: string);
Begin
 if GetComponent(0) is TKADaoDatabase then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Function TDBDaoVersionEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;
{$ENDIF}

//******************************************************************************
//                           EDITOR CODE - KADaoTable
//******************************************************************************
//*********************************************************** KADaoTable Editors
//******************************************************************** TableName
Procedure TTableNameEditor.GetValues( Proc: TGetStrProc);
Var
  DTable : TKADaoTable;
  DBase  : TKADaodatabase;
  X      : Integer;
Begin
  if GetComponent(0) is TKADaoTable then
  Begin
    DTable := TKADaoTable(GetComponent(0));
    if Assigned(DTable.Database) And (DTable.Database.Connected) Then
       Begin
        DBase:=DTable.Database;
        DBase.RefreshDefinitions;
        Try
          For X := 0 to DBase.TableNames.Count-1 do Proc(DBase.TableNames.Strings[X]);
        Finally
        End;
       End;
  End;
End;

Function TTableNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;
//******************************************************************** QueryName

Procedure TQueryNameEditor.GetValues( Proc: TGetStrProc);
Var
  DTable : TKADaoTable;
  DBase : TKADaodatabase;
  X     : Integer;
Begin
  if GetComponent(0) is TKADaoTable then
  Begin
    DTable := TKADaoTable(GetComponent(0));
    if Assigned(DTable.Database) And (DTable.Database.Connected) Then
       Begin
        DBase:=DTable.Database;
        DBase.RefreshDefinitions;
        Try
          For X := 0 to DBase.QueryDefNames.Count-1 do Proc(DBase.QueryDefNames.Strings[X]);
        Finally
        End;
       End;
  End;
End;

Function TQueryNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;
//******************************************************************** IndexName
Procedure TIndexNameEditor.GetValues( Proc: TGetStrProc);
Var
  DTable : TKADaoTable;
  DBase  : TKADaodatabase;
  X      : Integer;
  Count  : Integer;
Begin
  if GetComponent(0) is TKADaoTable then
  Begin
    DTable := TKADaoTable(GetComponent(0));
    if Assigned(DTable.Database) And (DTable.Database.Connected)  Then
    if DTable.TableName <> '' Then
       Begin
        DBase:=DTable.Database;
        Try
          DBase.RefreshDefinitions;
          Count :=DBase.CoreDatabase.TableDefs.Item[DTable.TableName].Indexes.Count;
          For X := 0 to  Count-1 do
              Begin
                Proc(DBase.CoreDatabase.TableDefs.Item[DTable.TableName].Indexes.Item[X].Name);
              End;
        Finally
        End;
       End;
  End;
End;


Function TIndexNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;

//******************************************************************************

Function TTableTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TTableTypeEditor.GetValue: string;
Begin
 if GetComponent(0) is TKADaoTable then
    Begin
      Result:='StandardTable';
      if TKADaoTable(GetComponent(0)).TableType=DaoApi.dbOpenTable       then Result:='StandardTable';
      if TKADaoTable(GetComponent(0)).TableType=DaoApi.dbOpenDynaset     then Result:='DynasetTable';
      if TKADaoTable(GetComponent(0)).TableType=DaoApi.dbOpenDynamic     then Result:='DynamicTable';
      if TKADaoTable(GetComponent(0)).TableType=DaoApi.dbOpenSnapshot    then Result:='SnapshotTable';
      if TKADaoTable(GetComponent(0)).TableType=DaoApi.dbOpenForwardOnly then Result:='ForwardOnlyTable';
    End;
End;

Procedure TTableTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoTable then
     Begin
       Proc('StandardTable');
       Proc('DynasetTable');
       Proc('DynamicTable');
       Proc('SnapshotTable');
       Proc('ForwardOnlyTable');
     End;
End;


Procedure TTableTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoTable then
       Begin
       Dat:=DaoApi.dbOpenTable;
       if Value='StandardTable'    Then Dat:=DaoApi.dbOpenTable;
       if Value='DynasetTable'     Then Dat:=DaoApi.dbOpenDynaset;
       if Value='DynamicTable'     Then Dat:=DaoApi.dbOpenDynamic;
       if Value='SnapshotTable'    Then Dat:=DaoApi.dbOpenSnapshot;
       if Value='ForwardOnlyTable' Then Dat:=DaoApi.dbOpenForwardOnly;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;

//******************************************************************************

Function TLockTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result := Inherited GetAttributes + [paValueList, paSortList];
End;

Function  TLockTypeEditor.GetValue: string;
Begin
 if GetComponent(0) is TKADaoTable then
    Begin
      Result:='dbPessimistic';
      if TKADaoTable(GetComponent(0)).LockType=DAOApi.dbPessimistic       then Result:='dbPessimistic';
      if TKADaoTable(GetComponent(0)).LockType=DAOApi.dbOptimistic        then Result:='dbOptimistic';
      if TKADaoTable(GetComponent(0)).LockType=DAOApi.dbOptimisticValue   then Result:='dbOptimisticValue';
      if TKADaoTable(GetComponent(0)).LockType=DAOApi.dbReadOnly          then Result:='dbReadOnly';
      if TKADaoTable(GetComponent(0)).LockType=DAOApi.dbOptimisticBatch   then Result:='dbOptimisticBatch';
    End;
End;

Procedure TLockTypeEditor.GetValues( Proc: TGetStrProc);
Begin
  if GetComponent(0) is TKADaoTable then
     Begin
       Proc('dbPessimistic');
       Proc('dbOptimistic');
       Proc('dbOptimisticValue');
       Proc('dbReadOnly');                                                            
       Proc('dbOptimisticBatch');
     End;
End;


Procedure TLockTypeEditor.SetValue(const Value: string);
Var
 Dat : Integer;
Begin
  if GetComponent(0) is TKADaoTable then
       Begin
       Dat:=DAOApi.dbPessimistic;
       if Value='dbPessimistic'     Then Dat:=DAOApi.dbPessimistic;
       if Value='dbOptimistic'      Then Dat:=DAOApi.dbOptimistic;
       if Value='dbOptimisticValue' Then Dat:=DAOApi.dbOptimisticValue;
       if Value='dbReadOnly'        Then Dat:=DAOApi.dbReadOnly;
       if Value='dbOptimisticBatch' Then Dat:=DAOApi.dbOptimisticBatch;
       Inherited SetValue(IntToStr(Dat));
       Modified;
     End;
End;


Function TSortByEditor.GetValue: string;
Begin
 Result := '(TStringList)'
End;

Function TSortByEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TSortByEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoTable then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Procedure TSortByEditor.Edit;
Var
 DT:TKADaoTable;
Begin
 if GetComponent(0) is TKADaoTable then
  Begin
    DT:=GetComponent(0) AS TKADaoTable;
    if NOT Assigned(DT.Database) Then Exit;
    if NOT (DT.Database.Connected) Then Exit;
    if (DT.Active=False) And (Not (csLoading in DT.ComponentState)) Then
       Begin
         Try
           DT.Active:=True;
           DT.Active:=False;
         Except
           DT.SortedBy.Clear;
           DatabaseError(E2051);
           Exit;
         End;
       End;
    Application.CreateForm(TSortByDialog,SortDialog);
    if SortDialog.Execute(DT.SortFieldNames,DT.SortedBy,DT.UseBrackets) Then
       Begin
         DT.Sort;
         Modified;
       End;
    SortDialog.Free;
  End;
End;

Function TQueryDefParamsEditor.GetValue: string;
Begin
 Result := '(TStringList)'
End;

Function TQueryDefParamsEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TQueryDefParamsEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoTable then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Procedure TQueryDefParamsEditor.Edit;
Var
 DT      : TKADaoTable;
Begin
 if GetComponent(0) is TKADaoTable then
  Begin
    DT:=GetComponent(0) AS TKADaoTable;
    if DT.PromptQueryDefParameters Then Modified;
  End;
End;


//******************************************************************************
Function TMasterFieldsEditor.GetValue: string;
Begin
 Result := '(TStringList)'
End;

Function TMasterFieldsEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TMasterFieldsEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoTable then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Procedure TMasterFieldsEditor.Edit;
Var
 MT,DT        : TKADaoTable;
 MF,DF,TF     : TStrings;
Begin
 if GetComponent(0) is TKADaoTable then
  Begin
    DT:=GetComponent(0) AS TKADaoTable;
    if NOT Assigned(DT.Database) Then Exit;
    If Not (DT.Database.Connected) Then Exit;
    if Not Assigned(DT.MasterSource) Then
       Begin
        DT.F_Master.Clear;
        DT.F_Detail.Clear;
        DT.MasterFields.Clear;
        DatabaseError(E2052);
        Exit;
       End;
    if NOT (DT.MasterSource.DataSet is TKADaoTable) Then
       Begin
        DT.F_Master.Clear;
        DT.F_Detail.Clear;
        DT.MasterFields.Clear;
        DatabaseError(E2053);
        Exit;
       End;
    MT:=DT.MasterSource.DataSet As TKADaoTable;
    if (DT.Active=False) and Not (csLoading in DT.ComponentState) Then
       Begin
         Try
           DT.Active:=True;
           DT.Active:=False;
         Except
           DT.F_Master.Clear;
           DT.F_Detail.Clear;
           DT.MasterFields.Clear;
           DatabaseError(E2054);
           Exit;
         End;
       End;
    if (MT.Active=False) and Not (csLoading in DT.ComponentState) Then
       Begin
         Try
           MT.Active:=True;
           MT.Active:=False;
         Except
           DT.F_Master.Clear;
           DT.F_Detail.Clear;
           DT.MasterFields.Clear;
           DatabaseError(E2055);
           Exit;
         End;
       End;
    MF     := TStringList.Create;
    DF     := TStringList.Create;
    TF     := TStringList.Create;
    MF.SetText(MT.F_MDFieldNames.GetText);
    DF.SetText(DT.F_MDFieldNames.GetText);
    TF.SetText(DT.MasterFields.GetText);
    Application.CreateForm(TMasterDetailForm,MasterDetailForm);
    if MasterDetailForm.Execute(DF,MF,TF) Then;
    MasterDetailForm.Free;
    DT.MasterFields:=TF;
    MF.Free;
    DF.Free;
    TF.Free;
    Modified;
  End;
End;

procedure TEncrypterEditor.GetValues(Proc: TGetStrProc);
Begin
 Designer.GetComponentNames(GetTypeData(TKADaoEncrypter.ClassInfo), Proc);
End;

//******************************************************************************
//                           EDITOR CODE - KADaoInfo
//******************************************************************************
//************************************************************ KADaoInfo Editors
//******************************************************************************
Function TDIDatabaseNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDIDatabaseNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoInfo then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDIDatabaseNameEditor.Edit;
var
   FileName           : String;
   DBase              : TKADaoInfo;
   DlgChooseDatabase  : TOpenDialog;
   Filter             : String;
Begin
   DBase:=TKADaoInfo(GetComponent(0));
   Filename := DBase.Database;
   DlgChooseDatabase := TOpenDialog.Create(Nil);
   if FileName = '' then
        Begin
           DlgChooseDatabase.FileName   := '';
        End
     Else
        Begin
           DlgChooseDatabase.FileName   := ExtractFileName(FileName);
           DlgChooseDatabase.InitialDir := ExtractFileDir(FileName);
        End;
   DlgChooseDatabase.FilterIndex:=1;     
   Filter:='Microsoft Access (*.mdb)|*.mdb';
   Filter:=Filter+'|All files (*.*)|*.*';
   DlgChooseDatabase.Title:='Choose MS Access Database:';
   DlgChooseDatabase.Options:=[ofPathMustExist,ofFileMustExist,ofHideReadOnly];
   DlgChooseDatabase.Filter :=Filter;
   DlgChooseDatabase.DefaultExt:='mdb';
   if DlgChooseDatabase.Execute then Filename := DlgChooseDatabase.FileName;
   if Filename <> '' then
      begin
      SetStrValue(Filename);
      Modified;
      end;
   DlgChooseDatabase.Free;
End;

//******************************************************************************
Function TDIDLLNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDIDLLNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TKADaoInfo then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDIDLLNameEditor.Edit;
var
   FileName           : String;
   DBase              : TKADaoInfo;
   DlgChooseDatabase  : TOpenDialog;
   Filter             : String;
Begin
   DBase:=TKADaoInfo(GetComponent(0));
   Filename := DBase.DaoInfoDll;
   DlgChooseDatabase := TOpenDialog.Create(Nil);
   if FileName = '' then
        Begin
           DlgChooseDatabase.FileName   := '';
        End
     Else
        Begin
           DlgChooseDatabase.FileName   := ExtractFileName(FileName);
           DlgChooseDatabase.InitialDir := ExtractFileDir(FileName);
        End;
   Filter:='Windows library (*.dll)|*.dll';
   Filter:=Filter+'|All files (*.*)|*.*';
   DlgChooseDatabase.Title:='Choose Micospft DAO Info Library:';
   DlgChooseDatabase.Options:=[ofPathMustExist,ofFileMustExist,ofHideReadOnly];
   DlgChooseDatabase.Filter :=Filter;
   DlgChooseDatabase.DefaultExt:='dll';
   if DlgChooseDatabase.Execute then Filename := DlgChooseDatabase.FileName;
   if Filename <> '' then
      begin
      SetStrValue(Filename);
      Modified;
      end;
   DlgChooseDatabase.Free;
End;                                   

//******************************************************************************
procedure Register;
begin
 //***************** KADaoDBEngine
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoDBEngine,'EngineType', TEngEngineTypeEditor);
 RegisterPropertyEditor(TypeInfo(String) ,TKADaoDBEngine,'SystemDatabase',TEngSystemDatabaseNameEditor);
 {$IFDEF DYNADAO}
 RegisterPropertyEditor(TypeInfo(String),TKADaoDBEngine, 'Version', TEngDaoVersionEditor);
 {$ENDIF}
 //***************** KADaoWorkspace
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoWorkspace,'EngineType', TWorkEngineTypeEditor);
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoWorkspace, 'DefaultCursorDriver', TWorkDefaultCursorDriverTypeEditor);
 //***************** KAAdoDatabase
 RegisterPropertyEditor(TypeInfo(String),TKADaoDatabase,'DatabaseType',TDBDatabaseTypeEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoDatabase,'Database',TDBDatabaseNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoDatabase,'SystemDatabase',TDBSystemDatabaseNameEditor);
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoDatabase, 'EngineType', TDBEngineTypeEditor);
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoDatabase, 'DefaultCursorDriver', TDBDefaultCursorDriverTypeEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoDatabase, 'Workspace', TDBWorkspaceEditor);
 {$IFDEF DYNADAO}
 RegisterPropertyEditor(TypeInfo(String),TKADaoDatabase, 'Version', TDBDaoVersionEditor);
 {$ENDIF}
 //***************** KAAdoTable
 RegisterPropertyEditor(TypeInfo(String),TKADaoTable,'TableName',TTableNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoTable,'QueryDefName',TQueryNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoTable,'IndexName',TIndexNameEditor);
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoTable,'TableType',TTableTypeEditor);
 RegisterPropertyEditor(TypeInfo(Integer),TKADaoTable,'LockType',TLockTypeEditor);
 RegisterPropertyEditor(TypeInfo(TStrings),TKADaoTable,'SortedBy',TSortByEditor);
 RegisterPropertyEditor(TypeInfo(TStrings),TKADaoTable,'QueryDefParameters',TQueryDefParamsEditor);
 RegisterPropertyEditor(TypeInfo(TStrings),TKADaoTable,'MasterFields',TMasterFieldsEditor);
 RegisterPropertyEditor(TypeInfo(TComponent),TKADaoTable,'Encrypter',TEncrypterEditor);
 //***************** KAAdoInfo
 RegisterPropertyEditor(TypeInfo(String),TKADaoInfo,'Database',TDIDatabaseNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TKADaoInfo,'DaoInfoDll',TDIDLLNameEditor);
end;

end.
