{$I fsdefine.inc}

Unit fsclreg;

Interface

Procedure Register;

Implementation

Uses
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
  fsclcoln,
  fsserverremoteclass,
  fsclsqle,
  fsclbase,
  fsconst,
  fsdbbase,
  fsdb,
  fsllbase,
  fsllgrid,
  fslllgcy,
  fslllog,
  fsserverclass,
  fsclfldg,
  fssrcmd,
  fssrsec,
  fsllthrd,
  fslleng,
  fsllcomm,
  fssqleng,
  fsexfield,
  fsllcomp;

{ TfsFieldLinkProperty }
Type
  TfsFieldLinkProperty = Class(TStringProperty)
  Public
    Procedure Edit; Override;
    Function GetAttributes: TPropertyAttributes; Override;
  End;

Procedure TfsFieldLinkProperty.Edit;
Var
  Table: TFSTable;
  lMasterTable: TDataset;
  lDetailIndex: TffShStr;
  lDetailFields: TffShStr;
  lMasterFields: TffShStr;
Begin
  Table := GetComponent(0) As TFSTable;
  With Table Do
    Begin
      If Not Assigned(MasterSource) Then {begin !!.06}
        {$IFDEF Delphi3}
        Begin
          ShowMessageFmt('The MasterSource property of ''%s'' must be linked to a DataSource', [Name]);
          Exit;
        End;
      {$ENDIF}
      {$IFDEF CBuilder3}
      Begin
        ShowMessageFmt('The MasterSource property of ''%s'' must be linked to a DataSource', [Name]);
        Exit;
      End;
      {$ENDIF}
      RaiseFSErrorObjFmt(Table, fsccDesign_SLinkMasterSource, [Name]);
      If Not Assigned(MasterSource.DataSet) Then
        {$IFDEF Delphi3}
        Begin
          ShowMessage('Unable to open the MasterSource Table');
          Exit;
        End;
      {$ENDIF}
      {$IFDEF CBuilder3}
      Begin
        ShowMessage('Unable to open the MasterSource Table');
        Exit;
      End;
      {$ENDIF}
      RaiseFSErrorObj(Table, fsccDesign_SLinkMaster); {end !!.06}
      lMasterTable := MasterSource.DataSet;
      lDetailIndex := IndexName;
      lDetailFields := IndexFieldNames;
      lMasterFields := GetValue;
    End;
  If ShowFieldLinkDesigner(lMasterTable,
    Table,
    lDetailIndex,
    lDetailFields,
    lMasterFields) = mrOK Then
    With Table Do
      Begin
        If lDetailIndex <> '' Then
          IndexName := lDetailIndex
        Else
          IndexFieldNames := lDetailFields;
        SetValue(lMasterFields);
      End;
End;

Function TfsFieldLinkProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paDialog, paRevertable];
End;

{ TfsDBStringProperty }

Type
  TfsDBStringProperty = Class(TStringProperty)
  Protected
    Procedure GetValueList(List: TStrings); Virtual;
  Public
    Function GetAttributes: TPropertyAttributes; Override;
    Procedure GetValues(Proc: TGetStrProc); Override;
  End;

Procedure TfsDBStringProperty.GetValueList(List: TStrings);
Begin
  { Do nothing - avoid compiler hint }
End;

Function TfsDBStringProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paValueList, paSortList, paMultiSelect];
End;

Procedure TfsDBStringProperty.GetValues(Proc: TGetStrProc);
Var
  i: Integer;
  Values: TStringList;
Begin
  Values := TStringList.Create;
  Try
    Values.BeginUpdate;
    Try
      GetValueList(Values);
      For i := 0 To Pred(Values.Count) Do
        Proc(Values[i]);
    Finally
      Values.EndUpdate;
    End;
  Finally
    Values.Free;
  End;
End;

{ TfsClientNameProperty }

Type
  TfsClientNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsClientNameProperty.GetValueList(List: TStrings);
Begin
  GetFSClientNames(List);
End;

{ TfsSessionNameProperty }

Type
  TfsSessionNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsSessionNameProperty.GetValueList(List: TStrings);
Begin
  GetFSSessionNames(List);
End;

{ TfsDatabaseNameProperty }

Type
  TfsDatabaseNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsDatabaseNameProperty.GetValueList(List: TStrings);
Var
  S: TFSSession;
Begin
  S := (GetComponent(0) As TFSDataSet).Session;
  If Assigned(S) Then
    GetFSDatabaseNames(S, List);
End;

{ TfsAliasNameProperty }

Type
  TfsAliasNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsAliasNameProperty.GetValueList(List: TStrings);
Var
  S: TFSSession;
Begin
  S := (GetComponent(0) As TFSDatabase).Session;
  If Assigned(S) Then
    S.GetAliasNames(List);
End;

{ TfsTableNameProperty }

Type
  TfsTableNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsTableNameProperty.GetValueList(List: TStrings);
Var
  DB: TFSDatabase;
Begin
  DB := TFSDatabase((GetComponent(0) As TFSTable).Database);
  If Assigned(DB) Then
    DB.GetTableNames(List);
End;

{ TfsIndexNameProperty }

Type
  TfsIndexNameProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsIndexNameProperty.GetValueList(List: TStrings);
Var
  Table: TFSTable;
Begin
  Table := GetComponent(0) As TFSTable;
  If Assigned(Table) Then
    Table.GetIndexNames(List);
End;

{ TfsIndexFieldNamesProperty }

Type
  TfsIndexFieldNamesProperty = Class(TfsDBStringProperty)
  Public
    Procedure GetValueList(List: TStrings); Override;
  End;

Procedure TfsIndexFieldNamesProperty.GetValueList(List: TStrings);
Var
  Table: TFSTable;
  i: Integer;
Begin
  Table := GetComponent(0) As TFSTable;
  If Assigned(Table) Then
    With Table Do
      Begin
        IndexDefs.Update;
        For i := 0 To Pred(IndexDefs.Count) Do
          With IndexDefs[i] Do
            If Not (ixExpression In Options) Then
              List.Add(Fields);
      End;
End;

{ TfsServerEngineProperty}
Type
  TfsServerEngineProperty = Class(TfsDBStringProperty)
  Public
    Function GetValue: String; Override;
    Procedure GetValueList(List: TStrings); Override;
    Procedure SetValue(Const aValue: String); Override;
  End;

Function TfsServerEngineProperty.GetValue: String;
Var
  i, j: Integer;
  Client: TFSClient; {!!.03}
  SvrEng: TFSBaseServerEngine;
  Cmpnt: TComponent;
  DataModule: TDataModule;
  Form: TForm;
Begin
  Result := '';
  Client := GetComponent(0) As TFSClient; {!!.03}
  If Assigned(Client) And Assigned(Client.ServerEngine) Then
    Begin
      If Client.OwnServerEngine Then
        Exit;

      SvrEng := Client.ServerEngine;
      {is the server engine on the table's form? if so just return the
       data source's name}
      For i := 0 To Pred(Client.Owner.ComponentCount) Do
        If (Client.Owner.Components[i] = SvrEng) Then
          Begin
            Result := SvrEng.Name;
            Exit;
          End;

      {is the master source on one of the project's data modules? if so
       return the data module name, period, and the data source's name}
      For j := 0 To Pred(Screen.DataModuleCount) Do
        Begin
          DataModule := Screen.DataModules[j];
          For i := 0 To pred(DataModule.ComponentCount) Do
            Begin
              Cmpnt := DataModule.Components[i];
              If (Cmpnt = SvrEng) {and
              Designer.IsComponentLinkable(Cmpnt)}Then
                Begin
                  Result := DataModule.Name + '.' + SvrEng.Name;
                  Exit;
                End;
            End;
        End;

      {is the master source on one of the project's forms? if so return the form
       name, period, and the data source's name}
      For j := 0 To pred(Screen.FormCount) Do
        Begin
          Form := Screen.Forms[j];
          For i := 0 To pred(Form.ComponentCount) Do
            Begin
              Cmpnt := Form.Components[i];
              If (Cmpnt = SvrEng) {and
              Designer.IsComponentLinkable(Cmpnt)}Then
                Begin
                  Result := Form.Name + '.' + SvrEng.Name;
                  Exit;
                End;
            End;
        End;

    End;
End;

Procedure TfsServerEngineProperty.GetValueList(List: TStrings);
Var
  i, j: Integer;
  Client: TFSClient;
  Cmpnt: TComponent;
  DataModule: TDataModule;
Begin
  Client := GetComponent(0) As TFSClient;
  If (Client <> Nil) And (Client.Owner <> Nil) Then
    Begin
      {first add all the names of the data sources on the table's owner}
      For i := 0 To pred(Client.Owner.ComponentCount) Do
        Begin
          Cmpnt := Client.Owner.Components[i];
          If (Cmpnt Is TFSBaseServerEngine) And
            (Cmpnt.Name <> '') Then
            List.Add(Cmpnt.Name);
        End;

      {then add all the names of the data sources on the project's data
       modules, at least those that can be linked; prefix with the data
       module name plus a period}
      For j := 0 To pred(Screen.DataModuleCount) Do
        Begin
          DataModule := Screen.DataModules[j];
          For i := 0 To pred(DataModule.ComponentCount) Do
            Begin
              Cmpnt := DataModule.Components[i];
              If (Cmpnt Is TFSBaseServerEngine) And
                Designer.IsComponentLinkable(Cmpnt) And
                (Cmpnt.Name <> '') Then
                Begin
                  List.Add(DataModule.Name + '.' + Cmpnt.Name);
                End;
            End;
        End;
    End;
End;

Procedure TfsServerEngineProperty.SetValue(Const aValue: String);
Var
  i, j: Integer;
  PosDot: Integer;
  Client: TFSClient;
  Cmpnt: TComponent;
  DataModule: TDataModule;
  DataModName: String;
  SvrEngName: String;
Begin
  Client := GetComponent(0) As TFSClient;
  If (Client <> Nil) And (Client.Owner <> Nil) Then
    Begin
      {assume we won't find the name; set the master source property
       to nil}
      Client.ServerEngine := Nil;
      If (aValue <> '') Then
        Begin
          {find the period in the master source name: its presence will
           indicate whether the component is on the same form or a
           separate data module}
          PosDot := Pos('.', aValue);
          If (PosDot = 0) {there is no period} Then
            Begin
              {find the data source on this form}
              For i := 0 To pred(Client.Owner.ComponentCount) Do
                Begin
                  Cmpnt := Client.Owner.Components[i];
                  If (Cmpnt Is TFSBaseServerEngine) And
                    (CompareText(Cmpnt.Name, aValue) = 0) Then
                    Begin
                      Client.ServerEngine := TFSBaseServerEngine(Cmpnt);
                      Exit;
                    End;
                End;
            End
          Else {there is a period}
            Begin
              DataModName := Copy(aValue, 1, pred(PosDot));
              SvrEngName := Copy(aValue, succ(PosDot), length(aValue));
              For j := 0 To pred(Screen.DataModuleCount) Do
                Begin
                  DataModule := Screen.DataModules[j];
                  If (CompareText(DataModule.Name, DataModName) = 0) Then
                    Begin
                      For i := 0 To pred(DataModule.ComponentCount) Do
                        Begin
                          Cmpnt := DataModule.Components[i];
                          If (Cmpnt Is TFSBaseServerEngine) And
                            Designer.IsComponentLinkable(Cmpnt) And
                            (CompareText(Cmpnt.Name, SvrEngName) = 0) Then
                            Begin
                              Client.ServerEngine := TFSBaseServerEngine(Cmpnt);
                              Exit;
                            End;
                        End;
                    End;
                End;
            End;
        End;
    End;
End;

{ TfsStringListProperty }
Type
  TfsStringListProperty = Class(TClassProperty)
  Public
    Procedure Edit; Override;
    Function GetAttributes: TPropertyAttributes; Override;
  End;

Procedure TfsStringListProperty.Edit;
Begin
  With TfsSQLEditor.Create(Application) Do
    Try
      SQLLines := GetOrdValue;
      ShowModal;
      If ModalResult = mrOK Then
        SetOrdValue(SQLLines);
    Finally
      Free;
    End;
End;

Function TfsStringListProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paDialog, paRevertable];
End;

{ TfsCollectionProperty }
Type
  TfsCollectionProperty = Class(TClassProperty)
  Public
    Procedure Edit; Override;
    Function GetAttributes: TPropertyAttributes; Override;
  End;

Procedure TfsCollectionProperty.Edit;
Begin
  FSShowParamEditor(Designer, TComponent(GetComponent(0)), GetName, GetOrdValue);
End;

Function TfsCollectionProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paDialog];
End;

{TfsServerEngineComponentEditor }
Type
  TfsServerEngineComponentEditor = Class(TComponentEditor)
    Function GetVerbCount: Integer; Override;
    Function GetVerb(Index: Integer): String; Override;
    Procedure ExecuteVerb(Index: Integer); Override;
  End;

Function TfsServerEngineComponentEditor.GetVerbCount: Integer;
Begin
  Result := 1;
End;

Function TfsServerEngineComponentEditor.GetVerb(Index: Integer): String;
Begin
  Case Index Of
    0: Result := 'Shutdown server engine';
    Else
      Result := 'ERROR!';
  End;
End;

Procedure TfsServerEngineComponentEditor.ExecuteVerb(Index: Integer);
Begin
  Case Index Of
    0: TfsStateComponent(Component).Shutdown;
    Else
      Assert(False);
  End;
End;

Procedure Register;
Begin
  RegisterComponents('FSSQL Client', [
    TFSRemoteServer,
      TFSClient,
      TFSSession,
      TFSDatabase,
      TFSTable,
      TFSQuery,
      TFSGrid
      ]);

  RegisterComponents('FSSQL Server', [
    TFSServer,
      TFSHandler,
      TFSParamConnect,
      TFSEventLog,
      TFSMonitor,
      TFSThreadPool
      ]);

  {register the experts}
  RegisterCustomModule(TFSBaseEngineManager, TCustomModule);

  {register the property editors...}
  {...for clients}
  RegisterPropertyEditor(TypeInfo(AnsiString), {!!.05}
    TFSClient,
    'ServerEngine',
    TfsServerEngineProperty);
  {...for sessions}
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSSession, 'ClientName', TfsClientNameProperty);
  {...for databases}
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSDatabase, 'AliasName', TfsAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSDatabase, 'SessionName', TfsSessionNameProperty);
  {...for tables}
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'SessionName', TfsSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'DatabaseName', TfsDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'TableName', TfsTableNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'IndexName', TfsIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'IndexFieldNames', TfsIndexFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSTable, 'MasterFields', TfsFieldLinkProperty);
  //  RegisterPropertyEditor(TypeInfo(TDataSource), TFSTable, 'MasterSource', TfsDataSourceProperty); {!!.06}
    {...for queries}
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSQuery, 'DatabaseName', TfsDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(TfsParams), TFSQuery, 'Params', TfsCollectionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFSQuery, 'SessionName', TfsSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFSQuery, 'SQL', TfsStringListProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TFSQuery, 'SQLORDER', TfsStringListProperty);
  RegisterClasses([TFSExtendedField, TFSBcdField, TFSCurrencyField,TfsIntegerField,
    TFSBlobField, TFSGraphicField, TFSMemoField, TFSFmtMemoField, TFSStringField,
    TfsArrayField]);
  RegisterFields([TFSExtendedField, TFSBcdField, TFSCurrencyField,TfsIntegerField,
    TFSBlobField, TFSGraphicField, TFSMemoField, TFSFmtMemoField, TFSStringField,
    TfsArrayField]);
  {register the component editors...}
  RegisterComponentEditor(TFSServer, TfsServerEngineComponentEditor);
  //  RegisterComponentEditor(TFSDatabase, TfsDatabaseEditor);
End;

End.

