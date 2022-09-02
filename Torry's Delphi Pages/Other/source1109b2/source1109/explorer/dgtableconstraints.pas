{$I fsdefine.inc}

Unit dgtableconstraints;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  jpeg,
  Controls,
  Forms,
  Dialogs,
  Db,
  Stdctrls,
  Grids,
  DBGrids,
  DBCtrls,
  ExtCtrls,
  Buttons,
  Menus,
  ComCtrls,
  fsdb,
  fsdbbase,
  fslllgcy,
  fsllbase,
  fssrbase,
  fsserverremoteclass,
  fsllprot,
  fslllog,
  fsutil,
  fsclbase,
  fssrbde,
  Mask,
  dgSetRng,
  uEntity,
  uConsts,
  Tabnotbk,
  fsexfield;

Type
  TdlgTableConstraints = Class(TForm)
    dsTableBrowser: TDataSource;
    navTableBrowser: TDBNavigator;
    barStatus: TStatusBar;
    MainMenu1: TMainMenu;
    mnuTable: TMenuItem;
    mnuTableClose: TMenuItem;
    mnuView: TMenuItem;
    mnuViewRefresh: TMenuItem;
    N2: TMenuItem;
    mnuViewShowRecordCount: TMenuItem;
    mnuOptions: TMenuItem;
    mnuOptionsTimeout: TMenuItem;
    paClient: TPanel;
    pcBlobfields: TPageControl;
    splGridAndPageControl: TSplitter;
    pnlIndex: TPanel;
    lblIndex: TLabel;
    cboIndex: TComboBox;
    tsMemoTemplate: TTabSheet;
    tsGraphicTemplate: TTabSheet;
    tsByteArrayTemplate: TTabSheet;
    cbStretch: TCheckBox;
    btnLoadGraphic: TButton;
    tsGenericBlobTemplate: TTabSheet;
    meGeneric: TMemo;
    mnuViewShowBLOBFields: TMenuItem;
    Label2: TLabel;
    btnClearBA: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnLoadGeneric: TButton;
    btnSaveGeneric: TButton;
    btnClearGeneric: TButton;
    btnSaveGraphic: TButton;
    btnClearGraphic: TButton;
    Label3: TLabel;
    meByteArray: TMaskEdit;
    dbMemo: TDBMemo;
    btnLoadMemo: TButton;
    btnSaveMemo: TButton;
    btnClearMemo: TButton;
    cbWordwrap: TCheckBox;
    mnuTableCopyToTable: TMenuItem;
    N5: TMenuItem;
    mnuTableDeleteRecords: TMenuItem;
    TabbedNotebook1: TTabbedNotebook;
    grdTableBrowser: TDBGrid;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    DBImage: TDBImage;
    Fontgrid1: TMenuItem;
    FontDialog1: TFontDialog;
    FTable1: TFSTable;
    Panel1: TPanel;
    DBCheckBox1: TDBCheckBox;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Splitter1: TSplitter;
    FTable1ACTIVE: TStringField;
    FTable1NAME: TStringField;
    FTable1METODENAME: TStringField;
    FTable1POSITION: TIntegerField;
    FTable1SOURCE: TStringField;
    FTable1SOURCEFIELD: TStringField;
    FTable1DESTINATION: TStringField;
    FTable1DESTINATIONFIELD: TStringField;
    FTable1EXPRESSION: TStringField;
    FTable1FLAGS: TIntegerField;
    FTable1DESCRIPTION: TFSMemoField;
    FTable1DATECREATE: TDateTimeField;
    Label4: TLabel;
    DBComboBox1: TDBComboBox;
    Label5: TLabel;
    DBComboBox2: TDBComboBox;
    Label6: TLabel;
    DBEdit2: TDBEdit;
    DBComboBox3: TDBComboBox;
    Label7: TLabel;
    DBEdit3: TDBEdit;
    Label8: TLabel;
    DBEdit4: TDBEdit;
    Label9: TLabel;
    FTable1DESTINATIONINDEX: TStringField;
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure cboIndexChange(Sender: TObject);
    Procedure mnuTableCloseClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure mnuViewRefreshClick(Sender: TObject);
    Procedure mnuViewShowFilterClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure mnuViewShowRecordCountClick(Sender: TObject);
    Procedure grdTableBrowserKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure mnuOptionsDebugClick(Sender: TObject);
    Procedure mnuOptionsTimeoutClick(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure cbStretchClick(Sender: TObject);
    Procedure btnClearBAClick(Sender: TObject);
    Procedure pcBlobfieldsChange(Sender: TObject);
    Procedure mnuViewShowBLOBFieldsClick(Sender: TObject);
    Procedure btnLoadMemoClick(Sender: TObject);
    Procedure btnSaveMemoClick(Sender: TObject);
    Procedure btnLoadGenericClick(Sender: TObject);
    Procedure btnSaveGenericClick(Sender: TObject);
    Procedure btnClearMemoClick(Sender: TObject);
    Procedure btnLoadGraphicClick(Sender: TObject);
    Procedure btnSaveGraphicClick(Sender: TObject);
    Procedure btnClearGraphicClick(Sender: TObject);
    Procedure btnClearGenericClick(Sender: TObject);
    Procedure meByteArrayKeyPress(Sender: TObject; Var Key: Char);
    Procedure mnuTableDesignReportClick(Sender: TObject);
    Procedure tsMemoTemplateResize(Sender: TObject);
    Procedure tsGraphicTemplateResize(Sender: TObject);
    Procedure tsGenericBlobTemplateResize(Sender: TObject);
    Procedure tsByteArrayTemplateResize(Sender: TObject);
    Procedure meByteArrayChange(Sender: TObject);
    Procedure cbWordwrapClick(Sender: TObject);
    Procedure mnuTableCopyToTableClick(Sender: TObject);
    Procedure mnuTableDeleteRecordsClick(Sender: TObject);
    Procedure Fontgrid1Click(Sender: TObject);
    Procedure DBEdit2KeyPress(Sender: TObject; Var Key: Char);
    Procedure DBComboBox2Change(Sender: TObject);
    Procedure dsTableBrowserDataChange(Sender: TObject; Field: TField); {!!.07}
  Private
    Procedure FTableAfterPost(DataSet: TDataSet); {!!.07}
    Procedure FTableAfterScroll(DataSet: TDataSet);
    Procedure FTableAfterCancel(DataSet: TDataSet);
    Procedure FTableBeforeEdit(DataSet: TDataSet);
    Procedure FTableBeforeInsert(DataSet: TDataSet);
    Procedure ViewActiveBlobField;
  Protected
    FClient: TFSClient;
    FDatabaseName: TffName;
    FEngine: TFSRemoteServer;
    FLog: TFSBaseLog;
    FProtocol: TfsProtocolType;
    FReadOnly: boolean;
    FServerName: TffNetAddress;
    FSession: TFSSession;
    FTableName: TffName;
    FUserName: TffName;
    FPassword: TffName;
    FTransport: TFSParamConnect;
    FTableItem: TffeTableItem;

    dtShown: boolean;
    {-Set to True if the form was actually displayed. Lets the form know
      it should save user preferences. }
    InRange: boolean;
    { true if SetRange has been performed }
    FRangeValues: TffRangeValues;
    { the start and end values for the active range }
    BeforeInitDone: Boolean;
    { to keep UpdateDisplay from being called repeatedly }
    BAKeyPressDetected: Boolean;
    { to avoid going to Edit mode when changing ByteArray edit programmatically }
    AddedComponentCount: Integer;
    { used to avoid duplicate names in dynamically added components }
    FDynEnabledComponents, {!!.11}
    FDynReadOnlyComponents: TList;
    { used to easily enable and disable the dynamically added components }
    Function HasBlobOrByteArrayField: Boolean; {!!.07}
  Protected { access methods }
    Procedure SetReadOnly(Const Value: Boolean);
  Public
    Procedure CloseDuringShow(Var Message: TMessage); Message ffm_Close;
    Procedure UpdateDisplay; {!!.01}
    Procedure UpdateDefaultTimeout; {!!.11}
    Procedure GetData(Var f, f1: String; SepL, SepR: String);
    Property Protocol: TfsProtocolType
      Read FProtocol Write FProtocol;

    Property ServerName: TffNetAddress
      Read FServerName Write FServerName;

    Property DataBaseName: TffName
      Read FDatabaseName Write FDatabaseName;

    Property Log: TFSBaseLog
      Read FLog Write FLog;

    Property Password: TffName
      Read FPassword Write FPassword;

    Property TableName: TffName
      Read FTableName Write FTableName;

    Property ReadOnly: boolean
      Read FReadOnly Write SetReadOnly;

    Property UserName: TffName
      Read FUserName Write FUserName;

    Property TableItem: TffeTableItem
      Read FTableItem Write FTableItem;
  End;

Var
  dlgTableConstraints: TdlgTableConstraints;

Implementation

Uses
  dgCpyTbl, {!!.10}
  typinfo, {!!.07}
  {$IFDEF DCC6ORLater}
  variants, {!!.07}
  {$ENDIF}
  FsLLComm,
  FsLLComp,
  FsLLEng,
  uConfig;

{$R *.DFM}

Const
  MaxFilterComboItems = 10; {!!.11}

Function GetVariable(f: String; Var i, j: Integer; SepL, SepR: String): String;
Var
  c: Integer;
Begin
  j := i;
  c := 0;
  Result := '';
  If f = '' Then
    Exit;
  Dec(j);
  Repeat
    Inc(j);
    If f[j] = SepL Then
      Begin
        If c = 0 Then
          i := j;
        Inc(c);
      End
    Else If f[j] = SepR Then
      Dec(c);
  Until (c = 0) Or (j >= Length(f));
  Result := Copy(f, i + 1, j - i - 1);
End;

Procedure TdlgTableConstraints.GetData(Var f, f1: String; SepL, SepR: String);
Var
  i, j: Integer;
  s1, s2: String;
Begin
  i := 1;
  Repeat
    While (i < Length(f)) And (f[i] <> SepL) Do
      Inc(i);
    s1 := GetVariable(f, i, j, SepL, SepR);
    If i <> j Then
      Begin
        s2 := '';
        s2 := s1;
        f1 := s2;
        System.Delete(f, i, j - i + 1);
        //Insert(s2, f, i); // s2- wewn¹trz zmienna f- przed zmienn¹
        Inc(i, Length(s2));
        j := 0;
      End;
  Until i = j;
End;

Procedure TdlgTableConstraints.FormCreate(Sender: TObject);
Begin

  FClient := Nil;
  FDatabaseName := '';
  FEngine := Nil;
  FLog := Nil;
  FProtocol := ptRegistry;
  FReadOnly := False;
  FServerName := '';
  FSession := Nil;
  FTableName := '';
  FTransport := Nil;
  FPassword := '';
  FUserName := '';

  InRange := False;
  BeforeInitDone := True;
  BAKeyPressDetected := False;
  AddedComponentCount := 0;
  FDynEnabledComponents := TList.Create; {!!.11}
  FDynReadOnlyComponents := TList.Create; {!!.11}
End;
{--------}

Procedure TdlgTableConstraints.SetReadOnly(Const Value: Boolean);
Var
  i: Integer;
  bm: Db.TBookmark;
  FieldsTags: TList;
Begin
  FReadOnly := Value;
  grdTableBrowser.ReadOnly := FReadOnly;
  {Begin !!.11}
  { only update the buttons after they are created,
    and table when it's opened. }
  If Not dtShown Then
    Exit;
  bm := FTable1.GetBookmark;
  FieldsTags := TList.Create;
  Try
    { save blob-support pointers }
    For i := 0 To Pred(FTable1.FieldCount) Do
      FieldsTags.Add(Pointer(FTable1.Fields[i].Tag));
    FTable1.Close;
    FTable1.ReadOnly := ReadOnly;
    FTable1.Open;
    For i := 0 To Pred(FTable1.FieldCount) Do
      FTable1.Fields[i].Tag := Integer(FieldsTags[i]);
    FTable1.GotoBookmark(bm);
  Finally
    FTable1.FreeBookmark(bm);
    FieldsTags.Free;
  End;
  For i := 0 To Pred(ComponentCount) Do
    If (Components[i] Is TButton) And
      (((Components[i] As TButton).Caption = 'Load from file...') Or
      ((Components[i] As TButton).Caption = 'Save to file...') Or
      ((Components[i] As TButton).Caption = 'Clear')) Then
      (Components[i] As TButton).Enabled := Not FReadOnly;
  {End !!.11}
End;
{--------}

Procedure TdlgTableConstraints.FormShow(Sender: TObject);
Var
  aServerName: String;
  aAddress: String;
  I: Integer;
  OldPass, OldUser: String;

  {$IFNDEF DCC5OrLater}

  Function IsPublishedProp(Source: TObject; Const PropName: String): Boolean;
  Var
    P: PPropInfo;
  Begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    Result := P <> Nil;
  End;
  {--------}

  Function GetStrProp(Source: TObject; Const PropName: String): String;
  Var
    P: PPropInfo;
  Begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    If Assigned(P) Then
      Begin
        Result := TypInfo.GetStrProp(Source, P);
      End
    Else
      Result := '';
  End;
  {--------}

  Function SetStrProp(Source: TObject; Const PropName, Value: String): String;
  Var
    P: PPropInfo;
  Begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    If Assigned(P) Then
      TypInfo.SetStrProp(Source, P, Value);
  End;
  {--------}

  Procedure SetMethodProp(Source: TObject; Const PropName: String; Value: TMethod);
  Var
    P: PPropInfo;
  Begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    If Assigned(P) Then
      TypInfo.SetMethodProp(Source, P, Value);
  End;
  {--------}

  Function GetMethodProp(Source: TObject; Const PropName: String): TMethod;
  Var
    P: PPropInfo;
  Begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    If Assigned(P) Then
      Result := TypInfo.GetMethodProp(Source, P);
  End;
  {$ENDIF}
  {Begin !!.07}

  Function CopyComponent(Source: TComponent): TComponent;
  Var
    PropStream: TMemoryStream;
    OldText, OldName: String;
  Begin
    Result := Nil;
    If assigned(Source) Then
      Begin
        PropStream := TMemoryStream.Create;
        Try
          //prevent doubled component names
          OldName := Source.Name;
          Source.Name := OldName + IntToStr(AddedComponentCount);
          Inc(AddedComponentCount);
          //Save the "stored" properties to memory
          PropStream.WriteComponent(Source);
          Source.Name := OldName;
          //e.g. TEdit will change it's content if renamed
          If IsPublishedProp(Source, 'Text') Then
            OldText := GetStrProp(Source, 'Text')
          Else If IsPublishedProp(Source, 'Caption') Then
            OldText := GetStrProp(Source, 'Caption');
          Result := TComponentClass(Source.ClassType).Create(Source.Owner);
          PropStream.Position := 0;
          PropStream.ReadComponent(Result);
          //        Result.Name := OldName + IntToStr(AddedComponentCount);
                  //Handle Components with a "Text" or "Caption" -property;
                  //e.g. TEdit, TLabel
          If IsPublishedProp(Source, 'Text') Then
            Begin
              SetStrProp(Source, 'Text', OldText);
              SetStrProp(Result, 'Text', OldText);
            End
          Else If IsPublishedProp(Source, 'Caption') Then
            Begin
              SetStrProp(Source, 'Caption', OldText);
              SetStrProp(Result, 'Caption', OldText);
            End;
        Finally
          PropStream.Free;
        End;
      End;
  End;

  { generates a new tabsheet and hooks up
    components on the new tabsheet to the field }

  Procedure CreateNewBlobTabSheet(SheetToCopy: TTabSheet; OnResizeProc: TNotifyEvent; FieldIndex: Integer);
  Var
    NewSheet: TTabSheet;
    Idx: Integer;
    NewComponent: TComponent;
  Begin
    NewSheet := TTabSheet.Create(pcBlobFields);
    NewSheet.PageControl := pcBlobFields;
    NewSheet.Caption := FTable1.Fields[FieldIndex].FieldName;
    {$IFDEF DCC4OrLater}
    NewSheet.OnResize := OnResizeProc;
    {$ENDIF}

    For Idx := 0 To SheetToCopy.ControlCount - 1 Do
      Begin
        NewComponent := CopyComponent(SheetToCopy.Controls[Idx]);
        TControl(NewComponent).Parent := NewSheet;
        If IsPublishedProp(NewComponent, 'DataField') Then
          SetStrProp(NewComponent, 'DataField', FTable1.Fields[FieldIndex].FieldName);
        If (IsPublishedProp(NewComponent, 'OnClick')) Then
          SetMethodProp(NewComponent, 'OnClick', GetMethodProp(SheetToCopy.Controls[Idx], 'OnClick'));
        If (IsPublishedProp(NewComponent, 'OnKeyPress')) Then
          SetMethodProp(NewComponent, 'OnKeyPress', GetMethodProp(SheetToCopy.Controls[Idx], 'OnKeyPress'));
        If (IsPublishedProp(NewComponent, 'OnChange')) Then
          SetMethodProp(NewComponent, 'OnChange', GetMethodProp(SheetToCopy.Controls[Idx], 'OnChange'));
        //      if NewComponent. IS TCheckBox
          //      SetStrProp(NewComponent, 'OnClick', FTable1.Fields.Fields[FieldIndex].FieldName);
              { save pointer to the control displaying the field }
        If (NewComponent Is TDbImage) Or { graphictemplate }
        (NewComponent Is TMaskEdit) Or { bytearraytemplate }
        (NewComponent Is TMemo) Or { generictemplate }
        (NewComponent Is TdbMemo) Then { memotemplate }
          FTable1.Fields[FieldIndex].Tag := Integer(NewComponent);

      End;
  End;
  {End !!.07}

Begin
  dtShown := False;
  mnuViewShowBLOBFields.Checked := True;
  Try
    { Set up the connection. }
    FTransport := TFSParamConnect.Create(Nil);
    With FTransport Do
      Begin
        Mode := fstmSend;
        Protocol := FProtocol;
        EventLog := FLog;
        If Assigned(FLog) Then
          Begin
            EventLogEnabled := True;
            EventLogOptions := [fstpLogErrors];
          End;
        ServerName := FServerName;
      End;

    FEngine := TFSRemoteServer.Create(Nil);
    FEngine.Transport := FTransport;

    FClient := TFSClient.Create(Nil);
    FClient.ServerEngine := FEngine;
    FClient.AutoClientName := True;
    FClient.TimeOut := Config.DefaultTimeout; {!!.11}

    FSession := TFSSession.Create(Nil);
    FSession.Passwords.Assign(FTableItem.Database.Server.Session.Passwords);
    FSession.ClientName := FClient.ClientName;
    FSession.AutoSessionName := True;
    OldPass := fsclPassword;
    OldUser := fsclUserName;
    Try
      If FPassword <> '' Then
        Begin
          fsclPassword := FPassword;
          fsclUserName := FUserName;
        End;
      FSession.Open;
    Finally
      fsclPassword := OldPass;
      fsclUserName := OldUser;
    End;

    FTable1.SessionName := FSession.SessionName;
    FTable1.DataBaseName := FDatabaseName;
    FTable1.TableName := FTableName;
    FTable1.AfterPost := FTableAfterPost; {!!.07}
    FTable1.AfterDelete := FTableAfterPost; {!!.07}
    FTable1.AfterScroll := FTableAfterScroll; {!!.07}
    FTable1.AfterCancel := FTableAfterCancel; {!!.07}
    FTable1.BeforeEdit := FTableBeforeEdit;
    FTable1.BeforeInsert := FTableBeforeInsert;
    FTable1.ReadOnly := ReadOnly; {!!.11}
    FTable1.RecLockedBeforeEdit := False;
    FTable1.Open;
    FTable1.Session.GetTableNames(FDatabaseName, '', False, False, DBComboBox1.Items);
    FTable1.Session.GetTableNames(FDatabaseName, '', False, False, DBComboBox3.Items);
    { Set up the indexes }
    cboIndex.Items.Clear;
    With FTable1.IndexDefs Do
      Begin
        Clear;
        Update;
        For I := 0 To Count - 1 Do
          cboIndex.Items.Add(Items[I].Name);
      End;

    cboIndex.ItemIndex := 0;
    FTable1.IndexName := cboIndex.Items[cboIndex.ItemIndex];

    { Update the find controls }
    cboIndexChange(Nil);

    FsSeparateAddress(FTransport.ServerName, aServerName, aAddress);
    Self.Caption := format('%s : %s : %s',
      [aServerName, FDatabaseName, FTableName]);

    dsTableBrowser.DataSet := FTable1;

    {Begin !!.07}
    { check if there are any BLOB fields in the table
      and populate the pagecontrol with appropriate controls if so }

    { make the templates invisible }
    For I := 0 To pcBlobFields.PageCount - 1 Do
      pcBlobFields.Pages[I].TabVisible := False;

    { generate new tabsheets for blobfields }
    For I := 0 To FTable1.Dictionary.FieldCount - 1 Do
      Begin
        Case FTable1.Dictionary.FieldType[I] Of
          fstBLOBMemo: CreateNewBlobTabSheet(tsMemoTemplate, tsMemoTemplateResize, I);
          // fstBLOBGraphic: CreateNewBlobTabSheet( tsGraphicTemplate, tsGraphicTemplateResize, I );
          fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: CreateNewBlobTabSheet(tsByteArrayTemplate, tsByteArrayTemplateResize, I);
          fstBLOB, fstBLOBGraphic: CreateNewBlobTabSheet(tsGenericBlobTemplate, tsGenericBlobTemplateResize, I);
        End;
      End;

    {End !!.07}

    BeforeInitDone := False;
    UpdateDisplay;

    pcBlobfields.Visible := mnuViewShowBLOBFields.Checked;
    splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked;
    If mnuViewShowBLOBFields.Checked Then
      ViewActiveBlobField;

    { make sure no column exceeds screen width }{!!.07}
    For I := 0 To grdTableBrowser.Columns.Count - 1 Do
      Begin
        If grdTableBrowser.Columns[i].Width > (Width Div 5) * 4 Then
          grdTableBrowser.Columns[i].Width := (Width Div 5) * 4;
      End;

    dtShown := True;
    { update newly created dynamic components }
    ReadOnly := FReadOnly; {!!.11}
    Height := 600;
    Width := 580;
    { large font support... }
    If (Screen.PixelsPerInch / PixelsPerInch) > 1.001 Then
      Begin
        Height := Round(Height * (Screen.PixelsPerInch / PixelsPerInch));
        Width := Round(Width * (Screen.PixelsPerInch / PixelsPerInch));
        barStatus.Height := Round(barStatus.Height * (Screen.PixelsPerInch / PixelsPerInch));
      End;

  Except
    On E: Exception Do
      Begin
        ShowMessage(E.message);
        PostMessage(Handle, ffm_Close, 0, Longint(Sender));
      End;
  End;
End;
{--------}

Procedure TdlgTableConstraints.cboIndexChange(Sender: TObject);
Var
  BaseSection: String;
  Index: Integer;
Begin
  BaseSection := ClassName + '.' + Self.Caption;
  With FTable1 Do
    If IndexName <> cboIndex.Items[cboIndex.ItemIndex] Then
      Begin
        IndexName := cboIndex.Items[cboIndex.ItemIndex];
      End;
End;
{--------}

Procedure TdlgTableConstraints.mnuTableCloseClick(Sender: TObject);
Begin
  Close;
End;
{--------}

Procedure TdlgTableConstraints.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  Action := caFree;
End;
{--------}

Procedure TdlgTableConstraints.mnuViewRefreshClick(Sender: TObject);
Begin
  FTable1.Refresh;
  UpdateDisplay;
End;
{--------}

Procedure TdlgTableConstraints.UpdateDisplay;
Begin
  If BeforeInitDone Then
    Exit;
  If mnuViewShowRecordCount.Checked Then
    barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable1.RecordCount, ThousandSeparator)
  Else
    barStatus.Panels[0].Text := '';
End;
{--------}

Procedure TdlgTableConstraints.mnuViewShowFilterClick(Sender: TObject);
Begin
End;
{--------}

Procedure TdlgTableConstraints.FormDestroy(Sender: TObject);
Begin
  {Begin !!.05 !!.10}
  Try
    FTable1.Close;
    TableItem.Table.Close;
  Except
  End;

  Try
    FSession.Active := False;
  Finally
    FSession.Free;
  End;

  Try
    FClient.Close;
  Finally
    FClient.Free;
  End;

  Try
    FEngine.Shutdown;
  Finally
    FEngine.Free;
  End;

  Try
    FTransport.Shutdown;
  Finally
    FTransport.Free;
  End;
  {End !!.05}
  FDynEnabledComponents.Free; {!!.11}
  FDynReadOnlyComponents.Free; {!!.11}
End;
{--------}

Procedure TdlgTableConstraints.CloseDuringShow(Var Message: TMessage);
Begin
  Close;
End;
{--------}
{--------}

{--------}

Procedure TdlgTableConstraints.mnuViewShowRecordCountClick(Sender: TObject);
Begin
  mnuViewShowRecordCount.Checked := Not mnuViewShowRecordCount.Checked;
  UpdateDisplay;
End;

Procedure TdlgTableConstraints.grdTableBrowserKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  DoPost: Boolean; {!!.11}
Begin
  { Delete record? }
  If ((key = VK_DELETE) And
    (shift = []) And
    (dsTableBrowser.State = dsBrowse)) And
    (Not grdTableBrowser.ReadOnly) Then
    If (MessageDlg('Delete record?', mtConfirmation, mbOKCancel, 0) <> idCancel) Then
      dsTableBrowser.DataSet.Delete;
  {Begin !!.11}
  { set field to NULL? }
  If ((key = Ord('0')) And
    (shift = [ssCtrl]) And
    (Not grdTableBrowser.ReadOnly) And
    (Not FTable1.IsEmpty)) Then
    Begin
      DoPost := Not (FTable1.State In [dsInsert, dsEdit]);
      If DoPost Then
        FTable1.Edit;
      grdTableBrowser.SelectedField.Clear;
      If DoPost Then
        FTable1.Post;
      { refresh; could be blobfield }
      ViewActiveBlobField;
    End;
  {End !!.11}
End;
{Begin !!.02}
{--------}

Procedure TdlgTableConstraints.mnuOptionsDebugClick(Sender: TObject);
Begin
End;
{End !!.02}

{Begin !!.07}

Procedure TdlgTableConstraints.FTableAfterPost(DataSet: TDataSet);
Begin
  If FTable1.Database.InTransaction Then
    FTable1.Database.Commit;
  UpdateDisplay;
End;
{--------}

Procedure TdlgTableConstraints.FTableAfterCancel(DataSet: TDataSet);
Begin
  If FTable1.Database.InTransaction Then
    FTable1.Database.Rollback;
  FTable1.Refresh;
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTableConstraints.FTableAfterScroll(DataSet: TDataSet);
Begin
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTableConstraints.FTableBeforeEdit(DataSet: TDataSet);
Begin
  If Not FTable1.Database.InTransaction Then
    FTable1.Database.StartTransaction;
End;
{--------}

Procedure TdlgTableConstraints.FTableBeforeInsert(DataSet: TDataSet);
Begin
  If Not FTable1.Database.InTransaction Then
    FTable1.Database.StartTransaction;
End;
{--------}

Procedure TdlgTableConstraints.mnuOptionsTimeoutClick(Sender: TObject);
Var
  sTimeout: String;
  res: Boolean;
Begin
  sTimeout := IntToStr(FTable1.Timeout);
  Repeat
    res := InputQuery('Table Timeout (ms)', 'Value:', sTimeout);
    If res Then
      Try
        FTable1.Timeout := StrToInt(sTimeout);
        If FTable1.Timeout < -1 Then
          Raise EConvertError.Create('');
        res := False;
      Except
        On EConvertError Do
          Begin
            MessageDlg('Value must be a number between -1 and ' + IntToStr(MaxInt), mtError, [mbOK], 0);
          End;
      End;
  Until Not res;
End;
{--------}

Procedure TdlgTableConstraints.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If (Not (FTable1.State In [dsInsert, dsEdit])) And
    (Key = VK_ESCAPE) Then
    Close;
End;
{--------}

Procedure TdlgTableConstraints.ViewActiveBlobField;
Const
  JPEGHeader: Array[0..10] Of Char = (Chr($FF), Chr($D8), Chr($FF), Chr($E0),
    Chr($0), Chr($10), 'J', 'F', 'I', 'F', Chr(0));
  BMPHeader: Array[0..1] Of char = ('B', 'M');
  WMFHeader: Array[0..1] Of char = ('B', 'M');
  ICOHeader: Array[0..1] Of char = ('B', 'M');
  HexChar: Array[0..15] Of char = '0123456789ABCDEF';
Var
  HeaderBuffer: Array[0..10] Of char;
  Stream: TStream;
  jpegImage: TJPEGImage;
  i: Integer;
  BlobBuffer: Array[0..1024] Of char;
  ByteArrayBuffer: Pointer;
  tempStr: String;

  { copied from TffEventLog.WriteBlock and transmogrified }

  Function GenerateHexLines(Buf: pointer; BufLen: TffMemSize): String;
  Const
    HexPos: Array[0..15] Of Byte =
    (1, 3, 5, 7, 10, 12, 14, 16, 19, 21, 23, 25, 28, 30, 32, 34);
  Var
    B: PffByteArray Absolute Buf;
    ThisWidth,
      i, j: Integer;
    Line: String[56];
    Work: Byte;
  Begin
    Result := '';
    If (BufLen = 0) Or (Buf = Nil) Then
      Exit
    Else
      Begin
        If (BufLen > 1024) Then
          Begin
            BufLen := 1024;
          End;
        For i := 0 To ((BufLen - 1) Shr 4) Do
          Begin
            FillChar(Line, 56, ' ');
            Line[0] := #55;
            Line[38] := '[';
            Line[55] := ']';
            If (BufLen >= 16) Then
              ThisWidth := 16
            Else
              ThisWidth := BufLen;
            For j := 0 To ThisWidth - 1 Do
              Begin
                Work := B^[(i Shl 4) + j];
                Line[HexPos[j]] := HexChar[Work Shr 4];
                Line[HexPos[j] + 1] := HexChar[Work And $F];
                If (Work < 32) {or (Work >= $80)} Then
                  Work := ord('.');
                Line[39 + j] := char(Work);
              End;
            Result := Result + Line + ffcCRLF;
            dec(BufLen, ThisWidth);
          End;
      End;
  End;

  Function ByteArrayToHexString(ByteArray: Pointer; ArrayLength: Integer): String;
  Var
    idx: Integer;
    BArr: PffByteArray Absolute ByteArray;
  Begin
    Result := '';
    For idx := 0 To ArrayLength - 1 Do
      Begin
        Result := Result + HexChar[BArr[idx] Shr 4];
        Result := Result + HexChar[BArr[idx] And $F];
      End;
  End;

Begin
  { load non-db blob controls }
  If mnuViewShowBLOBFields.Checked And
    HasBlobOrByteArrayField Then
    Begin

      For i := 0 To FTable1.Dictionary.FieldCount - 1 Do
        Begin
          { only load blob on active tabsheet }
          If Assigned(Pointer(FTable1.Fields[i].Tag)) And
            (FTable1.Fields[i].FieldName = pcBlobfields.ActivePage.Caption) Then
            Case FTable1.Dictionary.FieldType[i] Of
              fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
                Begin
                  With TMaskEdit(FTable1.Fields[i].Tag) Do
                    Begin
                      Text := '';
                      MaxLength := FTable1.Fields[i].Size * 2;
                      SetLength(tempStr, MaxLength);
                      FillChar(tempStr[1], MaxLength, 'a');
                      EditMask := tempStr + ';0;_';
                      GetMem(ByteArrayBuffer, FTable1.Fields[i].DataSize);
                      Try
                        If FTable1.Fields[i].GetData(ByteArrayBuffer) Then
                          Text := ByteArrayToHexString(ByteArrayBuffer, FTable1.Fields[i].DataSize);
                      Finally
                        FreeMem(ByteArrayBuffer);
                      End;
                    End;
                End;
              fstBLOB, fstBLOBGraphic:
                Begin
                  Try
                    Stream := FTable1.CreateBlobStream(FTable1.Fields[i], bmRead);
                    Try
                      TMemo(FTable1.Fields[i].Tag).Text := '';
                      Stream.Read(BlobBuffer, FFMinL(1024, Stream.Size));
                      TMemo(FTable1.Fields[i].Tag).Text :=
                        GenerateHexLines(@BlobBuffer, FFMinL(1024, Stream.Size));
                    Finally
                      Stream.Free;
                    End;
                  Except
                    On E: Exception Do
                      Begin
                        ShowMessage('Exception: ' + E.Message + ' when displaying blob field: ' + FTable1.Fields[i].FieldName);
                      End;
                  End;
                End;
            End;
        End;
    End;

End;
{--------}

Function TdlgTableConstraints.HasBlobOrByteArrayField: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  For i := 0 To FTable1.Dictionary.FieldCount - 1 Do
    If FTable1.Dictionary.FieldType[i] In [fstBLOB..ffcLastBLOBType, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble] Then
      Begin
        Result := True;
        Exit;
      End;
End;
{--------}

Procedure TdlgTableConstraints.cbStretchClick(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To TCheckBox(Sender).Parent.ControlCount - 1 Do
    If TCheckBox(Sender).Parent.Controls[i] Is TDbImage Then
      Begin
        TDbImage(TCheckBox(Sender).Parent.Controls[i]).Stretch := TCheckBox(Sender).Checked;
        Exit;
      End;
End;
{--------}

Procedure TdlgTableConstraints.btnClearBAClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aEdit: TMaskEdit;
  aField: TField;
Begin
  { find edit control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TMaskEdit Then
      Begin
        aEdit := TMaskEdit(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable1.Fields[fieldIdx].Tag) = aEdit Then
              Begin
                aField := FTable1.Fields[fieldIdx];
                If Not (FTable1.State In [dsInsert, dsEdit]) Then
                  FTable1.Edit;
                aField.Clear;
                aEdit.Text := '';
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTableConstraints.pcBlobfieldsChange(Sender: TObject);
Begin
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTableConstraints.mnuViewShowBLOBFieldsClick(Sender: TObject);
Begin
  mnuViewShowBLOBFields.Checked := Not mnuViewShowBLOBFields.Checked;
  pcBlobfields.Visible := mnuViewShowBLOBFields.Checked;
  splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked;
  If mnuViewShowBLOBFields.Checked Then
    ViewActiveBlobField;
End;
{--------}

Procedure TdlgTableConstraints.btnLoadMemoClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  adbMemo: TdbMemo;
  aField: TField;
Begin
  If opendialog.Execute Then
    { find dbmemo control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TdbMemo Then
        Begin
          adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = adbMemo Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  If Not (FTable1.State In [dsInsert, dsEdit]) Then
                    FTable1.Edit;
                  TMemoField(aField).LoadFromFile(opendialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnSaveMemoClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  adbMemo: TdbMemo;
  aField: TField;
Begin
  If savedialog.Execute Then
    { find dbmemo control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TdbMemo Then
        Begin
          adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = adbMemo Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  TMemoField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnLoadGenericClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aMemo: TMemo;
  aField: TField;
Begin
  If opendialog.Execute Then
    { find memo control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TMemo Then
        Begin
          aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = aMemo Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  If Not (FTable1.State In [dsInsert, dsEdit]) Then
                    FTable1.Edit;
                  TBlobField(aField).LoadFromFile(opendialog.FileName);
                  ViewActiveBlobField;
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnSaveGenericClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aMemo: TMemo;
  aField: TField;
Begin
  If savedialog.Execute Then
    { find memo control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TMemo Then
        Begin
          aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = aMemo Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  TBlobField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnClearMemoClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  adbMemo: TdbMemo;
  aField: TField;
Begin
  { find dbmemo control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TdbMemo Then
      Begin
        adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable1.Fields[fieldIdx].Tag) = adbMemo Then
              Begin
                aField := FTable1.Fields[fieldIdx];
                If Not (FTable1.State In [dsInsert, dsEdit]) Then
                  FTable1.Edit;
                aField.Clear;
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTableConstraints.btnLoadGraphicClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aImage: TDbImage;
  aField: TField;
Begin
  If opendialog.Execute Then
    { find Image control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TDbImage Then
        Begin
          aImage := TDbImage(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = aImage Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  If Not (FTable1.State In [dsInsert, dsEdit]) Then
                    FTable1.Edit;
                  TFSGraphicField(aField).LoadFromFile(opendialog.FileName);
                  // ViewActiveBlobField;
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnSaveGraphicClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aImage: TImage;
  aField: TField;
Begin
  If savedialog.Execute Then
    { find Image control }
    For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
      If TButton(Sender).Parent.Controls[controlIdx] Is TImage Then
        Begin
          aImage := TImage(TButton(Sender).Parent.Controls[controlIdx]);
          { find correct field }
          For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable1.Fields[fieldIdx].Tag) = aImage Then
                Begin
                  aField := FTable1.Fields[fieldIdx];
                  TBlobField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTableConstraints.btnClearGraphicClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aImage: TDbImage;
  aField: TField;
Begin
  { find image control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TDbImage Then
      Begin
        aImage := TDbImage(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable1.Fields[fieldIdx].Tag) = aImage Then
              Begin
                aField := FTable1.Fields[fieldIdx];
                If Not (FTable1.State In [dsInsert, dsEdit]) Then
                  FTable1.Edit;
                TGraphicField(aField).Clear;
                //ViewActiveBlobField;
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTableConstraints.btnClearGenericClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aMemo: TMemo;
  aField: TField;
Begin
  { find memo control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TMemo Then
      Begin
        aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable1.Fields[fieldIdx].Tag) = aMemo Then
              Begin
                aField := FTable1.Fields[fieldIdx];
                If Not (FTable1.State In [dsInsert, dsEdit]) Then
                  FTable1.Edit;
                aField.Clear;
                ViewActiveBlobField;
                Exit;
              End;
          End;
      End;
End;

Procedure TdlgTableConstraints.meByteArrayKeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (Key In [#8, #9, #27, '0'..'9', 'A'..'F', 'a'..'f']) Then
    Key := #0
  Else
    BAKeyPressDetected := True;
End;

Procedure TdlgTableConstraints.mnuTableDesignReportClick(Sender: TObject);
Begin
End;

{ magic resize numbers: 100 = width of buttons + 8 pixels of space on each side }

Procedure TdlgTableConstraints.tsMemoTemplateResize(Sender: TObject);
Var
  controlIdx: Integer;
Begin
  For controlIdx := 0 To TTabSheet(Sender).ControlCount - 1 Do
    If TTabSheet(Sender).Controls[controlIdx] Is TdbMemo Then
      Begin
        TdbMemo(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width - 116, TTabSheet(Sender).Height);
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TButton Then
      Begin
        TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 108;
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TCheckBox Then
      Begin
        TCheckBox(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 108;
      End;
End;

Procedure TdlgTableConstraints.tsGraphicTemplateResize(Sender: TObject);
Var
  controlIdx: Integer;
Begin
  For controlIdx := 0 To TTabSheet(Sender).ControlCount - 1 Do
    If TTabSheet(Sender).Controls[controlIdx] Is TdbImage Then
      Begin
        TImage(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width - 116, TTabSheet(Sender).Height);
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TButton Then
      Begin
        TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 108;
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TCheckBox Then
      Begin
        TCheckBox(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 108;
      End;
End;

Procedure TdlgTableConstraints.tsGenericBlobTemplateResize(Sender: TObject);
Var
  controlIdx: Integer;
Begin
  For controlIdx := 0 To TTabSheet(Sender).ControlCount - 1 Do
    If TTabSheet(Sender).Controls[controlIdx] Is TMemo Then
      Begin
        TMemo(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width - 116, TTabSheet(Sender).Height);
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TButton Then
      Begin
        TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 108;
      End;
End;

Procedure TdlgTableConstraints.tsByteArrayTemplateResize(Sender: TObject);
Var
  controlIdx: Integer;
Begin
  For controlIdx := 0 To TTabSheet(Sender).ControlCount - 1 Do
    If TTabSheet(Sender).Controls[controlIdx] Is TMaskEdit Then
      Begin
        TMaskEdit(TTabSheet(Sender).Controls[controlIdx]).Width := TTabSheet(Sender).Width - 2 * TMaskEdit(TTabSheet(Sender
          ).Controls[controlIdx]).Left;
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TLabel Then
      Begin
        TLabel(TTabSheet(Sender).Controls[controlIdx]).Width := TTabSheet(Sender).Width - 2 * TLabel(TTabSheet(Sender
          ).Controls[controlIdx]).Left;
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TButton Then
      Begin
        If TButton(TTabSheet(Sender).Controls[controlIdx]).Caption = 'Clear' Then
          TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 83;
      End;
End;

Procedure TdlgTableConstraints.meByteArrayChange(Sender: TObject);
Var
  ByteArrayBuffer: Pointer;
  fieldIdx,
    controlIdx: Integer;
  aEdit: TMaskEdit;
  aField: TField;

  Procedure HexStringToByteArray(ByteArray: Pointer; ArrayLength: Integer; S: String);
  Var
    idx: Integer;
    BArr: PffByteArray Absolute ByteArray;
  Begin
    For idx := 0 To ArrayLength - 1 Do
      Begin
        If Odd(Length(S)) Then
          S := S + '0';
        If Length(S) > 1 Then
          Begin
            Try
              BArr[idx] := StrToInt('$' + Copy(S, 1, 2));
            Except
              On EConvertError Do
                Begin
                  MessageDlg('Invalid character encountered - use only hex digits 0..9, A..F!', mtError, [mbOK], 0);
                  Abort;
                End;
            End;
            Delete(S, 1, 2);
          End
        Else
          Begin
            BArr[idx] := 0;
            BArr[idx] := 0;
          End;
      End;
  End;

Begin
  If Not BAKeyPressDetected Then
    Exit
  Else
    BAKeyPressDetected := False;
  FTable1.Edit;
  { find edit control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TMaskEdit Then
      Begin
        aEdit := TMaskEdit(TButton(Sender).Parent.Controls[controlIdx]);
        If aEdit.Text = '' Then
          Exit;
        { find correct field }
        For fieldIdx := 0 To FTable1.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable1.Fields[fieldIdx].Tag) = aEdit Then
              Begin
                aField := FTable1.Fields[fieldIdx];
                If Not (FTable1.State In [dsInsert, dsEdit]) Then
                  FTable1.Edit;
                GetMem(ByteArrayBuffer, aField.Size);
                Try
                  HexStringToByteArray(ByteArrayBuffer, aField.Size, aEdit.Text);
                  aField.SetData(ByteArrayBuffer);
                Finally
                  FreeMem(ByteArrayBuffer);
                End;
                Exit;
              End;
          End;
      End;
End;

Procedure TdlgTableConstraints.cbWordwrapClick(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To TCheckBox(Sender).Parent.ControlCount - 1 Do
    If TCheckBox(Sender).Parent.Controls[i] Is TdbMemo Then
      Begin
        With TdbMemo(TCheckBox(Sender).Parent.Controls[i]) Do
          Begin
            WordWrap := TCheckBox(Sender).Checked;
            If WordWrap Then
              ScrollBars := ssVertical
            Else
              ScrollBars := ssBoth;
          End;
        Exit;
      End;
End;

Procedure TdlgTableConstraints.mnuTableCopyToTableClick(Sender: TObject);
Var
  ExcludeIndex,
    TableIndex, CommitPerTr: Longint;
  CopyBlobs: Boolean;
  SaveTimeout: Integer;
Begin
  ExcludeIndex := TableItem.Database.IndexOf(TableItem);
  CommitPerTr := 0;
  If ShowCopyTableDlg(TableItem.Database, ExcludeIndex, FTable1,
    TableIndex, CopyBlobs, FTableItem, CommitPerTr) = mrOK Then
    Begin {!!.11}
      With TableItem.Database.Tables[TableIndex] Do
        Begin
          Screen.Cursor := crHourGlass;
          { the copy operation is used in the context of the table
            that's being copied to. Use the timeout of the active
            table, otherwise the user has no way of setting timeout. }
          SaveTimeout := Table.Timeout;
          Table.Timeout := FTable1.Timeout;
          Try
            Update;
            CopyRecords(FTable1, CopyBlobs, CommitPerTr);
          Finally
            Screen.Cursor := crDefault;
            Table.Timeout := SaveTimeout;
            { force the second table to close if it wasn't open before }
            FSession.CloseInactiveTables; {!!.11}
          End;
        End;
    End;
End;

Procedure TdlgTableConstraints.mnuTableDeleteRecordsClick(Sender: TObject);
Begin
  If MessageDlg('Delete all records matching the current filter and range - are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      Screen.Cursor := crHourGlass;
      Try
        Update;
        FTable1.DeleteRecords(0);
      Finally
        Screen.Cursor := crDefault;
      End;
    End;
End;
{End !!.07}

Procedure TdlgTableConstraints.UpdateDefaultTimeout;
Begin
  FClient.TimeOut := Config.DefaultTimeout; {!!.11}
End;

Procedure TdlgTableConstraints.Fontgrid1Click(Sender: TObject);
Begin
  If FontDialog1.Execute Then
    grdTableBrowser.Font := FontDialog1.Font;
  grdTableBrowser.Refresh;
End;

Procedure TdlgTableConstraints.DBEdit2KeyPress(Sender: TObject;
  Var Key: Char);
Begin
  If key In ['-'] Then key := #0;
End;

Procedure TdlgTableConstraints.DBComboBox2Change(Sender: TObject);
Begin
  Case DBComboBox2.ItemIndex Of
    -1:
      Begin
        Label4.caption := 'Not Assigned';
        Label7.caption := 'Not Assigned';
      End;
    0..6:
      Begin
        Label4.caption := 'Master Table:';
        Label7.caption := 'Detail Table (References):';
      End;
  End;
End;

Procedure TdlgTableConstraints.dsTableBrowserDataChange(Sender: TObject;
  Field: TField);
Begin
  DBComboBox2Change(Nil);
End;

End.

