{$I fsdefine.inc}

Unit dgtable;

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
  fsexfield,
  Tabnotbk;

Type
  TdlgTable = Class(TForm)
    dsTableBrowser: TDataSource;
    barStatus: TStatusBar;
    MainMenu1: TMainMenu;
    mnuTable: TMenuItem;
    mnuTableClose: TMenuItem;
    mnuView: TMenuItem;
    mnuViewRefresh: TMenuItem;
    N2: TMenuItem;
    mnuViewShowRecordCount: TMenuItem;
    mnuViewShowFilter: TMenuItem;
    mnuTableResetCol: TMenuItem;
    mnuOptions: TMenuItem;
    mnuOptionsDebug: TMenuItem;
    mnuOptionsTimeout: TMenuItem;
    N3: TMenuItem;
    paClient: TPanel;
    pcBlobfields: TPageControl;
    splGridAndPageControl: TSplitter;
    pnlIndex: TPanel;
    lblIndex: TLabel;
    cboIndex: TComboBox;
    lblFind: TLabel;
    edtFind: TEdit;
    btnFindNear: TButton;
    pnlFilter: TPanel;
    lblFilter: TLabel;
    btnSetFilter: TButton;
    pnlRange: TPanel;
    laRangeStartDesc: TLabel;
    btnSetClearRange: TButton;
    tsMemoTemplate: TTabSheet;
    tsGraphicTemplate: TTabSheet;
    tsByteArrayTemplate: TTabSheet;
    cbStretch: TCheckBox;
    btnLoadGraphic: TButton;
    tsGenericBlobTemplate: TTabSheet;
    meGeneric: TMemo;
    mnuViewShowRange: TMenuItem;
    mnuViewShowBLOBFields: TMenuItem;
    btnClearBA: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnLoadGeneric: TButton;
    btnSaveGeneric: TButton;
    btnClearGeneric: TButton;
    btnSaveGraphic: TButton;
    btnClearGraphic: TButton;
    dbMemo: TDBMemo;
    btnLoadMemo: TButton;
    btnSaveMemo: TButton;
    btnClearMemo: TButton;
    laRangeEndDesc: TLabel;
    btnEditRange: TButton;
    laRangeStart: TLabel;
    laRangeEnd: TLabel;
    cbWordwrap: TCheckBox;
    mnuTableCopyToTable: TMenuItem;
    N5: TMenuItem;
    mnuTableDeleteRecords: TMenuItem;
    cboFilter: TComboBox;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    DBImage: TDBImage;
    Transaction1: TMenuItem;
    Start1: TMenuItem;
    Commint1: TMenuItem;
    RollBack1: TMenuItem;
    Fontgrid1: TMenuItem;
    FontDialog1: TFontDialog;
    AutoTrans: TMenuItem;
    SetDeleteTimeout1: TMenuItem;
    SetFieldNull1: TMenuItem;
    meByteArray: TDBEdit;
    CheckBox1: TCheckBox;
    UndeleteAll1: TMenuItem;
    Undelete1: TMenuItem;
    N4: TMenuItem;
    Panel1: TPanel;
    navTableBrowser: TDBNavigator;
    Panel2: TPanel;
    spdundel: TSpeedButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    grdTableBrowser: TDBGrid;
    pan1: TPanel;
    Label1: TLabel;
    Getautoinc1: TMenuItem;
    MarkasBadRecord1: TMenuItem;
    UnMarkasBadRecord1: TMenuItem;
    N6: TMenuItem;
    SetONOFFcolorflag1: TMenuItem;
    ReOpen1: TMenuItem;
    CheckBox4: TCheckBox;
    SupportRecNo1: TMenuItem;
    SetCheckTimeout1: TMenuItem;
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure cboIndexChange(Sender: TObject);
    Procedure btnFindClick(Sender: TObject);
    Procedure mnuTableCloseClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure mnuViewRefreshClick(Sender: TObject);
    Procedure mnuViewShowFilterClick(Sender: TObject);
    Procedure btnFilterClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure btnFindNearClick(Sender: TObject);
    Procedure btnSetFilterClick(Sender: TObject);
    Procedure edtFindEnter(Sender: TObject);
    Procedure cboFilterEnter(Sender: TObject);
    Procedure mnuViewShowRecordCountClick(Sender: TObject);
    Procedure mnuTableResetColClick(Sender: TObject);
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
    Procedure btnSetClearRangeClick(Sender: TObject);
    Procedure mnuTableDesignReportClick(Sender: TObject);
    Procedure tsMemoTemplateResize(Sender: TObject);
    Procedure tsGraphicTemplateResize(Sender: TObject);
    Procedure tsGenericBlobTemplateResize(Sender: TObject);
    Procedure tsByteArrayTemplateResize(Sender: TObject);
    Procedure btnEditRangeClick(Sender: TObject);
    Procedure mnuViewShowRangeClick(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure meByteArrayChange(Sender: TObject);
    Procedure cbWordwrapClick(Sender: TObject);
    Procedure mnuTableCopyToTableClick(Sender: TObject);
    Procedure mnuTableDeleteRecordsClick(Sender: TObject);
    Procedure Start1Click(Sender: TObject);
    Procedure Commint1Click(Sender: TObject);
    Procedure RollBack1Click(Sender: TObject);
    Procedure Fontgrid1Click(Sender: TObject);
    Procedure AutoTransClick(Sender: TObject);
    Procedure SetDeleteTimeout1Click(Sender: TObject);
    Procedure dsTableBrowserUpdateData(Sender: TObject);
    Procedure dsTableBrowserDataChange(Sender: TObject; Field: TField);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure spdundelClick(Sender: TObject);
    Procedure navTableBrowserBeforeAction(Sender: TObject;
      Button: TNavigateBtn);
    Procedure UndeleteAll1Click(Sender: TObject);
    Procedure grdTableBrowserDrawColumnCell(Sender: TObject;
      Const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    Procedure Getautoinc1Click(Sender: TObject);
    Procedure MarkasBadRecord1Click(Sender: TObject);
    Procedure UnMarkasBadRecord1Click(Sender: TObject);
    Procedure SetONOFFcolorflag1Click(Sender: TObject);
    Procedure ReOpen1Click(Sender: TObject);
    Procedure CheckBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure CheckBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure CheckBox3KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure CheckBox2KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure SupportRecNo1Click(Sender: TObject);
    Procedure SetFieldNull1Click(Sender: TObject);
    procedure SetCheckTimeout1Click(Sender: TObject); {!!.07}
  Private
    Procedure FTableAfterPost(DataSet: TDataSet); {!!.07}
    Procedure FTableAfterScroll(DataSet: TDataSet);
    Procedure FTableAfterCancel(DataSet: TDataSet);
    Procedure FTableBeforeEdit(DataSet: TDataSet);
    Procedure FTableBeforeInsert(DataSet: TDataSet);
    Procedure ViewActiveBlobField;
    Procedure SetRange;
  Protected
    tableversion: Longint;
    FClient: TFSClient;
    FDatabaseName: TffName;
    FEngine: TFSRemoteServer;
    FLog: TFSBaseLog;
    FProtocol: TfsProtocolType;
    FReadOnly: boolean;
    FServerName: TffNetAddress;
    FSession: TFSSession;
    FTable: TFSTable;
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

    Procedure SavePreferences;
    Procedure LoadPreferences;
    Procedure WMGetMinMaxInfo(Var Message: TWMGetMinMaxInfo);
      Message WM_GETMINMAXINFO;
    Function HasBlobOrByteArrayField: Boolean; {!!.07}
    Procedure GenerateRangeDisplayStrings; {!!.07}
  Protected { access methods }
    Procedure SetReadOnly(Const Value: Boolean);
  Public
    Procedure CloseDuringShow(Var Message: TMessage); Message ffm_Close;
    Procedure UpdateDisplay; {!!.01}
    Procedure UpdateDefaultTimeout; {!!.11}
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
  dlgTable: TdlgTable;

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

Procedure TdlgTable.FormCreate(Sender: TObject);
Begin

  FClient := Nil;
  FDatabaseName := '';
  FEngine := Nil;
  FLog := Nil;
  FProtocol := ptRegistry;
  FReadOnly := False;
  FServerName := '';
  FSession := Nil;
  FTable := Nil;
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

Procedure TdlgTable.SetReadOnly(Const Value: Boolean);
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
  bm := FTable.GetBookmark;
  FieldsTags := TList.Create;
  Try
    { save blob-support pointers }
    For i := 0 To Pred(FTable.FieldCount) Do
      FieldsTags.Add(Pointer(FTable.Fields[i].Tag));
    FTable.Close;
    FTable.ReadOnly := ReadOnly;
    FTable.Open;
    For i := 0 To Pred(FTable.FieldCount) Do
      FTable.Fields[i].Tag := Integer(FieldsTags[i]);
    FTable.GotoBookmark(bm);
  Finally
    FTable.FreeBookmark(bm);
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

Procedure TdlgTable.FormShow(Sender: TObject);
Var
  aServerName: String;
  aAddress: String;
  I: Integer;
  OldPass, OldUser: String;
  RecLenPlusTrailer,
    RecsPerBlock: Longint;

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
    NewSheet.Caption := FTable.Fields[FieldIndex].FieldName;
    {$IFDEF DCC4OrLater}
    NewSheet.OnResize := OnResizeProc;
    {$ENDIF}

    For Idx := 0 To SheetToCopy.ControlCount - 1 Do
      Begin
        NewComponent := CopyComponent(SheetToCopy.Controls[Idx]);
        TControl(NewComponent).Parent := NewSheet;
        If IsPublishedProp(NewComponent, 'DataField') Then
          SetStrProp(NewComponent, 'DataField', FTable.Fields[FieldIndex].FieldName);
        If (IsPublishedProp(NewComponent, 'OnClick')) Then
          SetMethodProp(NewComponent, 'OnClick', GetMethodProp(SheetToCopy.Controls[Idx], 'OnClick'));
        If (IsPublishedProp(NewComponent, 'OnKeyPress')) Then
          SetMethodProp(NewComponent, 'OnKeyPress', GetMethodProp(SheetToCopy.Controls[Idx], 'OnKeyPress'));
        If (IsPublishedProp(NewComponent, 'OnChange')) Then
          SetMethodProp(NewComponent, 'OnChange', GetMethodProp(SheetToCopy.Controls[Idx], 'OnChange'));
        //      if NewComponent. IS TCheckBox
          //      SetStrProp(NewComponent, 'OnClick', FTable.Fields.Fields[FieldIndex].FieldName);
              { save pointer to the control displaying the field }
        If (NewComponent Is TDbImage) Or { graphictemplate }
        (NewComponent Is TdbEdit) Or { bytearraytemplate }
        (NewComponent Is TMemo) Or { generictemplate }
        (NewComponent Is TdbMemo) Then { memotemplate }
          FTable.Fields[FieldIndex].Tag := Integer(NewComponent);

      End;
  End;
  {End !!.07}

Begin
  dtShown := False;
  tableversion := 0;
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

    FTable := TFSTable.Create(Nil);
    FTable.SessionName := FSession.SessionName;
    FTable.DataBaseName := FDatabaseName;
    FTable.DataBase.RecLocking := FTableItem.Database.Database.RecLocking;
    FTable.TableName := FTableName;
    FTable.AfterPost := FTableAfterPost; {!!.07}
    FTable.AfterDelete := FTableAfterPost; {!!.07}
    FTable.AfterScroll := FTableAfterScroll; {!!.07}
    FTable.AfterCancel := FTableAfterCancel; {!!.07}
    FTable.BeforeEdit := FTableBeforeEdit;
    FTable.BeforeInsert := FTableBeforeInsert;
    FTable.ReadOnly := ReadOnly; {!!.11}
    fTable.RecLockedBeforeEdit := False;
    FTable.Open;
    tableversion := FTable.tableversion;

    { Set up the indexes }
    cboIndex.Items.Clear;
    With FTable.IndexDefs Do
      Begin
        Clear;
        Update;
        For I := 0 To Count - 1 Do
          cboIndex.Items.Add(Items[I].Name);
      End;

    cboIndex.ItemIndex := 0;
    FTable.IndexName := cboIndex.Items[cboIndex.ItemIndex];

    { Update the find controls }
    cboIndexChange(Nil);

    FsSeparateAddress(FTransport.ServerName, aServerName, aAddress);

    RecLenPlusTrailer := 0;
    RecsPerBlock := 0;

    If FTable.Dictionary.EngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob] Then
      RecLenPlusTrailer := FTable.Dictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) // extra info for delete
    Else If FTable.Dictionary.EngineDeleteType In [edtUndeleteFull] Then
      RecLenPlusTrailer := FTable.Dictionary.RecordLength + SizeOf(Byte) + sizeof(TffInt64) + sizeof(TffInt64) // extra info for delete
    Else
      RecLenPlusTrailer := FTable.Dictionary.RecordLength + SizeOf(Byte);
    If FTable.Dictionary.VersionRecord = trUseVersion Then
      RecLenPlusTrailer := RecLenPlusTrailer + Sizeof(Longword); // for recversion
    RecsPerBlock := (FTable.Dictionary.BlockSize - fsc_BlockHeaderSizeData) Div RecLenPlusTrailer;
    If RecsPerBlock = 0 Then RecsPerBlock := 1;

    Self.Caption := format('%s : %s : %s', [aServerName, FDatabaseName, FTableName]);

    label1.Caption :=
      '  Ver: ' + floatToStrf(tableversion / 1000, ffFixed, 18, 3) +
      ' [RealRecSize = ExtraInfo + RecSize]: ' +
      '[' + IntToStr(RecLenPlusTrailer) + ' = ' + IntToStr(RecLenPlusTrailer - FTable.Dictionary.RecordLength) + ' + ' +
      IntToStr(FTable.Dictionary.RecordLength) + ']' +
      ' BlockSize: ' + IntToStr(FTable.Dictionary.BlockSize) +
      ' RecPerBlock: ' + IntToStr(RecsPerBlock) +
      ' HeaderSizeData: ' + IntToStr(fsc_BlockHeaderSizeData) +
      ' Not used: ' + IntToStr(FTable.Dictionary.BlockSize - ((RecLenPlusTrailer * RecsPerBlock) + fsc_BlockHeaderSizeData)) +
      ' Recovery: ' + StrRecoveryEngine[FTable.Dictionary.EngineDeleteType];

    dsTableBrowser.DataSet := FTable;

    { check if there are any BLOB fields in the table
      and populate the pagecontrol with appropriate controls if so }

    { make the templates invisible }
    For I := 0 To pcBlobFields.PageCount - 1 Do
      pcBlobFields.Pages[I].TabVisible := False;

    { generate new tabsheets for blobfields }
    For I := 0 To FTable.Dictionary.FieldCount - 1 Do
      Begin
        Case FTable.Dictionary.FieldType[I] Of
          fstBLOBMemo: CreateNewBlobTabSheet(tsMemoTemplate, tsMemoTemplateResize, I);
          //fstBLOBGraphic: CreateNewBlobTabSheet( tsGraphicTemplate, tsGraphicTemplateResize, I );
          fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: CreateNewBlobTabSheet(tsByteArrayTemplate, tsByteArrayTemplateResize, I);
          fstBLOB, fstBLOBGraphic: CreateNewBlobTabSheet(tsGenericBlobTemplate, tsGenericBlobTemplateResize, I);
        End;
      End;

    {End !!.07}

    LoadPreferences;

    BeforeInitDone := False;
    UpdateDisplay;

    ViewActiveBlobField; {!!.07}

    { make sure no column exceeds screen width }{!!.07}
    For I := 0 To grdTableBrowser.Columns.Count - 1 Do
      Begin
        If grdTableBrowser.Columns[i].Width > (Width Div 5) * 4 Then
          grdTableBrowser.Columns[i].Width := (Width Div 5) * 4;
      End;

    dtShown := True;
    { update newly created dynamic components }
    ReadOnly := FReadOnly; {!!.11}

    { large font support... }
    If (Screen.PixelsPerInch / PixelsPerInch) > 1.001 Then
      Begin
        Height := Round(Height * (Screen.PixelsPerInch / PixelsPerInch));
        Width := Round(Width * (Screen.PixelsPerInch / PixelsPerInch));
        barStatus.Height := Round(barStatus.Height * (Screen.PixelsPerInch / PixelsPerInch));
      End;
    spdundel.enabled := FTable.Dictionary.EngineDeleteType <> edtNotUndelete;
    CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
    CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
    UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
    MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  Except
    On E: Exception Do
      Begin
        ShowMessage(E.message);
        PostMessage(Handle, ffm_Close, 0, Longint(Sender));
      End;
  End;
End;
{--------}

Procedure TdlgTable.cboIndexChange(Sender: TObject);
Var
  BaseSection: String;
  Index: Integer;
Begin
  BaseSection := ClassName + '.' + Self.Caption;
  With FTable Do
    If IndexName <> cboIndex.Items[cboIndex.ItemIndex] Then
      Begin
        IndexName := cboIndex.Items[cboIndex.ItemIndex];
      End;
  lblFind.Visible := cboIndex.ItemIndex > 0;
  edtFind.Visible := cboIndex.ItemIndex > 0;
  btnFindNear.Visible := cboIndex.ItemIndex > 0;
  btnSetClearRange.Enabled := cboIndex.ItemIndex > 0;
  btnEditRange.Enabled := cboIndex.ItemIndex > 0;
  { clear range - btnSetClearRangeClick flips InRange }
  InRange := True;
  btnSetClearRangeClick(Self);
  For Index := Low(FRangeValues.Field) To FTable.IndexFieldCount Do
    Begin
      FRangeValues.Field[Index].StartNull := FFEConfigGetBoolean(BaseSection, FTable.IndexName + '_RangeStartNull' + IntToStr(Index), True);
      FRangeValues.Field[Index].EndNull := FFEConfigGetBoolean(BaseSection, FTable.IndexName + '_RangeEndNull' + IntToStr(Index), True);
      FRangeValues.Field[Index].StartValue := FFEConfigGetString(BaseSection, FTable.IndexName + '_RangeStartValue' + IntToStr(Index), '');
      FRangeValues.Field[Index].EndValue := FFEConfigGetString(BaseSection, FTable.IndexName + '_RangeEndValue' + IntToStr(Index), '');
      ;
    End;
  FRangeValues.RangeStartKeyExclusive := FFEConfigGetBoolean(BaseSection, FTable.IndexName + '_RangeStartKeyExclusive', False);
  FRangeValues.RangeEndKeyExclusive := FFEConfigGetBoolean(BaseSection, FTable.IndexName + '_RangeEndKeyExclusive', False);
  GenerateRangeDisplayStrings;
End;
{--------}

Procedure TdlgTable.btnFindClick(Sender: TObject);
Begin
  Try
    FTable.FindNearest([edtFind.Text]);
  Except
    On E: EfsDatabaseError Do
      Begin
        If E.ErrorCode = 8706 Then
          ShowMessage(format('%s not found.', [edtFind.Text]))
        Else
          ShowMessage(E.Message);
      End;
  End;
End;
{--------}

Procedure TdlgTable.mnuTableCloseClick(Sender: TObject);
Begin
  Close;
End;
{--------}

Procedure TdlgTable.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  If dtShown Then
    SavePreferences;
  Action := caFree;
End;
{--------}

Procedure TdlgTable.mnuViewRefreshClick(Sender: TObject);
Begin
  FTable.Refresh;
  UpdateDisplay;
End;
{--------}

Procedure TdlgTable.UpdateDisplay;
Begin
  If BeforeInitDone Then
    Exit;
  If mnuViewShowRecordCount.Checked Then
    barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable.RecordCount, ThousandSeparator)
  Else
    barStatus.Panels[0].Text := '';

  If FTable.Filtered Then
    barStatus.Panels[1].Text := 'Filter: <ACTIVE>'
  Else
    barStatus.Panels[1].Text := 'Filter: <Inactive>';

  If InRange Then
    Begin
      barStatus.Panels[2].Text := 'Range: <ACTIVE>';
      laRangeStart.Font.Style := [fsBold];
      laRangeEnd.Font.Style := [fsBold];
      laRangeStartDesc.Font.Style := [fsBold];
      laRangeEndDesc.Font.Style := [fsBold];
    End
  Else
    Begin
      barStatus.Panels[2].Text := 'Range: <Inactive>';
      laRangeStart.Font.Style := [];
      laRangeEnd.Font.Style := [];
      laRangeStartDesc.Font.Style := [];
      laRangeEndDesc.Font.Style := [];
    End;

  With navTableBrowser Do
    Begin
      VisibleButtons := [nbFirst, nbLast, nbPrior, nbNext, nbRefresh];
      If (Not FTable.ReadOnly) And (Not FReadOnly) Then
        VisibleButtons := VisibleButtons + [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
    End;
End;
{--------}

Procedure TdlgTable.mnuViewShowFilterClick(Sender: TObject);
Begin
  mnuViewShowFilter.Checked := Not mnuViewShowFilter.Checked;
  pnlFilter.Visible := mnuViewShowFilter.Checked;
  { make sure to reset statusbar etc if status changes }
  If FTable.Filtered Then
    btnSetFilterClick(Self);
  //  edtFilter.Text := ''; why remove? makes sense to keep the text, the user might need it again!
End;
{--------}

Procedure TdlgTable.btnFilterClick(Sender: TObject);
Begin
  If FTable.Filtered Then
    Begin
      FTable.Filtered := False;
      btnSetFilter.Caption := 'S&et Filter'; {!!.03}
    End
  Else
    Begin
      FTable.Filter := cboFilter.Text;
      FTable.Filtered := True;
      btnSetFilter.Caption := 'Cl&ear Filter'; {!!.03}
    End;
End;
{--------}

Procedure TdlgTable.FormDestroy(Sender: TObject);
Begin
  Try
    Try
      FTable.Close;
    Finally
      FTable.Free;
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
    Self := Nil;
  Except
    Self := Nil;
  End;
End;
{--------}

Procedure TdlgTable.CloseDuringShow(Var Message: TMessage);
Begin
  Close;
End;
{--------}

Procedure TdlgTable.WMGetMinMaxInfo(Var Message: TWMGetMinMaxInfo);
Var
  MinMax: PMinMaxInfo;
Begin
  Inherited;
  MinMax := Message.MinMaxInfo;
  MinMax^.ptMinTrackSize.x := 590;
End;
{--------}

Procedure TdlgTable.btnFindNearClick(Sender: TObject);
Begin
  Try
    FTable.FindNearest([edtFind.Text]);
  Except
    On E: EfsDatabaseError Do
      Begin
        If E.ErrorCode = 8706 Then
          ShowMessage(format('%s not found.', [edtFind.Text]))
        Else
          ShowMessage(E.Message);
      End;
  End;
End;
{--------}

Procedure TdlgTable.btnSetFilterClick(Sender: TObject);
{Begin !!.05}
Var
  SavCursor: TCursor;
Begin
  SavCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Try
    If FTable.Filtered Then
      Begin
        FTable.Filtered := False;
        btnSetFilter.Caption := 'S&et Filter'; {!!.03}
      End
    Else
      Begin
        FTable.Filter := cboFilter.Text;
        FTable.Filtered := True;
        btnSetFilter.Caption := 'Cl&ear Filter'; {!!.03}
        {Begin !!.11}
        { update history list in combobox }
        If FTable.Filter <> '' Then
          Begin
            { does filter exist in the list? }
            If cboFilter.Items.IndexOf(FTable.Filter) >= 0 Then
              { if so remove it; no doubles needed }
              cboFilter.Items.Delete(cboFilter.Items.IndexOf(FTable.Filter));
            { make last filter string top of the history list }
            cboFilter.Items.Insert(0, FTable.Filter);
            cboFilter.ItemIndex := 0;
            { enforce maxcount }
            While cboFilter.Items.Count > MaxFilterComboItems Do
              cboFilter.Items.Delete(MaxFilterComboItems);
          End;
        {End !!.11}
      End;
    UpdateDisplay;
  Finally
    Screen.Cursor := SavCursor;
  End;
  {End !!.05}
End;
{--------}

Procedure TdlgTable.edtFindEnter(Sender: TObject);
Begin
  btnSetFilter.Default := False;
  btnFindNear.Default := True;
End;
{--------}

Procedure TdlgTable.cboFilterEnter(Sender: TObject);
Begin
  btnFindNear.Default := False;
  btnSetFilter.Default := True;
End;
{--------}

Procedure TdlgTable.SavePreferences;
Var
  BaseSection: String;
  i: Integer; {!!.11}
Begin
  Try
    BaseSection := ClassName + '.' + Self.Caption;
    FFEConfigSaveString(BaseSection, 'Last Filter', cboFilter.Text);
    {Begin !!.11}
    For i := 0 To Pred(cboFilter.Items.Count) Do
      FFEConfigSaveString(BaseSection, 'FilterHistory' + IntToStr(i), cboFilter.Items[i]);
    {End !!.11}
    FFEConfigSaveString(BaseSection, 'Last Find Nearest', edtFind.Text);
    FFEConfigSaveInteger(BaseSection, 'Last Index', cboIndex.ItemIndex);
    FFEConfigSaveBoolean(BaseSection, 'Show record count', mnuViewShowRecordCount.Checked);
    FFEConfigSaveFormPrefs(BaseSection, Self);
    FFEConfigSaveDBColumnPrefs(BaseSection + '.ColumnInfo', grdTableBrowser.Columns);
    FFEConfigSaveInteger(BaseSection, 'Table Timeout', FTable.Timeout); {!!.07}
    FFEConfigSaveInteger(BaseSection, 'PageControl size', pcBlobfields.Height); {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show blob fields', mnuViewShowBLOBFields.Checked); {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show range', mnuViewShowRange.Checked); {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show filter', mnuViewShowFilter.Checked); {!!.07}
  Except
    On E: Exception Do
      ShowMessage('Error writing INI file: ' + E.Message);
  End;
End;
{--------}

Procedure TdlgTable.LoadPreferences;
Var
  BaseSection: String;
  Index: Integer;
  s: String; {!!.11}
Begin
  BaseSection := ClassName + '.' + Self.Caption;
  cboFilter.Text := FFEConfigGetString(BaseSection, 'Last Filter', '');
  {Begin !!.11}
  For Index := 0 To Pred(MaxFilterComboItems) Do
    Begin
      s := FFEConfigGetString(BaseSection, 'FilterHistory' + IntToStr(Index), '');
      If s <> '' Then
        cboFilter.Items.Add(s);
    End;
  {End !!.11}
  Index := FFEConfigGetInteger(BaseSection, 'Last Index', 0);
  If (Index < cboIndex.Items.Count) Then
    Begin
      cboIndex.ItemIndex := Index;
      FTable.IndexName := cboIndex.Items[cboIndex.ItemIndex];
      { Update the find controls }
      cboIndexChange(Nil);
    End;
  edtFind.Text := FFEConfigGetString(BaseSection, 'Last Find Nearest', '');
  mnuViewShowRecordCount.Checked := FFEConfigGetBoolean(BaseSection, 'Show record count', True);
  FFEConfigGetFormPrefs(BaseSection, Self);
  FFEConfigGetDBColumnPrefs(BaseSection + '.ColumnInfo', grdTableBrowser.Columns);
  FTable.Timeout := FFEConfigGetInteger(BaseSection, 'Table Timeout', -1); {!!.07}
  pcBlobfields.Height := FFEConfigGetInteger(BaseSection, 'PageControl size', pcBlobfields.Height); {!!.07}
  mnuViewShowBLOBFields.Checked := HasBlobOrByteArrayField And FFEConfigGetBoolean(BaseSection, 'Show blob fields', True); {!!.07}
  If Not HasBlobOrByteArrayField Then
    mnuViewShowBLOBFields.Enabled := False;
  pcBlobfields.Visible := mnuViewShowBLOBFields.Checked And HasBlobOrByteArrayField;
  splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked And HasBlobOrByteArrayField;
  mnuViewShowRange.Checked := FFEConfigGetBoolean(BaseSection, 'Show range', True); {!!.07}
  pnlRange.Visible := mnuViewShowRange.Checked;
  mnuViewShowFilter.Checked := FFEConfigGetBoolean(BaseSection, 'Show filter', True); {!!.07}
  pnlFilter.Visible := mnuViewShowFilter.Checked;
End;
{--------}

Procedure TdlgTable.mnuViewShowRecordCountClick(Sender: TObject);
Begin
  mnuViewShowRecordCount.Checked := Not mnuViewShowRecordCount.Checked;
  UpdateDisplay;
End;

Procedure TdlgTable.mnuTableResetColClick(Sender: TObject);
Begin
  grdTableBrowser.Columns.RebuildColumns;
End;

Procedure TdlgTable.grdTableBrowserKeyDown(Sender: TObject; Var Key: Word;
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
    (Not FTable.IsEmpty)) Then
    Begin
      DoPost := Not (FTable.State In [dsInsert, dsEdit]);
      If DoPost Then
        FTable.Edit;
      grdTableBrowser.SelectedField.Clear;
      If DoPost Then
        FTable.Post;
      { refresh; could be blobfield }
      ViewActiveBlobField;
    End;
  {End !!.11}
End;
{Begin !!.02}
{--------}

Procedure TdlgTable.mnuOptionsDebugClick(Sender: TObject);
Begin
  mnuOptionsDebug.Checked := Not mnuOptionsDebug.Checked;
  If mnuOptionsDebug.Checked Then
    FTransport.EventLogOptions := [fstpLogErrors, fstpLogRequests,
      fstpLogReplies]
  Else
    FTransport.EventLogOptions := [fstpLogErrors];
End;
{End !!.02}

{Begin !!.07}

Procedure TdlgTable.FTableAfterPost(DataSet: TDataSet);
Begin
  If AutoTrans.Checked Then
    If FTable.Database.InTransaction Then
      FTable.Database.Commit;
  UpdateDisplay;
End;
{--------}

Procedure TdlgTable.FTableAfterCancel(DataSet: TDataSet);
Begin
  If AutoTrans.Checked Then
    If FTable.Database.InTransaction Then
      FTable.Database.Rollback;
  FTable.Refresh;
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTable.FTableAfterScroll(DataSet: TDataSet);
Begin
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTable.FTableBeforeEdit(DataSet: TDataSet);
Begin
  If AutoTrans.Checked Then
    If Not FTable.Database.InTransaction Then
      FTable.Database.StartTransaction;
End;
{--------}

Procedure TdlgTable.FTableBeforeInsert(DataSet: TDataSet);
Begin
  If AutoTrans.Checked Then
    If Not FTable.Database.InTransaction Then
      FTable.Database.StartTransaction;
End;
{--------}

Procedure TdlgTable.mnuOptionsTimeoutClick(Sender: TObject);
Var
  sTimeout: String;
  res: Boolean;
Begin
  sTimeout := IntToStr(FTable.Timeout);
  Repeat
    res := InputQuery('Table Timeout (ms)', 'Value:', sTimeout);
    If res Then
      Try
        FTable.Timeout := StrToInt(sTimeout);
        If FTable.Timeout < -1 Then
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

Procedure TdlgTable.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If (Not (FTable.State In [dsInsert, dsEdit])) And
    (Key = VK_ESCAPE) Then
    Close;
End;
{--------}

Procedure TdlgTable.ViewActiveBlobField;
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

      For i := 0 To FTable.Dictionary.FieldCount - 1 Do
        Begin
          { only load blob on active tabsheet }
          If Assigned(Pointer(FTable.Fields[i].Tag)) And
            (FTable.Fields[i].FieldName = pcBlobfields.ActivePage.Caption) Then
            Case FTable.Dictionary.FieldType[i] Of
              fstBLOB, fstBLOBGraphic:
                Begin
                  Try
                    Stream := FTable.CreateBlobStream(FTable.Fields[i], bmRead);
                    Try
                      TMemo(FTable.Fields[i].Tag).Text := '';
                      Stream.Read(BlobBuffer, FFMinL(1024, Stream.Size));
                      TMemo(FTable.Fields[i].Tag).Text :=
                        GenerateHexLines(@BlobBuffer, FFMinL(1024, Stream.Size));
                    Finally
                      Stream.Free;
                    End;

                  Except
                    On E: Exception Do
                      Begin
                        ShowMessage('Exception: ' + E.Message + ' when displaying blob field: ' + FTable.Fields[i].FieldName);
                      End;
                  End;
                End;
            End;
        End;
    End;

End;

Function TdlgTable.HasBlobOrByteArrayField: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  For i := 0 To FTable.Dictionary.FieldCount - 1 Do
    If FTable.Dictionary.FieldType[i] In [fstBLOB..ffcLastBLOBType, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble] Then
      Begin
        Result := True;
        Exit;
      End;
End;
{--------}

Procedure TdlgTable.cbStretchClick(Sender: TObject);
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

Procedure TdlgTable.btnClearBAClick(Sender: TObject);
Var
  fieldIdx,
    controlIdx: Integer;
  aEdit: TdbEdit;
  aField: TField;
Begin
  { find edit control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TdbEdit Then
      Begin
        aEdit := TdbEdit(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = aEdit Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If Not (FTable.State In [dsInsert, dsEdit]) Then
                  FTable.Edit;
                aField.Clear;
                aEdit.Text := '';
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTable.pcBlobfieldsChange(Sender: TObject);
Begin
  ViewActiveBlobField;
End;
{--------}

Procedure TdlgTable.mnuViewShowBLOBFieldsClick(Sender: TObject);
Begin
  mnuViewShowBLOBFields.Checked := Not mnuViewShowBLOBFields.Checked;
  pcBlobfields.Visible := mnuViewShowBLOBFields.Checked;
  splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked;
  If mnuViewShowBLOBFields.Checked Then
    ViewActiveBlobField;
End;
{--------}

Procedure TdlgTable.btnLoadMemoClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = adbMemo Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  If Not (FTable.State In [dsInsert, dsEdit]) Then
                    FTable.Edit;
                  TMemoField(aField).LoadFromFile(opendialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnSaveMemoClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = adbMemo Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  TMemoField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnLoadGenericClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = aMemo Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  If Not (FTable.State In [dsInsert, dsEdit]) Then
                    FTable.Edit;
                  TBlobField(aField).LoadFromFile(opendialog.FileName);
                  ViewActiveBlobField;
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnSaveGenericClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = aMemo Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  TBlobField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnClearMemoClick(Sender: TObject);
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
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = adbMemo Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If Not (FTable.State In [dsInsert, dsEdit]) Then
                  FTable.Edit;
                aField.Clear;
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTable.btnLoadGraphicClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = aImage Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  If Not (FTable.State In [dsInsert, dsEdit]) Then
                    FTable.Edit;
                  TFSGraphicField(aField).LoadFromFile(opendialog.FileName);
                  // ViewActiveBlobField;
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnSaveGraphicClick(Sender: TObject);
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
          For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
            Begin
              If Pointer(FTable.Fields[fieldIdx].Tag) = aImage Then
                Begin
                  aField := FTable.Fields[fieldIdx];
                  TBlobField(aField).SaveToFile(savedialog.FileName);
                  Exit;
                End;
            End;
        End;
End;
{--------}

Procedure TdlgTable.btnClearGraphicClick(Sender: TObject);
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
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = aImage Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If Not (FTable.State In [dsInsert, dsEdit]) Then
                  FTable.Edit;
                TGraphicField(aField).Clear;
                //ViewActiveBlobField;
                Exit;
              End;
          End;
      End;
End;
{--------}

Procedure TdlgTable.btnClearGenericClick(Sender: TObject);
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
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = aMemo Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If Not (FTable.State In [dsInsert, dsEdit]) Then
                  FTable.Edit;
                aField.Clear;
                ViewActiveBlobField;
                Exit;
              End;
          End;
      End;
End;

Procedure TdlgTable.meByteArrayKeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (Key In ['0'..'9', ',', '.', '-']) Then
    Key := #0
  Else
    BAKeyPressDetected := True;
End;

Procedure TdlgTable.mnuTableDesignReportClick(Sender: TObject);
Begin
End;

{ magic resize numbers: 100 = width of buttons + 8 pixels of space on each side }

Procedure TdlgTable.tsMemoTemplateResize(Sender: TObject);
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

Procedure TdlgTable.tsGraphicTemplateResize(Sender: TObject);
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

Procedure TdlgTable.tsGenericBlobTemplateResize(Sender: TObject);
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

Procedure TdlgTable.tsByteArrayTemplateResize(Sender: TObject);
Var
  controlIdx: Integer;
Begin
  For controlIdx := 0 To TTabSheet(Sender).ControlCount - 1 Do
    If TTabSheet(Sender).Controls[controlIdx] Is TdbEdit Then
      Begin
        TdbEdit(TTabSheet(Sender).Controls[controlIdx]).Width := TTabSheet(Sender).Width - 2 * TdbEdit(TTabSheet(Sender
          ).Controls[controlIdx]).Left;
      End
    Else If TTabSheet(Sender).Controls[controlIdx] Is TButton Then
      Begin
        If TButton(TTabSheet(Sender).Controls[controlIdx]).Caption = 'Clear' Then
          TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width - 83;
      End;
End;

Procedure TdlgTable.btnSetClearRangeClick(Sender: TObject);
Var
  NeedEdit: Boolean;
  FieldIdx: Integer;
Begin
  If Not InRange Then
    Begin
      { check wether we have a useable range (not all NULL) }
      NeedEdit := True;
      For FieldIdx := Low(FRangeValues.Field) To FTable.IndexFieldCount Do
        If (Not FRangeValues.Field[FieldIdx].StartNull) Or
          (Not FRangeValues.Field[FieldIdx].EndNull) Then
          Begin
            NeedEdit := False;
            Break;
          End;
      If NeedEdit Then
        btnEditRangeClick(Self)
      Else
        SetRange;
    End
  Else
    Begin
      btnSetClearRange.Caption := 'Set &Range';
      FTable.CancelRange;
      InRange := False;
      UpdateDisplay;
    End;
End;

Procedure TdlgTable.btnEditRangeClick(Sender: TObject);
Var
  BaseSection: String;
  Index: Integer;
Begin
  If SetRangeDlg(FTable, FRangeValues) = mrOK Then
    Begin
      SetRange;
      GenerateRangeDisplayStrings;
      BaseSection := ClassName + '.' + Self.Caption;
      For Index := Low(FRangeValues.Field) To FTable.IndexFieldCount Do
        Begin
          FFEConfigSaveBoolean(BaseSection, FTable.IndexName + '_RangeStartNull' + IntToStr(Index), FRangeValues.Field[Index].StartNull);
          FFEConfigSaveBoolean(BaseSection, FTable.IndexName + '_RangeEndNull' + IntToStr(Index), FRangeValues.Field[Index].EndNull);
          FFEConfigSaveString(BaseSection, FTable.IndexName + '_RangeStartValue' + IntToStr(Index), FRangeValues.Field[Index].StartValue);
          FFEConfigSaveString(BaseSection, FTable.IndexName + '_RangeEndValue' + IntToStr(Index), FRangeValues.Field[Index].EndValue);
        End;
      FFEConfigSaveBoolean(BaseSection, FTable.IndexName + '_RangeStartKeyExclusive', FRangeValues.RangeStartKeyExclusive);
      FFEConfigSaveBoolean(BaseSection, FTable.IndexName + '_RangeEndKeyExclusive', FRangeValues.RangeEndKeyExclusive);
    End;
End;

Procedure TdlgTable.mnuViewShowRangeClick(Sender: TObject);
Var
  FilterFix: Boolean;
Begin
  { necessary to get rangepanel to reappear below filterpanel }
  FilterFix := pnlFilter.Visible And Not pnlRange.Visible;
  If FilterFix Then
    pnlFilter.Visible := False;
  mnuViewShowRange.Checked := Not mnuViewShowRange.Checked;
  pnlRange.Visible := mnuViewShowRange.Checked;
  { remove range and update display etc }
  If InRange Then
    btnSetClearRangeClick(Self);
  If FilterFix Then
    pnlFilter.Visible := True;
End;

Procedure TdlgTable.FormResize(Sender: TObject);
Begin
  btnFindNear.Left := ClientWidth - btnFindNear.Width - 8;
  edtFind.Width := btnFindNear.Left - edtFind.Left - 8;
  btnSetFilter.Left := ClientWidth - btnSetFilter.Width - 8;
  cboFilter.Width := btnSetFilter.Left - cboFilter.Left - 8;
  btnSetClearRange.Left := ClientWidth - btnSetClearRange.Width - 8;
  laRangeStart.Width := btnSetClearRange.Left - laRangeStart.Left - 8;
  btnEditRange.Left := ClientWidth - btnEditRange.Width - 8;
  laRangeEnd.Width := btnEditRange.Left - laRangeEnd.Left - 8;
End;

Procedure TdlgTable.GenerateRangeDisplayStrings;
Var
  HighestNonNullIdx,
    FieldIdx: Integer;
  FirstField: Boolean;
Begin
  HighestNonNullIdx := FTable.IndexFieldCount;
  While (HighestNonNullIdx > 1) And
    FRangeValues.Field[HighestNonNullIdx].StartNull And
    FRangeValues.Field[HighestNonNullIdx].EndNull Do
    Dec(HighestNonNullIdx);
  laRangeStart.Caption := '[';
  FirstField := True;
  For FieldIdx := Low(FRangeValues.Field) To HighestNonNullIdx Do
    Begin
      If Not FirstField Then
        laRangeStart.Caption := laRangeStart.Caption + ', ';
      If FRangeValues.Field[FieldIdx].StartNull Then
        laRangeStart.Caption := laRangeStart.Caption + 'NULL'
      Else If FRangeValues.Field[FieldIdx].StartValue <> '' Then
        laRangeStart.Caption := laRangeStart.Caption + FRangeValues.Field[FieldIdx].StartValue
      Else
        laRangeStart.Caption := laRangeStart.Caption + '''''';
      FirstField := False;
    End;
  laRangeStart.Caption := laRangeStart.Caption + ']';
  If FRangeValues.RangeStartKeyExclusive Then
    laRangeStart.Caption := laRangeStart.Caption + ' - [KeyExclusive]';
  laRangeEnd.Caption := '[';
  FirstField := True;
  For FieldIdx := Low(FRangeValues.Field) To HighestNonNullIdx Do
    Begin
      If Not FirstField Then
        laRangeEnd.Caption := laRangeEnd.Caption + ', ';
      If FRangeValues.Field[FieldIdx].EndNull Then
        laRangeEnd.Caption := laRangeEnd.Caption + 'NULL'
      Else If FRangeValues.Field[FieldIdx].EndValue <> '' Then
        laRangeEnd.Caption := laRangeEnd.Caption + FRangeValues.Field[FieldIdx].EndValue
      Else
        laRangeEnd.Caption := laRangeEnd.Caption + '''''';
      FirstField := False;
    End;
  laRangeEnd.Caption := laRangeEnd.Caption + ']';
  If FRangeValues.RangeEndKeyExclusive Then
    laRangeEnd.Caption := laRangeEnd.Caption + ' - [KeyExclusive]';
End;

Procedure TdlgTable.SetRange;
Var
  HighestNonNullIdx,
    FieldIdx: Integer;
Begin
  HighestNonNullIdx := 0;
  FTable.SetRangeStart;
  FTable.KeyExclusive := FRangeValues.RangeStartKeyExclusive;
  For FieldIdx := Low(FRangeValues.Field) To FTable.IndexFieldCount Do
    Begin
      If Not FRangeValues.Field[FieldIdx].StartNull Then
        Begin
          FTable.IndexFields[FieldIdx - 1].AsString := FRangeValues.Field[FieldIdx].StartValue;
          HighestNonNullIdx := FFMaxL(HighestNonNullIdx, FieldIdx);
        End
      Else If Not FRangeValues.Field[FieldIdx].EndNull Then
        FTable.IndexFields[FieldIdx - 1].Value := NULL;
    End;
  FTable.SetRangeEnd;
  FTable.KeyExclusive := FRangeValues.RangeEndKeyExclusive;
  For FieldIdx := Low(FRangeValues.Field) To FTable.IndexFieldCount Do
    Begin
      If Not FRangeValues.Field[FieldIdx].EndNull Then
        Begin
          FTable.IndexFields[FieldIdx - 1].AsString := FRangeValues.Field[FieldIdx].EndValue;
          HighestNonNullIdx := FFMaxL(HighestNonNullIdx, FieldIdx);
        End
      Else If Not FRangeValues.Field[FieldIdx].StartNull Then
        FTable.IndexFields[FieldIdx - 1].Value := NULL;
    End;
  FTable.KeyFieldCount := HighestNonNullIdx;
  FTable.ApplyRange;
  InRange := True;
  btnSetClearRange.Caption := 'Clear &Range';
  UpdateDisplay;
End;

Procedure TdlgTable.meByteArrayChange(Sender: TObject);
Var
  ByteArrayBuffer: Pointer;
  fieldIdx,
    controlIdx: Integer;
  aEdit: TdbEdit;
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
  FTable.Edit;
  { find edit control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TEdit Then
      Begin
        aEdit := TdbEdit(TButton(Sender).Parent.Controls[controlIdx]);
        If aEdit.Text = '' Then
          Exit;
        { find correct field }
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = aEdit Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If Not (FTable.State In [dsInsert, dsEdit]) Then
                  FTable.Edit;
                TfsArrayField(aField).AsString := aEdit.Text;
                Exit;
              End;
          End;
      End;
End;

Procedure TdlgTable.cbWordwrapClick(Sender: TObject);
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

Procedure TdlgTable.mnuTableCopyToTableClick(Sender: TObject);
Var
  ExcludeIndex,
    TableIndex, CommitPerTr: Longint;
  CopyBlobs: Boolean;
  SaveTimeout: Integer;
Begin
  ExcludeIndex := TableItem.Database.IndexOf(TableItem);
  CommitPerTr := 0;
  If ShowCopyTableDlg(TableItem.Database, ExcludeIndex, FTable,
    TableIndex, CopyBlobs, FTableItem, CommitPerTr) = mrOK Then
    Begin {!!.11}
      With TableItem.Database.Tables[TableIndex] Do
        Begin
          Screen.Cursor := crHourGlass;
          { the copy operation is used in the context of the table
            that's being copied to. Use the timeout of the active
            table, otherwise the user has no way of setting timeout. }
          SaveTimeout := Table.Timeout;
          Table.Timeout := FTable.Timeout;
          Try
            Update;
            CopyRecords(FTable, CopyBlobs, CommitPerTr);
          Finally
            Screen.Cursor := crDefault;
            Table.Timeout := SaveTimeout;
            { force the second table to close if it wasn't open before }
            FSession.CloseInactiveTables; {!!.11}
          End;
        End;
    End;
End;

Procedure TdlgTable.mnuTableDeleteRecordsClick(Sender: TObject);
Var
  NewString: String;
  ClickedOK: Boolean;
  i: Integer;
Begin
  If MessageDlg('Delete all records matching the current filter and range - are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      Screen.Cursor := crHourGlass;
      NewString := '0';
      i := 0;
      ClickedOK := InputQuery('Commit records per transaction', '0 is none', NewString);
      If ClickedOK Then
        i := StrToInt(Trim(NewString));
      If i < 0 Then i := 0;
      Try
        Update;
        FTable.DeleteRecords(i);
        CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
        CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
        UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
        MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);

      Finally
        Screen.Cursor := crDefault;
      End;
    End;
End;
{End !!.07}

Procedure TdlgTable.UpdateDefaultTimeout;
Begin
  FClient.TimeOut := Config.DefaultTimeout; {!!.11}
End;

Procedure TdlgTable.Start1Click(Sender: TObject);
Begin
  If Not FTable.Database.InTransaction Then
    Ftable.Database.StartTransaction;
End;

Procedure TdlgTable.Commint1Click(Sender: TObject);
Begin
  If FTable.Database.InTransaction Then
    Begin
      FTable.Database.Commit;
      UpdateDisplay;
    End;
End;

Procedure TdlgTable.RollBack1Click(Sender: TObject);
Begin
  If FTable.Database.InTransaction Then
    Begin
      FTable.Database.Rollback;
      FTable.Refresh;
      ViewActiveBlobField;
    End;
End;

Procedure TdlgTable.Fontgrid1Click(Sender: TObject);
Begin
  If FontDialog1.Execute Then
    grdTableBrowser.Font := FontDialog1.Font;
  grdTableBrowser.Refresh;
End;

Procedure TdlgTable.AutoTransClick(Sender: TObject);
Begin
  AutoTrans.Checked := Not AutoTrans.Checked;
End;

Procedure TdlgTable.SetDeleteTimeout1Click(Sender: TObject);
Var
  sTimeout: String;
  res: Boolean;
Begin
  sTimeout := IntToStr(FTable.DeleteTimeout);
  Repeat
    res := InputQuery('Table Delete Timeout (ms)', 'Value:', sTimeout);
    If res Then
      Try
        FTable.DeleteTimeout := StrToInt(sTimeout);
        If FTable.DeleteTimeout < -1 Then
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

Procedure TdlgTable.dsTableBrowserUpdateData(Sender: TObject);
Begin
  meByteArrayChange(meByteArray);
  CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
End;

Procedure TdlgTable.dsTableBrowserDataChange(Sender: TObject;
  Field: TField);
Begin
  meByteArrayChange(meByteArray);
  If Not fTable.Active Then Exit;
  barStatus.Panels[3].Text := 'RecNo: ' + IntToStr(ftable.RecNo) + ' RefNr: ' +
    IntToStr(ftable.getrefnr.ilow) + ':' + IntToStr(ftable.getrefnr.ihigh);
  CheckBox2.checked := getflags(fTable.GetFlagRecord, frProtectDeleteRecord);
  CheckBox3.checked := getflags(fTable.GetFlagRecord, frProtectUpdateRecord);
End;

Procedure TdlgTable.CheckBox1Click(Sender: TObject);
Var
  fieldIdx,
    controlIdx, i: Integer;
  aEdit: TdbEdit;
  aField: TField;
Begin
  { find edit control }
  For controlIdx := 0 To TButton(Sender).Parent.ControlCount - 1 Do
    If TButton(Sender).Parent.Controls[controlIdx] Is TdbEdit Then
      Begin
        aEdit := TdbEdit(TButton(Sender).Parent.Controls[controlIdx]);
        { find correct field }
        For fieldIdx := 0 To FTable.Dictionary.FieldCount - 1 Do
          Begin
            If Pointer(FTable.Fields[fieldIdx].Tag) = aEdit Then
              Begin
                aField := FTable.Fields[fieldIdx];
                If aField <> Nil Then
                  If aField Is TfsArrayField Then
                    Begin
                      For i := 0 To TButton(Sender).Parent.ControlCount - 1 Do
                        If TCheckBox(Sender).Parent.Controls[i] Is TCheckBox Then
                          Begin
                            TfsArrayField(aField).ShowArray := TCheckBox(Sender).Checked;
                            Exit;
                          End;
                    End;
              End;
          End;
      End;
End;

Procedure TdlgTable.spdundelClick(Sender: TObject);
Begin
  If FTable.Dictionary.EngineDeleteType = edtNotUndelete Then
    Begin
      ShowMessage('Deleted records were not found!');
      Exit;
    End;

  If Not FTable.Undelete(True) Then ShowMessage('Deleted records were not found!');
  barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable.RecordCount, ThousandSeparator);
  CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);

  ftable.Refresh;
End;

Procedure TdlgTable.navTableBrowserBeforeAction(Sender: TObject;
  Button: TNavigateBtn);
Begin
  If (button = nbrefresh) Then
    Begin
      CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
      CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
      UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
      MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);

      barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable.RecordCount, ThousandSeparator);
    End;
End;

Procedure TdlgTable.UndeleteAll1Click(Sender: TObject);
Var
  aFound: Boolean;
  i, j: Integer;
Begin
  If FTable.Dictionary.EngineDeleteType = edtNotUndelete Then
    Begin
      ShowMessage('Deleted records were not found!');
      Exit;
    End;
  i := 0;
  j := 0;
  aFound := FTable.Undelete(False);
  If Not aFound Then
    ShowMessage('Deleted records were not found!')
  Else
    Begin
      Screen.Cursor := crHourGlass;
      Try
        While aFound Do
          Begin
            Inc(j);
            aFound := FTable.Undelete(False);
            inc(i);
            If i = 50 Then
              Begin
                application.ProcessMessages;
                i := 0;
                barStatus.Panels[0].Text := 'Undeleted records: ' + IntToStr(j);
              End;
          End;
        FTable.Refresh;
      Finally
        Screen.Cursor := crDefault;
        barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable.RecordCount, ThousandSeparator);
      End;
    End;
  CheckBox2.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  CheckBox3.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
  MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (tableversion >= 1059);
End;

Procedure TdlgTable.grdTableBrowserDrawColumnCell(Sender: TObject;
  Const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
Begin
  If Not fTable.Active Then Exit;
  If Not SetONOFFcolorflag1.Checked Then Exit;
  // for flags record
  {frDataRecord = 1;
  frEmptyRecord = 2; // block empty
  frDeleteRecord = 4;
  frUndeletedRecord = 8;
  frProtectDeleteRecord = 16;
  frProtectUpdateRecord = 32; }

  //If getflags(fTable.GetFlagRecord, frDataRecord) Then // normal
    //grdTableBrowser.Canvas.Font.Color := clblack;
  If (getflags(fTable.GetFlagRecord, frUndeletedRecord) And getflags(fTable.GetFlagRecord, frProtectUpdateRecord)) Then
    grdTableBrowser.Canvas.Font.Color := clolive
  Else If getflags(fTable.GetFlagRecord, frUndeletedRecord) Then
    grdTableBrowser.Canvas.Font.Color := clgreen
  Else If (getflags(fTable.GetFlagRecord, frProtectUpdateRecord) And getflags(fTable.GetFlagRecord, frProtectDeleteRecord)) Then
    grdTableBrowser.Canvas.Font.Color := clRed
  Else If getflags(fTable.GetFlagRecord, frProtectDeleteRecord) Then
    grdTableBrowser.Canvas.Font.Color := clYellow
  Else If getflags(fTable.GetFlagRecord, frProtectUpdateRecord) Then
    grdTableBrowser.Canvas.Font.Color := clFuchsia;
  If getflags(fTable.GetFlagRecord, frMarkAsBadRecord) Then
    Begin
      grdTableBrowser.Canvas.Font.Color := clblack;
      grdTableBrowser.Canvas.Brush.Color := clred;
    End;
  grdTableBrowser.DefaultDrawcolumnCell(Rect, DataCol, Column, State);
End;

Procedure TdlgTable.Getautoinc1Click(Sender: TObject);
Begin
  If Not fTable.Active Then Exit;
  ShowMessage(IntToStr(fTable.ReadLastAutoInc));
End;

Procedure TdlgTable.MarkasBadRecord1Click(Sender: TObject);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  FTable.SetFlagRecord(frMarkAsBadRecord, True);
  ftable.Refresh;
End;

Procedure TdlgTable.UnMarkasBadRecord1Click(Sender: TObject);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  FTable.SetFlagRecord(frMarkAsBadRecord, False);
  ftable.Refresh;
End;

Procedure TdlgTable.SetONOFFcolorflag1Click(Sender: TObject);
Begin
  SetONOFFcolorflag1.Checked := Not SetONOFFcolorflag1.Checked;
  fTable.Refresh;
End;

Procedure TdlgTable.ReOpen1Click(Sender: TObject);
Begin
  If fTable.State In [dsinsert, dsedit] Then Exit;
  fTable.Close;
  fTable.Open;
  If Not fTable.Active Then Exit;

  CheckBox2.Enabled := (Not FTable.IsEmpty) And (fTable.tableversion >= 1059);
  CheckBox3.Enabled := (Not FTable.IsEmpty) And (fTable.tableversion >= 1059);
  UnMarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (fTable.tableversion >= 1059);
  MarkasBadRecord1.Enabled := (Not FTable.IsEmpty) And (fTable.tableversion >= 1059);

  barStatus.Panels[0].Text := 'Records: ' + FsCommaizeChL(FTable.RecordCount, ThousandSeparator);
End;

Procedure TdlgTable.CheckBox2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  If CheckBox2.Checked Then
    FTable.SetFlagRecord(frProtectDeleteRecord, True)
  Else
    FTable.SetFlagRecord(frProtectDeleteRecord, False);
  ftable.Refresh;
End;

Procedure TdlgTable.CheckBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  If CheckBox3.Checked Then
    FTable.SetFlagRecord(frProtectUpdateRecord, True)
  Else
    FTable.SetFlagRecord(frProtectUpdateRecord, False);
  ftable.Refresh;
End;

Procedure TdlgTable.CheckBox3KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  If CheckBox3.Checked Then
    FTable.SetFlagRecord(frProtectUpdateRecord, True)
  Else
    FTable.SetFlagRecord(frProtectUpdateRecord, False);
  ftable.Refresh;
End;

Procedure TdlgTable.CheckBox2KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Not fTable.Active Then Exit;
  If fTable.IsEmpty Then Exit;
  If fTable.State In [dsedit, dsinsert] Then Exit;

  If CheckBox2.Checked Then
    FTable.SetFlagRecord(frProtectDeleteRecord, True)
  Else
    FTable.SetFlagRecord(frProtectDeleteRecord, False);
  ftable.Refresh;
End;

Procedure TdlgTable.CheckBox4Click(Sender: TObject);
Begin
  fTable.fliporder := checkBox4.checked;
End;

Procedure TdlgTable.SupportRecNo1Click(Sender: TObject);
Begin
  SupportRecNo1.Checked := Not SupportRecNo1.Checked;
  fTable.SupportRecNo := SupportRecNo1.Checked;
End;

Procedure TdlgTable.SetFieldNull1Click(Sender: TObject);
Var
  DoPost: boolean;
Begin
  If (Not FTable.IsEmpty) Then
    If (MessageDlg('Set Field Null?', mtConfirmation, mbOKCancel, 0) <> idCancel) Then
      Begin
        DoPost := Not (FTable.State In [dsInsert, dsEdit]);
        If DoPost Then
          FTable.Edit;
        grdTableBrowser.SelectedField.Clear;
        If DoPost Then
          FTable.Post;
      End;
End;

procedure TdlgTable.SetCheckTimeout1Click(Sender: TObject);
Var
  sTimeout: String;
  res: Boolean;
Begin
  sTimeout := IntToStr(FTable.CheckTimeout);
  Repeat
    res := InputQuery('Table Check Timeout (ms)', 'Value:', sTimeout);
    If res Then
      Try
        FTable.CheckTimeout := StrToInt(sTimeout);
        If FTable.CheckTimeout < -1 Then
          Raise EConvertError.Create('');
        res := False;
      Except
        On EConvertError Do
          Begin
            MessageDlg('Value must be a number between -1 and ' + IntToStr(MaxInt), mtError, [mbOK], 0);
          End;
      End;
  Until Not res;
end;

End.

