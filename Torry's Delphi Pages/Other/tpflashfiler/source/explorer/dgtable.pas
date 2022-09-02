{*********************************************************}
{* FlashFiler: Table Browser                             *}
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

unit dgtable;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Db,
  StdCtrls,
  Grids,
  DBGrids,
  DBCtrls,
  ExtCtrls,
  Buttons,
  Menus,
  ComCtrls,
  ffdb,
  ffdbbase,
  fflllgcy,
  ffllbase,
  ffclreng,
  ffllprot,
  fflllog,
  ffutil,
  ffclbase,
  Mask,
  dgSetRng,
  uEntity,
  uConsts;


type
  TdlgTable = class(TForm)
    dsTableBrowser: TDataSource;
    navTableBrowser: TDBNavigator;
    barStatus: TStatusBar;
    MainMenu1: TMainMenu;
    mnuTable: TMenuItem;
    N1: TMenuItem;
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
    grdTableBrowser: TDBGrid;
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
    Image: TImage;
    tsGenericBlobTemplate: TTabSheet;
    meGeneric: TMemo;
    mnuViewShowRange: TMenuItem;
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
    N4: TMenuItem;
    mnuTablePrintPreview: TMenuItem;
    mnuTableDesignReport: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cboIndexChange(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure mnuTableCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuViewRefreshClick(Sender: TObject);
    procedure mnuViewShowFilterClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnFindNearClick(Sender: TObject);
    procedure btnSetFilterClick(Sender: TObject);
    procedure edtFindEnter(Sender: TObject);
    procedure cboFilterEnter(Sender: TObject);
    procedure mnuViewShowRecordCountClick(Sender: TObject);
    procedure mnuTableResetColClick(Sender: TObject);
    procedure grdTableBrowserKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuOptionsDebugClick(Sender: TObject);
    procedure mnuOptionsTimeoutClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbStretchClick(Sender: TObject);
    procedure btnClearBAClick(Sender: TObject);
    procedure pcBlobfieldsChange(Sender: TObject);
    procedure mnuViewShowBLOBFieldsClick(Sender: TObject);
    procedure btnLoadMemoClick(Sender: TObject);
    procedure btnSaveMemoClick(Sender: TObject);
    procedure btnLoadGenericClick(Sender: TObject);
    procedure btnSaveGenericClick(Sender: TObject);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnLoadGraphicClick(Sender: TObject);
    procedure btnSaveGraphicClick(Sender: TObject);
    procedure btnClearGraphicClick(Sender: TObject);
    procedure btnClearGenericClick(Sender: TObject);
    procedure meByteArrayKeyPress(Sender: TObject; var Key: Char);
    procedure mnuTablePrintPreviewClick(Sender: TObject);
    procedure btnSetClearRangeClick(Sender: TObject);
    procedure mnuTableDesignReportClick(Sender: TObject);
    procedure tsMemoTemplateResize(Sender: TObject);
    procedure tsGraphicTemplateResize(Sender: TObject);
    procedure tsGenericBlobTemplateResize(Sender: TObject);
    procedure tsByteArrayTemplateResize(Sender: TObject);
    procedure btnEditRangeClick(Sender: TObject);
    procedure mnuViewShowRangeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure meByteArrayChange(Sender: TObject);
    procedure cbWordwrapClick(Sender: TObject);
    procedure mnuTableCopyToTableClick(Sender: TObject);
    procedure mnuTableDeleteRecordsClick(Sender: TObject);                   {!!.07}
  private
    procedure FTableAfterPost(DataSet: TDataSet);                      {!!.07}
    procedure FTableAfterScroll(DataSet: TDataSet);
    procedure FTableAfterCancel(DataSet: TDataSet);
    procedure FTableBeforeEdit(DataSet: TDataSet);
    procedure FTableBeforeInsert(DataSet: TDataSet);
    procedure ViewActiveBlobField;
    procedure SetRange;
  protected
    FClient       : TffClient;
    FDatabaseName : TffName;
    FEngine       : TffRemoteServerEngine;
    FLog          : TffBaseLog;
    FProtocol     : TffProtocolType;
    FReadOnly     : boolean;
    FServerName   : TffNetAddress;
    FSession      : TFfSession;
    FTable        : TFfTable;
    FTableName    : TffName;
    FUserName     : TffName;
    FPassword     : TffName;
    FTransport    : TffLegacyTransport;
    FTableItem    : TffeTableItem;

    dtShown       : boolean;
      {-Set to True if the form was actually displayed. Lets the form know
        it should save user preferences. }
    InRange       : boolean;
      { true if SetRange has been performed }
    FRangeValues  : TffRangeValues;
      { the start and end values for the active range }
    BeforeInitDone : Boolean;
      { to keep UpdateDisplay from being called repeatedly }
    BAKeyPressDetected : Boolean;
      { to avoid going to Edit mode when changing ByteArray edit programmatically }
    AddedComponentCount : Integer;
      { used to avoid duplicate names in dynamically added components }
    FDynEnabledComponents,                                              {!!.11}
    FDynReadOnlyComponents: TList;
      { used to easily enable and disable the dynamically added components }

    procedure SavePreferences;
    procedure LoadPreferences;
    procedure WMGetMinMaxInfo(var Message : TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    function  HasBlobOrByteArrayField : Boolean;                        {!!.07}
    procedure GenerateRangeDisplayStrings;                              {!!.07}
  protected { access methods }
    procedure SetReadOnly(const Value : Boolean);
  public
    procedure CloseDuringShow(var Message : TMessage); message ffm_Close;
    procedure UpdateDisplay;                                             {!!.01}
    procedure UpdateDefaultTimeout;                                      {!!.11}

    property Protocol : TffProtocolType
      read FProtocol write FProtocol;

    property ServerName : TffNetAddress
      read FServerName write FServerName;

    property DatabaseName : TffName
      read FDatabaseName write FDatabaseName;

    property Log : TffBaseLog
      read FLog write FLog;

    property Password : TffName
      read FPassword write FPassword;

    property TableName : TffName
      read FTableName write FTableName;

    property ReadOnly : boolean
      read FReadOnly write SetReadOnly;

    property UserName : TffName
      read FUserName write FUserName;

    property TableItem : TffeTableItem
      read FTableItem write FTableItem;
  end;

var
  dlgTable: TdlgTable;

implementation

uses
  dgCpyTbl,                                                            {!!.10}
  typinfo,                                                             {!!.07}
  jpeg,                                                                {!!.07}
  uReportEngineInterface,                                              {!!.07}
  {$IFDEF DCC6ORLater}
  variants,                                                            {!!.07}
  {$ENDIF}
  FFLLComm,
  FFLLComp,
  FFLLEng,
  uConfig;

{$R *.DFM}

const
  MaxFilterComboItems = 10;                                       {!!.11}

procedure TdlgTable.FormCreate(Sender: TObject);
begin

  FClient := nil;
  FDatabaseName := '';
  FEngine := nil;
  FLog := nil;
  FProtocol := ptRegistry;
  FReadOnly := False;
  FServerName := '';
  FSession := nil;
  FTable := nil;
  FTableName := '';
  FTransport := nil;
  FPassword := '';
  FUserName := '';

  InRange := False;
  BeforeInitDone := True;
  BAKeyPressDetected := False;
  AddedComponentCount := 0;
  FDynEnabledComponents := TList.Create;                                      {!!.11}
  FDynReadOnlyComponents := TList.Create;                                     {!!.11}
end;
{--------}
procedure TdlgTable.SetReadOnly(const Value : Boolean);
var
  i : Integer;
  bm: TBookmark;
  FieldsTags: TList;
begin
  FReadOnly := Value;
  grdTableBrowser.ReadOnly := FReadOnly;
  {Begin !!.11}
  { only update the buttons after they are created,
    and table when it's opened. }
  if not dtShown then
    Exit;
  bm := FTable.GetBookmark;
  FieldsTags := TList.Create;
  try
    { save blob-support pointers }
    for i := 0 to Pred(FTable.FieldCount) do
      FieldsTags.Add(Pointer(FTable.Fields[i].Tag));
    FTable.Close;
    FTable.ReadOnly := ReadOnly;
    FTable.Open;
    for i := 0 to Pred(FTable.FieldCount) do
      FTable.Fields[i].Tag := Integer(FieldsTags[i]);
    FTable.GotoBookmark(bm);
  finally
    FTable.FreeBookmark(bm);
    FieldsTags.Free;
  end;
  for i := 0 to Pred(ComponentCount) do
    if (Components[i] is TButton) and
     (((Components[i] as TButton).Caption='Load from file...') or
      ((Components[i] as TButton).Caption='Save to file...') or
      ((Components[i] as TButton).Caption='Clear')) then
       (Components[i] as TButton).Enabled := not FReadOnly;
  {End !!.11}
end;
{--------}
procedure TdlgTable.FormShow(Sender: TObject);
var
  aServerName : string;
  aAddress : string;
  I : Integer;
  OldPass, OldUser : string;

  {$IFNDEF DCC5OrLater}
  function IsPublishedProp(Source : TObject; const PropName : string) : Boolean;
  var
    P: PPropInfo;
  begin
   P := GetPropInfo(Source.ClassInfo, PropName);
   Result := P <> nil;
  end;
  {--------}
  function GetStrProp(Source : TObject; const PropName : string) : string;
  var
    P: PPropInfo;
  begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    if Assigned(P) then begin
      Result := TypInfo.GetStrProp(Source, P);
    end else
      Result := '';
  end;
  {--------}
  function SetStrProp(Source : TObject; const PropName, Value : string) : string;
  var
    P: PPropInfo;
  begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    if Assigned(P) then
      TypInfo.SetStrProp(Source, P, Value);
  end;
  {--------}
  procedure SetMethodProp(Source : TObject; const PropName : string; Value : TMethod);
  var
    P: PPropInfo;
  begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    if Assigned(P) then
      TypInfo.SetMethodProp(Source, P, Value);
  end;
  {--------}
  function GetMethodProp(Source : TObject; const PropName : string) : TMethod;
  var
    P: PPropInfo;
  begin
    P := GetPropInfo(Source.ClassInfo, PropName);
    if Assigned(P) then
      Result := TypInfo.GetMethodProp(Source, P);
  end;
  {$ENDIF}
  {Begin !!.07}
  function CopyComponent(Source : TComponent) : TComponent;
  var
    PropStream : TMemoryStream;
    OldText, OldName : String;
  begin
    Result := Nil;
    if assigned(Source) then
    begin
      PropStream := TMemoryStream.Create;
      try
        //prevent doubled component names
        OldName := Source.Name;
        Source.Name := OldName + IntToStr(AddedComponentCount);
        Inc(AddedComponentCount);
        //Save the "stored" properties to memory
        PropStream.WriteComponent(Source);
        Source.Name := OldName;
        //e.g. TEdit will change it's content if renamed
        if IsPublishedProp(Source,'Text') then
          OldText := GetStrProp(Source,'Text')
        else
          //Some Captions may face the same problem
          if IsPublishedProp(Source,'Caption') then
            OldText := GetStrProp(Source,'Caption');
        Result := TComponentClass(Source.ClassType).Create(Source.Owner);
        PropStream.Position := 0;
        PropStream.ReadComponent(Result);
//        Result.Name := OldName + IntToStr(AddedComponentCount);
        //Handle Components with a "Text" or "Caption" -property;
        //e.g. TEdit, TLabel
        if IsPublishedProp(Source,'Text') then
        begin
          SetStrProp(Source,'Text',OldText);
          SetStrProp(Result,'Text',OldText);
        end
        else
          if IsPublishedProp(Source,'Caption') then
          begin
            SetStrProp(Source,'Caption',OldText);
            SetStrProp(Result,'Caption',OldText);
          end;
      finally
        PropStream.Free;
      end;
    end;
  end;



  { generates a new tabsheet and hooks up
    components on the new tabsheet to the field }
  procedure CreateNewBlobTabSheet(SheetToCopy : TTabSheet; OnResizeProc : TNotifyEvent; FieldIndex : Integer);
  var
    NewSheet : TTabSheet;
    Idx : Integer;
    NewComponent : TComponent;
  begin
    NewSheet := TTabSheet.Create(pcBlobFields);
    NewSheet.PageControl := pcBlobFields;
    NewSheet.Caption := FTable.Fields[FieldIndex].FieldName;
    {$IFDEF DCC4OrLater}
    NewSheet.OnResize := OnResizeProc;
    {$ENDIF}

    for Idx := 0 to SheetToCopy.ControlCount-1 do begin
      NewComponent := CopyComponent(SheetToCopy.Controls[Idx]);
      TControl(NewComponent).Parent := NewSheet;
      if IsPublishedProp(NewComponent, 'DataField') then
        SetStrProp(NewComponent, 'DataField', FTable.Fields[FieldIndex].FieldName);
      if (IsPublishedProp(NewComponent, 'OnClick')) then
        SetMethodProp(NewComponent, 'OnClick', GetMethodProp(SheetToCopy.Controls[Idx], 'OnClick'));
      if (IsPublishedProp(NewComponent, 'OnKeyPress')) then
        SetMethodProp(NewComponent, 'OnKeyPress', GetMethodProp(SheetToCopy.Controls[Idx], 'OnKeyPress'));
      if (IsPublishedProp(NewComponent, 'OnChange')) then
        SetMethodProp(NewComponent, 'OnChange', GetMethodProp(SheetToCopy.Controls[Idx], 'OnChange'));
//      if NewComponent. IS TCheckBox
  //      SetStrProp(NewComponent, 'OnClick', FTable.Fields.Fields[FieldIndex].FieldName);
      { save pointer to the control displaying the field }
      if (NewComponent IS TImage) or     { graphictemplate }
         (NewComponent IS TMaskEdit) or  { bytearraytemplate }
         (NewComponent IS TMemo) or      { generictemplate }
         (NewComponent IS TdbMemo) then  { memotemplate }
        FTable.Fields[FieldIndex].Tag := Integer(NewComponent);

    end;
  end;
  {End !!.07}

begin
  dtShown := False;
  try
    { Set up the connection. }
    FTransport := TffLegacyTransport.Create(nil);
    with FTransport do begin
      Mode := fftmSend;
      Protocol := FProtocol;
      EventLog := FLog;
      if Assigned(FLog) then begin
        EventLogEnabled := True;
        EventLogOptions := [fftpLogErrors];
      end;
      ServerName := FServerName;
    end;

    FEngine := TffRemoteServerEngine.Create(nil);
    FEngine.Transport := FTransport;

    FClient := TffClient.Create(nil);
    FClient.ServerEngine := FEngine;
    FClient.AutoClientName := True;
    FClient.TimeOut := Config.DefaultTimeout;                        {!!.11}

    FSession := TffSession.Create(nil);
    FSession.ClientName := FClient.ClientName;
    FSession.AutoSessionName := True;
    OldPass := ffclPassword;
    OldUser := ffclUserName;
    try
      if FPassword <> '' then begin
        ffclPassword := FPassword;
        ffclUserName := FUserName;
      end;
      FSession.Open;
    finally
      ffclPassword := OldPass;
      ffclUserName := OldUser;
    end;

    FTable := TffTable.Create(nil);
    FTable.SessionName := FSession.SessionName;
    FTable.DatabaseName := FDatabaseName;
    FTable.TableName := FTableName;
    FTable.AfterPost := FTableAfterPost;                              {!!.07}
    FTable.AfterDelete := FTableAfterPost;                            {!!.07}
    FTable.AfterScroll := FTableAfterScroll;                          {!!.07}
    FTable.AfterCancel := FTableAfterCancel;                          {!!.07}
    FTable.BeforeEdit := FTableBeforeEdit;
    FTable.BeforeInsert := FTableBeforeInsert;
    FTable.ReadOnly := ReadOnly;                                      {!!.11}
    FTable.Open;

    { Set up the indexes }
    cboIndex.Items.Clear;
    with FTable.IndexDefs do begin
      Clear;
      Update;
      for I := 0 to Count - 1 do
        cboIndex.Items.Add(Items[I].Name);
    end;

    cboIndex.ItemIndex := 0;
    FTable.IndexName := cboIndex.Items[cboIndex.ItemIndex];

    { Update the find controls }
    cboIndexChange(nil);

    FFSeparateAddress(FTransport.ServerName, aServerName, aAddress);
    Self.Caption := format('%s : %s : %s',
                           [aServerName, FDatabaseName, FTableName]);

    dsTableBrowser.DataSet := FTable;

    {Begin !!.07}
    { check if there are any BLOB fields in the table
      and populate the pagecontrol with appropriate controls if so }

    { make the templates invisible }
    for I := 0 to pcBlobFields.PageCount-1 do
      pcBlobFields.Pages[I].TabVisible := False;

    { generate new tabsheets for blobfields }
    for I := 0 to FTable.Dictionary.FieldCount-1 do begin
      case FTable.Dictionary.FieldType[I] of
        fftBLOBMemo,
        fftBLOBFmtMemo : CreateNewBlobTabSheet(tsMemoTemplate, tsMemoTemplateResize, I);
        fftBLOBGraphic : CreateNewBlobTabSheet(tsGraphicTemplate, tsGraphicTemplateResize, I);
        fftByteArray   : CreateNewBlobTabSheet(tsByteArrayTemplate, tsByteArrayTemplateResize, I);
        fftBLOB,
        fftBLOBOLEObj,
        fftBLOBDBSOLEObj,
        fftBLOBTypedBin,
        fftBLOBFile       : CreateNewBlobTabSheet(tsGenericBlobTemplate, tsGenericBlobTemplateResize, I);
      end;
    end;

    {End !!.07}

    LoadPreferences;

    BeforeInitDone := False;
    UpdateDisplay;

    ViewActiveBlobField; {!!.07}

    { make sure no column exceeds screen width }                        {!!.07}
    for I := 0 to grdTableBrowser.Columns.Count-1 do begin
      if grdTableBrowser.Columns[i].Width>(Width DIV 5)*4 then
        grdTableBrowser.Columns[i].Width := (Width DIV 5)*4;
    end;

    dtShown := True;
    { update newly created dynamic components }
    ReadOnly := FReadOnly;                                              {!!.11}

    { large font support... }
    if (Screen.PixelsPerInch/PixelsPerInch)>1.001 then begin
      Height := Round(Height * (Screen.PixelsPerInch/PixelsPerInch));
      Width := Round(Width * (Screen.PixelsPerInch/PixelsPerInch));
      barStatus.Height := Round(barStatus.Height * (Screen.PixelsPerInch/PixelsPerInch));
    end;

    { report menuitems }
    mnuTablePrintPreview.Enabled := ReportEngineDLLLoaded;
    mnuTableDesignReport.Enabled := ReportEngineDLLLoaded;

  except
    on E:Exception do begin
      showMessage(E.message);
      PostMessage(Handle, ffm_Close, 0, longInt(Sender));
    end;
  end;
end;
{--------}
procedure TdlgTable.cboIndexChange(Sender: TObject);
var
  BaseSection : string;
  Index : Integer;
begin
  BaseSection := ClassName + '.' + Self.Caption;
  with FTable do
    if IndexName <> cboIndex.Items[cboIndex.ItemIndex] then begin
      IndexName := cboIndex.Items[cboIndex.ItemIndex];
    end;
  lblFind.Visible := cboIndex.ItemIndex > 0;
  edtFind.Visible := cboIndex.ItemIndex > 0;
  btnFindNear.Visible := cboIndex.ItemIndex > 0;
  btnSetClearRange.Enabled := cboIndex.ItemIndex > 0;
  btnEditRange.Enabled := cboIndex.ItemIndex > 0;
  { clear range - btnSetClearRangeClick flips InRange }
  InRange := True;
  btnSetClearRangeClick(Self);
  for Index := Low(FRangeValues.Field) to FTable.IndexFieldCount do begin
    FRangeValues.Field[Index].StartNull := FFEConfigGetBoolean(BaseSection, FTable.IndexName+'_RangeStartNull'+IntToStr(Index), True);
    FRangeValues.Field[Index].EndNull := FFEConfigGetBoolean(BaseSection, FTable.IndexName+'_RangeEndNull'+IntToStr(Index), True);
    FRangeValues.Field[Index].StartValue := FFEConfigGetString(BaseSection, FTable.IndexName+'_RangeStartValue'+IntToStr(Index), '');
    FRangeValues.Field[Index].EndValue := FFEConfigGetString(BaseSection, FTable.IndexName+'_RangeEndValue'+IntToStr(Index), '');;
  end;
  FRangeValues.RangeStartKeyExclusive := FFEConfigGetBoolean(BaseSection, FTable.IndexName+'_RangeStartKeyExclusive', False);
  FRangeValues.RangeEndKeyExclusive := FFEConfigGetBoolean(BaseSection, FTable.IndexName+'_RangeEndKeyExclusive', False);
  GenerateRangeDisplayStrings;
end;
{--------}
procedure TdlgTable.btnFindClick(Sender: TObject);
begin
  try
    FTable.FindNearest([edtFind.Text]);
  except
    on E: EffDatabaseError do begin
      if E.ErrorCode = 8706 then
        ShowMessage(format('%s not found.', [edtFind.Text]))
      else
        ShowMessage(E.Message);
    end;
  end;
end;
{--------}
procedure TdlgTable.mnuTableCloseClick(Sender: TObject);
begin
  Close;
end;
{--------}
procedure TdlgTable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if dtShown then
    SavePreferences;
  Action := caFree;
end;
{--------}
procedure TdlgTable.mnuViewRefreshClick(Sender: TObject);
begin
  FTable.Refresh;
  UpdateDisplay;
end;
{--------}
procedure TdlgTable.UpdateDisplay;
begin
  if BeforeInitDone then
    Exit;
  if mnuViewShowRecordCount.Checked then
    barStatus.Panels[0].Text := 'Records: ' + FFCommaizeChL(FTable.RecordCount, ThousandSeparator)
  else
    barStatus.Panels[0].Text := '';

  if FTable.Filtered then
    barStatus.Panels[1].Text := 'Filter: <ACTIVE>'
  else
    barStatus.Panels[1].Text := 'Filter: <Inactive>';

  if InRange then begin
    barStatus.Panels[2].Text := 'Range: <ACTIVE>';
    laRangeStart.Font.Style := [fsBold];
    laRangeEnd.Font.Style := [fsBold];
    laRangeStartDesc.Font.Style := [fsBold];
    laRangeEndDesc.Font.Style := [fsBold];
  end
  else begin
    barStatus.Panels[2].Text := 'Range: <Inactive>';
    laRangeStart.Font.Style := [];
    laRangeEnd.Font.Style := [];
    laRangeStartDesc.Font.Style := [];
    laRangeEndDesc.Font.Style := [];
  end;

  with navTableBrowser do begin
    VisibleButtons := [nbFirst, nbLast, nbPrior, nbNext, nbRefresh];
    if (not FTable.ReadOnly) and (not FReadOnly) then
      VisibleButtons := VisibleButtons + [nbInsert, nbDelete, nbEdit, nbPost, nbCancel];
  end;
end;
{--------}
procedure TdlgTable.mnuViewShowFilterClick(Sender: TObject);
begin
  mnuViewShowFilter.Checked := not mnuViewShowFilter.Checked;
  pnlFilter.Visible := mnuViewShowFilter.Checked;
  { make sure to reset statusbar etc if status changes }
  if FTable.Filtered then
    btnSetFilterClick(Self);
//  edtFilter.Text := ''; why remove? makes sense to keep the text, the user might need it again!
end;
{--------}
procedure TdlgTable.btnFilterClick(Sender: TObject);
begin
  if FTable.Filtered then begin
    FTable.Filtered := False;
    btnSetFilter.Caption := 'S&et Filter';                            {!!.03}
  end else begin
    FTable.Filter := cboFilter.Text;
    FTable.Filtered := True;
    btnSetFilter.Caption := 'Cl&ear Filter';                          {!!.03}
  end;
end;
{--------}
procedure TdlgTable.FormDestroy(Sender: TObject);
begin
{Begin !!.05 !!.10}
  try
    FTable.Close;
  finally
    FTable.Free;
  end;

  try
    FSession.Active := False;
  finally
    FSession.Free;
  end;

  try
    FClient.Close;
  finally
    FClient.Free;
  end;

  try
    FEngine.Shutdown;
  finally
    FEngine.Free;
  end;

  try
    FTransport.Shutdown;
  finally
    FTransport.Free;
  end;
{End !!.05}
  FDynEnabledComponents.Free;                                      {!!.11}
  FDynReadOnlyComponents.Free;                                     {!!.11}
end;
{--------}
procedure TdlgTable.CloseDuringShow(var Message : TMessage);
begin
  Close;
end;
{--------}
procedure TdlgTable.WMGetMinMaxInfo(var Message : TWMGetMinMaxInfo);
var
  MinMax : PMinMaxInfo;
begin
  inherited;
  MinMax := Message.MinMaxInfo;
  MinMax^.ptMinTrackSize.x := 590;
end;
{--------}
procedure TdlgTable.btnFindNearClick(Sender: TObject);
begin
  try
    FTable.FindNearest([edtFind.Text]);
  except
    on E: EffDatabaseError do begin
      if E.ErrorCode = 8706 then
        ShowMessage(format('%s not found.', [edtFind.Text]))
      else
        ShowMessage(E.Message);
    end;
  end;
end;
{--------}
procedure TdlgTable.btnSetFilterClick(Sender: TObject);
{Begin !!.05}
var
  SavCursor : TCursor;
begin
  SavCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if FTable.Filtered then begin
      FTable.Filtered := False;
      btnSetFilter.Caption := 'S&et Filter';                           {!!.03}
    end else begin
      FTable.Filter := cboFilter.Text;
      FTable.Filtered := True;
      btnSetFilter.Caption := 'Cl&ear Filter';                         {!!.03}
      {Begin !!.11}
      { update history list in combobox }
      if FTable.Filter<>'' then begin
        { does filter exist in the list? }
        if cboFilter.Items.IndexOf(FTable.Filter)>=0 then
          { if so remove it; no doubles needed }
          cboFilter.Items.Delete(cboFilter.Items.IndexOf(FTable.Filter));
        { make last filter string top of the history list }
        cboFilter.Items.Insert(0, FTable.Filter);
        cboFilter.ItemIndex := 0;
        { enforce maxcount }
        while cboFilter.Items.Count>MaxFilterComboItems do
          cboFilter.Items.Delete(MaxFilterComboItems);
      end;
      {End !!.11}
    end;
    UpdateDisplay;
  finally
    Screen.Cursor := SavCursor;
  end;
{End !!.05}
end;
{--------}
procedure TdlgTable.edtFindEnter(Sender: TObject);
begin
  btnSetFilter.Default := False;
  btnFindNear.Default := True;
end;
{--------}
procedure TdlgTable.cboFilterEnter(Sender: TObject);
begin
  btnFindNear.Default := False;
  btnSetFilter.Default := True;
end;
{--------}
procedure TdlgTable.SavePreferences;
var
  BaseSection : string;
  i : Integer;                                                      {!!.11}
begin
  try
    BaseSection := ClassName + '.' + Self.Caption;
    FFEConfigSaveString(BaseSection, 'Last Filter', cboFilter.Text);
    {Begin !!.11}
    for i := 0 to Pred(cboFilter.Items.Count) do
      FFEConfigSaveString(BaseSection, 'FilterHistory'+IntToStr(i), cboFilter.Items[i]);
    {End !!.11}
    FFEConfigSaveString(BaseSection, 'Last Find Nearest', edtFind.Text);
    FFEConfigSaveInteger(BaseSection, 'Last Index', cboIndex.ItemIndex);
    FFEConfigSaveBoolean(BaseSection, 'Show record count', mnuViewShowRecordCount.Checked);
    FFEConfigSaveFormPrefs(BaseSection, Self);
    FFEConfigSaveDBColumnPrefs(BaseSection + '.ColumnInfo', grdTableBrowser.Columns);
    FFEConfigSaveInteger(BaseSection, 'Table Timeout', FTable.Timeout);                       {!!.07}
    FFEConfigSaveInteger(BaseSection, 'PageControl size', pcBlobfields.Height);  {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show blob fields', mnuViewShowBLOBFields.Checked);  {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show range', mnuViewShowRange.Checked);             {!!.07}
    FFEConfigSaveBoolean(BaseSection, 'Show filter', mnuViewShowFilter.Checked);           {!!.07}
  except
    on E:Exception do
      ShowMessage('Error writing INI file: '+E.Message);
  end;
end;
{--------}
procedure TdlgTable.LoadPreferences;
var
  BaseSection : string;
  Index : Integer;
  s : String;                                                          {!!.11}
begin
  BaseSection := ClassName + '.' + Self.Caption;
  cboFilter.Text := FFEConfigGetString(BaseSection, 'Last Filter', '');
  {Begin !!.11}
  for Index := 0 to Pred(MaxFilterComboItems) do begin
    s := FFEConfigGetString(BaseSection, 'FilterHistory'+IntToStr(Index), '');
    if s<>'' then
      cboFilter.Items.Add(s);
  end;
  {End !!.11}
  Index := FFEConfigGetInteger(BaseSection, 'Last Index', 0);
  if (Index < cboIndex.Items.Count) then begin
    cboIndex.ItemIndex := Index;
    FTable.IndexName := cboIndex.Items[cboIndex.ItemIndex];
    { Update the find controls }
    cboIndexChange(nil);
  end;
  edtFind.Text := FFEConfigGetString(BaseSection, 'Last Find Nearest', '');
  mnuViewShowRecordCount.Checked := FFEConfigGetBoolean(BaseSection, 'Show record count', True);
  FFEConfigGetFormPrefs(BaseSection, Self);
  FFEConfigGetDBColumnPrefs(BaseSection + '.ColumnInfo', grdTableBrowser.Columns);
  FTable.Timeout := FFEConfigGetInteger(BaseSection, 'Table Timeout', -1);  {!!.07}
  pcBlobfields.Height := FFEConfigGetInteger(BaseSection, 'PageControl size', pcBlobfields.Height);  {!!.07}
  mnuViewShowBLOBFields.Checked := HasBlobOrByteArrayField and FFEConfigGetBoolean(BaseSection, 'Show blob fields', True);  {!!.07}
  if not HasBlobOrByteArrayField then
    mnuViewShowBLOBFields.Enabled := False;
  pcBlobfields.Visible := mnuViewShowBLOBFields.Checked and HasBlobOrByteArrayField;
  splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked and HasBlobOrByteArrayField;
  mnuViewShowRange.Checked := FFEConfigGetBoolean(BaseSection, 'Show range', True);             {!!.07}
  pnlRange.Visible := mnuViewShowRange.Checked;
  mnuViewShowFilter.Checked := FFEConfigGetBoolean(BaseSection, 'Show filter', True);           {!!.07}
  pnlFilter.Visible := mnuViewShowFilter.Checked;
end;
{--------}
procedure TdlgTable.mnuViewShowRecordCountClick(Sender: TObject);
begin
  mnuViewShowRecordCount.Checked := not mnuViewShowRecordCount.Checked;
  UpdateDisplay;
end;

procedure TdlgTable.mnuTableResetColClick(Sender: TObject);
begin
  grdTableBrowser.Columns.RebuildColumns;
end;

procedure TdlgTable.grdTableBrowserKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  DoPost : Boolean;                                            {!!.11}
begin
  { Delete record? }
  if ((key = VK_DELETE) and
      (shift = []) and
      (dsTableBrowser.State = dsBrowse)) and
      (not grdTableBrowser.ReadOnly) then
    if (MessageDlg('Delete record?', mtConfirmation, mbOKCancel, 0) <> idCancel) then
      dsTableBrowser.DataSet.Delete;
  {Begin !!.11}
  { set field to NULL? }
  if ((key = Ord('0')) and
      (shift = [ssCtrl]) and
      (not grdTableBrowser.ReadOnly) and
      (not FTable.IsEmpty)) then begin
    DoPost := not (FTable.State in [dsInsert, dsEdit]);
    if DoPost then
      FTable.Edit;
    grdTableBrowser.SelectedField.Clear;
    if DoPost then
      FTable.Post;
    { refresh; could be blobfield }
    ViewActiveBlobField;
  end;
  {End !!.11}
end;
{Begin !!.02}
{--------}
procedure TdlgTable.mnuOptionsDebugClick(Sender: TObject);
begin
  mnuOptionsDebug.Checked := not mnuOptionsDebug.Checked;
  if mnuOptionsDebug.Checked then
    FTransport.EventLogOptions := [fftpLogErrors, fftpLogRequests,
                                   fftpLogReplies]
  else
    FTransport.EventLogOptions := [fftpLogErrors];
end;
{End !!.02}

{Begin !!.07}
procedure TdlgTable.FTableAfterPost(DataSet: TDataSet);
begin
  if FTable.Database.InTransaction then
    FTable.Database.Commit;
  UpdateDisplay;
end;
{--------}
procedure TdlgTable.FTableAfterCancel(DataSet: TDataSet);
begin
  if FTable.Database.InTransaction then
    FTable.Database.Rollback;
  FTable.Refresh;
  ViewActiveBlobField;
end;
{--------}
procedure TdlgTable.FTableAfterScroll(DataSet: TDataSet);
begin
  ViewActiveBlobField;
end;
{--------}
procedure TdlgTable.FTableBeforeEdit(DataSet: TDataSet);
begin
  if not FTable.Database.InTransaction then
    FTable.Database.StartTransaction;
end;
{--------}
procedure TdlgTable.FTableBeforeInsert(DataSet: TDataSet);
begin
  if not FTable.Database.InTransaction then
    FTable.Database.StartTransaction;
end;
{--------}
procedure TdlgTable.mnuOptionsTimeoutClick(Sender: TObject);
var
  sTimeout : String;
  res : Boolean;
begin
  sTimeout := IntToStr(FTable.Timeout);
  repeat
    res := InputQuery('Table Timeout (ms)', 'Value:', sTimeout);
    if res then
    try
      FTable.Timeout := StrToInt(sTimeout);
      if FTable.Timeout<-1 then
        raise EConvertError.Create('');
      res := False;
    except
      on EConvertError do begin
        MessageDlg('Value must be a number between -1 and '+IntToStr(MaxInt), mtError, [mbOK], 0);
      end;
    end;
  until not res;
end;
{--------}
procedure TdlgTable.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not (FTable.State IN [dsInsert, dsEdit])) and
     (Key = VK_ESCAPE) then
    Close;
end;
{--------}
procedure TdlgTable.ViewActiveBlobField;
const
  JPEGHeader : array [0..10] of Char = (Chr($FF), Chr($D8), Chr($FF), Chr($E0),
                                        Chr($0), Chr($10), 'J', 'F', 'I', 'F', Chr(0));
  BMPHeader : array [0..1] of char = ('B', 'M');
  WMFHeader : array [0..1] of char = ('B', 'M');
  ICOHeader : array [0..1] of char = ('B', 'M');
  HexChar : array [0..15] of char = '0123456789ABCDEF';
var
  HeaderBuffer : array [0..10] of char;
  Stream : TffBlobStream;
  jpegImage : TJPEGImage;
  i : Integer;
  BlobBuffer : array [0..1024] of char;
  ByteArrayBuffer : Pointer;
  tempStr : String;

  { copied from TffEventLog.WriteBlock and transmogrified }
  function GenerateHexLines(Buf : pointer; BufLen : TffMemSize) : String;
  const
    HexPos : array [0..15] of byte =
      (1, 3, 5, 7, 10, 12, 14, 16, 19, 21, 23, 25, 28, 30, 32, 34);
  var
    B : PffByteArray absolute Buf;
    ThisWidth,
    i, j : integer;
    Line : string[56];
    Work : byte;
  begin
    Result := '';
    if (BufLen = 0) or (Buf = nil) then
      Exit
    else begin
      if (BufLen > 1024) then begin
        BufLen := 1024;
      end;
      for i := 0 to ((BufLen-1) shr 4) do begin
        FillChar(Line, 56, ' ');
        Line[0] := #55;
        Line[38] := '['; Line[55] := ']';
        if (BufLen >= 16) then
          ThisWidth := 16
        else
          ThisWidth := BufLen;
        for j := 0 to ThisWidth-1 do begin
          Work := B^[(i shl 4) + j];
          Line[HexPos[j]] := HexChar[Work shr 4];
          Line[HexPos[j]+1] := HexChar[Work and $F];
          if (Work < 32) {or (Work >= $80)} then
            Work := ord('.');
          Line[39+j] := char(Work);
        end;
        Result := Result + Line + ffcCRLF;
        dec(BufLen, ThisWidth);
      end;
    end;
  end;

  function ByteArrayToHexString(ByteArray : Pointer; ArrayLength : Integer) : String;
  var
    idx : Integer;
    BArr : PffByteArray absolute ByteArray;
  begin
    Result := '';
    for idx := 0 to ArrayLength-1 do begin
      Result := Result + HexChar[BArr[idx] shr 4];
      Result := Result + HexChar[BArr[idx] and $F];
    end;
  end;

begin
  { load non-db blob controls }
  if mnuViewShowBLOBFields.Checked and
     HasBlobOrByteArrayField then begin

    for i := 0 to FTable.Dictionary.FieldCount-1 do begin
      { only load blob on active tabsheet }
      if Assigned(Pointer(FTable.Fields[i].Tag)) and
         (FTable.Fields[i].FieldName=pcBlobfields.ActivePage.Caption) then
      case FTable.Dictionary.FieldType[i] of
        fftBLOBGraphic    : begin
                              try
                                Stream := TffBlobStream(FTable.CreateBlobStream(FTable.Fields[i], bmRead));
                                try
                                  TImage(FTable.Fields[i].Tag).Picture.Bitmap.Assign(NIL);
                                  TImage(FTable.Fields[i].Tag).Invalidate;
//                                  if Stream.Size>0 then
                                  { data in stream? }
                                  if (Stream.Read(HeaderBuffer, 11)=11) then
                                  { jpg? }
                                  if CompareMem(@jpegHeader, @HeaderBuffer, 11) then begin
                                    Stream.Position := 0;
                                    jpegImage := TJPEGImage.Create;
                                    try
                                      jpegImage.LoadFromStream(stream);
                                      TImage(FTable.Fields[i].Tag).Picture.Bitmap.Assign(jpegImage);
                                    finally
                                      jpegImage.Free;
                                    end;
                                  end
                                  else
                                  {bmp?}
                                  if CompareMem(@BMPHeader, @HeaderBuffer,  2) or
                                     CompareMem(@BMPHeader, @HeaderBuffer[8],  2) then begin
                                    if CompareMem(@BMPHeader, @HeaderBuffer,  2) then
                                      Stream.Position := 0
                                    else
                                      Stream.Position := 8;
                                    TImage(FTable.Fields[i].Tag).Picture.Bitmap.LoadFromStream(stream);
                                  end
                                  else begin
                                    {metafile?}
                                    { it's difficult to check for the metafile type. we just
                                      attempt to load and let the TImage component find out. }
                                    Stream.Position := 8;
                                    try
                                      TImage(FTable.Fields[i].Tag).Picture.Metafile.LoadFromStream(stream);
                                    except
                                      on EInvalidGraphic do begin
                                        Stream.Position := 0;
                                        try
                                          TImage(FTable.Fields[i].Tag).Picture.Metafile.LoadFromStream(stream);
                                        except
                                          on EInvalidGraphic do begin
                                            {icon?}
                                            { it's difficult to check for the icon type. we just
                                              attempt to load and let the TImage component find out. }
                                            Stream.Position := 8;
                                            try
                                              TImage(FTable.Fields[i].Tag).Picture.Icon.LoadFromStream(stream);
                                            except
                                              on EInvalidGraphic do begin
                                                Stream.Position := 0;
                                                try
                                                  TImage(FTable.Fields[i].Tag).Picture.Icon.LoadFromStream(stream);
                                                except
                                                  on EInvalidGraphic do
                                                  else
                                                    raise;
                                                end;
                                              end
                                              else
                                                raise;
                                            end;
                                          end
                                          else
                                            raise;
                                        end;
                                      end
                                      else
                                        raise;
                                    end;
                                  end;
                                finally
                                  Stream.Free;
                                end;
                              except
                                on E:Exception do begin
                                  ShowMessage('Exception: '+E.Message+' decoding graphic field: '+FTable.Fields[i].FieldName);
                                end;
                              end;
                            end;
        fftByteArray      : begin
                              with TMaskEdit(FTable.Fields[i].Tag) do begin
                                Text := '';
                                MaxLength := FTable.Fields[i].Size*2;
                                SetLength(tempStr, MaxLength);
                                FillChar(tempStr[1], MaxLength, 'a');
                                EditMask := tempStr + ';0;_';
                                GetMem(ByteArrayBuffer, FTable.Fields[i].DataSize);
                                try
                                  if FTable.Fields[i].GetData(ByteArrayBuffer) then
                                    Text := ByteArrayToHexString(ByteArrayBuffer, FTable.Fields[i].DataSize);
                                finally
                                  FreeMem(ByteArrayBuffer);
                                end;
                              end;
                            end;
        fftBLOB,
        fftBLOBOLEObj,
        fftBLOBDBSOLEObj,
        fftBLOBTypedBin,
        fftBLOBFile       : begin
                              try
                                Stream := TffBlobStream(FTable.CreateBlobStream(FTable.Fields[i], bmRead));
                                try
                                  TMemo(FTable.Fields[i].Tag).Text := '';
                                  Stream.Read(BlobBuffer, FFMinL(1024, Stream.Size));
                                  TMemo(FTable.Fields[i].Tag).Text :=
                                    GenerateHexLines(@BlobBuffer, FFMinL(1024, Stream.Size));
                                finally
                                  Stream.Free;
                                end;
                              except
                                on E:Exception do begin
                                  ShowMessage('Exception: '+E.Message+' when displaying blob field: '+FTable.Fields[i].FieldName);
                                end;
                              end;
                            end;
      end;
    end;
  end;

end;
{--------}
function TdlgTable.HasBlobOrByteArrayField: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FTable.Dictionary.FieldCount-1 do
    if FTable.Dictionary.FieldType[i] IN [fftBLOB..ffcLastBLOBType, fftByteArray] then begin
      Result := True;
      Exit;
    end;
end;
{--------}
procedure TdlgTable.cbStretchClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to TCheckBox(Sender).Parent.ControlCount-1 do
    if TCheckBox(Sender).Parent.Controls[i] IS TImage then begin
      TImage(TCheckBox(Sender).Parent.Controls[i]).Stretch := TCheckBox(Sender).Checked;
      Exit;
    end;
end;
{--------}
procedure TdlgTable.btnClearBAClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aEdit : TMaskEdit;
  aField : TField;
begin
  { find edit control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TMaskEdit then begin
      aEdit := TMaskEdit(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aEdit then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          aField.Clear;
          aEdit.Text := '';
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.pcBlobfieldsChange(Sender: TObject);
begin
  ViewActiveBlobField;
end;
{--------}
procedure TdlgTable.mnuViewShowBLOBFieldsClick(Sender: TObject);
begin
  mnuViewShowBLOBFields.Checked := not mnuViewShowBLOBFields.Checked;
  pcBlobfields.Visible := mnuViewShowBLOBFields.Checked;
  splGridAndPageControl.Visible := mnuViewShowBLOBFields.Checked;
  if mnuViewShowBLOBFields.Checked then
    ViewActiveBlobField;
end;
{--------}
procedure TdlgTable.btnLoadMemoClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  adbMemo : TdbMemo;
  aField : TField;
begin
  if opendialog.Execute then
  { find dbmemo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TdbMemo then begin
      adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=adbMemo then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          TMemoField(aField).LoadFromFile(opendialog.FileName);
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnSaveMemoClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  adbMemo : TdbMemo;
  aField : TField;
begin
  if savedialog.Execute then
  { find dbmemo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TdbMemo then begin
      adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=adbMemo then begin
          aField := FTable.Fields[fieldIdx];
          TMemoField(aField).SaveToFile(savedialog.FileName);
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnLoadGenericClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aMemo : TMemo;
  aField : TField;
begin
  if opendialog.Execute then
  { find memo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TMemo then begin
      aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aMemo then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          TBlobField(aField).LoadFromFile(opendialog.FileName);
          ViewActiveBlobField;
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnSaveGenericClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aMemo : TMemo;
  aField : TField;
begin
  if savedialog.Execute then
  { find memo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TMemo then begin
      aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aMemo then begin
          aField := FTable.Fields[fieldIdx];
          TBlobField(aField).SaveToFile(savedialog.FileName);
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnClearMemoClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  adbMemo : TdbMemo;
  aField : TField;
begin
  { find dbmemo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TdbMemo then begin
      adbMemo := TdbMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=adbMemo then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          aField.Clear;
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnLoadGraphicClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aImage : TImage;
  aField : TField;
begin
  if opendialog.Execute then
  { find Image control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TImage then begin
      aImage := TImage(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aImage then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          TBlobField(aField).LoadFromFile(opendialog.FileName);
          ViewActiveBlobField;
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnSaveGraphicClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aImage : TImage;
  aField : TField;
begin
  if savedialog.Execute then
  { find Image control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TImage then begin
      aImage := TImage(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aImage then begin
          aField := FTable.Fields[fieldIdx];
          TBlobField(aField).SaveToFile(savedialog.FileName);
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnClearGraphicClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aImage : TImage;
  aField : TField;
begin
  { find image control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TImage then begin
      aImage := TImage(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aImage then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          TGraphicField(aField).Clear;
          ViewActiveBlobField;
          Exit;
        end;
      end;
    end;
end;
{--------}
procedure TdlgTable.btnClearGenericClick(Sender: TObject);
var
  fieldIdx,
  controlIdx  : Integer;
  aMemo : TMemo;
  aField : TField;
begin
  { find memo control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TMemo then begin
      aMemo := TMemo(TButton(Sender).Parent.Controls[controlIdx]);
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aMemo then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
          aField.Clear;
          ViewActiveBlobField;
          Exit;
        end;
      end;
    end;
end;

procedure TdlgTable.meByteArrayKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key IN [#8, #9, #27, '0'..'9', 'A'..'F', 'a'..'f']) then
    Key := #0
  else
    BAKeyPressDetected := True;
end;

procedure TdlgTable.mnuTablePrintPreviewClick(Sender: TObject);
var
  Filter,
  DatabaseName,
  IndexName : Array[0..1024] of Char;
  i : Integer;
  RangeStart,
  RangeEnd : TRangeFieldValues;
begin
  StrPCopy(DatabaseName, FDatabaseName);
  if FTable.Filtered then begin
    StrPCopy(Filter, FTable.Filter);
  end
  else
    StrCopy(Filter, '');
  StrPCopy(IndexName, FTable.IndexName);
  { initialize }
  for i := 0 to ffcl_MaxIndexFlds-1 do begin
    RangeStart[i] := NULL;
    RangeEnd[i] := NULL;
  end;
  if InRange then begin
    FTable.EditRangeStart;
    for i := 0 to FTable.IndexFieldCount-1 do
      RangeStart[i] := FTable.IndexFields[i].Value;
    FTable.Cancel;
    FTable.EditRangeEnd;
    for i := 0 to FTable.IndexFieldCount-1 do
      RangeEnd[i] := FTable.IndexFields[i].Value;
    FTable.Cancel;
  end;
  SingleTableReport(FProtocol,
                    FServerName,
                    FUserName,
                    FPassword,
                    DatabaseName,
                    FTableName,
                    Filter,
                    IndexName,
                    RangeStart,
                    RangeEnd);
end;

procedure TdlgTable.mnuTableDesignReportClick(Sender: TObject);
var
  DatabaseName : Array[0..1024] of Char;
begin
  StrPCopy(DatabaseName, FDatabaseName);
  DesignReport(FProtocol,
               FServerName,
               FUserName,
               FPassword,
               DatabaseName);
end;


{ magic resize numbers: 100 = width of buttons + 8 pixels of space on each side }
procedure TdlgTable.tsMemoTemplateResize(Sender: TObject);
var
  controlIdx  : Integer;
begin
  for controlIdx := 0 to TTabSheet(Sender).ControlCount-1 do
    if TTabSheet(Sender).Controls[controlIdx] IS TdbMemo then begin
      TdbMemo(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width-116, TTabSheet(Sender).Height);
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TButton then begin
      TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-108;
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TCheckBox then begin
      TCheckBox(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-108;
    end;
end;

procedure TdlgTable.tsGraphicTemplateResize(Sender: TObject);
var
  controlIdx  : Integer;
begin
  for controlIdx := 0 to TTabSheet(Sender).ControlCount-1 do
    if TTabSheet(Sender).Controls[controlIdx] IS TImage then begin
      TImage(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width-116, TTabSheet(Sender).Height);
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TButton then begin
      TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-108;
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TCheckBox then begin
      TCheckBox(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-108;
    end;
end;

procedure TdlgTable.tsGenericBlobTemplateResize(Sender: TObject);
var
  controlIdx  : Integer;
begin
  for controlIdx := 0 to TTabSheet(Sender).ControlCount-1 do
    if TTabSheet(Sender).Controls[controlIdx] IS TMemo then begin
      TMemo(TTabSheet(Sender).Controls[controlIdx]).SetBounds(0, 0, TTabSheet(Sender).Width-116, TTabSheet(Sender).Height);
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TButton then begin
      TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-108;
    end;
end;

procedure TdlgTable.tsByteArrayTemplateResize(Sender: TObject);
var
  controlIdx  : Integer;
begin
  for controlIdx := 0 to TTabSheet(Sender).ControlCount-1 do
    if TTabSheet(Sender).Controls[controlIdx] IS TMaskEdit then begin
      TMaskEdit(TTabSheet(Sender).Controls[controlIdx]).Width := TTabSheet(Sender).Width-2*TMaskEdit(TTabSheet(Sender).Controls[controlIdx]).Left;
    end
    else
    if TTabSheet(Sender).Controls[controlIdx] IS TLabel then begin
      TLabel(TTabSheet(Sender).Controls[controlIdx]).Width := TTabSheet(Sender).Width-2*TLabel(TTabSheet(Sender).Controls[controlIdx]).Left;
    end
    else
    { this button is 75 pixels wide }
    if TTabSheet(Sender).Controls[controlIdx] IS TButton then begin
      if TButton(TTabSheet(Sender).Controls[controlIdx]).Caption='Clear' then
        TButton(TTabSheet(Sender).Controls[controlIdx]).Left := TTabSheet(Sender).Width-83;
    end;
end;

procedure TdlgTable.btnSetClearRangeClick(Sender: TObject);
var
  NeedEdit : Boolean;
  FieldIdx : Integer;
begin
  if not InRange then begin
    { check wether we have a useable range (not all NULL) }
    NeedEdit := True;
    for FieldIdx := Low(FRangeValues.Field) to FTable.IndexFieldCount do
      if (not FRangeValues.Field[FieldIdx].StartNull) or
         (not FRangeValues.Field[FieldIdx].EndNull)   then begin
        NeedEdit := False;
        Break;
      end;
    if NeedEdit then
      btnEditRangeClick(Self)
    else
      SetRange;
  end
  else begin
    btnSetClearRange.Caption := 'Set &Range';
    FTable.CancelRange;
    InRange := False;
    UpdateDisplay;
  end;
end;

procedure TdlgTable.btnEditRangeClick(Sender: TObject);
var
  BaseSection : string;
  Index : Integer;
begin
  if SetRangeDlg(FTable, FRangeValues)=mrOK then begin
    SetRange;
    GenerateRangeDisplayStrings;
    BaseSection := ClassName + '.' + Self.Caption;
    for Index := Low(FRangeValues.Field) to FTable.IndexFieldCount do begin
      FFEConfigSaveBoolean(BaseSection, FTable.IndexName+'_RangeStartNull'+IntToStr(Index), FRangeValues.Field[Index].StartNull);
      FFEConfigSaveBoolean(BaseSection, FTable.IndexName+'_RangeEndNull'+IntToStr(Index), FRangeValues.Field[Index].EndNull);
      FFEConfigSaveString(BaseSection, FTable.IndexName+'_RangeStartValue'+IntToStr(Index), FRangeValues.Field[Index].StartValue);
      FFEConfigSaveString(BaseSection, FTable.IndexName+'_RangeEndValue'+IntToStr(Index), FRangeValues.Field[Index].EndValue);
    end;
    FFEConfigSaveBoolean(BaseSection, FTable.IndexName+'_RangeStartKeyExclusive', FRangeValues.RangeStartKeyExclusive);
    FFEConfigSaveBoolean(BaseSection, FTable.IndexName+'_RangeEndKeyExclusive', FRangeValues.RangeEndKeyExclusive);
  end;
end;

procedure TdlgTable.mnuViewShowRangeClick(Sender: TObject);
var
  FilterFix : Boolean;
begin
  { necessary to get rangepanel to reappear below filterpanel }
  FilterFix := pnlFilter.Visible and not pnlRange.Visible;
  if FilterFix then
    pnlFilter.Visible := False;
  mnuViewShowRange.Checked := not mnuViewShowRange.Checked;
  pnlRange.Visible := mnuViewShowRange.Checked;
  { remove range and update display etc }
  if InRange then
    btnSetClearRangeClick(Self);
  if FilterFix then
    pnlFilter.Visible := True;
end;

procedure TdlgTable.FormResize(Sender: TObject);
begin
  btnFindNear.Left := ClientWidth - btnFindNear.Width - 8;
  edtFind.Width := btnFindNear.Left - edtFind.Left - 8;
  btnSetFilter.Left := ClientWidth - btnSetFilter.Width - 8;
  cboFilter.Width := btnSetFilter.Left - cboFilter.Left - 8;
  btnSetClearRange.Left := ClientWidth - btnSetClearRange.Width - 8;
  laRangeStart.Width := btnSetClearRange.Left - laRangeStart.Left - 8;
  btnEditRange.Left := ClientWidth - btnEditRange.Width - 8;
  laRangeEnd.Width := btnEditRange.Left - laRangeEnd.Left - 8;
end;

procedure TdlgTable.GenerateRangeDisplayStrings;
var
  HighestNonNullIdx,
  FieldIdx : Integer;
  FirstField : Boolean;
begin
  HighestNonNullIdx := FTable.IndexFieldCount;
  while (HighestNonNullIdx>1) and
         FRangeValues.Field[HighestNonNullIdx].StartNull and
         FRangeValues.Field[HighestNonNullIdx].EndNull do
    Dec(HighestNonNullIdx);
  laRangeStart.Caption := '[';
  FirstField := True;
  for FieldIdx := Low(FRangeValues.Field) to HighestNonNullIdx do begin
    if not FirstField then laRangeStart.Caption := laRangeStart.Caption + ', ';
    if FRangeValues.Field[FieldIdx].StartNull then
      laRangeStart.Caption := laRangeStart.Caption + 'NULL'
    else
    if FRangeValues.Field[FieldIdx].StartValue<>'' then
      laRangeStart.Caption := laRangeStart.Caption + FRangeValues.Field[FieldIdx].StartValue
    else
      laRangeStart.Caption := laRangeStart.Caption + '''''';
    FirstField := False;
  end;
  laRangeStart.Caption := laRangeStart.Caption + ']';
  if FRangeValues.RangeStartKeyExclusive then
    laRangeStart.Caption := laRangeStart.Caption + ' - [KeyExclusive]';
  laRangeEnd.Caption := '[';
  FirstField := True;
  for FieldIdx := Low(FRangeValues.Field) to HighestNonNullIdx do begin
    if not FirstField then laRangeEnd.Caption := laRangeEnd.Caption + ', ';
    if FRangeValues.Field[FieldIdx].EndNull then
      laRangeEnd.Caption := laRangeEnd.Caption + 'NULL'
    else
    if FRangeValues.Field[FieldIdx].EndValue<>'' then
      laRangeEnd.Caption := laRangeEnd.Caption + FRangeValues.Field[FieldIdx].EndValue
    else
      laRangeEnd.Caption := laRangeEnd.Caption + '''''';
    FirstField := False;
  end;
  laRangeEnd.Caption := laRangeEnd.Caption + ']';
  if FRangeValues.RangeEndKeyExclusive then
    laRangeEnd.Caption := laRangeEnd.Caption + ' - [KeyExclusive]';
end;

procedure TdlgTable.SetRange;
var
  HighestNonNullIdx,
  FieldIdx : Integer;
begin
  HighestNonNullIdx := 0;
  FTable.SetRangeStart;
  FTable.KeyExclusive := FRangeValues.RangeStartKeyExclusive;
  for FieldIdx := Low(FRangeValues.Field) to FTable.IndexFieldCount do begin
    if not FRangeValues.Field[FieldIdx].StartNull then begin
      FTable.IndexFields[FieldIdx-1].AsString := FRangeValues.Field[FieldIdx].StartValue;
      HighestNonNullIdx := FFMaxL(HighestNonNullIdx, FieldIdx);
    end
    else
    if not FRangeValues.Field[FieldIdx].EndNull then
      FTable.IndexFields[FieldIdx-1].Value := NULL;
  end;
  FTable.SetRangeEnd;
  FTable.KeyExclusive := FRangeValues.RangeEndKeyExclusive;
  for FieldIdx := Low(FRangeValues.Field) to FTable.IndexFieldCount do begin
    if not FRangeValues.Field[FieldIdx].EndNull then begin
      FTable.IndexFields[FieldIdx-1].AsString := FRangeValues.Field[FieldIdx].EndValue;
      HighestNonNullIdx := FFMaxL(HighestNonNullIdx, FieldIdx);
    end
    else
    if not FRangeValues.Field[FieldIdx].StartNull then
      FTable.IndexFields[FieldIdx-1].Value := NULL;
  end;
  FTable.KeyFieldCount := HighestNonNullIdx;
  FTable.ApplyRange;
  InRange := True;
  btnSetClearRange.Caption := 'Clear &Range';
  UpdateDisplay;
end;

procedure TdlgTable.meByteArrayChange(Sender: TObject);
var
  ByteArrayBuffer : Pointer;
  fieldIdx,
  controlIdx  : Integer;
  aEdit : TMaskEdit;
  aField : TField;

  procedure HexStringToByteArray(ByteArray : Pointer; ArrayLength : Integer; S : String);
  var
    idx : Integer;
    BArr : PffByteArray absolute ByteArray;
  begin
    for idx := 0 to ArrayLength-1 do begin
      if Odd(Length(S)) then
        S := S + '0';
      if Length(S)>1 then begin
        try
          BArr[idx] := StrToInt('$'+Copy(S, 1, 2));
        except
          on EConvertError do begin
            MessageDlg('Invalid character encountered - use only hex digits 0..9, A..F!', mtError, [mbOK], 0);
            Abort;
          end;
        end;
        Delete(S, 1, 2);
      end
      else begin
        BArr[idx] := 0;
        BArr[idx] := 0;
      end;
    end;
  end;

begin
  if not BAKeyPressDetected then
    Exit
  else
    BAKeyPressDetected := False;
  FTable.Edit;
  { find edit control }
  for controlIdx := 0 to TButton(Sender).Parent.ControlCount-1 do
    if TButton(Sender).Parent.Controls[controlIdx] IS TMaskEdit then begin
      aEdit := TMaskEdit(TButton(Sender).Parent.Controls[controlIdx]);
      if aEdit.Text='' then
        Exit;
      { find correct field }
      for fieldIdx := 0 to FTable.Dictionary.FieldCount-1 do begin
        if Pointer(FTable.Fields[fieldIdx].Tag)=aEdit then begin
          aField := FTable.Fields[fieldIdx];
          if not (FTable.State in [dsInsert, dsEdit]) then
            FTable.Edit;
            GetMem(ByteArrayBuffer, aField.Size);
            try
              HexStringToByteArray(ByteArrayBuffer, aField.Size, aEdit.Text);
              aField.SetData(ByteArrayBuffer);
            finally
              FreeMem(ByteArrayBuffer);
            end;
            Exit;
        end;
      end;
    end;
end;

procedure TdlgTable.cbWordwrapClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to TCheckBox(Sender).Parent.ControlCount-1 do
    if TCheckBox(Sender).Parent.Controls[i] IS TdbMemo then begin
      with TdbMemo(TCheckBox(Sender).Parent.Controls[i]) do begin
        WordWrap := TCheckBox(Sender).Checked;
        if WordWrap then
          ScrollBars := ssVertical
        else
          ScrollBars := ssBoth;
      end;
      Exit;
    end;
end;

procedure TdlgTable.mnuTableCopyToTableClick(Sender: TObject);
var
  ExcludeIndex,
  TableIndex: LongInt;
  CopyBlobs : Boolean;
  SaveTimeout : Integer;
begin
  ExcludeIndex := TableItem.Database.IndexOf(TableItem);
  if ShowCopyTableDlg(TableItem.Database, ExcludeIndex, FTable,
                      TableIndex, CopyBlobs, FTableItem) = mrOK then begin {!!.11}
    with TableItem.Database.Tables[TableIndex] do begin
      Screen.Cursor := crHourGlass;
      { the copy operation is used in the context of the table
        that's being copied to. Use the timeout of the active
        table, otherwise the user has no way of setting timeout. }
      SaveTimeout := Table.Timeout;
      Table.Timeout := FTable.Timeout;
      try
        Update;
        CopyRecords(FTable, CopyBlobs);
      finally
        Screen.Cursor := crDefault;
        Table.Timeout := SaveTimeout;
        { force the second table to close if it wasn't open before }
        FSession.CloseInactiveTables;                              {!!.11}                                                      
      end;
    end;
  end;
end;

procedure TdlgTable.mnuTableDeleteRecordsClick(Sender: TObject);
begin
  if MessageDlg('Delete all records matching the current filter and range - are you sure?', mtConfirmation, [mbYes,mbNo], 0)= mrYes then begin
    Screen.Cursor := crHourGlass;
    try
      Update;
      FTable.DeleteRecords;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;
{End !!.07}

procedure TdlgTable.UpdateDefaultTimeout;
begin
  FClient.TimeOut := Config.DefaultTimeout;                        {!!.11}
end;

end.

