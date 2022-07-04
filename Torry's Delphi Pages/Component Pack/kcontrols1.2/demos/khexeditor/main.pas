unit Main;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ComCtrls, StdCtrls, Menus, ToolWin, ExtCtrls, ImgList,
  KEditCommon, KHexEditor, KControls, KDialogs;


type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MGFile: TMenuItem;
    MINew: TMenuItem;
    MIOpen: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MIClose: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    StatusBar: TStatusBar;
    ALFile: TActionList;
    ACNew: TAction;
    ACOpen: TAction;
    ACSave: TAction;
    ACSaveAs: TAction;
    ACClose: TAction;
    ACExit: TAction;
    ODMain: TOpenDialog;
    SDMain: TSaveDialog;
    ACStatusBar: TAction;
    ALEdit: TActionList;
    ACUndo: TAction;
    ACRedo: TAction;
    ACCut: TAction;
    ACCopy: TAction;
    ACPaste: TAction;
    ACDelete: TAction;
    ACSelectAll: TAction;
    MGEdit: TMenuItem;
    MIUndo: TMenuItem;
    MIRedo: TMenuItem;
    N2: TMenuItem;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MIPaste: TMenuItem;
    MIDelete: TMenuItem;
    N3: TMenuItem;
    MISelectAll: TMenuItem;
    MGSearch: TMenuItem;
    MGOptions: TMenuItem;
    MISearch: TMenuItem;
    MIReplace: TMenuItem;
    MISearchAgain: TMenuItem;
    MIOptions: TMenuItem;
    MGHelp: TMenuItem;
    MIAbout: TMenuItem;
    ALSearch: TActionList;
    ACSearch: TAction;
    ACReplace: TAction;
    ACSearchAgain: TAction;
    ACEditorOptions: TAction;
    MIMRUFEnd: TMenuItem;
    MIMRUF1: TMenuItem;
    MIMRUF2: TMenuItem;
    MIMRUF3: TMenuItem;
    MIMRUF4: TMenuItem;
    MIMRUF5: TMenuItem;
    PMMRUFs: TPopupMenu;
    PMIMRUF1: TMenuItem;
    PMIMRUF2: TMenuItem;
    PMIMRUF3: TMenuItem;
    PMIMRUF4: TMenuItem;
    PMIMRUF5: TMenuItem;
    MIPrint: TMenuItem;
    PMMain: TPopupMenu;
    PMIUndo: TMenuItem;
    PMIRedo: TMenuItem;
    N6: TMenuItem;
    PMICut: TMenuItem;
    PMICopy: TMenuItem;
    PMIPaste: TMenuItem;
    PMIDelete: TMenuItem;
    PMISelectAll: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    ACPrint: TAction;
    ACViewToolBar: TAction;
    MIViewToolBar: TMenuItem;
    PNMain: TPanel;
    N11: TMenuItem;
    MIMRUFStart: TMenuItem;
    PMIEnvProps: TMenuItem;
    ACEnabledEditor: TAction;
    ACReadOnlyEditor: TAction;
    EnabledEditor1: TMenuItem;
    Close1: TMenuItem;
    ILMain: TImageList;
    Editor: TKHexEditor;
    ACPreview: TAction;
    MIPreview: TMenuItem;
    PSDMain: TKPrintSetupDialog;
    PPDMain: TKPrintPreviewDialog;
    ToBMain: TToolBar;
    TBNew: TToolButton;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    TBPreview: TToolButton;
    TBPrint: TToolButton;
    TBSep1: TToolButton;
    TBCut: TToolButton;
    TBCopy: TToolButton;
    TBPaste: TToolButton;
    TBDelete: TToolButton;
    TBSep2: TToolButton;
    TBUndo: TToolButton;
    TBRedo: TToolButton;
    TBSep3: TToolButton;
    TBFind: TToolButton;
    TBReplace: TToolButton;
    TBSearchAgain: TToolButton;
    TBSep4: TToolButton;
    TBEnvProps: TToolButton;
    ILMainDis: TImageList;
    procedure EditorReplaceText(Sender: TObject; const TextToFind,
      TextToReplace: string; var Action: TKEditReplaceAction);
    procedure FormCreate(Sender: TObject);
    procedure ACNewExecute(Sender: TObject);
    procedure ACEnabledUpdate(Sender: TObject);
    procedure ACOpenExecute(Sender: TObject);
    procedure ACSaveExecute(Sender: TObject);
    procedure ACSaveUpdate(Sender: TObject);
    procedure ACSaveAsExecute(Sender: TObject);
    procedure ACCloseExecute(Sender: TObject);
    procedure ACExitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ACStatusBarUpdate(Sender: TObject);
    procedure ACUndoExecute(Sender: TObject);
    procedure ACUndoUpdate(Sender: TObject);
    procedure ACRedoExecute(Sender: TObject);
    procedure ACRedoUpdate(Sender: TObject);
    procedure ACCutExecute(Sender: TObject);
    procedure ACCutUpdate(Sender: TObject);
    procedure ACCopyExecute(Sender: TObject);
    procedure ACPasteExecute(Sender: TObject);
    procedure ACPasteUpdate(Sender: TObject);
    procedure ACDeleteExecute(Sender: TObject);
    procedure ACSelectAllExecute(Sender: TObject);
    procedure ACSelectAllUpdate(Sender: TObject);
    procedure ACSearchExecute(Sender: TObject);
    procedure ACSearchAgainExecute(Sender: TObject);
    procedure ACSearchAgainUpdate(Sender: TObject);
    procedure ACSearchUpdate(Sender: TObject);
    procedure ACReplaceExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACEditorOptionsExecute(Sender: TObject);
    procedure ACPrintExecute(Sender: TObject);
    procedure ACPrintUpdate(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure ACViewToolBarExecute(Sender: TObject);
    procedure ACSaveAsUpdate(Sender: TObject);
    procedure ACCloseUpdate(Sender: TObject);
    procedure ACCopyUpdate(Sender: TObject);
    procedure ACDeleteUpdate(Sender: TObject);
    procedure ACReplaceUpdate(Sender: TObject);
    procedure EditorDropFiles(Sender: TObject; X, Y: Integer;
      Files: TStrings);
    procedure ACEnabledEditorExecute(Sender: TObject);
    procedure ACEnabledEditorUpdate(Sender: TObject);
    procedure ACReadOnlyEditorExecute(Sender: TObject);
    procedure ACReadOnlyEditorUpdate(Sender: TObject);
    procedure ACViewToolBarUpdate(Sender: TObject);
    procedure EditorPrintNotify(Sender: TObject; Status: TKPrintStatus;
      var Abort: Boolean);
    procedure EditorPrintPaint(Sender: TObject);
    procedure ACPreviewExecute(Sender: TObject);
    procedure ACPreviewUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FMRUFList: TStringList;
    FLastFileName: string;
    IsNew: Boolean;
    SearchData: TKEditSearchData;
    SearchAgainEnabled: Boolean;
    LastSearchCommand: TKEditCommand;
    TextToFind, TextToReplace: string;
    procedure AddMRUF(const FileName: string);
    procedure DeleteMRUF(const FileName: string);
    procedure DeleteMRUFs;
    procedure MRUFClick(Sender: TObject);
    procedure UpdateMRUFs;
    procedure LoadFromIni;
    procedure SaveToIni;
    procedure EnvironmentChanged;
    procedure CloseFile;
    procedure NewFile;
    procedure OpenFile(FileName: string);
    function SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
    procedure Search(Command: TKEditCommand);
    procedure HandleSearchError;
  {$IFDEF FPC}
    procedure LazDropFiles(Sender: TObject; const FileNames: Array of String);
  {$ENDIF}
  end;

var
  MainForm: TMainForm;

implementation

uses
  IniFiles, Res, Basic, About, Options,
  Replace, ReplacePrompt, Search, PrintStatus, ClipBrd;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := AppName;
{$IFDEF FPC}
  Application.OnDropFiles := LazDropFiles;
{$ENDIF}
  Caption := AppName;
  FLastFileName := '';
  SDMain.Filter := ODMain.Filter;
  FMRUFList := TStringList.Create;
  InitEnvironment(Environment);
  InitColors(Colors);
  SearchData := DefaultSearchData;
  LoadFromIni;
  EnvironmentChanged;
  UpdateMRUFs;
  SearchAgainEnabled := False;
  if ParamCount > 0 then
    FLastFileName := ParamStr(1);
  if FLastFileName <> '' then
    OpenFile(FLastFileName)
  else
    NewFile;
  //Editor.AddressOffset := 16384;
  Editor.AreaSpacing := 1;
  Editor.DoubleBuffered := True;  
  //define 1 cm space for custom header
  Editor.PageSetup.HeaderSpace := 1; //cm by default
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveToIni;
  CloseFile;
  FMRUFList.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveFile(False, True);
end;

procedure TMainForm.ACNewExecute(Sender: TObject);
begin
  NewFile;
end;

procedure TMainForm.ACEnabledUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
  StatusBar.Panels[4].Text := Application.Hint;
end;

procedure TMainForm.ACOpenExecute(Sender: TObject);
begin
  OpenFile('');
end;

procedure TMainForm.ACSaveExecute(Sender: TObject);
begin
  SaveFile(False, False);
end;

procedure TMainForm.ACSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.Visible and Editor.Modified;
end;

procedure TMainForm.ACSaveAsExecute(Sender: TObject);
begin
  SaveFile(True, False);
end;

procedure TMainForm.ACSaveAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.Visible;
end;

procedure TMainForm.ACCloseExecute(Sender: TObject);
begin
  if SaveFile(False, True) then
    CloseFile;
end;

procedure TMainForm.ACCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.Visible;
end;

procedure TMainForm.ACPreviewExecute(Sender: TObject);
begin
  PPDMain.Show;
end;

procedure TMainForm.ACPreviewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TMainForm.ACPrintExecute(Sender: TObject);
begin
  Editor.PageSetup.Title := FLastFileName;
  PSDMain.SelAvail := Editor.SelAvail;
  PSDMain.Execute;
end;

procedure TMainForm.ACPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CanPrint;
end;

procedure TMainForm.ACExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ACStatusBarUpdate(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  with StatusBar do
  begin
    if Editor.Visible then
    begin
      if Editor.AddressMode = eamDec then S := sStatusPosDec else S := sStatusPosHex;
      Panels[0].Text := Format(S, [Editor.SelEnd.Index]);
      Panels[1].Text := Format(sStatusDigit, [Editor.SelEnd.Digit]);
      Panels[2].Text := Modified2Text(Editor.Modified);
      Panels[3].Text := InsertMode2Text(Editor.InsertMode);
    end else
      for I := 0 to 3 do Panels[I].Text := '';
  end;
end;

procedure TMainForm.ACUndoExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecUndo);
end;

procedure TMainForm.ACUndoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecUndo);
end;

procedure TMainForm.ACRedoExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecRedo);
end;

procedure TMainForm.ACRedoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecRedo);
end;

procedure TMainForm.ACCutExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecCut);
end;

procedure TMainForm.ACCutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecCut);
end;

procedure TMainForm.ACCopyExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecCopy);
end;

procedure TMainForm.ACCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecCopy);
end;

procedure TMainForm.ACPasteExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecPaste);
end;

procedure TMainForm.ACPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecPaste);
end;

procedure TMainForm.ACDeleteExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecClearSelection);
end;

procedure TMainForm.ACDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecClearSelection);
end;

procedure TMainForm.ACSelectAllExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(ecSelectAll);
end;

procedure TMainForm.ACSelectAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecSelectAll);
end;

procedure TMainForm.ACSearchExecute(Sender: TObject);
begin
  Search(ecSearch);
end;

procedure TMainForm.ACSearchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecSearch);
end;

procedure TMainForm.ACReplaceExecute(Sender: TObject);
begin
  Search(ecReplace);
end;

procedure TMainForm.ACReplaceUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.CommandEnabled(ecReplace);
end;

procedure TMainForm.ACSearchAgainExecute(Sender: TObject);
begin
  Editor.ExecuteCommand(LastSearchCommand, @SearchData);
  HandleSearchError;
end;

procedure TMainForm.ACSearchAgainUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := SearchAgainEnabled;
end;

procedure TMainForm.ACEditorOptionsExecute(Sender: TObject);
begin
  OptionsForm.SetData(Environment, Colors);
  if OptionsForm.ShowModal = mrOk then
  begin
    OptionsForm.GetData(Environment, Colors);
    EnvironmentChanged;
  end;
end;

procedure TMainForm.ACViewToolBarExecute(Sender: TObject);
begin
  ACViewToolBar.Checked := not ACViewToolBar.Checked;
  ToBMain.Visible := ACViewToolBar.Checked;
end;

procedure TMainForm.ACViewToolBarUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ToBMain.Visible;
end;

procedure TMainForm.ACEnabledEditorExecute(Sender: TObject);
begin
  ACEnabledEditor.Checked := not ACEnabledEditor.Checked;
  Editor.Enabled := ACEnabledEditor.Checked;
end;

procedure TMainForm.ACEnabledEditorUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := Editor.Enabled;
end;

procedure TMainForm.ACReadOnlyEditorExecute(Sender: TObject);
begin
  ACReadOnlyEditor.Checked := not ACReadOnlyEditor.Checked;
  Editor.ReadOnly := ACReadOnlyEditor.Checked;
end;

procedure TMainForm.ACReadOnlyEditorUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := Editor.ReadOnly;
end;

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.AddMRUF(const FileName: string);
begin
  if FileName <> '' then
  try
    if FMRUFList.IndexOf(FileName) < 0 then
      FMRUFList.Insert(0, FileName);
    UpdateMRUFs;
  except
  end;
end;

procedure TMainForm.DeleteMRUF(const FileName: string);
var
  I: Integer;
begin
  if FileName <> '' then
  try
    I := FMRUFList.IndexOf(FileName);
    if I >= 0 then FMRUFList.Delete(I);
    UpdateMRUFs;
  except
  end;
end;

procedure TMainForm.DeleteMRUFs;
begin
  try
    FMRUFList.Clear;
    UpdateMRUFs;
  except
  end;
end;

procedure TMainForm.MRUFClick(Sender: TObject);
var
  S: string;
begin
  S := TMenuItem(Sender).Caption;
  Delete(S, 1, 3);
  if S <> '' then
    OpenFile(S);
end;

procedure TMainForm.UpdateMRUFs;
var
  I, Index: Integer;
  MI, PMI: TMenuItem;
  S: string;
  B: Boolean;
begin
  B := False;
  Index := MGFile.IndexOf(MIMRUF1);
  for I := 0 to 4 do
  begin
    MI := MGFile.Items[I + Index];
    PMI := PMMRUFs.Items[I];
    if I < FMRUFList.Count then
    begin
      S := Format('&%d %s', [I + 1, FMRUFList[I]]);
      MI.Visible := True;
      PMI.Visible := True;
      MI.Caption := S;
      PMI.Caption := S;
      MI.OnClick := MRUFClick;
      PMI.OnClick := MRUFClick;
      B := True;
    end else
    begin
      MI.Visible := False;
      PMI.Visible := False;
    end;
  end;
  MIMRUFEnd.Visible := B;
end;

procedure TMainForm.CloseFile;
begin
  if Editor.Visible then
  begin
    Editor.Clear;
    AddMRUF(FLastFileName);
    FLastFileName := '';
    Editor.Visible := False;
  end;  
end;

procedure TMainForm.NewFile;
begin
  if SaveFile(False, True) then
  begin
    CloseFile;
    IsNew := True;
    Caption := Format('%s - %s', [AppName, sNoname]);
    Editor.Visible := True;
  end;
end;

procedure TMainForm.OpenFile(FileName: string);
begin
  if SaveFile(False, True) then
  begin
    if FileName = '' then
      if ODMain.Execute then
        FileName := ODMain.FileName;
    if FileName <> '' then
    begin
      CloseFile;
      if ExtractFileDir(FileName) = '' then
        FileName := Format('%s\%s', [GetCurrentDir, FileName]);
      try
        Editor.LoadFromFile(FileName);
        FLastFileName := FileName;
        IsNew := False;
        Caption := Format('%s - %s', [AppName, FLastFileName]);
        Editor.Visible := True;
      except
        MessageBox(Handle, PChar(Format(sErrOpenError, [FileName])), PChar(sAppName), MB_OK);
      end;
      DeleteMRUF(FileName);
    end;
  end;
end;

function TMainForm.SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
var
  NeedDlg: Boolean;
  FileName: string;
begin
  Result := False;
  if IsNew then
    FileName := sNoname
  else
    FileName := ExtractFileName(FLastFileName);
  if NeedAnotherOp then
  begin
    if Editor.Modified then
    begin
      case MessageBox(Handle, PChar(Format(sNeedSaveQuestion, [FileName])), PChar(sAppName), MB_YESNOCANCEL) of
        ID_NO: begin Result := True; Exit end;
        ID_CANCEL: Exit;
      end
    end else
    begin
      Result := True;
      Exit;
    end;
  end;
  NeedDlg := IsNew or SaveAs;
  SDMain.FileName := FileName;
  if not NeedDlg or SDMain.Execute then
  begin
    if NeedDlg then FLastFileName := SDMain.Filename;
    try
      Editor.SaveToFile(FLastFileName);
      Editor.Modified := False;
      IsNew := False;
      Caption := Format('%s - %s', [AppName, FLastFileName]);
      Result := True;
    except
      MessageBox(Handle, PChar(Format(sErrSaveError, [FileName])), PChar(sAppName), MB_OK);
    end;
  end;
  if Result then Editor.Modified := False;
end;

procedure TMainForm.LoadFromIni;
var
  Ini: TIniFile;
  I: Integer;
  S: string;
begin
  Ini := TIniFile.Create(IniPath);
  try
    I := Ini.ReadInteger(secSettings, 'Version', 0);
    if I = IniVersion then with Environment do
    begin
      DataToString(@P, SizeOf(TEnvironmentPacked), S);
      S := Ini.ReadString(secSettings, 'Environment', S);
      StringToData(S, @P, SizeOf(TEnvironmentPacked));
      AddressPrefix := Ini.ReadString(secSettings, 'Address prefix', AddressPrefix);
      FontName := Ini.ReadString(secSettings, 'Font name', FontName);
    end;
    FLastFileName := Ini.ReadString(secSettings, 'Last file name', FLastFileName);
    WindowState := TWindowState(Ini.ReadInteger(secSettings, 'Window state', Integer(WindowState)));
    BoundsRect := Rect(
      Ini.ReadInteger(secSettings, 'Left', Left),
      Ini.ReadInteger(secSettings, 'Top', Top),
      Ini.ReadInteger(secSettings, 'Right', Left + Width),
      Ini.ReadInteger(secSettings, 'Bottom', Top + Height));
    FMRUFList.Clear;
    for I := 1 to 5 do
    begin
      S := Ini.ReadString(secMRUFs, Format('MRUF%d', [I]), '');
      if S <> '' then FMRUFList.Add(S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.SaveToIni;
var
  Ini: TIniFile;
  I: Integer;
  S: string;
begin
  Ini := TIniFile.Create(IniPath);
  try
    Ini.WriteInteger(secSettings, 'Version', IniVersion);
    with Environment do
    begin
      DataToString(@P, SizeOf(TEnvironmentPacked), S);
      Ini.WriteString(secSettings, 'Environment', S);
      Ini.WriteString(secSettings, 'Address prefix', AddressPrefix);
      Ini.WriteString(secSettings, 'Font name', FontName);
    end;
    Ini.WriteString(secSettings, 'Last file name', FLastFileName);
    Ini.WriteInteger(secSettings, 'Window state', Integer(WindowState));
    Ini.WriteInteger(secSettings, 'Left', Left);
    Ini.WriteInteger(secSettings, 'Top', Top);
    Ini.WriteInteger(secSettings, 'Right', Left + Width);
    Ini.WriteInteger(secSettings, 'Bottom', Top + Height);
    for I := 0 to FMRUFList.Count - 1 do
      Ini.WriteString(secMRUFs, Format('MRUF%d', [I + 1]), FMRUFList[I]);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.EnvironmentChanged;
var
  Options: TKEditOptions;
  DrawStyles: TKHexEditorDrawStyles;
begin
  with Environment do
  begin
    Options := [];
    if P.DropFiles then Include(Options, eoDropFiles);
    if P.GroupUndo then Include(Options, eoGroupUndo);
    if P.UndoAfterSave then Include(Options, eoUndoAfterSave);
    Editor.Options := Options;
    DrawStyles := [];
    if P.ShowAddress then Include(DrawStyles, edAddress);
    if P.ShowDigits then Include(DrawStyles, edDigits);
    if P.ShowText then Include(DrawStyles, edText);
    if P.ShowHorzLines then Include(DrawStyles, edHorzLines);
    if P.ShowVertLines then Include(DrawStyles, edVertLines);
    if P.ShowSeparators then Include(DrawStyles, edSeparators);
    if P.ShowInactiveCaret then Include(DrawStyles, edInactiveCaret);
    Editor.DrawStyles := DrawStyles;
    Editor.AddressMode := TKHexEditorAddressMode(P.AddressMode);
    Editor.AddressPrefix := AddressPrefix;
    Editor.AddressSize := P.AddressSize;
    Editor.CharSpacing := P.CharSpacing;
    Editor.DigitGrouping := P.DigitGrouping;
    Editor.DisabledDrawStyle := TKHexEditorDisabledDrawStyle(P.DisabledDrawStyle);
    Editor.LineHeightPercent := P.LineHeightPercent;
    Editor.LineSize := P.LineSize;
    Editor.UndoLimit := P.UndoLimit;
    // font
    Editor.Font.Name := FontName;
    Editor.Font.Size := P.FontSize;
    Editor.Font.Style := P.FontStyle;
    // colors
    Editor.Colors.Colors := Colors;
  end;
end;

procedure TMainForm.Search(Command: TKEditCommand);
var
  Form: TSearchForm;
  MResult: TModalResult;
begin
  if Command = ecReplace then
    Form := ReplaceForm
  else
    Form := SearchForm;
  if Editor.EditArea = eaDigits then
    Include(SearchData.Options, esoTreatAsDigits)
  else
    Exclude(SearchData.Options, esoTreatAsDigits);
  Form.SetData(SearchData, Editor.SelAvail);
  MResult := Form.ShowModal;
  if MResult in [mrYes, mrYesToAll] then
  begin
    Form.GetData(SearchData);
    SearchAgainEnabled := True;
    LastSearchCommand := Command;
    Include(SearchData.Options, esoFirstSearch);
    TextToFind := TrimToSize(SearchData.TextToFind, 40);
    TextToReplace := TrimToSize(SearchData.TextToReplace, 40);
    ReplacePromptForm.LBText.Caption := Format(sReplaceQuestion, [TextToFind]);
    if MResult = mrYesToAll then
      Include(SearchData.Options, esoAll);
    Editor.ExecuteCommand(Command, @SearchData);
    HandleSearchError;
  end;
end;

procedure TMainForm.EditorReplaceText(Sender: TObject; const TextToFind,
  TextToReplace: String; var Action: TKEditReplaceAction);
begin
  case ReplacePromptForm.ShowModal of
    mrYes: Action := eraYes;
    mrNo: Action := eraNo;
    mrYesToAll: Action := eraAll;
    mrCancel: Action := eraCancel;
  end;
end;

procedure TMainForm.HandleSearchError;
var
  S: string;
begin
  case SearchData.ErrorReason of
    eseNoMatch: S := Format(sErrTextNotFound, [TextToFind]);
    eseNoDigitsFind: S := Format(sErrNoDigitsInText, [TextToFind]);
    eseNoDigitsReplace: S := Format(sErrNoDigitsInText, [TextToReplace]);
  else
    S := '';
  end;
  if S <> '' then
    MessageBox(Handle, PChar(S), PChar(sAppName), MB_OK);
end;

{$IFDEF FPC}
procedure TMainForm.LazDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if Length(FileNames) > 0 then
  begin
    Application.BringToFront;
    OpenFile(FileNames[0]);
  end;
end;
{$ENDIF}

procedure TMainForm.EditorDropFiles(Sender: TObject; X, Y: Integer;
  Files: TStrings);
begin
  //not called in Lazarus but keep assigned in this demo
  if Files.Count > 0 then
  begin
    Application.BringToFront;
    OpenFile(Files[0]);
  end;
end;

procedure TMainForm.EditorPrintNotify(Sender: TObject;
  Status: TKPrintStatus; var Abort: Boolean);
begin
  with PrintStatusForm do
  begin
    case Status of
      epsBegin:
      begin
        Self.Enabled := False;
        Aborted := False;
        Show;
      end;
      epsNewPage:
      begin
        LBPage.Caption := Format(sPrintedPageAndCopy,
          [Editor.PageSetup.CurrentPage, Editor.PageSetup.PageCount,
           Editor.PageSetup.CurrentCopy, Editor.PageSetup.Copies]);
        Update;
      end;
      epsEnd:
      begin
         Hide;
         Self.Enabled := True;
      end;
    end;
    Abort := Aborted;
  end;
end;

procedure TMainForm.EditorPrintPaint(Sender: TObject);
var
  Y: Integer;
begin
  with Editor.PageSetup do
  begin
    // paint filename as header - we have 1 cm space defined
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Pitch := fpDefault;
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Height := VMap(16);
    Canvas.Font.Style := [fsBold];
    Canvas.Font.Color := clBlack;
    Canvas.TextOut(PrinterMarginLeftMirrored, PrinterMarginTop, FLastFileName);
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := VMap(2);
    Y := PrinterMarginTop + VMap(22);
    Canvas.MoveTo(PrinterMarginLeftMirrored, Y);
    Canvas.LineTo(PageWidth - PrinterMarginRightMirrored, Y);
  end;
end;

{$IFDEF FPC}
initialization
  {$i main.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
