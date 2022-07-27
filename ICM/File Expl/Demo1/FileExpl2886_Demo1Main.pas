{=============================================================================}
{  LsFileExplorer28 Demo1 (version 2.88?)                                     }
{=============================================================================}
{
This Demo was compiled by Delphi 7.  DFM files (ie. FileExpl287?_Demo1Main.dfm,
About.dfm and FileDate.dfm) are saved in binary format, instead of its default
TextFormat,  thus they will be bachwards compatible with previous versions of
Delphi.

If you want to re-compile it by Delphi 6, 5 or 4, it's necessary to load all
files (including FileExpl287?_Demo1.dpr, FileExpl287?_Demo1Main.pas, About.pas
and FileDate.pas) into Delphi's IDE befor rebuilding.  However, under D4,
you'll encounter error messages, because some properties on MainForm, AboutForm
or FileDateForm may be not available in D4.  Under such case, just ignore all
error messages, then proceed as usual.

Binary DFM files created by D5, D6 or D7 can be converted to default text format
by using 'textDFM' option in D5/D6/D7's context menu.

NOTE:- Where '?' represents the build number of LsFileExplorer28.

{=============================================================================}


unit FileExpl2886_Demo1Main;

interface
{$INCLUDE LSCOMP.INC}
{$IFDEF D6_OR_HIGHER}
  {$WARNINGS OFF}
//  {$WARN UNIT_PLATFORM OFF}
//  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, LsFileExplorer28, ToolWin, Menus, FileCtrl,
  Buttons, Clipbrd, ImgList;


type

  TMainForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    QuickView1: TMenuItem;
    N1: TMenuItem;
    DeleteFiles1: TMenuItem;
    RenameFile1: TMenuItem;
    N2: TMenuItem;
    NewFolder1: TMenuItem;
    DeleteFolder1: TMenuItem;
    RenameFolder1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    View1: TMenuItem;
    LargeIcon1: TMenuItem;
    SmallIcon1: TMenuItem;
    List1: TMenuItem;
    Detail1: TMenuItem;
    N6: TMenuItem;
    Refresh1: TMenuItem;
    Network1: TMenuItem;
    ConnecttoNetwork1: TMenuItem;
    DisconnectFromNetwork1: TMenuItem;
    Tools1: TMenuItem;
    FindFolderorFiles1: TMenuItem;
    Goto1: TMenuItem;
    N7: TMenuItem;
    CalculateTreeSize1: TMenuItem;
    N8: TMenuItem;
    RewriteFileDateTime1: TMenuItem;
    Help1: TMenuItem;
    LsDirTree21ShortCuts1: TMenuItem;
    LsFileListView28ShortCuts1: TMenuItem;
    N9: TMenuItem;
    AboutLsFileExplorer281: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    EditPath: TEdit;
    Label1: TLabel;
    CbxMask: TComboBox;
    Label2: TLabel;
    N10: TMenuItem;
    HideFileExtensions1: TMenuItem;
    ShowFolders1: TMenuItem;
    PopupMenu1: TPopupMenu;
    LargeIcons1: TMenuItem;
    SmallIcons1: TMenuItem;
    List2: TMenuItem;
    Delail1: TMenuItem;
    Language1: TMenuItem;
    Language_0: TMenuItem;
    Language_4: TMenuItem;
    Language_5: TMenuItem;
    Language_13: TMenuItem;
    Language_16: TMenuItem;
    ToolBar1: TToolBar;
    BtnPrevLevel: TToolButton;
    ToolButton4: TToolButton;
    BtnFolderTree: TToolButton;
    BtnShowFolder: TToolButton;
    BtnViewStyle: TToolButton;
    BtnCut: TToolButton;
    BtnCopy: TToolButton;
    BtnPaste: TToolButton;
    ToolButton12: TToolButton;
    BtnRefresh: TToolButton;
    Language_10: TMenuItem;
    BtnBack: TToolButton;
    Language_6: TMenuItem;
    Language_3: TMenuItem;
    ToolButton1: TToolButton;
    BtnExit: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Language_9: TMenuItem;
    BtnFind: TToolButton;
    ImageList1_N: TImageList;
    ImageList1_H: TImageList;
    ToolButton5: TToolButton;
    Language_1: TMenuItem;
    Language_2: TMenuItem;
    Language_7: TMenuItem;
    DosCommandPrompt1: TMenuItem;
    N11: TMenuItem;
    Gouponelevel1: TMenuItem;
    FormatDiskette1: TMenuItem;
    Language_12: TMenuItem;
    LsDirTree211: TLsDirTree21;
    LsFilelistView281: TLsFilelistView28;
    Langauge_11: TMenuItem;
    DiskFreeSpace1: TMenuItem;
    SelectedSize1: TMenuItem;
    Language_14: TMenuItem;
    BtnTextColor: TToolButton;
    ImageList_D: TImageList;
    Langauge_8: TMenuItem;
    Language_15: TMenuItem;
    ShowhiddenfilesandFolders1: TMenuItem;
    procedure ShowFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetBtnMenuState(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnPrevLevelClick(Sender: TObject);
    procedure BtnCutClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure QuickView1Click(Sender: TObject);
    procedure DeleteFiles1Click(Sender: TObject);
    procedure RenameFile1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Goto1Click(Sender: TObject);
    procedure LsDirTree21ShortCuts1Click(Sender: TObject);
    procedure LsFileListView28ShortCuts1Click(Sender: TObject);
    procedure AboutLsFileExplorer281Click(Sender: TObject);
    procedure LsFilelistView281Click(Sender: TObject);
    procedure LsFilelistView281DblClick(Sender: TObject);
    procedure CbxMaskClick(Sender: TObject);
    procedure LsDirTree211DirChange(Sender: TObject; SelectedPath: String);
    procedure BtnFolderContentsClick(Sender: TObject);
    procedure FindFolderorFiles1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SelectViewStyle(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure BtnFolderTreeClick(Sender: TObject);
    procedure BtnViewStyleClick(Sender: TObject);
    procedure NewFolder1Click(Sender: TObject);
    procedure DeleteFolder1Click(Sender: TObject);
    procedure RenameFolder1Click(Sender: TObject);
    procedure CalculateTreeSize1Click(Sender: TObject);
    procedure ConnecttoNetwork1Click(Sender: TObject);
    procedure DisconnectFromNetwork1Click(Sender: TObject);
    procedure RewriteFileDateTime1Click(Sender: TObject);
    procedure HideFileExtensions1Click(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
    procedure LsDirTree211MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnExitClick(Sender: TObject);
    procedure LsFilelistView281Enter(Sender: TObject);
    procedure DosCommandPrompt1Click(Sender: TObject);
    procedure Gouponelevel1Click(Sender: TObject);
    procedure FormatDiskette1Click(Sender: TObject);
    procedure DiskFreeSpace1Click(Sender: TObject);
    procedure SelectedSize1Click(Sender: TObject);
    procedure BtnTextColorClick(Sender: TObject);
    procedure LsFilelistView281ItemChange(Sender: TObject;
      SelectedItem: String);
    procedure LsDirTree211Click(Sender: TObject);
    procedure ShowhiddenfilesandFolders1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ShowHiddenFoldersandFiles: Boolean;
    CanPaste: Boolean;
    BackFwdLst: TStringList;
    idx: integer;
    CurDir: string;
  end;

//  TDriveType = (dtADrive, dtBDrive);
//  TCapacity  = (capDefault, capHigh, capLow);
//  TFmtType   = (fmtQuick, fmtFull, fmtBoot);
function  SHFormatDrive(hWnd : HWND; iDriveID, iCapacity, iFormatType : Integer):
  longint; stdcall;  //880

var
  MainForm1: TMainForm1;

implementation


uses FileDate, About;

{$R *.DFM}
{$IFDEF D6_OR_HIGHER}
  {$R WinXP.res}   //see Readme.txt
{$ENDIF}

function SHFormatDrive; Stdcall; external 'shell32.dll' name 'SHFormatDrive'; //880

procedure TMainForm1.FormCreate(Sender: TObject);
//var
//  CurDir: string;
begin
  SetBtnMenuState(Sender);
  LsFilelistView281.ShowFolders := True;
  LsFileListView281.LItemColorByATTR := True;
  LsFileListview281.ColWidth_Name := 190;
  BtnShowFolder.Down := True;
  ShowFolders1.Checked := True;
  BackFwdLst := TStringList.Create;
  BackFwdLst.Clear;
  idx := BackFwdLst.Count;
  LsFileListView281.Language := lnEnglish;
  LsDirTree211.Language := lnEnglish;
  Language_0.Checked := True;
  CurDir := GetCurrentDir;
  if DirectoryExists(CurDir) then
  begin
    if CurDir[Length(CurDir)] <> '\' then
       CurDir := CurDir + '\';
    LsDirTree211.InitialDir := CurDir;
  end;
  EditPath.Text := MinimizeName(LsFileListview281.Directory, Canvas, 340);
end;  //FormCreate

procedure TMainForm1.SetBtnMenuState(Sender: TObject);
begin
  CanPaste := False;
  if Clipboard.HasFormat(CF_HDROP) and (BtnCut.Enabled or BtnCopy.Enabled) then
    CanPaste := True;
  BtnCut.Enabled := (LsFileListView281.SelCount <> 0) and
    (LsFileListView281.Selected.Caption <> LsFileListview281.ParentDirCaption);
  BtnCopy.Enabled := (LsFileListView281.SelCount <> 0) and
    (LsFileListView281.Selected.Caption <> LsFileListview281.ParentDirCaption);
  BtnPaste.Enabled := (CanPaste = True) and (BtnCut.Enabled or BtnCopy.Enabled);
  Cut1.Enabled := (LsFileListView281.SelCount <> 0) and
    (LsFileListView281.Selected.Caption <> LsFileListview281.ParentDirCaption);
  //Assigned(LsFilelistView281.Selected);
  Copy1.Enabled := (LsFileListView281.SelCount <> 0) and
    (LsFileListView281.Selected.Caption <> LsFileListview281.ParentDirCaption);
  //Assigned(LsFilelistView281.Selected);
  Paste1.Enabled := (CanPaste = True) and (BtnCut.Enabled or BtnCopy.Enabled);
  Open1.Enabled := Assigned(LsFilelistView281.Selected);
  QuickView1.Enabled := Assigned(LsFilelistView281.Selected);
  DeleteFiles1.Enabled := Assigned(LsFilelistView281.Selected);
  RenameFile1.Enabled := Assigned(LsFilelistView281.Selected);
//  Properties1.Enabled := Assigned(LsFilelistView281.Selected);
end; //SetBtnMenuState

{*
procedure TMainForm1.GetStatistics(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  with LsFilelistView281 do
  begin
    if Selected <> nil then
    begin
      StatusBar1.Panels[0].Text := IntToStr(Items.Count) + ' object(s)  --  ' +
        IntToStr(SelectedNumber) + '  selected ';
      StatusBar1.Panels[1].Text := ' Selected Size = ' +
        FormatFloat('###,##0.00 KB', (SelectedSize / 1024)) +
        '( Disk free space: ' +
        FormatFloat('###,### MB', (DriveFreeSpace)) + ' )';
    end
    else begin
      StatusBar1.Panels[0].Text := IntToStr(Items.Count) + ' object(s)';
      StatusBar1.Panels[1].Text := ' Selected Size = 0 KB' +
        '  ( Disk free space: ' +
        FormatFloat('###,### MB', (DriveFreeSpace)) + ' )';
    end;
  end;
  EditPath.Text := MinimizeName(LsDirTree211.SelectedPath, Canvas, 340);;
end; //GetStatistics
*}

procedure TMainForm1.BtnRefreshClick(Sender: TObject);
var
  TreePath: string;
begin
  TreePath := '';
  with LsDirTree211 do
  begin
    if (Selected <> nil) and (Assigned(Selected)) then
      TreePath := SelectedPath;
    ReLoad;
    OpenPath(TreePath);
  end;
  BtnShowFolder.Down := True;
  LsFilelistView281.ShowFolders := True;
  LsFilelistView281.Directory := TreePath;
  LsFilelistView281.UpdateFileList;
end; //BtnRefreshClick

procedure TMainForm1.ShowFolderClick(Sender: TObject);
begin
  if LsFilelistView281.ShowFolders = True then
  begin
    LsFilelistView281.ShowFolders := False;
    BtnShowFolder.Down := False;
    ShowFolders1.Checked := False;
  end
  else begin
    LsFilelistView281.ShowFolders := True;
    BtnShowFolder.Down := True;
    ShowFolders1.Checked := True;
  end;
end;  //BtnShowFolderClick

procedure TMainForm1.BtnCutClick(Sender: TObject);
begin
  if LsDirTree211.Focused then
  begin
    if (LsDirTree211.Selected.Level <= 1) or (LsDirTree211.Selected = nil) then
      exit;
    LsDirTree211.CutOrCopyNode(2);
  end
  else if LsFileListView281.Focused then
    LsFilelistView281.CutCopy(2);
  Cut1.Enabled := False;
  CanPaste := True;
  SetBtnMenuState(Sender);
end; //BtnCutClick

procedure TMainForm1.BtnCopyClick(Sender: TObject);
begin
  if LsDirTree211.Focused then
  begin
    if (LsDirTree211.Selected.Level <= 1) or (LsDirTree211.Selected = nil) then
      exit;
    LsDirTree211.CutOrCopyNode(0);
  end
  else if LsFileListView281.Focused then
    LsFilelistView281.CutCopy(0);
  Copy1.Enabled := False;
  CanPaste := True;
  SetBtnMenuState(Sender);
end; //BtnCopyClick

procedure TMainForm1.BtnPasteClick(Sender: TObject);
begin
  if LsDirTree211.Focused then
    LsDirTree211.PasteNode
  else if LsFileListView281.Focused then
    LsFilelistView281.Paste;
  CanPaste := False;
  Cut1.Enabled := True;
  Copy1.Enabled := True;
  SetBtnMenuState(Sender);
end; //BtnPasteClick

procedure TMainForm1.Open1Click(Sender: TObject);
begin
  LsFilelistView281.OpenItem;
end;  //Open1Click

procedure TMainForm1.QuickView1Click(Sender: TObject);
begin
  if Assigned(LsFilelistView281) then
    if LsFilelistView281.Selected <> nil then
      LsFilelistView281.ViewFile;
end; //QuickView1Click

procedure TMainForm1.DeleteFiles1Click(Sender: TObject);
begin
  LsFilelistView281.DeleteItems;
end; //DeleteFiles1Click

procedure TMainForm1.RenameFile1Click(Sender: TObject);
begin
  LsFilelistView281.RenameFile;
end; //RenameFile1Click


procedure TMainForm1.SelectAll1Click(Sender: TObject);
var
  i: integer;
begin
  if not Assigned(LsFilelistView281) then exit;
  with LsFilelistView281 do
  begin
    if Items.Count = 0 then exit;
    for i := 0 to Items.Count - 1 do
      Selected := Items[i];
  end;
end; //SelectAll1Click

procedure TMainForm1.Goto1Click(Sender: TObject);
var
  PathName: string;
begin
  InputQuery('Go to Folder', 'Enter the name and path of the folder to open',
    PathName);
  if (PathName = '') or (Length(PathName) = 1) then exit;
  if PathName[Length(PathName)] <> '\' then
    PathName := PathName + '\';
  if DirectoryExists(PathName) then
  begin
    LsFilelistView281.Directory := PathName;
    LsDirTree211.OpenPath(PathName);
  end
  else begin
    if (Length(PathName) <= 3) and (PathName[2] = ':') then
      MessageDlg(UpperCase(PathName) + '  not ready', mtError, [mbOK], 0)
    else
      MessageDlg(PathName + '  not found', mtError, [mbOK], 0);
    exit;
  end;
end; //Goto1Click

procedure TMainForm1.LsDirTree21ShortCuts1Click(Sender: TObject);
begin
  AboutForm.PageControl1.ActivePage := AboutForm.TabSheet1;
  AboutForm.ShowModal;
end; //LsDirTree21ShortCuts1Click

procedure TMainForm1.LsFileListView28ShortCuts1Click(Sender: TObject);
begin
  AboutForm.PageControl1.ActivePage := AboutForm.TabSheet2;
  AboutForm.ShowModal;
end; //LsFilelistView28ShortCuts1Click

procedure TMainForm1.AboutLsFileExplorer281Click(Sender: TObject);
begin
  AboutForm.PageControl1.ActivePage := AboutForm.TabSheet3;
  AboutForm.ShowModal;
end; //AboutLsFileExplorer261Click

procedure TMainForm1.LsFilelistView281Click(Sender: TObject);
begin
  SetBtnMenuState(Sender);
//  ShowMessage(LsFileListView281.SelectedItem);  //85
end; //LsFilelistView281Click

procedure TMainForm1.LsFilelistView281DblClick(Sender: TObject);
begin
  LsFilelistView281.OpenItem;
end; //LsFilelistView281DblClick

procedure TMainForm1.CbxMaskClick(Sender: TObject);
begin
  LsFilelistView281.Mask := CbxMask.Text;
end;  //CbxMaskClick

procedure TMainForm1.LsDirTree211DirChange(Sender: TObject;
  SelectedPath: String);
begin
//  EditPath.Text := MinimizeName(LsDirTree211.SelectedPath, Canvas, 340);
  EditPath.Text := MinimizeName(LsFileListview281.Directory, Canvas, 340);
end;  //LsDirTree211DirChange

procedure TMainForm1.LsDirTree211Click(Sender: TObject);
begin
  EditPath.Text := MinimizeName(LsDirTree211.SelectedPath, Canvas, 340);
end;

procedure TMainForm1.BtnFolderContentsClick(Sender: TObject);
begin
  if LsDirTree211.Selected <> nil then
    LsDirTree211.SHowFolderContents;
end;  //BtnFolderContentsClick

procedure TMainForm1.SelectViewStyle(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Case Tag of
      0: LsFilelistView281.ViewStyle := vsIcon;
      1: LsFilelistView281.ViewStyle := vsSmallIcon;
      2: LsFilelistView281.ViewStyle := vsList;
      3: LsFilelistView281.ViewStyle := vsReport;
    end;
  end;
end;  //SelectViewStyle

procedure TMainForm1.FindFolderorFiles1Click(Sender: TObject);
begin
  LsFilelistView281.SetFocus;
  LsFilelistView281.FindFile;
end;  //FindFolderorFiles1Click

procedure TMainForm1.BtnFolderTreeClick(Sender: TObject);
begin
  if LsDirTree211.Align = alLeft then
  begin
    LsDirTree211.Align := alNone;
    BtnFolderTree.Down := False;
  end
  else if LsDirTree211.Align = alNone then
  begin
    LsDirTree211.Align := alLeft;
    BtnFolderTree.Down := True;
  end;
end;  //BtnFolderTreeClick

procedure TMainForm1.BtnViewStyleClick(Sender: TObject);
begin
  with LsFilelistView281 do
  begin
    if ViewStyle = vsIcon then
      ViewStyle := vsSmallIcon
    else if ViewStyle = vsSmallIcon then
      ViewStyle := vsList
    else if ViewStyle = vsList then
      ViewStyle := vsReport
    else if ViewStyle = vsReport then
      ViewStyle := vsIcon;
  end;
end;  //BtnViewStyleClick

procedure TMainForm1.NewFolder1Click(Sender: TObject);
var
  PNode: TTreeNode;
  NewFolder: string;
begin
  with LsDirTree211 do
  begin
    NewFolder := '';
    PNode := Selected;
    if Assigned(Selected) then
    begin
      LsDirTree211.AddNewNode(PNode, '');
    end;
  end;
end; //NewFolder1Click

procedure TMainForm1.DeleteFolder1Click(Sender: TObject);
begin
  with LsDirTree211 do
  begin
    if (Selected <> nil) and (Assigned(Selected)) then
      DeleteNode(Selected);
  end;
end;  //DeleteFolder1Click

procedure TMainForm1.RenameFolder1Click(Sender: TObject);
begin
  with LsDirTree211 do
  begin
    if (Selected <> nil) and (Assigned(Selected)) then
      Selected.EditText;
  end;
end;  //RenameFolder1Click

procedure TMainForm1.CalculateTreeSize1Click(Sender: TObject);
begin
  if LsDirTree211.Selected <> nil then
    LsDirTree211.GetTreeSize;
end;  //CalculateTreeSize1Click

procedure TMainForm1.ConnecttoNetwork1Click(Sender: TObject);
begin
  LsDirTree211.ConnectNetResource(Sender);
end;  //ConnecttoNetwork1Click

procedure TMainForm1.DisconnectFromNetwork1Click(Sender: TObject);
begin
  LsDirTree211.DisConnectNetResource(Sender);
end;  //DisconnectFromNetwork1Click

procedure TMainForm1.RewriteFileDateTime1Click(Sender: TObject);
var
  i: integer;
  FDate: Longint;
  NewDateTime: TDateTime;
  SelFileName: string;
  SelFile: File;
  OldCur: TCursor;
begin
  NewDateTime := Now;
  with LsFilelistView281 do
  begin
    if Selected = nil then
    begin
      ShowMessage('No file selected');
      exit;
    end;

    if FileDateForm.ShowModal = mrOK then
      NewDateTime := FileDateForm.SelectedDateTime;
    FDate := DateTimeToFileDate(NewDateTime);

    Items.BeginUpdate;
    OldCur := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    for i := 0 to Items.Count - 1 do
    begin
      if Items[i].Selected then
      begin
        System.FileMode := fmOpenWrite;
        SelFileName := Items[i].SubItems[4];
        AssignFile(SelFile, SelFileName );
        {$I-}
        Reset(SelFile);
        {$I+}
        if IOResult <> 0 then
        begin
          ShowMessage('Could not open ' + ExtractFileName(SelFileName));
          exit;
        end;
        FileSetDate(TFileRec(SelFile).Handle, FDate);
        System.CloseFile(SelFile);
      end;
    end;
    Items.EndUpdate;
    Screen.Cursor := OldCur;
    BtnRefreshClick(Sender);
  end;
end;  //RewriteFileDateTime1Click

procedure TMainForm1.HideFileExtensions1Click(Sender: TObject);
begin
  with LsFileListView281 do
  begin
    if HideFileExt = True then
    begin
      HideFileExt := False;
      HideFileExtensions1.Checked := False;
    end
    else begin
      HideFileExt := True;
      HideFileExtensions1.Checked := True;
    end;
    UpdateFileList;
  end;
end;  //HideFileExtensions1Click


procedure TMainForm1.BtnPrevLevelClick(Sender: TObject);
begin
  BackFwdLst.Add(LsDirTree211.SelectedPath);
  idx := BackFwdLst.Count; //idx + 1;
  LsFilelistView281.OneLevelUp;
  EditPath.Text := MinimizeName(LsFileListview281.Directory, Canvas, 340);
  BtnBack.Enabled := BackFwdLst.Count >= 1;
  SetBtnMenuState(Sender);
end; //BtnPrevLevelClick

procedure TMainForm1.Gouponelevel1Click(Sender: TObject);
begin
  BtnPrevLevelClick(Self);
end; //GoUpOneLevelClick

procedure TMainForm1.LsDirTree211MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTest: THitTests;
begin
  inherited;

  HitTest := LsDirTree211.GetHitTestInfoAt(X, Y);
  if (htOnItem in HitTest) {or (htOnButton in HitTest)}  then
  begin
    BackFwdLst.Add(LsDirTree211.SelectedPath);
    idx := BackFwdLst.Count; //idx + 1;
//    ShowMessage('BflCount = ' + IntToStr(BackFwdLst.Count)+ ',   ' + #13 +
//                'idx      = ' + IntToStr(idx));   //debugging
    BtnBack.Enabled := BackFwdLst.Count >= 1;
  end;
end;  //LsDirTree211MouseDown

procedure TMainForm1.LanguageClick(Sender: TObject);
var
  CurDir: String;
begin
  CurDir := GetCurrentDir;
  with Sender as TMenuItem do
  begin
    case Tag of
      0: begin
           LsFileListView281.Language := lnEnglish;
           LsDirTree211.Language := lnEnglish;
         end;
      1: begin
           LsFileListView281.Language := lnChinese_Tra;
           LsDirTree211.Language := lnChinese_Tra;
         end;
      2: begin
           LsFileListView281.Language := lnChinese_Sim;
           LsDirTree211.Language := lnChinese_Sim;
         end;
      3: begin
           LsFileListView281.Language := lnDutch;
           LsDirTree211.Language := lnDutch;
         end;
      4: begin
           LsFileListView281.Language := lnFrench;
           LsDirTree211.Language := lnFrench;
         end;
      5: begin
           LsFileListView281.Language := lnGerman;
           LsDirTree211.Language := lnGerman;
         end;
      6: begin
           LsFileListView281.Language := lnItalian;
           LsDirTree211.Language := lnItalian;
         end;
      7: begin  //880<
           LsFileListView281.Language := lnJapanese;
           LsDirTree211.Language := lnJapanese;
         end;  //>880
      8: begin  //885<
           LsFileListView281.Language := lnKorean;
           LsDirTree211.Language := lnKorean;
         end;  //>885
      9: begin
           LsFileListView281.Language := lnPolish;
           LsDirTree211.Language := lnPolish;
         end;
      10: begin
           LsFileListView281.Language := lnPortuguese;
           LsDirTree211.Language := lnPortuguese;
         end;
     11: begin
           LsFileListView281.Language := lnSlovak;
           LsDirTree211.Language := lnSlovak;
         end;
     12: begin  //881<
           LsFileListView281.Language := lnSlovenian;
           LsDirTree211.Language := lnSlovenian;
         end;  //>881
     13: begin
           LsFileListView281.Language := lnSpanish;
           LsDirTree211.Language := lnSpanish;
         end;
     14: begin
           LsFileListView281.Language := lnSwedish;
           LsDirTree211.Language := lnSwedish;
         end;
     15: begin
           LsFileListView281.Language := lnTurkish;
           LsDirTree211.Language := lnTurkish;
         end;
     16: begin
           LsFileListView281.Language := lnSysDefault;
           LsDirTree211.Language := lnSysDefault;
         end;
    end; //case
    Checked := True;
  end;  //with
  LsDirTree211.OpenPath(CurDir);
  LsFileListView281.Directory := CurDir;
  LsFileListView281.UpdateFileList;
end;  //LanguageClick

procedure TMainForm1.BtnBackClick(Sender: TObject);
var
  Path: string;
begin
  if BtnBack.Enabled = False then exit;
  Path := BackFwdLst.Strings[idx - 1];
  LsDirTree211.OpenPath(Path);
  LsFileListView281.Directory := Path;
  LsFileListView281.UpdateFileList;
  idx := idx - 1;
  if idx < 1 then
  begin
    idx := 0;
    BtnBack.Enabled := False;
    BackFwdLst.Clear;
    exit;
  end;
end;  //BtnBackClick

procedure TMainForm1.Exit1Click(Sender: TObject);
begin
  BackFwdLst.Free;
  Close;
end;   //Exit1Click

procedure TMainForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BackFwdLst.Free;
end;  //FormClose

procedure TMainForm1.BtnExitClick(Sender: TObject);
begin
  Close;
end;   //BtnExitClick

procedure TMainForm1.LsFilelistView281Enter(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_HDROP) then
    CanPaste := True;
  BtnCut.Enabled := Assigned(LsFilelistView281.Selected);
  BtnCopy.Enabled := Assigned(LsFilelistView281.Selected);
  BtnPaste.Enabled := CanPaste = True;
  Cut1.Enabled := Assigned(LsFilelistView281.Selected);
  Copy1.Enabled := Assigned(LsFilelistView281.Selected);
  Paste1.Enabled := CanPaste = True;
end;  //LsFilelistView281Enter

procedure TMainForm1.DosCommandPrompt1Click(Sender: TObject);  //880<
var
  WinPath: string;
begin
  WinPath := LsFileExplorer28.GetSystemPath(spWinRoot) + '\System32';
  LsFileExplorer28.ExecuteFile('Open', 'Command.com', '', WinPath, SW_SHOW);
end;  //DosCommandPrompt1Click  //>880

procedure TMainForm1.FormatDiskette1Click(Sender: TObject);  //880<
begin
  if not LsFileExplorer28.DiskinDrive('A', 0) then
    MessageDlg('Insert a disk into Drive A:' + #13 +
               'and click OK when ready ...' , mtError, [mbOK], 0);
  SHFormatDrive(Application.Handle, 0, 0, 0);
end;  //FormatDiskette1Click  //>880

procedure TMainForm1.DiskFreeSpace1Click(Sender: TObject);
var
  Drv, FSpace: string;
begin
  Drv := Copy(LsFileListview281.Directory, 1, 2);
  FSpace := 'Disk ' + Drv +' Free Space = ' +
    FormatFloat('###,##0.00', (LsFileExplorer28.GetFreeDiskSize(Drv)/1024)) + ' MB';
  Application.MessageBox(PChar(FSpace), 'Disk Free Space', MB_OK);
end;   //DiskFreeSpace1Click

procedure TMainForm1.SelectedSize1Click(Sender: TObject);
var
  SelNum, SelSize: string;
  OldCur: TCursor;
begin
  with LsFilelistView281 do
  begin
    if Selected <> nil then
    begin
      OldCur := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      SelNum := {IntToStr(Items.Count) + ' object(s)  -----  ' + }
        IntToStr(SelectedNumber) + ' Item(s) selected ';
      SelSize := 'Selected Size  = ' +
        FormatFloat('###,##0.00 KB', (SelectedSize / 1024));
      Application.MessageBox(PChar(SelNum + #13 + SelSize),
                             'Selected Size', MB_OK);
      Screen.Cursor := OldCur;
    end
    else
      Application.MessageBox('No Listitem selected',
                         'Selected Size', MB_OK);
  end;
end;  //SelectedSize1Click

procedure TMainForm1.BtnTextColorClick(Sender: TObject);
begin
  with LsFileListview281 do
  begin
    if LItemColorByATTR = True then
    begin
      LItemColorByATTR := False;
      BtnTextColor.down := False;
    end
    else if LItemColorByATTR = False then
    begin
      LItemColorByATTR := True;
      BtnTextColor.down := True;
    end;
    LsFileListview281.UpdateFileList;
  end;
end;

procedure TMainForm1.LsFilelistView281ItemChange(Sender: TObject;
  SelectedItem: String);
begin
  EditPath.Text := MinimizeName(LsFileListview281.Directory, Canvas, 340);
  SetBtnMenuState(Sender);
end;


procedure TMainForm1.ShowhiddenfilesandFolders1Click(Sender: TObject);
var
  FCurDir: string;
  ViewOption: Integer;
  FFileType: TFileType;
begin
  if ShowhiddenfilesandFolders1.Checked = True then
  begin
    if ftSystem in LsFileListview281.FileType then
      ViewOption := 3
    else
      ViewOption := 1;
    ShowhiddenfilesandFolders1.Checked := False;
  end
  else begin
    if ftSystem in LsFileListview281.FileType then
      ViewOption := 4
    else
      ViewOption := 2;
    ShowhiddenfilesandFolders1.Checked := True;
  end;

  Case ViewOption of
    1: FFileType := [ftReadOnly, ftDirectory, ftArchive, ftNormal];
    2: FFileType := [ftReadOnly, ftHidden, ftDirectory, ftArchive, ftNormal];
    3: FFileType := [ftReadOnly, ftSystem, ftDirectory, ftArchive, ftNormal];
    4: FFileType := [ftReadOnly, ftHidden, ftSystem, ftDirectory, ftArchive,
                     ftNormal];
  end;

  LsDirtree211.SetFolderType(FFileType);
  LsFileLIstview281.SetFileType(FFileType);

  FCurDir := 'C:\Windows';
  if FCurDir = '' then FCurDir := 'C:\';
  LsDirTree211.Reload;
  LsDirTree211.OpenPath(FCurDir);
  LsFileListView281.Directory := FCurDir;
  LsFileListView281.UpdateFileList;
end;

end.

