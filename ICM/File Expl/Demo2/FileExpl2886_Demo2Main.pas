
{FileExpl287?_Demo2Main.pas}
{=============================================================================
This Demo was compiled by Delphi 7.x. The DFM files (i.e. About.dfm and
FileExpl287?_Demo2Main.dfm) are saved in binary format instead of its default
TextFormat, thus it will be bachwards compatible with previous versions of
Delphi.

If you want to recompile it under Delphi 6, 5, or 4 , just load all files
(including FileExpl287?_Demo.dpr, FileExpl287?_Demo2Main.pas and About.pas)
into Delphi's IDE befor rebuilding. However, under D4 you may encounter
error messages, because some properties on Form1 and Form2 may be not available
in D4.  But don't worry, just ignore all error messages, then proceed
as usual.

These binary DFM files created by D6 can be converted to default text format
by using 'textDFM' option in D5/D6/D7's context menu.

NOTE:-  where '?' represents the build number of LsFileExplorer28.
==============================================================================}

unit FileExpl2886_Demo2Main;

interface

{$INCLUDE LsComp.inc}
{$IFDEF D6_OR_HIGHER}
  {$WARNINGS OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls, LsFileExplorer28, Menus, ShellAPI,
  FileCtrl, ToolWin, Clipbrd, ImgList;
//{$IFDEF D7_OR_HIGHER},
//  XPMan
//{$ENDIF};

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit2: TMenuItem;
    View1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    Rename1: TMenuItem;
    N3: TMenuItem;
    NewFolder1: TMenuItem;
    N4: TMenuItem;
    Close1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    LargeIcon1: TMenuItem;
    SmallIcon1: TMenuItem;
    List1: TMenuItem;
    Detail1: TMenuItem;
    N6: TMenuItem;
    Refresh1: TMenuItem;
    Tool1: TMenuItem;
    Find1: TMenuItem;
    GoTo1: TMenuItem;
    QuickView1: TMenuItem;
    Help1: TMenuItem;
    ShortCuts1: TMenuItem;
    ShortCuts2: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    Properties1: TMenuItem;
    N7: TMenuItem;
    ShowFolders1: TMenuItem;
    HideFileExtensions1: TMenuItem;
    PopupMenu1: TPopupMenu;
    LargeIcons1: TMenuItem;
    SmallIcons1: TMenuItem;
    List2: TMenuItem;
    Details1: TMenuItem;
    Language1: TMenuItem;
    English1: TMenuItem;
    French1: TMenuItem;
    German1: TMenuItem;
    Spanish1: TMenuItem;
    SystemDefault1: TMenuItem;
    Portuguese1: TMenuItem;
    Italian1: TMenuItem;
    Dutch1: TMenuItem;
    Polish1: TMenuItem;
    Slovak1: TMenuItem;
    MapNetworkDrives1: TMenuItem;
    DisconnectNetworkdrives1: TMenuItem;
    ImageList_N: TImageList;
    ImageList_H: TImageList;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    Label1: TLabel;
    MaskCombo1: TComboBox;
    BtnMapDrv: TToolButton;
    BtnDisconnect: TToolButton;
    BtnLevelUp: TToolButton;
    BtnRefresh: TToolButton;
    BtnShowFolder: TToolButton;
    BtnHideFileExt: TToolButton;
    BtnCut: TToolButton;
    BtnCopy: TToolButton;
    BtnPaste: TToolButton;
    BtnSelectViewStyle: TToolButton;
    BtnClose: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ChineseTra1: TMenuItem;
    ChineseSim1: TMenuItem;
    Japanese1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    FormatDiskette1: TMenuItem;
    DOSCommandPrompt1: TMenuItem;
    Slovenian1: TMenuItem;
    Swedish1: TMenuItem;
    BtnTextColor: TToolButton;
    LsDirTreeCombo281: TLsDirTreeCombo28;
    ImageList_D: TImageList;
    Korean1: TMenuItem;
    Turkish1: TMenuItem;
    LsFilelistView281: TLsFilelistView28;
    procedure FormCreate(Sender: TObject);
    procedure GetStatistics(Sender: TObject);
    procedure LsFilelistView281MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MaskCombo1Change(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure GoTo1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure MainMenu1Click(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure ViewStyleClick(Sender: TObject);
    procedure ViewStyleMenuClick(Sender: TObject);
    procedure LsFilelistView281KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConnecttoNetwork1Click(Sender: TObject);
    procedure DisconnectfromNetwork1Click(Sender: TObject);
    procedure Btn1LevelUpClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnShowFolderClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnHideFileExtClick(Sender: TObject);
    procedure BtnSelectViewStylesClick(Sender: TObject);
    procedure BtnMapDrvClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure ToolBar2Click(Sender: TObject);
    procedure BtnCutClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure FormatDiskette1Click(Sender: TObject);
    procedure DOSCommandPrompt1Click(Sender: TObject);
    procedure BtnTextColorClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
  { Private declarations }
    CanPaste: Boolean;
  public
  { Public declarations }
  end;

function  SHFormatDrive(hWnd : HWND; iDriveID, iCapacity, iFormatType : Integer):
  longint; stdcall;

var
  Form1: TForm1;

implementation

uses About;

{$R *.DFM}
{$IFDEF D6_OR_HIGHER}
  {$R WinXP.res}  //Please refer to Readme.txt
{$ENDIF}

function SHFormatDrive; Stdcall; external 'shell32.dll' name 'SHFormatDrive'; //880

procedure TForm1.FormCreate(Sender: TObject);
var
  CurDir: string;
begin
  CanPaste := False;
  LsDirTreeCombo281.TreeHeight := 300;
  with LsFileListview281 do
  begin
    English1.Checked := True;
    Language := lnEnglish;
    ColWidth_Name := 280;
    ColWidth_Type := 115;
  end;
  LsFileListView281.LItemColorByATTR := True;
  ToolBar2Click(Self);  //876
  Application.HintHidePause := 10000;  //876
  CurDir := GetCurrentDir;
  if CurDir[length(CurDir)] <> '\' then
    CurDir := CurDir + '\';
  LsDirTreeCombo281.OpenPath(CurDir);
  LsFileListview281.Directory := CurDir;
end;  //FormCreate

procedure TForm1.FormActivate(Sender: TObject);
begin
  LsDirTreeCombo281.SetFocus;
end;

procedure TForm1.GetStatistics(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  with LsFilelistView281 do
  begin
    if (Selected <> nil) and (UpperCase(Directory) <> 'DRIVES') then
    begin
      StatusBar1.Panels[0].Text := IntToStr(Items.Count) + ' object(s)  --  ' +
        IntToStr(SelectedNumber) + '  selected ';
      StatusBar1.Panels[1].Text := ' Disk free space: ' +
        FormatFloat('###,### MB', (DriveFreeSpace));
    end
    else begin
      StatusBar1.Panels[0].Text := IntToStr(Items.Count) + ' object(s)';
      StatusBar1.Panels[1].Text := 'Disk free space: ' +
        FormatFloat('###,### MB', (DriveFreeSpace));
    end;
  end;
end;  //GetStatistics

procedure TForm1.ToolBar2Click(Sender: TObject);
begin
  with LsFilelistView281 do
  begin
    BtnCut.Enabled :=  LsFileListview281.Selected <> nil;
    BtnCopy.Enabled :=  LsFileListview281.Selected <> nil;
    BtnPaste.Enabled := (CanPaste = True);
  end;
end;  //ToolBar2Click

procedure TForm1.MainMenu1Click(Sender: TObject);
begin
  with LsFilelistView281 do
  begin
    if Clipboard.HasFormat(CF_HDROP) then
      CanPaste := True;
    Open1.Enabled := Selected <> nil;
    QuickView1.Enabled := Selected <> nil;
    Delete1.Enabled := Selected <> nil;
    ReName1.Enabled := Selected <> nil;
    Properties1.Enabled := Selected <> nil;
    NewFolder1.Enabled := UpperCase(Directory) <> 'DRIVES';
    Cut1.Enabled := Selected <> nil;
    Copy1.Enabled := Selected <> nil;
    Paste1.Enabled := CanPaste;
    Find1.Enabled := Focused; //Selected <> nil;
    SelectAll1.Enabled := Focused;
  end;
end;  //MainMenu1Click

procedure TForm1.MenuItemClick(Sender: TObject);
var
  i: integer;
begin
  with sender as TMenuItem do
  begin // MenuItem
    case Tag of // -----------
      1: LsFilelistView281.OpenItem;            // Open1
      2: LsFilelistView281.ViewFile;            // QuickView1
      3: LsFilelistView281.DeleteItems;         // Delete1
      4: LsFilelistView281.ReNameFile;          // Rename1
      5: LsFilelistView281.ShowFileProperties;  // Properties
      6: LsFilelistView281.NewFolder;           // NewFolder1

      11: begin  // Cut
          LsFilelistView281.CutCopy(2);
          Cut1.Enabled := False;
          BtnCut.Enabled := False;  //876
          CanPaste := True;
        end;
      12: begin  // Copy
          LsFilelistView281.CutCopy(0);
          Copy1.Enabled := False;
          CanPaste := True;
        end;
      13: begin  // Paste
          LsFilelistView281.Paste;
          CanPaste := False;
          Cut1.Enabled := True;
          Copy1.Enabled := True;
        end;
      14: begin  // Select All
          with LsFilelistView281 do
          begin
            if Items.Count = 0 then exit;
            for i := 0 to Items.Count - 1 do
              Selected := Items[i];
          end;
        end;
      25: LsFilelistView281.UpdateFileList; // Refresh

      41: begin  // LsDirTreeCombo25 ShortCuts
          Form2.PageControl1.ActivePage := Form2.TabSheet1;
          Form2.ShowModal;
        end;
      42: begin  // LsFileListView25 ShortCuts
          Form2.PageControl1.ActivePage := Form2.TabSheet2;
          Form2.ShowModal;
        end;
      43: begin  // About LsFileExolorer25
          Form2.PageControl1.ActivePage := Form2.TabSheet3;
          Form2.ShowModal;
        end;
    end;  // case
  end;  // with
end;  //MenuItemClick

procedure TForm1.BtnCutClick(Sender: TObject);
begin
  if LsFileListview281.Focused then
    LsFileListview281.CutCopy(2);
//  BtnCut.Enabled := False;
  BtnPaste.Enabled := True;
  CanPaste := True;
  MainMenu1Click(Self);
end;  //BtnCutClick

procedure TForm1.BtnCopyClick(Sender: TObject);
begin
  if LsFileListview281.Focused then
    LsFileListview281.CutCopy(0);
//  BtnCopy.Enabled := False;
  BtnPaste.Enabled := True;
  CanPaste := True;
  MainMenu1Click(Self);
end;  //BtnCopyClick

procedure TForm1.BtnPasteClick(Sender: TObject);
begin
  if LsFileListview281.Focused then
    LsFileListview281.Paste;
  BtnPaste.Enabled := False;
  CanPaste := False;
  BtnCut.Enabled := Assigned(LsFileListview281.Selected);
  BtnCopy.Enabled := Assigned(LsFileListview281.Selected);
  MainMenu1Click(Self);
end;  //BtnPasteClick

procedure TForm1.LsFilelistView281MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetStatistics(Sender);
  if (CanPaste = False) then
    ToolBar2Click(Self);
end;  //LsFilelistView281MouseUp

procedure TForm1.Btn1LevelUpClick(Sender: TObject);
begin
  LsFilelistView281.OneLevelUp;
  GetStatistics(Sender);
  ToolBar2Click(Self);  //876
end;  //Btn1LevelUpClick

procedure TForm1.MaskCombo1Change(Sender: TObject);
begin
  LsFilelistView281.Mask := MaskCombo1.Text;
end;  //MaskCombo1Change

procedure TForm1.Find1Click(Sender: TObject);
begin
  LsFilelistView281.FindFile;
end;  //Find1Click

procedure TForm1.GoTo1Click(Sender: TObject);
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
    LsDirTreeCombo281.OpenPath(PathName);
  end
  else begin
    if (Length(PathName) <= 3) and (PathName[2] = ':') then
      MessageDlg(UpperCase(PathName) + '  not ready', mtError, [mbOK], 0)
    else
      MessageDlg(PathName + '  not found', mtError, [mbOK], 0);
    exit;
  end;
end;  //GoTo1Click

procedure TForm1.ViewStyleClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    case tag of
      1: LsFilelistView281.ViewStyle := vsIcon;
      2: LsFilelistView281.ViewStyle := vsSmallIcon;
      3: LsFilelistView281.ViewStyle := vsList;
      4: LsFilelistView281.ViewStyle := vsReport;
    end;
  end;
end;  //ViewStyleClick

procedure TForm1.ViewStyleMenuClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    case tag of
      21: LsFilelistView281.ViewStyle := vsIcon;
      22: LsFilelistView281.ViewStyle := vsSmallIcon;
      23: LsFilelistView281.ViewStyle := vsList;
      24: LsFilelistView281.ViewStyle := vsReport;
    end;
  end;
end;  //ViewStyleMenuClick

procedure TForm1.BtnSelectViewStylesClick(Sender: TObject);
begin
  with LsFileListView281 do
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
end;  //BtnSelectViewStylesClick

procedure TForm1.LsFilelistView281KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Return) or (key = VK_Space) or (Key = VK_BACK) then
  begin
    GetStatistics(Sender);
  end;
end;  //LsFilelistView281KeyUp

procedure TForm1.BtnRefreshClick(Sender: TObject);
var
  TreePath: string;
begin
  with LsDirTreeCombo281 do
  begin
    if (SelectedPath <> '') then
      TreePath := SelectedPath;
    ResetTreeView;
    OpenPath(TreePath);
  end;
  LsFilelistView281.Directory := TreePath;
//  LsFilelistView281.UpdateFileList;
end;  //BtnRefreshClick

procedure TForm1.ConnecttoNetwork1Click(Sender: TObject);
begin
  LsDirTreeCombo281.ConnectNetResource(Sender);
{*
var
  Drives: set of 0..25;
  ADrv: integer;
  DrvLtr: Char;
begin
  DrvLtr := ' ';
  Integer(Drives) := GetLogicalDrives;
  for ADrv := 0 to 25 do
    if ADrv in Drives then
      DrvLtr := Chr(ADrv + Ord('B'));
  if WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) =
    NO_ERROR then
  begin
    LsDirTreeCombo281.SelectedPath := (DrvLtr + ':\');
    BtnRefreshClick(Sender);
  end;
*}
end;  //ConnecttoNetwork1Click

procedure TForm1.DisconnectfromNetwork1Click(Sender: TObject);
begin
  LsDirTreeCombo281.DisconnectNetResource(Sender);
  {*
  WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK);
  LsDirTreeCombo281.SelectedPath := 'Drives';
  BtnRefreshClick(Sender);
  LsDirTreeCombo281.ExpandRoot := True;
  *}
end;  //DisconnectfromNetwork1Click

procedure TForm1.BtnMapDrvClick(Sender: TObject);
begin
  LsDirTreeCombo281.ConnectNetResource(Sender);
end;  //BtnMapDrvClick

procedure TForm1.BtnDisconnectClick(Sender: TObject);
begin
  LsDirTreeCombo281.DisconnectNetResource(Sender);
end;  //BtnDisconnectClick

procedure TForm1.BtnShowFolderClick(Sender: TObject);
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
end;  //ShowFolders

procedure TForm1.BtnHideFileExtClick(Sender: TObject);
begin
  with LsFileListView281 do
  begin
    if HideFileExt = True then
    begin
      HideFileExt := False;
      BtnHideFileExt.Down := False;
      HideFileExtensions1.Checked := False;
    end
    else begin
      HideFileExt := True;
      BtnHideFileExt.Down := True;
      HideFileExtensions1.Checked := True;
    end;
    UpdateFileList;
  end;
end;  //BtnHideFileExtClick

procedure TForm1.LanguageClick(Sender: TObject);
var
  CurDir: String;
begin
  CurDir := GetCurrentDir;
  with Sender as TMenuItem do
  begin
    case Tag of
      0:   LsFileListView281.Language := lnEnglish;
      1:   LsFileListView281.Language := lnChinese_Tra;
      2:   LsFileListView281.Language := lnChinese_Sim;
      3:   LsFileListView281.Language := lnDutch;
      4:   LsFileListView281.Language := lnFrench;
      5:   LsFileListView281.Language := lnGerman;
      6:   LsFileListView281.Language := lnItalian;
      7:   LsFileListView281.Language := lnJapanese;
      8:   LsFileListView281.Language := lnKorean;
      9:   LsFileListView281.Language := lnPolish;
     10:   LsFileListView281.Language := lnPortuguese;
     11:   LsFileListView281.Language := lnSlovak;
     12:   LsFileListView281.Language := lnSlovenian;
     13:   LsFileListView281.Language := lnSpanish;
     14:   LsFileListView281.Language := lnSwedish;
     15:   LsFileListView281.Language := lnTurkish;
     16:   LsFileListView281.Language := lnSysDefault;
    end;
    Checked := True;
  end;
  LsDirTreeCombo281.OpenPath(CurDir);
  LsFileListView281.Directory := CurDir;
  LsFileListView281.UpdateFileList;
end;  //LanguageClick

procedure TForm1.Close1Click(Sender: TObject);
begin
  Close;
end;  //Close1Click

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
  Close;
end;  //BtnCloseClick

procedure TForm1.FormatDiskette1Click(Sender: TObject);   //880>
begin
  if not LsFileExplorer28.DiskinDrive('A', 0) then
    MessageDlg('Insert a disk into Drive A:' + #13 +
               'and click OK when ready ...' , mtError, [mbOK], 0);
  SHFormatDrive(Application.Handle, 0, 0, 0);
end;  //880<

procedure TForm1.DOSCommandPrompt1Click(Sender: TObject);
var
  WinPath: string;
begin
  WinPath := LsFileExplorer28.GetSystemPath(spWinRoot) + '\System32';
  LsFileExplorer28.ExecuteFile('Open', 'Command.com', '', WinPath, SW_SHOW);
end;

procedure TForm1.BtnTextColorClick(Sender: TObject);
begin
  with LsFileListview281 do
  begin
    if LItemColorByATTR = False then
    begin
      LItemColorByATTR := True;
      BtnTextColor.down := True;
    end
    else if LItemColorByATTR = True then
    begin
      LItemColorByATTR := False;
      BtnTextColor.down := False;
    end;
    LsFileListview281.UpdateFileList;
  end;
end;





end.

