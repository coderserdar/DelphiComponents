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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: ExMain.PAS 2.57                           *}
{*********************************************************}
{* XMLPartner: XML Editor Main Window                    *}
{*********************************************************}
unit ExMain;

{$I XpDefine.inc}
{$IFNDEF DCC4OrLater}
!! Error: This example does not compile in Delphi 3.
{$ENDIF}

interface

uses
{$IFDEF WIN32}
  Windows,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Dialogs,
  Buttons,
  Messages,
  ExtCtrls,
  ComCtrls,
  ToolWin,
  Menus,
{$ENDIF}
{$IFDEF LINUX}
  QMenus,
  QDialogs,
  QImgList,
  QTypes,
  QComCtrls,
  QControls,
  QExtCtrls,
  QForms,
  QClipbrd,
{$ENDIF}
  {$IFDEF Delphi4orLater}
  ImgList,
  {$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  XpBase,
  XpDOM;

type
{$IFDEF XPDPRO}
  TXpStyleAction = (xpsaOpen, xpsaClose, xpsaSaveAs);
  
{$ENDIF}
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileClose: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    mnuWindowCascade: TMenuItem;
    mnuWindowTileH: TMenuItem;
    mnuWindowArrange: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    mnuWindowMinimize: TMenuItem;
    StatusBar: TStatusBar;
    mnuWindowTileV: TMenuItem;
    ToolBar2: TToolBar;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    ToolButton3: TToolButton;
    btnCut: TToolButton;
    btnCopy: TToolButton;
    btnPaste: TToolButton;
    btnNew: TToolButton;
    ToolButton7: TToolButton;
    btnCascade: TToolButton;
    btnTileHorz: TToolButton;
    btnTileVert: TToolButton;
    ImageList1: TImageList;
    SaveDialog: TSaveDialog;
    S1: TMenuItem;
    mnuFilePref: TMenuItem;
    mnuFileOpenURL: TMenuItem;
    mnuFileSaveURL: TMenuItem;
    N3: TMenuItem;
    Recent1: TMenuItem;
    Recent2: TMenuItem;
    Recent3: TMenuItem;
    Recent4: TMenuItem;
    Recent5: TMenuItem;
    CopyWOChildrenItem: TMenuItem;
    XslPopupMenu: TPopupMenu;
    XslInfo: TXpObjModel;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure mnuFilePrefClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCut1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure mnuFileOpenURLClick(Sender: TObject);
    procedure mnuFileSaveURLClick(Sender: TObject);
    procedure Recent1Click(Sender: TObject);
    procedure EditCopyWithoutChildrenExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveClick(Sender: TObject);
    procedure mnuWindowTileHClick(Sender: TObject);
    procedure mnuWindowTileVClick(Sender: TObject);
    procedure mnuWindowCascadeClick(Sender: TObject);
    procedure mnuWindowArrangeClick(Sender: TObject);
    procedure mnuWindowMinimizeClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
  private
    { Private declarations }
    FOptions: TINIFile;

    procedure ActiveMDIorChild(const sCaption, sUserID, sPassword : string);
    procedure CreateMDIChild(const sName: string; sUserId: string; sPassword: string);
    procedure InitMDIChild(aChild : TForm);
    procedure MDIChildNormal;
{$IFDEF XPDPRO}
    procedure OnXMLChildClose(Sender : TObject);
{$ENDIF}
    function  OnGetXSLDocElementEvent(Sender : TObject) : TXpElement;
    function  OnMDIChildCountEvent(Sender : TObject) : Integer;
    procedure OnNextChildEvent(Sender: TObject);
    procedure OnPrevChildEvent(Sender: TObject);
    procedure OnSetCtrlStatesEvent(Sender : TObject);
    procedure SetCtrlStates;
{$IFDEF XPDPRO}
    procedure StylesheetNotify(const sName1, sName2 : string;
                                     oChildWin : TForm;
                                     eAction : TXpStyleAction);
{$ENDIF}
    procedure UpdateRecents(bChange: Boolean; sFile: string);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
{$IFDEF WIN32}
  Clipbrd,
  XpAboutW,
{$ENDIF}
{$IFDEF LINUX}
  XpAbout,
{$ENDIF}
  ExChildW,
  ExPrefs,
  ExURL,
  ExSelAtt,
  ExUtil,
  ExErr;

const
{$IFDEF WIN32}
  csINIFile = 'EXML.INI';
{$ENDIF}
{$IFDEF LINUX}
  csINIFile = '.exmlrc';
{$ENDIF}
  csAppSection = 'Application';
  csPrefSection = 'Preferences';
  csSaveAppWinPos = 'SaveAppWinPos';
  csSectionRecent = 'RecentFiles';
  csFormatted = 'Formatted';
  csNormalize = 'Normalize';

function MakeFilePath(sPath, sFile: String): String;
begin
  if Length(sPath) > 0 then begin
    if sPath[Length(sPath)] = '\' then
      Result := sPath + sFile
    else
      Result := sPath + '\' + sFile;
  end
  else
    Result := '.\' + sFile;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i : Integer;
{$IFDEF LINUX}
  FileName : string;
{$ENDIF}
begin
  { Set window position }
  try
{$IFDEF WIN32}
    FOptions := TINIFile.Create(MakeFilePath(ExtractFilePath(
                  Application.EXEName), csINIFile));
{$ENDIF}
{$IFDEF LINUX}
    { MakeFilePath in linux prepends the file name with a \ an escape character
      in linux }
    FileName := ExtractFilePath(Application.EXEName) + csINIFile;
    FOptions := TINIFile.Create(FileName);
{$ENDIF}
    with FOptions do begin
      if ReadBool(csPrefSection, csSaveAppWinPos, True) then begin
        RestoreFormState(Self, FOptions, csAppSection);
        if Width < 200 then
          Width := 200;
        if Height < 200 then
          Height := 200;
      end;
    end;
  except
    { Do nothing on exception. }
  end;

  for i := 1 to ParamCount do begin
    if FileExists(ParamStr(i)) then
      CreateMDIChild(ParamStr(i), '', '');
  end;
  UpdateRecents(false, '');
  SetCtrlStates;
{$IFDEF XPDPRO}
  Caption := 'EXMLPro Editor';
{$ELSE}
  Caption := 'EXML Editor';
{$ENDIF}
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  anInx : Integer;
begin
  if assigned(FOptions) then
    SaveFormState(Self, FOptions, csAppSection);
  FOptions.Free;
  for anInx := 0 to pred(MDIChildCount) do
    MDIChildren[anInx].Close;
end;

procedure TMainForm.InitMDIChild(aChild : TForm);
begin
  with TXmlChild(aChild) do begin
    if assigned(FOptions) then begin
      FormattedOutput := FOptions.ReadBool(csPrefSection, csFormatted, True);
      NormalizeData := FOptions.ReadBool(csPrefSection, csNormalize, True);
    end;
    INIFIle := FOptions;
    OnGetXSLDocElement := OnGetXSLDocElementEvent;
    OnMDIChildCount := OnMDIChildCountEvent;
    OnNextChild := OnNextChildEvent;
    OnPrevChild := OnPrevChildEvent;
    OnSaveAs := FileSaveAs1Execute;
    OnSetCtrlStates := OnSetCtrlStatesEvent;
{$IFDEF XPDPRO}
    OnClosing := OnXMLChildClose;
{$ENDIF}
  end;
end;

{$IFDEF XPDPRO}
procedure TMainForm.OnXMLChildClose(Sender : TObject);
var
  oChild : TXmlChild;
begin
  oChild := TXmlChild(Sender);
  if Lowercase(ExtractFileExt(oChild.Filename)) = '.xsl' then
    StylesheetNotify(oChild.Filename, '', oChild, xpsaClose);
end;
{$ENDIF}

procedure TMainForm.CreateMDIChild(const sName: string;
                                         sUserId: string;
                                         sPassword: string);
var
  Child: TXmlChild;
{$IFDEF XPDPRO}
  Child2 : TXmlChild;
  Inx : Integer;
{$ENDIF}
  HasErrors : Boolean;
begin
  { create a new MDI child window }
  Child := TXmlChild.Create(Application);
  InitMDIChild(Child);
  with Child do begin
    HasErrors := not Child.LoadDataSource(sName, sUserId, sPassword);
    UpdateRecents(true, sName);
    if MDIChildCount = 1 then
      Child.WindowState := wsMaximized;
    if HasErrors and (Child.Errors.Count > 0) then
      with TfrmErrors.Create(Self) do
        try
          Errors := Child.Errors;
          INIFile := FOptions;
          Caption := format('Load errors for %s',[QuotedStr(sName)]);
          ShowModal;
        finally
          Free;
        end;
  end;
{$IFDEF XPDPRO}
  if Lowercase(ExtractFileExt(sName)) = '.xsl' then
    StylesheetNotify(sName, '', Child, xpsaOpen)
  else if MDIChildCount > 1 then
    { If any stylesheets are open, make sure the new document sees them. }
    for Inx := 0 to Pred(MDIChildCount) do begin
      Child2 := TXMLChild(MDIChildren[Inx]);
      if (Child2 <> Child) and
         (Lowercase(ExtractFileExt(Child2.Filename)) = '.xsl') then
        Child.AddStylesheet(ExtractFileName(Child2.Filename), Child2);
    end;
{$ENDIF}
  SetCtrlStates;
end;

procedure TMainForm.FileNew1Execute(Sender: TObject);
var
  Child: TXmlChild;
begin
  Child := TXmlChild.Create(Application);
  InitMDIChild(Child);
  Child.Caption := 'Untitled Document ' + IntToStr(MDIChildCount);
  Child.NewDocument;
  if MDIChildCount = 1 then
    Child.WindowState := wsMaximized;
end;

procedure TMainForm.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute then
    ActiveMDIOrChild(OpenDialog.FileName, '', '');
end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
begin
  XpAboutBox.ShowModal;
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

{ TMainForm.UpdateRecents - Updates the recently opened files list }
procedure TMainForm.UpdateRecents(bChange: Boolean; sFile: String);
var
  i, j: integer;
begin
  if bChange then begin
    { Check to see if project is in list yet }
    for i:= 1 to 5 do begin
      if sFile = FOptions.ReadString(csSectionRecent, 'File' + IntToStr(i), '') then
        break;
    end;
    if i > 5 then
      i := 5;

    { If project is not in list then add it to the top }
    for j := i downto 2 do
      FOptions.WriteString(csSectionRecent, 'File' + IntToStr(j),
                            FOptions.ReadString(csSectionRecent,
                                                 'File' + IntToStr(j - 1), ''));
    FOptions.WriteString(csSectionRecent, 'File1', sFile);
{$IFDEF LINUX}
    FOptions.UpdateFile;
{$ENDIF}
  end;


  { Set captions for menu items }
  Recent1.Caption := '&1. ' + FOptions.ReadString(csSectionRecent, 'File1', '');
  Recent2.Caption := '&2. ' + FOptions.ReadString(csSectionRecent, 'File2', '');
  Recent3.Caption := '&3. ' + FOptions.ReadString(csSectionRecent, 'File3', '');
  Recent4.Caption := '&4. ' + FOptions.ReadString(csSectionRecent, 'File4', '');
  Recent5.Caption := '&5. ' + FOptions.ReadString(csSectionRecent, 'File5', '');

  { Determine which items are visible }
  Recent1.Visible := Length(Recent1.Caption) > 4;
  Recent2.Visible := Length(Recent2.Caption) > 4;
  Recent3.Visible := Length(Recent3.Caption) > 4;
  Recent4.Visible := Length(Recent4.Caption) > 4;
  Recent5.Visible := Length(Recent5.Caption) > 4;
  S1.Visible := Recent1.Visible;
end;

procedure TMainForm.FileSaveAs1Execute(Sender: TObject);
var
  oChild : TXmlChild;
{$IFDEF XPDPRO}
  sOldName : string;
{$ENDIF}
begin
  oChild := TXmlChild(ActiveMDIChild);
  SaveDialog.InitialDir := ExtractFilePath(oChild.FileName);
{$IFDEF XPDPRO}
  sOldName := oChild.FileName;
{$ENDIF}
  if SaveDialog.Execute then begin
    if Pos('.', SaveDialog.FileName) = 0 then
      SaveDialog.FileName := SaveDialog.FileName + '.xml';
    oChild.SaveFile(SaveDialog.FileName);
{$IFDEF XPDPRO}
    if Lowercase(ExtractFileExt(SaveDialog.FileName)) = '.xsl' then
      StylesheetNotify(sOldName, oChild.FileName, oChild, xpsaSaveAs);
{$ENDIF}
  end;
end;

procedure TMainForm.mnuFilePrefClick(Sender: TObject);
begin
  with TPrefsForm.Create(Self) do
    try
      INIFile := FOptions;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.EditCopy1Execute(Sender: TObject);
begin
{$IFDEF LINUX}
  if not assigned(ActiveMDIChild)then begin
    SetCtrlStates;
    exit;
  end;
{$ENDIF}
  TXmlChild(ActiveMDIChild).CopyToClipboard;
{$IFDEF LINUX}
  SetCtrlStates;
{$ENDIF}
end;

procedure TMainForm.EditCut1Execute(Sender: TObject);
begin
{$IFDEF LINUX}
  if not assigned(ActiveMDIChild)then begin
    SetCtrlStates;
    exit;
  end;
{$ENDIF}
  TXmlChild(ActiveMDIChild).CutToClipboard;
end;

procedure TMainForm.EditPaste1Execute(Sender: TObject);
begin
{$IFDEF LINUX}
  if not assigned(ActiveMDIChild)then begin
    SetCtrlStates;
    exit;
  end;
{$ENDIF}
  TXmlChild(ActiveMDIChild).PasteFromClipboard;
end;

procedure TMainForm.mnuFileOpenURLClick(Sender: TObject);
begin
  with TURLForm.Create(Application) do
    try
      if ShowModal = mrOk then
        ActiveMDIorChild(URL, FTPUser, FTPPassword);
    finally
      Free;
    end;
end;

procedure TMainForm.mnuFileSaveURLClick(Sender: TObject);
var                                                                    {!!.51}
  aForm : TXmlChild;                                                   {!!.51}
begin
  aForm := TXmlChild(ActiveMDIChild);                                  {!!.51}
  with TURLForm.Create(Application) do
    try
      if ShowModal = mrOk then
        if not aForm.SaveToURL(URL, FTPUser, FTPPassword) then         {!!.51}
          MessageDlg('Unable to save to URL.  Be sure to save a local copy!',
                     mtError, [mbOk], 0);
    finally
      Free;
    end;
end;

procedure TMainForm.Recent1Click(Sender: TObject);
begin
  ActiveMDIOrChild(Copy(TMenuItem(Sender).Caption, 5, 255), '', '');
end;

procedure TMainForm.EditCopyWithoutChildrenExecute(Sender: TObject);
begin
  TXmlChild(ActiveMDIChild).CopyToClipboard(false);
end;

procedure TMainForm.OnSetCtrlStatesEvent(Sender : TObject);
begin
  SetCtrlStates;
end;

procedure TMainForm.SetCtrlStates;
var
  ChildWin : TXMLChild;
begin

  ChildWin := TXMLChild(ActiveMDIChild);

  { File->Close }
  mnuFileClose.Enabled := (ChildWin <> nil);

  { File->Save }
  btnSave.Enabled := (ChildWin <> nil) and
                     ChildWin.IsChanged;
  mnuFileSave.Enabled := btnSave.Enabled;

  { File->SaveAs }
  mnuFileSaveAs.Enabled := (ChildWin <> nil);

  { File->SaveToURL }
  mnuFileSaveURL.Enabled := mnuFileSaveAs.Enabled;

  { Edit->Copy }
  btnCopy.Enabled := (ChildWin <> nil);
  CopyItem.Enabled := btnCopy.Enabled;


  { Edit->Copy without children }
{$IFDEF LINUX}
  if assigned(ChildWin)then
{$ENDIF}
    CopyWOChildrenItem.Enabled := (ChildWin <> nil) and
                                  (ChildWin.TreeView.Selected <> nil) and
                                  (ChildWin.PageControl.ActivePage = ChildWin.StructTab);

  { Edit->Cut }
  btnCut.Enabled := (ChildWin <> nil) and
                    ((ChildWin.TreeView.Selected <> nil) or
                     (ChildWin.PageControl.ActivePage = ChildWin.SourceTab));

  CutItem.Enabled := btnCut.Enabled;

  { Edit->Paste }
{$IFDEF WIN32}
  btnPaste.Enabled := (ChildWin <> nil) and Clipboard.HasFormat(CF_TEXT);
{$ENDIF}
{$IFDEF LINUX}
  btnPaste.Enabled := (ChildWin <> nil) and (Clipboard.AsText <> '');
{$ENDIF}
  PasteItem.Enabled := btnPaste.Enabled;

end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
{$IFDEF LINUX}
  if not assigned(ActiveMDIChild)then begin
    SetCtrlStates;
    exit;
  end;
{$ENDIF}
  if TXmlChild(ActiveMDIChild).Filename = '' then
    FileSaveAs1Execute(Self)
  else
    TXmlChild(ActiveMDIChild).SaveFile;
end;

procedure TMainForm.mnuWindowTileHClick(Sender: TObject);
begin
  MDIChildNormal;
{$IFDEF WIN32}
  TileMode := tbHorizontal;
{$ENDIF}
  Tile;
end;

procedure TMainForm.ActiveMDIorChild(const sCaption, sUserID, sPassword : string);
var
  anInx : Integer;
  oChild : TForm;
  slCaption : string;
begin
{$IFDEF WIN32}
  slCaption := LowerCase(sCaption);
{$ENDIF}
{$IFDEF LINUX}
  slCaption := sCaption;
{$ENDIF}
  oChild := nil;
  { Is the file already loaded into a MDI child? }
  for anInx := 0 to pred(MDIChildCount) do
    if MDIChildren[anInx].Caption = slCaption then begin
      oChild := MDIChildren[anInx];
      break;
    end;

  if oChild <> nil then
    oChild.BringToFront
  else
    CreateMDIChild(slCaption, sUserID, sPassword);
end;

procedure TMainForm.MDIChildNormal;
var
  anInx : Integer;
begin
  for anInx := 0 to pred(MDIChildCount) do
    MDIChildren[anInx].WindowState := wsNormal;
end;

procedure TMainForm.mnuWindowTileVClick(Sender: TObject);
begin
  MDIChildNormal;
{$IFDEF WIN32}
  TileMode := tbVertical;
{$ENDIF}
  Tile;
end;

procedure TMainForm.mnuWindowCascadeClick(Sender: TObject);
begin
  MDIChildNormal;
  Cascade;
end;

procedure TMainForm.mnuWindowArrangeClick(Sender: TObject);
begin
  MDIChildNormal;
{$IFDEF WIN32}
  ArrangeIcons;
{$ENDIF}
{$IFDEF LINUX}
  ShowMessage( 'ArrangeIcons does not work in Kylix' );
{$ENDIF}
end;

procedure TMainForm.mnuWindowMinimizeClick(Sender: TObject);
var
  anInx : Integer;
begin
  for anInx := pred(MDIChildCount) downto 0 do
    MDIChildren[anInx].WindowState := wsMinimized;
end;

function TMainForm.OnGetXSLDocElementEvent(Sender : TObject) : TXpElement;
begin
  Result := XSLInfo.Document.DocumentElement;
end;

function TMainForm.OnMDIChildCountEvent(Sender : TObject) : Integer;
begin
  Result := MDIChildCount;
end;

procedure TMainForm.OnNextChildEvent(Sender: TObject);
var
  i, j: Integer;
begin
  for i := MDIChildCount - 1 downto 0 do
    if MDIChildren[i] = TForm(Sender) then begin
      j := i - 1;
      if j < 0 then
        j := MDIChildCount - 1;
      MDIChildren[j].BringToFront;
      exit;
    end;
end;

procedure TMainForm.OnPrevChildEvent(Sender: TObject);
var
  i, j: Integer;
begin
  for i := 0 to MDIChildCount - 1 do
    if MDIChildren[i] = TForm(Sender) then
    begin
      j := i + 1;
      if j >= MDIChildCount then
        j := 0;
      MDIChildren[j].BringToFront;
      exit;
    end;
end;

procedure TMainForm.mnuFileCloseClick(Sender: TObject);
{$IFDEF XPDPRO}
var
  oChild : TXmlChild;
{$ENDIF}
begin
{$IFDEF XPDPRO}
  oChild := TXmlChild(ActiveMDIChild);
  if Lowercase(ExtractFileExt(oChild.FileName)) = '.xsl' then
    StylesheetNotify(oChild.FileName, '', oChild, xpsaClose);
{$ENDIF}
  ActiveMDIChild.Close;
end;

procedure TMainForm.Edit1Click(Sender: TObject);
begin
  SetCtrlStates;
end;

{$IFDEF XPDPRO}
procedure TMainForm.StylesheetNotify(const sName1, sName2 : string;
                                           oChildWin : TForm;
                                           eAction : TXpStyleAction);
var
  oForm : TXmlChild;
  wInx : Integer;
begin
  for wInx := 0 to pred(MDIChildCount) do begin
    oForm := TXmlChild(MDIChildren[wInx]);
    if oForm <> oChildWin then
      case eAction of
        xpsaOpen :
          oForm.AddStylesheet(ExtractFileName(sName1), oChildWin);
        xpsaClose :
          oForm.RemoveStylesheet(ExtractFileName(sName1));
        xpsaSaveAs :
          oForm.RenameStylesheet(ExtractFileName(sName1),
                                 ExtractFileName(sName2));
      end;  { case }
  end; { for }
end;
{$ENDIF}

end.
