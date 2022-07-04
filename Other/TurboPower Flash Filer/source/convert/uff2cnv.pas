{*********************************************************}
{* FlashFiler: Application used to convert FF1 tables to *}
{* FF2 tables.                                           *}
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

unit uFF2Cnv;

{$I FFDEFINE.INC}                                                      {!!.01}

{ NOTE: The following define kills a warning in Delphi6. }             {!!.06}
{$IFDEF DCC6OrLater}                                                   {!!.06}
{$WARN UNIT_PLATFORM OFF}                                              {!!.06}
{$ENDIF}                                                               {!!.06}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl,
  StdCtrls, ComCtrls, ExtCtrls, FFConvrt, FFLLBase, fflleng,
  ffsrIntm, FFSrEng, FFLLLog, uFFNet, FFLLComp,
  {$IFDEF DCC4OrLater}
  ImgList,
  {$ENDIF}
  ToolWin, Menus;

type
  TfrmFF2Conv = class(TForm)
    pnlStatBars: TPanel;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    pnlSrcTgt: TPanel;
    gbSource: TGroupBox;
    srcFiles: TFileListBox;
    gbDest: TGroupBox;
    pnlStatusView: TPanel;
    lvStatus: TListView;
    splSplitter: TSplitter;
    pnlSrcDriveDir: TPanel;
    srcDirectory: TDirectoryListBox;
    pnlSrcDrive: TPanel;
    srcDrive: TDriveComboBox;
    pnlTgtDrvDir: TPanel;
    tgtDirectory: TDirectoryListBox;
    pnlTgtDrive: TPanel;
    tgtFiles: TFileListBox;
    tgtDrive: TDriveComboBox;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    ToolBar1: TToolBar;
    btnExecute: TToolButton;
    imMain: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    mnuFileSep: TMenuItem;
    mnuFileConvert: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    procedure btnConvertClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetControls(aIsConverting : Boolean);
    function  GetSourceDirectory : string;
    function  GetSourceDrive : char;
    function  GetTableSize(aFile : string) : string;
    function  GetTargetDirectory : string;
    function  GetTargetDrive : char;
    procedure SetSourceDirectory(const aDirectory : string);
    procedure SetSourceDrive(aDrive : char);
    procedure SetTargetDirectory(const aDirectory : string);
    procedure SetTargetDrive(aDrive : char);
    procedure srcDriveChange(Sender : TObject);
    procedure tgtDriveChange(Sender : TObject);
    procedure mnuFileExitClick(Sender : TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure BeforeConvert(aSender : TffDataConverter);
    function  CheckForOverwrites : Boolean;
      {Check if a user is overwritting files in the destination}
    procedure OnCancel(aSender : TffDataConverter);
    procedure OnComplete(aSender : TffDataConverter);
    procedure OnProgress(aSender : TffDataConverter);
    procedure OnNetBios(aSender   : TffDataConverter;
                    var aCanceled : Boolean;
                    var aOptions  : TffProtOptions);
    property SourceDirectory : string
               read GetSourceDirectory
               write SetSourceDirectory;
    property SourceDrive : char
               read GetSourceDrive
               write SetSourceDrive;
    property TargetDirectory : string
               read GetTargetDirectory
               write SetTargetDirectory;
    property TargetDrive : char
               read GetTargetDrive
               write SetTargetDrive;
  end;

var
  frmFF2Conv     : TfrmFF2Conv;
  TableConverter : TffDataConverter;
  ServerEngine   : TffServerEngine;
  StartTime      : DWord;
  CurrentTable   : Integer;
  SelTableCount  : Integer;
  Canceled       : Boolean;

implementation

uses
  FFLLWsck, FFAbout;

const
  cnExecute = 0;
  cnCancel = 1;
  UpdateFrequency = 100;

{$R *.DFM}

{====================================================================}
procedure TfrmFF2Conv.BeforeConvert(aSender : TffDataConverter);
var
  TotalRecords : TffWord32;
begin
  TotalRecords := aSender.TotalRecords;

  {setup the status bar and progress bar}
  StatusBar.Panels[1].Text := 'Adding records';
  StatusBar.Panels[2].Text := 'Record 0 of ' +
                              FFCommaizeChL(TotalRecords, ThousandSeparator);
  ProgressBar.Position := 0;
  {initialize our progress bar not that we can get total records from
   the converter}
  ProgressBar.Min := 0;
  ProgressBar.Max := TotalRecords;
  if TotalRecords <> 0 then
    ProgressBar.Step := UpdateFrequency
  else
    ProgressBar.Step := TotalRecords;
  Application.ProcessMessages;
end;
{--------}
procedure TfrmFF2Conv.btnConvertClick(Sender : TObject);
var
  ListItem   : TListItem;
  SourceFile : string;
  TargetDir  : string;
  i          : Integer;
begin
  {if the Convert button has been changed to a Cancel then we need to
   cancel the current conversion.}
  if btnExecute.ImageIndex = cnCancel then begin
    {tell the converter that we're canceling}
    TableConverter.Cancel;
    Canceled := True;
    Application.ProcessMessages;
    SetControls(False);
    exit;
  end;
  Canceled := False;
  SetControls(True);
  {Ensure we are not overwriting any tables that the user doesn't want
   overwritten. If this isn't a problem, continue.}
  if CheckForOverwrites then begin
    {make an entry for each selected table in the status view}
    lvStatus.Items.Clear;
    for i := 0 to pred(srcFiles.Items.Count) do begin
      if srcFiles.Selected[i] then begin
        ListItem := lvStatus.Items.Add;
        ListItem.Caption := srcFiles.Items[i];
        ListItem.SubItems.Add('0');
        SourceFile := srcDirectory.Directory + '\' + srcFiles.Items[i];
        ListItem.SubItems.Add(GetTableSize(SourceFile));
        ListItem.SubItems.Add('...');
        ListItem.SubItems.Add('...');
        ListItem.SubItems.Add('Queued');
      end;
    end;
    SelTableCount := srcFiles.SelCount;
    TargetDir := tgtDirectory.Directory;
    CurrentTable := -1;
    i := -1;
    while ((i <  pred(srcFiles.Items.Count)) and (not Canceled)) do begin
      inc(i);
      if srcFiles.Selected[i] then begin
        inc(CurrentTable);
        {change the status of the table about to be converted}
        lvStatus.Items[CurrentTable].SubItems[4] := 'Converting data';
        {update the status bar}
        StatusBar.Panels[0].Text := format('Table %d of %d in progress',
                                           [succ(CurrentTable), SelTableCount]);
        {build the complete path to the table we're updating}
        SourceFile := srcDirectory.Directory + '\' + srcFiles.Items[i];
        {convert the table}
        StartTime := GetTickCount;
        try
          TableConverter.Convert(SourceFile, TargetDir);
        except
          on E: Exception do begin
            lvStatus.Items[CurrentTable].SubItems[4] := 'FAILED';
            MessageDlg(format('ERROR: Unable to convert %s.' +  #13#10 +
                              '[%s]',
                              [lvStatus.Items[CurrentTable].Caption,
                               E.Message]),
                       mtError, [mbOK], 0);
            Break;                                                     {!!.07}
          end;
        end;
        {if the table is successfully converted, deselected it from
         the list of source files}
        srcFiles.Selected[i] := False;
        {update the list of target files so that it will show the new
         table}
        tgtFiles.Update;
      end;
    end;
  end;
  SetControls(False);
end;
{--------}
function TfrmFF2Conv.CheckForOverwrites : Boolean;
var
  i, k : Integer;
begin
  Result := True;
  {check if any of the selected files in srcFiles have the same name
   as any files in the destination directory.}
  for i := 0 to pred(srcFiles.Items.Count) do begin
    {is this srcFile selected?}
    if srcFiles.Selected[i] then
      {if selected, we need to check it against every file in the
       destination directory.}
      for k := 0 to pred(tgtFiles.Items.Count) do begin
        {if we find a match, ask the user if it's OK to overwrite the
         files in the destination directory.}
        if ChangeFileExt(srcFiles.Items[i], '.' + ffc_ExtForData) =    {!!.03}
           tgtFiles.Items[k] then begin                                {!!.03}
          Result := MessageDlg('You are going to overwrite tables ' +
                               'in your destination directory. '  +
                               'Continue?', mtWarning,
                               [mbYes, mbNo], 0) = mrYes;
          exit; {we only want to ask once}
        end;
      end;
  end;
end;
{--------}
procedure TfrmFF2Conv.FormCloseQuery(Sender   : TObject;
                                 var CanClose : Boolean);
begin
  {Clean up before we close}
  srcFiles.Items.Clear;
  {call ConvertClick}
  SetControls(True);
  btnConvertClick(self);
  {when it completes (btnConvert.Caption = &Convert) we can close}
  while btnExecute.ImageIndex = cnCancel do
    CanClose := False;
  CanClose := True;
end;
{--------}
procedure TfrmFF2Conv.FormCreate(Sender : TObject);
begin
  {startup our server engine}
  ServerEngine := TffServerEngine.Create(self);
  ServerEngine.Configuration.GeneralInfo.giNoAutoSaveCfg := True;
  ServerEngine.State := ffesStarted;
  {setup our table converter and its events}
  TableConverter := TffDataConverter.Create(ServerEngine);
  TableConverter.ProgressFrequency := UpdateFrequency;
  {Give ourself a 5 meg buffer on the FF2 server}
  TableConverter.BufferSize := 1024 * 1024;
  TableConverter.BeforeConvert := BeforeConvert;
  TableConverter.OnCancel := OnCancel;
  TableConverter.OnComplete := OnComplete;
  TableConverter.OnProgress := OnProgress;
  TableConverter.OnNetBIOS := OnNetBIOS;
end;
{--------}
procedure TfrmFF2Conv.FormDestroy(Sender : TObject);
begin
  TableConverter.Free;
  ServerEngine.State := ffesShuttingDown;
  ServerEngine.Free;
end;
{--------}
procedure TfrmFF2Conv.FormShow(Sender : TObject);
begin
  srcDrive.SetFocus;
end;
{--------}
function TfrmFF2Conv.GetSourceDirectory : string;
begin
  Result := srcDirectory.Directory;
end;
{--------}
function TfrmFF2Conv.GetSourceDrive : Char;
begin
  Result := srcDrive.Drive;
end;
{--------}
function TfrmFF2Conv.GetTableSize(aFile : string) : string;
var
  {TheFile : file of Byte;}                                           {!!.01 Deleted}
  FileHandle : DWord;                                                 {!!.01 Added}
begin
  FileHandle := CreateFile(PChar(aFile),                              {!!.01 Start - Added}
                             GENERIC_READ,
                             0,
                             nil,
                             OPEN_EXISTING,
                             FILE_ATTRIBUTE_NORMAL,
                             0);
  try
    try
      Result := FFCommaizeChL(GetFileSize(FileHandle, nil), ThousandSeparator);
    except
      Result := '0';
    end;
  finally
    CloseHandle(FileHandle);
  end;                                                                {!!.01 End - Added}

                                                                      {!!.01 Start - Deleted}
{  AssignFile(TheFile, aFile);
  try
    Reset(TheFile);
    try
      Result := FFCommaizeChL(FileSize(TheFile), ThousandSeparator);
    finally
      CloseFile(TheFile);
    end;
  except
    MessageDlg('Unable to read source file', mtError, [mbOK], 0);
    Canceled := True;
    Result := '';
  end;}                                                               {!!.01 End - Deleted}
end;
{--------}
function TfrmFF2Conv.GetTargetDirectory : string;
begin
  Result := tgtDirectory.Directory;
end;
{--------}
function TfrmFF2Conv.GetTargetDrive : char;
begin
  Result := tgtDrive.Drive;
end;
{--------}
procedure TfrmFF2Conv.OnCancel(aSender : TffDataConverter);
var
  i : Integer;
begin
  if lvStatus.Items.Count > 0 then begin
    {update the status view}
    lvStatus.Items[CurrentTable].SubItems[4] := 'Aborted';
    for i := CurrentTable to pred(SelTableCount) do begin
      lvStatus.Items[i].SubItems[4] := 'Canceled';
    end;
    {update the progress bar}
    ProgressBar.Position := 0;
    {update the status bar}
    StatusBar.Panels[0].Text := format('Canceled on table %d of %d',
                                       [succ(CurrentTable), SelTableCount]);
    StatusBar.Panels[2].Text := 'CONVERSION WAS NOT SUCCESSFUL!';
  end;
  Canceled := True;
end;
{--------}
procedure TfrmFF2Conv.OnComplete(aSender : TffDataConverter);
var
  RecordsProcessed : Integer;
  TotalRecords     : Integer;
begin
  RecordsProcessed := aSender.RecordsProcessed;
  TotalRecords := aSender.TotalRecords;
  {update the status view}
  lvStatus.Items[CurrentTable].SubItems[3] :=
    FFCommaizeChL(GetTickCount - StartTime, ThousandSeparator);
  lvStatus.Items[CurrentTable].SubItems[0] := FFCommaizeChL(RecordsProcessed, ThousandSeparator) +
                                              ' of ' +
                                              FFCommaizeChL(TotalRecords, ThousandSeparator);
  lvStatus.Items[CurrentTable].SubItems[4] := 'Converted';
  {setup the status bar and progress bar}
  StatusBar.Panels[0].Text := format('Table %d of %d converted',
                                     [succ(CurrentTable), SelTableCount]);
  StatusBar.Panels[1].Text := format('%s converted',
                                     [ExtractFileName(aSender.Source)]);
  StatusBar.Panels[2].Text := FFCommaizeChL(RecordsProcessed, ThousandSeparator) +
                              ' Records converted';
  ProgressBar.Position := RecordsProcessed;
  {set total time}
  lvStatus.Items[CurrentTable].SubItems[3] :=
    FFCommaizeChL(GetTickCount - StartTime, ThousandSeparator);
  {set new file size}
  lvStatus.Items[CurrentTable].SubItems[2] := GetTableSize(aSender.Destination);
  {change status to Completed}
  lvStatus.Items[CurrentTable].SubItems[4] := 'Successfully completed';
  lvStatus.Items[CurrentTable].SubItems[0] := FFCommaizeChL(RecordsProcessed, ThousandSeparator) +
                                              ' of ' +
                                              FFCommaizeChL(TotalRecords, ThousandSeparator);
  Application.ProcessMessages;
end;
{--------}
procedure TfrmFF2Conv.OnProgress(aSender : TffDataConverter);
var
  RecordsProcessed : Integer;
  TotalRecords     : Integer;
begin
  RecordsProcessed := aSender.RecordsProcessed;
  TotalRecords := aSender.TotalRecords;
  {step the progress bar}
  StatusBar.Panels[2].Text := 'Record ' +
                              FFCommaizeChL(RecordsProcessed, ThousandSeparator) +
                              ' of ' +
                              FFCommaizeChL(TotalRecords, ThousandSeparator);
  {update records converted in status view}
  lvStatus.Items[CurrentTable].SubItems[0] := FFCommaizeChL(RecordsProcessed, ThousandSeparator) +
                                              ' of ' +
                                              FFCommaizeChL(TotalRecords, ThousandSeparator);
  ProgressBar.StepIt;
  Application.ProcessMessages;
end;
{--------}
procedure TfrmFF2Conv.OnNetBios(aSender   : TffDataConverter;
                            var aCanceled : Boolean;
                            var aOptions  : TffProtOptions);
var
  ProtForm : TfrmFFTransport;
begin
  { This only occurs when we are converting a system table that uses
    NetBIOS as the default protocol.  Since FlashFiler 2 doesn't
    support the NetBIOS protocol.  We are going to present the user a
    dialog box that lets the user choose a new protocol and options.}
  aCanceled := False;
  ProtForm := TfrmFFTransport.Create(self);
  try
    {setup the protocol form with the values given in aOptions}
    with ProtForm, aOptions do begin
      cbxSUEnabled.Checked := IsSingleUser;
      cbxIPXEnabled.Checked := IsIPXSPX;
      cbxIPXListen.Checked := IPXSPXLFB;
      cbxTCPEnabled.Checked := IsTCPIP;
      cbxTCPListen.Checked := TCPIPLFB;
      edtTCPPort.Text := IntToStr(TCPIPPort);
      edtUDPServer.Text := IntToStr(UDPPortSr);
      edtUDPClient.Text := IntToStr(UDPPortCl);
      edtIPXSr.Text := IntToStr(IPXSocketSr);
      edtIPXCl.Text := IntToStr(IPXSocketCl);
      edtSPX.Text := IntToStr(SPXSocket);
      cbTCPIntf.ItemIndex := TCPIntf + 1;
      TCPIntfcNum := TCPIntf + 1;
    end;
    if ProtForm.ShowModal = MrOK then begin
      aCanceled := False;
      {update changes to the protocol form in aOptions}
      with ProtForm, aOptions do begin
        IsSingleUser := cbxSUEnabled.Checked;
        IsIPXSPX := cbxIPXEnabled.Checked;
        IPXSPXLFB := cbxIPXListen.Checked;
        IsTCPIP := cbxTCPEnabled.Checked;
        TCPIPLFB := cbxTCPListen.Checked;
        TCPIPPort := StrToInt(edtTCPPort.Text);
        UDPPortSr := StrToInt(edtUDPServer.Text);
        UDPPortCl := StrToInt(edtUDPClient.Text);
        IPXSocketSr := StrToInt(edtIPXSr.Text);
        IPXSocketCl := StrToInt(edtIPXCl.Text);
        SPXSocket := StrToInt(edtSPX.Text);
        TCPIntf := pred(cbTCPIntf.ItemIndex);
      end;
    end else
      aCanceled := True;
  finally
    ProtForm.Free;
  end;
end;
{--------}
procedure TfrmFF2Conv.SetControls(aIsConverting : Boolean);
begin
  if aIsConverting then begin
    btnExecute.ImageIndex := cnCancel;
    mnuFileConvert.Caption := '&Cancel';
    mnuFileConvert.ShortCut := ShortCut(Word('C'), [ssCtrl]);;
  end
  else begin
    btnExecute.ImageIndex := cnExecute;
    mnuFileConvert.Caption := '&Convert';
    mnuFileConvert.ShortCut := ShortCut(Word('E'), [ssCtrl]);;
  end;

  mnuFileExit.Enabled := not aIsConverting;
  gbSource.Enabled := not aIsConverting;
  gbDest.Enabled := not aIsConverting;
end;
{--------}
procedure TfrmFF2Conv.SetSourceDirectory(const aDirectory : string);
var
  OldDirectory : string;
begin
  OldDirectory := srcDirectory.Directory;
  try
    srcDrive.Drive := ExtractFileDrive(aDirectory)[1];
    srcDirectory.Drive := ExtractFileDrive(aDirectory)[1];
    srcDirectory.Directory := aDirectory;
  except
    on E : EInOutError do begin
      MessageDlg(aDirectory + ' doesn''t exist. Please choose ' +
                 'another directory.', mtWarning, [mbOK], 0);
      srcDirectory.Directory := OldDirectory;
    end;
  end;
end;
{--------}
procedure TfrmFF2Conv.SetSourceDrive(aDrive : char);
begin
  {set to both components and check for EInOutError}
  try
    srcDrive.Drive := aDrive;
    srcDirectory.Drive := aDrive;
  except
    on E : EInOutError do begin
      MessageDlg(aDrive + ' drive doesn''t exist. Please choose ' +
                 'another drive.', mtWarning, [mbOK], 0);
    end;
  end;
end;
{--------}
procedure TfrmFF2Conv.SetTargetDirectory(const aDirectory : string);
var
  OldDirectory : string;
begin
  {set to both components and check for EInOutError}
  OldDirectory := tgtDirectory.Directory;
  try
    tgtDrive.Drive := ExtractFileDrive(aDirectory)[1];
    tgtDirectory.Drive := ExtractFileDrive(aDirectory)[1];
    tgtDirectory.Directory := aDirectory;
  except
    on E : EInOutError do begin
      MessageDlg(aDirectory + ' doesn''t exist. Please choose ' +
                 'another directory.', mtWarning, [mbOK], 0);
      tgtDirectory.Directory := OldDirectory;
    end;
  end;
end;
{--------}
procedure TfrmFF2Conv.SetTargetDrive(aDrive : char);
var
  OldDrive : char;
begin
  OldDrive := tgtDrive.Drive;
  try
    tgtDrive.Drive := aDrive;
    tgtDirectory.Drive := aDrive;
  except
    on E : EInOutError do begin
      MessageDlg(aDrive + ' drive doesn''t exist. Please choose ' +
                 'another drive.', mtWarning, [mbOK], 0);
      tgtDrive.Drive := OldDrive;
    end;
  end;
end;
{--------}
procedure TfrmFF2Conv.srcDriveChange(Sender : TObject);
var
  OldDrive : char;
begin
  OldDrive := srcDirectory.Drive;
  try
    srcDirectory.Drive := srcDrive.Drive;
  except
    on E : EInOutError do begin
      MessageDlg(srcDrive.Drive + ' drive doesn''t exist. Please choose ' +
                 'another drive.', mtWarning, [mbOK], 0);
      srcDirectory.Drive := OldDrive;
      srcDrive.Drive := OldDrive;
    end;
  end;
  FocusControl(srcDirectory);
end;
{--------}
procedure TfrmFF2Conv.tgtDriveChange(Sender : TObject);
var
  OldDrive : char;
begin
  OldDrive := srcDirectory.Drive;
  try
    tgtDirectory.Drive := tgtDrive.Drive;
  except
    on E : EInOutError do begin
      MessageDlg(tgtDrive.Drive + ' drive doesn''t exist. Please choose ' +
                 'another drive.', mtWarning, [mbOK], 0);
      tgtDirectory.Drive := OldDrive;
      tgtDrive.Drive := OldDrive;
    end;
  end;
  FocusControl(tgtDirectory);
end;
{====================================================================}
procedure TfrmFF2Conv.mnuFileExitClick(Sender : TObject);
begin
  Close;
end;
{--------}
procedure TfrmFF2Conv.mnuAboutClick(Sender: TObject);            {new !!.07}
begin
  with TFFAboutBox.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;
{====================================================================}

end.
