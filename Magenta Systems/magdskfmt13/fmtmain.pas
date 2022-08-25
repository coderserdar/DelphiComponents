unit fmtmain;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Test program for Magenta Check Disk and Format Disk component
// 26th November 2018 - Release 1.3 (C) Magenta Systems Ltd, 2018

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// 20th Oct 2005 1.0 - baseline
// 30th Jul 2008 1.1 - tested with Unicode and Delphi 2009, and Vista
//                     added warning if program does not admin rights
// 20th Aug 2008 1.2 - corrected progress message charset which was OEM (IBM-PC) not
//                         ANSI or unicode, thanks to Francois Piette
// 8th Nov 2011  1.3 - added exFAT file system, added Refresh Drives button




// Note this project needs Win 3.1/Delphi 1.0 Comptability palette components
// from dcl31wXXX.bpl which may not be installed by default


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, magfmtdisk, FileCtrl, ComCtrls, magsubs1 ;

type
  TMainForm = class(TForm)
    Log: TMemo;
    Panel1: TPanel;
    doChkDsk: TButton;
    doExit: TButton;
    DriveBox: TDriveComboBox;
    OptCorrectErrors: TCheckBox;
    OptVerbose: TCheckBox;
    OptCheckDirty: TCheckBox;
    OptScanDrive: TCheckBox;
    OptQuickFmt: TCheckBox;
    doAbort: TButton;
    doFrmtDsk: TButton;
    ProgressBar: TProgressBar;
    FileSystem: TComboBox;
    Label1: TLabel;
    VolumeLabel: TEdit;
    dioRefresh: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doChkDskClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doAbortClick(Sender: TObject);
    procedure doFrmtDskClick(Sender: TObject);
    procedure dioRefreshClick(Sender: TObject);
  private
    { Private declarations }
    procedure LogInfo (Info: String) ;
    procedure ProgressEvent (Percent: integer; var Cancel: boolean) ;
    procedure InfoEvent (Info: string; var Cancel: boolean) ;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  MagFmtChkDsk: TMagFmtChkDsk ;
  CancelFlag: boolean ;

implementation

{$R *.dfm}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil (MagFmtChkDsk) ;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    MagFmtChkDsk := TMagFmtChkDsk.Create (self) ;
    MagFmtChkDsk.onProgressEvent := ProgressEvent ;
    MagFmtChkDsk.onInfoEvent := InfoEvent ;
    if NOT IsProgAdmin then
        Log.Lines.Add ('Warning - administrator rights needed to use this program') ;
end;

procedure TMainForm.LogInfo (Info: String) ;
begin
    Log.Lines.Add (Info) ;
end ;

procedure TMainForm.ProgressEvent (Percent: integer; var Cancel: boolean) ;
begin
//    LogInfo ('Percent Completed: ' + IntToStr (Percent)) ;
    ProgressBar.Position := Percent ;
    Application.ProcessMessages ;
    Cancel := CancelFlag ;
end ;

procedure TMainForm.InfoEvent (Info: string; var Cancel: boolean) ;
begin
    LogInfo (Info) ;
    Application.ProcessMessages ;
    Cancel := CancelFlag ;
end ;

procedure TMainForm.doChkDskClick(Sender: TObject);
begin
    CancelFlag := false ;
    doChkDsk.Enabled := false ;
    doFrmtDsk.Enabled := false ;
    try
        try
            ProgressBar.Position := 0 ;
            LogInfo ('Starting Check Disk for ' + Uppercase (DriveBox.Drive) + ':') ;
            if NOT MagFmtChkDsk.CheckDisk (DriveBox.Drive + ':\', OptCorrectErrors.Checked,
                 OptVerbose.Checked, OptCheckDirty.Checked, OptScanDrive.Checked) then
                LogInfo ('Check Disk Failed') ;
            ProgressBar.Position := 0 ;
            if MagFmtChkDsk.FileSysProblem then
                 LogInfo ('!!! Check Disk Found Problems with ' + Uppercase (DriveBox.Drive) + ':') ;
        except
            on E:Exception do LogInfo ('Error: ' + E.Message) ;
        end ;
    finally
        doChkDsk.Enabled := true ;
        doFrmtDsk.Enabled := true ;
        LogInfo ('') ;
    end ;
end;

procedure TMainForm.doFrmtDskClick(Sender: TObject);
var
    MediaType: TMediaType;
begin
    if Trim (VolumeLabel.Text) = '' then
    begin
        LogInfo ('Must Specify a Volume Label To Format a Drive') ;
        exit ;
    end ;
    CancelFlag := false ;
    doChkDsk.Enabled := false ;
    doFrmtDsk.Enabled := false ;
    MediaType := mtHardDisk ;
    if UpperCase (DriveBox.Drive) < 'C' then MediaType := mtFloppy ;
    try
        try
            ProgressBar.Position := 0 ;
            LogInfo ('Starting Format Disk for ' + Uppercase (DriveBox.Drive) + ':') ;
            if NOT MagFmtChkDsk.FormatDisk (DriveBox.Drive + ':\', MediaType,
                   TFileSystem (FileSystem.ItemIndex), Trim (VolumeLabel.Text),
                                                      OptQuickFmt.Checked, 0) then
            begin
                 LogInfo ('Format Disk Failed') ;
            end;
            ProgressBar.Position := 0 ;
        except
            on E:Exception do LogInfo ('Error: ' + E.Message) ;
        end ;
    finally
        doChkDsk.Enabled := true ;
        doFrmtDsk.Enabled := true ;
        LogInfo ('') ;
    end ;
end;

procedure TMainForm.doExitClick(Sender: TObject);
begin
    CancelFlag := true ;
    Close ;
end;

procedure TMainForm.dioRefreshClick(Sender: TObject);
begin
    DriveBox.TextCase := tcLowerCase ;
    DriveBox.TextCase := tcUpperCase ;
end;

procedure TMainForm.doAbortClick(Sender: TObject);
begin
    LogInfo ('Cancelled by User') ;
    CancelFlag := true ;
end;


end.
