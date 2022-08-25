unit xpemain;

// Test program for Enhanced Write Filter header conversion
// note this application is only useful on Windows XP Embedded with EWF installed
// 8th April 2009 - Release 1.2 (C) Magenta Systems Ltd, 2009

// EWF is designed to protect the Windows boot volume so effectively
// it's write protected and boots up identically each time.  EWF API
// calls are needed if changes are to be saved to disk.

// note this program has only been testing on Windows Embedded running
// on a flash memory C drive (IDE interface), it has not been tested
// with a hard disk which potentially offer multiple overlays.

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// 1.0 - 20th October 2005 - baseline
// 1.1 - 5th August 2008   - updated to be compatible with Delphi 2009
// 1.2 - 8th April 2009    - added MINENUMSIZE 4 so that enumerated
//                           variables are correct length for C++, thanks
//                           to Sven van Bruegge.  This fix means the
//                           protected info should now be correct

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ewfapi, magsubs1, Spin ;

type
  TMainForm = class(TForm)
    Log: TMemo;
    doExit: TButton;
    doEnable: TButton;
    doDisable: TButton;
    VolNr: TSpinEdit;
    doCommit: TButton;
    Label1: TLabel;
    PersData: TEdit;
    doSavePers: TButton;
    doClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doCommitClick(Sender: TObject);
    procedure doDisableClick(Sender: TObject);
    procedure doEnableClick(Sender: TObject);
    procedure doSavePersClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


    TProtVolInfo = record
        VolName: WideString ;
        VolLetter: WideChar ;
        VolHandle: THandle ;
        Config: EWF_VOLUME_CONFIG ;
    end ;

var
    MainForm: TMainForm;
    ProtVolInfo: array [0..3] of TProtVolInfo ;
    TotProtVol: integer = 0 ;
    ProtDevName: WideString ;


implementation

{$R *.dfm}


procedure TMainForm.FormCreate(Sender: TObject);
var
    EwfVolList: PEWF_VOLUME_NAME_ENTRY ;
    VOLUME_CONFIG: PEWF_VOLUME_CONFIG ;
    OVERLAY_STORE_CONFIG: PEWF_OVERLAY_STORE_CONFIG ;
    S, S1: string ;
    OverlayHandle: THandle ;
    I: integer ;
//    strptr: PWideChar ;
begin
    try
        EwfLoadDLL;
    except
        Log.Lines.Add ('Exception Loading Enhanced Write Filter DLL') ;
    end ;
    if NOT EwfDLLLoaded then
    begin
        Log.Lines.Add ('Loading Enhanced Write Filter DLL Not Found') ;
        exit ;
    end ;

 // get list of protectec volumes
    EwfVolList := EwfMgrGetProtectedVolumeList ;
    if EwfVolList = Nil then
    begin
        Log.Lines.Add ('Failed to Get EWF Volume List, Error ' + FormatLastError) ;
        exit ;
    end ;
    TotProtVol := 0 ;
  //  VOLUME_CONFIG := Nil ;
    while (NOT EwfMgrVolumeNameListIsEmpty (EwfVolList)) do
    begin
        with ProtVolInfo [TotProtVol] do
        begin
         //   strptr := EwfVolList.Name ;
            VolName := WideCharToString (EwfVolList.Name) ;
            VolLetter := EwfMgrGetDriveLetterFromVolumeName (PWideChar (VolName)) ;
            if (VolLetter < 'A') or (VolLetter >= 'Z') then
            begin
                Log.Lines.Add ('Failed to Get EWF Drive Letter for: ' + VolName +
                       ', Letter: ' + VolLetter + ', Error: ' + FormatLastError) ;
                continue ;
            end ;
            VolHandle := EwfMgrOpenProtected (PWideChar (VolName)) ;
            if VolHandle = INVALID_HANDLE_VALUE then
            begin
                Log.Lines.Add ('Failed to Open EWF Volume: ' + VolName +
                                            ', Error: ' + FormatLastError) ;
                continue ;
            end ;
            VOLUME_CONFIG := EwfMgrGetProtectedVolumeConfig (VolHandle) ;
            if VOLUME_CONFIG = Nil then
            begin
                Log.Lines.Add ('Failed to Get EWF Volume Config: ' + VolName +
                                            ', Error: ' + FormatLastError) ;
                continue ;
            end ;
            Config.VType := VOLUME_CONFIG.VType ;
            Config.State := VOLUME_CONFIG.State ;
            Config.BootCommand := VOLUME_CONFIG.BootCommand ;
            Config.PersistentData := VOLUME_CONFIG.PersistentData;
            Config.MaxLevels := VOLUME_CONFIG.MaxLevels ;
            Config.DiskOverlay  := VOLUME_CONFIG.DiskOverlay ;
            Config.RamOverlay := VOLUME_CONFIG.RamOverlay ;
            Config.VolumeDesc := VOLUME_CONFIG.VolumeDesc ;
         //   Config.LevelDescArray             VOLUME_CONFIG.
            LocalFree (Cardinal (VOLUME_CONFIG)) ;
        //    VOLUME_CONFIG := Nil ;

         // report it all
            S := 'Protected Volume Num ' + IntToStr (succ (TotProtVol)) +
                 ', Name: ' + VolName + CRLF +
                 'Drive: ' + VolLetter + ', Handle: ' + IntToStr (VolHandle) +
                 ', Overlay Type: ' ;
            case Config.VType of
                EWF_DISK: S := S + 'Disk Overlay' ;
                EWF_RAM:  S := S + 'RAM Overlay with Store' ;
                EWF_RAM_REG: S := S + 'RAM Registry Overlay no Store' ;
            end ;
            case Config.State of
                EWF_ENABLED: S := S + ' - Overlay Enabled' ;
                EWF_DISABLED: S := S + ' - Overlay Disabled' ;
            end ;
            S := S + CRLF + 'Next Restart Command: ' ;
            case Config.BootCommand.Command of
                EWF_NO_CMD: S := S + 'No Pending Command' ;
                EWF_ENABLE: S := S + 'Overlay will be enabled' ;
                EWF_DISABLE: S := S + 'Overlay will be disabled' ;
                EWF_SET_LEVEL: S := S + 'Overlay level will be set' ;
                EWF_COMMIT: S := S + 'Current level will be committed' ;
            end ;
            S := S + ', Persistent Data: ' + Config.PersistentData ;
            PersData.Text := Config.PersistentData ;
            Log.Lines.Add (S + CRLF) ;
            EwfMgrClose (VolHandle) ;
        end ;
        inc (TotProtVol) ;
        EwfMgrVolumeNameEntryPop (EwfVolList) ;
    end ;
    EwfMgrVolumeNameListDelete (EwfVolList) ;
    Log.Lines.Add ('Total Protected Volumes: ' + IntToStr (TotProtVol) + CRLF );

  // get overlay store info
    OverlayHandle := EwfMgrOpenOverlayStore (false) ;
    if OverlayHandle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open Overlay Store, Error: ' + FormatLastError) ;
        exit ;
    end ;
    OVERLAY_STORE_CONFIG := EwfMgrGetOverlayStoreConfig (OverlayHandle) ;
    if OVERLAY_STORE_CONFIG = Nil then
    begin
        Log.Lines.Add ('Failed to Get Overlay Store Config, Error: ' + FormatLastError) ;
    end
    else
    begin
        S := 'Overlay Version: ' + IntToStr (OVERLAY_STORE_CONFIG.FormatVersion) +
          ', Volume Size: ' + IntToKbyte (OVERLAY_STORE_CONFIG.VolumeSize) +
          ', Number of Vols: ' + IntToStr (OVERLAY_STORE_CONFIG.NumVolumes) +
          ', Max Levels: ' + IntToStr (OVERLAY_STORE_CONFIG.MaxLevels) + CRLF +
          'Number of Segments: ' + IntToCStr (OVERLAY_STORE_CONFIG.NumSegments) +
          ', Free of Segments: ' + IntToCStr (OVERLAY_STORE_CONFIG.FreeSegments) +
          ', Segment Size: ' + IntToCStr (OVERLAY_STORE_CONFIG.SegmentSize)  ;
        Log.Lines.Add (S + CRLF) ;
        if OVERLAY_STORE_CONFIG.NumVolumes <> 0 then
        begin
            S1 := '' ;
            for I := 0 to Pred (EWF_VOLUME_ID_SIZE) do
                S1 := S1 + IntToHex (Ord
                             (OVERLAY_STORE_CONFIG.VolumeDescArray [0].VolumeID [I]), 2) ;
            ProtDevName := OVERLAY_STORE_CONFIG.VolumeDescArray [0].DeviceName ;
            S := 'Protected Volume, Device Name: ' + ProtDevName +
               ', Volume Id: ' + S1 ;
              Log.Lines.Add (S) ;
         // for I := 0 to Pred (OVERLAY_STORE_CONFIG.NumVolumes) do
         // begin
         //     S := 'Protected Volume, Device Name: ' +
          //       OVERLAY_STORE_CONFIG.VolumeDescArray [I].DeviceName  +
          //      ', Volume Id: ' + ConvHexStr (OVERLAY_STORE_CONFIG.VolumeDescArray [I].VolumeID) ;
          //    Log.Lines.Add (S) ;
         // end ;
        end ;
        LocalFree (Cardinal (OVERLAY_STORE_CONFIG)) ;
    end ;
    EwfMgrClose (OverlayHandle) ;
    Log.Lines.Add (CRLF) ;
end;

procedure TMainForm.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TMainForm.doCommitClick(Sender: TObject);
var
    Handle: THandle ;
    VName: Widestring ;
begin
    if (VolNr.Value = 0) or (VolNr.Value > TotProtVol) then exit ;
    VName := ProtVolInfo [Pred (VolNr.Value)].VolName ;
    Handle := EwfMgrOpenProtected (PWideChar (VName)) ;
    if Handle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open EWF Volume: ' + VName + ', Error: ' + FormatLastError) ;
        exit ;
    end ;
    if EwfMgrCommit (Handle) then
        Log.Lines.Add ('Commit Succeeded, Need Reboot')
    else
        Log.Lines.Add ('Failed to Commit Overlay, Error: ' + FormatLastError) ;
    EwfMgrClose (Handle) ;
end;

procedure TMainForm.doDisableClick(Sender: TObject);
var
    Handle: THandle ;
    VName: Widestring ;
begin
    if (VolNr.Value = 0) or (VolNr.Value > TotProtVol) then exit ;
    VName := ProtVolInfo [Pred  (VolNr.Value)].VolName ;
    Handle := EwfMgrOpenProtected (PWideChar (VName)) ;
    if Handle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open EWF Volume: ' + VName + ', Error: ' + FormatLastError) ;
        exit ;
    end ;
    if EwfMgrDisable (Handle, false) then
        Log.Lines.Add ('Disable Succeeded, Need Reboot')
    else
        Log.Lines.Add ('Failed to Disable Overlay, Error: ' + FormatLastError) ;
    EwfMgrClose (Handle) ;
end;

procedure TMainForm.doEnableClick(Sender: TObject);
var
    Handle: THandle ;
    VName: Widestring ;
begin
    if (VolNr.Value = 0) or (VolNr.Value > TotProtVol) then exit ;
    VName := ProtVolInfo [Pred  (VolNr.Value)].VolName ;
    Handle := EwfMgrOpenProtected (PWideChar (VName)) ;
    if Handle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open EWF Volume: ' + VName + ', Error: ' + FormatLastError) ;
        exit ;
    end ;
    if EwfMgrEnable (Handle) then
        Log.Lines.Add ('Enable Succeeded, Need Reboot')
    else
        Log.Lines.Add ('Failed to Enable Overlay, Error: ' + FormatLastError) ;
    EwfMgrClose (Handle) ;

end;

procedure TMainForm.doSavePersClick(Sender: TObject);
var
    Handle: THandle ;
    VName: Widestring ;
begin
    if (VolNr.Value = 0) or (VolNr.Value > TotProtVol) then exit ;
    VName := ProtVolInfo [Pred  (VolNr.Value)].VolName ;
    Handle := EwfMgrOpenProtected (PWideChar (VName)) ;
    if Handle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open EWF Volume: ' + VName + ', Error: ' + FormatLastError) ;
        exit ;
    end ;
    if EwfMgrSetPersistentData (Handle,
                        @PersData.Text [1], Length (PersData.Text)) then
        Log.Lines.Add ('Save Persistent Data Succeeded, Need Reboot')
    else
        Log.Lines.Add ('Failed to Save Persistent Data, Error: ' + FormatLastError) ;
    EwfMgrClose (Handle) ;
end;

procedure TMainForm.doClearClick(Sender: TObject);
var
    Handle: THandle ;
    VName: Widestring ;
begin
    if (VolNr.Value = 0) or (VolNr.Value > TotProtVol) then exit ;
    VName := ProtVolInfo [Pred  (VolNr.Value)].VolName ;
    Handle := EwfMgrOpenProtected (PWideChar (VName)) ;
    if Handle = INVALID_HANDLE_VALUE then
    begin
        Log.Lines.Add ('Failed to Open EWF Volume: ' + VName + ', Error: ' + FormatLastError) ;
        exit ;
    end ;
    if EwfMgrClearCommand (Handle) then
        Log.Lines.Add ('Clear Command Succeeded')
    else
        Log.Lines.Add ('Failed to Clear Command, Error: ' + FormatLastError) ;
    EwfMgrClose (Handle) ;
end;

end.
