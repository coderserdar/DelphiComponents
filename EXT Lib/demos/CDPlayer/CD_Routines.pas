{**********************************************************}
{                                                          }
{  CD Player                                               }
{  Devrace Extension Library example of                    }
{  TELTrayIcon, TELInstanceChecker                         }
{  ELPackStrings, ELUnpackStrings                          }
{  See Readme.txt for comments                             }
{                                                          }
{  Copyright (c) 2000 - 2001, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit CD_Routines;

interface

uses
  MMSystem, SysUtils, Messages, Forms, Windows, Classes;

type
  CD_Exception = class(Exception);
  CD_PlayNotify = procedure(ResultCode: Integer) of object;

const
  CD_NOT_OPEN_EXCEPTION: string = 'CD device not opened';

function CD_Open: Boolean;
function CD_Close: Boolean;
function CD_DeviceID: MCIDEVICEID;
function CD_Opened: Boolean;
function CD_IsAudioMediaPresent: Boolean;

function CD_Play(NotifyProc: CD_PlayNotify): Boolean;
function CD_Stop: Boolean;

function CD_Position: Integer;
function CD_ChangePosition(NewPos: Integer): Boolean;
function CD_TrackCount: Integer;
function CD_TrackStartPos(TrackNum: Byte): Integer;
function CD_TrackLength(TrackNum: Byte): Integer;
function CD_Mode: Integer;

function CD_NextTrack: Boolean;
function CD_PrevTrack: Boolean;

function CD_OpenDoor: Boolean;
function CD_CloseDoor: Boolean;

type
  TCD_DecodeCDDAFileResult = Integer;

const
  CD_DECODECDDAFILE_OK          = 0;
  CD_DECODECDDAFILE_CANNOTOPEN  = 1;
  CD_DECODECDDAFILE_WRONGFORMAT = 2;

function CD_DecodeCDDAFile(FileName: string; out CDCode: Integer;
  out TrackNumber: Byte): TCD_DecodeCDDAFileResult;

implementation

type
  TCD_Notifyer = class
  private
    FHandle: Integer;
    FPlayNotifyProc: CD_PlayNotify;
  protected
    procedure WinProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property Handle: Integer read FHandle;
    property PlayNotifyProc: CD_PlayNotify read FPlayNotifyProc write FPlayNotifyProc;
  end;

var
  DevID: MCIDEVICEID = 0;
  Notifyer: TCD_Notifyer;

procedure _CheckOpened;
begin
  if not CD_Opened then
    raise CD_Exception.Create(CD_NOT_OPEN_EXCEPTION);
end;

function CD_ChangePosition(NewPos: Integer): Boolean;
var
  Parms: MCI_SEEK_PARMS;
  PlayParms: MCI_PLAY_PARMS;
begin
  _CheckOpened;
  if CD_Mode = MCI_MODE_PLAY then
  begin
    PlayParms.dwCallback := Notifyer.Handle;
    PlayParms.dwFrom := NewPos;
    Result := (mciSendCommand(DevID, MCI_PLAY, MCI_NOTIFY or MCI_FROM,
      Cardinal(@PlayParms)) = 0);
  end
  else
  begin
    Parms.dwTo := NewPos;
    Result := (mciSendCommand(DevID, MCI_SEEK, MCI_TO, Cardinal(@Parms)) = 0);
  end;
end;

function CD_Open: Boolean;
var
  Parms: MCI_OPEN_PARMS;
  SetParms: MCI_SET_PARMS;
begin
  if CD_Opened then
    raise CD_Exception.Create('Can not open already opened CD device');
  Parms.lpstrDeviceType := Pointer(MCI_DEVTYPE_CD_AUDIO);
  Result := (mciSendCommand(0, MCI_OPEN, MCI_OPEN_TYPE or MCI_OPEN_TYPE_ID,
    Cardinal(@Parms)) = 0);
  if Result then
  begin
    DevID := Parms.wDeviceID;
    SetParms.dwTimeFormat := MCI_FORMAT_TMSF;
    mciSendCommand(DevID, MCI_SET, MCI_SET_TIME_FORMAT, Cardinal(@SetParms));
  end
  else DevID := 0;
end;

function CD_Close: Boolean;
var
  Parms: MCI_GENERIC_PARMS;
begin
  _CheckOpened;
  Result := (mciSendCommand(DevID, MCI_CLOSE, 0, Cardinal(@Parms)) = 0);
  if Result then DevID := 0;
end;

function CD_DeviceID: MCIDEVICEID;
begin
  Result := DevID;
end;

function CD_Opened: Boolean;
begin
  Result := (DevID <> 0);
end;

function CD_IsAudioMediaPresent: Boolean;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_MEDIA_PRESENT;
  Result := (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM,
    Cardinal(@Parms)) = 0) and Boolean(Parms.dwReturn);
  if Result then
  begin
    Parms.dwTrack := 1;
    Parms.dwItem := MCI_CDA_STATUS_TYPE_TRACK;
    Result := (mciSendCommand(DevID, MCI_STATUS,
      MCI_TRACK or MCI_STATUS_ITEM,
      Cardinal(@Parms)) = 0) and (Parms.dwReturn = MCI_CDA_TRACK_AUDIO);
  end;
end;

function CD_Play(NotifyProc: CD_PlayNotify): Boolean;
var
  Parms: MCI_PLAY_PARMS;
begin
  _CheckOpened;
  Notifyer.PlayNotifyProc := NotifyProc;
  Parms.dwCallback := Notifyer.Handle;
  Result := (mciSendCommand(DevID, MCI_PLAY, MCI_NOTIFY, Cardinal(@Parms)) = 0);
end;

function CD_Stop: Boolean;
var
  Parms: MCI_GENERIC_PARMS;
begin
  _CheckOpened;
  Result := (mciSendCommand(DevID, MCI_STOP, 0, Cardinal(@Parms)) = 0);
end;

function CD_Position: Integer;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_POSITION;
  if (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM, Cardinal(@Parms)) = 0) then
    Result := Parms.dwReturn else Result := -1;
end;

function CD_TrackCount: Integer;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_NUMBER_OF_TRACKS;
  if (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM, Cardinal(@Parms)) = 0) then
    Result := Parms.dwReturn else Result := -1;
end;

function CD_TrackStartPos(TrackNum: Byte): Integer;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_POSITION;
  Parms.dwTrack := TrackNum;
  if (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM or MCI_TRACK,
    Cardinal(@Parms)) = 0) then Result := Parms.dwReturn else Result := -1;
end;

function CD_TrackLength(TrackNum: Byte): Integer;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_LENGTH;
  Parms.dwTrack := TrackNum;
  if (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM or MCI_TRACK,
    Cardinal(@Parms)) = 0) then Result := Parms.dwReturn else Result := -1;
end;

function CD_Mode: Integer;
var
  Parms: MCI_STATUS_PARMS;
begin
  _CheckOpened;
  Parms.dwItem := MCI_STATUS_MODE;
  if (mciSendCommand(DevID, MCI_STATUS, MCI_STATUS_ITEM,
    Cardinal(@Parms)) = 0) then Result := Parms.dwReturn else Result := -1;
end;

function CD_NextTrack: Boolean;
var
  CurTrack, NewTrack: Integer;
begin
  _CheckOpened;
  CurTrack := mci_TMSF_Track(CD_Position);
  if CurTrack < CD_TrackCount then NewTrack := CurTrack + 1
    else NewTrack := 1;
  Result := CD_ChangePosition(CD_TrackStartPos(NewTrack));
end;

function CD_PrevTrack: Boolean;
var
  CurTrack, NewTrack, Pos: Integer;
begin
  _CheckOpened;
  Pos := CD_Position;
  CurTrack := mci_TMSF_Track(Pos);
  if (mci_TMSF_Minute(Pos) <> 0) or (mci_TMSF_Second(Pos) <> 0) then
    NewTrack := CurTrack
  else
    if CurTrack > 1 then NewTrack := CurTrack - 1
      else NewTrack := CD_TrackCount;
  Result := CD_ChangePosition(CD_TrackStartPos(NewTrack));
end;

function CD_OpenDoor: Boolean;
var
  Parms: MCI_SET_PARMS;
begin
  Result := (mciSendCommand(DevID, MCI_SET, MCI_SET_DOOR_OPEN,
    Cardinal(@Parms)) = 0);
end;

function CD_CloseDoor: Boolean;
var
  Parms: MCI_SET_PARMS;
begin
  Result := (mciSendCommand(DevID, MCI_SET, MCI_SET_DOOR_CLOSED,
    Cardinal(@Parms)) = 0);
end;

function CD_DecodeCDDAFile(FileName: string; out CDCode: Integer;
  out TrackNumber: Byte): TCD_DecodeCDDAFileResult;

  function _CreateFOURCC(C1, C2, C3, C4: Char): Integer;
  var
    PC: PChar;
  begin
    Result := 0;
    PC := @Result;
    PC^ := C1;
    Inc(PC);
    PC^ := C2;
    Inc(PC);
    PC^ := C3;
    Inc(PC);
    PC^ := C4;
  end;

var
  Stream: TFileStream;
  RIFF, Len, CDDA: Integer;

begin
  Result := CD_DECODECDDAFILE_OK;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      if Stream.Size < 25 + SizeOf(Integer) then
        raise CD_Exception.Create('');
      Stream.Read(RIFF, SizeOf(Integer));
      Stream.Read(Len, SizeOf(Integer));
      Stream.Read(CDDA, SizeOf(Integer));
      if (_CreateFOURCC('R', 'I', 'F', 'F') <> RIFF) or
        (_CreateFOURCC('C', 'D', 'D', 'A') <> CDDA) then
        raise CD_Exception.Create('');
      Stream.Position := 22; // ??
      Stream.Read(TrackNumber, SizeOf(Byte));
      Stream.Position := 24; // ??
      Stream.Read(CDCode, SizeOf(Integer));
    finally
      Stream.Free;
    end;
  except
    on EFOpenError do Result := CD_DECODECDDAFILE_CANNOTOPEN;
  else
    Result := CD_DECODECDDAFILE_WRONGFORMAT;
  end;
end;

{ TCD_Notifyer }

constructor TCD_Notifyer.Create;
begin
  FHandle := AllocateHWnd(WinProc);
end;

destructor TCD_Notifyer.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited;
end;

procedure TCD_Notifyer.WinProc(var Message: TMessage);
begin
  if Message.Msg = MM_MCINOTIFY then
  begin
    if Assigned(PlayNotifyProc) then
      PlayNotifyProc(Message.WParam);
  end
  else
    with Message do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

initialization
  Notifyer := TCD_Notifyer.Create;

finalization
  Notifyer.Free;

end.
