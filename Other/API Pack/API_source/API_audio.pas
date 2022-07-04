unit API_audio;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Windows, Dialogs, Messages, API_base;

type
  TmmMode = (mpNotReady, mpStopped, mpPlaying, mpRecording, mpSeeking,
    mpPaused, mpOpen, mpUnknown);

  TmmTimeFormat = (tfMilliseconds, tfHMS, tfMSF, tfFrames, tfSMPTE24,
    tfSMPTE25, tfSMPTE30, tfSMPTE30Drop, tfBytes, tfSamples, tfTMSF);

  (*
      actual audiofile playing routine. this will manage all
      the things to do with mmsystems mci stuff that takes
      care of almost all multimedia to do in windows.
  *)
  TAPI_audiofile = class(TObject)
  private
    ffilename: string;
    fdevice: word;
    procedure mciCheckError(error: integer);
    procedure mciOpen;
    procedure mciClose;
    function  mciGetMode: TmmMode;
    procedure mciSetTimeFormat(value: TmmTimeFormat);
    function  mciGetLength: Longint;
    function  mciGetPosition: Longint;
    procedure mciPlay;
    procedure mciPause;
    procedure mciResume;
    procedure mciStop;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  (*
      actual component here, this is what you see on the
      form when you put the API_audio component there. this
      component manages the mmsystem to play whatever audio
      file codecs are installed onto your computer - and
      also this takes care of unloading the real player object
      from the memory, so everything you need to take care
      is the beatifull look for your application.
  *)
  TAPI_audio = class(TAPI_Custom_Component)
  private
    ffilename: string;
    faudio: tapi_audiofile;
  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    procedure Play;                     // playis defined file if exists
    procedure Stop;                     // stops playing and closes mci device
    procedure Pause;                    // pauses audio if playing
    procedure Resume;                   // resumes playing if paused
    function  GetState: TmmMode;        // current mci mode (MPModes)
    function  GetStateString: string;   // above state as preformatted string
    function  GetProgress: double;      // playing progress (0-100%)
    function  GetLength: integer;       // audio file length in seconds
    function  GetPosition: integer;     // current playing position (secs)
  published
    property  Filename: string read ffilename write ffilename;
  end;

procedure Register;

implementation

{$r *.res}

uses
  MMsystem;

const
  TMPModeString: array[0..6] of string = ('Not Ready', 'Stopped', 'Playing',
    'Seeking', 'Paused', 'Open', 'Unknown');

//===========
// testing this shit out !!
//===========

//------------------------------------------------------------------------------
constructor TAPI_audio.create(aowner: tcomponent);
begin
  inherited Create(aowner);
  version:= 'r1.00/ari.pikivirta(at)kolumbus.fi';
  ffilename:= '';
end;

//------------------------------------------------------------------------------
constructor TAPI_audiofile.create;
begin
  inherited Create;
  ffilename:= '';
  fdevice:= 0;
end;

//------------------------------------------------------------------------------
destructor TAPI_audio.destroy;
begin
  if faudio<>nil then freeandnil(faudio);
  inherited destroy;
end;

//------------------------------------------------------------------------------
destructor TAPI_audiofile.destroy;
begin
  if (fdevice>0) then
  begin
    if (mciGetMode = mpPlaying) then mciStop;
    mciClose;
  end;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_audio.GetState: TmmMode;
begin
  if (faudio<>nil) then result:= faudio.mciGetMode
    else result:= mpNotReady;
end;

//------------------------------------------------------------------------------
function TAPI_audio.getstatestring: string;
begin
  result:= TMPModeString[integer(GetState)];
end;

//------------------------------------------------------------------------------
function TAPI_audio.GetProgress: double;
begin
  result:= -1;
  if (faudio<>nil) then
  begin
    if (GetState = mpPlaying) then
    begin
      result:= 100*faudio.mciGetPosition/faudio.mciGetLength;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciCheckError(error: integer);
var
  errmsg: array[0..4095] of char;
  errs: string;
begin
  if (error = 0) then Exit;
  if mciGetErrorString(error, errmsg, SizeOf(errmsg)) then
    setstring(errs, errmsg, strlen(errmsg))
    else errs:='Unknown MCI Error';
  messagedlg('Error: '+errs, mterror, [mbok], 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciOpen;
var
  OpenParm: TMCI_Open_Parms;
  iflg: integer;
  ierr: integer;
begin
  mciClose;
  OpenParm.dwCallback:= 0;
  OpenParm.lpstrDeviceType:= 'WAVEAUDIO';
  OpenParm.lpstrElementName:= PChar(ffilename);
  iflg := MCI_WAIT or MCI_OPEN_ELEMENT or MCI_OPEN_SHAREABLE;
  ierr := mciSendCommand(0, mci_Open, iflg, Longint(@OpenParm));
  mciCheckError(ierr);
  fdevice := OpenParm.wDeviceID;
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciClose;
var
  iflg: integer;
  ierr: integer;
begin
  if (fdevice = 0) then exit;
  iflg:= MCI_WAIT;
  ierr:= mciSendCommand(fdevice, mci_Close, iflg, 0);
  mciCheckError(ierr);
  fdevice := 0;
end;

//------------------------------------------------------------------------------
function TAPI_audiofile.mciGetMode: TmmMode;
var
  StatusParm: TMCI_Status_Parms;
  iflg: integer;
  ierr: integer;
begin
  Result := mpUnknown;
  if (fdevice = 0) then exit;
  iflg := mci_Wait or mci_Status_Item;
  StatusParm.dwItem:= mci_Status_Mode;
  ierr := mciSendCommand(fdevice, mci_Status, iflg, Longint(@StatusParm));
  mciCheckError( ierr );
  case StatusParm.dwReturn of
    MCI_MODE_NOT_READY: result:= mpNotReady;
    MCI_MODE_STOP: result:= mpStopped;
    MCI_MODE_PLAY: result:= mpPlaying;
    MCI_MODE_RECORD: result:= mpRecording;
    MCI_MODE_SEEK: result:= mpSeeking;
    MCI_MODE_PAUSE: result:= mpPaused;
    MCI_MODE_OPEN: result:= mpOpen;
    else result:= mpUnknown;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciSetTimeFormat(value: TmmTimeFormat);
var
  SetParm: TMCI_Set_Parms;
  iflg: integer;
  ierr: integer;
begin
  if (fDevice = 0) then Exit;
  iflg:= mci_Set_Time_Format;
  SetParm.dwTimeFormat:= Longint(value);
  ierr:= mciSendCommand(fdevice, mci_Set, iflg, Longint(@SetParm));
  mciCheckError(ierr);
end;

//------------------------------------------------------------------------------
function TAPI_audio.GetLength: integer;
begin
  if faudio<>nil then
  begin
    faudio.mciSetTimeFormat(tfMilliseconds);
    result:= faudio.mciGetLength div 1000;
  end else
    result:= -1;
end;
//------------------------------------------------------------------------------
function TAPI_audio.GetPosition: integer;
begin
  if faudio<>nil then
  begin
    faudio.mciSetTimeFormat(tfMilliseconds);
    result:= faudio.mciGetPosition div 1000;
  end else
    result:= -1;
end;

//------------------------------------------------------------------------------
function TAPI_audiofile.mciGetLength: Longint;
var
  StatusParm: TMCI_Status_Parms;
  iflg: integer;
  ierr: integer;
begin
  Result := 0;
  if (fdevice = 0) then exit;
  iflg:= mci_Wait or mci_Status_Item;
  StatusParm.dwItem:= mci_Status_Length;
  ierr:= mciSendCommand(fdevice, mci_Status, iflg, Longint(@StatusParm));
  mciCheckError(ierr);
  Result:= StatusParm.dwReturn;
end;

//------------------------------------------------------------------------------
function TAPI_audiofile.mciGetPosition: Longint;
var
  StatusParm: TMCI_Status_Parms;
  iflg: integer;
  ierr: integer;
begin
  Result := 0;
  if (fdevice=0) then exit;
  iflg:= mci_Wait or mci_Status_Item;
  StatusParm.dwItem:= mci_Status_Position;
  ierr:= mciSendCommand(fdevice, mci_Status, iflg, Longint(@StatusParm));
  mciCheckError(ierr);
  Result:= StatusParm.dwReturn;
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciPlay;
var
  PlayParm: TMCI_Play_Parms;
  iflg: integer;
  ierr: integer;
begin
  if (fdevice=0) then mciOpen;
  iflg:= 0;
  PlayParm.dwCallback:= 0;
  ierr:= mciSendCommand(fdevice, mci_Play, iflg, Longint(@PlayParm));
  mciCheckError(ierr);
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciStop;
var
  ierr: integer;
begin
  if (fdevice=0) then exit;
  ierr:= mciSendCommand(fdevice, mci_Stop, 0, 0);
  mciCheckError(ierr);
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciPause;
var
  ierr: integer;
begin
  if (fdevice=0) then exit;
  ierr:= mciSendCommand(fdevice, mci_Pause, 0, 0);
  mciCheckError(ierr);
end;

//------------------------------------------------------------------------------
procedure TAPI_audiofile.mciResume;
var
  ierr: integer;
begin
  if (fdevice=0) then exit;
  ierr := mciSendCommand(fdevice,mci_Resume,0,0 );
  mciCheckError(ierr);
end;

//------------------------------------------------------------------------------
procedure TAPI_audio.Play;
begin
  // check if device is open
  if faudio<>nil then Stop;
  // check file presence
  if fileexists(ffilename) then
  begin
    faudio:= tapi_audiofile.Create;
    faudio.ffilename:= ffilename;
    faudio.mciOpen;
    // re-check and play
    if faudio<>nil then faudio.mciPlay;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_audio.Stop;
begin
  if faudio=nil then exit;
  if getstate = mpPlaying then faudio.mciStop;
  faudio.mciClose;
end;

//------------------------------------------------------------------------------
procedure TAPI_audio.Pause;
begin
  if faudio=nil then exit;
  if getstate = mpplaying then faudio.mciPause;
end;

//------------------------------------------------------------------------------
procedure TAPI_audio.Resume;
begin
  if faudio=nil then exit;
  if getstate = mppaused then faudio.mciResume;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_audio]);
end;

end.
