unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, ComCtrls, ExtCtrls, Bass, basscd;

type
  TForm1 = class(TForm)
    cmbDrives: TComboBox;
    lvlLeft: TProgressBar;
    lvlRight: TProgressBar;
    lstTracks: TListBox;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    btnPlay: TButton;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    chkAdvance: TCheckBox;
    GroupBox4: TGroupBox;
    trkSpeed: TTrackBar;
    trkVol: TTrackBar;
    Timer1: TTimer;
    lblStatus: TLabel;
    btnOpen: TButton;
    btnLock: TButton;
    trkPos: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cmbDrivesChange(Sender: TObject);
    procedure lstTracksClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnLockClick(Sender: TObject);
    procedure trkPosScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure trkSpeedChange(Sender: TObject);
    procedure trkVolChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure PlayTrack(drive, track: DWORD);
    procedure UpdateTrackList;
  end;

const
  MAXDRIVES = 10;

var
  Form1: TForm1;

  curdrive: Integer = 0;
  seeking: Integer = -1;
  updatecount: Integer = 0;
  levl: Integer = 0;
  levr: Integer = 0;
  stream: array[0..MAXDRIVES - 1] of HSTREAM = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

implementation

{$R *.dfm}

// End sync
procedure EndSync(handle: HSYNC; channel: DWORD; data: DWORD; user: DWORD); stdcall;
var
  track, drive, tracks: DWORD;
begin
  if (Form1.chkAdvance.Checked = True) then // advance onto next track
  begin
    track := BASS_CD_StreamGetTrack(channel);
    drive := HIWORD(track);
    tracks := BASS_CD_GetTracks(drive);
    if (tracks = DW_ERROR) then Exit; // error, eg. CD removed?
    track := (LOWORD(track) + 1) mod tracks;
    if (drive = DWORD(curdrive)) then
      Form1.lstTracks.ItemIndex := track;
    Form1.PlayTrack(drive, track);
  end;
end;

procedure TForm1.PlayTrack(drive, track: DWORD);
begin
  if (stream[drive] <> 0) then
    BASS_CD_StreamSetTrack(stream[drive],track) // already have a stream, so just set the track
  else
  begin
    stream[drive] := BASS_CD_StreamCreate(drive, track, 0); // create stream
    BASS_ChannelSetSync(stream[drive], BASS_SYNC_END, 0, @EndSync, nil); // set end sync
  end;
  if (drive = DWORD(curdrive)) then
    trkPos.Max := BASS_ChannelGetLength(stream[drive], BASS_POS_BYTE) div 176400; // set pos scroller range
  BASS_ChannelPlay(stream[drive], False); // start playing
end;

procedure TForm1.UpdateTrackList;
var
  vol, spd: Single;
  cdtext, t: PAnsiChar;
  a, tc, l: Integer;
  text, tag: String;
begin
  tc := BASS_CD_GetTracks(curdrive);
  lstTracks.Items.Clear;
  if (tc = -1) then // no CD
    Exit;

  cdtext := BASS_CD_GetID(curdrive, BASS_CDID_TEXT); // get CD-TEXT
  for a := 0 to tc - 1 do
  begin
    l := BASS_CD_GetTrackLength(curdrive, a);
    text := Format('Track %.2d', [a + 1]);
    if (cdtext <> nil) then
    begin
      t := cdtext;
      tag := Format('TITLE%d=', [a + 1]); // the CD-TEXT tag to look for
      while (t <> nil) do
      begin
         if (Copy(t, 1, Length(tag)) = tag) then // found the track title...
         begin
           text := Copy(t, Length(tag)+1, Length(t) - Length(tag)); // replace "track x" with title
           Break;
         end;
         t := t + Length(t) + 1;
      end;
    end;
    if (l = -1) then
      text := text + ' (data)'
    else
    begin
      l := l div 176400;
      text := text + Format(' (%d:%.2d)', [l div 60, l mod 60]);
    end;
    lstTracks.Items.Add(text)
  end;
  a := BASS_CD_StreamGetTrack(stream[curdrive]);
  if (a <> -1) then // this drive has a stream
  begin
    lstTracks.ItemIndex := LOWORD(a); // select current track
    trkPos.Max := BASS_ChannelGetLength(stream[curdrive], BASS_POS_BYTE) div 176400; // set pos scroller range
  end;
  vol := 1;
  spd := 44100;
  BASS_ChannelGetAttribute(stream[curdrive], BASS_ATTRIB_VOL, vol); // get volume
  BASS_ChannelGetAttribute(stream[curdrive], BASS_ATTRIB_FREQ, spd); // get speed
  trkVol.Position := Trunc((1 - vol) * 100); // set volume slider pos
  trkSpeed.Position := Trunc(spd / 441); // set speed slider pos
  GroupBox2.Caption := Format('Speed - %d%%', [trkVol.Position]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  a: Integer;
  text: String;
  cdi : BASS_CD_INFO ;
begin
  // check the correct BASS was loaded
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
  begin
    MessageBox(0,'An incorrect version of BASS.DLL was loaded',0,MB_ICONERROR);
    Halt;
  end;

  // Get list of available drives
  a := 0;
  while (a < MAXDRIVES) and BASS_CD_GetInfo(a,cdi) do
  begin
    cmbDrives.Items.Add(Char(cdi.letter + Ord('A'))+' '+cdi.vendor+' '+cdi.product+' v'+cdi.rev);
    a := a + 1;
  end;
  if (a = 0) then
  begin
    MessageBox(0, 'No CD drives found', 'Error', MB_ICONERROR);
    Halt;
  end;
  cmbDrives.ItemIndex := 0; // select 1st drive

  // Setup output - default device
  if (not BASS_Init(-1, 44100, 0, Handle, nil)) then
  begin
    MessageBox(0, 'Can''t initialize device', 'Error', MB_ICONERROR);
    Halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BASS_Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  p: DWORD;
  time: String;
  level: Integer;
  isopen, islock: Boolean;
begin
  // update levels
  level := BASS_ChannelGetLevel(stream[curdrive]);
  levl := levl - 1500;
  if (levl < 0) then
    levl := 0;
  levr := levr - 1500;
  if (levr < 0) then
    levr := 0;
  if (level <> -1) then
  begin
    if (levl < LOWORD(level)) then
      levl := LOWORD(level);
    if (levr < HIWORD(level)) then
      levr := HIWORD(level);
  end;
  lvlLeft.Position := levl; // left
  lvlRight.Position := levr; // right
  updatecount := updatecount + 1;
  if ((updatecount and 3) = 0) then // do other stuff (only every 4th visit)
  begin
    time := '-';
    isopen := BASS_CD_DoorIsOpen(curdrive);
    islock := BASS_CD_DoorIsLocked(curdrive);
    if (isopen) then
      lblStatus.Caption := 'Opened & '
    else
      lblStatus.Caption := 'Closed & ';
    if (islock) then
      lblStatus.Caption := lblStatus.Caption + 'locked'
    else
      lblStatus.Caption := lblStatus.Caption + 'unlocked';
    if (BASS_ChannelIsActive(stream[curdrive]) > 0) then // playing - update info
    begin
      p := seeking;
      if (seeking = -1) then // not seeking - update pos scroller
      begin
        p := Trunc(BASS_ChannelBytes2Seconds(stream[curdrive], BASS_ChannelGetPosition(stream[curdrive], BASS_POS_BYTE)));
        trkPos.Position := p;
      end;
      time := Format('%d - %d:%.2d', [LOWORD(BASS_CD_StreamGetTrack(stream[curdrive])) + 1, p div 60, p mod 60]);
    end
    else
    begin
      if (lstTracks.Items.Count = 0) then // empty track list - refresh
      begin
        if (not isopen) then
          UpdateTrackList;
      end
      else if (isopen) or (not BASS_CD_IsReady(curdrive)) then // no CD - free stream & clear list
      begin
        BASS_StreamFree(stream[curdrive]);
        stream[curdrive] := 0;
        lstTracks.Items.Clear;
      end;
      BASS_CD_Release(curdrive); // release the drive to allow others to access it
    end;
    Panel1.Caption := time;
  end;
end;

procedure TForm1.cmbDrivesChange(Sender: TObject);
begin
  // change current drive
  curdrive := cmbDrives.ItemIndex;
  UpdateTrackList;
end;

procedure TForm1.lstTracksClick(Sender: TObject);
begin
  // change playing track
  PlayTrack(curdrive, lstTracks.ItemIndex);
end;

procedure TForm1.btnPlayClick(Sender: TObject);
begin
  // play/pause
  if (BASS_ChannelIsActive(stream[curdrive]) = BASS_ACTIVE_PLAYING) then
    BASS_ChannelPause(stream[curdrive])
  else
    BASS_ChannelPlay(stream[curdrive], False);
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  // open/close door
  if (BASS_CD_DoorIsOpen(curdrive) = True) then
    BASS_CD_Door(curdrive, BASS_CD_DOOR_CLOSE)
  else
    BASS_CD_Door(curdrive, BASS_CD_DOOR_OPEN);
end;

procedure TForm1.btnLockClick(Sender: TObject);
begin
  // lock/unlock door
  if (BASS_CD_DoorIsLocked(curdrive) = True) then
    BASS_CD_Door(curdrive, BASS_CD_DOOR_UNLOCK)
  else
    BASS_CD_Door(curdrive,BASS_CD_DOOR_LOCK);
end;

procedure TForm1.trkPosScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // position scroller
  seeking := ScrollPos;
  if (ScrollCode = scEndScroll) then // seek to new pos
  begin
    BASS_ChannelSetPosition(stream[curdrive], seeking * 176400, BASS_POS_BYTE);
    seeking := -1;
  end;
end;

procedure TForm1.trkSpeedChange(Sender: TObject);
begin
  // adjust speed
  BASS_ChannelSetAttribute(stream[curdrive], BASS_ATTRIB_FREQ, trkSpeed.Position * 441);
  GroupBox2.Caption := Format('Speed - %d%%', [trkSpeed.Position]);
end;

procedure TForm1.trkVolChange(Sender: TObject);
begin
  // adjust volume
  BASS_ChannelSetAttribute(stream[curdrive], BASS_ATTRIB_VOL, 1 - (trkVol.Position / 100));
end;

end.
