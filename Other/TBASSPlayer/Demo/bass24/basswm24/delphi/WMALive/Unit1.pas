unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Bass, BassWMA;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Edit2: TEdit;
    Panel1: TPanel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateDisplay;
    procedure Start;
    procedure Stop;
  end;

var
  Form1: TForm1;

  rchan: HRECORD = 0; // recording channel
  ehandle: HWMENCODE; // encoder handle
  time: Single; // elapsed time
  level: DWORD; // input level
  lastip: array[0..31] of Char; // last client to connect
  displaycount: Integer;

const
  SAMPLERATE = 44100;
  CHANNELS = 2;

implementation

{$R *.dfm}

function RecordingCallback(chan: HRECORD; buffer: Pointer; length: DWORD; user: DWORD): BOOL;
  stdcall;
begin
  time := time + (length / (SAMPLERATE * CHANNELS * 2)); // increase elapsed time counter
  // encode the sample data, and continue recording if successful
  Result := BASS_WMA_EncodeWrite(ehandle, buffer, length);
end;

procedure ClientConnect(handle: HWMENCODE; connect: BOOL; ip: PChar; user: DWORD); stdcall;
begin
  if (connect) then
    StrCopy(lastip, ip); // keep the client's ip for display
end;

procedure TForm1.UpdateDisplay;
var
  l, t: DWORD;
  disptext: string;
begin
  disptext := 'Off Air';
  if (BASS_ChannelIsActive(rchan) > 0) then
  begin
    if (level > 1500) then
      level := level - 1500
    else
      level := 0;

    l := BASS_ChannelGetLevel(rchan); // get current level
    if (LOWORD(l) > level) then
      level := LOWORD(l);
    if (HIWORD(l) > level) then
      level := HIWORD(l);

    if ((displaycount and 128) <> 0) then
    begin
      if ((displaycount and 64) <> 0) then // display last client
        disptext := 'last: ' + lastip
      else // display client count
        disptext := 'current clients: ' + IntToStr(BASS_WMA_EncodeGetClients(ehandle));
    end
    else // display "on air"
    begin
      t := Trunc(time);
      disptext := Format('On Air - port: %d - %d:%.2d', [BASS_WMA_EncodeGetPort(ehandle), t div 60,
        t mod 60]);
    end;
  end
  else
    level := 0;

  // draw the level bar
  ProgressBar1.Position := level div 327;

  Panel1.Caption := disptext; // update status text
end;

// start recording & encoding

procedure TForm1.Start;
begin
  // initialize encoder - let system choose port, max 5 clients
  ehandle := BASS_WMA_EncodeOpenNetwork(SAMPLERATE, CHANNELS, BASS_WMA_ENCODE_SCRIPT, strtoint(combobox1.Text), 0, 5);
  {
   If you want mutible Bitrates then you must do it on this way
   little Example
   var
   Bitrates : array [0..3] of DWORD = (64000,128000,160000,0);

   ehandle := BASS_WMA_EncodeOpenNetworkMulti(SAMPLERATE, CHANNELS, BASS_WMA_ENCODE_SCRIPT, @Bitrates, 0, 5);
  }
  if (ehandle = 0) then
  begin
    MessageBox(0, 'Can''t initialize encoding', 'Error', 0);
    Exit;
  end;

  BASS_WMA_EncodeSetTag(ehandle, 'Title', PChar(Edit1.Text), BASS_WMA_TAG_ANSI); // set WMA title tag

  BASS_WMA_EncodeSetNotify(ehandle, @ClientConnect, 0); // setup client notification

  time := 0;
  displaycount := 0;

  // start recording
  rchan := BASS_RecordStart(SAMPLERATE, CHANNELS, 0, @RecordingCallback, 0);
  if (rchan = 0) then
  begin
    MessageBox(0, 'Can''t start recording', 'Error', 0);
    BASS_WMA_EncodeClose(ehandle);
    Exit;
  end;

  Button1.Caption := 'Stop';
  Edit1.Enabled := False;
  Edit1.Color := clBtnFace;
  ComboBox1.Enabled := False;
  ComboBox1.Color := clBtnFace;
  Edit2.Enabled := True;
  Edit2.Color := clWindow;

  Timer1.Enabled := True;
end;

// stop recording & encoding

procedure TForm1.Stop;
begin
  Timer1.Enabled := False;
  BASS_ChannelStop(rchan); // stop recording
  BASS_WMA_EncodeClose(ehandle); // stop encoding
  Button1.Caption := 'Start';
  Edit1.Enabled := True;
  Edit1.Color := clWindow;
  ComboBox1.Enabled := True;
  ComboBox1.Color := clWindow;
  Edit2.Enabled := False;
  Edit2.Color := clBtnFace;
  UpdateDisplay;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  rates: PDWORD;
begin
	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
	begin
		MessageBox(0,'An incorrect version of BASS.DLL was loaded',0,MB_ICONERROR);
		Halt;
	end;

  // setup recording (using default device)
  if not BASS_RecordInit(-1) then
  begin
    MessageBox(0, 'Can''t initialize device', 'Error', 0);
    Halt;
  end
  else
  begin
    // get the available bitrates
    rates := BASS_WMA_EncodeGetRates(SAMPLERATE, CHANNELS, 0);
    if (rates = nil) then
    begin
      MessageBox(0, 'Can''t find codec', 'Error', 0);
      Halt;
    end
    else
    begin
      while (rates^ <> 0) do
      begin
        ComboBox1.Items.Add(IntToStr(rates^));
        Inc(rates);
      end;
    end;

    UpdateDisplay;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (BASS_ChannelIsActive(rchan) = 0) then
    Start
  else
    Stop;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  // update "caption" tag
  BASS_WMA_EncodeSetTag(ehandle, 'Caption', PChar(Edit2.Text), BASS_WMA_TAG_ANSI);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BASS_RecordFree();
  BASS_WMA_EncodeClose(ehandle); // incase it was encoding on exit
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if (BASS_ChannelIsActive(rchan) = 0) then
    Stop
  else
    UpdateDisplay;
end;

end.

