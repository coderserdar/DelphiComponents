unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_soundfx, ExtCtrls, API_grbutton, StdCtrls, API_label, API_edit,
  API_progressbar, MPlayer, API_audio;

type
  TForm1 = class(TForm)
    API_edit1: TAPI_edit;
    API_label1: TAPI_label;
    API_soundfx1: TAPI_soundfx;
    OpenDialog1: TOpenDialog;
    API_label2: TAPI_label;
    API_edit2: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_progressbar1: TAPI_progressbar;
    Timer1: TTimer;
    API_label3: TAPI_label;
    API_audio1: TAPI_audio;
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  opendialog1.Filter:= 'Wave files (*.wav)|*.wav';
  if opendialog1.Execute then
  begin
    api_edit1.text:= opendialog1.FileName;
  end;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  if fileexists(api_edit1.text) then
    api_soundfx1.playsound(api_edit1.text);
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  opendialog1.Filter:= 'Music (*.wav, *.mp3)|*.wav;*.mp3';
  if opendialog1.Execute then
  begin
    api_edit2.text:= opendialog1.FileName;
    api_audio1.Filename:= api_edit2.Text;
  end;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  if fileexists(api_edit2.text) then
  begin
    api_audio1.Play;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  api_progressbar1.Position:= api_audio1.GetProgress;
  if api_audio1.GetState = mpPlaying then
  begin
    api_label3.caption:= api_audio1.GetStateString + ' ('+inttostr(api_audio1.GetPosition)+'/'+inttostr(api_audio1.getlength)+'s)';
  end else
    api_label3.Caption:= api_audio1.GetStateString;
end;

end.
