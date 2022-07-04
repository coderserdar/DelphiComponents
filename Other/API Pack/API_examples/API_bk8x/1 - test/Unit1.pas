unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_bk8x, StdCtrls, ExtCtrls, SyncObjs, API_base;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    API_bk8x1: TAPI_bk8x;
    procedure API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
    procedure API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
      Inputs: TBuffer; var Outputs: TBuffer);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCycleData = record
    lock: tcriticalsection;
    cycletime: double;
  end;

var
  Form1: TForm1;
  Data: tcycledata;

implementation

{$R *.dfm}

uses
  dateutils, api_files;

procedure TForm1.API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
begin
  // thread error event is always synchronized with the
  // mainthread to make sure that the message is able
  // to use on anything that it's needed..
  label1.caption:= errormsg;

  listbox1.items.insert(0, timetostr(now)+'> '+errormsg);

  // stop on error
  if checkbox1.checked then
    api_bk8x1.Open:= false;
end;

procedure TForm1.API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
  Inputs: TBuffer; var Outputs: TBuffer);
begin
  // ..
  data.lock.Acquire;
  try
    data.cycletime:= cycleMSec;
  finally
    data.lock.release;
  end;

  // notes:
  // sender.Synchronize();
  // sender.suspend;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not api_bk8x1.open then
  begin
    label1.caption:= '';
    label2.caption:= '';
    label3.caption:= '';
    label4.caption:= '';
    label5.caption:= '';
    listbox1.Clear;
  end;

  // change state
  api_bk8x1.Open:= not api_bk8x1.open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  data.lock:= tcriticalsection.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  data.lock.free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // show data transfers and last error
  label2.Caption:= api_files.bytestostr(api_bk8x1.BytesRead);
  label3.Caption:= api_files.bytestostr(api_bk8x1.BytesSent);

  // show online time and button text as possible
  if api_bk8x1.StartTime>0 then
    label5.caption:= formatfloat('0.0', minutespan(now, api_bk8x1.StartTime))+'min online'
    else label5.caption:= 'Stopped';
  if api_bk8x1.open then  button1.Caption:= 'Stop'
    else button1.caption:= 'Start';

  // show in/out queues
  label6.Caption:= inttostr(api_bk8x1.ReadQueue);
  label7.caption:= inttostr(api_bk8x1.SendQueue);

  // show cycle time
  data.lock.Acquire;
  try
    label1.Caption:= floattostr(data.cycletime)+'ms';
  finally
    data.lock.Release;
  end;
end;

end.
