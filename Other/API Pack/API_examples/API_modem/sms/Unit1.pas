unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_modem, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    API_modem1: TAPI_modem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
var
  port: integer;
  brate: integer;
  smscenter: string;
  recipient: string;
  text: string;
begin
  try
    // get input values
    port:= strtoint(edit4.text);
    brate:= strtoint(edit5.text);
    smscenter:= edit1.text;
    recipient:= edit2.text;
    text:= edit3.text;

    // set modem settings
    api_modem1.port:= port;
    api_modem1.baudrate:= brate;

    // open port
    api_modem1.ClearLog;
    api_modem1.open:= true;

    // send sms message
    if api_modem1.SendSMS_TextMode(recipient, text) then
      messagedlg('SMS sent succesfully', mtinformation, [mbok], 0)
      else messagedlg('Failed to send SMS message', mterror, [mbok], 0);

    // close port
    api_modem1.open:= false;
  except
    on e: exception do
      messagedlg('Exception while sending SMS:'+#13
        +e.message, mterror, [mbok], 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  memo1.Text:= api_modem1.GetLog;
end;

end.
