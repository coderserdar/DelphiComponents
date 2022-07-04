unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_tools, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
  StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    IdUDPClient1: TIdUDPClient;
    API_tools1: TAPI_tools;
    Edit2: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
begin

  // generate wakeup message
  s:= api_tools1.WakeOnLanMessage(edit1.text);

  // connect to host and send message
  idudpclient1.Host:= edit2.text;
  idudpclient1.Port:= 2020;
  try
    idudpclient1.Send(s);
    messagedlg('Wakeup Message Send.', mtinformation, [mbok], 0);
  except
    messagedlg('Failed to send Wakeup Message!', mterror, [mbok], 0);
  end;
  
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sl: tstringlist;
begin

  // get ethernet address(es)
  sl:= tstringlist.create;
  try
    sl.text:= api_tools1.EthernetAddresses.text;
    if sl.count>0 then edit1.text:= sl[0]
      else edit1.text:= '';
  finally
    sl.free;
  end;

  // set host
  edit2.text:= '127.0.0.1';
  
end;

end.
