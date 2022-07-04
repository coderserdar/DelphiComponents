unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_tools, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, ComCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Button1: TButton;
    API_tools1: TAPI_tools;
    IdTCPClient1: TIdTCPClient;
    Label2: TLabel;
    Edit1: TEdit;
    ProgressBar1: TProgressBar;
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
  sl: tstringlist;
  i: integer;
begin
  sl:= tstringlist.create;
  try
    // find computers on workgroup
    api_tools1.FindComputers(edit1.text, sl);

    // check basic services
    progressbar1.Position:= 0;
    for i:=0 to sl.count-1 do
    begin
      progressbar1.Position:= round(i/sl.count*100);
      progressbar1.Repaint;

      // remove "\\" from the beginning
      s:= sl[i];
      while pos('\',s)>0 do
        delete(s,pos('\',s),1);

      (*

      // define host to check
      idtcpclient1.Host:= s;
      idtcpclient1.ReadTimeout:= 100;

      // try ftp
      idtcpclient1.Port:= 21;
      try
        idtcpclient1.Connect;
        sl[i]:= sl[i] +', FTP';
        idtcpclient1.disconnect;
      except
      end;

      // try http
      idtcpclient1.port:= 80;
      try
        idtcpclient1.connect;
        sl[i]:= sl[i] +', HTTP';
        idtcpclient1.disconnect;
      except;
      end;

      *)

    end;
    progressbar1.position:= 100;

    // show all on the visible listbox
    listbox1.clear;
    listbox1.items.AddStrings(sl);

  finally
    sl.free;
  end;
end;

end.
