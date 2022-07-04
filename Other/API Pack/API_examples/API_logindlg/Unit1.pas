unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_logindlg, API_base, API_usermanager;

type
  TForm1 = class(TForm)
    tAPI_logindlg1: tAPI_logindlg;
    Button1: TButton;
    ListBox1: TListBox;
    API_usermanager1: TAPI_usermanager;
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
begin
  if tapi_logindlg1.Execute then
  begin
    api_usermanager1.Username:= tapi_logindlg1.User;
    api_usermanager1.Password:= tapi_logindlg1.Pass;
    if api_usermanager1.Level>0 then
    begin
      listbox1.items.insert(0,'Login: '+tapi_logindlg1.user+' '+tapi_logindlg1.Pass+' succesfully.');
    end else
      listbox1.items.insert(0,'Login: '+tapi_logindlg1.user+' '+tapi_logindlg1.Pass+' failed.');
  end else
    listbox1.items.insert(0,'Login cancelled.');
end;

end.
