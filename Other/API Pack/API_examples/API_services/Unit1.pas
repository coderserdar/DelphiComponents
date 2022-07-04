unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_edit, API_services,
  API_base;

type
  TForm1 = class(TForm)
    API_services1: TAPI_services;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    API_edit3: TAPI_edit;
    API_grbutton4: TAPI_grbutton;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
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
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_services1.Computer:= api_edit1.Text;
  api_services1.Service:= api_edit2.text;

  if api_services1.Running then
  begin
    messagedlg('Service is running.', mtinformation, [mbok], 0);
  end else
    messagedlg('Service is not running.', mtwarning, [mbok], 0);
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  api_services1.computer:= api_edit1.text;
  api_services1.Service:= api_edit2.text;

  api_services1.Running:= true;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  api_services1.computer:= api_edit1.text;
  api_services1.Service:= api_edit2.text;

  api_services1.Running:= false;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormShow(Sender: TObject);
begin
  listbox1.clear;
  listbox1.Items:= api_services1.Services;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton4Click(Sender: TObject);
var
  i: integer;
  p: integer;
begin
  if api_edit3.text<>'' then
  begin
    for i:=0 to listbox1.Items.Count-1 do
      if pos(api_edit3.text, listbox1.Items[i]) > 0 then
      begin
        listbox1.itemindex:= i;
        api_edit3.text:= '';
        break;
      end;
    if api_edit3.text<>'' then
      messagedlg('No match.', mtwarning, [mbok], 0);
  end;
end;

end.
