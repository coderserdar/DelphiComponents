unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_edit, API_inifile,
  API_base;

type
  TForm1 = class(TForm)
    API_inifile1: TAPI_inifile;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    API_edit4: TAPI_edit;
    API_edit5: TAPI_edit;
    API_edit6: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    procedure API_edit1Change(Sender: TObject);
    procedure API_edit2Change(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_edit1Change(Sender: TObject);
begin
  // set filename
  api_inifile1.Filename:= api_edit1.Text;
end;

procedure TForm1.API_edit2Change(Sender: TObject);
begin
  // set section
  api_inifile1.Section:= api_edit2.text;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  // read string
  api_edit4.text:= api_inifile1.TryReadStr(api_edit3.text, '');
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  // read integer
  api_edit5.asinteger(api_inifile1.TryReadInt(api_edit3.text,0));
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  // read float
  api_edit6.asfloat(api_inifile1.TryReadFloat(api_edit3.text,0));
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  // write string
  api_inifile1.WriteStr(api_edit3.Text, api_edit4.Text);
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  // write integer
  api_inifile1.WriteInt(api_edit3.text, api_edit5.asInteger);
end;

procedure TForm1.API_grbutton6Click(Sender: TObject);
begin
  // write float
  api_inifile1.WriteFloat(api_edit3.Text, api_edit6.asFloat);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // apply settings
  api_inifile1.Filename:= api_edit1.text;
  api_inifile1.Section:= api_edit2.text;
end;

end.
