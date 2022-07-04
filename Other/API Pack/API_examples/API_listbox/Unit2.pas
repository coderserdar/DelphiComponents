unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_listbox, API_edit, ComCtrls,
  API_base;

type
  TForm1 = class(TForm)
    API_listbox1: TAPI_listbox;
    API_grbutton1: TAPI_grbutton;
    API_edit1: TAPI_edit;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    API_edit2: TAPI_edit;
    Label1: TLabel;
    Label2: TLabel;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_listbox1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
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
  api_listbox1.Items.Add(api_edit1.text);
  api_listbox1.setprogress(api_listbox1.Items.Count-1, api_edit2.asFloat);
  api_edit1.Text:='';
  api_edit2.Text:='';
  api_edit1.SetFocus;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
var
  i: integer;
begin
  i:=api_listbox1.itemindex;
  if i>-1 then
  begin
    api_listbox1.Items[i]:=api_edit1.text;
    api_listbox1.setprogress(i, api_edit2.asfloat);
  end;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
var
  i: integer;
begin
  i:=api_listbox1.itemindex;
  if i>-1 then
  begin
    api_listbox1.items.delete(i);
    api_listbox1.deleteprogress(i); // NOT GOOD!!!!
    api_edit1.text:='';
    api_edit2.text:='';
    api_edit1.setfocus;
  end;
end;

procedure TForm1.API_listbox1Click(Sender: TObject);
begin
  if api_listbox1.itemindex>-1 then
  begin
    api_edit2.Text:=floattostr(api_listbox1.getprogress(api_listbox1.itemindex));
    api_edit1.text:=api_listbox1.items[api_listbox1.itemindex];
    api_edit1.setfocus;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  api_listbox1.LineColoring:=checkbox1.checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  api_listbox1.ProgressExists:=checkbox2.checked;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  api_listbox1.ItemIndex:= api_listbox1.GetName(api_edit1.Text);
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  api_listbox1.itemindex:= api_listbox1.GetValue(api_edit2.Text);
end;

end.
