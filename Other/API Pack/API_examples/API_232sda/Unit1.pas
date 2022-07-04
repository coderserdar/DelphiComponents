unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_grbutton, ExtCtrls, API_linechart, ComCtrls,
  API_232sda;

type
  TForm1 = class(TForm)
    API_232sda1: TAPI_232sda;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    API_linechart1: TAPI_linechart;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Timer1: TTimer;
    Panel1: TPanel;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    procedure API_grbutton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  if api_232sda1.open then
  begin
    api_232sda1.open:=false;
  end else
  begin
    api_232sda1.port:=tport(combobox1.ItemIndex+1);
    api_232sda1.open:=true;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // check port state
  if api_232sda1.open then
  begin
    panel1.Color:=clgreen;
    api_grbutton1.Caption:='stop';
  end else
  begin
    panel1.color:=clred;
    api_grbutton1.Caption:='start';
  end;

  // readwrite sda
  if api_232sda1.readwrite then
  begin
    // add new lines to chart
    api_linechart1.add(0,api_232sda1.ai00);
    api_linechart1.add(1,api_232sda1.ai01);
    api_linechart1.add(2,api_232sda1.ai02);
    api_linechart1.add(3,api_232sda1.ai03);
    api_linechart1.add(4,api_232sda1.ai04);
    api_linechart1.add(5,api_232sda1.ai05);
    api_linechart1.add(6,api_232sda1.ai06);
    api_linechart1.add(7,api_232sda1.ai07);
    api_linechart1.add(8,api_232sda1.ai08);
    api_linechart1.add(9,api_232sda1.ai09);
    api_linechart1.add(10,api_232sda1.ai10);

    // list values
    listbox1.clear;
    listbox1.items.add(inttostr(api_232sda1.ai10));
    listbox1.items.add(inttostr(api_232sda1.ai09));
    listbox1.items.add(inttostr(api_232sda1.ai08));
    listbox1.items.add(inttostr(api_232sda1.ai07));
    listbox1.items.add(inttostr(api_232sda1.ai06));
    listbox1.items.add(inttostr(api_232sda1.ai05));
    listbox1.items.add(inttostr(api_232sda1.ai04));
    listbox1.items.add(inttostr(api_232sda1.ai03));
    listbox1.items.add(inttostr(api_232sda1.ai02));
    listbox1.items.add(inttostr(api_232sda1.ai01));
    listbox1.items.add(inttostr(api_232sda1.ai00));
  end;

  // api_232sda last error
  label3.caption:=api_232sda1.lasterror;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  close;
end;

end.
