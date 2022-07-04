unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_edit, API_listbox,
  API_trackbar, API_linechart, API_base;

type
  TForm1 = class(TForm)
    API_listbox1: TAPI_listbox;
    API_edit1: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_trackbar1: TAPI_trackbar;
    API_linechart1: TAPI_linechart;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_listbox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure updatechart;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.updatechart;
var
  i: integer;
begin
  api_linechart1.LineColor( 0, clyellow );
  api_linechart1.HistoryCount:= api_listbox1.Items.count;
  for i:=0 to api_listbox1.items.count-1 do
  begin
    api_linechart1.Add( 0, api_listbox1.GetProgress(i) );
  end;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_listbox1.items.add( api_edit1.text );
  api_listbox1.SetProgress( api_listbox1.items.count-1, api_trackbar1.Value );
  api_edit1.Text:= '';
  api_edit1.SetFocus;
  updatechart;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  if api_listbox1.itemindex>-1 then
  begin
    api_listbox1.items[ api_listbox1.itemindex ]:= api_edit1.text;
    api_listbox1.setprogress( api_listbox1.itemindex, api_trackbar1.Value );
    api_edit1.Text:= '';
    api_edit1.SetFocus;
    updatechart;
  end;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  if api_listbox1.itemindex>-1 then
  begin
    api_listbox1.items.Delete( api_listbox1.itemindex );
    api_edit1.Text:= '';
    api_edit1.SetFocus;
    updatechart;
  end;
end;

procedure TForm1.API_listbox1Click(Sender: TObject);
begin
  if api_listbox1.itemindex>-1 then
  begin
    api_edit1.text:= api_listbox1.items[ api_listbox1.itemindex ];
    api_trackbar1.Value:= api_listbox1.GetProgress( api_listbox1.itemindex );
    api_edit1.SetFocus;
  end;
end;

end.
