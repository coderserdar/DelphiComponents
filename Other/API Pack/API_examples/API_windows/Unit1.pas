unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_linechart, StdCtrls, API_label, API_edit, API_windows,
  API_base;

type
  TForm1 = class(TForm)
    API_windows1: TAPI_windows;
    API_edit1: TAPI_edit;
    API_label1: TAPI_label;
    API_linechart1: TAPI_linechart;
    Timer1: TTimer;
    API_label2: TAPI_label;
    API_label3: TAPI_label;
    Button1: TButton;
    API_label4: TAPI_label;
    API_label5: TAPI_label;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    foldcaption: string;
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
  h: cardinal;
begin
  h:= api_windows1.HandleFromCaption(api_Edit1.text);
  if h>0 then
  begin
    api_windows1.ForceForeground(h);
  end else
    messagedlg('Handle not Found', mterror, [mbok], 0);
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  h: cardinal;
  mem: cardinal;
begin
  // find caption
  h:= api_windows1.HandleFromCaption(api_edit1.text);
  if h>0 then
  begin
    // if caption changed
    if foldcaption<>api_edit1.text then
    begin
      foldcaption:= api_edit1.text;
    end;

    // get memory usage
    mem:= api_windows1.MemoryUsage(h);

    // module filename
    api_label5.Caption:= api_windows1.Filename(h);

    // draw stuff
    api_label3.Caption:= inttostr(mem)+' Byte(s)';
    api_linechart1.LineColor(0, clred);
    api_linechart1.Add(0, mem);
  end else

    // no such window found
    api_label3.caption:= 'Handle not Found';
end;

end.
