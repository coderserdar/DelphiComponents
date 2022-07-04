unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_XMLDoc, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    API_XMLDoc1: TAPI_XMLDoc;
    Label1: TLabel;
    Label3: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  sl: tstringlist;
begin
  sl:= api_xmldoc1.Nodes(memo1.text, edit1.text, checkbox1.checked);
  try
    if sl.text<>'' then showmessage('Nodes found:'+#13+sl.text)
      else showmessage('No nodes under path'+#13+edit1.text);
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button2Click(Sender: TObject);
begin
  edit2.Text:= api_xmldoc1.GetValue(memo1.Text, edit1.Text);
end;

//------------------------------------------------------------------------------
procedure TForm1.Button3Click(Sender: TObject);
var
  tempXML: string;
begin
  tempXML:= memo1.text;
  if api_xmldoc1.SetValue(tempXML, edit1.text, edit2.text) then
    memo1.text:= tempXML;
end;

end.
