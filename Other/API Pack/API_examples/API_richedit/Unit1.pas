unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, API_richedit, ExtCtrls;

type
  TForm1 = class(TForm)
    API_richedit1: TAPI_richedit;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Bevel1: TBevel;
    Edit1: TEdit;
    Label12: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure API_richedit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    oldtext: string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  x,y: integer;
  fnt: tfont;
begin
  // show position
  x:=api_richedit1.PosX;
  y:=api_richedit1.PosY;
  label1.caption:= inttostr(api_richedit1.PosX);
  label2.caption:= inttostr(api_richedit1.posY);
  label3.caption:= api_richedit1.GetWord;

  // if highlight text is changed
  if edit1.text<>oldtext then
  begin
    oldtext:= edit1.text;
    fnt:= tfont.create;
    try
      // set defaults
      api_richedit1.SelStart:= 0;
      api_richedit1.SelLength:= length(api_richedit1.Lines.text);
      api_richedit1.SelAttributes:= api_richedit1.DefAttributes;
      // mark highlighted
      fnt.Name:= 'Tahome';                          //
      fnt.Color:= clred;
      api_richedit1.MarkWords(edit1.text, fnt);
      label5.caption:= edit1.text;
    finally
      fnt.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_richedit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // on left mouse button click, the word under
  // the mouse is shown at the label6.text
  if button=mbleft then
  begin
    label7.caption:=inttostr(x);
    label8.caption:=inttostr(y);
    label6.caption:=api_richedit1.GetWordAtPoint(x,y);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  font: tfont;
begin
  // clear old highlighted text memory
  oldtext:= '';

  // add couple of syntaxes :)
  font:= tfont.create;
  try
    font.Name:= 'verdana';
    font.Color:= clnavy;
    font.size:= 10;
    api_richedit1.AddSyntax('this', font);
  finally
    font.free;
  end;
end;

end.
