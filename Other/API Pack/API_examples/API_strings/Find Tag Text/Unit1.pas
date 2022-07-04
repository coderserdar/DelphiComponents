unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit3: TEdit;
    Label7: TLabel;
    Edit4: TEdit;
    Button3: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Button4: TButton;
    Edit5: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  api_strings;

{$R *.dfm}

//------------------------------------------------------------------------------
// find tagged text using customized tags, <b> and </b> on this example
procedure TForm1.Button2Click(Sender: TObject);
var
  StartPosition: integer;
  FoundAtPosition: integer;
begin
  StartPosition:= strtoint(edit2.text);
  label1.caption:= api_strings.FindTagText(edit1.text, StartPosition, FoundAtPosition, edit3.text, edit4.text);
  if FoundAtPosition>0 then edit2.text:= inttostr(FoundAtPosition);
end;

//------------------------------------------------------------------------------
// reset start position to zero
procedure TForm1.Button3Click(Sender: TObject);
begin
  edit1.text:= '<html><head>böö</head><><body bgcolor=#ffffff>testing <!-- <b>this</b> --> shit<img src="test"></body></html>'; //<html><body bgcolor=#ffffff>testing <!-- <b>this</b> --> shit<img src="test"></body></html>';
  edit2.Text:= '1';
  edit3.text:= '<b>';
  edit4.text:= '</b>';
end;

//------------------------------------------------------------------------------
procedure TForm1.Button4Click(Sender: TObject);
var
  p: integer;
begin
  p:= api_Strings.PosFromEnd(edit5.text, edit1.text);
  if p>0 then
  begin
    messagedlg('"'+copy(edit1.text, 1, p-1)+'"'+#13+'is located just before it.', mtinformation, [mbok], 0);
  end else
    messagedlg('Was not found', mterror, [mbok], 0);
end;

//------------------------------------------------------------------------------
procedure TForm1.FormActivate(Sender: TObject);
begin
  label2.caption:= inttostr(length(edit1.text));
end;

//------------------------------------------------------------------------------
// find tagged text default "<" and ">" as ending -> html tags search
procedure TForm1.Button1Click(Sender: TObject);
var
  StartPosition: integer;
  FoundAtPosition: integer;
  StartPos: integer;
  TagLength: integer;
begin
  StartPosition:= strtoint(edit2.text);
  label1.caption:= api_strings.FindTagText(edit1.text, StartPosition, FoundAtPosition, StartPos, TagLength);
  if FoundAtPosition>0 then
  begin
    edit2.text:= inttostr(FoundAtPosition);
  end;
  label9.caption:= inttostr(StartPos)+', len='+inttostr(TagLength);
end;

end.
