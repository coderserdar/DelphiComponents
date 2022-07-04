unit Unit1;

interface

uses
  Windows, Messages, Classes, Controls, Forms,
  CryptoAPI, HashTests, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  buf: array[0..2] of Char;
  ret: LongWord;
  ctx: THashContext;
  S: String;
begin
  buf[0] := 'a'; buf[1] := 'b'; buf[2] := 'c'; {or buf := 'abc';}
  ret := HashInit(@ctx, HASH_MD5);
  if ret = NO_ERROR then
    ret := HashUpdate(@ctx, @buf, SizeOf(buf));
  if ret = NO_ERROR then
    ret := HashFinal(@ctx, S);
  if ret = NO_ERROR then
    Edit1.Text := S  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit1.Text := HashErrorToStr(ret);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ret: LongWord;
  S: String;
begin
  ret := HashStr(HASH_MD5, 'abc', S);
  if ret = NO_ERROR then
    Edit2.Text := S  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit2.Text := HashErrorToStr(ret);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ret: LongWord;
  S: String;
begin
  ret := HashFilePartial(HASH_MD5, 'file.txt', 5, 7, S); //file.txt contains 'abc' substr from offset at 5-th byte
  if ret = NO_ERROR then
    Edit3.Text := S  //should be 900150983cd24fb0d6963f7d28e17f72
  else
    Edit3.Text := HashErrorToStr(ret);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ErrNo: LongWord;
begin
  {Test library before startup}
  ErrNo := HashTestLibrary;
  if ErrNo <> HASH_NOERROR then
  begin
    MessageBox(0, PChar('Could not verify library, error: ' + HashErrorToStr(ErrNo)), nil, 0);
    Halt;
  end;
end;

end.
