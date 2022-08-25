unit umain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UDESCryp;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  DESCrypt1: TDESCrypt;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
   with DESCrypt1 do
   begin
     Input:=AnsiString(edit1.text);
     Salt:=AnsiString(edit3.text);
     if execute then edit2.text:=String(Output);
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    DESCrypt1 := TDESCrypt.Create (self) ;
end;

end.
