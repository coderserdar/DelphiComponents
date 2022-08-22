unit Test_jbEdit1;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, jbEdit, Mask;

type
  TForm1 = class(TForm)
    PubEdit1: TPubEdit;
    Button1: TButton;
    PubEdit2: TPubEdit;
    PubEdit3: TPubEdit;
    PubEdit4: TPubEdit;
    PubEdit5: TPubEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    PubEdit6: TPubEdit;
    Label3: TLabel;
    Label7: TLabel;
    PubMaskEdit1: TPubMaskEdit;
    Label8: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure PubEdit1Mask(Sender: TObject; ToDirection: TToDirection;
      Mask: String; var Value: String);
    procedure PubEdit6Convert(Sender: TObject; ToDirection: TToDirection;
      var Value: String);
    procedure PubEdit1Validate(Sender: TObject; var Value: String;
      var CanExit: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

Uses jbStr;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.SetFocus;
  close
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  PubEdit3.Enabled := Not CheckBox1.Checked;
end;

procedure TForm1.PubEdit1Mask(Sender: TObject; ToDirection: TToDirection;
  Mask: String; var Value: String);
begin
  If Value <> '' Then
    Case ToDirection of
      eValueToText:Value:=ExtractNumber(Value);
      eTextToValue:Value:=Zip('XX.XX.XX/XXXX',Value);
    End;
end;

procedure TForm1.PubEdit6Convert(Sender: TObject;
  ToDirection: TToDirection; var Value: String);
begin
  If Value <> '' Then
    Case ToDirection of
      eValueToText: Value := IntToStr(Num(Value,16));
      eTextToValue: Value := Doc(StrToInt(Value),16);
    End;
end;

procedure TForm1.PubEdit1Validate(Sender: TObject; var Value: String;
  var CanExit: Boolean);
Var E:Extended;
begin
  CanExit:=True;
  If Value='' Then Exit;
  E:=StrToFloat(Value);
  {test na datum neni zaveden, sorry}
  If Length(Value)=9 Then Exit;
  If Length(Value)=10 Then Begin
    {lze pocitat jen pro RC po 1.1.1954 a delky 10 zn.}
    E:=E-11*Int(E/11);
    CanExit := E=0;
  End
end;

end.
