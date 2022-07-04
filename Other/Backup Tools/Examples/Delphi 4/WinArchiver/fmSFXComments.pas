unit fmSFXComments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TSFXComments = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    mmBefore: TMemo;
    mmAfter: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Déclarations privées }
    FBefore : String;
    FAfter : String;
  public
    { Déclarations publiques }
  end;

var
  SFXComments: TSFXComments;

implementation

{$R *.DFM}

procedure TSFXComments.Button1Click(Sender: TObject);
begin
  mmBefore.Lines.Clear;
  mmAfter.Lines.Clear;
end;

procedure TSFXComments.FormShow(Sender: TObject);
begin
  mmBefore.Text := FBefore;
  mmAfter.Text := FAfter;
end;

procedure TSFXComments.BitBtn1Click(Sender: TObject);
begin
  FBefore := mmBefore.Text;
  FAfter := mmAfter.Text;
end;

end.
