unit Bubblehint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TBubbleHintForm = class(TForm)
    Image1: TImage;
    Iconimage: TImage;
    Image3: TImage;
    TitleLabel: TLabel;
    InfoText: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Image3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  BubbleHintForm: TBubbleHintForm;

implementation

{$R *.DFM}

procedure TBubbleHintForm.FormShow(Sender: TObject);
begin
 Brush.style:= bsclear;
end;

procedure TBubbleHintForm.Image3Click(Sender: TObject);
begin
 ModalResult:= mrOK;
 close;
end;

end.
