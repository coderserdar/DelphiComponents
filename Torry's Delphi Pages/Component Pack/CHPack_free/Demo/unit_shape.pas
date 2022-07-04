unit unit_shape;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, CHShape;

type
  TfrmCHShape = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHShape1: TCHShape;
    CHShape2: TCHShape;
    CHShape3: TCHShape;
    CHShape4: TCHShape;
    CHShape5: TCHShape;
    CHShape6: TCHShape;
    CHShape7: TCHShape;
    CHShape8: TCHShape;
    CHShape9: TCHShape;
    CHShape10: TCHShape;
    CHShape11: TCHShape;
    CHShape12: TCHShape;
    CHShape13: TCHShape;
    CHShape14: TCHShape;
    CHShape15: TCHShape;
    CHShape16: TCHShape;
    CHShape17: TCHShape;
    CHShape18: TCHShape;
    CHShape19: TCHShape;
    CHShape20: TCHShape;
    CHShape21: TCHShape;
    CHShape22: TCHShape;
    CHShape23: TCHShape;
    CHShape24: TCHShape;
    CHShape25: TCHShape;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHShape: TfrmCHShape;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHShape.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHShape.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
