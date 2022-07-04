unit unit_Multigradient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ExtCtrls, StdCtrls, 
  CHMultiGradient;

type
  TfrmCHMultigradient = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    Timer2: TTimer;
    CHMultiGradient2: TCHMultiGradient;
    CHMultiGradient5: TCHMultiGradient;
    CHMultiGradient9: TCHMultiGradient;
    CHMultiGradient7: TCHMultiGradient;
    CHMultiGradient6: TCHMultiGradient;
    CHMultiGradient4: TCHMultiGradient;
    CHMultiGradient10: TCHMultiGradient;
    CHMultiGradient8: TCHMultiGradient;
    CHMultiGradient3: TCHMultiGradient;
    procedure close1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    nGradPos : Integer;
  end;

var
  frmCHMultigradient: TfrmCHMultigradient;

implementation

uses unit_About, _CHTypes;

{$R *.dfm}

procedure TfrmCHMultigradient.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHMultigradient.Timer2Timer(Sender: TObject);
begin
  nGradPos := nGradPos - 5;

  if nGradPos > -100 then
  begin
    CHMultiGradient7.Gradient.Rotation := nGradPos;
  end
  else
  begin
    nGradPos := 100;
    CHMultiGradient7.Gradient.Rotation := nGradPos;
  end;
end;

procedure TfrmCHMultigradient.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
