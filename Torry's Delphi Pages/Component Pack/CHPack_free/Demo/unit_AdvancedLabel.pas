unit unit_AdvancedLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus,
  CHAdvancedLabel;

type
  TfrmCHAdvancedLabel = class(TForm)
    MainMenu1: TMainMenu;
    Schlieen1: TMenuItem;
    Info1: TMenuItem;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHAdvancedLabel2: TCHAdvancedLabel;
    CHAdvancedLabel3: TCHAdvancedLabel;
    CHAdvancedLabel4: TCHAdvancedLabel;
    CHAdvancedLabel5: TCHAdvancedLabel;
    CHAdvancedLabel6: TCHAdvancedLabel;
    CHAdvancedLabel7: TCHAdvancedLabel;
    CHAdvancedLabel8: TCHAdvancedLabel;
    CHAdvancedLabel9: TCHAdvancedLabel;
    CHAdvancedLabel10: TCHAdvancedLabel;
    CHAdvancedLabel12: TCHAdvancedLabel;
    CHAdvancedLabel11: TCHAdvancedLabel;
    CHAdvancedLabel13: TCHAdvancedLabel;
    procedure Schlieen1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure CHAdvancedLabel6MouseEnter(Sender: TObject);
    procedure CHAdvancedLabel6MouseLeave(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHAdvancedLabel: TfrmCHAdvancedLabel;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHAdvancedLabel.Schlieen1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHAdvancedLabel.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;


procedure TfrmCHAdvancedLabel.CHAdvancedLabel6MouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TfrmCHAdvancedLabel.CHAdvancedLabel6MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

end.
