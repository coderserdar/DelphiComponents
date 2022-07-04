unit unit_Label;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, ExtCtrls,

  CHLabel, CHPanel;

type
  TfrmCHLabel = class(TForm)
    MainMenu1: TMainMenu;
    Schlieen1: TMenuItem;
    Info1: TMenuItem;
    CHLabel1: TCHLabel;
    CHLabel8: TCHLabel;
    CHLabel12: TCHLabel;
    CHLabel2: TCHLabel;
    CHLabel10: TCHLabel;
    CHLabel4: TCHLabel;
    CHLabel9: TCHLabel;
    CHLabel11: TCHLabel;
    CHLabel5: TCHLabel;
    CHLabel3: TCHLabel;
    CHLabel7: TCHLabel;
    CHLabel13: TCHLabel;
    CHLabel6: TCHLabel;
    procedure CHLabel6MouseEnter(Sender: TObject);
    procedure CHLabel6MouseLeave(Sender: TObject);
    procedure CHLabel7MouseEnter(Sender: TObject);
    procedure CHLabel7MouseLeave(Sender: TObject);
    procedure Schlieen1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHLabel: TfrmCHLabel;

implementation

uses unit_About, CHUnit, _CHTypes;

{$R *.dfm}

procedure TfrmCHLabel.CHLabel6MouseEnter(Sender: TObject);
begin
  CHLabel6.Cursor := crHandPoint;
end;

procedure TfrmCHLabel.CHLabel6MouseLeave(Sender: TObject);
begin
  CHLabel6.Cursor := crDefault;
end;

procedure TfrmCHLabel.CHLabel7MouseEnter(Sender: TObject);
begin
  CHLabel7.Cursor := crCross;
end;

procedure TfrmCHLabel.CHLabel7MouseLeave(Sender: TObject);
begin
  CHLabel7.Cursor := crDefault;
end;

procedure TfrmCHLabel.Schlieen1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHLabel.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;


end.
