unit unit_About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CHButton, CHAdvancedLabel;

type
  TfrmAbout = class(TForm)
    CHButton1: TCHButton;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHAdvancedLabel2: TCHAdvancedLabel;
    CHAdvancedLabel3: TCHAdvancedLabel;
    CHAdvancedLabel4: TCHAdvancedLabel;
    CHAdvancedLabel5: TCHAdvancedLabel;
    CHAdvancedLabel6: TCHAdvancedLabel;
    CHAdvancedLabel7: TCHAdvancedLabel;
    CHAdvancedLabel8: TCHAdvancedLabel;
    CHAdvancedLabel9: TCHAdvancedLabel;
    procedure CHButton1Click(Sender: TObject);
    procedure CHAdvancedLabel5MouseEnter(Sender: TObject);
    procedure CHAdvancedLabel5MouseLeave(Sender: TObject);
    procedure CHAdvancedLabel7MouseEnter(Sender: TObject);
    procedure CHAdvancedLabel7MouseLeave(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.CHButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.CHAdvancedLabel5MouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TfrmAbout.CHAdvancedLabel5MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TfrmAbout.CHAdvancedLabel7MouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TfrmAbout.CHAdvancedLabel7MouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

end.
