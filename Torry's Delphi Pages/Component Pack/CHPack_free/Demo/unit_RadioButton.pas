unit unit_RadioButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  CHRadioButton;

type
  TfrmCHRadioButton = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHRadioButton1: TCHRadioButton;
    CHRadioButton2: TCHRadioButton;
    CHRadioButton3: TCHRadioButton;
    CHRadioButton4: TCHRadioButton;
    CHRadioButton5: TCHRadioButton;
    CHRadioButton6: TCHRadioButton;
    CHRadioButton25: TCHRadioButton;
    CHRadioButton26: TCHRadioButton;
    CHRadioButton27: TCHRadioButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    CHRadioButton38: TCHRadioButton;
    CHRadioButton39: TCHRadioButton;
    CHRadioButton40: TCHRadioButton;
    CHRadioButton41: TCHRadioButton;
    CHRadioButton42: TCHRadioButton;
    CHRadioButton43: TCHRadioButton;
    CHRadioButton30: TCHRadioButton;
    CHRadioButton29: TCHRadioButton;
    CHRadioButton28: TCHRadioButton;
    CHRadioButton12: TCHRadioButton;
    CHRadioButton11: TCHRadioButton;
    CHRadioButton10: TCHRadioButton;
    CHRadioButton9: TCHRadioButton;
    CHRadioButton8: TCHRadioButton;
    CHRadioButton7: TCHRadioButton;
    CHRadioButton31: TCHRadioButton;
    CHRadioButton37: TCHRadioButton;
    CHRadioButton18: TCHRadioButton;
    CHRadioButton17: TCHRadioButton;
    CHRadioButton16: TCHRadioButton;
    CHRadioButton15: TCHRadioButton;
    CHRadioButton14: TCHRadioButton;
    CHRadioButton13: TCHRadioButton;
    CHRadioButton19: TCHRadioButton;
    CHRadioButton20: TCHRadioButton;
    CHRadioButton21: TCHRadioButton;
    CHRadioButton22: TCHRadioButton;
    CHRadioButton23: TCHRadioButton;
    CHRadioButton24: TCHRadioButton;
    CHRadioButton35: TCHRadioButton;
    CHRadioButton33: TCHRadioButton;
    CHRadioButton36: TCHRadioButton;
    CHRadioButton32: TCHRadioButton;
    CHRadioButton34: TCHRadioButton;
    procedure close1Click(Sender: TObject);
    procedure CHRadioButton33MouseEnter(Sender: TObject);
    procedure CHRadioButton33MouseLeave(Sender: TObject);
    procedure CHRadioButton35MouseEnter(Sender: TObject);
    procedure CHRadioButton36MouseEnter(Sender: TObject);
    procedure CHRadioButton32MouseEnter(Sender: TObject);
    procedure CHRadioButton34MouseEnter(Sender: TObject);
    procedure CHRadioButton34MouseLeave(Sender: TObject);
    procedure CHRadioButton36MouseLeave(Sender: TObject);
    procedure CHRadioButton32MouseLeave(Sender: TObject);
    procedure CHRadioButton35MouseLeave(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHRadioButton: TfrmCHRadioButton;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHRadioButton.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHRadioButton.CHRadioButton33MouseEnter(Sender: TObject);
begin
  CHRadioButton33.Color := clInfoBk;
end;


procedure TfrmCHRadioButton.CHRadioButton33MouseLeave(Sender: TObject);
begin
  CHRadioButton33.Color := clBtnFace;
end;

procedure TfrmCHRadioButton.CHRadioButton35MouseEnter(Sender: TObject);
begin
  CHRadioButton35.Color := clInfoBk;
end;

procedure TfrmCHRadioButton.CHRadioButton35MouseLeave(Sender: TObject);
begin
  CHRadioButton35.Color := clBtnFace;
end;

procedure TfrmCHRadioButton.CHRadioButton36MouseEnter(Sender: TObject);
begin
  CHRadioButton36.Color := clInfoBk;
end;

procedure TfrmCHRadioButton.CHRadioButton36MouseLeave(Sender: TObject);
begin
  CHRadioButton36.Color := clBtnFace;
end;

procedure TfrmCHRadioButton.CHRadioButton32MouseEnter(Sender: TObject);
begin
  CHRadioButton32.Color := clInfoBk;
end;

procedure TfrmCHRadioButton.CHRadioButton32MouseLeave(Sender: TObject);
begin
  CHRadioButton32.Color := clBtnFace;
end;

procedure TfrmCHRadioButton.CHRadioButton34MouseEnter(Sender: TObject);
begin
  CHRadioButton34.Color := clInfoBk;
end;

procedure TfrmCHRadioButton.CHRadioButton34MouseLeave(Sender: TObject);
begin
  CHRadioButton34.Color := clBtnFace;
end;







procedure TfrmCHRadioButton.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
