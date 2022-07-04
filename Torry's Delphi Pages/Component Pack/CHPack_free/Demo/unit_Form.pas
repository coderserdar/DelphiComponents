unit unit_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls,
  CHCheckBox, CHRadioButton, CHForm, CHButton;

type
  TfrmCHForm = class(TForm)
    GroupBox2: TGroupBox;
    cbOnTopForm: TCHCheckBox;
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    GroupBox1: TGroupBox;
    CHForm1: TCHForm;
    GroupBox3: TGroupBox;
    rbNormalOnTop: TCHRadioButton;
    rbForceOnTop: TCHRadioButton;
    rbShowTitle: TCHRadioButton;
    rbHideTitle: TCHRadioButton;
    GroupBox4: TGroupBox;
    rbMoveOnlyTitle: TCHRadioButton;
    rbMoveEverywhere: TCHRadioButton;
    Label1: TLabel;
    rbMoveNone: TCHRadioButton;
    GroupBox5: TGroupBox;
    CHRadioButton1: TCHRadioButton;
    CHRadioButton2: TCHRadioButton;
    Label2: TLabel;
    GroupBox6: TGroupBox;
    CHButton1: TCHButton;
    CHButton2: TCHButton;
    CHButton3: TCHButton;
    CHButton4: TCHButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure cbOnTopFormClick(Sender: TObject);
    procedure rbNormalOnTopClick(Sender: TObject);
    procedure rbForceOnTopClick(Sender: TObject);
    procedure rbShowTitleClick(Sender: TObject);
    procedure rbHideTitleClick(Sender: TObject);
    procedure rbMoveOnlyTitleClick(Sender: TObject);
    procedure rbMoveEverywhereClick(Sender: TObject);
    procedure rbMoveNoneClick(Sender: TObject);
    procedure CHRadioButton1Click(Sender: TObject);
    procedure CHRadioButton2Click(Sender: TObject);
    procedure CHButton1Click(Sender: TObject);
    procedure CHButton2Click(Sender: TObject);
    procedure CHButton3Click(Sender: TObject);
    procedure CHButton4Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHForm: TfrmCHForm;

implementation

uses CHUnit, unit_About, _CHTypes, unit_Form2, unit_Form3, unit_Form4,
  unit_Form5;

{$R *.dfm}

procedure TfrmCHForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CHForm1.ShowTitlebar(Self);
end;

procedure TfrmCHForm.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHForm.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHForm.cbOnTopFormClick(Sender: TObject);
begin
  if cbOnTopForm.Checked then
    CHForm1.AllwaysOnTop := True
  else
    CHForm1.AllwaysOnTop := False;
end;

procedure TfrmCHForm.rbNormalOnTopClick(Sender: TObject);
begin
  CHForm1.OnTopMode := otNormalOnTop;
end;

procedure TfrmCHForm.rbForceOnTopClick(Sender: TObject);
begin
  CHForm1.OnTopMode := otForceOnTop;
end;

procedure TfrmCHForm.rbShowTitleClick(Sender: TObject);
begin
  CHForm1.ShowTitlebar(Self);
end;

procedure TfrmCHForm.rbHideTitleClick(Sender: TObject);
begin
  CHForm1.HideTitlebar(self);
end;

procedure TfrmCHForm.rbMoveOnlyTitleClick(Sender: TObject);
begin
  CHForm1.MoveMode := foOnlyCaption;
end;

procedure TfrmCHForm.rbMoveEverywhereClick(Sender: TObject);
begin
  CHForm1.MoveMode := foAll;
end;

procedure TfrmCHForm.rbMoveNoneClick(Sender: TObject);
begin
  CHForm1.MoveMode := foNone;
end;

procedure TfrmCHForm.CHRadioButton1Click(Sender: TObject);
begin
  CHForm1.Fullscreen := True;
end;

procedure TfrmCHForm.CHRadioButton2Click(Sender: TObject);
begin
  CHForm1.Fullscreen := False;
end;

procedure TfrmCHForm.CHButton1Click(Sender: TObject);
begin
  frmCHForm2 := TfrmCHForm2.Create(Self);
  frmCHForm2.Show;
end;

procedure TfrmCHForm.CHButton2Click(Sender: TObject);
begin
  frmCHForm3 := TfrmCHForm3.Create(Self);
  frmCHForm3.show;
end;

procedure TfrmCHForm.CHButton3Click(Sender: TObject);
begin
  frmCHForm4 := TfrmCHForm4.Create(Self);
  frmCHForm4.Show;
end;

procedure TfrmCHForm.CHButton4Click(Sender: TObject);
begin
  frmCHForm5 := TfrmCHForm5.Create(Self);
  frmCHForm5.Show;
end;

end.
