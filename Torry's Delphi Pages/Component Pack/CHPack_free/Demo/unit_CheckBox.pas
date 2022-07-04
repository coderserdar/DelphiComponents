unit unit_CheckBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  CHCheckBox;

type
  TfrmCHCheckBox = class(TForm)
    MainMenu1: TMainMenu;
    Close1: TMenuItem;
    Info1: TMenuItem;
    CHCheckBox1: TCHCheckBox;
    CHCheckBox2: TCHCheckBox;
    CHCheckBox4: TCHCheckBox;
    CHCheckBox5: TCHCheckBox;
    CHCheckBox6: TCHCheckBox;
    CHCheckBox7: TCHCheckBox;
    CHCheckBox22: TCHCheckBox;
    CHCheckBox23: TCHCheckBox;
    CHCheckBox24: TCHCheckBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    CHCheckBox30: TCHCheckBox;
    CHCheckBox31: TCHCheckBox;
    CHCheckBox32: TCHCheckBox;
    CHCheckBox33: TCHCheckBox;
    CHCheckBox34: TCHCheckBox;
    CHCheckBox35: TCHCheckBox;
    CHCheckBox36: TCHCheckBox;
    CHCheckBox29: TCHCheckBox;
    CHCheckBox26: TCHCheckBox;
    CHCheckBox25: TCHCheckBox;
    CHCheckBox13: TCHCheckBox;
    CHCheckBox12: TCHCheckBox;
    CHCheckBox11: TCHCheckBox;
    CHCheckBox10: TCHCheckBox;
    CHCheckBox9: TCHCheckBox;
    CHCheckBox8: TCHCheckBox;
    CHCheckBox15: TCHCheckBox;
    CHCheckBox16: TCHCheckBox;
    CHCheckBox17: TCHCheckBox;
    CHCheckBox18: TCHCheckBox;
    CHCheckBox19: TCHCheckBox;
    CHCheckBox20: TCHCheckBox;
    CHCheckBox28: TCHCheckBox;
    CHCheckBox27: TCHCheckBox;
    Label2: TLabel;
    CHCheckBox21: TCHCheckBox;
    CHCheckBox14: TCHCheckBox;
    CHCheckBox3: TCHCheckBox;
    CHCheckBox37: TCHCheckBox;
    CHCheckBox38: TCHCheckBox;
    CHCheckBox39: TCHCheckBox;
    procedure Close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHCheckBox: TfrmCHCheckBox;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHCheckBox.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHCheckBox.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

end.
