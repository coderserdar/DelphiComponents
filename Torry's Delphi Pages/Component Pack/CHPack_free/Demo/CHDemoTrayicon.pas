unit CHDemoTrayicon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CHEdit, CHAdvancedLabel, CHRadioButton, Menus,
  CHTrayIcon, CHCheckBox;

type
  TfrmTrayiconDemo = class(TForm)
    CHCheckBox1: TCHCheckBox;
    CHCheckBox2: TCHCheckBox;
    PopupMenu1: TPopupMenu;
    About1: TMenuItem;
    N1: TMenuItem;
    Close2: TMenuItem;
    CHTrayIcon1: TCHTrayIcon;
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    GroupBox2: TGroupBox;
    CHRadioButton1: TCHRadioButton;
    CHRadioButton2: TCHRadioButton;
    CHRadioButton3: TCHRadioButton;
    CHRadioButton4: TCHRadioButton;
    GroupBox3: TGroupBox;
    CHRadioButton5: TCHRadioButton;
    CHRadioButton6: TCHRadioButton;
    CHRadioButton7: TCHRadioButton;
    CHRadioButton8: TCHRadioButton;
    GroupBox1: TGroupBox;
    CHAdvancedLabel1: TCHAdvancedLabel;
    CHEdit1: TCHEdit;
    CHCheckBox3: TCHCheckBox;
    Label1: TLabel;
    procedure CHCheckBox1Click(Sender: TObject);
    procedure CHCheckBox2Click(Sender: TObject);
    procedure CHRadioButton1Click(Sender: TObject);
    procedure CHRadioButton2Click(Sender: TObject);
    procedure CHRadioButton3Click(Sender: TObject);
    procedure CHRadioButton4Click(Sender: TObject);
    procedure CHRadioButton5Click(Sender: TObject);
    procedure CHRadioButton6Click(Sender: TObject);
    procedure CHRadioButton7Click(Sender: TObject);
    procedure CHRadioButton8Click(Sender: TObject);
    procedure CHCheckBox3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmTrayiconDemo: TfrmTrayiconDemo;

implementation

uses _CHTypes;

{$R *.dfm}

procedure TfrmTrayiconDemo.CHCheckBox1Click(Sender: TObject);
begin
  if CHCheckBox1.Checked then
    CHTrayIcon1.Blink.Enabled := True
  else
    CHTrayIcon1.Blink.Enabled := False;
end;

procedure TfrmTrayiconDemo.CHCheckBox2Click(Sender: TObject);
begin
  if CHCheckBox2.Checked then
    CHTrayIcon1.ShowTaskBar := True
  else
    CHTrayIcon1.ShowTaskBar := False;
end;

procedure TfrmTrayiconDemo.CHRadioButton1Click(Sender: TObject);
begin
  CHTrayIcon1.ShowIcon := tyAllways;
end;

procedure TfrmTrayiconDemo.CHRadioButton2Click(Sender: TObject);
begin
  CHTrayIcon1.ShowIcon := tyNever;
end;

procedure TfrmTrayiconDemo.CHRadioButton3Click(Sender: TObject);
begin
  CHTrayIcon1.ShowIcon := tyOnMinimized;
end;

procedure TfrmTrayiconDemo.CHRadioButton4Click(Sender: TObject);
begin
  CHTrayIcon1.ShowIcon := tyOnRestore;
end;

procedure TfrmTrayiconDemo.CHRadioButton5Click(Sender: TObject);
begin
  CHTrayIcon1.ShowTaskEntry := tyAllways;
end;

procedure TfrmTrayiconDemo.CHRadioButton6Click(Sender: TObject);
begin
  CHTrayIcon1.ShowTaskEntry := tyNever;
end;

procedure TfrmTrayiconDemo.CHRadioButton7Click(Sender: TObject);
begin
  CHTrayIcon1.ShowTaskEntry := tyOnMinimized;
end;

procedure TfrmTrayiconDemo.CHRadioButton8Click(Sender: TObject);
begin
  CHTrayIcon1.ShowTaskEntry := tyOnRestore;
end;

procedure TfrmTrayiconDemo.CHCheckBox3Click(Sender: TObject);
begin
  if CHCheckBox3.Checked then
  begin
    CHTrayIcon1.Mode := trmText;
    CHTrayIcon1.IconText.Text := CHEdit1.Text;
  end
  else
  begin
    CHTrayIcon1.Mode := trmIcon;
  end;
end;

end.
