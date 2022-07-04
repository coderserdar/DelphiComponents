unit unit_Panel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls, Buttons,

  CHPanel, CHCheckBox, CHLabel, CHRadioButton;

type
  TfrmCHPanel = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHPanel1: TCHPanel;
    cbTransparent: TCHCheckBox;
    CHPanel3: TCHPanel;
    Image1: TImage;
    ListBox1: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CHPanel4: TCHPanel;
    Label2: TLabel;
    Label1: TLabel;
    CHPanel2: TCHPanel;
    CHPanel5: TCHPanel;
    CHLabel1: TCHLabel;
    CHPanel6: TCHPanel;
    CHLabel2: TCHLabel;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure CHPanel4Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHPanel: TfrmCHPanel;

implementation

uses unit_About;

{$R *.dfm}

procedure TfrmCHPanel.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHPanel.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHPanel.cbTransparentClick(Sender: TObject);
begin
  if cbTransparent.Checked then
  begin
    CHPanel3.Fill.Transparent := True;
  end
  else
  begin
    CHPanel3.Fill.Transparent := False;
  end;
end;

procedure TfrmCHPanel.CHPanel4Resize(Sender: TObject);
begin
  label1.Top := CHPanel4.Height - Label1.Height - 10;
  label1.Left := CHPanel4.Width - label1.Width - 10;
end;

procedure TfrmCHPanel.FormCreate(Sender: TObject);
begin
  label1.Top := CHPanel4.Height - Label1.Height - 10;
  label1.Left := CHPanel4.Width - label1.Width - 10;
end;

end.
