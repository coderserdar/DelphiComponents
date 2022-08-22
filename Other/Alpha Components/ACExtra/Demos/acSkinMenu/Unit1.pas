unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, sSkinProvider, sSkinManager, StdCtrls, sEdit,
  sSpinEdit, sCheckBox, Mask, sMaskEdit, sCustomComboEdit, sTooledit,
  sSpeedButton, aceSkinMenu, Buttons;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    File1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    acSkinMenu1: TacSkinMenu;
    Externalskins1: TMenuItem;
    Internalskins1: TMenuItem;
    Allskins1: TMenuItem;
    N2: TMenuItem;
    sSpinEdit1: TsSpinEdit;
    sCheckBox1: TsCheckBox;
    sDirectoryEdit1: TsDirectoryEdit;
    pmAll: TPopupMenu;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    pmExt: TPopupMenu;
    pmInt: TPopupMenu;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sSpinEdit1Change(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure sDirectoryEdit1Change(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sSpinEdit1.Value := acSkinMenu1.ItemsInColumn;
  sCheckBox1.Checked := sSkinManager1.Active;
  sDirectoryEdit1.Text := sSkinManager1.SkinDirectory;
end;

procedure TForm1.sCheckBox1Click(Sender: TObject);
begin
  sSkinManager1.Active := sCheckBox1.Checked;
end;

procedure TForm1.sDirectoryEdit1Change(Sender: TObject);
begin
  sSkinManager1.SkinDirectory := sDirectoryEdit1.Text;
end;

procedure TForm1.sSpinEdit1Change(Sender: TObject);
begin
  acSkinMenu1.ItemsInColumn := sSpinEdit1.Value;
end;

end.
