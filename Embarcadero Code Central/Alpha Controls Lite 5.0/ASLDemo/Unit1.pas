unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, sPanel, 
  
  sScrollBar, ExtDlgs, sEdit, Menus, 
  sButton, StdCtrls, sSkinProvider, sSkinManager, sCheckBox, Buttons,
  sBitBtn, sComboBox;

type
  TForm1 = class(TForm)
    sPanel3: TsPanel;
    sPanel2: TsPanel;
    sButton9: TsButton;
    sButton1: TsButton;
    sButton2: TsButton;
    sButton5: TsButton;
    sButton6: TsButton;
    sButton4: TsBitBtn;
    sSkinManager1: TsSkinManager;
    sPanel1: TsPanel;
    sPanel4: TsPanel;
    ComboBox1: TsComboBox;
    Label1: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    sEdit1: TsEdit;
    MainMenu1: TMainMenu;
    MenuItem11: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem141: TMenuItem;
    MenuItem151: TMenuItem;
    MenuItem161: TMenuItem;
    MenuItem1511: TMenuItem;
    MenuItem1521: TMenuItem;
    MenuItem1531: TMenuItem;
    MenuItem1541: TMenuItem;
    MenuItem1551: TMenuItem;
    sScrollBar1: TsScrollBar;
    sSkinProvider1: TsSkinProvider;
    sCheckBox18: TsCheckBox;
    sPanel5: TsPanel;
    sPanel6: TsPanel;
    ComboBox2: TsComboBox;
    sCheckBox1: TsCheckBox;
    sButton3: TsButton;
    sComboBox1: TsComboBox;
    sPanel7: TsPanel;
    sComboBox2: TsComboBox;
    procedure sPanel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sButton4Click(Sender: TObject);
    procedure sButton6Click(Sender: TObject);
    procedure sButton5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure sButton9Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure sCheckBox1Change(Sender: TObject);
    procedure OpenPictureDialog1SelectionChange(Sender: TObject);
    procedure sSkinManager1AfterChange(Sender: TObject);
    procedure sCheckBox18Click(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure sComboBox1Change(Sender: TObject);
    procedure sComboBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Loading : boolean;

implementation

uses
  sSkinProps, FileCtrl, sStyleSimply, sMaskData, ShellApi;

{$R *.DFM}

procedure TForm1.sPanel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then begin
    ReleaseCapture;
    sPanel2.Perform(WM_SYSCOMMAND, $F012, 0);
  end
  else inherited;
end;

procedure TForm1.sButton4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.sButton6Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar('http://www.alphaskins.com'), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.sButton5Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar('mailto: acontrols@alphaskins.com'), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  sSkinManager1.SkinName := sSkinManager1.GetSkinNames(ComboBox1.Items);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  sl : TStringList;
  s : string;
  i : integer;
begin
  if Loading then Exit;
  if ComboBox1.ItemIndex = 0 then begin
    if SelectDirectory(s, [], 0) then begin
      sSkinManager1.SkinDirectory := s;
      sl := TStringList.Create;
      sSkinManager1.SkinName := sSkinManager1.GetSkinNames(sl);
      ComboBox1.Items.Clear;
      ComboBox1.Items.Add('Skins directory...');
      for i := 0 to sl.Count - 1 do begin
        ComboBox1.Items.Add(sl[i]);
      end;
      FreeAndNil(sl);
    end;
  end
  else begin
    sSkinManager1.SkinName := ComboBox1.Text;
  end;
end;

procedure TForm1.sButton9Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then begin
    // SkinSections and PropNames are defined in sSkinProps.pas unit
    sSkinManager1.ChangeImageInSkin(NormalForm, PatternFile, OpenPictureDialog1.FileName);
    sSkinManager1.ChangeImageInSkin(NormalForm, HotPatternFile, OpenPictureDialog1.FileName);
    // Update of all controls
    sSkinManager1.UpdateSkin;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  sl : TStringList;
  i : integer;
begin
  sl := TStringList.Create;
//  sSkinManager1.SkinName :=
  sSkinManager1.GetSkinNames(sl);
  ComboBox1.Clear;
  ComboBox1.Items.Add('Skins directory...');
  for i := 0 to sl.Count - 1 do begin
    ComboBox1.Items.Add(sl[i]);
  end;
  // If no available skins...
  if ComboBox1.Items.Count < 1 then begin
    ComboBox1.Items.Add('No skins available');
    ComboBox1.ItemIndex := 0;
  end
  else begin
    // Sets ComboBox to current skin name value without skin changing
    Loading := True;
    ComboBox1.ItemIndex := sl.IndexOf(sSkinManager1.SkinName) + 1;
    Loading := False;
  end;
  FreeAndNil(sl);
  ComboBox2.ItemIndex := 1;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0 : sSkinProvider1.CaptionAlignment := taLeftJustify;
    1 : sSkinProvider1.CaptionAlignment := taCenter;
    2 : sSkinProvider1.CaptionAlignment := taRightJustify;
  end;

end;

procedure TForm1.sCheckBox1Change(Sender: TObject);
begin
//  sSkinManager1.Active := sCheckBox1.Checked;
end;

procedure TForm1.OpenPictureDialog1SelectionChange(Sender: TObject);
begin
  if (pos('.BMP', UpperCase(OpenPictureDialog1.FileName)) > 0) or
       (pos('.JPG', UpperCase(OpenPictureDialog1.FileName)) > 0) or
         (pos('.BMP', UpperCase(OpenPictureDialog1.FileName)) > 0) then begin
    // SkinSections and PropNames are defined in sSkinProps.pas unit
    sSkinManager1.ChangeImageInSkin(NormalForm, PatternFile, OpenPictureDialog1.FileName);
    sSkinManager1.ChangeImageInSkin(NormalForm, HotPatternFile, OpenPictureDialog1.FileName);
    // Update of all controls
    sSkinManager1.UpdateSkin;
  end;
end;

procedure TForm1.sSkinManager1AfterChange(Sender: TObject);
var
  i : integer;
begin
  i := GetSkinIndex(NormalForm);
  sButton9.Enabled := gd[i].ImagePercent > 0;
end;

procedure TForm1.sCheckBox18Click(Sender: TObject);
begin
  sSkinProvider1.ShowAppIcon := sCheckBox18.Checked;
end;

procedure TForm1.sCheckBox1Click(Sender: TObject);
begin
  sSkinManager1.Active := sCheckBox1.Checked
end;

procedure TForm1.sComboBox1Change(Sender: TObject);
begin
  sButton4.SkinData.SkinSection := sComboBox1.Text
end;

procedure TForm1.sComboBox2Change(Sender: TObject);
begin
  sPanel7.SkinData.SkinSection := sComboBox2.Text
end;

end.
