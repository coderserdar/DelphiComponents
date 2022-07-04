unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, sScrollBar, ExtDlgs, sEdit, Menus,
  sButton, StdCtrls, sSkinProvider, sSkinManager, sCheckBox, Buttons, sBitBtn, sComboBox, sLabel, ImgList, sGauge, sPanel,
  ComCtrls, CheckLst, Mask, Grids, sCheckListBox, acAlphaImageList, XPMan, sListBox;

type
  TMainForm = class(TForm)
    sSkinManager1: TsSkinManager;
    sPanel4: TsPanel;
    ComboBox1: TsComboBox;
    MainMenu1: TMainMenu;
    MenuItem11: TMenuItem;
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
    sSkinProvider1: TsSkinProvider;
    sCheckBox1: TsCheckBox;
    About1: TMenuItem;
    Gotoonlinehome1: TMenuItem;
    Writetosupport1: TMenuItem;
    sButton4: TsButton;
    sButton5: TsButton;
    sButton6: TsButton;
    sButton7: TsButton;
    sBitBtn5: TsBitBtn;
    sAlphaImageList1: TsAlphaImageList;
    sPanel18: TsPanel;
    AnimPanel: TsPanel;
    sPanel15: TsPanel;
    sLabel7: TsLabel;
    sBitBtn4: TsBitBtn;
    sComboBox2: TsComboBox;
    sComboBox3: TsComboBox;
    sPanel16: TsPanel;
    sPanel17: TsPanel;
    Memo1: TMemo;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    ListBox1: TListBox;
    MaskEdit1: TMaskEdit;
    RichEdit1: TRichEdit;
    TreeView1: TTreeView;
    CheckListBox1: TCheckListBox;
    ListView1: TListView;
    sPanel1: TsPanel;
    sGauge1: TsGauge;
    sEdit1: TsEdit;
    sComboBox1: TsComboBox;
    sButton1: TsButton;
    sButton2: TsButton;
    sScrollBar3: TsScrollBar;
    sPanel2: TsPanel;
    sLabelFX1: TsLabelFX;
    sCheckBox3: TsCheckBox;
    sCheckBox2: TsCheckBox;
    sListBox1: TsListBox;
    sBitBtn1: TsBitBtn;
    sBitBtn3: TsBitBtn;
    sCheckListBox1: TsCheckListBox;
    sCheckBox10: TsCheckBox;
    sPanel3: TsPanel;
    sPanel5: TsPanel;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    sLabel4: TsLabel;
    sScrollBar1: TsScrollBar;
    sScrollBar2: TsScrollBar;
    sPanel6: TsPanel;
    sPanel7: TsPanel;
    sPanel9: TsPanel;
    sCheckBox4: TsCheckBox;
    sCheckBox5: TsCheckBox;
    sCheckBox6: TsCheckBox;
    sCheckBox7: TsCheckBox;
    sPanel10: TsPanel;
    sPanel11: TsPanel;
    sCheckBox8: TsCheckBox;
    sEdit2: TsEdit;
    sPanel12: TsPanel;
    sPanel14: TsPanel;
    sCheckBox9: TsCheckBox;
    sEdit3: TsEdit;
    sPanel13: TsPanel;
    sPanel8: TsPanel;
    sButton3: TsButton;
    sButton8: TsButton;
    ImageList32: TsAlphaImageList;
    sGauge2: TsGauge;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure sSkinManager1BeforeChange(Sender: TObject);
    procedure sScrollBar3Change(Sender: TObject);
    procedure sButton5Click(Sender: TObject);
    procedure sCheckBox4Click(Sender: TObject);
    procedure sCheckBox5Click(Sender: TObject);
    procedure sCheckBox6Click(Sender: TObject);
    procedure sCheckBox7Click(Sender: TObject);
    procedure sCheckBox8Click(Sender: TObject);
    procedure sEdit2Change(Sender: TObject);
    procedure sComboBox2Change(Sender: TObject);
    procedure sComboBox3Change(Sender: TObject);
    procedure SetActivePage(PageIndex : integer);
    procedure sEdit3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sBitBtn5Click(Sender: TObject);
    procedure sScrollBar1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sScrollBar2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sCheckBox10Click(Sender: TObject);
    procedure sButton3Click(Sender: TObject);
    procedure sButton8Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;
  Loading : boolean;
  NewBGName : string;

implementation

uses
  sSkinProps, FileCtrl, sStyleSimply, sConst, sMaskData, sVclUtils;

var
  CurPanel : TsPanel = nil;

{$R *.DFM}

procedure TMainForm.ComboBox1Change(Sender: TObject);
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

procedure TMainForm.FormShow(Sender: TObject);
var
  sl : TStringList;
  i : integer;
begin
  sl := TStringList.Create;
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
end;

procedure TMainForm.sCheckBox1Click(Sender: TObject);
begin
  sSkinManager1.Active := sCheckBox1.Checked;
  ComboBox1.Enabled := sSkinManager1.Active;

  sComboBox2.Enabled := sSkinManager1.Active;
  sComboBox3.Enabled := sSkinManager1.Active;
end;

procedure TMainForm.sSkinManager1BeforeChange(Sender: TObject);
begin
  sSkinManager1.FHueOffset := 0;
  sSkinManager1.FSaturation := 0;
  sScrollBar1.Position := 0;
  sScrollBar2.Position := 0;
end;

procedure TMainForm.sScrollBar3Change(Sender: TObject);
begin
  sGauge1.Progress := sScrollBar3.Position;
  sGauge2.Progress := sScrollBar3.Position
end;

procedure TMainForm.sButton5Click(Sender: TObject);
begin
  SetActivePage(TControl(Sender).Tag)
end;

procedure TMainForm.sCheckBox4Click(Sender: TObject);
begin
  if sCheckBox4.Checked
    then sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events + [beMouseEnter]
    else sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events - [beMouseEnter]
end;

procedure TMainForm.sCheckBox5Click(Sender: TObject);
begin
  if sCheckBox5.Checked
    then sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events + [beMouseLeave]
    else sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events - [beMouseLeave]
end;

procedure TMainForm.sCheckBox6Click(Sender: TObject);
begin
  if sCheckBox6.Checked
    then sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events + [beMouseDown]
    else sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events - [beMouseDown]
end;

procedure TMainForm.sCheckBox7Click(Sender: TObject);
begin
  if sCheckBox4.Checked
    then sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events + [beMouseUp]
    else sSkinManager1.AnimEffects.Buttons.Events := sSkinManager1.AnimEffects.Buttons.Events - [beMouseUp]
end;

procedure TMainForm.sCheckBox8Click(Sender: TObject);
begin
  sSKinManager1.AnimEffects.SkinChanging.Active := sCheckBox8.Checked
end;

procedure TMainForm.sEdit2Change(Sender: TObject);
begin
  sSKinManager1.AnimEffects.SkinChanging.Time := StrtoInt(sEdit2.Text)
end;

procedure TMainForm.sComboBox2Change(Sender: TObject);
begin
  sBitBtn4.SkinData.SkinSection := sComboBox2.Text
end;

procedure TMainForm.sComboBox3Change(Sender: TObject);
begin
  sPanel16.SkinData.SkinSection := sComboBox3.Text
end;

procedure TMainForm.SetActivePage(PageIndex: integer);
var
  OldPanel : TsPanel;
  procedure ChangeBtn(Btn : TsButton);
  begin
    Btn.Down := PageIndex = Btn.Tag;
  end;
begin
  ChangeBtn(sButton4);
  ChangeBtn(sButton5);
  ChangeBtn(sButton6);
  ChangeBtn(sButton7);
  OldPanel := CurPanel;

  case PageIndex of
    0 : CurPanel := sPanel3;
    1 : CurPanel := sPanel1;
    2 : CurPanel := sPanel15;
    3 : CurPanel := sPanel17;
  end;

  if sCheckBox9.Checked and sSkinManager1.Active then PrepareForAnimation(AnimPanel);

  if sCheckBox9.Checked and sSkinManager1.Active then begin
    CurPanel.Perform(WM_SETREDRAW, 0, 0);
    CurPanel.Visible := True;
    CurPanel.BringToFront;
    CurPanel.Perform(WM_SETREDRAW, 1, 0);
    AnimShowControl(AnimPanel, StrToInt(sEdit3.Text));
    RedrawWindow(AnimPanel.Handle, nil, 0, RDW_ERASE or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
  end
  else CurPanel.Visible := True;
  if OldPanel <> nil then OldPanel.Visible := False;
end;

procedure TMainForm.sEdit3Change(Sender: TObject);
begin
  try
    StrtoInt(sEdit3.Text)
  except
    sEdit3.Text := '200'
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  sSkinProvider1.AddedTitle.Text := '( Version ' + sSkinManager1.Version + ' )';
  CurPanel := sPanel1;
end;

procedure TMainForm.sBitBtn5Click(Sender: TObject);
begin
  Close
end;

procedure TMainForm.sScrollBar1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  sLabel3.Caption := IntToStr(sScrollBar1.Position);
  sSkinManager1.Saturation := sScrollBar1.Position;
end;

procedure TMainForm.sScrollBar2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  sLabel4.Caption := IntToStr(sScrollBar2.Position);
  sSkinManager1.HueOffset := sScrollBar2.Position;
end;

procedure TMainForm.sCheckBox10Click(Sender: TObject);
begin
  sBitBtn1.Reflected := sCheckBox10.Checked;
  sBitBtn3.Reflected := sCheckBox10.Checked;
end;

procedure TMainForm.sButton3Click(Sender: TObject);
begin
  sButton3.Down := not sButton3.Down;
  sSkinManager1.ExtendedBorders := sButton3.Down
end;

procedure TMainForm.sButton8Click(Sender: TObject);
begin
  sButton8.Down := not sButton8.Down;
  sSkinProvider1.DrawNonClientArea := sButton8.Down
end;

end.
