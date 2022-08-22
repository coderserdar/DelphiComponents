unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, sSkinManager, sSkinProvider,
  StdCtrls, sButton, sComboBoxes, acShellCtrls, ExtCtrls, sPanel, Buttons,
  sSpeedButton, sColorSelect, Mask, sMaskEdit, sCustomComboEdit, sComboEdit,
  sListView, ImgList, sGauge, acHeaderControl, ActnMan,
  ActnColorMaps, XPMan, DBCtrls, Grids, ValEdit, sComboBox, acAlphaImageList,
  sCheckBox, sEdit, sSpinEdit, acntUtils, Menus, sTooledit, sHintManager,
  aceListView, sBitBtn;

type
  TForm1 = class(TForm)
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    listViewImages: TImageList;
    ListViewSmallImages: TsAlphaImageList;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    Properties1: TMenuItem;
    UseSkinsCheck: TsCheckBox;
    KssListView1: TacListView;
    sPanel1: TsPanel;
    RegularItemColorSelect: TsColorSelect;
    HotItemColorSelect: TsColorSelect;
    SelectItemColorSelect: TsColorSelect;
    ProgressItemColorSelect: TsColorSelect;
    GridColorSelect: TsColorSelect;
    GridLinesCombo: TsComboBox;
    sButton1: TsButton;
    RegularBackCombo: TsComboBox;
    ItemWidthCombo: TsComboBox;
    ShowColumnHeadersCheck: TsCheckBox;
    ShowGroupsCheck: TsCheckBox;
    ColumnHeightEdit: TsSpinEdit;
    GroupHeightEdit: TsSpinEdit;
    ItemHeightEdit: TsSpinEdit;
    ItemSkinCombo: TsComboBox;
    GroupSkinCombo: TsComboBox;
    ColumnSkinCombo: TsComboBox;
    ListViewCombo: TsComboBox;
    MultiselectCheck: TsCheckBox;
    CheckBoxesCombo: TsComboBox;
    CheckOnClickCheck: TsCheckBox;
    SelectAllButton: TsButton;
    UnselectAllButton: TsButton;
    SkinsDirEdit: TsDirectoryEdit;
    SelectionFrameColorSelect: TsColorSelect;
    AllowSelectionCheck: TsCheckBox;
    ClearButton: TsButton;
    HideUnhideColButton: TsButton;
    ShowGroupButtons: TsCheckBox;
    ShowGroupItemsCount: TsCheckBox;
    Add2000items: TsButton;
    sHintManager1: TsHintManager;
    ListViewGroupImages: TsAlphaImageList;
    procedure sButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure KssListView1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GridLinesComboChange(Sender: TObject);
    procedure RegularBackComboChange(Sender: TObject);
    procedure ItemWidthComboChange(Sender: TObject);
    procedure ShowColumnHeadersCheckClick(Sender: TObject);
    procedure ShowGroupsCheckClick(Sender: TObject);
    procedure RegularItemColorSelectChange(Sender: TObject);
    procedure HotItemColorSelectChange(Sender: TObject);
    procedure SelectItemColorSelectChange(Sender: TObject);
    procedure ProgressItemColorSelectChange(Sender: TObject);
    procedure GridColorSelectChange(Sender: TObject);
    procedure ItemHeightEditChange(Sender: TObject);
    procedure GroupHeightEditChange(Sender: TObject);
    procedure ColumnHeightEditChange(Sender: TObject);
    procedure UseSkinsCheckClick(Sender: TObject);
    procedure ColumnSkinComboChange(Sender: TObject);
    procedure ItemSkinComboChange(Sender: TObject);
    procedure GroupSkinComboChange(Sender: TObject);
    procedure KssListView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Properties1Click(Sender: TObject);
    procedure ListViewComboChange(Sender: TObject);
    procedure MultiselectCheckClick(Sender: TObject);
    procedure CheckBoxesComboChange(Sender: TObject);
    procedure CheckOnClickCheckClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure UnselectAllButtonClick(Sender: TObject);
    procedure SkinsDirEditChange(Sender: TObject);
    procedure SelectionFrameColorSelectChange(Sender: TObject);
    procedure AllowSelectionCheckClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure HideUnhideColButtonClick(Sender: TObject);
    procedure ShowGroupButtonsClick(Sender: TObject);
    procedure ShowGroupItemsCountClick(Sender: TObject);
    procedure Add2000itemsClick(Sender: TObject);
    procedure KssListView1GetGroupCount(Sender: TacListGroup;
      var CountString: string);
  private
    { Private declarations }
  public
    ProgressItem: TacListItem;
  end;

var
  Form1: TForm1;

implementation

uses sSkinProps;

{$R *.dfm}

procedure TForm1.Add2000itemsClick(Sender: TObject);
var
  i: Integer;
begin
  KssListView1.BeginUpdate;

  if KssListView1.Groups.Count = 0 then
    KssListView1.Groups.Add('New group');

  for i := 1 to 2000 do
    KssListView1.Groups[0].Add('New item ' + IntToStr(i));

  KssListView1.EndUpdate;
end;

procedure TForm1.AllowSelectionCheckClick(Sender: TObject);
begin
  KssListView1.AllowSelection := AllowSelectionCheck.Checked;
end;

procedure TForm1.CheckBoxesComboChange(Sender: TObject);
begin
  KssListView1.CheckBoxes := TListCheckBoxes(CheckBoxesCombo.ItemIndex);
end;

procedure TForm1.CheckOnClickCheckClick(Sender: TObject);
begin
  KssListView1.CheckOnClik := CheckOnClickCheck.Checked;
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  KssListView1.Clear;
end;

procedure TForm1.ColumnHeightEditChange(Sender: TObject);
begin
  KssListView1.ColumnHeight := ColumnHeightEdit.Value;
end;

procedure TForm1.ColumnSkinComboChange(Sender: TObject);
begin
  KssListView1.ColumnSkin := ColumnSkinCombo.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ListGroup: TacListGroup;
  ListItem: TacListItem;
  i: integer;
begin
 KssListView1.SkinData.SkinSection := s_BAR;
 KssListView1.ColumnSkin := s_BUTTON;

 KssListView1.BeginUpdate;

 ListGroup := KssListView1.Groups.Add('History', 0);
 ListGroup.Description := 'Several control tests to check knowledge';
 ListGroup.Footer := 'It`s history tests group footer';
 for i := 0 to 4 do begin
   Randomize;
   ListItem := ListGroup.Add('Test_h No' + IntToStr(Random(30)+1) + '');
   ListItem.SubItems.Add('Control-check');
   ListItem.SubItems.Add('00:' + IntToStr(Random(50)+10) + ':00');
   ListItem.SubItems.Add(IntToStr(Random(3)) + '/3');
   ListItem.SubItems.Add('+');
   ListItem.ImageIndex := 2;
 end;

 ListGroup := KssListView1.Groups.Add('Mathematics', 1);
 for i := 0 to 15 do begin
   Randomize;
   ListItem := ListGroup.Add('Test_m No' + IntToStr(Random(30)+1));
   ListItem.SubItems.Add('Control-check');
   ListItem.SubItems.Add('00:' + IntToStr(Random(50)+10) + ':00');
   ListItem.SubItems.Add(IntToStr(Random(3)) + '/3');
   ListItem.SubItems.Add('+');
   ListItem.ImageIndex := 2;
 end;

 ListGroup := KssListView1.Groups.ItemByCaption('History');
 if Assigned(ListGroup) then begin
   ListItem := ListGroup.Add('Test #50');
   ListItem.SubItems.Add('Control-check');
   ListItem.SubItems.Add('00:' + IntToStr(Random(50)+10) + ':00');
   ListItem.SubItems.Add(IntToStr(Random(3)) + '/3');
   ListItem.SubItems.Add('+');
   ListItem.ImageIndex := 2;
 end;

 KssListView1.EndUpdate(true);

 GridLinesCombo.ItemIndex := integer(KssListView1.GridLines);
 RegularBackCombo.ItemIndex := integer(KssListView1.RegularBack);
 ItemWidthCombo.ItemIndex := integer(KssListView1.ItemWidth);
 CheckBoxesCombo.ItemIndex := integer(KssListView1.CheckBoxes);

 CheckOnClickCheck.Checked := KssListView1.CheckOnClik;

 RegularItemColorSelect.ColorValue := KssListView1.RegularItemColor;
 HotItemColorSelect.ColorValue := KssListView1.HotItemColor;
 SelectItemColorSelect.ColorValue := KssListView1.SelectItemColor;
 ProgressItemColorSelect.ColorValue := KssListView1.ProgressItemColor;
 GridColorSelect.ColorValue := KssListView1.GridColor;
 SelectionFrameColorSelect.ColorValue :=  KssListView1.SelectionFrameColor;

// sSkinManager1.SkinDirectory := GetAppPath + '\Skins\';

 sSkinManager1.GetSkinSections(ItemSkinCombo.Items);
 ItemSkinCombo.Text := KssListView1.ItemSkin;

 sSkinManager1.GetSkinSections(GroupSkinCombo.Items);
 GroupSkinCombo.Text := KssListView1.GroupSkin;

 sSkinManager1.GetSkinSections(ColumnSkinCombo.Items);
 ColumnSkinCombo.Text := KssListView1.ColumnSkin;

 sSkinManager1.GetSkinSections(ListViewCombo.Items);
 ListViewCombo.Text := KssListView1.SkinData.SkinSection;
end;

procedure TForm1.KssListView1Click(Sender: TObject);
begin
  Caption := Format('Checked: %d; Selected: %d;',
    [KssListView1.Items.Count([iaCheck]),
     KssListView1.Items.Count([iaSelect])]);
end;

procedure TForm1.KssListView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  ElementAtPos: TacListElement;
begin
  ElementAtPos := KssListView1.GetElementAt(MousePos);
  if Assigned(ElementAtPos) and (ElementAtPos is TacListItem) then begin
    KssListView1.SelectedItem := TacListItem(ElementAtPos);
    KssListView1.PopupMenu := PopupMenu1
  end
  else
      KssListView1.PopupMenu := nil;
end;

procedure TForm1.KssListView1GetGroupCount(Sender: TacListGroup;
  var CountString: string);
begin
  CountString := Format('%d/%d', [Sender.Count([iaCheck, iaVisible]),
    Sender.Count([iaVisible])]);
end;

procedure TForm1.ListViewComboChange(Sender: TObject);
begin
  KssListView1.SkinData.SkinSection := ListViewCombo.Text;
end;

procedure TForm1.MultiselectCheckClick(Sender: TObject);
begin
  KssListView1.MultiSelect := MultiselectCheck.Checked;
end;

procedure TForm1.ProgressItemColorSelectChange(Sender: TObject);
begin
  KssListView1.ProgressItemColor := ProgressItemColorSelect.ColorValue;
end;

procedure TForm1.Properties1Click(Sender: TObject);
begin
  MessageBox(Handle, PChar(KssListView1.SelectedItem.Caption), 'Properties', MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.RegularBackComboChange(Sender: TObject);
begin
  KssListView1.RegularBack := TRegularBack(RegularBackCombo.ItemIndex);
end;

procedure TForm1.RegularItemColorSelectChange(Sender: TObject);
begin
  KssListView1.RegularItemColor := RegularItemColorSelect.ColorValue;
end;

procedure TForm1.sButton1Click(Sender: TObject);
begin
  if KssListView1.Items.Count > 0 then begin
    ProgressItem := KssListView1.Groups[0].GetItem(0);
    ProgressItem.ShowProgress := true;
    Timer1.Enabled := true;
  end;
end;

procedure TForm1.SelectAllButtonClick(Sender: TObject);
begin
  KssListView1.SelectAll;
end;

procedure TForm1.SelectionFrameColorSelectChange(Sender: TObject);
begin
  KssListView1.SelectionFrameColor := SelectionFrameColorSelect.ColorValue;
end;

procedure TForm1.SelectItemColorSelectChange(Sender: TObject);
begin
  KssListView1.SelectItemColor := SelectItemColorSelect.ColorValue;
end;

procedure TForm1.ShowColumnHeadersCheckClick(Sender: TObject);
begin
  KssListView1.ShowColumnHeaders := ShowColumnHeadersCheck.Checked;
end;

procedure TForm1.ShowGroupButtonsClick(Sender: TObject);
begin
  KssListView1.ShowGroupButtons := ShowGroupButtons.Checked;
end;

procedure TForm1.ShowGroupItemsCountClick(Sender: TObject);
begin
  KssListView1.ShowGroupItemsCount := ShowGroupItemsCount.Checked;
end;

procedure TForm1.ShowGroupsCheckClick(Sender: TObject);
begin
  KssListView1.ShowGroups := ShowGroupsCheck.Checked;
end;

procedure TForm1.SkinsDirEditChange(Sender: TObject);
begin
  sSkinManager1.SkinDirectory := SkinsDirEdit.Text;
end;

procedure TForm1.ItemHeightEditChange(Sender: TObject);
begin
  KssListView1.ItemHeight := ItemHeightEdit.Value;
end;

procedure TForm1.ItemSkinComboChange(Sender: TObject);
begin
  KssListView1.ItemSkin := ItemSkinCombo.Text;
end;

procedure TForm1.GridColorSelectChange(Sender: TObject);
begin
  KssListView1.GridColor := GridColorSelect.ColorValue;
end;

procedure TForm1.GridLinesComboChange(Sender: TObject);
begin
  KssListView1.GridLines := TGridLines(GridLinesCombo.ItemIndex);
end;

procedure TForm1.GroupHeightEditChange(Sender: TObject);
begin
  KssListView1.GroupHeight := GroupHeightEdit.Value;
end;

procedure TForm1.GroupSkinComboChange(Sender: TObject);
begin
  KssListView1.GroupSkin := GroupSkinCombo.Text;
end;

procedure TForm1.HideUnhideColButtonClick(Sender: TObject);
begin
  KssListView1.Columns[1].Visible := not KssListView1.Columns[1].Visible;
end;

procedure TForm1.HotItemColorSelectChange(Sender: TObject);
begin
  KssListView1.HotItemColor := HotItemColorSelect.ColorValue;
end;

procedure TForm1.ItemWidthComboChange(Sender: TObject);
begin
  KssListView1.ItemWidth := TItemWidth(ItemWidthCombo.ItemIndex);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with ProgressItem do begin
    ProgressPosition := ProgressPosition + 1;
    if ProgressPosition > 100 then begin
      ProgressPosition := 0;
      ShowProgress := false;
      Timer1.Enabled := false;
    end;
  end;
end;

procedure TForm1.UnselectAllButtonClick(Sender: TObject);
begin
  KssListView1.UnSelectAll;
end;

procedure TForm1.UseSkinsCheckClick(Sender: TObject);
begin
  sSkinManager1.Active := UseSkinsCheck.Checked;
end;

end.
