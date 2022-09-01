unit gridcolors_frm_setup;

interface
{$I psc_defines.inc}

uses
  Windows,
  Messages,
  SysUtils,
{$IFDEF D6}
  Variants,
{$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  DBGrids,
  DB,


  myla_interfaces,
  myla_system,

  psc_procs,
  psc_listbox,
  psc_button_color,
  psc_expreval,
  gridcolors_frm_setup_fields,
  psc_fltbox,
  psc_edit,
  psc_edit_color;

type
  Tpsc_frm_setup = class(TForm)
    Panel_Buttons: TPanel;
    Button_Ok: TButton;
    Button_Cancel: TButton;
    Panel_Setup: TPanel;
    FontDialog1: TFontDialog;
    Panel_Main: TPanel;
    PSCListBox_RecordsInfo: TPSCListBox;
    Panel2: TPanel;
    Button_Add: TButton;
    Button_Delete: TButton;
    PSCFltDlg1: TPSCFltDlg;
    Button_MoveUp: TButton;
    Button_MoveDown: TButton;
    GroupBox_Properties: TGroupBox;
    Label_Name: TLabel;
    RadioGroup_Format: TRadioGroup;
    PSCColorEdit1: TPSCColorEdit;
    BitBtn_Condition: TBitBtn;
    BitBtn_SelectFields: TBitBtn;
    PSCEdit_Name: TPSCEdit;
    PSCColorEdit2: TPSCColorEdit;
    CheckBox_Bold: TCheckBox;
    CheckBox_Italic: TCheckBox;
    CheckBox_Underline: TCheckBox;
    Button_Apply: TButton;
    CheckBox_StrikeOut: TCheckBox;
    CheckBox_UseColor: TCheckBox;
    CheckBox_UseTextColor: TCheckBox;
    procedure BitBtn_ConditionClick(Sender: TObject);
    procedure BitBtn_SelectFieldsClick(Sender: TObject);
    procedure RadioGroup_FormatClick(Sender: TObject);
    procedure PSCColorEdit1Change(Sender: TObject);
    procedure Button_DeleteClick(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure PSCListBox_RecordsInfoClick(Sender: TObject);
    procedure Button_MoveUpClick(Sender: TObject);
    procedure Button_MoveDownClick(Sender: TObject);
    procedure PSCEdit_NameChange(Sender: TObject);
    procedure PSCColorEdit2Change(Sender: TObject);
    procedure CheckBox_BoldClick(Sender: TObject);
    procedure CheckBox_ItalicClick(Sender: TObject);
    procedure CheckBox_UnderlineClick(Sender: TObject);
    procedure Button_ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox_StrikeOutClick(Sender: TObject);
    procedure CheckBox_UseColorClick(Sender: TObject);
    procedure CheckBox_UseTextColorClick(Sender: TObject);
  private
    FMyGridColorsUpdate : TNotifyEvent;
    FMyDataSet: TDataSet;
    function FillOptions(AListBoxItem : TPSCListBoxItem) : TPSCRecordSetStyleOptions;
    procedure InitControls;
    procedure SetControlsEnabled(AEnabled : boolean);
    procedure FillListBox(AGridColors: TPSCGridColors);
    procedure SetFltDlgReadOnly(Sender : TObject);
    { Private declarations }
  public
    procedure LoadGridProperties(AGridColors : TPSCGridColors;
     ADataSet :  TDataset);
    procedure SaveGridProperties(AGridColors : TPSCGridColors);
    property MyGridColorsUpdate : TNotifyEvent read FMyGridColorsUpdate write FMyGridColorsUpdate;
    { Public declarations }
  end;

var
  psc_frm_setup: Tpsc_frm_setup;


implementation

{$R *.dfm}

{ Tpsc_frm_setup }

const
  CPSCFieldColor = 0;
  CPSCFieldFontColor = 1;
  CPSCFieldBold_InFont = 2;
  CPSCFieldItalic_InFont = 3;
  CPSCFieldUnderline_InFont = 4;
  CPSCFieldStrikeOut_InFont = 5;
  CPSCFieldDataField = 6;
  CPSCFieldReadOnly = 7;
  CPSCFieldUseColor = 8;
  CPSCFieldUseFontColor = 9;
  CPSCFieldUseBold = 10;
  CPSCFieldUseItalic = 11;
  CPSCFieldUseUnderline = 12;
  CPSCFieldUseStrikeOut = 13;
  CPSCFieldFilterSource = 14;

procedure Tpsc_frm_setup.LoadGridProperties(AGridColors: TPSCGridColors;
 ADataSet :  TDataset);
begin
  FMyDataSet := ADataSet;
  FillListBox(AGridColors);
  PSCListBox_RecordsInfo.ItemIndex := 0;
end;

procedure Tpsc_frm_setup.SaveGridProperties(AGridColors: TPSCGridColors);
var
  i : integer;
begin
  AGridColors.RecordsInfo.Clear;
  for i := 0 to PSCListBox_RecordsInfo.Items.Count - 1 do
    with TPSCRecordSetStyle(AGridColors.RecordsInfo.Add) do
    begin
      FilterSource := TPSCCustomFltBld(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldFilterSource].AsObject);    
      Name := PSCListBox_RecordsInfo.Items[i].Caption;
      Active := PSCListBox_RecordsInfo.Items[i].Checked;
      ReadOnly := PSCListBox_RecordsInfo.Items[i].ReadOnly;
      Color :=
       TColor(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldColor].AsInteger);
      FontColor :=
       TColor(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldFontColor].AsInteger);
      if (TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldBold_InFont].AsBoolean) then
        FontStyle := FontStyle + [fs_Bold];
      if (TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldItalic_InFont].AsBoolean) then
        FontStyle := FontStyle + [fs_Italic];
      if (TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldUnderline_InFont].AsBoolean) then
        FontStyle := FontStyle + [fs_Underline];
      if (TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldStrikeOut_InFont].AsBoolean) then
        FontStyle := FontStyle + [fs_StrikeOut];
      DataField :=
       TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldDataField].AsString;
      ReadOnly :=
       TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]).UserFields[CPSCFieldReadOnly].AsBoolean;
      Options := FillOptions(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[i]));
    end;
end;

procedure Tpsc_frm_setup.BitBtn_ConditionClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  PSCFltDlg1.FilterSource :=
   TPSCCustomFltBld(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldFilterSource].AsObject);
  PSCFltDlg1.Execute;
end;

procedure Tpsc_frm_setup.BitBtn_SelectFieldsClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  psc_frm_setup_fields := Tpsc_frm_setup_fields.Create(nil);
  try
    psc_frm_setup_fields.LoadFields(TPSCCustomFltBld(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldFilterSource].AsObject).DataSet.Fields,
     TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldDataField].AsString);
    psc_frm_setup_fields.ShowModal;
    if psc_frm_setup_fields.ModalResult = mrOk then
      TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldDataField].AsString :=
       psc_frm_setup_fields.SelectFields;
    if TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldDataField].AsString = '' then
      RadioGroup_Format.ItemIndex := 0;
    if RadioGroup_Format.CanFocus then
      RadioGroup_Format.SetFocus;
  finally
    psc_frm_setup_fields.Free;
  end;
end;

procedure Tpsc_frm_setup.RadioGroup_FormatClick(Sender: TObject);
var
  MyIndex : integer;
begin
  case RadioGroup_Format.ItemIndex of
    0:
      begin
        BitBtn_SelectFields.Enabled := false;
        MyIndex := PSCListBox_RecordsInfo.ItemIndex;
        if MyIndex < 0 then
          exit;
        TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldDataField].AsString := '';
      end;
    1:
      BitBtn_SelectFields.Enabled := true;
  end;
end;

procedure Tpsc_frm_setup.InitControls;
var
  MyIndex : integer;
  MyOptions : TPSCRecordSetStyleOptions;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  MyOptions := FillOptions(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]));
  PSCEdit_Name.Text := PSCListBox_RecordsInfo.Items[MyIndex].Caption;
  PSCColorEdit1.SelectedColor :=
   TColor(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldColor].AsInteger);
  PSCColorEdit2.SelectedColor :=
   TColor(TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldFontColor].AsInteger);
  CheckBox_Bold.Checked := TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldBold_InFont].AsBoolean;
  CheckBox_Italic.Checked := TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldItalic_InFont].AsBoolean;
  CheckBox_Underline.Checked := TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUnderline_InFont].AsBoolean;
  CheckBox_StrikeOut.Checked := TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldStrikeOut_InFont].AsBoolean;
  CheckBox_UseColor.Checked := rssoSetColor in MyOptions;
  CheckBox_UseTextColor.Checked := rssoSetFontColor in MyOptions;
  if not (rssoSetFontBold in MyOptions) then
    CheckBox_Bold.State := cbGrayed;
  if not (rssoSetFontItalic in MyOptions) then
    CheckBox_Italic.State := cbGrayed;
  if not (rssoSetFontUnderline in MyOptions) then
    CheckBox_Underline.State := cbGrayed;
  if not (rssoSetFontStrikeOut in MyOptions) then
    CheckBox_StrikeOut.State := cbGrayed;
  if (TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldDataField].AsString = '') then
    RadioGroup_Format.ItemIndex := 0
  else
    RadioGroup_Format.ItemIndex := 1;
  SetControlsEnabled(not TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldReadOnly].AsBoolean);
end;

procedure Tpsc_frm_setup.PSCColorEdit1Change(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldColor].AsInteger := PSCColorEdit1.SelectedColor;
end;

procedure Tpsc_frm_setup.Button_DeleteClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  PSCListBox_RecordsInfo.Items[MyIndex].Free;
  if MyIndex < PSCListBox_RecordsInfo.Items.Count then
    PSCListBox_RecordsInfo.ItemIndex := MyIndex
  else
    PSCListBox_RecordsInfo.ItemIndex := PSCListBox_RecordsInfo.Items.Count - 1;
end;

procedure Tpsc_frm_setup.FillListBox(AGridColors: TPSCGridColors);
var
  i : integer;
begin
  PSCListBox_RecordsInfo.ItemIndex := -1;
  PSCListBox_RecordsInfo.Items.Clear;
  for i := 0 to AGridColors.RecordsInfo.Count - 1 do
    with TPSCListBoxItem(PSCListBox_RecordsInfo.Items.Add) do
    begin
      with TPSCField(UserFields.Add) do
      begin
        Name := 'Color';
        AsInteger := AGridColors.RecordsInfo[i].Color;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'FontColor';
        AsInteger := AGridColors.RecordsInfo[i].FontColor;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'Bold_InFont';
        AsBoolean := fs_Bold in AGridColors.RecordsInfo[i].FontStyle;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'Italic_InFont';
        AsBoolean := fs_Italic in AGridColors.RecordsInfo[i].FontStyle;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'Underline_InFont';
        AsBoolean := fs_Underline in AGridColors.RecordsInfo[i].FontStyle;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'StrikeOut_InFont';
        AsBoolean := fs_StrikeOut in AGridColors.RecordsInfo[i].FontStyle;
      end;
{      with TPSCField(UserFields.Add) do
      begin
        Name := 'Filter';
        AsString := AGridColors.RecordsInfo[i].Filter;
      end;}
      with TPSCField(UserFields.Add) do
      begin
        Name := 'DataField';
        AsString := AGridColors.RecordsInfo[i].DataField;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'ReadOnly';
        AsBoolean := AGridColors.RecordsInfo[i].ReadOnly;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseColor';
        AsBoolean := rssoSetColor in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseFontColor';
        AsBoolean := rssoSetFontColor in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseBoldInFont';
        AsBoolean := rssoSetFontBold in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseItalicInFont';
        AsBoolean := rssoSetFontItalic in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseUnderlineInFont';
        AsBoolean := rssoSetFontUnderline in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseStrikeOutInFont';
        AsBoolean := rssoSetFontStrikeOut in AGridColors.RecordsInfo[i].Options;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'FilterSource';
        AsObject := AGridColors.RecordsInfo[i].FilterSource;
      end;

      Caption := AGridColors.RecordsInfo[i].Name;
      Checked := AGridColors.RecordsInfo[i].Active;
    end;
end;

procedure Tpsc_frm_setup.Button_AddClick(Sender: TObject);
var
  MyFilterSource : TPSCCustomFltBld;
begin
  with TPSCListBoxItem(PSCListBox_RecordsInfo.Items.Add) do
  begin
    with TPSCField(UserFields.Add) do
    begin
      Name := 'Color';
      AsInteger := clWindow;
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'FontColor';
      AsInteger := clWindowText;
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'Bold_InFont';
      AsBoolean := false;
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'Italic_InFont';
      AsBoolean := false;
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'Underline_InFont';
      AsBoolean := false;
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'StrikeOut_InFont';
      AsBoolean := false;
    end;
{    with TPSCField(UserFields.Add) do
    begin
      Name := 'Filter';
      AsString := '';
    end;}
    with TPSCField(UserFields.Add) do
    begin
      Name := 'DataField';
      AsString := '';
    end;
    with TPSCField(UserFields.Add) do
    begin
      Name := 'ReadOnly';
      AsBoolean := false;
    end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseColor';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseFontColor';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseBoldInFont';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseItalicInFont';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseUnderlineInFont';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'UseStrikeOutInFont';
        AsBoolean := false;
      end;
      with TPSCField(UserFields.Add) do
      begin
        Name := 'FilterSource';
        MyFilterSource := TPSCCustomFltBld.Create(nil);
        MyFilterSource.DataSet := FMyDataSet;
        AsObject := MyFilterSource;
      end;

    Caption := 'Untitled' + IntToStr(PSCListBox_RecordsInfo.Items.Count);
    Checked := true;
  end;
  PSCListBox_RecordsInfo.ItemIndex := PSCListBox_RecordsInfo.Items.Count - 1;
  if PSCListBox_RecordsInfo.CanFocus then
    PSCListBox_RecordsInfo.SetFocus;
end;

procedure Tpsc_frm_setup.PSCListBox_RecordsInfoClick(Sender: TObject);
begin
  InitControls;
end;

procedure Tpsc_frm_setup.Button_MoveUpClick(Sender: TObject);
begin
  PSCListBox_RecordsInfo.MoveItemUp;
end;

procedure Tpsc_frm_setup.Button_MoveDownClick(Sender: TObject);
begin
  PSCListBox_RecordsInfo.MoveItemDown;
end;

procedure Tpsc_frm_setup.SetControlsEnabled(AEnabled : boolean);
begin
  PSCEdit_Name.Enabled := AEnabled;
  PSCColorEdit1.Enabled := AEnabled;
  PSCColorEdit2.Enabled := AEnabled;
  Button_Delete.Enabled := AEnabled;
  RadioGroup_Format.Enabled := AEnabled;
  Label_Name.Enabled := AEnabled;
  CheckBox_Bold.Enabled := AEnabled;
  CheckBox_Italic.Enabled := AEnabled;
  CheckBox_Underline.Enabled := AEnabled;
  CheckBox_StrikeOut.Enabled := AEnabled;
  CheckBox_UseColor.Enabled := AEnabled;
  CheckBox_UseTextColor.Enabled := AEnabled;
  BitBtn_SelectFields.Enabled := AEnabled and (RadioGroup_Format.ItemIndex = 1);
  Button_MoveUp.Enabled := PSCListBox_RecordsInfo.CanMoveItemUp;
  Button_MoveDown.Enabled := PSCListBox_RecordsInfo.CanMoveItemDown;
  PSCColorEdit1.Enabled := AEnabled and CheckBox_UseColor.Checked;
  PSCColorEdit2.Enabled := AEnabled and CheckBox_UseTextColor.Checked;
end;

procedure Tpsc_frm_setup.PSCEdit_NameChange(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).Caption := PSCEdit_Name.Text;
end;

procedure Tpsc_frm_setup.PSCColorEdit2Change(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[1].AsInteger := PSCColorEdit2.SelectedColor;
end;

procedure Tpsc_frm_setup.CheckBox_BoldClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldBold_InFont].AsBoolean :=
   CheckBox_Bold.Checked;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseBold].AsBoolean :=
   not (CheckBox_Bold.State = cbGrayed);
end;

procedure Tpsc_frm_setup.CheckBox_ItalicClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldItalic_InFont].AsBoolean :=
   CheckBox_Italic.Checked;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseItalic].AsBoolean :=
   not (CheckBox_Italic.State = cbGrayed);
end;

procedure Tpsc_frm_setup.CheckBox_UnderlineClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUnderline_InFont].AsBoolean :=
   CheckBox_Underline.Checked;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseUnderline].AsBoolean :=
   not (CheckBox_Underline.State = cbGrayed);
end;

procedure Tpsc_frm_setup.Button_ApplyClick(Sender: TObject);
begin
  MyGridColorsUpdate(Self);
end;

procedure Tpsc_frm_setup.FormCreate(Sender: TObject);
begin
  PSCFltDlg1.OnBeforeExecute := SetFltDlgReadOnly;
end;

procedure Tpsc_frm_setup.SetFltDlgReadOnly(Sender: TObject);
var
  MyIndex : integer;
  MyReadOnly : boolean;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  MyReadOnly :=
   TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldReadOnly].AsBoolean;
  PSCFltDlg1.Dialog.FltBox.Enabled := not MyReadOnly;
  PSCFltDlg1.Dialog.FltBox.Options := PSCFltDlg1.Dialog.FltBox.Options +
   [fboHideSortOrder, fboHideFindRecord, fboHideToggleFilter];
  if MyReadOnly then
  begin
    PSCFltDlg1.Dialog.FltBox.Options :=
     [fboHideSortOrder, fboHideFindRecord, fboHideToggleFilter,fboCheckReadOnly];
    PSCFltDlg1.Dialog.OKButton.Enabled := false;
  end;
end;

procedure Tpsc_frm_setup.CheckBox_StrikeOutClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldStrikeOut_InFont].AsBoolean :=
   CheckBox_StrikeOut.Checked;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseStrikeOut].AsBoolean :=
   not (CheckBox_StrikeOut.State = cbGrayed);
end;

procedure Tpsc_frm_setup.CheckBox_UseColorClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseColor].AsBoolean :=
   CheckBox_UseColor.Checked;
 PSCColorEdit1.Enabled := CheckBox_UseColor.Checked;
end;

procedure Tpsc_frm_setup.CheckBox_UseTextColorClick(Sender: TObject);
var
  MyIndex : integer;
begin
  MyIndex := PSCListBox_RecordsInfo.ItemIndex;
  if MyIndex < 0 then
    exit;
  TPSCListBoxItem(PSCListBox_RecordsInfo.Items[MyIndex]).UserFields[CPSCFieldUseFontColor].AsBoolean :=
   CheckBox_UseTextColor.Checked;
  PSCColorEdit2.Enabled := CheckBox_UseTextColor.Checked;
end;

function Tpsc_frm_setup.FillOptions(
  AListBoxItem: TPSCListBoxItem): TPSCRecordSetStyleOptions;
begin
  result := [];
  if AListBoxItem.UserFields[CPSCFieldUseColor].AsBoolean then
    result := result + [rssoSetColor];
  if AListBoxItem.UserFields[CPSCFieldUseFontColor].AsBoolean then
    result := result + [rssoSetFontColor];
  if AListBoxItem.UserFields[CPSCFieldUseBold].AsBoolean then
    result := result + [rssoSetFontBold];
  if AListBoxItem.UserFields[CPSCFieldUseItalic].AsBoolean then
    result := result + [rssoSetFontItalic];
  if AListBoxItem.UserFields[CPSCFieldUseUnderline].AsBoolean then
    result := result + [rssoSetFontUnderline];
  if AListBoxItem.UserFields[CPSCFieldUseStrikeOut].AsBoolean then
    result := result + [rssoSetFontStrikeOut];
end;

end.
