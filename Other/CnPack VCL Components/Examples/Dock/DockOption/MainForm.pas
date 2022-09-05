unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, CnDockFormControl, CnVIDDockStyle, StdCtrls, Spin, ComCtrls,
  ImgList, CnClasses, CnCommon;

type
  TMain_Form = class(TForm)
    CnDockServer1: TCnDockServer;
    CnVIDDockStyle1: TCnVIDDockStyle;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    ActivePanelFont_Button: TButton;
    ActivePanelStartColor_Button: TButton;
    ActivePanelEndColor_Button: TButton;
    GroupBox3: TGroupBox;
    InactivePanelFont_Button: TButton;
    InactivePanelStartColor_Button: TButton;
    InactivePanelEndColor_Button: TButton;
    GrabbersSize_SpinEdit: TSpinEdit;
    SplitterWidth_SpinEdit: TSpinEdit;
    TextAlignment_ComboBox: TComboBox;
    SystemInfo_CheckBox: TCheckBox;
    TextEllipsis_CheckBox: TCheckBox;
    Label6: TLabel;
    GroupBox5: TGroupBox;
    ActiveTabFont_Button: TButton;
    ActiveTabColor_Button: TButton;
    GroupBox6: TGroupBox;
    InactiveTabFont_Button: TButton;
    InctiveTabColor_Button: TButton;
    TabPosition_ComboBox: TComboBox;
    HotTrack_CheckBox: TCheckBox;
    ShowIcon_CheckBox: TCheckBox;
    TrackColor_Button: TButton;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    ImageList1: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActivePanelFont_ButtonClick(Sender: TObject);
    procedure ActivePanelStartColor_ButtonClick(Sender: TObject);
    procedure ActivePanelEndColor_ButtonClick(Sender: TObject);
    procedure InactivePanelFont_ButtonClick(Sender: TObject);
    procedure InactivePanelStartColor_ButtonClick(Sender: TObject);
    procedure InactivePanelEndColor_ButtonClick(Sender: TObject);
    procedure GrabbersSize_SpinEditChange(Sender: TObject);
    procedure SplitterWidth_SpinEditChange(Sender: TObject);
    procedure TextAlignment_ComboBoxChange(Sender: TObject);
    procedure SystemInfo_CheckBoxClick(Sender: TObject);
    procedure TextEllipsis_CheckBoxClick(Sender: TObject);
    procedure ActiveTabFont_ButtonClick(Sender: TObject);
    procedure InactiveTabFont_ButtonClick(Sender: TObject);
    procedure ActiveTabColor_ButtonClick(Sender: TObject);
    procedure InctiveTabColor_ButtonClick(Sender: TObject);
    procedure HotTrack_CheckBoxClick(Sender: TObject);
    procedure ShowIcon_CheckBoxClick(Sender: TObject);
    procedure TabPosition_ComboBoxChange(Sender: TObject);
    procedure CnVIDDockStyle1SystemInfoChange(Value: Boolean);
    procedure TrackColor_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DoReadOption;
    procedure DoReadConjoinOption;
    procedure DoReadTabOption;
  public
    { Public declarations }
    procedure CreateDockWidnow;
  end;

var
  Main_Form: TMain_Form;


implementation

uses DockWindow;

const
  DockFormCount = 10;

var
  { ����ʮ��ͣ���ͻ����� }
  DockForms: array[0..DockFormCount - 1] of TDockWindow_Form;
  { ����һ��ȫ�ֶ���ConjoinOption������ʵ��CnVIDDockStyle1��ConjoinServerOption���� }
  ConjoinOption: TCnVIDConjoinServerOption;
  { ����һ��ȫ�ֶ���ConjoinOption������ʵ��CnVIDDockStyle1��TabServerOption���� }
  TabOption: TCnVIDTabServerOption;

{$R *.DFM}

procedure TMain_Form.DoReadOption;
begin
  DoReadConjoinOption;
  DoReadTabOption;
end;

procedure TMain_Form.DoReadConjoinOption;
begin
  { ���ȸ�ConjoinOption��ֵ }
  ConjoinOption := TCnVIDConjoinServerOption(CnVIDDockStyle1.ConjoinServerOption);
  { Ȼ��ȡ��ConjoinOption�еĸ�������ֵ��ֵ�������ؼ�����ʾ }
  GrabbersSize_SpinEdit.Value := ConjoinOption.GrabbersSize;
  SplitterWidth_SpinEdit.Value := ConjoinOption.SplitterWidth;
  TextAlignment_ComboBox.ItemIndex := Integer(ConjoinOption.TextAlignment);
  SystemInfo_CheckBox.Checked := ConjoinOption.SystemInfo;
  TextEllipsis_CheckBox.Checked := ConjoinOption.TextEllipsis;
end;

procedure TMain_Form.DoReadTabOption;
begin
  { ���ȸ�TabOption��ֵ}
  TabOption := TCnVIDTabServerOption(CnVIDDockStyle1.TabServerOption);
  { Ȼ��ȡ��TabOption�еĸ�������ֵ��ֵ�������ؼ�����ʾ }
  HotTrack_CheckBox.Checked := TabOption.HotTrack;
  ShowIcon_CheckBox.Checked := TabOption.ShowTabImages;
  case TabOption.TabPosition of
    tpTop:    TabPosition_ComboBox.ItemIndex := 0;
    tpBottom: TabPosition_ComboBox.ItemIndex := 1;
  end;
end;

procedure TMain_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //SaveDockTreeToFile(_CnExtractFilePath(Application.ExeName) + 'DockInfo.ini');
end;

procedure TMain_Form.ActivePanelFont_ButtonClick(Sender: TObject);
begin
  // ��������ý���ʱ�������
  FontDialog1.Font.Assign(ConjoinOption.ActiveFont);
  if FontDialog1.Execute then
    ConjoinOption.ActiveFont := FontDialog1.Font;
end;

procedure TMain_Form.ActivePanelStartColor_ButtonClick(Sender: TObject);
begin
  // ��������ý���ʱ��������Ŀ�ʼ���ֵ���ɫ
  ColorDialog1.Color := ConjoinOption.ActiveTitleStartColor;
  if ColorDialog1.Execute then
    ConjoinOption.ActiveTitleStartColor := ColorDialog1.Color;
end;

procedure TMain_Form.ActivePanelEndColor_ButtonClick(Sender: TObject);
begin
  // ��������ý���ʱ��������Ľ������ֵ���ɫ
  ColorDialog1.Color := ConjoinOption.ActiveTitleEndColor;
  if ColorDialog1.Execute then
    ConjoinOption.ActiveTitleEndColor := ColorDialog1.Color;
end;

procedure TMain_Form.InactivePanelFont_ButtonClick(Sender: TObject);
begin
  // ������ʧȥ����ʱ�������
  FontDialog1.Font.Assign(ConjoinOption.InactiveFont);
  if FontDialog1.Execute then
    ConjoinOption.InactiveFont := FontDialog1.Font;
end;

procedure TMain_Form.InactivePanelStartColor_ButtonClick(Sender: TObject);
begin
  // ʧȥ����ʱ��������Ŀ�ʼ���ֵ���ɫ
  ColorDialog1.Color := ConjoinOption.ActiveTitleStartColor;
  if ColorDialog1.Execute then
    ConjoinOption.ActiveTitleStartColor := ColorDialog1.Color;
end;

procedure TMain_Form.InactivePanelEndColor_ButtonClick(Sender: TObject);
begin
  // ʧȥ����ʱ��������Ľ������ֵ���ɫ
  ColorDialog1.Color := ConjoinOption.InactiveTitleEndColor;
  if ColorDialog1.Execute then
    ConjoinOption.InactiveTitleEndColor := ColorDialog1.Color;
end;

procedure TMain_Form.GrabbersSize_SpinEditChange(Sender: TObject);
begin
  // ���ֵĴ�С
  ConjoinOption.GrabbersSize := GrabbersSize_SpinEdit.Value;
end;

procedure TMain_Form.SplitterWidth_SpinEditChange(Sender: TObject);
begin
  // �ָ����Ŀ��
  ConjoinOption.SplitterWidth := SplitterWidth_SpinEdit.Value;
end;

procedure TMain_Form.TextAlignment_ComboBoxChange(Sender: TObject);
begin
  // �����������ֶ��뷽ʽ
  ConjoinOption.TextAlignment := TAlignment(TextAlignment_ComboBox.ItemIndex);
end;

procedure TMain_Form.SystemInfo_CheckBoxClick(Sender: TObject);
begin
  // �Ƿ���ϵͳ��Ϣ����������
  if ConjoinOption <> nil then
    ConjoinOption.SystemInfo := SystemInfo_CheckBox.Checked;
  DoReadConjoinOption;
end;

procedure TMain_Form.TextEllipsis_CheckBoxClick(Sender: TObject);
begin
  // �������������Ƿ���ʡ�Ժ�
//  if ConjoinOption <> nil then
    ConjoinOption.TextEllipsis := TextEllipsis_CheckBox.Checked;
end;

procedure TMain_Form.ActiveTabFont_ButtonClick(Sender: TObject);
begin
  // Tabҳ���ý���ʱ�������
  FontDialog1.Font.Assign(TabOption.ActiveFont);
  if FontDialog1.Execute then
    TabOption.ActiveFont := FontDialog1.Font;
end;

procedure TMain_Form.ActiveTabColor_ButtonClick(Sender: TObject);
begin
  // Tabҳ���ý���ʱ�����ɫ
  ColorDialog1.Color := TabOption.ActiveSheetColor;
  if ColorDialog1.Execute then
    TabOption.ActiveSheetColor := ColorDialog1.Color;
end;

procedure TMain_Form.InactiveTabFont_ButtonClick(Sender: TObject);
begin
  // Tabҳ��ʧȥ����ʱ�������
  FontDialog1.Font.Assign(TabOption.InactiveFont);
  if FontDialog1.Execute then
    TabOption.InactiveFont := FontDialog1.Font;
end;

procedure TMain_Form.InctiveTabColor_ButtonClick(Sender: TObject);
begin
  // Tabҳ��ʧȥ����ʱ�����ɫ
  ColorDialog1.Color := TabOption.InactiveSheetColor;
  if ColorDialog1.Execute then
    TabOption.InactiveSheetColor := ColorDialog1.Color;
end;

procedure TMain_Form.HotTrack_CheckBoxClick(Sender: TObject);
begin
  // ������ƶ���ĳһ��ҳ���ʱ��Tabҳ���Ƿ������ʾ
  TabOption.HotTrack := HotTrack_CheckBox.Checked;
end;

procedure TMain_Form.ShowIcon_CheckBoxClick(Sender: TObject);
begin
  // Tabҳ�������ʾʱ�����ɫ
  TabOption.ShowTabImages := ShowIcon_CheckBox.Checked;
end;

procedure TMain_Form.TabPosition_ComboBoxChange(Sender: TObject);
begin
  // Tabҳ���λ��
  case TabPosition_ComboBox.ItemIndex of
    0: TabOption.TabPosition := tpTop;
    1: TabOption.TabPosition := tpBottom;
  end;
end;

procedure TMain_Form.CnVIDDockStyle1SystemInfoChange(Value: Boolean);
begin
  SystemInfo_CheckBox.Checked := Value;
end;

procedure TMain_Form.TrackColor_ButtonClick(Sender: TObject);
begin
  // ������ʾ��Tab����ɫ
  ColorDialog1.Color := TabOption.HotTrackColor;
  if ColorDialog1.Execute then
    TabOption.HotTrackColor := ColorDialog1.Color;
end;

procedure TMain_Form.CreateDockWidnow;
var i: Integer;
begin
  for i := 0 to DockFormCount - 1 do
  begin
    DockForms[i] := TDockWindow_Form.Create(nil);
    DockForms[i].Caption := DockForms[i].Caption + IntToStr(i+1);
    { Ϊÿһ���������ò�ͬ��ͼ�� }
    ImageList1.GetIcon(i, DockForms[i].Icon);
  end;
  LoadDockTreeFromFile(_CnExtractFilePath(Application.ExeName) + 'DockInfo.ini');
  DoReadOption;
end;

procedure TMain_Form.FormCreate(Sender: TObject);
begin
  CreateDockWidnow;
end;

end.
