unit PBEditPackDemo_unit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, PBBinHexEdit, Mask, PBMaskEdit, StdCtrls, PBEdit, PBNumEdit, Spin,
	PBSpinEdit, ComCtrls, PBSuperSpin, ImgList, PBEditEx;

type
  TPBEditPackDemoForm = class(TForm)
    PBNumEdit1: TPBNumEdit;
    PBBinHexEdit1: TPBBinHexEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    Bevel2: TBevel;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Bevel3: TBevel;
    Bevel1: TBevel;
    PBMaskEdit1: TPBMaskEdit;
    PBSpinEdit1: TPBSpinEdit;
    CheckBox2: TCheckBox;
    PBEdit1: TPBEdit;
    PBSuperSpin1: TPBSuperSpin;
    Label4: TLabel;
    DecimalEdit: TPBSuperSpin;
    Label5: TLabel;
    CancelButton: TButton;
		ImageList1: TImageList;
    CheckBox3: TCheckBox;
    PBEditEx1: TPBEditEx;
    Label1: TLabel;
    Label6: TLabel;
    PBSpinEdit2: TPBSpinEdit;
    PBSpinEdit3: TPBSpinEdit;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetDecimalsClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure PBSpinEdit3Change(Sender: TObject);
    procedure PBSpinEdit2Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
	public
    { Public declarations }
  end;

var
  PBEditPackDemoForm: TPBEditPackDemoForm;

implementation

{$R *.DFM}

procedure TPBEditPackDemoForm.Button1Click(Sender: TObject);
begin
	if PBEdit1.Alignment = taCenter then PBEdit1.Alignment := taLeftJustify
	else if PBEdit1.Alignment = taLeftJustify then PBEdit1.Alignment := taRightJustify
	else PBEdit1.Alignment := taCenter;
	if PBMaskEdit1.Alignment = taCenter then PBMaskEdit1.Alignment := taLeftJustify
	else if PBMaskEdit1.Alignment = taLeftJustify then PBMaskEdit1.Alignment := taRightJustify
	else PBMaskEdit1.Alignment := taCenter;
	if PBNumEdit1.Alignment = taRightJustify then PBNumEdit1.Alignment := taCenter
	else if PBNumEdit1.Alignment = taCenter then PBNumEdit1.Alignment := taLeftJustify
	else PBNumEdit1.Alignment := taRightJustify;
	if PBSuperSpin1.Alignment = taRightJustify then PBSuperSpin1.Alignment := taCenter
	else if PBSuperSpin1.Alignment = taCenter then PBSuperSpin1.Alignment := taLeftJustify
	else PBSuperSpin1.Alignment := taRightJustify;
	if PBBinHexEdit1.Alignment = taCenter then PBBinHexEdit1.Alignment := taLeftJustify
	else if PBBinHexEdit1.Alignment = taLeftJustify then PBBinHexEdit1.Alignment := taRightJustify
	else PBBinHexEdit1.Alignment := taCenter;
	if PBSpinEdit1.Alignment = taCenter then PBSpinEdit1.Alignment := taLeftJustify
	else if PBSpinEdit1.Alignment = taLeftJustify then PBSpinEdit1.Alignment := taRightJustify
	else PBSpinEdit1.Alignment := taCenter;
	if PBEditEx1.Alignment = taCenter then PBEditEx1.Alignment := taLeftJustify
	else if PBEditEx1.Alignment = taLeftJustify then PBEditEx1.Alignment := taRightJustify
	else PBEditEx1.Alignment := taCenter;
end;

procedure TPBEditPackDemoForm.Button2Click(Sender: TObject);
begin
	if PBBinHexEdit1.BaseFormat = Binary then PBBinHexEdit1.BaseFormat := HexaDecimal
	else if PBBinHexEdit1.BaseFormat = HexaDecimal then PBBinHexEdit1.BaseFormat := Number
	else if PBBinHexEdit1.BaseFormat = Number then PBBinHexEdit1.BaseFormat := Binary;
end;

procedure TPBEditPackDemoForm.Button3Click(Sender: TObject);
begin
	if PBNumEdit1.NumberFormat = Engineering then PBNumEdit1.NumberFormat := Standard
	else if PBNumEdit1.NumberFormat = Standard then PBNumEdit1.NumberFormat := Thousands
	else if PBNumEdit1.NumberFormat = Thousands then PBNumEdit1.NumberFormat := Scientific
	else PBNumEdit1.NumberFormat := Engineering;
	if PBSuperSpin1.NumberFormat = Engineering then PBSuperSpin1.NumberFormat := Standard
	else if PBSuperSpin1.NumberFormat = Standard then PBSuperSpin1.NumberFormat := Thousands
	else if PBSuperSpin1.NumberFormat = Thousands then PBSuperSpin1.NumberFormat := Scientific
	else PBSuperSpin1.NumberFormat := Engineering;
end;

procedure TPBEditPackDemoForm.FormCreate(Sender: TObject);
begin
	DecimalEdit.AsInteger := PBNumEdit1.Decimals;
end;

procedure TPBEditPackDemoForm.SetDecimalsClick(Sender: TObject);
begin
	PBNumEdit1.Decimals := DecimalEdit.AsInteger;
	PBSuperSpin1.Decimals := DecimalEdit.AsInteger;
	if PBEditEx1 <> nil then PBEditEx1.ImageIndex := DecimalEdit.AsInteger;
end;


procedure TPBEditPackDemoForm.CheckBox1Click(Sender: TObject);
begin
	Button1.Default := CheckBox1.Checked;
end;

procedure TPBEditPackDemoForm.CheckBox2Click(Sender: TObject);
begin
	PBBinHexEdit1.AutoSelect := not PBBinHexEdit1.AutoSelect;
	PBEdit1.AutoSelect := not PBEdit1.AutoSelect;
	PBMaskEdit1.AutoSelect := not PBMaskEdit1.AutoSelect;
	PBNumEdit1.AutoSelect := not PBNumEdit1.AutoSelect;
	PBSuperSpin1.AutoSelect := not PBSuperSpin1.AutoSelect;
	PBSpinEdit1.AutoSelect := not PBSpinEdit1.AutoSelect;
	PBEditEx1.AutoSelect := not PBEditEx1.AutoSelect;
end;

procedure TPBEditPackDemoForm.CancelButtonClick(Sender: TObject);
begin
	Application.MessageBox('Pressing Escape automatically calls the CancelButton''s OnClick-event !',
		'Cancel !', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
end;

procedure TPBEditPackDemoForm.CheckBox3Click(Sender: TObject);
begin
	PBBinHexEdit1.Enabled := not PBBinHexEdit1.Enabled;
	PBEdit1.Enabled := not PBEdit1.Enabled;
	PBMaskEdit1.Enabled := not PBMaskEdit1.Enabled;
	PBNumEdit1.Enabled := not PBNumEdit1.Enabled;
	PBSuperSpin1.Enabled := not PBSuperSpin1.Enabled;
	PBSpinEdit1.Enabled := not PBSpinEdit1.Enabled;
	PBEditEx1.Enabled := not PBEditEx1.Enabled;
end;

procedure TPBEditPackDemoForm.PBSpinEdit3Change(Sender: TObject);
begin
	PBBinHexEdit1.BinLength := PBSpinEdit3.Value;
end;

procedure TPBEditPackDemoForm.PBSpinEdit2Change(Sender: TObject);
begin
	PBBinHexEdit1.HexLength := PBSpinEdit2.Value;
end;

procedure TPBEditPackDemoForm.Button4Click(Sender: TObject);
var
	DStr : string;
begin
	DStr := InputBox('Separators', '(Only this app)'#13#10
		+ 'Decimal & Thousand (2 chars):',
		DecimalSeparator + ThousandSeparator);
	if Length(DStr) = 2 then
	begin
		DecimalSeparator := DStr[1];
		ThousandSeparator := DStr[2];
		NotifyControls(PB_SETTINGCHANGE);
	end;
end;

procedure TPBEditPackDemoForm.Button5Click(Sender: TObject);
var
	DStr, Ch1, Ch2 : string;
begin
	DStr := InputBox('Separators', '(Same as controlpanel/international)'
		+ #13#10'Decimal & Thousand (2 chars):',
		DecimalSeparator + ThousandSeparator);
	if Length(DStr) = 2 then
	begin
		Ch1 := Dstr[1];
		Ch2 := Dstr[2];
		SetLocaleInfo(GetUserDefaultLCID, LOCALE_SDECIMAL, @Ch1[1]);
		SetLocaleInfo(GetUserDefaultLCID, LOCALE_STHOUSAND, @Ch2[1]);
		SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
	end;
end;

end.

