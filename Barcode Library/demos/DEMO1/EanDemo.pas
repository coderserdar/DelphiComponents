unit EanDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, EanKod, StdCtrls, Buttons, quickrpt, Qrctrls, EanQr, Grids,
  Mask, EanSpecs;

type
  TForm1 = class(TForm)
    PC: TPageControl;
    SheetEAN: TTabSheet;
    SheetMemo: TTabSheet;
    TabSheet5: TTabSheet;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Timer1: TTimer;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    EanMemo: TEanMemo;
    Mem: TMemo;
    Panel2: TPanel;
    PE: TLabel;
    CB9: TCheckBox;
    Label1: TLabel;
    ZnakovaSada: TLabel;
    Splitter1: TSplitter;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    TabSheet1: TTabSheet;
    ScrollBox1: TScrollBox;
    Memo1: TMemo;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    BitBtn8: TBitBtn;
    BitBtn10: TBitBtn;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    UpDown3: TUpDown;
    EanMemo1: TEanMemo;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    BitBtn3: TBitBtn;
    CC2: TComboBox;
    CC1: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    BarCode: TEdit;
    CB1: TCheckBox;
    CB2: TCheckBox;
    EdWidth: TEdit;
    EdHeight: TEdit;
    EdAngle: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    FTGroup: TRadioGroup;
    BitBtn4: TBitBtn;
    EdFileName: TEdit;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    BoxCaption: TGroupBox;
    CBCaption: TCheckBox;
    Label11: TLabel;
    CaptionText: TEdit;
    CaptionAlignment: TComboBox;
    Label12: TLabel;
    CaptionAutoSize: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    BitBtn5: TBitBtn;
    BCStyle: TCheckBox;
    TypBarCode: TRadioGroup;
    CaptionAuto: TCheckBox;
    EAN1: TEan;
    TabSheet3: TTabSheet;
    CB_AutoSize: TCheckBox;
    CB_Editor: TCheckBox;
    Se: TCheckBox;
    CB3: TCheckBox;
    CB4: TCheckBox;
    CB5: TCheckBox;
    Label16: TLabel;
    Label17: TLabel;
    BitBtn9: TBitBtn;
    BitBtn13: TBitBtn;
    Label18: TLabel;
    Ean2: TEan;
    Label19: TLabel;
    BitBtn14: TBitBtn;
    Button1: TButton;
    SelectCaption: TComboBox;
    procedure BitBtn2Click(Sender: TObject);
    procedure TypBarCodeClick(Sender: TObject);
    procedure BarCodeChange(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure CB2Click(Sender: TObject);
    procedure SeClick(Sender: TObject);
    procedure CB3Click(Sender: TObject);
    procedure CB4Click(Sender: TObject);
    procedure CB5Click(Sender: TObject);
    procedure CC1Change(Sender: TObject);
    procedure CC2Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CB9Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure EdWidthChange(Sender: TObject);
    procedure EdHeightChange(Sender: TObject);
    procedure Ean1Paint(Sender: TObject; R: TRect; ABarCode: String);
    procedure MemChange(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BCStyleClick(Sender: TObject);
    procedure FTGroupClick(Sender: TObject);
    procedure EdAngleChange(Sender: TObject);
    procedure CBCaptionClick(Sender: TObject);
    procedure CaptionTextChange(Sender: TObject);
    procedure CaptionAlignmentChange(Sender: TObject);
    procedure CaptionAutoSizeClick(Sender: TObject);
    procedure CaptionAutoClick(Sender: TObject);
    procedure CB_AutoSizeClick(Sender: TObject);
    procedure CB_EditorClick(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SelectCaptionChange(Sender: TObject);
  private
    FSelectedCaption : TBarcodeCaption;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses ShellAPI,EanAbout,EanRpt1,EanRpt2,EanRpt3;

{$R *.DFM}

const EanColors : Array [0..15] of TColor =
   (clBlack, clMaroon, clGreen, clOlive,
    clNavy,  clPurple, clNavy,  clGray,
    clSilver,clRed,clLime,clYellow,
    clBlue,clFuchsia,clAqua,clWhite);

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
     About;
end;

procedure TForm1.TypBarCodeClick(Sender: TObject);
var m:Integer;
begin
     Ean1.TypbarCode:=TTypBarCode(TypBarCode.ItemIndex);
     m:=Ean1.MinWidth;
     if Ean1.Width<m then
        UpDown4.Position := m;

     CB1.Enabled:=(Ean1.TypBarCode in [bcEan8,bcEan13,bcISBN, bcISSN, bcISMN,
            bcUPCA,bcUPCE0,bcUPCE1,bcUPCShipping]);

     ZnakovaSada.Caption:=Ean1.GetSetOfCharsVisible;
     FSelectedCaption.Text := 'Sample '+Ean1.GetBarcodeInfo.Name;
     FSelectedCaption.UpdateCaption;
     
     BarCode.Text:= Ean1.BarCode;
end;

procedure TForm1.BarCodeChange(Sender: TObject);
begin
     Ean1.BarCode:=BarCode.Text;
end;

procedure TForm1.CB1Click(Sender: TObject);
begin
     Ean1.Ean13AddUp:=CB1.Checked;
end;

procedure TForm1.CB2Click(Sender: TObject);
begin
     EAN1.FontAutoSize:=CB2.Checked;
end;

procedure TForm1.SeClick(Sender: TObject);
begin
     Ean1.Security := Se.Checked;
end;

procedure TForm1.CB3Click(Sender: TObject);
begin
     Ean1.StartStopLines:=CB3.Checked;
end;

procedure TForm1.CB4Click(Sender: TObject);
begin
     Ean1.ShowLabels:=CB4.Checked;
end;

procedure TForm1.CB5Click(Sender: TObject);
begin
     Ean1.Transparent:=CB5.Checked;
end;

procedure TForm1.CC1Change(Sender: TObject);
begin
   Ean1.BackgroundColor := EanColors[CC1.ItemIndex];
end;

procedure TForm1.CC2Change(Sender: TObject);
begin
     Ean1.LinesColor:=EanColors[CC2.ItemIndex];
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
var F:TFontDialog;
begin
     F:=TFontDialog.Create(Application);
     try
        F.Font.Assign(Ean1.Font);
        if F.Execute then
           Ean1.Font.Assign(F.Font);
     finally
        F.Free;
     end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
     ZnakovaSada.Caption:=Ean1.GetSetOfCharsVisible;
     BarCode.Text :=Ean1.BarCode;
     CB1.Checked  :=Ean1.Ean13AddUp;
     CB2.Checked  :=Ean1.FontAutoSize;
     Se.Checked   :=Ean1.Security;
     CB3.Checked  :=Ean1.StartStopLines;
     CB4.Checked  :=Ean1.ShowLabels;
     CB5.Checked  :=Ean1.Transparent;
     EdWidth.Text :=IntToStr(Ean1.Width);
     EdHeight.Text:=IntToStr(Ean1.Height);
     EdAngle.Text :=IntToStr(Ean1.Angle);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     if (CB9.Checked) then begin
        Ean1.Next;
        BarCode.Text:=Ean1.BarCode;
     end;
end;

procedure TForm1.CB9Click(Sender: TObject);
begin
     Timer1.Enabled:=CB9.Checked;
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
     Ean1.SaveToFile(EdFileName.Text);
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
var F:TFontDialog;
begin
     F:=TFontDialog.Create(Application);
     try
        F.Font.Assign(FSelectedCaption.Font);
        if F.Execute then
           FSelectedCaption.Font.Assign(F.Font);
     finally
        F.Free;
     end;
end;

procedure TForm1.EdWidthChange(Sender: TObject);
begin
     Ean1.Width:=StrToInt(EdWidth.Text);
end;

procedure TForm1.EdHeightChange(Sender: TObject);
begin
     Ean1.Height:=StrToInt(EdHeight.Text);
end;

procedure TForm1.Ean1Paint(Sender: TObject; R: TRect; ABarCode: String);
begin
     PE.Caption := Ean1.LastPaintErrorText;
     inherited; 
end;


procedure TForm1.MemChange(Sender: TObject);
begin
     EanMemo.Lines.Assign(Mem.Lines);
     EanMemo.Invalidate;
     EanMemo1.Lines.Assign(Mem.Lines);
     EanMemo1.Invalidate;
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
     EanMemo.CopyToClipboard;
     MessageDlg('Barcode coppied to clipboard ...',mtInformation,[mbOK],0);
end;

procedure TForm1.BitBtn7Click(Sender: TObject);
begin
     EanMemo.SaveAsBitmap('');
end;

procedure TForm1.BitBtn9Click(Sender: TObject);
begin
     Rpt1.QR1.Preview;
end;



procedure TForm1.BitBtn10Click(Sender: TObject);
begin
     EanMemo.CopyToClipboardWMF;
     MessageDlg('Barcode coppied to clipboard ...',mtInformation,[mbOK],0);
end;

procedure TForm1.BitBtn8Click(Sender: TObject);
begin
     EanMemo.SaveAsWMF('');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     FSelectedCaption := EAN1.Caption;
     
     About;
     CC1.ItemIndex :=15;
     CC2.ItemIndex := 0;

     TypBarCode.Items.Clear;
     EAN1.AddTypesToList(TypBarCode.Items,btSymbol);
     TypBarCode.ItemIndex:=1;
     CaptionAlignment.ItemIndex := Integer(FSelectedCaption.Alignment);
     PC.ActivePage:=SheetEAN;

     SelectCaption.ItemIndex := 0;
end;

procedure TForm1.BitBtn11Click(Sender: TObject);
begin
  ShellExecute(handle, nil, 'http://www.ke.telecom.sk/psoft', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.BitBtn12Click(Sender: TObject);
begin
  ShellExecute(handle, nil, 'mailto:psoft@ke.telecom.sk', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.BCStyleClick(Sender: TObject);
begin
     if BCStyle.Checked then
        EAN1.AddTypesToList(TypBarCode.Items,btText)
     else
        EAN1.AddTypesToList(TypBarCode.Items,btSymbol);

end;




procedure TForm1.FTGroupClick(Sender: TObject);
var ext:String;
begin
     case FTGroup.ItemIndex of
          0 : Ext := '.BMP';
          1 : Ext := '.WMF';
          2 : Ext := '.EMF';
          3 : Ext := '.GIF';
          4 : Ext := '.JPG';
     end;
     EdFileName.Text := ChangeFileExt(EdFileName.Text,ext);
end;

procedure TForm1.EdAngleChange(Sender: TObject);
begin
     Ean1.Angle := StrToInt(EdAngle.Text);
end;

procedure TForm1.CBCaptionClick(Sender: TObject);
begin
     BoxCaption.Enabled   := CBCaption.Checked;
     FSelectedCaption.Visible := CBCaption.Checked;
end;

procedure TForm1.CaptionTextChange(Sender: TObject);
begin
     FSelectedCaption.Text := CaptionText.Text;
end;

procedure TForm1.CaptionAlignmentChange(Sender: TObject);
begin
     FSelectedCaption.Alignment := TAlignment(CaptionAlignment.ItemIndex);
end;



procedure TForm1.CaptionAutoSizeClick(Sender: TObject);
begin
     FSelectedCaption.AutoSize := CaptionAutoSize.Checked;
end;


procedure TForm1.CaptionAutoClick(Sender: TObject);
begin
     FSelectedCaption.AutoCaption := CaptionAuto.Checked;
end;

procedure TForm1.CB_AutoSizeClick(Sender: TObject);
begin
     EAN1.AutoSize := CB_AutoSize.Checked;
end;

procedure TForm1.CB_EditorClick(Sender: TObject);
begin
     Ean1.DisableEditor := CB_Editor.Checked;
end;





procedure TForm1.BitBtn13Click(Sender: TObject);
begin
     Rpt2.QR2.Preview;
end;


procedure TForm1.BitBtn14Click(Sender: TObject);
begin
     Rpt3.QR3.Preview;
end;

procedure TForm1.Button1Click(Sender: TObject);
var fn:String;
begin
     fn:=ExtractFileDir(Application.ExeName)+'\html\ean.htm';
     EAN1.ExportToHTML(fn,gtBmp);
     ShellExecute(handle, nil, PChar(fn), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.SelectCaptionChange(Sender: TObject);
begin
     if SelectCaption.ItemIndex=0 then FSelectedCaption := EAN1.Caption
     else                              FSelectedCaption := EAN1.CaptionBottom;

     CBCaption.Checked := FSelectedCaption.Visible;
     CaptionText.Text  := FSelectedCaption.Text;
     CaptionAlignment.ItemIndex  := Integer(FSelectedCaption.Alignment);
     CaptionAutosize.Checked :=FSelectedCaption.AutoSize;
     CaptionAuto.Checked     :=FSelectedCaption.AutoCaption;

end;

end.

