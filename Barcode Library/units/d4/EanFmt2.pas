unit EanFmt2;

interface

{$I ean.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, EanKod, ExtCtrls, EanSpecs,
  {$ifdef PSOFT_PDF417} EanPDF417, {$endif}
  ComCtrls;

const MAX_CB = 50;

type
  TEANSetupFmt = class(TForm)
    ZAL: TPageControl;
    SH_MAIN: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    SH_COPYRIGHT: TTabSheet;
    BtnOk: TBitBtn;
    CBTyp: TComboBox;
    BarCode: TEdit;
    ZNAKY: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    BGColor: TComboBox;
    LinesColor: TComboBox;
    AddUp: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    Transparent: TCheckBox;
    StartStopLines: TCheckBox;
    ShowLabels: TCheckBox;
    SecurityZoom: TCheckBox;
    FontAutoSize: TCheckBox;
    L3: TLabel;
    FD: TFontDialog;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    BtnApply: TBitBtn;
    BitBtn2: TBitBtn;
    LblAngle: TLabel;
    EAN_ANGLE: TEdit;
    UD_Angle: TUpDown;
    TabSheet1: TTabSheet;
    Label6: TLabel;
    EANMEMO: TMemo;
    CO: TColorDialog;
    TabSheet4: TTabSheet;
    GR_Caption: TGroupBox;
    CA_Alignment: TComboBox;
    L4: TLabel;
    CA_AutoCaption: TCheckBox;
    CA_AutoSize: TCheckBox;
    L2: TLabel;
    Label7: TLabel;
    CA_Font: TLabel;
    FONT_UP: TBitBtn;
    EAN_Font: TLabel;
    L1: TLabel;
    Label8: TLabel;
    EAN_Width: TEdit;
    EAN_Height: TEdit;
    UD_Width: TUpDown;
    UD_Height: TUpDown;
    TabSheet5: TTabSheet;
    BitBtn3: TBitBtn;
    SD: TSaveDialog;
    CA_Visible: TCheckBox;
    BCA_Visible: TCheckBox;
    BGR_CAPTION: TGroupBox;
    BCA_FONT: TLabel;
    BCA_Alignment: TComboBox;
    BCA_AutoCaption: TCheckBox;
    BCA_AutoSize: TCheckBox;
    BCA_TEXT: TEdit;
    FONT_DOWN: TBitBtn;
    CA_TEXT: TEdit;
    C1: TCheckBox;
    C2: TCheckBox;
    c3: TCheckBox;
    c4: TCheckBox;
    c5: TCheckBox;
    c6: TCheckBox;
    c7: TCheckBox;
    c8: TCheckBox;
    c9: TCheckBox;
    c10: TCheckBox;
    c11: TCheckBox;
    c12: TCheckBox;
    c13: TCheckBox;
    c14: TCheckBox;
    c15: TCheckBox;
    c16: TCheckBox;
    c17: TCheckBox;
    c18: TCheckBox;
    c19: TCheckBox;
    c20: TCheckBox;
    c21: TCheckBox;
    c22: TCheckBox;
    c23: TCheckBox;
    c24: TCheckBox;
    c25: TCheckBox;
    c26: TCheckBox;
    c27: TCheckBox;
    c28: TCheckBox;
    c29: TCheckBox;
    c30: TCheckBox;
    EAN: TEan;
    Image2: TImage;
    SH_PDF417: TTabSheet;
    PDF_ErrorLevel: TComboBox;
    Label9: TLabel;
    PDF_MODE: TComboBox;
    Label10: TLabel;
    PDF_COLSlbl: TLabel;
    PDF_ROWSlbl: TLabel;
    PDF_COLS: TEdit;
    PDF_ROWS: TEdit;
    PDF_TRUNCATED: TCheckBox;
    procedure CBTypChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TransparentClick(Sender: TObject);
    procedure BGColorDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure BGColorChange(Sender: TObject);
    procedure LinesColorChange(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BarCodeChange(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure CA_VisibleClick(Sender: TObject);
    procedure CA_AutoCaptionClick(Sender: TObject);
    procedure FONT_UPClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure PDF_ErrorLevelChange(Sender: TObject);
    procedure PDF_MODEChange(Sender: TObject);
    procedure PDF_COLSChange(Sender: TObject);
    procedure PDF_ROWSChange(Sender: TObject);
    procedure PDF_TRUNCATEDClick(Sender: TObject);
  private
    ParentEAN        : TCustomEAN;
    procedure UpdateParentEAN;
  public
    procedure SetSelectedCaption(C:TBarcodeCaption);
    procedure SetParentEAN(E:TCustomEAN);
  end;

var
  EANSetupFmt: TEANSetupFmt;

implementation

{$R *.DFM}




type TIdentMapEntry = record
          Value : TColor;
          Name  : String[20];
     end;
var MyColors : array[0..16] of TIdentMapEntry = (
    (Value: clBlack;   Name: 'Black'),
    (Value: clMaroon;  Name: 'Maroon'),
    (Value: clGreen;   Name: 'Green'),
    (Value: clOlive;   Name: 'Olive'),
    (Value: clNavy;    Name: 'Navy'),
    (Value: clPurple;  Name: 'Purple'),
    (Value: clTeal;    Name: 'Teal'),
    (Value: clGray;    Name: 'Gray'),
    (Value: clSilver;  Name: 'Silver'),
    (Value: clRed;     Name: 'Red'),
    (Value: clLime;    Name: 'Lime'),
    (Value: clYellow;  Name: 'Yellow'),
    (Value: clBlue;    Name: 'Blue'),
    (Value: clFuchsia; Name: 'Fuchsia'),
    (Value: clAqua;    Name: 'Aqua'),
    (Value: clWhite;   Name: 'White'),
    (Value: 0;         Name: 'User color'));

function IndexMyColor(Color:TColor):Integer;
var i:Integer;
begin
    Result := High(MyColors);
    for i:=Low(MyColors) to High(MyColors)-1 do
        if MyColors[i].Value=Color then begin
           Result := i;
           Exit;
        end;

end;

procedure TEANSetupFmt.CBTypChange(Sender: TObject);
begin
     EAN.TypBarCode := TTypBarCode(CBTyp.ItemIndex);
     EAN.Width      := EAN.MinWidth;
     BarCode.Text   := Ean.BarCode;
     AddUp.Visible  := (EAN.TypBarCode=bcEan13);
     ZNAKY.Caption  := EAN.GetSetOfCharsVisible;
end;

procedure TEANSetupFmt.FormCreate(Sender: TObject);
var i:Integer;
    C:TCheckBox;
begin
     Caption := 'Barcode library - version '+BarcodeLibraryVersion ;
     for i:=1 to MAX_CB do begin
         C:=TCheckBox(FindComponent('C'+IntToStr(i)));
         if C<>nil then begin
            C.caption := IntToStr(i)+'. Digit';
         end;
     end;
     ZAL.ActivePage  := SH_MAIN;
     EAN.AddTypesToList(CBTyp.Items,btText);
     CBTyp.ItemIndex := 1;
     EAN.TypBarCode  := bcEan13;
     EAN.Width       := EAN.MinWidth;
     BarCode.Text    := EAN.BarCode;

     BGColor.Items.Clear;
     for i:= Low(MyColors) to High(MyColors) do
         BGColor.Items.Add(MyColors[i].Name);
     LinesColor.Items.Assign(BGColor.Items);

     BGColor.ItemIndex    :=15;
     LinesColor.ItemIndex :=1 ;

     ZNAKY.Caption  := EAN.GetSetOfCharsVisible;

    {$ifndef PSOFT_PDF417}
             SH_PDF417.Visible := False;
    {$endif}
end;

procedure TEANSetupFmt.TransparentClick(Sender: TObject);
begin
     if Sender=Transparent    then EAN.Transparent           := Transparent.Checked;
     if Sender=FontAutoSize   then EAN.FontAutoSize          := FontAutoSize.Checked;
     if Sender=StartStopLines then EAN.StartStopLines        := StartStopLines.Checked;
     if Sender=ShowLabels     then EAN.ShowLabels            := ShowLabels.Checked;
     if Sender=SecurityZoom   then EAN.Security              := SecurityZoom.Checked;
end;

procedure TEANSetupFmt.BGColorDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var ARect : TRect;
    Text  : array[0..255] of Char;
    Old   : TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  ARect.Right := ARect.Left + 50;

  with TComboBox(Control).Canvas do begin
    FillRect(Rect);
    Old := Brush.Color;
    Pen.Color   := clWindowText;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Brush.Color := MyColors[Index].Value;
    Brush.Style := bsSolid;
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect);
    finally
      Brush.Color := Old;
    end;

    StrPCopy(Text, MyColors[Index].Name);
    Rect.Left := Rect.Left + 60;
    DrawText(Handle, Text, StrLen(Text), Rect,
{$IFDEF PSOFT_D4}
    DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
{$ELSE}
    DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
{$ENDIF}
  end;
end;

procedure TEANSetupFmt.BGColorChange(Sender: TObject);
begin
     if TComboBox(Sender).ItemIndex = High(MyCOlors) then begin
        CO.Color := MyColors[High(MyColors)].Value;
        if CO.Execute then begin
           MyColors[High(MyColors)].Value := CO.Color;
        end;
     end;
end;

procedure TEANSetupFmt.LinesColorChange(Sender: TObject);
begin
     EAN.LinesColor := MyColors[TComboBox(Sender).ItemIndex].Value;
end;

procedure TEANSetupFmt.BitBtn2Click(Sender: TObject);
begin
     FD.Font.Assign(EAN_FONT.Font);
     if FD.Execute then
        EAN_Font.Font.Assign(FD.Font);
end;

procedure TEANSetupFmt.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree;
end;


procedure TEANSetupFmt.SetParentEAN(E:TCustomEAN);
var i:Integer;
    C:TCheckBox;
begin
     ParentEAN            := E;

     CBTyp.ItemIndex      := Integer(E.TypBarCode);
     BarCode.Text         := E.BarCode;
     EanMemo.Visible      := false;
     BarCode.Visible      := true;
     AddUp.Checked        := E.Ean13AddUp;
     BGColor.ItemIndex    := IndexMyColor(E.BackgroundColor);
     LinesColor.ItemIndex := IndexMyColor(E.LinesColor);
     Transparent.Checked  := E.Transparent;
     FontAutoSize.Checked := E.FontAutoSize;
     EAN_FONT.Font.Assign(E.Font);

     StartStopLines.Checked  := E.StartStopLines;
     ShowLabels.Checked      := E.ShowLabels;
     SecurityZoom.Checked    := E.Security;

     UD_Angle.Position       := E.Angle;
     UD_Width.Position       := E.Width;
     UD_Height.Position      := E.Height;
     EAN_Angle.Text          := IntToStr(E.Angle);
     EAN_Width.Text          := IntToStr(E.Width);
     EAN_Height.Text         := IntToStr(E.Height);

     for i:=1 to MAX_CB do begin
         C:=TCheckBox(FindComponent('C'+IntToStr(i)));
         if C<>nil then
            C.Checked := (E.DigitVisible(i));
     end;

     AddUp.Visible  := (E.TypBarCode=bcEan13);

     CA_Visible.Checked      := E.Caption.Visible;
     GR_CAPTION.Visible      := E.Caption.Visible;
     CA_AutoSize.Checked     := E.Caption.AutoSize;
     CA_AutoCaption.Checked  := E.Caption.AutoCaption;
     CA_TEXT.Text            := E.Caption.Text;
     CA_ALIGNMENT.ItemIndex  := Integer(E.Caption.Alignment);
     CA_FONT.Font.Assign(E.Caption.Font);

     BCA_Visible.Checked      := E.CaptionBottom.Visible;
     BGR_CAPTION.Visible      := E.CaptionBottom.Visible;
     BCA_AutoSize.Checked     := E.CaptionBottom.AutoSize;
     BCA_AutoCaption.Checked  := E.CaptionBottom.AutoCaption;
     BCA_TEXT.Text            := E.CaptionBottom.Text;
     BCA_ALIGNMENT.ItemIndex  := Integer(E.CaptionBottom.Alignment);
     BCA_FONT.Font.Assign(E.CaptionBottom.Font);
     {$ifdef PSOFT_PDF417}
             PDF_ERRORLevel.ItemIndex := Integer(E.PDF417.SecurityLevel);
             PDF_MODE.ItemIndex       := Integer(E.PDF417.Mode);
             PDF_COLS.Text            := IntToStr(E.PDF417.Cols);
             PDF_ROWS.Text            := IntToStr(E.PDF417.Rows);
             PDF_TRUNCATED.Checked    := E.PDF417.Truncated;
     {$endif}

     EAN.Assign(E);
end;

procedure TEANSetupFmt.BarCodeChange(Sender: TObject);
begin
     EAN.BarCode := BarCode.Text;
     EAN.Width   := EAN.MinWidth;
end;

procedure TEANSetupFmt.BtnCancelClick(Sender: TObject);
begin
     Close;
end;

procedure TEANSetupFmt.BtnOkClick(Sender: TObject);
begin
     UpdateParentEAN;
     Close;
end;

procedure TEANSetupFmt.UpdateParentEAN;
var i:Integer;
    s:String;
    C:TCheckBox;
begin
     if ParentEAN=nil then Exit;

     ParentEAN.TypBarCode := TTypBarCode(CBTyp.ItemIndex);

     ParentEAN.BarCode        := BarCode.Text;

     ParentEAN.EAN13AddUp         := AddUp.Checked;

     ParentEAN.BackgroundColor := MyColors[BGColor.ItemIndex].Value;
     ParentEAN.LinesColor      := MyColors[LinesColor.ItemIndex].Value;
     ParentEAN.Transparent     := Transparent.Checked;
     ParentEAN.Font.Assign(EAN_FONT.Font);
     ParentEAN.FontAutoSize    := FontAutoSize.Checked;

     ParentEAN.StartStopLines := StartStopLines.Checked;
     ParentEAN.ShowLabels     := ShowLabels.Checked;
     ParentEAN.Security       := SecurityZoom.Checked;

     s:='';
     for i:=1 to MAX_CB do begin
         C:=TCheckBox(FindComponent('C'+IntToStr(i)));
         if C<>nil then begin
            if C.Checked then  s:=s+'_'
            else               s:=s+' ';
         end else
            s:=s+'_';
     end;

     ParentEAN.LabelMask := s;

     ParentEAN.Width := UD_Width.Position;
     ParentEAN.Height:= UD_Height.Position;
     ParentEAN.Angle := UD_Angle.Position;

     with ParentEAN.Caption do begin
          Visible     := CA_Visible.Checked;
          AutoCaption := CA_AutoCaption.Checked;
          AutoSize    := CA_AutoSize.Checked;
          Font.Assign(CA_FONT.FONT);
          Text        := CA_TEXT.Text;
          Alignment   := TAlignment(CA_ALIGNMENT.ItemIndex);
     end;
     with ParentEAN.CaptionBottom do begin
          Visible     := BCA_Visible.Checked;
          AutoCaption := BCA_AutoCaption.Checked;
          AutoSize    := BCA_AutoSize.Checked;
          Font.Assign(BCA_FONT.FONT);
          Text        := BCA_TEXT.Text;
          Alignment   := TAlignment(BCA_ALIGNMENT.ItemIndex);
     end;

     {$ifdef PSOFT_PDF417}
             ParentEan.PDF417.SecurityLevel   := psPDF417ErrorCorrection(PDF_ERRORLevel.ItemIndex);
             ParentEan.PDF417.Mode            := psPDF417Mode(PDF_MODE.ItemIndex);
             ParentEan.PDF417.Cols            := StrToIntDef(PDF_COLS.Text, 5);
             ParentEan.PDF417.Rows            := StrToIntDef(PDF_ROWS.Text, 0);
             ParentEan.PDF417.Truncated       := PDF_TRUNCATED.Checked;
     {$endif}

     // if ParentEan.Owner<>nil then
     //    TControl(ParentEan.Owner).Invalidate;
end;


procedure TEANSetupFmt.BtnApplyClick(Sender: TObject);
begin
     UpdateParentEAN;
end;


procedure TEANSetupFmt.CA_VisibleClick(Sender: TObject);
begin
     GR_Caption.Visible  := CA_Visible.Checked;
     BGR_Caption.Visible := BCA_Visible.Checked;
end;

procedure TEANSetupFmt.CA_AutoCaptionClick(Sender: TObject);
begin
     CA_TEXT.Enabled:= not CA_AutoCaption.Checked;
end;

procedure TEANSetupFmt.FONT_UPClick(Sender: TObject);
begin
  if Sender=FONT_UP then begin
     FD.Font.Assign(CA_FONT.Font);
     if FD.Execute then
        CA_FONT.Font.Assign(FD.Font);
  end;
  if Sender=FONT_DOWN then begin
     FD.Font.Assign(BCA_FONT.Font);
     if FD.Execute then
        BCA_FONT.Font.Assign(FD.Font);
  end;

end;

procedure TEANSetupFmt.BitBtn3Click(Sender: TObject);
begin
     if SD.Execute then
        ParentEAN.ExportToHTML(SD.FileName, gtjpeg);
end;

procedure TEANSetupFmt.SetSelectedCaption(C:TBarcodeCaption);
begin
end;

procedure TEANSetupFmt.PDF_ErrorLevelChange(Sender: TObject);
begin
     Ean.PDF417.SecurityLevel   := psPDF417ErrorCorrection(PDF_ERRORLevel.ItemIndex);
end;

procedure TEANSetupFmt.PDF_MODEChange(Sender: TObject);
begin
     Ean.PDF417.Mode := psPDF417Mode(PDF_MODE.ItemIndex);
end;

procedure TEANSetupFmt.PDF_COLSChange(Sender: TObject);
begin
     Ean.PDF417.Cols := StrToIntDef(PDF_COLS.Text, 5);
end;

procedure TEANSetupFmt.PDF_ROWSChange(Sender: TObject);
begin
     Ean.PDF417.Rows := StrToIntDef(PDF_ROWS.Text, 0);
end;

procedure TEANSetupFmt.PDF_TRUNCATEDClick(Sender: TObject);
begin
     Ean.PDF417.Truncated := PDF_TRUNCATED.Checked;
end;

end.
