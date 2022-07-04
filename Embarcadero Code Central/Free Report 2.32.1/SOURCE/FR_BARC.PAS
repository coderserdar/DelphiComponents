{*******************************************}
{                                           }
{            FastReport v2.3                }
{         Barcode Add-in object             }
{                                           }
{  Copyright (c) 1998-99 by Tzyganenko A.   }
{                                           }

//  Barcode Component
//  Version 1.3
//  Copyright 1998-99 Andreas Schmidt and friends

//  Freeware

//  for use with Delphi 2/3/4


//  this component is for private use only!
//  i am not responsible for wrong barcodes
//  Code128C not implemented

//  bug-reports, enhancements:
//  mailto:shmia@bizerba.de or
//  a_j_schmidt@rocketmail.com

{  Fr_BarC:     Guilbaud Olivier            }
{               golivier@worldnet.fr        }
{  Ported to FR2.3: Alexander Tzyganenko    }
{                                           }
{*******************************************}

unit FR_BarC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, Barcode, FR_Class, ExtCtrls, FR_Ctrls;

type
  TfrBarCodeObject = class(TComponent)  // fake component
  end;

  TfrBarCode = packed record
    cCheckSum : Boolean;
    cShowText : Boolean;
    cCadr     : Boolean;
    cBarType  : TBarcodeType;
    cModul    : Integer;
    cRatio    : Double;
    cAngle    : Double;
  end;

  TfrBarCodeView = class(TfrView)
  private
    BarC: TBarCode;
  public
    Param: TfrBarCode;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(From: TfrView); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
  end;

  TfrBarCodeForm = class(TfrObjEditorForm)
    bCancel: TButton;
    bOk: TButton;
    M1: TEdit;
    Label1: TLabel;
    cbType: TComboBox;
    Label2: TLabel;
    Image1: TImage;
    Panel1: TPanel;
    DBBtn: TfrSpeedButton;
    VarBtn: TfrSpeedButton;
    GroupBox1: TGroupBox;
    ckCheckSum: TCheckBox;
    ckViewText: TCheckBox;
    GroupBox2: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    RB4: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure VarBtnClick(Sender: TObject);
    procedure DBBtnClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

uses FR_Var, FR_Flds, FR_Const, FR_Utils;

{$R *.DFM}

var
  frBarCodeForm: TfrBarCodeForm;

const
   cbDefaultText ='12345678';
   bcNames: array[bcCode_2_5_interleaved..bcCodeEAN13, 0..1] of string =
     (('2_5_interleaved', 'A'),
      ('2_5_industrial', 'A'),
      ('2_5_matrix', 'A'),
      ('Code39', 'A'),
      ('Code39 Extended', 'A'),
      ('Code128A', 'A'),
      ('Code128B', 'A'),
      ('Code128C', 'A'),
      ('Code93', 'A'),
      ('Code93 Extended', 'A'),
      ('MSI', 'N'),
      ('PostNet', 'N'),
      ('Codebar', 'N'),
      ('EAN8', 'N'),
      ('EAN13', 'N'));


{$HINTS OFF}
function isNumeric(St: String): Boolean;
var
  R: Double;
  E: Integer;
begin
  Val(St, R, E);
  Result := (E = 0);
end;
{$HINTS ON}

constructor TfrBarCodeView.Create;
begin
  inherited Create;

  BarC := TBarCode.Create(nil);
  Param.cCheckSum := True;
  Param.cShowText := True;
  Param.cCadr     := False;
  Param.cBarType  := bcCode39;
  Param.cModul    := 1;
  Param.cRatio    := 2;
  Param.cAngle    := 0;
  Memo.Add(cbDefaultText);
  Typ := gtAddIn;
  BaseName := 'Bar';
end;

destructor TfrBarCodeView.Destroy;
begin
  BarC.Free;
  inherited Destroy;
end;

procedure TfrBarCodeView.Assign(From:TfrView);
begin
  inherited Assign(From);
  Param := (From as TfrBarCodeView).Param;
end;

procedure TfrBarCodeView.LoadFromStream(Stream:TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(Param, SizeOf(Param));
end;

procedure TfrBarCodeView.SaveToStream(Stream:TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(Param, SizeOf(Param));
end;

procedure TfrBarCodeView.Draw(Canvas:TCanvas);
var
  Txt: String;
  hg: Integer;
  EMF: TMetafile;
  EMFCanvas: TMetafileCanvas;
  h, oldh: HFont;

  function CreateRotatedFont(Font: TFont; Angle: Integer): HFont;
  var
    F: TLogFont;
  begin
    GetObject(Font.Handle, SizeOf(TLogFont), @F);
    F.lfEscapement := Angle * 10;
    F.lfOrientation := Angle * 10;
    Result := CreateFontIndirect(F);
  end;

begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);

  if Memo1.Count > 0 then
    Txt := Memo1.Strings[0] else
    Txt := cbDefaultText;

  BarC.Angle := Param.cAngle;
  BarC.Ratio := Param.cRatio;
  BarC.Modul := Param.cModul;
  BarC.Checksum := Param.cCheckSum;
  BarC.ShowText := False;
  BarC.Typ := Param.cBarType;
  if bcNames[Param.cBarType, 1] = 'A' then
    BarC.Text := Txt
  else if IsNumeric(Txt) then
    BarC.Text := Txt else
    BarC.Text := cbDefaultText;
  if (Param.cAngle = 90) or (Param.cAngle = 270) then
    dy := BarC.Width else
    dx := BarC.Width;

  if (Param.cAngle = 90) or (Param.cAngle = 270) then
    if Param.cShowText then
      hg := dx - 14 else
      hg := dx
  else if Param.cShowText then
      hg := dy - 14 else
      hg := dy;
  if Param.cAngle = 0 then
  begin
    BarC.Left := 0; BarC.Top := 0;
    BarC.Height := hg;
  end
  else if Param.cAngle = 90 then
  begin
    BarC.Left := 0; BarC.Top := dy;
    BarC.Height := hg;
  end
  else if Param.cAngle = 180 then
  begin
    BarC.Left := dx; BarC.Top := dy;
    BarC.Height := hg;
  end
  else
  begin
    BarC.Left := dx; BarC.Top := 0;
    BarC.Height := hg;
  end;

  EMF := TMetafile.Create;
  EMF.Width := dx;
  EMF.Height := dy;
  EMFCanvas := TMetafileCanvas.Create(EMF, 0);
  BarC.DrawBarcode(EMFCanvas);

  if Param.cShowText then
  with EMFCanvas do
  begin
    Font.Color := clBlack;
    Font.Name := 'Courier New';
    Font.Height := -12;
    Font.Style := [];
    h := CreateRotatedFont(Font, Round(Param.cAngle));
    oldh := SelectObject(Handle, h);
    if Param.cAngle = 0 then
      TextOut((dx - TextWidth(Txt)) div 2, dy - 12, Txt)
    else if Param.cAngle = 90 then
      TextOut(dx - 12, dy - (dy - TextWidth(Txt)) div 2, Txt)
    else if Param.cAngle = 180 then
      TextOut(dx - (dx - TextWidth(Txt)) div 2, 12, Txt)
    else
      TextOut(12, (dy - TextWidth(Txt)) div 2, Txt);
    SelectObject(Handle, oldh);
    DeleteObject(h);
  end;
  EMFCanvas.Free;

  CalcGaps;
  ShowBackground;
  Canvas.StretchDraw(DRect, EMF);
  EMF.Free;
  ShowFrame;
  RestoreCoord;
end;

procedure TfrBarCodeView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    if (Length(Memo1[0]) > 0) and (Memo1[0][1] = '[') then
      Memo1[0] := frParser.Calc(Memo1[0]);
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrBarCodeView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;


//--------------------------------------------------------------------------
procedure TfrBarCodeForm.FormCreate(Sender: TObject);
var
  i: TBarcodeType;
begin
  CbType.Items.Clear;
  for i := bcCode_2_5_interleaved to bcCodeEAN13 do
    cbType.Items.Add(bcNames[i, 0]);
  cbType.ItemIndex := 0;

  Caption := LoadStr(frRes + 650);
  Label1.Caption := LoadStr(frRes + 651);
  Label2.Caption := LoadStr(frRes + 652);
  GroupBox1.Caption := LoadStr(frRes + 653);
  ckCheckSum.Caption := LoadStr(frRes + 654);
  ckViewText.Caption := LoadStr(frRes + 655);
  DBBtn.Hint := LoadStr(frRes + 656);
  VarBtn.Hint := LoadStr(frRes + 657);
  GroupBox2.Caption := LoadStr(frRes + 658);
  bOk.Caption := LoadStr(SOk);
  bCancel.Caption := LoadStr(SCancel);
end;

procedure TfrBarCodeForm.FormActivate(Sender: TObject);
begin
  M1.SetFocus;
end;

procedure TfrBarCodeForm.ShowEditor(t:TfrView);
begin
  if t.Memo.Count > 0 then
    M1.Text := t.Memo.Strings[0];
  with t as TfrBarCodeView do
  begin
    cbType.ItemIndex   := ord(Param.cBarType);
    ckCheckSum.checked := Param.cCheckSum;
    ckViewText.Checked := Param.cShowText;
    if Param.cAngle = 0 then
      RB1.Checked := True
    else if Param.cAngle = 90 then
      RB2.Checked := True
    else if Param.cAngle = 180 then
      RB3.Checked := True
    else
      RB4.Checked := True;
    if ShowModal = mrOk then
    begin
      Memo.Clear;
      Memo.Add(M1.Text);
      Param.cCheckSum  := ckCheckSum.Checked;
      Param.cShowText  := ckViewText.Checked;
      Param.cBarType := TBarcodeType(cbType.ItemIndex);
      if RB1.Checked then
        Param.cAngle := 0
      else if RB2.Checked then
        Param.cAngle := 90
      else if RB3.Checked then
        Param.cAngle := 180
      else
        Param.cAngle := 270;
    end;
  end;
end;

procedure TfrBarCodeForm.VarBtnClick(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(nil);
  with frVarForm do
  if ShowModal = mrOk then
    if SelectedItem <> '' then
      M1.Text := '[' + SelectedItem + ']';
  frVarForm.Free;
  M1.SetFocus;
end;

procedure TfrBarCodeForm.DBBtnClick(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    if DBField <> '' then
      M1.Text := '[' + DBField + ']';
  frFieldsForm.Free;
  M1.SetFocus;
end;

procedure TfrBarCodeForm.bOkClick(Sender: TObject);
var
  bc: TBarCode;
  Bmp: TBitmap;
begin
  bc := TBarCode.Create(nil);
  bc.Text := M1.Text;
  bc.CheckSum  := ckCheckSum.Checked;
  bc.Typ := TBarcodeType(cbType.ItemIndex);
  Bmp := TBitmap.Create;
  Bmp.Width := 16; Bmp.Height := 16;
  try
    bc.DrawBarcode(Bmp.Canvas);
  except
    MessageBox(0, PChar(LoadStr(SBarcodeError)), PChar(LoadStr(SError)),
      mb_Ok + mb_IconError);
    ModalResult := 0;
  end;
  Bmp.Free;
end;


initialization
  frBarCodeForm := TfrBarCodeForm.Create(nil);
  frRegisterObject(TfrBarCodeView, frBarCodeForm.Image1.Picture.Bitmap,
    LoadStr(SInsBarcode), frBarCodeForm);

finalization
  frBarCodeForm.Free;

end.
