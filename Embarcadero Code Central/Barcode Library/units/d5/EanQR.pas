unit EanQR;

interface

{$I ean.inc}

uses SysUtils,EanKod,Classes,Graphics,Messages,
     Controls,Dialogs,EanSpecs,
     quickrpt, Qrctrls;

type
  TQrCustomEan = class(TQrPrintable)
     private
         FCharWidthMM : Double;
         FOnBeforePrint,
         FOnAfterPrint : TNotifyEvent;
         procedure   SetBarCode(Value:String);
         procedure   SetShowLabels(Value:Boolean);
         procedure   SetTransparent(Value:Boolean);
         procedure   SetStartStopLines(Value:Boolean);
         procedure   SetLinesColor(Value:TColor);
         procedure   SetBackgroundColor(Value:TColor);
         procedure   SetSecurity(Value:Boolean);
         procedure   SetDemoVersion(Value:Boolean);
         procedure   SetEan13AddUp(Value:Boolean);
         procedure   SetFontAutoSize(Value:Boolean);
         Function    GetBarCode       :String;
         Function    GetShowLabels    :Boolean;
         Function    GetTransparent   :Boolean;
         Function    GetStartStopLines:Boolean;
         Function    GetLinesColor    :TColor;
         Function    GetBackgroundColor :TColor;
         Function    GetEan13AddUp      :Boolean;
         Function    GetFontAutoSize    :Boolean;
         Function    GetSecurity        :Boolean;
         Function    GetDemoVersion     :Boolean;
         Function    GetTypBarCode      :TTypBarCode;
         Function    GetLAstPaintError  :TLastPaintError;
         Function    GetAngle           :integer;
         Procedure   SetAngle(Value:Integer);
         Function    GetLabelMask:String;
         Procedure   SetLabelMask(Value:String);
         Procedure   SetCharWidthMM(Value:Double);
         function    GetCaption:TBarcodeCaption;
         function    GetCaptionBottom:TBarcodeCaption;
         Procedure   SetAutoSize(Value:Boolean);
         Function    GetAutoSize:Boolean;
         Procedure   SetDisableEditor(Value:Boolean);
         Function    GetDisableEditor:Boolean;
         procedure   RekalkWidthMM;
         procedure   FontChange(Sender:TObject);
         procedure   SetHorzLines(Value:TBarcodeHorzLines);
         function    GetHorzLines:TBarcodeHorzLines;
         function    GetAutoCheckDigit:Boolean;
         procedure   SetAutoCheckDigit(Value:Boolean);
         procedure   DoChange(Sender:TObject);
        {$ifdef PSOFT_PDF417}
         function    GetPDF417:TpsPDF417;
        {$endif}
     protected
            FEan : TCustomEan;
            constructor Create(AOwner:TComponent); override;
            destructor  Destroy;                   override;
            procedure   LoadEanProperty(Stream:TStream);
            procedure   StoreEanProperty(Stream:TStream);
            procedure   DefineProperties(Filer: TFiler);    override;
            procedure   Paint; override;
            procedure   SetTypBarCode(Value:TTypBarCode);   virtual;
            property    DemoVersion : Boolean Read GetDemoVersion Write SetDemoVersion;
            property    OnBeforePrint : TNotifyEvent Read FOnBeforePrint Write FOnBeforePrint;
            property    OnAfterPrint  : TNotifyEvent Read FOnAfterPrint  Write FOnAfterPrint;
     public
         procedure  Next;
         function   GetSetOfChars:string;
         function   GetSetOfCharsVisible:String;
         function   CheckBarCode(var S:String):Boolean;
         procedure  Print(OfsX,OfsY:Integer);  override;
         function   LastPaintErrorText:String;
         procedure  ActiveSetupWindow;
         procedure  Copyright;
         function   DigitVisible(idx:integer):Boolean;
         procedure  DblClick; override;
         // function   EAN : TCustomEAN;
         procedure  Loaded;          override;
         property Ean : TCustomEan Read FEan stored False;
         property BackgroundColor   : TColor         Read GetBackgroundColor Write SetBackgroundColor;
         property Transparent       : Boolean        Read GetTransparent     Write SetTransparent;
         property ShowLabels        : Boolean        Read GetShowLabels      Write SetShowLabels;
         property StartStopLines    : Boolean        Read GetStartStopLines  Write SetStartStopLines;
         property TypBarCode        : TTypBarCode    Read GetTypBarCode      Write SetTypBarCode;
         property LinesColor        : TColor         Read GetLinesColor      Write SetLinesColor;
         property Ean13AddUp        : Boolean        Read GetEan13AddUp      Write SetEan13AddUp;
         property FontAutoSize      : Boolean        Read GetFontAutoSize    Write SetFontAutoSize;
         property Security          : Boolean        Read GetSecurity        Write SetSecurity;
         property Font;
         property BarCode           : string          Read GetBarCode         Write SetBarCode;
         property LastPaintError    : TLastPaintError Read GetLastPaintError;
         property Angle             : Integer         Read GetAngle           Write SetAngle;
         property LabelMask         : string          Read GetLabelMask       Write SetLabelMask;
         property CharWidthMM       : Double          Read FCharWidthMM       Write SetCharWidthMM;
         property Caption           : TBarcodeCaption Read GetCaption;
         property CaptionBottom     : TBarcodeCaption Read GetCaptionBottom;
         property AutoSize          : Boolean         Read GetAutoSize        Write SetAutoSize;
         property DisableEditor     : Boolean         Read GetDisableEditor   Write SetDisableEditor;
         property HorzLines         : TBarcodeHorzLines Read GetHorzLines Write SetHorzLines;
         property AutoCheckDigit    : Boolean           Read GetAutoCheckDigit Write SetAutoCheckDigit;
         {$ifdef PSOFT_PDF417}
                property PDF417:TpsPDF417 Read GetPDF417;
         {$endif}
     published
         property Visible;
         property OnClick;
         property OnDblClick;
         property OnDragDrop;
         property OnDragOver;
         property OnEndDrag;
         property OnMouseDown;
         property OnMouseMove;
         property OnMouseUp;
         property OnStartDrag;
     end;

  TQrEan   = class(TQrCustomEan)
     published
         property BackgroundColor;
         property Transparent;
         property ShowLabels;
         property StartStopLines;
         property TypBarCode;
         property LinesColor;
         property Ean13AddUp;
         property FontAutoSize;
         property Security;
         property DemoVersion;
         property Font;
         property LastPaintError;
         property BarCode;
         property Angle;
         property LabelMask;
         property CharWidthMM;
         property Caption;
         property CaptionBottom;
         property AutoSize;
         property DisableEditor;
         // property Size;
         property OnBeforePrint;
         property OnAfterPrint;
         property HorzLines;
         property AutoCheckDigit;
         {$ifdef PSOFT_PDF417}
                property PDF417;
         {$endif}
  end;


implementation
uses Windows, Forms,EanFmt2;


constructor TQrCustomEan.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);
     FEan          := TEan.Create(self);
     FEan.Security := False;
     FEan.PDF417.OnChange := DoChange;
     Height        := FEan.Height;
     Width         := FEan.Width;
     FCharWidthMM  := 0; //FEan.GetBarcodeInfo.OptCharWidthMM;
     Font.OnChange := FontChange;
end;

destructor  TQrCustomEan.Destroy;
begin
     inherited Destroy;
end;

procedure TQrCustomEan.DoChange(Sender:TObject);
begin
     Invalidate;
end;

procedure TQrCustomEan.RekalkWidthMM;
var SizeMM : Double;
begin
     if CharWidthMM<>0 then begin
           SizeMM:=CharWidthMM*Length(FEan.Barcode);
           case Self.ParentReport.Units of
            Characters : ;
            MM     : Size.Width := Round(SizeMM);
            Inches : Size.Width := Round(SizeMM/25.4);
            Native : Size.Width := Round(10*SizeMM);
            Pixels : ;
           end;
     end;
end;

procedure   TQrCustomEan.SetBarCode(Value:String);
begin
     if FEan.BarCode<>Value then begin
        FEan.BarCode:=Value;
        RekalkWidthMM;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetShowLabels(Value:Boolean);
begin
     if FEan.ShowLabels<>Value then begin
        FEan.ShowLabels:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetTransparent(Value:Boolean);
begin
     if FEan.Transparent<>Value then begin
        FEan.Transparent:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetStartStopLines(Value:Boolean);
begin
     if FEan.StartStopLines<>Value then begin
        FEan.StartStopLines:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetLinesColor(Value:TColor);
begin
     if FEan.LinesColor<>Value then begin
        FEan.LinesColor:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetBackgroundColor(Value:TColor);
begin
     if FEan.BackgroundColor<>Value then begin
        FEan.BackgroundColor:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetEan13AddUp(Value:Boolean);
begin
     if FEan.Ean13AddUp<>Value then begin
        FEan.Ean13AddUp:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetFontAutoSize(Value:Boolean);
begin
     if FEan.FontAutoSize<>Value then begin
        FEan.FontAutoSize:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetSecurity(Value:Boolean);
begin
     if FEan.Security<>Value then begin
        FEan.Security:=Value;
        Invalidate;
     end;
end;

procedure   TQrCustomEan.SetDemoVersion(Value:Boolean);
begin
     if FEan.DemoVersion<>Value then begin
        FEan.DemoVersion:=Value;
        Invalidate;
     end;
end;


procedure   TQrCustomEan.SetTypBarCode(Value:TTypBarCode);
var i:Integer;
begin
     if FEan.TypBarCode<>Value then begin
        FEan.TypBarCode:=Value;
        RekalkWidthMM;
        i:=fEan.MinWidth;
        if Width<i then Width := i;
        Invalidate;
     end;
end;


Function    TQrCustomEan.GetBarCode       :String;
begin
     Result:=FEan.BarCode;
end;

Function    TQrCustomEan.GetShowLabels    :Boolean;
begin
     Result:=FEan.ShowLabels;
end;

Function    TQrCustomEan.GetTransparent   :Boolean;
begin
     Result:=FEan.Transparent;
end;

Function    TQrCustomEan.GetStartStopLines:Boolean;
begin
     Result:=FEan.StartStopLines;
end;

Function    TQrCustomEan.GetLinesColor    :TColor;
begin
     Result:=FEan.LinesColor;
end;

Function    TQrCustomEan.GetBackgroundColor :TColor;
begin
     Result:=FEan.BackgroundColor;
end;

Function    TQrCustomEan.GetEan13AddUp      :Boolean;
begin
     Result:=FEan.Ean13AddUp;
end;

Function    TQrCustomEan.GetFontAutoSize  :Boolean;
begin
     Result:=FEan.FontAutoSize;
end;

Function    TQrCustomEan.GetSecurity        :Boolean;
begin
     Result:=FEan.Security;
end;

Function    TQrCustomEan.GetDemoVersion:Boolean;
begin
     Result:=FEan.DemoVersion;
end;

Function    TQrCustomEan.GetTypBarCode      :TTypBarCode;
begin
     Result:=fEan.TypBarCode;
end;


Function    TQrCustomEan.GetLastPaintError  :TLastPaintError;
begin
     Result:=fEan.LastPaintError;
end;


procedure TQrCustomEan.Paint;
begin
     RekalkWidthMM;
     PaintBarCode(Canvas,Rect(0,0,Width,Height),FEan);
end;


procedure TQrCustomEan.Next;
begin
     FEan.Next;
end;

function   TQrCustomEan.GetSetOfChars:string;
begin
     Result:=FEan.GetSetOfChars;
end;

function   TQrCustomEan.GetSetOfCharsVisible:String;
begin
     Result:=FEan.GetSetOfCharsVisible;
end;

function   TQrCustomEan.CheckBarCode(var S:String):Boolean;
begin
     Result:=FEan.CheckBarCode(S);
end;

procedure  TQrCustomEan.Print(OfsX,OfsY:Integer);
var R:TRect;

begin
     if Assigned(FOnBeforePrint) then FOnBeforePrint(Self);
     RekalkWidthMM;

     with ParentReport.QRPrinter do begin
        R.Left    := XPos(OfsX + Size.Left);
        R.Top     := YPos(OfsY + Size.Top);
        R.Right   := XPos(OfsX + Size.Left + Size.Width);
        R.Bottom  := YPos(OfsY + Size.Top  + Size.Height);
     end;

     FEan.Width  := (R.Right-R.Left);
     FEan.Height := (R.Bottom - R.Top);
     PaintBarCode(ParentReport.QRPrinter.Canvas,R,FEan);

     if Assigned(FOnAfterPrint) then FOnAfterPrint(Self);
end;

function   TQrCustomEan.LastPaintErrorText:String;
begin
     Result:=FEan.LastPaintErrorText;
end;

Function    TQrCustomEan.GetAngle:integer;
begin
     Result := FEan.Angle;
end;

Procedure   TQrCustomEan.SetAngle(Value:Integer);
begin
     FEan.Angle := Value;
     Invalidate;
end;

procedure TQRCustomEAN.ActiveSetupWindow;
begin
   if not (Parent is TEanSetupFmt) then
     with TEanSetupFmt.Create(Application) do
       Try
          SetParentEAN(FEan);
          if ShowModal=mrOK then
             FEan.Assign(EAN);
       Finally
          Free;
       End;
end;


procedure  TQRCustomEAN.Copyright;
begin
     with TEanSetupFmt.Create(Application) do
       Try
          SetParentEAN(FEan);
          ZAL.ActivePage := SH_COPYRIGHT;
          if ShowModal=mrOK then
             FEan.Assign(EAN);
       Finally
          Free;
       End;
end;


function TQRCustomEAN.DigitVisible(idx:integer):Boolean;
begin
     Result := True;
     if Length(LabelMask)>=idx then
        if LabelMask[idx]<>'_' then
        Result:=False;
end;

procedure TQRCustomEan.DblClick;
begin
     if not Assigned(OnDblClick) then ActiveSetupWindow
     else                             inherited DblClick;
end;

Function    TQRCustomEan.GetLabelMask:String;
begin
     Result := FEan.LabelMask;
end;


Procedure   TQRCustomEan.SetLabelMask(Value:String);
begin
     FEan.LabelMask := Value;
end;

Procedure   TQRCustomEan.SetCharWidthMM(Value:Double);
begin
     FCharWidthMM := Value;
     RekalkWidthMM;
end;

function    TQRCustomEan.GetCaption:TBarcodeCaption;
begin
     Result := FEan.Caption;
end;

function    TQRCustomEan.GetCaptionBottom:TBarcodeCaption;
begin
     Result := FEan.CaptionBottom;
end;

Procedure   TQRCustomEan.SetAutoSize(Value:Boolean);
var i:Integer;
begin
     FEan.AutoSize := Value;
     i:=FEan.MinWidth;
     if Width<i then
        Width := i;
end;

Function    TQRCustomEan.GetAutoSize:Boolean;
begin
     Result := FEan.AutoSize;
end;

Procedure   TQRCustomEan.SetDisableEditor(Value:Boolean);
begin
     FEan.DisableEditor:=Value;
end;

Function    TQRCustomEan.GetDisableEditor:Boolean;
begin
     Result := FEan.DisableEditor;
end;

procedure  TQRCustomEan.Loaded;
begin
     inherited Loaded;
     RekalkWidthMM;
end;

procedure  TQRCustomEan.FontChange(Sender:TObject);
begin
     FEan.Font.Assign(Font);
     Invalidate;
end;

procedure  TQRCustomEan.SetHorzLines(Value:TBarcodeHorzLines);
begin
     FEan.HorzLines.Assign(Value);
end;

function   TQRCustomEan.GetHorzLines:TBarcodeHorzLines;
begin
     Result := FEan.HorzLines;
end;

{$ifdef PSOFT_PDF417}
function    TQRCustomEan.GetPDF417:TpsPDF417;
begin
        Result := FEan.PDF417;
end;
{$endif}





procedure TQrCustomEan.LoadEanProperty(Stream:TStream);
begin
     Stream.ReadComponent(FEan);
end;

procedure TQrCustomEan.StoreEanProperty(Stream:TStream);
begin
  Stream.WriteComponent(FEan);
end;

procedure TQrCustomEan.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Ean', LoadEanProperty, StoreEanProperty, True );
end;

function    TQrCustomEan.GetAutoCheckDigit:Boolean;
begin
     Result := FEan.AutoCheckDigit;
end;

procedure   TQrCustomEan.SetAutoCheckDigit(Value:Boolean);
begin
     FEan.AutoCheckDigit := Value;
end;


end.
