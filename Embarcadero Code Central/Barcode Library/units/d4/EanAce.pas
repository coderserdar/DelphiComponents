unit EanAce;

interface

uses Classes, Windows, SysUtils,EanKod, Graphics, Messages,
     Controls,Dialogs,EanSpecs, SctCtrl, AceOut;

{$I EAN.INC}

type
  TAceCustomEan = class(TSctLabel)
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
        {$ifdef PSOFT_PDF417}
         function    GetPDF417:TpsPDF417;
        {$endif}
         procedure   DoChange(Sender:TObject);
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
         // procedure  Print(OfsX,OfsY:Integer);  override;
         procedure  PrintLabel( AceCanvas: TAceCanvas; Rect: TRect; Space: Integer); override;
         function   LastPaintErrorText:String;
         procedure  ActiveSetupWindow;
         procedure  Copyright;
         function   DigitVisible(idx:integer):Boolean;
         function   Ean : TCustomEan;
         procedure  DblClick; override;
         procedure  Loaded;          override;
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

  TAceEan   = class(TAceCustomEan)
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
         {$ifdef PSOFT_PDF417}
                property PDF417;
         {$endif}
  end;






implementation
uses Forms, EanFmt2 ;

constructor TAceCustomEan.Create(AOwner:TComponent);
var i:Integer;
begin
     inherited Create(AOwner);
     FEan          := TEan.Create(self);
     FEan.Security := False;
     FEan.PDF417.OnChange := DoChange;

     i:=FEan.MinWidth;
     if Width<i then Width:=i;

     Height := FEan.Height;
     Width  := FEan.Width;
     FCharWidthMM := 0;
end;

destructor  TAceCustomEan.Destroy;
begin
     inherited Destroy;
end;

procedure TAceCustomEan.LoadEanProperty(Stream:TStream);
begin
     Stream.ReadComponent(FEan);
end;

procedure TAceCustomEan.StoreEanProperty(Stream:TStream);
begin
  Stream.WriteComponent(FEan);
end;

procedure TAceCustomEan.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Ean', LoadEanProperty, StoreEanProperty, True );
end;


{$ifdef PSOFT_PDF417}
function    TAceCustomEan.GetPDF417:TpsPDF417;
begin
        Result := FEan.PDF417;
end;
{$endif}


procedure TAceCustomEan.RekalkWidthMM;
var SizeMM : Double;
begin{
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
     }
end;

procedure   TAceCustomEan.SetBarCode(Value:String);
begin
     if FEan.BarCode<>Value then begin
        FEan.BarCode:=Value;
        RekalkWidthMM;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetShowLabels(Value:Boolean);
begin
     if FEan.ShowLabels<>Value then begin
        FEan.ShowLabels:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetTransparent(Value:Boolean);
begin
     if FEan.Transparent<>Value then begin
        FEan.Transparent:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetStartStopLines(Value:Boolean);
begin
     if FEan.StartStopLines<>Value then begin
        FEan.StartStopLines:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetLinesColor(Value:TColor);
begin
     if FEan.LinesColor<>Value then begin
        FEan.LinesColor:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetBackgroundColor(Value:TColor);
begin
     if FEan.BackgroundColor<>Value then begin
        FEan.BackgroundColor:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetEan13AddUp(Value:Boolean);
begin
     if FEan.Ean13AddUp<>Value then begin
        FEan.Ean13AddUp:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetFontAutoSize(Value:Boolean);
begin
     if FEan.FontAutoSize<>Value then begin
        FEan.FontAutoSize:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetSecurity(Value:Boolean);
begin
     if FEan.Security<>Value then begin
        FEan.Security:=Value;
        Invalidate;
     end;
end;

procedure   TAceCustomEan.SetDemoVersion(Value:Boolean);
begin
     if FEan.DemoVersion<>Value then begin
        FEan.DemoVersion:=Value;
        Invalidate;
     end;
end;


procedure   TAceCustomEan.SetTypBarCode(Value:TTypBarCode);
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


Function    TAceCustomEan.GetBarCode       :String;
begin
     Result:=FEan.BarCode;
end;

Function    TAceCustomEan.GetShowLabels    :Boolean;
begin
     Result:=FEan.ShowLabels;
end;

Function    TAceCustomEan.GetTransparent   :Boolean;
begin
     Result:=FEan.Transparent;
end;

Function    TAceCustomEan.GetStartStopLines:Boolean;
begin
     Result:=FEan.StartStopLines;
end;

Function    TAceCustomEan.GetLinesColor    :TColor;
begin
     Result:=FEan.LinesColor;
end;

Function    TAceCustomEan.GetBackgroundColor :TColor;
begin
     Result:=FEan.BackgroundColor;
end;

Function    TAceCustomEan.GetEan13AddUp      :Boolean;
begin
     Result:=FEan.Ean13AddUp;
end;

Function    TAceCustomEan.GetFontAutoSize  :Boolean;
begin
     Result:=FEan.FontAutoSize;
end;

Function    TAceCustomEan.GetSecurity        :Boolean;
begin
     Result:=FEan.Security;
end;

Function    TAceCustomEan.GetDemoVersion:Boolean;
begin
     Result:=FEan.DemoVersion;
end;

Function    TAceCustomEan.GetTypBarCode      :TTypBarCode;
begin
     Result:=fEan.TypBarCode;
end;


Function    TAceCustomEan.GetLastPaintError  :TLastPaintError;
begin
     Result:=fEan.LastPaintError;
end;


procedure TAceCustomEan.Paint;
begin
     RekalkWidthMM;
     PaintBarCode(Canvas,Rect(0,0,Width,Height),FEan);
end;


procedure TAceCustomEan.Next;
begin
     FEan.Next;
end;

function   TAceCustomEan.GetSetOfChars:string;
begin
     Result:=FEan.GetSetOfChars;
end;

function   TAceCustomEan.GetSetOfCharsVisible:String;
begin
     Result:=FEan.GetSetOfCharsVisible;
end;

function   TAceCustomEan.CheckBarCode(var S:String):Boolean;
begin
     Result:=FEan.CheckBarCode(S);
end;

procedure TAceCustomEan.PrintLabel( AceCanvas: TAceCanvas; Rect: TRect; Space: Integer);
var R      : TRect;
    Bitmap : TBitmap;
begin
     if Assigned(FOnBeforePrint) then FOnBeforePrint(Self);
     RekalkWidthMM;
     bitmap:=TBitmap.Create;
     try
           Bitmap.PixelFormat := pf4Bit;
           Bitmap.Height      := Height;
           Bitmap.Width       := FEan.MinWidth;
           R.Left := 0;
           R.Top  := 0;
           R.Right  := Bitmap.Width;
           R.Bottom := Rect.Bottom - Rect.Top;
           PaintBarCode(Bitmap.Canvas, R,FEan);
           AceCanvas.Draw(Rect.Left, Rect.Top, Bitmap);
     finally
           Bitmap.Free;
     end;
     if Assigned(FOnAfterPrint) then FOnAfterPrint(Self);
end;



function   TAceCustomEan.Ean : TCustomEan;
begin
     Result := FEan;
end;

function   TAceCustomEan.LastPaintErrorText:String;
begin
     Result:=FEan.LastPaintErrorText;
end;

Function    TAceCustomEan.GetAngle:integer;
begin
     Result := FEan.Angle;
end;

Procedure   TAceCustomEan.SetAngle(Value:Integer);
begin
     FEan.Angle := Value;
     Invalidate;
end;

procedure TAceCustomEAN.ActiveSetupWindow;
begin
     FEan.ActiveSetupWindow('');
end;


procedure  TAceCustomEAN.Copyright;
begin
     FEan.Copyright;
end;


function TAceCustomEAN.DigitVisible(idx:integer):Boolean;
begin
     Result := True;
     if Length(LabelMask)>=idx then
        if LabelMask[idx]<>'_' then
        Result:=False;
end;

procedure TAceCustomEan.DblClick;
begin
     if not Assigned(OnDblClick) then ActiveSetupWindow
     else                             inherited DblClick;
end;

Function    TAceCustomEan.GetLabelMask:String;
begin
     Result := FEan.LabelMask;
end;


Procedure   TAceCustomEan.SetLabelMask(Value:String);
begin
     FEan.LabelMask := Value;
end;

Procedure   TAceCustomEan.SetCharWidthMM(Value:Double);
var s:String;
begin
     FCharWidthMM := Value;
     RekalkWidthMM;
end;

function    TAceCustomEan.GetCaption:TBarcodeCaption;
begin
     Result := FEan.Caption;
end;

function    TAceCustomEan.GetCaptionBottom:TBarcodeCaption;
begin
     Result := FEan.CaptionBottom;
end;

Procedure   TAceCustomEan.SetAutoSize(Value:Boolean);
var i:Integer;
begin
     FEan.AutoSize := Value;
     i:=FEan.MinWidth;
     if Width<i then
        Width := i;
end;

Function    TAceCustomEan.GetAutoSize:Boolean;
begin
     Result := FEan.AutoSize;
end;

Procedure   TAceCustomEan.SetDisableEditor(Value:Boolean);
begin
     FEan.DisableEditor:=Value;
end;

Function    TAceCustomEan.GetDisableEditor:Boolean;
begin
     Result := FEan.DisableEditor;
end;

procedure  TAceCustomEan.Loaded;
begin
     inherited Loaded;
     RekalkWidthMM;
end;


procedure   TAceCustomEan.DoChange(Sender:TObject);
begin
     Invalidate;
end;



end.
