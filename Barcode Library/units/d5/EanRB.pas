unit EanRB;

interface
{$I ean.inc}

uses Classes, Windows, SysUtils,Graphics, Messages,
     Controls,EanSpecs,EanKod, Printers, Dialogs,
     ppCtrls, ppDsgnCt, ppDevice, ppDrwCmd, ppTypes, ppClass;

{$I EAN.INC}

type

  TRBCustomEan = class(TppCustomComponent)
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
         Procedure   SetDisableEditor(Value:Boolean);
         Function    GetDisableEditor:Boolean;
        {$ifdef PSOFT_PDF417}
         function    GetPDF417:TpsPDF417;
        {$endif}
        procedure    DoChange(Sender:TObject);
     protected
            FEan : TCustomEan;
            constructor Create(AOwner:TComponent); override;
            destructor  Destroy;                   override;
            procedure   Loaded;                    override;
            procedure   SetTypBarCode(Value:TTypBarCode);   virtual;
            property    DemoVersion : Boolean Read GetDemoVersion Write SetDemoVersion;
            property    OnBeforePrint : TNotifyEvent Read FOnBeforePrint Write FOnBeforePrint;
            property    OnAfterPrint  : TNotifyEvent Read FOnAfterPrint  Write FOnAfterPrint;

            procedure CreatePopupMenu(aOwner: TComponent; var aPopupMenu: TppPopupMenu); override;
            procedure PaintDesignControl(aCanvas: TCanvas); override;
            procedure PropertiesToDrawCommand(aDrawCommand: TppDrawCommand); override;
            function  GetDefaultPropHint: String;
            procedure Invalidate;
            procedure PopupMenuClick(Sender: TObject); override;
            procedure SettingsMenuClick(Sender: TObject);
            procedure GetDefaultPropEnumNames(aList: TStrings); override;
     public
         procedure  Next;
         function   GetSetOfChars:string;
         function   GetSetOfCharsVisible:String;
         function   CheckBarCode(var S:String):Boolean;
         function   LastPaintErrorText:String;
         procedure  ActiveSetupWindow;
         procedure  Copyright;
         function   DigitVisible(idx:integer):Boolean;
         function   Ean : TCustomEan;
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
         property DisableEditor     : Boolean         Read GetDisableEditor   Write SetDisableEditor;
         {$ifdef PSOFT_PDF417}
                property PDF417:TpsPDF417 Read GetPDF417;
         {$endif}
     published
         property Visible;
     end;

  TRBEan   = class(TRBCustomEan)
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
         property OnBeforePrint;
         property OnAfterPrint;
         {events - inherited from TppPrintable}
         property OnDrawCommandClick;
         property OnDrawCommandCreate;
         property OnPrint;
         {$ifdef PSOFT_PDF417}
                property PDF417;
         {$endif}
  end;





implementation
uses Forms, EanFmt2 ;

constructor TRBCustomEan.Create(AOwner:TComponent);
var i:Integer;
begin
     inherited Create(AOwner);
     FEan          := TEan.Create(self);
     FEan.Security := False;
     FEan.PDF417.OnChange := DoChange;

     i:=FEan.MinWidth;
     if Width<i then Width:=i;

     Height       := FEan.Height;
     Width        := FEan.Width;
     FCharWidthMM := 0;


     // properties needed by Report Builder
     DrawCommandClass    := TppDrawImage;
     DefaultPropName     := 'TypBarCode';
     DefaultPropEditType := etValueList;

     spHeight := FEan.Height;
     spWidth  := FEan.Width;
end;

destructor  TRBCustomEan.Destroy;
begin
     FEan.Free;
     FEan := nil;
     inherited Destroy;
end;

procedure    TRBCustomEan.DoChange(Sender:TObject);
begin
     Invalidate;
end;     


function TRBCustomEan.GetDefaultPropHint: String;
begin
  Result := 'Bar code types';
end;

{------------------------------------------------------------------------------}
procedure TRBCustomEan.GetDefaultPropEnumNames(aList: TStrings);
begin
  aList.Clear;
  FEan.AddTypesToList(aList, btSymbol);
end;


procedure   TRBCustomEan.SetBarCode(Value:String);
begin
     if FEan.BarCode<>Value then begin
        FEan.BarCode:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetShowLabels(Value:Boolean);
begin
     if FEan.ShowLabels<>Value then begin
        FEan.ShowLabels:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetTransparent(Value:Boolean);
begin
     if FEan.Transparent<>Value then begin
        FEan.Transparent:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetStartStopLines(Value:Boolean);
begin
     if FEan.StartStopLines<>Value then begin
        FEan.StartStopLines:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetLinesColor(Value:TColor);
begin
     if FEan.LinesColor<>Value then begin
        FEan.LinesColor:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetBackgroundColor(Value:TColor);
begin
     if FEan.BackgroundColor<>Value then begin
        FEan.BackgroundColor:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetEan13AddUp(Value:Boolean);
begin
     if FEan.Ean13AddUp<>Value then begin
        FEan.Ean13AddUp:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetFontAutoSize(Value:Boolean);
begin
     if FEan.FontAutoSize<>Value then begin
        FEan.FontAutoSize:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetSecurity(Value:Boolean);
begin
     if FEan.Security<>Value then begin
        FEan.Security:=Value;
        Invalidate;
     end;
end;

procedure   TRBCustomEan.SetDemoVersion(Value:Boolean);
begin
     if FEan.DemoVersion<>Value then begin
        FEan.DemoVersion:=Value;
        Invalidate;
     end;
end;


procedure   TRBCustomEan.SetTypBarCode(Value:TTypBarCode);
var i:Integer;
begin
     if FEan.TypBarCode<>Value then begin
        FEan.TypBarCode:=Value;
        Invalidate;
     end;
end;


Function    TRBCustomEan.GetBarCode       :String;
begin
     Result:=FEan.BarCode;
end;

Function    TRBCustomEan.GetShowLabels    :Boolean;
begin
     Result:=FEan.ShowLabels;
end;

Function    TRBCustomEan.GetTransparent   :Boolean;
begin
     Result:=FEan.Transparent;
end;

Function    TRBCustomEan.GetStartStopLines:Boolean;
begin
     Result:=FEan.StartStopLines;
end;

Function    TRBCustomEan.GetLinesColor    :TColor;
begin
     Result:=FEan.LinesColor;
end;

Function    TRBCustomEan.GetBackgroundColor :TColor;
begin
     Result:=FEan.BackgroundColor;
end;

Function    TRBCustomEan.GetEan13AddUp      :Boolean;
begin
     Result:=FEan.Ean13AddUp;
end;

Function    TRBCustomEan.GetFontAutoSize  :Boolean;
begin
     Result:=FEan.FontAutoSize;
end;

Function    TRBCustomEan.GetSecurity        :Boolean;
begin
     Result:=FEan.Security;
end;

Function    TRBCustomEan.GetDemoVersion:Boolean;
begin
     Result:=FEan.DemoVersion;
end;

Function    TRBCustomEan.GetTypBarCode      :TTypBarCode;
begin
     Result:=fEan.TypBarCode;
end;


Function    TRBCustomEan.GetLastPaintError  :TLastPaintError;
begin
     Result:=fEan.LastPaintError;
end;


procedure TRBCustomEan.Next;
begin
     FEan.Next;
end;

function   TRBCustomEan.GetSetOfChars:string;
begin
     Result:=FEan.GetSetOfChars;
end;

function   TRBCustomEan.GetSetOfCharsVisible:String;
begin
     Result:=FEan.GetSetOfCharsVisible;
end;

function   TRBCustomEan.CheckBarCode(var S:String):Boolean;
begin
     Result:=FEan.CheckBarCode(S);
end;


function   TRBCustomEan.Ean : TCustomEan;
begin
     Result := FEan;
end;

function   TRBCustomEan.LastPaintErrorText:String;
begin
     Result:=FEan.LastPaintErrorText;
end;

Function    TRBCustomEan.GetAngle:integer;
begin
     Result := FEan.Angle;
end;

Procedure   TRBCustomEan.SetAngle(Value:Integer);
begin
     FEan.Angle := Value;
     Invalidate;
end;

procedure TRBCustomEAN.ActiveSetupWindow;
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


procedure  TRBCustomEAN.Copyright;
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


function TRBCustomEAN.DigitVisible(idx:integer):Boolean;
begin
     Result := True;
     if Length(LabelMask)>=idx then
        if LabelMask[idx]<>'_' then
        Result:=False;
end;

Function    TRBCustomEan.GetLabelMask:String;
begin
     Result := FEan.LabelMask;
end;


Procedure   TRBCustomEan.SetLabelMask(Value:String);
begin
     FEan.LabelMask := Value;
end;

Procedure   TRBCustomEan.SetCharWidthMM(Value:Double);
var s:String;
begin
     FCharWidthMM := Value;
end;

function    TRBCustomEan.GetCaption:TBarcodeCaption;
begin
     Result := FEan.Caption;
end;

function    TRBCustomEan.GetCaptionBottom:TBarcodeCaption;
begin
     Result := FEan.CaptionBottom;
end;

procedure   TRBCustomEan.SetDisableEditor(Value:Boolean);
begin
     FEan.DisableEditor:=Value;
end;

Function    TRBCustomEan.GetDisableEditor:Boolean;
begin
     Result := FEan.DisableEditor;
end;

procedure  TRBCustomEan.Loaded;
begin
     inherited Loaded;
     Invalidate;
end;

procedure TRBCustomEan.Invalidate;
begin
  {notify report designer}
  PropertyChange;
  {notify report engine}
  Reset;
  {redraw design control}
  InvalidateDesignControl;
end;


procedure TRBCustomEan.CreatePopupMenu(aOwner: TComponent; var aPopupMenu: TppPopupMenu);
begin
  inherited CreatePopupMenu(aOwner, aPopupMenu);
  aPopupMenu.AddItem(80, 'Line2', '-',  0);
  aPopupMenu.AddItem(90, 'Settings', 'Settings',  0);
end;

procedure TRBCustomEan.PopupMenuClick(Sender: TObject);
begin
  TppPopupMenu(Sender).ItemByName('Settings').OnClick := SettingsMenuClick;
end;

procedure TRBCustomEan.PaintDesignControl(aCanvas: TCanvas);
begin
     PaintBarCode(aCanvas,spClientRect, FEan);
end;

procedure TRBCustomEan.PropertiesToDrawCommand(aDrawCommand: TppDrawCommand);
var lDrawImage : TppDrawImage;
    Bitmap     : TBitmap;
    R          : TRect;
    dpix, dpiy : Integer;
begin
  inherited PropertiesToDrawCommand(aDrawCommand);

  if not(aDrawCommand is TppDrawImage) then Exit;

  if Assigned(FOnBeforePrint) then
     FOnBeforePrint(Self);

  lDrawImage := TppDrawImage(aDrawCommand);
  Bitmap := TBitmap.Create;
  try
         lDrawImage.Left         := PrintPosRect.Left;
         lDrawImage.Top          := PrintPosRect.Top;
         lDrawImage.Height       := PrintPosRect.Bottom - PrintPosRect.Top;
         lDrawImage.Width        := PrintPosRect.Right - PrintPosRect.Left;

         dpix  := GetDeviceCaps(Printer.handle, LOGPIXELSX);
         dpiy  := GetDeviceCaps(Printer.handle, LOGPIXELSY);

         R := Rect(0,0, MulDiv(mmWidth,  dpiy, 25400), MulDiv(mmHeight, dpiy, 25400));

         Bitmap.PixelFormat := pf4Bit;
         Bitmap.Width  := R.Right;
         Bitmap.Height := R.Bottom;

         if IsdataAware then
            if (DataPipeline <> nil) then
               FEan.Barcode := DataPipeline.GetFieldAsString(DataField);

         PaintBarCode(Bitmap.Canvas, R, FEan);

         lDrawImage.Stretch := True;
         lDrawImage.DataType := dtGraphic;
         lDrawImage.Picture.Bitmap.Assign(Bitmap);
  finally
         Bitmap.Free;
  end;

  if Assigned(FOnAfterPrint) then
     FOnAfterPrint(Self);
end;


procedure TRBCustomEan.SettingsMenuClick(Sender: TObject);
begin
     FEan.ActiveSetupWindow('');
     Invalidate;
end;


{$ifdef PSOFT_PDF417}
function    TRBCustomEan.GetPDF417:TpsPDF417;
begin
        Result := FEan.PDF417;
end;
{$endif}




initialization
  ppRegisterComponent(TRBEan,     'Ean components', 10, 0, 'RbEan', HInstance);

finalization

  ppUnRegisterComponent(TRbEan);

end.
