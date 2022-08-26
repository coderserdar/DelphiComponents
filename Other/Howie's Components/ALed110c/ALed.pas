unit ALed;

{ Original by Chang-Ting SU.  E-mail:ctsu@ms12.hinet.net }

{ Modified by H.J. Harvey     E-mail:hharvey@dove.net.au }
{ Version 1.10c  24/MAY/99 }

{ Now provides 6 different LED styles:
      Large Round (the original)
      Small Round
      Large Square
      Small Square
      Vertical Rect
      Horizontal Rect }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TLEDStyle = (LEDSmall,LEDLarge,LEDSqSmall,LEDSqLarge,LEDVertical,LEDHorizontal) ;
  ThhALed = class(TGraphicControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    fLedBitmap    : Array[0..1] of TBITMAP;
    fLedTimer     : TTimer;
    fTrueColor    : TColor;
    fFalseColor   : TColor;
    fBordColor    : TColor;
    fBordered     : Boolean;
    fBlink        : Boolean;
    fLEDStyle     : TLEDStyle;
    fInterval     : longint;
    fValue        : Boolean;
    ColorTemp     : Boolean;
    fOnTimer      : TNotifyEvent;
    fOnMouseEnter : TNotifyEvent;
    fOnMouseLeave : TNotifyEvent;
    procedure Paint;override;
    procedure OnLedTimer(Sender : TObject);
    procedure CMMouseEnter(var Message:TMessage);message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message:TMessage);message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure CreateLedBitmap;
    procedure FreeLedBitmap;
    procedure ChangeValue(V : Boolean);
    procedure ChangeBlink(V : Boolean);
    procedure ChangeBorder(V : Boolean);
    procedure ChangeStyle(V : TLEDStyle);
    procedure SetTrueColor(V : TColor);
    procedure SetFalseColor(V : TColor);
    procedure SetBordColor(V : TColor);
    procedure SetInterval(V : longint);
    procedure SetToTrueColor;
    procedure SetToFalseColor;
    procedure SetLedTimer;
    procedure ResetLedTimer;
  published
    { Published declarations }
    property TrueColor  : TColor  read fTrueColor write SetTrueColor default clLime;
    property FalseColor : TColor  read fFalseColor write SetFalseColor default clSilver;
    property BorderColor: TColor  read fBordColor write SetBordColor default clBtnFace;
    property Blink      : Boolean read fBlink write ChangeBlink default true;
    property Bordered   : Boolean read fBordered write ChangeBorder default true;
    property Value      : Boolean read fValue write ChangeValue default false;
    property Interval   : longint read fInterval write SetInterval default 1000;
    property LEDStyle   : TLEDStyle read fLEDStyle write ChangeStyle default LEDSmall;
    property OnTimer    : TNotifyEvent read fOnTimer write fOnTimer;
    property OnMouseEnter : TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnClick;
    property ShowHint;
  end;

procedure Register;

implementation
{$R ALed.res}

constructor ThhALed.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Height      := 16;
  Width       := 16;
  fTrueColor  := clLime;
  fFalseColor := clSilver;
  fBordColor  := clBtnFace;
  fBlink      := true;
  fBordered   := true;
  fValue      := false;
  fLEDStyle   := LEDSmall ;
  fInterval   := 1000;
  fLedTimer   := nil;
  fLedBitmap[0] := nil;
  fLedBitmap[1] := nil;
  ColorTemp   := true;
  CreateLedBitmap;
end;

destructor ThhALed.Destroy;
begin
  ResetLedTimer;
  FreeLedBitmap;
  inherited;
end;

procedure ThhALed.CreateLedBitmap;
var
  BitmapName : Pchar ;
  Xloc : integer ;
begin
  FreeLedBitmap;
  fLedBitmap[0] := TBitmap.Create;
  fLedBitmap[1] := TBitmap.Create;
  case fLEDStyle of
  LEDLarge:
    begin
      Width := 22 ;
      Height := 22 ;
      if fBordered then BitmapName := 'BLG' else BitmapName := 'NLG' ;
    end;
  LEDSmall:
    begin
      Width := 16 ;
      Height := 16 ;
      if fBordered then BitmapName := 'BSM' else BitmapName := 'NSM' ;
    end ;
  LEDSqLarge:
    begin
      Width := 22 ;
      Height := 22 ;
      if fBordered then BitmapName := 'BSQLG' else BitmapName := 'NSQLG' ;
    end;
  LEDSqSmall:
    begin
      Width := 16 ;
      Height := 16 ;
      if fBordered then BitmapName := 'BSQSM' else BitmapName := 'NSQSM' ;
    end;
  LEDHorizontal:
    begin
      Width := 22 ;
      Height := 16 ;
      if fBordered then BitmapName := 'BHZ' else BitmapName := 'NHZ' ;
    end;
  LEDVertical:
    begin
      Width := 16 ;
      Height := 22 ;
      if fBordered then BitmapName := 'BVT' else BitmapName := 'NVT' ;
    end;
  end ;
  fLedBitmap[0].Handle := LoadImage(HINSTANCE,BitmapName,IMAGE_BITMAP,0,0,0);
  fLedBitmap[1].Handle := LoadImage(HINSTANCE,BitmapName,IMAGE_BITMAP,0,0,0);
  fLedBitmap[0].Canvas.Brush.Color := fTrueColor;
  fLedBitmap[0].Canvas.FloodFill(Width DIV 2,Height DIV 2, clLime, fsSurface);
  fLedBitmap[1].Canvas.Brush.Color := fFalseColor;
  fLedBitmap[1].Canvas.FloodFill(Width DIV 2, Height DIV 2, clLime, fsSurface);
  if (not fBordered)
  then begin
    Xloc := 0 ;
    fLedBitmap[0].Canvas.Brush.Color := fBordColor;
    fLedBitmap[0].Canvas.FloodFill(Xloc, Xloc, clSilver, fsSurface);
    fLedBitmap[1].Canvas.Brush.Color := fBordColor;
    fLedBitmap[1].Canvas.FloodFill(Xloc, Xloc, clSilver, fsSurface);
  end ;
end;

procedure ThhALed.FreeLedBitmap;
begin
  if (Assigned(fLedBitmap[0])) then fLedBitmap[0].Destroy;
  if (Assigned(fLedBitmap[1])) then fLedBitmap[1].Destroy;
  fLedBitmap[0] := nil;
  fLedBitmap[1] := nil;
end;

procedure ThhALed.Paint;
begin
  Canvas.StretchDraw(Rect(0,0,Width,Height),fLedBitmap[integer(ColorTemp)]);
end;

procedure ThhALed.OnLedTimer(Sender: TObject);
begin
  ColorTemp := not ColorTemp;
  Canvas.StretchDraw(Rect(0,0,Width,Height),fLedBitmap[integer(ColorTemp)]);
  if (Assigned(OnTimer)) then fOnTimer(Self);
end;

procedure ThhALed.SetToTrueColor;
begin
  Canvas.StretchDraw(Rect(0,0,Width,Height),fLedBitmap[0]);
  ColorTemp := false;
end;

procedure ThhALed.SetToFalseColor;
begin
  Canvas.StretchDraw(Rect(0,0,Width,Height),fLedBitmap[1]);
  ColorTemp := true;
end;

procedure ThhALed.SetLedTimer;
begin
  if (Assigned(fLedTimer)) then Exit;
  if (csDesigning in ComponentState) then Exit;
  ColorTemp := false;
  fLedTimer := TTimer.Create(Self);
  fLedTimer.Interval := fInterval;
  fLedTimer.OnTimer  := OnLedTimer;
end;

procedure ThhALed.ResetLedTimer;
begin
  if (Assigned(fLedTimer)) then begin
    fLedTimer.Destroy;
    fLedTimer := nil;
  end;
end;

procedure ThhALed.ChangeStyle(V : TLEDStyle);
begin
  if (fLEDStyle <> V) then begin
    fLEDStyle := V ;
    CreateLedBitmap;
    Repaint;
  end;
end;

procedure ThhALed.ChangeBorder(V : Boolean);
begin
  if (fBordered <> V) then begin
    fBordered := V ;
    CreateLedBitmap;
    Repaint;
  end;
end;

procedure ThhALed.ChangeValue(V : Boolean);
begin
  if (fValue <> V)
  then begin
    fValue := V;
    if (fValue)
    then begin
      SetToTrueColor;
      if (fBlink)
      then
        SetLedTimer;
    end
    else begin
      ResetLedTimer;
      SetToFalseColor;
    end
       (* fValue == true or false *)
  end; (*(fValue <> V)*)
end;

procedure ThhALed.ChangeBlink(V : Boolean);
begin
  if (fBlink <> V)
  then begin
    if (fValue)
    then
      SetToTrueColor
    else
      SetToFalseColor;
      fBlink := V;
      if (V and fValue)
      then
        SetLedTimer
      else
        ResetLedTimer
  end;
end;

procedure ThhALed.SetTrueColor(V : TColor);
var
  Temp : Boolean;
begin
  if (fTrueColor <> V)
  then begin
    Temp := fBlink;
    if (fBlink) then Blink := false;
    fTrueColor := V;
    CreateLedBitmap;
    Blink := Temp;
    Repaint;
  end
end;

procedure ThhALed.SetFalseColor(V : TColor);
var
  Temp : Boolean;
begin
  if (fFalseColor <> V)
  then begin
    Temp := fBlink;
    if (fBlink) then Blink := false;
    fFalseColor := V;
    CreateLedBitmap;
    Blink := Temp;
    Repaint;
  end
end;

procedure ThhALed.SetBordColor(V : TColor);
var
  Temp : Boolean;
begin
  if (fBordColor <> V)
  then begin
    Temp := fBlink;
    if (fBlink) then Blink := false;
    fBordColor := V;
    CreateLedBitmap;
    Blink := Temp;
    Repaint;
  end
end;

procedure ThhALed.SetInterval(V : longint);
begin
  fInterval := V;
  if (Assigned(fLedTimer)) then
    fLedTimer.Interval := V;
end;

procedure ThhALed.CmMouseEnter(var Message:TMessage);
begin
  inherited;
  if (Assigned(fOnMouseEnter)) then fOnMouseEnter(Self);
end;

procedure ThhALed.CmMouseLeave(var Message:TMessage);
begin
  inherited;
  if (Assigned(fOnMouseLeave)) then fOnMouseLeave(Self);
end;

procedure Register;
begin
  RegisterComponents('Howie', [ThhALed]);
end;

end.
