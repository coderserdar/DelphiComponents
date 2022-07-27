{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmClock
Purpose  : Visual UI eye-candy type component.
Date     : 11-21-2002
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmClock;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, math, rmlibrary,
  types;

type
  TClockBorder = (cbNone, cbSingle, cbRaised, cbLowered);
  TrmCustomClock = class(TGraphicControl)
  private
    { Private declarations }
    fBorder : TClockBorder;
    fFaceColor: TColor;
    fTime: TTime;

    fFaceDiameter : integer;
    fHandColor: TColor;

    procedure SetBorder(Value : TClockBorder);
    procedure SetFaceColor(const Value: TColor);
    procedure SetTime(const Value: TTime);
    procedure SetFaceSize(const Value: integer);
    procedure SetHandColor(const Value: TColor);
    procedure wmEraseBkgnd(var MSG : TMessage); message wm_EraseBkgnd;
  protected
    { Protected declarations }
    procedure paint; override;
    procedure GetBorderColors(var TopColor, BottomColor:TColor);
    property Border : TClockBorder read fborder write SetBorder default cbsingle;
    property FaceColor : TColor read fFaceColor write SetFaceColor default clSilver;
    property FaceSize : integer read fFaceDiameter write SetFaceSize default 100;
    property HandsColor : TColor read fHandColor write SetHandColor default clNavy;
    property Time : TTime read fTime write SetTime;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
  published
    { Published declarations }
  end;

  TrmClock = class(TrmCustomClock)
  published
    property Border;
    property FaceColor;
    property FaceSize;
    property Time;
  end;

implementation

{ TrmCustomClock }

constructor TrmCustomClock.Create(AOwner:TComponent);
begin
     inherited create(AOwner);
     ControlStyle := ControlStyle + [csopaque];
     width := 100;
     height := 100;
     fborder := cbSingle;
     fFaceColor := clSilver;
     fFaceDiameter := 100;
     fHandColor := clNavy;
     fTime := now;
end;

procedure TrmCustomClock.paint;
var
   bmp : TBitMap;
   topColor, bottomcolor : TColor;
   angle : double;
   h, m, s, ms : word;
   wLineSize : extended;
   loop : integer;
   CurPt : TPoint;
   TmpRect : TRect;

begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clBtnFace;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     DecodeTime(fTime, h, m, s, ms);

     GetBorderColors(TopColor,BottomColor);
     with bmp.canvas do
     begin
       Brush.Color := fFaceColor;
       pen.color := fFaceColor;
       Ellipse(0, 0, fFaceDiameter-1, fFaceDiameter - 1);

       loop := 0;
       brush.color := clWindowFrame;
       while loop < 60 do
       begin
            Angle := (2 * Pi * loop / 60);

            CurPt := Point(
                       trunc(  ((fFaceDiameter DIV 2)-((fFaceDiameter div 2) div 10)) * sin( Angle)) + (fFacediameter div 2),
                       trunc( -((fFaceDiameter DIV 2)-((fFaceDiameter div 2) div 10)) * cos( Angle)) + (fFacediameter div 2));

            TmpRect := Rect( CurPt.X-1, CurPt.Y-1, CurPt.X+1, CurPt.Y+1);

            framerect(tmprect);

            inc(loop, 5);
       end;

       //Minute hand
       Pen.Width := 3;
       Pen.Color := fHandColor;
       Angle := (2 * Pi * m / 60);
       MoveTo( (fFaceDiameter div 2), (fFaceDiameter div 2));
       wLineSize := 0.90 * (fFaceDiameter div 2);
       LineTo( trunc(  wLineSize * sin( Angle)) + (fFaceDiameter div 2),
               trunc( -wLineSize * cos( Angle)) + (fFaceDiameter div 2));

       //Hour hand
       Angle := (H MOD 12) * 2 * Pi / 12 + Angle / 12;
       MoveTo( (fFaceDiameter div 2), (fFaceDiameter div 2));
       wLineSize := 0.65 * (fFaceDiameter div 2);
       LineTo( trunc(  wLineSize * sin( Angle)) + (fFaceDiameter div 2),
               trunc( -wLineSize * cos( Angle)) + (fFaceDiameter div 2));

       if border <> cbNone then
       begin
            Pen.Width := 1;
            Pen.Color :=  TopColor;
            Arc (0, 0, fFaceDiameter-1, fFaceDiameter-1, // ellipse
                 fFaceDiameter-1, 0, // start
                 0, fFaceDiameter-1); // end

            Pen.Color :=  BottomColor;
            Arc (0, 0, fFaceDiameter-1, fFaceDiameter-1, // ellipse
                 0, fFaceDiameter-1, // start
                 fFaceDiameter, 0); // end
       end;
     end;
     bitblt(canvas.handle,0,0,width-1,height-1,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmCustomClock.GetBorderColors(var TopColor, BottomColor:TColor);
begin
     case border of
          cbSingle:begin
                        topColor := clWindowFrame;
                        bottomcolor := topcolor;
                   end;
          cbRaised:begin
                        topcolor := clbtnhighlight;
                        bottomcolor := clbtnshadow;
                   end;
          cbLowered:begin
                         topcolor := clbtnshadow;
                         bottomcolor := clbtnhighlight;
                    end;
          else
          begin
               topcolor := clbtnface;
               bottomcolor := topcolor;
          end;
     end;
end;

procedure TrmCustomClock.SetBorder(Value : TClockBorder);
begin
     if fborder <> value then
     begin
          fborder := value;
          invalidate;
     end;
end;

procedure TrmCustomClock.SetFaceColor(Const Value : TColor);
begin
   if fFaceColor <> value then
   begin
     fFaceColor := Value;
     invalidate;
   end;
end;

procedure TrmCustomClock.SetTime(const Value: TTime);
begin
   if ftime <> value then
   begin
     fTime := Value;
     Invalidate;
   end;
end;

procedure TrmCustomClock.SetFaceSize(const Value: integer);
begin
  if facesize <> value then
  begin
     fFaceDiameter := Value;
     invalidate;
  end;
end;

procedure TrmCustomClock.SetHandColor(const Value: TColor);
begin
  if fHandColor <> Value then
  begin
    fHandColor := Value;
    invalidate;
  end;
end;

procedure TrmCustomClock.wmEraseBkgnd(var MSG: TMessage);
begin
  msg.Result := 1;
end;

end.
