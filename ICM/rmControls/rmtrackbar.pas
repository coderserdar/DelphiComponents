{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTrackBar
Purpose  : An enhanced Trackbar allowing for multiple new styles
Date     : 12-01-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmTrackBar;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, extctrls,
  rmLibrary;

type
  TMarkPosition = (mpTopLeft, mpBottomRight, mpNone);
  TTrackOrientation = (toHorizontal, toVertical);
  TTrackPosition = (tpCenter, tpTopLeft, tpBottomRight);
  TrmTrackBar = class(TCustomControl)
  private
    { Private declarations }
    fTrackOrientation : TTrackOrientation;
    fTrackSize : integer;
    fTrackColor : TColor;
    fMinValue : integer;
    fMaxValue : integer;
    fThumbPosition : integer;
    fTrackPosition : TTrackPosition;
    fPageSize : integer;
    fMarkFrequency : integer;
    fMarkSpacing : integer;
    fMarkPosition : TMarkPosition;
    fThumb : tbitmap;
    fChanged : TNotifyEvent;
    fMarkData : TStrings;
    fmouseon : boolean;
    fShowFocus: boolean;
    fShowMarks: boolean;
    function ptinthumb(x,y:integer):boolean;
    function ptintrack(x,y:integer):boolean;
    procedure SetTrackOrientation(value:TTrackOrientation);
    procedure SetTrackSize(value:integer);
    procedure SetTrackColor(value:TColor);
    procedure SetMinValue(value:integer);
    procedure SetMaxValue(value:integer);
    procedure SetThumbPos(value:integer);
    procedure SetTrackPosition(value:TTrackPosition);
    procedure SetThumb(value:tbitmap);
    procedure SetMarkData(value:TStrings);
    procedure setmarkfrequency(value:integer);
    procedure SetMarkPosition(value:TMarkPosition);
    procedure SetPagesize(value:integer);
    procedure SetMarkSpacing(value:integer);
    function GetTrackRect:TRect;
    function PointPosition(x,y:integer):integer;
    procedure wmSetFocus(var msg:TWMSetFocus); message wm_setfocus;
    procedure wmKillFocus(var msg:TWMKillFocus); message wm_killfocus;
    procedure wmEraseBkGnd(var msg:TWMEraseBkgnd); message wm_erasebkgnd;
    procedure wmGetDLGCode(var msg:TWMGetDLGCode); message wm_GetDLGCode;
    procedure wmMouseActivate(var msg:TWMMouseActivate); message wm_MouseActivate;
    procedure SetshowFocus(const Value: boolean);
    procedure SetShowMarks(const Value: boolean);
  protected
    { Protected declarations }
    procedure paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    constructor create(aowner:TComponent); override;
    destructor destroy; override;
  published
    { Published declarations }
    property Align;
    property Color;
    property Font;
    property ShowFocus : boolean read fShowFocus write SetshowFocus default true;
    property MarkFrequency:integer read fmarkfrequency write setmarkfrequency default 1;
    property MarkPosition : TMarkPosition read fMarkPosition write SetMarkPosition default mpTopLeft;
    property MarkSpacing : integer read fmarkspacing write setmarkspacing default 2;
    property MaxValue : integer read fMaxValue write SetMaxValue default 10;
    property MinValue : integer read fminvalue write SetMinValue default 0;
    property PageSize:integer read fpagesize write setpagesize default 2;
    property ShowMarks : boolean read fShowMarks write SetShowMarks default true;
    property TabStop;
    property TabOrder;
    property Thumb : tbitmap read fthumb write SetThumb stored true;
    property ThumbPosition : integer read fThumbPosition write SetThumbPos default 0;
    property TrackColor : TColor read ftrackcolor write SetTrackColor default clWindow;
    property TrackOrientation : TTrackOrientation read fTrackOrientation write SetTrackOrientation default toHorizontal;
    property TrackPosition : TTrackPosition read ftrackposition write SetTrackPosition default tpcenter;
    property TrackSize : integer read ftracksize write SetTrackSize default 8;
    property OnChange:TNotifyEvent read fchanged write fchanged;
    property MarkData : TStrings read fMarkData write SetMarkData stored true;
  end;

implementation

{ TrmTrackBar }
{$R rmTrackBar.res}

constructor TrmTrackBar.create(aowner:TComponent);
begin
     inherited create(aowner);
     fmouseon := false;
     ControlStyle := [csCaptureMouse,csClickEvents];
     fThumb := tbitmap.create;
     fMarkData := TStringList.Create;
     height := 50;
     width := 150;
     fmarkspacing := 2;
     fTrackOrientation := toHorizontal;
     fMarkPosition := mpTopLeft;
     ftracksize := 8;
     ftrackcolor := clwindow;
     ftrackposition := tpcenter;
     fminvalue := 0;
     fmaxvalue := 10;
     fThumbPosition := 0;
     fPageSize := 1;
     tabstop := true;
     fmarkfrequency := 1;
     fShowFocus := true;
     fShowMarks := true;
     Thumb := nil;
end;

destructor TrmTrackBar.destroy;
begin
     fMarkData.free;
     fThumb.free;
     inherited;
end;

procedure TrmTrackBar.Paint;
var
   wr, wRect : TRect;
   hcenter, vcenter : integer;
   loop : integer;
   xstart, xend, xadj, xwidth, ystart, yend, yadj, yheight, calcstep : single;
   wmaxvalue, wminvalue, wmarkfrequency, wTextMeasure : integer;
   newimage : tbitmap;
   xspacing, yspacing : integer;
   wText : string;
begin
     newimage := tbitmap.create;
     try
        newimage.height := height;
        newimage.width := width;
        if fmarkdata.count > 0 then
        begin
             wmaxvalue := fmarkdata.count-1;
             wminvalue := 0;
             wmarkfrequency := 1;
        end
        else
        begin
             wmaxvalue := fmaxvalue;
             wminvalue := fminvalue;
             wmarkfrequency := fmarkfrequency;
        end;
        calcstep := (wmaxvalue - wminvalue) / wmarkfrequency;
        newimage.canvas.font := font;
        newimage.canvas.brush.color := color;
        newimage.canvas.fillrect(GetClientRect);
        wr := GetTrackRect;
        ystart := 0;
        xstart := 0;
        xwidth := 0;
        yheight := 0;
        hcenter := 0;
        vcenter := 0;
        if fTrackOrientation = tovertical then
        begin
             if fmarkposition = mptopleft then
             begin
                  xstart := wr.Left-3;
                  xwidth := -5;
             end
             else
             if fmarkposition = mpbottomright then
             begin
                  xstart := wr.Right+2;
                  xwidth := 5;
             end;
             xend := xstart;
             xadj := 0;
             ystart :=wr.top + 2;
             yend := wr.bottom - 2;
             yadj := (yend - ystart) / calcstep;
             yheight := 0;
             hcenter := wr.left + ((wr.right-wr.left) shr 1);
             if (fThumbPosition >= wminvalue) and (fThumbPosition <= wmaxvalue) then
                vcenter := round( ystart + round ( (yend-ystart) * ( (fThumbPosition - wminvalue) / (wmaxvalue - wminvalue) ) ) )
             else
             if (fThumbPosition < wminvalue) then
                vcenter := round(ystart)
             else
             if (fThumbPosition > wmaxvalue) then
                vcenter := round(yend);
        end
        else
        begin
             if fmarkposition = mptopleft then
             begin
                  ystart := wr.Top - 3;
                  yheight := 5;
             end
             else
             if fmarkposition = mpbottomright then
             begin
                  ystart := wr.Bottom + 2;
                  yheight := -5;
             end;

             yend := ystart;
             yadj := 0;
             xstart := wr.Left+2;
             xend := wr.right-2;
             xadj := (xend - xstart) / calcstep;
             xwidth := 0;
             vcenter := wr.top + ((wr.bottom-wr.top) shr 1);
             if (fThumbPosition >= wminvalue) and (fThumbPosition <= wmaxvalue) then
                hcenter := round( xstart + round ( (xend-xstart) * ( (fThumbPosition - wminvalue) / (wmaxvalue - wminvalue) ) ) )
             else
             if (fThumbPosition > wmaxvalue) then
                hcenter := round(xend)
             else
             if (fThumbPosition < wminvalue) then
                hcenter := round(xstart);
        end;
        if fmarkposition <> mpNone then
        begin
             if fmarkdata.count > 0 then
             begin
                  if fTrackOrientation = tovertical then
                  begin
                       xspacing := fmarkspacing;
                       yspacing := -(newimage.canvas.TextHeight('X') shr 1);
                  end
                  else
                  begin
                       if fMarkPosition = mpTopLeft then
                          yStart := yStart - newimage.Canvas.TextHeight('X');
                       
                       xspacing := -(newimage.canvas.TextHeight('X') shr 1);
                       yspacing := fmarkspacing;
                  end;
                  loop := 0;
                  while loop < fmarkdata.count do
                  begin
                       wText := fmarkdata[loop];
                       if fTrackOrientation = toHorizontal then
                       begin
{                          Case MarkRotationAngle of
                             ra0, ra180 :
                                begin }
                                   wTextMeasure := (newimage.Canvas.TextWidth(wText) div 2);
                                   wRect.Left := (round(xstart) + xspacing) - wTextMeasure;
                                   wRect.Right := (round(xstart) + xspacing) + wTextMeasure;

                                   if fMarkPosition = mpTopLeft then
                                   begin
                                      wRect.Top := round(ystart) - yspacing;
                                      wRect.Bottom := round(ystart) - yspacing + newimage.Canvas.TextHeight(wText);
                                   end
                                   else
                                   begin
                                      wRect.Top := round(ystart) + yspacing;
                                      wRect.Bottom := round(ystart) + yspacing + newimage.Canvas.TextHeight(wText);
                                   end
{                                end;
                             ra90, ra270 :
                                begin
                                   wTextMeasure := newimage.Canvas.TextHeight(wText);
                                   wRect.Left := (round(xstart) + xspacing) - wTextMeasure;
                                   wRect.Right := (round(xstart) + xspacing) + wTextMeasure;

                                   wRect.Top := round(ystart) + yspacing;
                                   wRect.Bottom := round(ystart) + yspacing + newimage.Canvas.TextWidth(wText);
                                end;
                          end;}
                       end
                       else
                       begin
{                          Case MarkRotationAngle of
                             ra0, ra180 :
                                begin}
                                   wTextMeasure := (newimage.Canvas.TextHeight(wText) div 2);
                                   wRect.Top := round(ystart) + yspacing + wTextMeasure;
                                   wRect.Bottom := round(ystart) + yspacing + wTextMeasure;

                                   if MarkPosition = mpTopLeft then
                                   begin
                                      wRect.Left := (round(xstart) - xspacing) - newimage.Canvas.TextWidth(wText);
                                      wRect.Right := (round(xstart) - xspacing);
                                   end
                                   else
                                   begin
                                      wRect.Left := (round(xstart) + xspacing) ;
                                      wRect.Right := (round(xstart) + xspacing) + newimage.Canvas.TextWidth(wText);
                                   end;
{                                end;
                             ra90, ra270 :
                                begin
                                   wTextMeasure := newimage.Canvas.TextHeight(wText);
                                   wRect.Left := (round(xstart) + xspacing) - wTextMeasure;
                                   wRect.Right := (round(xstart) + xspacing) + wTextMeasure;

                                   wRect.Top := round(ystart) + yspacing;
                                   wRect.Bottom := round(ystart) + yspacing + newimage.Canvas.TextWidth(wText);
                                end;
                          end;}
                       end;
//                       RotateText(wText,frotationangle,newimage.canvas,wRect);
                       RotateText(newimage.canvas,wText,wRect,0);
                       xstart := xstart+xadj;
                       ystart := ystart+yadj;
                       inc(loop);
                  end;
             end
             else
             begin
                  if fTrackOrientation = toHorizontal then
                  begin
                       ystart := yStart - yheight;
                       yend := yend - yheight;
                  end;
                  newimage.canvas.Pen.color := clbtntext;
                  loop := 0;
                  while loop < round(calcstep) do
                  begin
                       newimage.canvas.moveto(round(xstart), round(ystart));
                       newimage.canvas.lineto(round(xstart+xwidth), round(ystart+yheight));
                       xstart := xstart+xadj;
                       ystart := ystart+yadj;
                       inc(loop);
                  end;
                  newimage.canvas.moveto(round(xend), round(yend));
                  newimage.canvas.lineto(round(xend+xwidth), round(yend+yheight));
             end;
        end;
        frame3d(newimage.canvas,wr,clBtnShadow,clBtnhighlight,1);
        frame3d(newimage.canvas,wr,cl3ddkshadow,cl3dlight,1);
        newimage.canvas.brush.color := ftrackColor;
        newimage.canvas.FillRect(wr);
        fThumb.Transparent := true;
        if not fThumb.Empty then
        begin
             newimage.canvas.draw(hcenter-(fThumb.Width shr 1),vcenter-(fThumb.height shr 1),fThumb);
        end;
        canvas.Draw(0,0,newimage);
        if Focused and fShowFocus then
           canvas.drawfocusrect(GetClientRect);
     finally
        newimage.free;
     end;
end;

procedure TrmTrackBar.SetTrackOrientation(value:TTrackOrientation);
begin
     if value <> fTrackOrientation then
     begin
          fTrackOrientation := value;
          if not Thumb.transparent then
             Thumb := nil;
     end;
     invalidate;
end;

procedure TrmTrackBar.SetMarkPosition(value:TMarkPosition);
begin
     if value <> fMarkPosition then
     begin
          fMarkPosition := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetTrackSize(value:integer);
begin
     if value <> ftracksize then
     begin
          ftracksize := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetTrackColor(value:TColor);
begin
     if value <> ftrackcolor then
     begin
          ftrackcolor := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetMinValue(value:integer);
begin
     if fmarkdata.count > 0 then exit;
     if value >= fmaxvalue then exit;
     if value <> fminvalue then
     begin
          fminvalue := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetMaxValue(value:integer);
begin
     if fmarkdata.count > 0 then exit;
     if value <= fminvalue then exit;
     if value <> fmaxvalue then
     begin
          fmaxvalue := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetThumbPos(value:integer);
begin
     if fmarkdata.count > 0 then
     begin
          if value < 0 then value := 0;
          if value >= fmarkdata.count then value := fmarkdata.count-1;
     end
     else
     begin
          if value < fminvalue then value := fminvalue;
          if value > fmaxvalue then value := fmaxvalue;
     end;
     if value <> fThumbPosition then
     begin
          fThumbPosition := value;
          invalidate;
          if assigned(fchanged) then fchanged(self);
     end;
end;

procedure TrmTrackBar.SetMarkSpacing(value:integer);
begin
     if value <> fmarkspacing then
     begin
          fmarkspacing := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetTrackPosition(value:TTrackPosition);
begin
     if value <> ftrackposition then
     begin
          ftrackposition := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetPageSize(value:integer);
begin
     if value <= 0 then exit;
     if value <> fPageSize then
     begin
          fPageSize := value;
          invalidate;
     end;
end;

procedure TrmTrackBar.SetThumb(value:tbitmap);
begin
     if value = nil then
        fThumb.LoadFromResourceName(HInstance, 'RMTRACKBAR')
     else
        fThumb.assign(value);
        
     fThumb.Transparent := true;
     invalidate;
end;

procedure TrmTrackBar.SetMarkData(value:TStrings);
begin
     if value.Count = 1 then
     begin
          showmessage('More than one point is required');
          exit;
     end;
     fmarkdata.assign(value);
     invalidate;
end;

procedure TrmTrackBar.setmarkfrequency(value:integer);
begin
     if value <= 0 then exit;
     if fmarkdata.count > 0 then exit;
     if value <> fmarkfrequency then
     begin
          fmarkfrequency := value;
          invalidate;
     end;
end;

function TrmTrackBar.GetTrackRect:TRect;
var
   wr : TRect;
   TCenter : integer;
   fVerticalIndent,
   fHorizontalIndent : integer;
begin
     fVerticalIndent :=  GreaterThanInt(Thumb.Height, Canvas.TextHeight('X'));
     fHorizontalIndent := GreaterThanInt(Thumb.Width, Canvas.TextWidth('X'));
     wr := Rect(0,0,width,height);
     if fTrackOrientation = tovertical then
     begin
          wr.top := wr.top + fVerticalindent;
          wr.bottom := wr.bottom - fVerticalIndent;
          case ftrackposition of
          tpcenter:
             begin
                  tcenter := (wr.left + ((wr.right-wr.left) shr 1));
                  wr.left :=  tcenter - (ftracksize shr 1);
                  wr.right := tcenter + (ftracksize shr 1);
             end;
          tpTopLeft:
             begin
                  wr.left := wr.left + fHorizontalIndent;
                  wr.right := wr.left + ftracksize;
             end;
          tpBottomRight:
             begin
                  wr.right := wr.right - fHorizontalIndent;
                  wr.left := wr.right - ftracksize;
             end;
          end;
     end
     else
     begin
          wr.left := wr.left + fHorizontalIndent;
          wr.right := wr.Right - fHorizontalIndent;
          case ftrackposition of
          tpcenter:
             begin
                  tcenter := (wr.top + ((wr.bottom-wr.top) shr 1));
                  wr.top :=  tcenter - (ftracksize shr 1);
                  wr.bottom := tcenter + (ftracksize shr 1);
             end;
          tpTopLeft:
             begin
                  wr.top := wr.top + fVerticalIndent;
                  wr.bottom := wr.top + ftracksize;
             end;
          tpBottomRight:
             begin
                  wr.bottom := wr.bottom - fVerticalIndent;
                  wr.top := wr.bottom - ftracksize;
             end;
          end;
     end;
     result := wr;
end;

procedure TrmTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
     case key of
        vk_down     :if fTrackOrientation = tovertical then ThumbPosition := ThumbPosition + 1;
        vk_up       :if fTrackOrientation = tovertical then ThumbPosition := ThumbPosition - 1;
        vk_right    :if fTrackOrientation = tohorizontal then ThumbPosition := ThumbPosition + 1;
        vk_left     :if fTrackOrientation = tohorizontal then ThumbPosition := ThumbPosition - 1;
        vk_next     :ThumbPosition := ThumbPosition + fpagesize;
        vk_prior    :ThumbPosition := ThumbPosition - fpagesize;
        vk_Home     :begin
                          if fMarkData.count > 0 then
                             ThumbPosition := 0
                          else
                             ThumbPosition := fminvalue;
                     end;
        vk_end      :begin
                          if fMarkData.count > 0 then
                             ThumbPosition := fmarkdata.count-1
                          else
                             ThumbPosition := fmaxvalue;
                     end;
     end;
end;

procedure TrmTrackBar.wmSetFocus(var msg:TWMSetFocus);
begin
     msg.result := 0;
     invalidate;
end;

procedure TrmTrackBar.wmKillFocus(var msg:TWMKillFocus);
begin
     msg.result := 0;
     invalidate;
end;

procedure TrmTrackBar.wmEraseBkGnd(var msg:TWMEraseBkGnd);
begin
     msg.result := 1;
end;

procedure TrmTrackBar.wmGetDLGCode(var msg:TWMGetDLGCode);
begin
     inherited;
     msg.Result := msg.Result or DLGC_WANTARROWS;
end;

procedure TrmTrackBar.wmMouseActivate(var msg:TWMMouseActivate);
begin
     inherited;
     msg.Result := msg.result or MA_ACTIVATE;
     if not (csdesigning in componentstate) then setfocus;
end;

procedure TrmTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
     inherited;
     if (csLButtonDown in controlstate) and (fmouseon) then thumbposition := pointposition(x,y);
end;

procedure TrmTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   mousepos : integer;
begin
     inherited;
     if button = mbleft then
        if ptinThumb(x,y) then
        begin
             thumbposition := pointposition(x,y);
             fmouseon := true;
        end
        else
        if ptinTrack(x,y) then
        begin
             mousepos := pointposition(x,y);
             if thumbposition > mousepos then
                thumbposition := thumbposition - fpagesize
             else
                thumbposition := thumbposition + fpagesize;
        end;
end;

procedure TrmTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     inherited;
     fmouseon := false;
end;

function TrmTrackBar.PointPosition(x,y:integer):integer;
var
   wr : TRect;
   wmaxvalue, wminvalue: integer;
   calcpos : integer;
   pstart, pend : integer;
begin
     if fmarkdata.count > 0 then
     begin
          wmaxvalue := fmarkdata.count-1;
          wminvalue := 0;
     end
     else
     begin
          wmaxvalue := fmaxvalue;
          wminvalue := fminvalue;
     end;
     wr := GetTrackRect;
     if fTrackOrientation = tovertical then
     begin
          pstart :=wr.top + 2;
          pend := wr.bottom - 2;
          calcpos := round((((y-pstart)/(pend-pstart))*(wmaxvalue - wminvalue)))+wminvalue;
     end
     else
     begin
          pstart := wr.Left+2;
          pend := wr.right-2;
          calcpos := round((((x-pstart)/(pend-pstart))*(wmaxvalue - wminvalue)))+wminvalue;
     end;
     result := calcpos;
end;

function TrmTrackBar.ptinthumb(x,y:integer):boolean;
var
   wr : TRect;
   hcenter, vcenter : integer;
   xstart, xend, ystart, yend : single;
   wmaxvalue, wminvalue : integer;
begin
     if fmarkdata.count > 0 then
     begin
          wmaxvalue := fmarkdata.count-1;
          wminvalue := 0;
     end
     else
     begin
          wmaxvalue := fmaxvalue;
          wminvalue := fminvalue;
     end;
     wr := GetTrackRect;
     hcenter := 0;
     vcenter := 0;
     if fTrackOrientation = tovertical then
     begin
          ystart :=wr.top + 2;
          yend := wr.bottom - 2;
          hcenter := wr.left + ((wr.right-wr.left) shr 1);
          if (fThumbPosition >= wminvalue) and (fThumbPosition <= wmaxvalue) then
             vcenter := round( ystart + round ( (yend-ystart) * ( (fThumbPosition - wminvalue) / (wmaxvalue - wminvalue) ) ) )
          else
          if (fThumbPosition < wminvalue) then
             vcenter := round(ystart)
          else
          if (fThumbPosition > wmaxvalue) then
             vcenter := round(yend);
     end
     else
     begin
          xstart := wr.Left+2;
          xend := wr.right-2;
          vcenter := wr.top + ((wr.bottom-wr.top) shr 1);
          if (fThumbPosition >= wminvalue) and (fThumbPosition <= wmaxvalue) then
             hcenter := round( xstart + round ( (xend-xstart) * ( (fThumbPosition - wminvalue) / (wmaxvalue - wminvalue) ) ) )
          else
          if (fThumbPosition > wmaxvalue) then
             hcenter := round(xend)
          else
          if (fThumbPosition < wminvalue) then
             hcenter := round(xstart);
     end;
     wr.top := vcenter-round(fThumb.height / 2);
     wr.left := hcenter-round(fThumb.Width / 2);
     wr.bottom := wr.top+fThumb.height;
     wr.right := wr.left+fThumb.Width;
     result := ptinrect(wr,point(x,y));
end;

function TrmTrackBar.ptintrack(x,y:integer):boolean;
begin
     result := ptinrect(gettrackrect,point(x,y));
end;

procedure TrmTrackBar.SetshowFocus(const Value: boolean);
begin
  fShowFocus := Value;
  Invalidate;
end;

procedure TrmTrackBar.SetShowMarks(const Value: boolean);
begin
  fShowMarks := Value;
  Invalidate;
end;

end.
