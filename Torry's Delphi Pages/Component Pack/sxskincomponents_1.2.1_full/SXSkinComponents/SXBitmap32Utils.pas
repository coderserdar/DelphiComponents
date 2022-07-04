unit SXBitmap32Utils;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Types, Windows, Graphics, Classes, GR32, GR32_Polygons;

type

 TSXCorner=(crLeftTop,crRightTop,crRightBottom,crLeftBottom);
 TSXCorners=set of TSXCorner;

procedure DrawWindowShadow(B:TBitmap32;Left,Top,Right,Bottom:Integer);
procedure SetEllipse(P:TPolygon32;X,Y,RX,RY:Single);
procedure SetRoundRectangle(P:TPolygon32;X,Y,W,H,R:Single;Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
procedure SetRectangle(P:TPolygon32;X,Y,W,H:Single);
procedure DrawTextInBitmapCenter(B:TBitmap32;const Text:String;
           Width,Height,TextWidth,OX,OY:Integer;Color:TColor;
           ShadowColor1:TColor=clNone;ShadowColor2:TColor=clNone);

procedure MultiplyAlpha(Bitmap:TBitmap32;MulAlpha:Integer);
procedure Monochrome(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure ColorOverlay(Bitmap:TBitmap32;Color:TColor32);
procedure ColorOverlayHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure ColorOverlayVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure Lighten(Bitmap:TBitmap32;Color:TColor32);
procedure LightenHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure LightenVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure Darken(Bitmap:TBitmap32;Color:TColor32);
procedure DarkenHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure DarkenVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
procedure AdvDrawTo(Bitmap:TBitmap32;Dst:TBitmap32); overload;
procedure AdvDrawTo(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer); overload;
procedure DrawMixedBitmap(Bitmap:TBitmap32;B1,B2:TBitmap32;Step:Byte);
procedure DrawHColorFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;iLeft,iTop,iRight,iBottom:Integer);
procedure DrawVColorFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;iLeft,iTop,iRight,iBottom:Integer);
procedure RectVFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
procedure RectHFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
procedure DrawToAsGray(Bitmap:TBitmap32;Dst:TBitmap32;DstRect:TRect;MAlpha:Integer=255); overload;
procedure DrawToAsGray(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;MAlpha:Integer=255); overload;
procedure DrawToAsChannel(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;Ch:Byte;MAlpha:Integer=255);
procedure AdvDrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;MAlpha:Integer=255);
procedure DrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;MAlpha:Integer=255); overload;
procedure DrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstRect:TRect;MAlpha:Integer=255); overload;
procedure RoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]); overload;
procedure RoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single); overload;
procedure RoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;X,Y,W,H,R:Integer);
procedure RoundRectVFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
procedure RoundRectHFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
procedure RoundRectFill(Bitmap:TBitmap32;Color:TColor;X,Y,W,H,R:Integer); overload;
procedure RoundRectFill(Bitmap:TBitmap32;Color:TColor32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]); overload;
procedure Ellipse(Bitmap:TBitmap32;X,Y,W,H:Single);
procedure EllipseFill(Bitmap:TBitmap32;Color:TColor32;X,Y,W,H:Integer);
procedure EllipseHFade(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
procedure EllipseVFade(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
procedure BottomRoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single);
procedure BottomRoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           X,Y,W,H,R:Integer;TotalHeight:Integer=0);
procedure TopRoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single);
procedure TopRoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           X,Y,W,H,R:Integer;TotalHeight:Integer=0);
procedure BlendBitmap(B:TBitmap32;A:Integer;C:TColor);

procedure GetRenderedTextSize(Bitmap:TBitmap32;const Text:String;AALevel:Integer;
           var Width,Height:Integer); overload;
procedure GetRenderedTextSize(Canvas:TCanvas;const Text:String;AALevel:Integer;
           var Width,Height:Integer); overload;
procedure DrawSmoothText(Bitmap:TBitmap32;const Text:String;var TextRect:TRect;
           Flags:UINT;AALevel:Integer;Color:TColor32); overload;
procedure DrawSmoothText(Canvas:TCanvas;const Text:String;var TextRect:TRect;
           Flags:UINT;AALevel:Integer); overload;
procedure DrawAlphaText(Bitmap:TBitmap32;const Text:String;var TextRect:TRect;
           Flags:UINT;Color:TColor32);           

procedure SX_BlendLineEx(Src,Dst:PColor32;Count:Integer;M:TColor32);
procedure SX_CombineMem(F:TColor32;var B:TColor32;W:TColor32);
procedure SX_BlendMem(F:TColor32;var B:TColor32);

procedure ColorToRedComponent(Dst,Src:TBitmap32);
procedure ColorToGreenComponent(Dst,Src:TBitmap32);
procedure ColorToBlueComponent(Dst,Src:TBitmap32);

procedure AdvBlockTransfer(Dst:TBitmap32;DstX:Integer;DstY:Integer;DstClip:TRect;
           Src:TBitmap32;SrcRect:TRect;CombineOp:TDrawMode;CombineCallBack:TPixelCombineEvent=nil);

implementation

uses GR32_LowLevel, GR32_Filters, Math, GR32_Blend;

const WShadow:array[0..4,0..5]of Byte=((   3,  14,  42,  82, 110, 113),
                                       (   2,  13,  38,  65,  82,  84),
                                       (   2,  10,  18,  38,  42,  43),
                                       (   1,   5,  10,  13,  14,  14),
                                       (   1,   1,   2,   2,   3,   3));

procedure DrawWindowShadow(B:TBitmap32;Left,Top,Right,Bottom:Integer);
var X,Y:Integer;
  RS,BS:Integer;
      P:PColor32Array;
begin
 RS:=Right+1;
 BS:=Bottom+1;
 //Top Right Corner
 for Y:=0 to 4 do
  begin
   P:=B.ScanLine[Y+5+Top];
   for X:=0 to 4 do
    P[X+RS]:=CombineReg(clBlack32,P[X+RS],WShadow[4-Y,4-X]);
  end;
 //Bottom Right Corner
 for Y:=0 to 4 do
  begin
   P:=B.ScanLine[Y+BS];
   for X:=0 to 4 do
    P[X+RS]:=CombineReg(clBlack32,P[X+RS],WShadow[Y,4-X]);
  end;
 //Bottom Left Corner
 for Y:=0 to 4 do
  begin
   P:=B.ScanLine[Y+BS];
   for X:=0 to 4 do
    P[X+5+Left]:=CombineReg(clBlack32,P[X+5+Left],WShadow[Y,X]);
  end;
 //Right Center Line
 for Y:=10+Top to Bottom do
  begin
   P:=B.ScanLine[Y];
   for X:=0 to 4 do
    P[X+RS]:=CombineReg(clBlack32,P[X+RS],WShadow[X,5]);
  end;
 //Bottom Center Line
 for Y:=0 to 4 do
  begin
   P:=B.ScanLine[Y+BS];
   for X:=10+Left to Right do
    P[X]:=CombineReg(clBlack32,P[X],WShadow[Y,5]);
  end;
 EMMS;
end;

procedure SetEllipse(P:TPolygon32;X,Y,RX,RY:Single);
const S_PI=3.14159;
var Step:Single;
   Angle:Single;
       R:Single;
begin
 P.Clear;
 if RX>RY then R:=RX else R:=RY;
 if R<3 then Step:=0.2 else
  if R<10 then Step:=0.1 else
   Step:=0.05;
 Angle:=0;
 repeat
  P.Add(FixedPoint(X+RX*cos(Angle),Y+RY*sin(Angle)));
  Angle:=Angle+Step;
 until Angle>S_PI*2;
 P.Closed:=True;
end;

procedure SetRoundRectangle(P:TPolygon32;X,Y,W,H,R:Single;Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
const S_PI=3.141592;
var T:Double;
 Step:Single;
begin
 W:=W-1; H:=H-1;
 P.Clear;
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.5 else
   if R<20 then Step:=0.1 else
    Step:=0.05;
 if crLeftTop in Corners then
  begin
   T:=-S_PI/2;
   while T>-S_PI do
    begin
     P.Add(FixedPoint(X+Cos(T)*R+R,Y+Sin(T)*R+R));
     T:=T-Step;
    end;
   P.Add(FixedPoint(X,Y+R));
  end else P.Add(FixedPoint(X,Y));
 if crLeftBottom in Corners then
  begin
   P.Add(FixedPoint(X,Y+H-R));
   T:=S_PI;
   while T>S_PI/2 do
    begin
     P.Add(FixedPoint(X+Cos(T)*R+R,Y+Sin(T)*R+H-R));
     T:=T-Step;
    end;
   P.Add(FixedPoint(X+R,Y+H));
  end else P.Add(FixedPoint(X,Y+H));
 if crRightBottom in Corners then
  begin
   P.Add(FixedPoint(X+W-R,Y+H));
   T:=S_PI/2;
   while T>0 do
    begin
     P.Add(FixedPoint(X+Cos(T)*R-R+W,Y+Sin(T)*R+H-R));
     T:=T-Step;
    end;
   P.Add(FixedPoint(X+W,Y+H-R));
  end else P.Add(FixedPoint(X+W,Y+H));
 if crRightTop in Corners then
  begin
   P.Add(FixedPoint(X+W,Y+R));
   T:=0;                              
   while T>-S_PI/2 do
    begin
     P.Add(FixedPoint(X+Cos(T)*R-R+W,Y+Sin(T)*R+R));
     T:=T-Step;
    end;
   P.Add(FixedPoint(X-R+W,Y)); 
  end else P.Add(FixedPoint(X+W,Y));
 P.Closed:=True;
end;

procedure SetRectangle(P:TPolygon32;X,Y,W,H:Single);
begin
 P.Clear;
 P.Add(FixedPoint(X,Y));
 P.Add(FixedPoint(X+W-1,Y));
 P.Add(FixedPoint(X+W-1,Y+H-1));
 P.Add(FixedPoint(X,Y+H-1));
 P.Closed:=True;
end;

procedure DrawTextInBitmapCenter(B:TBitmap32;const Text:String;Width,Height,TextWidth,OX,OY:Integer;
           Color:TColor;ShadowColor1:TColor=clNone;ShadowColor2:TColor=clNone);
var R:TRect;
  A,H:Integer;
begin
 R:=Rect((Width-TextWidth) div 2+OX,OY,(Width+TextWidth) div 2+OX,Height+OY);
 B.Canvas.Brush.Style:=bsClear;
 A:=DT_VCENTER or DT_WORDBREAK or DT_CENTER or DT_NOCLIP or DT_CALCRECT;
 DrawText(B.Canvas.Handle,PChar(Text),length(Text),R,A);
 H:=R.Bottom-R.Top;
 R:=Rect((Width-TextWidth) div 2+OX,(Height-H) div 2+OY,(Width+TextWidth) div 2+OX,Height+OY);
 A:=DT_VCENTER or DT_WORDBREAK or DT_CENTER or DT_NOCLIP;
 Inc(R.Left,2); Inc(R.Top,2); Inc(R.Right,2);
 if ShadowColor2<>clNone then
  begin
   B.Canvas.Font.Color:=ShadowColor2;
   DrawText(B.Canvas.Handle,PChar(Text),length(Text),R,A);
  end;
 Dec(R.Left); Dec(R.Top); Dec(R.Right);
 if ShadowColor1<>clNone then
  begin
   B.Canvas.Font.Color:=ShadowColor1;
   DrawText(B.Canvas.Handle,PChar(Text),length(Text),R,A);
  end;
 Dec(R.Left); Dec(R.Top); Dec(R.Right);
 B.Canvas.Font.Color:=Color;
 DrawText(B.Canvas.Handle,PChar(Text),length(Text),R,A);
end;

procedure DrawToAsGray(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;
           MAlpha:Integer=255);
var B2:TBitmap32;
begin
 if Bitmap.Empty or Dst.Empty then exit;
 B2:=TBitmap32.Create;
 try
  ColorToGrayScale(B2,Bitmap,True);
  B2.DrawMode:=dmBlend;
  B2.MasterAlpha:=MAlpha;
  B2.DrawTo(Dst,DstX,DstY);
 finally
  B2.Free;
 end;
end;

procedure DrawToAsGray(Bitmap:TBitmap32;Dst:TBitmap32;DstRect:TRect;
           MAlpha:Integer=255);
var B2:TBitmap32;
begin
 if Bitmap.Empty or Dst.Empty then exit;
 B2:=TBitmap32.Create;
 try
  ColorToGrayScale(B2,Bitmap,True);
  B2.DrawMode:=dmBlend;
  B2.MasterAlpha:=MAlpha;
  B2.DrawTo(Dst,DstRect);
 finally
  B2.Free;
 end;
end;

procedure DrawToAsChannel(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;
           Ch:Byte;MAlpha:Integer=255);
var B2:TBitmap32;
begin
 if Bitmap.Empty or Dst.Empty then exit;
 B2:=TBitmap32.Create;
 try
  case Ch of
   0: ColorToRedComponent(B2,Bitmap);
   1: ColorToGreenComponent(B2,Bitmap);
   2: ColorToBlueComponent(B2,Bitmap);
  end;
  B2.DrawMode:=dmBlend;
  B2.MasterAlpha:=MAlpha;
  B2.DrawTo(Dst,DstX,DstY);
 finally
  B2.Free;
 end;
end;

procedure MultiplyAlpha(Bitmap:TBitmap32;MulAlpha:Integer);
var I:Integer;
    P:PByte;
begin
 P:=@Bitmap.Bits[0];
 Inc(P,3);
 for I:=0 to Bitmap.Width*Bitmap.Height-1 do
  begin
   P^:=Integer(P^)*MulAlpha div 255;
   Inc(P,4);
  end;
end;

procedure Monochrome(Bitmap:TBitmap32;Color1,Color2:TColor32);
var   P:PColor32;
    A,C:Integer;
  Color:TColor32;
 Colors:array[Byte]of TColor32;
begin
 P:=@Bitmap.Bits[0];
 Colors[0]:=Color1;
 for A:=1 to 254 do
  Colors[A]:=CombineReg(Color2,Color1,A);
 Colors[255]:=Color2;
 for C:=0 to Bitmap.Width*Bitmap.Height-1 do
  begin
   A:=Intensity(P^);
   Color:=Colors[A];
   P^:=(DivTable[P^ shr 24,Color shr 24] shl 24) or (Color and $00FFFFFF);
   Inc(P);
  end;
 EMMS;
end;

procedure ColorOverlay(Bitmap:TBitmap32;Color:TColor32);
var I:Integer;
    D:PColor32;
begin
 D:=@Bitmap.Bits[0];
 for I:=0 to Bitmap.Width*Bitmap.Height-1 do
  begin
   D^:=(BlendReg(Color,D^) and $00FFFFFF) or (D^ and $FF000000);
   Inc(D);
  end;
 EMMS;
end;

procedure ColorOverlayHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
 Colors:array of TColor32;
    CLA:Integer;
begin
 SetLength(Colors,Bitmap.Width);
 for X:=0 to Bitmap.Width-1 do
  begin
   CLA:=255*(X+1) div Bitmap.Width;
   Colors[X]:=CombineReg(Color2,Color1,CLA);
  end;
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  for X:=0 to Bitmap.Width-1 do
   begin
    D^:=(BlendReg(Colors[X],D^) and $00FFFFFF) or (D^ and $FF000000);
    Inc(D);
   end;
 EMMS;
end;

procedure ColorOverlayVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
  Color:TColor32;
    CLA:Integer;
begin
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  begin
   CLA:=255*(Y+1) div Bitmap.Height;
   Color:=CombineReg(Color2,Color1,CLA);
   for X:=0 to Bitmap.Width-1 do
    begin
     D^:=(BlendReg(Color,D^) and $00FFFFFF) or (D^ and $FF000000);
     Inc(D);
    end;
  end;
 EMMS;
end;

procedure Lighten(Bitmap:TBitmap32;Color:TColor32);
var I:Integer;
    D:PColor32;
begin
 D:=@Bitmap.Bits[0];
 for I:=0 to Bitmap.Width*Bitmap.Height-1 do
  begin
   D^:=ColorAdd(D^,Color);
   Inc(D);
  end;
 EMMS;
end;

procedure LightenHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
 Colors:array of TColor32;
    CLA:Integer;
begin
 SetLength(Colors,Bitmap.Width);
 for X:=0 to Bitmap.Width-1 do
  begin
   CLA:=255*(X+1) div Bitmap.Width;
   Colors[X]:=CombineReg(Color2,Color1,CLA);
  end;
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  for X:=0 to Bitmap.Width-1 do
   begin
    D^:=ColorAdd(D^,Colors[X]);
    Inc(D);
   end;
 EMMS;
end;

procedure LightenVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
  Color:TColor32;
    CLA:Integer;
begin
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  begin
   CLA:=255*(Y+1) div Bitmap.Height;
   Color:=CombineReg(Color2,Color1,CLA);
   for X:=0 to Bitmap.Width-1 do
    begin
     D^:=ColorAdd(D^,Color);
     Inc(D);
    end;
  end;
 EMMS;
end;

procedure Darken(Bitmap:TBitmap32;Color:TColor32);
 var I:Integer;
     D:PColor32;
begin
 D:=@Bitmap.Bits[0];
 for I:=0 to Bitmap.Width*Bitmap.Height-1 do
  begin
   D^:=ColorSub(D^,Color);
   Inc(D);
  end;
 EMMS;
end;

procedure DarkenHorizG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
 Colors:array of TColor32;
    CLA:Integer;
begin
 SetLength(Colors,Bitmap.Width);
 for X:=0 to Bitmap.Width-1 do
  begin
   CLA:=255*(X+1) div Bitmap.Width;
   Colors[X]:=CombineReg(Color2,Color1,CLA);
  end;
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  for X:=0 to Bitmap.Width-1 do
   begin
    D^:=ColorSub(D^,Colors[X]);
    Inc(D);
   end;
 EMMS;
end;

procedure DarkenVertG(Bitmap:TBitmap32;Color1,Color2:TColor32);
var X,Y:Integer;
      D:PColor32;
  Color:TColor32;
    CLA:Integer;
begin
 D:=@Bitmap.Bits[0];
 for Y:=0 to Bitmap.Height-1 do
  begin
   CLA:=255*(Y+1) div Bitmap.Height;
   Color:=CombineReg(Color2,Color1,CLA);
   for X:=0 to Bitmap.Width-1 do
    begin
     D^:=ColorSub(D^,Color);
     Inc(D);
    end;
  end;
 EMMS;
end;

procedure AdvDrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;
           MAlpha:Integer=255);
var A:Cardinal;
begin
 A:=Bitmap.MasterAlpha;
 Bitmap.MasterAlpha:=MAlpha;
 AdvDrawTo(Bitmap,Dst,DstX,DstY);
 Bitmap.MasterAlpha:=A;
end;

procedure DrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer;
           MAlpha:Integer=255);
var A:Cardinal;
begin
 A:=Bitmap.MasterAlpha;
 Bitmap.MasterAlpha:=MAlpha;
 Bitmap.DrawTo(Dst,DstX,DstY);
 Bitmap.MasterAlpha:=A;
end;

procedure DrawToAsAlpha(Bitmap:TBitmap32;Dst:TBitmap32;DstRect:TRect;
           MAlpha:Integer=255);
var A:Cardinal;
begin
 A:=Bitmap.MasterAlpha;
 Bitmap.MasterAlpha:=MAlpha;
 Bitmap.DrawTo(Dst,DstRect);
 Bitmap.MasterAlpha:=A;
end;

procedure DrawMixedBitmap(Bitmap:TBitmap32;B1,B2:TBitmap32;Step:Byte);
var A:Integer;
  D,D1,D2:PColor32;
begin
 if (B1.Width<>B2.Width) or (B1.Height<>B2.Height) or (Bitmap.Width<>B1.Width) or
    (Bitmap.Height<>B1.Height) then exit;
 D:=@Bitmap.Bits[0];
 D1:=@B1.Bits[0];
 D2:=@B2.Bits[0];
 for A:=0 to B1.Width*B1.Height-1 do
  begin
   if D2^ shr 24=0 then
    begin
     D^:=SetAlpha(D1^,DivTable[D1^ shr 24,255-Step]);
    end else
   if D1^ shr 24=0 then
    begin
     D^:=SetAlpha(D2^,((D2^ shr 24)*Step) div 255);
    end else
     begin
      D^:=MergeRegEx(D2^,SetAlpha(D1^,DivTable[D1^ shr 24,255-Step]),Step);
      D^:=SetAlpha(D^,Integer(D1^ shr 24)+((Integer(D2^ shr 24)-Integer(D1^ shr 24))*Step) div 255);
     end;
   Inc(D); Inc(D1); Inc(D2);
  end;
 EMMS;
end;

procedure DrawHColorFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           iLeft,iTop,iRight,iBottom:Integer);
var iCounter:Integer;
   aStepsNum:Integer;
  CL,CL1,CL2:TColor32;
         CLA:Integer;
begin
 if iBottom>Bitmap.Height then iBottom:=Bitmap.Height;
 if iTop<0 then iTop:=0;
 if iRight>Bitmap.Width then iRight:=Bitmap.Width;
 if iLeft<0 then iLeft:=0;
 Dec(iRight);
 CL1:=Color32(StartColor);
 CL2:=Color32(StopColor);
 aStepsNum:=iRight-iLeft+1;
 for iCounter:=iRight downto iLeft do
  begin
   CLA:=Trunc(255/aStepsNum*(iCounter-iLeft));
   CL:=CL1;
   BlendMem(SetAlpha(CL2,CLA),CL);
   EMMS;
   Bitmap.VertLineS(iCounter,iTop,iBottom,CL);
  end;
end;

procedure DrawVColorFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           iLeft,iTop,iRight,iBottom:Integer);
var iCounter:Integer;
   aStepsNum:Integer;
  CL,CL1,CL2:TColor32;
         CLA:Integer;
begin
 if iBottom>Bitmap.Height then iBottom:=Bitmap.Height;
 if iTop<0 then iTop:=0;
 if iRight>Bitmap.Width then iRight:=Bitmap.Width;
 if iLeft<0 then iLeft:=0;
 Dec(iBottom);
 CL1:=Color32(StartColor);
 CL2:=Color32(StopColor);
 aStepsNum:=iBottom-iTop+1;
 for iCounter:=iBottom downto iTop do
  begin
   CLA:=Trunc(255/aStepsNum*(iCounter-iTop));
   CL:=CL1;
   BlendMem(SetAlpha(CL2,CLA),CL);
   EMMS;
   Bitmap.HorzLineS(iLeft,iCounter,iRight,CL);
  end;
end;

procedure TopRoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           X,Y,W,H,R:Integer;TotalHeight:Integer=0);
var     A,B:Integer;
 CL,CL1,CL2:TColor32;
        CLA:Integer;
         L:Integer;
begin
 B:=H;
 CL1:=Color32(StartColor);
 CL2:=Color32(StopColor);
 for A:=0 to B-1 do
  begin
   CLA:=255*(A+1) div B;
   CL:=CL1;
   BlendMem(SetAlpha(CL2,CLA),CL);
   EMMS;
   if A<R then L:=Ceil(R-Sqrt(2*R*A-Sqr(A))) else L:=0;
   Bitmap.HorzLineS(L+X,Y+A,L+X+W-2*L,CL);
  end;
end;

procedure BottomRoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           X,Y,W,H,R:Integer;TotalHeight:Integer=0);
var     A,B:Integer;
 CL,CL1,CL2:TColor32;
        CLA:Integer;
          L:Integer;
begin
 B:=Trunc(H);
 CL1:=Color32(StartColor);
 CL2:=Color32(StopColor);
 for A:=0 to B-1 do
  begin
   CLA:=255*(A+1) div B;
   CL:=CL1;
   BlendMem(SetAlpha(CL2,CLA),CL);
   EMMS;
   if A>H-R then L:=Ceil(R-Sqrt(2*R*(H-A-1)-Sqr(H-A-1))) else L:=0;
   if (L=0) and (TotalHeight<>0) then
    begin
     if (TotalHeight-H<R) and (A<R-TotalHeight+H) then
      L:=Ceil(R-Sqrt(2*R*(TotalHeight-H+A)-Sqr(TotalHeight-H+A)));
    end;
   Bitmap.HorzLineS(L+X,Y+A,L+X+W-2*L,CL);
  end;
end;

procedure RoundRectFill(Bitmap:TBitmap32;Color:TColor;X,Y,W,H,R:Integer);
var A,B,D:Integer;
        L:Integer;
        C:TColor32;
begin
 C:=Color32(Color);
 B:=H;
 for A:=0 to B-1 do
  begin
   if A<R then L:=Ceil(R-Sqrt(A*(2*R-A))) else
    if A>H-R then
     begin
      D:=H-A-1;
      L:=Ceil(R-Sqrt(D*(2*R-D)));
     end else L:=0;
   Bitmap.HorzLineS(L+X,Y+A,X+W-L,C);
  end;
end;

procedure RoundRectFill(Bitmap:TBitmap32;Color:TColor32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
var         A:Integer;
         SA,D:Single;
      L,LL,RR:Integer;
        SH,FH:Integer;
 HorzLineFunc:procedure(X1,Y,X2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if Color and $FF000000=$FF000000 then
  HorzLineFunc:=Bitmap.HorzLine else
   HorzLineFunc:=Bitmap.HorzLineT;
 SH:=0;
 if Bitmap.ClipRect.Top>Y then
  SH:=Bitmap.ClipRect.Top-Y;
 FH:=H-1;
 if Bitmap.ClipRect.Bottom<Y+H then
  FH:=Bitmap.ClipRect.Bottom-Y-1;
 for A:=SH to FH do
  begin
   SA:=A+0.5;
   if SA<R then
    begin
     L:=Ceil(R-Sqrt(SA*(2*R-SA)));
     if crLeftTop in Corners then
      LL:=L+X else LL:=X;
     if crRightTop in Corners then
      RR:=X+W-L else RR:=X+W;
    end else
   if SA>H-R then
    begin
     D:=H-SA;
     L:=Ceil(R-Sqrt(D*(2*R-D)));
     if crLeftBottom in Corners then
      LL:=L+X else LL:=X;
     if crRightBottom in Corners then
      RR:=X+W-L else RR:=X+W;
    end else
     begin
      LL:=X;
      RR:=X+W;
     end;
   if (LL<Bitmap.ClipRect.Right) and (RR>Bitmap.ClipRect.Left) and (LL<RR) then
    begin
     if LL<Bitmap.ClipRect.Left then
      LL:=Bitmap.ClipRect.Left;
     if RR>Bitmap.ClipRect.Right then
      RR:=Bitmap.ClipRect.Right;
     HorzLineFunc(LL,Y+A,RR-1,Color);
    end;
  end;
end;

procedure EllipseFill(Bitmap:TBitmap32;Color:TColor32;X,Y,W,H:Integer);
var   A,LL,RR:Integer;
      L,SH,FH:Integer;
   R1,R2,SA,D:Double;
 HorzLineFunc:procedure(X1,Y,X2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if Color and $FF000000=$FF000000 then
  HorzLineFunc:=Bitmap.HorzLine else
   HorzLineFunc:=Bitmap.HorzLineT;
 SH:=0;
 if Bitmap.ClipRect.Top>Y then
  SH:=Bitmap.ClipRect.Top-Y;
 FH:=H-1;
 if Bitmap.ClipRect.Bottom<Y+H then
  FH:=Bitmap.ClipRect.Bottom-Y-1;
 R1:=W/2; R2:=H/2;
 for A:=SH to FH do
  begin
   SA:=A+0.5;
   if SA<R2 then L:=round((R2-Sqrt(SA*(2*R2-SA)))*R1/R2) else
    if SA>H-R2 then
     begin
      D:=H-SA;
      L:=round((R2-Sqrt(D*(2*R2-D)))*R1/R2);
     end else L:=0;
   LL:=L+X; RR:=X+W-L;
   if (LL<Bitmap.ClipRect.Right) and (RR>Bitmap.ClipRect.Left) and (LL<RR) then
    begin
     if LL<Bitmap.ClipRect.Left then
      LL:=Bitmap.ClipRect.Left;
     if RR>Bitmap.ClipRect.Right then
      RR:=Bitmap.ClipRect.Right;
     HorzLineFunc(LL,Y+A,RR-1,Color);
    end;
  end;
end;

procedure RoundRectVFade(Bitmap:TBitmap32;StartColor,StopColor:TColor;
           X,Y,W,H,R:Integer);
var     A,B:Integer;
 CL,CL1,CL2:TColor32;
        CLA:Integer;
         L:Integer;
begin
 B:=H;
 CL1:=Color32(StartColor);
 CL2:=Color32(StopColor);
 for A:=0 to B-1 do
  begin
   CLA:=255*(A+1) div B;
   CL:=CL1;
   BlendMem(SetAlpha(CL2,CLA),CL);
   EMMS;
   if A<R then L:=Ceil(R-Sqrt(2*R*A-Sqr(A))) else
    if A>H-R then L:=Ceil(R-Sqrt(2*R*(H-A)-Sqr(H-A))) else L:=0;
   Bitmap.HorzLineS(L+X,Y+A,L+X+W-2*L,CL);
  end;
end;

procedure RoundRectVFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
var   A,SH,FH:Integer;
         SA,D:Single;
           CL:TColor32;
          CLA:Integer;
      L,LL,RR:Integer;
 HorzLineFunc:procedure(X1,Y,X2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  HorzLineFunc:=Bitmap.HorzLine else
   HorzLineFunc:=Bitmap.HorzLineT;
 SH:=0;
 if Bitmap.ClipRect.Top>Y then
  SH:=Bitmap.ClipRect.Top-Y;
 FH:=H-1;
 if Bitmap.ClipRect.Bottom<Y+H then
  FH:=Bitmap.ClipRect.Bottom-Y-1;
 for A:=SH to FH do
  begin
   CLA:=255*(A+1) div H;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   SA:=A+0.5;
   if SA<R then
    begin
     L:=Ceil(R-Sqrt(SA*(2*R-SA)));
     if crLeftTop in Corners then
      LL:=L+X else LL:=X;
     if crRightTop in Corners then
      RR:=X+W-L else RR:=X+W;
    end else
   if SA>H-R then
    begin
     D:=H-SA;
     L:=Ceil(R-Sqrt(D*(2*R-D)));
     if crLeftBottom in Corners then
      LL:=L+X else LL:=X;
     if crRightBottom in Corners then
      RR:=X+W-L else RR:=X+W;
    end else
     begin
      LL:=X;
      RR:=X+W;
     end;
   if (LL<Bitmap.ClipRect.Right) and (RR>Bitmap.ClipRect.Left) and (LL<RR) then
    begin
     if LL<Bitmap.ClipRect.Left then
      LL:=Bitmap.ClipRect.Left;
     if RR>Bitmap.ClipRect.Right then
      RR:=Bitmap.ClipRect.Right;
     HorzLineFunc(LL,Y+A,RR-1,CL);
    end;
  end;
end;

procedure RoundRectHFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;
           X,Y,W,H,R:Integer;Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
var   A,SW,FW:Integer;
         SA,D:Single;
           CL:TColor32;
          CLA:Integer;
      T,TT,BB:Integer;
 VertLineFunc:procedure(X,Y1,Y2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  VertLineFunc:=Bitmap.VertLine else
   VertLineFunc:=Bitmap.VertLineT;
 SW:=0;
 if Bitmap.ClipRect.Left>X then
  SW:=Bitmap.ClipRect.Left-X;
 FW:=W-1;
 if Bitmap.ClipRect.Right<X+W then
  FW:=Bitmap.ClipRect.Right-X-1;
 for A:=SW to FW do
  begin
   CLA:=255*(A+1) div W;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   SA:=A+0.5;
   if SA<R then
    begin
     T:=Ceil(R-Sqrt(SA*(2*R-SA)));
     if crLeftTop in Corners then
      TT:=T+Y else TT:=Y;
     if crLeftBottom in Corners then
      BB:=Y+H-T else BB:=Y+H;
    end else
   if SA>W-R then
    begin
     D:=W-SA;
     T:=Ceil(R-Sqrt(D*(2*R-D)));
     if crRightTop in Corners then
      TT:=T+Y else TT:=Y;
     if crRightBottom in Corners then
      BB:=Y+H-T else BB:=Y+H;
    end else
     begin
      TT:=Y;
      BB:=Y+H;
     end;
   if (TT<Bitmap.ClipRect.Bottom) and (BB>Bitmap.ClipRect.Top) and (TT<BB) then
    begin
     if TT<Bitmap.ClipRect.Top then
      TT:=Bitmap.ClipRect.Top;
     if BB>Bitmap.ClipRect.Bottom then
      BB:=Bitmap.ClipRect.Bottom;
     VertLineFunc(X+A,TT,BB-1,CL);
    end; 
  end;
end;

procedure RectVFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
var         A:Integer;
           CL:TColor32;
          CLA:Integer;
        SH,FH:Integer;
 HorzLineFunc:procedure(X1,Y,X2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  HorzLineFunc:=Bitmap.HorzLine else
   HorzLineFunc:=Bitmap.HorzLineT;
 SH:=0;
 if Bitmap.ClipRect.Top>Y then
  SH:=Bitmap.ClipRect.Top-Y;
 FH:=H-1;
 if Bitmap.ClipRect.Bottom<Y+H then
  FH:=Bitmap.ClipRect.Bottom-Y-1;
 if X<Bitmap.ClipRect.Left then
  begin
   W:=W-Bitmap.ClipRect.Left+X;
   X:=Bitmap.ClipRect.Left;
  end;
 if X+W>Bitmap.ClipRect.Right then W:=Bitmap.ClipRect.Right-X;
 for A:=SH to FH do
  begin
   CLA:=255*(A+1) div H;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   HorzLineFunc(X,Y+A,X+W-1,CL);
  end;
end;

procedure RectHFadeT(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
var         A:Integer;
           CL:TColor32;
          CLA:Integer;
        SW,FW:Integer;  
 VertLineFunc:procedure(X,Y1,Y2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  VertLineFunc:=Bitmap.VertLine else
   VertLineFunc:=Bitmap.VertLineT;
 SW:=0;
 if Bitmap.ClipRect.Left>X then
  SW:=Bitmap.ClipRect.Left-X;
 FW:=W-1;
 if Bitmap.ClipRect.Right<X+W then
  FW:=Bitmap.ClipRect.Right-X-1;
 if Y<Bitmap.ClipRect.Top then
  begin
   H:=H-Bitmap.ClipRect.Top+Y;
   Y:=Bitmap.ClipRect.Top;
  end; 
 if Y+H>Bitmap.ClipRect.Bottom then H:=Bitmap.ClipRect.Bottom-Y;
 for A:=SW to FW do
  begin
   CLA:=255*(A+1) div W;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   VertLineFunc(X+A,Y,Y+H-1,CL);
  end;
end;

procedure EllipseHFade(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
var         A:Integer;
         SA,D:Single;
           CL:TColor32;
    CLA,SW,FW:Integer;
      L,TT,BB:Integer;
        R1,R2:Single;
 VertLineFunc:procedure(X,Y1,Y2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  VertLineFunc:=Bitmap.VertLine else
   VertLineFunc:=Bitmap.VertLineT;
 SW:=0;
 if Bitmap.ClipRect.Left>X then
  SW:=Bitmap.ClipRect.Left-X;
 FW:=W-1;
 if Bitmap.ClipRect.Right<X+W then
  FW:=Bitmap.ClipRect.Right-X-1;
 R1:=W/2; R2:=H/2;
 for A:=SW to FW do
  begin
   CLA:=255*(A+1) div W;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   SA:=A+0.5;
   if SA<R1 then L:=Ceil((R1-Sqrt(SA*(2*R1-SA)))*R2/R1) else
    if SA>W-R1 then
     begin
      D:=W-SA;
      L:=Ceil((R1-Sqrt(D*(2*R1-D)))*R2/R1);
     end else L:=0;
   TT:=L+Y; BB:=-L+Y+H;
   if (TT<Bitmap.ClipRect.Bottom) and (BB>Bitmap.ClipRect.Top) and (TT<BB) then
    begin
     if TT<Bitmap.ClipRect.Top then
      TT:=Bitmap.ClipRect.Top;
     if BB>Bitmap.ClipRect.Bottom then
      BB:=Bitmap.ClipRect.Bottom;
     VertLineFunc(X+A,TT,BB-1,CL);
    end; 
  end;
end;

procedure EllipseVFade(Bitmap:TBitmap32;CL1,CL2:TColor32;X,Y,W,H:Integer);
var         A:Integer;
         SA,D:Single;
           CL:TColor32;
    CLA,LL,RR:Integer;
      L,SH,FH:Integer;
        R1,R2:Single;
 HorzLineFunc:procedure(X1,Y,X2:Integer;Value:TColor32)of object;
begin
 if (X>=Bitmap.ClipRect.Right) or (Y>=Bitmap.ClipRect.Bottom) or
    (X+W<=Bitmap.ClipRect.Left) or (Y+H<=Bitmap.ClipRect.Top) then exit;
 if (CL1 and $FF000000=$FF000000) and (CL2 and $FF000000=$FF000000) then
  HorzLineFunc:=Bitmap.HorzLine else
   HorzLineFunc:=Bitmap.HorzLineT;
 SH:=0;
 if Bitmap.ClipRect.Top>Y then
  SH:=Bitmap.ClipRect.Top-Y;
 FH:=H-1;
 if Bitmap.ClipRect.Bottom<Y+H then
  FH:=Bitmap.ClipRect.Bottom-Y-1;
 R1:=W/2; R2:=H/2;
 for A:=SH to FH do
  begin
   CLA:=255*(A+1) div H;
   CL:=CombineReg(CL2,CL1,CLA);
   EMMS;
   SA:=A+0.5;
   if SA<R2 then L:=Ceil((R2-Sqrt(SA*(2*R2-SA)))*R1/R2) else
    if SA>H-R2 then
     begin
      D:=H-SA;
      L:=Ceil((R2-Sqrt(D*(2*R2-D)))*R1/R2);
     end else L:=0;
   LL:=L+X; RR:=-L+X+W;
   if (LL<Bitmap.ClipRect.Right) and (RR>Bitmap.ClipRect.Left) and (LL<RR) then
    begin
     if LL<Bitmap.ClipRect.Left then
      LL:=Bitmap.ClipRect.Left;
     if RR>Bitmap.ClipRect.Right then
      RR:=Bitmap.ClipRect.Right;
     HorzLineFunc(LL,Y+A,RR-1,CL);
    end;
  end;
end;

procedure TopRoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single);
const S_PI=3.141592;
var T:Double;
    F:Boolean;
 Step:Single;
begin
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.5 else
   if R<20 then Step:=0.1 else
    Step:=0.05;
 T:=-S_PI/2;
 F:=True;
 while T>-S_PI do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X,Y+H);
 Bitmap.LineToFS(X+W,Y+H);
 Bitmap.LineToFS(X+W,Y+R);

 T:=0;
 F:=True;
 while T>-S_PI/2 do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X+R,Y-0.001);
end;

procedure BottomRoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single);
const S_PI=3.141592;
var T:Double;
    F:Boolean;
 Step:Single;
begin
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.5 else
   if R<20 then Step:=0.1 else
    Step:=0.05;
 T:=-S_PI/2;
 F:=True;
 while T>-S_PI do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X,Y+H);
 Bitmap.LineToFS(X+W,Y+H);
 Bitmap.LineToFS(X+W,Y+R);

 T:=0;
 F:=True;
 while T>-S_PI/2 do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X+R,Y-0.001);
end;

procedure RoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Integer;
           Corners:TSXCorners=[crLeftTop,crRightTop,crRightBottom,crLeftBottom]);
const S_PI=3.141592;
var T:Double;
    F:Boolean;
 Step:Single;
begin
 Dec(W); Dec(H);
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.5 else
   if R<20 then Step:=0.1 else
    Step:=0.05;
 if crLeftTop in Corners then
  begin
   T:=-S_PI/2;
   F:=True;
   while T>-S_PI do
    begin
     if F then
      begin
       F:=False;
       Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+R);
      end else
       Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+R);
     T:=T-Step;
    end;
  end else Bitmap.MoveToF(X,Y);
 if crLeftBottom in Corners then
  begin
   Bitmap.LineToFS(X,Y+H-R);
   T:=S_PI;
   F:=True;
   while T>S_PI/2 do
    begin
     if F then
      begin
       F:=False;
       Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+H-R);
      end else
       Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+H-R);
     T:=T-Step;
    end;
  end else Bitmap.LineToFS(X,Y+H);
 if crRightBottom in Corners then
  begin
   Bitmap.LineToFS(X+W-R,Y+H);
   T:=S_PI/2;
   F:=True;
   while T>0 do
    begin
     if F then
      begin
       F:=False;
       Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+H-R);
      end else
       Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+H-R);
     T:=T-Step;
    end;
  end else Bitmap.LineToFS(X+W,Y+H);
 if crRightTop in Corners then
  begin
   Bitmap.LineToFS(X+W,Y+R);
   T:=0;
   F:=True;
   while T>-S_PI/2 do
    begin
     if F then
      begin
       F:=False;
       Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
      end else
       Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
     T:=T-Step;
    end;
  end else Bitmap.LineToFS(X+W,Y);
 if crLeftTop in Corners then
  Bitmap.LineToFS(X+R,Y-0.001) else
   Bitmap.LineToFS(X,Y);
end;

procedure RoundRectangle(Bitmap:TBitmap32;X,Y,W,H,R:Single);
const S_PI=3.141592;
var T:Double;
    F:Boolean;
 Step:Single;
begin
 W:=W-1; H:=H-1;
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.5 else
   if R<20 then Step:=0.1 else
    Step:=0.05;
 T:=-S_PI/2;
 F:=True;
 while T>-S_PI do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X,Y+H-R);

 T:=S_PI;
 F:=True;
 while T>S_PI/2 do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R+R,Y+Sin(T)*R+H-R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R+R,Y+Sin(T)*R+H-R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X+W-R,Y+H);

 T:=S_PI/2;
 F:=True;
 while T>0 do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+H-R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+H-R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X+W,Y+R);

 T:=0;
 F:=True;
 while T>-S_PI/2 do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
    end else
     Bitmap.LineToFS(X+Cos(T)*R-R+W,Y+Sin(T)*R+R);
   T:=T-Step;
  end;
 Bitmap.LineToFS(X+R,Y-0.001);
end;

procedure Ellipse(Bitmap:TBitmap32;X,Y,W,H:Single);
const S_PI=3.141592;
var  T:Double;
     F:Boolean;
 R1,R2:Double;
     R:Double;
  Step:Single;
begin
 W:=W-1; H:=H-1;
 R1:=W/2; R2:=H/2;
 if R1>R2 then R:=R1 else R:=R2;
 if R<3 then Step:=0.7 else
  if R<6 then Step:=0.1 else
   if R<20 then Step:=0.05 else
    Step:=0.02;
 T:=0;
 F:=True;
 while T<2*S_PI do
  begin
   if F then
    begin
     F:=False;
     Bitmap.MoveToF(X+Cos(T)*R1+R1,Y+Sin(T)*R2+R2);
    end else
     Bitmap.LineToFS(X+Cos(T)*R1+R1,Y+Sin(T)*R2+R2);
   T:=T+Step;
  end;
 Bitmap.LineToFS(X+R1*2,Y+R2);
end;

procedure AdvDrawTo(Bitmap:TBitmap32;Dst:TBitmap32);
begin
 if Bitmap.Empty or Dst.Empty then exit;
 AdvBlockTransfer(Dst,0,0,Dst.BoundsRect,Bitmap,Bitmap.BoundsRect,Bitmap.DrawMode,
                  Bitmap.OnPixelCombine);
 Dst.Changed;
end;

procedure AdvDrawTo(Bitmap:TBitmap32;Dst:TBitmap32;DstX,DstY:Integer);
begin
 if Bitmap.Empty or Dst.Empty then exit;
 AdvBlockTransfer(Dst,DstX,DstY,Dst.BoundsRect,Bitmap,Bitmap.BoundsRect,
                  Bitmap.DrawMode,Bitmap.OnPixelCombine);
 Dst.Changed;
end;

procedure BlendBitmap(B:TBitmap32;A:Integer;C:TColor);
var X,Y:Integer;
     PP:PColor32;
     CC:TColor32;
begin
 PP:=@B.Bits[0];
 CC:=Color32(C);
 CC:=SetAlpha(CC,A);
 for X:=0 to B.Width-1 do
  for Y:=0 to B.Height-1 do
  begin
    BlendMem(CC,PP^);
    Inc(PP);
   end;
 EMMS;
end;

procedure SX_BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
var C:Cardinal;
begin
 while Count>0 do
  begin
   C:=Src^;
   C:=SetAlpha(C,MulDiv(AlphaComponent(C),M,255));
   SX_BlendMem(C,Dst^);
   Inc(Src);
   Inc(Dst);
   Dec(Count);
  end;
end;

procedure SX_CombineMem(F: TColor32; var B: TColor32; W: TColor32);
var  A1,R1,G1,B1:Cardinal;
     A2,R2,G2,B2:Cardinal;
 AA,AA2,RR,GG,BB:Cardinal;
begin
 A1:=(B and $FF000000) shr 24;
 R1:=(B and $00FF0000) shr 16;
 G1:=(B and $0000FF00) shr  8;
 B1:=(B and $000000FF);
 A2:=W;
 R2:=(F and $00FF0000) shr 16;
 G2:=(F and $0000FF00) shr  8;
 B2:=(F and $000000FF);
 AA:=A1+A2-Cardinal(MulDiv(A1,A2,255));
 AA2:=MulDiv(255-A2,A1,255);
 RR:=(R2*A2+AA2*R1) div (AA2+A2);
 GG:=(G2*A2+AA2*G1) div (AA2+A2);
 BB:=(B2*A2+AA2*B1) div (AA2+A2);
 B:=Color32(RR,GG,BB,AA);
end;

procedure SX_BlendMem(F: TColor32; var B: TColor32);
var  A1,R1,G1,B1:Cardinal;
     A2,R2,G2,B2:Cardinal;
 AA,AA2,RR,GG,BB:Cardinal;
begin
 A1:=(B and $FF000000) shr 24;
 R1:=(B and $00FF0000) shr 16;
 G1:=(B and $0000FF00) shr  8;
 B1:=(B and $000000FF);
 A2:=(F and $FF000000) shr 24;
 R2:=(F and $00FF0000) shr 16;
 G2:=(F and $0000FF00) shr  8;
 B2:=(F and $000000FF);
 AA:=A1+A2-Cardinal(MulDiv(A1,A2,255));
 AA2:=MulDiv(255-A2,A1,255);
 if A2+AA2=0 then
  begin
   RR:=0; GG:=0; BB:=0;
  end else
   begin
    RR:=(R2*A2+AA2*R1) div (AA2+A2);
    GG:=(G2*A2+AA2*G1) div (AA2+A2);
    BB:=(B2*A2+AA2*B1) div (AA2+A2);
   end;
 B:=Color32(RR,GG,BB,AA);
end;

procedure ColorToRedComponent(Dst,Src:TBitmap32);
var I:Integer;
  D,S:PColor32;
begin
 CheckParams(Dst,Src);
 Dst.SetSize(Src.Width,Src.Height);
 D:=@Dst.Bits[0];
 S:=@Src.Bits[0];
 for I:=0 to Src.Width*Src.Height-1 do
  begin
   D^:=Color32(Intensity(S^),0,0,AlphaComponent(S^));
   Inc(S); Inc(D);
  end;
 Dst.Changed;
end;

procedure ColorToGreenComponent(Dst, Src: TBitmap32);
var I:Integer;
  D,S:PColor32;
begin
 CheckParams(Dst,Src);
 Dst.SetSize(Src.Width,Src.Height);
 D:=@Dst.Bits[0];
 S:=@Src.Bits[0];
 for I:=0 to Src.Width*Src.Height-1 do
  begin
   D^:=Color32(0,Intensity(S^),0,AlphaComponent(S^));
   Inc(S); Inc(D);
  end;
 Dst.Changed;
end;

procedure ColorToBlueComponent(Dst,Src:TBitmap32);
var I:Integer;
  D,S:PColor32;
begin
 CheckParams(Dst,Src);
 Dst.SetSize(Src.Width,Src.Height);
 D:=@Dst.Bits[0];
 S:=@Src.Bits[0];
 for I:=0 to Src.Width*Src.Height-1 do
  begin
   D^:=Color32(0,0,Intensity(S^),AlphaComponent(S^));
   Inc(S); Inc(D);
  end;
 Dst.Changed;
end;

procedure AdvBlendBlock(Dst:TBitmap32;DstRect:TRect;Src:TBitmap32;SrcX,SrcY:Integer;
           CombineOp:TDrawMode;CombineCallBack:TPixelCombineEvent);
var SrcP,DstP:PColor32;
        SP,DP:PColor32;
     W,I,DstY:Integer;
begin
 W:=DstRect.Right-DstRect.Left;
 SrcP:=Src.PixelPtr[SrcX,SrcY];
 DstP:=Dst.PixelPtr[DstRect.Left,DstRect.Top];
 for DstY:=DstRect.Top to DstRect.Bottom-1 do
  begin
   case CombineOp of
    dmOpaque:MoveLongWord(SrcP^, DstP^, W);
    dmBlend:
        {if Src.MasterAlpha >= 255 then BlendLine(SrcP, DstP, W)
        else} SX_BlendLineEx(SrcP, DstP, W, Src.MasterAlpha);
    else //  dmCustom:
      begin
        SP := SrcP;
        DP := DstP;
        for I := 0 to W - 1 do
        begin
          CombineCallBack(SP^, DP^, Src.MasterAlpha);
          Inc(SP); Inc(DP);
        end;
      end;
   end;
   Inc(SrcP,Src.Width);
   Inc(DstP,Dst.Width);
  end;
end;

procedure AdvBlockTransfer(Dst:TBitmap32;DstX:Integer;DstY:Integer;DstClip:TRect;
           Src:TBitmap32;SrcRect:TRect;CombineOp:TDrawMode;CombineCallBack:TPixelCombineEvent);
var SrcX,SrcY:Integer;
begin
 if Src.Empty then exit;
 if (CombineOp=dmCustom) and not Assigned(CombineCallBack) then
  CombineOp:=dmOpaque;
 if (CombineOp=dmBlend) and (Src.MasterAlpha=0) then exit;
 SrcX:=SrcRect.Left;
 SrcY:=SrcRect.Top;
 IntersectRect(DstClip, DstClip, Dst.BoundsRect);
 IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
 OffsetRect(SrcRect, DstX - SrcX, DstY - SrcY);
 IntersectRect(SrcRect, DstClip, SrcRect);
 DstClip := SrcRect;
 OffsetRect(SrcRect, SrcX - DstX, SrcY - DstY);
 if not IsRectEmpty(SrcRect) then
  try
   AdvBlendBlock(Dst,DstClip,Src,SrcRect.Left,SrcRect.Top,CombineOp,CombineCallBack);
  finally
   EMMS;
  end;
end;

procedure GetRenderedTextSize(Bitmap:TBitmap32;const Text:String;AALevel:Integer;var Width,Height:Integer);
var SZ:TSize;
begin
 Bitmap.Font.Size:=Bitmap.Font.Size shl AALevel;
 SZ:=Bitmap.TextExtent(Text);
 Width:=SZ.cx shr AALevel;
 Height:=SZ.cy shr AALevel;
 if Width shl AALevel<SZ.cx then
  Inc(Width);
 if Height shl AALevel<SZ.cy then
  Inc(Height);
 Bitmap.Font.Size:=Bitmap.Font.Size shr AALevel;
end;

procedure GetRenderedTextSize(Canvas:TCanvas;const Text:String;AALevel:Integer;var Width,Height:Integer);
var SZ:TSize;
begin
 Canvas.Font.Size:=Canvas.Font.Size shl AALevel;
 SZ:=Canvas.TextExtent(Text);
 Width:=SZ.cx shr AALevel;
 Height:=SZ.cy shr AALevel;
 if Width shl AALevel<SZ.cx then
  Inc(Width);
 if Height shl AALevel<SZ.cy then
  Inc(Height);
 Canvas.Font.Size:=Canvas.Font.Size shr AALevel;
end;

//Taken as private mothod of TBitmap32
procedure TextScaleDown(Bitmap:TBitmap32;const B,B2:TBitmap32;const N:Integer;const Color:TColor32);
var I, J, X, Y, P, Q, Sz, S: Integer;
    Src: PColor32;
    Dst: PColor32;
begin
  Sz := 1 shl N - 1;
  Dst := B.PixelPtr[0, 0];
  for J := 0 to B.Height - 1 do
  begin
    Y := J shl N;
    for I := 0 to B.Width - 1 do
    begin
      X := I shl N;
      S := 0;
      for Q := Y to Y + Sz do
      begin
        Src := B2.PixelPtr[X, Q];
        for P := X to X + Sz do
        begin
          S := S + Integer(Src^ and $000000FF);
          Inc(Src);
        end;
      end;
      S := S shr N shr N;
      Dst^ := TColor32(S shl 24) + Color;
      Inc(Dst);
    end;
  end;
end;

procedure DrawSmoothText(Bitmap:TBitmap32;const Text:String;var TextRect:TRect;
           Flags:UINT;AALevel:Integer;Color:TColor32);
var        R:TRect;
        B,B2:TBitmap32;
 StockBitmap:TBitmap;
 StockCanvas:TCanvas;
       Alpha:Integer;

 function ShrAALevelToMax(A:Integer):Integer;
 begin
  Result:=A shr AALevel;
  if Result shl AALevel<A then Inc(Result);
 end;

begin
 Alpha:=Color shr 24;
 Color:=Color and $00FFFFFF;
 AALevel:=Constrain(AALevel,1,4);
 R:=Rect(0,0,(TextRect.Right-TextRect.Left) shl AALevel,
             (TextRect.Bottom-TextRect.Top) shl AALevel);
 if Flags and DT_CALCRECT<>0 then
  begin
   Bitmap.Font.Size:=Bitmap.Font.Size shl AALevel;
   DrawText(Bitmap.Handle,PChar(Text),-1,R,Flags);
   TextRect.Right:=TextRect.Left+ShrAALevelToMax(R.Right);
   TextRect.Bottom:=TextRect.Top+ShrAALevelToMax(R.Bottom);
   Bitmap.Font.Size:=Bitmap.Font.Size shr AALevel;
   exit;
  end;
 B:=TBitmap32.Create;
 StockBitmap:=TBitmap.Create;
 try
  StockCanvas:=StockBitmap.Canvas;
  StockCanvas.Lock;
  try
   StockCanvas.Font:=Bitmap.Font;
   StockCanvas.Font.Size:=Bitmap.Font.Size shl AALevel;
   B2:=TBitmap32.Create;
   try
    B2.SetSize(R.Right,R.Bottom);
    B2.Clear(0);
    B2.Font:=StockCanvas.Font;
    B2.Font.Color:=clWhite;
    B2.Textout(R,Flags,Text);
    B.SetSize(ShrAALevelToMax(R.Right),ShrAALevelToMax(R.Bottom));
    TextScaleDown(Bitmap,B,B2,AALevel,Color);
   finally
    B2.Free;
   end;
  finally
   StockCanvas.Unlock;
  end;
  B.DrawMode:=dmBlend;
  B.MasterAlpha:=Alpha;
  B.CombineMode:=cmMerge;
  B.DrawTo(Bitmap,TextRect.Left,TextRect.Top);
 finally
  StockBitmap.Free;
  B.Free;
 end;
end;

//Taken from GR32
procedure TextBlueToAlpha(const B:TBitmap32;const Color:TColor32);
var I:Integer;
    P:PColor32;
    C:TColor32;
begin
 P:=@B.Bits[0];
 for I:=0 to B.Width*B.Height-1 do
  begin
   C:=P^;
   if C<>$00FFFFFF then
    begin
     C:=($FF-(P^ and $FF)) shl 24;
     C:=C or Color;
     P^:=C;
    end;
   Inc(P);
  end;
end;

procedure DrawAlphaText(Bitmap:TBitmap32;const Text:String;var TextRect:TRect;
           Flags:UINT;Color:TColor32);
var  B:TBitmap32;
     R:TRect;
 Alpha:TColor32;
begin
 if Bitmap.Empty then exit;
 Alpha:=Color shr 24;
 Color:=Color and $00FFFFFF;
 B:=TBitmap32.Create;
 try
  TextBlueToAlpha(B,Color);
  Bitmap.Textout(TextRect,Flags or DT_CALCRECT,Text);
  B.SetSize(TextRect.Right-TextRect.Left,TextRect.Bottom-TextRect.Top);
  B.Font:=Bitmap.Font;
  B.Clear($00FFFFFF);
  B.Font.Color:=clBlack;
  R:=TextRect;
  OffsetRect(R,-TextRect.Left,-TextRect.Top);
  B.Textout(R,Flags,Text);
  OffsetRect(R,TextRect.Left,TextRect.Top);
  TextRect:=R;
  TextBlueToAlpha(B,Color);
  B.DrawMode:=dmBlend;
  B.MasterAlpha:=Alpha;
  B.CombineMode:=cmMerge;
  B.DrawTo(Bitmap,TextRect.Left,TextRect.Top);
 finally
  B.Free;
 end;
end;

procedure DrawSmoothText(Canvas:TCanvas;const Text:String;var TextRect:TRect;
           Flags:UINT;AALevel:Integer);
var R:TRect;

 function ShrAALevelToMax(A:Integer):Integer;
 begin
  Result:=A shr AALevel;
  if Result shl AALevel<A then Inc(Result);
 end;

begin
 AALevel:=Constrain(AALevel,1,4);
 R:=Rect(0,0,(TextRect.Right-TextRect.Left) shl AALevel,
             (TextRect.Bottom-TextRect.Top) shl AALevel);
 if Flags and DT_CALCRECT<>0 then
  begin
   Canvas.Font.Size:=Canvas.Font.Size shl AALevel;
   DrawText(Canvas.Handle,PChar(Text),-1,R,Flags);
   TextRect.Right:=TextRect.Left+ShrAALevelToMax(R.Right);
   TextRect.Bottom:=TextRect.Top+ShrAALevelToMax(R.Bottom);
   Canvas.Font.Size:=Canvas.Font.Size shr AALevel;
  end;
end;


function IsInteger(A:Single):Boolean;
begin
 Result:=A+0.001-Floor(A)<0.01;
end;

procedure B32_Rectangle(B:TBitmap32;X1,Y1,X2,Y2:Single;Color:TColor32);
begin
 //B.FillRect
end;

end.
