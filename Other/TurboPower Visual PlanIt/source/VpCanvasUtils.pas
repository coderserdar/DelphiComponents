{*********************************************************}
{*               VPCANVASUTILS.PAS 1.03                  *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{
  The drawing of Visual PlanIt controls supports rendering of the controls
  to arbitrary rectangles in a Canvas. In addition, the controls can be
  rendered rotated in 90 degree increments.

  Instead of rendering the VisualPlanIt control and then rotating it and
  translating it, the control is drawn taking account the translation and
  rotation.  This unit contains a helper class, TVpExCanvas that is used as
  a go-between for the component and the TCanvas that it needs to render
  itself to.

  The component will use members of the TVpExCanvas class to draw to the
  TCanvas.  This class will take the rotation and viewport (the rectangle
  in which the component should be rendered) into account and then draw
  the correct shape (line, text or whatever) to the canvas.

  There are three parameters of the TVpExCanvas class that must be
  initialized.  These are Angle, ViewPort and Canvas.  Angle specifies the
  rotation angle to use when drawing to the Canvas.  ViewPort specifies the
  rectangle in which all the drawing should take place.  Canvas is the
  TCanvas class to draw on.

  For the most part, methods in TVpExCanvas are analagous to methods in
  TCanvas.  There are some additional convenience methods to make dealing
  with the rotated canvas easier.

  In addition to the methods of the TVpExCanvas class, static methods are
  provided to access the TVpExCanvas functionality without having create
  and instance of the class.  This will use a built global TVpExCanvas that
  is created in the initialization section of this unit and destroyed in
  its finalization.

  -----------------------------------------------------------------------------

  VpCanvasUtils also contains an additional helper class, TVpLineWrapper.
  This class is used to wrap text within rectangles and irregularily shaped
  regions on a canvas.  This is used primarily by the TVpDayView component
  to draw multiline events (If icons are used in the TVpDayView, this is
  the class that wraps the text around the icons).

  The TVpLineWrapper class supports the rotation and viewport capablilities
  provided by the TVpExCanvas class.
  
  Like TVpExCanvas, static methods are provided to access the TVpLineWrapper
  functionality without having to explicitly create and destroy an instance
  of the class. 

}

{$I Vp.INC}

unit VpCanvasUtils;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Controls,
  Messages,
  VpException,
  VpSR,
  VpBase;

type
  TVpPaletteArray = array [0..255] of TPALETTEENTRY;

  { !!.01 Begin changes !!.01 }

  TVpExCanvas = class (TObject)                                          
    private                                                              
      FAngle    : TVpRotationAngle;                                      
      FCanvas   : TCanvas;                                               
      FViewPort : TRect;                                                 

    protected                                                            
      procedure DrawRotatedText (x, y   : Integer;
                                 Text   : string;
                                 Rotate : Boolean);
      procedure Swap (var a, b : Integer);

    public                                                               
      constructor Create;                                                

      function NormalizeRectangle (const ARect : TRect) : TRect;         
      function RotatePoint (const APoint : TPoint) : TPoint;             
      function RotateRectangle (const ARect : TRect) : TRect;            
      function ViewportWidth : Integer;
      function ViewportHeight : Integer;
      function ViewportLeft : Integer;                                   
      function ViewportRight : Integer;                                  
      function ViewportTop : Integer;                           
      function ViewportBottom : Integer;                        
      procedure Arc (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
      procedure BrushCopy (const Dest   : TRect;                      
                                 Bitmap : TBitmap;
                           const Source : TRect;
                                 AColor : TColor);                    
      procedure Chord (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
      procedure CopyRect (      Dest   : TRect;
                                Canvas : TCanvas;
                          const Source : TRect);
      procedure Draw (X, Y    : Integer;
                      Graphic : TGraphic);
      procedure DrawFocusRect (const ARect : TRect);
      procedure Ellipse (X1, Y1, X2, Y2 : Integer); overload;
      procedure Ellipse (const ARect : TRect); overload;
      procedure FillRect (const ARect : TRect);
      procedure FloodFill (X, Y      : Integer;
                           AColor    : TColor;
                           FillStyle : TFillStyle);                   
      procedure FrameRect (const ARect : TRect);                      
      procedure LineTo (X, Y : Integer);
      procedure MoveTo (X, Y : Integer);                              
      procedure Pie (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);       
      procedure PolyBezier (const Points : array of TPoint);          
      procedure PolyBezierTo (const Points : array of TPoint);        
      procedure Polygon (Points : array of TPoint);                   
      procedure Polyline (Points : array of TPoint);                  
      procedure Rectangle (X1, Y1, X2, Y2 : Integer); overload;       
      procedure Rectangle (const ARect : TRect); overload;            
      procedure RoundRect (X1, Y1, X2, Y2, X3, Y3 : Integer);         
      procedure StretchDraw (const ARect   : TRect;                   
                                   Graphic : TGraphic);               
      procedure TextOut (      X, Y : Integer;
                         const Text : string);
      procedure TextRect (      ARect : TRect;
                                X, Y  : Integer;
                          const Text  : string);                      
      function  GetPixel (const x : Integer;                          
                          const y : Integer) : TColor;                
      procedure SetPixel (x      : Integer;
                          y      : Integer;
                          AColor : TColor);
      procedure CenteredTextOut (      ARect : TRect;
                                 const Text  : string);
      procedure TextOutAtPoint (      X, Y : Integer;
                                const Text : string);
      function RGBToTColor (Red, Green, Blue : Byte) : TColor;
      procedure TColorToRGB (    Color : TColor;
                             var Red   : Byte;
                             var Green : Byte;
                             var Blue  : Byte);
      procedure CachePalette (    ABitmap : TBitmap;
                              var PaletteEntries : TVpPaletteArray);
      function GetBmpPixel (ABitmap      : TBitmap;
                            PaletteCache : TVpPaletteArray;
                            x            : Integer;
                            y            : Integer) : TColor;
      procedure SetBmpPixel (ABitmap      : TBitmap;
                             PaletteCache : TVpPaletteArray;
                             x            : Integer;
                             y            : Integer;
                             AColor       : TColor);

      property Viewport : TRect read FViewport write FViewport;

    published
      property Angle : TVpRotationAngle read FAngle write FAngle
               default ra0;
      property Canvas : TCanvas read FCanvas write FCanvas;
  end;

  TVpOnFindWordBreak = procedure (    Sender    : TObject;               
                                      AString   : string;                
                                      APosition : Integer;               
                                  var IsBreak   : Boolean) of object;    

  TVpLineWrapper = class (TObject)
    private                                                              
      FAngle           : TVpRotationAngle;                                      
      FMinChars        : Integer;
      FTextMargin      : Integer;
      FOnFindWordBreak : TVpOnFindWordBreak;
      FViewPort        : TRect;

    protected
      function FindEndingPos (ARegion  : HRGN;                           
                              LineSize : Integer;                        
                              HPos     : Integer;                        
                              YPos     : Integer) : TPoint;              
      function FindNextStartingPoint (    ARegion  : HRGN;               
                                          LineSize : Integer;            
                                      var HPos     : Integer;            
                                      var YPos     : Integer) : Boolean; 
      function FindWordBreaks (AString : string;                         
                               CharPos : Integer) : Integer;
      function FitStringInRect (    ACanvas     : TCanvas;               
                                    RectWidth   : Integer;               
                                    AvgCharSize : Integer;               
                                var AString     : string;
                                var CharsOut    : Integer) : string;      
      function GetAverageCharSize (ACanvas : TCanvas) : Integer;         
      function GetNextRectangle (    ARegion : HRGN;                     
                                     LineSize : Integer;                 
                                     AvgCharSize : Integer;              
                                 var HPos    : Integer;                  
                                 var LinePos : Integer) : TRect;         
      function IsWordBreak (AString : string;                            
                            CharPos : Integer) : Boolean;                
      function NextChar (AString : string; CharPos : Integer) : Char;    
      function PrevChar (AString : string; CharPos : Integer) : Char;    
      function ThisChar (AString : string; CharPos : Integer) : Char;    

    public                                                               
      constructor Create;                                                

      function RenderTextToCanvas (ACanvas : TCanvas;
                                   ARect   : TRect;
                                   AString : string) : Integer;
      function RenderTextToCanvasRegion (ACanvas : TCanvas;
                                         ARegion : HRGN;
                                         AString : string) : Integer;

      property MinChars : Integer read FMinChars write FMinChars         
                          default 5;                                     
      property TextMargin : Integer read FTextMargin write FTextMargin   
               default 3;                                                

      property Viewport : TRect read FViewport write FViewport;

    published
      property Angle : TVpRotationAngle read FAngle write FAngle
               default ra0;
      property OnFindWordBreak : TVpOnFindWordBreak
               read FOnFindWordBreak write FOnFindWordBreak;
  end;

function TPSNormalizeRectangle (const ARect : TRect) : TRect;

function TPSRotatePoint (const Angle    : TVpRotationAngle;
                         const ViewPort : TRect;
                         const APoint   : TPoint) : TPoint;

function TPSRotateRectangle (const Angle    : TVpRotationAngle;
                             const ViewPort : TRect;
                             const ARect    : TRect) : TRect;

function TPSViewportWidth (const Angle    : TVpRotationAngle;
                           const ViewPort : TRect) : Integer;

function TPSViewportHeight (const Angle    : TVpRotationAngle;
                            const ViewPort : TRect) : Integer;

function TPSViewportLeft (const Angle    : TVpRotationAngle;
                          const ViewPort : TRect) : Integer;
function TPSViewportRight (const Angle    : TVpRotationAngle;
                           const ViewPort : TRect) : Integer;
function TPSViewportTop (const Angle    : TVpRotationAngle;
                         const ViewPort : TRect) : Integer;
function TPSViewportBottom (const Angle    : TVpRotationAngle;
                            const ViewPort : TRect) : Integer;


procedure TPSArc (      ACanvas                        : TCanvas;
                  const Angle                          : TVpRotationAngle;
                  const ViewPort                       : TRect;
                        X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);

procedure TPSBrushCopy (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const Dest     : TRect;
                              Bitmap   : TBitmap;
                        const Source   : TRect;
                              AColor   : TColor);

procedure TPSChord (      ACanvas                        : TCanvas;
                    const Angle                          : TVpRotationAngle;
                    const ViewPort                       : TRect;
                          X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);

procedure TPSCopyRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             Dest     : TRect;
                             Canvas   : TCanvas;
                       const Source   : TRect);

procedure TPSDraw (      ACanvas  : TCanvas;
                   const Angle    : TVpRotationAngle;
                   const ViewPort : TRect;
                         X, Y     : Integer;
                         Graphic  : TGraphic);

procedure TPSDrawFocusRect (      ACanvas  : TCanvas;
                            const Angle    : TVpRotationAngle;
                            const ViewPort : TRect;
                            const ARect    : TRect);

procedure TPSEllipse (      ACanvas        : TCanvas;
                      const Angle          : TVpRotationAngle;
                      const ViewPort       : TRect;
                            X1, Y1, X2, Y2 : Integer); overload;

procedure TPSEllipse (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                      const ARect    : TRect); overload;

procedure TPSFillRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                       const ARect    : TRect);

procedure TPSFloodFill (      ACanvas   : TCanvas;
                        const Angle     : TVpRotationAngle;
                        const ViewPort  : TRect;
                              X, Y      : Integer;
                              AColor    : TColor;
                              FillStyle : TFillStyle);

procedure TPSFrameRect (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const ARect    : TRect);

procedure TPSLineTo (      ACanvas  : TCanvas;
                     const Angle    : TVpRotationAngle;
                     const ViewPort : TRect;
                           X, Y     : Integer);

procedure TPSMoveTo (      ACanvas  : TCanvas;
                     const Angle    : TVpRotationAngle;
                     const ViewPort : TRect;
                           X, Y     : Integer);

procedure TPSPie (      ACanvas                        : TCanvas;
                  const Angle                          : TVpRotationAngle;
                  const ViewPort                       : TRect;
                        X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);

procedure TPSPolyBezier (      ACanvas  : TCanvas;
                         const Angle    : TVpRotationAngle;
                         const ViewPort : TRect;
                         const Points   : array of TPoint);

procedure TPSPolyBezierTo (      ACanvas  : TCanvas;
                           const Angle    : TVpRotationAngle;
                           const ViewPort : TRect;
                           const Points   : array of TPoint);

procedure TPSPolygon (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                            Points   : array of TPoint);

procedure TPSPolyline (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             Points   : array of TPoint);

procedure TPSRectangle (      ACanvas        : TCanvas;
                        const Angle          : TVpRotationAngle;
                        const ViewPort       : TRect;
                              X1, Y1, X2, Y2 : Integer); overload;

procedure TPSRectangle (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const ARect    : TRect); overload;

procedure TPSRoundRect (      ACanvas                : TCanvas;
                        const Angle                  : TVpRotationAngle;
                        const ViewPort               : TRect;
                              X1, Y1, X2, Y2, X3, Y3 : Integer);

procedure TPSStretchDraw (      ACanvas  : TCanvas;
                          const Angle    : TVpRotationAngle;
                          const ViewPort : TRect;
                          const ARect    : TRect;
                                Graphic  : TGraphic);

procedure TPSTextOut (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                            X, Y     : Integer;
                      const Text     : string);

procedure TPSTextRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             ARect    : TRect;
                             X, Y     : Integer;
                       const Text     : string);

function  TPSGetPixel (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                       const x        : Integer;
                       const y        : Integer) : TColor;

procedure TPSSetPixel (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             x        : Integer;
                             y        : Integer;
                             AColor   : TColor);

procedure TPSCenteredTextOut (      ACanvas  : TCanvas;
                              const Angle    : TVpRotationAngle;
                              const ViewPort : TRect;
                                    ARect    : TRect;
                              const Text     : string);

procedure TPSTextOutAtPoint (      ACanvas  : TCanvas;
                             const Angle    : TVpRotationAngle;
                             const ViewPort : TRect;
                                   X, Y     : Integer;
                             const Text     : string);

function RGBToTColor (Red, Green, Blue : Byte) : TColor;

procedure TColorToRGB (    Color : TColor;
                       var Red   : Byte;
                       var Green : Byte;
                       var Blue  : Byte);

procedure TPSCachePalette (ABitmap : TBitmap;
                           var PaletteEntries : TVpPaletteArray);

function TPSGetBmpPixel (ABitmap      : TBitmap;
                         PaletteCache : TVpPaletteArray;
                         x            : Integer;
                         y            : Integer) : TColor;

procedure TPSSetBmpPixel (ABitmap      : TBitmap;
                          PaletteCache : TVpPaletteArray;
                          x            : Integer;
                          y            : Integer;
                          AColor       : TColor);

function RenderTextToRect (      ACanvas  : TCanvas;
                           const Angle    : TVpRotationAngle;
                           const Viewport : TRect;
                                 ARect    : TRect;
                                 AString  : string) : Integer;

function RenderTextToRegion (      ACanvas  : TCanvas;
                             const Angle    : TVpRotationAngle;
                             const Viewport : TRect;
                                   ARegion  : HRGN;
                                   AString  : string) : Integer;

implementation

var
  VpRotatedCanvas : TVpExCanvas;
  VpTextRenderer  : TVpLineWrapper;                                 

{ Function based TVpExCanvas Access ***************************************** }

procedure SetTVpExCanvasAV (const Angle    : TVpRotationAngle;
                            const Viewport : TRect);
begin
  VpRotatedCanvas.Angle := Angle;
  VpRotatedCanvas.Viewport := Viewport;
end;

procedure SetTVpExCanvasAVC (      ACanvas  : TCanvas;
                             const Angle    : TVpRotationAngle;
                             const ViewPort : TRect);
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  VpRotatedCanvas.Canvas := ACanvas;
end;

function TPSNormalizeRectangle (const ARect : TRect) : TRect;
begin
  Result := VpRotatedCanvas.NormalizeRectangle (ARect);
end;

function TPSRotatePoint (const Angle    : TVpRotationAngle;
                         const ViewPort : TRect;
                         const APoint   : TPoint) : TPoint;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.RotatePoint (APoint);
end;

function TPSRotateRectangle (const Angle    : TVpRotationAngle;
                             const ViewPort : TRect;
                             const ARect    : TRect) : TRect;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.RotateRectangle (ARect);
end;

function TPSViewportWidth (const Angle    : TVpRotationAngle;
                           const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportWidth;
end;

function TPSViewportHeight (const Angle    : TVpRotationAngle;
                            const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportHeight;
end;

function TPSViewportLeft (const Angle    : TVpRotationAngle;
                          const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportLeft;
end;

function TPSViewportRight (const Angle    : TVpRotationAngle;
                           const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportRight;
end;

function TPSViewportTop (const Angle    : TVpRotationAngle;
                         const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportTop;
end;

function TPSViewportBottom (const Angle    : TVpRotationAngle;
                            const ViewPort : TRect) : Integer;
begin
  SetTVpExCanvasAV (Angle, ViewPort);
  Result := VpRotatedCanvas.ViewportBottom;
end;


procedure TPSArc (      ACanvas                        : TCanvas;
                  const Angle                          : TVpRotationAngle;
                  const ViewPort                       : TRect;
                        X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Arc (X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TPSBrushCopy (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const Dest     : TRect;
                              Bitmap   : TBitmap;
                        const Source   : TRect;
                              AColor   : TColor);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.BrushCopy (Dest, Bitmap, Source, AColor);
end;

procedure TPSChord (      ACanvas                        : TCanvas;
                    const Angle                          : TVpRotationAngle;
                    const ViewPort                       : TRect;
                          X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Chord (X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TPSCopyRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             Dest     : TRect;
                             Canvas   : TCanvas;
                       const Source   : TRect);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.CopyRect (Dest, Canvas, Source);
end;

procedure TPSDraw (      ACanvas  : TCanvas;
                   const Angle    : TVpRotationAngle;
                   const ViewPort : TRect;
                         X, Y     : Integer;
                         Graphic  : TGraphic);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Draw (X, Y, Graphic);
end;

procedure TPSDrawFocusRect (      ACanvas  : TCanvas;
                            const Angle    : TVpRotationAngle;
                            const ViewPort : TRect;
                            const ARect    : TRect);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.DrawFocusRect (ARect);
end;

procedure TPSEllipse (      ACanvas        : TCanvas;
                      const Angle          : TVpRotationAngle;
                      const ViewPort       : TRect;
                            X1, Y1, X2, Y2 : Integer); overload;
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Ellipse (X1, Y1, X2, Y2);
end;

procedure TPSEllipse (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                      const ARect    : TRect); overload;
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Ellipse (ARect);
end;

procedure TPSFillRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                       const ARect    : TRect);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.FillRect (ARect);
end;

procedure TPSFloodFill (      ACanvas   : TCanvas;
                        const Angle     : TVpRotationAngle;
                        const ViewPort  : TRect;
                              X, Y      : Integer;
                              AColor    : TColor;
                              FillStyle : TFillStyle);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.FloodFill (X, Y, AColor, FillStyle);
end;

procedure TPSFrameRect (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const ARect    : TRect);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.FrameRect (ARect);
end;

procedure TPSLineTo (      ACanvas  : TCanvas;
                     const Angle    : TVpRotationAngle;
                     const ViewPort : TRect;
                           X, Y     : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.LineTo (X, Y);
end;

procedure TPSMoveTo (      ACanvas  : TCanvas;
                     const Angle    : TVpRotationAngle;
                     const ViewPort : TRect;
                           X, Y     : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.MoveTo (X, Y);
end;

procedure TPSPie (      ACanvas                        : TCanvas;
                  const Angle                          : TVpRotationAngle;
                  const ViewPort                       : TRect;
                        X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Pie (X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TPSPolyBezier (      ACanvas  : TCanvas;
                         const Angle    : TVpRotationAngle;
                         const ViewPort : TRect;
                         const Points   : array of TPoint);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.PolyBezier (Points);
end;

procedure TPSPolyBezierTo (      ACanvas  : TCanvas;
                           const Angle    : TVpRotationAngle;
                           const ViewPort : TRect;
                           const Points   : array of TPoint);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.PolyBezierTo (Points);
end;

procedure TPSPolygon (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                            Points   : array of TPoint);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Polygon (Points);
end;

procedure TPSPolyline (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             Points   : array of TPoint);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Polyline (Points);
end;

procedure TPSRectangle (      ACanvas        : TCanvas;
                        const Angle          : TVpRotationAngle;
                        const ViewPort       : TRect;
                              X1, Y1, X2, Y2 : Integer); overload;
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Rectangle (X1, Y1, X2, Y2);
end;

procedure TPSRectangle (      ACanvas  : TCanvas;
                        const Angle    : TVpRotationAngle;
                        const ViewPort : TRect;
                        const ARect    : TRect); overload;
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.Rectangle (ARect);
end;

procedure TPSRoundRect (      ACanvas                : TCanvas;
                        const Angle                  : TVpRotationAngle;
                        const ViewPort               : TRect;
                              X1, Y1, X2, Y2, X3, Y3 : Integer);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.RoundRect (X1, Y1, X2, Y2, X3, Y3);
end;

procedure TPSStretchDraw (      ACanvas  : TCanvas;
                          const Angle    : TVpRotationAngle;
                          const ViewPort : TRect;
                          const ARect    : TRect;
                                Graphic  : TGraphic);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.StretchDraw (ARect, Graphic);
end;

procedure TPSTextOut (      ACanvas  : TCanvas;
                      const Angle    : TVpRotationAngle;
                      const ViewPort : TRect;
                            X, Y     : Integer;
                      const Text     : string);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.TextOut (X, Y, Text);
end;

procedure TPSTextRect (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             ARect    : TRect;
                             X, Y     : Integer;
                       const Text     : string);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.TextRect (ARect, X, Y, Text);
end;

function  TPSGetPixel (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                       const x        : Integer;
                       const y        : Integer) : TColor;
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  Result := VpRotatedCanvas.GetPixel (x, y);
end;

procedure TPSSetPixel (      ACanvas  : TCanvas;
                       const Angle    : TVpRotationAngle;
                       const ViewPort : TRect;
                             x        : Integer;
                             y        : Integer;
                             AColor   : TColor);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.SetPixel (x, y, AColor);
end;

procedure TPSCenteredTextOut (      ACanvas  : TCanvas;
                              const Angle    : TVpRotationAngle;
                              const ViewPort : TRect;
                                    ARect    : TRect;
                              const Text     : string);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.CenteredTextOut (ARect, Text);
end;

procedure TPSTextOutAtPoint (      ACanvas  : TCanvas;
                             const Angle    : TVpRotationAngle;
                             const ViewPort : TRect;
                                   X, Y     : Integer;
                             const Text     : string);
begin
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  VpRotatedCanvas.TextOutAtPoint (X, Y, Text);
end;

function RGBToTColor (Red, Green, Blue : Byte) : TColor;
begin
  Result := VpRotatedCanvas.RGBToTColor (Red, Green, Blue);
end;

procedure TColorToRGB (    Color : TColor;
                       var Red   : Byte;
                       var Green : Byte;
                       var Blue  : Byte);
begin
  VpRotatedCanvas.TColorToRGB (Color, Red, Green, Blue);
end;

procedure TPSCachePalette (    ABitmap        : TBitmap;
                           var PaletteEntries : TVpPaletteArray);
begin
  VpRotatedCanvas.CachePalette (ABitmap, PaletteEntries);
end;

function TPSGetBmpPixel (ABitmap      : TBitmap;
                         PaletteCache : TVpPaletteArray;
                         x            : Integer;
                         y            : Integer) : TColor;
begin
  Result := VpRotatedCanvas.GetBmpPixel (ABitmap, PaletteCache, x, y);
end;

procedure TPSSetBmpPixel (ABitmap      : TBitmap;
                          PaletteCache : TVpPaletteArray;
                          x            : Integer;
                          y            : Integer;
                          AColor       : TColor);
begin
  VpRotatedCanvas.SetBmpPixel (ABitmap, PaletteCache, x, y, AColor);
end;

function RenderTextToRect (      ACanvas  : TCanvas;
                           const Angle    : TVpRotationAngle;
                           const Viewport : TRect;
                                 ARect    : TRect;
                                 AString  : string) : Integer;
begin
  VpTextRenderer.Angle    := Angle;
  VpTextRenderer.Viewport := Viewport;
  Result := VpTextRenderer.RenderTextToCanvas (ACanvas, ARect, AString);
end;

function RenderTextToRegion (      ACanvas  : TCanvas;
                             const Angle    : TVpRotationAngle;
                             const Viewport : TRect;
                                   ARegion  : HRGN;
                                   AString  : string) : Integer;
begin
  VpTextRenderer.Angle    := Angle;
  VpTextRenderer.Viewport := Viewport;
  Result := VpTextRenderer.RenderTextToCanvasRegion (ACanvas, ARegion,
                                                     AString);
end;

{ TVpExCanvas *************************************************************** }

constructor TVpExCanvas.Create;
begin
  inherited Create;

  FAngle    := ra0;
  FViewPort := Rect (0, 0, 0, 0);
  FCanvas   := nil;
end;

procedure TVpExCanvas.Swap (var a, b : Integer);
var
  t : Integer;

begin
  t := a;
  a := b;
  b := t;
end;

function TVpExCanvas.NormalizeRectangle (const ARect : TRect) : TRect;
begin
  Result := ARect;
  if Result.Left > Result.Right then
    Swap (Result.Left, Result.Right);

  if Result .Top > Result.Bottom then
    Swap (Result.Top, Result.Bottom);
end;

function TVpExCanvas.RotatePoint (const APoint : TPoint) : TPoint;
begin
  Result := APoint;

  case Angle of
    ra0   :
      Result := Point (APoint.X,
                       APoint.Y);

    ra90  :
      Result := Point (ViewPort.Left + ViewPort.Right - APoint.Y,
                       APoint.X);

    ra180 :
      Result := Point (ViewPort.Left + ViewPort.Right - APoint.X,
                       ViewPort.Top + ViewPort.Bottom - APoint.Y);

    ra270 :
      Result := Point (APoint.Y,
                       ViewPort.Top + ViewPort.Bottom - APoint.X);
  end;
end;

function TVpExCanvas.RotateRectangle (const ARect : TRect) : TRect;
begin
  Result := ARect;

  case Angle of
    ra0   :
      Result := TPSNormalizeRectangle (Rect (ARect.Left,
                                             ARect.Top,
                                             ARect.Right,
                                             ARect.Bottom));

    ra90  :
      Result := TPSNormalizeRectangle (Rect (ViewPort.Left + ViewPort.Right - ARect.Top,
                                             ARect.Left,
                                             ViewPort.Left + ViewPort.Right - ARect.Bottom,
                                             ARect.Right));

    ra180 :
      Result := TPSNormalizeRectangle (Rect (ViewPort.Left + ViewPort.Right - ARect.Left,
                                             ViewPort.Top + ViewPort.Bottom - ARect.Top,
                                             ViewPort.Left + ViewPort.Right - ARect.Right,
                                             ViewPort.Top + ViewPort.Bottom - ARect.Bottom));

    ra270 :
      Result := TPSNormalizeRectangle (Rect (ARect.Top,
                                             ViewPort.Top + ViewPort.Bottom - ARect.Left,
                                             ARect.Bottom,
                                             ViewPort.Top + ViewPort.Bottom - ARect.Right));
  end;
end;

procedure TVpExCanvas.DrawRotatedText (x, y   : Integer;
                                       Text   : string;
                                       Rotate : Boolean);

var
  LF            : TLogFont;
  OldFont       : TFont;
  RealPoint     : TPoint;
  OldBrushStyle : TBrushStyle;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FillChar (LF, SizeOf (LF), #0);

  LF.lfHeight           := FCanvas.Font.Height;
  LF.lfWidth            :=  0;
  case Angle of
    ra0   : LF.lfEscapement:= 0;
    ra90  : LF.lfEscapement:= 2700;
    ra180 : LF.lfEscapement:= 1800;
    ra270 : LF.lfEscapement:= 900;
  end;
  LF.lfOrientation      := 0;
  if fsBold in FCanvas.Font.Style then
    LF.lfWeight         := FW_BOLD
  else
    LF.lfWeight         := FW_NORMAL;
  LF.lfItalic           := Byte (fsItalic in FCanvas.Font.Style);
  LF.lfUnderline        := Byte (fsUnderline in FCanvas.Font.Style);
  LF.lfStrikeOut        := Byte (fsStrikeOut in FCanvas.Font.Style);
  LF.lfCharSet          := DEFAULT_CHARSET;
  LF.lfQuality          := DEFAULT_QUALITY;
  if Length(FCanvas.Font.Name) <= 31 then                                
    StrCopy(LF.lfFaceName, PChar(FCanvas.Font.Name));                    
  {everything else as default}
  LF.lfOutPrecision     := OUT_DEFAULT_PRECIS;
  LF.lfClipPrecision    := CLIP_DEFAULT_PRECIS;
  case FCanvas.Font.Pitch of
    fpVariable : LF.lfPitchAndFamily := VARIABLE_PITCH or FF_DONTCARE;
    fpFixed    : LF.lfPitchAndFamily := FIXED_PITCH or FF_DONTCARE;
  else
    LF.lfPitchAndFamily := DEFAULT_PITCH;
  end;

  { Create new font to use }
  OldFont := FCanvas.Font;
  try
    FCanvas.Font.Handle:= CreateFontIndirect (LF);

    { Output the text }
    if Rotate then
      RealPoint := TPSRotatePoint (Angle, ViewPort, Point (x, y))
    else
      RealPoint := Point (x, y);
    OldBrushStyle := FCanvas.Brush.Style;       
    try
      FCanvas.Brush.Style := bsClear;
      FCanvas.TextOut (RealPoint.X, RealPoint.Y, Text);
    finally
      FCanvas.Brush.Style := OldBrushStyle;
    end;
  finally
    FCanvas.Font := OldFont;
  end;
end;      

function TVpExCanvas.ViewportWidth : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Right - FixRect.Left;
    ra90, ra270 : Result := FixRect.Bottom - FixRect.Top;
  else
    Result := FixRect.Right - FixRect.Left;
  end;
end;

function TVpExCanvas.ViewportHeight : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Bottom - FixRect.Top;
    ra90, ra270 : Result := FixRect.Right - FixRect.Left;
  else
    Result := FixRect.Bottom - FixRect.Top;
  end;
end;

function TVpExCanvas.ViewportLeft : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Left;
    ra90, ra270 : Result := FixRect.Top;
  else
    Result := FixRect.Left;
  end;
end;

function TVpExCanvas.ViewportRight : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Right;
    ra90, ra270 : Result := FixRect.Bottom;
  else
    Result := FixRect.Right;
  end;
end;

function TVpExCanvas.ViewportTop : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Top;
    ra90, ra270 : Result := FixRect.Left;
  else
    Result := FixRect.Top;
  end;
end;

function TVpExCanvas.ViewportBottom : Integer;
var
  FixRect : TRect;

begin
  FixRect := TPSNormalizeRectangle (ViewPort);
  case Angle of
    ra0,  ra180 : Result := FixRect.Bottom;
    ra90, ra270 : Result := FixRect.Right;
  else
    Result := FixRect.Bottom;
  end;
end;

procedure TVpExCanvas.Arc (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;
  Point3 : TPoint;
  Point4 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));
  Point3 := TPSRotatePoint (Angle, ViewPort, Point (X3, Y3));
  Point4 := TPSRotatePoint (Angle, ViewPort, Point (X4, Y4));

  FCanvas.Arc (Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y,
               Point4.X, Point4.Y);
end;

procedure TVpExCanvas.BrushCopy (const Dest   : TRect;
                                       Bitmap : TBitmap;
                                 const Source : TRect;
                                       AColor : TColor);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.BrushCopy (TPSRotateRectangle (Angle, ViewPort, Dest),
                     Bitmap, Source, AColor);
end;

procedure TVpExCanvas.Chord (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;
  Point3 : TPoint;
  Point4 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));
  Point3 := TPSRotatePoint (Angle, ViewPort, Point (X3, Y3));
  Point4 := TPSRotatePoint (Angle, ViewPort, Point (X4, Y4));

  FCanvas.Chord (Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y,
                 Point4.X, Point4.Y);
end;

procedure TVpExCanvas.CopyRect (      Dest   : TRect;
                                      Canvas : TCanvas;
                                const Source : TRect);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.CopyRect (TPSRotateRectangle (Angle, ViewPort, Dest),
                    Canvas, Source);
end;

procedure TVpExCanvas.Draw (X, Y    : Integer;
                            Graphic : TGraphic);
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (X, Y));
  FCanvas.Draw (RealPoint.X, RealPoint.Y, Graphic);
end;

procedure TVpExCanvas.DrawFocusRect (const ARect : TRect);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.DrawFocusRect (TPSRotateRectangle (Angle, ViewPort, ARect));
end;

procedure TVpExCanvas.Ellipse (X1, Y1, X2, Y2 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));

  FCanvas.Ellipse (Point1.X, Point1.Y, Point2.X, Point2.Y);
end;

procedure TVpExCanvas.Ellipse (const ARect : TRect);
{$IFNDEF VERSION5}
var
  R: TRect;
{$ENDIF}
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  {$IFDEF VERSION5}
  FCanvas.Ellipse (TPSRotateRectangle (Angle, ViewPort, ARect));
  {$ELSE}
  R := TPSRotateRectangle (Angle, ViewPort, ARect);
  FCanvas.Ellipse (R.Left, R.Top, R.Right, R.Bottom);
  {$ENDIF}
end;

procedure TVpExCanvas.FillRect (const ARect : TRect);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.FillRect (TPSRotateRectangle (Angle, ViewPort, ARect));
end;

procedure TVpExCanvas.FloodFill (X, Y      : Integer;
                                 AColor    : TColor;
                                 FillStyle : TFillStyle);
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (X, Y));
  FCanvas.FloodFill (RealPoint.X, RealPoint.Y, AColor, FillStyle);
end;

procedure TVpExCanvas.FrameRect (const ARect : TRect);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.FrameRect (TPSRotateRectangle (Angle, ViewPort, ARect));
end;

procedure TVpExCanvas.LineTo (X, Y : Integer);
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (X, Y));
  FCanvas.LineTo (RealPoint.X, RealPoint.Y);
end;

procedure TVpExCanvas.MoveTo (X, Y : Integer);
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (X, Y));
  FCanvas.MoveTo (RealPoint.X, RealPoint.Y);
end;

procedure TVpExCanvas.Pie (X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;
  Point3 : TPoint;
  Point4 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));
  Point3 := TPSRotatePoint (Angle, ViewPort, Point (X3, Y3));
  Point4 := TPSRotatePoint (Angle, ViewPort, Point (X4, Y4));

  FCanvas.Pie (Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y,
               Point4.X, Point4.Y);
end;

procedure TVpExCanvas.PolyBezier (const Points : array of TPoint);
var
  i          : Integer;
  PointArray : array of TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  SetLength (PointArray, Length (Points));

  for i := 0 to Length (Points) - 1 do
    PointArray[i] := TPSRotatePoint (Angle, ViewPort, Points[i]);

  FCanvas.PolyBezier (PointArray);
end;

procedure TVpExCanvas.PolyBezierTo (const Points : array of TPoint);
var
  i          : Integer;
  PointArray : array of TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  SetLength (PointArray, Length (Points));

  for i := 0 to Length (Points) - 1 do
    PointArray[i] := TPSRotatePoint (Angle, ViewPort, Points[i]);

  FCanvas.PolyBezierTo (PointArray);
end;

procedure TVpExCanvas.Polygon (Points : array of TPoint);
var
  i          : Integer;
  PointArray : array of TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  SetLength (PointArray, Length (Points));

  for i := 0 to Length (Points) - 1 do
    PointArray[i] := TPSRotatePoint (Angle, ViewPort, Points[i]);

  FCanvas.Polygon (PointArray);
end;

procedure TVpExCanvas.Polyline (Points : array of TPoint);
var
  i          : Integer;
  PointArray : array of TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  SetLength (PointArray, Length (Points));

  for i := 0 to Length (Points) - 1 do
    PointArray[i] := TPSRotatePoint (Angle, ViewPort, Points[i]);

  FCanvas.Polyline (PointArray);
end;

procedure TVpExCanvas.Rectangle (X1, Y1, X2, Y2 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));

  FCanvas.Rectangle (Point1.X, Point1.Y, Point2.X, Point2.Y);
end;

procedure TVpExCanvas.Rectangle (const ARect : TRect); 
{$IFNDEF VERSION5}
var
  R: TRect;
{$ENDIF}
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  {$IFDEF VERSION5}
  FCanvas.Rectangle (TPSRotateRectangle (Angle, ViewPort, ARect));
  {$ELSE}
  R := TPSRotateRectangle (Angle, ViewPort, ARect);
  FCanvas.Rectangle (R.Left, R.Top, R.Right, R.Bottom);
  {$ENDIF}
end;

procedure TVpExCanvas.RoundRect (X1, Y1, X2, Y2, X3, Y3 : Integer);
var
  Point1 : TPoint;
  Point2 : TPoint;
  Point3 : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  Point1 := TPSRotatePoint (Angle, ViewPort, Point (X1, Y1));
  Point2 := TPSRotatePoint (Angle, ViewPort, Point (X2, Y2));
  Point3 := TPSRotatePoint (Angle, ViewPort, Point (X3, Y3));

  FCanvas.RoundRect (Point1.X, Point1.Y, Point2.X, Point2.Y,
                     Point3.X, Point3.Y);
end;

procedure TVpExCanvas.StretchDraw (const ARect   : TRect;
                                         Graphic : TGraphic);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  FCanvas.StretchDraw (TPSRotateRectangle (Angle, ViewPort, ARect), Graphic);
end;

procedure TVpExCanvas.TextOut (      X, Y : Integer;
                               const Text : string);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  DrawRotatedText (X, Y, Text, True);
end;

procedure TVpExCanvas.TextRect (      ARect : TRect;
                                      X, Y  : Integer;
                                const Text  : string);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);
end;

function  TVpExCanvas.GetPixel (const x : Integer;
                                const y : Integer) : TColor;
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (x, y));
  Result := FCanvas.Pixels [RealPoint.X, RealPoint.Y];
end;

procedure TVpExCanvas.SetPixel (x      : Integer;
                                y      : Integer;
                                AColor : TColor);
var
  RealPoint : TPoint;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  RealPoint := TPSRotatePoint (Angle, ViewPort, Point (x, y));
  FCanvas.Pixels [RealPoint.X, RealPoint.Y] := AColor;
end;

procedure TVpExCanvas.CenteredTextOut (      ARect : TRect;
                                       const Text  : string);
var
  TW : Integer;

begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  TW := FCanvas.TextWidth (Text);
  if TW < ARect.Right - ARect.Left then
    ARect.Left := ARect.Left + ((ARect.Right - ARect.Left - TW) div 2);

  TPSTextOut (FCanvas, Angle, ViewPort, ARect.Left, ARect.Top, Text);
end;

procedure TVpExCanvas.TextOutAtPoint (      X, Y : Integer;
                                      const Text : string);
begin
  if not Assigned (FCanvas) then
    raise EVpCanvasError.Create (RSNoCanvas);

  DrawRotatedText (X, Y, Text, False);
end;

function TVpExCanvas.RGBToTColor (Red, Green, Blue : Byte) : TColor;
var
  RedPart, GreenPart, BluePart : Integer;

begin
  RedPart := Red;
  GreenPart := Green shl 8;
  BluePart := Blue shl 16;
  Result := $02000000 or RedPart or GreenPart or BluePart;
end;

procedure TVpExCanvas.TColorToRGB (    Color : TColor;
                                   var Red   : Byte;
                                   var Green : Byte;
                                   var Blue  : Byte);
begin
  Red := Color and $0000ff;
  Green := (Color and $00ff00) shr 8;
  Blue := (Color and $ff0000) shr 16;
end;

procedure TVpExCanvas.CachePalette (    ABitmap : TBitmap;
                                    var PaletteEntries : TVpPaletteArray);
var
  PaletteSize : Integer;

begin
  case ABitmap.PixelFormat of
    pfDevice :
      PaletteSize := 0;
    pf1bit   :
      PaletteSize := 2;
    pf4bit   :
      PaletteSize := 16;
    pf8bit   :
      PaletteSize := 256;
    pf15bit  :
      PaletteSize := 0;
    pf16bit  :
      PaletteSize := 0;
    pf24bit  :
      PaletteSize := 0;
    pf32bit  :
      PaletteSize := 0;
    pfCustom :
      PaletteSize := 0;
    else
      PaletteSize := 0;
  end;

  if PaletteSize > 0  then
    GetPaletteEntries (ABitmap.Palette, 0, PaletteSize, PaletteEntries);
end;

function TVpExCanvas.GetBmpPixel (ABitmap      : TBitmap;
                                  PaletteCache : TVpPaletteArray;
                                  x            : Integer;
                                  y            : Integer) : TColor;
// Fast scanline based pixel access
var
  ByteArray    : PChar;
  WorkByte     : Byte;
  WorkWord     : Word;
  Red          : Byte;
  Blue         : Byte;
  Green        : Byte;

begin
  if (x < 0) or (x >= ABitmap.Width) or
     (y < 0) or (y >= ABitmap.Height) then
    raise EVpCanvasError.Create (RSOutOfRange);

  case ABitmap.PixelFormat of
    pfDevice : begin
      raise EVpCanvasError.Create (RSNotSupported);
    end;

    pf1bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      WorkByte := (Byte (ByteArray[x div 8]) shr (7 - (x mod 8))) and $01;
      Result := RGBToTColor (PaletteCache[WorkByte].peRed,
                             PaletteCache[WorkByte].peGreen,
                             PaletteCache[WorkByte].peBlue);
    end;

    pf4bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      WorkByte := (Byte (ByteArray[x div 2]) shr (((x + 1) mod 2) * 4)) and $0F;
      Result := RGBToTColor (PaletteCache[WorkByte].peRed,
                             PaletteCache[WorkByte].peGreen,
                             PaletteCache[WorkByte].peBlue);
    end;

    pf8bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      WorkByte := Byte (ByteArray[x]);
      Result := RGBToTColor (PaletteCache[WorkByte].peRed,
                             PaletteCache[WorkByte].peGreen,
                             PaletteCache[WorkByte].peBlue);
    end;

    pf15bit  : begin
      ByteArray := ABitmap.ScanLine[y];
      WorkWord := Byte (ByteArray[x * 2]) +
                  256 * Byte (ByteArray[(x * 2) + 1]);
      Red := ((WorkWord shr 10) and $1f) shl 3;
      Green := ((WorkWord shr 5) and $1f) shl 3;
      Blue := (WorkWord and $1f) shl 3;
      Result := RGBToTColor (Red, Green, Blue);
    end;

    pf16bit  : begin
      ByteArray := ABitmap.ScanLine[y];
      WorkWord := Byte (ByteArray[x * 2]) +
                  256 * Byte (ByteArray[(x * 2) + 1]);
      Red := ((WorkWord shr 11) and $1f) shl 3;
      Green := ((WorkWord shr 5) and $3f) shl 2;
      Blue := (WorkWord and $1f) shl 3;
      Result := RGBToTColor (Red, Green, Blue);
    end;

    pf24bit  : begin
      ByteArray := ABitmap.ScanLine[y];
      Result := RGBToTColor (Byte (ByteArray[x * 3 + 2]),
                             Byte (ByteArray[x * 3 + 1]),
                             Byte (ByteArray[x * 3]));
    end;

    pf32bit  : begin
      ByteArray := ABitmap.ScanLine[y];
      Result := RGBToTColor (Byte (ByteArray[x * 4 + 2]),
                             Byte (ByteArray[x * 4 + 1]),
                             Byte (ByteArray[x * 4]));
    end;
    pfCustom : begin
      raise EVpCanvasError.Create (RSNotSupported);
    end;
    else
      raise EVpCanvasError.Create (RSNotSupported);
  end;
end;

procedure TVpExCanvas.SetBmpPixel (ABitmap      : TBitmap;
                                   PaletteCache : TVpPaletteArray;
                                   x            : Integer;
                                   y            : Integer;
                                   AColor       : TColor);
// Fast scanline based pixel access
var
  BytePos      : Integer;
  WorkByte     : Byte;
  WorkWord     : Word;
  ByteArray    : PChar;
  PaletteIndex : Byte;
  Red          : Byte;
  Green        : Byte;
  Blue         : Byte;

begin
  if (x < 0) or (x >= ABitmap.Width) or
     (y < 0) or (y >= ABitmap.Height) then
    Exit;

  case ABitmap.PixelFormat of
    pfDevice : begin
      raise EVpCanvasError.Create (RSNotSupported); 
    end;

    pf1bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      BytePos := x div 8;
      WorkByte := Byte (ByteArray[BytePos]);
      WorkByte := WorkByte and (not ($01 shl (7 - (x mod 8))));
      PaletteIndex := GetNearestPaletteIndex (ABitmap.Palette, AColor) and $01;
      WorkByte := WorkByte or (PaletteIndex shl (7 - (x mod 8)));
      ByteArray[BytePos] := Char (WorkByte);
    end;

    pf4bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      BytePos := x div 2;
      WorkByte := Byte (ByteArray[BytePos]);
      WorkByte := WorkByte and (not ($0f shl (((x + 1) mod 2) * 4)));
      PaletteIndex := GetNearestPaletteIndex (ABitmap.Palette, AColor) and $0f;
      WorkByte := WorkByte or (PaletteIndex shl (((x + 1) mod 2) * 4));
      ByteArray[BytePos] := Char (WorkByte);
    end;

    pf8bit   : begin
      ByteArray := ABitmap.ScanLine[y];
      PaletteIndex := GetNearestPaletteIndex (ABitmap.Palette, AColor);
      ByteArray[x] := Char (PaletteIndex);
    end;

    pf15bit  : begin
      TColorToRGB (AColor, Red, Green, Blue);
      ByteArray := ABitmap.ScanLine[y];
      WorkWord := ((Red and $f8) shl 7) or
                  ((Green and $f8) shl 3) or
                  ((Blue and $f8) shr 3);
      ByteArray[x * 2] := Char (WorkWord and $ff);
      ByteArray[(x * 2) + 1] := Char ((WorkWord shr 8) and $ff);;
    end;

    pf16bit  : begin
      TColorToRGB (AColor, Red, Green, Blue);
      ByteArray := ABitmap.ScanLine[y];
      WorkWord := ((Red and $f8) shl 8) or
                  ((Green and $fc) shl 3) or
                  ((Blue and $f8) shr 3);
      ByteArray[x * 2] := Char (WorkWord and $ff);
      ByteArray[(x * 2) + 1] := Char ((WorkWord shr 8) and $ff);;
    end;

    pf24bit  : begin
      TColorToRGB (AColor, Red, Green, Blue);
      ByteArray := ABitmap.ScanLine[y];
      ByteArray[(x * 3) + 2] := Char (Red);
      ByteArray[(x * 3) + 1] := Char (Green);
      ByteArray[x * 3] := Char (Blue);
    end;

    pf32bit  : begin
      TColorToRGB (AColor, Red, Green, Blue);
      ByteArray := ABitmap.ScanLine[y];
      ByteArray[(x * 4) + 2] := Char (Red);
      ByteArray[(x * 4) + 1] := Char (Green);
      ByteArray[x * 4] := Char (Blue);
    end;

    pfCustom : begin
      raise EVpCanvasError.Create (RSNotSupported);
    end;
  end;
end;

{ TVpLineWrapper ************************************************************ }

constructor TVpLineWrapper.Create;
begin
  inherited Create;

  FTextMargin := 3;
  FMinChars   := 5;
  FAngle      := ra0;
  FViewPort   := Rect (0, 0, 0, 0);
end;

function TVpLineWrapper.FindEndingPos (ARegion  : HRGN;
                                       LineSize : Integer;               
                                       HPos     : Integer;               
                                       YPos     : Integer) : TPoint;     
var                                                                      
  WorkRect : TRect;

begin
  GetRgnBox (ARegion, WorkRect);

  Result.x := HPos;
  Result.y := YPos;
  while (PtInRegion (ARegion, Result.x, Result.y)) and
        (PtInRegion (ARegion, Result.x, Result.y + LineSize)) and
        (Result.x < WorkRect.Right) do 
    Inc (Result.x);
end;

function TVpLineWrapper.FindNextStartingPoint (
                 ARegion  : HRGN;                                        
                 LineSize : Integer;                                     
             var HPos     : Integer;                                     
             var YPos     : Integer) : Boolean;

var                                                                      
  WorkRect : TRect;                                                      
  Done     : Boolean;

begin
  GetRgnBox (ARegion, WorkRect);

  Result := False;
  Done   := False;
  while not Done do begin
    if HPos > WorkRect.Right then begin 
      HPos := WorkRect.Left;
      Inc (YPos, LineSize);
      if YPos > WorkRect.Bottom then 
        Break;
    end;
    if (not PtInRegion (ARegion, HPos, YPos)) or
       (not PtInRegion (ARegion, HPos, YPos + LineSize)) then begin
      Inc (HPos);
      if HPos > WorkRect.Right then begin
        HPos := WorkRect.Left;
        Inc (YPos, LineSize);
        if YPos > WorkRect.Bottom then 
          Break;                                                         
      end;
    end else begin                                                       
      Result := True;                                                    
      Break;                                                             
    end;                                                                 
  end;                                                                   
end;                                                                     

function TVpLineWrapper.FindWordBreaks (AString : string;                
                                        CharPos : Integer) : Integer;    
var                                                                      
  Done    : Boolean;                                                     
  WorkPos : Integer;                                                     

begin                                                                    
  Done    := False;                                                      
  WorkPos := CharPos;                                                    

  while not Done do begin                                                
    if IsWordBreak (AString, WorkPos) then                               
      Done := True                                                       
    else                                                                 
      Dec (WorkPos);                                                     

    if WorkPos = 0 then                                                  
      Done := True;                                                      
  end;

  if WorkPos > 0 then                                                    
    Result := WorkPos                                                    
  else                                                                   
    Result := CharPos;                                                   
end;                                                                     

function TVpLineWrapper.FitStringInRect (                                
                 ACanvas     : TCanvas;
                 RectWidth   : Integer;                                  
                 AvgCharSize : Integer;                                  
             var AString     : string;
             var CharsOut    : Integer) : string;                         

var                                                                      
  CharsToRender : Integer;                                               
  L             : Integer;                                               
  R             : Integer;                                               
  M             : Integer;                                               

begin                                                                    
  if AvgCharSize > 0 then begin                                          
    { Guess at the number of characters that can fit on a line }         
    CharsToRender := RectWidth div AvgCharSize;                          

    if CharsToRender > 0 then begin                                      
      Result := Copy (AString, 1, CharsToRender);                        
      while (ACanvas.TextWidth (Result) < RectWidth) and                 
            (CharsToRender < Length (Result)) do begin                   
        Inc (CharsToRender);                                             
        Result := Copy (AString, 1, CharsToRender);                      
      end;                                                               
      while (ACanvas.TextWidth (Result) > RectWidth) and                 
            (CharsToRender > 0) do begin                                 
        Dec (CharsToRender);                                             
        Result := Copy (AString, 1, CharsToRender);                      
      end;

      if CharsToRender >= Length (AString) then begin
        CharsOut := CharsToRender;                    
        AString := Trim (Copy (AString,
                               CharsToRender + 1,
                               Length (AString) - 1));                          
        Exit;                                                            
      end;                                                               

      CharsToRender := FindWordBreaks (AString, CharsToRender);          
      Result := Copy (AString, 1, CharsToRender);                        
    end;                                                                        

    if CharsToRender > 0 then begin                                      
      AString := Copy (AString,                                          
                       CharsToRender + 1,                                
                       Length (AString) - CharsToRender + 1);
    end else begin                                                       
      Result  := Copy (AString, 1, 1);                                   
      AString := Copy (AString,                                          
                       2,                                                
                       Length (AString) - 1);                            
    end;                                                                 
  end else begin                                                         
    { Use binary search if the average character guess fails }           
    L := 1;                                                              
    R := Length (AString);                                               
    M := 0;                                                              
    while L <= R do begin                                                
      M := (L + R) div 2;                                                
      Result := Copy (AString, 1, M);                                    
      if (ACanvas.TextWidth (Result) < RectWidth) then                   
        L := Succ (M)                                                    
      else                                                               
        R := Pred (M);                                                   
      if M >= Length (AString) then begin
        CharsOut := Length (AString);
        AString := Trim (Copy (AString, M + 1, Length (AString) - 1));
        Exit;                                                            
      end;                                                               
    end;                                                                 
    CharsToRender := FindWordBreaks (AString, M);                        
    Result := Copy (AString, 1, CharsToRender);                          
    AString := Copy (AString,                                            
                     CharsToRender + 1,                                  
                     Length (AString) - 1);                              
  end;
  CharsOut := CharsToRender;                    
  Result := Trim (Result);
end;

function TVpLineWrapper.GetAverageCharSize (                             
             ACanvas : TCanvas) : Integer;                               

var
  Metrics         : TTextMetric;                                         
  SavedFontHandle : THandle;                                             
  DC              : HDC;                                                 

begin                                                                    
  DC              := GetDC (0);                                          
  SavedFontHandle := SelectObject (DC, ACanvas.Font.Handle);             
  try                                                                    
    GetTextMetrics (DC, Metrics);                                        
    Result := Metrics.tmAveCharWidth;                                    
  finally                                                                
    SelectObject (DC, SavedFontHandle);                                  
    ReleaseDC (0, DC);                                                   
  end;                                                                   
end;                                                                     

function TVpLineWrapper.GetNextRectangle (                               
                 ARegion     : HRGN;                                     
                 LineSize    : Integer;                                  
                 AvgCharSize : Integer;                                  
             var HPos        : Integer;
             var LinePos     : Integer) : TRect;

var                                                                      
  EndPoint : TPoint;                                                     
  Done     : Boolean;                                                    

begin                                                                    
  Result := Rect (0, 0, 0, 0);                                           
  Done   := False;                                                       
  while not Done do                                                      
    if FindNextStartingPoint (ARegion, LineSize,
                              HPos, LinePos) then begin                  
      EndPoint := FindEndingPos (ARegion, LineSize, HPos, LinePos);      
      if EndPoint.x - HPos > FMinChars * AvgCharSize then begin          
        Result := Rect (HPos, LinePos, EndPoint.x, EndPoint.y);          
        Break;                                                           
      end else                                                           
        Inc (HPos);
    end else                                                             
      Break;                                                             
end;                                                                     

function TVpLineWrapper.IsWordBreak (AString : string;                   
                                     CharPos : Integer) : Boolean;       
var                                                                      
  NC : Char;                                                             
  PC : Char;                                                             
  C  : Char;                                                             

begin                                                                    
  C      := ThisChar (AString, CharPos);                                 
  NC     := NextChar (AString, CharPos);                                 
  PC     := PrevChar (AString, CharPos);                                 
  Result := False;                                                       

  if C = '.' then begin
    if not (NC in ['0'..'9']) then                                       
      Result := True;                                                    
  end else if (C in [' ', #10, #13, #9]) then                            
    Result := True                                                       
  else if C = '-' then begin                                             
    if (PC in ['0'..'9', 'A'..'Z', 'a'..'z']) and
       (NC in ['0'..'9', 'A'..'Z', 'a'..'z']) then
      Result := True;                                                    
  end;                                                                   
  if Assigned (FOnFindWordBreak) then                                    
    FOnFindWordBreak (Self, AString, CharPos, Result);                   
end;                                                                     

function TVpLineWrapper.NextChar (AString : string;                      
                                  CharPos : Integer) : Char;             
begin                                                                    
  if (CharPos >= 1) and (CharPos < Length (AString)) then                
    Result := AString[CharPos + 1]                                       
  else                                                                   
    Result := #0;                                                        
end;

function TVpLineWrapper.PrevChar (AString : string;                      
                                  CharPos : Integer) : Char;             
begin
  if (CharPos > 1) and (CharPos <= Length (AString)) then                
    Result := AString[CharPos - 1]                                       
  else                                                                   
    Result := #0;                                                        
end;                                                                     

function TVpLineWrapper.RenderTextToCanvas (ACanvas : TCanvas;
                                            ARect   : TRect;
                                            AString : string) : Integer;          

var
  LineHeight    : Integer;                                               
  RectWidth     : Integer;                                               
  RectHeight    : Integer;                                               
  LinePos       : Integer;                                               
  AvgCharSize   : Integer;                                               
  Done          : Boolean;
  CharsWritten  : Integer;                                               

begin                                                                    
  { Initialize stuff }
  Result       := 0;
  CharsWritten := 0;
                                                     
  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);

  LineHeight   := ACanvas.TextHeight ('yY0');
  if Angle = ra0 then
    RectWidth  := ARect.Right - ARect.Left - 2 * FTextMargin
  else
    RectWidth  := VpRotatedCanvas.ViewportWidth;
  if Angle = ra0 then
    RectHeight := ARect.Bottom - ARect.Top
  else
    RectHeight := VpRotatedCanvas.ViewportHeight;
  LinePos      := ARect.Top + FTextMargin;
  AvgCharSize  := GetAverageCharSize (ACanvas);
  Done         := False;

  if LineHeight > RectHeight then
    Exit;

  while not Done do begin
    if LinePos + LineHeight > ARect.Bottom then
      Break;

    if AString = '' then
      Break;

    VpRotatedCanvas.TextOut (ARect.Left + FTextMargin,
                             LinePos,
                             FitStringInRect (ACanvas,
                                              RectWidth,
                                              AvgCharSize,
                                              AString,
                                              CharsWritten));
    Result := Result + CharsWritten;

    LinePos := LinePos + LineHeight + FTextMargin;
  end;
end;

function TVpLineWrapper.RenderTextToCanvasRegion (ACanvas : TCanvas;
                                                  ARegion : HRGN;
                                                  AString : string) : Integer;
var
  LineHeight    : Integer;
  RectHeight    : Integer;
  LinePos       : Integer;
  AvgCharSize   : Integer;
  Done          : Boolean;
  HPos          : Integer;

  RegionRect    : TRect;
  WorkRect      : TRect;
  CharsWritten  : Integer;

begin
  Result       := 0;
  CharsWritten := 0;

  SetTVpExCanvasAVC (ACanvas, Angle, ViewPort);
  GetRgnBox (ARegion, RegionRect);

  LineHeight   := ACanvas.TextHeight ('yY0');
  if Angle = ra0 then
    RectHeight := RegionRect.Bottom - RegionRect.Top
  else
    RectHeight := VpRotatedCanvas.ViewportHeight;
  LinePos      := RegionRect.Top + FTextMargin;
  HPos         := RegionRect.Left + FTextMargin;
  AvgCharSize  := GetAverageCharSize (ACanvas);
  Done         := False;

  if LineHeight > RectHeight then
    Exit;

  while not Done do begin
    if LinePos + LineHeight > RegionRect.Bottom then 
      Break;

    if AString = '' then
      Break;

    WorkRect := GetNextRectangle (ARegion, LineHeight,
                                  AvgCharSize, HPos, linepos);
    if WorkRect.Right - WorkRect.Left > 0 then begin
      VpRotatedCanvas.TextOut (WorkRect.Left + FTextMargin,
                               WorkRect.Top,
                               FitStringInRect (ACanvas,
                                                WorkRect.Right -
                                                    WorkRect.Left -
                                                    FTextMargin,
                                                AvgCharSize,
                                                AString,
                                                CharsWritten));
      Result := Result + CharsWritten;
    end else
      Break;
    HPos := WorkRect.Right + 1;                                          
  end;                                                                   
end;                                                                     

function TVpLineWrapper.ThisChar (AString : string;                      
                                  CharPos : Integer) : Char;             
begin                                                                    
  if (CharPos >= 1) and (CharPos <= Length (AString)) then               
    Result := AString[CharPos]                                           
  else                                                                   
    Result := #0;                                                        
end;                                                                     

initialization                                                           

  VpRotatedCanvas := TVpExCanvas.Create;
  VpTextRenderer  := TVpLineWrapper.Create;                                 

finalization                                                             

  VpRotatedCanvas.Free;
  VpTextRenderer.Free;                              

{ !!.01 End changes !!.01 }

end.
