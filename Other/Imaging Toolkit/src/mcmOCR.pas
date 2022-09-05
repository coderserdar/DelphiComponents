// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  17575: mcmOCR.pas
//
//    Rev 1.10    01-03-2011 20:39:44  mcm    Version: IMG 3.4
//
//    Rev 1.9    25-10-2009 17:23:48  mcm    Version: IMG 3.3
// Support for Delphi 2010
//
//    Rev 1.8    31-08-2009 19:05:44  mcm
// Delphi 2009 support
//
//    Rev 1.7    27-08-2007 18:54:00  mcm    Version: IMG 3.1
// Added support for Delphi 2007
//
//    Rev 1.6    05-03-2006 10:52:20  mcm    Version: IMG 2.16
// Added check for image being 8 bit grey scale.
//
//    Rev 1.5    22-12-2005 20:52:14  mcm    Version: IMG 2.12
// Delphi 2006 support.
//
//   Rev 1.4    03-01-2005 18:30:42  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.3    30-01-2004 20:37:48  mcm    Version: IMG 2.3
// Fixed a problem when first loading an OCR library for White characters on a
// black background and then another library for black characters on a white
// background. 
// Added glyph seperation facility to the Match method. 

//
//   Rev 1.2    17-11-2003 10:07:44  mcm    Version: OCR 1.0
// Internal.

//
//   Rev 1.1    16-10-2003 11:32:40  mcm    Version: OCR 1.0

//
//   Rev 1.0    27-05-2002 16:22:18  mcm

unit mcmOCR;

interface

{$Include 'mcmDefines.pas'}

{ $DEFINE SHOWCHARPOSONIMAGE}

uses {$IFNDEF GE_DXE2}
       Windows, Classes,
     {$ELSE}
       System.Classes, WinApi.Windows,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage;

type
  TOnDuplicateGlyph = procedure(Sender : TObject; Index : integer; var Replace : boolean) of object;
  TOCRMatchProc     = function(var Error : single) : integer of object;

  TmcmGlyphMatch    = (OMM_ALLPIXELS,
                       OMM_FEATUREPIXELS);

  //----------------------------------------------------------------------------
  // OCR Glyph color
  TmcmGlyphColor = (GC_BLACK_ON_WHITE,
                    GC_WHITE_ON_BLACK);

  //----------------------------------------------------------------------------
  // OCR Glyph Group
  TmcmGlyphGroup  = (OGG_ALL,
                     OGG_DIGITS,
                     OGG_UPPERCASE,
                     OGG_LOWERCASE,
                     OGG_SPECIAL,
                     OGG_CASE1,
                     OGG_CASE2,
                     OGG_CASE3,
                     OGG_CASE4,
                     OGG_CASE5,
                     OGG_CASE6,
                     OGG_CASE7,
                     OGG_CASE8,
                     OGG_CASE9);

  // Rectangle holding coordinates of a text line.
  TGlyphLineRect = record
  x1, y1     : integer;  // Left top line point.
  x2, y2     : integer;  // Bottom right line point.
  cy, ty, by : integer;  // Y centre, 1/3 top and 1/3 bottom "line".
  CharPos    : PVectorB; // Dynamic array of possible glyph start, end and break
                         // positions.
  end;
  PGlyphLineRect = ^TGlyphLineRect;

  // "Database" glyph holder.
  TmcmGlyph = record
  Identifier : WideChar; // Glyph identifier ex. 'a'..'z', 'A'..'Z' or '0'..'9'.
  Group      : word;     // Glyph group, one of TOCRGlyphGroup.
  Height     : word;     // Glyph height.
  Width      : word;     // Glyph width.
  Size       : cardinal; // Glyph size = height * width, 8 byte aligned.
  yPos       : word;     // Indication if glyph is above (1) or below (2) line centre,
                         // or full (0) height.
  Reservedw1 : word;     // Reserved word 1.
  Reservedw2 : word;     // Reserved word 2.
  Reserved   : cardinal; // Reserved cardinal.
  Aspect     : single;   // Acspect ratio height / width.
  Pattern    : PVectorB; // Glyph bit pattern.
  end;
  PmcmGlyph = ^TmcmGlyph;


  TmcmOCR = class(TComponent)
  private
    FMMX              : boolean;
    FFileVer          : word;          // OCR file version.
    FError            : TmcmErrorCode; // Error code.

    FImage            : TmcmImage;     // Image to read.
    FInputImage       : TmcmImage;     // Thresholded image.
    FImageResize      : TmcmImage;     // Resize images of glyphs.
    FRegion           : TRect;
    FGlyphResize      : TmcmGlyph;     // Resized glyph.
    FGlyphList        : TList;         // List of known glyphs.

    FGlyphHeight      : word;          // Height of glyphs.
    FGlyphWidth       : word;          // Width of glyphs.
    FGlyphColor       : TmcmGlyphColor;// Glyph color (Black or White).
    FColorIndex       : byte;          // Glyph color (index).
    FGlyphMinHeight   : word;          // Minimum glyph height.
    FGlyphMinWidth    : word;          // Minimum glyph width.
    FGlyphMaxHeight   : word;          // Maximum glyph height.
    FGlyphMaxWidth    : word;          // Maximum glyph width.
    FGlyphMinSpace    : word;          // Minimum space between characters.

    FThreshold        : TmcmThreshold; // Threshold method.
    FThresholdLevel   : word;          // Gray scale threshold value.
                                       // Pixel values below becomes black and
                                       // those above becomes white.

    FBreakCost        : boolean;       // Use Break Cost technique for glyph
                                       // segmentation.

    FUseAspect        : boolean;       // Use aspect ratio in match.
    FMatchMethod      : TmcmGlyphMatch;// Match method to use.
    FMatchMethodProc  : TOCRMatchProc; // Object procedure! INTERNAL ONLY!

    FLinesList        : TList;         // List of lines detected in image.
    FLineIndex        : word;          // Index to current line to read
    FCharPos          : integer;       // Index to char right position.
    FSeparateAt       : integer;       // Last glyph was seperated at this
                                       // position, if > 0.
    FMaxFoundHeight   : integer;       // Heighest glyph found.
    FMaxFoundWidth    : integer;       // Widest glyph found.
    FExpectGlyphCount : cardinal;      // Possible number of glyphs in image.
    FAddSpaces        : boolean;
    FSpaceWidth       : integer;       // Estimated min pixel width for a space
                                       // character.
    FStringList       : TStringList;   // List of read "text" lines.

    FReplaceGlyph     : boolean;
    FOnDuplicateGlyph : TOnDuplicateGlyph;
  protected
    procedure   CloseFile(var Stream : TFileStream);
    procedure   FindPaletteColor;
    procedure   FillValue(x, y : integer; Value : byte);
    function    GetImage : TmcmImage;
    function    GetGlyph(Index : word) : PmcmGlyph;
    function    GetGlyphColor : TmcmGlyphColor;
    function    GetNoGlyph : word;
    function    GetLineRect(Index : word) : TRect;
    function    GetNoLines : integer;
    function    GetRegion : TRect;
    function    GetString(Index : integer) : string;
    procedure   LocateTextLines;
    procedure   LocateGlyphsOnLine(Index : word);
    function    MatchPixels(var Error : single) : integer;
    function    MatchPixelFeature(var Error : single) : integer;
    function    OpenFile(Filename : string; Mode : word; var Stream : TFileStream) : boolean;
    procedure   SetBreakCost(Value : boolean);
    procedure   SetGlyphColor(Value : TmcmGlyphColor);
    procedure   SetGlyphHeight(Value : word);
    procedure   SetGlyphMaxHeight(Value : word);
    procedure   SetGlyphMaxWidth(Value : word);
    procedure   SetGlyphMinHeight(Value : word);
    procedure   SetGlyphMinWidth(Value : word);
    procedure   SetGlyphWidth(Value : word);
    procedure   SetGlyphResize;
    procedure   SetImage(Value : TmcmImage);
    procedure   SetMatchMethod(Value : TmcmGlyphMatch);
    procedure   SetRegion(Value : TRect);
    procedure   SetThreshold(Value : TmcmThreshold);
    procedure   SetThresholdLevel(Value : word);
    procedure   SetUseAspect(Value : boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    procedure   About;
    function    AddGlyph(Identifier : WideChar; Group : TmcmGlyphGroup; Position : TRect) : integer;
    procedure   Clear;
    procedure   ClearGlyphs;
    procedure   ClearLines;
    procedure   DeleteGlyph(Index : integer);
    function    GetFirstGlyph(var Position : TRect) : integer;
    procedure   GetGlyphTopBottom(var Position : TRect);
    function    GetNextGlyph(var Position : TRect) : integer;
    function    GetSeparatorGlyph(var Position : TRect; Separators : PVectorI; Ranking : PVectorB; Count : word) : word;
    procedure   LoadFromFile(FileName : string);
    procedure   LoadFromStream(Stream : TStream);
    procedure   LocateGlyphs;
    function    Match(var Error : single; Position : TRect) : integer;
    function    MatchAndCut(var Error : single; var Position : TRect) : integer;
    procedure   ReadText;
    function    ResizeGlyph(Position : TRect) : PmcmGlyph;
    procedure   SaveToFile(FileName : string);
    procedure   SaveToStream(Stream : TStream);
    procedure   Threshold;

    property    Glyph[Index : word] : PmcmGlyph
      read      GetGlyph;
    property    Image : TmcmImage
      read      GetImage
      write     SetImage;
    property    ImageThreshold : TmcmImage
      read      FImage;
    property    LineRect[Index : word] : TRect
      read      GetLineRect;
    property    NoGlyph : word
      read      GetNoGlyph;
    property    NoLines : integer
      read      GetNoLines;
    property    Region : TRect
      read      GetRegion
      write     SetRegion;
    property    SeparateAt : integer
      read      FSeparateAt
      write     FSeparateAt;
    property    Text[Index : integer] : string
      read      GetString;
  published
    property    AddSpaces : boolean
      read      FAddSpaces
      write     FAddSpaces default False;
    property    CutLargeGlyphs : boolean
      read      FBreakCost
      write     SetBreakCost default True;
    property    Error : TmcmErrorCode
      read      FError;
    property    GlyphColor : TmcmGlyphColor
      read      GetGlyphColor
      write     SetGlyphColor default GC_BLACK_ON_WHITE;
    property    GlyphHeight : word
      read      FGlyphHeight
      write     SetGlyphHeight default 8;
    property    GlyphMaxHeight : word
      read      FGlyphMaxHeight
      write     SetGlyphMaxHeight default 17;
    property    GlyphMaxWidth : word
      read      FGlyphMaxWidth
      write     SetGlyphMaxWidth default 15;
    property    GlyphMinHeight : word
      read      FGlyphMinHeight
      write     SetGlyphMinHeight default 3;
    property    GlyphMinWidth : word
      read      FGlyphMinWidth
      write     SetGlyphMinWidth default 3;
    property    GlyphWidth : word
      read      FGlyphWidth
      write     SetGlyphWidth default 6;
    property    MatchMethod : TmcmGlyphMatch
      read      FMatchMethod
      write     SetMatchMethod default OMM_FEATUREPIXELS;
    property    MaxFoundHeight : integer
      read      FMaxFoundHeight;
    property    MaxFoundWidth : integer
      read      FMaxFoundWidth;
    property    ReplaceGlyph : boolean
      read      FReplaceGlyph
      write     FReplaceGlyph default False;
    property    ThresholdMethod : TmcmThreshold
      read      FThreshold
      write     SetThreshold default TH_ISODATA;
    property    ThresholdLevel : word
      read      FThresholdLevel
      write     SetThresholdLevel default 128;
    property    UseAspectRatio : boolean
      read      FUseAspect
      write     SetUseAspect default True;

    // Events
    property    OnDuplicateGlyph : TOnDuplicateGlyph
      read      FOnDuplicateGlyph
      write     FOnDuplicateGlyph;
  end;

implementation

uses {$IFNDEF GE_DXE2} SysUtils, {$ELSE} System.SysUtils, System.Types, {$ENDIF}
     {$IFNDEF GE_DXE2} Dialogs, {$ELSE} Vcl.Dialogs, {$ENDIF}
     uOcrAbout, mcmImageColor;

const OCRFEATURE    = 0;
      OCRHOLE       = 1;
      OCRBACKGROUND = 2;
      GLYPHSTART    = 1;
      GLYPHEND      = 2;
      GLYPHSTARTEND = 3;
      GLYPHBREAK    = 4;

const mcmOCRId      : string[6] = 'mcmOCR';
      mcmOCRVersion : word = 101;
{$IFDEF DCB3_5}
      CPUID  = $A20F;
{$ENDIF}



constructor TmcmOCR.Create(AOwner : TComponent);
var bMMX : boolean;
begin
  Inherited Create(AOwner);

  // Check if MMX is supported by processor.
  FMMX := False;
  try
    asm
      mov   bMMX,$00
      mov   eax,1
      {$IFNDEF DCB3_5}
      cpuid
      {$ELSE}
      dw    CPUID
      {$ENDIF}
      test  edx,$800000
      jz    @NoMMX
      mov   bMMX,$01
      @NoMMX:
    end;
  except
  // do nothing.
  end;
  FMMX := bMMX;

  FError := EC_OK;

  FMatchMethod  := OMM_FEATUREPIXELS;
  FMatchMethodProc := MatchPixelFeature;

  FImage := Nil;
  FInputImage := Nil;
  FImageResize := TmcmImage.Create;
  FImageResize.ImageFormat := IF_GREY8;
  FImageResize.CreateGreyPalette;
  FGlyphResize.Pattern := Nil;

  FBreakCost        := True;
  FUseAspect        := True;
  FGlyphMinHeight   := 3;
  FGlyphMinWidth    := 3;
  FGlyphMaxHeight   := 17;
  FGlyphMaxWidth    := 15;
  FGlyphMinSpace    := 0;
  GlyphHeight       := 8;
  GlyphWidth        := 6;
  FGlyphColor       := GC_BLACK_ON_WHITE;
  FColorIndex       := 0;
  FAddSpaces        := False;
  FSpaceWidth       := 32767;
  FThreshold        := TH_ISODATA;
  FThresholdLevel   := 128;

  FReplaceGlyph     := False;
  FOnDuplicateGlyph := Nil;

  FGlyphList  := TList.Create;
  FLinesList  := TList.Create;
  FStringList := TStringList.Create;
end; // TmcmOCR.Create.


destructor TmcmOCR.Destroy;
begin
  if Assigned(FImage) and Assigned(FInputImage) and (FInputImage <> FImage)
  then FImage.Free;
  FImage := Nil;
  FInputImage := Nil;

  FImageResize.Free;
  Clear;
  if Assigned(FGlyphResize.Pattern)
  then FreeMem(FGlyphResize.Pattern);
  FGlyphResize.Pattern := Nil;
  FGlyphList.Free;
  FLinesList.Free;
  FStringList.Free;
  Inherited Destroy;
end; // TmcmOCR.Destroy.


procedure TmcmOCR.Clear;
begin
  ClearLines;
  ClearGlyphs;
  FStringList.Clear;

  if Assigned(FGlyphResize.Pattern)
  then FreeMem(FGlyphResize.Pattern);
  FGlyphResize.Pattern := Nil;

end; // TmcmOCR.Clear.


procedure TmcmOCR.ClearGlyphs;
var i : longint;
begin
  for i := (FGlyphList.Count - 1) downto 0
  do begin
     if Assigned(FGlyphList.Items[i])
     then begin
          if Assigned(PmcmGlyph(FGlyphList.Items[i])^.Pattern)
          then FreeMem(PmcmGlyph(FGlyphList.Items[i])^.Pattern);
          FreeMem(PmcmGlyph(FGlyphList.Items[i]));
          FGlyphList.Items[i] := Nil;
     end;
  end;
  FGlyphList.Clear;
end; // TmcmOCR.ClearGlyphs.


procedure TmcmOCR.ClearLines;
var i : longint;
begin
  for i := (FLinesList.Count - 1) downto 0
  do begin
     if Assigned(PGlyphLineRect(FLinesList.Items[i]).CharPos)
     then FreeMem(PGlyphLineRect(FLinesList.Items[i]).CharPos);

     FreeMem(PGlyphLineRect(FLinesList.Items[i]), SizeOf(TGlyphLineRect));
     FLinesList.Items[i] := Nil;
  end;
  FLinesList.Clear;
end; // TmcmOCR.ClearLines.


procedure TmcmOCR.FindPaletteColor;
// Find palette entry representing the glyph color (black or white).
var c : byte;
begin
  if Assigned(FImage)
  then begin
       if (FGlyphColor = GC_BLACK_ON_WHITE)
       then c := 0
       else c := 255;
       FColorIndex := FImage.GetNearestPaletteIndex(RGB(c, c, c));
  end;
end; // TmcmOCR.FindPaletteColor.


function TmcmOCR.GetImage : TmcmImage;
begin
  Result := FInputImage;
end; // TmcmOCR.GetImage.


procedure TmcmOCR.SetImage(Value : TmcmImage);
begin
  FError := EC_OK;
  if (FInputImage <> Value)
  then begin
       ClearLines;
       if Assigned(FImage) and Assigned(FInputImage) and (FInputImage <> FImage)
       then FImage.Free;
       if (Value.ImageFormat = IF_GREY8) 
       then begin
            FImage := Value;
            FInputImage := Value;
            if Assigned(FInputImage)
            then begin
                 FRegion.Left   := 0;
                 FRegion.Top    := 0;
                 FRegion.Right  := FInputImage.Width;
                 FRegion.Bottom := FInputImage.Height;
            end;

            FindPaletteColor;
       end
       else FError := EC_OCRBADCOLORFORMAT;
  end;
end; // TmcmOCR.SetImage.


function TmcmOCR.GetNoLines : integer;
begin
  Result := FLinesList.Count;
end; // TmcmOCR.GetNoLines.


function TmcmOCR.GetLineRect(Index : word) : TRect;
begin
  if (Index < FLinesList.Count)
  then begin
       Result.Left   := PGlyphLineRect(FLinesList.Items[Index]).x1;
       Result.Top    := PGlyphLineRect(FLinesList.Items[Index]).y1;
       Result.Right  := PGlyphLineRect(FLinesList.Items[Index]).x2;
       Result.Bottom := PGlyphLineRect(FLinesList.Items[Index]).y2;
  end;
end; // TmcmOCR.GetLineRect.


procedure TmcmOCR.SetBreakCost(Value : boolean);
begin
  FBreakCost := Value;
end; // TmcmOCR.SetBreakCost.


function TmcmOCR.GetGlyph(Index : word) : PmcmGlyph;
begin
  if (Index < FGlyphList.Count)
  then Result := FGlyphList.Items[Index]
  else Result := Nil;
end; // TmcmOCR.GetGlyph.


function TmcmOCR.GetNoGlyph : word;
begin
  Result := FGlyphList.Count;
end; // TmcmOCR.GetNoGlyph.


function TmcmOCR.GetGlyphColor : TmcmGlyphColor;
begin
  Result := FGlyphColor;
end; // TmcmOCR.GetGlyphColor.


procedure TmcmOCR.SetGlyphColor(Value : TmcmGlyphColor);
begin
  FGlyphColor := Value;
  FindPaletteColor;
end; // TmcmOCR.SetGlyphColor


procedure TmcmOCR.SetGlyphHeight(Value : word);
begin
  if (FGlyphHeight <> Value) and (Value > 2)
  then begin
       FGlyphHeight := Value;
       SetGlyphResize;
  end;
end; // TmcmOCR.SetGlyphHeight.


procedure TmcmOCR.SetGlyphWidth(Value : word);
begin
  if (FGlyphWidth <> Value) and (Value > 2)
  then begin
       FGlyphWidth := Value;
       SetGlyphResize;
  end;
end; // TmcmOCR.SetGlyphWidth.


procedure TmcmOCR.SetGlyphMinHeight(Value : word);
begin
  if (FGlyphMinHeight <> Value) //and (Value < FGlyphMaxHeight)
  then begin
       FGlyphMinHeight := Value;
  end;
end; // TmcmOCR.SetGlyphMinHeight.


procedure TmcmOCR.SetGlyphMinWidth(Value : word);
begin
  if (FGlyphMinWidth <> Value) //and (Value < FGlyphMaxWidth)
  then begin
       FGlyphMinWidth := Value;
  end;
end; //TmcmOCR.SetGlyphMinWidth.


procedure TmcmOCR.SetGlyphMaxHeight(Value : word);
begin
  if (FGlyphMaxHeight <> Value) // and (Value > FGlyphMinHeight)
  then begin
       FGlyphMaxHeight := Value;
  end;
end; // TmcmOCR.SetGlyphMaxHeight.


procedure TmcmOCR.SetGlyphMaxWidth(Value : word);
begin
  if (FGlyphMaxWidth <> Value) //and (Value > FGlyphMinWidth)
  then begin
       FGlyphMaxWidth := Value;
  end;
end; // TmcmOCR.SetGlyphMaxWidth.


procedure TmcmOCR.SetThreshold(Value : TmcmThreshold);
begin
  FThreshold := Value;
end; // TmcmOCR.SetThreshold.


procedure TmcmOCR.SetThresholdLevel(Value : word);
begin
  if (FThresholdLevel <> Value)
  then begin
       FThresholdLevel := Value;
  end;
end; // TmcmOCR.SetThresholdLevel.


procedure TmcmOCR.Threshold;
var ImageColor : TmcmImageColor;
begin
  if (FInputImage = FImage)
  then FImage := TmcmImage.Create;

  if Assigned(FInputImage) and Assigned(FImage)
  then begin
       if (FImage.Width <> FInputImage.Width)
       then FImage.Width  := FInputImage.Width;
       if (FImage.Height <> FInputImage.Height)
       then FImage.Height := FInputImage.Height;
       if (FImage.ImageFormat <> IF_GREY8)
       then begin
            FImage.ImageFormat := IF_GREY8;
            FImage.CreateGreyPalette;
       end;
       ImageColor := TmcmImageColor.Create(Self);
       ImageColor.SourceImage[0] := FInputImage;
       ImageColor.ResultImage := FImage;
       ImageColor.Threshold(FThreshold, FThresholdLevel, 1, 1);
       ImageColor.Free;
  end;
end; // TmcmOCR.Threshold.


procedure TmcmOCR.SetUseAspect(Value : boolean);
begin
  FUseAspect := Value;
end; // TmcmOCR.SetUseAspect.


procedure TmcmOCR.LocateTextLines;
var i, j     : longint;
    pLine    : PVectorB;
    Count    : cardinal;
    Rows     : PVectorC;
    Lefts    : PVectorC;
    Rights   : PVectorC;
    ix1, iy1 : cardinal;
    ix2, iy2 : cardinal;
    lx1, ly1 : cardinal;
    lx2, ly2 : cardinal;
    dy       : longint;
    LineRect : PGlyphLineRect;
begin
  FMaxFoundHeight   := 0;
  FMaxFoundWidth    := 0;
  FExpectGlyphCount := 0;
  FSpaceWidth       := 32767;

  ix1 := FRegion.Left;
  iy1 := FRegion.Top;
  if Assigned(FImage) and (FImage.Height > 0) and (FImage.Width > 0)
  then begin
       ix2 := FRegion.Right;
       iy2 := FRegion.Bottom;
       GetMem(Rows, FImage.Height * SizeOf(cardinal));
       GetMem(Lefts, FImage.Height * SizeOf(cardinal));
       GetMem(Rights, FImage.Height * SizeOf(cardinal));
       try
         for j := iy1 to (iy2 - 1)
         do begin
            // Get number of black pixels in row.
            Count := 0;
            pLine := FImage.ScanLine[j];
            for i := ix1 to (ix2 - 1)
            do begin
               if (pLine[i] = FColorIndex)
               then inc(Count);
            end;
            Rows[j] := Count;

            // If row has black pixels
            if (Rows[j] > 0)
            then begin
                 // Get this row's left (start) position.
                 i := ix1;
                 while (cardinal(i) < ix2) and (pLine[i] <> FColorIndex)
                 do inc(i);
                 Lefts[j] := i; // Row "j" left-most pixel

                 // Get this row's right (end) position.
                 i := ix2 - 1;
                 while (cardinal(i) >= ix1) and (pLine[i] <> FColorIndex)
                 do dec(i);
                 Rights[j] := i; // Row "j" right-most pixel
            end;
         end;

         j := iy1;
         while (cardinal(j) < iy2)
         do begin
            if (Rows[j] > 0)
            then begin
                 lx1 := Lefts[j];
                 lx2 := Rights[j];
                 ly1 := j; // Top of "text" line.
                 while (cardinal(j) < iy2) and (Rows[j] > 0)
                 do begin
                    if (lx1 > Lefts[j])
                    then lx1 := Lefts[j];
                    if (lx2 < Rights[j])
                    then lx2 := Rights[j];
                    inc(j);
                 end;
                 ly2 := j; // Bottom of "text" line.

                 dy := ly2 - ly1;
                 if (FMaxFoundHeight < dy)
                 then FMaxFoundHeight := dy;
                 if (dy > 2) and ((lx2 - lx1) > 2)
                 then begin
                      GetMem(LineRect, SizeOf(TGlyphLineRect));
                      LineRect^.x1 := lx1;
                      LineRect^.y1 := ly1;
                      LineRect^.x2 := lx2;
                      LineRect^.y2 := ly2;
                      LineRect^.cy := ly1 + (ly2 - ly1) div 2;
                      LineRect^.ty := ly1 + (ly2 - ly1) div 3;
                      LineRect^.by := ly1 + 2 * (ly2 - ly1) div 3;
                      GetMem(LineRect^.CharPos, FImage.Width * SizeOf(byte));
                      FillChar(LineRect^.CharPos^, FImage.Width, 0);
                      FLinesList.Add(LineRect);
                 end;
            end;
            inc(j);
         end;
       finally
         FreeMem(Rights);
         FreeMem(Lefts);
         FreeMem(Rows);
       end;

       //
  end;
end; // TmcmOCR.LocateTextLines.


procedure TmcmOCR.LocateGlyphsOnLine(Index : word);
var i, j         : integer;
    dx           : longint;
    pLine        : PVectorB;
    Cols         : PVectorL;
    BreakCost    : PVectorL;
    AveBreakCost : integer;
    {$IFDEF SHOWCHARPOSONIMAGE}
      PalEntry   : TPaletteEntry;
    {$ENDIF}
begin
  if (Index < FLinesList.Count)
  then begin
       {$IFDEF SHOWCHARPOSONIMAGE}
       PalEntry.peRed   := 0;
       PalEntry.peGreen := 255;
       PalEntry.peBlue  := 0;
       PalEntry.peFlags := 0;
       FImage.SetPaletteEntry(2, @PalEntry);

       PalEntry.peRed   := 255;
       PalEntry.peGreen := 0;
       PalEntry.peBlue  := 0;
       PalEntry.peFlags := 0;
       FImage.SetPaletteEntry(3, @PalEntry);

       PalEntry.peRed   := 0;
       PalEntry.peGreen := 0;
       PalEntry.peBlue  := 255;
       PalEntry.peFlags := 0;
       FImage.SetPaletteEntry(4, @PalEntry);

       PalEntry.peRed   := 0;
       PalEntry.peGreen := 192;
       PalEntry.peBlue  := 0;
       PalEntry.peFlags := 0;
       FImage.SetPaletteEntry(5, @PalEntry);

       PalEntry.peRed   := 0;
       PalEntry.peGreen := 128;
       PalEntry.peBlue  := 0;
       PalEntry.peFlags := 0;
       FImage.SetPaletteEntry(9, @PalEntry);
       {$ENDIF}

       FSpaceWidth := 32767;

       with PGlyphLineRect(FLinesList.Items[Index])^
       do begin
          FillMemory(@CharPos[0], FImage.Width * SizeOf(byte), 0);

          GetMem(Cols, FImage.Width * SizeOf(longint));
          GetMem(BreakCost, FImage.Width * SizeOf(longint));
          try
            FillMemory(Cols, FImage.Width * SizeOf(longint), 0);
            FillMemory(BreakCost, FImage.Width * SizeOf(longint), 0);

            // Get horizontal projection of text line.
            for j := y1 to y2
            do begin
               pLine := FImage.ScanLine[j];
               for i := x1 to x2
               do if (pLine[i] = FColorIndex)
                  then inc(Cols[i]);
            end;

            // Determin glyph start and end positions.
            // Only where Columns have zero black pixels.
            i := x1;
            while (i <= x2)
            do begin
               // Find start marker, left position of glyph.
               while (i <= x2) and (Cols[i] = 0)
               do inc(i);
               CharPos[i] := CharPos[i] or GLYPHSTART;

               // Find end marker, right position of glyph.
               j := i;
               while (j <= x2) and (Cols[j] > 0)
               do inc(j);
               if (j > x1)
               then CharPos[j-1] := CharPos[j-1] or GLYPHEND;

               // Calc. glyph width
               dx := j - i;

               // Update Max found glyph width.
               if (FMaxFoundWidth < dx)
               then FMaxFoundWidth := dx;

               // Determind space " " width in pixels.
               if (dx > 2)
               then if (FSpaceWidth > dx)
                    then FSpaceWidth := dx;

               // Increment expected number of glyphs.
               inc(FExpectGlyphCount);

               i := j;
            end;

            if FBreakCost
            then begin
                 // Calculate "Break Cost".
                 // Tsujimoto 1991.
                 // Sum of black pixels having a neighbour in the previous column.
                 // Columns with low or zero black pixels are possible locations to
                 // split glyphs.
                 for j := y1 to y2
                 do begin
                    pLine := FImage.ScanLine[j];
                    for i := (x1 + 1) to x2
                    do if (pLine[i] = FColorIndex) and (pLine[i-1] = FColorIndex)
                       then inc(BreakCost[i]);
                 end;

                 AveBreakCost := 0;
                 j := 0;
                 for i := (x1 + 1) to x2
                 do begin
                    if (BreakCost[i] > 0)
                    then begin
                         inc(j);
                         AveBreakCost := AveBreakCost + BreakCost[i];
                    end;
                 end;
                 if (j > 0)
                 then AveBreakCost := (AveBreakCost div j) - 1
                 else AveBreakCost := 5;
                 if (AveBreakCost < 2)
                 then AveBreakCost := 2;

                 // Determin possible glyph-breaks based on "Break Cost".
                 i := x1;
                 while ((i + 3) < x2)
                 do begin
                    while (i < x2) and (CharPos[i] <> GLYPHSTART)
                    do inc(i);

                    if (FGlyphMinWidth > 3)
                    then inc(i, FGlyphMinWidth - 1)
                    else inc(i, 3);

                    while (i < x2) and (CharPos[i] <> GLYPHEND)
                    do begin
                       while (i < x2) and (CharPos[i] <> GLYPHEND) and
                             (BreakCost[i] <= BreakCost[i+1])
                       do inc(i);
                       while (i < x2) and
                             (CharPos[i] <> GLYPHEND) and
                             (BreakCost[i] >= BreakCost[i+1])
                       do inc(i);
                       if (BreakCost[i] < AveBreakCost)
                       then begin
                            if (CharPos[i] = 0)   and
                               (CharPos[i-1] = 0) and
                               (CharPos[i+1] = 0)
                            then begin
                                 // Mark possible glyph break.
                                 CharPos[i] := GLYPHBREAK + BreakCost[i];

                                 // Increment expected number of glyphs.
                                 inc(FExpectGlyphCount);
                            end;
                       end;
                    end;
                 end;
            end;

            {$IFDEF SHOWCHARPOSONIMAGE}
            // Show start, end and div char markers.
            pLine := FImage.ScanLine[y2+1];
            for i := x1 to x2
            do begin
               case CharPos[i] of
               0 : ;
               1..3 : pLine[i] := CharPos[i] + 1;
               else   pLine[i] := (CharPos[i] and GLYPHBREAK) + 1;
               end;
            end;
            {$ENDIF}

          finally
            FreeMem(BreakCost);
            FreeMem(Cols);
          end;
       end;
       inc(FSpaceWidth); // Space should be one pixel wider than the minimum
                         // pixel count found between two glyphs.
  end;
end; // TmcmOCR.LocateGlyphsOnLine.


procedure TmcmOCR.LocateGlyphs;
var i, h, w : longint;
begin
  if (FLinesList.Count > 0)
  then ClearLines;

  LocateTextLines;
  for i := 0 to (FLinesList.Count - 1)
  do LocateGlyphsOnLine(i);

  h := FMaxFoundHeight;
  if (h < FGlyphHeight)
  then h := FGlyphHeight;
  w := FMaxFoundWidth;
  if (w < FGlyphWidth)
  then w := FGlyphWidth;

  if (FImageResize.Width <> w)
  then FImageResize.Width := w;
  if (FImageResize.Height <> h)
  then FImageResize.Height := h;
end; // TmcmOCR.LocateGlyphs.


procedure TmcmOCR.GetGlyphTopBottom(var Position : TRect);
var i, j        : integer;
    Continue    : boolean;
    pLine       : PVectorB;
begin
  // Based on the left and right glyph positions, determin the top and bottom
  // positions.
  with PGlyphLineRect(FLinesList.Items[FLineIndex])^
  do begin
     Continue := True;
     j := Position.Top - 1;
     while (j <= y2) and Continue
     do begin
        inc(j);
        pLine := FImage.ScanLine[j];
        i := Position.Left;
        while (i <= Position.Right) and Continue
        do begin
           if (pLine[i] = FColorIndex)
           then Continue := False
           else inc(i);
        end;
     end;
     Position.Top := j;

     // Determin correct bottom of glyph.
     Continue := True;
     j := Position.Bottom + 1;
     while (j > y1) and Continue
     do begin
        dec(j);
        pLine := FImage.ScanLine[j];
        i := Position.Left;
        while (i <= Position.Right) and Continue
        do begin
           if (pLine[i] = FColorIndex)
           then Continue := False
           else inc(i);
        end;
     end;
     Position.Bottom := j;
  end;
end; // TmcmOCR.GetGlyphTopBottom.


function TmcmOCR.GetFirstGlyph(var Position : TRect) : integer;
begin
  FLineIndex  := 0;
  FCharPos    := 0;
  FSeparateAt := 0;
  Result := GetNextGlyph(Position);
end; // TmcmOCR.GetFirstGlyph.


function TmcmOCR.GetNextGlyph(var Position : TRect) : integer;
var i, j        : integer;
    Continue    : boolean;
    NewLine     : boolean;
    UseSeparate : boolean;
begin
  // Search for the next character in AOI.
  Continue := True;
  NewLine  := False;
  if (FLineIndex < FLinesList.Count)
  then begin
       while (FLineIndex < FLinesList.Count) and Continue
       do begin
           with PGlyphLineRect(FLinesList.Items[FLineIndex])^
           do begin
              i := FCharPos;

              UseSeparate := False;
              if (FCharPos > 0) and (0 < FSeparateAt) and (FSeparateAt < x2)
              then if (CharPos[FSeparateAt] > GLYPHSTARTEND)
                   then begin
                        i := FSeparateAt;
                        FSeparateAt := 0;
                        UseSeparate := True;
                   end;

              while (i < x2) and Continue
              do begin
                 if Not(UseSeparate)
                 then while (i < x2) and (CharPos[i] <> GLYPHSTART)
                      do inc(i);

                 if (CharPos[i] = GLYPHSTART) or UseSeparate // Have left pos.
                 then begin
                      UseSeparate := False;
                      j := i + 1;
                      while (j < x2) and ((CharPos[j] <> GLYPHEND) and (CharPos[j] <> GLYPHSTART))
                      do inc(j);
                      if (CharPos[j] = GLYPHEND) // Have right pos.
                      then begin
                           FCharPos := j;
                           Position.Left   := i;
                           Position.Right  := j;

                           // Preliminary top and bottom coordinates.
                           Position.Top    := y1;
                           Position.Bottom := y2;

                           // Determin correct top of glyph.
                           GetGlyphTopBottom(Position);
                           if (FGlyphMinHeight <= (Position.Bottom - Position.Top + 1)) and
                              (FGlyphMinWidth  <= (Position.Right - Position.Left + 1))
                           then Continue := False;
                      end;
                      i := j;
                 end;
              end;
           end;
{
           if Not(Continue)
           then begin
                // We have a glyph, but is is small enough.
                if (FGlyphMinHeight <= (Position.Bottom - Position.Top)) and
                   (FGlyphMinWidth  <= (Position.Right - Position.Left))
                then Continue := False;
           end;
}
           if Continue
           then begin
                inc(FLineIndex);
                FCharPos := 0;
                NewLine := True;
           end;
       end;
  end
  else FillChar(Position, SizeOf(TRect), #0);

  if Continue
  then Result := 0
  else if NewLine
       then Result := 2
       else Result := 1;
end; // TmcmOCR.GetNextGlyph.


function TmcmOCR.GetSeparatorGlyph(var Position   : TRect;
                                       Separators : PVectorI;
                                       Ranking    : PVectorB;
                                       Count      : word) : word;
var i, j : cardinal;
begin
  // Fills Separators with guesses on where to separate glyphs.
  // Count is the number of entries Separators can hold.
  j := 0;
  if (FLineIndex < FLinesList.Count)
  then begin
       with PGlyphLineRect(FLinesList.Items[FLineIndex])^
       do begin
          i := Position.Left;
          inc(i);
          while (longint(i) < Position.Right) and (CharPos[i] <> GLYPHEND) and (j < Count)
          do begin
             if (CharPos[i] > GLYPHSTARTEND)
             then begin
                  Separators^[j] := i;
                  if (Ranking <> Nil)
                  then Ranking^[j] := CharPos[i] - GLYPHBREAK;
                  inc(j);
             end;
             inc(i);
          end;
       end;
  end;
  Result := j;
end; // TmcmOCR.GetSeparatorGlyph.


procedure TmcmOCR.SetGlyphResize;
// Allocate memory for FGlyphResize
begin
  FGlyphResize.Height:= FGlyphHeight;
  FGlyphResize.Width := FGlyphWidth;
  if Assigned(FGlyphResize.Pattern)
  then FreeMem(FGlyphResize.Pattern);
  FGlyphResize.Pattern := Nil;
  // Glyph size is double word aligned.
  FGlyphResize.Size := 8 * ((FGlyphHeight * FGlyphWidth + 4) div 8);
  GetMem(FGlyphResize.Pattern, FGlyphResize.Size * SizeOf(byte));
end; // TmcmOCR.SetGlyphResize.


procedure TmcmOCR.FillValue(x, y : integer; Value : byte);
var i : integer;
begin
  i := y * FGlyphResize.Width + x;
  FGlyphResize.Pattern[i] := Value;
  if (x - 1 >= 0)
  then if (FGlyphResize.Pattern[i-1] = 1)
       then FillValue(x-1, y, Value);
  if (x + 1 < FGlyphResize.Width)
  then if (FGlyphResize.Pattern[i+1] = 1)
       then FillValue(x+1, y, Value);
  if (y - 1 >= 0)
  then if (FGlyphResize.Pattern[i-FGlyphResize.Width] = 1)
       then FillValue(x, y-1, Value);
  if (y + 1 < FGlyphResize.Height)
  then if (FGlyphResize.Pattern[i+FGlyphResize.Width] = 1)
       then FillValue(x, y+1, Value);
end; // TmcmImage.FillValue.


function TmcmOCR.ResizeGlyph(Position : TRect) : PmcmGlyph;
// Resize found glyph and place this in FGlyphResize.
var HScale  : single;
    VScale  : single;
    gh, gw  : integer;
    pS, pT  : PVectorB;
    LinDir  : integer;
    x, y, z : integer;
    nx, ny  : integer;

    //pS2     : PVectorB;
    //rx, ry  : double;
    //dx, dy  : double;
    //v1, v2  : double;
    //nx2, ny2  : integer;
begin
  gh := (Position.Bottom - Position.Top) + 1;
  gw := (Position.Right - Position.Left) + 1;

  HScale := (gw - 1) / (FGlyphWidth - 1);
  VScale := (gh - 1) / (FGlyphHeight - 1);

  FImageResize.FillAll(0);

  LinDir := 0;
  if (HScale > 1.0)
  then begin
       inc(LinDir);
       HScale := 1.0 / HScale;
  end;
  if (VScale > 1.0)
  then begin
       inc(LinDir, 2);
       VScale := 1.0 / VScale;
  end;

  // ST_NEAREST
  case LinDir of
  0 : // Glyph is horizonatally & vertically smaller than Template
      for y := 0 to (FGlyphHeight - 1)
      do begin
         ny := Round(y * VScale);
         pS := FImage.ScanLine[ny+Position.Top];
         inc(pS, Position.Left);
         pT := FImageResize.ScanLine[y];

         for x := 0 to (FGlyphWidth - 1)
         do begin
              // Calculate nearest point.
              nx := Round(x * HScale);
              if (pS[nx] = FColorIndex)
              then pT[nx] := 1;
         end;
      end;
  1 : // Glyph is horizonatally larger & vertically smaller than Template
      for y := 0 to (FGlyphHeight - 1)
      do begin
         ny := Round(y * VScale);
         pS := FImage.ScanLine[ny+Position.Top];
         inc(pS, Position.Left);
         pT := FImageResize.ScanLine[y];

         for x := 0 to (gw - 1)
         do begin
              // Calculate nearest point.
              nx := Round(x * HScale);
              if (pS[x] = FColorIndex)
              then pT[nx] := 1;
         end;
      end;
  2 : // Glyph is horizonatally smaller & vertically larger than Template
      for y := 0 to (gh - 1)
      do begin
         ny := Round(y * VScale);
         pS := FImage.ScanLine[y+Position.Top];
         inc(pS, Position.Left);
         pT := FImageResize.ScanLine[ny];

         for x := 0 to (FGlyphWidth - 1)
         do begin
              // Calculate nearest point.
              nx := Round(x * HScale);
              if (pS[nx] = FColorIndex)
              then pT[nx] := 1;
         end;
      end;
  3 : // Glyph is horizonatally & vertically larger than Template
      for y := 0 to (gh - 1)
      do begin
         ny := Round(y * VScale);
         pS := FImage.ScanLine[y+Position.Top];
         inc(pS, Position.Left);
         pT := FImageResize.ScanLine[ny];

         for x := 0 to (gw - 1)
         do begin
              // Calculate nearest point.
              nx := Round(x * HScale);
              if (pS[x] = FColorIndex)
              then pT[nx] := 1;
         end;
      end;
  end;

{
  // ST_BILINEAR
  for y := 0 to (FGlyphHeight - 1)
  do begin
     ry := y * VScale;
     ny := Round(ry - 0.4999);
     dy := ry - ny;

     ny2 := ny + 1;

     pS := FImage.ScanLine[ny+Position.Top];
     pS2 := FImage.ScanLine[ny2+Position.Top];

     inc(pS, Position.Left);
     inc(pS2, Position.Left);
     pT := FImageResize.ScanLine[y];

     for x := 0 to (FGlyphWidth - 1)
     do begin
          z := 0;
          // Calculate nearest point.
          rx := x * HScale;
          nx := Round(rx - 0.4999);
          dx := rx - nx;
          nx2 := nx + 1;

          v1 := (1.0 - dx) * pS[nx] + dx * pS[nx2];
          v2 := (1.0 - dx) * pS2[nx] + dx * pS2[nx2];
          v1 := (1.0 - dy) * v1 + dy * v2;


          if (v1 < 160)
          then inc(pT[x]);

     end;
  end;
}

  // Move found feature to FGlyphResize
  z := 0;
  for y := 0 to (FGlyphHeight - 1)
  do begin
     pS := FImageResize.ScanLine[y];
     for x := 0 to (FGlyphWidth - 1)
     do begin
        if (pS[x] = 0)
        then FGlyphResize.Pattern[z] := OCRHOLE
        else FGlyphResize.Pattern[z] := OCRFEATURE;
        inc(z);
     end;
  end;

  if (FMatchMethod = OMM_FEATUREPIXELS)
  then begin
       // Mark background pixels, so that enclosed background regions,
       // i.e. holes in features, are destinct.
       for x := 0 to (FGlyphResize.Width - 1)
       do if (FGlyphResize.Pattern[x] = 1)
          then FillValue(x, 0, OCRBACKGROUND);

       y := (FGlyphResize.Height - 1) * FGlyphResize.Width;
       for x := y to (y + FGlyphResize.Width - 1)
       do if (FGlyphResize.Pattern[x] = 1)
          then FillValue(x-y, FGlyphResize.Height - 1, OCRBACKGROUND);

       y := FGlyphResize.Width;
       for x := 1 to (FGlyphResize.Height - 1)
       do begin
          if (FGlyphResize.Pattern[y] = 1)
          then FillValue(0, x, OCRBACKGROUND);
          inc(y, FGlyphResize.Width);
       end;

       y := 2 * FGlyphResize.Width - 1;
       for x := 1 to (FGlyphResize.Height - 1)
       do begin
          if (FGlyphResize.Pattern[y] = 1)
          then FillValue(FGlyphResize.Width - 1, x, OCRBACKGROUND);
          inc(y, FGlyphResize.Width);
       end;
  end;

  if (gw > 0)
  then FGlyphResize.Aspect := 1.0 * gh / gw
  else FGlyphResize.Aspect := 1.0;

  Result := @FGlyphResize;
end; // TmcmOCR.ResizeGlyph.


function TmcmOCR.AddGlyph(Identifier : WideChar;
                          Group      : TmcmGlyphGroup;
                          Position   : TRect) : integer;
var NewGlyph : PmcmGlyph;
    k        : integer;
    Err      : single;
    Replace  : boolean;
begin
  // Check that the glyph is not in the library already.
  k := Match(Err, Position);
  if (Err = 0.0)
  then begin
       Replace := FReplaceGlyph;
       if Assigned(FOnDuplicateGlyph)
       then FOnDuplicateGlyph(Self, k, Replace);
       if Replace
       then begin
            // Existing glyph is deleted and the new is added below.
            DeleteGlyph(k);
            Err := 1.0;
       end;
  end;

  k := -1;
  if (Err <> 0.0)
  then begin
       GetMem(NewGlyph, SizeOf(TmcmGlyph));
       if Assigned(NewGlyph)
       then begin
            NewGlyph^.Identifier := Identifier;
            NewGlyph^.Group      := word(Group);
            NewGlyph^.Height     := FGlyphHeight;
            NewGlyph^.Width      := FGlyphWidth;
            NewGlyph^.Size       := FGlyphResize.Size;
            NewGlyph^.Reserved   := 0;
            NewGlyph^.Aspect     := FGlyphResize.Aspect;
            GetMem(NewGlyph^.Pattern, NewGlyph^.Size * SizeOf(byte));
            if Assigned(NewGlyph^.Pattern)
            then begin
                 CopyMemory(NewGlyph^.Pattern, FGlyphResize.Pattern, NewGlyph^.Size * SizeOf(Byte));
                 k := FGlyphList.Add(NewGlyph);
            end
            else FreeMem(NewGlyph);
       end;
  end;
  Result := k;
end; // TmcmOCR.AddGlyph.


procedure TmcmOCR.DeleteGlyph(Index : integer);
begin
  if (0 <= Index) and (Index < FGlyphList.Count)
  then begin
     if Assigned(FGlyphList.Items[Index])
     then begin
          if Assigned(PmcmGlyph(FGlyphList.Items[Index])^.Pattern)
          then FreeMem(PmcmGlyph(FGlyphList.Items[Index])^.Pattern);
          FreeMem(PmcmGlyph(FGlyphList.Items[Index]));
          FGlyphList.Items[Index] := Nil;
     end;
     FGlyphList.Delete(Index);
  end;
end; // TmcmOCR.DeleteGlyph.


procedure TmcmOCR.SetMatchMethod(Value : TmcmGlyphMatch);
begin
  if (FMatchMethod <> Value)
  then begin
       FMatchMethod := Value;
       case FMatchMethod of
       OMM_ALLPIXELS     : FMatchMethodProc := MatchPixels;
       OMM_FEATUREPIXELS : FMatchMethodProc := MatchPixelFeature;
       end;
  end;
end; // TmcmOCR.SetMatchMethod.


function TmcmOCR.MatchAndCut(var Error : single; var Position : TRect) : integer;
var i, Index   : integer;
    Count      : integer;
    Separators : array[0..9] of integer;
    MinError   : single;
    ARect      : TRect;
    ARight     : integer;
    ATop       : integer;
    ABottom    : integer;
begin
  Index := -1;
  MinError := 1.0;
  ARect   := Position;
  ARight  := Position.Right;
  ATop    := ARect.Top;
  ABottom := ARect.Bottom;
  Count := GetSeparatorGlyph(Position, @Separators, Nil, 10);

  repeat
    ResizeGlyph(ARect);
    i := FMatchMethodProc(Error);
    if (MinError > Error)
    then begin
         MinError := Error;
         Index   := i;
         ARight  := ARect.Right;
         ATop    := ARect.Top;
         ABottom := ARect.Bottom;
         if (FCharPos > ARight)
         then FSeparateAt := ARight + 1;
    end;
    dec(Count);
    if (Count >= 0)
    then begin
         ARect.Right := Separators[Count] - 1;
         GetGlyphTopBottom(ARect);
    end;
  until (Count < 0);

  Error := MinError;
  Position.Right  := ARight;
  Position.Top    := ATop;
  Position.Bottom := ABottom;

  Result := Index;
end; // TmcmOCR.MatchAndCut.


function TmcmOCR.Match(var Error : single; Position : TRect) : integer;
begin
  if (FSeparateAt <> 0)
  then if (Position.Right > FSeparateAt - 1)
       then Position.Right := FSeparateAt - 1;
  //GetGlyphTopBottom(Position);
  ResizeGlyph(Position);
  Result := FMatchMethodProc(Error);
end; // TmcmOCR.Match.


function TmcmOCR.MatchPixels(var Error : single) : integer;
var Index      : longint;
    i, j       : integer;
    Mm, Mp     : integer;
    Score, MNI : single;
    Template   : PmcmGlyph;
    pT, pG     : pointer;
    Count      : integer;
begin
  // Match templates
  // Use Normalised Match Index (NMI).
  //
  Index := -1;
  Score := 0.0;

  Count := FGlyphResize.Size div 8 - 1;
  pG := @FGlyphResize.Pattern[0];

  for i := 0 to (FGlyphList.Count - 1)
  do begin
     if Assigned(FGlyphList.Items[i])
     then begin
          Mm := 0;
          Mp := 0;
          Template := FGlyphList.Items[i];
          pT := @Template^.Pattern[0];

          if FMMX
          then begin
               asm
                 // Save registers to stack
                 push      ebx
                 push      edi
                 push      esi

                 // data points to process.
                 mov       ecx,Count

                 // Set-up initial pointers to coordinates and delta.
                 mov       edi,pT
                 mov       esi,pG

                 // Compare patterns
                 {$IFNDEF DCB3_5}
                 pxor      mm6,mm6
                 pxor      mm7,mm7

                 @LoopQWORD:
                 movq      mm0,[edi+ecx*8]
                 movq      mm1,[esi+ecx*8]
                 pcmpeqb   mm1,mm0

                 movq      mm0,mm1  // Matching pixel

                 pxor      mm2,mm2
                 pcmpeqb   mm2,mm2  // mm2 = FFFFFFFF
                 pandn     mm1,mm2  // Non-matching pixels

                 pxor      mm3,mm3  // Convert FF to 1
                 psubb     mm3,mm0
                 paddb     mm6,mm3  // add counts to mm6

                 pxor      mm4,mm4  // Convert FF to 1
                 psubb     mm4,mm1
                 paddb     mm7,mm4  // add counts to mm7

                 dec       ecx
                 jns       @LoopQWORD

                 // Add up the 8 byte counts in mm6 - matching pixels
                 punpckhbw mm0,mm6
                 psrlw     mm0,$08
                 punpcklbw mm1,mm6
                 psrlw     mm1,$08
                 paddw     mm0,mm1 // mm0 contains 4 words

                 punpckhwd mm1,mm0
                 psrld     mm1,$10
                 punpcklwd mm2,mm0
                 psrld     mm2,$10
                 paddd     mm1,mm2 // mm0 contains 2 dwords

                 movq      mm0,mm1
                 psrlq     mm1,$20
                 paddw     mm0,mm1 // mm0 = no. matching pixels

                 // Add up the 8 byte counts in mm7 - non-matching pixels
                 punpckhbw mm1,mm7
                 psrlw     mm1,$08
                 punpcklbw mm2,mm7
                 psrlw     mm2,$08
                 paddw     mm1,mm2 // mm1 contains 4 words

                 punpckhwd mm2,mm1
                 psrld     mm2,$10
                 punpcklwd mm3,mm1
                 psrld     mm3,$10
                 paddd     mm2,mm3 // mm2 contains 2 dwords

                 movq      mm1,mm2
                 psrlq     mm2,$20
                 paddw     mm1,mm2 // mm1 = no. non-matching pixels

                 movd      dword ptr Mp,mm0
                 movd      dword ptr Mm,mm1
                 { Solution for DCB 3-5 for above two lines in db section.
                 movd      eax,mm0
                 movd      ebx,mm1
                 mov       Mp,eax
                 mov       Mm,ebx
                 }
                 {$ELSE}

                 db $0F,$EF,$F6
                 db $0F,$EF,$FF

                 @LoopQWORD:
                 db $0F,$6F,$04,$CF
                 db $0F,$6F,$0C,$CE
                 db $0F,$74,$C8

                 db $0F,$6F,$C1

                 db $0F,$EF,$D2
                 db $0F,$74,$D2
                 db $0F,$DF,$CA

                 db $0F,$EF,$DB
                 db $0F,$F8,$D8
                 db $0F,$FC,$F3

                 db $0F,$EF,$E4
                 db $0F,$F8,$E1
                 db $0F,$FC,$FC

                 dec       ecx
                 jns       @LoopQWORD

                 // Add up the 8 byte counts in mm6 - matching pixels
                 db $0F,$68,$C6
                 db $0F,$71,$D0,$08
                 db $0F,$60,$CE
                 db $0F,$71,$D1,$08
                 db $0F,$FD,$C1

                 db $0F,$69,$C8
                 db $0F,$72,$D1,$10
                 db $0F,$61,$D0
                 db $0F,$72,$D2,$10
                 db $0F,$FE,$CA

                 db $0F,$6F,$C1
                 db $0F,$73,$D1,$20
                 db $0F,$FD,$C1

                 // Add up the 8 byte counts in mm7 - non-matching pixels
                 db $0F,$68,$CF
                 db $0F,$71,$D1,$08
                 db $0F,$60,$D7
                 db $0F,$71,$D2,$08
                 db $0F,$FD,$CA

                 db $0F,$69,$D1
                 db $0F,$72,$D2,$10
                 db $0F,$61,$D9
                 db $0F,$72,$D3,$10
                 db $0F,$FE,$D3

                 db $0F,$6F,$CA
                 db $0F,$73,$D2,$20
                 db $0F,$FD,$CA

                 // Move results to eax, ebx
                 db $0F,$7E,$C0
                 db $0F,$7E,$CB
                 mov       Mp,eax
                 mov       Mm,ebx
                 {$ENDIF}

                 @EndOfData:

                 // Restore stack
                 pop       esi
                 pop       edi
                 pop       ebx

                 // Empty EMMS registers.
                 {$IFNDEF DCB3_5}
                 emms
                 {$ELSE}
                 db        $0F,$77  // emms - clean-up que.
                 {$ENDIF}
               end;
          end
          else begin
               for j := 0 to (Template^.Size - 1)
               do begin
                  if (Template^.Pattern[j] = FGlyphResize.Pattern[j])
                  then inc(Mp)
                  else inc(Mm);
               end;
          end;

          try
            MNI := 1 + ((Mp - Mm) / (Mp + Mm));
          except
            MNI := 1.0;
          end;

          if FUseAspect
          then begin
               if (Template^.Aspect < FGlyphResize.Aspect)
               then MNI := MNI * Template^.Aspect / FGlyphResize.Aspect
               else MNI := MNI * FGlyphResize.Aspect / Template^.Aspect;
          end;

          if (Score < MNI)
          then begin
               Score := MNI;
               Index := i;
          end;
     end;
  end;
  Error := 1.0 - 0.5 * Score;
  Result := Index;
end; // TmcmOCR.MatchPixels.


function TmcmOCR.MatchPixelFeature(var Error : single) : integer;
var Index      : longint;
    i, j       : integer;
    Mm, Mp     : integer;
    Score, MNI : single;
    Template   : PmcmGlyph;
    pT, pG     : pointer;
    Count      : integer;
begin
  // Match templates including feature pixels.
  // Use Normalised Match Index (NMI).
  //
  Index := -1;
  Score := 0.0;

  Count := FGlyphResize.Size div 8 - 1;
  pG := @FGlyphResize.Pattern[0];

  for i := 0 to (FGlyphList.Count - 1)
  do begin
     if Assigned(FGlyphList.Items[i])
     then begin
          Mm := 0;
          Mp := 0;
          Template := FGlyphList.Items[i];
          pT := @Template^.Pattern[0];
          
          if FMMX 
          then begin
               asm
                 // Save registers to stack
                 push      ebx
                 push      edi
                 push      esi

                 // data points to process.
                 mov       ecx,Count

                 // Set-up initial pointers to coordinates and delta.
                 mov       edi,pT
                 mov       esi,pG

                 // Compare patterns
                 {$IFNDEF DCB3_5}
                 pxor      mm6,mm6
                 pxor      mm7,mm7

                 pxor      mm4,mm4
                 pxor      mm5,mm5
                 pcmpeqb   mm4,mm4  // mm2 = FFFFFFFF
                 psubb     mm5,mm4  //
                 paddb     mm5,mm5  // mm5 = 2

                 @LoopQWORD:
                 movq      mm0,[edi+ecx*8]
                 movq      mm1,[esi+ecx*8]

                 movq      mm3,mm5
                 movq      mm4,mm5

                 pcmpgtb   mm3,mm0 // mm4 = Background filter
                 pcmpgtb   mm4,mm1 // mm4 = Background filter
                 por       mm4,mm3

                 pcmpeqb   mm1,mm0
                 movq      mm0,mm1  // Matching pixel

                 pxor      mm2,mm2
                 pcmpeqb   mm2,mm2  // mm2 = FFFFFFFF
                 pandn     mm1,mm2  // Non-matching pixels

                 pand      mm0,mm4  // Disregard background pixels
                 pand      mm1,mm4  // -

                 pxor      mm3,mm3  // Convert FF to 1
                 psubb     mm3,mm0
                 paddb     mm6,mm3  // add counts to mm6

                 pxor      mm4,mm4  // Convert FF to 1
                 psubb     mm4,mm1
                 paddb     mm7,mm4  // add counts to mm7

                 dec       ecx
                 jns       @LoopQWORD

                 // Add up the 8 byte counts in mm6 - matching pixels
                 punpckhbw mm0,mm6
                 psrlw     mm0,$08
                 punpcklbw mm1,mm6
                 psrlw     mm1,$08
                 paddw     mm0,mm1 // mm0 contains 4 words

                 punpckhwd mm1,mm0
                 psrld     mm1,$10  // 0f 72 d1 10
                 punpcklwd mm2,mm0
                 psrld     mm2,$10
                 paddd     mm1,mm2 // mm0 contains 2 dwords

                 movq      mm0,mm1
                 psrlq     mm1,$20
                 paddw     mm0,mm1 // mm0 = no. matching pixels

                 // Add up the 8 byte counts in mm7 - non-matching pixels
                 punpckhbw mm1,mm7
                 psrlw     mm1,$08
                 punpcklbw mm2,mm7
                 psrlw     mm2,$08
                 paddw     mm1,mm2 // mm1 contains 4 words

                 punpckhwd mm2,mm1
                 psrld     mm2,$10
                 punpcklwd mm3,mm1
                 psrld     mm3,$10
                 paddd     mm2,mm3 // mm2 contains 2 dwords

                 movq      mm1,mm2
                 psrlq     mm2,$20
                 paddw     mm1,mm2 // mm1 = no. non-matching pixels

                 movd      Mp,mm0
                 movd      Mm,mm1
                 {$ELSE}
                 db $0F,$EF,$F6
                 db $0F,$EF,$FF

                 db $0F,$EF,$E4
                 db $0F,$EF,$ED
                 db $0F,$74,$E4
                 db $0F,$F8,$EC
                 db $0F,$FC,$ED

                 @LoopQWORD:
                 db $0F,$6F,$04,$CF
                 db $0F,$6F,$0C,$CE

                 db $0F,$6F,$DD
                 db $0F,$6F,$E5

                 db $0F,$64,$D8
                 db $0F,$64,$E1
                 db $0F,$EB,$E3

                 db $0F,$74,$C8
                 db $0F,$6F,$C1

                 db $0F,$EF,$D2
                 db $0F,$74,$D2
                 db $0F,$DF,$CA

                 db $0F,$DB,$C4
                 db $0F,$DB,$CC

                 db $0F,$EF,$DB
                 db $0F,$F8,$D8
                 db $0F,$FC,$F3

                 db $0F,$EF,$E4
                 db $0F,$F8,$E1
                 db $0F,$FC,$FC

                 dec       ecx
                 jns       @LoopQWORD

                 // Add up the 8 byte counts in mm6 - matching pixels
                 db $0F,$68,$C6
                 db $0F,$71,$D0,$08
                 db $0F,$60,$CE
                 db $0F,$71,$D1,$08
                 db $0F,$FD,$C1

                 db $0F,$69,$C8
                 db $0F,$72,$D1,$10
                 db $0F,$61,$D0
                 db $0F,$72,$D2,$10
                 db $0F,$FE,$CA

                 db $0F,$6F,$C1
                 db $0F,$73,$D1,$20
                 db $0F,$FD,$C1

                 // Add up the 8 byte counts in mm7 - non-matching pixels
                 db $0F,$68,$CF
                 db $0F,$71,$D1,$08
                 db $0F,$60,$D7
                 db $0F,$71,$D2,$08
                 db $0F,$FD,$CA

                 db $0F,$69,$D1
                 db $0F,$72,$D2,$10
                 db $0F,$61,$D9
                 db $0F,$72,$D3,$10
                 db $0F,$FE,$D3

                 db $0F,$6F,$CA
                 db $0F,$73,$D2,$20
                 db $0F,$FD,$CA

                 // Move results to eax, ebx
                 db $0F,$7E,$C0
                 db $0F,$7E,$CB
                 mov       Mp,eax
                 mov       Mm,ebx
                 {$ENDIF}

                 @EndOfData:

                 // Restore stack
                 pop       esi
                 pop       edi
                 pop       ebx

                 // Empty EMMS registers.
                 {$IFNDEF DCB3_5}
                 emms
                 {$ELSE}
                 db        $0F,$77  // emms - clean-up que.
                 {$ENDIF}
               end;
          end
          else begin
               for j := 0 to (Template^.Size - 1)
               do begin
                  if (FGlyphResize.Pattern[j] < OCRBACKGROUND) or
                     (Template^.Pattern[j]    < OCRBACKGROUND)
                  then if (Template^.Pattern[j] = FGlyphResize.Pattern[j])
                       then inc(Mp)
                       else inc(Mm);
               end;
          end;
          try
            MNI := 1 + ((Mp - Mm) / (Mp + Mm));
          except
            MNI := 1.0;
          end;
          if FUseAspect
          then begin
               if (Template^.Aspect < FGlyphResize.Aspect)
               then MNI := MNI * Template^.Aspect / FGlyphResize.Aspect
               else MNI := MNI * FGlyphResize.Aspect / Template^.Aspect;
          end;

          if (Score < MNI)
          then begin
               Score := MNI;
               Index := i;
          end;
     end;
  end;
  Error := 1.0 - 0.5 * Score;
  Result := Index;
end; // TmcmOCR.MatchPixelFeature.


procedure TmcmOCR.ReadText;
var Continue : integer;
    ARect    : TRect;
    OCRStr   : Widestring;
    OCRLen   : word;
    LastPos  : longint;
    i        : integer;
    OCRIndex : integer;
    Error    : single;
begin
  // Read the image/region.
  if (FGlyphList.Count > 0)
  then begin
       if (FLinesList.Count > 0)
       then begin
            FStringList.Capacity := FStringList.Capacity + FLinesList.Count;
            Continue := GetFirstGlyph(ARect);
            i := 1;
            OCRLen := 128;
            SetLength(OCRStr, OCRLen);
            while (Continue <> 0)
            do begin
               if (Continue = 2)
               then begin
                    // Add read glyph line to list of strings.
                    OCRStr[i] := #0;
                    FStringList.Add(OCRStr);
                    i := 1;
                    OCRStr[i] := #0;
               end;

               ResizeGlyph(ARect);
               OCRIndex := MatchAndCut(Error, ARect);
               if (OCRIndex >= 0)
               then begin
                    OCRStr[i] := Glyph[OCRIndex].Identifier;
                    inc(i);
                    if (i >= OCRLen)
                    then begin
                         inc(OCRLen, 128);
                         SetLength(OCRStr, OCRLen);
                    end;
               end;
               LastPos := ARect.Right;
               Continue := GetNextGlyph(ARect);
               if ((ARect.Left - LastPos) > FSpaceWidth) and FAddSpaces
               then OCRStr := OCRStr + ' ';
            end;

            // Add lasty line to string list.
            if (OCRStr <> '')
            then begin
                 OCRStr[i] := #0;
                 FStringList.Add(OCRStr);
            end;
       end;
  end;
end; // TmcmOCR.ReadText.


function TmcmOCR.GetRegion : TRect;
begin
  // Returned search region.
  Result := FRegion;
end; // TmcmOCR.GetRegion.


procedure TmcmOCR.SetRegion(Value : TRect);
begin
  // Set region in which to search for "text" lines.
  // Used if not all of the image area is to be used.
  // Note, this region is cleared when assigning an image to the Image property.
  FRegion := Value;
  if (FRegion.Left < 0)
  then FRegion.Left := 0;
  if (FRegion.Top < 0)
  then FRegion.Top := 0;
  if (FRegion.Right < 0)
  then FRegion.Right := 0;
  if (FRegion.Bottom < 0)
  then FRegion.Bottom := 0;

  if Assigned(FImage)
  then begin
       ClearLines;
       if (FRegion.Left >= FImage.Width)
       then FRegion.Left := FImage.Width;
       if (FRegion.Top >= FImage.Height)
       then FRegion.Top := FImage.Height;
       if (FRegion.Right >= FImage.Width)
       then FRegion.Right := FImage.Width;
       if (FRegion.Bottom >= FImage.Height)
       then FRegion.Bottom := FImage.Height;
  end;
end; // TmcmOCR.SetRegion.


function TmcmOCR.GetString(Index : integer) : string;
begin
  if (Index < FStringList.Count)
  then Result := FStringList.Strings[Index];
end; // TmcmOCR.GetString.


function TmcmOCR.OpenFile(Filename : string; Mode : word; var Stream : TFileStream) : boolean;
begin
  FError := EC_OK;
  try
    // Lock;
    Stream := TFileStream.Create(FileName, Mode);
  except
    Stream.Free;
    Stream := Nil;
    if ((Mode and fmCreate) <> 0)
    then FError := EC_CREATEFILE
    else FError := EC_OPENFILE;
  end;
  //if Not(Assigned(Stream))
  //then Unlock;
  Result := Assigned(Stream);
end; // TmcmOCR.OpenFile.


procedure TmcmOCR.CloseFile(var Stream : TFileStream);
begin
  if Assigned(Stream)
  then begin
       Stream.Free;
       Stream := Nil;
       // Unlock;
  end;
end; // TmcmOCR.CloseFile.


procedure TmcmOCR.LoadFromStream(Stream : TStream);
var TmpStr   : string[6];
    i, Count : integer;
    NewGlyph : PmcmGlyph;
begin
  Clear;
  Count := -1;
  if Assigned(Stream)
  then begin
       // Read OCR Identifier
       SetLength(TmpStr, 6);
       Stream.Read(TmpStr[1], 6);

       if (TmpStr = mcmOCRId)
       then begin
            // Read version number
            Stream.Read(FFileVer, SizeOf(word));
            if (FFileVer >= 100) // FFileVer := mcmOCRVersion;
            then begin
                 // Write default OCR settings.
                 Stream.Read(FMatchMethod, SizeOf(TmcmGlyphMatch));
                 Stream.Read(FGlyphHeight, SizeOf(word));
                 Stream.Read(FGlyphWidth, SizeOf(word));
                 Stream.Read(FGlyphColor, SizeOf(byte));
                 Stream.Read(FBreakCost, SizeOf(boolean));
                 Stream.Read(FUseAspect, SizeOf(boolean));
                 Stream.Read(FGlyphMinHeight, SizeOf(word));
                 Stream.Read(FGlyphMinWidth, SizeOf(word));
                 Stream.Read(FGlyphMaxHeight, SizeOf(word));
                 Stream.Read(FGlyphMaxWidth, SizeOf(word));
                 Stream.Read(FGlyphMinSpace, SizeOf(word));
                 Stream.Read(FThreshold, SizeOf(integer));
                 Stream.Read(FThresholdLevel, SizeOf(word));

                 // Read number of glyphs stored in this file
                 Stream.Read(Count, SizeOf(integer));

                 // Read glyphs
                 for i := 0 to (Count - 1)
                 do begin
                    GetMem(NewGlyph, SizeOf(TmcmGlyph));
                    if Assigned(NewGlyph)
                    then begin
                         Stream.Read(NewGlyph^, SizeOf(TmcmGlyph) - SizeOf(PVectorB));
                         GetMem(NewGlyph^.Pattern, NewGlyph^.Size * SizeOf(byte));
                         if Assigned(NewGlyph^.Pattern)
                         then Stream.Read(NewGlyph^.Pattern^, NewGlyph^.Size * SizeOf(byte))
                         else begin
                              FreeMem(NewGlyph);
                              NewGlyph := Nil;
                         end;
                         if Assigned(NewGlyph)
                         then FGlyphList.Add(NewGlyph);
                    end
                    else Count := -1;
                 end;

                 // Initialise FGlyphResize.
                 SetGlyphResize;
                 FindPaletteColor;
            end;
       end;
  end;
end; // TmcmOCR.LoadFromStream.


procedure TmcmOCR.SaveToStream(Stream : TStream);
var i : integer;
begin
  if Assigned(Stream)
  then begin
       // Write OCR Identifier
       Stream.Write(mcmOCRId[1], 6);

       // Write version number
       Stream.Write(mcmOCRVersion, SizeOf(word));
       FFileVer := mcmOCRVersion;

       // Write default OCR settings.
       Stream.Write(FMatchMethod, SizeOf(TmcmGlyphMatch));
       Stream.Write(FGlyphHeight, SizeOf(word));
       Stream.Write(FGlyphWidth, SizeOf(word));
       Stream.Write(FGlyphColor, SizeOf(byte));
       Stream.Write(FBreakCost, SizeOf(boolean));
       Stream.Write(FUseAspect, SizeOf(boolean));
       Stream.Write(FGlyphMinHeight, SizeOf(word));
       Stream.Write(FGlyphMinWidth, SizeOf(word));
       Stream.Write(FGlyphMaxHeight, SizeOf(word));
       Stream.Write(FGlyphMaxWidth, SizeOf(word));
       Stream.Write(FGlyphMinSpace, SizeOf(word));
       Stream.Write(FThreshold, SizeOf(integer));
       Stream.Write(FThresholdLevel, SizeOf(word));

       // Write number of glyphs stored in this file
       Stream.Write(FGlyphList.Count, SizeOf(integer));

       // Write glyphs
       for i := 0 to (FGlyphList.Count - 1)
       do begin
          Stream.Write(PmcmGlyph(FGlyphList.Items[i])^, SizeOf(TmcmGlyph) - SizeOf(PVectorB));
          Stream.Write(PmcmGlyph(FGlyphList.Items[i])^.Pattern^, PmcmGlyph(FGlyphList.Items[i])^.Size * SizeOf(byte));
       end;
  end;
end; // TmcmOCR.SaveToStream.


procedure TmcmOCR.LoadFromFile(FileName : string);
var Stream : TFileStream;
begin
  if OpenFile(Filename, fmOpenRead or fmShareDenyWrite, Stream)
  then begin
       try
         LoadFromStream(Stream);
       finally
         CloseFile(Stream);
       end;
  end;
end; // TmcmOCR.LoadFromFile.


procedure TmcmOCR.SaveToFile(FileName : string);
var Stream : TFileStream;
begin
  if OpenFile(Filename, fmCreate or fmOpenWrite or fmShareExclusive, Stream)
  then begin
       try
         SaveToStream(Stream);
       finally
         CloseFile(Stream);
       end;
  end;
end; // TmcmOCR.SaveToFile.


procedure TmcmOCR.About;
begin
  mcmOcrAboutBox := TmcmOcrAboutBox.Create(Nil);
  {$IFDEF MCMDEMO}
  mcmOcrAboutBox.DemoVersion := True;
  {$ENDIF}
  mcmOcrAboutBox.ShowModal;
  mcmOcrAboutBox.Free;
end; // TmcmOCR.About.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

initialization
  {$IFDEF MCMDEMO}
    if ((FindWindow('TApplication', 'Delphi 3') = 0) and
        (FindWindow('TApplication', 'Delphi 4') = 0) and
        (FindWindow('TApplication', 'Delphi 5') = 0) and
        (FindWindow('TApplication', 'Delphi 6') = 0) and
        (FindWindow('TApplication', 'Delphi 7') = 0) and
        (FindWindow('TApplication', 'C++Builder') = 0)) or
       (FindWindow('TAlignPalette', nil) = 0) or
       (FindWindow('TAppBuilder', nil) = 0)
    then begin
         mcmOcrAboutBox := TmcmOcrAboutBox.Create(Nil);
         mcmOcrAboutBox.DemoVersion := True;
         mcmOcrAboutBox.ShowModal;
         mcmOcrAboutBox.Free;
         mcmOcrAboutBox := Nil;
    end;
  {$ENDIF}
end.

