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
// $Log:  20273: mcmImageMorph.pas 
//
//    Rev 1.12    2014-02-02 21:10:04  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.11    21-05-2006 21:08:44  mcm
// Modified Outline to generate outline lines next to the image edges when
// setting the property EdgeOutline to True.
//
//    Rev 1.10    20/05/2006 12:37:10  mcm
// Added Euclidean Distance Map method.
//
//   Rev 1.9    15-05-2005 19:40:44  mcm    Version: IMG 2.9
// Moved TNeighbour record to mcmImageTypeDef.

//
//   Rev 1.8    20-02-2005 13:45:28  mcm
// Fixed missing return/error value if a ResultImage wasn't created.

//
//   Rev 1.7    03-02-2005 21:17:44  mcm
// Modified HasNeighbour to check for max posible end-x coordinate.

//
//   Rev 1.6    28-10-2004 19:22:30  mcm    Version: IMG 2.6
// Improved checking and auto-creation of ResultImage. 
// Improved error handling/reporting.

//
//   Rev 1.5    29-09-2003 18:44:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.4    25-07-2003 00:15:30  mcm
// Code clean-up.

//
//   Rev 1.3    06-07-2003 10:48:54  mcm    Version: IMG 1.3.4
// Modified to work in BCB.
// All image operators returns a TmcmImage.

//
//   Rev 1.2    15-06-2003 13:07:02  mcm    Version: IMG 1.3.4

//
//   Rev 1.1    05-06-2003 21:30:12  mcm    Version: IMG 1.3.4
// Optimised DoBoundary/Outline for MMX. 

//
//   Rev 1.0    12-05-2003 15:36:56  mcm    Version: IMG 1.3.4
// Initial revision.

unit mcmImageMorph;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmImageKernel;

type
  TmcmPipeline = (MPL_SHRINK, MPL_THIN, MPL_SKELET);

  TmcmImageMorph = class(TmcmImageKernel)
  private
    // Private declarations
    FPixON          : byte;
    FPixOFF         : byte;
    FCoef           : array[0..1] of byte;
    FKeepFeatures   : boolean;
    FKernelHeight   : word;
    FKernelWidth    : word;
    FNeighbourWidth : word;
    FEdgeOutline    : boolean;
  protected
    // Protected declarations
    procedure   DoBoundary(index : longint; AImage : TmcmImage; pBuf : PRMatrixB; var More : boolean);
    procedure   DoSkeleton(index : longint; AImage : TmcmImage; pBuf : PRMatrixB; var More : boolean);
    function    GetCoef(Index : word) : word;
    function    GetFeaturesAreWhite : boolean;
    procedure   SetCoef(Index : word; Value : word);
    procedure   SetFeaturesAreWhite(Value : boolean);
    procedure   LabelFeature(AImage : TmcmImage; LPixON, LPixOFF : byte);
    procedure   LabelRemove(AImage : TmcmImage; LPixON, LPixOFF : byte);

    procedure   CopyLineToBinary(ResLine, SrcLine : PVectorB; Count : integer);
    procedure   ErasePixels(AImage : TmcmImage; LoopAgain : boolean);
    procedure   FlagClear(AImage : TmcmImage);
    procedure   FlagSet(AImage : TmcmImage);
    procedure   Pipeline(Method : TmcmPipeline; Iterations : word);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Close(Iterations : word) : TmcmImage;
    function    Dilate(Iterations : word) : TmcmImage;
    function    EuclideanDistanceMap : TmcmImage;
    function    Erode(Iterations : word) : TmcmImage;
    function    Open(Iterations : word) : TmcmImage;
    function    Outline : TmcmImage;
    function    Shrink(MaxIterations : word) : TmcmImage;
    function    Skeleton : TmcmImage;
    function    Skeleton2(MaxIterations : word) : TmcmImage;
    function    Thin(MaxIterations : word) : TmcmImage;
    property    Coefficient[Index : word] : word
      read      GetCoef
      write     SetCoef;
    property    EdgeOutline : boolean
      read      FEdgeOutline
      write     FEdgeOutline default False;
    property    FeaturesAreWhite : boolean
      read      GetFeaturesAreWhite
      write     SetFeaturesAreWhite default True;
    property    KeepSeparate : boolean
      read      FKeepFeatures
      write     FKeepFeatures default False;
  published
    // Published declarations
  end;

implementation

{ $DEFINE MCMASM}

constructor TmcmImageMorph.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FKernelHeight := 3;
  FKernelWidth  := 3;
  FKeepFeatures := False;
  FEdgeOutline  := True;
  FPixON   := 255;
  FPixOFF  := 0;
  FCoef[0] := 0;
  FCoef[1] := 1;
end; // TmcmImageMorph.Create.


destructor TmcmImageMorph.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageMorph.Destroy.


function TmcmImageMorph.GetCoef(Index : word) : word;
begin
  if (Index < 2)
  then Result := FCoef[Index]
  else Result := 0;
end; // TmcmImageMorph.GetCoef.


procedure TmcmImageMorph.SetCoef(Index : word; Value : word);
begin
  if (Index < 2)
  then FCoef[Index] := Value;
end; // TmcmImageMorph.SetCoef.


function TmcmImageMorph.GetFeaturesAreWhite : boolean;
begin
  Result := (FPixON = 255);
end; // TmcmImageMorph.GetFeaturesAreWhite.


procedure TmcmImageMorph.SetFeaturesAreWhite(Value : boolean);
begin
  if Value
  then begin
       FPixON   := 255;
       FPixOFF  := 0;
  end
  else begin
       FPixON   := 0;
       FPixOFF  := 255;
  end;
end; // TmcmImageMorph.SetFeaturesAreWhite.


procedure TmcmImageMorph.LabelFeature(AImage : TmcmImage; LPixON, LPixOFF : byte);

     function GetPrevious(sx, sy : integer; Up : boolean) : byte;
     var a          : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Get current line source address.
       Apt := AImage.ScanLine[sy];

       // Search for features above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       a := sx;
       while (a < AImage.Width) and
             (Apt^[a] = LPixON) and
             ((Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF))
       do inc(a);

       if (Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF)
       then Result := LPixOFF
       else Result := Bpt^[a];
     end; // GetPrevious.


     function HasNeighbour(sx, sy, ex, ey : integer; Up : boolean; Value : byte) : byte;
     var a, am, ap  : integer;
         maxx       : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Calculate Current Line source address.
       Apt := AImage.ScanLine[sy];

       // Search for neighbours above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       // Start search one pixel "early".
       a := sx - 1;
       if (a < 0)
       then a := 0;

       am := a - 1;
       if (am < 0)
       then am := 0;

       ap := a + 1;
       if (ap >= AImage.Width)
       then ap := AImage.Width - 1;

       maxx := ex + 1;
       if (maxx > AImage.Width)
       then maxx := AImage.Width;

       // Search for neighbour pixel being "ON" next to a pixel having feature
       // "Value".
       while (a < maxx {AImage.Width}) and
             Not(((Apt^[a] = Value) or (Apt^[am] = Value) or (Apt^[ap] = Value)) and
                 (Bpt^[a] = LPixON))
       do begin
          inc(a);
          am := a - 1;
          inc(ap);
          if (ap >= AImage.Width)
          then ap := AImage.Width - 1;
       end;

       // Return pixel value.
       if (a < AImage.Width)
       then Result := Bpt^[a]
       else Result := LPixOFF;
     end; // HasNeighbour.


     function FirstLine(var sx, sy, ex, ey : integer; Value : byte) : longint;
     var a          : integer;
         Count      : longint;
         ALine      : PvectorB;
     begin
       Count := 0;

       // Calculate Current Line source address.
       ALine := AImage.ScanLine[sy];

       // Fill coherent "ON"-pixels with feature "Value".
       a := sx;
       while (0 <= a) and (a < AImage.Width) and (ALine^[a] = LPixON)
       do begin
          ALine^[a] := Value;
          inc(a);
          inc(Count);
       end;
       dec(a);
       if (a < 0)
       then a := 0;
       ex := a;
       Result := Count;
     end; // FirstLine.


     function FillLine(var sx, sy, ex, ey : integer; Up : boolean; Value : byte) : longint;
     var a, b, c, d : integer;
         bm, bp     : integer;
         Count      : longint;
         ALine      : PvectorB;
         BLine      : PvectorB;
     begin
       Count := 0;

       dec(sx);
       if (sx < 0)
       then sx := 0;
       inc(ex);
       if (ex > (AImage.Width - 1))
       then ex := AImage.Width - 1;

       // Get current line source address.
       ALine := AImage.ScanLine[sy];

       if Up
       then BLine := AImage.ScanLine[sy-1]
       else BLine := AImage.ScanLine[sy+1];

       a := sx;
       d := sx;
       while (a <= ex) and (ALine^[a] = LPixOFF)
       do inc(a);

       if (a <= ex)
       then begin
            b  := a;
            bm := b - 1;
            if (bm < 0)
            then bm := 0;

            bp := b + 1;
            if (bp >= AImage.Width)
            then bp := AImage.Width - 1;

            while (b <= ex)
            do begin
               if (ALine^[b] = LPixON)
               then begin
                    if (BLine^[b]  = Value) or
                       (BLine^[bm] = Value) or
                       (BLine^[bp] = Value)
                    then begin
                         c := b;
                         while (c >= 0) and (ALine^[c] = LPixON)
                         do begin
                            ALine^[c] := Value;
                            dec(c);
                            inc(Count);
                         end;
                         if (a > c)
                         then a := c + 1;
                         if (d <= c)
                         then d := c + 1;

                         inc(b);
                         while (b < AImage.Width) and (ALine^[b] = LPixON)
                         do begin
                            ALine^[b] := Value;
                            d := b;
                            inc(b);
                            inc(Count);
                         end;
                    end
                    else inc(b);
               end
               else inc(b);
               bm := b - 1;
               bp := b + 1;
               if (bp >= AImage.Width)
               then bp := AImage.Width - 1;
            end;
       end;

       if (Count = 0)
       then begin
            a := ex;
            d := 0;
       end;

       if (a < 0)
       then a := 0;
       sx := a;
       ex := d;
       ey := sy;
       Result := Count;
     end; // FillLine.


var x, y       : longint;
    Value      : word;
    pNeighbour : PVectorNeighbour;
    NIndex     : longint;
    NCount     : longint;
    ix, iy     : integer;
    ox, oy     : integer;
    Dir        : boolean;
    ThisLine   : PVectorB;
begin
  // Make sure we have a valid Result Image.
  if Assigned(AImage)
  then begin
       if AImage.Empty
       then Exit;

       NCount := 4 * FSrcHeight;
       GetMem(pNeighbour, NCount * SizeOf(TNeighbour));
       try
         NIndex := 0;
         Value  := 1;
         y      := 0;
         while (y < (FSrcHeight - 1))
         do begin
            // Calculate Current Line source address.
            ThisLine := AImage.ScanLine[y];

            x := 0;
            while (x < FSrcWidth)
            do begin
               // Search for "ON" pixels in current line.
               if (ThisLine^[x] = LPixON)
               then begin
                    ix := x;
                    iy := y;
                    ox := FSrcWidth;

                    // Feature fill current line segment.
                    FirstLine(ix, iy, ox, oy, Value);
                    inc(iy);

                    Dir := True;
                    repeat
                      // If at end of feature (top or bottom) check for saved
                      // lines to search for in opersite direction.
                      if Not(ix <= ox) or (0 > iy) or (iy >= FSrcHeight)
                      then begin
                           inc(iy);
                           if (NIndex > 0)
                           then begin
                                dec(NIndex);
                                ix := pNeighbour[NIndex].x1;
                                iy := pNeighbour[NIndex].y1;
                                ox := pNeighbour[NIndex].x2;
                                oy := pNeighbour[NIndex].y2;
                                Dir := pNeighbour[NIndex].Up;
                           end;
                      end;

                      while (ix <= ox) and (0 <= iy) and (iy < FSrcHeight)
                      do begin
                         // Fill line segments belonging to current feature.
                         if (FillLine(ix, iy, ox, oy, Dir, Value) = 0)
                         then begin
                              // If at end of feature (top or bottom) check for saved
                              // lines to search for in opersite direction.
                              inc(iy);
                              if (NIndex > 0)
                              then begin
                                   dec(NIndex);
                                   ix := pNeighbour[NIndex].x1;
                                   iy := pNeighbour[NIndex].y1;
                                   ox := pNeighbour[NIndex].x2;
                                   oy := pNeighbour[NIndex].y2;
                                   Dir := pNeighbour[NIndex].Up;
                              end;
                         end
                         else begin
                              // Check if line-segments in the line in opersite
                              // direction (the line above or below) belongs to
                              // the current feature (pixels = PixON).
                              if (HasNeighbour(ix, iy, ox, oy, Not(Dir), Value) = LPixON)
                              then begin
                                   // Increase allocated memory if required.
                                   if (NIndex >= NCount)
                                   then begin
                                        ReallocMem(pNeighbour, (NCount + FSrcHeight) * SizeOf(TNeighbour));
                                        if (pNeighbour <> Nil)
                                        then NCount := NCount + FSrcHeight
                                        else NCount := 0;
                                   end;

                                   // Save left- and rightmost x coordinate for
                                   // "later" filling.
                                   if (NIndex < NCount)
                                   then begin
                                        pNeighbour[NIndex].x1 := ix;
                                        pNeighbour[NIndex].x2 := ox;
                                        if Dir
                                        then begin
                                             pNeighbour[NIndex].y1 := iy - 1;
                                             pNeighbour[NIndex].y2 := iy - 1;
                                             inc(iy);
                                        end
                                        else begin
                                             pNeighbour[NIndex].y1 := iy + 1;
                                             pNeighbour[NIndex].y2 := iy + 1;
                                             dec(iy);
                                        end;
                                        pNeighbour[NIndex].Up := Not(Dir);

                                        if (0 <= pNeighbour[NIndex].y1) and (pNeighbour[NIndex].y1 < FSrcHeight)
                                        then Inc(NIndex);
                                   end;
                              end
                              else begin
                                   if Dir
                                   then inc(iy)
                                   else dec(iy);
                              end;
                         end;
                      end;
                    until (NIndex <= 0);

                    // Increment to next feature value.
                    Value := (Value + 1) mod 255;
                    if (Value = 0)
                    then Value := 1;
               end;
               inc(x);
            end;

            // Increment current Line Scource address with LongWidth.
            inc(y);
         end;
       finally
         if (pNeighbour <> Nil)
         then FreeMem(pNeighbour);
       end;
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageMorph.LabelFeature.


procedure TmcmImageMorph.LabelRemove(AImage : TmcmImage; LPixON, LPixOFF : byte);
var x, y   : longint;
    pImage : PVectorB;
begin
  // Make sure we have a valid Result Image.
  if Assigned(AImage)
  then begin
       if AImage.Empty
       then Exit;

       for y := 0 to (FSrcHeight - 1)
       do begin
          // Calculate new source address.
          pImage := AImage.ScanLine[y];
          for x := 0 to (FSrcWidth - 1)
          do begin
             if (pImage^[x] <> LPixOFF)
             then pImage^[x] := LPixON;
          end;
       end;
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageMorph.LabelRemove.


function TmcmImageMorph.Close(Iterations : word) : TmcmImage;
var AImage : TKernelImage;
begin
  AImage := FSrcImage[0];
  Dilate(Iterations);
  FSrcImage[0] := FResImage;
  Erode(Iterations);
  FSrcImage[0] := AImage;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.Close.


function TmcmImageMorph.Dilate(Iterations : word) : TmcmImage;
Label EndKernel;
var x, y, yi : longint;
    i, j     : longint;
    r        : integer;
    m, n, l  : integer;
    c        : integer;
    pCRes    : PVectorB;
    pPRes    : PVectorB;
    Fiw      : longint; // Image height - 1
    Fih      : longint; // Image width - 1
    Fkh      : longint; // Filter kernel height - 1
    Fkw      : longint;
    LPixON   : byte;
    LPixOFF  : byte;
    NoON     : byte;  // Kernel, number of ON pixels.
    Coef     : integer;
    Value    : byte;
    AImage   : TKernelImage;
begin
  FError := EC_OK;
  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_GREY8])
  then begin
       AImage := FSrcImage[0];
       CheckResult(IF_GREY8, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then begin
                 FError := EC_MISRESULTIMAGE;
                 Result := Nil;
                 Exit;
            end;

            // If grey scale palette is inverted, swap FPixON and FPixOFF values.
            if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
            then begin
                 LPixON  := FPixOFF;
                 LPixOFF := FPixON;
            end
            else begin
                 LPixON  := FPixON;
                 LPixOFF := FPixOFF;
            end;

            if FKeepFeatures
            then LabelFeature(FSrcImage[0], LPixON, LPixOFF);

            FNeighbourWidth  := FKernelWidth shr 1;
            Fih := FSrcHeight - 1;
            Fiw := FSrcWidth - 1;
            Fkh := FKernelHeight - 1;
            Fkw := FKernelWidth - 1;

            try
              if AllocLines((FSrcWidth + FKernelWidth), FKernelHeight)
              then begin
                   for r := 1 to Iterations
                   do begin
                      if (r = 2)
                      then AImage := FResImage;
                      if Odd(r)
                      then Coef := FCoef[0]
                      else Coef := FCoef[1];

                      // Fill buffer with a blank line and the two first image lines.
                      yi := -FNeighbourWidth; // FNeighbourWidth and Height are equal  
                      for i := 0 to (Fkh - 1)
                      do begin
                         if (yi < 0)
                         then FillChar(FLines[i]^[FNeighbourWidth], FSrcWidth, LPixOFF)
                         else CopyMemory(@FLines[i]^[FNeighbourWidth], AImage.ScanLine[yi], FSrcWidth);
                         for j := 0 to (FNeighbourWidth - 1)
                         do begin
                            FLines[i]^[j] := LPixOFF;
                            FLines[i]^[FNeighbourWidth+Fiw+1+j] := LPixOFF;
                         end;
                         inc(yi);
                      end;

                      m := 0;   // m = index to buffer for next line.
                      n := Fkh; // n = index to last line in buffer.
                      c := Fkh shr 1; // c = Y centre index.

                      pPRes := FResImage.ScanLine[0];
                      for y := 0 to Fih // All lines in image
                      do begin
                         if (yi >= FSrcHeight)
                         then FillChar(FLines[n]^[FNeighbourWidth], FSrcWidth, LPixOFF)
                         else CopyMemory(@FLines[n]^[FNeighbourWidth], AImage.ScanLine[yi], FSrcWidth);

                         for j := 0 to (FNeighbourWidth - 1)
                         do begin
                            FLines[n]^[j] := LPixOFF;
                            FLines[n]^[FNeighbourWidth+Fiw+1+j] := LPixOFF;
                         end;
                         pCRes := FResImage.ScanLine[y];
                         FillChar(pCRes^[0], FSrcWidth, LPixOFF);

                         for x := 0 to Fiw  // All pixels in line
                         do begin
                            // Filter kernel.
                            if (FLines[c]^[x+1] = LPixOFF)
                            then begin
                                 NoON := 0;
                                 Value := LPixOFF;
                                 l := m;
                                 for j := 0 to Fkh
                                 do begin
                                    for i := x to x + Fkw
                                    do begin
                                       // Get neighbour pixel.
                                       if (FLines[l]^[i] <> LPixOFF)
                                       then begin
                                            // Neighbour pixel is background.
                                            inc(NoON);
                                            if (Value = LPixOFF)
                                            then Value := FLines[l]^[i]
                                            else begin
                                                 if (Value <> FLines[l]^[i])
                                                 then GOTO EndKernel;
                                            end;
                                       end;
                                    end;
                                    l := (l + 1) mod FKernelHeight;
                                 end;

                                 if (NoON > Coef)
                                 then begin
                                      if FKeepFeatures
                                      then begin
                                           // Check for already dilated (Result
                                           // image) differently labeled features.
                                           l := x - 1;
                                           if (l < 0)
                                           then l := 0;
                                           for i := l to x + 1
                                           do begin
                                              if ((pCRes^[i] <> LPixOFF) and (pCRes^[i] <> Value)) or
                                                 ((pPRes^[i] <> LPixOFF) and (pPRes^[i] <> Value))
                                              then GOTO EndKernel;
                                           end;
                                      end;
                                      pCRes[x] := Value;
                                 end;

                                 // LABEL, Dirty jump to end of kernel.
                                 EndKernel:
                            end
                            else pCRes[x] := FLines[c]^[x+1];
                         end; // x loop

                         c := RotateIndex(c, FKernelHeight);
                         m := RotateIndex(m, FKernelHeight);
                         n := RotateIndex(n, FKernelHeight);
                         pPRes := pCRes;
                         inc(yi);
                      end; // y loop
                   end; // Iterations loop
              end;
            finally
              FreeLines((FSrcWidth + FKernelWidth), FKernelHeight);
            end;

            if FKeepFeatures
            then begin
                 LabelRemove(FSrcImage[0], LPixON, LPixOFF);
                 if (FSrcImage[0] <> ResultImage)
                 then LabelRemove(ResultImage, LPixON, LPixOFF);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.Dilate.


function TmcmImageMorph.Erode(Iterations : word) : TmcmImage;
var x, y     : longint;
    yi       : longint;
    i, j     : longint;
    r        : integer;
    m, n, l  : integer;
    c        : integer;
    pRes     : PVectorB;
    Fiw      : longint; // Image height - 1
    Fih      : longint; // Image width - 1
    Fkh      : longint; // Filter kernel height - 1
    Fkw      : longint;
    LPixON   : byte;
    LPixOFF  : byte;
    NoOFF    : byte;  // Kernel, number of OFF pixels.
    Coef     : integer;
    AImage   : TKernelImage;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8])
  then begin
       AImage := FSrcImage[0];
       CheckResult(IF_GREY8, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then Exit;

            // If grey scale palette is inverted, swap FPixON and FPixOFF values.
            if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
            then begin
                 LPixON  := FPixOFF;
                 LPixOFF := FPixON;
            end
            else begin
                 LPixON  := FPixON;
                 LPixOFF := FPixOFF;
            end;

            FNeighbourWidth  := FKernelWidth shr 1;
            Fih := FSrcHeight - 1;
            Fiw := FSrcWidth - 1;
            Fkh := FKernelHeight - 1;
            Fkw := FKernelWidth - 1;

            try
              if AllocLines((FSrcWidth + FKernelWidth), FKernelHeight)
              then begin
                   for r := 1 to Iterations
                   do begin
                      if (r = 2)
                      then AImage := FResImage;
                      if Odd(r)
                      then Coef := FCoef[0]
                      else Coef := FCoef[1];

                      // Fill buffer with a blank line and the two first image lines.
                      yi := -FNeighbourWidth; // FNeighbourWidth and Height are equal
                      for i := 0 to (Fkh - 1)
                      do begin
                         if (yi < 0)
                         then FillChar(FLines[i]^[FNeighbourWidth], FSrcWidth, LPixON)
                         else CopyMemory(@FLines[i]^[FNeighbourWidth], AImage.ScanLine[yi], FSrcWidth);
                         for j := 0 to (FNeighbourWidth - 1)
                         do begin
                            FLines[i]^[j] := LPixON;
                            FLines[i]^[FNeighbourWidth+Fiw+1+j] := LPixON;
                         end;
                         inc(yi);
                      end;

                      m := 0;   // m = index to buffer for next line.
                      n := Fkh; // n = index to last line in buffer.
                      c := Fkh shr 1;

                      for y := 0 to Fih // All lines in image
                      do begin
                         CopyMemory(@FLines[n]^[FNeighbourWidth], AImage.ScanLine[yi], FSrcWidth);
                         for j := 0 to (FNeighbourWidth - 1)
                         do begin
                            FLines[n]^[j] := LPixON;
                            FLines[n]^[FNeighbourWidth+Fiw+1+j] := LPixON;
                         end;
                         pRes := FResImage.ScanLine[y];

                         for x := 0 to Fiw  // All pixels in line
                         do begin
                            // Filter kernel.
                            if (FLines[c]^[x+1] <> LPixOFF)
                            then begin
                                 NoOFF := 0;
                                 l := m;
                                 for j := 0 to Fkh
                                 do begin
                                    for i := x to x + Fkw
                                    do begin
                                       // Get neighbour pixel.
                                       if (FLines[l]^[i] = LPixOFF)
                                       then inc(NoOFF); // Neighbour pixel is background.
                                    end;
                                    l := (l + 1) mod FKernelHeight;
                                 end;
                                 if (NoOFF > Coef)
                                 then pRes[x] := LPixOFF
                                 else pRes[x] := LPixON;
                            end
                            else pRes[x] := LPixOFF;
                         end; // x loop

                         c := RotateIndex(c, FKernelHeight);
                         m := RotateIndex(m, FKernelHeight);
                         n := RotateIndex(n, FKernelHeight);
                         inc(yi);
                      end; // y loop
                   end; // Iterations loop
              end;
            finally
              FreeLines((FSrcWidth + FKernelWidth), FKernelHeight);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.Erode.


function TmcmImageMorph.Open(Iterations : word) : TmcmImage;
var AImage : TKernelImage;
    AKeep  : boolean;
begin
  AKeep := FKeepFeatures;
  FKeepFeatures := True;
  AImage := FSrcImage[0];
  Erode(Iterations);
  FSrcImage[0] := FResImage;
  Dilate(Iterations);
  FSrcImage[0] := AImage;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
  FKeepFeatures := AKeep;
end; // TmcmImageMorph.Open.


procedure TmcmImageMorph.FlagClear(AImage : TmcmImage);
var x, y    : longint;
    ALine   : PVectorB;
    LPixON  : byte;
    LPixOFF : byte;
begin
  // If grey scale palette is inverted, swap FPixON and FPixOFF values.
  if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
  then begin
       LPixON  := FPixOFF;
       LPixOFF := FPixON;
  end
  else begin
       LPixON  := FPixON;
       LPixOFF := FPixOFF;
  end;

  if (LPixON > LPixOFF)
  then begin
       for y := 0 to (AImage.Height - 1)
       do begin
          ALine := AImage.ScanLine[y];
          for x := 0 to (AImage.Width - 1)
          do if (ALine^[x] <> 0)
             then ALine^[x] := $7F;
       end;
  end
  else begin
       for y := 0 to (AImage.Height - 1)
       do begin
          ALine := AImage.ScanLine[y];
          for x := 0 to (AImage.Width - 1)
          do if (ALine^[x] = 0)
             then ALine^[x] := $7F
             else ALine^[x] := $00;
       end;
  end;
end; // TmcmImageMorph.FlagClear.


procedure TmcmImageMorph.CopyLineToBinary(ResLine, SrcLine : PVectorB; Count : integer);
var i : integer;
begin
  if FMMX
  then begin
       asm
         // Save registers to stack
         push  ebx
         push  edi
         push  esi

         // Calculate Qwords per line.
         mov   eax,Count
         test  eax,eax
         jz    @EndOfImage     // Check that image > zero bytes.
         push  eax             // Save image byte count
         and   eax,$FFFFFFF8   // ecx := Count - (Count mod 8)
         sub   eax,8
         shr   eax,3
         mov   ecx,eax

         // Set-up initial pointers to source and result images.
         mov   edi,ResLine
         mov   esi,SrcLine


         mov   eax,$01010101
         {$IFNDEF DCB3_5}
         pxor     mm1,mm1
         movd     mm1,eax
         movq     mm2,mm1
         psllq    mm2,32
         por      mm1,mm2      // mm2 = Flag-bit = $80 (at 4 positions)
         {$ELSE}
         db $0F,$EF,$C9
         db $0F,$6E,$C8
         db $0F,$6F,$D1
         db $0F,$73,$F2,$20
         db $0F,$EB,$CA
         {$ENDIF}

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         // If flag ($80) is set set pixel to zero.
         movq    mm0,qword ptr [esi+ecx*8]
         pand    mm0,mm1
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db $0F,$6F,$04,$CE
         db $0F,$DB,$C1
         db $0F,$7F,$04,$CF
         {$ENDIF}
         dec   ecx
         jns   @LoopQWORD

         // Check for last unprocessed bytes
         @NoQWORD:
         pop   ecx
         mov   ebx,ecx
         and   ebx,$07         // Offset address - ebx := Count mod 8.
         jz    @EndOfImage
         and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
         dec   ebx

         // Process remaining bytes in image.
         @LoopRemain:

         mov   al,[esi+ecx]
         and   al,$01
         mov   [edi+ecx],al    // Set negativ signed pixel to zero.
         inc   ecx
         dec   ebx
         jns   @LoopRemain

         @EndOfImage:

         // Restore stack
         pop   esi
         pop   edi
         pop   ebx

         // Empty EMMS registers.
         {$IFNDEF DCB3_5}
         emms
         {$ELSE}
         db    $0F,$77  // emms - clean-up que.
         {$ENDIF}
       end;
  end
  else begin
       for i := 0 to (Count - 1)
       do ResLine^[i] := SrcLine^[i] and $01;
  end;
end; // TmcmImageMorph.CopyLineToBinary.


procedure TmcmImageMorph.FlagSet(AImage : TmcmImage);
// A pixels flag is the pixels MSB.
var x, y    : longint;
    LPixON  : byte;
    LPixOFF : byte;
    Count   : longint;
    Height  : longword;
    pS, pR  : PVectorB;
begin
  // If grey scale palette is inverted, swap FPixON and FPixOFF values.
  if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
  then begin
       LPixON  := FPixOFF;
       LPixOFF := FPixON;
  end
  else begin
       LPixON  := FPixON;
       LPixOFF := FPixOFF;
  end;

  if FMMX
  then begin
       Count := AImage.LongLineWidth;
       Height := AImage.Height;
       pS := AImage.pDib;
       pR := AImage.pDib;
       asm
         // Save registers to stack
         push  ebx
         push  edi
         push  esi

         // Calculate Qwords per line.
         mov   eax,Count
         {$IFNDEF DCB3_5}
         mul   eax,Height
         {$ELSE}
         mov   ecx,Height
         db    $F7,$65,$F8
         {$ENDIF}
         test  eax,eax
         jz    @EndOfImage     // Check that image > zero bytes.
         push  eax             // Save image byte count
         and   eax,$FFFFFFF8   // ecx := Count - (Count mod 8)
         sub   eax,8
         shr   eax,3
         mov   ecx,eax

         // Set-up initial pointers to source and result images.
         mov   edi,pR
         mov   esi,pS

         mov   eax,$80808080
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         pxor     mm2,mm2
         movd     mm2,eax
         movq     mm3,mm2
         psllq    mm3,32
         por      mm2,mm3      // mm2 = Flag-bit = $80 (at 4 positions)
         {$ELSE}
         db $0F,$EF,$FF
         db $0F,$EF,$D2
         db $0F,$6E,$D0
         db $0F,$6F,$DA
         db $0F,$73,$F3,$20
         db $0F,$EB,$D3
         {$ENDIF}

         mov   bl,LPixOn
         test  bl,bl
         jz    @DoBlackWhite

         test  ecx,ecx
         jz    @NoQWORD1

         // process image
         @LoopQWORD1:
         {$IFNDEF DCB3_5}
         // If flag ($80) is set set pixel to zero.
         movq    mm0,qword ptr [esi+ecx*8]
         por     mm0,mm2
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db $0F,$6F,$04,$CE
         db $0F,$EB,$C2
         db $0F,$7F,$04,$CF
         {$ENDIF}
         dec   ecx
         jns   @LoopQWORD1

         // Check for last unprocessed bytes
         @NoQWORD1:
         pop   ecx
         mov   ebx,ecx
         and   ebx,$07         // Offset address - ebx := Count mod 8.
         jz    @EndOfImage
         and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
         dec   ebx

         // Process remaining bytes in image.
         mov   ah,$FF
         mov   dh,$80
         @LoopRemain1:

         mov   al,[esi+ecx]
         test  al,al
         jz   @NotEqual1       // Is zero.
         mov   [edi+ecx],ah
         jmp   @NextPix1
         @NotEqual1:
         mov   [edi+ecx],dh
         @NextPix1:
         inc   ecx
         dec   ebx
         jns   @LoopRemain1
         jmp   @EndOfImage     // we're done.

         @DoBlackWhite:        // LPixON < LPixOFF
         test  ecx,ecx
         jz    @NoQWORD2

         // process image
         @LoopQWORD2:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         pcmpeqb mm0,mm7
         por     mm0,mm2
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db $0F,$6F,$04,$CE
         db $0F,$74,$C7
         db $0F,$EB,$C2
         db $0F,$7F,$04,$CF
         {$ENDIF}
         dec   ecx
         jns   @LoopQWORD2

         // Check for last unprocessed bytes
         @NoQWORD2:
         pop   ecx
         mov   ebx,ecx
         and   ebx,$07         // Offset address - ebx := Count mod 8.
         jz    @EndOfImage
         and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
         dec   ebx

         // Process remaining bytes in image.
         mov   ah,$80
         mov   dh,$FF
         @LoopRemain2:

         mov   al,[esi+ecx]
         test  al,al
         jz   @NotEqual2       // Is zero.
         mov   [edi+ecx],ah
         jmp   @NextPix1
         @NotEqual2:
         mov   [edi+ecx],dh
         @NextPix2:
         inc   ecx
         dec   ebx
         jns   @LoopRemain2


         @EndOfImage:
         // Restore stack
         pop   esi
         pop   edi
         pop   ebx

         // Empty EMMS registers.
         {$IFNDEF DCB3_5}
         emms
         {$ELSE}
         db    $0F,$77  // emms - clean-up que.
         {$ENDIF}
       end;
  end
  else begin
       if (LPixON > LPixOFF)
       then begin
            for y := 0 to (AImage.Height - 1)
            do begin
               pS := AImage.ScanLine[y];
               for x := 0 to (AImage.Width - 1)
               do if (pS^[x] = 0)
                  then pS^[x] := $80
                  else pS^[x] := $FF;
            end;
       end
       else begin
            for y := 0 to (AImage.Height - 1)
            do begin
               pS := AImage.ScanLine[y];
               for x := 0 to (AImage.Width - 1)
               do if (pS^[x] = 0)
                  then pS^[x] := $FF
                  else pS^[x] := $80;
            end;
       end;
  end;
end; // TmcmImageMorph.FlagSet.


procedure TmcmImageMorph.ErasePixels(AImage : TmcmImage; LoopAgain : boolean);
// A pixels flag is the pixels MSB.
var x, y    : longint;
    ALine   : PVectorShI;
    LPixON  : byte;
    LPixOFF : byte;
    Count   : longint;
    Height  : longword;
    pS, pR  : PVectorB;
begin
  if FMMX
  then begin
       Count := AImage.LongLineWidth;
       Height := AImage.Height;
       pS := AImage.pDib;
       pR := AImage.pDib;
       asm
         // Save registers to stack
         push  ebx
         push  edi
         push  esi

         // Calculate Qwords per line.
         mov   eax,Count
         {$IFNDEF DCB3_5}
         mul   eax,Height
         {$ELSE}
         mov   ecx,Height
         db    $F7,$65,$F8
         {$ENDIF}
         test  eax,eax
         jz    @EndOfImage     // Check that image > zero bytes.
         push  eax             // Save image byte count
         and   eax,$FFFFFFF8   // ecx := Count - (Count mod 8)
         sub   eax,8
         shr   eax,3
         mov   ecx,eax

         // Set-up initial pointers to source and result images.
         mov   edi,pR
         mov   esi,pS

         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         {$ELSE}
         db $0F,$EF,$FF
         {$ENDIF}

         mov   bl,LoopAgain
         test  bl,bl
         jz    @DoBlackWhite

         test  ecx,ecx
         jz    @NoQWORD1

         // process image
         @LoopQWORD1:
         {$IFNDEF DCB3_5}
         // If flag ($80) is set set pixel to zero.
         movq    mm0,qword ptr [esi+ecx*8]
         movq    mm1,mm0
         pcmpgtb mm1,mm7
         pand    mm0,mm1
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db $0F,$6F,$04,$CE
         db $0F,$6F,$C8
         db $0F,$64,$CF
         db $0F,$DB,$C1
         db $0F,$7F,$04,$CF
         {$ENDIF}
         dec   ecx
         jns   @LoopQWORD1

         // Check for last unprocessed bytes
         @NoQWORD1:
         pop   ecx
         mov   ebx,ecx
         and   ebx,$07         // Offset address - ebx := Count mod 8.
         jz    @EndOfImage
         and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
         dec   ebx

         // Process remaining bytes in image.
         xor   ah,ah           // ah := zero.
         @LoopRemain1:

         mov   al,[esi+ecx]
         test  al,al
         jge   @NotEqual1      // Is flag set - Yes if negativ.
         mov   [edi+ecx],ah    // Set negativ signed pixel to zero.
         //jmp   @NextPix1
         @NotEqual1:
         //mov   [edi+ecx],al <- Not necessary as source and target is the same!
         //@NextPix1:
         inc   ecx
         dec   ebx
         jns   @LoopRemain1
         jmp   @EndOfImage    // we're done.

         @DoBlackWhite:
         test  ecx,ecx
         jz    @NoQWORD2

         // process image
         @LoopQWORD2:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         pcmpgtb mm0,mm7
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db $0F,$6F,$04,$CE
         db $0F,$64,$C7
         db $0F,$7F,$04,$CF
         {$ENDIF}
         dec   ecx
         jns   @LoopQWORD2

         // Check for last unprocessed bytes
         @NoQWORD2:
         pop   ecx
         mov   ebx,ecx
         and   ebx,$07         // Offset address - ebx := Count mod 8.
         jz    @EndOfImage
         and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
         dec   ebx

         // Process remaining bytes in image.
         xor   ah,ah           // ah := zero.
         @LoopRemain2:
         mov   al,[esi+ecx]
         test  al,al
         jns   @NotEqual2      // Is flag set
         mov   [edi+ecx],ah    // Set negativ signed pixel to zero.
         jmp   @NextPix2
         @NotEqual2:
         {$IFNDEF DCB3_5}
         mov   [edi+ecx],$0FF   // all other pixels are set to 255.
         {$ELSE}
         mov   al,$0FF
         mov   [edi+ecx],al
         {$ENDIF}
         @NextPix2:
         inc   ecx
         dec   ebx
         jns   @LoopRemain2

         @EndOfImage:

         // Restore stack
         pop   esi
         pop   edi
         pop   ebx

         // Empty EMMS registers.
         {$IFNDEF DCB3_5}
         emms
         {$ELSE}
         db    $0F,$77  // emms - clean-up que.
         {$ENDIF}
       end;
  end
  else begin
       if LoopAgain
       then begin
            for y := 0 to (AImage.Height - 1)
            do begin
               ALine := AImage.ScanLine[y];
               for x := 0 to (AImage.Width - 1)
               do if (ALine^[x] < 0)
                  then ALine^[x] := 0;
            end;
       end
       else begin
            // If grey scale palette is inverted, swap FPixON and FPixOFF values.
            // This always generate white features on a black background.

            if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
            then begin
                 LPixON  := 0;
                 LPixOFF := 255;
            end
            else begin
                 LPixON  := 255;
                 LPixOFF := 0;
            end;
            for y := 0 to (AImage.Height - 1)
            do begin
               ALine := AImage.ScanLine[y];
               for x := 0 to (AImage.Width - 1)
               do
               if (ALine^[x] > 0)
                  then ALine^[x] := LPixON
                  else ALine^[x] := LPixOFF;
            end;
       end;
  end;
end; // TmcmImageMorph.ErasePixels.


procedure TmcmImageMorph.DoBoundary(    index  : longint;
                                        AImage : TmcmImage;
                                        pBuf   : PRMatrixB;
                                    var More   : boolean);
// Kernel:
// ----------------
// |  |n5|n6|n7|  | <- Line(a)
// ----------------
// |  |n4|nc|n0|  | <- Line(b)
// ----------------
// |  |n3|n2|n1|  | <- Line(c)
// ----------------
{$DEFINE FASTOUTLINE}
var i, x, y   : longint;
    a, b, c   : integer;
    ALine     : PVectorB;
    Count     : longint;
    pa, pc    : PVectorB;
    {$IFNDEF FASTOUTLINE}
    n0, n1    : integer;
    n2, n3    : integer;
    n4, n5    : integer;
    n6, n7    : integer;
    EdgePoint : integer;
    SafePoint : integer;
    {$ENDIF}
begin
  FError := EC_OK;
  // Set-up initial line index's.
  a := 0;
  b := 1;
  c := 2;

{$IFDEF FASTOUTLINE}
  Count := (AImage.Width + 7) shr 3; // PROBLEM, Run one 8 byte too far on the result image!!!!!!!
  case Index of
  0 : begin // Horizontal
        // Copy two first lines: line(a) and line(b) into rotating buffer.
        for i := -1 to 1
        do begin
           ALine := AImage.ScanLine[i];
           if FEdgeOutline
           then // Features on the image edge are terminated with an edge
                // line.
                FillMemory(@pBuf[i+1]^[0], AImage.Width + 2, FPixOFF)
           else begin
                x := 1;
                CopyLineToBinary(@pBuf[i+1]^[x], ALine, AImage.Width);
                pBuf[i+1]^[0] := pBuf[i+1]^[x];
                x := AImage.Width;
                pBuf[i+1]^[x+1] := pBuf[i+1]^[x];
           end;
        end;

        if FMMX
        then begin
             asm
             mov   eax,$81818181   // Used to check for safe point
             {$IFNDEF DCB3_5}
             movd     mm6,eax
             psllq    mm6,32
             movd     mm5,eax
             por      mm6,mm5
             {$ELSE}
             db $0F,$6E,$F0
             db $0F,$73,$F6,$20
             db $0F,$6E,$E8
             db $0F,$EB,$F5
             {$ENDIF}

             mov   eax,$7F7F7F7F   // Used for marking point for keeping.
             {$IFNDEF DCB3_5}
             movd     mm5,eax
             psllq    mm5,32
             movd     mm4,eax
             por      mm5,mm4
             {$ELSE}
             db $0F,$6E,$E8
             db $0F,$73,$F5,$20
             db $0F,$6E,$E0
             db $0F,$EB,$EC
             {$ENDIF}
             end;
             for y := 0 to (AImage.Height - 1) // Horizontal
             do begin
                // Copy in last line(c) into rotating buffer.
                ALine := AImage.ScanLine[y+1];
                x := 1;
                CopyLineToBinary(@pBuf[c]^[x], ALine, AImage.Width);

                if FEdgeOutline
                then begin
                     // Features on the image edge are terminated with an edge
                     // line.
                     if (y >= AImage.Height - 2)
                     then FillMemory(@pBuf[c]^[0], AImage.Width + 2, FPixOFF);
                     x := AImage.Width - 1;
                     pBuf[c]^[x] := FPixOFF;
                end
                else begin
                     pBuf[c]^[0] := pBuf[c]^[x];
                     x := AImage.Width;
                     pBuf[c]^[x+1] := pBuf[c]^[x];
                end;

                ALine := AImage.ScanLine[y];
                pc := pBuf[b];

                asm
                // Save registers to stack
                push    ebx
                push    edi
                push    esi

                mov     ecx,Count  // ecx = (Pixels + 7) div 8
                dec     ecx

                // Set-up initial pointers to source and result images.
                mov     edi,ALine  // edi = ALine
                mov     esi,pc     // esi = pBuf[b]

                {$IFNDEF DCB3_5}
                movq    mm3,mm6
                {$ELSE}
                db $0F,$6F,$DE
                {$ENDIF}
                @LoopQWORD:
                {$IFNDEF DCB3_5}
                movq    mm1,qword ptr [edi+ecx*8] // ALine
                movq    mm2,qword ptr [esi+ecx*8] // pBuf[b]

                movq    mm0,mm1      // copy Aline
                movq    mm4,mm1      // copy Aline

                movq    mm7,mm2      //
                psrlq   mm7,16       // mm7 = mm2 shr (2 pixels)
                por     mm7,mm3      // mm7 = mm2 shr (2 pixels) or (2 previous LSB pixels)
                pxor    mm3,mm3      // mm3 = 0

                pand    mm1,mm6      // mm1 = safe point mask
                pcmpeqb mm1,mm6      // mm1 := ((mm1 and $81) = $81)

                pand    mm2,mm7      // pBuf[b+2] and pBuf[b]
                pand    mm4,mm5      // mm4 := mm4 and $7F
                pcmpeqb mm2,mm3      // compare mm2,00
                pand    mm1,mm2      // mm1 = safe point mask

                movq    mm3,qword ptr [esi+ecx*8] // pBuf[b]
                psllq   mm3,48       // Keep (2 LSB pixels) shifted to MSB

                pand    mm4,mm1      // Mask safe points
                pandn   mm1,mm0      // Mask points for erasure. Not(mm1) is mask of unsafe points.
                por     mm1,mm4      // Or Safe points and points for erasure.

                movq    qword ptr [edi+ecx*8],mm1

                {$ELSE}
                db $0F,$6F,$0C,$CF
                db $0F,$6F,$14,$CE

                db $0F,$6F,$C1
                db $0F,$6F,$E1

                db $0F,$6F,$FA
                db $0F,$73,$D7,$10
                db $0F,$EB,$FB
                db $0F,$EF,$DB

                db $0F,$DB,$CE
                db $0F,$74,$CE

                db $0F,$DB,$D7
                db $0F,$DB,$E5
                db $0F,$74,$D3
                db $0F,$DB,$CA

                db $0F,$6F,$1C,$CE
                db $0F,$73,$F3,$30

                db $0F,$DB,$E1
                db $0F,$DF,$C8
                db $0F,$EB,$CC

                db $0F,$7F,$0C,$CF
                {$ENDIF}
                dec     ecx
                jns     @LoopQWORD

                // Restore stack
                pop   esi
                pop   edi
                pop   ebx
                end;

                // Rotate index's
                a := RotateIndex(a, 3);
                b := RotateIndex(b, 3);
                c := RotateIndex(c, 3);
             end;
             asm
             // Empty EMMS registers.
             {$IFNDEF DCB3_5}
             emms
             {$ELSE}
             db    $0F,$77  // emms - clean-up que.
             {$ENDIF}
             end;
        end
        else begin
             for y := 0 to (AImage.Height - 1) // Horizontal
             do begin
                // Copy in last line(c) into rotating buffer.
                ALine := AImage.ScanLine[y+1];
                x := 1;
                CopyLineToBinary(@pBuf[c]^[x], ALine, AImage.Width);
                pBuf[c]^[0] := pBuf[c]^[x];
                x := AImage.Width;
                pBuf[c]^[x+1] := pBuf[c]^[x];

                ALine := AImage.ScanLine[y];
                for x := 0 to (AImage.Width - 1)
                do begin
                   // Is EdgePoint ?
                   if ((ALine^[x] and $81) = $81)  // Is Falg set and pixel = 1
                   // If not a safe point & point intensity > thresshold.
                   then begin
                        if ((pBuf[b]^[x+2] and pBuf[b]^[x]) = 0)
                        then ALine^[x] := ALine^[x] and $7F; // This point will not be erased.
                   end;
                end;

                // Rotate index's
                a := RotateIndex(a, 3);
                b := RotateIndex(b, 3);
                c := RotateIndex(c, 3);
             end;
        end;
      end;
  2 : begin // Vertical
        // Copy two first lines: line(a) and line(b) into rotating buffer.
        for i := -1 to 1
        do begin
           ALine := AImage.ScanLine[i];
           CopyLineToBinary(pBuf[i+1], ALine, AImage.Width);
        end;

        if FMMX
        then begin
             asm
             mov   eax,$81818181   // Used to check for safe point
             {$IFNDEF DCB3_5}
             movd     mm6,eax
             psllq    mm6,32
             movd     mm5,eax
             por      mm6,mm5
             {$ELSE}
             db $0F,$6E,$F0
             db $0F,$73,$F6,$20
             db $0F,$6E,$E8
             db $0F,$EB,$F5
             {$ENDIF}

             mov   eax,$7F7F7F7F   // Used for marking point for keeping.
             {$IFNDEF DCB3_5}
             movd     mm5,eax
             psllq    mm5,32
             movd     mm4,eax
             por      mm5,mm4
             pxor     mm3,mm3
             {$ELSE}
             db $0F,$6E,$E8
             db $0F,$73,$F5,$20
             db $0F,$6E,$E0
             db $0F,$EB,$EC
             db $0F,$EF,$DB
             {$ENDIF}
             end;
             for y := 0 to (AImage.Height - 1) // Vertical
             do begin
                // Copy in last line(c) into rotating buffer.
                ALine := AImage.ScanLine[y+1];
                CopyLineToBinary(pBuf[c], ALine, AImage.Width);

                ALine := AImage.ScanLine[y];
                pa := pBuf[a];
                pc := pBuf[c];

                asm
                // Save registers to stack
                push    ebx
                push    edi
                push    esi

                mov     ecx,Count  // ecx = (Pixels + 7) div 8
                dec     ecx

                // Set-up initial pointers to source and result images.
                mov     edi,ALine
                mov     esi,pa
                mov     ebx,pc

                @LoopQWORD:
                {$IFNDEF DCB3_5}
                movq    mm1,qword ptr [edi+ecx*8] // ALine
                movq    mm2,qword ptr [esi+ecx*8] // pBuf[a]

                movq    mm0,mm1      // copy Aline
                movq    mm4,mm1      // copy Aline

                pand    mm1,mm6      // mm1 = safe point mask
                pcmpeqb mm1,mm6      // mm1 := ((mm1 and $81) = $81)

                pand    mm2,qword ptr [ebx+ecx*8] // pBuf[a] and pBuf[c]
                pand    mm4,mm5      // mm4 := mm4 and $7F
                pcmpeqb mm2,mm3      // compare mm2,00
                pand    mm1,mm2      // mm1 = safe point mask

                pand    mm4,mm1      // Mask safe points
                pandn   mm1,mm0      // Mask points for erasure. Not(mm1) is mask of unsafe points.
                por     mm1,mm4      // Or Safe points and points for erasure.
                movq    qword ptr [edi+ecx*8],mm1
                {$ELSE}
                db $0F,$6F,$0C,$CF
                db $0F,$6F,$14,$CE

                db $0F,$6F,$C1
                db $0F,$6F,$E1

                db $0F,$DB,$CE
                db $0F,$74,$CE

                db $0F,$DB,$14,$CB
                db $0F,$DB,$E5
                db $0F,$74,$D3
                db $0F,$DB,$CA

                db $0F,$DB,$E1
                db $0F,$DF,$C8
                db $0F,$EB,$CC
                db $0F,$7F,$0C,$CF
                {$ENDIF}
                dec     ecx
                jns     @LoopQWORD

                // Restore stack
                pop   esi
                pop   edi
                pop   ebx
                end;

                // Rotate index's
                a := RotateIndex(a, 3);
                b := RotateIndex(b, 3);
                c := RotateIndex(c, 3);
             end;
             asm
             // Empty EMMS registers.
             {$IFNDEF DCB3_5}
             emms
             {$ELSE}
             db    $0F,$77  // emms - clean-up que.
             {$ENDIF}
             end;
        end
        else begin
             for y := 0 to (AImage.Height - 1) // Vertical
             do begin
                // Copy in last line(c) into rotating buffer.
                ALine := AImage.ScanLine[y+1];
                CopyLineToBinary(pBuf[c], ALine, AImage.Width);

                ALine := AImage.ScanLine[y];
                pa := pBuf[a];
                pc := pBuf[c];
                for x := 0 to (AImage.Width - 1)
                do begin
                   // Is EdgePoint ?
                   if ((ALine^[x] and $81) = $81) // Is Falg set and pixel = 1
                   // If not a safe point & point intensity > thresshold.
                   then begin
                        if ((pBuf[c]^[x] and pBuf[a]^[x]) = 0)
                        then ALine^[x] := ALine^[x] and $7F; // This point will not be erased.
                   end;
                end;
                // Rotate index's
                a := RotateIndex(a, 3);
                b := RotateIndex(b, 3);
                c := RotateIndex(c, 3);
             end;
        end;
      end;
  end;

{$ELSE}
  // old method -
  n0 := 0;
  n2 := 0;
  n4 := 0;
  n6 := 0;
  EdgePoint := 0;

  for y := 0 to (AImage.Height - 1)
  do begin
     // Copy in last line(c) into rotating buffer.
     ALine := AImage.ScanLine[y+1];
     x := 1;
     CopyMemory(@pBuf[c]^[x], ALine, AImage.Width);
     pBuf[c]^[0] := pBuf[c]^[x];
     x := AImage.Width;
     pBuf[c]^[x+1] := pBuf[c]^[x];

     for x := 0 to (AImage.Width + 1)
     do pBuf[c]^[x] := pBuf[c]^[x] and $01;

     ALine := AImage.ScanLine[y];
     x := 1;
     CopyMemory(@pBuf[3]^[x], ALine, AImage.Width);

     for x := 1 to (AImage.Width)
     do begin
        // Is EdgePoint ?
        if ((pBuf[3]^[x] and $80) = $80) and
            (pBuf[3]^[x] and $7F  > 0)
        // If not a safe point & point intensity > thresshold.
        then begin
             case Index of
             0 : begin // Horizontal
                   n0 := pBuf[b]^[x+1];
                   n4 := pBuf[b]^[x-1];
                   EdgePoint := n0 and n4;
                 end;
             2 : begin // Vertical
                   n2 := pBuf[c]^[x];
                   n6 := pBuf[a]^[x];
                   EdgePoint := n2 and n6;
                 end;
             end;

             if (EdgePoint = 0)
             then begin
                  // Is SafePoint ?
                  SafePoint := 1;
                  case Index of
                  0 : begin
                        n1 := pBuf[c]^[x+1];
                        n2 := pBuf[c]^[x];
                        n3 := pBuf[c]^[x-1];
                        n5 := pBuf[a]^[x-1];
                        n6 := pBuf[a]^[x];
                        n7 := pBuf[a]^[x+1];

                        // Binary Image (0, 255).
                        SafePoint := n4 *
                                     (n5 + n6 + n2 + n3) *
                                     (n6 + 1 - n7) *
                                     (n2 + 1 - n1) *
                                     n0 *
                                     (n1 + n2 + n6 + n7) *
                                     (n2 + 1 - n3) *
                                     (n6 + 1 - n5);
                      end;
                  2 : begin
                        n0 := pBuf[b]^[x+1];
                        n1 := pBuf[c]^[x+1];
                        n3 := pBuf[c]^[x-1];
                        n4 := pBuf[b]^[x-1];
                        n5 := pBuf[a]^[x-1];
                        n7 := pBuf[a]^[x+1];

                        // Binary Image (0, 255).
                        SafePoint := n6 *
                                     (n7 + n0 + n4 + n5) *
                                     (n0 + 1 - n1) *
                                     (n4 + 1 - n3) *
                                     n2 *
                                     (n3 + n4 + n0 + n1) *
                                     (n4 + 1 - n5) *
                                     (n0 + 1 - n7);
                      end;
                  end;
                  if (SafePoint = 0)
                  then begin // This point will not be erased.
                       pBuf[3]^[x] := pBuf[3]^[x] and $7F;
                       More := True;
                  end;
             end;
        end;
     end;
     // Rotate index's
     a := (a + 1) mod 3;
     b := (b + 1) mod 3;
     c := (c + 1) mod 3;

     // Copy result line to image.
     x := 1;
     CopyMemory(ALine, @pBuf[3]^[x], AImage.Width);
  end;
{$ENDIF}
end; // TmcmImageMorph.DoBoundary.


function TmcmImageMorph.Outline : TmcmImage;
var LoopAgain : boolean;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8,IF_PAL8])
  then begin
       CheckResult(IF_GREY8, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then Exit;

            if (FSrcImage[0] <> FResImage)
            then FSrcImage[0].CopyRegion(TmcmImage(FResImage), Rect(0, 0, FSrcWidth, FSrcHeight));
            try
              if AllocLines(FSrcWidth + 3 + 8, 4)
              then begin
                   FlagSet(FResImage);
                   DoBoundary(0, FResImage, FLines, LoopAgain);
                   DoBoundary(2, FResImage, FLines, LoopAgain);
                   ErasePixels(FResImage, False);
              end;
            finally
              FreeLines(FSrcWidth + 3 + 8, 4);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.Outline.


procedure TmcmImageMorph.DoSkeleton(    index  : longint;
                                        AImage : TmcmImage;
                                        pBuf   : PRMatrixB;
                                    var More   : boolean);
// Kernel:
// ----------------
// |  |n7|n0|n1|  |
// ----------------
// |  |n6|nc|n2|  |
// ----------------
// |  |n5|n4|n3|  |
// ----------------
var x, y      : longint;
    i, j      : longint;
    Np, Sp    : word;
    n         : array[0..7] of integer;
    EdgePoint : integer;
    ALine     : PVectorB;
begin
  for y := 0 to (AImage.Height - 1)
  do begin
     for i := -1 to 1
     do begin
        ALine := AImage.ScanLine[y+i];
        x := 1;
        CopyMemory(@pBuf[i+1]^[x], ALine, AImage.Width);
        // Copy first and last real pixel to pre- and post-pixel.
        pBuf[i+1]^[0] := pBuf[i+1]^[x];
        x := AImage.Width;
        pBuf[i+1]^[x+1] := pBuf[i+1]^[x];
     end;

     for x := 1 to (AImage.Width)
     do begin
        // Is EdgePoint ?
        if (pBuf[1]^[x] and $7F <> 0)
        // If not a safe point & point intensity > thresshold.
        then begin
             EdgePoint := 0;
             case Index of
             0 : begin
                   n[0] := pBuf[0]^[x]   and $01;
                   n[2] := pBuf[1]^[x+1] and $01;
                   n[4] := pBuf[2]^[x]   and $01;
                   n[6] := pBuf[1]^[x-1] and $01;
                   if ((n[2] = 0) or  (n[4] = 0)) or
                      ((n[0] = 0) and (n[6] = 0))
                   then begin
                        EdgePoint := 1;
                        n[1] := pBuf[0]^[x+1] and $01;
                        n[3] := pBuf[2]^[x+1] and $01;
                        n[5] := pBuf[2]^[x-1] and $01;
                        n[7] := pBuf[0]^[x-1] and $01;
                   end;
                 end;
             2 : begin
                   n[0] := pBuf[0]^[x]   and $01;
                   n[2] := pBuf[1]^[x+1] and $01;
                   n[4] := pBuf[2]^[x]   and $01;
                   n[6] := pBuf[1]^[x-1] and $01;
                   if ((n[0] = 0) or  (n[6] = 0)) or
                      ((n[2] = 0) and (n[4] = 0))
                   then begin
                        EdgePoint := 1;
                        n[1] := pBuf[0]^[x+1] and $01;
                        n[3] := pBuf[2]^[x+1] and $01;
                        n[5] := pBuf[2]^[x-1] and $01;
                        n[7] := pBuf[0]^[x-1] and $01;
                   end;
                 end;
             end;

             if (EdgePoint = 1)
             then begin
                  i  := 0;
                  j  := 1;
                  Sp := 0;
                  while (i < 8) and (Sp < 2)
                  do begin
                     if ((n[i] < n[j]))
                     then inc(Sp);
                     inc(j);
                     if (j > 7)
                     then j := 0;
                     inc(i);
                  end;

                  if (Sp = 1)
                  then begin
                       Np := 0;
                       for i := 0 to 7
                       do begin
                          if (n[i] <> 0)
                          then inc(Np);
                       end;

                       if (1 < Np) and (Np < 7)
                       then begin // This point will be erased.
                            pBuf[1]^[x] := pBuf[1]^[x] or $80;
                            More := True;
                       end;
                  end;
             end;
        end;
     end;
     ALine := AImage.ScanLine[y];
     x := 1;
     CopyMemory(ALine, @pBuf[1]^[x], AImage.Width);
  end;
end; // TmcmImageMorph.DoSkeleton.


function TmcmImageMorph.Skeleton : TmcmImage;
var i         : integer;
    LoopAgain : boolean;
    MaxLoop   : longint;
begin
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8,IF_PAL8])
  then begin
       CheckResult(IF_GREY8, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then begin
                 Result := Nil;
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            if (FSrcImage[0] <> FResImage)
            then FSrcImage[0].CopyRegion(TmcmImage(FResImage), Rect(0, 0, FSrcWidth, FSrcHeight));
            try
              if AllocLines(FSrcWidth + 3, 3)
              then begin
                   FlagClear(FResImage);
                   i := 0;
                   LoopAgain := True;
                   if (FSrcWidth > FSrcHeight)
                   then MaxLoop := FSrcWidth shr 1
                   else MaxLoop := FSrcHeight shr 1;

                   while LoopAgain and (i < MaxLoop)
                   do begin
                      LoopAgain := False;
                      inc(i);
                      DoSkeleton(0, FResImage, FLines, LoopAgain);
                      ErasePixels(FResImage, True);
                      DoSkeleton(2, FResImage, FLines, LoopAgain);
                      ErasePixels(FResImage, LoopAgain and (i < MaxLoop));
                   end;
              end;
            finally
              FreeLines(FSrcWidth + 3, 3);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.Skeletonize.


//------------------------------------------------------------------------------
// The pipeline process uses two look-up tables to determine whether or not to
// operate on pixels.
//
//    9 Binary Pixels                  9 bit Integer index to m/uLut
//   ----------------
//   | X3 | X2 | X1 |
//   ----------------         ---------------------------------------------
//   | X4 | X  | X0 |   <=>   | X3| X2 | X1 | X4 | X | X0 | X5 | X6 | X7 |
//   ----------------         ---------------------------------------------
//   | X5 | X6 | X7 |
//   ----------------
//
// Shrinking, Thinning and skeletonization is performed in two stages.
// The first stage uses the (mark - m) mLut table to create a new image which
// markes the likely candidates.
// The second stage, uses the original and the marked image to determine if a
// pixel is a hit or miss for the desired operation.
//------------------------------------------------------------------------------

const
  mShrink : array[0..255] of byte = (
  $00, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $FF, $FF, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $FF, $FF, $00, $00, $FF, $FF, $00, $00);

  uShrink : array[0..255] of byte = (
  $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $00, $FF, $00, $00, $FF, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $00, $FF, $FF, $FF, $FF, $00, $FF, $00,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $00,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00,
  $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $FF, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $FF, $FF, $00, $FF,
  $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $00, $00, $FF, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $00, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $00, $FF, $FF, $00, $FF, $00, $00, $00,
  $00, $00, $FF, $00, $FF, $FF, $00, $FF, $FF, $FF, $00, $FF, $00, $00, $00, $00,
  $00, $FF, $FF, $FF, $FF, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

  mThin : array[0..255] of byte = (
  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $FF, $00, $00, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $FF, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $FF, $FF, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $FF, $FF, $00, $00, $FF, $FF, $00, $00);

  uThin : array[0..255] of byte = (
  $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, 00{ $FF}, $00, $00, $00, $FF, $00,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $00, $FF, $00, $00, $FF, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $00, $FF, $FF, $FF, $FF, $00, $FF, $00,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $00,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00,
  $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $FF, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $FF, $FF, $00, $FF,
  $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $00, $00, $FF, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $00, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $00, $FF, $FF, $00, $FF, $00, $00, $00,
  $00, $00, $FF, $00, $FF, $FF, $00, $FF, $FF, $FF, $00, $FF, $00, $00, $00, $00,
  $00, $FF, $FF, $FF, $FF, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);


  mSkel : array[0..255] of byte = (
  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $FF, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $FF, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $FF, $00, $FF, $FF,
  $00, $00, $FF, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $FF, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF,
  $00, $FF, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $FF,
  $FF, $FF, $00, $FF, $00, $00, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $00);


  uSkel : array[0..255] of byte = (
  $00, $FF, $FF, $00, $FF, $00, $00, $00, $FF, $00, $FF, $00, $00, $00, $FF, $FF,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $00, $00, $00, $00, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $00, $FF, $FF, $FF, $FF, $00, $FF, $FF,
  $00, $00, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $FF, $FF, $00, $00, $00, $FF, $00, $00, $FF, $FF, $00, $00, $FF, $FF,
  $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF,
  $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $00, $00, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $FF, $FF, $00, $FF, $FF, $FF,
  $00, $FF, $FF, $FF, $FF, $00, $FF, $FF, $00, $FF, $FF, $FF, $FF, $00, $FF, $FF,
  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

{------------------------------------------------------------------------------}
{ Pipelined morphological operations                                           }
{ Process the image until final conclusions                                    }
{------------------------------------------------------------------------------}


procedure TmcmImageMorph.Pipeline(Method : TmcmPipeline; Iterations : word);

var LPixON    : byte;
    LPixOFF   : byte;
    mtLut     : PVectorB;
    utLut     : PVectorB;
    mLut      : array[0..255] of byte;
    uLut      : array[0..255] of byte;
    x, y      : integer;
    Changed   : boolean;
    SrcImage  : TKernelImage;
    SrcLine   : array[0..2] of PVectorB;
    MarkImage : TKernelImage;
    MarkLine  : array[0..2] of PVectorB;
    ResLine   : PVectorB;
    Index     : integer;
begin
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8,IF_PAL8])
  then begin
       // Determine which look-up tables to user.
       case Method of
       MPL_SHRINK : begin
                      mtLut := @mShrink;
                      utLut := @uShrink;
                    end;
       MPL_THIN   : begin
                      mtLut := @mThin;
                      utLut := @uThin;
                    end;
       MPL_SKELET : begin
                      mtLut := @mSkel;
                      utLut := @uSkel;
                    end;
       else begin
            FError := EC_UNKNOWNMETHOD;
            Exit;
       end;
       end;

       CheckResult(IF_GREY8, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then Exit;

            MarkImage := TKernelImage.Create;
            if Assigned(MarkImage)
            then begin
                 MarkImage.Width  := FSrcImage[0].Width;
                 MarkImage.Height := FSrcImage[0].Height;
                 MarkImage.ImageFormat := IF_GREY8;
                 if (MarkImage.Empty)
                 then begin
                      FError := EC_NOMEMORY;
                      Exit;
                 end;
            end
            else begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            SrcImage := FSrcImage[0];

            // If grey scale palette is inverted, swap FPixON and FPixOFF values.
            if (TKernelImage(FSrcImage[0]).DibInfo^.bmiColors[0].rgbRed <> 0)
            then begin
                 LPixON  := FPixOFF;
                 LPixOFF := FPixON;
            end
            else begin
                 LPixON  := FPixON;
                 LPixOFF := FPixOFF;
            end;

            for x := 0 to 255
            do begin
               if (mtLut^[x] = 0)
               then mLut[x] := LPixOFF
               else mLut[x] := LPixON;
               if (utLut^[x] = 0)
               then uLut[x] := LPixOFF
               else uLut[x] := LPixON;
            end;

            // repetitively process the image until not changes are made or
            // max. allowed iterations has been processed.
            Changed := True;
            try
              while Changed and (Iterations > 0)
              do begin
                 Changed := False;

                 // Pipeline stage 1:
                 // Update undisputed pixels and mark potential pixels
                 SrcLine[1] := SrcImage.ScanLine[0];
                 SrcLine[2] := SrcImage.ScanLine[1];

                 for y := 1 to (FSrcHeight - 2)
                 do begin
                    SrcLine[0] := SrcLine[1];
                    SrcLine[1] := SrcLine[2];
                    SrcLine[2] := SrcImage.ScanLine[y+1];
                    MarkLine[1] := MarkImage.ScanLine[y];

                    for x := 1 to (FSrcWidth - 2)
                    do begin
                       if (SrcLine[1]^[x] <> LPixOFF) // X
                       then begin
                            Index := 0;
                            if (SrcLine[1]^[x+1] <> LPixOFF) // X0
                            then inc(Index, $080);
                            if (SrcLine[0]^[x+1] <> LPixOFF) // X1
                            then inc(Index, $40);
                            if (SrcLine[0]^[x] <> LPixOFF)   // X2
                            then inc(Index, $20);
                            if (SrcLine[0]^[x-1] <> LPixOFF) // X3
                            then inc(Index, $10);
                            if (SrcLine[1]^[x-1] <> LPixOFF) // X4
                            then inc(Index, $08);
                            if (SrcLine[2]^[x-1] <> LPixOFF) // X5
                            then inc(Index, $04);
                            if (SrcLine[2]^[x] <> LPixOFF)   // X6
                            then inc(Index, $02);
                            if (SrcLine[2]^[x+1] <> LPixOFF) // X7
                            then inc(Index, $01);
                            MarkLine[1]^[x] := mLut[Index];
                       end
                       else MarkLine[1]^[x] := LPixOFF;
                    end;
                 end;

                 // Pipeline stage 2:
                 // Revisit marked pixels and update accordingly re-initialize
                 // pointers into buffers
                 MarkLine[1] := MarkImage.ScanLine[0];
                 MarkLine[2] := MarkImage.ScanLine[1];

                 for y := 1 to (FSrcHeight - 2)
                 do begin
                    MarkLine[0] := MarkLine[1];
                    MarkLine[1] := MarkLine[2];
                    MarkLine[2] := MarkImage.ScanLine[y+1];
                    SrcLine[1] := SrcImage.ScanLine[y];
                    ResLine := FResImage.ScanLine[y];

                    for x := 1 to (FSrcWidth - 2)
                    do begin
                       if (MarkLine[1]^[x] <> LPixOFF)
                       then begin
                            Index := 0;
                            if (MarkLine[1]^[x+1] <> LPixOFF)
                            then inc(Index, $080);
                            if (MarkLine[0]^[x+1] <> LPixOFF)
                            then inc(Index, $40);
                            if (MarkLine[0]^[x] <> LPixOFF)
                            then inc(Index, $20);
                            if (MarkLine[0]^[x-1] <> LPixOFF)
                            then inc(Index, $10);
                            if (MarkLine[1]^[x-1] <> LPixOFF)
                            then inc(Index, $08);
                            if (MarkLine[2]^[x-1] <> LPixOFF)
                            then inc(Index, $04);
                            if (MarkLine[2]^[x] <> LPixOFF)
                            then inc(Index, $02);
                            if (MarkLine[2]^[x+1] <> LPixOFF)
                            then inc(Index, $01);

                            if (SrcLine[1]^[x] <> uLut[Index])
                            then Changed := True;
                            ResLine^[x] := uLut[Index];
                       end
                       else ResLine^[x] := SrcLine[1]^[x];
                    end;
                 end;

                 SrcImage := FResImage;
                 dec(Iterations);
              end;
            finally
              MarkImage.Free;
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
end; // TmcmImageMorph.Pipeline.


function TmcmImageMorph.Shrink(MaxIterations : word) : TmcmImage;
begin
  Pipeline(MPL_SHRINK, MaxIterations);
  if (FError <> EC_OK)
  then Result := Nil
  else Result := FResImage;
end; // TmcmImageMorph.Shrink.


function TmcmImageMorph.Skeleton2(MaxIterations : word) : TmcmImage;
begin
  Pipeline(MPL_SKELET, MaxIterations);
  if (FError <> EC_OK)
  then Result := Nil
  else Result := FResImage;
end; // TmcmImageMorph.Skeleton2.


function TmcmImageMorph.Thin(MaxIterations : word) : TmcmImage;
begin
  Pipeline(MPL_THIN, MaxIterations);
  if (FError <> EC_OK)
  then Result := Nil
  else Result := FResImage;
end; // TmcmImageMorph.Thin.


function TmcmImageMorph.EuclideanDistanceMap : TmcmImage;
var x, y    : integer;
    i, j, n : integer;
    Min     : word;
    SrcLine : array[0..2] of PVectorB;
    ResLine : PVectorW;
    FWLines : array[0..3] of PVectorW;
begin
  // Danielsson,1980
  //
  // 1. Assign the brightness value 0 to each pixel in the background and a large
  //    value (greater than the maximum feature width) to each ¨pixel in the
  //    feature.
  // 2. Proceed from left to right and top to bottom, assigning each pixel
  //    within a feature a brightness value one greater than the smallest value
  //    of any of its neighbours.
  // 3. Repeat step 2, instead proceeding from right to left and bottom to top.

  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8,IF_PAL8])
  then begin
       if (FSrcImage[0] = FResImage)
       then FResImage := Nil;

       CheckResult(IF_GREY16, FSrcImage[0].Width, FSrcImage[0].Height, True);
       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then Exit;

            try
              // Initialise Result image, setting background to "0" and
              // features to 65535.
              FResImage.FillAll(0);
              for y := 1 to (FSrcHeight - 2)
              do begin
                 SrcLine[0] := FSrcImage[0].ScanLine[y];
                 ResLine    := FResImage.ScanLine[y];
                 for x := 1 to (FSrcWidth - 2)
                 do begin
                    if (SrcLine[0]^[x] = FPixON)
                    then ResLine[x] := $FFFF;
                 end;
              end;

              // 1. Pass
              for i := 0 to 1
              do FWLines[i] := FResImage.ScanLine[0];
              n := 2;

              for y := 0 to (FSrcHeight - 1)
              do begin
                 FWLines[n] := FResImage.ScanLine[y+1];
                 ResLine := FResImage.ScanLine[y];
                 for x := 1 to (FSrcWidth - 2)
                 do begin
                    if (ResLine[x] > 0)
                    then begin
                         Min := 65535;
                         for j := 0 to 2
                         do for i := (x - 1) to (x + 1)
                            do if (Min > FWLines[j]^[i])
                            then Min := FWLines[j]^[i];
                         ResLine[x] := Min + 1;
                    end;
                 end;
                 n := (n + 1) mod 3;
              end;

              // 2. Pass
              for i := 0 to 1
              do FWLines[i] := FResImage.ScanLine[FSrcHeight-1];
              n := 2;

              for y := (FSrcHeight - 1) downto 0
              do begin
                 FWLines[n] := FResImage.ScanLine[y-1];
                 ResLine := FResImage.ScanLine[y];
                 for x := (FSrcWidth - 2) downto 1
                 do begin
                    if (ResLine[x] > 0)
                    then begin
                         Min := 65535;
                         for j := 0 to 2
                         do for i := (x - 1) to (x + 1)
                            do if (Min > FWLines[j]^[i])
                            then Min := FWLines[j]^[i];
                         ResLine[x] := Min + 1;
                    end;
                 end;
                 n := (n + 1) mod 3;
              end;      
            finally
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMorph.EuclideanDistanceMap.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
