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
// $Log:  17543: mcmImageColor.pas
//
//    Rev 1.28    2014-02-02 21:09:56  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.27    01-03-2011 20:39:52  mcm    Version: IMG 3.4
//
//    Rev 1.26    20-04-2006 21:29:02  mcm
// Added method ThresholdColorBand.
//
//    Rev 1.25    05-03-2006 13:02:36  mcm    Version: IMG 2.16
// Added helper methods to convert between RGB to/from HSI/HSL and HSV.
//
//    Rev 1.24    29-01-2006 10:26:40  mcm    Version: IMG 2.14
// Improved support for 32 bit image creation of the alpha channel. The
// ConvertTo method now sets the alpha channel to 255, i.e. not transparent.
// The method CombineRGB determines if a 24 or 32 bit image should be created
// based on the value of SourceImage[3].
//
//   Rev 1.23    23-05-2005 22:00:56  mcm    Version: IMG 2.9
// Added Trace threshold method.

//
//   Rev 1.22    29-03-2005 20:20:12  mcm    Version: IMG 2.9
// Set FError to EC_OK at start of every entry method.

//
//   Rev 1.21    13-02-2005 20:00:28  mcm
// Added ThresholdTriangular method

//
//   Rev 1.20    27-10-2004 22:49:26  mcm    Version: IMG 2.6
// Improved error reporting when calling methods on the TmcmImageCOlor control,
// and unified verification of source image. 

//
//   Rev 1.19    30-01-2004 20:10:58  mcm    Version: IMG 2.3
// Modified Set-Alpha, -Blue, -Green, -Red-Channel to return the result image. 
// Added support for BW images in GetIntensity.

//
//   Rev 1.18    21-01-2004 12:28:46  mcm    Version: IMG 2.3
// Modified conditional defines around MMX code.

//
//   Rev 1.17    22-12-2003 18:31:12  mcm
// Optimised GetVectorChannel & GetMatrixChannel by implementing MMX solutions.
// Added CombineRGBA, Get/SetAlphaChannel.

//
//   Rev 1.16    24-11-2003 20:19:28  mcm

//
//   Rev 1.15    17-11-2003 10:03:54  mcm    Version: IMG 2.0
// Added conversion from 8 bit Pal to 8 bit Grey in ConvertTo.

//
//   Rev 1.14    04-11-2003 22:39:00  mcm
// Changed calc of level in ThresholdIsoData - decreases iterations by one.

//
//   Rev 1.13    03-10-2003 14:56:26  mcm
// Fixed memory leak that occured when converting color resolution.

//
//   Rev 1.12    29-09-2003 18:44:34  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.11    13-08-2003 18:05:20  mcm
// Fixed index range problem in ConvertTo

//
//   Rev 1.10    25-07-2003 09:37:38  mcm
// ConvertBWTo, fixed converting inverted (Black = 1) images to higher bit depth.

//
//   Rev 1.9    12-05-2003 15:27:24  mcm    Version: IMG 1.3.4
// Threshold modified to use MMX code.

//
//   Rev 1.8    27-03-2003 16:20:26  mcm    Version: IMG 1.3.3
// Modified ThresholdSymmetry, Level parameter is now the percentage of pixel
// from white and down towards the histogram peak-position.
// Remove redundant code in ThresholdStatistic.

//
//   Rev 1.7    24-02-2003 22:24:28  mcm    Version: IMG 1.3.2
// ConvertTo includes adding/preserving the standard Windows colors, creating
// "web-safe" images.

//
//   Rev 1.6    17-02-2003 10:28:16  mcm    Version: IMG 1.3
// Added luminance histogram equalize method.
// Implemented that Invert creates a Result image if non is assigned.

//
//   Rev 1.5    05-02-03 16:19:02  mcm
// Added Error code to GetHistogram.

//
//   Rev 1.4    27-01-2003 13:38:50  mcm
// Added support for 15 & 16 bit RGB images in ConvertTo.

//
//   Rev 1.3    27-09-2002 12:55:30  mcm    Version: IMG 1.2
// Added 
// - Bayer color filter.
// - Threshold functions: ISO data, Background Symmetry.

//
//   Rev 1.2    07-08-2002 14:14:04  mcm    Version: IMG 1.1
// Included inverting 32 bit images.

//
//   Rev 1.1    01-08-2002 11:25:50  mcm    Version: IMG 1.1
// Added SetAChannel, that copies a grey image to one of the channels R, G, B or
// A.

//
//   Rev 1.0    27-05-2002 16:22:00  mcm

unit mcmImageColor;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmImageKernel;

type
  TmcmBayer = (BF_None, BF_OddOdd, BF_OddEven, BF_EvenOdd, BF_EvenEven);

  TmcmImageColor = class(TmcmImageKernel)
  private
    // Private declarations
    FRedHist       : PVectorC;
    FGreenHist     : PVectorC;
    FBlueHist      : PVectorC;
    FIntHist       : PVectorC;
    FLogPalette    : PLogPalette;
    FUseWinColor   : boolean;

    // ThresholdTrace parameters.
    FBlx, FBby     : integer;
    FBrx, FBty     : integer;
    FNeighbourSize : integer;
    FNeighbour     : PVectorNeighbour;

    // Trace threshold
    FTraceLo       : word;
    FTraceHi       : word;
    FTraceAuto     : boolean;
  protected
    // Protected declarations
    procedure AdaptivePalette(    NoColors   : integer;
                                  Quality    : integer;
                                  AssignMode : integer;
                              var pRGBIndex  : PVectorB);
    procedure ConvertBWTo;
    procedure ConvertPal4To;
    procedure ConvertPal8To;
    procedure ConvertRGB16To;
    procedure ConvertRGB24To;
    procedure ConvertRGBA32To;
    function  GetMatrixChannel(Vector : array of double) : TmcmImage;
    function  GetVectorChannel(Vector : array of double) : TmcmImage;
    procedure GetVolumeHist(VHist : PVectorB);

    procedure EstimatePctThreshold(Src : TKernelImage; Percent : cardinal; var Hi, Lo : integer);
    procedure TraceAndThreshold(AImage : TmcmImage; OImage : TmcmImage; x, y : integer; Value, Hi, Lo : byte);
    procedure ThresholdIsoData(var Level  : word;
                                   xs, ys : longword;
                                   xe, ye : longword);
    procedure ThresholdSymmetry(var Level  : word;
                                    xs, ys : longword;
                                    xe, ye : longword);
    procedure ThresholdLevel(Level  : word;
                             xs, ys : longword;
		             xe, ye : longword);
    procedure ThresholdTrace(Level : word;
                             xs, ys : longword;
		             xe, ye : longword);
    procedure ThresholdTriangular(var Level  : word;
                                     xs, ys : longword;
                                     xe, ye : longword);
    function  ThresholdStatistic(xs, ys : longword;
		                 xe, ye : longword) : double;
    function  ThresholdEdge     (xs, ys : longword;
		                 xe, ye : longword) : double;
    function  ThresholdOptimized(xs, ys : longword;
		                 xe, ye : longword) : double;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override; 
    destructor  Destroy; override;
    function    BayerToRGB(Pattern : TmcmBayer) : TmcmImage;
    function    BrightContrast(Offset, Width : integer) : TmcmImage;
    procedure   Clear; override;
    function    CombineCIED65 : TmcmImage;
    function    CombineCMYK : TmcmImage;
    function    CombineHSV : TmcmImage;
    function    CombineRGB : TmcmImage;
    function    CombineRGBA : TmcmImage;
    function    CombineYCbCr : TmcmImage;
    function    CombineYIQ : TmcmImage;
    function    CombineYUV : TmcmImage;
    function    ConvertTo(ColorFormat : TmcmImageFormat; UseWindowsColor : boolean) : TmcmImage;
    procedure   Equalize;
    procedure   EqualizeLuminance;
    function    Gamma(Value : double) : TmcmImage;
    function    GetAChannel(Channel : byte) : TmcmImage;
    function    GetAlphaChannel : TmcmImage;
    function    GetBlackChannel : TmcmImage;
    function    GetCbChannel : TmcmImage;
    function    GetBlueChannel : TmcmImage;
    function    GetCIEXD65 : TmcmImage;
    function    GetCIEYD65 : TmcmImage;
    function    GetCIEZD65 : TmcmImage;
    function    GetCrChannel : TmcmImage;
    function    GetCyanChannel : TmcmImage;
    function    GetGreenChannel : TmcmImage;
    procedure   GetHistogram;
    function    GetHueChannel : TmcmImage;
    function    GetIntensity : TmcmImage;
    function    GetLuminanceChannel : TmcmImage;
    function    GetMagentaChannel : TmcmImage;
    function    GetNTSCIChannel : TmcmImage;
    function    GetNTSCQChannel : TmcmImage;
    function    GetPALUChannel : TmcmImage;
    function    GetPALVChannel : TmcmImage;
    function    GetRedChannel : TmcmImage;
    function    GetSaturationChannel : TmcmImage;
    function    GetValueChannel : TmcmImage;
    function    GetYellowChannel : TmcmImage;
    procedure   Invert;
    procedure   LookupTable(RedLookup    : PVectorB;
                            GreenLookup  : PVectorB;
                            BlueLookup   : PVectorB);
    procedure   SetAChannel(Channel : byte);
    function    SetAlphaChannel : TmcmImage;
    function    SetBlueChannel : TmcmImage;
    function    SetGreenChannel : TmcmImage;
    procedure   SetHistogram(RedSlope,   RedOffset   : double;
                             GreenSlope, GreenOffset : double;
                             BlueSlope,  BlueOffset  : double);
    function    SetRedChannel : TmcmImage;
    function    Stretch : TmcmImage;
    procedure   Threshold(Method : TmcmThreshold; var Level : word; Col : word; Row : word);
    procedure   ThresholdColorBand(Color : TColorRef; Tolerance : word);

    property    BlueHistogram : PVectorC
      read      FBlueHist;
    property    GreenHistogram : PVectorC
      read      FGreenHist;
    property    IntHistogram : PVectorC
      read      FIntHist;
    property    RedHistogram : PVectorC
      read      FRedHist;
    property    TraceAuto : boolean
      read      FTraceAuto
      write     FTraceAuto;
    property    TraceHigh : word
      read      FTraceHi
      write     FTraceHi;
    property    TraceLow : word
      read      FTraceLo
      write     FTraceLo;
  published
    // Published declarations
  end;

// Helper functions.
procedure RGBToHSI(const R, G, B : integer; var H, S, I : integer);
procedure HSIToRGB(const H, S, I : integer; var R, G, B : integer);
procedure HSVToRGB(const H, S, V : integer; var R, G, B : integer);
procedure RGBToHSV(const R, G, B : integer; var H, S, V : integer);
procedure HSLtoRGB(const H, S, L : integer; var R, G, B : integer);
procedure RGBToHSL(const R, G, B : integer; var H, S, L : integer);

implementation

uses {$IFNDEF GE_DXE2}
      SysUtils, Math;
     {$ELSE}
      System.SysUtils, System.Math;
     {$ENDIF}


{$DEFINE MCMASM}

constructor TmcmImageColor.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FRedHist   := Nil;
  FGreenHist := Nil;
  FBlueHist  := Nil;
  FIntHist   := Nil;
  Clear;
end; // TmcmImageColor.Create.


destructor TmcmImageColor.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmImageColor.Destroy.


procedure TmcmImageColor.Clear;
begin
  FError := EC_OK;
  FTraceLo   := 0;
  FTraceHi   := 0;
  FTraceAuto := True;

  if Assigned(FRedHist)
  then FreeMem(FRedHist);
  FRedHist   := Nil;
  if Assigned(FGreenHist)
  then FreeMem(FGreenHist);
  FGreenHist := Nil;
  if Assigned(FBlueHist)
  then FreeMem(FBlueHist);
  FBlueHist  := Nil;
  if Assigned(FIntHist)
  then FreeMem(FIntHist);
  FIntHist   := Nil;
  Inherited Clear;
end; // TmcmImageColor.Clear.


//------------------------------------------------------------------------------
// Volume Histogram.
//
// This routine analyzes a True Color image, and generates a 3D R-G-B Volume
// Histogram which has 32 elements on a side (32 x 32 x 32).
// The histogram is returned in 'VHist[]'.  Each element of 'VHist[]' stores a
// count from 0 to 255 which is the frequency of occurance for that color in
// the image. The count "saturates" at 255.
// ("That color" means the value indexing 'VHist') The value indexing 'VHist'
// is a 15 bit color value whos 5 MSBs are Red, then 5 bits Green and finally
// 5 LSBs of Blue.
//------------------------------------------------------------------------------

procedure TmcmImageColor.GetVolumeHist(VHist : PVectorB);
var x, y, z   : cardinal;
    v         : byte;
    LongWidth : cardinal;
    pS        : PVectorB;
    Sum       : word;
begin
  // Initialize 32 x 32 x 32 R-G-B historgram to all zeros.
  FillChar(VHist^, 32768 * SizeOf(byte), 0);

  LongWidth := FSrcImage[0].LongLineWidth;
  case FSrcImage[0].ImageFormat of
  IF_GREY4,
  IF_PAL4   : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      x := 0;
                      while (x < LongWidth)
                      do begin
                         z := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z] and $F0) shr 4
                         else v := (pS^[z] and $0F);

                         Sum := 0;
                         // Blue
                         Sum := Sum + (bmiColors[v].rgbBlue shr 3);
                         // Green
                         Sum := Sum + ((bmiColors[v].rgbGreen shr 3) shl 5);
                         // Red
                         Sum := Sum + ((bmiColors[v].rgbRed shr 3) shl 10);
                         inc(x);
                         if (VHist^[Sum] <> 255)
                         then inc(VHist^[Sum]);
                      end;
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      x := 0;
                      while (x < LongWidth)
                      do begin
                         Sum := 0;
                         // Blue
                         Sum := Sum + (bmiColors[pS^[x]].rgbBlue shr 3);
                         // Green
                         Sum := Sum + ((bmiColors[pS^[x]].rgbGreen shr 3) shl 5);
                         // Red
                         Sum := Sum + ((bmiColors[pS^[x]].rgbRed shr 3) shl 10);
                         inc(x);
                         if (VHist^[Sum] <> 255)
                         then inc(VHist^[Sum]);
                      end;
                   end;
                end;
              end;
  IF_RGB15,
  IF_RGB16  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   x := 0;
                   while (x < LongWidth - 1)
                   do begin
                      Sum := pS^[x];
                      inc(x);
                      Sum := Sum + (pS^[x] shl 8);
                      inc(x);
                      if (VHist^[Sum] <> 255)
                      then inc(VHist^[Sum]);
                   end;
                end;
              end;
  IF_RGB24  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   x := 0;
                   while (x < LongWidth - 2)
                   do begin
                      Sum := 0;
                      // Blue
                      Sum := Sum + (pS^[x] shr 3);
                      inc(x);
                      // Green
                      Sum := Sum + ((pS^[x] shr 3) shl 5);
                      inc(x);
                      // Red
                      Sum := Sum + ((pS^[x] shr 3) shl 10);
                      inc(x);
                      if (VHist^[Sum] <> 255)
                      then inc(VHist^[Sum]);
                   end;
                end;
              end;
  IF_RGBA32 : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   x := 0;
                   while (x < LongWidth - 3)
                   do begin
                      Sum := 0;
                      // Blue
                      Sum := Sum + (pS^[x] shr 3);
                      inc(x);
                      // Green
                      Sum := Sum + ((pS^[x] shr 3) shl 5);
                      inc(x);
                      // Red
                      Sum := Sum + ((pS^[x] shr 3) shl 10);
                      inc(x);
                      inc(x);
                      if (VHist^[Sum] <> 255)
                      then inc(VHist^[Sum]);
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
end; // TmcmImageColor.GetVolumeHist.


//------------------------------------------------------------------------------
// AdaptivePalette.
//
// This routine analyzes a True Color image and computes an Adpative Color
// Palette which is an optimal selection of the 256 colors for this particular
// image.  Before calling this routine, 'array' must have been allocated 32768
// bytes!
// First a 32 x 32 x 32 volume histogram is computed.  Then the volume
// histogram "cube" is subdivide into up to 255 cubes (color clusters) used in
// the image.  One more cube in this collection is always reserved for BLACK,
// in RGB space, (this is done so that images which have few colors will still
// get a good solid BLACK color in the CLUT).
// Finally it assigns each one of the cubes to each one of the up to 256
// palette entries, and returns the new Color Look Up Table in the
// 'color_array'. The function return value is the number of colors in the
// 'color_array' and is always equal to 256 for now (unused colors set to black).
// Upon completion, the 32k elements of 'array' store the palette index for
// their respective true color value (down converted to 15 bits).
//------------------------------------------------------------------------------

type TCubeRec  = record
                 x0    : word;
                 y0    : word;
                 z0    : word;
                 x1    : word;    // Outside of cube volume!
                 y1    : word;    // Outside of cube volume!
                 z1    : word;    // Outside of cube volume!
                 vol   : word;    // this is the volume of the cube.
                 r_avg : longint; // this is the Average Red value for cube.
                 g_avg : longint; // this is the Average Green value for cube.
                 b_avg : longint; // this is the Average Blue value for cube.
	               fsum  : longint; // this is the sum of frequencies in cube.
                 cerr  : longint; // this is the NET color error for the cube.
                 fom   : double;  // Figure Of Merit, Worthyness of cube split.
                 Reserved : boolean;
                 end;

     PCubeRec  = ^TCubeRec;

     TCubeList = array[0..256] of TCubeRec;
     PCubeList = ^TCubeList;

// NoColors   - the number of colors in the resulting image.
// Quality    - The range is from 0 to 100. A value close to 0 generates fewer
//              colors but many shades per color. A value closer to 100 generates
//              a broader color distribution but at the expense of shades.
// AssignMode -
procedure TmcmImageColor.AdaptivePalette(    NoColors   : integer;
                                             Quality    : integer;
                                             AssignMode : integer;
                                         var pRGBIndex  : PVectorB);
var pLogPal : PLogPalette;

    //--------------------------------------------------------------------------
    // AddCube
    //
    // Adds a cube to list.  Inserts cube appropriately to maintain
    // a descending cube sort based on CUBE_LIST[i].fom.  In
    // otherwords, CUBE_LIST[0].fom is always going to be the
    // largest value of all cubes.  Give this routine a cube and
    // the current number of cubes in the list 'n'. Routine returns
    // the new value of 'n'.  NOTE: For n cubes, you have
    // CUBE_LIST[0] thru CUBE_LIST[n-1].
    //--------------------------------------------------------------------------

    function AddCube(pCube_List : PCubeList;
                     pNew_Cube  : PCubeRec;
                     n          : word;
                     index      : integer) : word;
    var m : word;
    begin
      m := n + 1;
      if (n > 0)
      then begin
           if (index < 0)
           then begin
                while (pCube_List^[n-1].fom <= pNew_Cube^.fom)
                do begin
                   CopyMemory(@pCube_List^[n], @pCube_List^[n-1], SizeOf(TCubeRec));
                   dec(n);
                   if (n = 0)
                   then break;
                end;
           end
           else begin
                while (n > index)
                do begin
                   CopyMemory(@pCube_List^[n], @pCube_List^[n-1], SizeOf(TCubeRec));
                   dec(n);
                   if (n = 0)
                   then break;
                end;
           end;
      end;
      CopyMemory(@pCube_List^[n], pNew_Cube, SizeOf(TCubeRec));
      AddCube := m;
    end; // AddCube.


    //--------------------------------------------------------------------------
    // DeleteCube
    //
    // Deletes cube 'p' from cube list. Bumps up remaining cubes to
    // keep list consecutive and hole free.  Give this routine the
    // current number of cubes in list ('n') and the cube id to be
    // deleted ('p').  Routine returns the new value of 'n'.
    // NOTE: For n cubes, you have CUBE_LIST[0] thru CUBE_LIST[n-1]
    //--------------------------------------------------------------------------

    function DeleteCube(pCube_List : PCubeList; p, n : word) : word;
    begin
      if (p < n)
      then begin
           while (p < (n - 1))
           do begin
              CopyMemory(@pCube_List^[p], @pCube_List^[p+1], SizeOf(TCubeRec));
              inc(p);
           end;
           DeleteCube := n - 1;
      end
      else DeleteCube := n;
    end; // DeleteCube.


    //--------------------------------------------------------------------------
    // CopyCube
    //
    // This routine copies the 'cube' struct to the 'new_cube' struct.
    //--------------------------------------------------------------------------

    procedure CopyCube(pTarget_Cube, pSource_Cube : PCubeRec);
    begin
      CopyMemory(pTarget_Cube, pSource_Cube, SizeOf(TCubeRec));
    end; // CopyCube.


    //--------------------------------------------------------------------------
    // ValidateCube
    //
    // Insures that cube point 0 is closer to (or same distance  from) origin
    // of RGB Color than cube point 1.
    //--------------------------------------------------------------------------

    procedure ValidateCube(pCube : PCubeRec);
    var Tmp : word;
    begin
      if (pCube^.x0 > pCube^.x1)
      then begin
           Tmp       := pCube^.x0;
           pCube^.x0 := pCube^.x1;
           pCube^.x1 := Tmp;
      end;
      if (pCube^.y0 > pCube^.y1)
      then begin
           Tmp       := pCube^.y0;
           pCube^.y0 := pCube^.y1;
           pCube^.y1 := Tmp;
      end;
      if (pCube^.z0 > pCube^.z1)
      then begin
           Tmp       := pCube^.z0;
           pCube^.z0 := pCube^.z1;
           pCube^.z1 := Tmp;
      end;
    end; // ValidateCube.


    //--------------------------------------------------------------------------
    // BisectCube
    //
    // Input cube in 'cube', output two cubes in 'cube' and 'new_cube'.
    // Cubes are split along longest axis.  New cube guaranteed to be
    // "validated".
    //--------------------------------------------------------------------------

    procedure BisectCube(pOld_Cube, pNew_Cube : PCubeRec);
    var dx      : word;
        dy      : word;
        dz      : word;
        Splited : boolean;
        //sIndex  : longint;
        //MidFreq : longint;
    begin
      Splited := False;
      ValidateCube(pOld_Cube);

      //MidFreq := pOld_Cube^.fsum shr 1;
      //sIndex  := (pOld_Cube^.x0  shl 10) + (pOld_Cube^.y0 shl 5) + pOld_Cube^.z0;


      // Get Red, Green and Blue extent of cube.
      dx := pOld_Cube^.x1 - pOld_Cube^.x0;
      dy := pOld_Cube^.y1 - pOld_Cube^.y0;
      dz := pOld_Cube^.z1 - pOld_Cube^.z0;

      //pNew_Cube^.Reserved := False;
      CopyMemory(pNew_Cube, pOld_Cube, SizeOf(TCubeRec));

      // If Red has longest dimension, split red axis of cube.
      if (dx >= dy) and (dx >= dz)
      then begin
           pNew_Cube^.x0 := (pOld_Cube^.x1 + pOld_Cube^.x0) div 2;
           //pNew_Cube^.y0 := pOld_Cube^.y0;
           //pNew_Cube^.z0 := pOld_Cube^.z0;
           //pNew_Cube^.x1 := pOld_Cube^.x1;
           //pNew_Cube^.y1 := pOld_Cube^.y1;
           //pNew_Cube^.z1 := pOld_Cube^.z1;
           pOld_Cube^.x1 := pNew_Cube^.x0;
           Splited := True;
      end;

      // If Green has longest dimension, split red axis of cube.
      if (dy >= dx) and (dy >= dz) and Not(Splited)
      then begin
           //pNew_Cube^.x0 := pOld_Cube^.x0;
           pNew_Cube^.y0 := (pOld_Cube^.y1 + pOld_Cube^.y0) div 2;
           //pNew_Cube^.z0 := pOld_Cube^.z0;
           //pNew_Cube^.x1 := pOld_Cube^.x1;
           //pNew_Cube^.y1 := pOld_Cube^.y1;
           //pNew_Cube^.z1 := pOld_Cube^.z1;
           pOld_Cube^.y1 := pNew_Cube^.y0;
           Splited := True;
      end;

      // If Blue has longest dimension, split red axis of cube.
      if (dz >= dx) and (dz >= dy) and Not(Splited)
      then begin
           //pNew_Cube^.x0 := pOld_Cube^.x0;
           //pNew_Cube^.y0 := pOld_Cube^.y0;
           pNew_Cube^.z0 := (pOld_Cube^.z1 + pOld_Cube^.z0) div 2;
           //pNew_Cube^.x1 := pOld_Cube^.x1;
           //pNew_Cube^.y1 := pOld_Cube^.y1;
           //pNew_Cube^.z1 := pOld_Cube^.z1;
           pOld_Cube^.z1 := pNew_Cube^.z0;
      end;
    end; // BisectCube.


    //--------------------------------------------------------------------------
    // CalcCubeState
    //
    // Compute cube's color statistics (Volume, Frequency Sum, Average Color
    // value for Red, Green, and Blue, Color Error, and Figure-Of-Merit).
    // The color error is the sum of ABS(color - avg_color)*frequency (sort of)
    // for each color in cube.
    //--------------------------------------------------------------------------

    procedure CalcCubeState(pCube : PCubeRec;
                            VHist : PVectorB;
                            Mode  : integer);
    var i, j, k    : word;
        Freq       : word;
        dr, dg, db : integer;
        sum        : longint;
        Index      : longint;
        Index0     : longint;
        Error      : longint;
        avg_red    : longint;
        avg_green  : longint;
        avg_blue   : longint;
    begin
      sum       := 0;
      avg_red   := 0;
      avg_green := 0;
      avg_blue  := 0;
      Error     := 0;

      // Comput cube's volume.
      pCube^.Vol := (pCube^.x1 - pCube^.x0) *
                    (pCube^.y1 - pCube^.y0) *
                    (pCube^.z1 - pCube^.z0);

      // Compute average red, green and blue values.
      for k := pCube^.x0 to (pCube^.x1 - 1)
      do begin
         index0 := k shl 10;
         for j := pCube^.y0 to (pCube^.y1 - 1)
         do begin
            index := index0 + (j shl 5) + pCube^.z0;
            for i := pCube^.z0 to (pCube^.z1 - 1)
            do begin
               if (VHist^[index] <> 0)
               then begin
                    freq      := VHist^[index];
                    avg_red   := avg_red   + k * freq;
                    avg_green := avg_green + j * freq;
                    avg_blue  := avg_blue  + i * freq;
                    sum       := sum + freq;
               end;
               inc(index);
            end;
         end;
      end;

      if (sum <> 0)
      then begin
           avg_red   := avg_red   div sum;
           avg_green := avg_green div sum;
           avg_blue  := avg_blue  div sum;
      end
      else begin
           avg_red   := 0;
           avg_green := 0;
           avg_blue  := 0;
      end;

      // Now compute colour error.
      for k := pCube^.x0 to (pCube^.x1 - 1)
      do begin
         index0 := k shl 10;
         for j := pCube^.y0 to (pCube^.y1 - 1)
         do begin
            index := index0 + (j shl 5) + pCube^.z0;
            for i := pCube^.z0 to (pCube^.z1 - 1)
            do begin
               if (VHist^[index] <> 0)
               then begin
                    freq := VHist^[index];
                    dr := avg_red - k;
                    dg := avg_green - j;
                    db := avg_blue - i;
                    Error := Error + Round(freq * Sqrt(dr * dr + dg * dg + db * db));
               end;
               inc(index);
            end;
         end;
      end;

      pCube^.r_avg := avg_red;
      pCube^.g_avg := avg_green;
      pCube^.b_avg := avg_blue;
      pCube^.fsum  := sum;
      pCube^.cerr  := Error;

      // You can decide what characteristic is used for the Figure of
      // Merit (FOM) by setting 'mode' to 1 - 5. Modes 1 and 2 are
      // normally used.  Modes 3, 4, and 5 are for play.
      case Mode of
      1 : pCube^.fom := 1.0 * pCube^.vol;
      2 : pcube^.fom := 1.0 * pCube^.cerr;
      3 : pcube^.fom := 1.0 * pCube^.fsum;
      4 : pcube^.fom := 1.0 * pCube^.cerr * pCube^.fsum;
      5 : pcube^.fom := 1.0 * (pCube^.vol - 1) * pCube^.fsum;
      end;
    end; // CalcCubeState.


    //--------------------------------------------------------------------------
    // MOVE_CUBES_TO_LUT
    //
    // This routine loads 'color_array' with the colors of the cubes in the
    // 'CUBE_LIST' (after the 'CUBE_LIST' has been generated).  The return
    // value is the number of colors loaded into 'color_array', and is always
    // 256.
    //--------------------------------------------------------------------------

    procedure CubesToLogPal(pCube_List : PCubeList;
                            Num_Cubes  : integer);
    var i : integer;
        v1, v2 : cardinal;
    begin
      if (pLogPal <> Nil)
      then begin
           // Create new palette.
           FillChar(pLogPal^, (SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * NoColors), 0);

           pLogPal^.palVersion := $300;
           pLogPal^.palNumEntries := NoColors;

           //-------------------------------------------------------------------
           // Compute Color Lookup Table entries. You could do this by taking
           // average of each color component axis of cube.  But instead ...
           // Use the Average color computed earlier for each cube! (This is
           // more accurate color). Multiply by two to scale the 5 bit color
           // into the 6 bit palette entry. Multiply by eight to scale the 5 bit
           // color into the 8 bit palette entry.
           //-------------------------------------------------------------------

           if (Num_Cubes > 256)
           then Num_Cubes := 256;
           if (Num_Cubes > 2)
           then begin
                for i := 0 to (Num_Cubes - 1)
                do begin
                   with pLogPal^.palPalEntry[i]
                   do begin
                      if pCube_List^[i].Reserved
                      then begin
                           peRed   := pCube_List^[i].r_avg;
                           peGreen := pCube_List^[i].g_avg;
                           peBlue  := pCube_List^[i].b_avg;
                      end
                      else begin
                           peRed   := 8 * pCube_List^[i].r_avg;
                           peGreen := 8 * pCube_List^[i].g_avg;
                           peBlue  := 8 * pCube_List^[i].b_avg;

                           peRed   := peRed + (peRed div 35);
                           peGreen := peGreen + (peGreen div 35);
                           peBlue  := peBlue + (peBlue div 35);
                      end;
                   end;
                end;
           end
           else begin
                // Ensure that Black is at entry zero.
                v1 := pCube_List^[0].r_avg * pCube_List^[0].g_avg * pCube_List^[0].b_avg;
                v2 := pCube_List^[1].r_avg * pCube_List^[1].g_avg * pCube_List^[1].b_avg;

                if (v1 < v2)
                then begin
                     for i := 0 to (Num_Cubes - 1)
                     do begin
                        with pLogPal^.palPalEntry[i]
                        do begin
                           peRed   := i * 255;
                           peGreen := i * 255;
                           peBlue  := i * 255;
                        end;
                     end;
                end
                else begin
                     for i := 0 to (Num_Cubes - 1)
                     do begin
                        with pLogPal^.palPalEntry[i]
                        do begin
                           peRed   := (1 - i) * 255;
                           peGreen := (1 - i) * 255;
                           peBlue  := (1 - i) * 255;
                        end;
                     end;
                end;
           end;
      end;
    end; // CubesToLogPal.


    //--------------------------------------------------------------------------
    // ASSIGN_LUT_TO_COLORS
    //
    // Assign each of the 32768 color values in the RGB Color Space (compressed
    // from 16M) to one of the up to 256 cubes (which map to the color lut).
    // 'AssignMode' determines the way that colors are assigned.  If '0',
    // then color error is minimized. Otherwise, all colors which land within a
    // cube get set to cube average color.
    //--------------------------------------------------------------------------

    procedure LutToColours(VHist      : PVectorB;
                           pCube_List : PCubeList;
                           Num_Cubes  : integer;
                           AssignMode : integer);
    var i, j, k, n : word;
    var Red        : integer;
        Green      : integer;
        Blue       : integer;
        dr, dg, db : integer;
        Error      : cardinal;
        Last_Error : cardinal;
        index      : longint;
        index0     : longint;
    begin
      if (AssignMode <> 0)
      then begin
           //-------------------------------------------------------------------
           // For each colour in the image, assign it the Cube ID for the Cube
           // which has the nearest average colour value.
           //-------------------------------------------------------------------
           for i := 0 to 32767
           do begin
              if (VHist^[i] <> 0)
              then begin
                   Red        := (i and $7C00) shr 10;
                   Green      := (i and $03E0) shr 5;
                   Blue       := (i and $001F);
                   Last_Error := 1000000;
                   for n := 0 to (Num_Cubes - 1)
                   do begin
                      if pCube_List^[n].Reserved
                      then begin
                           dr := pCube_List^[n].r_avg - Red shl 3;
                           dg := pCube_List^[n].g_avg - Green shl 3;
                           db := pCube_List^[n].b_avg - Blue shl 3;
                      end
                      else begin
                           dr := pCube_List^[n].r_avg - Red;
                           dg := pCube_List^[n].g_avg - Green;
                           db := pCube_List^[n].b_avg - Blue;
                      end;
                      Error := dr * dr + dg * dg + db * db;
                      if (Error <= Last_Error)
                      then begin
                           VHist^[i]  := n;
                           Last_Error := Error;
                      end;
                   end;
              end;
           end;
      end
      else begin
           //-------------------------------------------------------------------
           // For each cube, assign its ID to all of the colours in its boundary.
           // This methode is not as accurate as the one above but it's faster.
           //-------------------------------------------------------------------
           for n := 0 to (Num_Cubes - 1)
           do begin
              for k := pCube_List^[n].x0 to (pCube_List^[n].x1 - 1)
              do begin
                 Index0 := k shl 10;
                 for j := pCube_List^[n].y0 to (pCube_List^[n].y1 - 1)
                 do begin
                    Index := Index0 + (j shl 5) + pCube_List^[n].z0;
                    for i := pCube_List^[n].z0 to (pCube_List^[n].z1 - 1)
                    do begin
                       VHist^[Index] := n;
                       inc(Index);
                    end;
                 end;
              end;
           end;
      end;
    end; // LutToColours.


    function ReserveColor(Index, R, G, B : word;
                          pCube_List     : PCubeList;
                          Num_Cubes      : integer) : integer;
    var Cube       : TCubeRec;
        i, j       : integer;
        dr, dg, db : integer;
        Error      : cardinal;
        Last_Error : cardinal;
    begin
      j := 0;
      if (NoColors <= Num_Cubes)
      then begin
           Last_Error := 1000000;
           // Find cube closest to color defined by R, G and B.
           for i := 0 to (Num_Cubes - 1)
           do begin
              if Not(pCube_List^[i].Reserved)
              then begin
                   dr := (pCube_List^[i].r_avg shl 3) - R;
                   dg := (pCube_List^[i].g_avg shl 3) - G;
                   db := (pCube_List^[i].b_avg shl 3) - B;
                   Error := dr * dr + dg * dg + db * db;
                   if (Error <= Last_Error)
                   then begin
                        j := i;
                        Last_Error := Error;
                   end;
              end;
           end;
           CopyCube(@Cube, @pCube_List[j]);
      end;

      // Change color.
      Cube.r_avg := R;
      Cube.g_avg := G;
      Cube.b_avg := B;
      Cube.Reserved := True;

      // Move cube to position defined by index.
      if (NoColors <= Num_Cubes)
      then Num_Cubes := DeleteCube(pCube_List, j, Num_Cubes);

      Num_Cubes := AddCube(pCube_List, @Cube, Num_Cubes, Index);
      Result := Num_Cubes;
    end; // ReserveColor.


var  Num_Cubes  : integer;
     Num_Vol    : integer;
     Most_Cubes : integer;
     Cube       : TCubeRec;
     NewCube    : TCubeRec;
     Cube_List  : TCubeList;
begin
  GetMem(pLogPal, (SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * NoColors));
  pRGBIndex := AllocMem(32800 * SizeOf(byte));

  if (pLogPal <> Nil) and (pRGBIndex <> Nil)
  then begin
       if (FSrcImage[0].BitCount in [4,8,15,16,24,32])
       then begin
            if (NoColors <= 2)
            then Most_Cubes := 2
            else Most_Cubes := NoColors;
            Num_Cubes  := 0;
            if (Quality < 0)
            then Quality := 0;
            if (Quality > 100)
            then Quality := 100;
            Num_Vol := (Quality * Most_Cubes) div 100;
            if (NoColors <= 2)
            then Num_Vol := 2;
            GetVolumeHist(pRGBIndex);

            // Initialize first Cube. Get it's colour state and put it in the
            // Cube list.
            if (Most_Cubes > 0)
            then begin
                 Cube.x0 := 0;
                 Cube.y0 := 0;
                 Cube.z0 := 0;
                 Cube.x1 := 32;
                 Cube.y1 := 32;
                 Cube.z1 := 32;
                 Cube.Reserved := False;
                 CalcCubeState(@Cube, pRGBIndex, 1);
                 Num_Cubes := AddCube(@Cube_List, @Cube, Num_Cubes, -1);
            end;

            // Subdivide colour volume based on minimizing cube volumes.
            // Do for Num_Vol cubes.
            while (Num_Cubes < Num_Vol) and (Cube_List[0].Vol > 1)
            do begin
               CopyCube(@Cube, @Cube_List[0]);
               BisectCube(@Cube, @NewCube);
               CalcCubeState(@Cube, pRGBIndex, 1);
               CalcCubeState(@NewCube, pRGBIndex, 1);
               Num_Cubes := DeleteCube(@Cube_List, 0, Num_Cubes);
               if (Cube.fsum <> 0)
               then Num_Cubes := AddCube(@Cube_List, @Cube, Num_Cubes, -1);
               if (NewCube.fsum <> 0)
               then Num_Cubes := AddCube(@Cube_List, @NewCube, Num_Cubes, -1);
            end;

            // Continue subdivide colour volume based on minimizing cube
            // colour errors.
            // Do for remaining cubes (up to Num_Colours - 1).
            while (Num_Cubes < Most_Cubes) and (Cube_List[0].Vol > 1)
            do begin
               CopyCube(@Cube, @Cube_List[0]);
               BisectCube(@Cube, @NewCube);
               CalcCubeState(@Cube, pRGBIndex, 2);
               CalcCubeState(@NewCube, pRGBIndex, 2);
               Num_Cubes := DeleteCube(@Cube_List, 0, Num_Cubes);
               if (Cube.fsum <> 0)
               then Num_Cubes := AddCube(@Cube_List, @Cube, Num_Cubes, -1);
               if (NewCube.fsum <> 0)
               then Num_Cubes := AddCube(@Cube_List, @NewCube, Num_Cubes, -1);
            end;

            // Reserve Windows palette colors.
            if FUseWinColor or (NoColors = 2)
            then begin
                 Num_Cubes := ReserveColor(0, 0, 0, 0, @Cube_List, Num_Cubes); // Black

                 if (NoColors > 15)
                 then begin
                      Num_Cubes := ReserveColor(1, 128, 0, 0, @Cube_List, Num_Cubes); // Maroon
                      Num_Cubes := ReserveColor(2, 0, 128, 0, @Cube_List, Num_Cubes); // Green
                      Num_Cubes := ReserveColor(3, 128, 128, 0, @Cube_List, Num_Cubes); // Olive
                      Num_Cubes := ReserveColor(4, 0, 0, 128, @Cube_List, Num_Cubes); // Navy
                      Num_Cubes := ReserveColor(5, 128, 0, 128, @Cube_List, Num_Cubes); // Purple
                      Num_Cubes := ReserveColor(6, 0, 128, 128, @Cube_List, Num_Cubes); // Teal
                      Num_Cubes := ReserveColor(7, 192, 192, 192, @Cube_List, Num_Cubes); // Silver
                      Num_Cubes := ReserveColor(8, 127, 127, 127, @Cube_List, Num_Cubes); // Grey
                      
                      if (NoColors > 19)
                      then begin
                           Num_Cubes := ReserveColor(9, 166, 202, 240, @Cube_List, Num_Cubes); // Blue
                           Num_Cubes := ReserveColor(NoColors-1, 224, 224, 224, @Cube_List, Num_Cubes); // Grey
                           Num_Cubes := ReserveColor(NoColors-1, 160, 160, 164, @Cube_List, Num_Cubes); // Grey
                           Num_Cubes := ReserveColor(NoColors-1, 128, 128, 128, @Cube_List, Num_Cubes); // Grey
                      end;

                      Num_Cubes := ReserveColor(NoColors-1, 255, 0, 0, @Cube_List, Num_Cubes); // Red
                      Num_Cubes := ReserveColor(NoColors-1, 0, 255, 0, @Cube_List, Num_Cubes); // Lime
                      Num_Cubes := ReserveColor(NoColors-1, 255, 255, 0, @Cube_List, Num_Cubes); // Yellow
                      Num_Cubes := ReserveColor(NoColors-1, 0, 0, 255, @Cube_List, Num_Cubes); // Blue
                      Num_Cubes := ReserveColor(NoColors-1, 255, 0, 255, @Cube_List, Num_Cubes); // Fuchsia
                      Num_Cubes := ReserveColor(NoColors-1, 0, 255, 255, @Cube_List, Num_Cubes); // Aqua
                 end;
                 Num_Cubes := ReserveColor(NoColors-1, 255, 255, 255, @Cube_List, Num_Cubes); // White
            end;

            // Finally, copy cubes to hPal = HPalette.
            CubesToLogPal(@Cube_List, Num_Cubes);
            FResImage.SetPaletteEntries(pLogPal);

            LutToColours(pRGBIndex, @Cube_List, Num_Cubes, AssignMode);
       end;
  end;
  if (pLogPal <> Nil)
  then FreeMem(pLogPal);
end; // TmcmImageColor.AdaptivePalette


procedure TmcmImageColor.ConvertBWTo;
var x, y, z : cardinal;
    i, j    : longint;
    pS, pR  : PVectorB;
    BColor  : byte;
    WColor  : byte;

begin
  if (FSrcImage[0].DibInfo.bmiColors[0].rgbBlue = 0)
  then begin
       BColor  := 0;
       WColor  := 255;
  end
  else begin
       BColor  := 255;
       WColor  := 0;
  end;

  case FResImage.ImageFormat of
  IF_GREY4,
  IF_PAL4   : begin
                FResImage.CreateGreyPalette;
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   j := 0;
                   i := 7;
                   if (BColor = 0)
                   then begin
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           z := x shr 1;
                           if ((pS[j] and BitMask[i]) <> 0)
                           then pR^[z] := pR^[z] or ($0F shl (4 - 4 * (x and 1)))
                           else pR^[z] := pR^[z] and ($0F shl (4 * (x and 1)));
                           dec(i);
                           if (i < 0)
                           then begin
                                i := 7;
                                inc(j);
                           end;
                        end;
                   end
                   else begin
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           z := x shr 1;
                           if ((pS[j] and BitMask[i]) = 0)
                           then pR^[z] := pR^[z] or ($0F shl (4 - 4 * (x and 1)))
                           else pR^[z] := pR^[z] and ($0F shl (4 * (x and 1)));
                           dec(i);
                           if (i < 0)
                           then begin
                                i := 7;
                                inc(j);
                           end;
                        end;
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                FResImage.CreateGreyPalette;
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   j := 0;
                   i := 7;
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      if ((pS[j] and BitMask[i]) <> 0)
                      then pR^[x] := WColor
                      else pR^[x] := BColor;
                      dec(i);
                      if (i < 0)
                      then begin
                           i := 7;
                           inc(j);
                      end;
                   end;
                end;
              end;
  IF_RGB15,
  IF_RGB16  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   j := 0;
                   i := 7;
                   x := 0;
                   while (x < FResImage.LongLineWidth)
                   do begin
                      if ((pS[j] and BitMask[i]) <> 0)
                      then begin
                           pR^[x] := WColor;
                           inc(x);
                           pR^[x] := WColor;
                      end
                      else begin
                           pR^[x] := BColor;
                           inc(x);
                           pR^[x] := BColor;
                      end;
                      inc(x);
                      dec(i);
                      if (i < 0)
                      then begin
                           i := 7;
                           inc(j);
                      end;
                   end;
                end;
              end;
  IF_RGB24  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   j := 0;
                   i := 7;
                   z := 0;
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      if ((pS[j] and BitMask[i]) <> 0)
                      then begin
                           pR^[z] := WColor;
                           inc(z);
                           pR^[z] := WColor;
                           inc(z);
                           pR^[z] := WColor;
                           inc(z);
                      end
                      else begin
                           pR^[z] := BColor;
                           inc(z);
                           pR^[z] := BColor;
                           inc(z);
                           pR^[z] := BColor;
                           inc(z);
                      end;
                      dec(i);
                      if (i < 0)
                      then begin
                           i := 7;
                           inc(j);
                      end;
                   end;
                end;
              end;
  IF_RGBA32 : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   j := 0;
                   i := 7;
                   z := 0;
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      if ((pS[j] and BitMask[i]) <> 0)
                      then begin
                           pR^[z] := WColor;
                           inc(z);
                           pR^[z] := WColor;
                           inc(z);
                           pR^[z] := WColor;
                           inc(z);
                      end
                      else begin
                           pR^[z] := BColor;
                           inc(z);
                           pR^[z] := BColor;
                           inc(z);
                           pR^[z] := BColor;
                           inc(z);
                      end;
                      pR^[z] := 255;
                      inc(z);
                      dec(i);
                      if (i < 0)
                      then begin
                           i := 7;
                           inc(j);
                      end;
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
end; // TmcmImageColor.ConvertBWTo.


procedure TmcmImageColor.ConvertPal4To;
var x, y       : cardinal;
    z1, z4     : cardinal;
    i, j       : longint;
    v          : byte;
    pS, pR     : PVectorB;
    Quality    : integer;
    AssignMode : integer;
    AdaptMode  : integer;
    pRGBIndex  : PVectorB;
    Sum        : cardinal;
    pPal       : PLogPalette;
    NoColors   : integer;
begin
  pRGBIndex  := Nil;
  case FResImage.ImageFormat of
  IF_BW     : begin
                AdaptMode  := 0;
                AssignMode := 1;
                with FSrcImage[0].DibInfo^
                do begin
                   Quality := 100;
                   if (AdaptMode = 0)
                   then AdaptivePalette(2, Quality, AssignMode, pRGBIndex)
                   else ; // Load True color palette!

                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];

                      j  := 7;
                      z1 := 0;
                      // Convert RGB True Colour to RGB palette values.
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z4 := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z4] and $F0) shr 4
                         else v := (pS^[z4] and $0F);

                         Sum := 0;
                         Sum := Sum + (bmiColors[v].rgbBlue shr 3);
                         Sum := Sum + (bmiColors[v].rgbGreen shr 3) shl 5;
                         Sum := Sum + (bmiColors[v].rgbRed shr 3) shl 10;

                         if (pRGBIndex^[Sum] = 0)
                         then pR^[z1] := (pR^[z1] and Not(BitMask[j]))
                         else pR^[z1] := (pR^[z1] or BitMask[j]);

                         dec(j);
                         if (j < 0)
                         then begin
                              j := 7;
                              inc(z1);
                         end;
                      end;
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                NoColors := FSrcImage[0].GetPaletteEntries(Nil);
                if (NoColors > 0)
                then begin
                     GetMem(pPal, 256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                     if Assigned(pPal)
                     then begin
                          FSrcImage[0].GetPaletteEntries(pPal);
                          pPal^.palNumEntries := 256;
                          FResImage.SetPaletteEntries(pPal);
                          FreeMem(pPal);
                     end;
                end;

                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z4 := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z4] and $F0) shr 4
                         else v := (pS^[z4] and $0F);

                         pR^[x] := v;
                      end;
                   end;
                end;
              end;
  IF_RGB15,
  IF_RGB16  : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z4 := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z4] and $F0) shr 4
                         else v := (pS^[z4] and $0F);

                         pR^[i] := ((bmiColors[v].rgbBlue and $F8) shr 3);
                         pR^[i] := pR^[i] + ((bmiColors[v].rgbGreen and $38) shl 2);
                         inc(i);
                         pR^[i] := (bmiColors[v].rgbGreen and $C0) shr 6;
                         pR^[i] := pR^[i] + ((bmiColors[v].rgbRed and $F8) shr 1);
                         inc(i);
                      end;
                   end;
                end;
              end;
  IF_RGB24  : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z4 := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z4] and $F0) shr 4
                         else v := (pS^[z4] and $0F);

                         pR^[i] := bmiColors[v].rgbBlue;
                         inc(i);
                         pR^[i] := bmiColors[v].rgbGreen;
                         inc(i);
                         pR^[i] := bmiColors[v].rgbRed;
                         inc(i);
                      end;
                   end;
                end;
              end;
  IF_RGBA32 : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z4 := x shr 1;
                         if ((x and 1) = 0)
                         then v := (pS^[z4] and $F0) shr 4
                         else v := (pS^[z4] and $0F);

                         pR^[i] := bmiColors[v].rgbBlue;
                         inc(i);
                         pR^[i] := bmiColors[v].rgbGreen;
                         inc(i);
                         pR^[i] := bmiColors[v].rgbRed;
                         inc(i);
                         pR^[i] := 255;
                         inc(i);
                      end;
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
  if (pRGBIndex <> Nil)
  then FreeMem(pRGBIndex);
end; // TmcmImageColor.ConvertPal4To.


procedure TmcmImageColor.ConvertPal8To;
var x, y, z    : cardinal;
    i, j       : longint;
    pS, pR     : PVectorB;
    Quality    : integer;
    AssignMode : integer;
    AdaptMode  : integer;
    pRGBIndex  : PVectorB;
    Sum        : cardinal;
begin
  pRGBIndex  := Nil;
  AdaptMode  := 0;
  AssignMode := 1;
  case FResImage.ImageFormat of
  IF_BW     : begin
                with FSrcImage[0].DibInfo^
                do begin
                   Quality    := 100;
                   if (AdaptMode = 0)
                   then AdaptivePalette(2, Quality, AssignMode, pRGBIndex)
                   else ; // Load True color palette!

                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];

                      i := 0;
                      j := 7;
                      z := 0;
                      // Convert RGB True Colour to RGB palette values.
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         Sum := 0;
                         Sum := Sum + (bmiColors[pS^[i]].rgbBlue shr 3);
                         Sum := Sum + (bmiColors[pS^[i]].rgbGreen shr 3) shl 5;
                         Sum := Sum + (bmiColors[pS^[i]].rgbRed shr 3) shl 10;
                         inc(i);

                         if (pRGBIndex^[Sum] = 0)
                         then pR^[z] := (pR^[z] and Not(BitMask[j]))
                         else pR^[z] := (pR^[z] or BitMask[j]);

                         dec(j);
                         if (j < 0)
                         then begin
                              j := 7;
                              inc(z);
                         end;
                      end;
                   end;
                end;
              end;
  IF_GREY4,
  IF_PAL4   : begin
                pRGBIndex  := Nil;
                Quality    := 100;
                AdaptMode  := 0;
                AssignMode := 1;
                if (AdaptMode = 0)
                then AdaptivePalette(16, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];

                      i := 0;
                      // Convert RGB True Colour to RGB palette values.
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         Sum := 0;
                         Sum := Sum + (bmiColors[pS^[i]].rgbBlue shr 3);
                         Sum := Sum + (bmiColors[pS^[i]].rgbGreen shr 3) shl 5;
                         Sum := Sum + (bmiColors[pS^[i]].rgbRed shr 3) shl 10;
                         inc(i);

                         z := x shr 1;
                         if ((x and 1) = 0)
                         then pR^[z] := pRGBIndex^[Sum] shl 4
                         else pR^[z] := (pR^[z] or pRGBIndex^[Sum]);
                      end;
                   end;
                end;
              end;
  IF_GREY8  : GetLuminanceChannel;
  IF_RGB15,
  IF_RGB16  : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         z := pS^[x];
                         pR^[i] := ((bmiColors[z].rgbBlue and $F8) shr 3);
                         pR^[i] := pR^[i] + ((bmiColors[z].rgbGreen and $38) shl 2);
                         inc(i);
                         pR^[i] := (bmiColors[z].rgbGreen and $C0) shr 6;
                         pR^[i] := pR^[i] + ((bmiColors[z].rgbRed and $F8) shr 1);
                         inc(i);
                      end;
                   end;
                end;
              end;
  IF_RGB24  : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         pR^[i] := bmiColors[pS^[x]].rgbBlue;
                         inc(i);
                         pR^[i] := bmiColors[pS^[x]].rgbGreen;
                         inc(i);
                         pR^[i] := bmiColors[pS^[x]].rgbRed;
                         inc(i);
                      end;
                   end;
                end;
              end;
  IF_RGBA32 : begin
                with FSrcImage[0].DibInfo^
                do begin
                   for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      i := 0;
                      pS := FSrcImage[0].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         pR^[i] := bmiColors[pS^[x]].rgbBlue;
                         inc(i);
                         pR^[i] := bmiColors[pS^[x]].rgbGreen;
                         inc(i);
                         pR^[i] := bmiColors[pS^[x]].rgbRed;
                         inc(i);
                         pR^[i] := 255;
                         inc(i);
                      end;
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
  if (pRGBIndex <> Nil)
  then FreeMem(pRGBIndex);
end; // TmcmImageColor.ConvertPal8To.


procedure TmcmImageColor.ConvertRGB16To;
var x, y, z    : longint;
    i, j       : longint;
    pS, pR     : PVectorB;
    Quality    : integer;
    AssignMode : integer;
    AdaptMode  : integer;
    pRGBIndex  : PVectorB;
    Sum        : cardinal;
begin
  pRGBIndex  := Nil;
  AdaptMode  := 0;
  AssignMode := 1;
  case FResImage.ImageFormat of
  IF_BW     : begin
                Quality    := 100;
                if (AdaptMode = 0)
                then AdaptivePalette(2, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   j := 7;
                   z := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := pS^[i];
                      inc(i);
                      Sum := Sum + (pS^[i] shl 8);
                      inc(i);

                      if (pRGBIndex^[Sum] = 0)
                      then pR^[z] := (pR^[z] and Not(BitMask[j]))
                      else pR^[z] := (pR^[z] or BitMask[j]);

                      dec(j);
                      if (j < 0)
                      then begin
                           j := 7;
                           inc(z);
                      end;
                   end;
                end;
              end;
  IF_GREY4,
  IF_PAL4   : begin
                Quality    := 100;
                if (AdaptMode = 0)
                then AdaptivePalette(16, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := pS^[i];
                      inc(i);
                      Sum := Sum + (pS^[i] shl 8);
                      inc(i);

                      z := x shr 1;
                      if ((x and 1) = 0)
                      then pR^[z] := pRGBIndex^[Sum] shl 4
                      else pR^[z] := (pR^[z] or pRGBIndex^[Sum]);
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                Quality    := 50;
                if (AdaptMode = 0)
                then AdaptivePalette(256, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   if (AdaptMode = 0)
                   then begin
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           Sum := pS^[i];
                           inc(i);
                           Sum := Sum + (pS^[i] shl 8);
                           inc(i);
                           pR^[x] := pRGBIndex^[Sum];
                        end;
                   end
                   else begin
                        // Requires a Fixed RGB palette that is not included
                        // here.
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           Sum := 0;
                           Sum := Sum + (pS^[i] div 64);
                           inc(i);
                           Sum := Sum + (pS^[i] div 32) shl 2;
                           inc(i);
                           Sum := Sum + (pS^[i] div 32) shl 5;
                           inc(i);
                           pR^[x] := byte(Sum);
                        end;
                   end;
                end;
              end;
  IF_RGB24  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (x < 2 * FSrcImage[0].Width)
                   do begin
                      pR^[i] := (pS^[x] and $1F) shl 3;
                      inc(i);
                      pR^[i] := (pS^[x] and $E0) shr 2;
                      inc(x);
                      pR^[i] := pR^[i] + (pS^[x] and $3) shl 6;
                      inc(i);
                      pR^[i] := (pS^[x] and $7C) shl 1;
                      inc(x);
                      inc(i);
                   end;
                end;
              end;
  IF_RGBA32 : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (x < 2 * FSrcImage[0].Width)
                   do begin
                      pR^[i] := (pS^[x] and $1F) shl 3;
                      inc(i);
                      pR^[i] := (pS^[x] and $E0) shr 2;
                      inc(x);
                      pR^[i] := pR^[i] + (pS^[x] and $3) shl 6;
                      inc(i);
                      pR^[i] := (pS^[x] and $7C) shl 1;
                      inc(x);
                      inc(i);
                      pR^[i] := 255;
                      inc(i);
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
  if (pRGBIndex <> Nil)
  then FreeMem(pRGBIndex);
end; // TmcmImageColor.ConvertRGB16To.


procedure TmcmImageColor.ConvertRGB24To;
var x, y, z    : cardinal;
    i, j       : longint;
    pS, pR     : PVectorB;
    Quality    : integer;
    AssignMode : integer;
    AdaptMode  : integer;
    pRGBIndex  : PVectorB;
    Sum        : cardinal;
begin
  pRGBIndex  := Nil;
  AdaptMode  := 0;
  AssignMode := 1;
  case FResImage.ImageFormat of
  IF_BW     : begin
                Quality := 100;
                if (AdaptMode = 0)
                then AdaptivePalette(2, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   j := 7;
                   z := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := 0;
                      Sum := Sum + (pS^[i] shr 3);
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 5;
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 10;
                      inc(i);

                      if (pRGBIndex^[Sum] = 0)
                      then pR^[z] := (pR^[z] and Not(BitMask[j]))
                      else pR^[z] := (pR^[z] or BitMask[j]);

                      dec(j);
                      if (j < 0)
                      then begin
                           j := 7;
                           inc(z);
                      end;
                   end;
                end;
              end;
  IF_GREY4,
  IF_PAL4   : begin
                Quality := 100;
                if (AdaptMode = 0)
                then AdaptivePalette(16, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := 0;
                      Sum := Sum + (pS^[i] shr 3);
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 5;
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 10;
                      inc(i);

                      z := x shr 1;
                      if ((x and 1) = 0)
                      then pR^[z] := pRGBIndex^[Sum] shl 4
                      else pR^[z] := (pR^[z] or pRGBIndex^[Sum]);
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                Quality    := 50;
                if (AdaptMode = 0)
                then AdaptivePalette(256, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   if (AdaptMode = 0)
                   then begin
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           Sum := 0;
                           Sum := Sum + (pS^[i] shr 3);
                           inc(i);
                           Sum := Sum + (pS^[i] shr 3) shl 5;
                           inc(i);
                           Sum := Sum + (pS^[i] shr 3) shl 10;
                           inc(i);
                           pR^[x] := pRGBIndex^[Sum];
                        end;
                   end
                   else begin
                        // Requires a Fixed RGB palette that is not included
                        // here.
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           Sum := 0;
                           Sum := Sum + (pS^[i] div 64);
                           inc(i);
                           Sum := Sum + (pS^[i] div 32) shl 2;
                           inc(i);
                           Sum := Sum + (pS^[i] div 32) shl 5;
                           inc(i);
                           pR^[x] := byte(Sum);
                        end;
                   end;
                end;
              end;
  IF_RGB15,
  IF_RGB16  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (x < FSrcImage[0].LongLineWidth - 2)
                   do begin
                      pR^[i] := pS^[x] shr 3;
                      inc(x);
                      pR^[i] := pR^[i] + ((pS^[x] and $38) shl 2);
                      inc(i);
                      pR^[i] := ((pS^[x] and $C0) shr 6);
                      inc(x);
                      pR^[i] := pR^[i] + (pS^[x] and $F8) shr 1;
                      inc(x);
                      inc(i);
                   end;
                end;
              end;
  IF_RGBA32 : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (x < FSrcImage[0].LongLineWidth - 2)
                   do begin
                      pR^[i] := pS^[x];
                      inc(x);
                      inc(i);
                      pR^[i] := pS^[x];
                      inc(x);
                      inc(i);
                      pR^[i] := pS^[x];
                      inc(x);
                      inc(i);
                      pR^[i] := 255;
                      inc(i);
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
  if (pRGBIndex <> Nil)
  then FreeMem(pRGBIndex);
end; // TmcmImageColor.ConvertRGB24To.


procedure TmcmImageColor.ConvertRGBA32To;
var x, y, z    : cardinal;
    i, j       : longint;
    pS, pR     : PVectorB;
    Quality    : integer;
    AssignMode : integer;
    AdaptMode  : integer;
    pRGBIndex  : PVectorB;
    Sum        : cardinal;
begin
  pRGBIndex  := Nil;
  AdaptMode  := 0;
  AssignMode := 1;
  case FResImage.ImageFormat of
  IF_BW     : begin
                Quality    := 100;
                if (AdaptMode = 0)
                then AdaptivePalette(2, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   j := 7;
                   z := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := 0;
                      Sum := Sum + (pS^[i] shr 3);
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 5;
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 10;
                      inc(i);
                      inc(i);

                      if (pRGBIndex^[Sum] = 0)
                      then pR^[z] := (pR^[z] and Not(BitMask[j]))
                      else pR^[z] := (pR^[z] or BitMask[j]);

                      dec(j);
                      if (j < 0)
                      then begin
                           j := 7;
                           inc(z);
                      end;
                   end;
                end;
              end;
  IF_PAL4   : begin
                pRGBIndex  := Nil;
                Quality    := 100;
                AdaptMode  := 0;
                AssignMode := 1;
                if (AdaptMode = 0)
                then AdaptivePalette(16, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := 0;
                      Sum := Sum + (pS^[i] shr 3);
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 5;
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 10;
                      inc(i);
                      inc(i);

                      z := x shr 1;
                      if ((x and 1) = 0)
                      then pR^[z] := pRGBIndex^[Sum] shl 4
                      else pR^[z] := (pR^[z] or pRGBIndex^[Sum]);
                   end;
                end;
              end;
  IF_GREY8,
  IF_PAL8   : begin
                Quality    := 50;
                if (AdaptMode = 0)
                then AdaptivePalette(256, Quality, AssignMode, pRGBIndex)
                else ; // Load True color palette!

                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];

                   i := 0;
                   // Convert RGB True Colour to RGB palette values.
                   for x := 0 to (FSrcImage[0].Width - 1)
                   do begin
                      Sum := 0;
                      Sum := Sum + (pS^[i] shr 3);
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 5;
                      inc(i);
                      Sum := Sum + (pS^[i] shr 3) shl 10;
                      inc(i);
                      inc(i);
                      pR^[x] := pRGBIndex^[Sum];
                   end;
                end;
              end;
  IF_RGB15,
  IF_RGB16  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (x < FSrcImage[0].LongLineWidth - 3)
                   do begin
                      pR^[i] := pS^[x] shr 3;
                      inc(x);
                      pR^[i] := pR^[i] + ((pS^[x] and $38) shl 2);
                      inc(i);
                      pR^[i] := ((pS^[x] and $C0) shr 6);
                      inc(x);
                      pR^[i] := pR^[i] + (pS^[x] and $F8) shr 1;
                      inc(x);
                      inc(x);
                      inc(i);
                   end;
                end;
              end;
  IF_RGB24  : begin
                for y := 0 to (FSrcImage[0].Height - 1)
                do begin
                   i  := 0;
                   pS := FSrcImage[0].ScanLine[y];
                   pR := FResImage.ScanLine[y];
                   x  := 0;
                   while (i < longint(FSrcImage[0].LongLineWidth - 3))
                   do begin
                      pR^[x] := pS^[i];
                      inc(x);
                      inc(i);
                      pR^[x] := pS^[i];
                      inc(x);
                      inc(i);
                      pR^[x] := pS^[i];
                      inc(x);
                      inc(i);
                      inc(i);
                   end;
                end;
              end;
  // Error - Image color format cannot be applied in this algorithme.
  else FError := EC_BADCOLORFORMAT;
  end;
  if (pRGBIndex <> Nil)
  then FreeMem(pRGBIndex);
end; // TmcmImageColor.ConvertRGBA32To.


function TmcmImageColor.ConvertTo(ColorFormat : TmcmImageFormat; UseWindowsColor : boolean) : TmcmImage;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0])
  then begin
       CheckResult(ColorFormat, FSrcImage[0].Width, FSrcImage[0].Height, False);
       if Assigned(FResImage)
       then if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Result := Nil;
                 Exit;
            end;

       if (FSrcImage[0].ImageFormat = ColorFormat)
       then begin
            // Copy Source to Result image.
            FResImage.Assign(FSrcImage[0]);
            Result := FResImage;
            Exit;
       end;

       FUseWinColor := UseWindowsColor;

       case FSrcImage[0].ImageFormat of
       IF_BW     : ConvertBWTo;
       IF_GREY4,
       IF_PAL4   : ConvertPal4To;
       IF_GREY8,
       IF_PAL8   : ConvertPal8To;
       IF_RGB15,
       IF_RGB16  : ConvertRGB16To;
       IF_RGB24  : ConvertRGB24To;
       IF_RGBA32 : ConvertRGBA32To;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.ConvertTo.


function TmcmImageColor.BayerToRGB(Pattern : TmcmBayer) : TmcmImage;
// Converts a greyscale bayer image to a 24 bit colour image.

  //-------------------------------------------------------
  // Bayer pattern                                        |
  //                                                      |
  //      |   Odd column   |  Even column   |             |
  //-------------------------------------------------------
  // Odd  |  Blue   Green  |  Green  Blue   |  Even lines |
  // row  |  Green  Red    |  Red    Green  |  Odd lines  |
  //-------------------------------------------------------
  // Even |  Green  Red    |  Red    Green  |  Even lines |
  // row  |  Blue   Green  |  Green  Blue   |  Odd lines  |
  //-------------------------------------------------------
var x, y      : longint;
    i         : longint;
    pS, pR    : PVectorB;
    pF        : array[0..2] of PVectorB;
    WidthRGB  : longint;
    WidthGrey : longint;
    Height    : longint;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0])
  then begin
       CheckResult(IF_RGB24, FSrcImage[0].Width, FSrcImage[0].Height, False);
       if Assigned(FResImage)
       then if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Result := Nil;
                 Exit;
            end;

       Height    := FSrcImage[0].Height;
       WidthGrey := FSrcImage[0].Width;
       WidthRGB  := FSrcImage[0].Width * 3;
       y := 0;
       try
       case FSrcImage[0].ImageFormat of
       IF_GREY8 : begin
                    case Pattern of
                    BF_OddOdd   : begin
                                    // Target
                                    // line 1: B G  -  Even line
                                    // line 2: G R
                                    // Source
                                    // B G R B G R

                                    while (y < FSrcImage[0].Height)
                                    do begin
                                       pS := FSrcImage[0].ScanLine[y];
                                       pR := FResImage.ScanLine[y];
                                       i := 0;
                                       x := 0;

                                       if Odd(y)
                                       then begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);

                                               // RED
                                               inc(i,2);
                                               pR^[i] := pS^[x]; // Red
                                               inc(i);
                                               inc(x);
                                            end;
                                       end
                                       else begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // BLUE
                                               pR^[i] := pS^[x]; // Blue
                                               inc(i,3);
                                               inc(x);

                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);
                                            end;
                                       end;
                                       inc(y);
                                    end;

                                    y := 1;
                                    while (y < FSrcImage[0].Height - 1)
                                    do begin
                                       pF[0] := FResImage.ScanLine[y-1];
                                       pF[1] := FResImage.ScanLine[y];
                                       pF[2] := FResImage.ScanLine[y+1];
                                       pR := FResImage.ScanLine[y];

                                       if Odd(y)
                                       then begin
                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Blue channel
                                            x := 6;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 4;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 8;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;
                                       end
                                       else begin
                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 7;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;
                               
                                            // Interpolate Red channel
                                            x := 8;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;
                                       end;
                                       inc(y);
                                    end;
                                  end;
                    BF_OddEven  : begin
                                    // Target
                                    // line 1: G B  -  Even line
                                    // line 2: R G
                                    // Source
                                    // B G R B G R

                                    while (y < FSrcImage[0].Height)
                                    do begin
                                       pS := FSrcImage[0].ScanLine[y];
                                       pR := FResImage.ScanLine[y];
                                       i := 0;
                                       x := 0;

                                       if Odd(y)
                                       then begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // RED
                                               inc(i,2);
                                               pR^[i] := pS^[x]; // Red
                                               inc(i);
                                               inc(x);

                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);
                                            end;
                                       end
                                       else begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);

                                               // BLUE
                                               pR^[i] := pS^[x]; // Blue
                                               inc(i,3);
                                               inc(x);
                                            end;
                                       end;
                                       inc(y);
                                    end;

                                    y := 1;
                                    while (y < FSrcImage[0].Height - 1)
                                    do begin
                                       pF[0] := FResImage.ScanLine[y-1];
                                       pF[1] := FResImage.ScanLine[y];
                                       pF[2] := FResImage.ScanLine[y+1];
                                       pR := FResImage.ScanLine[y];

                                       if Odd(y)
                                       then begin
                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Blue channel
                                            x := 6;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 7;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;
                                       end
                                       else begin
                                            // Interpolate Blue channel
                                            x := 6;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 4;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 2;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;
                                       end;
                                       inc(y);
                                    end;
                                  end;
                    BF_EvenOdd  : begin
                                    // Target
                                    // line 1: G R  -  Even line
                                    // line 2: B G
                                    // Source
                                    // B G R B G R

                                    while (y < FSrcImage[0].Height)
                                    do begin
                                       pS := FSrcImage[0].ScanLine[y];
                                       pR := FResImage.ScanLine[y];
                                       i := 0;
                                       x := 0;

                                       if Odd(y)
                                       then begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // BLUE
                                               pR^[i] := pS^[x]; // Blue
                                               inc(i,3);
                                               inc(x);

                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);
                                            end;
                                       end
                                       else begin
                                            while (x < FSrcImage[0].Width)
                                            do begin
                                               // GREEN
                                               inc(i);
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);

                                               // RED
                                               inc(i,2);
                                               pR^[i] := pS^[x]; // Red
                                               inc(i);
                                               inc(x);
                                            end;
                                       end;
                                       inc(y);
                                    end;

                                    y := 1;
                                    while (y < FSrcImage[0].Height - 1)
                                    do begin
                                       pF[0] := FResImage.ScanLine[y-1];
                                       pF[1] := FResImage.ScanLine[y];
                                       pF[2] := FResImage.ScanLine[y+1];
                                       pR := FResImage.ScanLine[y];

                                       if Odd(y)
                                       then begin
                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 7;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 8;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;
                                       end
                                       else begin
                                            // Interpolate Blue channel
                                            x := 0;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 4;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 8;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;
                                       end;
                                       inc(y);
                                    end;
                                  end;
                    BF_EvenEven : begin
                                    // Target
                                    // line 0: R G  -  Even line
                                    // line 1: G B
                                    // Source
                                    // B G R B G R

                                    while (y < Height)
                                    do begin
                                       pS := FSrcImage[0].ScanLine[y];
                                       pR := FResImage.ScanLine[y];

                                       {$IFDEF MCMASM}
                                       asm
                                         push  edi
                                         push  esi

                                         mov   esi,pS                     // ebx pointer to source image
                                         mov   edi,pR                     // edx pointer to result image
                                         mov   ecx,WidthGrey

                                         test  y,$01
                                         jz    @EvenLine

                                         @OddLine:
                                         add   edi,$01

                                         @LoopOdd:
                                         mov   al,BYTE PTR [esi] // Get 1 pixel (1 byte)
                                         mov   BYTE PTR [edi],al
                                         inc   esi
                                         add   edi,$02

                                         mov   al,BYTE PTR [esi] // Get 1 pixel (1 byte)
                                         mov   BYTE PTR [edi],al
                                         inc   esi
                                         add   edi,$04

                                         dec   ecx
                                         dec   ecx
                                         jnz   @LoopOdd
                                         jmp   @EndLine

                                         @EvenLine:
                                         add   edi,$02

                                         @LoopEven:
                                         mov   al,BYTE PTR [esi] // Get 1 pixel (1 byte)
                                         mov   BYTE PTR [edi],al
                                         inc   esi
                                         add   edi,$02

                                         mov   al,BYTE PTR [esi] // Get 1 pixel (1 byte)
                                         mov   BYTE PTR [edi],al
                                         inc   esi
                                         add   edi,$04

                                         dec   ecx
                                         dec   ecx
                                         jnz   @LoopEven

                                         @EndLine:

                                         pop   esi
                                         pop   edi
                                       end;

                                       {$ELSE}

                                       x := 0;
                                       if Odd(y)
                                       then begin
                                            i := 1;
                                            while (x < WidthGrey)
                                            do begin
                                               // GREEN
                                               pR^[i] := pS^[x]; // Green
                                               inc(i,2);
                                               inc(x);

                                               // BLUE
                                               pR^[i] := pS^[x]; // Blue
                                               inc(i,4);
                                               inc(x);
                                            end;
                                       end
                                       else begin
                                            i := 2;
                                            while (x < WidthGrey)
                                            do begin
                                               // RED
                                               pR^[i] := pS^[x];
                                               inc(i,2);
                                               inc(x);

                                               // GREEN
                                               pR^[i] := pS^[x];
                                               inc(i,4);
                                               inc(x);
                                            end;
                                       end;
                                       {$ENDIF}
                                       inc(y);
                                    end;

                                    y := 1;
                                    while (y < FSrcImage[0].Height - 1)
                                    do begin
                                       pF[0] := FResImage.ScanLine[y-1];
                                       pF[1] := FResImage.ScanLine[y];
                                       pF[2] := FResImage.ScanLine[y+1];
                                       pR := FResImage.ScanLine[y];

                                       if Odd(y)
                                       then begin
                                            // Interpolate Blue channel
                                            x := 6;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 4;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 8;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;
                                       end
                                       else begin
                                            // Interpolate Blue channel
                                            x := 3;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x]) shr 1;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Blue channel
                                            x := 6;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x-3] + pF[2]^[x-3] + pF[0]^[x+3] + pF[2]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Green channel
                                            x := 7;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[0]^[x] + pF[2]^[x] + pF[1]^[x-3] + pF[1]^[x+3]) shr 2;
                                               inc(x, 6);
                                            end;

                                            // Interpolate Red channel
                                            x := 5;
                                            while (x < WidthRGB)
                                            do begin
                                               pR^[x] := word(pF[1]^[x-3] + pF[1]^[x+3]) shr 1;
                                               inc(x, 6);
                                            end;
                                       end;
                                       inc(y);
                                    end;
                                  end;
                    end;
                  end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end; // End case ImageFormat.
       except
         FError := EC_UNKNOWN;
       end;
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.BayerToRGB.


function TmcmImageColor.CombineCIED65 : TmcmImage;
begin
  Result := GetMatrixChannel([ 3.240479, -1.537150, -0.498535, 0.0,
                              -0.969256,  1.875992,  0.041556, 0.0,
                               0.055648, -0.204043,  1.057311, 0.0]);
end; // TmcmImageColor.CombineCIED65.


function TmcmImageColor.CombineCMYK : TmcmImage;
// Red     = (2^Sample Precision - 1) - K - C
// Blue    = (2^Sample Precision - 1) - K - Y
// Green   = (2^Sample Precision - 1) - K - M
var x, y, i    : longint;
    pSC, pSM   : PVectorB;
    pSY, pSK   : PVectorB;
    pR         : PVectorB;
begin
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if (CheckSource(0, [IF_GREY8]) and
      CheckSource(1, [IF_GREY8]) and
      CheckSource(2, [IF_GREY8]) and
      CheckSource(3, [IF_GREY8]))
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_RGB24;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if Not(FResImage.ImageFormat in [IF_RGB24,IF_RGBA32])
            then FResImage.ImageFormat := IF_RGB24;
            FResImage.Width := FSrcImage[0].Width;
            FResImage.Height := FSrcImage[0].Height;

            if (FSrcImage[0].Empty or
                FSrcImage[1].Empty or
                FSrcImage[2].Empty or
                FSrcImage[3].Empty or
                FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Result := Nil;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pSC := FSrcImage[0].ScanLine[y];
                        pSM := FSrcImage[1].ScanLine[y];
                        pSY := FSrcImage[2].ScanLine[y];
                        pSK := FSrcImage[3].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           // Blue.
                           pR^[i] := 255 - pSK[x] - pSM[x];
                           inc(i);

                           // Green.
                           pR^[i] := 255 - pSK[x] - pSY[x];
                           inc(i);

                           // Red.
                           pR^[i] := 255 - pSK[x] - pSC[x];
                           inc(i);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.CombineCMYK.


function TmcmImageColor.CombineHSV : TmcmImage;
var x, y, i    : longint;
    pS1, pS2   : PVectorB;
    pS3, pR    : PVectorB;
    Hue        : double;
    s, v       : double;
    f, p, q, t : double;
    iHue       : word;
begin
  Result := Nil;
  FError := EC_OK;

  // Make sure we have a valid Result Image.
  if (CheckSource(0, [IF_GREY8]) and
      CheckSource(1, [IF_GREY8]) and
      CheckSource(2, [IF_GREY8]))
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_RGB24;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or
                FSrcImage[1].Empty or
                FSrcImage[2].Empty or
                FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS1 := FSrcImage[0].ScanLine[y]; // Hue
                        pS2 := FSrcImage[1].ScanLine[y]; // Saturation
                        pS3 := FSrcImage[2].ScanLine[y]; // Value
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           if (pS2[x] = 0)
                           then begin // Achromatic (Grey).
                                // Blue.
                                pR^[i] := pS3[x];
                                inc(i);
                                // Green.
                                pR^[i] := pS3[x];
                                inc(i);
                                // Red.
                                pR^[i] := pS3[x];
                                inc(i);
                           end
                           else begin
                                Hue  := pS1[x] / 42.50; // 6667;
                                iHue := Trunc(Hue);
                                f := Hue - iHue;
                                s := pS2[x] / 255.0;
                                v := pS3[x] / 255.0;
                                p := v * (1.0 - s);
                                q := v * (1.0 - s * f);
                                t := v * (1.0 - s * (1.0 - f));
                                case iHue of
                                0 :  begin
                                       // Blue.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                       // Green.
                                       pR^[i] := Round(255.0 * t);
                                       inc(i);
                                       // Red.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                     end;
                                1 :  begin
                                       // Blue.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                       // Green.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                       // Red.
                                       pR^[i] := Round(255.0 * q);
                                       inc(i);
                                     end;
                                2 :  begin
                                       // Blue.
                                       pR^[i] := Round(255.0 * t);
                                       inc(i);
                                       // Green.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                       // Red.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                     end;
                                3 :  begin
                                       // Blue.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                       // Green.
                                       pR^[i] := Round(255.0 * q);
                                       inc(i);
                                       // Red.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                     end;
                                4 :  begin
                                       // Blue.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                       // Green.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                       // Red.
                                       pR^[i] := Round(255.0 * t);
                                       inc(i);
                                     end;
                                else begin
                                       // Blue.
                                       pR^[i] := Round(255.0 * q);
                                       inc(i);
                                       // Green.
                                       pR^[i] := Round(255.0 * p);
                                       inc(i);
                                       // Red.
                                       pR^[i] := pS3[x];
                                       inc(i);
                                     end;
                                end;
                           end;
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.CombineHSV.


function TmcmImageColor.CombineRGB : TmcmImage;
var x, y, i  : longint;
    pS1, pS2 : PVectorB;
    pS3, pS4 : PVectorB;
    pR       : PVectorB;
begin
  Result := Nil;
  FError := EC_OK;

  // Make sure we have a valid Result Image.
  if (CheckSource(0, [IF_GREY8]) and
      CheckSource(1, [IF_GREY8]) and
      CheckSource(2, [IF_GREY8]))
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            if CheckSource(3, [IF_GREY8])
            then FResImage.ImageFormat := IF_RGBA32
            else FResImage.ImageFormat := IF_RGB24;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or
                FSrcImage[1].Empty or
                FSrcImage[2].Empty or
                (FSrcImage[2].Empty and (FResImage.ImageFormat = IF_RGBA32)) or
                FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FResImage.ImageFormat of
       IF_RGB24  : for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS1 := FSrcImage[0].ScanLine[y];
                      pS2 := FSrcImage[1].ScanLine[y];
                      pS3 := FSrcImage[2].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      i := 0;
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         // Blue.
                         pR^[i] := pS3[x];
                         inc(i);

                         // Green.
                         pR^[i] := pS2[x];
                         inc(i);

                         // Red.
                         pR^[i] := pS1[x];
                         inc(i);
                      end;
                   end;
       IF_RGBA32 : for y := 0 to (FSrcImage[0].Height - 1)
                   do begin
                      pS1 := FSrcImage[0].ScanLine[y];
                      pS2 := FSrcImage[1].ScanLine[y];
                      pS3 := FSrcImage[2].ScanLine[y];
                      pS4 := FSrcImage[3].ScanLine[y];
                      pR := FResImage.ScanLine[y];
                      i := 0;
                      for x := 0 to (FSrcImage[0].Width - 1)
                      do begin
                         // Blue.
                         pR^[i] := pS3[x];
                         inc(i);

                         // Green.
                         pR^[i] := pS2[x];
                         inc(i);

                         // Red.
                         pR^[i] := pS1[x];
                         inc(i);

                         // Alpha.
                         pR^[i] := pS4[x];
                         inc(i);
                      end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.CombineRGB.


function TmcmImageColor.CombineRGBA : TmcmImage;
var x, y, i  : longint;
    pS1, pS2 : PVectorB;
    pS3, pS4 : PVectorB;
    pR       : PVectorB;
begin
  Result := Nil;
  FError := EC_OK;

  // Make sure we have a valid Result Image.
  if (CheckSource(0, [IF_GREY8]) and
      CheckSource(1, [IF_GREY8]) and
      CheckSource(2, [IF_GREY8]) and
      CheckSource(3, [IF_GREY8]))
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_RGBA32;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or
                FSrcImage[1].Empty or
                FSrcImage[2].Empty or
                FSrcImage[3].Empty or
                FResImage.Empty)
            then begin
                 Exit;
                 FError := EC_NOMEMORY;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS1 := FSrcImage[0].ScanLine[y];
                        pS2 := FSrcImage[1].ScanLine[y];
                        pS3 := FSrcImage[2].ScanLine[y];
                        pS4 := FSrcImage[3].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           // Blue.
                           pR^[i] := pS3[x];
                           inc(i);

                           // Green.
                           pR^[i] := pS2[x];
                           inc(i);

                           // Red.
                           pR^[i] := pS1[x];
                           inc(i);

                           // Alpha.
                           pR^[i] := pS4[x];
                           inc(i);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.CombineRGBA.


function TmcmImageColor.CombineYCbCr : TmcmImage;
begin
  Result := GetMatrixChannel([1.0,  0.0,     1.4022, 0.0,
                              1.0, -0.3456, -0.7145, -127.50,
                              1.0,  1.7710,  0.0,    -127.50]);
end; // TmcmImageColor.CombineYCbCr.


function TmcmImageColor.CombineYIQ : TmcmImage;
begin
  Result := GetMatrixChannel([1.0,  0.956,  0.621, 0.0,
                              1.0, -0.272, -0.647, {-127.5}0,
                              1.0, -1.105,  1.702, {-127.5}0]);
end; // TmcmImageColor.CombineYIQ.


function TmcmImageColor.CombineYUV : TmcmImage;
begin
  Result := GetMatrixChannel([1.0,  0.0,    1.140,  0.0,
                              1.0, -0.394, -0.581, {-127.5}0,
                              1.0,  2.028,  0.000, {-127.5}0]);
end; // TmcmImageColor.CombineYUV.


function TmcmImageColor.GetAChannel(Channel : byte) : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
begin
  Result := Nil;
  FError := EC_OK;
  // Returns either the R, G or B color channel.

  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     with FSrcImage[0].DibInfo^
                     do begin
                        case Channel of
                        // Blue
                        0 : for y := 0 to (FSrcImage[0].Height - 1)
                            do begin
                               pS := FSrcImage[0].ScanLine[y];
                               pR := FResImage.ScanLine[y];
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do pR^[x] := bmiColors[pS^[x]].rgbBlue;
                            end;
                        // Green
                        1 : for y := 0 to (FSrcImage[0].Height - 1)
                            do begin
                               pS := FSrcImage[0].ScanLine[y];
                               pR := FResImage.ScanLine[y];
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do pR^[x] := bmiColors[pS^[x]].rgbGreen;
                            end;
                        // Red
                        2 : for y := 0 to (FSrcImage[0].Height - 1)
                            do begin
                               pS := FSrcImage[0].ScanLine[y];
                               pR := FResImage.ScanLine[y];
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do pR^[x] := bmiColors[pS^[x]].rgbRed;
                            end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := Channel;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           pR^[x] := pS^[i];
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := Channel;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           pR^[x] := pS^[i];
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetAChannel.


procedure TmcmImageColor.SetAChannel(Channel : byte);
var x, y, i : longint;
    pS, pR  : PVectorB;
begin
  FError := EC_OK;
  // Apply either the R, G or B color channel.

  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            if (Channel = 3)
            then FResImage.ImageFormat := IF_RGBA32
            else FResImage.ImageFormat := IF_RGB24;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
            if (Channel = 3) and (FResImage.ImageFormat <> IF_RGBA32)
            then begin
                 FError := EC_BADCOLORFORMAT;
                 Exit;
            end;
       end;

       if (FSrcImage[0].ImageFormat = IF_GREY8)
       then begin
            case FResImage.ImageFormat of
            //IF_RGB15
            //IF_RGB16
            IF_RGB24  : begin
                          for y := 0 to (FSrcImage[0].Height - 1)
                          do begin
                             pS := FSrcImage[0].ScanLine[y];
                             pR := FResImage.ScanLine[y];
                             i := Channel;
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                pR^[i] := pS^[x];
                                inc(i,3);
                             end;
                          end;
                        end;
            IF_RGBA32 : begin
                          for y := 0 to (FSrcImage[0].Height - 1)
                          do begin
                             pS := FSrcImage[0].ScanLine[y];
                             pR := FResImage.ScanLine[y];
                             i := Channel;
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                pR^[i] := pS^[x];
                                inc(i,4);
                             end;
                          end;
                        end;
            // Error - Image color format cannot be applied in this algorithme.
            else FError := EC_BADCOLORFORMAT;
            end;
       end;
  end;
end; // TmcmImageColor.SetAChannel.


function TmcmImageColor.GetVectorChannel(Vector : array of double) : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    Value   : longint;
    {$IFNDEF DCB3_5}
    Count   : integer;
    Coef    : array[0..3] of smallint;
    YR1G0R0 : array[0..3] of smallint;
    YB1G1B0 : array[0..3] of smallint;
    COffset : array[0..1] of longint;
    {$ENDIF}
begin
  Result := Nil;
  FError := EC_OK;
  
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            FResImage.CreateGreyPalette;
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       {$IFNDEF DCB3_5}
       if FMMX
       then begin
            Count := FSrcImage[0].Width;
            case FSrcImage[0].ImageFormat of
            IF_RGB24  : begin // RGB 24
                          YR1G0R0[0] := Round(32768.0 * Vector[2]);
                          YR1G0R0[1] := Round(32768.0 * Vector[1]);
                          YR1G0R0[2] := 0;
                          YR1G0R0[3] := Round(32768.0 * Vector[2]);

                          YB1G1B0[0] := Round(32768.0 * Vector[0]);
                          YB1G1B0[1] := 0;
                          YB1G1B0[2] := Round(32768.0 * Vector[1]);
                          YB1G1B0[3] := Round(32768.0 * Vector[0]);
                          COffset[0] := Round(32768.0 * (Vector[3] {+ 0.4999}));
                          COffset[1] := COffset[0];
                        end;
            IF_RGBA32 : begin
                          Coef[0] := Round(32768.0 * Vector[2]);
                          Coef[1] := Round(32768.0 * Vector[1]);
                          Coef[2] := Round(32768.0 * Vector[0]);
                          COffset[0] := Round(32768.0 * (Vector[3] {+ 0.4999}));
                          COffset[1] := 0;
                        end;
            end;
       end;
       {$ENDIF}

       try
         case FSrcImage[0].ImageFormat of
         IF_GREY8,
         IF_PAL8   : begin
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          pR := FResImage.ScanLine[y];
                          with FSrcImage[0]
                          do begin
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                Value := Round(Vector[0] * DibInfo.bmiColors[pS^[x]].rgbRed +
                                               Vector[1] * DibInfo.bmiColors[pS^[x]].rgbGreen +
                                               Vector[2] * DibInfo.bmiColors[pS^[x]].rgbBlue +
                                               Vector[3]);
                                if (Value < 0)
                                then pR^[x] := 0
                                else if (Value > 255)
                                     then pR^[x] := 255
                                     else pR^[x] := Value;
                             end;
                          end;
                       end;
                     end;
         //IF_RGB15
         //IF_RGB16
         IF_RGB24  : begin
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          pR := FResImage.ScanLine[y];

                          {$IFNDEF DCB3_5}
                          if FMMX
                          then begin
                               asm
                                 // Save registers to stack
                                 push      ebx
                                 push      edi
                                 push      esi

                                 // data points to process.
                                 mov       ecx,Count
                                 shr       ecx,3        // Count := Count div 8
                                 test      ecx,ecx
                                 jz        @EndOfLine   // Check that data count > zero bytes.

                                 // Set-up initial pointers to image line and coefficients.
                                 mov       esi,pS
                                 mov       edi,pR

                                 {$IFNDEF DCB3_5}
                                 movq      mm4,COffset
                                 {$ELSE}
                                 {$ENDIF}

                                 // process 3 byte RGB pixels
                                 @LoopQWORD:
                                 {$IFNDEF DCB3_5}
                                 // Read source data
	                               movq      mm1,[esi]    // load G2R2B1G1R1B0G0R0
                                 pxor	     mm3,mm3	    // 0 -> mm6

	                               movq	     mm0,mm1	    // G2R2B1G1R1B0G0R0 -> mm0
	                               psrlq	   mm1,16	      // 0000G2R2B1G1R1B0 -> mm1
	                               punpcklbw mm0,mm3      // R1B0G0R0 -> mm0
	                               movq	     mm7,mm1      // 00G2R2B1G1R1B0 -> mm7
	                               punpcklbw mm1,mm3      // B1G1R1B0 -> mm1
	                               pmaddwd	 mm0,YR1G0R0  // yrR1, ygG0+yrR0 -> mm0
	                               pmaddwd	 mm1,YB1G1B0  // ybB1+ygG1, ybB0 -> mm1
	                               punpckhbw mm7,mm3      // 00G2R2 -> mm7
                                 paddd     mm0,mm1	    // Y1Y0 -> mm0

                                 // Read source data
	                               movq	     mm1,[esi+8]  // R5B4G4R4B3G3R3B2 -> mm1
                                 paddd     mm0,mm4      // Y1Y0+Offset
	                               movq	     mm6,mm1	    // R5B4G4R4B3G3R3B2 -> mm6
	                               punpcklbw mm1,mm3      // B3G3R3B2 -> mm1
	                               movq	     mm5,mm1	    // B3G3R3B2 -> mm5
 	                               psllq	   mm1,32	      // R3B200 -> mm1
                                 paddd     mm1,mm7      // R3B200+00G2R2 = R3B2G2R2 -> mm1
	                               punpckhbw mm6,mm3      // R5B4G4R3 -> mm6
	                               pmaddwd	 mm1,YR1G0R0  // yrR3, ygG2+yrR2 -> mm1
                                 pmaddwd   mm5,YB1G1B0  // ybB3+ygG3, ybB2 -> mm5
	                               psrad	   mm0,15	      // 32-bit scaled Y1Y0 -> mm0
                                 paddd     mm1,mm4      // Y3Y2+Offset
	                               paddd	   mm1,mm5	    // Y3Y2 -> mm1

	                               psrad		 mm1,15	      // 32-bit scaled Y3Y2 -> mm1
	                               packssdw  mm0,mm1	    // Y3Y2Y1Y0 -> mm0

                                 // Read source data
	                               movq	     mm1,[esi+16] // B7G7R7B6G6R6B5G5 -> mm7
	                               movq 	   mm7,mm1	    // B7G7R7B6G6R6B5G5 -> mm1

	                               movq	     mm2,mm6      // R5B4G4R4 -> mm0
	                               psllq	   mm7,16	      // R7B6G6R6B5G500 -> mm7
	                               movq  	   mm5,mm7	    // R7B6G6R6B5G500 -> mm5
	                               punpcklbw mm7,mm3      // B5G500 -> mm7
                                 psrlq	   mm2,32	      // 00R5B4 -> mm0
                                 punpckhbw mm1,mm3	    // B7G7R7B6 -> mm1
	                               paddw 	   mm7,mm2	    // B5G5R5B4 -> mm7
	                               pmaddwd	 mm6,YR1G0R0  // yrR5, ygG4+yrR4 -> mm2
	                               pmaddwd	 mm7,YB1G1B0  // ybB5+ygG5, ybB4 -> mm7
	                               punpckhbw mm5,mm3	    // R7B6G6R6 -> mm5
                                 paddd     mm6,mm4      // Y5Y4+Offset
	                               paddd	   mm6,mm7	    // Y5Y4 -> mm2

	                               pmaddwd	 mm1,YB1G1B0  // ybB7+ygG7, ybB6 -> mm6
	                               pmaddwd	 mm5,YR1G0R0	// yrR7, ygG6+yrR6 -> mm5
                                 paddd     mm1,mm4      // Y7Y6+Offset
	                               psrad	   mm6,15       // 32-bit scaled Y5Y4 -> mm2
	                               paddd	   mm1,mm5	    // Y7Y6 -> mm6

	                               psrad	   mm1,15	      // 32-bit scaled Y7Y6 -> mm6
	                               packssdw  mm6,mm1	    // Y7Y6Y5Y4 -> mm2

                                 add       esi,24       // Increment source addr.

	                               packuswb  mm0,mm6	    // all 8 Y values -> mm6
                                 movq	     [edi],mm0	  // store Y

                                 {$ELSE}
                                 {$ENDIF}
                                 add       edi,8
                                 dec       ecx        // dec index to coordinate buffers.
                                 jnz       @LoopQWORD

                                 @EndOfLine:
                                 // Check for remaining (1-3) pixels in line
                                 mov       ecx,Count
                                 mov       edx,ecx
                                 shr       ecx,3
                                 shl       ecx,3
                                 sub       edx,ecx
                                 jz        @EndOfData
                                 //dec       ecx

                                 xor       eax,eax
                                 mov       ebx,eax
                                 @LoopLast:
                                 mov       ax,[esi]
                                 add       esi,2
                                 movd      mm0,eax
	                               punpcklbw mm0,mm3      // B3G3R3B2 -> mm1

                                 mov       bl,[esi]
                                 movd      mm1,ebx

	                               pmaddwd	 mm0,YR1G0R0
                                 pmaddwd   mm1,YB1G1B0
                                 paddd     mm0,COffset
                                 inc       esi
                                 paddd     mm0,mm1	    // Y0 -> mm0
	                               psrad	   mm0,15	      // 32-bit scaled Y0 -> mm0
                                 movd      eax,mm0

                                 mov       [edi],al
                                 inc       edi
                                 dec       edx
                                 jnz       @LoopLast


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
                          {$ENDIF}
                               i := 0;
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do begin
                                  Value := Round(Vector[0] * pS^[i+2] +
                                                 Vector[1] * pS^[i+1] +
                                                 Vector[2] * pS^[i] + Vector[3]);
                                  if (Value < 0)
                                  then pR^[x] := 0
                                  else if (Value > 255)
                                       then pR^[x] := 255
                                       else pR^[x] := Value;
                                  inc(i,3);
                               end;
                          {$IFNDEF DCB3_5}
                          end;
                          {$ENDIF}
                       end;
                     end;
         IF_RGBA32 : begin
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          pR := FResImage.ScanLine[y];
                          {$IFNDEF DCB3_5}
                          if FMMX
                          then begin
                               asm
                                 // Save registers to stack
                                 push      ebx
                                 push      edi
                                 push      esi

                                 // data points to process.
                                 mov       ecx,Count
                                 mov       edx,ecx
                                 test      ecx,ecx
                                 jz        @EndOfData   // Check that data count > zero bytes.

                                 shr       ecx,2        // Count := Count div 2
                                 shl       ecx,1
                                 dec       ecx
                                 test      ecx,ecx
                                 jz        @EndOfData   // Check that data count > zero bytes.

                                 shr       edx,2
                                 dec       edx

                                 // Set-up initial pointers to image line and coefficients.
                                 mov       edi,pR
                                 mov       esi,pS

                                 {$IFNDEF DCB3_5}
                                 pxor      mm7,mm7
                                 movq      mm6,Coef // Load coefficients into mm6
                                 movq      mm3,COffset
                                 {$ELSE}
                                 {$ENDIF}

                                 // process 4 byte RGBA pixels
                                 @LoopQWORD:
                                 {$IFNDEF DCB3_5}
                                 movq      mm0,[esi+ecx*8] // Read two pixels
                               movq      mm1,mm0
                                 punpcklbw mm0,mm7 // unpack lower 4 bytes to 4 words
                               punpckhbw mm1,mm7 // unpack upper 4 bytes to 4 words

                                 // pixel in lower dword of [esi+ecx*8]
                                 pmaddwd   mm0,mm6 // 0 + G*Coef[2], B*Coef[1] + R*Coef[0]
                               // pixel in upper dword of [esi+ecx*8]
                               pmaddwd   mm1,mm6 // 0 + G*Coef[2], B*Coef[1] + R*Coef[0]

                                 movq      mm4,mm0 // Move mm0 upper dword
                               movq      mm2,mm1 // Move mm1 upper dword
                                 punpckhdq mm4,mm7 // to lower mm4 dword
                               punpckhdq mm2,mm7 // to lower mm4 dword
                                 paddd     mm0,mm4 // G*Coef[2] + B*Coef[1] + R*Coef[0]
                               paddd     mm1,mm2 // G*Coef[2] + B*Coef[1] + R*Coef[0]
                                 paddd     mm0,mm3 // Add offset.
                               paddd     mm1,mm3 // Add offset.
                                 psrad     mm0,15  // Scale result to byte value.
                               psrad     mm1,15  // Scale result to byte value.
                                 packuswb  mm0,mm7
                               packuswb  mm1,mm7
                                 punpcklbw mm0,mm1
                                 pslld     mm0,16  // Shift to output byte position.
                                 movq      mm5,mm0 // Move to mm5 output reg.
                               dec       ecx     // dec index to coordinate buffers.

                                 // Repition of above.
                                 movq      mm0,[esi+ecx*8]
                               movq      mm1,mm0
                                 punpcklbw mm0,mm7
                               punpckhbw mm1,mm7
                                 pmaddwd   mm0,mm6
                               pmaddwd   mm1,mm6
                                 movq      mm4,mm0
                               movq      mm2,mm1
                                 punpckhdq mm4,mm7
                               punpckhdq mm2,mm7
                                 paddd     mm0,mm4
                               paddd     mm1,mm2
                                 paddd     mm0,mm3
                               paddd     mm1,mm3
                                 psrad     mm0,15
                               psrad     mm1,15
                                 packuswb  mm0,mm7
                               packuswb  mm1,mm7
                                 punpcklbw mm0,mm1
                                 por       mm5,mm0
                                 movd      [edi+edx*4],mm5 // output 4 8 bit pixels.
                                 {$ELSE}
                                 {$ENDIF}

                                 dec       edx
                                 dec       ecx        // dec index to coordinate buffers.
                                 jns       @LoopQWORD

                                 // Check for remaining (1-3) pixels in line
                                 mov       ecx,Count
                                 mov       ebx,ecx
                                 shr       ecx,2
                                 shl       ecx,2
                                 sub       ebx,ecx
                                 jz        @EndOfData
                                 mov       ecx,Count
                                 dec       ecx

                                 @LoopLast:
                                 {$IFNDEF DCB3_5}
                                 movd      mm0,[esi+ecx*4]
                                 punpcklbw mm0,mm7 // unpack lower 4 bytes to 4 words

                                 // pixel in lower dword of [esi+ecx*8]
                                 pmaddwd   mm0,mm6 // 0 + G*Coef[2], B*Coef[1] + R*Coef[0]
                                 movq      mm4,mm0 // Move mm0 upper dword
                                 punpckhdq mm4,mm7 // to lower mm4 dword
                                 paddd     mm0,mm4 // G*Coef[2] + B*Coef[1] + R*Coef[0]
                                 paddd     mm0,mm3 // Add offset.
                                 psrad     mm0,15  // Scale result to byte value.
                                 packuswb  mm0,mm7
                                 movd      eax,mm0
                                 {$ELSE}
                                 {$ENDIF}
                                 mov       [edi+ecx],al
                                 dec       ecx
                                 dec       ebx
                                 jnz       @LoopLast

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
                          {$ENDIF}
                               i := 0;
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do begin
                                  Value := Round(Vector[0] * pS^[i+2] +
                                                 Vector[1] * pS^[i+1] +
                                                 Vector[2] * pS^[i] + Vector[3]);
                                  if (Value < 0)
                                  then pR^[x] := 0
                                  else if (Value > 255)
                                       then pR^[x] := 255
                                       else pR^[x] := Value;
                                  inc(i,4);
                               end;
                          {$IFNDEF DCB3_5}
                          end;
                          {$ENDIF}
                       end;
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;
       except
         On E:Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_MISSOURCEIMAGE;
  Result := FResImage;
end; // TmcmImageColor.GetVectorChannel.


function TmcmImageColor.GetMatrixChannel(Vector : array of double) : TmcmImage;
// This procedure multiplies three grey scale images with the matrix, to form
// an RGB image.
var x, y, i    : longint;
    c1, c2, c3 : double;
    pS1, pS2   : PVectorB;
    pS3, pR    : PVectorB;
    Value      : longint;
    {$IFNDEF DCB3_5}
    Count      : integer;
    cr         : array[0..3] of smallint;
    cg         : array[0..3] of smallint;
    cb         : array[0..3] of smallint;
    co         : array[0..3] of smallint;
   {$ENDIF}
begin
  Result := Nil;
  FError := EC_OK;

  // Make sure we have a valid Result Image.
  if (CheckSource(0, [IF_GREY8]) and
      CheckSource(1, [IF_GREY8]) and
      CheckSource(2, [IF_GREY8]))
  then begin
       if (FSrcImage[0].ImageFormat <> IF_GREY8)
       then begin
            FError := EC_BADCOLORFORMAT;
            Exit;
       end;

       if (FSrcImage[0].Width <> FSrcImage[1].Width) or (FSrcImage[0].Width <> FSrcImage[2].Width) or
          (FSrcImage[0].Height <> FSrcImage[1].Height) or (FSrcImage[0].Height <> FSrcImage[2].Height) or
          (FSrcImage[0].ImageFormat <> FSrcImage[1].ImageFormat) or (FSrcImage[0].ImageFormat <> FSrcImage[2].ImageFormat)
       then begin
            FError := EC_NOMATCHFORMAT;
            Exit;
       end;

       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_RGB24;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            // FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            // Force to 24 bit RGB.
            if (FResImage.ImageFormat <> IF_RGB24)
            then FResImage.ImageFormat := IF_RGB24;

            if (FSrcImage[0].Empty or
                FSrcImage[1].Empty or
                FSrcImage[2].Empty or
                FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       {$IFNDEF DCB3_5}
       if FMMX
       then begin
            Count := FSrcImage[0].Width;
            for i := 0 to 3
            do begin
               cr[i] := Round(16384.0 * Vector[i]);
               cg[i] := Round(16384.0 * Vector[i+4]);
               cb[i] := Round(16384.0 * Vector[i+8]);
            end;
            for i := 0 to 2
            do co[i] := Round(Vector[i*4+3]);
            co[3] := 0;
       end;
       {$ELSE}
       {$ENDIF}

       case FSrcImage[0].ImageFormat of
       IF_GREY8  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS1 := FSrcImage[0].ScanLine[y];
                        pS2 := FSrcImage[1].ScanLine[y];
                        pS3 := FSrcImage[2].ScanLine[y];
                        pR := FResImage.ScanLine[y];

                        {$IFNDEF DCB3_5}
                        if FMMX
                        then begin
                             asm
                               // Save registers to stack
                               push      ebx
                               push      edi
                               push      esi

                               // data points to process.
                               mov       ecx,Count
                               test      ecx,ecx
                               jz        @EndOfData   // Check that data count > zero bytes.

                               shr       ecx,2
                               dec       ecx
                               test      ecx,ecx
                               jz        @EndOfLine   // Check that data count > zero bytes.


                               // Set-up initial pointers to image line and coefficients.
                               mov       edi,pR
                               mov       esi,pS1
                               mov       ebx,pS2
                               mov       edx,pS3

                               xor       eax,eax

                               {$IFNDEF DCB3_5}
                               pxor      mm7,mm7
                               {$ELSE}
                               {$ENDIF}

                               // process pixels
                               @LoopQWORD:
                               {$IFNDEF DCB3_5}
                               movd      mm0,[esi] // Read X3 X2 X1 X0
                               punpcklbw mm0,mm7  // convert bytes to words
                               movd      mm1,[ebx] // Read Y3 Y2 Y1 Y0
                               punpcklbw mm1,mm7  // convert bytes to words
                               movd      mm2,[edx] // Read Z3 Z2 Z1 Z0
                               punpcklbw mm2,mm7  // convert bytes to words

                               add       esi,4
                               add       ebx,4

                               // Perform a 4x4 matrix transposition,
                               // from:
                               //   x3 x2 x1 x0
                               //   y3 y2 y1 y0
                               //   z3 z2 z1 z0
                               //   o3 o2 o1 o0
                               // to:
                               //  o0 z0 y0 x0
                               //  o1 z1 y1 x1
                               //  o2 z2 y2 x2
                               //  o3 z3 y3 x3
                               movq      mm4,mm0
                               pxor      mm3,mm3

                               punpckhwd mm4,mm1
                               punpcklwd mm0,mm1

                               movq      mm5,mm2
                               punpckhwd mm5,mm3
                               punpcklwd mm2,mm3

                               movq      mm1,mm0
                               punpckldq mm0,mm2  // mm0 = c0z0y0x0
                               punpckhdq mm1,mm2  // mm1 = c1z1y1x1

                               movq      mm2,mm4
                               movq      mm3,mm4
                               punpckldq mm2,mm5  // mm2 = c2z2y2x2
                               punpckhdq mm3,mm5  // mm3 = c3z3y3x3

                               // Add Offset
                               movq      mm4,co   // Add offset
                               add       edx,4
                               paddsw    mm0,mm4
                               paddsw    mm1,mm4

                               // Compute color conversion.
                               movq      mm5,mm0
                               movq      mm6,mm0
                               pmaddwd   mm0,cb   // mm0 = z0cb2, y0cb1+x0cb0
                               pmaddwd   mm5,cg   // mm5 = z0cg2, y0cg1+x0cg0
                             paddsw    mm2,mm4
                             paddsw    mm3,mm4

                               // Sum mm0 & mm5's upper and lower dwords.
                               movq      mm4,mm0
                               punpckldq mm0,mm5  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                               punpckhdq mm4,mm5  // mm4 = z0cg2, z0cb2
                               paddd     mm0,mm4  // mm0 = 00 G0 00 B0
                             pmaddwd   mm6,cr   // mm6 = z0cr2, y0cr1+x0cr0
                               psrad     mm0,14   // Scale dwords by 16384
                             movq      mm5,mm1  //
                             pmaddwd   mm1,cb   // mm1 = z1cb2, y1cb1+x1cb1
                               packssdw  mm0,mm7  // Pack dwords to words, mm0 = 00 00 G0 B0

                               // Sum mm6 & mm1's upper and lower dwords.
                               movq      mm4,mm6
                               punpckldq mm6,mm1  // mm6 = y1cb1+x1cb1, y0cr1+x0cr0
                               punpckhdq mm4,mm1  // mm4 = z1cb2, z0cr2
                               paddd     mm6,mm4  // mm6 = 00 B1 00 R0
                               psrad     mm6,14   // Scale dwords by 16384
                               packssdw  mm6,mm7  // Pack dwords to words, mm6 = 00 00 B1 R0
                               psllq     mm6,32   // mm6 = B1 R0 00 00

                               por       mm0,mm6  // mm0 = B1 R0 G0 B0
                             movq      mm6,mm5
                             pmaddwd   mm5,cg   // mm5 = z1cg2, y1cg1+x1cg0
                               packuswb  mm0,mm7  // Pack words to bytes

                               pmaddwd   mm6,cr   // mm6 = z1cr2, y1cr1+x1cr0

                               movd      [edi+eax*4],mm0 // write B1 R0 G0 B0
                               inc       eax

                              //---

                               // Sum mm5 & mm6's upper and lower dwords.
                               movq      mm1,mm5
                               punpckldq mm5,mm6  // mm5 = y1cr1+x1cr0, y1cg1+x1cg0
                               punpckhdq mm1,mm6  // mm1 = z1cr2, z1cg2
                             movq      mm6,mm2
                               paddd     mm1,mm5  // mm1 = 00 R1 00 G1
                             movq      mm5,mm2
                               psrad     mm1,14   // Scale dwords by 16384
                             pmaddwd   mm2,cb   // mm2 = z2cb2, y2cb1+x2cb1
                               packssdw  mm1,mm7  // mm1 = 00 00 R1 G1

                               pmaddwd   mm5,cg   // mm5 = z2cg2, y2cg1+x2cg0

                               // Sum mm2 & mm5's upper and lower dwords.
                               movq      mm4,mm2
                               punpckldq mm2,mm5  // mm2 = y2cg1+x2cg0, y2cb1+x2cb1
                               punpckhdq mm4,mm5  // mm4 = z2cg2, z2cb2
                             pmaddwd   mm6,cr   // mm6 = z2cr2, y2cr1+x2cr0
                               paddd     mm2,mm4  // mm2 = 00 G2 00 B2
                             movq      mm5,mm3
                               psrad     mm2,14   // Scale dwords by 16384
                             pmaddwd   mm3,cb   // mm3 = z3cb2, y3cb1+x3cb1
                               packssdw  mm2,mm7  // mm2 = 00 00 G2 B2
                             // Sum mm6 & mm3's upper and lower dwords.
                             movq      mm0,mm6
                               psllq     mm2,32   // mm2 = G2 B2 00 00
                             punpckldq mm6,mm3  // mm6 = y3cb1+x3cb1, y2cr1+x2cr0
                               por       mm1,mm2  // mm1 = G2 B2 R1 G1
                             punpckhdq mm0,mm3  // mm0 = z3cb2, z2cr2
                               packuswb  mm1,mm7  // Pack words to bytes
                               movd      [edi+eax*4],mm1 // Write G2 B2 R1 G1
                               inc       eax

                             //---

                               paddd     mm0,mm6  // mm0 = 00 B3 00 R2
                             movq      mm6,mm5  // copy mm5 to mm6
                               psrad     mm0,14   // Scale dwords by 16384
                             pmaddwd   mm5,cg   // mm5 = z3cg2, y3cg1+x3cg0
                               packssdw  mm0,mm7  // 00 00 B3 R2
                             pmaddwd   mm6,cr   // mm6 = z3cr2, y3cr1+x3cr0

                               // Sum mm5 & mm6's upper and lower dwords.
                               movq      mm4,mm5
                               punpckldq mm5,mm6  // y3cr1+x3cr0, y3cg1+x3cg0
                               punpckhdq mm4,mm6  // z3cr2, z3cg2
                               paddd     mm5,mm4  // mm5 = 00 R3 00 G3
                               psrad     mm5,14   // Scale dwords by 16384
                               packssdw  mm5,mm7  // mm5 = 00 00 R3 G3
                               psllq     mm5,32   // mm5 = R3 G3 00 00

                               por       mm0,mm5  // mm0 = R3 G3 B3 R2

                               packuswb  mm0,mm7  // Pack words to bytes
                               movd      [edi+eax*4],mm0 // Write R3 G3 B3 R2
                               inc       eax
                               {$ELSE}
                               {$ENDIF}

                               dec       ecx
                               jns       @LoopQWORD
                               @EndOfLine:

                               // Calc remaining byte in line.
                               push      eax
                               mov       ecx,Count
                               mov       eax,ecx
                               shr       eax,2
                               shl       eax,2
                               sub       ecx,eax
                               pop       eax
                               test      ecx,ecx
                               jz        @EndOfData   // Check that data count > zero bytes.

                               shl       eax,2
                               add       edi,eax

                               @LoopLast:
                               xor       eax,eax
                               mov       al,[edx]
                               shl       eax,16
                               mov       al,[esi]
                               mov       ah,[ebx]
                               {$IFNDEF DCB3_5}
                               movd      mm0,eax
                               punpcklbw mm0,mm7  // convert bytes to words

                               paddsw    mm0,co   // Add offset
                               movq      mm5,mm0
                               movq      mm6,mm0
                               pmaddwd   mm0,cb   // mm0 = z0cb2, y0cb1+x0cb0
                               pmaddwd   mm5,cg   // mm5 = z0cg2, y0cg1+x0cg0
                               pmaddwd   mm6,cr   // mm6 = z0cr2, y0cr1+x0cr0

                               movq      mm4,mm0
                               punpckldq mm0,mm5  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                               punpckhdq mm4,mm5  // mm4 = z0cg2, z0cb2
                               paddd     mm0,mm4  // mm0 = 00 G0 00 B0

                               movq      mm4,mm6
                               punpckhdq mm4,mm7  // mm0 = y0cg1+x0cg0, y0cb1+x0cb0
                               paddd     mm6,mm4  // mm0 = 00 00 00 R0

                               psrad     mm0,14   // Scale dwords by 16384
                               psrad     mm6,14   // Scale dwords by 16384

                               packssdw  mm0,mm7  // Pack dwords to words,
                               packssdw  mm6,mm7  // Pack dwords to words,
                               psllq     mm6,32
                               por       mm0,mm6
                               packuswb  mm0,mm7  // Pack words to bytes

                               movd      eax,mm0
                               {$ELSE}
                               {$ENDIF}
                               mov       [edi],ax
                               add       edi,2
                               shr       eax,16
                               mov       [edi],al
                               inc       edi

                               inc       esi
                               inc       ebx
                               inc       edx
                               dec       ecx
                               jnz       @LoopLast

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
                        {$ENDIF}
                             i := 0;
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                c1 := pS1^[x] + Vector[3];
                                c2 := pS2^[x] + Vector[7];
                                c3 := pS3^[x] + Vector[11];
                                // Blue
                                Value := Round(Vector[8]  * c1 +
                                               Vector[9]  * c2 +
                                               Vector[10] * c3); // + Vector[11]);
                                if (Value < 0)
                                then pR^[i] := 0
                                else if (Value > 255)
                                     then pR^[i] := 255
                                     else pR^[i] := Value;
                                inc(i);

                                // Green
                                Value := Round(Vector[4] * c1 +
                                               Vector[5] * c2 +
                                               Vector[6] * c3); // + Vector[7]);
                                if (Value < 0)
                                then pR^[i] := 0
                                else if (Value > 255)
                                     then pR^[i] := 255
                                     else pR^[i] := Value;
                                inc(i);

                                // Red
                                Value := Round(Vector[0] * c1 +
                                               Vector[1] * c2 +
                                               Vector[2] * c3); // + Vector[3]);
                                if (Value < 0)
                                then pR^[i] := 0
                                else if (Value > 255)
                                     then pR^[i] := 255
                                     else pR^[i] := Value;
                                inc(i);
                             end;
                        {$IFNDEF DCB3_5}
                        end;
                        {$ENDIF}
                     end;
                     Result := FResImage;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end
  else FError := EC_MISSOURCEIMAGE;
end; // TmcmImageColor.GetMatrixChannel.


function TmcmImageColor.GetAlphaChannel : TmcmImage;
begin
  Result := GetAChannel(3);
end; // TmcmImageColor.GetAlphaChannel.


function TmcmImageColor.GetBlueChannel : TmcmImage;
begin
  Result := GetAChannel(0);
end; // TmcmImageColor.GetBlueChannel.


function TmcmImageColor.GetGreenChannel : TmcmImage;
begin
    Result := GetAChannel(1);
end; // TmcmImageColor.GetGreenChannel.


function TmcmImageColor.GetRedChannel : TmcmImage;
begin
    Result := GetAChannel(2);
end; // TmcmImageColor.GetRedChannel.


function TmcmImageColor.SetAlphaChannel : TmcmImage;
begin
  SetAChannel(3);
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.SetAlphaChannel.


function TmcmImageColor.SetBlueChannel : TmcmImage;
begin
  SetAChannel(0);
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.SetBlueChannel.


function TmcmImageColor.SetGreenChannel : TmcmImage;
begin
  SetAChannel(1);
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.SetGreenChannel.


function TmcmImageColor.SetRedChannel : TmcmImage;
begin
  SetAChannel(2);
  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageColor.SetRedChannel.


function TmcmImageColor.GetIntensity : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    int     : cardinal;
begin
  Result := Nil;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_BW     : Result := ConvertTo(IF_GREY8, False);
       IF_GREY8,
       IF_PAL8   : begin
                     with FSrcImage[0].DibInfo^
                     do begin
                        for y := 0 to (FSrcImage[0].Height - 1)
                        do begin
                           pS := FSrcImage[0].ScanLine[y];
                           pR := FResImage.ScanLine[y];
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              pR^[x] := (bmiColors[pS^[x]].rgbBlue +
                                         bmiColors[pS^[x]].rgbGreen +
                                         bmiColors[pS^[x]].rgbRed) div 3;
                           end;
                        end;
                     end;
                     Result := FResImage;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     if FMMX
                     then Result := GetVectorChannel([0.333333,0.333333,0.33333,0])
                     else begin
                          for y := 0 to (FSrcImage[0].Height - 1)
                          do begin
                             pS := FSrcImage[0].ScanLine[y];
                             pR := FResImage.ScanLine[y];
                             i := 0;
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                int := pS^[i];
                                inc(i);
                                inc(int, pS^[i]);
                                inc(i);
                                inc(int, pS^[i]);
                                inc(i);
                                pR^[x] := int div 3;
                             end;
                          end;
                          Result := FResImage;
                     end;
                   end;
       IF_RGBA32 : begin
                     if FMMX
                     then Result := GetVectorChannel([0.333333,0.333333,0.33333,0])
                     else begin
                          for y := 0 to (FSrcImage[0].Height - 1)
                          do begin
                             pS := FSrcImage[0].ScanLine[y];
                             pR := FResImage.ScanLine[y];
                             i := 0;
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                int := pS^[i];
                                inc(i);
                                inc(int, pS^[i]);
                                inc(i);
                                inc(int, pS^[i]);
                                inc(i,2);
                                pR^[x] := int div 3;
                             end;
                          end;
                          Result := FResImage;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
end; // TmcmImageColor.GetIntensity.


//------------------------------------------------------------------------------
// CIE Rec 709, D65 white point

function TmcmImageColor.GetCIEXD65 : TmcmImage;
begin
  Result := GetVectorChannel([0.412453,0.357580,0.180423,0]);
end; // TmcmImageColor.GetCIEXD65.


function TmcmImageColor.GetCIEYD65 : TmcmImage;
begin
  Result := GetVectorChannel([0.212671,0.715160,0.072169,0]);
end; // TmcmImageColor.GetCIEYD65.


function TmcmImageColor.GetCIEZD65 : TmcmImage;
begin
  Result := GetVectorChannel([0.019334,0.119193,0.950227,0]);
end; // TmcmImageColor.GetCIEZD65.


//------------------------------------------------------------------------------
// HSV

function TmcmImageColor.GetHueChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
    MinVal  : word;
    DifVal  : word;
    Hue     : double;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              MinVal := MaxVal;

                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;

                              if (MinVal > bmiColors[pS^[x]].rgbGreen)
                              then MinVal := bmiColors[pS^[x]].rgbGreen;
                              if (MinVal > bmiColors[pS^[x]].rgbBlue)
                              then MinVal := bmiColors[pS^[x]].rgbBlue;

                              if (MaxVal > 0)
                              then begin
                                   DifVal := (MaxVal - MinVal);
                                   if (DifVal <> 0)
                                   then begin
                                        if (MaxVal = bmiColors[pS^[x]].rgbRed)
                                        then Hue := (bmiColors[pS^[x]].rgbGreen - bmiColors[pS^[x]].rgbBlue) / DifVal
                                        else begin
                                             if (MaxVal = bmiColors[pS^[x]].rgbGreen)
                                             then Hue := 2 + (bmiColors[pS^[x]].rgbBlue - bmiColors[pS^[x]].rgbRed) / DifVal
                                             else Hue := 4 + (bmiColors[pS^[x]].rgbRed - bmiColors[pS^[x]].rgbGreen) / DifVal;
                                        end;
                                        Hue := Hue * 42.60; // 42.6667;
                                        if (Hue < 0.0)
                                        then Hue := Hue + 255.0;
                                        pR^[x] := Round(Hue);
                                   end
                                   else pR^[x] := 0;
                              end
                              else pR^[x] := 0;

                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           MinVal := MaxVal;

                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];

                           if (MinVal > pS^[i+1])
                           then MinVal := pS^[i+1];
                           if (MinVal > pS^[i+2])
                           then MinVal := pS^[i+2];

                           if (MaxVal > 0)
                           then begin
                                DifVal := (MaxVal - MinVal);
                                if (DifVal <> 0)
                                then begin
                                     if (MaxVal = pS^[i+2])
                                     then Hue := (pS^[i+1] - pS^[i]) / DifVal
                                     else begin
                                          if (MaxVal = pS^[i+1])
                                          then Hue := 2.0 + (pS^[i] - pS^[i+2]) / DifVal
                                          else Hue := 4.0 + (pS^[i+2] - pS^[i+1]) / DifVal;
                                     end;
                                     Hue := Hue * 42.60; // 42.6667;
                                     if (Hue < 0.0)
                                     then Hue := Hue + 255.0;
                                     pR^[x] := Round(Hue);
                                end
                                else pR^[x] := 0;
                           end
                           else pR^[x] := 0;

                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           MinVal := MaxVal;

                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];

                           if (MinVal > pS^[i+1])
                           then MinVal := pS^[i+1];
                           if (MinVal > pS^[i+2])
                           then MinVal := pS^[i+2];

                           if (MaxVal > 0)
                           then begin
                                DifVal := (MaxVal - MinVal);
                                if (DifVal <> 0)
                                then begin
                                     if (MaxVal = pS^[i+2])
                                     then Hue := (pS^[i+1] - pS^[i]) / DifVal
                                     else begin
                                          if (MaxVal = pS^[i+1])
                                          then Hue := 2 + (pS^[i] - pS^[i+2]) / DifVal
                                          else Hue := 4 + (pS^[i+2] - pS^[i+1]) / DifVal;
                                     end;
                                     Hue := Hue * 42.60; // 42.6667;
                                     if (Hue < 0.0)
                                     then Hue := Hue + 255.0;
                                     pR^[x] := Round(Hue);
                                end
                                else pR^[x] := 0;
                           end
                           else pR^[x] := 0;
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetHueChannel.


function TmcmImageColor.GetSaturationChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
    MinVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              MinVal := MaxVal;

                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;

                              if (MinVal > bmiColors[pS^[x]].rgbGreen)
                              then MinVal := bmiColors[pS^[x]].rgbGreen;
                              if (MinVal > bmiColors[pS^[x]].rgbBlue)
                              then MinVal := bmiColors[pS^[x]].rgbBlue;

                              if (MaxVal > 0)
                              then pR^[x] := 255 * (MaxVal - MinVal) div MaxVal
                              else pR^[x] := 0;
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           MinVal := MaxVal;

                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];

                           if (MinVal > pS^[i+1])
                           then MinVal := pS^[i+1];
                           if (MinVal > pS^[i+2])
                           then MinVal := pS^[i+2];

                           if (MaxVal > 0)
                           then pR^[x] := 255 * (MaxVal - MinVal) div MaxVal
                           else pR^[x] := 0;
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           MinVal := MaxVal;

                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];

                           if (MinVal > pS^[i+1])
                           then MinVal := pS^[i+1];
                           if (MinVal > pS^[i+2])
                           then MinVal := pS^[i+2];

                           if (MaxVal > 0)
                           then pR^[x] := 255 * (MaxVal - MinVal) div MaxVal
                           else pR^[x] := 0;
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetSaturationChannel.


function TmcmImageColor.GetValueChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;
                              pR^[x] := MaxVal;
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := MaxVal;
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := MaxVal;
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetValueChannel.


//------------------------------------------------------------------------------
// YCbCr (Recommendation 601-1)

function TmcmImageColor.GetCbChannel : TmcmImage;
begin
  Result := GetVectorChannel([-0.1687,-0.3312,0.5000,127.50]);
end; // TmcmImageColor.GetCbChannel.


function TmcmImageColor.GetCrChannel : TmcmImage;
begin
  Result := GetVectorChannel([0.5000,-0.4183,-0.0816,127.50]);
end; // TmcmImageColor.GetCrChannel.


function TmcmImageColor.GetLuminanceChannel : TmcmImage;
begin
  Result := GetVectorChannel([0.2989,0.5867,0.1144,0.0]);
end; // TmcmImageColor.GetLuminanceChannel.


//------------------------------------------------------------------------------
// YIQ NTSC, Y = GetLuminanceChannel from YCbCr.

function TmcmImageColor.GetNTSCIChannel : TmcmImage;
begin
  Result := GetVectorChannel([0.596,-0.274,-0.322,0.0]);
end; // TmcmImageColor.GetNTSCIChannel.


function TmcmImageColor.GetNTSCQChannel : TmcmImage;
begin
  Result := GetVectorChannel([0.212,-0.523,0.311,0.0]);
end; // TmcmImageColor.GetNTSCQChannel.


//------------------------------------------------------------------------------
// YUV PAL, Y = GetLuminanceChannel from YCbCr.

function TmcmImageColor.GetPALUChannel : TmcmImage;
begin
  Result := GetVectorChannel([-0.147,-0.289,0.437,0.0]);
end; // TmcmImageColor.GetPALUChannel.


function TmcmImageColor.GetPALVChannel : TmcmImage;
begin
  Result := GetVectorChannel([0.615,-0.515,-0.100,0.0]);
end; // TmcmImageColor.GetPALVChannel.


//------------------------------------------------------------------------------
// CMYK
//
// Black   = (2^Sample Precision - 1) - Max(R, G, B)
// Cyan    = (2^Sample Precision - 1) - R - K
// Magenta = (2^Sample Precision - 1) - G - K
// Yellow  = (2^Sample Precision - 1) - B - K

function TmcmImageColor.GetBlackChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;
                              pR^[x] := 255 - MaxVal;
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - MaxVal;
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - MaxVal;
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetBlackChannel.


function TmcmImageColor.GetCyanChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;
                              pR^[x] := 255 - bmiColors[pS^[x]].rgbRed - (255 - MaxVal);
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i+2] - (255 - MaxVal);
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i+2] - (255 - MaxVal);
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetCyanChannel.


function TmcmImageColor.GetMagentaChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;
                              pR^[x] := 255 - bmiColors[pS^[x]].rgbBlue - (255 - MaxVal);
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i] - (255 - MaxVal);
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i] - (255 - MaxVal);
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetMagentaChannel.


function TmcmImageColor.GetYellowChannel : TmcmImage;
var x, y, i : longint;
    pS, pR  : PVectorB;
    MaxVal  : word;
begin
  Result := Nil;
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY8,
       IF_PAL8   : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        with FSrcImage[0].DibInfo^
                        do begin
                           for x := 0 to (FSrcImage[0].Width - 1)
                           do begin
                              MaxVal := bmiColors[pS^[x]].rgbRed;
                              if (MaxVal < bmiColors[pS^[x]].rgbGreen)
                              then MaxVal := bmiColors[pS^[x]].rgbGreen;
                              if (MaxVal < bmiColors[pS^[x]].rgbBlue)
                              then MaxVal := bmiColors[pS^[x]].rgbBlue;
                              pR^[x] := 255 - bmiColors[pS^[x]].rgbGreen - (255 - MaxVal);
                           end;
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i+1] - (255 - MaxVal);
                           inc(i,3);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        i := 0;
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           MaxVal := pS^[i];
                           if (MaxVal < pS^[i+1])
                           then MaxVal := pS^[i+1];
                           if (MaxVal < pS^[i+2])
                           then MaxVal := pS^[i+2];
                           pR^[x] := 255 - pS^[i+1] - (255 - MaxVal);
                           inc(i,4);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
  Result := FResImage;
end; // TmcmImageColor.GetYellowChannel.


//------------------------------------------------------------------------------
// Histogram
//------------------------------------------------------------------------------

procedure TmcmImageColor.LookupTable(RedLookup    : PVectorB;
                                     GreenLookup  : PVectorB;
                                     BlueLookup   : PVectorB);
var x, y, i : longint;
    pS, pR  : PVectorB;
begin
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       case FSrcImage[0].ImageFormat of
       IF_GREY4,
       IF_PAL4   : begin
                     // Copy image data.
                     if (FSrcImage[0] <> FResImage)
                     then CopyMemory(FResImage.pDib, FSrcImage[0].pDib, TKernelImage(FSrcImage[0]).DibInfo.bmiHeader.biSizeImage);

                     // Modify palette.
                     FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                     FSrcImage[0].GetPaletteEntries(FLogPalette);
                     for i := 0 to 15
                     do begin
                        FLogPalette.palPalEntry[i].peRed   := RedLookup^[FLogPalette.palPalEntry[i].peRed];
                        FLogPalette.palPalEntry[i].peGreen := GreenLookup^[FLogPalette.palPalEntry[i].peGreen];
                        FLogPalette.palPalEntry[i].peBlue  := BlueLookup^[FLogPalette.palPalEntry[i].peBlue];
                     end;
                     FResImage.SetPaletteEntries(FLogPalette);
                     FreeMem(FLogPalette);
                   end;
       IF_PAL8   : begin
                     // Copy image data.
                     if (FSrcImage[0] <> FResImage)
                     then CopyMemory(FResImage.pDib, FSrcImage[0].pDib, TKernelImage(FSrcImage[0]).DibInfo.bmiHeader.biSizeImage);

                     // Modify palette.
                     FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                     FSrcImage[0].GetPaletteEntries(FLogPalette);
                     for i := 0 to 255
                     do begin
                        FLogPalette.palPalEntry[i].peRed   := RedLookup^[FLogPalette.palPalEntry[i].peRed];
                        FLogPalette.palPalEntry[i].peGreen := GreenLookup^[FLogPalette.palPalEntry[i].peGreen];
                        FLogPalette.palPalEntry[i].peBlue  := BlueLookup^[FLogPalette.palPalEntry[i].peBlue];
                     end;
                     FResImage.SetPaletteEntries(FLogPalette);
                     FreeMem(FLogPalette);
                   end;
       IF_GREY8  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           pR^[x] := RedLookup^[pS^[x]];
                        end;
                     end;
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           PVectorRGB(pR)^[x].rgbtRed := RedLookup^[PVectorRGB(pS)^[x].rgbtRed];
                           PVectorRGB(pR)^[x].rgbtGreen := GreenLookup^[PVectorRGB(pS)^[x].rgbtGreen];
                           PVectorRGB(pR)^[x].rgbtBlue := BlueLookup^[PVectorRGB(pS)^[x].rgbtBlue];
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        pR := FResImage.ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           PVectorRGBA(pR)^[x].rgbRed := RedLookup^[PVectorRGBA(pS)^[x].rgbRed];
                           PVectorRGBA(pR)^[x].rgbGreen := GreenLookup^[PVectorRGBA(pS)^[x].rgbGreen];
                           PVectorRGBA(pR)^[x].rgbBlue := BlueLookup^[PVectorRGBA(pS)^[x].rgbBlue];
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;
  end;
end; // TmcmImageColor.LookupTable.


procedure TmcmImageColor.GetHistogram;
var x, y, z : cardinal;
    i       : longint;
    v       : byte;
    pS      : PVectorB;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0]) //and Assigned(FResImage)
  then begin
       if FSrcImage[0].Empty //or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       if Not(Assigned(FRedHist))
       then FRedHist := AllocMem(256 * SizeOf(cardinal))
       else FillMemory(FRedHist, 256 * SizeOf(cardinal), 0);
       if Not(Assigned(FGreenHist))
       then FGreenHist := AllocMem(256 * SizeOf(cardinal))
       else FillMemory(FGreenHist, 256 * SizeOf(cardinal), 0);
       if Not(Assigned(FBlueHist))
       then FBlueHist := AllocMem(256 * SizeOf(cardinal))
       else FillMemory(FBlueHist, 256 * SizeOf(cardinal), 0);
       if Not(Assigned(FIntHist))
       then FIntHist := AllocMem(256 * SizeOf(cardinal))
       else FillMemory(FIntHist, 256 * SizeOf(cardinal), 0);

       if (FRedHist = Nil) or (FGreenHist = Nil) or
          (FBlueHist = Nil) or (FIntHist = Nil)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       case FSrcImage[0].ImageFormat of
       // IF_BW     : ;
       IF_GREY4,
       IF_PAL4   : begin
                     FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                     FSrcImage[0].GetPaletteEntries(FLogPalette);
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           z := x shr 1;
                           if ((x and 1) = 0)
                           then v := (pS^[z] and $F0) shr 4
                           else v := (pS^[z] and $0F);

                           inc(FBlueHist^[FLogPalette.palPalEntry[v].peBlue]);
                           inc(FGreenHist^[FLogPalette.palPalEntry[v].peGreen]);
                           inc(FRedHist^[FLogPalette.palPalEntry[v].peRed]);
                        end;
                     end;
                     FreeMem(FLogPalette);
                   end;
       IF_GREY8,
       IF_PAL8   : begin
                     FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                     FSrcImage[0].GetPaletteEntries(FLogPalette);
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           inc(FBlueHist^[FLogPalette.palPalEntry[pS^[x]].peBlue]);
                           inc(FGreenHist^[FLogPalette.palPalEntry[pS^[x]].peGreen]);
                           inc(FRedHist^[FLogPalette.palPalEntry[pS^[x]].peRed]);
                        end;
                     end;
                     FreeMem(FLogPalette);
                   end;
       //IF_RGB15
       //IF_RGB16
       IF_RGB24  : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           inc(FBlueHist^[PVectorRGB(pS)^[x].rgbtBlue]);
                           inc(FGreenHist^[PVectorRGB(pS)^[x].rgbtGreen]);
                           inc(FRedHist^[PVectorRGB(pS)^[x].rgbtRed]);
                        end;
                     end;
                   end;
       IF_RGBA32 : begin
                     for y := 0 to (FSrcImage[0].Height - 1)
                     do begin
                        pS := FSrcImage[0].ScanLine[y];
                        for x := 0 to (FSrcImage[0].Width - 1)
                        do begin
                           inc(FBlueHist^[PVectorRGBA(pS)^[x].rgbBlue]);
                           inc(FGreenHist^[PVectorRGBA(pS)^[x].rgbGreen]);
                           inc(FRedHist^[PVectorRGBA(pS)^[x].rgbRed]);
                        end;
                     end;
                   end;
       // Error - Image color format cannot be applied in this algorithme.
       else FError := EC_BADCOLORFORMAT;
       end;

       for i := 0 to 255
       do FIntHist^[i] := (FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i]) div 3;
       {
       for i := 0 to 255
       do FIntHist^[i] := Round(0.2989 * FRedHist^[i] + 0.5867 * FGreenHist^[i] + 0.1144 * FBlueHist^[i]);
       }
  end;
end; // TmcmImageColor.GetHistogram.


procedure TmcmImageColor.SetHistogram(RedSlope,   RedOffset   : double;
                                      GreenSlope, GreenOffset : double;
                                      BlueSlope,  BlueOffset  : double);
var i, Value    : longint;
    RedLookup   : PVectorB;
    GreenLookup : PVectorB;
    BlueLookup  : PVectorB;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       GetMem(RedLookup, 256 * SizeOf(byte));
       GetMem(GreenLookup, 256 * SizeOf(byte));
       GetMem(BlueLookup, 256 * SizeOf(byte));

       try
         for i := 0 to 255
         do begin
            Value := Round(i * RedSlope + RedOffset);
            if (Value < 0)
            then RedLookup^[i] := 0
            else if (Value > 255)
                 then RedLookup^[i] := 255
                 else RedLookup^[i] := Value;

            Value := Round(i * GreenSlope + GreenOffset);
            if (Value < 0)
            then GreenLookup^[i] := 0
            else if (Value > 255)
                 then GreenLookup^[i] := 255
                 else GreenLookup^[i] := Value;

            Value := Round(i * BlueSlope + BlueOffset);
            if (Value < 0)
            then BlueLookup^[i] := 0
            else if (Value > 255)
                 then BlueLookup^[i] := 255
                 else BlueLookup^[i] := Value;
         end;
         LookupTable(RedLookup, GreenLookup, BlueLookup);
       finally
         FreeMem(RedLookup);
         FreeMem(GreenLookup);
         FreeMem(BlueLookup);
       end;
  end;
end; // TmcmImageColor.SetHistogram.


function TmcmImageColor.BrightContrast(Offset, Width : integer) : TmcmImage;
var x        : integer;
    Value    : longint;
    dOffset  : double;
    Slope    : double;
    Lookup   : PVectorB;
    NoColors : integer;
    MaxVal   : integer;
    MinVal   : integer;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := FSrcImage[0].ImageFormat;
            FResImage.Palette     := FSrcImage[0].Palette;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            GetHistogram;
            if (FSrcImage[0].BitCount <= 8)
            then NoColors := 1 shl FSrcImage[0].BitCount
            else NoColors := 256;
            case FSrcImage[0].ImageFormat of
            IF_GREY4,
            IF_GREY8 : begin
                         x := 0;
                         while (x < NoColors) and (FRedHist^[x] = 0)
                         do inc(x);
                         MinVal := x;
                         x := NoColors - 1;
                         while (x > 0) and (FRedHist^[x] = 0)
                         do dec(x);
                         MaxVal := x;
                       end;
            else begin
                 x := 0;
                 while (x < NoColors) and
                       (FRedHist^[x] = 0) and
                       (FGreenHist^[x] = 0) and
                       (FBlueHist^[x] = 0)
                 do inc(x);
                 MinVal := x;
                 x := NoColors - 1;
                 while (x > 0) and
                       (FRedHist^[x] = 0) and
                       (FGreenHist^[x] = 0) and
                       (FBlueHist^[x] = 0)
                 do dec(x);
                 MaxVal := x;
            end;
            end;
            dOffset := MinVal + (MaxVal - MinVal) / 2.0;
            //dOffset := 127.50;

            if (Width = 0)
            then Slope := 1.0
            else begin
                 if (Width > 0)
                 then begin
                      if (Width > 254)
                      then Width := 254;
                      Slope := 255.0 / (255.0 - Width);
                 end
                 else begin
                      if (Width < -254)
                      then Width := -254;
                      Slope := (255.0 + Width) / 255.0;
                 end;
            end;
            // dOffset ensures that the intensities are stretched or compressed
            // around the centre of the current dynamic range.
            dOffset := dOffset * Slope - dOffset;

            GetMem(Lookup, 256 * SizeOf(byte));
            try
              for x := 0 to 255
              do begin
                 Value := Round(x * Slope - dOffset) + Offset;
                 if (Value < 0)
                 then Lookup^[x] := 0
                 else if (Value > 255)
                      then Lookup^[x] := 255
                      else Lookup^[x] := Value;
              end;
              LookupTable(Lookup, Lookup, Lookup);
              if (FError = EC_OK)
              then Result := FResImage;
            finally
              FreeMem(Lookup);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
end; // TmcmImageColor.BrightContrast.


function power(x, y : double) : double;
// x^y, but only for positiv values of x.
begin
  if (y = 0)
  then power := 1.0
  else if ((x > 0.0) and (y > 0.0))
       then power := 1.0 * exp(y * ln(x))
       else if (x > 0.0) and (y < 0.0)
            then power := 1.0 / exp(abs(y) * ln(x))
            else if (x = 0.0)
                 then power := 0.0
                 else power := 1.0;
end; // power.


function CalcGamma(Value, Gamma : double) : double;
begin
  Result := Power(Value, 1.0 / Gamma);
end; // CalcGamma.


function TmcmImageColor.Gamma(Value : double) : TmcmImage;
var x        : cardinal;
//    LValue   : longint;
    Lookup   : PVectorB;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := FSrcImage[0].ImageFormat;
            FResImage.Palette     := FSrcImage[0].Palette;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            GetMem(Lookup, 256 * SizeOf(byte));
            try
              for x := 0 to 255
              do Lookup^[x] := Round(255.0 * CalcGamma(1.0 * x / 255.0, Value));

              LookupTable(Lookup, Lookup, Lookup);
              if (FError = EC_OK)
              then Result := FResImage;
            finally
              FreeMem(Lookup);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
end; // TmcmImageColor.Gamma.


function TmcmImageColor.Stretch : TmcmImage;
var x, y, z  : cardinal;
    i        : cardinal;
    v        : byte;
    Value    : longint;
    pS       : PVectorB;
    LoValue  : byte;
    HiValue  : byte;
    Slope    : double;
    Offset   : double;
    Lookup   : PVectorB;
    iWidth   : cardinal;
begin
  Result := Nil;
  FError := EC_OK;
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       GetMem(Lookup, 256 * SizeOf(byte));
       try
         LoValue := 255;
         HiValue := 0;
         case FSrcImage[0].ImageFormat of
         IF_PAL4,
         IF_GREY4  : begin
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          with FSrcImage[0]
                          do begin
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                z := x shr 1;
                                if ((x and 1) = 0)
                                then v := (pS^[z] and $F0) shr 4
                                else v := (pS^[z] and $0F);

                                if (LoValue > DibInfo.bmiColors[v].rgbRed)
                                then LoValue := DibInfo.bmiColors[v].rgbRed;
                                if (HiValue < DibInfo.bmiColors[v].rgbRed)
                                then HiValue := DibInfo.bmiColors[v].rgbRed;
                                if (LoValue > DibInfo.bmiColors[v].rgbGreen)
                                then LoValue := DibInfo.bmiColors[v].rgbGreen;
                                if (HiValue < DibInfo.bmiColors[v].rgbGreen)
                                then HiValue := DibInfo.bmiColors[v].rgbGreen;
                                if (LoValue > DibInfo.bmiColors[v].rgbBlue)
                                then LoValue := DibInfo.bmiColors[v].rgbBlue;
                                if (HiValue < DibInfo.bmiColors[v].rgbBlue)
                                then HiValue := DibInfo.bmiColors[v].rgbBlue;
                             end;
                          end;
                       end;
                     end;
         IF_PAL8,
         IF_GREY8  : begin
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          with FSrcImage[0]
                          do begin
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                if (LoValue > DibInfo.bmiColors[pS^[x]].rgbRed)
                                then LoValue := DibInfo.bmiColors[pS^[x]].rgbRed;
                                if (HiValue < DibInfo.bmiColors[pS^[x]].rgbRed)
                                then HiValue := DibInfo.bmiColors[pS^[x]].rgbRed;
                                if (LoValue > DibInfo.bmiColors[pS^[x]].rgbGreen)
                                then LoValue := DibInfo.bmiColors[pS^[x]].rgbGreen;
                                if (HiValue < DibInfo.bmiColors[pS^[x]].rgbGreen)
                                then HiValue := DibInfo.bmiColors[pS^[x]].rgbGreen;
                                if (LoValue > DibInfo.bmiColors[pS^[x]].rgbBlue)
                                then LoValue := DibInfo.bmiColors[pS^[x]].rgbBlue;
                                if (HiValue < DibInfo.bmiColors[pS^[x]].rgbBlue)
                                then HiValue := DibInfo.bmiColors[pS^[x]].rgbBlue;
                             end;
                          end;
                       end;
                     end;
         IF_RGB24  : begin
                       iWidth := FSrcImage[0].Width;
                       if (FSrcImage[0].ImageFormat = IF_RGB24)
                       then iWidth := 3 * iWidth;

                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          for x := 0 to (iWidth - 1)
                          do begin
                             if (LoValue > pS^[x])
                             then LoValue := pS^[x];
                             if (HiValue < pS^[x])
                             then HiValue := pS^[x];
                          end;
                       end;
                     end;
         //IF_RGB15
         //IF_RGB16
         IF_RGBA32 : begin
                       iWidth := 4 * FSrcImage[0].Width;
                       for y := 0 to (FSrcImage[0].Height - 1)
                       do begin
                          pS := FSrcImage[0].ScanLine[y];
                          x := 0;
                          while (x < iWidth)
                          do begin
                             for i := 0 to 2
                             do begin
                                if (LoValue > pS^[x])
                                then LoValue := pS^[x];
                                if (HiValue < pS^[x])
                                then HiValue := pS^[x];
                                inc(x);
                             end;
                             inc(x);
                          end;
                       end;
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;

         if (0 < LoValue) or (HiValue < 255)
         then begin
              if (HiValue <> LoValue)
              then begin
                   Slope  := 255.0 / (HiValue - LoValue);
                   Offset := Slope * LoValue;
                   for x := 0 to 255
                   do begin
                      Value := Round(x * Slope - Offset);
                      if (Value < 0)
                      then Lookup^[x] := 0
                      else if (Value > 255)
                           then Lookup^[x] := 255
                           else Lookup^[x] := Value;
                   end;
                   LookupTable(Lookup, Lookup, Lookup);
              end;
         end;
       finally
         FreeMem(Lookup);
       end;
  end;
end; // TmcmImageColor.Stretch.


procedure TmcmImageColor.Equalize;
var i, Value    : longint;
    M_xy        : double;
    SumRed      : cardinal;
//    SumGreen    : cardinal;
//    SumBlue     : cardinal;
    RedLookup   : PVectorB;
    GreenLookup : PVectorB;
    BlueLookup  : PVectorB;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       GetMem(RedLookup, 256 * SizeOf(byte));
       GetMem(GreenLookup, 256 * SizeOf(byte));
       GetMem(BlueLookup, 256 * SizeOf(byte));

       try
         GetHistogram;

         M_xy := 1.0 / (FSrcImage[0].Width * FSrcImage[0].Height);
         case FSrcImage[0].ImageFormat of
         IF_GREY8  : begin
                       M_xy := 256.0 * M_xy;

                       SumRed := 0;
                       for i := 0 to 255
                       do begin
                          SumRed := SumRed + FRedHist^[i];
                          Value  := Round(M_xy * SumRed) - 1;
                          if (Value < 0)
                          then RedLookup[i] := 0
                          else if (Value < 256)
                               then RedLookup[i] := Value
                               else RedLookup[i] := 255;
                       end;
                     end;
         //IF_RGB15
         //IF_RGB16
         IF_RGB24,
         IF_RGBA32 : begin
                       M_xy := 256 * M_xy / 3.0;

                       SumRed := 0;
                       for i := 0 to 255
                       do begin
                          SumRed := SumRed + (FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i]);
                          Value  := Round(M_xy * SumRed) - 1;
                          if (Value < 0)
                          then RedLookup[i] := 0
                          else if (Value < 256)
                               then RedLookup[i] := Value
                               else RedLookup[i] := 255;
                          GreenLookup[i] := RedLookup[i];
                          BlueLookup[i]  := RedLookup[i];
                       end;
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;
         LookupTable(RedLookup, GreenLookup, BlueLookup);
       finally
         FreeMem(RedLookup);
         FreeMem(GreenLookup);
         FreeMem(BlueLookup);
       end;
  end;
end; // TmcmImageColor.Equalize.


procedure TmcmImageColor.EqualizeLuminance;
var YImage      : TmcmImage;
    CbImage     : TmcmImage;
    CrImage     : TmcmImage;
    SaveSource  : TmcmImage;
    SaveResult  : TmcmImage;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;

       SaveSource := FSrcImage[0];
       try
         if (SaveSource.ImageFormat in [IF_RGB24,IF_RGBA32])
         then begin
              SaveResult := FResImage;
              FResImage := Nil;
              CbImage := GetCbChannel;
              FResImage := Nil;
              CrImage := GetCrChannel;
              FResImage := Nil;
              YImage  := GetLuminanceChannel;

              FSrcImage[0] := TKernelImage(YImage);
              Equalize;

              FSrcImage[0] := TKernelImage(YImage);
              FSrcImage[1] := TKernelImage(CbImage);
              FSrcImage[2] := TKernelImage(CrImage);
              FResImage := TKernelImage(SaveResult);
              CombineYCbCr;

              YImage.Free;
              CbImage.Free;
              CrImage.Free;
         end
         else Equalize;

       finally
         FSrcImage[0] := TKernelImage(SaveSource);
       end;
  end;
end; // TmcmImageColor.EqualizeLuminance;


//------------------------------------------------------------------------------
// Invert Image
//------------------------------------------------------------------------------

procedure TmcmImageColor.Invert;
// Invert bitmap data.
// Supports 1, 8 and 24 bit images.
var pS, pR    : PVectorB;
    ImageSize : cardinal;
    i         : cardinal;
begin
  FError := EC_OK;
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := FSrcImage[0].ImageFormat;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            pS := FSrcImage[0].pDib;
            pR := FResImage.pDib;
            ImageSize := FSrcImage[0].LongLineWidth * cardinal(FSrcImage[0].Height);
            {$IFDEF MCMASM}
              case FResImage.ImageFormat of // BitCount of
              IF_BW    : asm
                           push  edi
                           push  esi
                           mov   ebx,ImageSize           // Pixels to iterate through
                           sub   ebx,4
                           mov   esi,pS                  // esi pointer to source image
                           mov   edi,pR                  // edi pointer to result image
                           @loop:
                           mov   eax,DWORD PTR [esi+ebx] // Get 32 pixels (4 byte)
                           not   eax                     // Pixel = Not Pixel
                           mov   DWORD PTR [edi+ebx],eax //
                           sub   ebx,4                   // (4 pixels take at a time).
                           jnz   @loop                   // Loop until ebx = 0
                           mov   eax,DWORD PTR [esi+ebx]
                           not   eax                     // Pixel = Not Pixel
                           mov   DWORD PTR [edi+ebx],eax
                           pop   esi
                           pop   edi
                         end;
              IF_PAL4  : begin
                           FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                           FSrcImage[0].GetPaletteEntries(FLogPalette);
                           for i := 0 to 15
                           do begin
                              FLogPalette.palPalEntry[i].peRed   := 255 - FLogPalette.palPalEntry[i].peRed;
                              FLogPalette.palPalEntry[i].peGreen := 255 - FLogPalette.palPalEntry[i].peGreen;
                              FLogPalette.palPalEntry[i].peBlue  := 255 - FLogPalette.palPalEntry[i].peBlue;
                           end;
                           FResImage.SetPaletteEntries(FLogPalette);
                           FreeMem(FLogPalette);
                         end;
              IF_PAL8  : begin
                           FLogPalette := AllocMem(256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
                           FSrcImage[0].GetPaletteEntries(FLogPalette);
                           for i := 0 to 255
                           do begin
                              FLogPalette.palPalEntry[i].peRed   := 255 - FLogPalette.palPalEntry[i].peRed;
                              FLogPalette.palPalEntry[i].peGreen := 255 - FLogPalette.palPalEntry[i].peGreen;
                              FLogPalette.palPalEntry[i].peBlue  := 255 - FLogPalette.palPalEntry[i].peBlue;
                           end;
                           FResImage.SetPaletteEntries(FLogPalette);
                           FreeMem(FLogPalette);
                         end;
              //IF_RGB15,
              //IF_RGB16 : ;
              IF_GREY4,
              IF_GREY8,
              IF_RGB24,
              IF_RGBA32 : asm
                           push  edi
                           push  esi
                           mov   ecx,ImageSize
                           dec   ecx
                           mov   esi,pS                // esi pointer to source image
                           mov   edi,pR                // edi pointer to result image
                           @loop:
                           mov   al,BYTE PTR [esi+ecx] // Get pixel index value
                           mov   ah,$0FF
                           sub   ah,al
                           mov   BYTE PTR [edi+ecx],ah
                           dec   ecx                   // Get next Pixel
                           jge   @loop                 // Loop until ecx < 0
                           @end:
                           pop   esi
                           pop   edi
                         end;
              // Error - Image color format cannot be applied in this algorithme.
              else FError := EC_BADCOLORFORMAT;
              end;
            {$ELSE}
              case FResImage.BitCount of
              1  : for i := 0 to (ImageSize - 1)
                   do pR^[i] := Not(pS^[i]);
              8,
              24,
              32 : begin // 8 & 24 bit.
                     for i := 0 to (ImageSize - 1)
                     do pR^[i] := 255 - pS^[i];
                   end;
              // Error - Image color format cannot be applied in this algorithme.
              else FError := EC_BADCOLORFORMAT;
              end;
            {$ENDIF}
       end
       else ; // No result
  end
  else ; // No source.
end; // TmcmImageColor.Invert.


//------------------------------------------------------------------------------
// Threshold Image
//------------------------------------------------------------------------------

procedure TmcmImageColor.ThresholdLevel(Level  : word;
                                        xs, ys : longword;
            	                          xe, ye : longword);
var pS, pR     : PVectorB;
    sLineWidth : longword;
    rLineWidth : longword;
    {$IFNDEF MCMASM}
    Bits       : byte;
    BitIndex   : byte;
    i, j       : longword;
    r, s       : longword;
    {$ELSE}
     iSize     : longword;
    {$ENDIF}
begin
  pS := FSrcImage[0].pDib;
  pR := FResImage.pDib;
  sLineWidth := FSrcImage[0].LongLineWidth;
  rLineWidth := FResImage.LongLineWidth;
  {$IFDEF MCMASM}
    case FResImage.BitCount of
    1  : asm
                  push  edi
                  push  esi

                  mov   bl,Byte(Level)
                  mov   ecx,ys                  // Loop from ys to ye.
         @yloop:
                  mov   eax,sLineWidth          // Source image long line width
                  mul   ecx                     // y line position (clears edx)
                  add   eax,pS                  // esi pointer to source image
                  add   eax,xs
                  mov   esi,eax                 // esi pointer to source image at line edx

                  mov   eax,rLineWidth          // Result image long line width
                  mul   ecx                     // y line position (clears edx)
                  add   eax,pR                  // esi pointer to source image
                  mov   edi,eax                 // esi pointer to source image at line edx

                  push  ecx                     // Save y position.
                  mov   ecx,xe                  // Loop from xs to xe.
                  sub   ecx,xs

                  xor   al,al                   // Resulting byte (Bits)
                  mov   ah,$080                 // Bit index (BitIndex)
         @xloop:
                  // Data process begin
                  mov   bh,BYTE PTR [esi]       // Get pixel index value
                  cmp   bh,bl
                  jbe   @loval
         @hival:
                  or    al,ah                   // Set pixel.
         @loval:
                  shr   ah,$01
                  jnz   @hinext
                  mov   BYTE PTR [edi],al       // Set (eight) pixels to Result image.
                  xor   al,al
                  mov   ah,$080
                  inc   edi
         @hinext:
                  inc   esi
                  dec   ecx                     // Increament and check y position
                  jnz   @xloop

                  cmp   ah,$080                 // Make sure to set last bits
                  je    @eofl
                  mov   BYTE PTR [edi],al       // Set remaining pixels to Result image.

         @eofl:                                 // Reached end of line.
                  pop   ecx                     // Increament and check y position
                  inc   ecx
                  cmp   ecx,ye
                  jne   @yloop

                  pop   esi
                  pop   edi
         end;
    8  : if FMMX
         then begin
              iSize := FSrcImage[0].DibInfo^.bmiHeader.biSizeImage;
              pS := FSrcImage[0].pDib;
              pR := FResImage.pDib;
              asm
                // Save registers to stack
                push  ebx
                push  edi
                push  esi

                // Calculate Qwords per line.
                mov   eax,iSize
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

                test  ecx,ecx
                jz    @NoQWORD

                // process image
                xor      eax,eax      // Duplicate Level
                mov      al,Byte(Level)
                shl      eax,16
                mov      al,Byte(Level)

                {$IFNDEF DCB3_5}
                pxor     mm7,mm7      // mm7 = zero reg.
                pxor     mm2,mm2
                movd     mm2,eax
                movq     mm3,mm2
                psllq    mm3,32
                por      mm2,mm3      // mm2 = Level (at 4 positions)
                {$ELSE}
                db $0F,$EF,$FF
                db $0F,$EF,$D2
                db $0F,$6E,$D0
                db $0F,$6F,$DA
                db $0F,$73,$F3,$20
                db $0F,$EB,$D3
                {$ENDIF}
                @LoopQWORD:
                {$IFNDEF DCB3_5}
                movq      mm0,qword ptr [esi+ecx*8]
                movq      mm1,mm0     // copy

                punpcklbw mm0,mm7     // unpack lower 4 bytes to words
                punpckhbw mm1,mm7     // unpack higher 4 bytes to words

                pcmpgtw   mm0,mm2     // Compare Pixel values with Level.
                pcmpgtw   mm1,mm2     // do.

                packsswb  mm0,mm1     // Pack pixel data.
                movq      qword ptr [edi+ecx*8],mm0 // Return threshold result to image.
                {$ELSE}
                db $0F,$6F,$04,$CE
                db $0F,$6F,$C8

                db $0F,$60,$C7
                db $0F,$68,$CF

                db $0F,$65,$C2
                db $0F,$65,$CA

                db $0F,$63,$C1
                db $0F,$7F,$04,$CF
                {$ENDIF}
                dec   ecx
                jns   @LoopQWORD

                // Check for unprocessed bytes
                @NoQWORD:
                pop   ecx
                mov   ebx,ecx
                and   ebx,$07         // Offset address - ebx := Count mod 8.
                jz    @EndOfImage
                and   ecx,$FFFFFFF8   // ecx := Count - (Count mod 8)
                dec   ebx

                // Process remaining bytes in image.
                xor   ah,ah           // ah := zero.
                mov   dl,Byte(Level)
                @LoopRemain:
                mov   al,[esi+ecx]
                cmp   al,dl
                jbe   @loval
                mov   al,$FF
                mov   [edi+ecx],al
                jmp   @NextPix
                @loval:
                mov   [edi+ecx],ah
                @NextPix:
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
              asm
                push  edi
                push  esi

                mov   bl,Byte(Level)
                mov   ecx,ys                  // Loop from ys to ye.

                @yloop:
                mov   eax,sLineWidth          // Source image long line width
                mul   ecx                     // y line position
                add   eax,pS                  // esi pointer to source image
                add   eax,xs                  // Add x offset
                mov   esi,eax                 // esi pointer to source image at line edx

                mov   eax,rLineWidth          // Result image long line width
                mul   ecx                     // y line position
                add   eax,pR                  // esi pointer to source image
                add   eax,xs                  // Add x offset
                mov   edi,eax                 // esi pointer to source image at line edx

                push  ecx                     // Save y position.
                mov   ecx,xe                  // Loop from xs to xe.
                sub   ecx,xs

                @xloop:
                // Data process begin
                mov   al,BYTE PTR [esi]       // Get pixel index value
                cmp   al,bl
                jbe   @loval

                @hival:
                mov   BYTE PTR [edi],255      // Set resulting high pixel index value
                // Data process end

                inc   edi
                inc   esi
                dec   ecx                     // Increament and check x position
                jnz   @xloop
                jmp   @eofl                   // Reached end of line.

                @loval:
                mov   BYTE PTR [edi],0        // Set resulting low pixel index value

                inc   edi
                inc   esi
                dec   ecx                     // Increament and check x position
                jnz   @xloop

                @eofl:
                pop   ecx                     // Increament and check y position
                inc   ecx
                cmp   ecx,ye
                jne   @yloop

                pop   esi
                pop   edi
              end;
         end;
    // Error - Image color format cannot be applied in this algorithme.
    else FError := EC_BADCOLORFORMAT;
    end;
  {$ELSE}
    // Source is 8 bit gray.
    case FResImage.BitCount of
    1 : begin
          for j := ys to (ye - 1)
          do begin
             Bits := 0;
             BitIndex := $80;
             s := j * FSrcImage[0].LongLineWidth;
             r := j * FResImage.LongLineWidth;
             for i := xs to (xe - 1)
             do begin
                if (pS[s] > Level)
                then Bits := Bits or BitIndex;
                if (BitIndex = $01) // 8 pixels has been tested, save result.
                then begin
                     pR[r] := Bits;
                     inc(r);
                     Bits := 0;
                     BitIndex := $80;
                end
                else BitIndex := BitIndex shr 1;
                inc(s);
             end;
             // Set last byte
             if (BitIndex <> $80)
             then pR[r] := Bits;
          end;
        end;
    8 : for j := ys to (ye - 1)
        do begin
             s := j * FSrcImage[0].LongLineWidth;
             r := j * FResImage.LongLineWidth;
             for i := xs to (xe - 1)
             do begin
                if (pS[s] > Level)
                then pR[r] := 255
                else pR[r] := 0;
                inc(s);
                inc(r);
             end;
        end;
    // Error - Image color format cannot be applied in this algorithme.
    else FError := EC_BADCOLORFORMAT;
    end;
  {$ENDIF}
end; // TmcmImageColor.ThresholdLevel.


procedure TmcmImageColor.ThresholdIsoData(var Level  : word;
                                              xs, ys : longword;
		                              xe, ye : longword);
var Min, Max  : longint;
    BackMean  : Comp;
    ForeMean  : Comp;
    BackCount : Comp;
    ForeCount : Comp;
    OldLevel  : word;
    i, j      : longword;
begin
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;
       try
         GetHistogram;

         case FSrcImage[0].ImageFormat of
         IF_GREY8  : begin
                       i := 0;
                       while (i < 256) and (FRedHist^[i] = 0)
                       do inc(i);
                       Min := i;

                       i := 255;
                       while (i > 0) and (FRedHist^[i] = 0)
                       do dec(i);
                       Max := i;

                       Level := Min + Round((Max - Min) / 2.0);
                       OldLevel := 0;

                       j := 0;
                       while (Level <> OldLevel) and (j < 50)
                       do begin
                          OldLevel := Level;
                          BackMean := 0;
                          BackCount := 0;
                          for i := Min to (Level - 1)
                          do begin
                             BackMean := BackMean + i * FRedHist^[i];
                             BackCount := BackCount + FRedHist^[i];
                          end;

                          if (BackCount > 0)
                          then BackMean := (BackMean / BackCount)
                          else BackMean := 0;

                          ForeMean := 0;
                          ForeCount := 0;
                          for i := Level to Max
                          do begin
                             ForeMean := ForeMean + i * FRedHist^[i];
                             ForeCount := ForeCount + FRedHist^[i];
                          end;

                          if (ForeCount > 0)
                          then ForeMean := ForeMean / ForeCount
                          else ForeMean := 0;

                          Level := Round((ForeMean + BackMean) / 2.0);
                       end;
                     end;
         //IF_RGB15
         //IF_RGB16
         IF_RGB24,
         IF_RGBA32 : begin
                       i := 0;
                       while (i < 256) and
                             (FRedHist^[i] = 0) and
                             (FGreenHist^[i] = 0) and
                             (FBlueHist^[i] = 0)
                       do inc(i);
                       Min := i;

                       i := 255;
                       while (i > 0) and
                             (FRedHist^[i] = 0) and
                             (FGreenHist^[i] = 0) and
                             (FBlueHist^[i] = 0)
                       do dec(i);
                       Max := i;

                       Level := Min + ((Max - Min) div 2);
                       OldLevel := 0;

                       j := 0;
                       while (Level <> OldLevel) and (j < 50)
                       do begin
                          OldLevel := Level;
                          BackMean := 0;
                          BackCount := 0;
                          for i := Min to (Level - 1)
                          do begin
                             BackMean := BackMean + i * (FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i]);
                             BackCount := BackCount + FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i];
                          end;

                          if (BackCount > 0)
                          then BackMean := (BackMean / BackCount)
                          else BackMean := 0;

                          ForeMean := 0;
                          ForeCount := 0;
                          for i := Level to Max
                          do begin
                             ForeMean := ForeMean + i * (FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i]);
                             ForeCount := ForeCount + FRedHist^[i] + FGreenHist^[i] + FBlueHist^[i];
                          end;

                          if (ForeCount > 0)
                          then ForeMean := ForeMean / ForeCount
                          else ForeMean := 0;

                          Level := Round((ForeMean + BackMean) / 2.0);
                       end;
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;

         ThresholdLevel(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
       finally
       end;
  end;
end; // TmcmImageColor.ThresholdIsoData.


procedure TmcmImageColor.ThresholdSymmetry(var Level  : word;
                                               xs, ys : longword;
                                               xe, ye : longword);
var i          : integer;
    PixelCount : Comp;
    PeakPos    : integer;
    TmpCount   : Comp;
    LevelCount : Comp;
    LevelPos   : integer;
    LastValue  : Cardinal;
begin
  if (Level >= 100)
  then Level := 99;

  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;
       try
         GetHistogram;

         case FSrcImage[0].ImageFormat of
         IF_GREY8  : begin
                       //i := 0;
                       PixelCount := FSrcImage[0].Width * FSrcImage[0].Height;
                       {
                       while (i < 256)
                       do begin
                          PixelCount := PixelCount + FRedHist^[i];
                          inc(i);
                       end;
                       }

                       // Determin symmetry level
                       LevelCount := ({100 -} Level) * PixelCount / 100;

                       // Smooth histogram
                       for i := 1 to 254
                       do FBlueHist^[i] := (FRedHist^[i+1] + FRedHist^[i] + FRedHist^[i-1]) div 3;

                       i := 0;
                       FBlueHist^[i] := (FRedHist^[i+1] + FRedHist^[i]) div 2;
                       i := 255;
                       FBlueHist^[i] := (FRedHist^[i] + FRedHist^[i-1]) div 2;

                       // Find histogram peak (highest count).
                       PeakPos := 255;
                       LastValue := 0;
                       for i := 255 downto 0
                       do begin
                          if (FBlueHist^[i] > LastValue)
                          then begin
                               LastValue := FBlueHist^[i];
                               PeakPos := i;
                          end;
                       end;

                       // Count from top of histogram until LevelCount is
                       // reached.
                       LevelPos := 255;
                       TmpCount := 0;
                       while (LevelPos > PeakPos) and (TmpCount < LevelCount)
                       do begin
                          TmpCount := TmpCount + FRedHist^[LevelPos];
                          if (TmpCount < LevelCount)
                          then dec(LevelPos);
                       end;

                       if (LevelPos < PeakPos)
                       then Level := PeakPos
                       else Level := PeakPos - (LevelPos - PeakPos);
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;

         ThresholdLevel(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
       finally
       end;
  end;
end; // TmcmImageColor.ThresholdSymmetry.


procedure TmcmImageColor.EstimatePctThreshold(    Src     : TKernelImage;
                                                  Percent : cardinal;
                                              var Hi, Lo  : integer);
var PctSize  : cardinal;
    i        : integer;
    Count    : cardinal;
    Depth    : integer;
    Max, Min : integer;
begin
  Hi := 255;
  Lo := 0;

  GetHistogram;

  Depth := (1 shl Src.BitCount) - 1;
  Min := 0;
  Max := Depth;
  PctSize := Src.DibInfo^.bmiHeader.biSizeImage;
  if (FIntHist[Depth] > PctSize div 5) // more than 20%
  then begin
       PctSize := PctSize - FIntHist[Max];
       dec(Max);
  end;
  if (FIntHist[0] > PctSize div 5) // more than 20%
  then begin
       PctSize := PctSize - FIntHist[Min];
       inc(Min);
  end;
  PctSize := (PctSize * (100 - Percent)) div 100;

  if (FError = EC_OK)
  then begin
       Count := 0;
       for i := Max downto Min
       do begin
          Hi := i;
          Count := Count + FIntHist[i];
          if (Count > PctSize)
          then break;
       end;

       PctSize := (PctSize * (Percent)) div 100;
       Count := 0;
       for i := (Hi - 1) downto Min
       do begin
          Lo := i;
          Count := Count + FIntHist[i];
          if (Count > PctSize)
          then break;
       end;

       // Make sure that Lo is always above the lowest present intensity, and
       // Hi is greater than Lo.
       i := 0;
       while (FIntHist[i] = 0)
       do inc(i);

       if (Lo < i)
       then Lo := i;
       if (Hi <= Lo)
       then Hi := Lo + 1;
  end;
end; // TmcmImageColor.EstimatePctThreshold.


procedure TmcmImageColor.TraceAndThreshold(AImage : TmcmImage; OImage : TmcmImage; x, y : integer; Value, Hi, Lo : byte);
var LPixON    : byte;
    LPixOFF   : byte;
    LPixThres : byte;

     function GetPrevious(sx, sy : integer; Up : boolean) : byte;
     var a          : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Get current line source address.
       Apt := AImage.ScanLine[sy];

       // Search for features above (Up=True) or below current line.
       if Up
       then Bpt := OImage.ScanLine[sy+1]
       else Bpt := OImage.ScanLine[sy-1];

       a := sx;
       while (a < AImage.Width) and
             (Apt^[a] >= LPixON) and
             (Bpt^[a] <> LPixThres) // ((Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF))
       do inc(a);

       if (Bpt^[a] <> LPixThres) // (Bpt^[a] = LPixON) or (Bpt^[a] <= LPixOFF)
       then Result := 0//LPixOFF
       else Result := Bpt^[a];
     end; // GetPrevious.


     function HasNeighbour(sx, sy, ex, ey : integer; Up : boolean; Value : byte) : byte;
     var a, am, ap  : integer;
         maxx       : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Calculate Current Line source address.
       Apt := OImage.ScanLine[sy];

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
       while (a < maxx) and
             Not(((Apt^[a] = Value) or (Apt^[am] = Value) or (Apt^[ap] = Value)) and
                 (Bpt^[a] >= LPixON))
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
       else Result := 0; //LPixOFF;
     end; // HasNeighbour.


     function FirstLine(var sx, sy, ex, ey : integer) : longint;
     var a          : integer;
         Count      : longint;
         ALine      : PvectorB;
         OLine      : PvectorB;
     begin
       Count := 0;

       // Calculate Current Line source address.
       ALine := AImage.ScanLine[sy];
       OLine := OImage.ScanLine[sy];

       // Fill coherent "ON"-pixels with feature "Value".
       a := sx;

       while (0 < sx) and (ALine^[sx-1] >= LPixON)
       do begin
          OLine^[sx-1] := 255;
          dec(sx);
          inc(Count);
       end;

       while (0 <= a) and (a < AImage.Width) and (ALine^[a] >= LPixON)
       do begin
          OLine^[a] := 255;
          inc(a);
          inc(Count);
       end;
       dec(a);
       if (a < 0)
       then a := 0;
       ex := a;
       Result := Count;
     end; // FirstLine.


     function FillLine(var sx, sy, ex, ey : integer; Up : boolean) : longint;
     var a, b, c, d : integer;
         bm, bp     : integer;
         Count      : longint;
         ALine      : PvectorB;
         BLine      : PvectorB;
         OLine      : PvectorB;
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
       OLine := OImage.ScanLine[sy];

       if Up
       then BLine := OImage.ScanLine[sy-1]
       else BLine := OImage.ScanLine[sy+1];

       a := sx;
       d := sx;
       while (a <= ex) and (ALine^[a] <= LPixOFF)
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
               if (ALine^[b] >= LPixON) and (OLine^[b] <> LPixThres)
               then begin
                    if (BLine^[b]  = LPixThres) or
                       (BLine^[bm] = LPixThres) or
                       (BLine^[bp] = LPixThres)
                    then begin
                         c := b;
                         while (c >= 0) and (ALine^[c] >= LPixON)
                         do begin
                            OLine^[c] := LPixThres;
                            dec(c);
                            inc(Count);
                         end;
                         if (a > c)
                         then a := c + 1;
                         if (d <= c)
                         then d := c + 1;

                         inc(b);
                         while (b < AImage.Width) and (ALine^[b] >= LPixON)
                         do begin
                            OLine^[b] := LPixThres;
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


var NIndex     : longint;
    ix, iy     : integer;
    ox, oy     : integer;
    Dir        : boolean;
begin
  LPixThres := Value;
  LPixON    := Lo + 1;
  LPixOFF   := Lo; //BkValue;

  NIndex := 0;

  ix := x;
  iy := y;
  ox := FSrcWidth;
  oy := y;

  // Feature fill current line segment.
  FirstLine(ix, iy, ox, oy);

  // Save x and/or y position if it marks a new boundary of the feature.
  if (FBlx > ox)
  then FBlx := ox;
  if (FBby < iy)
  then FBby := iy;
  if (FBrx < ox)
  then FBrx := ox;
  if (FBty > iy)
  then FBty := iy;

  // Place the line above the first line on our neighbour stack.
  if (iy > 0)
  then begin
       FNeighbour[NIndex].x1 := ix;
       FNeighbour[NIndex].x2 := ox;
       FNeighbour[NIndex].y1 := iy - 1;
       FNeighbour[NIndex].y2 := iy - 1;
       FNeighbour[NIndex].Up := False;
       inc(NIndex);
  end;

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
              ix := FNeighbour[NIndex].x1;
              iy := FNeighbour[NIndex].y1;
              ox := FNeighbour[NIndex].x2;
              oy := FNeighbour[NIndex].y2;
              Dir := FNeighbour[NIndex].Up;
         end;
    end;

    while (ix <= ox) and (0 <= iy) and (iy < FSrcHeight)
    do begin
       // Fill line segments belonging to current feature.
       if (FillLine(ix, iy, ox, oy, Dir) = 0)
       then begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // If at end of feature (top or bottom) check for saved
            // lines to search for in opersite direction.
            inc(iy);
            if (NIndex > 0)
            then begin
                 dec(NIndex);
                 ix := FNeighbour[NIndex].x1;
                 iy := FNeighbour[NIndex].y1;
                 ox := FNeighbour[NIndex].x2;
                 oy := FNeighbour[NIndex].y2;
                 Dir := FNeighbour[NIndex].Up;
            end;
       end
       else begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // Check if line-segments in the line in opersite
            // direction (the line above or below) belongs to
            // the current feature (pixels = PixON).
            if (HasNeighbour(ix, iy, ox, oy, Not(Dir), 255) >= LPixON)
            then begin
                 // Increase allocated memory if required.
                 if (NIndex >= FNeighbourSize)
                 then begin
                      ReallocMem(FNeighbour, (FNeighbourSize + FSrcHeight) * SizeOf(TNeighbour));
                      if (FNeighbour <> Nil)
                      then FNeighbourSize := FNeighbourSize + FSrcHeight
                      else FNeighbourSize := 0;
                 end;

                 // Save left- and rightmost x coordinate for
                 // "later" filling.
                 if (NIndex < FNeighbourSize)
                 then begin
                      FNeighbour[NIndex].x1 := ix;
                      FNeighbour[NIndex].x2 := ox;
                      if Dir
                      then begin
                           FNeighbour[NIndex].y1 := iy - 1;
                           FNeighbour[NIndex].y2 := iy - 1;
                           inc(iy);
                      end
                      else begin
                           FNeighbour[NIndex].y1 := iy + 1;
                           FNeighbour[NIndex].y2 := iy + 1;
                           dec(iy);
                      end;
                      FNeighbour[NIndex].Up := Not(Dir);

                      if (0 <= FNeighbour[NIndex].y1) and (FNeighbour[NIndex].y1 < FSrcHeight)
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
end; // TmcmImageColor.TraceAndThreshold.


procedure TmcmImageColor.ThresholdTrace(Level  : word;
                                        xs, ys : longword;
		                        xe, ye : longword);
var Hi, Lo : integer;
    x, y   : integer;
    pS, pR : PVectorB;
begin
  // Get Hi and Lo intensity values based on the percentage given by Level.
  if FTraceAuto // (FTraceLo = 0) and (FTraceHi = 0)
  then begin
       EstimatePctThreshold(FSrcImage[0], Level, Hi, Lo);
       FTraceLo := Lo;
       FTraceHi := Hi;
  end
  else begin
       Lo := FTraceLo;
       Hi := FTraceHi;
  end;

  if (FError = EC_OK)
  then begin
       FResImage.FillAll(0);

       // Find points having an intensity higher than Hi and trace all neighbour
       // points having intensities greater than Lo.

       // FNeighbour is a stack of coordinates used to link parts of a feature.
       FNeighbourSize := 4 * FSrcHeight;
       GetMem(FNeighbour, FNeighbourSize * SizeOf(TNeighbour));
       try
         for y := 0 to (FSrcHeight - 1)
         do begin
            pS := FSrcImage[0].ScanLine[y];
            pR := FResImage.ScanLine[y];
            for x := 0 to (FSrcWidth - 1)
            do begin
               if (pS^[x] >= Hi) and (pR^[x] <> 255)
               then begin
                    pR^[x] := 255;
                    FBlx := x;
                    FBby := y;
                    FBrx := x;
                    FBty := y;

                    TraceAndThreshold(FSrcImage[0], FResImage, x, y, 255, Hi, Lo);
               end;
            end;
         end;
       finally
         if (FNeighbour <> Nil)
         then FreeMem(FNeighbour);
       end;
  end;
end; // TmcmImageColor.ThresholdTrace.


procedure TmcmImageColor.ThresholdTriangular(var Level  : word;
                                                 xs, ys : longword;
                                                 xe, ye : longword);
var xMin, xMax    : integer;
    yMin, yMax    : integer;
    iMax          : integer;
    x, y          : longword;
    a, b, c       : double; // Line equation : ax + by + c = 0
    alpha, q      : double; // Line equation : y = alpha * x + q
    Sqrt_ab       : double;
    Dist, MaxDist : double;
    xDist         : integer;

begin
  if Assigned(FSrcImage[0]) and Assigned(FResImage)
  then begin
       if (FSrcImage[0].Empty or FResImage.Empty)
       then begin
            FError := EC_NOMEMORY;
            Exit;
       end;
       try
         GetHistogram;

         case FSrcImage[0].ImageFormat of
         IF_GREY8  : begin
                       // Average histogram to avoid spikes dominating the threshold value.
                       {
                       for x := 0 to 255
                       do FIntHist^[x] := FRedHist^[x];
                       }
                       for x := 1 to 254
                       do FIntHist^[x] := (FRedHist^[x-1] + FRedHist^[x] + FRedHist^[x+1]) div 3;
                       x := 1;
                       FIntHist^[0] := FIntHist^[x];
                       x := 255;
                       FIntHist^[x] := FIntHist^[x-1];

                     end;
         //IF_RGB15
         //IF_RGB16
         IF_RGB24,
         IF_RGBA32 : begin
                       // Convert color to luminance.
                       for x := 0 to 255
                       do FIntHist^[x] := Round(0.2989 * FRedHist^[x] + 0.5867 * FGreenHist^[x] + 0.1144 * FBlueHist^[x]);

                       for x := 0 to 255
                       do FRedHist^[x] := Round(0.2989 * FRedHist^[x] + 0.5867 * FGreenHist^[x] + 0.1144 * FBlueHist^[x]);

                       // Average histogram to avoid spikes dominating the threshold value.
                       for x := 1 to 254
                       do FIntHist^[x] := (FRedHist^[x-1] + FRedHist^[x] + FRedHist^[x+1]) div 3;
                       x := 1;
                       FIntHist^[0] := FIntHist^[x];
                       x := 255;
                       FIntHist^[x] := FIntHist^[x-1];
                     end;
         // Error - Image color format cannot be applied in this algorithme.
         else FError := EC_BADCOLORFORMAT;
         end;

         if (FError = EC_OK)
         then begin
              // Find lowest intensity different from zero.
              x := 0;
              while (x < 256) and (FIntHist^[x] = 0)
              do inc(x);
              xMin := x;

              // Find highest intensity different from zero.
              x := 255;
              while (x > 0) and (FIntHist^[x] = 0)
              do dec(x);
              xMax := x;

              // Find maximum luminance position in histogram.
              x := 0;
              iMax := 0;
              yMax := 0;
              while (x < 256)
              do begin
                 if (yMax < integer(FIntHist^[x]))
                 then begin
                      iMax := x;
                      yMax := FIntHist^[x];
                 end;
                 inc(x);
              end;

              if (abs(xMin - iMax) < abs(xMax - iMax))
              then begin
                   xMin := iMax;
                   yMin := FIntHist^[iMax];
                   yMax := FIntHist^[xMax];
              end
              else begin
                   xMax := iMax;
                   yMax := FIntHist^[iMax];
                   yMin := FIntHist^[xMin];
              end;

              // Find largest distance from a point in the histogram,
              // between xMin and xMax, perpendicular to the line
              // (xMín,yMin), (xMax,yMax).
              alpha := (yMax - yMin) / (xMax - xMin); // solve: y = ax + q
              q     := yMax - alpha * xMax;

              // Solve: 0 = ax + by + c
              b := -2.0; // Quess b.
              a := -b * alpha;
              c := -b * q;
              Sqrt_ab := sqrt(a * a + b * b);

              xDist   := 0;
              MaxDist := 0.0;
              for x := (xMin + 1) to (xMax - 1)
              do begin
                 y := FIntHist^[x]; // Point in Histogram is (i,j)

                 // Calculate perpendicular distance to line (iMín,0), (iMax,jMax).
                 try
                   Dist := abs(a * x + b * y + c) / Sqrt_ab;
                   // Find x where Dist is greatest.
                   if (MaxDist < Dist)
                   then begin
                        MaxDist := Dist;
                        xDist := x;
                   end;
                 except
                 end;
              end;
              Level := xDist;
         end;

         ThresholdLevel(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
       finally
       end;
  end;
end; // TmcmImageColor.ThresholdTriangular.


function TmcmImageColor.ThresholdStatistic(xs, ys : longword;
                                           xe, ye : longword) : double;
var Threshold      : double;
    OldThreshold   : double;
    // Max, Min       : double;
    Mean1, Mean2   : double;
    i, x, y        : integer;
    Count1, Count2 : integer;
    pS             : PVectorB;
begin

  // Find max and min.
  {
  pS := FSrcImage[0].ScanLine[ys];
  Max := pS^[xs];
  Min := pS^[xs];

  for y := ys to (ye - 1)
  do begin
     pS := FSrcImage[0].ScanLine[y];
     for x := xs to (xe - 1)
     do begin
        if (pS^[x] > Max)
        then Max := pS^[x]
        else if (pS^[x] < Min)
             then Min := pS^[x];
     end;
  end;
  }
  // Threshold := (Max - Min) / 2.0;

  // Find mean of whole distribution.
  Mean1 := 0.0;
  for y := ys to (ye - 1)
  do begin
     pS := FSrcImage[0].ScanLine[y];
     for x := xs to (xe - 1)
     do Mean1 := Mean1 + pS^[x];
  end;
  Threshold := Mean1 / ((xe - xs) * (ye - ys));

  // Find mean of two distributions.
  // Revise threshold value.
  i := 0;
  OldThreshold := -1.0;
  while (i < 11) and (Threshold <> OldThreshold)
  do begin
     Mean1  := 0.0;
     Mean2  := 0.0;
     Count1 := 1;
     Count2 := 1;

     for y := ys to (ye - 1)
     do begin
        pS := FSrcImage[0].ScanLine[y];
        for x := xs to (xe - 1)
        do begin
           if (pS^[x] < Threshold)
           then begin
                inc(Count1);
                Mean1 := Mean1 + pS^[x];
           end
           else begin
                inc(Count2);
                Mean2 := Mean2 + pS^[x];
           end;
        end;
     end;
     OldThreshold := Threshold;
     Threshold := ((Mean1 / Count1) + (Mean2 / Count2)) / 2.0;
  end;
  Result := Threshold;
end; // TmcmImageColor.ThresholdStatistic.


function TmcmImageColor.ThresholdEdge(xs, ys : longword;
                                      xe, ye : longword) : double;
var Threshold : double;
    Grad      : double;
    Sum_Grad  : double;
    Sum_Edge  : double;
    x, y      : integer;
    pS        : PVectorB;
begin
  if (xs = 0)
  then xs := 1;
  Sum_Grad := 0.0;
  Sum_Edge := 0.0;
  for y := ys to (ye - 1)
  do begin
     pS := FSrcImage[0].ScanLine[y];
     for x := xs to (xe - 1)
     do begin
        Grad := pS^[x] - pS^[x-1];
        if (Grad < 0)
        then Grad := -Grad;
        Sum_Grad := Sum_Grad + Grad;
        Sum_Edge := Sum_Edge + Grad * pS^[x];
     end;
  end;
  if (Sum_Grad <> 0.0)
  then Threshold := Sum_Edge / Sum_Grad
  else Threshold := 0.0;
  Result := Threshold;
end; // TmcmImageColor.ThresholdEdge.


function TmcmImageColor.ThresholdOptimized(xs, ys : longword;
                                           xe, ye : longword) : double;
var Max, Min       : double;
    Mean1, Mean2   : double;
    Var1, Var2     : double;
    LowThresh      : double;
    Score          : double;
    LowScore       : double;
    Threshold      : double;
    Count1, Count2 : integer;
    i, x, y        : integer;
    dxMuldy        : longint;
    pS             : PVectorB;
begin
  pS := FSrcImage[0].ScanLine[ys];
  LowScore := 0.0;
  dxMuldy  := ((ye - ys) * (xe - xs));

  // Get max and min intensity value (index).
  Max := pS^[xs];
  Min := pS^[xs];
  for y := ys to (ye - 1)
  do begin
     pS := FSrcImage[0].ScanLine[y];
     for x := xs to (xe - 1)
     do begin
        if (pS^[x] > Max)
        then Max := pS^[x]
        else if (pS^[x] < Min)
             then Min := pS^[x];
     end;
  end;
  LowThresh := 0;

  // Search for optimal threshold.
  for i := 1 to 9
  do begin
     // Calculate means and variance of two classes
     Threshold := Min + i * (Max - Min) / 10;
     Var1      := 0.0;
     Var2      := 0.0;
     Mean1     := 0.0;
     Mean2     := 0.0;
     Count1    := 1;
     Count2    := 1;

     for y := ys to (ye - 1)
     do begin
        pS := FSrcImage[0].ScanLine[y];
        for x := xs to (xe - 1)
        do begin
           if (pS^[x] < Threshold)
           then begin
                inc(Count1);
                Mean1 := Mean1 + pS^[x];
                Var1  := Var1  + pS^[x] * pS^[x];
           end
           else begin
                inc(Count2);
                Mean2 := Mean2 + pS^[x];
                Var2  := Var2  + pS^[x] * pS^[x];
           end;
        end;
     end;

     Mean1 := Mean1 / Count1;
     Mean2 := Mean2 / Count2;
     Var1  := (Var1 / Count1) - (Mean1 * Mean1);
     Var2  := (Var2 / Count2) - (Mean2 * Mean2);

     // See if this is the best score.
     Score := (Count1 * Var1 + Count2 * Var2) / dxMuldy;

     if ((i = 1) or (Score < LowScore))
     then begin
          LowScore  := Score;
          LowThresh := Threshold;
     end;
  end;

  // Set optimal threshold.
  Threshold := LowThresh;
  Result := Threshold;
end; // TmcmImageColor.ThresholdOptimized.


procedure TmcmImageColor.Threshold(Method : TmcmThreshold; var Level : word; Col : word; Row : word);
var i, j            : integer;
    xDim, yDim      : integer;
    StepX, StepY    : integer;
    endX, endY      : integer;
    x, y            : integer;
    dx, dy, rx, ry  : integer;
    dxDiv2          : integer;
    dyDiv2          : integer;
    Thresholds      : PMatrixD;
    Threshold       : double;
    Bits            : byte;
    BitIndex        : byte;
    r               : longword;
    TopThreshold    : double;
    BottomThreshold : double;
    pS, pR          : PVectorB;
begin
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_BW;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            case Method of
            TH_LEVEL      : ThresholdLevel(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
            TH_ISODATA    : ThresholdIsoData(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
            TH_SYMMETRY   : ThresholdSymmetry(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
            TH_TRACE      : ThresholdTrace(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
            TH_TRIANGULAR : ThresholdTriangular(Level, 0, 0, FSrcImage[0].Width, FSrcImage[0].Height);
            else begin // Segmented threshold.
                 if (Col > 128)
                 then Col := 128;
                 if (Row > 128)
                 then Row := 128;

                 xDim := FSrcImage[0].Width;
                 yDim := FSrcImage[0].Height;

                 // Check that x sub-divisions are less than half width.
                 if (Col > xDim div 2)
                 then Col := xDim div 2;

                 // Check that y sub-divisions are less than half height.
                 if (Row > yDim div 2)
                 then Row := yDim div 2;

                 GetMem(Thresholds, Row * SizeOf(PVectorD));
                 for i := 0 to (Row - 1)
                 do GetMem(Thresholds^[i], Col * SizeOf(double));

                 // Determine region size.
                 dx := xDim div Col;
                 dy := yDim div Row;

                 // Determine the remainder of region size determination.
                 rx := xDim - dx * Col;
                 ry := yDim - dy * Row;

                 // Adjust the remainders.
                 if (dx mod 2 <> 0)
                 then inc(rx);
                 if (dy mod 2 <> 0)
                 then inc(ry);
                 j := 0;


                 // Threshold each partition of the image.
                 y := 0;
                 while ((y + dy) <= yDim)
                 do begin
                    i := 0;
                    x := 0;
                    while ((x + dx) <= xDim)
                    do begin
                       // Determine the last column of a partition.
                       if ((x + 2 * dx) > xDim)
                       then endX := xDim
                       else endX := x + dx;

                       // Determine the last row of a partition.
                       if ((y + 2 * dy) > yDim)
                       then endY := yDim
                       else endY := y + dy;

                       if (i < 128) and (j < 128)
                       then begin
                            case Method of
                            TH_STATISTIC : Thresholds^[j]^[i] := ThresholdStatistic(x, y, endX, endY);
                            TH_EDGE      : Thresholds^[j]^[i] := ThresholdEdge(x, y, endX, endY);
                            TH_OPTIMIZED : Thresholds^[j]^[i] := ThresholdOptimized(x, y, endX, endY);
                            // NOT USED - TH_LEVEL     : Thresholds^[j]^[i] := Level;
                            end;
                       end;
                       inc(i);
                       x := x + dx;
                    end;
                    inc(j);
                    y := y + dy;
                 end;

                 // Perform actual threshold.
                 dxDiv2 := dx div 2;
                 dyDiv2 := dy div 2;

                 for y := 0 to (yDim - 1)
                 do begin
                    pS := FSrcImage[0].ScanLine[y];
                    pR := FResImage.ScanLine[y];

                    r := 0;
                    Bits := 0;
                    BitIndex := $80;

                    for x := 0 to (xDim - 1)
                    do begin
                       // Y - Top
                       if (y < dyDiv2)
                       then begin
                            // X - Left
                            if (x < dxDiv2)
                            then Threshold := Thresholds^[0]^[0]
                            else begin
                                 // X - Right
                                 if (x >= (xDim - dxDiv2 - rx))
                                 then Threshold := Thresholds^[0]^[Col-1]
                                 else begin
                                      // X - Centre
                                      i     := (x - dxDiv2) div dx;
                                      StepX := x - dxDiv2 - (dx * i) + 1;
                                      Threshold := Thresholds^[0]^[i] + StepX * ((Thresholds^[0]^[i+1] - Thresholds^[0]^[i]) / dx);
                                 end;
                            end;
                       end
                       else begin
                            // Y - Bottom
                            if (y >= (yDim - dyDiv2 - ry))
                            then begin
                                 // X - Left
                                 if (x < dxDiv2)
                                 then Threshold := Thresholds^[Row-1]^[0]
                                 else begin
                                      // X - Right
                                      if (x >= (xDim - dxDiv2 - rx))
                                      then Threshold := Thresholds^[Row-1]^[Col-1]
                                      else begin
                                           // X - Centre
                                           i     := (x - dxDiv2) div dx;
                                           StepX := x - dxDiv2 - (dx * i) + 1;
                                           Threshold := Thresholds^[Row-1]^[i] + StepX * ((Thresholds^[Row-1]^[i+1] - Thresholds^[Row-1]^[i]) / dx);
                                      end;
                                 end;
                            end
                            // Y - Centre
                            else begin
                                 // X - Left
                                 if (x < dxDiv2)
                                 then begin
                                      i := (y - dyDiv2) div dy;
                                      StepY := y - dyDiv2 - (dy * i) + 1;
                                      Threshold := Thresholds^[i]^[0] + StepY * ((Thresholds[i+1]^[0] - Thresholds^[i]^[0]) / dy);
                                 end
                                 else begin
                                      // X - Right
                                      if (x >= (xDim - dxDiv2 - rx))
                                      then begin
                                           i     := (y - dyDiv2) div dy;
                                           StepY := y - dyDiv2 - (dy * i) + 1;
                                           Threshold := Thresholds^[i]^[Col-1] + StepY * ((Thresholds^[i+1]^[Col-1] - Thresholds^[i]^[Col-1]) / dy);
                                      end
                                      else begin
                                           // X - Centre
                                           i     := (x - dxDiv2) div dx;
                                           j     := (y - dyDiv2) div dy;
                                           StepX := x - dxDiv2 - (dx * i) + 1;
                                           StepY := y - dyDiv2 - (dy * j) + 1;

                                           TopThreshold    := Thresholds^[j]^[i] + StepX * ((Thresholds^[j]^[i+1] - Thresholds^[j]^[i]) / dx);
                                           BottomThreshold := Thresholds^[j+1]^[i] + StepX * ((Thresholds^[j+1]^[i+1] - Thresholds^[j+1]^[i]) / dx);
                                           Threshold := TopThreshold + StepY * ((BottomThreshold - TopThreshold) / dy);
                                      end;
                                 end;
                            end;
                       end;

                       case FResImage.BitCount of
                       1 : begin
                             if (pS[x] > Threshold)
                             then Bits := Bits or BitIndex;
                             if (BitIndex = $01) // 8 pixels has been tested, save result.
                             then begin
                                  pR[r] := Bits;
                                  inc(r);
                                  Bits := 0;
                                  BitIndex := $80;
                             end
                             else BitIndex := BitIndex shr 1;
                           end;
                       8 : if (pS^[x] < Threshold)
                           then pR^[x] := 0
                           else pR^[x] := 255;
                       end;
                    end;

                    // Set last byte, if Target is 1 bit.
                    if (FResImage.BitCount = 1)
                    then begin
                         if (BitIndex <> $80)
                         then pR[r] := Bits;
                    end;
                 end;

                 for i := (Row - 1) downto 0
                 do FreeMem(Thresholds^[i], Col * SizeOf(double));
                 FreeMem(Thresholds, Row * SizeOf(PVectorD));
            end;
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageColor.Threshold.


procedure TmcmImageColor.ThresholdColorBand(Color : TColorRef; Tolerance : word);
var pS, pR : PVectorB;
    xs, ys : longint;
    xe, ye : longint;
    x, y   : longint;
    rgbdx  : longint;
    rgbx   : longint;
    Red    : integer;
    Green  : integer;
    Blue   : integer;
    Value  : integer;
    MaxVal : integer;
begin
  FError := EC_OK;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if Not(Assigned(FResImage))
       then begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := FSrcImage[0].Width;
            FResImage.Height      := FSrcImage[0].Height;
            FResImage.ImageFormat := IF_GREY8;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.CreateGreyPalette;
       end;

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then begin
                 FError := EC_NOMEMORY;
                 Exit;
            end;

            try
              xs := 0;
              ys := 0;
              xe := FSrcImage[0].Width;
              ye := FSrcImage[0].Height;

              // TColorRef = 0x00bbggrr
              Red   := Color and $FF;
              Green := (Color shr 8) and $FF;
              Blue  := (Color shr 16) and $FF;
              MaxVal := Sqr(Tolerance);

              case FSrcImage[0].ImageFormat of
              IF_GREY8,
              IF_PAL8   : begin
                            with FSrcImage[0].DibInfo^
                            do begin
                               for y := ys to (ye - 1)
                               do begin
                                  pS := FSrcImage[0].ScanLine[y];
                                  pR := FResImage.ScanLine[y];
                                  for x := xs to (xe - 1)
                                  do begin
                                     Value := sqr(Blue - bmiColors[pS[x]].rgbBlue);
                                     Value := Value + sqr(Green - bmiColors[pS[x]].rgbGreen);
                                     Value := Value + sqr(Red - bmiColors[pS[x]].rgbRed);

                                     if (Value < MaxVal)
                                     then pR[x] := 255
                                     else pR[x] := 0;
                                  end;
                               end;
                            end;
                          end;
              //IF_RGB15
              //IF_RGB16
              IF_RGB24,
              IF_RGBA32 : begin
                            if (FSrcImage[0].ImageFormat = IF_RGB24)
                            then rgbdx := 0
                            else rgbdx := 1;

                            for y := ys to (ye - 1)
                            do begin
                               rgbx := xs * rgbdx;

                               pS := FSrcImage[0].ScanLine[y];
                               pR := FResImage.ScanLine[y];
                               for x := xs to (xe - 1)
                               do begin
                                  Value := sqr(Blue - pS[rgbx]);
                                  inc(rgbx);
                                  Value := Value + sqr(Green - pS[rgbx]);
                                  inc(rgbx);
                                  Value := Value + sqr(Red - pS[rgbx]);
                                  inc(rgbx);

                                  if (Value < MaxVal)
                                  then pR[x] := 255
                                  else pR[x] := 0;

                                  rgbx := rgbx + rgbdx;
                               end;
                            end;
                          end;
              // Error - Image color format cannot be applied in this algorithme.
              else FError := EC_BADCOLORFORMAT;
              end;
            finally
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageColor.ThresholdColorBand.


//------------------------------------------------------------------------------
// Helper functions.
//------------------------------------------------------------------------------

procedure RGBToHSI(const R, G, B : integer; var H, S, I : integer);
var Hd, Sd, Id : double;
    N, ND      : double;
    b0, g0     : double;
    minVal     : byte;
begin
  // Calculate Intensity value.
  Id := 1.0 * (R + G + B) / 3.0;

  // Calculate Saturation value.
  if (Id <> 0.0)
  then begin
       if (R < G)
       then begin
            if (R < B)
            then minVal := R
            else minVal := B;
       end
       else begin
            if (G < B)
            then minVal := G
            else minVal := B;
       end;
       Sd := 255.0 - ((255.0 * minVal) / Id);
  end
  else Sd := 0.0;

  // Calculate Hue value.
  if (Sd <> 0.0)
  then begin
       N  := 0.5 * ((R - G) + (R - B));
       ND := ((R - G) * (R - G)) + ((R - B) * (G - B));
       if (ND > 0.0)
       then begin
            ND := Sqrt(ND);
            N := N / ND;
            if (-1.0 <= N) and (N <= 1.0)
            then begin
                 Hd := 180.0 * ArcCos(N) / pi;
            end
            else Hd := 0;
            b0 := B / Id;
            g0 := G / Id;
            if (b0 > g0)
            then Hd := 360.0 - Hd;
       end
       else Hd := 0.0;
  end
  else Hd := 0.0;

  H := Round(Hd);
  S := Round(Sd);
  I := Round(Id);
end; // RGBToHSI.


procedure HSIToRGB(const H, S, I : integer; var R, G, B : integer);
var r0, g0, b0 : double;
    Hd, Sd     : double;
    DegToRad   : double;
    rad60      : double;
    rad120     : double;
    rad240     : double;
    Hue        : integer;
begin
  rad60    := pi / 3.0;
  rad120   := 2.0 * rad60;
  rad240   := 2.0 * rad120;
  DegToRad := pi / 180.0;
  if (H = 0)
  then Hue := 360
  else Hue := H;
  Hd := DegToRad * Hue;
  Sd := (1.0 * S) / 255.0;
  case H of
  1..120   : begin
               b0 := (1.0 - Sd) / 3.0;
               r0 := (1.0 + (Sd * cos(Hd)) / cos(rad60 - Hd)) / 3.0;
               g0 := 1.0 - (r0 + b0);
             end;
  121..240 : begin
               Hd := Hd - rad120;
               r0 := (1.0 - Sd) / 3.0;
               g0 := (1.0 + (Sd * cos(Hd)) / cos(rad60 - Hd)) / 3.0;
               b0 := 1.0 - (r0 + g0);
             end;
  241..360 : begin
               Hd := Hd - rad240;
               g0 := (1.0 - Sd) / 3.0;
               b0 := (1.0 + (Sd * cos(Hd)) / cos(rad60 - Hd)) / 3.0;
               r0 := 1.0 - (g0 + b0);
             end
  else       begin
               r0 := 0.0;
               g0 := 0.0;
               b0 := 0.0;
             end;
  end;
  R := Round(r0 * I * 3.0{ * 255.0});
  G := Round(g0 * I * 3.0{ * 255.0});
  B := Round(b0 * I * 3.0{ * 255.0});
  if (R > 255)
  then R := 255;
  if (G > 255)
  then G := 255;
  if (B > 255)
  then B := 255;
end; // HSIToRGB.


procedure HSVToRGB(const H, S, V : integer; var R, G, B : integer);
// Input
// R - 0..255, Red channel
// G - 0..255, Green channel
// B - 0..255, Blue channel
// Output
// H - 0..359, Hue
// S - 0..255, shade of grey, where S = 255 is pure color.
// V - 0..255, Value = color brightness
const Scale : integer = 15300; // = 255 * 60;
var
  HueGroup : integer;
  HueSub   : integer;
  VS       : integer;
  rgb0     : integer;
  rgb1     : integer;
  rgb2     : integer;
begin
  if (S = 0)
  then begin // Achromatic, that is we have a grey shade.
       R := V;
       G := V;
       B := V;
  end
  else begin // Chromatic color.
       if (H = 360)
       then HueGroup := 0
       else HueGroup := H;

       HueSub   := HueGroup mod 60; // 0 <= HueSub < 60
       HueGroup := HueGroup div 60; // 0 <= HueGroup < 6

       VS := V * S;
       rgb0 := V - VS div 255;
       rgb1 := V - (VS * HueSub) div Scale;
       rgb2 := V - (VS * (60 - HueSub)) div Scale;

       case HueGroup of
       0 : begin
             R := V;
             G := rgb2;
             B := rgb0;
           end;
       1 : begin
             R := rgb1;
             G := V;
             B := rgb0;
           end;
       2 : begin
             R := rgb0;
             G := V;
             B := rgb2;
           end;
       3 : begin
             R := rgb0;
             G := rgb1;
             B := V;
           end;
       4 : begin
             R := rgb2;
             G := rgb0;
             B := V;
           end;
       5 : begin
             R := V;
             G := rgb0;
             B := rgb1;
           end;
       else begin
             // Avoid compiler warning.
             R := 0;
             G := 0;
             B := 0;
           end;
       end;
  end;
end; // HSVtoRGB


procedure RGBToHSV(const R, G, B : integer; var H, S, V : integer);
// Input
// H - 0..359, Hue
// S - 0..255, shade of grey, where S = 255 is pure color.
// V - 0..255, Value = color brightness
// Output
// R - 0..255, Red channel
// G - 0..255, Green channel
// B - 0..255, Blue channel
var Delta  : integer;
    MinRGB : integer;
begin
  // Calculate Value.
  MinRGB := MinIntValue([R, G, B]);
  V      := MaxIntValue([R, G, B]);
  Delta  := V - MinRGB;

  // Calculate Saturation.
  if (V = 0)
  then S := 0
  else S := MulDiv(Delta, 255, V);

  // Calculate Hue.
  if (S = 0)
  then H := 0 // Achromatic
  else begin // Chromatic
       if (R = V)
       then // Degree is between Yellow and Magenta
            H := MulDiv(G - B, 60, Delta)
       else if (G = V)
            then // Degree is between Cyan and Yellow
                 H := 120 + MulDiv(B - R, 60, Delta)
            else if (B = V)
                 then // Degree is between Magenta and Cyan
                      H := 240 + MulDiv(R - G, 60, Delta);
       // Adjust Hue to: 0 <= H < 360
       if (H < 0)
       then H := H + 360;
 end;
end; // RGBToHSV.


procedure HSLtoRGB(const H, S, L : integer; var R, G, B : integer);
var m1 : double;
    m2 : double;

    function Value(const n1, n2 : double; Hue : integer) : integer;
    begin
      if (Hue > 360)
      then Hue := Hue - 360
      else if (Hue < 0)
           then Hue := Hue + 360;

      if (Hue < 60)
      then Result := Round(255.0 * (n1 + (n2 - n1) * Hue / 60.0))
      else if (Hue < 180)
           then Result := Round(255.0 * n2)
           else if (Hue < 240)
                then Result := Round(255.0 * (n1 + (n2 - n1) * (240.0 - Hue) / 60.0))
                else Result := Round(255.0 * n1);
    end; // Value.

begin
  if (S = 0)
  then begin // Achromatic, no hue
       if true // IsNAN(H)
       then begin
            R := L;
            G := L;
            B := L
       end
       else ; //Raise EColorError.Create('HLStoRGB:  S = 0 and H has a value');
  end
  else begin
       // Chromatic, there is a hue.
       if (L < 128)
       then m2 := (L * (L + S)) / 65025.0
       else m2 := (L + S - (L * S) / 255.0) / 255.0;
       m1 := (L / 127.5) - m2;

       R := Value(m1, m2, H + 120);
       G := Value(m1, m2, H);
       B := Value(m1, m2, H - 120)
  end;
end; // HSLToRGB.


function MinOfRGB(R, G, B : integer) : integer;
begin
  if (R < G)
  then begin
       if (R < B)
       then Result := R
       else Result := B;
  end
  else begin
       if (G < B)
       then Result := G
       else Result := B;
  end;
end; // MinOfRGB.


function MaxOfRGB(R, G, B : integer) : integer;
begin
  if (R > G)
  then begin
       if (R > B)
       then Result := R
       else Result := B;
  end
  else begin
       if (G > B)
       then Result := G
       else Result := B;
  end;
end; // MaxOfRGB.


procedure RGBToHSL(const R, G, B : integer; var H, S, L : integer);
var Delta  : integer;
    MaxRGB : integer;
    MinRGB : integer;
begin
  MaxRGB := MaxOfRGB(R, G, B);
  MinRGB := MinOfRGB(R, G, B);

  L := (MaxRGB + MinRGB) div 2; // Lightness

  if (MaxRGB = MinRGB) // Achromatic as R = G = B
  then begin
       S := 0;
       H := 0; // Is actually undefined.
  end
  else begin
       Delta := MaxRGB - MinRGB;
       if (L <= 128)
       then S := Delta div (MaxRGB + MinRGB)
       else S := Delta div (2 - (MaxRGB + MinRGB));

       if (R = MaxRGB)
       then // Degree is between Yellow and Magenta
            H := (60 * (G - B)) div Delta
       else if (G = MaxRGB)
            then // Degree is between Cyan and Yellow
                 H := 120 + (60 * (B - R)) div Delta
            else if (B = MaxRGB)
                 then // Degree is between Magenta and Cyan
                      H := 240 + (60 * (R - G)) div Delta;
       // Adjust Hue to: 0 <= H < 360
       if (H < 0)
       then H := H + 360;
  end;
end; // RGBToHSL.

{$UNDEF DCB3}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.

