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
// $Log:  17551: mcmImageKernel.pas 
//
//    Rev 1.13    2014-02-02 21:10:02  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.12    20/05/2006 11:59:18  mcm
// Fixed memory leak in FreeLines.
//
//   Rev 1.11    15-05-2005 19:42:00  mcm    Version: IMG 2.9
// Added check for SSE instruction set capable processors.

//
//   Rev 1.10    28-10-2004 19:18:44  mcm    Version: IMG 2.6
// Improved CheckResult to support better auto-creation of ResultImage or
// correct ResultImage if dimensions does not match the required.

//
//   Rev 1.9    02-01-2004 11:28:08  mcm
// Added detection for MMX support.

//
//   Rev 1.8    29-09-2003 18:44:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.7    25-09-2003 23:34:18  mcm    Version: IMG 1.5
// Added EC_OK in Create method.

//
//   Rev 1.6    25-07-2003 00:14:00  mcm
// Modified parameter names in AllocLines.
// Added validation check of Image parameter in SetSourceImage.

//
//   Rev 1.5    05-06-2003 21:43:18  mcm    Version: IMG 1.3.4
// Added RotateIndex for calculation of indexes to rotating image line buffers.

//
//   Rev 1.4    12-05-2003 16:10:48  mcm    Version: IMG 1.3.4
// Added protected methods to check source and result image.
// Added a protected method to allocate work buffer. 

//
//   Rev 1.3    11-02-2003 12:27:42  mcm    Version: IMG 1.3
// Made FError public

//
//   Rev 1.2    05-02-03 16:23:22  mcm
// Added detection of MMX support by Pentium processors.

//
//   Rev 1.1    27-01-2003 14:25:44  mcm
// Added FError to protected section.

//
//   Rev 1.0    27-05-2002 16:22:06  mcm

unit mcmImageKernel;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage;


const MaxSourceImage = 5;
type
  TRMatrixB    = array[0..127] of PVectorB;
  PRMatrixB    = ^TRMatrixB;

  TKernelImage = class(TmcmImage)
  public
    property    DibInfo;
  end;

  TmcmImageKernel = class(TComponent)
  private
    // Private declarations
  protected
    // Protected declarations
    FMMX        : boolean; // Indicated if MMX is supported by the processor.
    FSSE       : boolean; // Indicated if SSE is supported by the processor.
//    FSSE2      : boolean; // Indicated if SSE2 is supported by the processor.
    FError      : TmcmErrorCode;
    FLines      : PRMatrixB;
    FSrcHeight  : longint;
    FSrcWidth   : longint;
    FSrcImage   : array[0..MaxSourceImage-1] of TKernelImage; // Source images.
    FResImage   : TKernelImage; // Result image
    function    AllocLines(NoPixels, NoLines : cardinal) : boolean;
    function    CheckResult(ImageFormat : TmcmImageFormat; Width, Height : longint; CopyPal : boolean) : boolean;
    function    CheckSource(Index : word; ImageFormat : TmcmImageFormats) : boolean;
    procedure   FreeLines(Pixels, Lines : cardinal);
    function    GetSourceImage(Index : word) : TmcmImage;
    procedure   SetSourceImage(Index : word; Image : TmcmImage); virtual;
    function    GetResImage : TmcmImage;
    procedure   SetResImage(Value : TmcmImage);
    function    RotateIndex(Index, MaxIndex : integer) : integer;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    property    Error : TmcmErrorCode
      read      FError;
    property    ResultImage : TmcmImage
      read      GetResImage
      write     SetResImage;
    property    SourceImage[Index : word] : TmcmImage
      read      GetSourceImage
      write     SetSourceImage;
  published
    // Published declarations
  end;


implementation

uses {$IFNDEF GE_DXE2}
      SysUtils;
     {$ELSE}
      System.SysUtils;
     {$ENDIF}

constructor TmcmImageKernel.Create(AOwner : TComponent);
var i : word;
    bMMX : boolean;
begin
  Inherited Create(AOwner);
  FError := EC_OK;
  FLines := Nil;
  
  // Test for MMX support
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

  // Test for SSE support
  FSSE := False;
  try
    asm
      mov   bMMX,$00
      mov   eax,1
      {$IFNDEF DCB3_5}
      cpuid
      {$ELSE}
      dw    CPUID
      {$ENDIF}
      test edx,$2000000
      jz    @NoSSE
      mov   bMMX,$01
      @NoSSE:
    end;
  except
  // do nothing.
  end;
  FSSE := bMMX;

  for i := 0 to (MaxSourceImage - 1)
  do FSrcImage[i] := Nil;
  FResImage := Nil;
end; // TmcmImageKernel.Create.


destructor  TmcmImageKernel.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageKernel.Destroy.


procedure TmcmImageKernel.Clear;
var i : integer;
begin
  for i := 0 to (MaxSourceImage - 1)
  do FSrcImage[i] := Nil;
  FResImage := Nil;
end; // TmcmImageKernel.Clear.


function TmcmImageKernel.CheckResult(ImageFormat : TmcmImageFormat; Width, Height : longint; CopyPal : boolean) : boolean;
// 1. Checks if the result image is assigned, and if not create an instance,
//    using the ImageFormat, Heigh and Width parameters.
// 2. Checks that the ImageFormat, Heigh and Width are as specified, otherwise
//    adjust these parameters.
begin
  if (FResImage <> FSrcImage[0])
  then begin
       if Assigned(FResImage)
       then begin
            FResImage.ImageFormat := ImageFormat;
            FResImage.Width := Width;
            FResImage.Height := Height;
       end
       else begin
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.Width       := Width;
            FResImage.Height      := Height;
            FResImage.ImageFormat := ImageFormat;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
       end;

       if Assigned(FResImage)
       then begin
            if CopyPal
            then if (FSrcImage[0].Palette <> 0) and (FResImage <> FSrcImage[0])
                 then FResImage.Palette := FSrcImage[0].Palette
                 else begin
                      case FResImage.ImageFormat of
                      IF_GREY8 : ;
                      end;
                 end;
       end;
  end;
  Result := (FResImage <> Nil);
end; // TmcmImageKernel.CheckResult.


function TmcmImageKernel.CheckSource(Index : word; ImageFormat : TmcmImageFormats) : boolean;
// Checks the source image for being assigned, empty and having the correct
// color format. 
begin
  if Assigned(FSrcImage[Index])
  then begin
       if (FSrcImage[Index].Empty)
       then FError := EC_MISSOURCEIMAGE // No valid source image.
       else if (ImageFormat <> [IF_NONE]) // If IF_NONE process doesn't care!
            then if Not(FSrcImage[Index].ImageFormat in ImageFormat)
                 then FError := EC_BADCOLORFORMAT; // Color/bit depth is incorrect.
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
  Result := (FError = EC_OK);
end; // TmcmImageKernel.CheckSource.


function TmcmImageKernel.RotateIndex(Index, MaxIndex : integer) : integer;
asm
  mov eax,Index
  inc eax
  cmp eax,MaxIndex
  jl  @Done
  xor eax,eax
  @Done:
end; // TmcmImageKernel.RotateIndex.


function TmcmImageKernel.AllocLines(NoPixels, NoLines : cardinal) : boolean;
var i : cardinal;
begin
  try
    GetMem(FLines, NoLines * SizeOf(PVectorB));
    for i := 0 to (NoLines - 1)
    do begin
       FLines[i] := Nil;
       GetMem(FLines[i], NoPixels);
    end;
  except
    On E:EOutOfMemory
    do FError := EC_NOMEMORY;
    On E:Exception
    do FError := EC_UNKNOWN;
  end;
  Result := (FError = EC_OK);
end; // TmcmImageKernel.AllocLines.


procedure TmcmImageKernel.FreeLines(Pixels, Lines : cardinal);
var i : cardinal;
begin
  if Assigned(FLines)
  then begin
       for i := (Lines - 1) downto 0
       do if Assigned(FLines[i])
          then FreeMem(FLines[i]);
       FreeMem(FLines, Lines * SizeOf(PVectorB));
       FLines := Nil;
  end;
end; // TmcmImageKernel.FreeLines.


function TmcmImageKernel.GetSourceImage(Index : word) : TmcmImage;
begin
  if (Index < MaxSourceImage)
  then Result := FSrcImage[Index]
  else Result := Nil;
end; // TmcmImageKernel.GetSourceImage.


procedure TmcmImageKernel.SetSourceImage(Index : word; Image : TmcmImage);
begin
  if (Index < MaxSourceImage)
  then begin
       FSrcImage[Index] := TKernelImage(Image);
       if (Index = 0) and Assigned(Image)
       then begin
            FSrcHeight := Image.Height;
            FSrcWidth  := Image.Width;
       end;
  end;
end; // TmcmImageKernel.SetSourceImage.


function TmcmImageKernel.GetResImage : TmcmImage;
begin
  Result := FResImage;
end; // TmcmImageKernel.GetResImage.


procedure TmcmImageKernel.SetResImage(Value : TmcmImage);
begin
  FResImage := TKernelImage(Value);
end; // TmcmImageKernel.SetResImage.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
