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
// $Log:  19045: mcmImageMath.pas
//
//    Rev 1.7    2014-02-02 21:10:04  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.6    23-05-2005 22:00:04  mcm    Version: IMG 2.9

//
//   Rev 1.5    28-10-2004 19:20:38  mcm    Version: IMG 2.6
// Modified Execute method to improve checking and auto-creation of ResultImage.

//
//   Rev 1.4    29-09-2003 18:44:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.3    06-07-2003 10:46:48  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.2    11-02-2003 12:28:22  mcm    Version: IMG 1.3
// Implemented Mul & DivImages

//
//   Rev 1.1    05-02-03 16:25:28  mcm
// Added/replaced Delphi code with assembler/MMX code. 

//
//   Rev 1.0    29-01-2003 16:17:18  mcm
// Initial revision

unit mcmImageMath;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}
// NOTE: DELPHI 3 cannot debug through MMX code ! resulting image becomes
//       incorrect. Further $O must be ON
{$IFOPT O-} {$DEFINE OPTIMIZATION_ON} {$O+} {$ENDIF}
{$IFOPT A-} {$DEFINE ALIGN_ON} {$A+} {$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Classes, SysUtils,
     {$ELSE}
      WinApi.Windows, System.Classes, System.SysUtils,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmImageKernel;

type
  TmcmImageMath = class(TmcmImageKernel)
  private
    // Private declarations
    FScale : array[0..MaxSourceImage-1] of double;
  protected
    // Protected declarations
    function    GetScale(Index : word) : double;
    procedure   SetScale(Index : word; Value : double);
    procedure   AddImages;
    procedure   AndImages;
    procedure   AndBitwiseImages;
    procedure   AverageImages;
    procedure   BlendImages;
    procedure   DiffImages;
    procedure   DivImages;
    procedure   EqualImages;
    procedure   GreaterImages;
    procedure   MagnitudeImages;
    procedure   MulImages;
    procedure   OrientateImages;
    procedure   OrImages;
    procedure   SubImages;
    procedure   XorImages;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Execute(Method : TmcmImageMathematics) : TmcmImage;

    property    Scale[Index : word] : double
      read      GetScale
      write     SetScale;
  published
    // Published declarations
  end;

implementation

uses {$IFNDEF GE_DXE2}
      Math;
     {$ELSE}
      System.Math;
     {$ENDIF}

constructor TmcmImageMath.Create(AOwner : TComponent);
var i : integer;
begin
  Inherited Create(AOwner);
  for i := 0 to (MaxSourceImage - 1)
  do FScale[i] := 1.0;
end; // TmcmImageMath.Create.


destructor TmcmImageMath.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageMath.Destroy.


function TmcmImageMath.GetScale(Index : word) : double;
begin
  if (Index < MaxSourceImage)
  then Result := FScale[Index]
  else Result := 1.0;
end; // TmcmImageMath.GetScale.


procedure TmcmImageMath.SetScale(Index : word; Value : double);
begin
  if (Value < 0)
  then Value := 0
  else if (Value > 255)
       then Value := 255; 
  if (Index < MaxSourceImage)
  then FScale[Index] := Value;
end; // TmcmImageMath.SetScale.


procedure TmcmImageMath.AddImages;
var Count    : longint;
    i, y     : longint;
    Value    : integer;
    Height   : longint;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         paddusb mm0,qword ptr [edx+ecx*8] // Add unsigned bytes with saturation.
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$DC,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         add   al,[edx+ecx]
         jnc   @NoSat
         mov   al,$FF
         @NoSat:
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             Value := pS1^[i] + pS0^[i];
             if (Value > 255)
             then pR^[i] := 255
             else pR^[i] := Value;
          end;
       end;
  end;
end; // TmcmImageMath.AddImages.


procedure TmcmImageMath.AndImages;
var Count     : longint;
    i, y      : longint;
    Height    : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         {$IFNDEF DCB3_5}
         pxor    mm7,mm7       // mm7 = zero reg.
         {$ELSE}
         db $0F,$EF,$FF
         {$ENDIF}

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         movq    mm1,mm0
         pcmpeqb mm1,qword ptr [edx+ecx*8]
         pcmpeqb mm0,mm7
         pandn   mm0,mm1
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$C8
         db    $0F,$74,$0C,$CA
         db    $0F,$74,$C7
         db    $0F,$DF,$C1
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         cmp   al,[edx+ecx]
         jne   @DiffValue      // Go to DiffValue if values are different
         test  al,al
         jz    @DiffValue      // Go to DiffValue if values are zero
         {$IFNDEF DCB3_5}
         mov   [edi+ecx],$FF   // Values are equal and non-zero
         {$ELSE}
         mov   al,$FF
         mov   [edi+ecx],al
         {$ENDIF}
         inc   ecx
         dec   ebx
         jns   @LoopRemain
         jmp   @EndOfImage

         @DiffValue:           // Values are not equal or zero
         {$IFNDEF DCB3_5}
         mov   [edi+ecx],0
         {$ELSE}
         xor   al,al
         mov   [edi+ecx],al
         {$ENDIF}
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             if (pS1^[i] = pS0^[i]) and (pS0^[i] > 0)
             then pR^[i] := 255
             else pR^[i] := 0;
          end;
       end;
  end;
end; // TmcmImageMath.AndImages.


procedure TmcmImageMath.AndBitwiseImages;
var Count     : longint;
    i, y      : longint;
    Height    : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq  mm0,qword ptr [esi+ecx*8]
         pand  mm0,qword ptr [edx+ecx*8]
         movq  qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$DB,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         and   al,[edx+ecx]
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do pR^[i] := pS1^[i] and pS0^[i];
       end;
  end;
end; // TmcmImageMath.AndBitwiseImages.


procedure TmcmImageMath.AverageImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         {$ELSE}
         db $0F,$EF,$FF
         {$ENDIF}

         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq      mm0,qword ptr [esi+ecx*8]
         movq      mm2,qword ptr [edx+ecx*8]

         movq      mm1,mm0     // copy
         punpcklbw mm0,mm7     // unpack low 4 bytes to words
         punpckhbw mm1,mm7     // unpack high 4 bytes to words

         movq      mm3,mm2
         punpcklbw mm2,mm7
         punpckhbw mm3,mm7

         paddw     mm0,mm2
         paddw     mm1,mm3

         psrlw     mm0,1
         psrlw     mm1,1
         packuswb  mm0,mm1
         movq      qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$14,$CA

         db    $0F,$6F,$C8
         db    $0F,$60,$C7
         db    $0F,$68,$CF

         db    $0F,$6F,$DA
         db    $0F,$60,$D7
         db    $0F,$68,$DF

         db    $0F,$FD,$C2
         db    $0F,$FD,$CB

         db    $0F,$71,$D0,$01
         db    $0F,$71,$D1,$01
         db    $0F,$67,$C1
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         add   al,[edx+ecx]
         jnc   @NoSat
         mov   ah,$01
         @NoSat:
         shr   ax,1
         mov   [edi+ecx],al
         inc   ecx
         dec   bl
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do pR^[i] := (pS1^[i] + pS0^[i]) shr 1;
       end;
  end;
end; // TmcmImageMath.AverageImages.


procedure TmcmImageMath.BlendImages;
var i, Count : longint;
    y        : longint;
    Value    : integer;
    Height   : longword;
    Scale1   : int64;
    Scale2   : int64;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;

       {$IFNDEF DCB3}
       Scale1 := Round(FScale[0] * 1.28);
       Scale2 := Round(FScale[1] * 1.28);
       for i := 0 to 3
       do begin
          Scale1 := Scale1 or (Scale1 shl (16 * i));
          Scale2 := Scale2 or (Scale2 shl (16 * i));
       end;
       {$ELSE}
       Scale1.LowPart := Round(FScale[0] * 1.28);
       Scale1.LowPart := Scale1.LowPart or (Scale1.LowPart shl 16);
       Scale1.HighPart := Scale1.LowPart;
       Scale2.LowPart := Round(FScale[1] * 1.28);
       Scale2.LowPart := Scale2.LowPart or (Scale2.LowPart shl 16);
       Scale2.HighPart := Scale2.LowPart;
       {$ENDIF}

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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         movq     mm5,Scale1
         movq     mm6,Scale2
         {$ELSE}
         db $0F,$EF,$FF
         db $0F,$6F,$6D,$E0
         db $0F,$6F,$75,$D8
         {$ENDIF}

         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq      mm0,qword ptr [esi+ecx*8]
         movq      mm2,qword ptr [edx+ecx*8]

         movq      mm1,mm0     // copy
         punpcklbw mm0,mm7     // unpack lower 4 bytes to words
         pmullw    mm0,mm5
         punpckhbw mm1,mm7     // unpack higher 4 bytes to words
         pmullw    mm1,mm5

         movq      mm3,mm2     // copy
         punpcklbw mm2,mm7     // unpack lower 4 bytes to words
         pmullw    mm2,mm6
         punpckhbw mm3,mm7     // unpack higher 4 bytes to words
         pmullw    mm3,mm6

         paddw     mm0,mm2     // Add low 4
         paddw     mm1,mm3

         psrlw     mm0,7
         psrlw     mm1,7

         packuswb  mm0,mm1
         movq      qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$14,$CA

         db    $0F,$6F,$C8
         db    $0F,$60,$C7
         db    $0F,$D5,$C5
         db    $0F,$68,$CF
         db    $0F,$D5,$CD

         db    $0F,$6F,$DA
         db    $0F,$60,$D7
         db    $0F,$D5,$D6
         db    $0F,$68,$DF
         db    $0F,$D5,$DE

         db    $0F,$FD,$C2
         db    $0F,$FD,$CB


         db    $0F,$71,$D0,$07
         db    $0F,$71,$D1,$07

         db    $0F,$67,$C1
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         add   al,[edx+ecx]
         jnc   @NoSat
         mov   ah,$01
         @NoSat:
         shr   ax,1
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             Value := Round(FScale[1] * pS1^[i] + FScale[0] * pS0^[i]);
             if (Value > 255)
             then pR^[i] := 255
             else if (Value < 0)
                  then pR^[i] := 0
                  else pR^[i] := Value;
          end;
       end;
  end;
end; // TmcmImageMath.BlendImages.


procedure TmcmImageMath.DiffImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         movq    mm1,qword ptr [edx+ecx*8]
         movq    mm2,mm0       // mm2 is a copy of mm0
         // Saturation set negative values to zero whay mm0 and mm1 will hold only
         // positive sub-bytes.
         psubusb mm0,mm1       // mm0 := mm0 - mm1,
         psubusb mm1,mm2       // mm1 := mm1 - mm2,
         por     mm0,mm1       // or mm0 and mm1
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$0C,$CA
         db    $0F,$6F,$D0

         db    $0F,$D8,$C1
         db    $0F,$D8,$CA
         db    $0F,$EB,$C1

         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         sub   al,[edx+ecx]
         jnb   @NoSat
         mov   al,$00
         @NoSat:
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do pR^[i] := abs(pS1^[i] - pS0^[i]);
       end;
  end;
end; // TmcmImageMath.DiffImages.


procedure TmcmImageMath.DivImages;
var Count    : longint;
    i, y     : longint;
    //Height   : longword;
    pS0      : PVectorB;
    pR       : PVectorB;
    Value    : integer;
    //iScale   : integer;
    //Scale1   : int64;
begin
  Count  := FSrcImage[0].LongLineWidth;
  if (FScale[0] <> 0.0)
  then FScale[0] := 1.0 / FScale[0];
  if FMMX
  then begin
       MulImages;
       (*
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pR  := FResImage.pDib;

       {$IFNDEF DCB3}
       iScale := Round(FScale[0] * 65535);
       Scale1 := Round(FScale[0] * 65535);
       for i := 0 to 3
       do begin
          Scale1 := Scale1 or (Scale1 shl (16 * i));
       end;
       {$ELSE}
       iScale := Round(FScale[0] * 256);
       Scale1.LowPart := Round(FScale[0] * 1.28 );
       Scale1.LowPart := Scale1.LowPart or (Scale1.LowPart shl 16);
       Scale1.HighPart := Scale1.LowPart;
       {$ENDIF}


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
         mov   esi,pS0

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         movq     mm5,Scale1
         {$ELSE}
         db $0F,$EF,$FF
         db $0F,$6F,$6D,$E0
         {$ENDIF}
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq      mm0,qword ptr [esi+ecx*8]
         movq      mm1,mm0     // copy

         punpcklbw mm0,mm7     // unpack lower 4 bytes to words
         punpckhbw mm1,mm7     // unpack higher 4 bytes to words

         pmulhw    mm0,mm5
         pmulhw    mm1,mm5

         packuswb  mm0,mm1

         movq      qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$C8

         db    $0F,$60,$C7
         db    $0F,$68,$CF
         db    $0F,$6F,$D0
         db    $0F,$6F,$D9

         db    $0F,$D5,$C5
         db    $0F,$D5,$CD

         db    $0F,$E5,$D5
         db    $0F,$E5,$DD
{        //*
         db    $0F,$65,$D7
         db    $0F,$71,$D2,$08
         db    $0F,$65,$DF
         db    $0F,$71,$D3,$08
}
         db    $0F,$71,$D0,$08
         db    $0F,$71,$F2,$08 //*
         db    $0F,$71,$D1,$08
         db    $0F,$71,$F3,$08 //*

         db    $0F,$EB,$C2
         db    $0F,$EB,$CB

         db    $0F,$67,$C1
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         xor   eax,eax
         mov   al,[esi+ecx]
         {$IFNDEF DCB3_5}
         mul   eax,iScale
         {$ELSE}
         mov   edx,iScale
         db    $F7,$65,$E4
         {$ENDIF}
         jc    @Overflow
         shr   eax,8
         cmp   eax,$FF
         jl    @SetData

         @Overflow:
         mov   al,$FF

         @SetData:
         mov   [edi+ecx],al

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
       *)
  end
  else begin
       if (FScale[0] <> 0)
       then begin
            for y := 0 to (FSrcImage[0].Height - 1)
            do begin
               pS0 := FSrcImage[0].ScanLine[y];
               pR  := FResImage.ScanLine[y];
               for i := 0 to (Count - 1)
               do begin
                  Value := Round(pS0^[i] * FScale[0]);
                  if (Value > 255)
                  then pR^[i] := 255
                  else pR^[i] := Value;
               end;
            end;
       end;
  end;
  if (FScale[0] <> 0.0)
  then FScale[0] := 1.0 / FScale[0];
end; // TmcmImageMath.DivImages.


procedure TmcmImageMath.EqualImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         pcmpeqb mm0,qword ptr [edx+ecx*8]
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$74,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         cmp   al,[edx+ecx]
         je    @Less
         mov   [edi+ecx],ah
         jmp   @NextPix
         @Less:
         mov   al,$FF
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             if (pS0^[i] = pS1^[i])
             then pR^[i] := 255
             else pR^[i] := 0;
          end;
       end;
  end;
end; // TmcmImageMath.EqualImages.


procedure TmcmImageMath.GreaterImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         {$ELSE}
         db $0F,$EF,$FF
         {$ENDIF}
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         movq    mm2,qword ptr [edx+ecx*8]

         movq      mm1,mm0     // copy
         punpcklbw mm0,mm7     // unpack lower 4 bytes to words
         punpckhbw mm1,mm7     // unpack higher 4 bytes to words

         movq      mm3,mm2     // copy
         punpcklbw mm2,mm7     // unpack lower 4 bytes to words
         punpckhbw mm3,mm7     // unpack higher 4 bytes to words

         pcmpgtw   mm0,mm2
         pcmpgtw   mm1,mm3

         packsswb  mm0,mm1

         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$14,$CA

         db    $0F,$6F,$C8
         db    $0F,$60,$C7
         db    $0F,$68,$CF

         db    $0F,$6F,$DA
         db    $0F,$60,$D7
         db    $0F,$68,$DF

         db    $0F,$65,$C2
         db    $0F,$65,$CB

         db    $0F,$63,$C1

         db    $0F,$7F,$04,$CF
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
         @LoopRemain:

         mov   al,[esi+ecx]
         cmp   al,[edx+ecx]
         jl    @Less
         mov   al,$FF
         mov   [edi+ecx],al
         jmp   @NextPix
         @Less:
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             if (pS0^[i] > pS1^[i])
             then pR^[i] := 255
             else pR^[i] := 0;
          end;
       end;
  end;
end; // TmcmImageMath.GreaterImages.


procedure TmcmImageMath.MulImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0      : PVectorB;
    pR       : PVectorB;
    Value    : integer;
    iScale   : integer;
    Scale1   : int64;
begin
  Count  := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pR  := FResImage.pDib;

       {$IFNDEF DCB3}
       iScale := Round(FScale[0] * 256);
       Scale1 := iScale;
       for i := 0 to 3
       do begin
          Scale1 := Scale1 or (Scale1 shl (16 * i));
       end;
       {$ELSE}
       iScale := Round(FScale[0] * 256);
       Scale1.LowPart := iScale;
       Scale1.LowPart := Scale1.LowPart or (Scale1.LowPart shl 16);
       Scale1.HighPart := Scale1.LowPart;
       {$ENDIF}


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
         mov   esi,pS0

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         {$IFNDEF DCB3_5}
         pxor     mm7,mm7      // mm7 = zero reg.
         movq     mm5,Scale1
         {$ELSE}
         db $0F,$EF,$FF
         db $0F,$6F,$6D,$E0
         {$ENDIF}
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq      mm0,qword ptr [esi+ecx*8]
         movq      mm1,mm0     // copy

         punpcklbw mm0,mm7     // unpack lower 4 bytes to words
         punpckhbw mm1,mm7     // unpack higher 4 bytes to words
         movq      mm2,mm0     // copy
         movq      mm3,mm1

         pmullw    mm0,mm5
         pmullw    mm1,mm5

         pmulhw    mm2,mm5
         pmulhw    mm3,mm5
{ //*
         pcmpgtw   mm2,mm7
         psrlw     mm2,8
         pcmpgtw   mm3,mm7
         psrlw     mm3,8
}
         psrlw     mm0,8
         psllw     mm2,8 //*
         psrlw     mm1,8
         psllw     mm3,8 //*

         por       mm0,mm2
         por       mm1,mm3

         packuswb  mm0,mm1

         movq      qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$6F,$C8

         db    $0F,$60,$C7
         db    $0F,$68,$CF
         db    $0F,$6F,$D0
         db    $0F,$6F,$D9

         db    $0F,$D5,$C5
         db    $0F,$D5,$CD

         db    $0F,$E5,$D5
         db    $0F,$E5,$DD
{        //*
         db    $0F,$65,$D7
         db    $0F,$71,$D2,$08
         db    $0F,$65,$DF
         db    $0F,$71,$D3,$08
}
         db    $0F,$71,$D0,$08
         db    $0F,$71,$F2,$08 //*
         db    $0F,$71,$D1,$08
         db    $0F,$71,$F3,$08 //*

         db    $0F,$EB,$C2
         db    $0F,$EB,$CB

         db    $0F,$67,$C1
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         xor   eax,eax
         mov   al,[esi+ecx]
         {$IFNDEF DCB3_5}
         mul   eax,iScale
         {$ELSE}
         mov   edx,iScale
         db    $F7,$65,$E4
         {$ENDIF}
         jc    @Overflow
         shr   eax,8
         cmp   eax,$FF
         jl    @SetData

         @Overflow:
         mov   al,$FF

         @SetData:
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             Value := Round(pS0^[i] * FScale[0]);
             if (Value > 255)
             then pR^[i] := 255
             else pR^[i] := Value;
          end;
       end;
  end;
end; // TmcmImageMath.MulImages.


procedure TmcmImageMath.OrImages;
var Count     : longint;
    i, y      : longint;
    Height    : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq  mm0,qword ptr [esi+ecx*8]
         por   mm0,qword ptr [edx+ecx*8]
         movq  qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$EB,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         or    al,[edx+ecx]
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do pR^[i] := pS1^[i] or pS0^[i];
       end;
  end;
end; // TmcmImageMath.OrImages.


procedure TmcmImageMath.SubImages;
var Count    : longint;
    i, y     : longint;
    Value    : integer;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq    mm0,qword ptr [esi+ecx*8]
         psubusb mm0,qword ptr [edx+ecx*8] // Add unsigned bytes with saturation.
         movq    qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$D8,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         sub   al,[edx+ecx]
         jnb   @NoSat
         mov   al,$00
         @NoSat:
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             Value := pS1^[i] - pS0^[i];
             if (Value < 0)
             then pR^[i] := 0
             else pR^[i] := Value;
          end;
       end;
  end;
end; // TmcmImageMath.SubImages.


procedure TmcmImageMath.XorImages;
var Count    : longint;
    i, y     : longint;
    Height   : longword;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX
  then begin
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
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
         mov   esi,pS0
         mov   edx,pS1

         test  ecx,ecx
         jz    @NoQWORD

         // process image
         @LoopQWORD:
         {$IFNDEF DCB3_5}
         movq  mm0,qword ptr [esi+ecx*8]
         pxor  mm0,qword ptr [edx+ecx*8]
         movq  qword ptr [edi+ecx*8],mm0
         {$ELSE}
         db    $0F,$6F,$04,$CE
         db    $0F,$EF,$04,$CA
         db    $0F,$7F,$04,$CF
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
         @LoopRemain:
         mov   al,[esi+ecx]
         xor   al,[edx+ecx]
         mov   [edi+ecx],al
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
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do pR^[i] := pS1^[i] xor pS0^[i];
       end;
  end;
end; // TmcmImageMath.XorImages.


procedure TmcmImageMath.MagnitudeImages;
var Count    : longint;
    i, y     : longint;
    //Height   : longword;
    Value    : integer;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
    MagScale : double;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX and False
  then begin
  (*
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
       asm
         // Save registers to stack
         push  ebx
         push  edi
         push  esi


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
  *)
  end
  else begin
       MagScale := 255.0 / sqrt(130050.0);
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             Value := Round(MagScale * sqrt(pS1^[i] * pS1^[i] + pS0^[i] * pS0^[i]));
             if (Value > 255)
             then pR^[i] := 255
             else pR^[i] := Value;
          end;
       end;
  end;
end; // TmcmImageMath.MagnitudeImages.


procedure TmcmImageMath.OrientateImages;
var Count    : longint;
    i, y     : longint;
    //Height   : longword;
    Value    : integer;
    pS0, pS1 : PVectorB;
    pR       : PVectorB;
    AT2Scale : double;
begin
  Count := FSrcImage[0].LongLineWidth;
  if FMMX and False
  then begin
  (*
       Height := FSrcImage[0].Height;
       pS0 := FSrcImage[0].pDib;
       pS1 := FSrcImage[1].pDib;
       pR  := FResImage.pDib;
       asm
         // Save registers to stack
         push  ebx
         push  edi
         push  esi


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
  *)
  end
  else begin
       AT2Scale := 255.0 / (ArcTan2(255.0, 0.01) - ArcTan2(0.0, 255.0));
       for y := 0 to (FSrcImage[0].Height - 1)
       do begin
          pS0 := FSrcImage[0].ScanLine[y];
          pS1 := FSrcImage[1].ScanLine[y];
          pR  := FResImage.ScanLine[y];
          for i := 0 to (Count - 1)
          do begin
             if (pS0^[i] = 0)
             then pR^[i] := 255
             else begin
                  Value := Round(AT2Scale * ArcTan2(pS1^[i], pS0^[i]));
                  if (Value > 255)
                  then pR^[i] := 255
                  else pR^[i] := Value;
             end;
          end;
       end;
  end;
end; // TmcmImageMath.OrientateImages.


function TmcmImageMath.Execute(Method : TmcmImageMathematics) : TmcmImage;
var SingleImage : boolean;
begin
  FError := EC_OK;
  SingleImage := (Method in [IM_DIV, IM_MUL]);
  if (FSrcImage[0] <> Nil) and ((FSrcImage[1] <> Nil) or SingleImage)
  then begin
       if SingleImage or
          (FSrcImage[0].Width = FSrcImage[1].Width) and
          (FSrcImage[0].Height = FSrcImage[1].Height) and
          (FSrcImage[0].ImageFormat = FSrcImage[1].ImageFormat)
       then begin
            if CheckSource(0, [IF_GREY8,IF_RGB24,IF_RGBA32]) and
               (SingleImage or CheckSource(1, [IF_GREY8,IF_RGB24,IF_RGBA32]))
            then begin
                 CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

                 if (FResImage <> Nil)
                 then if (FSrcImage[0].Empty or FResImage.Empty)
                      then begin
                           FError := EC_NOMEMORY;
                           Result := Nil;
                           Exit;
                      end;

                 try
                   case Method of
                   IM_ADD   : AddImages;
                   IM_AND   : AndImages;
                   IM_ANDBW : AndBitwiseImages;
                   IM_AVE   : AverageImages;
                   IM_BLEND : BlendImages;
                   IM_DIFF  : DiffImages;
                   IM_DIV   : DivImages;
                   IM_EQU   : EqualImages;
                   IM_GT    : GreaterImages;
                   IM_MAG   : MagnitudeImages;
                   IM_MUL   : MulImages;
                   IM_ORBW  : OrImages;
                   IM_ORI   : OrientateImages;
                   IM_SUB   : SubImages;
                   IM_XORBW : XorImages;
                   else FError := EC_UNKNOWNMETHOD;
                   end;
                 except
                   On E:Exception
                   do FError := EC_UNKNOWN;
                 end;
            end;
       end
       else FError := EC_NOMATCHFORMAT;
  end
  else FError := EC_MISSOURCEIMAGE;

  if (FError = EC_OK)
  then Result := FResImage
  else Result := Nil;
end; // TmcmImageMath.Execute.



{$IFDEF OPTIMIZATION_ON} {$O-} {$UNDEF OPTIMIZATION_ON} {$ENDIF}
{$IFDEF ALIGN_ON} {$A-} {$UNDEF ALIGN_ON} {$ENDIF}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
