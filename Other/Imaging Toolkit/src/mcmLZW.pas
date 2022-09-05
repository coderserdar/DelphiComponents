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
// $Log:  17824: mcmLZW.pas
//
//    Rev 1.12    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.11    15-01-2009 00:19:32  mcm    Version: IMG 3.3
// Added support for Delphi 2009
//
//    Rev 1.10    02/04/2006 11:43:32  mcm
// Fixed exception occuring when decompression GIF data.
//
//   Rev 1.9    15-05-2005 20:23:14  mcm    Version: IMG 2.9
// Internal.

//
//   Rev 1.8    14-01-2005 19:45:54  mcm    Version: IMG 2.7
// Corrected a problem reading some 4 bit GIF images.

//
//   Rev 1.7    26-01-2004 23:28:52  mcm    Version: IMG 2.3
// TIFF, Corrected calculation of FPackWidth in SetImage for 1 & 4 bit images. 

//
//   Rev 1.6    24-11-2003 20:14:56  mcm

//
//   Rev 1.5    29-09-2003 18:40:36  mcm    Version: IMG 1.6
// Added a minor fix. If an LZW end-code was packed in as single "byte" in the
// input buffer this causes an unjustified error message as FIndex would be less
// than zero.

//
//   Rev 1.4    29-08-2003 18:24:50  mcm    Version: IMG 1.4
// Completed LZW compression for both TIFF & GIF.

//
//   Rev 1.3    25-07-2003 10:41:50  mcm
// Code clean-up

//
//   Rev 1.2    25-07-2003 00:01:10  mcm
// Added LZW Compression using hash tables.
// Improved Get/SetBigNBits (increased speed by 33%).

//
//   Rev 1.1    27-01-2003 13:42:30  mcm
// Modified parameters in GetBigNBits & GetLittleNBits.

//
//   Rev 1.0    07-08-2002 11:43:36  mcm    Version: IMG 1.1
// Initial edition. Compression is not implemented

unit mcmLZW;

//------------------------------------------------------------------------------
// GIF & TIFF, LZW compression and decompression.
//------------------------------------------------------------------------------

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef, mcmImageCompress, mcmHashTable;

//------------------------------------------------------------------------------
// LZW Image compression class.
//------------------------------------------------------------------------------

type
//------------------------------------------------------------------------------
  TLZWCodeMode = (LZWCM_TIF, LZWCM_GIF);

  TLZWEntry = packed record
    Character  : byte;
    Parent     : longint;
  end;

  TLZWDictionary = array[0..0] of TLZWEntry;
  PLZWDictionary = ^TLZWDictionary;

  TWriteCodeNBits = procedure(Code : longint) of object;

  TLZWEnumExtraData = packed record
    Data     : PAnsiChar;
    CurCode  : integer;
    CurLen   : integer;
    MaxLen   : integer;
  end;
  PLZWEnumExtraData = ^TLZWEnumExtraData;

  TmcmImageLZW = class(TmcmImageCompress)
  private
    FBitEndian    : boolean; // False -> Little endian,
                             // True -> Big Endian.
    FGetNBits     : TGetEndianNBits;
    FSetNBits     : TSetEndianNBits;
    FWriteCodeBit : TWriteCodeNBits;

    FCodeMode     : TLZWCodeMode;

    FIncSize      : word;
    FPackWidth    : longint;  // Packed line width.

    FDictionary   : PLZWDictionary;
    FStack        : PVectorB;
    FStackIndex   : integer;
    FMinCodeSize  : integer;
    FMaxCodeSize  : integer;
    FCodeSize     : integer;
    FEndCode      : longint;
    FClearCode    : longint;
    FNextCode     : longint;
    FFirstChar    : longint;

    FCodeBit      : integer;  // Bit index at FCodeByte.
    FCodeByte     : byte;     // Byte index into compressed data buffer
    FCodeIndex    : longword; // index into compressed data buffer.
    FDataBit      : integer;
    FDataByte     : byte;

    FCBufferSize  : longword;
    FLastBits     : longint;
    FLastCode     : longint;

    FPrevCode     : longint;
    FFirstRun     : boolean;

    FInterlaced   : boolean;
    FPass         : integer;

    FHashTable    : TmcmHashTable; // Initial search table. Used only when compressing.
    FKey          : THashKey;      // Current search key.
    FEnumData     : TLZWEnumExtraData;

    FData         : PVectorB;      //
    FDataSize     : longint;

    FDiffPredictor : boolean;       // Difference Predictor.
  protected
    function    GetBigNBits(Count : word) : cardinal;
    function    GetLittleNBits(Count : word) : cardinal;
    procedure   SetBigNBits(Bits : cardinal; Count : word);
    procedure   SetLittleNBits(Bits : cardinal; Count : word);

    procedure   AddToLZWDictionary(NewData : PAnsiChar; DataLength : integer);
    procedure   InitLZWDictionary;
    procedure   MatchLongest(ExtraData : pointer; const Key : THashKey; Offset, Len, Code : longint);
    procedure   PredictorApply(Buffer : pointer);
    procedure   SetBigEndian(Value : boolean);
    procedure   SetCodeMode(Value : TLZWCodeMode);
    procedure   SetCodeSize(Value : integer);
    procedure   SetImage(Value : HBITMAP); override;
    procedure   SetInterlaced(Value : boolean);
    procedure   UpdateMaxCode;
    procedure   WriteCode1Bits(Code : longint);
    procedure   WriteCode4Bits(Code : longint);
    procedure   WriteCode8Bits(Code : longint);
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    procedure   SetDataSize(BitCount : integer);
    property    CodeMode : TLZWCodeMode
      read      FCodeMode
      write     SetCodeMode default LZWCM_TIF;
    property    CodeSize : integer
      read      FCodeSize
      write     SetCodeSize;
    property    BitEndian : boolean
      read      FBitEndian
      write     SetBigEndian default False;
    property    Interlaced : boolean
      read      FInterlaced
      write     FInterlaced default False;
    property    DiffPredictor : boolean
      read      FDiffPredictor
      write     FDiffPredictor default False;
  published
  end;

implementation


uses {$IFNDEF GE_DXE2}
      SysUtils,
     {$ELSE}
      System.SysUtils,
     {$ENDIF}
     DefGIF;


//------------------------------------------------------------------------------
// TmcmImageLZW.
//------------------------------------------------------------------------------

constructor TmcmImageLZW.Create;
begin
  Inherited Create;

  FSupColors := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32];

  FBitEndian    := False;
  FGetNBits     := GetLittleNBits;
  FSetNBits     := SetLittleNBits;
  FWriteCodeBit := WriteCode8Bits;
  FIncSize      := 1;
  FCodeBit      := 8;
  FCodeByte     := 0;
  FDataBit      := 8;
  FDataByte     := 0;
  FCodeMode     := LZWCM_TIF;

  FDictionary   := AllocMem(4096 * SizeOf(TLZWEntry));
  FillChar(FDictionary^, 4096 * SizeOf(TLZWEntry), 0);
  FStack        := AllocMem(4096 * SizeOf(Byte));
  FMinCodeSize  := 8;
  FCodeSize     := FMinCodeSize + 1;
  FClearCode    := 1 shl FMinCodeSize;
  FEndCode      := FClearCode + 1;
  FNextCode     := FEndCode + 1;
  FLastBits     := -1;
  FFirstRun     := True;
  FFlipVert     := True;

  FInterlaced   := False;
  FPass         := 1;

  FHashTable    := Nil;
  FKey.AsString[0] := #3;
  FData         := Nil;
  FDiffPredictor := False;
end; // TmcmImageLZW.Create.


destructor TmcmImageLZW.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageLZW.Destroy.


procedure TmcmImageLZW.Clear;
begin
  if (FData <> Nil)
  then FreeMem(FData);
  FData := Nil;

  FreeMem(FDictionary);
  FreeMem(FStack);

  if Assigned(FHashTable)
  then FHashTable.Free;
  FHashTable := Nil;

  Inherited Clear;
end; // TmcmImageLZW.Clear.


procedure TmcmImageLZW.SetImage(Value : HBITMAP);
var BitCount : integer;
begin
  Inherited SetImage(Value);
  if FBitEndian
  then FCodeBit := 0
  else FCodeBit := 8;
  FLastBits := -1;

  case FDIBSection.dsBmih.biBitCount of
  1  : FIncSize := 8; // TIFF
  4  : FIncSize := 2; // TIFF
  8  : FIncSize := 1; // GIF and TIFF
  16 : FMaxCol := 2 * FMaxCol; // TIFF
  24 : FMaxCol := 3 * FMaxCol; // TIFF
  32 : FMaxCol := 4 * FMaxCol; // TIFF
  end;

  // FPackWidth is only used in Compress!
  BitCount := FDIBSection.dsBmih.biBitCount * FDIBSection.dsBmih.biPlanes;
  case BitCount of
  1  : if (FCodeMode = LZWCM_TIF)
       then FPackWidth := (FDIBSection.dsBmih.biWidth + 7) div 8 // TIFF
       else FPackWidth := FDIBSection.dsBmih.biWidth; // GIF
  4  : if (FCodeMode = LZWCM_TIF)
       then FPackWidth := (FDIBSection.dsBmih.biWidth + 1) div 2 // TIFF
       else FPackWidth := FDIBSection.dsBmih.biWidth; // GIF
  8  : FPackWidth := FDIBSection.dsBmih.biWidth; // GIF and TIFF
  15,
  16 : FPackWidth := 2 * FDIBSection.dsBmih.biWidth; // TIFF
  24 : FPackWidth := 3 * FDIBSection.dsBmih.biWidth; // TIFF
  32 : FPackWidth := 4 * FDIBSection.dsBmih.biWidth; // TIFF
  end;
end; // TmcmImageLZW.SetImage.


procedure TmcmImageLZW.SetInterlaced(Value : boolean);
begin
  FInterlaced := Value;
end; // TmcmImageLZW.SetInterlaced.


procedure TmcmImageLZW.SetBigEndian(Value : boolean);
begin
  FBitEndian := Value;
  if FBitEndian
  then begin
       FGetNBits := GetBigNBits;
       FSetNBits := SetBigNBits;
       FCodeBit := 0;
  end
  else begin
       FGetNBits := GetLittleNBits;
       FSetNBits := SetLittleNBits;
       FCodeBit := 8;
  end;
end; // TmcmImageLZW.SetBigEndian.


procedure TmcmImageLZW.SetCodeMode(Value : TLZWCodeMode);
begin
  FCodeMode := Value;
  // UpdateMaxCode;
end; // TmcmImageLZW.SetCodeMode.


procedure TmcmImageLZW.SetCodeSize(Value : integer);
begin
  FMinCodeSize := Value;
  InitLZWDictionary;
end; // TmcmImageLZW.SetCodeSize.


procedure TmcmImageLZW.SetDataSize(BitCount : integer);
// Used only with GIF images. TIFF always use "WriteCode8Bits".
begin
  case BitCount of
  1 : FWriteCodeBit := WriteCode1Bits;
  4 : FWriteCodeBit := WriteCode4Bits;
  8 : FWriteCodeBit := WriteCode8Bits;
  else FError := EC_BITDEPTH;
  end;
end; // TmcmImageLZW.SetDataSize.


procedure TmcmImageLZW.PredictorApply(Buffer : pointer);
// Swaps R and B component in RGB images and calculates the difference
// between each
type T24RGB = array[0..0] of TRGBTriple;
     T32RGB = array[0..0] of TRGBQuad;
var i      : longint;
    RVal   : byte;
    BVal   : byte;
begin
  case FDIBSection.dsBmih.biBitCount of
  8  : begin
         for i := (FDIBSection.dsBmih.biWidth - 1) downto 1
         do TVectorB(Buffer^)[i] := TVectorB(Buffer^)[i] - TVectorB(Buffer^)[i-1];
       end;
  24 : begin
         for i := (FDIBSection.dsBmih.biWidth - 1) downto 1
         do begin
            RVal := T24RGB(Buffer^)[i].RGBtred;
            BVal := T24RGB(Buffer^)[i].RGBtblue;
            T24RGB(Buffer^)[i].RGBtblue  := byte(RVal - T24RGB(Buffer^)[i-1].RGBtred);
            T24RGB(Buffer^)[i].rgbtGreen := byte(T24RGB(Buffer^)[i].rgbtGreen - T24RGB(Buffer^)[i-1].rgbtGreen);
            T24RGB(Buffer^)[i].RGBtred   := byte(BVal - T24RGB(Buffer^)[i-1].RGBtblue);
         end;
         RVal := T24RGB(Buffer^)[0].RGBtblue;
         BVal := T24RGB(Buffer^)[0].RGBtred;
         T24RGB(Buffer^)[0].RGBtred := RVal;
         T24RGB(Buffer^)[0].RGBtblue := BVal;
       end;
  32 : begin
         for i := (FDIBSection.dsBmih.biWidth - 1) downto 1
         do begin
            RVal := T32RGB(Buffer^)[i].rgbRed;
            BVal := T32RGB(Buffer^)[i].rgbBlue;
            T32RGB(Buffer^)[i].rgbBlue  := byte(RVal - T32RGB(Buffer^)[i-1].rgbRed);
            T32RGB(Buffer^)[i].rgbGreen := byte(T32RGB(Buffer^)[i].rgbGreen - T32RGB(Buffer^)[i-1].rgbGreen);
            T32RGB(Buffer^)[i].rgbRed   := byte(BVal - T32RGB(Buffer^)[i-1].rgbBlue);
            T32RGB(Buffer^)[i].rgbReserved := byte(T32RGB(Buffer^)[i].rgbReserved - T32RGB(Buffer^)[i-1].rgbReserved);
         end;
         RVal := T32RGB(Buffer^)[0].rgbBlue;
         BVal := T32RGB(Buffer^)[0].rgbRed;
         T32RGB(Buffer^)[0].rgbRed := RVal;
         T32RGB(Buffer^)[0].rgbBlue := BVal;
       end;
  end;
end; // TmcmImageLZW.PredictorApply.


procedure TmcmImageLZW.InitLZWDictionary;
var i, j : integer;
begin
  j := (1 shl FCodeSize) - 1;
  for i := 0 to j
  do begin
     FDictionary^[i].Character := i;
     FDictionary^[i].Parent := -1;
  end;
  FCodeSize    := FMinCodeSize + 1;
  FClearCode   := 1 shl FMinCodeSize;
  FEndCode     := FClearCode + 1;
  FNextCode    := FEndCode + 1;
  UpdateMaxCode;
  if Assigned(FHashTable)
  then FHashTable.Clear;
end; // TmcmImageLZW.InitLZWDictionary.


procedure TmcmImageLZW.UpdateMaxCode;
begin
  case FCodeMode of
  LZWCM_TIF : FMaxCodeSize := (1 shl FCodeSize) - 1;
  LZWCM_GIF : FMaxCodeSize := 1 shl FCodeSize;
  end;
end; // TmcmImageLZW.UpdateMaxCode;


function TmcmImageLZW.GetBigNBits(Count : word) : cardinal;
var Value : longint;
begin
  Value := 0;
  if (Count <> 0)
  then begin
       while (Count > 0)
       do begin
          if (FCodeBit = 0)
          then begin
               if (FCodeIndex >= FCBufferSize)
               then begin
                    FLastBits := FCodeBit;
                    FLastCode := Value;
                    inc(FCodeIndex);
                    Result := cardinal(-1);
                    Exit;
               end;
               FCodeByte := FCBuffer^[FCodeIndex];
               inc(FCodeIndex);
               FCodeBit  := 8;
          end;

          if (Count <= FCodeBit)
          then begin
               Value := Value or (FCodeByte shr (FCodeBit - Count)) and ((1 shl Count) - 1);
               dec(FCodeBit, Count);
               Count := 0;
          end
          else begin
               Value := Value or (FCodeByte and ((1 shl FCodeBit) - 1)) shl (Count - FCodeBit);
               dec(Count, FCodeBit);
               FCodeBit := 0;
          end;
       end;
  end;
  Result := Value;
end; // TmcmImageLZW.GetBigNBits.


function TmcmImageLZW.GetLittleNBits(Count : word) : cardinal;
var Value : cardinal;
    i     : word;
begin
  Value := 0;
  for i := 0 to (Count - 1)
  do begin
     if (FCodeBit >= 8)
     then begin
          if (FCodeIndex >= FCBufferSize)
          then begin
               FLastBits := i;
               FLastCode := Value;
               inc(FCodeIndex);
               Result := cardinal(-1);
               Exit;
          end;
          FCodeByte := FCBuffer^[FCodeIndex];
          inc(FCodeIndex);
          FCodeBit  := 0;
     end;

     if ((FCodeByte and BitMask[FCodeBit]) <> 0)
     then Value := Value or BitMask[i];
     inc(FCodeBit);
  end;
  Result := Value;
end; // TmcmImageLZW.GetLittleNBits.


procedure TmcmImageLZW.SetBigNBits(Bits : cardinal; Count : word);
var NewByte : byte;
begin
  while (Count > 0)
  do begin
     // FCodeBit is [1..8]
     if (Count < FCodeBit)
     then begin
          NewByte := ((Bits and ((1 shl Count) - 1)) shl (FCodeBit - Count));
          dec(FCodeBit, Count);
          FCodeByte := FCodeByte or NewByte;
          Count := 0;
     end
     else begin
          NewByte := ((Bits shr (Count - FCodeBit)) and ((1 shl FCodeBit) - 1));
          dec(Count, FCodeBit);
          FCodeByte := FCodeByte or NewByte;
          FDBuffer[FCodeIndex] := FCodeByte;
          inc(FCodeIndex);
          FCodeByte := 0;
          FCodeBit := 8;
     end;
  end;
end; // TmcmImageLZW.SetBigNBits.


procedure TmcmImageLZW.SetLittleNBits(Bits : cardinal; Count : word);
var i : word;
begin
  for i := 0 to (Count - 1)
  do begin
     if (FCodeBit >= 8)
     then begin
          FDBuffer[FCodeIndex] := FCodeByte;
          inc(FCodeIndex);
          FCodeByte := 0;
          FCodeBit := 0;
     end;
     if ((Bits and BitMask[i]) <> 0)
     then FCodeByte := FCodeByte or BitMask[FCodeBit];
     inc(FCodeBit);
  end;
end; // TmcmImageLZW.SetLittleNBits.


procedure TmcmImageLZW.WriteCode1Bits(Code : longint);
// ONLY used by GIF.
// Write data to bitmap - used when decompressing LZW data.
var i       : longint;
    OldCode : longint;
begin
  if (FIndex < 0) or (FIndex > FImageSize)
  then begin
       FError := EC_BADDECOMPRESS;
       Exit;
  end;

  try
    FStackIndex := 0;
    repeat
      FStack^[FStackIndex] := FDictionary^[Code].Character;
      inc(FStackIndex);
      OldCode := Code;
      Code := FDictionary^[Code].Parent;
    until (FDictionary^[OldCode].Parent = -1) or (FStackIndex >= 4096);
    FFirstChar := FStack^[FStackIndex-1];

    for i := (FStackIndex - 1) downto 0
    do begin
       dec(FDataBit);
       if (FStack^[i] <> 0)
       then FDibBits^[FIndex] := FDibBits^[FIndex] or FStack^[i] shl FDataBit;

       if (FDataBit = 0)
       then begin
            inc(FIndex);
            FDataBit := 8;
       end;
       inc(FCol);
       if (FCol >= FMaxCol)
       then begin
            if FInterlaced
            then begin
                 if FFlipVert
                 then dec(FRow, GIFInterlaced[FPass].Interval)
                 else inc(FRow, GIFInterlaced[FPass].Interval);

                 if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                 then begin
                      inc(FPass);
                      if FFlipVert
                      then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
                      else FRow := GIFInterlaced[FPass].Row;
                 end;
            end
            else begin
                 if FFlipVert
                 then dec(FRow)
                 else inc(FRow);
            end;
            FIndex := FRow * FLongWidth;
            FDataBit := 8;
            FCol := 0;
            if (i > 0)
            then begin
                 if (FIndex < 0) or (FIndex > FImageSize)
                 then begin
                      FError := EC_BADDECOMPRESS;
                      Break;
                 end;
            end;
       end;
    end;
  except
    FError := EC_BADDECOMPRESS;
  end;
end; // TmcmImageLZW.WriteCode1Bits.


procedure TmcmImageLZW.WriteCode4Bits(Code : longint);
// ONLY used by GIF.
// Write data to bitmap - used when decompressing LZW data.
var i       : longint;
    OldCode : longint;
    BitMask : byte;
begin
  if (FIndex < 0) or (FIndex > FImageSize)
  then begin
       FError := EC_BADDECOMPRESS;
       Exit;
  end;

  try
    FStackIndex := 0;
    repeat
      FStack^[FStackIndex] := FDictionary^[Code].Character;
      inc(FStackIndex);
      OldCode := Code;
      Code := FDictionary^[Code].Parent;
    until (FDictionary^[OldCode].Parent = -1) or (FStackIndex >= 4096);
    FFirstChar := FStack^[FStackIndex-1];

    for i := (FStackIndex - 1) downto 0
    do begin
       dec(FDataBit, 4);
       if (FDataBit = 0)
       then BitMask := $F0
       else BitMask := $0F;
       FDibBits^[FIndex] := (FDibBits^[FIndex] and BitMask) or FStack^[i] shl FDataBit;

       if (FDataBit = 0)
       then begin
            inc(FIndex);
            FDataBit := 8;
       end;
       inc(FCol);
       if (FCol >= FMaxCol)
       then begin
            if FInterlaced
            then begin
                 if FFlipVert
                 then dec(FRow, GIFInterlaced[FPass].Interval)
                 else inc(FRow, GIFInterlaced[FPass].Interval);

                 if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                 then begin
                      inc(FPass);
                      if FFlipVert
                      then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
                      else FRow := GIFInterlaced[FPass].Row;
                 end;
            end
            else begin
                 if FFlipVert
                 then dec(FRow)
                 else inc(FRow);
            end;
            FIndex := FRow * FLongWidth;
            FDataBit := 8;
            FCol := 0;
            if (i > 0)
            then begin
                 if (FIndex < 0) or (FIndex > FImageSize)
                 then begin
                      FError := EC_BADDECOMPRESS;
                      Break;
                 end;
            end;
       end;
    end;
  except
    FError := EC_BADDECOMPRESS;
  end;
end; // TmcmImageLZW.WriteCode4Bits.


procedure TmcmImageLZW.WriteCode8Bits(Code : longint);
// Used by GIF and TIFF. TIFF always pack data in 8 bit regardless of the image
// bit depth.
// Write data to bitmap - used when decompressing LZW data.
var i       : longint;
    OldCode : longint;
begin
  if (FIndex < 0) or (FIndex > FImageSize)
  then begin
       FError := EC_BADDECOMPRESS;
       Exit;
  end;

  try
    FStackIndex := 0;
    repeat
      FStack^[FStackIndex] := FDictionary^[Code].Character;
      inc(FStackIndex);
      OldCode := Code;
      Code := FDictionary^[Code].Parent;
    until (FDictionary^[OldCode].Parent = -1) or (FStackIndex >= 4096);
    FFirstChar := FStack^[FStackIndex-1];

    for i := (FStackIndex - 1) downto 0
    do begin
       FDibBits^[FIndex] := FStack^[i];
       inc(FIndex);
       inc(FCol, FIncSize);

       if (FCol >= FMaxCol)
       then begin
            if FInterlaced
            then begin
                 if FFlipVert
                 then dec(FRow, GIFInterlaced[FPass].Interval)
                 else inc(FRow, GIFInterlaced[FPass].Interval);

                 if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                 then begin
                      inc(FPass);
                      if FFlipVert
                      then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
                      else FRow := GIFInterlaced[FPass].Row;
                 end;
            end
            else begin
                 if FFlipVert
                 then dec(FRow)
                 else inc(FRow);
            end;
            FIndex := FRow * FLongWidth;
            FCol := 0;
            if (i > 0)
            then begin
                 if (FIndex < 0) or (FIndex > FImageSize)
                 then begin
                      FError := EC_BADDECOMPRESS;
                      Break;
                 end;
            end;
       end;
    end;
  except
    FError := EC_BADDECOMPRESS;
  end;
end; // TmcmImageLZW.WriteCode8Bits.


procedure TmcmImageLZW.MatchLongest(      ExtraData   : pointer;
                                    const Key         : THashKey;
                                          Offset, Len, Code : longint);
var KeyData  : PAnsiChar;
    i        : integer;
begin
  with PLZWEnumExtraData(ExtraData)^
  do begin
     KeyData := PAnsiChar(Offset);
     if (Len <= MaxLen) and (Len > CurLen)
     then begin
          i := 2;
          while (i < Len)
          do begin
             if (KeyData[i] <> Data[i])
             then Break;
             inc(i);
          end;

          if (i = Len)
          then begin
               CurLen := Len;
               CurCode := Code;
          end;
     end;
  end;
end; // TmcmImageLZW.MatchLongest.


procedure TmcmImageLZW.AddToLZWDictionary(NewData : PAnsiChar; DataLength : integer);
begin
  if (FNextCode < 4096)
  then begin
       if (FNextCode >= FMaxCodeSize) and (FCodeSize < 12)
       then begin
            inc(FCodeSize);
            UpdateMaxCode;
       end;
       FHashTable.Insert(FKey, integer(NewData), DataLength, FNextCode);
       inc(FNextCode);
  end
  else inc(FNextCode);
end; // TmcmImageLZW.AddToLZWDictionary.


function TmcmImageLZW.Compress(var Buffer     : pointer;
                               var BufferSize : longword;
                               var DataCount  : longword) : TmcmErrorCode;
var i, j       : longint;
    CountDown  : longint;
    DataLength : integer;
    pCurrent   : PAnsiChar;
begin
  FError := EC_OK;
  BufferSize := 0;
  FCodeIndex := 0;
  try
    // Allocate buffer for compressed data and initialise Hash table.
    if (FDBuffer = Nil)
    then begin
         AllocCompressBuffer(6 * DataCount div 5);
         FHashTable := TmcmHashTable.Create(521);
    end;

    // Allocate buffer for continues image data.
    if (FData = Nil)
    then begin
         if (FCodeMode = LZWCM_GIF)
         then begin
              case FDIBSection.dsBmih.biBitCount of
              1 : FDataSize := 8 * DataCount;
              4 : FDataSize := 2 * DataCount;
              8 : FDataSize := DataCount;
              end;
         end
         else FDataSize := DataCount;
         GetMem(FData, FDataSize);
    end
    else begin
         if (FDataSize < longint(DataCount))
         then begin
              FDataSize := DataCount;
              ReallocMem(FData, FDataSize);
         end;
    end;

    if FFirstRun
    then begin
         FFirstRun := False;
         FCol := 0;
         if (FCodeMode = LZWCM_GIF)
         then begin
              FCodeBit := 0;
              if FInterlaced
              then begin
                   FPass := 1;
                   if FFlipVert
                   then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
                   else FRow := GIFInterlaced[FPass].Row;
              end;
              // GIF only
              // Write clear code - to initialise/reset lzw dictionary in reader.
              FSetNBits(FClearCode, FCodeSize);
         end;
    end;

    // Copy image data to FData.
    // (Continues data flow - without bitmap line alignment).
    pCurrent := pointer(FData);
    FIndex := FRow * FLongWidth + FCol;
    CountDown := DataCount;
    DataCount := 0;
    while (CountDown > 0)
    do begin
       case FCodeMode of
       LZWCM_GIF : begin
                     case FDIBSection.dsBmih.biBitCount of
                     1 : begin
                           for i := 0 to (FPackWidth - 1)
                           do begin
                              j := (7 - i mod 8);
                              PVectorB(pCurrent)^[i] := (FDibBits^[FIndex] shr j) and $01;
                              if (j = 0)
                              then inc(FIndex);
                           end;
                         end;
                     4 : begin
                           for i := 0 to (FPackWidth - 1)
                           do if Odd(i)
                              then begin
                                   PVectorB(pCurrent)^[i] := FDibBits^[FIndex] and $0F;
                                   inc(FIndex);
                              end
                              else begin
                                   PVectorB(pCurrent)^[i] := FDibBits^[FIndex] shr 4
                              end;
                         end;
                     8 : begin
                           CopyMemory(pCurrent, @FDibBits^[FIndex], FPackWidth);
                         end;
                     else
                     end;
                   end;
       LZWCM_TIF : begin
                     CopyMemory(pCurrent, @FDibBits^[FIndex], FPackWidth);

                     // Either apply Predictor scheme or swap R and B component.
                     if FDiffPredictor
                     then PredictorApply(pCurrent)
                     else if (FDIBSection.dsBmih.biBitCount in [24,32])
                          then SwapByteRB(pCurrent);
                   end;
       end;

       // Increment source buffer.
       inc(pCurrent, FPackWidth);
       FCol := 0;

       // Get next line to process.
       if FInterlaced
       then begin
            if FFlipVert
            then dec(FRow, GIFInterlaced[FPass].Interval)
            else inc(FRow, GIFInterlaced[FPass].Interval);
       end
       else begin
            if FFlipVert
            then dec(FRow)
            else inc(FRow);
       end;
       if (0 > FRow) or (FRow > FMaxRow)
       then CountDown := 0;
       FIndex := FRow * FLongWidth;

       dec(CountDown, FLongWidth);
       inc(DataCount, FLongWidth);
    end;

    // Calc data size.
    CountDown := pCurrent - pointer(FData);
    pCurrent := pointer(FData);


    // Initialise TIFF
    if (FCodeMode = LZWCM_TIF)
    then begin
         // TIFF, LZW compressed data are always packed independently in each strip.
         // Therefore, re-set CodeSize back to 9 bit and NextCode to 258.
         FCodeBit     := 8;
         FCodeSize    := FMinCodeSize + 1;
         FNextCode    := FEndCode + 1;
         UpdateMaxCode;

         // Write clear code - to initialise/reset lzw dictionary in reader.
         InitLZWDictionary;
         FSetNBits(FClearCode, FCodeSize);
    end;

    // Perform LZW compression on input data.
    while (CountDown > 1)
    do begin
       DataLength := 2;
       FKey.AsString[1] := pCurrent[0];
       FKey.AsString[2] := pCurrent[1];
       {
       if (CountDown > 2)
       then begin
            //DataLength := 3;
            FKey.AsString[3] := pCurrent[2];
       end;
       }
       FEnumData.Data := pCurrent;
       FEnumData.CurCode := -1;
       FEnumData.CurLen  := 0;
       FEnumData.MaxLen := CountDown;
       if FHashTable.FindAll(FKey, 0, MatchLongest, @FEnumData)
       then begin
            FPrevCode := FEnumData.CurCode;
            DataLength := FEnumData.CurLen;
            inc(DataLength);
       end
       else FPrevCode := byte(pCurrent[0]);

       // We have a "string" of data with no match in the dictionary.
       // Write the code for our longest matched data string.
       FSetNBits(FPrevCode, FCodeSize);

       // Add the non-matching data string to our dictionary.
       AddToLZWDictionary(pCurrent, DataLength);

       if (FNextCode = 4093)
       then begin
            if (CountDown - DataLength > 0)
            then begin
                 FSetNBits(byte(pCurrent[DataLength-1]), FCodeSize);
                 FSetNBits(FClearCode, FCodeSize);
                 InitLZWDictionary;
            end;
            pCurrent := pCurrent + DataLength;
       end
       else begin
            // Start a new data string, containing the last data byte that
            // caused a no-match
            dec(DataLength);
            pCurrent := pCurrent + DataLength;
            FPrevCode := byte(pCurrent[0]);
       end;
       dec(CountDown, DataLength);
    end;

    // Write last code to compress buffer.
    if (CountDown = 1)
    then begin
         FPrevCode := byte(pCurrent[0]);
         FSetNBits(FPrevCode, FCodeSize);
    end;

    case FCodeMode of
    LZWCM_TIF : begin
                  // Write End code.
                  FSetNBits(FEndCode, FCodeSize);

                  // Flush remaining bits to compressed buffer.
                  if (FCodeBit <> 8)
                  then FSetNBits(0, FCodeBit + 1);
                end;
    LZWCM_GIF : begin
                  if (FFlipVert and (FRow < 0)) or
                     (Not(FFlipVert) and (FRow >= FDIBSection.dsBmih.biHeight))
                  then begin
                       if FInterlaced
                       then begin
                            inc(FPass);
                            if FFlipVert
                            then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
                            else FRow := GIFInterlaced[FPass].Row;
                            FSetNBits(FClearCode, FCodeSize);
                            InitLZWDictionary;
                       end;

                       if (FRow < 0) or (FPass = 5)
                       then begin
                            // Write End code.
                            FSetNBits(FEndCode, FCodeSize);
                            // Flush remaining bits to compressed buffer.
                            if (FCodeBit <> 0)
                            then FSetNBits(0, 9 - FCodeBit);
                       end;
                  end
                  else begin
                       FSetNBits(FClearCode, FCodeSize);
                       InitLZWDictionary;
                  end;
                end;
    end;
  except
    On EOutOfMemory
    do begin
       FError := EC_NOMEMORY;
       DataCount := 0;
    end;
    On Exception
    do begin
       FError := EC_BADCOMPRESS;
       DataCount := 0;
    end;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := FCodeIndex;
  Result := FError;
end; // TmcmImageLZW.Compress.


function TmcmImageLZW.Decompress(    Buffer     : pointer;
                                 var BufferSize : longword;
                                 var DataCount  : longword) : TmcmErrorCode;
Label NASTYJUMP;
var SaveIndex  : longint;
    Code       : longint;
begin
  FError := EC_OK;

  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  FCBufferSize := BufferSize;
  FCodeIndex := 0;

  if (FCodeMode = LZWCM_TIF) 
  then begin
       //
       FLastBits := -1;
       // TIFF, LZW compressed data are always packed independently in each strip.
       // Therefore, re-set CodeSize back to 9 bit and NextCode to 258.
       FCol := 0;
       FLastBits := -1;
       FCodeBit     := 0;
       FCodeSize    := FMinCodeSize + 1;
       FNextCode    := FEndCode + 1;
       UpdateMaxCode;
  end;

  try
    if FFirstRun
    then begin
         FFirstRun := False;

         Code := FGetNBits(FCodeSize);
         while (Code = FClearCode)
         do Code := FGetNBits(FCodeSize);
         FWriteCodeBit(Code);
         FPrevCode := Code;

         if FInterlaced
         then begin
              FPass := 1;
              if FFlipVert
              then FRow := FDIBSection.dsBmih.biHeight - 1 - GIFInterlaced[FPass].Row
              else FRow := GIFInterlaced[FPass].Row;
         end;
    end
    else begin
         if (FLastBits <> -1)
         then begin
              if FBitEndian
              then Code := FLastCode + longint(FGetNBits(FLastBits))
              else begin
                   Code := FGetNBits(FCodeSize - FLastBits);
                   Code := (Code shl FLastBits) + FLastCode;
              end;
              FLastBits := -1;
              Goto NASTYJUMP;
         end;
    end;

    while (FCodeIndex < BufferSize) and (FError = EC_OK)
    do begin
       if (FNextCode >= FMaxCodeSize) and (FCodeSize < 12)
       then begin
            inc(FCodeSize);
            UpdateMaxCode;
       end;

       // Get next code.
       Code := FGetNBits(FCodeSize);

       NASTYJUMP: // Label - Used to jump inside this loop.
       if (Code = -1) // Reached end of buffer - requires more data
                      // FLastBits bits are remembered.
       then begin
       end
       else begin
            if (Code = FEndCode)
            then begin
                 DataCount := abs(FIndex - SaveIndex);
                 Result := FError;
                 if (FCodeMode = LZWCM_GIF)
                 then FPass := 1;
                 Exit;
            end
            else begin
                 if (Code = FClearCode)
                 then begin
                      InitLZWDictionary;
                      FPrevCode := -1;
                      if (FCodeIndex <= BufferSize) and (FError = EC_OK) 
                      then begin
                           while (Code = FClearCode)
                           do Code := FGetNBits(FCodeSize);
                           if (Code = FEndCode) 
                           then begin
                                DataCount := abs(FIndex - SaveIndex);
                                Result := FError;
                                Exit;
                           end;
                           if (Code <> - 1)
                           then FWriteCodeBit(Code);
                      end
                      else begin
                           FError := EC_COMPRESSION;
                           Result := FError;
                           Exit;
                      end;
                 end
                 else begin
                      if (FNextCode < 4096)
                      then begin
                           if (Code < FNextCode)
                           then begin
                                FWriteCodeBit(Code);
                                FDictionary^[FNextCode].Parent := FPrevCode;
                                FDictionary^[FNextCode].Character := FFirstChar;
                                inc(FNextCode);
                           end
                           else begin
                                // Takes care of a yet undefined code!
                                FDictionary^[FNextCode].Parent := FPrevCode;
                                FDictionary^[FNextCode].Character := FFirstChar;
                                inc(FNextCode);
                                FWriteCodeBit(Code);
                           end;
                      end
                      // If FNextCode >= 4096 then don't add new codes to
                      // dictionary - just read and translate code.
                      else FWriteCodeBit(Code); // GIF only.
                 end;
            end;
            FPrevCode := Code;
       end;
    end;

    // Caluclate decompressed data size.
    DataCount := abs(abs(FIndex) - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageLZW.Decompress.

{$IFDEF RANGEOFF}{$UNDEF RANGEOFF}{$R+}{$ENDIF}

end.
