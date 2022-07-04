unit kbmCompress;

// Version 1.01
//
// By using this file you agree to the license terms and disclaimer.
//
// LICENSE TERMS and DISCLAIMER
// ============================
// This unit is a rewritten collection of sources delivered by Bruno Depero (bdepero@usa.net).
// The unit can be used to simplify LZH or ZIP compression together with TkbmMemTable.
//
// ZIP support requires the PasZLIB compression library. (http://www.tu-chemnitz.de/~nomssi/paszlib.html)
// LZH support requires the LZH compression library.
//
// Define LZH to support LZH compression.
// Define ZIP to support ZIP compression.
//
// The file can be used or modified as you see fit.
//
// This file is for demonstation purposes. The author or coauthors can not be made responsible in
// any way for problems or errors occuring using this file.
// Optical Services - Scandinavia doesnt hold any responsibility or claims on this source file.
//
// History:
//
// 1.00 Initial version.
// 1.01 Changed names of the compression flags (clNone...clMax) to cplNone...cplMax to avoid conflict
//      with the color clNone used in other components.                             (30. Sep. 1999)
//*************************************************************************************************

{$define LZH}
//{$define ZIP}

interface

uses SysUtils,Classes
{$ifdef ZIP}
     ,zCompres, zUnCompr, ZLib, ZUtil
{$endif}
;

type
    TCRC32 = class
    private
        fCRC32Table: array[0..255] of longint;
        procedure InitCRC32;
    public
        constructor Create;
        function CalcCRC32(Stream:TStream):longint;
    end;

{$ifdef LZH}
    // Memory table LZH support.
    procedure LZHCompressBlobStream(UnCompressedStream,CompressedStream:TStream);
    procedure LZHDecompressBlobStream(CompressedStream,DeCompressedStream:TStream);
    procedure LZHCompressSave(UnCompressedStream,CompressedStream:TStream);
    procedure LZHDecompressLoad(CompressedStream,DeCompressedStream:TStream);
{$endif}

{$ifdef ZIP}
type
    TCompressionLevel = (cplNone, cplFastest, cplDefault, cplMax);
    TPnt = array[0..1024] of Byte;
    PTPnt = ^TPnt;
const
    Levels: array[TCompressionLevel] of ShortInt =
             (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
    ZLCfileID: LONGINT = ((((((ORD('C') shl 8) + ORD('L')) shl 8) + ORD('Z')) shl 8) + ORD('@'));

    // Memory table ZIP support.
    procedure ZIPCompressBlobStream(UnCompressedStream,CompressedStream:TStream);
    procedure ZIPDecompressBlobStream(CompressedStream,DeCompressedStream:TStream);
    procedure ZIPCompressSave(UnCompressedStream,CompressedStream:TStream);
    procedure ZIPDecompressLoad(CompressedStream,DeCompressedStream:TStream);
{$endif}

implementation

{$ifdef LZH}
uses lzh;
{$endif}

constructor TCRC32.Create;
begin
     inherited;
     InitCRC32;
end;

// Creates the table needed to calculate a crc32
procedure TCRC32.InitCRC32;
var
   crc, poly: longint;
   i, j: longint;
begin
     poly:=longint($EDB88320);
     for i:=0 to 255 do
     begin
          crc:=i;
          for j:=8 downto 1 do
          begin
               if (crc and 1) = 1 then
                  crc:=(crc shr 1) xor poly
               else
                  crc:=crc shr 1;
          end;
          fcrc32table[i]:=crc;
     end;
end;

// Calculate CRC32 on the contents of a stream.
function TCRC32.CalcCRC32(Stream:TStream):longint;
var
   crc, checked, buffersize, fsize, count: longint;
   BufferArray: array[0..10239] of byte;
   originalposition: longint;
begin
     originalposition := Stream.Position;
     Stream.Seek(0, soFromBeginning);
     crc := longint($FFFFFFFF);
     fsize := Stream.size;
     while True do
     begin
          if fsize <= 0 then break;
          if fsize >= 10240 then
             buffersize := 10240
          else
             buffersize := fsize;
          Count := Stream.Read(BufferArray, BufferSize);
          checked := 0;
          while checked < Count do
          begin
               crc := ((crc shr 8) and $FFFFFF) xor fcrc32table[(crc xor bufferArray[checked]) and $FF];
               inc(checked);
          end;
          dec(fsize, buffersize);
     end;
     result := (crc xor longint($FFFFFFFF));
     Stream.seek(originalposition, soFromBeginning);
end;

{$ifdef LZH}
// LZH Compress inmemory BLOB.
procedure LZHCompressBlobStream(UnCompressedStream,CompressedStream:TStream);
var
   LZH: TLZH;
   Size, Bytes: Longint;
begin
     LZH:=TLZH.create;
     try
        LZH.StreamIn := UnCompressedStream;
        LZH.StreamOut := CompressedStream;
        LZH.StreamIn.Position := 0;
        LZH.StreamOut.Position := 0;

        //write uncompressed size
        Size := LZH.StreamIn.Size;
        LZH.StreamOut.Write(Size, sizeof(Longint));

        // Compress stream.
        LZH.LZHPack(Bytes, LZH.GetBlockStream, LZH.PutBlockStream);
     finally
        LZH.Free;
     end;
end;

// LZH Decompress inmemory BLOB.
procedure LZHDecompressBlobStream(CompressedStream,DeCompressedStream:TStream);
var
   LZH: TLZH;
   Size, Bytes: Longint;
begin
    // Decompress in memory blob.
    LZH := TLZH.Create;
    try
       LZH.StreamIn:=CompressedStream;
       LZH.StreamOut:=DeCompressedStream;
       LZH.StreamIn.Position := 0;
       LZH.StreamOut.Position := 0;

       // Uncompressed file size
       LZH.StreamIn.Read(size, sizeof(Longint));
       Bytes := Size;

       // Decompress rest of stream.
       LZH.LZHUnpack(Bytes, LZH.GetBlockStream, LZH.PutBlockStream);
    finally
       LZH.Free;
    end;
end;

// LZH Compress CSV stream.
procedure LZHCompressSave(UnCompressedStream,CompressedStream:TStream);
var
   Size, Bytes:Longint;
   CheckValue: Longint;
   A: PChar;
   LZH:TLZH;
begin
     LZH:=TLZH.create;
     try
        LZH.StreamIn:=UnCompressedStream;
        LZH.StreamOut:=CompressedStream;
        LZH.StreamIn.Position := 0;
        LZH.StreamOut.Position := 0;

        //header
        GetMem(A, 10);
        try
           StrPCopy(A, '@LHC');

           { *** : improving productivity
           with TCRC32.Create do
             try
                CheckValue := CalcCRC32(LZH.StreamIn);
             finally
                free;
             end;
           {}

           //write header
           LZH.StreamOut.WriteBuffer(A^, 4);

           //write uncompressed size
           Size := LZH.StreamIn.Size;
           LZH.StreamOut.Write(Size, sizeof(Longint));

           //write crc32
           Size := CheckValue;
           LZH.StreamOut.Write(CheckValue, sizeof(Longint));

           //compress it !!! LZH.PutBlockStream stores into the destination stream
           LZH.LZHPack(Bytes, LZH.GetBlockStream, LZH.PutBlockStream);
        finally
           FreeMem(A);
        end;

     finally
        LZH.free;
     end;
end;

// LZH Decompress CSV stream.
procedure LZHDecompressLoad(CompressedStream,DeCompressedStream:TStream);
var
   Size, Bytes: Longint;
   CheckValue: Longint;
   A: array[0..10] of Char;
   LZH:TLZH;
begin
     LZH:=TLZH.create;
     try
       LZH.StreamIn:=CompressedStream;
       LZH.StreamOut:=DeCompressedStream;
       LZH.StreamIn.Position := 0;
       LZH.StreamOut.Position := 0;

       //read header
       Size := 4;
       FillChar(A[0], Length(A), 0); // *** FIXED.
       A[4] := #0;
       LZH.StreamIn.ReadBuffer(A[0], Size);
       LZH.StreamIn.Read(size, sizeof(Longint));
       LZH.StreamIn.Read(CheckValue, sizeof(Longint));

       //check header
       if Copy(StrPAS(A), 1, 4) <> '@LHC' then
          raise Exception.Create('Not LZH Compressed !');

       //uncompressed file size
       Bytes := Size;

       //decompress it !!!
       LZH.LZHUnpack(Bytes, LZH.GetBlockStream, LZH.PutBlockStream);

       //check original size
       if LZH.StreamOut.Size <> Size then
          raise Exception.Create('Decompression Error: Size Mismatch !');

       //check crc32
       {with TCRC32.Create do // *** FIXED (d'nt correctly calculate crc)
         try
            if CalcCRC32(LZH.StreamOut)<>CheckValue then
               raise Exception.Create('CRC Decompression Error !');
         finally
            free;
         end;
       {}

     finally
        LZH.free;
     end;
end;
{$endif}

{$ifdef ZIP}
function ZIPCompressBlockStream(const sIN, sOUT: TStream; CompressionLevel: TCompressionLevel): Boolean;
var
  p: PTPnt;
  Buf1, Buf2: PChar;
  BufSize, ZipSize: Longint;
begin
  BufSize := sIN.Size;
  ZipSize := BufSize + BufSize div 10;
  GetMem(Buf1, BufSize + 100);
  GetMem(Buf2, ZipSize);
  try
    sIN.ReadBuffer(Buf1^, BufSize);
    p := PTPnt(Buf1);
    Result := zCompres.compress2(pBytef(Buf2), ZipSize, p^, BufSize, Levels[CompressionLevel]) = Z_OK;
    sOUT.WriteBuffer(Buf2^, ZipSize);
    FreeMem(Buf2);
    FreeMem(Buf1);
  except
    Result := False;
  end;
end;

function ZIPUncompressBlockStream(const sIN, sOUT: TStream): Boolean;
var
  p: PTPnt;
  Buf1, Buf2: PChar;
  BufSize, ZipSize: Longint;
begin
  BufSize := sIN.Size;
  ZipSize := BufSize * 30; //enough ? :-)
  GetMem(Buf1, BufSize + 100);
  GetMem(Buf2, ZipSize);
  try
    sIN.Position := 0;
    sIN.ReadBuffer(Buf1^, BufSize);
    p := PTPnt(Buf1);
    Result := zUnCompr.uncompress(pBytef(Buf2), ZipSize, p^, BufSize) = Z_OK;
    sOUT.WriteBuffer(Buf2^, ZipSize);
    FreeMem(Buf2);
    FreeMem(Buf1);
  except
    Result := False;
  end;
end;

// ZIP Compress inmemory BLOB.
procedure ZIPCompressBlobStream(UnCompressedStream,CompressedStream:TStream);
var
   Size: Longint;
   TmpStrm: TStream;
begin
     TmpStrm := TMemoryStream.Create;
     try
        // Compress data to temporary stream.
        UnCompressedStream.Position := 0;
        ZIPCompressBlockStream(UnCompressedStream, TmpStrm, cplDefault);
        TmpStrm.Position := 0;
        Size := UnCompressedStream.Size;

        // Write header and copy temporary stream.
        CompressedStream.Write(Size, sizeof(Longint));
        CompressedStream.CopyFrom(TmpStrm, TmpStrm.Size);
     finally
        TmpStrm.Free;
     end;
end;

// ZIP Decompress inmemory BLOB.
procedure ZIPDecompressBlobStream(CompressedStream,DeCompressedStream:TStream);
var
   Size: Longint;
   TmpStrm: TStream;
begin
     // Read header.
     CompressedStream.Read(Size, sizeof(Longint));

     // Uncompress to a temporary stream.
     TmpStrm := TMemoryStream.Create;
     try
        CompressedStream.Seek(sizeof(Longint), soFromBeginning);
        TmpStrm.CopyFrom(CompressedStream, CompressedStream.Size - sizeof(Longint));
        ZIPUncompressBlockStream(TmpStrm, DeCompressedStream);

        // Check original size.
        if DeCompressedStream.Size <> Size then
           raise Exception.Create('Decompression Error ! Size Mismatch !');
     finally
        TmpStrm.Free;
     end;
end;

// ZIP Compress CSV stream.
procedure ZIPCompressSave(UnCompressedStream,CompressedStream:TStream);
var
  Size: Longint;
  TmpStrm: TStream;
begin
     // Compress the stream to a temporary stream.
     TmpStrm := TMemoryStream.Create;
     try
        UnCompressedStream.Position := 0;
        ZIPCompressBlockStream(UnCompressedStream, TmpStrm, cplDefault);
        TmpStrm.Position := 0;

        // Write header.
        CompressedStream.WriteBuffer(ZLCfileID, SizeOf(Longint));

        // Write size.
        Size := UnCompressedStream.Size;
        CompressedStream.Write(Size, SizeOf(Longint));

        // Calc. CRC 32 and write it.
        with TCRC32.Create do
          try
             Size:=CalcCRC32(UnCompressedStream);
          finally
             free;
          end;
        CompressedStream.Write(Size, SizeOf(Longint));

        // Add the compressed data.
        CompressedStream.CopyFrom(TmpStrm, TmpStrm.Size);
     finally
        TmpStrm.Free;
     end;
end;

// ZIP Decompress CSV stream.
procedure ZIPDecompressLoad(CompressedStream,DeCompressedStream:TStream);
var
   Size, FID: Longint;
   CheckValue: Longint;
   TmpStrm: TStream;
begin
     // Read header.
     CompressedStream.Read(FID, 4);
     CompressedStream.Read(Size, 4);
     CompressedStream.Read(CheckValue, 4);

     // Check signature.
     if FID <> ZLCfileID then
        raise Exception.Create('Not ZIP Compressed !');

     // Create temp. stream to decompress zip contents through.
     TmpStrm := TMemoryStream.Create;
     try
        CompressedStream.Seek(12, soFromBeginning);
        TmpStrm.CopyFrom(CompressedStream, CompressedStream.Size - 12);
        ZIPUncompressBlockStream(TmpStrm, DeCompressedStream);

        // Check original size.
        if DeCompressedStream.Size <> Size then
           raise Exception.Create('Decompression Error ! Size Mismatch !');

        // Check the crc.
        with TCRC32.Create do
          try
             if CheckValue <> CalcCRC32(DeCompressedStream) then
                raise Exception.Create('CRC32 Decompression Error !');
          finally
             free;
          end;

     finally
        TmpStrm.Free;
     end;
end;
{$endif}

end.
