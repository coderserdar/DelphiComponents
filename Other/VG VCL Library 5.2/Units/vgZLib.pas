{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ZLIB compressor                               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgZLib;

interface
uses Classes, vgSystem, ZLIB;

type
{ TZLIBCompressor }
  TZLIBCompressor = class(TCompressor)
  public
    procedure Compress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    procedure UnCompress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    class function Sign: TSignature; override;
  end;

const
  SignZLIB       = 'ZLIB';

implementation
uses vgUtils;

{ TZLIBCompressor }
class function TZLIBCompressor.Sign: TSignature;
begin
  Result := SignZLIB;
end;

procedure TZLIBCompressor.Compress(Stream: TStream; const Buff;
  Count: Integer; Data: Pointer);
var
  OldPos, Size: Integer;
  ZStream: TStream;
  InStream: TReadMemoryStream;
begin
  InStream := TReadMemoryStream.Create;
  try
    InStream.SetPointer(@Buff, Count);
    OldPos := Stream.Position;
    Stream.WriteBuffer(Size, SizeOf(Size));
    Stream.WriteBuffer(Count, SizeOf(Size));
    ZStream := TCompressionStream.Create(TCompressionLevel(Data), Stream);
    try
      ZStream.CopyFrom(InStream, 0);
    finally
      ZStream.Free;
    end;
    Size := Stream.Size - 2 * SizeOf(Size);
    WriteBufferAt(Stream, Size, SizeOf(Size), OldPos);
  finally
    InStream.Free;
  end;
end;

procedure TZLIBCompressor.UnCompress(Stream: TStream; const Buff;
  Count: Integer; Data: Pointer);
var
  InStream: TReadMemoryStream;
  ZStream: TStream;
  Size: Integer;
begin
  InStream := TReadMemoryStream.Create;
  try
    Size := Integer(Buff);
    Count := Integer(Pointer(PChar(@Buff) + SizeOf(Size))^);
    InStream.SetPointer((PChar(@Buff) + 2 * SizeOf(Size)), Size);
    ZStream := TDecompressionStream.Create(InStream);
    try
      Stream.CopyFrom(ZStream, Count);
    finally
      ZStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

initialization
  CompressorList.RegisterCompressor(TZLIBCompressor);

end.
