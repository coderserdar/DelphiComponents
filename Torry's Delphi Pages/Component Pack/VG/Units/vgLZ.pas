{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Compression: LZSS with Huffman coding         }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgLZ;

interface
uses Classes, vgSystem;

type
{ TLZCompressor }
  TLZCompressor = class(TBlockCompressor)
  public
    procedure Compress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    procedure UnCompress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    class function Sign: TSignature; override;
  end;

const
  SignLZ         = 'vgLZ';

implementation
uses LZH;

class function TLZCompressor.Sign: TSignature;
begin
  Result := SignLZ;
end;

procedure TLZCompressor.Compress(Stream: TStream; const Buff; Count: Integer; Data: Pointer);
begin
  inherited;
  Stream.WriteBuffer(Count, SizeOf(Count));
  LZHPack(Count, GetBlock, PutBlock);
end;

procedure TLZCompressor.UnCompress(Stream: TStream; const Buff; Count: Integer; Data: Pointer);
var
  TextSize, I: Integer;
begin
  inherited;
  GetBlock(TextSize, SizeOf(TextSize), I);
  LZHUnPack(TextSize, GetBlock, PutBlock);
end;

initialization
  CompressorList.RegisterCompressor(TLZCompressor);

end.
