{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Compression: LZSS32                           }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgLZSS32;

interface
uses Classes, vgSystem;

type
{ TLZ32Compressor }
  TLZSS32Compressor = class(TBlockCompressor)
  public
    procedure Compress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    procedure UnCompress(Stream: TStream; const Buff; Count: Integer; Data: Pointer); override;
    class function Sign: TSignature; override;
  end;

{$I vgLZSS32.INC}

const
  DLLName         = 'vgLZSS32.DLL';
  SignLZSS32      = 'LZ32';

procedure LZSquash(ReadProc: TLZReadEvent; WriteProc: TLZWriteEvent); stdcall; external DLLName;
procedure LZUnSquash(ReadProc: TLZReadEvent; WriteProc: TLZWriteEvent); stdcall; external DLLName;

implementation

class function TLZSS32Compressor.Sign: TSignature;
begin
  Result := SignLZSS32;
end;

procedure TLZSS32Compressor.Compress(Stream: TStream; const Buff; Count: Integer; Data: Pointer);
begin
  inherited;
  Stream.WriteBuffer(Count, SizeOf(Count));
  LZSquash(GetBlock, PutBlock);
end;

procedure TLZSS32Compressor.UnCompress(Stream: TStream; const Buff; Count: Integer; Data: Pointer);
var
  TextSize, I: Integer;
begin
  inherited;
  GetBlock(TextSize, SizeOf(TextSize), I);
  LZUnSquash(GetBlock, PutBlock);
end;

initialization
  CompressorList.RegisterCompressor(TLZSS32Compressor);

end.
