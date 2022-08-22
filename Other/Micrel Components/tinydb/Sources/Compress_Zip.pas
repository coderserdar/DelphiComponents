
{**********************************************************}
{                                                          }
{  Zip Compression Algorithm for TinyDB                    }
{                                                          }
{**********************************************************}

unit Compress_Zip;

{$I TinyDB.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, TinyDB, {$IFDEF BUILT_ZLIB}ZLibUnit{$ELSE}ZLib{$ENDIF};

type

  TCompAlgo_Zip = class(TCompressAlgo)
  private
    FLevel: TCompressLevel;
    FBlockSize: Integer;

    function ConvertCompLevel(Value: TCompressLevel): TCompressionLevel;
    procedure SetBlockSize(Value: Integer);
    function CompressBlock( DestBlock: PAnsiChar; var DestSize: Integer; SrcBlock: PAnsiChar; SrcSize: Integer ): Boolean;
    function UncompressBlock( DestBlock: PAnsiChar; var DestSize: Integer; SrcBlock: PAnsiChar; SrcSize: Integer): Boolean;
    procedure InternalDoEncodeProgress(Size, Pos: Integer);
    procedure InternalDoDecodeProgress(Size, Pos: Integer);
  protected
    procedure SetLevel(Value: TCompressLevel); override;
    function GetLevel: TCompressLevel; override;

    property BlockSize: Integer read FBlockSize write SetBlockSize;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
  end;

implementation

function Min( a, b : Integer ) : Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function EncodeBlockSize( IsCompressed : Boolean; BlockSize : Integer ) : Integer;
begin
  // Format:
  // Bit 31 = 1 ? Compressed : Uncompressed
  // Bits 30..0 = Block size
  Result := BlockSize;
  if not IsCompressed then
    Result := - Result;
end;

procedure DecodeBlockSize( Size : Integer; var IsCompressed : Boolean; var BlockSize : Integer);
begin
  // Format:
  // Bit 31 = 1 ? Compressed : Uncompressed
  // Bits 30..0 = Block size
  IsCompressed := Size >= 0;
  Size := Abs(Size);
  BlockSize := Size;
end;

function ReadInteger( S : TStream ) : Integer;
begin
  S.ReadBuffer( Result, Sizeof(Result) );
end;

procedure WriteInteger( S : TStream; Val : Integer );
begin
  S.WriteBuffer( Val, Sizeof(Val) );
end;

constructor TCompAlgo_Zip.Create(AOwner: TObject);
begin
  inherited;
  FLevel := clNormal;
  FBlockSize := 65536;
end;

destructor TCompAlgo_Zip.Destroy;
begin
  inherited Destroy;
end;

function TCompAlgo_Zip.ConvertCompLevel(Value: TCompressLevel): TCompressionLevel;
begin
  case Value of
    clMaximum:              Result := clMax;
    clNormal:               Result := clDefault;
    clFast, clSuperFast:    Result := clFastest;
  else
    Result := clNone;
  end;
end;

procedure TCompAlgo_Zip.SetBlockSize(Value: Integer);
begin
  if (Value < 100) then Value := 100;
  FBlockSize := Value;
end;

function TCompAlgo_Zip.CompressBlock( DestBlock : PAnsiChar; var DestSize : Integer; SrcBlock : PAnsiChar; SrcSize : Integer ) : Boolean;
var
  DestPtr: Pointer;
begin
  Result := True;
  try
    {$IFDEF DELPHI_16_UP}
    ZCompress(SrcBlock, SrcSize, DestPtr, DestSize, ConvertCompLevel(FLevel));
    {$ELSE}
    CompressBuf(SrcBlock, SrcSize, DestPtr, DestSize, ConvertCompLevel(FLevel));
    {$ENDIF}
    Move(DestPtr^, DestBlock^, DestSize);
    FreeMem(DestPtr);
  except
    Result := False;
  end;
end;

function  TCompAlgo_Zip.UncompressBlock( DestBlock : PAnsiChar; var DestSize : Integer; SrcBlock : PAnsiChar; SrcSize : Integer) : Boolean;
var
  DestPtr: Pointer;
begin
  Result := True;
  try
    {$IFDEF DELPHI_16_UP}
    ZDecompress(SrcBlock, SrcSize, DestPtr, DestSize);
    {$ELSE}
    DecompressBuf(SrcBlock, SrcSize, 0, DestPtr, DestSize);
    {$ENDIF}
    Move(DestPtr^, DestBlock^, DestSize);
    FreeMem(DestPtr);
  except
    Result := False;
  end;
end;

procedure TCompAlgo_Zip.InternalDoEncodeProgress(Size, Pos: Integer);
begin
  if Size = 0 then
    DoEncodeProgress(0)
  else
    DoEncodeProgress(Round(Pos / Size * 100));
end;

procedure TCompAlgo_Zip.InternalDoDecodeProgress(Size, Pos: Integer);
begin
  if Size = 0 then
    DoDecodeProgress(0)
  else
    DoDecodeProgress(Round(Pos / Size * 100));
end;

function TCompAlgo_Zip.GetLevel: TCompressLevel;
begin
  Result := FLevel;
end;

procedure TCompAlgo_Zip.SetLevel(Value: TCompressLevel);
begin
  FLevel := Value;
end;

procedure TCompAlgo_Zip.EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
var
  SrcBlock, DstBlock: PAnsiChar;
  BytesProcessed, ToRead, CompressBlockSize: Integer;
  IsCompressed : Boolean;
  DestStream: TMemoryStream;
begin
  Source.Position := 0;
  Dest.Position := 0;
  if Source = Dest then
    DestStream := TMemoryStream.Create
  else begin
    Dest.Clear;
    DestStream := Dest;
  end;
  GetMem(SrcBlock, FBlockSize);
  GetMem(DstBlock, FBlockSize);
  try
    BytesProcessed := 0;
    while BytesProcessed < DataSize do
    begin
      InternalDoEncodeProgress(DataSize, BytesProcessed);

      ToRead := Min(FBlockSize, DataSize - BytesProcessed);
      Source.Read(SrcBlock^, ToRead);
      CompressBlockSize := FBlockSize;
      if not CompressBlock(DstBlock, CompressBlockSize, SrcBlock, ToRead) then
        raise Exception.Create( 'Cannot Compress.' );

      if CompressBlockSize >= ToRead then
      begin
        // Compression was not interressant, so use uncompressed block
        IsCompressed := False;
        CompressBlockSize := ToRead;
        Move(SrcBlock^, DstBlock^, CompressBlockSize);
      end
      else
        IsCompressed := True;

      WriteInteger(DestStream, EncodeBlockSize(IsCompressed, CompressBlockSize));
      DestStream.Write(DstBlock^, CompressBlockSize);
      Inc(BytesProcessed, ToRead);
    end;
    if Source = Dest then
    begin
      Dest.Clear;
      Dest.CopyFrom(DestStream, 0);
    end;
    InternalDoEncodeProgress(DataSize, BytesProcessed);
  finally
    if Source = Dest then DestStream.Free;
    FreeMem(DstBlock);
    FreeMem(SrcBlock);
    InternalDoEncodeProgress(0, 0);
  end;
end;

procedure TCompAlgo_Zip.DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
var
  SrcBlock, DstBlock: PAnsiChar;
  BytesProcessed, ToRead: Integer;
  BlockSize, UncompressBlockSize: Integer;
  IsCompressed: Boolean;
  DestStream: TMemoryStream;
begin
  Source.Position := 0;
  Dest.Position := 0;
  if Source = Dest then
    DestStream := TMemoryStream.Create
  else begin
    Dest.Clear;
    DestStream := Dest;
  end;
  GetMem(SrcBlock, FBlockSize);
  GetMem(DstBlock, FBlockSize);
  try
    BytesProcessed := 0;
    IsCompressed := True;
    while BytesProcessed < DataSize do
    begin
      InternalDoDecodeProgress(DataSize, BytesProcessed);

      ToRead := ReadInteger(Source);
      DecodeBlockSize(ToRead, IsCompressed, BlockSize);
      ToRead := BlockSize;
      Source.Read(SrcBlock^, BlockSize);

      if IsCompressed then
      begin
        UncompressBlockSize := FBlockSize;
        if not UncompressBlock(DstBlock, UncompressBlockSize, SrcBlock, BlockSize) then
          raise Exception.Create('Cannot Uncompress.');
      end
      else
      begin // if it was not, then just copy it
        UncompressBlockSize := BlockSize;
        Move(SrcBlock^, DstBlock^, UncompressBlockSize);
      end;

      DestStream.Write(DstBlock^, UncompressBlockSize);
      BytesProcessed := BytesProcessed + ToRead + Sizeof(Integer);
    end;
    if Source = Dest then
    begin
      Dest.Clear;
      Dest.CopyFrom(DestStream, 0);
    end;
    InternalDoDecodeProgress(DataSize, BytesProcessed);
  finally
    if Source = Dest then DestStream.Free;
    FreeMem(DstBlock);
    FreeMem(SrcBlock);
    InternalDoDecodeProgress(0, 0);
  end;
end;

initialization
  RegisterCompressClass(TCompAlgo_Zip, 'ZIP');

end.