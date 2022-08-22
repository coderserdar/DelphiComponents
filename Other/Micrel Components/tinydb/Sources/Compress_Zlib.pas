
{**********************************************************}
{                                                          }
{  Zlib Compression Algorithm for TinyDB                   }
{                                                          }
{**********************************************************}

unit Compress_Zlib;

{$I TinyDB.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs,
  Db, TinyDB, {$IFDEF BUILT_ZLIB}ZLibUnit{$ELSE}ZLib{$ENDIF};

type
  TCompAlgo_Zlib = class(TCompressAlgo)
  private
    FLevel: TCompressLevel;
    FSourceSize: Integer;

    function ConvertCompLevel(Value: TCompressLevel): TCompressionLevel;

    procedure Compress(SourceStream, DestStream: TMemoryStream;
      const CompressionLevel: TCompressionLevel);
    procedure Decompress(SourceStream, DestStream: TMemoryStream);

    procedure DoCompressProgress(Sender: TObject);
    procedure DoDecompressProgress(Sender: TObject);
    procedure InternalDoEncodeProgress(Size, Pos: Integer);
    procedure InternalDoDecodeProgress(Size, Pos: Integer);
  protected
    procedure SetLevel(Value: TCompressLevel); override;
    function GetLevel: TCompressLevel; override;
  public
    constructor Create(AOwner: TObject); override;

    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
  end;

implementation

{ TCompAlgo_Zlib }

constructor TCompAlgo_Zlib.Create(AOwner: TObject);
begin
  inherited;
  FLevel := clNormal;
end;

function TCompAlgo_Zlib.ConvertCompLevel(Value: TCompressLevel): TCompressionLevel;
begin
  case Value of
    clMaximum:              Result := clMax;
    clNormal:               Result := clDefault;
    clFast, clSuperFast:    Result := clFastest;
  else
    Result := clNone;
  end;
end;

//-----------------------------------------------------------------------------
// Compress data
//-----------------------------------------------------------------------------
procedure TCompAlgo_Zlib.Compress(SourceStream, DestStream: TMemoryStream;
  const CompressionLevel: TCompressionLevel);
var
  CompStream: TCompressionStream;
  Count: Integer;
begin
  //If the source data length is 0
  FSourceSize := SourceStream.Size;
  if SourceStream.Size = 0 then
  begin
    DestStream.Clear;
    Exit;
  end;

  //In DestStream head size before writing data compression
  Count := SourceStream.Size;
  DestStream.Clear;
  DestStream.WriteBuffer(Count, SizeOf(Integer));

  //Write compressed data stream (SourceStream in the preservation of the original data stream)
  CompStream := TCompressionStream.Create(CompressionLevel, DestStream);
  try
    CompStream.OnProgress := DoCompressProgress;
    SourceStream.SaveToStream(CompStream);
  finally
    CompStream.Free;
  end;
  DestStream.Position := 0;

  //If the compressed length but an increase
  if DestStream.Size - SizeOf(Integer) >= SourceStream.Size then
  begin
    Count := -Count;   //With a negative number indicates that the data is not compressed
    DestStream.Clear;
    DestStream.WriteBuffer(Count, SizeOf(Integer));
    DestStream.CopyFrom(SourceStream, 0);
  end;
end;

//-----------------------------------------------------------------------------
// Decompress data
//-----------------------------------------------------------------------------
procedure TCompAlgo_Zlib.Decompress(SourceStream, DestStream: TMemoryStream);
var
  DecompStream: TDecompressionStream;
  TempStream: TMemoryStream;
  Count: Integer;
begin
  //If the source data length is 0
  FSourceSize := SourceStream.Size;
  if SourceStream.Size = 0 then
  begin
    DestStream.Clear;
    Exit;
  end;

  //Never compressed data bytes read head
  SourceStream.Position := 0;
  SourceStream.ReadBuffer(Count, SizeOf(Integer));

  //If the data is compressed
  if Count >= 0 then
  begin
    //Data decompression
    DecompStream := TDecompressionStream.Create(SourceStream);
    try
      DecompStream.OnProgress := DoDecompressProgress;
      TempStream := TMemoryStream.Create;
      try
        TempStream.SetSize(Count);
        DecompStream.ReadBuffer(TempStream.Memory^, Count);
        DestStream.LoadFromStream(TempStream);
        DestStream.Position := 0;
      finally
        TempStream.Free;
      end;
    finally
      DecompStream.Free;
    end;
  end else
  //If the data is not compressed
  begin
    DestStream.Clear;
    DestStream.CopyFrom(SourceStream, SourceStream.Size - SizeOf(Integer));
  end;
end;

procedure TCompAlgo_Zlib.DoCompressProgress(Sender: TObject);
begin
  InternalDoEncodeProgress(FSourceSize, (Sender as TCompressionStream).{$IFDEF BUILT_ZLIB}BytesProcessed{$ELSE}Position{$ENDIF});
end;

procedure TCompAlgo_Zlib.DoDecompressProgress(Sender: TObject);
begin
  InternalDoDecodeProgress(FSourceSize, (Sender as TDecompressionStream).{$IFDEF BUILT_ZLIB}BytesProcessed{$ELSE}Position{$ENDIF});
end;

procedure TCompAlgo_Zlib.InternalDoEncodeProgress(Size, Pos: Integer);
begin
  if Size = 0 then
    DoEncodeProgress(0)
  else
    DoEncodeProgress(Round(Pos / Size * 100));
end;

procedure TCompAlgo_Zlib.InternalDoDecodeProgress(Size, Pos: Integer);
begin
  if Size = 0 then
    DoDecodeProgress(0)
  else
    DoDecodeProgress(Round(Pos / Size * 100));
end;

procedure TCompAlgo_Zlib.EncodeStream(Source, Dest: TMemoryStream;
  DataSize: Integer);
begin
  Compress(Source, Dest, ConvertCompLevel(FLevel));
end;

procedure TCompAlgo_Zlib.DecodeStream(Source, Dest: TMemoryStream;
  DataSize: Integer);
begin
  Decompress(Source, Dest);
end;

procedure TCompAlgo_Zlib.SetLevel(Value: TCompressLevel);
begin
  FLevel := Value;
end;

function TCompAlgo_Zlib.GetLevel: TCompressLevel;
begin
  Result := FLevel;
end;

initialization
  RegisterCompressClass(TCompAlgo_Zlib, 'ZLIB');
   
end.
