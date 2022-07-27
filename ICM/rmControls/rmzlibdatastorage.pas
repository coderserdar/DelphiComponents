{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmZLIBDataStorage
Purpose  : To allow the storage of ZLIB Compressed data within the application
Date     : 03-04-2000
Author   : Ryan J. Mills
Version  : 1.90
Notes    : See rmDataStorage.pas for further notes.
================================================================================}

unit rmZLIBDataStorage;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rmDataStorage, rmZLIB, rmLibrary;

type
  TrmZLIBDataStorage = class(TrmCustomDataStorage)
  private
    { Private declarations }
    fData : TMemoryStream;
    fCompressionLevel: TCompressionLevel;
    fOriginalSize: TrmDataStorageLongint;
    function GetData: TMemoryStream;
    procedure SetOriginalSize(const Value: TrmDataStorageLongint);
  protected
    { Protected declarations }
    procedure ReadBinaryData(Stream: TStream); override;
    procedure WriteBinaryData(Stream: TStream); override;
    function GetDataSize: TrmDataStorageLongint; override;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    procedure WriteToFile(filename:string); override;
    procedure LoadFromFile(FileName:string); override;
    procedure WriteToStream(Dest:TStream); override;

    procedure ClearData; override;
    property Data : TMemoryStream read GetData;

  published
    { Published declarations }
    property OriginalSize : TrmDataStorageLongint read fOriginalSize write SetOriginalSize;
    property CompressionLevel : TCompressionLevel read fCompressionLevel write fCompressionLevel default clDefault;
  end;

implementation

{ TrmZLIBDataStorage }

procedure TrmZLIBDataStorage.ClearData;
begin
  inherited;
  fData.Clear;
  fOriginalSize := 0;
end;

constructor TrmZLIBDataStorage.create(AOwner: TComponent);
begin
  inherited;
  fData := TMemoryStream.Create;
  fCompressionLevel := clDefault;
end;

destructor TrmZLIBDataStorage.destroy;
begin
  inherited;
  fData.free;
end;

function TrmZLIBDataStorage.GetData: TMemoryStream;
begin
   fData.Position := 0;
   Result := fData;
end;

function TrmZLIBDataStorage.GetDataSize: TrmDataStorageLongint;
begin
   result := fData.Size;
end;

procedure TrmZLIBDataStorage.LoadFromFile(FileName: string);
var
   InFile : TFileStream;
   wCompStream : TrmZLIBCompressionStream;
begin
   Inherited;
   ClearData;

   InFile := TFileStream.Create(filename, fmopenread);
   try
      fOriginalSize := InFile.Size;
      wCompStream := TrmZLIBCompressionStream.create(fCompressionLevel, fData);
      try
         wCompStream.CopyFrom(InFile, 0);
      finally
         wCompStream.Free;
      end;
   finally
      InFile.free;
   end;
end;

procedure TrmZLIBDataStorage.ReadBinaryData(Stream: TStream);
var
  n : longint;
begin
  fData.Clear;

  Stream.ReadBuffer(n,SizeOf(n));

  if n > 0 then
     fData.CopyFrom(Stream, n);
end;

procedure TrmZLIBDataStorage.setOriginalSize(const Value: integer);
begin
  if (csLoading in componentstate) then
     fOriginalSize := Value;
end;

procedure TrmZLIBDataStorage.WriteBinaryData(Stream: TStream);
var
   n : longint;
begin
  n := fData.Size;

  Stream.WriteBuffer(n, sizeof(n));

  if n > 0 then
    Stream.CopyFrom(fData, 0);
end;

procedure TrmZLIBDataStorage.WriteToFile(filename: string);
var
   OutFile : TFileStream;
   wCompStream : TrmZLIBDecompressionStream;
begin
   OutFile := TFileStream.Create(filename, fmcreate);
   try
      fData.Position := 0;
      wCompStream := TrmZLIBDecompressionStream.create(fData);
      try
         TrmStream(wCompStream).CopyTo(OutFile, fOriginalSize);
      finally
         wCompStream.Free;
      end;
   finally
      OutFile.free;
   end;
end;

procedure TrmZLIBDataStorage.WriteToStream(Dest: TStream);
var
   wCompStream : TrmZLIBDecompressionStream;
begin
   fData.Position := 0;
   wCompStream := TrmZLIBDecompressionStream.create(fData);
   try
      TrmStream(wCompStream).CopyTo(Dest, fOriginalSize);
   finally
      wCompStream.Free;
   end;
end;

end.
