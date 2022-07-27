{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmDataStorageEx
Purpose  : To allow the storage of ZLIB Compressed data within the application
Date     : 03-04-2000
Author   : Ryan J. Mills
Version  : 1.70
Notes    : See rmDataStorage.pas for further notes.
================================================================================}

unit rmDataStorageEx;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rmDataStorage, ZLIB;

type
  TrmCompressedDataStorage = class(TrmCustomDataStorage)
  private
    { Private declarations }
    fData : TMemoryStream;
    function GetData: TMemoryStream;
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
    procedure ClearData; override;
    property Data : TMemoryStream read GetData;
  published
    { Published declarations }
  end;

implementation

{ TrmCompressedDataStorage }

procedure TrmCompressedDataStorage.ClearData;
begin
  inherited;
  fData.Clear;
end;

constructor TrmCompressedDataStorage.create(AOwner: TComponent);
begin
  inherited;
  fData := TMemoryStream.Create;
end;

destructor TrmCompressedDataStorage.destroy;
begin
  inherited;
  fData.free;
end;

function TrmCompressedDataStorage.GetData: TMemoryStream;
begin
   fData.Position := 0;
   Result := fData;
end;

function TrmCompressedDataStorage.GetDataSize: TrmDataStorageLongint;
begin
   result := fData.Size;  
end;

procedure TrmCompressedDataStorage.LoadFromFile(FileName: string);
begin
   inherited;
   fData.LoadFromFile(filename);
end;

procedure TrmCompressedDataStorage.ReadBinaryData(Stream: TStream);
var
  n : longint;
begin
  fData.Clear;

  Stream.ReadBuffer(n,SizeOf(n));

  if n > 0 then
     fData.CopyFrom(Stream, n);
end;

procedure TrmCompressedDataStorage.WriteBinaryData(Stream: TStream);
var
   n : longint;
begin
  n := fData.Size;

  Stream.WriteBuffer(n, sizeof(n));

  if n > 0 then
    Stream.CopyFrom(fData, 0);
end;

procedure TrmCompressedDataStorage.WriteToFile(filename: string);
begin
   fData.SaveToFile(filename);
end;

end.
