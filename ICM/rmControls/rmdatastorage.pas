{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmDataStorage
Purpose  : To allow the storage of text or binary data within the application 
Date     : 03-04-2000
Author   : Ryan J. Mills
Version  : 1.90
Notes    : The original idea for these components came from Daniel Parnell's
           TStringz component.  I created these after I came across some
           limitations in that component.  These components are more functional
           and provide more design time properties.
================================================================================}

unit rmDataStorage;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rmLibrary;

type
  TrmDataStorageFileName = string;
  TrmDataStorageLongint = Longint;

  TrmCustomDataStorage = class(TComponent)
  private
    { Private declarations }
    procedure SetDataSize(const Value: TrmDataStorageLongint);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBinaryData(Stream: TStream); virtual;
    procedure WriteBinaryData(Stream: TStream); virtual;
    function GetDataSize: TrmDataStorageLongint; virtual;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
    procedure WriteToFile(filename:string); virtual;
    procedure LoadFromFile(FileName:string); virtual;
    procedure WriteToStream(Dest:TStream); virtual;
    procedure ClearData; virtual;
  published
    { Published declarations }
    property DataSize : TrmDataStorageLongint read GetDataSize write SetDataSize stored false;
  end;

  TrmTextDataStorage = class(TrmCustomDataStorage)
  private
    { Private declarations }
    fData : TStringList;
    procedure SetData(const Value: TStringList);
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
  published
    { Published declarations }
    property Data : TStringList read fData write SetData stored False;
  end;

  TrmBinaryDataStorage = class(TrmCustomDataStorage)
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
    procedure WriteToStream(Dest:TStream); override;
    procedure ClearData; override;
    property Data : TMemoryStream read GetData;
  published
    { Published declarations }
  end;

implementation

{ TrmCustomDataStorage }

procedure TrmCustomDataStorage.ClearData;
begin
   if not (csdesigning in componentstate) then
      Raise Exception.create('Not in design mode');
end;

constructor TrmCustomDataStorage.create(AOwner: TComponent);
begin
  inherited;
end;

procedure TrmCustomDataStorage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('StoredData',ReadBinaryData, WriteBinaryData, True);
end;

function TrmCustomDataStorage.GetDataSize: TrmDataStorageLongint;
begin
   Result := 0;  
end;

procedure TrmCustomDataStorage.LoadFromFile(FileName: string);
begin
   if not (csdesigning in componentstate) then
      Raise Exception.create('Not in design mode.');

   if not FileExists(filename) then
      Raise Exception.create('File does not exist.');
end;

procedure TrmCustomDataStorage.ReadBinaryData(Stream: TStream);
begin
   //Do Nothing....
end;

procedure TrmCustomDataStorage.SetDataSize(
  const Value: TrmDataStorageLongint);
begin
   //Do Nothing....
end;

procedure TrmCustomDataStorage.WriteBinaryData(Stream: TStream);
begin
   //Do Nothing....
end;

procedure TrmCustomDataStorage.WriteToFile(filename: string);
begin
   //Do Nothing....
end;

procedure TrmCustomDataStorage.WriteToStream(Dest: TStream);
begin
//Do Nothing...
end;

{ TrmTextDataStorage }

procedure TrmTextDataStorage.ClearData;
begin
  inherited;
  fData.Clear;
end;

constructor TrmTextDataStorage.create(AOwner: TComponent);
begin
  inherited;
  fData := TStringlist.create;
end;

destructor TrmTextDataStorage.destroy;
begin
  inherited;
  fData.free;
end;

function TrmTextDataStorage.GetDataSize: TrmDataStorageLongint;
begin
   Result := length(fData.Text);  
end;

procedure TrmTextDataStorage.LoadFromFile(FileName: string);
begin
   inherited;
   fData.LoadFromFile(filename);
end;

procedure TrmTextDataStorage.ReadBinaryData(Stream: TStream);
var
  n : longint;
  wData : TMemoryStream;
begin
  fData.Clear;

  Stream.ReadBuffer(n,SizeOf(n));

  if n > 0 then
  begin
    wData := tmemoryStream.create;
    try
       if not assigned(wData) then
         raise Exception.Create('Out of memory');
       wData.CopyFrom(Stream, n);
       wData.Position := 0;
       fData.LoadFromStream(wData);
    finally
       wData.free;    
    end;
  end;
end;

procedure TrmTextDataStorage.SetData(const Value: TStringList);
begin
  if csDesigning in componentState then
     FData.assign(Value)
  else
     Raise Exception.create('Not in design mode.');
end;

procedure TrmTextDataStorage.WriteBinaryData(Stream: TStream);
var
   wData : TMemoryStream;
   n : longint;
begin
  wData := tmemoryStream.create;
  try
     fData.SaveToStream(wData);
     n := wData.Size;

     Stream.WriteBuffer(n, sizeof(n));
     if n > 0 then
       Stream.CopyFrom(wData, 0);
  finally
     wData.free;
  end;
end;

procedure TrmTextDataStorage.WriteToFile(filename: string);
begin
  inherited;
  fData.SaveToFile(filename);
end;

procedure TrmTextDataStorage.WriteToStream(Dest: TStream);
begin
   fdata.SaveToStream(dest);
end;

{ TrmBinaryDataStorage }

procedure TrmBinaryDataStorage.ClearData;
begin
  inherited;
  fData.Clear;
end;

constructor TrmBinaryDataStorage.create(AOwner: TComponent);
begin
  inherited;
  fData := TMemoryStream.Create;
end;

destructor TrmBinaryDataStorage.destroy;
begin
  inherited;
  fData.free;
end;

function TrmBinaryDataStorage.GetData: TMemoryStream;
begin
   fData.Position := 0;
   Result := fData;  
end;

function TrmBinaryDataStorage.GetDataSize: TrmDataStorageLongint;
begin
   result := fData.Size;  
end;

procedure TrmBinaryDataStorage.LoadFromFile(FileName: string);
begin
   inherited;
   fData.LoadFromFile(filename);
end;

procedure TrmBinaryDataStorage.ReadBinaryData(Stream: TStream);
var
  n : longint;
begin
  fData.Clear;

  Stream.ReadBuffer(n,SizeOf(n));

  if n > 0 then
     fData.CopyFrom(Stream, n);
end;

procedure TrmBinaryDataStorage.WriteBinaryData(Stream: TStream);
var
   n : longint;
begin
  n := fData.Size;

  Stream.WriteBuffer(n, sizeof(n));

  if n > 0 then
    Stream.CopyFrom(fData, 0);
end;

procedure TrmBinaryDataStorage.WriteToFile(filename: string);
begin
   fData.SaveToFile(filename);
end;

procedure TrmBinaryDataStorage.WriteToStream(Dest: TStream);
begin
   fData.Position := 0; 
   TrmStream(fData).copyto(Dest,0);  
end;

end.
