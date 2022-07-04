(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomDataProcessors;

interface
uses Classes, Windows, Graphics, NMMCommon;

type
 TCustomImageComparer = class(TObject)
  procedure GetDeltaStream( AOldBitmap: TStream;
                            ANewBitmap: TStream;
                            ADiffStream: TStream ); virtual; abstract;
  procedure AssembleNextImage( AOldBitmap: TStream;
                               ANewBitmap: TStream;
                               ADiffStream: TStream ); virtual; abstract;
 end;

 TNMMCustomImageComparer = class(TObject)
 protected
   FOnPictureWndUpdate: TPictureWndUpdateEvent;
   procedure PictureWndUpdate(Sender: TObject; R: TRect);
   function GetPacketCount: Integer; virtual; abstract;
 public
   {for server side}
   function GetIniImageStream: TStream; virtual; abstract;
   procedure ProcessNewImage(ANewBitmap: TBitmap); virtual; abstract;
   procedure GetPacket(ADelta: TStream); virtual; abstract;
   procedure SetIniImage(AImage: TBitmap); virtual; abstract;
   procedure Clear; virtual;
   function Empty: Boolean; virtual; abstract;
   {for client side}
   procedure AddPacket(APacket: TStream); virtual; abstract;
   procedure AssembleNextImage(ANewBitmap: TBitmap); virtual; abstract;
   procedure SetIniImageStream(AStream: TStream); virtual; abstract;
   procedure GetIniImage(ABitmap: TBitmap); virtual; abstract;

   property PacketCount: Integer read GetPacketCount;
   property OnPictureWndUpdate: TPictureWndUpdateEvent
            read FOnPictureWndUpdate write FOnPictureWndUpdate;
 end;

 TNMMCustomBitmapProcessor = class(TObject)
   procedure Process( ABitmap: TBitmap ); virtual; abstract;
 end;

 TNMMImageProcessor = class(TObject)
 public
   procedure Bmp2Jpg( ADecodedBitmap: TBitmap; AEncodedImageStream: TStream ); overload; virtual; abstract;
   procedure Bmp2Jpg( ADecodedImageStream: TStream;
                     AEncodedImageStream: TStream ); overload; virtual; abstract;
   procedure Bmp2Jpg( AStream: TStream ); overload; virtual; abstract;
   procedure Jpg2Bmp( AEncodedImageStream: TStream; ADecodedBitmap: TBitmap ); overload; virtual; abstract;
   procedure Jpg2Bmp( AEncodedImageStream: TStream;
                     ADecodedImageStream: TStream ); overload; virtual; abstract;
   procedure Jpg2Bmp( AStream: TStream ); overload; virtual; abstract;
 end;

 TNMMMultiReferedObject= class(TObject)
 public
   RefCount: Integer;
   procedure AddRef;
   procedure Release;
   constructor Create; overload;
 end;

 TNMMDataPacket = class(TNMMMultiReferedObject)
 protected
   FData: TStream;
   FTime: TDateTime;
   FMarked: Boolean;
 public
   constructor Create; overload;
   destructor Destroy; override;

   function GetId: DWord; virtual; abstract;
   procedure SetID(AValue: DWord); virtual; abstract;
   procedure Update(AStream: TStream);
   procedure GetPackedData(AStream: TStream); virtual;
   procedure SetPackedData(AStream: TStream); virtual;
   procedure Clear; virtual;

   property Data: TStream read FData write FData;
   property Time: TDateTime read FTime write FTime;
   property Marked: Boolean read FMarked write FMarked;
 end;

 TNMMCountedDataPacket = class(TNMMDataPacket)
 protected
   FID: Integer;
 public
   function GetId: DWord; override;
   procedure SetID(AValue: DWord); override;
 end;

 TNMMDataPacketInfo = class(TObject)
 public
   Id: Integer;
   Time: TDateTime;
   procedure CopyFrom(ADataPacket: TNMMDataPacket);
 end;

 TNMMDataQueue= class(TObject)
 protected
   FList: TThreadList;
   FInBytes, FOutBytes: Longint;
   FFinished: Boolean;
   function GetCount: Integer;
 public
   constructor Create;
   destructor Destroy; override;
   procedure Add(ADataPacket: TNMMDataPacket); virtual;
   function Get(AIndex: Integer): TNMMDataPacket; virtual;
   procedure Remove(AIndex: Integer); virtual;
   procedure RemoveMarked;
   procedure Clear; virtual;

   property Count: Integer read GetCount;
   property InBytes: Longint read FInBytes;
   property OutBytes: Longint read FOutBytes;
   property Finished: Boolean read FFinished write FFinished;
 end;

 TCustomNMMPacketManager= class(TObject)
 protected
   FQueue: TNMMDataQueue;
   function CreateQueue: TNMMDataQueue; virtual; abstract;
 public
   constructor Create;
   destructor Destroy; override;
   procedure Clear; virtual;
   property Queue: TNMMDataQueue read FQueue;
 end;


implementation
uses SysUtils;

{TNMMCustomImageComparer}
procedure TNMMCustomImageComparer.Clear;
begin
end;

procedure TNMMCustomImageComparer.PictureWndUpdate(Sender: TObject; R: TRect);
begin
  if Assigned(FOnPictureWndUpdate) then
     FOnPictureWndUpdate(Sender,R);
end;


{TCustomNMMPacketManager}
constructor TCustomNMMPacketManager.Create;
begin
  inherited;
  FQueue:= CreateQueue;
end;

destructor TCustomNMMPacketManager.Destroy;
begin
  if FQueue<>nil then
    FreeAndNil(FQueue);
  inherited;
end;

procedure TCustomNMMPacketManager.Clear;
begin
  if FQueue<>nil then
    FQueue.Clear;
end;


{TNMMDataQueue}
constructor TNMMDataQueue.Create;
begin
  inherited;
  FList:= TThreadList.Create;
  FList.Duplicates:= dupError;
end;

destructor TNMMDataQueue.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TNMMDataQueue.Clear;
begin
  while Getcount > 0 do
    Remove(0);
end;

function TNMMDataQueue.GetCount: Integer;
var LList: TList;
begin
  LList:= FList.LockList;
  try
    result:= LList.Count;
  finally
    FList.UnlockList;
  end;
end;

procedure TNMMDataQueue.Add(ADataPacket: TNMMDataPacket);
begin
  try
    FList.Add(ADataPacket); //todo for RC thread: check if it's here
  except
    on EListError do
      exit;
  end;
  ADataPacket.AddRef;
  inc(FInBytes,ADataPacket.Data.Size);

  Finished:= false;
end;

function TNMMDataQueue.Get(AIndex: Integer): TNMMDataPacket;
var LList: TList;
begin
  LList:= FList.LockList;
  try
    if AIndex < LList.Count then
      result:= LList[AIndex]
    else
      result:= nil;
  finally
    FList.UnlockList;
  end;
end;

procedure TNMMDataQueue.Remove(AIndex: Integer);
var LDataPacket: TNMMDataPacket;
    LList: TList;
begin
  try
    inc(FOutBytes,Get(0).Data.Size);
  finally
    LList:= FList.LockList;
    try
      if AIndex < LList.Count then
      begin
        LDataPacket:= LList[AIndex];
        LList.Remove(LDataPacket);
        LDataPacket.Release;
        if LDataPacket.RefCount=0 then
           LDataPacket.Free;
      end;
    finally
      FList.UnlockList;
    end;
  end;
end;

procedure TNMMDataQueue.RemoveMarked;
var LList: TList;
    i: Integer;
    LDataPacket: TNMMDataPacket;
begin
  LList:= FList.LockList;
  try
    for i:=LList.Count-1 downto 0 do
    begin
      LDataPacket:= TNMMDataPacket(LList[i]);
      if LDataPacket.Marked then
         LList.Remove(LDataPacket);
    end;
  finally
    FList.UnlockList;
  end;
end;


{TNMMMultiReferedObject}
constructor TNMMMultiReferedObject.Create;
begin
  inherited;
  RefCount:= 0;
end;

procedure TNMMMultiReferedObject.AddRef;
begin
  Inc(RefCount);
end;

procedure TNMMMultiReferedObject.Release;
begin
  Dec(RefCount);
end;


{TNMMDataPacket}
constructor TNMMDataPacket.Create;
begin
  inherited;
  FData:= TMemoryStream.Create;
  FTime:= 0;
  Marked:= false; //todo: this line check with Tiles
end;

destructor TNMMDataPacket.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TNMMDataPacket.Clear;
begin
  FTime:= 0;
  SetId(0);
  if FData<>nil then
    FData.Size:= 0;
end;

procedure TNMMDataPacket.Update(AStream: TStream);
begin
  FData.Size:= 0;
  AStream.Seek(0,soFromBeginning);
  FData.CopyFrom(AStream,AStream.Size);
  FData.Seek(0,soFromBeginning);
  AStream.Seek(0,soFromBeginning);
  FTime:= Now;
end;

procedure TNMMDataPacket.SetPackedData(AStream: TStream);
var LGetID: Integer;
begin
  FData.Size:= 0;
  AStream.Seek(0,soFromBeginning);
  AStream.ReadBuffer(LGetID,SizeOf(LGetID));
  SetID(LGetID);
  AStream.ReadBuffer(FTime,SizeOf(FTime));
  FData.Seek(0,soFromBeginning);
  FData.CopyFrom(AStream,AStream.Size-AStream.Position);
  FData.Seek(0,soFromBeginning);
  AStream.Seek(0,soFromBeginning);
end;

procedure TNMMDataPacket.GetPackedData(AStream: TStream);
var LID: Integer;
begin
  AStream.Size:= 0;
  LID:= GetID;
  AStream.WriteBuffer(LID,SizeOf(LID));
  AStream.WriteBuffer(FTime,SizeOf(FTime));
  FData.Seek(0,soFromBeginning);
  AStream.CopyFrom(FData,FData.Size{-FData.Position});
  FData.Seek(0,soFromBeginning);
  AStream.Seek(0,soFromBeginning);
end;

{TNMMDataPacketInfo}
procedure TNMMDataPacketInfo.CopyFrom(ADataPacket: TNMMDataPacket);
begin
  Id:= ADataPacket.GetId;
  Time:= ADataPacket.Time;
end;

{ TNMMCountedDataPacket }
function TNMMCountedDataPacket.GetId: DWord;
begin
  result:= FID;
end;

procedure TNMMCountedDataPacket.SetID(AValue: DWord);
begin
  FID:= AValue;
end;


end.
