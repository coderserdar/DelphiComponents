(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMTiles;

interface
uses Windows, Classes, NMMCustomConnectionHandle, NMMCustomDataProcessors, Graphics,
     NMMCommon;

type
  TTile = class(TNMMDataPacket)
  protected
    FCol, FRow, FX, FY, FHeight, FWidth: Integer;
  public
    constructor Create; overload;
    constructor Create(ACol, ARow, AX, AY: Integer); overload;
    destructor Destroy; override;
    function GetId: DWord; override;
    procedure SetID(AValue: DWord); override;
    procedure GetPackedData(AStream: TStream); override;
    procedure SetPackedData(AStream: TStream); override;

    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Col: Integer read FCol write FCol;
    property Row: Integer read FRow write FRow;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
  end;


  TNMMTileManager = class(TCustomNMMPacketManager)
  protected
    FCols, FRows, FImageWidth, FImageHeight, FTileWidth, FTileHeight,
    FLastTileWidth, FLastTileHeight: Integer;
    FTiles: array of array of TTile;
    FConnectionHandle: TNMMCustomConnectionHandle;
    FOnPictureWndUpdate: TPictureWndUpdateEvent;
    function CreateQueue: TNMMDataQueue; override;
    procedure PictureWndUpdate(AR: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(AImageWidth, AImageHeight, ATileWidth, ATileHeight: Integer);
    procedure Clear; override;
    function GetTile(ACol,ARow: Integer): TTile;
    procedure Input(AImage: TBitmap; AUpdate: Boolean = true);
    procedure MakeWholePicture(AImage: TBitmap);

    property Cols: Integer read FCols;
    property Rows: Integer read FRows;
    property ConnectionHandle: TNMMCustomConnectionHandle read FConnectionHandle write FConnectionHandle;

    property OnPictureWndUpdate: TPictureWndUpdateEvent
             read FOnPictureWndUpdate write FOnPictureWndUpdate;
  end;


implementation
uses SysUtils, NMMBmp2Jpg, NMMRCQueue,
     SyncObjs;

var
  GNMMBmp2Jpg: TNMMBmp2Jpg;
  GNMMBmp2JpgCS: TCriticalSection;

{----------------------------------------------}
{                    TTile                     }
{----------------------------------------------}
constructor TTile.Create;
begin
  inherited;
end;

constructor TTile.Create(ACol, ARow, AX, AY: Integer);
begin
  Create;
  FCol:= ACol; FRow:= ARow; FX:= AX; FY:= AY;
end;

destructor TTile.Destroy;
begin
  inherited;
end;

function TTile.GetId: DWord;
begin
  result:= FCol*1000 + FRow;
end;

procedure TTile.SetID(AValue: DWord);
begin
  FCol:= AValue div 1000;
  FRow:= AValue mod 1000;
end;

procedure TTile.SetPackedData(AStream: TStream);
var LGetID: Integer;
    LStream: TMemoryStream;
begin
  LStream:= TMemoryStream.Create;
  try
    GNMMBmp2JpgCS.Enter;
    try
      FData.Size:= 0;
      AStream.Seek(0,soFromBeginning);
      AStream.ReadBuffer(LGetID,SizeOf(LGetID));
      SetID(LGetID);
      AStream.ReadBuffer(FTime,SizeOf(FTime));

      if GNMMBmp2Jpg<>nil then
      begin
        LStream.CopyFrom(AStream,AStream.Size-AStream.Position);
        FData.Seek(0,soFromBeginning);
        LStream.Seek(0,soFromBeginning);
        GNMMBmp2Jpg.Jpg2Bmp(LStream,FData);
      end
      else
        FData.CopyFrom(AStream,AStream.Size-AStream.Position);//rem

      FData.Seek(0,soFromBeginning);
    finally
      GNMMBmp2JpgCS.Leave;
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TTile.GetPackedData(AStream: TStream);
var LID: Integer;
    LStream: TMemoryStream;
begin
  LStream:= TMemoryStream.Create;
  try
    GNMMBmp2JpgCS.Enter;
    try
      AStream.Size:= 0;
      LID:= GetID;
      AStream.WriteBuffer(LID,SizeOf(LID));
      AStream.WriteBuffer(FTime,SizeOf(FTime));
      FData.Seek(0,soFromBeginning);

      if GNMMBmp2Jpg<>nil then
        GNMMBmp2Jpg.Bmp2Jpg(FData,LStream)
      else
        LStream.CopyFrom(FData,FData.Size);

      LStream.Seek(0,soFromBeginning);
      AStream.CopyFrom(LStream,LStream.Size);
      FData.Seek(0,soFromBeginning);
      AStream.Seek(0,soFromBeginning);
    finally
      GNMMBmp2JpgCS.Leave;
    end;
  finally
    FreeAndNil(LStream);
  end;
end;


{----------------------------------------------}
{                 TNMMTileManager              }
{----------------------------------------------}
constructor TNMMTileManager.Create;
begin
  inherited Create;
end;

function TNMMTileManager.CreateQueue: TNMMDataQueue;
begin
  result:= TNMMRCQueue.Create;
end;

procedure TNMMTileManager.Init(AImageWidth, AImageHeight, ATileWidth,
                               ATileHeight: Integer);
var i,j: Integer;
begin
  Clear;

  FImageWidth:= AImageWidth;
  FImageHeight:= AImageHeight;
  FTileWidth:= ATileWidth;
  FTileHeight:= ATileHeight;

  FCols:= FImageWidth div FTileWidth;
  FLastTileWidth:= FImageWidth mod FTileWidth;
  if FLastTileWidth>0 then inc(FCols);

  FRows:= FImageHeight div FTileHeight;
  FLastTileHeight:= FImageHeight mod FTileHeight;
  if FLastTileHeight>0 then inc(FRows);

  SetLength(FTiles,FCols,FRows);
  for i:=0 to FCols-1 do
    for j:=0 to FRows-1 do
    begin
      FTiles[i,j]:= TTile.Create(i,j,i*FTileWidth,j*FTileHeight);
      FTiles[i,j].AddRef;
      if (i=FCols-1) and (FLastTileWidth>0) then
        FTiles[i,j].Width:= FLastTileWidth
      else
        FTiles[i,j].Width:= FTileWidth;

      if (j=FRows-1) and (FLastTileHeight>0) then
        FTiles[i,j].Height:= FLastTileHeight
      else
        FTiles[i,j].Height:= FTileHeight;
    end;
end;

procedure TNMMTileManager.Clear;
var i,j: Integer;
begin
  inherited;

  for i:=0 to FCols-1 do
    for j:=0 to FRows-1 do
    begin
      Assert(FTiles[i,j].RefCount<=1,'Unresolved links to data packet with ID='+IntToStr(FTiles[i,j].GetId));
      FTiles[i,j].Release;
      if FTiles[i,j].RefCount<=1 then
        FTiles[i,j].Free;
    end;
  SetLength(FTiles,0,0);
  FCols:= 0; FRows:= 0; 
end;

destructor TNMMTileManager.Destroy;
begin
  Clear;
  inherited;
end;

function TNMMTileManager.GetTile(ACol,ARow: Integer): TTile;
begin
  result:= nil;
  if (ACol < FCols) and (ARow < FRows) then
  begin
    result:= FTiles[ACol,ARow];
  end;
end;

var FBuf1: Array [0..MaxWord] of byte;
var FBuf2: Array [0..MaxWord] of byte;

function EqualStreams(AStream1, AStream2: TStream): Boolean;
var i, LSize: Integer;
begin
  result:= true;
  AStream1.Seek(0,soFromBeginning);
  AStream2.Seek(0,soFromBeginning);

  if AStream1.Size <> AStream2.Size then
  begin
    result:= false; exit;
  end;

  LSize:= AStream1.Size;
  FillChar(FBuf1,LSize,0);
  FillChar(FBuf2,LSize,0);
  try
    AStream1.ReadBuffer(FBuf1,LSize);
    AStream2.ReadBuffer(FBuf2,LSize);
    for i:=0 to LSize-1 do
    begin
      if FBuf1[i] <> FBuf2[i] then
      begin
        result:= false;
        break;
      end;
    end;
  finally
    AStream1.Seek(0,soFromBeginning);
    AStream2.Seek(0,soFromBeginning);
  end;
end;

const
  MaxPixelCount = 32768;
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

procedure TNMMTileManager.Input(AImage: TBitmap; AUpdate: Boolean);
var
  x, y, LRow, LCol:  Integer;
  LStream, LPacket: TMemoryStream;
  LBitMap: TBitMap;
  P, P1: PRGBArray;
  LTile: TTile;
begin
  LBitMap:= TBitMap.Create;
  LStream:= TMemoryStream.Create;
  LPacket:= TMemoryStream.Create;
  try
    AImage.PixelFormat:= pf24bit;
    LBitMap.PixelFormat:= AImage.PixelFormat;

    for LCol:= 0 to FCols-1 do
      for LRow:= 0 to FRows-1 do
      begin
        LTile:= FTiles[LCol,LRow];
        LBitMap.Width:= LTile.Width;
        LBitMap.Height:= LTile.Height;
        for y:=0 to LTile.Height-1 do
        begin
          P:= AImage.ScanLine[y+LTile.Y];
          P1:= LBitMap.ScanLine[y];
          for x:= 0 to (LBitMap.Width-1) do
          begin
            P1[x] := P[x+LTile.x];
          end;
        end;
        LBitMap.SaveToStream(LStream);
        if not AUpdate or not EqualStreams(LStream,LTile.Data) then
        begin
          LTile.Update(LStream);
          if FConnectionHandle<>nil then
          begin
             LTile.GetPackedData(LPacket);
             if FConnectionHandle<>nil then
               FConnectionHandle.SendData(LPacket);
          end;
        end;
      end;
  finally
    FreeAndNil(LBitMap);
    FreeAndNil(LStream);
    FreeAndNil(LPacket);
  end;
end;

procedure TNMMTileManager.MakeWholePicture(AImage: TBitmap);
var LTile: TTile;
    LBitMap: TBitMap;
    P, P1: PRGBArray;
    x, y: Integer;
    LR: TRect;
begin
  while FQueue.Count>0 do
  begin
    LTile:= TTile(FQueue.Get(0));
    LTile.X:= LTile.Col * FTileWidth;
    LTile.Y:= LTile.Row * FTileHeight;
    if LTile.Data<>nil then
    begin
      LBitmap:= TBitmap.Create;
      try
        LBitmap.PixelFormat:= pf24bit;
        if AImage.PixelFormat<>pf24bit then
          AImage.PixelFormat:= pf24bit;
        if AImage.Width<>FImageWidth then
          AImage.Width:= FImageWidth;
        if AImage.Height<>FImageHeight then
          AImage.Height:= FImageHeight;
        if LTile.Data.Size > 0 then
        begin
          LTile.Data.Seek(0,soFromBeginning);
          LBitmap.LoadFromStream(LTile.Data);
          LTile.Data.Seek(0,soFromBeginning);
          for y:=0 to LBitmap.Height-1 do
          begin
            P:= AImage.ScanLine[y+LTile.Y];
            P1:= LBitmap.ScanLine[y];
            for x:= 0 to (LBitMap.Width-1) do
            begin
              P[x+LTile.x]:= P1[x];
            end;
          end;
          LR:= Rect(LTile.x,LTile.y,LTile.x+LBitmap.Width,LTile.y+LBitmap.Height);
          try
            PictureWndUpdate(LR);
          finally
          end;
        end;
      finally
        FreeAndNil(LBitmap);
      end;
    end;

    FQueue.Remove(0);
  end;
end;

procedure TNMMTileManager.PictureWndUpdate(AR: TRect);
begin
  if Assigned(FOnPictureWndUpdate) then
     FOnPictureWndUpdate(Self,AR);
end;

initialization
  GNMMBmp2Jpg:= TNMMBmp2Jpg.Create;
  GNMMBmp2JpgCS:= TCriticalSection.Create;
finalization
  FreeAndNil(GNMMBmp2Jpg);
  FreeAndNil(GNMMBmp2JpgCS);
end.
