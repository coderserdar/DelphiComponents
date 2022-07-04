(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMTileComparer;

interface
uses Classes, NMMTiles, NMMCustomConnectionHandle, NMMCustomDataProcessors,
     NMMCommon, Windows, Graphics;


type
  TNMMTileComparer = class (TNMMCustomImageComparer)
  protected
    FTileManager: TNMMTileManager;
    FWorking: Boolean;
    function GetPacketCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    {for server side}
    procedure SetIniImage(AImage: TBitmap); override;
    function GetIniImageStream: TStream; override;
    procedure ProcessNewImage(ANewBitmap: TBitmap); override;
    procedure SetIniImageStream(AStream: TStream); override;
    procedure AddPacket(APacket: TStream); override;
    procedure Clear; override;
    function Empty: Boolean; override;
    procedure GetPacket(ADelta: TStream); override;
    {for client side}
    procedure GetIniImage(ABitmap: TBitmap); override;
    procedure AssembleNextImage(ANewBitmap: TBitmap); override;
    procedure SetConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
  end;


implementation
uses SysUtils;


constructor TNMMTileComparer.Create;
begin
  inherited;
  FTileManager:= TNMMTileManager.Create;
  FTileManager.OnPictureWndUpdate:= PictureWndUpdate;
  FWorking:= false;
end;

destructor TNMMTileComparer.Destroy;
begin
  FreeAndNil(FTileManager);
  inherited;
end;

procedure TNMMTileComparer.SetConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
  FTileManager.ConnectionHandle:= AConnectionHandle;
end;

function TNMMTileComparer.GetIniImageStream: TStream;
begin
  raise Exception.Create('Obsolete GetIniImageStream call');
end;

procedure TNMMTileComparer.GetIniImage(ABitmap: TBitmap);
begin
  AssembleNextImage(ABitmap);
end;

procedure TNMMTileComparer.SetIniImage(AImage: TBitmap);
begin
  FTileManager.Init(AImage.Width, AImage.Height, 75, 50);
  FTileManager.Input(AImage,False);
  FWorking:= true;
end;

procedure TNMMTileComparer.ProcessNewImage(ANewBitmap: TBitmap);
begin
  FTileManager.Input(ANewBitmap);
end;

procedure TNMMTileComparer.GetPacket(ADelta: TStream);
begin
  if FTileManager.Queue.Count > 0 then
  begin
    FTileManager.Queue.Get(0).GetPackedData(ADelta);
    FTileManager.Queue.Remove(0);
  end;
end;

function TNMMTileComparer.GetPacketCount: Integer;
begin
  result:= FTileManager.Queue.Count;
end;

function TNMMTileComparer.Empty: Boolean;
begin
  result:= not FWorking;
end;

procedure TNMMTileComparer.SetIniImageStream(AStream: TStream);
var LBitmap: TBitmap;
begin
  LBitmap:= TBitmap.Create;
  try
    LBitmap.LoadFromStream(AStream);
    SetIniImage(LBitmap);
    AStream.Seek(0,soFromBeginning);
  finally
    FreeAndNil(LBitmap);
  end;
end;

procedure TNMMTileComparer.AssembleNextImage(ANewBitmap: TBitmap);
begin
  FTileManager.MakeWholePicture(ANewBitmap);
end;

procedure TNMMTileComparer.AddPacket(APacket: TStream);
var ATile: TTile;
begin
  ATile:= TTile.Create;
  ATile.SetPackedData(APacket);
  FTileManager.Queue.Add(ATile);
end;

procedure TNMMTileComparer.Clear;
begin
  FTileManager.Clear;
  FWorking:= false;
end;

end.
