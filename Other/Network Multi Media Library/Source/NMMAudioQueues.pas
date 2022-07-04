(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioQueues;

interface

uses NMMAudioDataPacket, NMMCustomDataProcessors, Types;

type
  TNMMAudioQueue= class(TNMMDataQueue)
  protected
    FLastId: Integer;
    FPacketCount: Integer;
    FSizeErrorLogged: Boolean;
  public
    function PushRawData(APtr: Pointer; var ASize: DWORD; var AID: Integer; var ATime: TDateTime): Boolean;
    function PutRawData(APtr: Pointer; ASize: DWORD; AID: Integer; ATime: TDateTime): TNMMAudioDataPacketInfo;
    procedure Clear; override;
    procedure Add(ADataPacket: TNMMDataPacket); override;
    procedure Remove(AItem: Integer); override;
    constructor Create;
    property PacketCount: Integer read FPacketCount;
  end;

  TNMMAudioRecordQueue= class(TNMMAudioQueue)
  end;

implementation

uses Classes, NMMCommon, SysUtils;

{TNMMAudioQueue}
constructor TNMMAudioQueue.Create;
begin
  inherited;
  FPacketCount:= 0;
  FSizeErrorLogged:= false;
  FFinished:= false;
  FInBytes:= 0;
  FOutBytes:= 0;
end;

procedure TNMMAudioQueue.Clear;
begin
  inherited;
  FInBytes:= 0;
  FOutBytes:= 0;
  FLastId:= 0;
end;

procedure TNMMAudioQueue.Add(ADataPacket: TNMMDataPacket);
var LId: DWord;
begin
  LId:= ADataPacket.GetId;
  if (LId = 0) or (FLastId < LId) then
  begin
    inherited;
    FLastId:= LId;
  end;
end;

procedure TNMMAudioQueue.Remove(AItem: Integer);
begin
  inherited;
end;

function TNMMAudioQueue.PutRawData(APtr: Pointer; ASize: DWORD; AID: Integer; ATime: TDateTime): TNMMAudioDataPacketInfo;
var LDataPacket: TNMMAudioDataPacket;
begin
  LDataPacket:= TNMMAudioDataPacket.Create;
  LDataPacket.Data.WriteBuffer(APtr^,ASize);
  LDataPacket.Data.Seek(0,soFromBeginning);
  LDataPacket.SetID(FPacketCount);
  LDataPacket.Time:= ATime;
  result.Id:= AID;
  result.Time:= LDataPacket.Time;
  result.Size:= LDataPacket.Data.Size;
  Add(LDataPacket);
  inc(FPacketCount);
end;

function TNMMAudioQueue.PushRawData(APtr: Pointer; var ASize: DWORD; var AID: Integer; var ATime: TDateTime): Boolean;
var LDataPacket: TNMMDataPacket;
begin
//  FillChar(APtr,LSize-1,0);
  result:= false;
//  LSize:= 0;
  if Count>0 then
  begin
    LDataPacket:= Get(0);
    if LDataPacket=nil then
    begin
      Remove(0);
      exit;
    end;
    try
      try
        if ASize>LDataPacket.Data.Size then
           ASize:= LDataPacket.Data.Size;

        LDataPacket.Data.ReadBuffer(APtr^,ASize);
        ATime:= LDataPacket.Time;
        AID:= LDataPacket.GetId;
        result:= true;
      finally
        Remove(0);
      end;
    except
    end;
  end;
end;

end.
