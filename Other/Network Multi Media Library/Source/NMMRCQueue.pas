(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMRCQueue;

interface

uses
  Classes,
  NMMCustomDataProcessors;

type
  TNMMRCQueue=class(TNMMDataQueue)
     procedure Add(ADataPacket: TNMMDataPacket); override;
  end;

implementation
uses Types;


procedure TNMMRCQueue.Add(ADataPacket: TNMMDataPacket);
var LList: TList;
    i: Integer;
    LId: DWord;
    LRemDataPacket: TNMMDataPacket;
begin
  LList:= FList.LockList;
  LId:= ADataPacket.GetId;
  try
    for i:=0 to LList.Count-1 do
    begin
      if TNMMDataPacket(LList[i]).GetId = LId then
      begin
        LRemDataPacket:= LList[i];
        LList.Remove(LRemDataPacket);
        LRemDataPacket.Release;
        if LRemDataPacket.RefCount=0 then
           LRemDataPacket.Free;
        break;
      end;
    end;
  finally
    FList.UnlockList;
  end;
  inherited;
end;

end.
