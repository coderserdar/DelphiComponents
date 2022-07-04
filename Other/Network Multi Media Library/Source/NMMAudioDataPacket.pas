(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioDataPacket;

interface

uses NMMCustomDataProcessors;

type                      
 TNMMAudioDataPacket = class(TNMMCountedDataPacket)
 end;

 TNMMAudioDataPacketInfo = record
   Id: Integer;
   Time: TDateTime;
   Size: Longint;
 end;

implementation

end.
