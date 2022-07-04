(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMP2PAudioConnectionHandle;

interface

uses NMMACMThread, NMMAudioQueues, NMMConnectionHandle;

type
  TNMMP2PAudioConnectionHandle = class(TNMMConnectionHandle)
  protected
    FInpEncQueue: TNMMAudioQueue;
    FInpPCMQueue: TNMMAudioQueue;
    FOutPCMQueue: TNMMAudioQueue;
    FOutEncQueue: TNMMAudioQueue;
  public
    property FInpEncQueue: TNMMAudioQueue read FInpEncQueue write FInpEncQueue;
    property FInpPCMQueue: TNMMAudioQueue read FInpPCMQueue write FInpPCMQueue;
    property FOutPCMQueue: TNMMAudioQueue read FOutPCMQueue write FOutPCMQueue;
    property FOutEncQueue: TNMMAudioQueue read FOutEncQueue write FOutEncQueue;
  end;

implementation


end.
