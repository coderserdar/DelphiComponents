(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioRegister;

interface

uses Classes,
  NMMVoiceBroadcastServer, NMMVoiceInClient, NMMP2PVoiceServer, NMMVoiceClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NMM', [TNMMVoiceBroadcastServer]);
  RegisterComponents('NMM', [TNMMVoiceInClient]);
  RegisterComponents('NMM', [TNMMP2PVoiceServer]);
  RegisterComponents('NMM', [TNMMVoiceClient]);
end;

end.
