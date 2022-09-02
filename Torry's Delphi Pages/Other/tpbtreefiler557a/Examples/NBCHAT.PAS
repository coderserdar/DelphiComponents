{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$B-,S-,R-,V-,X+}
program NBChat;
uses
  {$IFDEF Windows}
  {$IFDEF Ver80}
  SysUtils,
  {$ENDIF}
  WinProcs,
  WinCrt,
  DPMI,
  {$ENDIF}
  {$IFDEF DPMI}
  WinApi,
  Crt,
  DPMI,
  {$ENDIF}
  {$IFDEF MsDOS}
  Crt,
  {$ENDIF}
  NetBios;
type
  PString = ^string;
const
  Pending : Boolean = False;
  Msg : String = '';
  NameAdded : Boolean = False;
  NameExisted : Boolean = False;
  Exiting : Boolean = False;
var
  SendNBName : NBNameStr;
  RecNBName  : NBNameStr;
  NBNameNo : Byte;
  Ret : Byte;
  LSN : Byte;
  SaveExitProc : Pointer;

  RecStr, SendStr : PString;
  RecNCB, SendNCB : PNCB;
  RecPost, SendPost : PPostHandler;

  function HexB(B : byte) : string;
    const
      HexChar : array [0..$F] of char = '0123456789ABCDEF';
    var
      S : string[2];
    begin
      S[0] := #2;
      S[1] := HexChar[(B and $F0) shr 4];
      S[2] := HexChar[(B and $0F)];
      HexB := S;
    end;

  function Pad(S : String; Num : Byte) : String;
  var
    Len : Byte;
  begin
    Len := Length(S);
    if Len < Num then
      FillChar(S[Succ(Len)], Num-Len, ' ');
    S[0] := Chr(Num);
    Pad := S;
  end;


  procedure PostRoutine(LastError : Byte; N : PNCB); Far;
  begin
    if Exiting then Exit;
    Pending := True;
    if LastError = 0 then
      Msg := RecStr^
    else
      Msg := 'NetBIOS error = ' + HexB(LastError);
    NetBiosReceiveDGNoWait(N, RecPost, NBNameNo, SizeOf(String), RecStr);
  end;

  procedure GetNames;
  begin
    Write('Enter name for this station: ');
    ReadLn(SendNBName);
    if Length(SendNBName) = 0 then
      Halt;
    SendNBName := Pad(SendNBName, SizeOf(SendNBName) - 1);
    Write('Enter name for partner: ');
    ReadLn(RecNBName);
    if Length(RecNBName) = 0 then
      Halt;
    RecNBName := Pad(RecNBName, SizeOf(RecNBName) - 1);
  end;

  procedure ShowIncoming;
  begin
    WriteLn('<incoming>', Msg);
    Pending := False;
  end;

  procedure SendOutgoing;
  begin
    Write('Enter outgoing message: ');
    ReadLn(SendStr^);
    if Length(SendStr^) <> 0 then
      NetBiosSendDGNoWait(SendNCB, SendPost, NBNameNo, RecNBName, SizeOf(String), SendStr);
  end;

  procedure MessageLoop;
  var
    C : Char;
  begin
    WriteLn('Press space bar to enter message, ESC to quit');
    NetBiosReceiveDGNoWait(RecNCB, RecPost, NBNameNo, SizeOf(String), RecStr);
    repeat
      while not KeyPressed do begin
        if Pending then
          ShowIncoming;
      end;
      C := ReadKey;
      if C <> ^[ then
        SendOutgoing;
    until C = ^[;
  end;

  procedure AllocateMemory;
  begin
    RecStr := PString(NetBiosAllocPacket(sizeof(string)));
    if (RecStr = nil) then
    begin
      WriteLn('Unable to obtain memory for received messages');
      Halt;
    end;
    SendStr := PString(NetBiosAllocPacket(sizeof(string)));
    if (SendStr = nil) then
    begin
      WriteLn('Unable to obtain memory for send messages');
      Halt;
    end;
    RecNCB := NetBiosAllocNCB;
    if (RecNCB = nil) then
    begin
      WriteLn('Unable to allocate NCBs');
      Halt;
    end;
    SendNCB := NetBiosAllocNCB;
    if (SendNCB = nil) then
    begin
      WriteLn('Unable to allocate NCBs');
      Halt;
    end;
    SendPost := nil;
    RecPost := NetBiosAllocPost(PostRoutine);
    if (RecPost = nil) then
    begin
      WriteLn('Unable to allocate a post routine handler');
      Exit;
    end;
  end;

  procedure NBExitProc; Far;
  var
    Ret : Byte;
    C   : char;
  begin
    ExitProc := SaveExitProc;

    Exiting := True;
    if (RecNCB <> nil) then begin
      Ret := NetBiosCancelRequest(RecNCB);
      if (RecPost <> nil) then
        NetBiosFreePost(RecPost);
      NetBiosFreeNCB(RecNCB);
    end;
    if (SendNCB <> nil) then begin
      while (SendNCB^.CmdComplete = $FF) do
        {nothing but wait - cannot cancel a send datagram};
      if (SendPost <> nil) then
        NetBiosFreePost(SendPost);
      NetBiosFreeNCB(SendNCB);
    end;
    if NameAdded then
      Ret := NetBiosDeleteName(SendNBName);
    if (RecStr <> nil) then
      NetBiosFreePacket(RecStr);
    if (SendStr <> nil) then
      NetBiosFreePacket(SendStr);
  end;

procedure ClearSendRecVariables;
  begin
    SendStr := nil;
    SendNCB := nil;
    SendPost:= nil;
    RecStr := nil;
    RecNCB := nil;
    RecPost:= nil;
  end;

begin
  if not NetBiosInstalled then begin
    WriteLn('NetBIOS not installed');
    Halt;
  end;
  ClearSendRecVariables;
  GetNames;
  SaveExitProc := ExitProc;
  ExitProc := @NBExitProc;
  Ret := NetBiosAddName(SendNBName, NBNameNo);
  if Ret = NBESuccess then
    NameAdded := true
  else if Ret = NBEDuplicateName then
    NameExisted := true
  else begin
    WriteLn('Error adding NetBIOS name');
    Halt;
  end;
  AllocateMemory;
  MessageLoop;
end.
