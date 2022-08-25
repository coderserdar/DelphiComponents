{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Speeds up TApplication's message pump by removing mouse message
              filtering from TApplication.ProcessMessage in D2009 and later and
              by optionally removing checks for UNICODE windows in D2007 and
              later (this option is ON by default). Works with C++ Builder as
              well. Add this unit to the .dpr file's uses clause or to the
              project.
Creation:     Sep 02, 2009
Version:      1.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by Arno Garrels <arno.garrels@gmx.de>
              Portions created by Andreas Hausladen are Copyright (C)
              Andreas Hausladen <andreas.hausladen@gmx.de>
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FastAppProcessMessagePatch;
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

interface

implementation

{$IFDEF CONDITIONALEXPRESSIONS}
{$IFDEF WIN32}
{$IF CompilerVersion >= 18.50} // 2007 and later

{ Uncomment next line to include checks for UNICODE windows (VCL default) }
{.$DEFINE APP_CHECK_UNICODE_HWND}

uses Windows, Messages, SysUtils, Forms;

type
  TJump = packed record
    Jump    : Byte;
    Offset  : Integer;
  end;
  PJump = ^TJump;

  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;
    Addr: Pointer;
    JMP: TJump;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word; //$FF25(Jmp, FF /4)
    Addr: PPointer;
  end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ function GetActualAddr() published by Andreas Hausladen source:           }
{ http://www.delphigroups.info/2/8/1026271.html                             }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetActualAddr(Proc: Pointer): Pointer;

  function IsWin9xDebugThunk(AAddr: Pointer): Boolean;
  begin
    Result := (AAddr <> nil) and
              (PWin9xDebugThunk(AAddr).PUSH = $68) and
              (PWin9xDebugThunk(AAddr).JMP.Jump = $E9);
  end;

begin
  if Proc <> nil then
  begin
    if (Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Patch(Src, Dest: Pointer; var Backup: TJump);
var
  OldProtect : Cardinal;
  Code       : PJump;
begin
  Assert(Src <> nil);
  if VirtualProtect(Src, SizeOf(TJump), PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    Code := PJump(Src);
    Backup := Code^;
    Code^.Jump := $E9;
    Code^.Offset := PAnsiChar(Dest) - PAnsiChar(Src) - SizeOf(TJump);
    FlushInstructionCache(GetCurrentProcess, Src, SizeOf(TJump));
    VirtualProtect(Src, SizeOf(TJump), OldProtect, OldProtect);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnPatch(Src: Pointer; var Backup: TJump);
var
  OldProtect : Cardinal;
  Code       : PJump;
begin
  if (Backup.Jump <> 0) and (Src <> nil) then
  begin
    Assert(Src <> nil);
    if VirtualProtect(Src, SizeOf(TJump), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Code := PJump(Src);
      Code^ := Backup;
      Backup.Jump := 0;
      FlushInstructionCache(GetCurrentProcess, Src, SizeOf(TJump));
      VirtualProtect(Src, SizeOf(TJump), OldProtect, OldProtect);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
type
  TAppClassHelper = class helper for TApplication
     procedure SetTerminate;
  end;

procedure TAppClassHelper.SetTerminate;
begin
  Self.FTerminate := TRUE; // Access to private fields only in D2009 and later
end;
*)

var
  PTerminateOffs : PByte;

type
  THackApplication = class(TApplication)
  private
    function FastProcessMessage(var Msg: TMsg): Boolean;
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THackApplication.FastProcessMessage(var Msg: TMsg): Boolean;
var
{$IFDEF APP_CHECK_UNICODE_HWND}
  Unicode   : Boolean;
{$ENDIF}
  Handled   : Boolean;
  MsgExists : Boolean;
begin
  Result := False;
{$IFDEF APP_CHECK_UNICODE_HWND}
  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
  begin
    Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
    if Unicode then
      MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
    else
      MsgExists := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);
{$ELSE}
    MsgExists := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
{$ENDIF}
    if not MsgExists then
      Exit;
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      Handled := False;
      if Assigned(OnMessage) then
          OnMessage(Msg, Handled);
      if not IsPreProcessMessage(Msg) and not
        IsHintMsg(Msg) and not Handled and not
        IsMDIMsg(Msg) and not
        IsKeyMsg(Msg) and not
        IsDlgMsg(Msg) then
      begin
        TranslateMessage(Msg);
      {$IFDEF APP_CHECK_UNICODE_HWND}
        if Unicode then
          DispatchMessageW(Msg)
        else
          DispatchMessageA(Msg);
      {$ELSE}
        DispatchMessage(Msg);
      {$ENDIF}
      end;
    end
    else
      PByte(Integer(Self) + PTerminateOffs^)^ := $01; //FTerminate := TRUE
      //SetTerminate;
{$IFDEF APP_CHECK_UNICODE_HWND}
  end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
Forms.pas.9681: begin
004694C0 53               push ebx
004694C1 83C4E4           add esp,-$1c
004694C4 8BD8             mov ebx,eax
Forms.pas.9682: while ProcessMessage(Msg) do {loop};
004694C6 8BD4             mov edx,esp
004694C8 8BC3             mov eax,ebx
004694CA E8B1FEFFFF       call TApplication.ProcessMessage
004694CF 84C0             test al,al
004694D1 75F3             jnz $004694c6
Forms.pas.9683: end;
004694D3 83C41C           add esp,$1c
004694D6 5B               pop ebx
004694D7 C3               ret
*)

function GetProcAddrProcessMessage: Pointer;
var
  P : PByteArray;
begin
  P := GetActualAddr(@TApplication.ProcessMessages);
  if (P[0] = $53) and (P[1] = $83) and (P[2] = $C4) and (P[3] = $E4) and
     (P[4] = $8B) and (P[5] = $D8) and (P[6] = $8B) and (P[7] = $D4) and
     (P[8] = $8B) and (P[9] = $C3) and (P[10] = $E8) and (P[15] = $84) and
     (P[16] = $C0) then
    Result := Pointer(Integer(@P[15]) + PInteger(@P[11])^)
  else
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
Forms.pas.9754: begin
004A9808 53               push ebx
004A9809 56               push esi
004A980A 57               push edi
004A980B 55               push ebp
004A980C 51               push ecx
004A980D 8BF2             mov esi,edx
004A980F 8BF8             mov edi,eax
Forms.pas.9755: Result := False;
004A9811 C6042400         mov byte ptr [esp],$00
Forms.pas.9757: MsgExists := PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE);
004A9815 6A01             push $01
004A9817 680E020000       push $0000020e
004A981C 6800020000       push $00000200
004A9821 6A00             push $00
004A9823 56               push esi
004A9824 E89B22F6FF       call PeekMessage
004A9829 83F801           cmp eax,$01
004A982C 1BDB             sbb ebx,ebx
004A982E 43               inc ebx
Forms.pas.9759: if MsgExists or PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
004A982F 84DB             test bl,bl
004A9831 7516             jnz $004a9849
004A9833 6A00             push $00
004A9835 6A00             push $00
004A9837 6A00             push $00
004A9839 6A00             push $00
004A983B 56               push esi
004A983C E88322F6FF       call PeekMessage
004A9841 85C0             test eax,eax
004A9843 0F84F2000000     jz $004a993b
Forms.pas.9761: Unicode := (Msg.hwnd = 0) or IsWindowUnicode(Msg.hwnd);
004A9849 8B2E             mov ebp,[esi]
004A984B 85ED             test ebp,ebp
004A984D 740E             jz $004a985d
004A984F 55               push ebp
004A9850 E8F721F6FF       call IsWindowUnicode
004A9855 85C0             test eax,eax
004A9857 7504             jnz $004a985d
004A9859 33C0             xor eax,eax
004A985B EB02             jmp $004a985f
004A985D B001             mov al,$01
004A985F 88442402         mov [esp+$02],al
Forms.pas.9763: if not MsgExists then
004A9863 84DB             test bl,bl
004A9865 7531             jnz $004a9898
Forms.pas.9765: if Unicode then
004A9867 807C240200       cmp byte ptr [esp+$02],$00
004A986C 7416             jz $004a9884
Forms.pas.9766: MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
004A986E 6A01             push $01
004A9870 6A00             push $00
004A9872 6A00             push $00
004A9874 6A00             push $00
004A9876 56               push esi
004A9877 E85822F6FF       call PeekMessageW
004A987C 83F801           cmp eax,$01
004A987F 1BDB             sbb ebx,ebx
004A9881 43               inc ebx
004A9882 EB14             jmp $004a9898
Forms.pas.9768: MsgExists := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);
004A9884 6A01             push $01
004A9886 6A00             push $00
004A9888 6A00             push $00
004A988A 6A00             push $00
004A988C 56               push esi
004A988D E83A22F6FF       call PeekMessageA
004A9892 83F801           cmp eax,$01
004A9895 1BDB             sbb ebx,ebx
004A9897 43               inc ebx
Forms.pas.9771: if MsgExists then
004A9898 84DB             test bl,bl
004A989A 0F849B000000     jz $004a993b
Forms.pas.9773: Result := True;
004A98A0 C6042401         mov byte ptr [esp],$01
Forms.pas.9774: if Msg.Message <> WM_QUIT then
004A98A4 837E0412         cmp dword ptr [esi+$04],$12
004A98A8 0F8486000000     jz $004a9934
Forms.pas.9776: Handled := False;
004A98AE C644240100       mov byte ptr [esp+$01],$00
Forms.pas.9777: if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
004A98B3 6683BF0A01000000 cmp word ptr [edi+$0000010a],$00
004A98BB 7412             jz $004a98cf
004A98BD 8D4C2401         lea ecx,[esp+$01]
004A98C1 8BD6             mov edx,esi
004A98C3 8B870C010000     mov eax,[edi+$0000010c]
004A98C9 FF9708010000     call dword ptr [edi+$00000108]
Forms.pas.9778: if not IsPreProcessMessage(Msg) and not IsHintMsg(Msg) and
004A98CF 8BD6             mov edx,esi
004A98D1 8BC7             mov eax,edi
004A98D3 E8D41D0000       call TApplication.IsPreProcessMessage
004A98D8 84C0             test al,al
004A98DA 755F             jnz $004a993b
004A98DC 8BD6             mov edx,esi
004A98DE 8BC7             mov eax,edi
004A98E0 E8CFFDFFFF       call TApplication.IsHintMsg
004A98E5 84C0             test al,al
004A98E7 7552             jnz $004a993b
004A98E9 807C240100       cmp byte ptr [esp+$01],$00
004A98EE 754B             jnz $004a993b
004A98F0 8BD6             mov edx,esi
004A98F2 8BC7             mov eax,edi
004A98F4 E873FCFFFF       call TApplication.IsMDIMsg
004A98F9 84C0             test al,al
004A98FB 753E             jnz $004a993b
004A98FD 8BD6             mov edx,esi
004A98FF 8BC7             mov eax,edi
004A9901 E8B6FCFFFF       call TApplication.IsKeyMsg
004A9906 84C0             test al,al
004A9908 7531             jnz $004a993b
004A990A 8BD6             mov edx,esi
004A990C 8BC7             mov eax,edi
004A990E E811FCFFFF       call TApplication.IsDlgMsg
004A9913 84C0             test al,al
004A9915 7524             jnz $004a993b
Forms.pas.9782: TranslateMessage(Msg);
004A9917 56               push esi
004A9918 E83B23F6FF       call TranslateMessage
Forms.pas.9783: if Unicode then
004A991D 807C240200       cmp byte ptr [esp+$02],$00
004A9922 7408             jz $004a992c
Forms.pas.9784: DispatchMessageW(Msg)
004A9924 56               push esi
004A9925 E8821EF6FF       call DispatchMessageW
004A992A EB0F             jmp $004a993b
Forms.pas.9786: DispatchMessageA(Msg);
004A992C 56               push esi
004A992D E8721EF6FF       call DispatchMessageA
004A9932 EB07             jmp $004a993b
Forms.pas.9795: FTerminate := True;
004A9934 C687A400000001   mov byte ptr [edi+$000000a4],$01
Forms.pas.9799: end;
004A993B 0FB60424         movzx eax,[esp]
004A993F 5A               pop edx
004A9940 5D               pop ebp
004A9941 5F               pop edi
004A9942 5E               pop esi
004A9943 5B               pop ebx
004A9944 C3               ret
*)

function GetFTerminateOffs(StartAddr: Pointer) : PByte;
var
  P   : PByteArray;
  Len : Integer;
begin
  Assert(StartAddr <> nil);
  P := StartAddr;
  Inc(PByte(P), 250);
  for Len := 1 to 60 do
  begin
    if (P[0] = $C6) and (P[3] = $00) and (P[4] = $00) and (P[5] = $00) and
       (P[6] = $01) then
    begin  //004A9934 C687A400000001   mov byte ptr [edi+$000000a4],$01
      Result := @P[2];
      Exit;
    end;
    Inc(PByte(P));
  end;
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
  ProcessMessageBak: TJump;
  ProcAddrProcessMessage: Pointer;

procedure Init;
begin
  ProcAddrProcessMessage := GetProcAddrProcessMessage;
  PTerminateOffs := GetFTerminateOffs(ProcAddrProcessMessage);
  if PTerminateOffs <> nil then
    Patch(ProcAddrProcessMessage, @THackApplication.FastProcessMessage,
          ProcessMessageBak);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
{$IF CompilerVersion = 18.50}
  {$IFNDEF APP_CHECK_UNICODE_HWND}
    Init;
  {$ENDIF}
{$ELSEIF True}
    Init;
{$IFEND}

finalization
{$IF CompilerVersion = 18.50}
  {$IFNDEF APP_CHECK_UNICODE_HWND}
    UnPatch(ProcAddrProcessMessage, ProcessMessageBak);
  {$ENDIF}
{$ELSEIF True}
    UnPatch(ProcAddrProcessMessage, ProcessMessageBak);
{$IFEND}

{$IFEND}
{$ENDIF}
{$ENDIF}
end.
