(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMServerGlobals;

interface

uses IdThreadSafe;

function GetNewSessionName: String;
function GetCurrentSessionName: String;

var
  GcgtInstances: TIdThreadSafeInteger;   // counter for TNMMConnectionGroupThreads
  GchwstInstances: TIdThreadSafeInteger; // counter for TNMMConnectionHandleWriteThreads
  GchInstances: TIdThreadSafeInteger;    // counter for TNMMConnectionHandles

implementation


uses SysUtils, Windows;


Var
  LastSessionValue: Integer;
  SessionNameSection: TRTLCriticalSection;

ThreadVar
  DBISamSessionName: Array [0..255] of Char;


function GetNewSessionName: String;
begin
 EnterCriticalSection(SessionNameSection);
 try
   LastSessionValue:=LastSessionValue+1;
   Result:= 'AccountSession'+IntToStr(LastSessionValue);
   StrPCopy(DBISamSessionName,Result);
 finally
   LeaveCriticalSection(SessionNameSection);
 end;
end;

function GetCurrentSessionName: String;
begin
 if StrLen(DBISamSessionName) > 0 then
   result:= StrPas(DBISamSessionName)
 else
   result:= '';
end;


initialization
  GchwstInstances:= TIdThreadSafeInteger.Create;
  GchwstInstances.Value:= 0;
  GcgtInstances:= TIdThreadSafeInteger.Create;
  GcgtInstances.Value:= 0;
  GchInstances:= TIdThreadSafeInteger.Create;
  GchInstances.Value:= 0;
  LastSessionValue:=0;
  InitializeCriticalSection(SessionNameSection);
finalization
  FreeAndNil(GchwstInstances);
  FreeAndNil(GcgtInstances);
  DeleteCriticalSection(SessionNameSection);
end.
