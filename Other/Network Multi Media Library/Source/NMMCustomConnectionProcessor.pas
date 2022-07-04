(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomConnectionProcessor;

interface

uses
 Windows, Classes, NMMConnectionHandles,
 NMMCustomConnectionHandle, NMMCustomDataProcessors, DateUtils,
 IdThreadSafe, NMMStatistics, SyncObjs, SysUtils;


type
 TConnectionProcessorState = set of (cpsStarting,cpsStopped,cpsTerminating,
                                   cpsTerminated,cpsCalculations);


type
 TNMMCustomConnectionProcessor = class(TThread)
 private
 protected
   FPeriod: Int64;
   FState: TConnectionProcessorState;
   FStatistics: TNMMStatistics;
   FCS: TCriticalSection;
   procedure SetName;
   procedure Execute; override;
   procedure DoWork; virtual; abstract;
   function StopWork: Boolean; 
   procedure CheckTermination;
 public
   constructor Create(APeriod: Integer);
   destructor Destroy; override;
   procedure AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);  virtual; abstract;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);  virtual; abstract;
   property Period: Int64 read FPeriod write FPeriod;
   property State: TConnectionProcessorState read FState write FState;
 end;


implementation
uses NMMCommon, NMMServerGlobals;

type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;


constructor TNMMCustomConnectionProcessor.Create(APeriod: Integer);
begin
 inherited Create(true);
 FPeriod:= APeriod;
 GcgtInstances.Increment;
end;

procedure TNMMCustomConnectionProcessor.CheckTermination;
var i: Integer;
begin
 i:= 0;
 while StopWork and not Terminated do
 begin
   Terminate;
   inc(i);
   if i>=100 then
   begin
     break;
   end;
   Sleep(100);
 end;
end;

destructor TNMMCustomConnectionProcessor.Destroy;
begin
 GcgtInstances.Decrement;
 inherited Destroy;
end;

function TNMMCustomConnectionProcessor.StopWork: Boolean;
begin
 result:= Terminated or (cpsTerminating in State) or (cpsTerminated in State);
end;

procedure TNMMCustomConnectionProcessor.SetName;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar('P' + IntToStr(FPeriod) + '_' + TimeToStr(Time));
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
end;

procedure TNMMCustomConnectionProcessor.Execute;
const LFireTolerance= 1; {MilliSec}
      LMaxThreadSleepTime= 300; {MilliSecs}
var LLastShotTime, LCurrentTime: TDateTime;
    MilliSecondsLeft, LThreadSleepTime: Int64;
begin
 SetName;
 FreeOnTerminate:= false;
 LLastShotTime:= Now - FPeriod;
 while not StopWork do
 begin
     LCurrentTime:= Now;
     MilliSecondsLeft:= FPeriod - MilliSecondsBetween(LLastShotTime,LCurrentTime);
     if MilliSecondsLeft <= LFireTolerance then
     begin
       try
         DoWork;
       except
         on E: Exception do
         begin
           try
             AddErrorToLog(E.Message);
           finally
             Terminate;
           end;
         end;
       end;
       LLastShotTime:= LCurrentTime;
     end
     else
     begin
       if MilliSecondsLeft > LFireTolerance then
       begin
         LThreadSleepTime:= MilliSecondsLeft - LFireTolerance;
         if LThreadSleepTime > LMaxThreadSleepTime then
         begin
           LThreadSleepTime:= LMaxThreadSleepTime;
         end;
         Sleep(LThreadSleepTime);
       end;
     end;
 end;

 if Terminated then
 begin
   FState:= FState-[cpsTerminating];
   FState:= FState+[cpsTerminated];
 end;
end;

end.
