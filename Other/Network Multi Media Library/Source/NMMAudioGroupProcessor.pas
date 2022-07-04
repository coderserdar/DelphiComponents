(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioGroupProcessor;

interface
uses Classes,
     NMMAudioQueues, NMMCustomConnectionGroupThread, NMMCustomConnectionHandle;

type
 TNMMAudioGroupProcessor = class(TNMMCustomConnectionGroupThread)
 protected
   FDataStream: TMemoryStream;
   FIniDataStream: TMemoryStream;

   procedure DoWork; override;
 public
   DataQueue: TNMMAudioQueue;
   constructor Create;
   destructor Destroy; override;
   procedure AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
   property IniDataStream: TMemoryStream read FIniDataStream write FIniDataStream;
 end;

implementation

uses NMMCustomDataProcessors, NMMConnectionHandles, NMMCustomConnectionProcessor, SysUtils;

constructor TNMMAudioGroupProcessor.Create;
begin
  inherited Create(10);
  FDataStream:= TMemoryStream.Create;
  FIniDataStream:= TMemoryStream.Create;
end;

destructor TNMMAudioGroupProcessor.Destroy;
begin
  FreeAndNil(FDataStream);
  FreeAndNil(FIniDataStream);
  inherited;
end;

procedure TNMMAudioGroupProcessor.DoWork;
var i: Integer;
    LConnectionHandles: TList;
begin
  if Assigned(DataQueue) then
    while not StopWork and (DataQueue.Count>0) do
    begin
      (DataQueue.Get(0) as TNMMDataPacket).GetPackedData(FDataStream);
      DataQueue.Remove(0);
      LConnectionHandles:= FConnectionHandles.List.LockList;
      try
        for i:=0 to LConnectionHandles.Count-1 do
        begin
          if StopWork then
          begin
            TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
            continue;
          end;
          if csNewConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State then
          begin
            if not (csInvalidConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State) then
            begin
              try
                TNMMCustomConnectionHandle(LConnectionHandles[i]).SendIniData(FIniDataStream,0,0);
                TNMMCustomConnectionHandle(LConnectionHandles[i]).State:= TNMMCustomConnectionHandle(LConnectionHandles[i]).State - [csNewConnection];
              except
                TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
              end;
            end else
            begin
              FConnectionHandles.State:= FConnectionHandles.State + [chssNeedsGarbage];
            end;
          end;
         // else

          begin
            if not (csInvalidConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State) then
            begin
              try
                TNMMCustomConnectionHandle(LConnectionHandles[i]).SendData(FDataStream);
              except
                TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
              end;
            end
            else
            begin
              FConnectionHandles.State:= FConnectionHandles.State + [chssNeedsGarbage];
            end;
          end;
        end; // for
      finally
        FConnectionHandles.List.UnlockList;
      end;
    end;

  if chssNeedsGarbage in FConnectionHandles.State then
    FConnectionHandles.GarbageConnectionHandles;
  FState:= FState - [cpsStarting];
end;

procedure TNMMAudioGroupProcessor.RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
  FConnectionHandles.Remove(AConnectionHandle);
  if FConnectionHandles.GetCount=0 then
  begin
  //  FState:= [cpsTerminating,cpsStopped}];
//    FState:= cpsStopped;
    Suspend;
  end;
end;

procedure TNMMAudioGroupProcessor.AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
  if (FConnectionHandles.GetCount=0) or Suspended then
  begin
    DataQueue.Clear;
  end;
  inherited;
end;

end.
