(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomConnectionGroupThread;

interface

uses NMMConnectionHandles, NMMCustomConnectionProcessor, NMMCustomConnectionHandle;

type
 TNMMCustomConnectionGroupThread = class(TNMMCustomConnectionProcessor)
 protected
   FConnectionHandles: TNMMConnectionHandles;
 public
   constructor Create(APeriod: Integer);
   destructor Destroy; override;
   procedure AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
   property ConnectionHandles: TNMMConnectionHandles read FConnectionHandles;
 end;

implementation

uses SysUtils;

constructor TNMMCustomConnectionGroupThread.Create(APeriod: Integer);
begin
 inherited Create(APeriod);
 FConnectionHandles:= TNMMConnectionHandles.Create;
end;

destructor TNMMCustomConnectionGroupThread.Destroy;
begin
 CheckTermination;
 FreeAndNil(FConnectionHandles);
 inherited Destroy;
end;

procedure TNMMCustomConnectionGroupThread.AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 FConnectionHandles.Add(AConnectionHandle);

 if (FConnectionHandles.GetCount=1) then
 begin
   FState:= FState+[cpsStarting];
   if Suspended then
   begin
     Resume;
     FState:= FState-[cpsStopped];
   end;
 end;
end;

procedure TNMMCustomConnectionGroupThread.RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 FConnectionHandles.Remove(AConnectionHandle);
 if FConnectionHandles.GetCount=0 then
 begin
   FState:= [cpsTerminating{cpsStopped}];
   FState:= FState+[cpsTerminating];
   Terminate;
 end;
end;


end.
