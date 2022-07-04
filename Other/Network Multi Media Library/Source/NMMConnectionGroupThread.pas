(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMConnectionGroupThread;

interface

uses
 Windows, NMMBmp2Jpg, NMMBmpColorReducer, Classes, NMMConnectionHandles,
 NMMCustomConnectionHandle, NMMCustomDataProcessors, DateUtils,
 NMMCustomImageSource, Graphics, NMMCustomConnectionProcessor,
 NMMCustomConnectionGroupThread,
 IdThreadSafe, NMMStatistics, SyncObjs, SysUtils;


type
 TNMMConnectionGroupThread = class(TNMMCustomConnectionGroupThread)
 protected
   FImageSource: TNMMCustomImageSource;
   FImageComparer: TNMMCustomImageComparer;
   FBitmapPreprocessor: TNMMCustomBitmapProcessor;
   FTmpBitmap: TBitmap;
   FTmpDelta: TStream;
   procedure ClearDataProcessors; 
   procedure DoWork; override;
 public
   constructor Create(AImageSource: TNMMCustomImageSource;
                      AImageComparer: TNMMCustomImageComparer;
                      APeriod: Integer);
   destructor Destroy; override;
   procedure AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
 end;


implementation
uses NMMCommon, NMMServerGlobals;



{TNMMConnectionGroupThread}
constructor TNMMConnectionGroupThread.Create(AImageSource: TNMMCustomImageSource;
                                AImageComparer: TNMMCustomImageComparer;
                                APeriod: Integer);
begin
 inherited Create(APeriod);
 FImageSource:= AImageSource;
 FImageComparer:= AImageComparer;
 FTmpBitmap:= TBitmap.Create;
 FBitmapPreprocessor:= DataProcessorsFactory.CreateBitmapPreprocessor;
 FTmpDelta:= TMemoryStream.Create;
end;

destructor TNMMConnectionGroupThread.Destroy;
begin
 CheckTermination;
 try
   FConnectionHandles.DisconnectAll;
   FreeAndNil(FImageComparer);
   FreeAndNil(FTmpBitmap);
   if FBitmapPreprocessor<>nil then
     FreeAndNil(FBitmapPreprocessor);
 finally
 end;
 FreeAndNil(FTmpDelta);
 inherited Destroy;
end;

procedure TNMMConnectionGroupThread.DoWork;
var i: Integer;
    LImageIsEmpty: Boolean;
    LConnectionHandles: TList;
    LHeader: String;
begin
 FTmpBitmap.FreeImage;

 FImageSource.GetImage(FTmpBitmap);
 if StopWork then exit;
 LImageIsEmpty:= (FTmpBitmap.Width*FTmpBitmap.Height = 0);

 if not LImageIsEmpty then
 begin
   if cpsStarting in FState then
   begin
     if FBitmapPreprocessor<>nil then
     begin
       FState:= FState + [cpsCalculations];
       try
         FBitmapPreprocessor.Process(FTmpBitmap);
       finally
         FState:= FState - [cpsCalculations];
       end;
     end;
     if StopWork then exit;

     FState:= FState + [cpsCalculations];
     try
       FImageComparer.SetIniImage(FTmpBitmap);
     finally
       FState:= FState - [cpsCalculations];
     end;
     if StopWork then exit;
   end
   else
   begin
     if FBitmapPreprocessor<>nil then
     begin
       FState:= FState + [cpsCalculations];
       try
         FBitmapPreprocessor.Process(FTmpBitmap);
       finally
         FState:= FState - [cpsCalculations];
       end;
     end;
     if StopWork then exit;

     try
       FState:= FState + [cpsCalculations];
       try
         FImageComparer.ProcessNewImage(FTmpBitmap);
       finally
         FState:= FState - [cpsCalculations];
       end;
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
     if StopWork then exit;
   end;
 end;

 LConnectionHandles:= FNMMConnectionHandles.List.LockList;
 try
   for i:=0 to LConnectionHandles.Count-1 do
   begin
     if StopWork then
     begin
       TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
       continue;
     end;

     if LImageIsEmpty then
     begin
       TNMMCustomConnectionHandle(LConnectionHandles[i]).SendRawText(cmdNoImages)
     end
     else
     begin
       if csNewConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State then
       begin
         if not (csInvalidConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State) then
         begin
           try
             LHeader:= cmdIniData + ' ' +
                       EncodeParam(prmWidth,IntToStr(FTmpBitmap.Width)) +
                       EncodeParam(prmHeight,IntToStr(FTmpBitmap.Height));

             TNMMCustomConnectionHandle(LConnectionHandles[i]).PeerThread.Connection.WriteLn(LHeader); //todo ->Client.SendHeader
             TNMMCustomConnectionHandle(LConnectionHandles[i]).State:= TNMMCustomConnectionHandle(LConnectionHandles[i]).State - [csNewConnection];
           except
             TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
           end;
         end else
         begin
           FConnectionHandles.State:= FConnectionHandles.State + [chssNeedsGarbage];
         end;
       end
       else
       begin
         if not (csInvalidConnection in TNMMCustomConnectionHandle(LConnectionHandles[i]).State) then
         begin
           try
             while FImageComparer.PacketCount > 0 do
             begin
               FImageComparer.GetPacket(FTmpDelta);
               TNMMCustomConnectionHandle(LConnectionHandles[i]).SendData(FTmpDelta);
             end;
           except
             TNMMCustomConnectionHandle(LConnectionHandles[i]).SetInvalid;
           end;
         end
         else
         begin
           FConnectionHandles.State:= FConnectionHandles.State + [chssNeedsGarbage];
         end;
       end;
     end;
   end;

   if LConnectionHandles.Count=0 then
   begin
     FState:= FState-[cpsTerminating];
     Terminate;
   end;
 finally
   FConnectionHandles.List.UnlockList;
 end;

 if chssNeedsGarbage in FConnectionHandles.State then
   FConnectionHandles.GarbageConnectionHandles;

 FState:= FState - [cpsStarting];
end;

procedure TNMMConnectionGroupThread.ClearDataProcessors;
begin
  FImageComparer.Clear;
end;

procedure TNMMConnectionGroupThread.AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 try
   FConnectionHandles.Add(AConnectionHandle);

   if not (FConnectionHandles.GetCount=1) then
   begin
    if FImageComparer.PacketCount > 0 then
      AConnectionHandle.SendIniData(FImageComparer.GetIniImageStream,FTmpBitmap.Width,FTmpBitmap.Height);
   end
   else
   begin
     ClearDataProcessors;
     FState:= FState+[cpsStarting];
     if Suspended then
       Resume;
   end;
 finally
 end;
end;


end.
