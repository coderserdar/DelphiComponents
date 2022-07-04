(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMRCConnectionProcessor;

interface

uses
 Windows, NMMBmpColorReducer, Classes, Graphics,
 NMMCustomDataProcessors, DateUtils,
 NMMCustomImageSource, NMMCustomConnectionProcessor,
 NMMTileComparer, NMMCustomConnectionHandle,
 IdThreadSafe, NMMStatistics, SyncObjs, SysUtils;


type
 TRCConnectionProcessor = class(TNMMCustomConnectionProcessor)
 protected
   FImageSource: TNMMCustomImageSource;
   FImageComparer: TNMMTileComparer;
   FBitmapPreprocessor: TNMMCustomBitmapProcessor;
   FConnectionHandle: TNMMCustomConnectionHandle;
   FTmpBitmap: TBitmap;
   FTmpDelta: TStream;
   procedure DoWork; override;
 public
   constructor Create(APeriod: Integer);
   destructor Destroy; override;
   procedure AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
   procedure RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle); override;
 end;


implementation
uses NMMCommon, NMMScreenCapturer, NMMServerGlobals;


constructor TRCConnectionProcessor.Create(APeriod: Integer);
begin
 inherited Create(APeriod);
 FImageSource:= TNMMScreenCapturer.Create;
 FImageComparer:= TNMMTileComparer.Create;
 FTmpBitmap:= TBitmap.Create;
 FBitmapPreprocessor:= TNMMBmpColorReducer.Create(pf24bit);
 FTmpDelta:= TMemoryStream.Create;
end;

destructor TRCConnectionProcessor.Destroy;
begin
 CheckTermination;

 try
   if FConnectionHandle<>nil then
     FConnectionHandle.Disconnect;
 except
 end;
 FreeAndNil(FImageSource);
 FreeAndNil(FImageComparer);
 FreeAndNil(FTmpBitmap);
 if FBitmapPreprocessor<>nil then
   FreeAndNil(FBitmapPreprocessor);
 FreeAndNil(FTmpDelta);
 inherited Destroy;
end;

procedure TRCConnectionProcessor.DoWork;
var LImageIsEmpty: Boolean;
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
       LHeader:= cmdIniData + ' ' +
                 EncodeParam(prmWidth,IntToStr(FTmpBitmap.Width)) +
                 EncodeParam(prmHeight,IntToStr(FTmpBitmap.Height));

       FConnectionHandle.PeerThread.Connection.WriteLn(LHeader); //todo ->Client.SendHeader
       FConnectionHandle.State:= FConnectionHandle.State - [csNewConnection];
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

 FState:= FState - [cpsStarting];
end;

procedure TRCConnectionProcessor.AddConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 FConnectionHandle:= AConnectionHandle;
 FImageComparer.Clear;
 FImageComparer.SetConnectionHandle(AConnectionHandle);
 FState:= FState+[cpsStarting];
 if Suspended then
    Resume;
end;

procedure TRCConnectionProcessor.RemoveConnectionHandle(AConnectionHandle: TNMMCustomConnectionHandle);
begin
 FConnectionHandle:= nil;

 FState:= [cpsTerminating];
 FState:= FState+[cpsTerminating];

 FImageComparer.SetConnectionHandle(nil);
 Terminate;
end;

end.
