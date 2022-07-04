(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMRCClient;

interface
uses NMMBmp2Jpg, NMMCustomDataProcessors, NMMCustomClient,
     Classes, Graphics, ExtCtrls,
     NMMCommon, Types
     ;

const MsgNoImages= 'No images available on the server';

type
 TNMMRCClientThread = class(TNMMCustomClientThread)
 protected
   FTmpBitmap: TBitmap;
   FTmpStream: TMemoryStream;
   procedure LoadBitmap;
   procedure UpdateBitmap;
   procedure IndicateEmptyImage;
   procedure PrintIntoImage(AText: String);
   procedure UpdateLoadingIni;
   procedure SetInitialScreen(ABitmap: TBitmap);
   procedure DoHandleCommand(S: String); override;
 public
   constructor Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
   destructor Destroy; override;
 end;

 TNMMRCClient = class(TNMMCustomClient)
 protected
   FImage: TImage;
   FImageComparer: TNMMCustomImageComparer;

   FOnPictureWndUpdate: TPictureWndUpdateEvent;
   procedure PictureWndUpdate(Sender: TObject; R: TRect);
   function CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread; override;
   procedure ClearData; override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
 published
   property Image: TImage read FImage write FImage;

   property OnPictureWndUpdate: TPictureWndUpdateEvent
            read FOnPictureWndUpdate write FOnPictureWndUpdate;
   property ReduceLoadOnDisconnect;
   property Period;
 end;


implementation

uses SysUtils, NMMTileComparer;


{TNMMRCClient}
constructor TNMMRCClient.Create(AOwner: TComponent);
begin
 inherited;
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FImageComparer:= TNMMTileComparer.Create;;//DataProcessorsFactory.CreateImageComparer;
   FImageComparer.OnPictureWndUpdate:= PictureWndUpdate;
 end;
end;

destructor TNMMRCClient.Destroy;
begin
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   DoDisconnect;
   FreeAndNil(FImageComparer);
 end;
 inherited;
end;

function TNMMRCClient.CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread;
begin
 result:= TNMMRCClientThread.Create(CreateSuspended,AParent);
end;

procedure TNMMRCClient.PictureWndUpdate(Sender: TObject; R: TRect);
begin
 if Assigned(FOnPictureWndUpdate) then
    FOnPictureWndUpdate(Sender,R);
end;

procedure TNMMRCClient.ClearData;
begin
 FImageComparer.Clear;
 inherited;
end;


{TNMMRCClientThread}
constructor TNMMRCClientThread.Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
begin
 inherited Create(CreateSuspended, AParent);
 FTmpBitmap:= TBitmap.Create;
 FTmpStream:= TMemoryStream.Create;
end;

destructor TNMMRCClientThread.Destroy;
begin
 FreeAndNil(FTmpBitmap);
 FreeAndNil(FTmpStream);
 inherited;
end;

procedure TNMMRCClientThread.SetInitialScreen(ABitmap: TBitmap);
var
  LFrameRect: TRect;
  LTextSize: TSize;
  LFrameWidth, LFrameHeight: Integer;
  AText: String;
begin
  AText:= 'Loading ...';
  try
    ABitmap.Canvas.Lock;
    try
      try
        LTextSize:= ABitmap.Canvas.TextExtent(AText);
        LFrameWidth:= ABitmap.Width;
        LFrameHeight:= ABitmap.Height;
        ABitmap.Canvas.Font.Color:= clBlue;
        ABitmap.Canvas.Font.Style:= [fsBold];
        ABitmap.Canvas.Brush.Color:= clBtnFace;
        ABitmap.Canvas.TextOut( LFrameRect.Left + (LFrameWidth-LTextSize.cx) div 2,
                                LFrameRect.Top  + (LFrameHeight-LTextSize.cy) div 2 - LFrameHeight,
                                AText );
      except
        on E: EOutOfResources do
        begin
          AddErrorToLog(E.Message + ' (appeared in TImageProgressBar.WriteToImage)');
        end;
        on E: Exception do
        begin
          AddErrorToLog(E.Message + ' (appeared in TImageProgressBar.WriteToImage)');
          raise;
        end;
        else
        begin
          AddErrorToLog('Unknown error');
        end;
      end;
    finally
      ABitmap.Canvas.Unlock;
    end;
  except
  end;
end;

procedure TNMMRCClientThread.DoHandleCommand(S: String);
var LStrWidth, LStrHeight, LStrImageSize: String;
begin
  if Pos(cmdIniData,S)=1 then
  begin
    ChangeStatus('Loading initial image');
    LStrWidth:= DecodeParam(prmWidth,S);
    LStrHeight:= DecodeParam(prmHeight,S);
    LStrImageSize:= DecodeParam(prmImageSize,S);
    FTmpBitmap.Width:= StrToInt(LStrWidth);
    FTmpBitmap.Height:= StrToInt(LStrHeight);
    (FParent as TNMMRCClient).FImageComparer.SetIniImage(FTmpBitmap);
    ChangeStatus('');
  end;

  if Pos(cmdData,S)=1 then
  begin
    ChangeStatus('Loading new frame');
    FTmpStream.Size:= 0;
    ReadStream(S,cmdData,FTmpStream);
    inc(FParent.Statistics.DeltasReceivedCount);
    FParent.Statistics.LastDeltaSize:= FTmpStream.Size;
    inc(FParent.Statistics.DeltasProcessedCount);
    inc(FParent.Statistics.DeltaBytesReceived, FParent.Statistics.LastDeltaSize);

    (FParent as TNMMRCClient).FImageComparer.AddPacket(FTmpStream);
    Synchronize(LoadBitmap);
    ChangeStatus('');
    FParent.LastDeltaTime:= Now;
  end;

  if Pos(cmdNoImages,S)=1 then
  begin
    Synchronize(IndicateEmptyImage);
    ChangeStatus(MsgNoImages);
  end;
end;

procedure TNMMRCClientThread.UpdateLoadingIni;
begin
 (FParent As TNMMRCClient).LoadingIni:= FLoadingIni;
end;

procedure TNMMRCClientThread.LoadBitmap;
begin
 try
   (FParent As TNMMRCClient).FImageComparer.AssembleNextImage((FParent As TNMMRCClient).Image.Picture.Bitmap);
 except
   on E: Exception do
   begin
     FParent.Disconnect;
     AddErrorToLog(E.Message);
     Sleep(5000);
     FParent.Connect;
     exit;
   end;
 end;
end;

procedure TNMMRCClientThread.UpdateBitmap;
begin
 try
   (FParent As TNMMRCClient).FImageComparer.AssembleNextImage((FParent as TNMMRCClient).FImage.Picture.Bitmap);
 except
   on E: Exception do
   begin
     FParent.Disconnect;
     AddErrorToLog(E.Message);
     Sleep(5000);
     FParent.Connect;
     exit;
   end;
 end;
end;

procedure TNMMRCClientThread.IndicateEmptyImage;
begin
 PrintIntoImage(MsgNoImages);
end;

procedure TNMMRCClientThread.PrintIntoImage(AText: String);
begin
end;


end.
