(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomAudioThread;

interface

uses
  Classes,
  NMMAudioDataPacket, NMMCustomDataProcessors;

type
  TNMMCustomAudioThread = class;
  TOnDataEvent = procedure(Sender: TNMMCustomAudioThread; ADataPacket: TNMMAudioDataPacketInfo) of object;

  TNMMCustomAudioThread = class(TThread)
  protected
    { Private declarations }
    FLastOutPacketInfo: TNMMAudioDataPacketInfo;
    FOnOutData: TOnDataEvent;
    FOnFinished: TNotifyEvent;
    FExceptionStr: String;
    procedure OutData;
    procedure Finish;
    procedure Execute; override;
    procedure DoExecute; virtual; abstract;
    procedure DoPopException;
  public
    { Events }
    constructor Create(CreateSuspended: Boolean);
    property OnOutData: TOnDataEvent read FOnOutData write FOnOutData;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
  end;

implementation

uses NMMCommon, SysUtils;

constructor TNMMCustomAudioThread.Create(CreateSuspended: Boolean);
begin
  inherited;
end;

procedure TNMMCustomAudioThread.Finish;
begin
  if Assigned(FOnFinished) then
     FOnFinished(self);
end;

procedure TNMMCustomAudioThread.OutData;
begin
  if Assigned(FOnOutData) then
     FOnOutData(self,FLastOutPacketInfo);
end;

procedure TNMMCustomAudioThread.DoPopException;
begin
  if FExceptionStr<>'' then
  begin
    // to do: if DebugMode then achive the user with the E.message
  end;
end;

procedure TNMMCustomAudioThread.Execute;
begin
  FreeOnTerminate:= true;
  try
    try
      DoExecute;
    finally
      Finish;
    end;
  except
    on E: Exception do
    begin
      AddErrorToLog('Thread: ' + ClassName + ' ' + E.Message);
      FExceptionStr:= E.Message;
      Synchronize(DoPopException);
      Sleep(100);
      Terminate;
    end;
  end;
end;

end.


