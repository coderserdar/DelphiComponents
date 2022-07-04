(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMACMThread;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF},
  MMSystem, msacm, SysUtils,
  NMMAudioQueues, NMMAudioCommon, NMMCustomAudioThread;


type
  TNMMACMThreadType= (attEncode,attDecode);

  TNMMACMThread = class(TNMMCustomAudioThread)
  private
    procedure SetName;
  protected
    procedure DoExecute; override;
  public
    wfxIn, wfxOut: PWAVEFORMATEX;
    InQueue, OutQueue: TNMMAudioQueue;
//    DriverID: HACMDRIVERID;
    m_Stop: Boolean;
    ThreadType: TNMMACMThreadType;
    InitialDelay: Integer;
    RejectedOverduedPackets: Integer;
  end;

implementation

uses DateUtils, NMMCommon;

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TNMMACMThread }

procedure TNMMACMThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  case ThreadType of
    attEncode:
     ThreadNameInfo.FName:= 'NMMACMThread encoding';
    attDecode:
     ThreadNameInfo.FName:= 'NMMACMThread decoding';
  end;
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

procedure TNMMACMThread.DoExecute;
var
  hStream: HACMSTREAM;
  Header: ACMSTREAMHEADER;
  iStreamClose: Integer;
  LPacketsCount: Longint;
  SrcSize, DestSize, Flags: DWORD;
  LPackID: Integer;
  LPackTime: TDateTime;
  lVad: LPARAM;
  LStub1, LStub2: LPARAM;
begin
  RejectedOverduedPackets:= 0;
  SetName;
  Sleep(InitialDelay);
  m_Stop:= false; LPacketsCount:= 0;
  OutQueue.Clear;
  OutQueue.Finished:= false;

  FillChar(Header, sizeof(Header), 0);
  Header.cbStruct:= sizeof(Header);
  acmCheck(acmStreamOpen(hStream, nil, wfxIn^, wfxOut^, nil, 0, 0, 0{ACM_STREAMOPENF_NONREALTIME}));
  try
    SrcSize:= GBlockSize;  DestSize:= GBlockSize;
    case ThreadType of
      attEncode:
      begin
        acmStreamSize(hStream, SrcSize, DestSize, ACM_STREAMSIZEF_SOURCE);
        {
        LStub:= 0;
        acmDriverID(@hStream,DriverID,LStub);
        LStub:= 0;
        acmDriverOpen(hDriver,DriverID,LStub);
        }
       // if DriverID=HACMDRIVERID(517){Speex} then
        begin
{
MMSYSERR_INVALHANDLE
MMSYSERR_INVALPARAM
MMSYSERR_NOTSUPPORTED
}
(*
          LStub1:= 0;
          LStub2:= 0;
          lVAD:= acmStreamMessage(hStream, ACMDM_USER+31{SPEEX_GET_VAD}, LStub1, LStub2);
          LStub1:= 1;
          LStub2:= 0;
          if lVAD=0 then
             acmStreamMessage(hStream, ACMDM_USER+30{SPEEX_SET_VAD}, LStub1, LStub2);
          LStub1:= 0;
          LStub2:= 0;
          lVAD:= acmStreamMessage(hStream, ACMDM_USER+31{SPEEX_GET_VAD}, LStub1, LStub2);
*)          
        end;
      end;
      attDecode:
        acmStreamSize(hStream, DestSize, SrcSize, ACM_STREAMSIZEF_DESTINATION);
    end;

    GetMem(Header.pbSrc,SrcSize); Win32Check(Header.pbSrc <> nil);
    GetMem(Header.pbDst,DestSize);   Win32Check(Header.pbDst <> nil);
    Header.cbSrcLength:= SrcSize;
    Header.cbDstLength:= DestSize;

    try
      if MMSYSERR_NOERROR <> acmStreamPrepareHeader(hStream, Header, 0) then
         raise Exception.Create('Cannot prepare stream header');
      try
        while not (Terminated and (InQueue.Count=0)) and not m_stop
              and not InQueue.Finished do
        begin
          if InQueue.Count = 0 then
          begin
            if InQueue.Finished then
               break
            else
            begin
              Sleep(GSleepTime); Continue;
            end;
          end;

          Flags:= ACM_STREAMCONVERTF_BLOCKALIGN;
          if LPacketsCount=0 then
             Flags:= ACM_STREAMCONVERTF_START;
          if (Terminated or m_Stop) and (InQueue.Count=1) then
             Flags:= Flags + ACM_STREAMCONVERTF_END;

          InQueue.PushRawData(Header.pbSrc, Header.cbSrcLength, LPackID, LPackTime);
          if (InitialDelay > 0) and
             (MillisecondsBetween(LPackTime,Now) > 2*InitialDelay) then
          begin
            inc(RejectedOverduedPackets);
            Continue;
          end;

          if MMSYSERR_NOERROR <> acmStreamConvert(hStream, Header, Flags) then
             raise Exception.Create('Data conversion error');
          if (Header.cbSrcLengthUsed<>Header.cbSrcLength) and
              not ( (InQueue.Count=0) and InQueue.Finished) then
              raise Exception.Create('Header.cbSrcLengthUsed<>Header.cbSrcLength');
          FLastOutPacketInfo:= OutQueue.PutRawData(Header.pbDst, Header.cbDstLengthUsed, LPackID, LPackTime);
          if Assigned(FOnOutData) then
             Synchronize(OutData);

          inc(LPacketsCount);
          if m_Stop then break;
        end;
      finally
	acmStreamUnprepareHeader(hStream, Header, 0);
        OutQueue.Finished:= true;
      end;
    finally
      FreeMem(Header.pbSrc,SrcSize);  FreeMem(Header.pbDst,DestSize);
      if RejectedOverduedPackets>0 then
        AddErrorToLog('RejectedOverduedPackets='+IntToStr(RejectedOverduedPackets));
    end;
  finally
    for iStreamClose:=1 to 500 do
    begin
      if acmStreamClose(@hStream,0) <> ACMERR_BUSY then break
      else Sleep(10);
    end;
    {
    LStub:= 0;
    if hDriver<>nil then
       acmDriverClose(hDriver,LStub);
       }
  end;
end;

end.
