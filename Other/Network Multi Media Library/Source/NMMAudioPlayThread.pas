(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioPlayThread;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF},
  MMSystem, SysUtils,
  NMMAudioQueues, NMMAudioCommon, NMMCustomAudioThread;

type
  TNMMAudioPlayThread = class(TNMMCustomAudioThread)
  private
    procedure SetName;
  protected
    hPlay: HWAVEOUT;
    FDataQueue: TNMMAudioQueue;
    procedure DoExecute; override;
  public
    m_Stop: Boolean;
    pwfx: PWAVEFORMATEX;
    DeviceID: Integer;
    property DataQueue: TNMMAudioQueue read FDataQueue write FDataQueue;
    procedure Stop;
  end;

implementation


{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TNMMAudioPlayThread }

procedure TNMMAudioPlayThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'NMMAudioPlayThread';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

procedure WaveOutCheck(result: MMRESULT); // throw(char*)
begin
  case result of
    MMSYSERR_NOERROR:
      exit;
    MMSYSERR_ALLOCATED:
      raise Exception.Create('Driver is already open');
    MMSYSERR_BADDEVICEID:
      raise Exception.Create('Wrong device ID');
    MMSYSERR_NODRIVER:
      raise Exception.Create('No device driver');
    MMSYSERR_NOMEM:
      raise Exception.Create('No memory');
    WAVERR_BADFORMAT:
      raise Exception.Create('Bad data format');
    WAVERR_SYNC:
      raise Exception.Create('Try of playing on a synchronous device');
    MMSYSERR_INVALHANDLE:
      raise Exception.Create('Wrong device key');
    WAVERR_UNPREPARED:
      raise Exception.Create('Try of playing unprepared data block');
    WAVERR_STILLPLAYING:
      raise Exception.Create('Data block is in the playing queue');
  end;
end;


procedure TNMMAudioPlayThread.DoExecute;
const
  HeaderArraySize = 2;
var
  BlockSize, WaitTime, LGetMemSize, ii: Integer;
//  PlayPosition: Longint;
  Header: Array [0..HeaderArraySize-1] of TWAVEHDR;
  curhdr: PWAVEHDR;
  hEvent: THANDLE;
  si: TSystemInfo;
  LPackID: Integer;
  LPackTime: TDateTime;
begin
  FreeOnTerminate:= true;
  SetName;

  BlockSize:= GBlockSize;
  hEvent:= CreateEvent(nil, FALSE, FALSE, nil);

//  PlayPosition:= 0;
  WaitTime:= BlockSize*2000 div (pwfx^.nAvgBytesPerSec);
  m_Stop:= false;
  GetSystemInfo(si);
  FillChar(Header, sizeof(Header), 0);
  WaveOutCheck(waveOutOpen(@hPlay, DeviceID, pwfx, hEvent, 0, CALLBACK_EVENT));

  // dwPageSize tricks - not sure it's needed in Pascal. Just BlockSize?
  LGetMemSize:= (BlockSize+si.dwPageSize-1) div si.dwPageSize*si.dwPageSize;
  GetMem(Header[0].lpData,LGetMemSize);
  Win32Check(Header[0].lpData<>Nil);
  GetMem(Header[1].lpData,LGetMemSize);
  Win32Check(Header[1].lpData<>Nil);
  Header[1].dwBufferLength:= BlockSize;
  Header[0].dwBufferLength:= Header[1].dwBufferLength;
  WaveOutCheck(waveOutPrepareHeader(hPlay, @Header[0], sizeof(WAVEHDR)));
  WaveOutCheck(waveOutPrepareHeader(hPlay, @Header[1], sizeof(WAVEHDR)));

  try
    //if Only One packet then ResetEvent(hEvent);

   // PlayPosition:=0;
    ii:= 0;
    while not Terminated and not m_Stop and not DataQueue.Finished do
    begin
      if DataQueue.Count = 0 then
      begin
        if DataQueue.Finished then
           break
        else
        begin
          Sleep(GSleepTime); Continue;
        end;
      end;
      if m_Stop then break;

     // inc(PlayPosition,BlockSize);
     // if PlayPosition<DataSize then
      begin
        curhdr:= @Header[ii];
        inc(ii);
        if ii >= HeaderArraySize then ii:= 0;
        {
        if BlockSize > DataSize-PlayPosition then
           BlockSize:= DataSize-PlayPosition;
         }
//        curhdr.dwBufferLength:= BlockSize;

        // get data
        if Assigned(DataQueue) then
        begin
          if DataQueue.PushRawData(curhdr.lpData, curhdr.dwBufferLength, LPackID, LPackTime) then
             WaveOutCheck(waveOutWrite(hPlay, curhdr, sizeof(WAVEHDR)));
        end;
      end;

      if WAIT_TIMEOUT = WaitForSingleObject(hEvent, WaitTime) then
         raise Exception.Create('Playing failed due to timeout');

      if m_Stop then break;
    end; //while

  finally
    WaveOutCheck(waveOutReset(hPlay));
    m_Stop:= true;
    waveOutUnprepareHeader(hPlay, @Header[0], sizeof(WAVEHDR));
    waveOutUnprepareHeader(hPlay, @Header[1], sizeof(WAVEHDR));
    waveOutClose(hPlay);
    FreeMem(Header[0].lpData,LGetMemSize);
    FreeMem(Header[1].lpData,LGetMemSize);
  end;
end;

procedure TNMMAudioPlayThread.Stop;
begin
  Suspend;
  try
    if hPlay <> 0 then
    begin
      m_Stop:= true;
      WaveOutCheck(waveOutReset(hPlay));
    end;
  finally
    Resume;
  end;
end;

end.
