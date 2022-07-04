(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioRecordThread;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF},
  MMSystem, SysUtils,
  NMMAudioQueues, NMMAudioCommon, NMMCustomAudioThread;

type
  TNMMAudioRecordThread = class(TNMMCustomAudioThread)
  private
    procedure SetName;
  protected
    m_Stop: Boolean;
    m_hRecord: HWAVEIN;
    m_RecordPosition: Longint;
    FDataQueue: TNMMAudioRecordQueue;
    procedure DoExecute; override;
  public
    pwfx: PWAVEFORMATEX;
    DeviceID: Integer;
    property DataQueue: TNMMAudioRecordQueue read FDataQueue write FDataQueue;
    procedure Stop;
  end;

implementation

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TNMMAudioRecordThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TNMMAudioRecordThread }

procedure TNMMAudioRecordThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'NMMAudioRecordThread';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

procedure Win32Check(IsOk: Boolean); // throw(char*)
var msg: Array[0..299] of Char;
begin
  if IsOk then exit;
  FormatMessage(FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_FROM_SYSTEM,
		nil, GetLastError(),  0, msg, sizeof(msg)-1, nil);
  raise Exception.Create(StrPas(msg));
end;

procedure WaveInCheck(res: MMRESULT); // throw(char*)
var ErrText: Array[0..MAXERRORLENGTH+1] of Char;
begin
  if MMSYSERR_NOERROR = res then exit;
  ErrText:= '';
  waveInGetErrorText(res, ErrText, sizeof(ErrText));
  raise Exception.Create(StrPas(ErrText));
end;


procedure TNMMAudioRecordThread.DoExecute;
const
  HeaderArraySize = 2;
var
  BlockSize, WaitTime, LGetMemSize, ii: Integer;
  Header: Array [0..HeaderArraySize-1] of TWAVEHDR;
  curhdr: PWAVEHDR;
  hEvent: THANDLE;
  si: TSystemInfo;
begin
  SetName;
  DataQueue.Clear;
  DataQueue.Finished:= false;

  BlockSize:= GBlockSize;
  WaitTime:= {!!2*}BlockSize*2000 div pwfx^.nAvgBytesPerSec;
  m_Stop:= false;

  GetSystemInfo(si);

  FillChar(Header,sizeof(Header),0);

  m_hRecord:= 0;
  m_RecordPosition:= 0;
  hEvent:= CreateEvent(nil, TRUE, FALSE, nil);

  Win32Check(hEvent<>0);

  WaveInCheck(waveInOpen(@m_hRecord, DeviceID, pwfx,
          hEvent, 0, CALLBACK_EVENT));

  // dwPageSize tricks - not sure it's needed in Pascal. Just BlockSize?
  LGetMemSize:= (BlockSize+si.dwPageSize-1) div si.dwPageSize*si.dwPageSize;
  GetMem(Header[0].lpData,LGetMemSize);
  Win32Check(Header[0].lpData<>Nil);
  GetMem(Header[1].lpData,LGetMemSize);
  Win32Check(Header[1].lpData<>Nil);

  Header[1].dwBufferLength:= BlockSize;
  Header[0].dwBufferLength:= Header[1].dwBufferLength;
  WaveInCheck(waveInPrepareHeader(m_hRecord, @Header[0], sizeof(WAVEHDR)));
  WaveInCheck(waveInPrepareHeader(m_hRecord, @Header[1], sizeof(WAVEHDR)));

  try
    WaveInCheck(waveInAddBuffer(m_hRecord, @Header[0], sizeof(WAVEHDR)));
    WaveInCheck(waveInAddBuffer(m_hRecord, @Header[1], sizeof(WAVEHDR)));
    ResetEvent(hEvent);
    WaveInCheck(waveInStart(m_hRecord));
    ii:=0;

    while not Terminated do
    begin
      if WAIT_TIMEOUT = WaitForSingleObject(hEvent, WaitTime) then
         exit;
        // raise Exception.Create('Recording failed due to timeout');
      ResetEvent(hEvent);
      curhdr:= @Header[ii];
      inc(ii);
      if ii >= HeaderArraySize then ii:= 0;

      if curhdr.dwBytesRecorded > 0 then
      begin
        if Assigned(DataQueue) then
        begin
          FLastOutPacketInfo:= DataQueue.PutRawData(curhdr.lpData, curhdr.dwBytesRecorded, DataQueue.PacketCount, Now);
          if Assigned(FOnOutData) then Synchronize(OutData);
        end;
        inc(m_RecordPosition, curhdr.dwBytesRecorded);
        curhdr.dwBytesRecorded:= 0;
      end;

      if m_Stop then
      begin
        if (0 = Header[0].dwBytesRecorded) and
           (0 = Header[1].dwBytesRecorded) then break;
      end
      else
        WaveInCheck(waveInAddBuffer(m_hRecord, curhdr, sizeof(WAVEHDR)));
    end;
  finally
    if m_hRecord<>0 then
    begin
      try
        DataQueue.Finished:= true;
      except
      end;
      waveInReset(m_hRecord);
      waveInUnprepareHeader(m_hRecord, @Header[0], sizeof(WAVEHDR));
      waveInUnprepareHeader(m_hRecord, @Header[1], sizeof(WAVEHDR));
      waveInClose(m_hRecord);
      m_hRecord:= 0;
      FreeMem(Header[0].lpData,LGetMemSize);
      FreeMem(Header[1].lpData,LGetMemSize);
    end;
    CloseHandle(hEvent);
  end;
end;

procedure TNMMAudioRecordThread.Stop;
begin
  Suspend;
  try
    if m_hRecord <> 0 then
    begin
      WaveInCheck(waveInReset(m_hRecord));
      m_Stop:= true;
    end;
  finally
    Resume;
  end;
end;

end.
