(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioCommon;

interface

uses Classes, MsACM, MMSystem;

const
  GBlockSize= 3200;//4000;//8*1000;
  GSleepTime= 5;//50;

procedure AcmCheck(AResult: Integer);
procedure GetCodecs(ASL: TStrings);

type
  TAudioDataParams= class(TObject)
  public
    WfxCodeMaxSize: Integer;
    PWfxInOut: PWAVEFORMATEX;
    PWfxCode: PWAVEFORMATEX;
    szFormatTag: array [0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of AnsiChar;
    szFormat: array [0..ACMFORMATDETAILS_FORMAT_CHARS-1] of AnsiChar;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Types;

procedure AcmCheck(AResult: Integer);
begin
  case AResult of
  {
    ACMERR_CANCELED:
      raise Exception.Create('The user chose the Cancel button or the Close command on the System menu to close the dialog box.');
      }
    ACMERR_NOTPOSSIBLE:
      raise Exception.Create('Convertion not possible.');
   //   raise Exception.Create('The buffer identified by the pwfx member of the ACMFORMATCHOOSE structure is too small to contain the selected format.');
    MMSYSERR_INVALFLAG:
      raise Exception.Create('At least one flag is invalid.');
    MMSYSERR_INVALHANDLE:
      raise Exception.Create('The specified handle is invalid.');
    MMSYSERR_INVALPARAM:
      raise Exception.Create('At least one parameter is invalid.');
    MMSYSERR_NODRIVER:
      raise Exception.Create('A suitable driver is not available to provide valid format selections.');
    MMSYSERR_NOMEM:
      raise Exception.Create('The system is unable to allocate resources.');
    MMSYSERR_NOTENABLED:
      raise Exception.Create('The driver is not enabled.');
  end;
end;



{TAudioDataParams}
constructor TAudioDataParams.Create;
begin
  inherited;
  GetMem(PWfxInOut,SizeOf(PWfxInOut^));
end;

destructor TAudioDataParams.Destroy;
begin
  if Assigned(PWfxInOut) then FreeMem(PWfxInOut);
  if Assigned(PWfxCode) then FreeMem(PWfxCode);
  inherited;
end;

procedure TAudioDataParams.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    ReadBuffer(PWfxInOut^,SizeOf(PWfxInOut^));
    ReadBuffer(WfxCodeMaxSize,SizeOf(WfxCodeMaxSize));
    if Assigned(PWfxCode) then FreeMem(PWfxCode);
    GetMem(PWfxCode,WfxCodeMaxSize);
    FillChar(PWfxCode^,WfxCodeMaxSize,0);
    ReadBuffer(PWfxCode^,WfxCodeMaxSize);
    ReadBuffer(szFormatTag,SizeOf(szFormatTag));
    ReadBuffer(szFormat,SizeOf(szFormat));
  //  PWfxCode^.cbSize:= 0;
  end;
end;

procedure TAudioDataParams.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    WriteBuffer(PWfxInOut^,SizeOf(PWfxInOut^));
    WriteBuffer(WfxCodeMaxSize,SizeOf(WfxCodeMaxSize));
    WriteBuffer(PWfxCode^,WfxCodeMaxSize);
    WriteBuffer(szFormatTag,SizeOf(szFormatTag));
    WriteBuffer(szFormat,SizeOf(szFormat));
  end;
end;


var AcmDriverEnumCallbackCount: Integer;
var GCodecs: TStrings;

function AcmDriverEnumCallback(hadid: HACMDRIVERID;
         dwInstance: DWORD; fdwSupport: DWORD): LongBool; stdcall;
var
  LPADD: TACMDriverDetails;
  LDetails: DWORD;
begin
  result:= true;
  FillChar(LPADD,SizeOf(LPADD),0);
  LDetails:= 0;
  LPADD.cbStruct:= SizeOf(LPADD);
  try
    AcmCheck(acmDriverDetails(hadid,LPADD,LDetails));
  except
    exit;
  end;
  inc(AcmDriverEnumCallbackCount);
  GCodecs.AddObject(
          Format('%2d',[AcmDriverEnumCallbackCount]) + ' '+  StrPas(LPADD.szShortName),
          Pointer(hadid) );
end;

procedure GetCodecs(ASL: TStrings);
begin
  GCodecs:= ASL;
  AcmDriverEnumCallbackCount:= 0;
  AcmDriverEnum(AcmDriverEnumCallback,0,0);
end;


end.
