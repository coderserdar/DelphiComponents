{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnModem;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�CnModem��׼���ƽ���������Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��CnModem�����CnRS232����ͨѶ�����������
*           �ṩ����AT����ͨ������ֱ�Ӳ������ƽ�����Ĺ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.08 V1.0
*                ������Ԫ
*                ����ע��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnConsts, CnNetConsts, CnRS232, IniFiles;

type

//------------------------------------------------------------------------------
// ��׼���ƽ�������
//------------------------------------------------------------------------------

{ TCnModem }

  TDialResult = (drConnect, drOpenCommFail, drNoModem, drNoDialtone, drBusy,
    drNoAnswer, drNoCarrier, drTimeout, drUnknow);
  {* Modem���Ž������
   |<PRE>
     drConnect:         - ���ӳɹ�
     drOpenCommFail:    - �򿪴���ʧ��
     drNoModem:         - û�м�⵽Modem
     drNoDialtone:      - �޲�����
     drBusy:            - ��⵽æ�ź�
     drNoAnswer:        - ��Ӧ���ź�
     drNoCarrier:       - û�м�⵽�ز��ź�
     drTimeout:         - ��ʱ��
     drUnknow:          - δ֪����
   |</PRE>}

  TATResult = (arOk, arConnect, arRing, arNoCarrier, arError, arNoDialtone,
    arBusy, arNoAnswer, arTimeout, arUnknow);
  {* AT����ִ�н������
   |<PRE>
     arOk:              - �ɹ�
     arConnect:         - ������
     arRing:            - �����ź�
     arNoCarrier:       - û�м�⵽�ز��ź�
     arError:           - ִ�д���
     arNoDialtone:      - �޲�����
     arBusy:            - ��⵽æ�ź�
     arNoAnswer:        - ��Ӧ���ź�
     arTimeout:         - ��ʱ��
     arUnknow:          - δ֪����
   |</PRE>}

  TModemVolume = (mvLowest, mvLow, mvMiddle, mvHigh);
  {* Modem ����
   |<PRE>
     mvLowest:          - ��С����
     mvLow:             - С����
     mvMiddle:          - �е�����
     mvHigh:            - ������
   |</PRE>}

  TRingEvent = procedure(Sender: TObject; var Answer: Boolean) of object;
  {* ���յ������¼�����������Answer�����Ƿ�Ӧ��}
  TConnectEvent = procedure(Sender: TObject; Rate: Integer) of object;
  {* �����ӳɹ��¼�������RateΪ�����ٶ�}
  TInvalidCommandEvent = procedure(Sender: TObject; const Command: string) of object;
  {* �Ƿ���AT�����¼�������Ϊ�����������}
  TModemState = (msUnknow, msOffline, msOnline, msOnlineCommand, msConnecting);
  {* ��ǰModem״̬����
   |<PRE>
     msUnknow:          - δ֪״̬
     msOffline:         - ����״̬
     msOnline:          - ����״̬
     msOnlineCommand:   - ��������״̬
     msConnecting:      - ��������״̬
   |</PRE>}
  TStateChangeEvent = procedure(Sender: TObject; State: TModemState) of object;
  {* ��ǰModem״̬�ı��¼�}

  TCnModem = class(TCnRS232)
  {* ��׼���ƽ����ͨѶ���
   |<PRE>
     * �����TCnRS232����������ͨ���򴮿ڷ���AT���������Ʊ�׼ Modem ͨѶ��
     * ʹ��ʱ��ֱ�ӵ��� Dial �������в������ӣ�������ɷ���ִ�н����
     * �� Modem ��⵽�����ź�ʱ������ OnRing �¼���
     * Hangup �����ɹһ�������ӣ�ͨѶʱ��������жϣ������� OnDisConnect �¼���
     * ���ӳɹ���ͨ��ʹ�ü̳����ķ��� WriteCommData �� Modem �������ݡ�
     * ֻ�е� Modem ��������״̬ʱ���յ����ݲŻ���� OnReceiveData �¼���
   |</PRE>}
  private
    { Private declarations }
    FCheckDialtone: Boolean;
    FCheckBusy: Boolean;
    FAutoAnswer: Boolean;
    FVolume: TModemVolume;
    FWaitEscapeTime: Integer;
    FWaitDialtoneTime: Integer;
    FWaitCarrierTime: Integer;
    FInitATCommand: string;
    FModemState: TModemState;
    FOnConnect: TConnectEvent;
    FOnDisConnect: TNotifyEvent;
    FOnRing: TRingEvent;
    FOnInvalidCommand: TInvalidCommandEvent;
    FOnStateChange: TStateChangeEvent;
    FWaitATResult: Boolean;
    FATResult: string;
    FConnectRate: Integer;
    procedure SetAutoAnswer(const Value: Boolean);
    procedure SetVolume(const Value: TModemVolume);
    procedure SetInitATCommand(const Value: string);
    procedure SetWaitCarrierTime(const Value: Integer);
    procedure SetWaitDialtoneTime(const Value: Integer);
    procedure SetWaitEscapeTime(const Value: Integer);
    procedure SetCheckBusy(const Value: Boolean);
    procedure SetCheckDialtone(const Value: Boolean);
    procedure SetModemState(const Value: TModemState);
    function WaitATResult(Delay: Cardinal): string;
    function SendATOk(AT: string; Delay: Cardinal = 200): Boolean;
    function StrToIntEx(const Str: string): Integer;
  protected
    { Protected declarations }
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function CommOpened: Boolean;
    function OpenComm: Boolean;
    procedure Changed;
    procedure ReceiveData(Buffer: PAnsiChar; BufferLength: WORD); override;
    procedure _SendDataEmpty; override;
    procedure Ring; virtual;
    procedure Connect(Rate: Integer); virtual;
    procedure DisConnect; virtual;
    procedure InvalidCommand(const Command: string); virtual;
    procedure Escape;
    procedure Resume;
    function Answer: TDialResult;
    property ModemState: TModemState read FModemState write SetModemState;
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ��ʽ}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InitModem: Boolean;
    {* ��ʼ��Modem��һ�㲻��Ҫ�ֹ�����}
    function Dial(const Number: string): TDialResult;
    {* ���ŷ���������Ϊ�Է��绰������}
    procedure WriteATCommand(const Command: string; Return: Boolean = True);
    {* дAT������������û��ֹ��� Modem ����AT���
     |<PRE>
       Command: string  - AT������
       Return: Boolean  - �Ƿ��Զ���������ĩβ���ӻس���Ĭ��Ϊ��
     |</PRE>}
    procedure Hangup;
    {* �һ������ǰ����}
    procedure ReadFromIni(Ini: TCustomIniFile; const Section: string); override;
    procedure WriteToIni(Ini: TCustomIniFile; const Section: string); override;
    property State: TModemState read FModemState;
    {* ��ǰ�� Modem ״̬��������ֻ������}
    property ConnectRate: Integer read FConnectRate;
    {* ��ǰ�������ٶȣ�������ֻ������}
  published
    { Published declarations }
    property CheckDialtone: Boolean read FCheckDialtone write SetCheckDialtone default
      True;
    {* ����ǰ�Ƿ��Ⲧ����}
    property CheckBusy: Boolean read FCheckBusy write SetCheckBusy default True;
    {* ����ǰ�Ƿ���æ�ź�}
    property AutoAnswer: Boolean read FAutoAnswer write SetAutoAnswer default False;
    {* �Ƿ������Զ�Ӧ��������������� OnRing �¼��Ƿ����������Զ�Ӧ��}
    property Volume: TModemVolume read FVolume write SetVolume default mvMiddle;
    {* Modem ����}
    property WaitDialtoneTime: Integer read FWaitDialtoneTime write SetWaitDialtoneTime
      default 2;
    {* �ȴ����������ʱ�䣬��λΪ��}
    property WaitCarrierTime: Integer read FWaitCarrierTime write SetWaitCarrierTime
      default 50;
    {* �ȴ�����ز����ʱ�䣬��λΪ��}
    property WaitEscapeTime: Integer read FWaitEscapeTime write SetWaitEscapeTime
      default 50;
    {* �л�����������״̬�ĵȴ�ʱ�䣬��λΪ 20 ����}
    property InitATCommand: string read FInitATCommand write SetInitATCommand;
    {* ���ڳ�ʼ�� Modem �Ķ���AT���������߼��û�ʹ��}
    property OnRing: TRingEvent read FOnRing write FOnRing;
    {* �����¼�}
    property OnConnect: TConnectEvent read FOnConnect write FOnConnect;
    {* ���ӳɹ��¼�}
    property OnInvalidCommand: TInvalidCommandEvent read FOnInvalidCommand write
      FOnInvalidCommand;
    {* ��⵽��Ч��AT�����¼�}
    property OnDisConnect: TNotifyEvent read FOnDisConnect write FOnDisConnect;
    {* �����ж��¼�}
    property OnStateChange: TStateChangeEvent read FOnStateChange write FOnStateChange;
    {* Modem ״̬�ı��¼�}
  end;

implementation

//------------------------------------------------------------------------------
// ��׼���ƽ�������
//------------------------------------------------------------------------------

{ TCnModem }

// ����ֵ����
procedure TCnModem.Assign(Source: TPersistent);
begin
  if Source is TCnModem then
  begin
    FCheckDialtone := TCnModem(Source).FCheckDialtone;
    FCheckBusy := TCnModem(Source).FCheckBusy;
    FAutoAnswer := TCnModem(Source).FAutoAnswer;
    FWaitEscapeTime := TCnModem(Source).FWaitEscapeTime;
    FWaitDialtoneTime := TCnModem(Source).FWaitDialtoneTime;
    FWaitCarrierTime := TCnModem(Source).FWaitCarrierTime;
    FInitATCommand := TCnModem(Source).FInitATCommand;
  end;
  inherited;
end;

// ��ʼ��
constructor TCnModem.Create(AOwner: TComponent);
begin
  inherited;
  FCheckDialtone := True;
  FCheckBusy := True;
  FAutoAnswer := False;
  FVolume := mvMiddle;
  FWaitDialtoneTime := 2;
  FWaitCarrierTime := 50;
  FWaitEscapeTime := 50;
  FInitATCommand := '';
  FModemState := msOffline;
  FWaitATResult := False;
  FATResult := '';
  FConnectRate := 0;
  CommConfig.Outx_CtsFlow := True;
  CommConfig.Outx_DsrFlow := True;
end;

// �ͷ�
destructor TCnModem.Destroy;
begin
  Hangup;
  inherited;
end;

// ͨѶ״̬
function TCnModem.CommOpened: Boolean;
begin
  Result := Handle <> 0;
end;

// �򿪴��ڣ����سɹ����
function TCnModem.OpenComm: Boolean;
begin
  Result := CommOpened;
  if not Result then
  begin
    try
      StartComm;
      Result := True;
    except
      Exit;
    end;
  end;
end;

// �����ѱ��
procedure TCnModem.Changed;
begin
  if (ComponentState * [csDesigning, csLoading, csDestroying] = [])
    and CommOpened then
    InitModem;
end;

// �������ݻ�������
procedure TCnModem._SendDataEmpty;
begin
  if ModemState = msOnline then // ��������״̬�²����¼�
    inherited;
end;

// �ַ���ת��Ϊ����
function TCnModem.StrToIntEx(const Str: string): Integer;
var
  SInt: string;
  i: Integer;
begin
  SInt := '';
  for i := 1 to Length(Str) do
    if {$IFDEF UNICODE}CharInSet(Str[i], ['0'..'9']){$ELSE}Str[i] in ['0'..'9']{$ENDIF} then // ��ȡ�����ַ�
      SInt := SInt + Str[i];
  if SInt <> '' then
    Result := StrToInt(SInt)
  else
    Result := 0;
end;

// ����AT�������
procedure TCnModem.WriteATCommand(const Command: string; Return: Boolean);
var
  s: AnsiString;
begin
  if (csDesigning in ComponentState) or not CommOpened then
    Exit;
  if Return then
    s := {$IFDEF UNICODE}AnsiString{$ENDIF}(Command) + #13
  else
    s := {$IFDEF UNICODE}AnsiString{$ENDIF}(Command);
  WriteCommData(PAnsiChar(s), Length(s));
end;

// �ȴ�һ��AT����ִ�н��
function TCnModem.WaitATResult(Delay: Cardinal): string;
var
  Tick: Cardinal;
begin
  FWaitATResult := True;
  try
    FATResult := '';
    Tick := GetTickCount;
    while (GetTickCount - Tick < Delay) and (FATResult = '') do
      Application.HandleMessage;
    Result := FATResult;
    FATResult := '';
  finally
    FWaitATResult := False;
  end;
end;

// ����һ��AT��������Ƿ�ɹ�
function TCnModem.SendATOk(AT: string; Delay: Cardinal): Boolean;
var
  i, j: Integer;
  s: string;
begin
  Result := False;
  for i := 0 to 2 do
  begin
    WriteATCommand(AT);
    for j := 0 to 2 do
    begin
      s := Trim(UpperCase(WaitATResult(Delay)));
      if Pos('OK', s) > 0 then
      begin
        Result := True;
        Exit;
      end
      else if Pos('ERROR', s) > 0 then
      begin
        InvalidCommand(AT);
        Exit;
      end;
    end;
  end;
end;

// ���յ�����
procedure TCnModem.ReceiveData(Buffer: PAnsiChar; BufferLength: WORD);
var
  s: AnsiString;
begin
  if FWaitATResult then       // ���ڵȴ�AT����ִ�н��
  begin
    FATResult := {$IFDEF UNICODE}String{$ENDIF}(Buffer);
    Exit;
  end;
  s := Buffer;
  s := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim(UpperCase({$IFDEF UNICODE}String{$ENDIF}(s))));
  if (ModemState in [msOffline, msOnlineCommand, msConnecting]) and (s = 'RING') then
    Ring                      // �����ź�
  else if (ModemState = msOnline) and (s = 'NO CARRIER') then
    DisConnect                // �ز���ʧ
  else
    inherited;
end;

// ����
function TCnModem.Dial(const Number: string): TDialResult;
var
  s: string;
begin
  if not OpenComm then
  begin
    Result := drOpenCommFail;
    Exit;
  end;
  Result := drNoModem;
  if InitModem then
  begin
    WriteATCommand('ATD' + Number);
    ModemState := msConnecting;
    s := Trim(UpperCase(WaitATResult(Round(WaitCarrierTime * 1000 * 1.2))));
    if Pos('CONNECT', s) > 0 then
    begin
      Result := drConnect;
      FConnectRate := StrToIntEx(s);
      ModemState := msOnline;
      Exit;
    end;
    if Pos('NO DIALTONE', s) > 0 then
      Result := drNoDialtone
    else if Pos('BUSY', s) > 0 then
      Result := drBusy
    else if Pos('NO CARRIER', s) > 0 then
      Result := drNoCarrier
    else if Pos('NO ANSWER', s) > 0 then
      Result := drNoAnswer
    else if s = '' then
      Result := drTimeout
    else
      Result := drUnknow;
    ModemState := msOffline;
  end;
end;

// Ӧ��
function TCnModem.Answer: TDialResult;
var
  s: string;
begin
  Result := drUnknow;
  if CommOpened and (ModemState = msOffline) then
  begin
    WriteATCommand('ATA');
    ModemState := msConnecting;
    s := Trim(UpperCase(WaitATResult(Round(WaitCarrierTime * 1000 * 1.2))));
    if Pos('CONNECT', s) > 0 then
    begin
      FConnectRate := StrToIntEx(s);
      ModemState := msOnline;
      Connect(FConnectRate);
      Result := drConnect;
      Exit;
    end;
    if Pos('NO DIALTONE', s) > 0 then
      Result := drNoDialtone
    else if Pos('BUSY', s) > 0 then
      Result := drBusy
    else if Pos('NO CARRIER', s) > 0 then
      Result := drNoCarrier
    else if Pos('NO ANSWER', s) > 0 then
      Result := drNoAnswer
    else if s = '' then
      Result := drTimeout
    else
      Result := drUnknow;
    ModemState := msOffline;
  end;
end;

// �л�����������״̬
procedure TCnModem.Escape;
var
  Tick: Integer;
begin
  if CommOpened and (ModemState = msOnline) then
  begin
    Tick := Round(FWaitEscapeTime * 20 * 1.3);
    Sleep(Tick);
    WriteATCommand('+++', False);
    Sleep(Tick);
    ModemState := msOnlineCommand;
  end;
end;

// �ص�����״̬
procedure TCnModem.Resume;
begin
  if CommOpened and (ModemState = msOnlineCommand) then
  begin
    if SendATOk('ATO') then
      ModemState := msOnline
    else
      Hangup;
  end;
end;

// �һ�
procedure TCnModem.Hangup;
begin
  if CommOpened then
  begin
    Escape;
    WriteATCommand('ATH');
    Sleep(1000);
    ModemState := msOffline;
    StopComm;
  end;
end;

// ��ʼ�� Modem
function TCnModem.InitModem: Boolean;
const
  AutoAnswers: array[Boolean] of Integer = (0, 1);
  Checks: array[Boolean, Boolean] of Integer = ((0, 2), (3, 4));
begin
  Result := False;
  if not OpenComm then
    Exit;
  if ModemState <> msOffline then
    Hangup;
  if not SendATOk('ATQ0E0V1') then Exit; //������ԣ����ַ���ʽ��ʾ�����
  if not SendATOk('ATX' + IntToStr(Checks[CheckDialtone, CheckBusy])) then Exit;
  if not SendATOk('ATL' + IntToStr(Ord(FVolume))) then Exit;
  if not SendATOk('ATS0=' + IntToStr(AutoAnswers[AutoAnswer])) then Exit;
  if not SendATOk('ATS6=' + IntToStr(WaitDialtoneTime)) then Exit;
  if not SendATOk('ATS7=' + IntToStr(WaitCarrierTime)) then Exit;
  if not SendATOk('ATS12=' + IntToStr(WaitEscapeTime)) then Exit;
  Result := True;
  if InitATCommand <> '' then
    SendATOk(InitATCommand);
end;

// �Ƿ�AT����
procedure TCnModem.InvalidCommand(const Command: string);
begin
  if Assigned(FOnInvalidCommand) then
    FOnInvalidCommand(Self, Command);
end;

// ������
procedure TCnModem.Connect(Rate: Integer);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self, Rate);
end;

// �����ж�
procedure TCnModem.DisConnect;
begin
  if Assigned(FOnDisConnect) then
    FOnDisConnect(Self);
end;

// �����¼�
procedure TCnModem.Ring;
var
  Ans: Boolean;
begin
  Ans := True;
  if Assigned(FOnRing) then
    FOnRing(Self, Ans);
  if not AutoAnswer and Ans then
    Answer;
end;

// ����Modem״̬
procedure TCnModem.SetModemState(const Value: TModemState);
begin
  if FModemState <> Value then
  begin
    FModemState := Value;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, FModemState);
  end;
end;

// �����Զ�Ӧ��
procedure TCnModem.SetAutoAnswer(const Value: Boolean);
begin
  if FAutoAnswer <> Value then
  begin
    FAutoAnswer := Value;
    Changed;
  end;
end;

// ��������
procedure TCnModem.SetVolume(const Value: TModemVolume);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    Changed;
  end;
end;

// ���ó�ʼ��AT����
procedure TCnModem.SetInitATCommand(const Value: string);
begin
  if FInitATCommand <> Value then
  begin
    FInitATCommand := UpperCase(Trim(Value));
    if Pos('AT', FInitATCommand) <> 1 then
      FInitATCommand := 'AT' + FInitATCommand;
    if FInitATCommand = 'AT' then
      FInitATCommand := '';
    Changed;
  end;
end;

// ���õȴ��ز�ʱ��
procedure TCnModem.SetWaitCarrierTime(const Value: Integer);
begin
  if FWaitCarrierTime <> Value then
  begin
    FWaitCarrierTime := Value;
    Changed;
  end;
end;

// ���õȴ�������ʱ��
procedure TCnModem.SetWaitDialtoneTime(const Value: Integer);
begin
  if FWaitDialtoneTime <> Value then
  begin
    FWaitDialtoneTime := Value;
    Changed;
  end;
end;

// �����л�����������״̬�ĵȴ�ʱ��
procedure TCnModem.SetWaitEscapeTime(const Value: Integer);
begin
  if FWaitEscapeTime <> Value then
  begin
    FWaitEscapeTime := Value;
    Changed;
  end;
end;

// ���ü��æ��
procedure TCnModem.SetCheckBusy(const Value: Boolean);
begin
  if FCheckBusy <> Value then
  begin
    FCheckBusy := Value;
    Changed;
  end;
end;

// ���ü�Ⲧ����
procedure TCnModem.SetCheckDialtone(const Value: Boolean);
begin
  if FCheckDialtone <> Value then
  begin
    FCheckDialtone := Value;
    Changed;
  end;
end;

const
  csCheckDialtone = 'CheckDialtone';
  csCheckBusy = 'CheckBusy';
  csAutoAnswer = 'AutoAnswer';
  csWaitEscapeTime = 'WaitEscapeTime';
  csWaitDialtoneTime = 'WaitDialtoneTime';
  csWaitCarrierTime = 'WaitCarrierTime';
  csInitATCommand = 'InitATCommand';

// ��INI�ж�����
procedure TCnModem.ReadFromIni(Ini: TCustomIniFile;
  const Section: string);
begin
  inherited;
  FCheckDialtone := Ini.ReadBool(Section, csCheckDialtone, FCheckDialtone);
  FCheckBusy := Ini.ReadBool(Section, csCheckBusy, FCheckBusy);
  FAutoAnswer := Ini.ReadBool(Section, csAutoAnswer, FAutoAnswer);
  FWaitEscapeTime := Ini.ReadInteger(Section, csWaitEscapeTime, FWaitEscapeTime);
  FWaitDialtoneTime := Ini.ReadInteger(Section, csWaitDialtoneTime, FWaitDialtoneTime);
  FWaitCarrierTime := Ini.ReadInteger(Section, csWaitCarrierTime, FWaitCarrierTime);
  FInitATCommand := Ini.ReadString(Section, csInitATCommand, FInitATCommand);
  FInitATCommand := UpperCase(Trim(FInitATCommand));
  if Pos('AT', FInitATCommand) <> 1 then
    FInitATCommand := 'AT' + FInitATCommand;
  if FInitATCommand = 'AT' then
    FInitATCommand := '';
end;

// д������INI
procedure TCnModem.WriteToIni(Ini: TCustomIniFile; const Section: string);
begin
  inherited;
  Ini.WriteBool(Section, csCheckDialtone, FCheckDialtone);
  Ini.WriteBool(Section, csCheckBusy, FCheckBusy);
  Ini.WriteBool(Section, csAutoAnswer, FAutoAnswer);
  Ini.WriteInteger(Section, csWaitEscapeTime, FWaitEscapeTime);
  Ini.WriteInteger(Section, csWaitDialtoneTime, FWaitDialtoneTime);
  Ini.WriteInteger(Section, csWaitCarrierTime, FWaitCarrierTime);
  Ini.WriteString(Section, csInitATCommand, FInitATCommand);
end;

// ȡ���ע��
procedure TCnModem.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnModemName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnModemComment;
end;

end.

