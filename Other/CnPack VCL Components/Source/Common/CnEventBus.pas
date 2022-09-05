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

unit CnEventBus;
{* |<PRE>
================================================================================
* ������ƣ�CnPack
* ��Ԫ���ƣ�CnEventBus ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�Liu Xiao
* ��    ע���õ�ԪΪ CnEventBus ��ʵ�ֵ�Ԫ��ģ��һ���򵥵� EventBus��ʵ�ֵ���϶�
*           ��֪ͨ��ע���Լ�֪ͨ���ͣ������߳̿��ƵȻ��ơ�
*           �¼����ַ������͵��������֡�
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2015.09.24 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Contnrs, CnHashMap;

type
  ECnEventBusException = class(Exception);

  TCnEvent = class(TObject)
  {* �¼���ʵ����}
  private
    FEventName: string;
    FEventData: Pointer;
    FEventTag: Pointer;
    constructor Create(const AEventName: string; AEventData: Pointer = nil;
      AEventTag: Pointer = nil);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TCnEvent); virtual;

    class function ObtainEvent(const AEventName: string; AEventData: Pointer = nil;
      AEventTag: Pointer = nil): TCnEvent;
    class procedure RecycleEvent(AEvent: TCnEvent);

    function GetEventName: string;
    procedure SetEventName(const AEventName: string);
    function GetEventData: Pointer;
    procedure SetEventData(AEventData: Pointer);
    function GetEventTag: Pointer;
    procedure SetEventTag(AEventTag: Pointer);

    property EventName: string read GetEventName write SetEventName;
    {* �¼�����}
    property EventData: Pointer read GetEventData write SetEventData;
    {* �¼�Я��������}
    property EventTag: Pointer read GetEventTag write SetEventTag;
    {* �¼�Я���ı�ǩ}
  end;

  ICnEventBusReceiver = interface
  {* ֪ͨ�������ӿ�}
  ['{F03E825C-FD29-4AD8-B48E-D3BBF1DDE045}']
    procedure OnEvent(Event: TCnEvent);
    {* �����¼�֪ͨʱ����}
  end;

  TCnEventBus = class(TObject)
  {* EventBus ֪ͨע����ַ���ʵ����}
  private
    FReceivers: TCnStrToPtrHashMap;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    procedure FreeReceiverSlots;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterReceiver(Receiver: ICnEventBusReceiver); overload;
    {* ע��һ��֪ͨ�����������������¼�֪ͨ}
    procedure RegisterReceiver(Receiver: ICnEventBusReceiver; EventName: string); overload;
    {* ע��һ��֪ͨ������������һ�ض����ַ����¼���֪ͨ����֧��ͨ���}
    procedure RegisterReceiver(Receiver: ICnEventBusReceiver; EventNames: array of const); overload;
    {* ע��һ��֪ͨ������������ƥ���ض�һ���ַ����¼�����֪ͨ����֧��ͨ���}

    procedure UnRegisterReceiver(Receiver: ICnEventBusReceiver);
    {* ȡ��ע��һ��֪ͨ������}

    procedure PostEvent(Event: TCnEvent); overload;
    {* ����һ���¼�������Ϊ�¼�ʵ��}
    procedure PostEvent(const EventName: string); overload;
    {* ����һ���¼�������Ϊ�¼������¼�ʵ�������ڲ�������֪ͨ��������Զ��ͷ�}
    procedure PostEvent(const EventName: string; EventData: Pointer); overload;
    {* ����һ���¼�������Ϊ�¼��������ݣ��¼�ʵ�������ڲ�������֪ͨ��������Զ��ͷ�}
    procedure PostEvent(const EventName: string; EventData: Pointer; EventTag: Pointer); overload;
    {* ����һ���¼�������Ϊ�¼������������ǩ���¼�ʵ�������ڲ�������֪ͨ��������Զ��ͷ�}
  end;

function EventBus: TCnEventBus;
{* ���ȫ�� EventBus ����}

implementation

const
  ANY_EVENT = '*';

type
  NoRef = Pointer;

var
  FEventBus: TCnEventBus = nil;
  FEventPool: TObjectList = nil;
  FEventPoolMutex: TRTLCriticalSection;

function EventBus: TCnEventBus;
begin
  if FEventBus = nil then
    FEventBus := TCnEventBus.Create;
  Result := FEventBus;
end;

{ TCnEvent }

procedure TCnEvent.Assign(Source: TCnEvent);
begin
  if Source <> nil then
  begin
    FEventName := Source.EventName;
    FEventData := Source.EventData;
    FEventTag := Source.EventTag;
  end;
end;

procedure TCnEvent.Clear;
begin
  FEventTag := nil;
  FEventData := nil;
  FEventName := '';
end;

constructor TCnEvent.Create(const AEventName: string; AEventData: Pointer;
  AEventTag: Pointer);
begin
  inherited Create;
  FEventName := AEventName;
  FEventData := AEventData;
  FEventTag := AEventTag;
end;

destructor TCnEvent.Destroy;
begin

  inherited;
end;

function TCnEvent.GetEventData: Pointer;
begin
  Result := FEventData;
end;

function TCnEvent.GetEventName: string;
begin
  Result := FEventName;
end;

function TCnEvent.GetEventTag: Pointer;
begin
  Result := FEventTag;
end;

class function TCnEvent.ObtainEvent(const AEventName: string; AEventData,
  AEventTag: Pointer): TCnEvent;
begin
  EnterCriticalSection(FEventPoolMutex);
  if FEventPool.Count = 0 then
  begin
    Result := TCnEvent.Create(AEventName, AEventData, AEventTag);
  end
  else
  begin
    Result := TCnEvent(FEventPool[FEventPool.Count - 1]);
    FEventPool.Extract(Result);

    Result.EventName := AEventName;
    Result.EventData := AEventData;
    Result.EventTag := AEventTag;
  end;

  LeaveCriticalSection(FEventPoolMutex);
end;

class procedure TCnEvent.RecycleEvent(AEvent: TCnEvent);
begin
  AEvent.Clear;
  EnterCriticalSection(FEventPoolMutex);
  FEventPool.Add(AEvent);
  LeaveCriticalSection(FEventPoolMutex);
end;

procedure TCnEvent.SetEventData(AEventData: Pointer);
begin
  FEventData := AEventData;
end;

procedure TCnEvent.SetEventName(const AEventName: string);
begin
  FEventName := AEventName;
end;

procedure TCnEvent.SetEventTag(AEventTag: Pointer);
begin
  FEventTag := AEventTag;
end;

{ TCnEventBus }

constructor TCnEventBus.Create;
begin
  inherited;
  FReceivers := TCnStrToPtrHashMap.Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TCnEventBus.Destroy;
begin
  FreeReceiverSlots;
  FReceivers.Free;
  FSynchronizer.Free;
  inherited;
end;

procedure TCnEventBus.PostEvent(Event: TCnEvent);
var
  I: Integer;
  List: Pointer;
begin
  if Event = nil then
    Exit;

  if FReceivers.Find(ANY_EVENT, List) then
  begin
    for I := 0 to TList(List).Count - 1 do
    try
      ICnEventBusReceiver(TList(List)[I]).OnEvent(Event);
    except
      ;
    end;
  end;

  if (Event.EventName <> ANY_EVENT) and FReceivers.Find(Event.EventName, List) then
  begin
    for I := 0 to TList(List).Count - 1 do
    try
      ICnEventBusReceiver(TList(List)[I]).OnEvent(Event);
    except
      ;
    end;
  end;
end;

procedure TCnEventBus.FreeReceiverSlots;
var
  Key: string;
  List: Pointer;
begin
  FSynchronizer.BeginWrite;

  FReceivers.StartEnum;
  while FReceivers.GetNext(Key, List) do
    TList(List).Free;

  FSynchronizer.EndWrite;
end;

procedure TCnEventBus.PostEvent(const EventName: string; EventData,
  EventTag: Pointer);
var
  Event: TCnEvent;
begin
  Event := TCnEvent.ObtainEvent(EventName, EventData, EventTag);
  PostEvent(Event);
  TCnEvent.RecycleEvent(Event);
end;

procedure TCnEventBus.PostEvent(const EventName: string; EventData: Pointer);
var
  Event: TCnEvent;
begin
  Event := TCnEvent.ObtainEvent(EventName, EventData);
  PostEvent(Event);
  TCnEvent.RecycleEvent(Event);
end;

procedure TCnEventBus.PostEvent(const EventName: string);
var
  Event: TCnEvent;
begin
  Event := TCnEvent.ObtainEvent(EventName);
  PostEvent(Event);
  TCnEvent.RecycleEvent(Event);
end;

procedure TCnEventBus.RegisterReceiver(Receiver: ICnEventBusReceiver);
begin
  RegisterReceiver(Receiver, ANY_EVENT);
end;

procedure TCnEventBus.RegisterReceiver(Receiver: ICnEventBusReceiver;
  EventNames: array of const);
var
  I: Integer;
begin
  for I := Low(EventNames) to High(EventNames) do
  begin
    case EventNames[I].VType of
      vtString: RegisterReceiver(Receiver, string(EventNames[I].VString^));
      vtAnsiString: RegisterReceiver(Receiver, string(AnsiString(PAnsiChar(EventNames[I].VAnsiString))));
      vtWideString: RegisterReceiver(Receiver, string(WideString(PWideChar(EventNames[I].VWideString))));
{$IFDEF UNICODE}
      vtUnicodeString: RegisterReceiver(Receiver, string(PWideChar(EventNames[I].VUnicodeString)));
{$ENDIF}
    else
      raise ECnEventBusException.Create('Invalid Event Name. Must Be String.');
    end;
  end;
end;

procedure TCnEventBus.RegisterReceiver(Receiver: ICnEventBusReceiver;
  EventName: string);
var
  List: Pointer;
begin
  FSynchronizer.BeginWrite;

  if not FReceivers.Find(EventName, List) then
  begin
    List := TList.Create;
    FReceivers.Add(EventName, List);
  end;
  TList(List).Add(NoRef(Receiver));

  FSynchronizer.EndWrite;
end;

procedure TCnEventBus.UnRegisterReceiver(Receiver: ICnEventBusReceiver);
var
  I: Integer;
  Key: string;
  List: Pointer;
begin
  FSynchronizer.BeginWrite;

  FReceivers.StartEnum;
  while FReceivers.GetNext(Key, List) do
  begin
    for I := TList(List).Count - 1 downto 0 do
      if TList(List)[I] = NoRef(Receiver) then
        TList(List).Delete(I);
  end;

  FSynchronizer.EndWrite;
end;

initialization
  InitializeCriticalSection(FEventPoolMutex);
  FEventPool := TObjectList.Create(True);

finalization
  FEventPool.Free;
  DeleteCriticalSection(FEventPoolMutex);
  FEventBus.Free;

end.
