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

unit CnTimer;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ��߾��ȶ�ʱ�����TCnTimer��Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��- Delphi�Դ���TTimerʹ�ò���ϵͳ����Ϣ��ʽ�ṩ�Ķ�ʱ������Win9X��
*             ��ʱ���Ƚ�Ϊ55ms��NT��Լ10ms��
*           - TCnTimerʹ�ö�ý�嶨ʱ�����ж�ʱ���ƣ����Ƚϸߡ���ʹ�÷�ʽ��TTimer
*             ��ȫ���ݣ����ṩ�˸���Ĺ��ܡ�
*           - TCnTimerList��ʱ���б����ͬʱ���������ʱ����
*           - ���ж�ʱ��ʹ��ͬһ���ڲ���ʱ�����ʺϴ���ʹ�õĳ��ϡ�
*           - ����Win32����ռʽ���������ϵͳ�������߳���������CPUʱ��Ƭ�����
*             �������߳�ռ�ô���CPUʱ�䣬��ʹ������߾��ȣ�Ҳ��һ���ܱ�֤��ȷ
*             �Ķ�ʱ�����
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.12.22 V2.2
*               ����ͬ���¼����� SyncEvent ��Ĭ��Ϊ�棬ʹ��ʱ���¼����������߳���ִ��
*           2006.12.28 V2.1
*               ȥ����ʱ�̣߳����ö�ý�嶨ʱ�����Լ�����Դռ�ò������ DLL �в���
*               ʹ�õ�����
*           2002.11.05 V2.0
*               ��дȫ�����룬���Ӷ�ʱ���б����ж�ʱ��ʹ��ͬһ�̶߳�ʱ
*           2002.04.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Forms, MMSystem, CnClasses, CnConsts, CnCompConsts,
  CnNativeDecl;

type

//==============================================================================
// �߾��ȶ�ʱ������
//==============================================================================

{ TCnTimerObject }

  TCnTimerObject = class(TObject)
  private
    FActualFPS: Double;
    FEnabled: Boolean;
    FExecCount: Cardinal;
    FInterval: Cardinal;
    FLastTickCount: Cardinal;
    FOnTimer: TNotifyEvent;
    FRepeatCount: Cardinal;
    FSyncEvent: Boolean;
    function GetFPS: Double;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetRepeatCount(Value: Cardinal);
  protected
    procedure Timer; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    property ActualFPS: Double read FActualFPS;
    property ExecCount: Cardinal read FExecCount;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FPS: Double read GetFPS write SetFPS stored False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property RepeatCount: Cardinal read FRepeatCount write SetRepeatCount default 0;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
  end;

//==============================================================================
// �߾��ȶ�ʱ�����
//==============================================================================

{ TCnTimer }

  TCnTimer = class(TCnComponent)
  {* �߾��ȶ�ʱ�������ʹ�÷������� TTimer��}
  private
    FTimerObject: TCnTimerObject;
    function GetActualFPS: Double;
    function GetEnabled: Boolean;
    function GetExecCount: Cardinal;
    function GetFPS: Double;
    function GetInterval: Cardinal;
    function GetOnTimer: TNotifyEvent;
    function GetRepeatCount: Cardinal;
    function GetSyncEvent: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetRepeatCount(Value: Cardinal);
    procedure SetSyncEvent(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    property ActualFPS: Double read GetActualFPS;
    {* ʵ�ʵĶ�ʱ�����ʣ���ÿ��}
    property ExecCount: Cardinal read GetExecCount;
    {* �Ѿ�ִ�й��Ĵ���}
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    {* ��ʱ���Ƿ�����}
    property FPS: Double read GetFPS write SetFPS stored False;
    {* ��ʱ���ٶȣ���ÿ��}
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    {* ��ʱ���������}
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    {* ��ʱ�¼�}
    property RepeatCount: Cardinal read GetRepeatCount write SetRepeatCount default 0;
    {* ��ʱ�¼�����������ʱ�¼�����ָ���������Զ��رա����Ϊ 0 ��ʾ������}
    property SyncEvent: Boolean read GetSyncEvent write SetSyncEvent default True;
    {* ��ʱ�¼��Ƿ���ͬ����ʽ�����߳��в��������Ϊ False ��ʱ�¼��ڶ�ý�嶨ʱ���߳��е��á�
       ��ʱ�¼�������漰�� VCL ���������Ӧ��Ϊ True��}
  end;

//==============================================================================
// �߾��ȶ�ʱ���б�������
//==============================================================================

{ TCnTimerItem }

  TCnTimerItem = class(TCollectionItem)
  {* �߾��ȶ�ʱ���б����ʹ�÷������� TTimer��}
  private
    FOnTimer: TNotifyEvent;
    FTimerObject: TCnTimerObject;
    function GetActualFPS: Double;
    function GetEnabled: Boolean;
    function GetExecCount: Cardinal;
    function GetFPS: Double;
    function GetInterval: Cardinal;
    function GetRepeatCount: Cardinal;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetRepeatCount(Value: Cardinal);
    function GetSyncEvent: Boolean;
    procedure SetSyncEvent(const Value: Boolean);
  protected
    procedure Timer(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ����}
    property ActualFPS: Double read GetActualFPS;
    {* ʵ�ʵĶ�ʱ�����ʣ���ÿ��}
    property ExecCount: Cardinal read GetExecCount;
    {* �Ѿ�ִ�й��Ĵ���}
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    {* ��ʱ���Ƿ�����}
    property FPS: Double read GetFPS write SetFPS stored False;
    {* ��ʱ���ٶȣ���ÿ��}
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    {* ��ʱ���������}
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    {* ��ʱ�¼�}
    property RepeatCount: Cardinal read GetRepeatCount write SetRepeatCount default 0;
    {* ��ʱ�¼�����������ʱ�¼�����ָ���������Զ��رա����Ϊ 0 ��ʾ������}
    property SyncEvent: Boolean read GetSyncEvent write SetSyncEvent default True;
    {* ��ʱ�¼��Ƿ���ͬ����ʽ�����߳��в��������Ϊ False ��ʱ�¼��ڶ�ý�嶨ʱ���߳��е��á�
       ��ʱ�¼�������漰�� VCL ���������Ӧ��Ϊ True��}
  end;

//==============================================================================
// �߾��ȶ�ʱ���б�����
//==============================================================================

{ TCnTimerCollection }

  TCnTimerList = class;

  TCnTimerCollection = class(TOwnedCollection)
  {* �߾��ȶ�ʱ���б���}
  private
    FTimerList: TCnTimerList;
    function GetItems(Index: Integer): TCnTimerItem;
    procedure SetItems(Index: Integer; Value: TCnTimerItem);
  protected
    property TimerList: TCnTimerList read FTimerList;
  public
    constructor Create(AOwner: TPersistent);
    {* �๹����}
    property Items[Index: Integer]: TCnTimerItem read GetItems write SetItems; default;
    {* ��ʱ����������}
  end;

//==============================================================================
// �߾��ȶ�ʱ���б����
//==============================================================================

{ TCnTimerList }

  TCnTimerEvent = procedure(Sender: TObject; Index: Integer; var Handled:
    Boolean) of object;
  {* �߾��ȶ�ʱ���б��¼���Index Ϊ�����¼��Ķ�ʱ��������ţ�Handle �����Ƿ��Ѵ���
     ������¼��н� Handle ��Ϊ true�����������ö�ʱ�������¼�}
    
  TCnTimerList = class(TCnComponent)
  {* �߾��ȶ�ʱ���б���������Զ�������ʱ����}
  private
    FItems: TCnTimerCollection;
    FOnTimer: TCnTimerEvent;
    procedure SetItems(Value: TCnTimerCollection);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
    function Timer(Index: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
  published
    property Items: TCnTimerCollection read FItems write SetItems;
    {* ��ʱ���б�}
    property OnTimer: TCnTimerEvent read FOnTimer write FOnTimer;
    {* ��ʱ���¼�}
  end;

implementation

uses
  Messages;

const
  UM_CNTIMER = WM_USER + 101;

type

//==============================================================================
// �߾��ȶ�ʱ����������˽���ࣩ
//==============================================================================

{ TCnTimerMgr }

  TCnTimerMgr = class(TObject)
  private
    FTimerList: TThreadList;
    FTimerRes: Integer;
    FTimerID: Integer;
    FHwnd: HWND;
    function InitMMTimer: Boolean;
    procedure FreeMMTimer;
  protected
    procedure ClearTimer;
    procedure DoTimer(Sync: Boolean);
    procedure Timer; virtual;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function AddTimer: TCnTimerObject;
    procedure DeleteTimer(TimerObject: TCnTimerObject);
  end;

//==============================================================================
// �߾��ȶ�ʱ����������˽���ࣩ
//==============================================================================

{ TCnTimerMgr }

constructor TCnTimerMgr.Create;
begin
  inherited Create;
  FTimerList := TThreadList.Create;
  FHwnd := AllocateHWnd(WndProc);
  InitMMTimer;
end;

destructor TCnTimerMgr.Destroy;
begin
  DeallocateHWnd(FHwnd);
  FreeMMTimer;
  ClearTimer;
  FreeAndNil(FTimerList);
  inherited Destroy;
end;

procedure MMTimerProc(uTimerID, uMessage: UINT; dwUser, dw1, dw2: TCnNativePointer) stdcall;
begin
  TCnTimerMgr(dwUser).Timer;
end;  

function TCnTimerMgr.InitMMTimer: Boolean;
var
  tc: TIMECAPS;
begin
  Result := False;
  if timeGetDevCaps(@tc, SizeOf(TIMECAPS)) = TIMERR_NOERROR then
  begin
    FTimerRes := tc.wPeriodMin;
    if timeBeginPeriod(FTimerRes) = TIMERR_NOERROR then
    begin
      FTimerID := timeSetEvent(tc.wPeriodMin, 0, MMTimerProc, Cardinal(Self),
        TIME_PERIODIC);
      Result := FTimerID <> 0;
    end
    else
      FTimerRes := 0;
  end;
end;

procedure TCnTimerMgr.FreeMMTimer;
begin
  if FTimerID <> 0 then
  begin
    timeKillEvent(FTimerID);
  end;
  
  if FTimerRes <> 0 then
  begin
    timeEndPeriod(FTimerRes);
  end;   
end;

function TCnTimerMgr.AddTimer: TCnTimerObject;
begin
  Result := TCnTimerObject.Create;
  with FTimerList.LockList do
  try
    Add(Result);
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.ClearTimer;
var
  i: Integer;
begin
  with FTimerList.LockList do
  try
    for i := Count - 1 downto 0 do
    begin
      TCnTimerObject(Items[i]).Free;
      Delete(i);
    end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.DeleteTimer(TimerObject: TCnTimerObject);
var
  i: Integer;
begin
  with FTimerList.LockList do
  try
    for i := 0 to Count - 1 do
      if Items[i] = TimerObject then
      begin
        TimerObject.Free;
        Delete(i);
        Exit;
      end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.DoTimer(Sync: Boolean);
var
  i: Integer;
  CurrTick: Cardinal;
begin
  with FTimerList.LockList do
  try
    CurrTick := timeGetTime;
    for i := 0 to Count - 1 do
      with TCnTimerObject(Items[i]) do
        if Enabled and (FSyncEvent = Sync) and(Interval <> 0) and
          (CurrTick - FLastTickCount >= Interval) and Assigned(FOnTimer) then
        begin
          if CurrTick <> FLastTickCount then
            FActualFPS := 1000 / (CurrTick - FLastTickCount)
          else
            FActualFPS := 0;
          FLastTickCount := CurrTick;
          try
            Timer;
          except
            Application.HandleException(Self);
          end;
        end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.Timer;
begin
  DoTimer(False);
  PostMessage(FHwnd, UM_CNTIMER, 0, 0);
end;

procedure TCnTimerMgr.WndProc(var Message: TMessage);
begin
  if Message.Msg = UM_CNTIMER then
  begin
    DoTimer(True);
  end;  
end;

var
  TimerMgr: TCnTimerMgr;

function GetTimerMgr: TCnTimerMgr;
begin
  if TimerMgr = nil then
    TimerMgr := TCnTimerMgr.Create;
  Result := TimerMgr;
end;

//==============================================================================
// �߾��ȶ�ʱ������
//==============================================================================

{ TCnTimerObject }

constructor TCnTimerObject.Create;
begin
  inherited Create;
  FEnabled := True;
  FExecCount := 0;
  FInterval := 1000;
  FLastTickCount := timeGetTime;
  FRepeatCount := 0;
  FSyncEvent := True;
end;

destructor TCnTimerObject.Destroy;
begin

  inherited;
end;

function TCnTimerObject.GetFPS: Double;
begin
  if Interval = 0 then
    Result := 0
  else
    Result := 1000 / Interval;
end;

procedure TCnTimerObject.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FExecCount := 0;
    if FEnabled then
    begin
      FLastTickCount := timeGetTime;
    end;
  end;
end;

procedure TCnTimerObject.SetFPS(Value: Double);
begin
  if Value < 0 then
    Exit
  else if Value < 1 / High(Word) then
    Value := 1 / High(Word)
  else if Value > 1000 then
    Value := 1000;
  FInterval := Round(1000 / Value);
end;

procedure TCnTimerObject.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    FLastTickCount := timeGetTime;
  end;
end;

procedure TCnTimerObject.SetRepeatCount(Value: Cardinal);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
  end;
end;

procedure TCnTimerObject.Timer;
begin
  Inc(FExecCount);
  if Assigned(FOnTimer) then FOnTimer(Self);
  if (RepeatCount <> 0) and (FExecCount >= RepeatCount) then
  begin
    Enabled := False;
  end;
end;

//==============================================================================
// �߾��ȶ�ʱ�����
//==============================================================================

{ TCnTimer }

constructor TCnTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimerObject := GetTimerMgr.AddTimer;
end;

destructor TCnTimer.Destroy;
begin
  if TimerMgr <> nil then
    TimerMgr.DeleteTimer(FTimerObject);
  inherited Destroy;
end;

function TCnTimer.GetActualFPS: Double;
begin
  Result := FTimerObject.ActualFPS;
end;

procedure TCnTimer.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnTimerName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnTimerComment;
end;

function TCnTimer.GetEnabled: Boolean;
begin
  Result := FTimerObject.Enabled;
end;

function TCnTimer.GetExecCount: Cardinal;
begin
  Result := FTimerObject.ExecCount;
end;

function TCnTimer.GetFPS: Double;
begin
  Result := FTimerObject.FPS;
end;

function TCnTimer.GetInterval: Cardinal;
begin
  Result := FTimerObject.Interval;
end;

function TCnTimer.GetOnTimer: TNotifyEvent;
begin
  Result := FTimerObject.OnTimer;
end;

function TCnTimer.GetRepeatCount: Cardinal;
begin
  Result := FTimerObject.RepeatCount;
end;

function TCnTimer.GetSyncEvent: Boolean;
begin
  Result := FTimerObject.SyncEvent;
end;

procedure TCnTimer.SetEnabled(Value: Boolean);
begin
  FTimerObject.Enabled := Value;
end;

procedure TCnTimer.SetFPS(Value: Double);
begin
  FTimerObject.FPS := Value;
end;

procedure TCnTimer.SetInterval(Value: Cardinal);
begin
  FTimerObject.Interval := Value;
end;

procedure TCnTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FTimerObject.OnTimer := Value;
end;

procedure TCnTimer.SetRepeatCount(Value: Cardinal);
begin
  FTimerObject.RepeatCount := Value;
end;

procedure TCnTimer.SetSyncEvent(const Value: Boolean);
begin
  FTimerObject.SyncEvent := Value;
end;

//==============================================================================
// �߾��ȶ�ʱ���б�������
//==============================================================================

{ TCnTimerItem }

constructor TCnTimerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTimerObject := GetTimerMgr.AddTimer;
  FTimerObject.OnTimer := Timer;
end;

destructor TCnTimerItem.Destroy;
begin
  if TimerMgr <> nil then
    TimerMgr.DeleteTimer(FTimerObject);
  inherited Destroy;
end;

procedure TCnTimerItem.Assign(Source: TPersistent);
begin
  if Source is TCnTimerItem then
  begin
    Enabled := TCnTimerItem(Source).Enabled;
    Interval := TCnTimerItem(Source).Interval;
    RepeatCount := TCnTimerItem(Source).RepeatCount;
  end
  else
    inherited;
end;

function TCnTimerItem.GetActualFPS: Double;
begin
  Result := FTimerObject.ActualFPS;
end;

function TCnTimerItem.GetEnabled: Boolean;
begin
  Result := FTimerObject.Enabled;
end;

function TCnTimerItem.GetExecCount: Cardinal;
begin
  Result := FTimerObject.ExecCount;
end;

function TCnTimerItem.GetFPS: Double;
begin
  Result := FTimerObject.FPS;
end;

function TCnTimerItem.GetInterval: Cardinal;
begin
  Result := FTimerObject.Interval;
end;

function TCnTimerItem.GetRepeatCount: Cardinal;
begin
  Result := FTimerObject.RepeatCount;
end;

function TCnTimerItem.GetSyncEvent: Boolean;
begin
  Result := FTimerObject.SyncEvent;
end;

procedure TCnTimerItem.SetEnabled(Value: Boolean);
begin
  FTimerObject.Enabled := Value;
end;

procedure TCnTimerItem.SetFPS(Value: Double);
begin
  FTimerObject.FPS := Value;
end;

procedure TCnTimerItem.SetInterval(Value: Cardinal);
begin
  FTimerObject.Interval := Value;
end;

procedure TCnTimerItem.SetRepeatCount(Value: Cardinal);
begin
  FTimerObject.RepeatCount := Value;
end;

procedure TCnTimerItem.SetSyncEvent(const Value: Boolean);
begin
  FTimerObject.SyncEvent := Value;
end;

procedure TCnTimerItem.Timer(Sender: TObject);
begin
  if not TCnTimerList(TCnTimerCollection(Collection).GetOwner).Timer(Index) then
    if Assigned(FOnTimer) then
      FOnTimer(Self);
end;

//==============================================================================
// �߾��ȶ�ʱ���б�����
//==============================================================================

{ TCnTimerCollection }

constructor TCnTimerCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnTimerItem);
  Assert(AOwner is TCnTimerList);
end;

function TCnTimerCollection.GetItems(Index: Integer): TCnTimerItem;
begin
  Result := TCnTimerItem(inherited Items[Index]);
end;

procedure TCnTimerCollection.SetItems(Index: Integer; Value: TCnTimerItem);
begin
  inherited Items[Index] := Value;
end;

//==============================================================================
// �߾��ȶ�ʱ���б����
//==============================================================================

{ TCnTimerList }

constructor TCnTimerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TCnTimerCollection.Create(Self);
end;

destructor TCnTimerList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TCnTimerList.GetComponentInfo(var AName, Author, Email, Comment:
  string);
begin
  AName := SCnTimerListName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnTimerListComment;
end;

procedure TCnTimerList.SetItems(Value: TCnTimerCollection);
begin
  FItems.Assign(Value);
end;

function TCnTimerList.Timer(Index: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnTimer) then
    FOnTimer(Self, Index, Result);
end;

initialization

finalization
  if TimerMgr <> nil then
    FreeAndNil(TimerMgr);

end.

