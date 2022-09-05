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

unit CnActionListHook;
{ |<PRE>
================================================================================
* ������ƣ�CnWizards IDE ר�ҹ��߰�
* ��Ԫ���ƣ�ActionList �ҽӷ���Ԫ
* ��Ԫ���ߣ���Х��Passion�� liuxiao@cnpack.org;
* ��    ע���õ�Ԫ����ʵ�ֶ� IDE �ڲ� ActionList �ĹҽӲ������û������ȹҽ�һ��
            ActionList�����ܶ����ڲ��� Action ���йҽӡ����ҽӹ������� Active Ϊ
            False ��ʱ�����йҽӵ� Action ���¼�������ʱ�ָ����� ActionList ��
            Action ���ͷŵ�ʱ����Զ�ȡ���ҽӡ�
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ���֧�ֱ��ػ�����ʽ
* �޸ļ�¼��2003.07.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, ActnList, Contnrs,
  CnConsts, CnClasses, CnCompConsts;

type

  TCnActionHookObj = class(TObject)
  {* ��������һ���ҽӵ� Action}
  private
    FAction: TAction;

    FNewOnExecute: TNotifyEvent;
    FNewOnUpdate: TNotifyEvent;
    FOldOnExecute: TNotifyEvent;
    FOldOnUpdate: TNotifyEvent;
    procedure SetAction(const Value: TAction);
    procedure SetNewOnExecute(const Value: TNotifyEvent);
    procedure SetNewOnUpdate(const Value: TNotifyEvent);
    procedure SetOldOnExecute(const Value: TNotifyEvent);
    procedure SetOldOnUpdate(const Value: TNotifyEvent);
  protected
    procedure HookAction;
    {* ���о���� Action �¼��滻����}
    procedure RestoreAction;
    {* �ָ� Action ��ԭ���¼�}
  public
    constructor Create(AAction: TAction; NewOnExecute, NewOnUpdate: TNotifyEvent);
    destructor Destroy; override;
    property Action: TAction read FAction write SetAction;
    property OldOnUpdate: TNotifyEvent read FOldOnUpdate write SetOldOnUpdate;
    property OldOnExecute: TNotifyEvent read FOldOnExecute write SetOldOnExecute;
    property NewOnUpdate: TNotifyEvent read FNewOnUpdate write SetNewOnUpdate;
    property NewOnExecute: TNotifyEvent read FNewOnExecute write SetNewOnExecute;
  end;

//==============================================================================
// ActionList �ҽӹ�����
//==============================================================================

{ TCnActionListHook }

  THookActionListEvent = procedure(Sender: TObject; ActionList: TActionList) of object;

  TCnActionListHook = class(TCnComponent)
  private
    FActionListList: TList;
    FHookItemList: TObjectList;
    FActive: Boolean;
    FOnAddActionList: THookActionListEvent;
    FOnRemoveActionList: THookActionListEvent;
    procedure SetActive(const Value: Boolean);
    function GetHookedActionList(Index: Integer): TActionList;
    function GetHookedActionListCount: Integer;
    function GetHookedAction(Index: Integer): TAction;
    function GetHookedActionCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetActionHookObj(AAction: TAction): TCnActionHookObj;

    procedure DoRemoveActionList(AActionList: TActionList);
    procedure DoAddActionList(AActionList: TActionList);
    procedure UpdateHookedActions;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsHooked(AActionList: TActionList): Boolean;
    {* �ж�һ ActionList �Ƿ� Hook}
    function IsActionHooked(AAction: TAction): Boolean;
    {* �ж�һ Action �Ƿ� Hook}
    procedure UnHookActionItems(ActionList: TActionList);
    {* ȡ��һ ActionList �е����� Action �� Hook}

    procedure HookActionList(AActionList: TActionList);
    {* ���û����ã��ҽ�һ�� ActionList}
    procedure UnHookActionList(AActionList: TActionList);
    {* ���û����ã�ȡ����һ�� ActionList �Ĺҽ�}

    function AddActionNotifier(Action: TAction; NewOnExecute, NewOnUpdate:
      TNotifyEvent): Boolean;
    {* ���û����ã��ҽ�һ Action �� OnExecute �� OnUpdate �¼�}

    procedure RemoveNotifiler(Action: TAction);
    {* ���û����ã�ȡ���ҽ�һ Action���ָ���ԭ�е� OnExecute �� OnUpdate �¼�}

{   function AddActionNotifier(const ActionName: String; NewOnExecute, NewOnUpdate:
      TNotifyEvent): Boolean; overload;
    procedure RemoveNotifiler(const ActionName: String); overload;  }

    property Active: Boolean read FActive write SetActive;
    {* ���Ʊ��ҽӹ������Ƿ���Ч }
    property HookedActionListCount: Integer read GetHookedActionListCount;
    {* ���ر��ҽӵ� ActionList ��Ŀ }
    property HookedActionLists[Index: Integer]: TActionList
      read GetHookedActionList;
    {* ���ر��ҽӵ� ActionList }
    property HookedActionCount: Integer read GetHookedActionCount;
    {* ���ر��ҽӵ� Action ��Ŀ }
    property HookedActions[Index: Integer]: TAction read GetHookedAction;
    {* ���ر��ҽӵ� Action }
    property OnRemoveActionList: THookActionListEvent
      read FOnRemoveActionList write FOnRemoveActionList;
    property OnAddActionList: THookActionListEvent
      read FOnAddActionList write FOnAddActionList;
  end;

implementation

//==============================================================================
// ActionList �ҽӹ�����
//==============================================================================

{ TCnActionListHook }

function TCnActionListHook.AddActionNotifier(Action: TAction; NewOnExecute,
  NewOnUpdate: TNotifyEvent): Boolean;
var
  HookObj: TCnActionHookObj;
begin
  Result := False;
  if (Action <> nil) and (FHookItemList.IndexOf(Action) < 0) then
  begin
    if IsHooked(TActionList(Action.ActionList)) and not IsActionHooked(Action) then
    begin
      HookObj := TCnActionHookObj.Create(Action, NewOnExecute, NewOnUpdate);
      FHookItemList.Add(HookObj);
      if Active then
        HookObj.HookAction;
      Action.FreeNotification(Self);
      Result := True;
    end;
  end;
end;

constructor TCnActionListHook.Create(AOwner: TComponent);
begin
  inherited;
  FActionListList := TList.Create;
//  FActionListList.OwnsObjects := False;
  // ����Ҫ���ƶ� ActionList ���ͷš�
  FHookItemList := TObjectList.Create;
  FActive := True;
  FOnAddActionList := nil;
  FOnRemoveActionList := nil;
end;

destructor TCnActionListHook.Destroy;
begin
  FHookItemList.Free;
  FActionListList.Free;
  inherited;
end;

procedure TCnActionListHook.DoAddActionList(AActionList: TActionList);
begin
  if Assigned(FOnAddActionList) then
    FOnAddActionList(Self, AActionList);
end;

procedure TCnActionListHook.DoRemoveActionList(AActionList: TActionList);
begin
  if Assigned(FOnRemoveActionList) then
    FOnRemoveActionList(Self, AActionList);
end;

function TCnActionListHook.GetActionHookObj(
  AAction: TAction): TCnActionHookObj;
var
  i: Integer;
begin
  for i := 0 to FHookItemList.Count - 1 do
    if TCnActionHookObj(FHookItemList[i]).Action = AAction then
    begin
      Result := TCnActionHookObj(FHookItemList[i]);
      Exit;
    end;
  Result := nil;
end;

function TCnActionListHook.GetHookedActionList(Index: Integer): TActionList;
begin
  if (Index >= 0) and (Index < FActionListList.Count) then
    Result := TActionList(FActionListList[Index])
  else
    Result := nil;
end;

function TCnActionListHook.GetHookedActionListCount: Integer;
begin
  Result := FActionListList.Count;
end;

procedure TCnActionListHook.HookActionList(AActionList: TActionList);
begin
  if (AActionList <> nil) and not IsHooked(AActionList) then
  begin
    DoAddActionList(AActionList);
    FActionListList.Add(AActionList);
    AActionList.FreeNotification(Self);
  end
end;

function TCnActionListHook.IsHooked(AActionList: TActionList): Boolean;
begin
  Result := (FActionListList.IndexOf(AActionList) >= 0);
end;

function TCnActionListHook.IsActionHooked(AAction: TAction): Boolean;
begin
  Result := GetActionHookObj(AAction) <> nil;
end;

procedure TCnActionListHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TActionList) then
  begin
    UnHookActionItems(AComponent as TActionList);
    UnHookActionList(AComponent as TActionList);
  end
  else if (Operation = opRemove) and (AComponent is TAction) then
  begin
    RemoveNotifiler(AComponent as TAction);
  end;
end;

procedure TCnActionListHook.RemoveNotifiler(Action: TAction);
var
  HookObj: TCnActionHookObj;
begin
  if IsHooked(TActionList(Action.ActionList)) then
    if IsActionHooked(Action) then
    begin
      Action.RemoveFreeNotification(Self);
      HookObj := GetActionHookObj(Action);
      HookObj.RestoreAction;
      FHookItemList.Delete(FHookItemList.IndexOf(HookObj));
      HookObj.Free;
    end;
end;

procedure TCnActionListHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
  UpdateHookedActions;
end;

procedure TCnActionListHook.UnHookActionItems(ActionList: TActionList);
var
  i: Integer;
begin
  for i := 0 to ActionList.ActionCount - 1 do
    if GetActionHookObj(ActionList.Actions[i] as TAction) <> nil then
      RemoveNotifiler(ActionList.Actions[i] as TAction);
end;

procedure TCnActionListHook.UnHookActionList(AActionList: TActionList);
begin
  if IsHooked(AActionList) then
  begin
    DoRemoveActionList(AActionList);
    AActionList.RemoveFreeNotification(Self);
    UnHookActionItems(AActionList);
    FActionListList.Remove(AActionList);
  end;
end;

procedure TCnActionListHook.UpdateHookedActions;
var
  i: Integer;
begin
  if Active then
    for i := 0 to FHookItemList.Count - 1 do
      TCnActionHookObj(FHookItemList[i]).HookAction
  else
    for i := 0 to FHookItemList.Count - 1 do
      TCnActionHookObj(FHookItemList[i]).RestoreAction;
end;

{function TCnActionListHook.AddActionNotifier(const ActionName: String;
  NewOnExecute, NewOnUpdate: TNotifyEvent): Boolean;
begin
  if (FindComponent(ActionName) <> nil) and
    (FindComponent(ActionName) is TAction) then
      Self.AddActionNotifier((FindComponent(ActionName) as TAction),
        NewOnUpdate, NewOnExecute);
end;

procedure TCnActionListHook.RemoveNotifiler(const ActionName: String);
begin
  if (FindComponent(ActionName) <> nil) and
    (FindComponent(ActionName) is TAction) then
      Self.RemoveNotifiler(FindComponent(ActionName) as TAction);
end; }

function TCnActionListHook.GetHookedAction(Index: Integer): TAction;
begin
  if (Index >= 0) and (Index < FHookItemList.Count) then
    Result := TCnActionHookObj(FHookItemList[Index]).Action
  else
    Result := nil;
end;

function TCnActionListHook.GetHookedActionCount: Integer;
begin
  Result := FHookItemList.Count;
end;

procedure TCnActionListHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnActionListHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnActionListHookComment;
end;

{ TCnActionHookObj }

constructor TCnActionHookObj.Create(AAction: TAction; NewOnExecute,
  NewOnUpdate: TNotifyEvent);
begin
  FAction := AAction;
  FOldOnExecute := AAction.OnExecute;
  FOldOnUpdate := AAction.OnUpdate;
  FNewOnExecute := NewOnExecute;
  FNewOnUpdate := NewOnUpdate;
end;

destructor TCnActionHookObj.Destroy;
begin
  if Self.FAction <> nil then
    Self.RestoreAction;
  inherited;
end;

procedure TCnActionHookObj.HookAction;
begin
  if FAction <> nil then
  begin
    if Assigned(FNewOnExecute) then
      FAction.OnExecute := NewOnExecute;
    if Assigned(FNewOnUpdate) then
      FAction.OnUpdate := NewOnUpdate;
  end;
end;

procedure TCnActionHookObj.RestoreAction;
begin
  if FAction <> nil then
  begin
    FAction.OnExecute := OldOnExecute;
    FAction.OnUpdate := OldOnUpdate;
  end;
end;

procedure TCnActionHookObj.SetAction(const Value: TAction);
begin
  FAction := Value;
end;

procedure TCnActionHookObj.SetNewOnExecute(const Value: TNotifyEvent);
begin
  FNewOnExecute := Value;
end;

procedure TCnActionHookObj.SetNewOnUpdate(const Value: TNotifyEvent);
begin
  FNewOnUpdate := Value;
end;

procedure TCnActionHookObj.SetOldOnExecute(const Value: TNotifyEvent);
begin
  FOldOnExecute := Value;
end;

procedure TCnActionHookObj.SetOldOnUpdate(const Value: TNotifyEvent);
begin
  FOldOnUpdate := Value;
end;

end.
