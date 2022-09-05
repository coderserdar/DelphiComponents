{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2006 CnPack ������                       }
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

unit CnGlobalKeyHook;

{* |<PRE>
================================================================================
* ������ƣ�ϵͳ���������
* ��Ԫ���ƣ�ʵ��ȫ�ּ��̹��ӵ�Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע��ʹ��ϵͳAPIʵ�ֵ���dll�������
* ����ƽ̨��Windows2003 Server + Delphi2007 up2
* ���ݲ��ԣ�Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Messages, Windows, Menus, Forms;

type
  TCnHotKeyItem = class(TCollectionItem)
  private
    FHotKey             : TShortCut;
    FOnExecute          : TNotifyEvent;
    FApplicationToFront : Boolean;
    FID                 : Integer;
    procedure Changed;
    procedure SetHotKey(const Value: TShortCut);
    procedure SetOnExecute(const Value: TNotifyEvent);
    procedure SetApplicationToFront(const Value: Boolean);
    procedure DoExecute;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    {* �Ƿ� Application �ᵽ��ǰ }
    property ApplicationToFront: Boolean read FApplicationToFront
      write SetApplicationToFront default True;
    {* �ȼ���ֵ���� }
    property HotKey: TShortCut read FHotKey write SetHotKey default 0;
    {* �����ȼ�ʱִ�е��¼� }
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
  end;
  
  TCnHotKeyCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCnHotKeyItem;
    procedure SetItem(Index: Integer; const Value: TCnHotKeyItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TCnHotKeyItem;
    function FindItemID(ID: Integer): TCnHotKeyItem;
    function Insert(Index: Integer): TCnHotKeyItem;
    {* �ȼ����� }
    property Items[Index: Integer]: TCnHotKeyItem read GetItem write SetItem;
      default;
  end;

  TCnCustomGlobalKeyHook = class(TComponent)
  private
    FHotKeys  : TCnHotKeyCollection;
    FIDs      : array of Integer;
    FHandle   : THandle;
    FActive   : Boolean;
    procedure SetHotKeys(const Value: TCnHotKeyCollection);
    procedure SetActive(const Value: Boolean);
    procedure WndProc(var Message: TMessage);
  protected
    procedure Changed;
    property HotKeys: TCnHotKeyCollection read FHotKeys write SetHotKeys;
    property Active: Boolean read FActive write SetActive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCnGlobalKeyHook = class(TCnCustomGlobalKeyHook)
  published
    {* �ȼ����� }
    property HotKeys;
    {* �Ƿ�����ʹ���ȼ����� }
    property Active;
  end;

implementation

type
  TIDManager = class
  private
    FIDs  : array of Integer;
  public
    function GetAvailableID: Integer;
    procedure ReleaseID(const ID: Integer);
  end;

var
  IDManager : TIDManager;

function TIDManager.GetAvailableID: Integer;
var
  Ok    : Boolean;
  Index : Integer;
begin
  Result := $F000;

  repeat
    Ok := True;

    for Index := Low(FIDs) to High(FIDs) do
    begin
      if FIDs[Index] = Result then
      begin
        Inc(Result);
        Ok := False;
        Break;
      end;
    end;
  until Ok;

  SetLength(FIDs, Length(FIDs)+1);
  FIDs[High(FIDs)] := Result;
end;

procedure TIDManager.ReleaseID(const ID: Integer);
var
  Index : Integer;
begin
  for Index := Low(FIDs) to High(FIDs) do
  begin
    if FIDs[Index] = ID then
    begin
      if Index < High(FIDs) then
        FIDs[Index] := FIDs[High(FIDs)];

      SetLength(FIDs, Length(FIDs)-1);
      Break;
    end;
  end;
end;

procedure TCnHotKeyItem.Changed;
begin
  (TCnHotKeyCollection(Collection).GetOwner as TCnCustomGlobalKeyHook).Changed;
end;

constructor TCnHotKeyItem.Create(Collection: TCollection);
begin
  inherited;
  FID := IDManager.GetAvailableID;
  FApplicationToFront := True;
end;

destructor TCnHotKeyItem.Destroy;
begin
  IDManager.ReleaseID(FID);
  inherited;
end;

procedure TCnHotKeyItem.DoExecute;
begin
  if FApplicationToFront then
    SetForegroundWindow(Application.Handle);
  if Assigned(FOnExecute) then
    FOnExecute(TCnHotKeyCollection(Collection).GetOwner);
end;

function TCnHotKeyItem.GetDisplayName: string;
begin
  if FHotKey <> 0 then
    Result := ShortCutToText(FHotKey)
  else
    Result := inherited GetDisplayName;
end;

procedure TCnHotKeyItem.SetApplicationToFront(const Value: Boolean);
begin
  if Value <> FApplicationToFront then
  begin
    FApplicationToFront := Value;
    Changed;
  end;
end;

procedure TCnHotKeyItem.SetHotKey(const Value: TShortCut);
begin
  if Value <> FHotKey then
  begin
    FHotKey := Value;
    Changed;
  end;
end;

procedure TCnHotKeyItem.SetOnExecute(const Value: TNotifyEvent);
begin
  FOnExecute := Value;
  Changed;
end;

procedure TCnCustomGlobalKeyHook.Changed;
var
  Index     : Integer;
  ShortCut  : TShortCut;
  Modifiers : Cardinal;
begin
  for Index := Low(FIDs) to High(FIDs) do
    UnregisterHotKey(FHandle, FIDs[Index]);
  SetLength(FIDs, 0);

  if FActive and (not (csDesigning in ComponentState)) then
  begin
    for Index := 0 to FHotKeys.Count-1 do
    begin
      if (FHotKeys[Index].HotKey <> 0) and
        (Assigned(FHotKeys[Index].OnExecute) or
        FHotKeys[Index].ApplicationToFront) then
      begin
        SetLength(FIDs, Length(FIDs)+1);
        FIDs[High(FIDs)] := FHotKeys[Index].FID;
        ShortCut := FHotKeys[Index].HotKey;

        Modifiers := 0;
        if (ShortCut and scShift) <> 0 then
        begin
          Modifiers := Modifiers or MOD_SHIFT;
          ShortCut := ShortCut and (not scShift);
        end;
        if (ShortCut and scCtrl) <> 0 then
        begin
          Modifiers := Modifiers or MOD_CONTROL;
          ShortCut := ShortCut and (not scCtrl);
        end;
        if (ShortCut and scAlt) <> 0 then
        begin
          Modifiers := Modifiers or MOD_ALT;
          ShortCut := ShortCut and (not scAlt);
        end;

        if not RegisterHotKey(FHandle, FIDs[High(FIDs)], Modifiers, ShortCut) then
        begin
          SetLength(FIDs, Length(FIDs)-1);
          RaiseLastWin32Error;
        end;
      end;
    end;
  end;
end;

constructor TCnCustomGlobalKeyHook.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    FHandle := AllocateHWnd(WndProc);

  FActive := True;
  FHotKeys := TCnHotKeyCollection.Create(Self);
end;

destructor TCnCustomGlobalKeyHook.Destroy;
begin
  Active := False;
  FHotKeys.Free;

  if FHandle <> 0 then
    DeallocateHWnd(FHandle);

  inherited;
end;

procedure TCnCustomGlobalKeyHook.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TCnCustomGlobalKeyHook.SetHotKeys(
  const Value: TCnHotKeyCollection);
begin
  FHotKeys.Assign(Value);
end;

procedure TCnCustomGlobalKeyHook.WndProc(var Message: TMessage);
var
  Index : Integer;
begin
  if Message.Msg = WM_HOTKEY then
  begin
    for Index := 0 to FHotKeys.Count-1 do
      if Integer(Message.WParam) = FHotKeys[Index].FID then
      FHotKeys[Index].DoExecute;
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam,
      Message.LParam);
end;

function TCnHotKeyCollection.Add: TCnHotKeyItem;
begin
  Result := inherited Add as TCnHotKeyItem;
end;

constructor TCnHotKeyCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnHotKeyItem);
end;

function TCnHotKeyCollection.FindItemID(ID: Integer): TCnHotKeyItem;
begin
  Result := inherited FindItemID(ID) as TCnHotKeyItem;
end;

function TCnHotKeyCollection.GetItem(Index: Integer): TCnHotKeyItem;
begin
  Result := inherited Items[Index] as TCnHotKeyItem;
end;

function TCnHotKeyCollection.Insert(Index: Integer): TCnHotKeyItem;
begin
  Result := inherited Insert(Index) as TCnHotKeyItem;
end;

procedure TCnHotKeyCollection.SetItem(Index: Integer;
  const Value: TCnHotKeyItem);
begin
  inherited Items[Index] := Value;
end;

initialization
  IDManager := TIDManager.Create;

finalization
  IDManager.Free;
  
end.

