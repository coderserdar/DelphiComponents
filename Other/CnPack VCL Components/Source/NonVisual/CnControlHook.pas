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

unit CnControlHook;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��ؼ���Ϣ������̹ҽ������Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ������ TCnControlHook ���������ͨ���滻 TControl �����
*           WindowProc ��������ÿؼ�����Ϣ֪ͨ��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.04.30 V1.2
*               �����ؼ�����Ϣ����������ͷŵ��¹ҽӶ�����������
*           2002.10.19 V1.1
*               ���±�д�Ƚ����Ƶ����
*           2002.10.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, CnClasses, CnConsts,
  CnCompConsts;

type

//==============================================================================
// �ؼ��ҽ�����
//==============================================================================

{ TCnControlHookItem }

  TCnControlHook = class;
  TCnControlHookCollection = class;

  THookMessageEvent = procedure (Sender: TObject; Control: TControl;
    var Msg: TMessage; var Handled: Boolean) of object;
  {* �ҽ���Ϣ�¼�
   |<PRE>
     Sender: TObject      - �����¼������
     Control: TControl    - ����ϢҪ���͵Ŀؼ����󣬼����ҽӵĿؼ�
     var Msg: TMessage    - ��Ϣ����
     var Handled: Boolean - �¼���������Ƿ񲶻����Ϣ�����Ϊ�潫������ԭ�ؼ���Ϣ����
   |</PRE>}

  TCnControlHookItem = class(TCollectionItem)
  {* �ؼ��ҽ������࣬���� TCnControlHook ����С�
     �����ҽӵĿؼ��ͷ�ʱ��������� Item ����Ҳ�ᱻ�Զ��ͷţ�
     �û��ɲ��ÿ����ظ��ҽӵ����⣬��Ҳ��Ҫ��̬���� Item ����}
  private
    FOwner: TCnControlHookCollection;
    FControl: TControl;
    FBeforeMessage: THookMessageEvent;
    FAfterMessage: THookMessageEvent;
    procedure SetControl(const Value: TControl);
    procedure Hook;
    procedure UnHook;
  protected
    function DoAfterMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    function DoBeforeMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    property Owner: TCnControlHookCollection read FOwner;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Control: TControl read FControl write SetControl;
    {* Ҫ Hook �Ŀؼ�}
    property BeforeMessage: THookMessageEvent read FBeforeMessage write FBeforeMessage;
    {* �ؼ���Ϣ�¼�����Ĭ����Ϣ�������֮ǰ����}
    property AfterMessage: THookMessageEvent read FAfterMessage write FAfterMessage;
    {* �ؼ���Ϣ�¼�����Ĭ����Ϣ�������֮�����}
  end;

//==============================================================================
// �ؼ��ҽ��б���
//==============================================================================
   
{ TCnControlHookCollection }

  TCnControlHookCollection = class(TOwnedCollection)
  {* �ؼ��ҽ��б��࣬���� TCnControlHook �����}
  private
    FOwner: TCnControlHook;
    function GetItem(Index: Integer): TCnControlHookItem;
    procedure SetItem(Index: Integer; const Value: TCnControlHookItem);
  protected
    property ControlHook: TCnControlHook read FOwner;
  public
    constructor Create(AOwner: TCnControlHook);
    destructor Destroy; override;
    function Add(Control: TControl): TCnControlHookItem;
    {* ����һ���ؼ��ҽ���}
    procedure Remove(Control: TControl);
    {* ɾ��һ���ؼ��ҽ���}
    function IndexOf(Control: TControl): Integer;
    {* ���ҿؼ��ҽ���}
    property Items[Index: Integer]: TCnControlHookItem read GetItem write SetItem; default;
    {* �ؼ��ҽ�������}
  end;

//==============================================================================
// �ؼ���Ϣ���̹ҽ����
//==============================================================================
   
{ TCnControlHook }

  TCnControlHook = class(TCnComponent)
  {* �ؼ���Ϣ���̹ҽ����������ͨ���滻 TControl ����� WindowProc ��������ÿؼ�����Ϣ֪ͨ}
  private
    FActive: Boolean;
    FItems: TCnControlHookCollection;
    FBeforeMessage: THookMessageEvent;
    FAfterMessage: THookMessageEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetItems(const Value: TCnControlHookCollection);
  protected
    function DoAfterMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    function DoBeforeMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    procedure Loaded; override;
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    function IndexOf(Control: TControl): Integer;
    {* ����ָ�����ҽӿؼ��������ţ���������ڣ����� -1}
    function Hook(Control: TControl): TCnControlHookItem;
    {* �ҽ�ָ���ؼ������عҽ������ѹҽӷ���ԭ�ҽ���}
    procedure UnHook(Control: TControl);
    {* ȡ����ָ���ؼ��Ĺҽ�}
    function IsHooked(Control: TControl): Boolean;
    {* �ж�ָ���ؼ��Ƿ񱻹ҽ�}
  published
    property Active: Boolean read FActive write SetActive default True;
    {* �Ƿ�����ʹ��}
    property Items: TCnControlHookCollection read FItems write SetItems;
    {* �ҽӿؼ��б�}
    property BeforeMessage: THookMessageEvent read FBeforeMessage write FBeforeMessage;
    {* �ؼ���Ϣ�¼�����Ĭ����Ϣ�������֮ǰ����}
    property AfterMessage: THookMessageEvent read FAfterMessage write FAfterMessage;
    {* �ؼ���Ϣ�¼�����Ĭ����Ϣ�������֮�����}
  end;

implementation

const
  UM_DESTROYHOOK = WM_USER + 101;

type

//==============================================================================
// �ؼ���Ϣ������̹ҽӶ���˽���ࣩ
//==============================================================================
   
{ TCnControlHookObject }

  TCnControlHookMgr = class;

  TCnControlHookObject = class
  private
    FList: TList;
    FControlHookMgr: TCnControlHookMgr;
    FControl: TControl;
    FOldWndProc: TWndMethod;
    FUpdateCount: Integer;
    FAutoFree: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCnControlHookItem;
  protected
    procedure WndProc(var Message: TMessage);
    property Control: TControl read FControl;
    property ControlHookMgr: TCnControlHookMgr read FControlHookMgr;
  public
    constructor Create(AControlHookMgr: TCnControlHookMgr; AControl: TControl);
    destructor Destroy; override;
    function Add(Item: TCnControlHookItem): Integer;
    procedure DoFree;
    function Updating: Boolean;
    
    procedure Delete(Item: TCnControlHookItem); overload;
    procedure Delete(Index: Integer); overload;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCnControlHookItem read GetItem;
  end;

//==============================================================================
// �ؼ���Ϣ������̹ҽ������˽���ࣩ
//==============================================================================

{ TCnControlHookMgr }

  TCnControlHookMgr = class(TComponent)
  {* �ؼ���Ϣ�ҽ������ͨ���滻 TControl ����� WindowProc ����������}
  private
    FList: TList;
    function GetCount: Integer;
    function GetHookedControls(Index: Integer): TControl;
    function GetItem(Index: Integer): TCnControlHookObject;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookControl(Item: TCnControlHookItem);
    procedure UnhookControl(Item: TCnControlHookItem); overload;
    procedure UnhookControl(Control: TControl); overload;
    function IndexOf(Control: TControl): Integer;
    property Count: Integer read GetCount;
    property HookedControls[Index: Integer]: TControl read GetHookedControls;
    property Items[Index: Integer]: TCnControlHookObject read GetItem;
  end;

var
  ControlHookMgr: TCnControlHookMgr;

// ���عҽӹ�����
function GetControlHookMgr: TCnControlHookMgr;
begin
  if not Assigned(ControlHookMgr) then
    ControlHookMgr := TCnControlHookMgr.Create(nil);
  Result := ControlHookMgr;
end;
  
//==============================================================================
// �ؼ���Ϣ������̹ҽӶ���˽���ࣩ
//==============================================================================
   
{ TCnControlHookObject }

// ������
constructor TCnControlHookObject.Create(AControlHookMgr: TCnControlHookMgr;
  AControl: TControl);
begin
  Assert(Assigned(AControlHookMgr) and Assigned(AControl));
  FUpdateCount := 0;
  FAutoFree := False;
  FList := TList.Create;
  FControlHookMgr := AControlHookMgr;
  FControl := AControl;
  FOldWndProc := FControl.WindowProc;
  FControl.WindowProc := WndProc;
  FControl.FreeNotification(FControlHookMgr);
end;

// ������
destructor TCnControlHookObject.Destroy;
var
  i: Integer;
begin
  try                                  // �쳣����
    if Assigned(FControl) then
    begin
      FControlHookMgr.FList.Remove(Self);
      FControl.RemoveFreeNotification(FControlHookMgr);
      FControl.WindowProc := FOldWndProc;
      FControl := nil;
    end;

    for i := 0 to Count - 1 do
      Items[i].Free;
    FList.Free;
  except
    Application.HandleException(Self);
  end;
  inherited;
end;

function TCnControlHookObject.Updating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TCnControlHookObject.DoFree;
begin
  if Updating then
  begin
    FAutoFree := True;
    try
      FControlHookMgr.FList.Remove(Self);
      FControl.RemoveFreeNotification(FControlHookMgr);
      FControl.WindowProc := FOldWndProc;
      FControl := nil;
    except
      Application.HandleException(Self);
    end;
  end
  else
    Free;
end;

// �µ���Ϣ�������
procedure TCnControlHookObject.WndProc(var Message: TMessage);
var
  i: Integer;
  Handled: Boolean;
begin
  try
    Inc(FUpdateCount);
    try
      Handled := False;
      // ���ùҽ���Ϣǰ�������
      for i := Count - 1 downto 0 do       // ��ҽӵ��ȴ���
        if Assigned(Items[i].FOwner) and Assigned(Items[i].FOwner.FOwner) and
          Items[i].FOwner.FOwner.FActive and Items[i].DoBeforeMessage(FControl,
          Message) then
        begin
          Handled := True;
          Break;
        end;

      if Handled then Exit;

      // ����ԭ�������
      if Assigned(FOldWndProc) then
        FOldWndProc(Message);

      // ���ùҽ���Ϣ�������
      if not FAutoFree then
      begin
        for i := Count - 1 downto 0 do       // ��ҽӵ��ȴ���
          if Assigned(Items[i].FOwner) and Assigned(Items[i].FOwner.FOwner) and
            Items[i].FOwner.FOwner.FActive and Items[i].DoAfterMessage(FControl,
            Message) then
            Break;
      end;
    finally
      Dec(FUpdateCount);
    end;

    // �˴������ͷ�
    if FAutoFree then
      Free;
  except
    Application.HandleException(Self);
  end;
end;

//------------------------------------------------------------------------------
// �б��������
//------------------------------------------------------------------------------

// ����һ��
function TCnControlHookObject.Add(Item: TCnControlHookItem): Integer;
begin
  if FList.IndexOf(Item) < 0 then
  begin
    Item.FControl := FControl;
    Result := FList.Add(Item);
  end
  else
    Result := -1;
end;

// ����������ɾ��һ��
procedure TCnControlHookObject.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    FList.Delete(Index);
    if Count = 0 then                  // �޹ҽ���ʱ�Զ��ͷ�
      DoFree;
  end;
end;

// ��������ɾ��һ��
procedure TCnControlHookObject.Delete(Item: TCnControlHookItem);
begin
  Delete(FList.IndexOf(Item));
end;

//------------------------------------------------------------------------------
// ���Զ�д����
//------------------------------------------------------------------------------

// Count ���Զ�����
function TCnControlHookObject.GetCount: Integer;
begin
  Result := FList.Count;
end;

// Items �������Զ�����
function TCnControlHookObject.GetItem(Index: Integer): TCnControlHookItem;
begin
  Result := TCnControlHookItem(FList[Index]);
end;

//==============================================================================
// �ؼ���Ϣ������̹ҽ������˽���ࣩ
//==============================================================================

{ TCnControlHookMgr }

// ������
constructor TCnControlHookMgr.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

// ������
destructor TCnControlHookMgr.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].DoFree;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// �ҽ���ط���
//------------------------------------------------------------------------------

// ���֪ͨ�¼�
procedure TCnControlHookMgr.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TControl) then
    UnhookControl(TControl(AComponent)); // �ؼ��ͷ�ʱ���ҽ�
end;

// ���ؿؼ�������
function TCnControlHookMgr.IndexOf(Control: TControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if HookedControls[i] = Control then
    begin
      Result := i;
      Exit;
    end;
end;

// �ҽӿؼ�
procedure TCnControlHookMgr.HookControl(Item: TCnControlHookItem);
var
  Obj: TCnControlHookObject;
  Idx: Integer;
begin
  Assert(Assigned(Item) and Assigned(Item.FControl));
  Idx := IndexOf(Item.FControl);
  if Idx < 0 then
  begin
    Obj := TCnControlHookObject.Create(Self, Item.FControl);
    Obj.Add(Item);
    FList.Add(Obj);
  end
  else
    Items[Idx].Add(Item);
end;

// ���ҽӿؼ�
procedure TCnControlHookMgr.UnhookControl(Item: TCnControlHookItem);
var
  Idx: Integer;
begin
  Assert(Assigned(Item) and Assigned(Item.FControl));
  Idx := IndexOf(Item.FControl);
  if Idx >= 0 then
    Items[Idx].Delete(Item);
end;

// ���ҽӿؼ�
procedure TCnControlHookMgr.UnhookControl(Control: TControl);
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Items[Idx].DoFree;
end;

//------------------------------------------------------------------------------
// ���Զ�д����
//------------------------------------------------------------------------------

// HookedControlCount ���Զ�����
function TCnControlHookMgr.GetCount: Integer;
begin
  Result := FList.Count;
end;

// HookedControls ���Զ�����
function TCnControlHookMgr.GetHookedControls(Index: Integer): TControl;
begin
  Result := TCnControlHookObject(FList[Index]).Control;
end;

// Items �������Զ�����
function TCnControlHookMgr.GetItem(Index: Integer): TCnControlHookObject;
begin
  Result := TCnControlHookObject(FList[Index]);
end;

//==============================================================================
// �ؼ��ҽ�����
//==============================================================================
   
{ TCnControlHookItem }

// �๹����
constructor TCnControlHookItem.Create(Collection: TCollection);
begin
  inherited;
  Assert(Assigned(Collection));
  FOwner := TCnControlHookCollection(Collection);
end;

// ��������
destructor TCnControlHookItem.Destroy;
begin
  if Assigned(FControl) then
    GetControlHookMgr.UnhookControl(Self);
  inherited;
end;

// ����ֵ
procedure TCnControlHookItem.Assign(Source: TPersistent);
begin
  if Source is TCnControlHookItem then
  begin
    TCnControlHookItem(Source).Control := FControl;
  end
  else
    inherited;
end;

// ���� AfterMessage �¼�
function TCnControlHookItem.DoAfterMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := FOwner.FOwner.DoAfterMessage(Control, Msg);
  if not Result and FOwner.FOwner.FActive and Assigned(FAfterMessage) then
    FAfterMessage(Self, Control, Msg, Result);
end;

// ���� BeforeMessage �¼�
function TCnControlHookItem.DoBeforeMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := FOwner.FOwner.DoBeforeMessage(Control, Msg);
  if not Result and FOwner.FOwner.FActive and Assigned(FBeforeMessage) then
    FBeforeMessage(Self, Control, Msg, Result);
end;

// �ҽ�
procedure TCnControlHookItem.Hook;
begin
  if ([csLoading, csDesigning] * FOwner.FOwner.ComponentState = []) and
    Assigned(FControl) then
    GetControlHookMgr.HookControl(Self);
end;

// ���ҽ�
procedure TCnControlHookItem.UnHook;
begin
  if ([csLoading, csDesigning] * FOwner.FOwner.ComponentState = []) and
    Assigned(FControl) then
    GetControlHookMgr.UnhookControl(Self);
end;

// Control ����д����
procedure TCnControlHookItem.SetControl(const Value: TControl);
begin
  if Value <> FControl then
  begin
    UnHook;
    FControl := Value;
    Hook;
  end;
end;

//==============================================================================
// �ؼ��ҽ��б���
//==============================================================================
   
{ TCnControlHookCollection }

// ������
constructor TCnControlHookCollection.Create(AOwner: TCnControlHook);
begin
  inherited Create(AOwner, TCnControlHookItem);
  FOwner := AOwner;
end;

// ������
destructor TCnControlHookCollection.Destroy;
begin
  inherited;
end;

// ����һ��
function TCnControlHookCollection.Add(Control: TControl): TCnControlHookItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Result := Items[Idx]
  else
  begin
    Result := TCnControlHookItem(inherited Add);
    Result.Control := Control;
  end;
end;

// ɾ��һ��
procedure TCnControlHookCollection.Remove(Control: TControl);
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Items[Idx].Free;
end;

// ��������
function TCnControlHookCollection.IndexOf(Control: TControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].FControl = Control then
    begin
      Result := i;
      Exit;
    end;
end;

// Items �������Զ�����
function TCnControlHookCollection.GetItem(
  Index: Integer): TCnControlHookItem;
begin
  Result := TCnControlHookItem(inherited Items[Index]);
end;

// Items ��������д����
procedure TCnControlHookCollection.SetItem(Index: Integer;
  const Value: TCnControlHookItem);
begin
  inherited SetItem(Index, Value);
end;

//==============================================================================
// �ؼ���Ϣ���̹ҽ����
//==============================================================================
   
{ TCnControlHook }

// ������
constructor TCnControlHook.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TCnControlHookCollection.Create(Self);
  FActive := True;
end;

// ������
destructor TCnControlHook.Destroy;
begin
  FItems.Free;
  inherited;
end;

// ������������װ��
procedure TCnControlHook.Loaded;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Items.Count - 1 do
    Items.Items[i].Hook;
end;

// �ҽ�ָ���ؼ������عҽ��������ţ�����ѹҽӷ���ԭ�ҽ���������
function TCnControlHook.Hook(Control: TControl): TCnControlHookItem;
begin
  Result := Items.Add(Control);
end;

// ����ָ�����ҽӿؼ��������ţ���������ڣ����� -1
function TCnControlHook.IndexOf(Control: TControl): Integer;
begin
  Result := Items.IndexOf(Control);
end;

// �ж�ָ���ؼ��Ƿ񱻹ҽ�
function TCnControlHook.IsHooked(Control: TControl): Boolean;
begin
  Result := IndexOf(Control) >= 0;
end;

// ȡ����ָ���ؼ��Ĺҽ�
procedure TCnControlHook.UnHook(Control: TControl);
begin
  Items.Remove(Control);
end;

//------------------------------------------------------------------------------
// �����¼�����
//------------------------------------------------------------------------------

// ����AfterMessage�¼�
function TCnControlHook.DoAfterMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := False;
  if Active and Assigned(FAfterMessage) then
    FAfterMessage(Self, Control, Msg, Result);
end;

// ����BeforeMessage�¼�
function TCnControlHook.DoBeforeMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := False;
  if Active and Assigned(FBeforeMessage) then
    FBeforeMessage(Self, Control, Msg, Result);
end;

// Active ����д����
procedure TCnControlHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

// Items ����д����
procedure TCnControlHook.SetItems(
  const Value: TCnControlHookCollection);
begin
  FItems.Assign(Value);
end;

// ȡ������Ϣ
procedure TCnControlHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnControlHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnControlHookComment;
end;

initialization

finalization
  if Assigned(ControlHookMgr) then
    FreeAndNil(ControlHookMgr);

end.
