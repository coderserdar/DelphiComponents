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

unit CnMenuHook;
{ |<PRE>
================================================================================
* ������ƣ�CnWizards IDE ר�ҹ��߰�
* ��Ԫ���ƣ��˵��ҽӷ���Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ����ʵ�ֶ� IDE �ڲ� PopupMenu �ĹҽӲ�����ͨ���޸Ĳ˵���
*           OnPopup �¼����ڵ���ǰ��ɾ���Զ���Ĳ˵���ִ��ԭ���� OnPopup ������
*           �����Ӷ���Ĳ˵�����ʵ���Զ���˵��Ĺ��ܡ�
*           ֮���Բ��ø÷���������Ϊֱ���޸� PopupMenu �� IDE �п��ܻᵼ�³���
*           ��Ԫ�ṩ�������ࣺ
*             - TCnAbstractMenuItemDef
*               ������û��˵�����࣬�����Ҫ�ر��ƵĲ˵�������񣬿����Լ�
*               �Ӹ�����������
*             - TCnMenuItemDef
*               ��ͨ���û��˵����࣬����������󲿷���Ҫ��ʹ��ʱֱ�Ӵ�������ʵ
*               ����ע�ᵽ�������м��ɡ�
*             - TCnSepMenuItemDef
*               ��������һ���ָ��˵��
*             - TCnMenuHook
*               �˵������������ڹ���һ����ͬ���ܵĲ˵��������༭�����ܻ��ж�
*               ��ʵ����ÿ��ʵ������һ�� PopupMenu�������Ϳ�����һ������������
*               ���������ṩ�˹ҽ� PopupMenu ������ע���Զ���˵����Լ�����
*               ����
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ���֧�ֱ��ػ�����ʽ
* �޸ļ�¼��2003.10.11
*               �޸Ĳ��ֱ�ʶ����ʹ֮��������⣬����ע��
*           2003.05.01
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, ActnList, Menus, Contnrs,
  CnConsts, CnClasses, CnCompConsts;

type

//==============================================================================
// ������û��˵������
//==============================================================================

{ TCnAbstractMenuItemDef }

  TCnMenuItemInsertPos = (ipFirst, ipLast, ipAfter, ipBefore);
  TCnMenuItemStatus = set of (msVisible, msEnabled, msChecked);

  TCnAbstractMenuItemDef = class(TObject)
  private
    FActive: Boolean;
  protected
    function GetName: string; virtual; abstract;
    function GetInsertPos: TCnMenuItemInsertPos; virtual; abstract;
    function GetRelItemName: string; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetHint: string; virtual; abstract;
    function GetStatus : TCnMenuItemStatus; virtual; abstract;
    function GetAction: TCustomAction; virtual; abstract;
    function GetImageIndex: Integer; virtual; abstract;

    procedure MenuItemCreated(MenuItem: TMenuItem); virtual; abstract;
    {* ���û��˵����������ø÷���}
  public
    procedure Execute(Sender: TObject); virtual; abstract;
    {* �˵���ִ�з���}

    property Active: Boolean read FActive write FActive;
    {* �˵�����Ƿ���Ч�������Ч����˵������Զ�����}
    property Name: string read GetName;
    {* �˵���������}
    property InsertPos: TCnMenuItemInsertPos read GetInsertPos;
    {* �û��˵���Ĳ���λ��}
    property RelItemName: string read GetRelItemName;
    {* �� InsertPos Ϊ ipAfter, ipBefore ʱ����Ե�ԭ�˵���}
    property Caption: string read GetCaption;
    {* �˵���ı���}
    property Hint: string read GetHint;
    {* �˵������ʾ��Ϣ}
    property Status: TCnMenuItemStatus read GetStatus;
    {* �˵����״̬}
    property Action: TCustomAction read GetAction;
    {* �˵����Ӧ�� Action}
    property ImageIndex: Integer read GetImageIndex;
    {* �˵����Ӧ�� ImageIndex}
  end;

//==============================================================================
// ��ͨ���û��˵�����
//==============================================================================

{ TCnMenuItemDef }

  TMenuItemCreatedEvent = procedure (Sender: TObject; MenuItem: TMenuItem) of object;

  TCnMenuItemDef = class(TCnAbstractMenuItemDef)
  private
    FName: string;
    FInsertPos: TCnMenuItemInsertPos;
    FRelItemName: string;
    FCaption: string;
    FHint: string;
    FAction: TCustomAction;
    FImageIndex: Integer;
    FStatus: TCnMenuItemStatus;
    FOnClick: TNotifyEvent;
    FOnCreated: TMenuItemCreatedEvent;
  protected
    function GetName: string; override;
    function GetInsertPos: TCnMenuItemInsertPos; override;
    function GetRelItemName: string; override;
    function GetCaption: string; override;
    function GetHint: string; override;
    function GetStatus: TCnMenuItemStatus; override;
    function GetAction: TCustomAction; override;
    function GetImageIndex: Integer; override;
    procedure MenuItemCreated(MenuItem: TMenuItem); override;
  public
    constructor Create(const AName, ACaption: string; AOnClick: TNotifyEvent;
      AInsertPos: TCnMenuItemInsertPos; const ARelItemName: string = '';
      const AHint: string = ''; AAction: TCustomAction = nil; ImgIndex: Integer = -1);
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;

    procedure SetCaption(const Value: string);
    {* ���ò˵�����}
    procedure SetHint(const Value: string);
    {* ���ò˵���ʾ��Ϣ}
    procedure SetImageIndex(Value: Integer);
    {* ���ò˵���� ImageIndex}

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {* �˵�����¼�}
    property OnCreated: TMenuItemCreatedEvent read FOnCreated write FOnCreated;
    {* ���˵����̬����֮����ã��û������ڸ��¼����޸Ĳ˵�����}
  end;

//==============================================================================
// �ָ��˵�����
//==============================================================================

{ TCnSepMenuItemDef }

  TCnSepMenuItemDef = class(TCnMenuItemDef)
  public
    constructor Create(AInsertPos: TCnMenuItemInsertPos; const ARelItemName: string);
  end;

//==============================================================================
// ���ҽӵ� TPopupMenu �˵�����������
//==============================================================================

{ TMenuObj }

  TMenuObj = class(TObject)
  private
    FOldOnPopup: TNotifyEvent;
    FMenu: TPopupMenu;
  public
    constructor Create(AMenu: TPopupMenu; NewOnPopup: TNotifyEvent);
    destructor Destroy; override;
    property Menu: TPopupMenu read FMenu;
    property OldOnPopup: TNotifyEvent read FOldOnPopup;
  end;

//==============================================================================
// �˵��ҽӹ�����
//==============================================================================

{ TCnMenuHook }

  TMenuPopupEvent = procedure (Sender: TObject; Menu: TPopupMenu) of object;

  TCnMenuHook = class(TCnComponent)
  private
    FMenuList: TObjectList;
    FMenuItemDefList: TObjectList;
    FActive: Boolean;
    FOnAfterPopup: TMenuPopupEvent;
    FOnBeforePopup: TMenuPopupEvent;
    procedure SetActive(const Value: Boolean);
    function GetMenuItemDef(Index: Integer): TCnAbstractMenuItemDef;
    function GetMenuItemDefCount: Integer;
  protected
    function GetMenuObj(Menu: TPopupMenu): TMenuObj;
    procedure OnMenuPopup(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function FindMenuItem(AMenu: TPopupMenu; const AName: string): TMenuItem;
    procedure DoRemoveMenuItem(AMenu: TPopupMenu; const AName: string);
    procedure DoAddMenuItem(AMenu: TPopupMenu; Item: TCnAbstractMenuItemDef);

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HookMenu(AMenu: TPopupMenu);
    {* �ҽ�һ�� PopupMenu �˵�}
    procedure UnHookMenu(AMenu: TPopupMenu);
    {* ȡ���� PopupMenu �˵��Ĺҽ�}
    function IsHooked(AMenu: TPopupMenu): Boolean;
    {* �ж� PopupMenu �˵��Ƿ��Ѿ��ҽ�}

    function AddMenuItemDef(Item: TCnAbstractMenuItemDef): Integer;
    {* ����һ���û��˵���壬�����б�������}
    procedure RemoveMenuItemDef(Item: TCnAbstractMenuItemDef);
    {* ��ȥһ���û��˵����}
    function IndexOfMenuItemDef(const AName: string): Integer;
    {* ����ָ���˵����б��е�������}

    property Active: Boolean read FActive write SetActive;
    {* �˵��ҽӻ�Ծ����}
    property MenuItemDefCount: Integer read GetMenuItemDefCount;
    {* �û��˵�������}
    property MenuItemDefs[Index: Integer]: TCnAbstractMenuItemDef read GetMenuItemDef;
    {* �û��˵��������}
    property OnBeforePopup: TMenuPopupEvent read FOnBeforePopup write FOnBeforePopup;
    {* ���ҽӵĲ˵�����ǰ�¼�����ʱ�û��˵����Ѿ��ͷţ��û����ڴ˽����ر�Ĵ���}
    property OnAfterPopup: TMenuPopupEvent read FOnAfterPopup write FOnAfterPopup;
    {* ���ҽӵĲ˵��������¼�����ʱ�û��˵����Ѿ��������û����ڴ˽����ر�Ĵ���}
  end;

implementation

const
  csMenuItemTag = $8080;

//==============================================================================
// ��ͨ���û��˵�����
//==============================================================================

{ TCnMenuItemDef }

constructor TCnMenuItemDef.Create(const AName, ACaption: string;
  AOnClick: TNotifyEvent; AInsertPos: TCnMenuItemInsertPos; const ARelItemName,
  AHint: string; AAction: TCustomAction; ImgIndex: Integer);
begin
  inherited Create;
  FActive := True;
  FStatus := [msVisible, msEnabled];
  FName := AName;
  FCaption := ACaption;
  FOnClick := AOnClick;
  FInsertPos := AInsertPos;
  FRelItemName := ARelItemName;
  FHint := AHint;
  FAction := AAction;
  FImageIndex := ImgIndex;
  FOnCreated := nil;
end;

destructor TCnMenuItemDef.Destroy;
begin

  inherited;
end;

procedure TCnMenuItemDef.Execute(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Sender);
end;

function TCnMenuItemDef.GetAction: TCustomAction;
begin
  Result := FAction;
end;

function TCnMenuItemDef.GetCaption: string;
begin
  Result := FCaption;
end;

function TCnMenuItemDef.GetHint: string;
begin
  Result := FHint;
end;

function TCnMenuItemDef.GetInsertPos: TCnMenuItemInsertPos;
begin
  Result := FInsertPos;
end;

function TCnMenuItemDef.GetName: string;
begin
  Result := FName;
end;

function TCnMenuItemDef.GetRelItemName: string;
begin
  Result := FRelItemName;
end;

function TCnMenuItemDef.GetStatus: TCnMenuItemStatus;
begin
  Result := FStatus;
end;

procedure TCnMenuItemDef.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCnMenuItemDef.SetHint(const Value: string);
begin
  FHint := Value;
end;

procedure TCnMenuItemDef.MenuItemCreated(MenuItem: TMenuItem);
begin
  if Assigned(FOnCreated) then
    FOnCreated(Self, MenuItem);
end;

function TCnMenuItemDef.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TCnMenuItemDef.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
end;

//==============================================================================
// �ָ��˵�����
//==============================================================================

{ TCnSepMenuItemDef }

constructor TCnSepMenuItemDef.Create(AInsertPos: TCnMenuItemInsertPos;
  const ARelItemName: string);
begin
  inherited Create('', '-', nil, AInsertPos, ARelItemName, '', nil);
end;

//==============================================================================
// ���ҽӵ� TPopupMenu �˵�����������
//==============================================================================

{ TMenuObj }

constructor TMenuObj.Create(AMenu: TPopupMenu; NewOnPopup: TNotifyEvent);
begin
  inherited Create;
  FMenu := AMenu;
  FOldOnPopup := FMenu.OnPopup;
  FMenu.OnPopup := NewOnPopup;
end;

destructor TMenuObj.Destroy;
begin
  FMenu.OnPopup := FOldOnPopup;
  inherited;
end;

//==============================================================================
// �˵��ҽӹ�����
//==============================================================================

{ TCnMenuHook }

constructor TCnMenuHook.Create(AOwner: TComponent);
begin
  inherited;
  FMenuList := TObjectList.Create;
  FMenuItemDefList := TObjectList.Create;
  FActive := True;
  FOnAfterPopup := nil;
  FOnBeforePopup := nil;
end;

destructor TCnMenuHook.Destroy;
begin
  FMenuItemDefList.Free;
  FMenuList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// �˵����
//------------------------------------------------------------------------------

function TCnMenuHook.FindMenuItem(AMenu: TPopupMenu;
  const AName: string): TMenuItem;
var
  i: Integer;
begin
  Result := nil;
  if (AMenu = nil) or (AName = '') then Exit;

  for i := 0 to AMenu.Items.Count - 1 do
    if SameText(AMenu.Items[i].Name, AName) then
    begin
      Result := AMenu.Items[i];
      Exit;
    end;
end;

procedure TCnMenuHook.DoAddMenuItem(AMenu: TPopupMenu;
  Item: TCnAbstractMenuItemDef);
var
  MenuItem, RelItem: TMenuItem;
  Idx: Integer;
begin
  Assert(Assigned(AMenu));
  Assert(Assigned(Item));
  
  if FActive and Item.Active then
  begin
    MenuItem := FindMenuItem(AMenu, Item.Name);
    if not Assigned(MenuItem) then
    begin
      MenuItem := TMenuItem.Create(AMenu);
      MenuItem.Name := Item.Name;

      RelItem := FindMenuItem(AMenu, Item.RelItemName);
      Idx := 0;
      case Item.InsertPos of
        ipFirst: Idx := 0;
        ipLast: Idx := AMenu.Items.Count;
        ipAfter:
          if Assigned(RelItem) then
            Idx := RelItem.MenuIndex + 1
          else
            Idx := AMenu.Items.Count;
        ipBefore:
          if Assigned(RelItem) then
            Idx := RelItem.MenuIndex
          else
            Idx := 0;
      end;

      AMenu.Items.Insert(Idx, MenuItem);
    end;

    // ����һ�� Tag���Ա�־û�� Name ���Զ���˵�
    MenuItem.Tag := csMenuItemTag;
    MenuItem.Caption := Item.Caption;
    MenuItem.Hint := Item.Hint;
    MenuItem.Enabled := msEnabled in Item.Status;
    MenuItem.Visible := msVisible in Item.Status;
    MenuItem.Checked := msChecked in Item.Status;
    MenuItem.ImageIndex := Item.ImageIndex;
    MenuItem.OnClick := Item.Execute;
    MenuItem.Action := Item.Action;
    
    Item.MenuItemCreated(MenuItem);
  end
end;

procedure TCnMenuHook.DoRemoveMenuItem(AMenu: TPopupMenu;
  const AName: string);
var
  Item: TMenuItem;
begin
  Item := FindMenuItem(AMenu, AName);
  if Assigned(Item) then
    Item.Free;
end;

//------------------------------------------------------------------------------
// �˵��ҽӴ���
//------------------------------------------------------------------------------

function TCnMenuHook.GetMenuObj(Menu: TPopupMenu): TMenuObj;
var
  i: Integer;
begin
  for i := 0 to FMenuList.Count - 1 do
    if TMenuObj(FMenuList[i]).Menu = Menu then
    begin
      Result := TMenuObj(FMenuList[i]);
      Exit;
    end;
  Result := nil;
end;

procedure TCnMenuHook.HookMenu(AMenu: TPopupMenu);
begin
  if not IsHooked(AMenu) then
  begin
    FMenuList.Add(TMenuObj.Create(AMenu, OnMenuPopup));
    AMenu.FreeNotification(Self);
  end;
end;

procedure TCnMenuHook.UnHookMenu(AMenu: TPopupMenu);
var
  Obj: TMenuObj;
begin
  Obj := GetMenuObj(AMenu);
  if Assigned(Obj) then
  begin
    Obj.Menu.RemoveFreeNotification(Self);
    FMenuList.Remove(Obj);
  end;
end;

function TCnMenuHook.IsHooked(AMenu: TPopupMenu): Boolean;
begin
  Result := Assigned(GetMenuObj(AMenu)); 
end;

procedure TCnMenuHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TPopupMenu) then
    UnHookMenu(AComponent as TPopupMenu)
end;

//------------------------------------------------------------------------------
// �����˵��ҽ����
//------------------------------------------------------------------------------

function TCnMenuHook.AddMenuItemDef(
  Item: TCnAbstractMenuItemDef): Integer;
begin
  Result := FMenuItemDefList.IndexOf(Item);
  if Result < 0 then
    Result := FMenuItemDefList.Add(Item);
end;

procedure TCnMenuHook.RemoveMenuItemDef(Item: TCnAbstractMenuItemDef);
begin
  FMenuItemDefList.Remove(Item);
end;

function TCnMenuHook.IndexOfMenuItemDef(
  const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to MenuItemDefCount - 1 do
    if SameText(MenuItemDefs[i].Name, AName) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TCnMenuHook.GetMenuItemDefCount: Integer;
begin
  Result := FMenuItemDefList.Count;
end;

function TCnMenuHook.GetMenuItemDef(
  Index: Integer): TCnAbstractMenuItemDef;
begin
  Result := TCnAbstractMenuItemDef(FMenuItemDefList[Index]);
end;

procedure TCnMenuHook.OnMenuPopup(Sender: TObject);
var
  Menu: TPopupMenu;
  MenuObj: TMenuObj;
  i: Integer;
begin
  if not (Sender is TPopupMenu) then
    Exit;
    
  Menu := Sender as TPopupMenu;

  // �����Ȱ���ǰע��Ĳ˵��������������
  for i := 0 to MenuItemDefCount - 1 do
    DoRemoveMenuItem(Menu, MenuItemDefs[i].Name);
    
  // ���� Tag ��ȥû�����ֵĲ˵���
  for i := Menu.Items.Count - 1 downto 0 do
    if Menu.Items[i].Tag = csMenuItemTag then
      Menu.Items[i].Free;

  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, Menu);

  // ����ԭ�����¼�
  MenuObj := GetMenuObj(Menu);
  if Assigned(MenuObj) then
    if Assigned(MenuObj.OldOnPopup) then
      MenuObj.OldOnPopup(Sender);

  // ����˵����û�����ݣ���˵�����ᵯ�����˴�Ҳ��������ݣ�����ǿ�е���
  if Menu.Items.Count = 0 then
    Exit;

  if Active then
  begin
    // ���¸����Զ���˵���
    for i := 0 to MenuItemDefCount - 1 do
      if MenuItemDefs[i].Active then
        DoAddMenuItem(Menu, MenuItemDefs[i]);

    if Assigned(FOnAfterPopup) then
      FOnAfterPopup(Self, Menu);
  end;
end;

procedure TCnMenuHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCnMenuHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnMenuHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnMenuHookComment;
end;

end.
