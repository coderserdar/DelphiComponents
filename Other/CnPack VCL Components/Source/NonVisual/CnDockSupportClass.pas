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

{*******************************************************}
{                                                       }
{       һЩͨ�õ���                                    }
{       SupportClass ��Ԫ                               }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockSupportClass;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�ͣ������е�һЩͨ�õ��൥Ԫ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, Messages, Controls, Forms, CnNativeDecl;

type

  TCnBaseTree = class;

  {���Ľڵ�Ļ�����}
  TCnBaseZone = class
  private
    FBaseTree: TCnBaseTree;      //�����Ŀ���
    FChildZone: TCnBaseZone;     //����Ů
    FNextSibling: TCnBaseZone;   //���ֵ�
    FPrevSibling: TCnBaseZone;   //��һ���ֵ�
    FParentZone: TCnBaseZone;    //����
  protected
    function GetNextSibingCount: Integer; //������ֵܵĸ���
    function GetPrevSibingCount: Integer; //���ǰ�ֵܵĸ���
  public
    constructor Create(BaseTree: TCnBaseTree); virtual;
    function CreateChildZone: TCnBaseZone;
    function GetParentZone: TCnBaseZone; virtual;
    function GetChildCount: Integer;      //�������ĸ���
    function GetChildZone(Index: Word): TCnBaseZone;
                                          //�������ΪIndex������
    property ChildZone: TCnBaseZone read FChildZone write FChildZone;
    property NextSibling: TCnBaseZone read FNextSibling write FNextSibling;
    property PrevSibling: TCnBaseZone read FPrevSibling write FPrevSibling;
    property ParentZone: TCnBaseZone read FParentZone write FParentZone;
    property BaseTree: TCnBaseTree read FBaseTree write FBaseTree;
  end;

  TScanZoneNotification = (snNone, snAdded, snExtracted, snDeleted);

  TCnTreeZoneClass = class of TCnBaseZone;

  TScanTreeZoneProc = procedure(TreeZone: TCnBaseZone);

  {���Ļ�����}
  TCnBaseTree = class
  private
    FScanAction: TScanZoneNotification;
    FTreeZoneClass: TCnTreeZoneClass;
    FTopTreeZone: TCnBaseZone;        //���ĸ��ڵ�
    FCurrTreeZone: TCnBaseZone;       //��ǰ�����ڵ�
    FScanZoneProc: TScanTreeZoneProc; //TScanTreeZoneProc;
  protected
    procedure ForwardScanTree(TreeZone: TCnBaseZone); virtual; //ǰ�����
    procedure BackwardScanTree(TreeZone: TCnBaseZone); virtual;//�������
    procedure MiddleScanTree(TreeZone: TCnBaseZone); virtual;  //�������
    procedure ScanTreeZone(TreeZone: TCnBaseZone); virtual;//��ɨ�赽һ���ڵ�ʱ�������������
  public
    constructor Create(TreeZone: TCnTreeZoneClass); virtual;
    destructor Destroy; override;
    function AddChildZone(TreeZone, NewZone: TCnBaseZone): TCnBaseZone; virtual;
    function AddNextSibling(TreeZone, NewZone: TCnBaseZone): TCnBaseZone; virtual;
    function AddPrevSibling(TreeZone, NewZone: TCnBaseZone): TCnBaseZone; virtual;
    function AddParentZone(TreeZone, NewZone: TCnBaseZone): TCnBaseZone; virtual;
    procedure RemoveChildZone(TreeZone: TCnBaseZone); virtual;
    procedure RemoveNextSibling(TreeZone: TCnBaseZone); virtual;
    procedure RemovePrevSibling(TreeZone: TCnBaseZone); virtual;
    procedure RemoveParentZone(TreeZone: TCnBaseZone); virtual;
    property TreeZoneClass: TCnTreeZoneClass read FTreeZoneClass write FTreeZoneClass;
    property TopTreeZone: TCnBaseZone read FTopTreeZone write FTopTreeZone;
    property CurrTreeZone: TCnBaseZone read FCurrTreeZone write FCurrTreeZone;
    property ScanZoneProc: TScanTreeZoneProc read FScanZoneProc write FScanZoneProc;
  end;

  TCnBaseGetFormEventComponent = class(TComponent)
  private
    { �̳���TCustomForm }
    FOldOnActivate: TNotifyEvent;
    FOldOnClose: TCloseEvent;
    FOldOnCloseQuery: TCloseQueryEvent;
    FOldOnCreate: TNotifyEvent;
    FOldOnDeactivate: TNotifyEvent;
    FOldOnDestroy: TNotifyEvent;
    FOldOnHelp: THelpEvent;
    FOldOnHide: TNotifyEvent;
    FOldOnPaint: TNotifyEvent;
    FOldOnShortCut: TShortCutEvent;
    FOldOnShow: TNotifyEvent;
    { �̳���TWinControl }
    FOldOnDockDrop: TDockDropEvent;
    FOldOnDockOver: TDockOverEvent;
    FOldOnExit: TNotifyEvent;
    FOldOnGetSiteInfo: TGetSiteInfoEvent;
    FOldOnKeyDown: TKeyEvent;
    FOldOnKeyPress: TKeyPressEvent;
    FOldOnKeyUp: TKeyEvent;
    FOldOnMouseWheel: TMouseWheelEvent;
    FOldOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOldOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOldOnUndock: TUnDockEvent;
    { �̳���TControl }
    FOldOnCanResize: TCanResizeEvent;
    FOldOnClick: TNotifyEvent;
    FOldOnConstrainedResize: TConstrainedResizeEvent;
    FOldOnContextPopup: TContextPopupEvent;
    FOldOnDblClick: TNotifyEvent;
    FOldOnDragDrop: TDragDropEvent;
    FOldOnDragOver: TDragOverEvent;
    FOldOnEndDock: TEndDragEvent;
    FOldOnEndDrag: TEndDragEvent;
    FOldOnMouseDown: TMouseEvent;
    FOldOnMouseMove: TMouseMoveEvent;
    FOldOnMouseUp: TMouseEvent;
    FOldOnResize: TNotifyEvent;
    FOldOnStartDock: TStartDockEvent;

    FParentForm: TForm;

    FOldWindowProc: TWndMethod;
  protected
    { �̳���TCustomForm }
    procedure DoFormOnActivate(Sender: TObject); virtual;
    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCloseQuery(Sender: TObject;
      var CanClose: Boolean); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;
    procedure DoFormOnDeactivate(Sender: TObject); virtual;
    procedure DoFormOnDestroy(Sender: TObject); virtual;
    function DoFormOnHelp(Command: Word; Data: TCnNativeInt;
      var CallHelp: Boolean): Boolean;
    procedure DoFormOnHide(Sender: TObject); virtual;
    procedure DoFormOnPaint(Sender: TObject); virtual;
    procedure DoFormOnShortCut(var Msg: TWMKey; var Handled: Boolean); virtual;
    procedure DoFormOnShow(Sender: TObject); virtual;
    procedure DoFormOnDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer); virtual;
    { �̳���TWinControl }
    procedure DoFormOnDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean); virtual;
    procedure DoFormOnExit(Sender: TObject); virtual;
    procedure DoFormOnGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure DoFormOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure DoFormOnKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure DoFormOnKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure DoFormOnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnUndock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean); virtual;
    { �̳���TControl }
    procedure DoFormOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean); virtual;
    procedure DoFormOnClick(Sender: TObject); virtual;
    procedure DoFormOnConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer); virtual;
    procedure DoFormOnContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean); virtual;
    procedure DoFormOnDblClick(Sender: TObject); virtual;
    procedure DoFormOnDragDrop(Sender, Source: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure DoFormOnEndDock(Sender, Target: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnEndDrag(Sender, Target: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoFormOnMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure DoFormOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoFormOnResize(Sender: TObject); virtual;
    procedure DoFormOnStartDock(Sender: TObject;
      var DragObject: TDragDockObject); virtual;

    { ������ĸ��ؼ���WindowProc��Ϣ���麯�� }
    procedure WindowProc(var Message: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ParentForm: TForm read FParentForm;
  end;

implementation

{ TCnBaseZone }

constructor TCnBaseZone.Create(BaseTree: TCnBaseTree);
begin
  FBaseTree := BaseTree;
  ChildZone := nil;
  NextSibling := nil;
  PrevSibling := nil;
  ParentZone := nil;
end;

function TCnBaseZone.CreateChildZone: TCnBaseZone;
begin
  Result := nil;
end;

function TCnBaseZone.GetChildCount: Integer;
var AZone: TCnBaseZone;
begin
  Result := 0;
  if FChildZone <> nil then
  begin
    Inc(Result);
    AZone := FChildZone;
    while AZone.NextSibling <> nil do
    begin
      AZone := AZone.NextSibling;
      Inc(Result);
    end;
  end;
end;

function TCnBaseZone.GetChildZone(Index: Word): TCnBaseZone;
begin
  Result := ChildZone;
  while Index > 0 do
  begin
    Result := Result.NextSibling;
    Dec(Index);
  end;
end;

function TCnBaseZone.GetNextSibingCount: Integer;
var AZone: TCnBaseZone;
begin
  Result := 0;
  AZone := Self;
  while AZone.NextSibling <> nil do
  begin
    AZone := AZone.NextSibling;
    Inc(Result);
  end;
end;

function TCnBaseZone.GetParentZone: TCnBaseZone;
var TreeZone: TCnBaseZone;
begin
  TreeZone := Self;
  while (TreeZone <> nil) and (TreeZone.ParentZone = nil)
    and (TreeZone.PrevSibling <> nil) do
    TreeZone := TreeZone.PrevSibling;
  if TreeZone <> nil then Result := TreeZone.ParentZone
  else Result := nil;
end;

function TCnBaseZone.GetPrevSibingCount: Integer;
var AZone: TCnBaseZone;
begin
  Result := 0;
  AZone := Self;
  while AZone.PrevSibling <> nil do
  begin
    AZone := AZone.PrevSibling;
    Inc(Result);
  end;
end;

{ TCnBaseTree }

function TCnBaseTree.AddChildZone(TreeZone, NewZone: TCnBaseZone): TCnBaseZone;
begin
  if TreeZone.ChildZone <> nil then
  begin
    Result := AddNextSibling(TreeZone.ChildZone, NewZone);
  end else
  begin
    if NewZone = nil then
      Result := FTreeZoneClass.Create(Self)
    else Result := NewZone;
    TreeZone.ChildZone := Result;
    Result.ParentZone := TreeZone;
  end;
end;

function TCnBaseTree.AddNextSibling(TreeZone, NewZone: TCnBaseZone): TCnBaseZone;
begin
  while TreeZone.NextSibling <> nil do
    TreeZone := TreeZone.NextSibling;
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else Result := NewZone;
  TreeZone.NextSibling := Result;
  Result.PrevSibling := TreeZone;
  Result.ParentZone := TreeZone.ParentZone;
end;

function TCnBaseTree.AddParentZone(TreeZone, NewZone: TCnBaseZone): TCnBaseZone;
begin
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else Result := NewZone;
  while TreeZone.PrevSibling <> nil do
    TreeZone := TreeZone.PrevSibling;
  if TreeZone.ParentZone <> nil then
  begin
    TreeZone.ParentZone.ChildZone := Result;
  end else
  begin
    TopTreeZone := Result;
  end;
  Result.ParentZone := TreeZone.ParentZone;
  TreeZone.ParentZone := Result;
end;

function TCnBaseTree.AddPrevSibling(TreeZone, NewZone: TCnBaseZone): TCnBaseZone;
begin
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else Result := NewZone;
  if TreeZone.PrevSibling <> nil then
  begin
    TreeZone.PrevSibling.NextSibling := Result;
    Result.PrevSibling := TreeZone.PrevSibling;
    TreeZone.PrevSibling := Result;
    Result.NextSibling := TreeZone;
    Result.ParentZone := TreeZone.ParentZone;
  end else
  begin
    if TreeZone.ParentZone <> nil then
    begin
      TreeZone.ParentZone.ChildZone := Result;
    end else
    begin
      TopTreeZone := Result;
    end;
    Result.ParentZone := TreeZone.ParentZone;
    Result.NextSibling := TreeZone;
    TreeZone.PrevSibling := Result;
//    TreeZone.ParentZone := nil;
  end;
end;

constructor TCnBaseTree.Create(TreeZone: TCnTreeZoneClass);
begin
  FTreeZoneClass := TreeZone;
  FTopTreeZone := FTreeZoneClass.Create(Self);
  FCurrTreeZone := FTopTreeZone;
  FScanZoneProc := nil;
  FScanAction := snNone;
end;

destructor TCnBaseTree.Destroy;
begin
  FScanAction := snDeleted;
  BackwardScanTree(TopTreeZone);
  FScanAction := snNone;
  inherited Destroy;
end;

procedure TCnBaseTree.ForwardScanTree(TreeZone: TCnBaseZone);
begin
  if TreeZone <> nil then
  begin
    ScanTreeZone(TreeZone);
    ForwardScanTree(TreeZone.ChildZone);
    ForwardScanTree(TreeZone.NextSibling);
  end;
end;

procedure TCnBaseTree.MiddleScanTree(TreeZone: TCnBaseZone);
begin
  if TreeZone <> nil then
  begin
    MiddleScanTree(TreeZone.ChildZone);
    ScanTreeZone(TreeZone);
    MiddleScanTree(TreeZone.NextSibling);
  end;
end;

procedure TCnBaseTree.BackwardScanTree(TreeZone: TCnBaseZone);
begin
  if TreeZone <> nil then
  begin
    BackwardScanTree(TreeZone.ChildZone);
    BackwardScanTree(TreeZone.NextSibling);
    ScanTreeZone(TreeZone);
  end;
end;

procedure TCnBaseTree.ScanTreeZone(TreeZone: TCnBaseZone);
begin
  if Assigned(FScanZoneProc) then
    FScanZoneProc(TreeZone);
  if FScanAction = snDeleted then
   TreeZone.Free;
end;

procedure TCnBaseTree.RemoveChildZone(TreeZone: TCnBaseZone);
begin
  if TreeZone.ChildZone <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.ChildZone);
    FScanAction := snNone;
  end;
end;

procedure TCnBaseTree.RemoveNextSibling(TreeZone: TCnBaseZone);
begin
  if TreeZone.NextSibling <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.NextSibling);
    FScanAction := snNone;
  end;
end;

procedure TCnBaseTree.RemoveParentZone(TreeZone: TCnBaseZone);
begin
end;

procedure TCnBaseTree.RemovePrevSibling(TreeZone: TCnBaseZone);
begin
  if TreeZone.PrevSibling <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.PrevSibling);
    FScanAction := snNone;
  end;
end;

{ TCnBaseGetFormEventComponent }

constructor TCnBaseGetFormEventComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentForm := TForm(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    { �̳���TCustomForm }
    FOldOnActivate := FParentForm.OnActivate;
    FParentForm.OnActivate := DoFormOnActivate;
    FOldOnClose := FParentForm.OnClose;
    FParentForm.OnClose := DoFormOnClose;
    FOldOnCloseQuery := FParentForm.OnCloseQuery;
    FParentForm.OnCloseQuery := DoFormOnCloseQuery;
    FOldOnCreate := FParentForm.OnCreate;
    FParentForm.OnCreate := DoFormOnCreate;
    FOldOnDeactivate := FParentForm.OnDeactivate;
    FParentForm.OnDeactivate := DoFormOnDeactivate;
    FOldOnDestroy := FParentForm.OnDestroy;
    FParentForm.OnDestroy := DoFormOnDestroy;
    FOldOnHelp := FParentForm.OnHelp;
    FParentForm.OnHelp := DoFormOnHelp;
    FOldOnHide := FParentForm.OnHide;
    FParentForm.OnHide := DoFormOnHide;
    FOldOnPaint := FParentForm.OnPaint;
    FParentForm.OnPaint := DoFormOnPaint;
    FOldOnShortCut := FParentForm.OnShortCut;
    FParentForm.OnShortCut := DoFormOnShortCut;
    FOldOnShow := FParentForm.OnShow;
    FParentForm.OnShow := DoFormOnShow;
    { �̳���TWinControl }
    FOldOnDockDrop := FParentForm.OnDockDrop;
    FParentForm.OnDockDrop := DoFormOnDockDrop;
    FOldOnDockOver := FParentForm.OnDockOver;
    FParentForm.OnDockOver := DoFormOnDockOver;
  //  FOldOnExit := FParentForm.OnExit;
    FOldOnGetSiteInfo := FParentForm.OnGetSiteInfo;
    FParentForm.OnGetSiteInfo := DoFormOnGetSiteInfo;
    FOldOnKeyDown := FParentForm.OnKeyDown;
    FParentForm.OnKeyDown := DoFormOnKeyDown;
    FOldOnKeyPress := FParentForm.OnKeyPress;
    FParentForm.OnKeyPress := DoFormOnKeyPress;
    FOldOnKeyUp := FParentForm.OnKeyUp;
    FParentForm.OnKeyUp := DoFormOnKeyUp;
    FOldOnMouseWheel := FParentForm.OnMouseWheel;
    FParentForm.OnMouseWheel := DoFormOnMouseWheel;
    FOldOnMouseWheelDown := FParentForm.OnMouseWheelDown;
    FParentForm.OnMouseWheelDown := DoFormOnMouseWheelDown;
    FOldOnMouseWheelUp := FParentForm.OnMouseWheelUp;
    FParentForm.OnMouseWheelUp := DoFormOnMouseWheelUp;
    FOldOnUndock := FParentForm.OnUnDock;
    FParentForm.OnUnDock := DoFormOnUnDock;
    { �̳���TControl }
    FOldOnCanResize := FParentForm.OnCanResize;
    FParentForm.OnCanResize := DoFormOnCanResize;
    FOldOnClick := FParentForm.OnClick;
    FParentForm.OnClick := DoFormOnClick;
    FOldOnConstrainedResize := FParentForm.OnConstrainedResize;
    FParentForm.OnConstrainedResize := DoFormOnConstrainedResize;
    FOldOnContextPopup := FParentForm.OnContextPopup;
    FParentForm.OnContextPopup := DoFormOnContextPopup;
    FOldOnDblClick := FParentForm.OnDblClick;
    FParentForm.OnDblClick := DoFormOnDblClick;
    FOldOnDragDrop := FParentForm.OnDragDrop;
    FParentForm.OnDragDrop := DoFormOnDragDrop;
    FOldOnDragOver := FParentForm.OnDragOver;
    FParentForm.OnDragOver := DoFormOnDragOver;
    FOldOnEndDock := FParentForm.OnEndDock;
    FParentForm.OnEndDock := DoFormOnEndDock;
  //  FOldOnEndDrag := FParentForm.OnEndDrag;
    FOldOnMouseDown := FParentForm.OnMouseDown;
    FParentForm.OnMouseDown := DoFormOnMouseDown;
    FOldOnMouseMove := FParentForm.OnMouseMove;
    FParentForm.OnMouseMove := DoFormOnMouseMove;
    FOldOnMouseUp := FParentForm.OnMouseUp;
    FParentForm.OnMouseUp := DoFormOnMouseUp;
    FOldOnResize := FParentForm.OnResize;
    FParentForm.OnResize := DoFormOnResize;
    FOldOnStartDock := FParentForm.OnStartDock;
    FParentForm.OnStartDock := DoFormOnStartDock;

    { �����ϵĴ��ڹ��� }
    FOldWindowProc := FParentForm.WindowProc;
    { ���ش��ڹ��� }
    FParentForm.WindowProc := WindowProc;
  end;
end;

destructor TCnBaseGetFormEventComponent.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if @FOldWindowProc <> nil then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
    { �̳���TCustomForm }
    FParentForm.OnActivate := FOldOnActivate;
    FOldOnActivate := nil;
    FParentForm.OnClose := FOldOnClose;
    FOldOnClose := nil;
    FParentForm.OnCloseQuery := FOldOnCloseQuery;
    FOldOnCloseQuery := nil;
    FParentForm.OnCreate := FOldOnCreate;
    FOldOnCreate := nil;
    FParentForm.OnDeactivate := FOldOnDeactivate;
    FOldOnDeactivate := nil;
    FParentForm.OnDestroy := FOldOnDestroy;
    FOldOnDestroy := nil;
    FParentForm.OnHelp := FOldOnHelp;
    FOldOnHelp := nil;
    FParentForm.OnHide := FOldOnHide;
    FOldOnHide := nil;
    FParentForm.OnPaint := FOldOnPaint;
    FOldOnPaint := nil;
    FParentForm.OnShortCut := FOldOnShortCut;
    FOldOnShortCut := nil;
    FParentForm.OnShow := FOldOnShow;
    FOldOnShow := nil;
    { �̳���TWinControl }
    FParentForm.OnDockDrop := FOldOnDockDrop;
    FOldOnDockDrop := nil;
    FParentForm.OnDockOver := FOldOnDockOver;
    FOldOnDockOver := nil;
//    FParentForm.OnExit := FOldOnExit;
//    FOldOnExit := nil;
    FParentForm.OnGetSiteInfo := FOldOnGetSiteInfo;
    FOldOnGetSiteInfo := nil;
    FParentForm.OnKeyDown := FOldOnKeyDown;
    FOldOnKeyDown := nil;
    FParentForm.OnKeyPress := FOldOnKeyPress;
    FOldOnKeyPress := nil;
    FParentForm.OnKeyUp := FOldOnKeyUp;
    FOldOnKeyUp := nil;
    FParentForm.OnMouseWheel := FOldOnMouseWheel;
    FOldOnMouseWheel := nil;
    FParentForm.OnMouseWheelDown := FOldOnMouseWheelDown;
    FOldOnMouseWheelDown := nil;
    FParentForm.OnMouseWheelUp := FOldOnMouseWheelUp;
    FOldOnMouseWheelUp := nil;
    FParentForm.OnUndock := FOldOnUndock;
    FOldOnUndock := nil;
    { �̳���TControl }
    FParentForm.OnCanResize := FOldOnCanResize;
    FOldOnCanResize := nil;
    FParentForm.OnClick := FOldOnClick;
    FOldOnClick := nil;
    FParentForm.OnConstrainedResize := FOldOnConstrainedResize;
    FOldOnConstrainedResize := nil;
    FParentForm.OnContextPopup := FOldOnContextPopup;
    FOldOnContextPopup := nil;
    FParentForm.OnDblClick := FOldOnDblClick;
    FOldOnDblClick := nil;
    FParentForm.OnDragDrop := FOldOnDragDrop;
    FOldOnDragDrop := nil;
    FParentForm.OnDragOver := FOldOnDragOver;
    FOldOnDragOver := nil;
    FParentForm.OnEndDock := FOldOnEndDock;
    FOldOnEndDock := nil;
//    FParentForm.OnEndDrag := FOldOnEndDrag;
//    FOldOnEndDrag := nil;
    FParentForm.OnMouseDown := FOldOnMouseDown;
    FOldOnMouseDown := nil;
    FParentForm.OnMouseMove := FOldOnMouseMove;
    FOldOnMouseMove := nil;
    FParentForm.OnMouseUp := FOldOnMouseUp;
    FOldOnMouseUp := nil;
    FParentForm.OnResize := FOldOnResize;
    FOldOnResize := nil;
    FParentForm.OnStartDock := FOldOnStartDock;
    FOldOnStartDock := nil;

    FParentForm := nil;

  end;
  inherited;
end;

procedure TCnBaseGetFormEventComponent.DoFormOnActivate(Sender: TObject);
begin
  if Assigned(FOldOnActivate) then
    FOldOnActivate(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnCanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  if Assigned(FOldOnCanResize) then
    FOldOnCanResize(Sender, NewWidth, NewHeight, Resize);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnClick(Sender: TObject);
begin
  if Assigned(FOldOnClick) then
    FOldOnClick(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOldOnCloseQuery) then
    FOldOnCloseQuery(Sender, CanClose);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnConstrainedResize(
  Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  if Assigned(FOldOnConstrainedResize) then
    FOldOnConstrainedResize(Sender, MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOldOnContextPopup) then
    FOldOnContextPopup(Sender, MousePos, Handled);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnCreate(Sender: TObject);
begin
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDblClick(Sender: TObject);
begin
  if Assigned(FOldOnDblClick) then
    FOldOnDblClick(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDeactivate(Sender: TObject);
begin
  if Assigned(FOldOnDeactivate) then
    FOldOnDeactivate(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDestroy(Sender: TObject);
begin
  if Assigned(FOldOnDestroy) then
    FOldOnDestroy(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if Assigned(FOldOnDockDrop) then
    FOldOnDockDrop(Sender, Source, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if Assigned(FOldOnDockOver) then
    FOldOnDockOver(Sender, Source,  X, Y, State, Accept);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnDragDrop) then
    FOldOnDragDrop(Sender, Source, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOldOnDragOver) then
    FOldOnDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnEndDock(Sender,
  Target: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnEndDock) then
    FOldOnEndDock(Sender, Target, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnEndDrag) then
    FOldOnEndDrag(Sender, Target, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnExit(Sender: TObject);
begin
  if Assigned(FOldOnExit) then
    FOldOnExit(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  if Assigned(FOldOnGetSiteInfo) then
    FOldOnGetSiteInfo(Sender, DockClient, InfluenceRect, MousePos, CanDock);
end;

function TCnBaseGetFormEventComponent.DoFormOnHelp(Command: Word;
  Data: TCnNativeInt; var CallHelp: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOldOnHelp) then
    Result := FOldOnHelp(Command, Data, CallHelp);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnHide(Sender: TObject);
begin
  if Assigned(FOldOnHide) then
    FOldOnHide(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOldOnKeyDown) then
    FOldOnKeyDown(Sender, Key, Shift);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Assigned(FOldOnKeyPress) then
    FOldOnKeyPress(Sender, Key);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOldOnKeyUp) then
    FOldOnKeyUp(Sender, Key, Shift);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseDown) then
    FOldOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseMove) then
    FOldOnMouseMove(Sender, Shift, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseUp) then
    FOldOnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheel) then
    FOldOnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseWheelDown(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheelDown) then
    FOldOnMouseWheelDown(Sender, Shift, MousePos, Handled);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheelUp) then
    FOldOnMouseWheelUp(Sender, Shift, MousePos, Handled);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnPaint(Sender: TObject);
begin
  if Assigned(FOldOnPaint) then
    FOldOnPaint(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnResize(Sender: TObject);
begin
  if Assigned(FOldOnResize) then
    FOldOnResize(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  if Assigned(FOldOnShortCut) then
    FOldOnShortCut(Msg, Handled);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnShow(Sender: TObject);
begin
  if Assigned(FOldOnShow) then
    FOldOnShow(Sender);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  if Assigned(FOldOnStartDock) then
    FOldOnStartDock(Sender, DragObject);
end;

procedure TCnBaseGetFormEventComponent.DoFormOnUndock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  if Assigned(FOldOnUndock) then
    FOldOnUndock(Sender, Client, NewTarget, Allow);
end;

procedure TCnBaseGetFormEventComponent.WindowProc(var Message: TMessage);
begin
  if Assigned(FOldWindowProc) then
    FOldWindowProc(Message);
end;

end.


