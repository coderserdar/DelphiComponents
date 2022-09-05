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
{       ��������Visual C++��ͣ�����                    }
{       CnVCDockStyle ��Ԫ                              }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnVCDockStyle;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ���������Visual C++��ͣ�����ĵ�Ԫ 
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
  Windows, Classes, Controls, Math, Messages, Graphics, ExtCtrls, Consts,
  CnDockFormControl, CnDockSupportControl, CnDockTree, CnConsts, CnCompConsts;

const
  { ��չ��ť }
  HTEXPAND  = 31;

  VCDefaultGrabberSize   = 15;
  VCDefaultSplitterWidth = 4;
  VCDefaultBorderWidth = 4;

type
  { VCƽ�̷�������ѡ���� }
  TCnVCConjoinServerOption = class(TCnBasicConjoinServerOption)
  private
    FBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);         // �߿�Ŀ��
  protected
    procedure ResetDockControlOption; override;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 4;
  end;

  { VC��ҳ��������ѡ���� }
  TCnVCTabServerOption = class(TCnBasicTabServerOption);

  TCnVCDockStyle = class(TCnAdvDockStyle)
  private
    FOldEachOtherDock: Boolean;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure FormGetDockEdge(DockClient: TCnDockClient; Source: TCnDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;
    procedure FormStartDock(DockClient: TCnDockClient;
      var Source: TCnDragDockObject); override;
    procedure AssignConjoinServerOption(APanel: TCnCustomDockPanel); override;
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean; override;
    function GetControlName: string; override;
    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TCnDockBaseControl); override;
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TCnVCDockSplitter = class(TCnDockSplitter)
  private
    FOldSize: Integer;
  protected
    function DoCanResize(var NewSize: Integer): Boolean; override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCnVCDockPanel = class(TCnAdvDockPanel)
  protected
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function GetDockEdge(MousePos: TPoint): TAlign; override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
  public
  end;

  TCnVCConjoinPanel = class(TCnConjoinPanel);
  TCnVCTabPageControl = class(TCnAdvTabPageControl);

  TZoneSizeStyle = (zssMinimum, zssNormal, zssMaximum);
                   {���״̬, ����״̬,  ��С��״̬}

  TCnVCDockZone = class(TCnAdvDockZone)
  private
    FZoneSizeStyle: TZoneSizeStyle;
    FExpandBtnDown: Boolean;
    procedure DoSetChildSizeStyle(ZoneSizeStyle: TZoneSizeStyle);
  public
    constructor Create(Tree: TCnDockTree); override;
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
    procedure InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean); override;
    procedure SetZoneSize(Size: Integer; Show: Boolean); override;
    property ZoneSizeStyle: TZoneSizeStyle read FZoneSizeStyle write FZoneSizeStyle;
    property ExpandBtnDown: Boolean read FExpandBtnDown write FExpandBtnDown;
  end;

  TCnVCDockTree = class(TCnAdvDockTree)
  private
    FExpandBtnZone: TCnVCDockZone;
  protected
    procedure WindowProc(var Message: TMessage); override;
    { ------------------------------------------------------------------------ }
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    { ------------------------------------------------------------------------ }
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean); override;
    function GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign; override;
    { ------------------------------------------------------------------------ }
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoOtherHint(Zone: TCnDockZone;
      HTFlag: Integer; var HintStr: string); override;

    procedure CustomSaveZone(Stream: TStream;
      Zone: TCnDockZone); override;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TCnDockZone); override;
    { ------------------------------------------------------------------------ }
    procedure CalcSplitterPos; override;
    { ���DropOnControl�ĸ�Zone }
    function GetDropOnZone(Orient: TDockOrientation; DockRect: TRect; var DropAlign: TAlign): TCnDockZone; virtual;
    { ���DropOnControl }
    function GetDropOnControl(Orient: TDockOrientation; Zone: TCnDockZone; DockRect: TRect;
      var DropAlign: TAlign; Control: TControl): TControl; virtual;

    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;

    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    { ------------------------------------------------------------------------ }
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
      InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TControl;
      const ARect: TRect); override;
    procedure DrawDockSiteRect; override;
    procedure DrawSplitterRect(
      const ARect: TRect); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    procedure RemoveControl(Control: TControl); override;
    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean = True); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure ResetZoneSizeStyle(Parent: TCnDockZone;
      ZoneSizeStyle: TZoneSizeStyle; Exclude: TCnDockZone);
    procedure ScaleZone(Zone: TCnDockZone); override;
    procedure ScaleChildZone(Zone: TCnDockZone); override;
    procedure ScaleSiblingZone(Zone: TCnDockZone); override;

    procedure ShiftZone(Zone: TCnDockZone); override;
    procedure SplitterMouseUp; override;

  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
  end;

  TCnVCDragDockObject = class(TCnDragDockObject)
  private
    FDockOverBrush: TBrush;         //ͣ��Ԥ���Ļ���
    FDockOverFrameWidth: Integer;   //ͣ��Ԥ���Ŀ�ܿ��
    FCurrState,                     //��ǰ��DockOver��State;
    FOldState: TDragState;          //ǰһ��DockOver��State;
    FOldTarget: Pointer;            //ǰһ��ͣ��������
    procedure SetOldState(const Value: TDragState);
    procedure SetCurrState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); override;
    procedure SetDefaultBrushStyle; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): HWND; override;
    function GetDropCtl: TControl; override;
    property CurrState: TDragState read FCurrState write SetCurrState;
    property OldState: TDragState read FOldState write SetOldState;
    property OldTarget: Pointer read FOldTarget write FOldTarget;
    property DockOverFrameWidth: Integer read FDockOverFrameWidth write FDockOverFrameWidth;
    property DockOverBrush: TBrush read FDockOverBrush;
  end;

implementation

uses Forms, SysUtils, CnDockSupportProc, CnDockGlobal;

const DefaultFrameWidth = 3;
  DefaultDockOverFrameWidth = 1;
  DefaultDockOverBrushStyle = bsSolid;

                     { ���� }           { �Ƿ��ں��� }
  DropAlignArr: array[TDockOrientation, Boolean] of TAlign =
    ((alNone, alNone), (alTop, alBottom), (alLeft, alRight));

type
  TCnTempWinControl = class(TWinControl);

{ TCnVCDockStyle }

procedure TCnVCDockStyle.AssignConjoinServerOption(
  APanel: TCnCustomDockPanel);
begin
  inherited;
  if ConjoinServerOption is TCnVCConjoinServerOption then
    APanel.CnDockManager.BorderWidth := TCnVCConjoinServerOption(ConjoinServerOption).BorderWidth;
end;

function TCnVCDockStyle.CanSetEachOtherDocked(
  ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := False;
end;

constructor TCnVCDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnDockPanelClass := TCnVCDockPanel;
  CnDockSplitterClass := TCnVCDockSplitter;
  CnConjoinPanelClass := TCnVCConjoinPanel;
  CnTabDockClass := TCnVCTabPageControl;
  CnDockPanelTreeClass := TCnVCDockTree;
  CnDockPanelZoneClass := TCnVCDockZone;
  CnConjoinPanelTreeClass := TCnVCDockTree;
  CnConjoinPanelZoneClass := TCnVCDockZone;
  CnConjoinServerOptionClass := TCnVCConjoinServerOption;
  CnTabServerOptionClass := TCnVCTabServerOption;
end;

procedure TCnVCDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnVCConjoinServerOption.Create(Self);
end;

procedure TCnVCDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnVCTabServerOption.Create(Self);
end;

destructor TCnVCDockStyle.Destroy;
begin
  inherited Destroy;
end;

procedure TCnVCDockStyle.FormGetDockEdge(DockClient: TCnDockClient;
  Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TCnVCDockStyle.FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  Source := TCnVCDragDockObject.Create(DockClient.ParentForm);
end;

procedure TCnVCDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVCDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnVCDockStyleComment;
end;

function TCnVCDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeVCStyle, [inherited GetControlName]);
end;

procedure TCnVCDockStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TCnDockBaseControl);
var ADockClient: TCnDockClient;
begin
  if DockBaseControl is TCnDockClient then
  begin
    ADockClient := TCnDockClient(DockBaseControl);
    if IsCreate then
    begin
      FOldEachOtherDock := ADockClient.EachOtherDock;
      ADockClient.EachOtherDock := False;
      ADockClient.DirectDrag := True;
    end else
    begin
      ADockClient.EachOtherDock := FOldEachOtherDock;
    end;
  end;
end;

{ TCnVCDockPanel }

procedure TCnVCDockPanel.CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer);
begin
  if Source is TCnVCDragDockObject then
  begin
    TCnVCDragDockObject(Source).CurrState := dsDragEnter;
    TCnVCDragDockObject(Source).OldState := dsDragEnter;
  end;

  if Source.DropOnControl <> Source.Control then
    inherited CustomDockDrop(Source, X, Y);
end;

procedure TCnVCDockPanel.CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var DropAlign: TAlign;
  VCSource: TCnVCDragDockObject;
  SysCaptionHeight: Integer; //ϵͳ�ı������ĸ߶�
  PanelScreenRect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);

  if Source is TCnVCDragDockObject then
  begin
    VCSource := TCnVCDragDockObject(Source);
    VCSource.OldState := VCSource.CurrState;
    VCSource.CurrState := State;
  end;

  if State = dsDragMove then
  begin
    { ���TCnDragDockObject��DropOnControl��DropAlign }
    DropAlign := Source.DropAlign;
    Source.DropOnControl := CnDockManager.GetDockEdge(Source.EraseDockRect, Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;

    SysCaptionHeight := Integer(Source.Control.Floating) * Cn_GetSysCaptionHeight;

    { ӳ������DockPanel������λ�� }
    PanelScreenRect := BoundsRect;
    MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

    if ((Align in [alTop, alBottom]) and
      (Source.DockRect.Right = PanelScreenRect.Right) and
      (Source.DockRect.Left = PanelScreenRect.Left)) or
      ((Align in [alLeft, alRight]) and
      (Source.DockRect.Top = PanelScreenRect.Top) and
      (Source.DockRect.Bottom = PanelScreenRect.Bottom)) then
        Exit;

    if ((Source.DropOnControl <> nil) and (Source.DropOnControl <> Source.Control)) and
      (Source.DropOnControl.HostDockSite <> Source.Control.HostDockSite) then
    begin
      if (DropAlign in [alTop, alBottom]) then
      begin
        if ((Source.Control.DockOrientation = doVertical) or (Source.Control.HostDockSite = nil)) then
          Source.DockRect.Bottom := Source.DockRect.Top + Source.Control.UnDockHeight - SysCaptionHeight;
      end else
      if (DropAlign in [alLeft, alRight]) then
      begin
        if (Source.Control.DockOrientation = doHorizontal) or (Source.Control.HostDockSite = nil) then
          Source.DockRect.Right := Source.DockRect.Left + Source.Control.UnDockWidth - SysCaptionHeight;
      end;
    end;
  end;
end;

function TCnVCDockPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
var DropAlign: TAlign;
  MousePos: TPoint;
begin
  if (NewTarget = nil) or (NewTarget = Client.HostDockSite) then
  begin
    DropAlign := Source.DropAlign;
    Source.DropOnControl := CnDockManager.GetDockEdge(
      Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;
  end;
  MousePos := ScreenToClient(Source.DragPos);
  if ((Align in [alTop, alBottom]) and ((0 > MousePos.X) or (Width < MousePos.X)))
    or ((Align in [alLeft, alRight]) and ((0 > MousePos.Y) or (Height < MousePos.Y)))
    or (Source.CtrlDown) or Source.Floating then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else if (Source.DropOnControl <> Source.Control) then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else Result := True;
end;

procedure TCnVCDockPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle, InfluenceRect);
  with Client, CnGlobalDockPresident.DragObject do
  begin
    case Self.Align of
    alTop:
      begin
        if MousePos.y >= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            Cn_GetMinOffset(TBDockHeight, Height, MouseDeltaY));
      end;
    alBottom:
      begin
        if MousePos.y <= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            Cn_GetMinOffset(TBDockHeight, Height, 1 - MouseDeltaY));
      end;
    alLeft:
      begin
        if MousePos.x >= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            Cn_GetMinOffset(LRDockWidth, Width, MouseDeltaX), 0);
      end;
    alRight:
      begin
        if MousePos.x <= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            Cn_GetMinOffset(LRDockWidth, Width, 1 - MouseDeltaX), 0);
      end;
    end;
  end;
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

function TCnVCDockPanel.GetDockEdge(MousePos: TPoint): TAlign;
begin
  Result := inherited GetDockEdge(MousePos);
end;

procedure TCnVCDockPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
var ARect: TRect;
  BorderWidth: Integer;      //�߿���
  GrabberSize: Integer;      //���ִ�С
  PanelScreenRect: TRect;
  SysCaptionHeight: Integer; //ϵͳ�ı������ĸ߶�
  DockSize: Integer;

  procedure GetDockSize;
  begin
    if Align in [alLeft, alRight] then
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockHeight
      else
        DockSize := Source.Control.Height;
    end else
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockWidth
      else
        DockSize := Source.Control.Width;
    end;
  end;

  procedure SetMaxDockSize(Align: TAlign);
  begin
    if Align in [alLeft, alRight] then
    begin
      if ARect.Bottom - ARect.Top >= PanelScreenRect.Bottom - PanelScreenRect.Top then
      begin
        ARect.Top := PanelScreenRect.Top;
        ARect.Bottom := PanelScreenRect.Bottom;
      end;
    end else
    begin
      if ARect.Right - ARect.Left >= PanelScreenRect.Right - PanelScreenRect.Left then
      begin
        ARect.Left := PanelScreenRect.Left;
        ARect.Right := PanelScreenRect.Right;
      end;
    end;
  end;

begin
  { ���Source.Control��HostDockSite�ı߿��ȺͰ��ִ�С }
  if Source.Control.HostDockSite is TCnCustomDockPanel then
  begin
    BorderWidth := TCnCustomDockPanel(Source.Control.HostDockSite).CnDockManager.BorderWidth;
    GrabberSize := TCnCustomDockPanel(Source.Control.HostDockSite).CnDockManager.GrabberSize;
  end else
  begin
    BorderWidth := 0;
    GrabberSize := 0;
  end;

  { ӳ������DockPanel������λ�� }
  PanelScreenRect := BoundsRect;
  MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

  SysCaptionHeight := Integer(Source.Control.Floating) * Cn_GetSysCaptionHeight;

  GetDockSize;

  with Source.Control do
  begin
    case Self.Align of
      alTop:
      begin
        ARect.TopLeft := Self.ClientToScreen(Point(0, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, TBDockHeight));
        ARect.Top := ARect.Top + Y -
          Cn_GetMinOffset(TBDockHeight, Height + 2 * BorderWidth, Source.MouseDeltaY);
        ARect.Bottom := ARect.Top + TBDockHeight;
        if (Self.Height > 0) and (ARect.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
          (ARect.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
        begin
          ARect.Left := ARect.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
          ARect.Right := ARect.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alBottom:
      begin
        ARect.TopLeft := Self.ClientToScreen(
          Point(0, -TBDockHeight));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, 0));
        ARect.Top := ARect.Top + Y +
          Cn_GetMinOffset(TBDockHeight, Height + 2 * BorderWidth, 1 - Source.MouseDeltaY);
        ARect.Bottom := ARect.Top + TBDockHeight;
        if (Self.Height > 0) and (ARect.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
          (ARect.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
        begin
          ARect.Left := ARect.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
          ARect.Right := ARect.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alLeft:
      begin
        ARect.TopLeft := Self.ClientToScreen(Point(0, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(LRDockWidth, Self.Height));
        ARect.Left := ARect.Left + X -
          Cn_GetMinOffset(LRDockWidth, Width + 2*BorderWidth, Source.MouseDeltaX);
        ARect.Right := ARect.Left + LRDockWidth;
        if (Self.Width > 0) and ((ARect.Left + LRDockWidth div 2 < PanelScreenRect.Right) and
          (ARect.Right - LRDockWidth div 2 > PanelScreenRect.Left)) then
        begin
          ARect.Top := ARect.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
          ARect.Bottom := ARect.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alRight:
      begin
        ARect.TopLeft := Self.ClientToScreen(
          Point(-LRDockWidth, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, Self.Height));
        ARect.Left := ARect.Left + X +
          Cn_GetMinOffset(LRDockWidth, Width + 2*BorderWidth, 1 - Source.MouseDeltaX);
        ARect.Right := ARect.Left + LRDockWidth;
        if (Self.Width > 0) and (ARect.Left + LRDockWidth div 2 > PanelScreenRect.Left) and
          (ARect.Right - LRDockWidth div 2 < PanelScreenRect.Right) then
        begin
          ARect.Top := ARect.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
          ARect.Bottom := ARect.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
    end;
  end;
  SetMaxDockSize(Align);
  Inc(ARect.Left);
  Source.DockRect := ARect;
end;

procedure TCnVCDockPanel.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

{ TCnVCDockTree }

procedure TCnVCDockTree.BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer);
begin
  CnGlobalDockPresident.BeginDrag(Control, True, 0);
end;

constructor TCnVCDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited Create(DockSite, CnDockZoneClass);
  Version := gs_VCDockTreeVersion;
  BorderWidth     := 4;
  MinSize         := 20;
end;

function TCnVCDockTree.GetDockEdge(DockRect: TRect;
  MousePos: TPoint; var DropAlign: TAlign; Control: TControl): TControl;
var Zone: TCnDockZone;
  TempOrient: TDockOrientation;
begin
  inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);
  { ��ʼ�� }
  MapWindowPoints(0, DockSite.Handle, DockRect, 2);
  SetDockHeightWidthArr(0, DockSite.Height, DockSite.Width);
  SetDockRectArr(DockRect);
  { ��ʼ���� }
  TempOrient := DockSiteOrient;
  Zone := GetDropOnZone(TempOrient, DockRect, DropAlign);
  TempOrient := Cn_ExchangeOrient(TempOrient);
  Result := GetDropOnControl(TempOrient, Zone, DockRect, DropAlign, Control);
  DropDockSize := DockRectArr[TempOrient, True] - DockRectArr[TempOrient, False];// + BorderWidth div 2;
end;

function TCnVCDockTree.GetDropOnZone(Orient: TDockOrientation; DockRect: TRect; var DropAlign: TAlign): TCnDockZone;

var TempZone: TCnDockZone;
  Scale: Double;

    procedure GetBeginBorderZone(BorderLimit: Integer);
    begin
      if DockRectArr[Orient, True] = BorderLimit then
        Scale := 9999
      else
        Scale := (BorderLimit - DockRectArr[Orient, False]) / (DockRectArr[Orient, True] - BorderLimit);
      if Scale >= 0 then
      begin
        if Scale >= 1 then
          Result := TempZone.BeforeClosestVisibleZone
        else Result := TempZone;
      end;
    end;

    procedure GetEndBorderZone(BorderLimit: Integer);
    begin
      if (DockRectArr[Orient, True] <= BorderLimit) then
        Scale := 9999
      else
        Scale := (BorderLimit - DockRectArr[Orient, False]) / (DockRectArr[Orient, True] - BorderLimit);
      if Scale >= 0 then
      begin
        if Scale < 1 then
          Result := TempZone.AfterClosestVisibleZone
        else Result := TempZone;
      end;
    end;

var TempOrient: TDockOrientation;
begin
  Result := nil;
  TempOrient := Cn_ExchangeOrient(Orient);
  if (DockRectArr[TempOrient, False] > DockHeightWidth[TempOrient])
    or (DockRectArr[TempOrient, True] < 0) then
      Exit;

  { ���TopZone��Orientation������'��ֱ'���ͼ���DockRect��TempZone֮���λ�ù�ϵ }
  if (DockRectArr[Orient, False] + DockRectArr[Orient, True]) div 2 <= 0 then
    { ����DockSite������� }
    DropAlign := DropAlignArr[Orient, False]
  else if (DockRectArr[Orient, False] + DockRectArr[Orient, True]) div 2 >= DockHeightWidth[Orient] then
    { ����DockSite���ұ��� }
    DropAlign := DropAlignArr[Orient, True]
  else
  begin
    { �Ǿ�����DockSite���м� }
    if (TopZone.ChildCount <= 1) or (TopZone.Orientation <> Orient) then
      Result := TopZone
    else
    begin
      Scale := 0;
      TempZone := TopZone.ChildZones;
      GetBeginBorderZone(0);
      while (TempZone <> nil) and (Scale <= 0) do
      begin
        GetEndBorderZone(TempZone.ZoneLimit);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

function TCnVCDockTree.GetDropOnControl(Orient: TDockOrientation; Zone: TCnDockZone; DockRect: TRect;
  var DropAlign: TAlign; Control: TControl): TControl;

var TempZone: TCnDockZone;
  Scale: Double;
  BeginBorderLimit,
  EndBorderLimit: Integer;

  procedure GetBeginBorderControl(Zone: TCnDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];

    if DockRectArr[Orient, False] < BeginBorderLimit then
    begin
      Result := Zone.ChildControl;
      DropAlign := DropAlignArr[Orient, False];
    end;
  end;

  procedure GetEndBorderControl(Zone: TCnDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];
    EndBorderLimit := BeginBorderLimit + Zone.HeightWidth[Orient];

    if DockRectArr[Orient, False] < EndBorderLimit then
    begin
      Result := Zone.ChildControl;
      if DockRectArr[Orient, False] = BeginBorderLimit then
        Scale := 9999

      else       { �±߾�/�ϱ߾� }
        Scale := (EndBorderLimit - DockRectArr[Orient, True]) / (DockRectArr[Orient, False] - BeginBorderLimit);
      if Scale >= 1 then
        DropAlign := DropAlignArr[Orient, False]
      else
      begin
        if (Zone.AfterClosestVisibleZone <> nil) and (Zone.AfterClosestVisibleZone.ChildControl = Control) then
        begin
          Result := Zone.AfterClosestVisibleZone.ChildControl;
          DropAlign := DropAlignArr[Orient, False];
        end else
          DropAlign := DropAlignArr[Orient, True];
      end;
    end;
  end;

begin
  Result := nil;
  Scale := 0;
  if Zone <> nil then
  begin
    if Zone.ChildCount = 0 then
    begin
      GetBeginBorderControl(Zone);
      if Result = nil then
        GetEndBorderControl(Zone);
    end else
    begin
      TempZone := Zone.ChildZones;
      if TempZone <> nil then
        GetBeginBorderControl(TempZone);
      while (TempZone <> nil) and (Result = nil) do
      begin
        GetEndBorderControl(TempZone);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

procedure TCnVCDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
  inherited InsertControl(Control, InsertAt, DropCtl);

end;

procedure TCnVCDockTree.DrawDockGrabber(Control: TControl;
  const ARect: TRect);
var CnVCDockZone: TCnVCDockZone;

  procedure DrawCloseButton(Left, Top: Integer);
  var ADockClient: TCnDockClient;
  begin
    if CnVCDockZone <> nil then
    begin
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+ButtonWidth,
        Top+ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(CnVCDockZone.CloseBtnDown) * DFCS_PUSHED)
    end;
  end;

  procedure DrawExpendBotton(Left, Top: Integer);
  const
{$IFDEF COMPILER6_UP}
    ArrowOrient: array[TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0, 0);
{$ELSE}
    ArrowOrient: array[TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0);
{$ENDIF}


    CurrArrow: array[Boolean, TDockOrientation] of TAlign =
      ((alNone, alLeft, alTop), (alNone, alRight, alBottom));

  var InActive: Boolean;  //�Ƿ����
      IsMaximum: Boolean; //�Ƿ������
  begin
    if CnVCDockZone <> nil then
    begin
      InActive := not ((CnVCDockZone.ParentZone.Orientation <> DockSiteOrient) and
        (CnVCDockZone.ParentZone.VisibleChildCount >= 2));
      IsMaximum := CnVCDockZone.ZoneSizeStyle in [zssMaximum];
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+ButtonWidth,
        Top+ButtonHeight), DFC_SCROLL,
        ArrowOrient[CurrArrow[IsMaximum, DockSiteOrient]] +
        Cardinal(InActive) * (DFCS_INACTIVE) + Cardinal(CnVCDockZone.ExpandBtnDown) * DFCS_PUSHED);
    end;
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    if (Left >= Right) or (Top >= Bottom) then Exit;
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top-1);
    end;
  end;

var DrawRect: TRect;

begin

  CnVCDockZone := TCnVCDockZone(FindControlZone(Control));
  DrawRect := ARect;
  Canvas.Brush.Color := TCnTempWinControl(DockSite).Color;
  Canvas.FillRect(DrawRect);
  with ARect do
    case GrabbersPosition of
      gpLeft:
        begin
          DrawExpendBotton(Left+BorderWidth+LeftOffset, Top+TopOffset+ButtonHeight+ButtonSplitter+BorderWidth);
          DrawCloseButton(Left+BorderWidth+LeftOffset, Top+TopOffset+BorderWidth);
          DrawGrabberLine(Left+BorderWidth+LeftOffset+3, Top+2*ButtonHeight+TopOffset+ButtonSplitter+BottomOffset+BorderWidth+3, Left+BorderWidth+LeftOffset+5, Bottom-BorderWidth-2);
          DrawGrabberLine(Left+BorderWidth+LeftOffset+7, Top+2*ButtonHeight+TopOffset+ButtonSplitter+BottomOffset+BorderWidth+3, Left+BorderWidth+LeftOffset+9, Bottom-BorderWidth-2);
        end;
      gpTop:
        begin
          DrawExpendBotton(Right-LeftOffset-2*ButtonWidth-ButtonSplitter-BorderWidth, Top+TopOffset+BorderWidth);
          DrawCloseButton(Right-LeftOffset-ButtonWidth-BorderWidth, Top+TopOffset+BorderWidth);
          DrawGrabberLine(Left+BorderWidth, Top+BorderWidth+TopOffset+3, Right-2*ButtonWidth-RightOffset-ButtonSplitter-LeftOffset-BorderWidth-3, Top+BorderWidth+TopOffset+5);
          DrawGrabberLine(Left+BorderWidth, Top+BorderWidth+TopOffset+7, Right-2*ButtonWidth-RightOffset-ButtonSplitter-LeftOffset-BorderWidth-3, Top+BorderWidth+TopOffset+9);
        end;
      gpBottom:
        begin

        end;
      gpRight:
        begin

        end;
    end;
end;

procedure TCnVCDockTree.DrawDockSiteRect;
var
  Rect: TRect;
begin
  inherited DrawDockSiteRect;
  Rect := DockSite.ClientRect;
  InflateRect(Rect, BorderWidth, 0);
  if DockSite.Align = alTop then
    Inc(Rect.Bottom, BorderWidth)
  else if DockSite.Align = alBottom then
    Dec(Rect.Top, BorderWidth);
  Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
  Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);

  Canvas.Pen.Color := clBlack;
  if DockSite.Align = alRight then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, DockSite.Height);
  end else if DockSite.Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(DockSite.Width, 0);
  end;
end;

procedure TCnVCDockTree.DrawSplitterRect(const ARect: TRect);
var Rect: TRect;
begin
  inherited;
  Rect := ARect;
  InflateRect(Rect, 1, 1);
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

procedure TCnVCDockTree.WindowProc(var Message: TMessage);
begin
  inherited WindowProc(Message);
end;

procedure TCnVCDockTree.SplitterMouseUp;
begin
  BeginUpdate;
  try
    ShiftBy := 0;
    { ����DockSite��Align���Ժ�SizingZone.ParentZone��Orientation����
      �õ�Ӧ�õ�����ƫ���� }
    if (DockSiteOrient = doVertical) and
      (SizingZone.ParentZone.Orientation = doVertical) then
      ShiftBy := SizePos.x + (SplitterWidth div 2) - SizingZone.ZoneLimit
    else if (DockSiteOrient = doHorizontal) and
      (SizingZone.ParentZone.Orientation = doHorizontal) then
      ShiftBy := SizePos.y + (SplitterWidth div 2) - SizingZone.ZoneLimit;

    if (ShiftBy <> 0) and (SizingZone.AfterClosestVisibleZone <> nil) then
    begin
      if (DockSite.Align in [alLeft, alTop]) then
      begin
        ShiftScaleOrient := DockSiteOrient;
        { SizingZone����Ľڵ㶼Ҫ���е��� }
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        { ����Ĭ�ϵ�SplitterMouseUp }
        inherited SplitterMouseUp;
      end else
      begin
        { Ȼ��SizingZone��NextSibling����Ľڵ㶼Ҫ���е��� }
        ShiftBy := -ShiftBy;
        ShiftScaleOrient := DockSiteOrient;
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        SizePos := Point(SizePos.x + ShiftBy, SizePos.Y + ShiftBy);
        inherited SplitterMouseUp;
      end;

      { ����DockSite��Align���Ե�����Ȼ��߸߶� }
      DockSiteSize := DockSiteSize + ShiftBy;
    end else
    begin
      TCnVCDockZone(SizingZone.ParentZone).DoSetChildSizeStyle(zssNormal);
      inherited SplitterMouseUp;
    end;
  finally
    EndUpdate;
  end;

end;

procedure TCnVCDockTree.ResetBounds(Force: Boolean);
var R: TRect;
begin
  { ��ֹ���� }
  BeginUpdate;
  try
    if not IsLoading then
    begin
      { ���Ȼ��DockSite�Ŀͻ�����С }
      R := Docksite.ClientRect;
      { ֻ�е����ֹ�����DockSite��С��ʱ��ŵ�������ĳ��� }
      if ResizeCount > 0 then
      begin
        if TopZone.ChildZones <> nil then
        begin
          { ���DockSite�Ŀͻ�����С�Ѿ��ı��ˣ��ͼ����ƫ����ShiftBy��Ȼ��͵���
            ForEachAt�������ڵ�ĵ�һ���ӽڵ������нڵ��С}
          if (DockSite.Align = alRight) and (R.Right <> OldRect.Right) then
          begin
            ShiftBy := - OldRect.Right + R.Right;
            ShiftScaleOrient := doVertical;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
          if (DockSite.Align = alBottom) and (R.Bottom <> OldRect.Bottom) then
          begin
            ShiftBy := - OldRect.Bottom + R.Bottom;
            ShiftScaleOrient := doHorizontal;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
        end;
      end;
      { �����һ�δ����DockSite�е�ͣ���ؼ����б������Ų��� }
      if (DockSiteOrient = doVertical) and (R.Bottom <> OldRect.Bottom) then
      begin
        { ��ֱ }
        if OldRect.Bottom - OldRect.Top = 0 then
          ScaleBy := R.Bottom - R.Top
        else if OldRect.Bottom - OldRect.Top > 0 then
          ScaleBy := (R.Bottom - R.Top) / (OldRect.Bottom - OldRect.Top)
        else ScaleBy := 1;
        ShiftScaleOrient := doHorizontal;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
      if (DockSiteOrient = doHorizontal) and (R.Right <> OldRect.Right) then
      begin
        { ˮƽ }
        if OldRect.Right - OldRect.Left = 0 then
          ScaleBy := R.Right - R.Left
        else if OldRect.Right - OldRect.Left > 0 then
          ScaleBy := (R.Right - R.Left) / (OldRect.Right - OldRect.Left)
        else ScaleBy := 1;
        ShiftScaleOrient := doVertical;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
    end;
    inherited ResetBounds(Force);
  finally
    { ������� }
    EndUpdate;
  end;
end;

procedure TCnVCDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
    inherited;
end;

procedure TCnVCDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
begin
    inherited;
end;

procedure TCnVCDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
begin
  inherited;
end;

procedure TCnVCDockTree.ScaleZone(Zone: TCnDockZone);
begin
  if Zone <> nil then
    case TCnVCDockZone(Zone).ZoneSizeStyle of
      zssMinimum:
      begin
        Zone.ZoneLimit := Zone.LimitBegin + MinSize;
        Exit;
      end;
      zssMaximum:
      begin
        Zone.ZoneLimit := DockSiteSizeA - Zone.VisibleNextSiblingCount * MinSize;
        Exit;
      end;
    end;
  inherited ScaleZone(Zone);
end;

procedure TCnVCDockTree.RemoveControl(Control: TControl);
var DockRect: TRect;
  OldDockSize: Integer;
begin
  OldDockSize := DropDockSize;

  DockRect := GetFrameRect(Control);
  if DockSiteOrient = doHorizontal then
    { ˮƽ }
    DropDockSize := DockRect.Right - DockRect.Left
  else if DockSiteOrient = doVertical then
    { ��ֱ }
    DropDockSize := DockRect.Bottom - DockRect.Top;

  inherited RemoveControl(Control);

  DropDockSize := OldDockSize;
end;

procedure TCnVCDockTree.ShiftZone(Zone: TCnDockZone);
begin
  inherited ShiftZone(Zone);
  if (Zone <> nil) and (Zone <> TopZone) and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    if Zone.LimitSize < MinSize then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;
  end;
end;

procedure TCnVCDockTree.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);
begin
  inherited ControlVisibilityChanged(Control, Visible);
end;

function TCnVCDockTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + GrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + TopOffset,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + ButtonHeight + TopOffset + ButtonSplitter,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + 2*ButtonHeight + TopOffset + ButtonSplitter), MousePos) then
        HTFlag := HTEXPAND
      else HTFlag := HTCAPTION;
    end;
  end else Result := nil;
end;

function TCnVCDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else if PtInRect(Rect(
        Left + Width - 2*ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTEXPAND
      else HTFlag := HTCAPTION;
    end;
  end else Result := nil;
end;

function TCnVCDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var TempZone: TCnVCDockZone;
  Active: Boolean;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTEXPAND) then
  begin
    TempZone := TCnVCDockZone(Zone);
    Active := ((TempZone.ParentZone.Orientation <> DockSiteOrient) and
      (TempZone.ParentZone.VisibleChildCount >= 2));
    if Active then
    begin
      TempZone.ExpandBtnDown := True;
      TempZone.MouseDown := True;
      FExpandBtnZone := TempZone;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnVCDockZone;

begin
  inherited DoLButtonUp(Message, Zone, HTFlag);
  if (SizingZone = nil) and (FExpandBtnZone <> nil) then
  begin
    FExpandBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTEXPAND) then
    begin
      TempZone := TCnVCDockZone(Zone);
      TempZone.ExpandBtnDown := False;
      if TempZone.ZoneSizeStyle in [zssMaximum] then
      begin
        TCnVCDockZone(TempZone.ParentZone).DoSetChildSizeStyle(zssNormal);
      end else
      begin
        TCnVCDockZone(TempZone.ParentZone).DoSetChildSizeStyle(zssMinimum);
        TempZone.ZoneSizeStyle := zssMaximum;
      end;
      ResetZoneSizeStyle(TempZone.ParentZone, TempZone.ZoneSizeStyle, nil);
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnVCDockZone;
begin
  inherited DoMouseMove(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TCnVCDockZone(Zone);
    if ((TempZone <> nil) and (TempZone.ExpandBtnDown <> (HTFlag = HTEXPAND))
      and ((FExpandBtnZone = TempZone) and FExpandBtnZone.MouseDown)) then
    begin
      TempZone.ExpandBtnDown := (HTFlag = HTEXPAND) and FExpandBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.ResetZoneSizeStyle(Parent: TCnDockZone;
  ZoneSizeStyle: TZoneSizeStyle; Exclude: TCnDockZone);
var AZone: TCnVCDockZone;
  ChildCount: Integer;
  AverageSize: Integer;
begin
  ChildCount := Parent.VisibleChildCount - Integer((Exclude <> nil) and (Exclude.ParentZone = Parent));
  AverageSize := DockSiteSizeA div ChildCount;
  Assert(AverageSize > 0);
  AZone := TCnVCDockZone(Parent.FirstVisibleChildZone);
  while AZone <> nil do
  begin
    if Exclude <> AZone then
    begin
      Dec(ChildCount);
      if ZoneSizeStyle in [zssMaximum] then
      begin
        if AZone.ZoneSizeStyle = zssMinimum then
          AZone.ZoneLimit := AZone.LimitBegin + MinSize
        else if AZone.ZoneSizeStyle = zssMaximum then
        begin
          AZone.ZoneLimit := DockSiteSizeA - ChildCount * MinSize;
        end;
      end else if ZoneSizeStyle in [zssNormal] then
        AZone.ZoneLimit := AZone.LimitBegin + AverageSize;
    end else if {(ZoneSizeStyle in [zssMaximum]) and }(Exclude <> nil) then
      Exclude.ZoneLimit := Exclude.LimitBegin;

    AZone := TCnVCDockZone(AZone.AfterClosestVisibleZone);
  end;
  SetNewBounds(Parent);
  ForEachAt(Parent, UpdateZone, tskForward);
end;

procedure TCnVCDockTree.CalcSplitterPos;
var
  TestLimit: Integer;
  TempPos: TPoint;
begin
  TempPos := SizePos;
  if (SizingZone.ParentZone.Orientation = doHorizontal) then
  begin
    TestLimit := SizingZone.Top + MinSize;
    if (TempPos.y <= TestLimit) then
    begin
      if (DockSiteOrient = doVertical) then
      begin
        if TempPos.y <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.y := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end else
        TempPos.y := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if (TempPos.y >= TestLimit) then
    begin
      if (DockSiteOrient = doVertical) then
      begin
        if TempPos.y >= DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.y := DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize;
      end else
        TempPos.y := TestLimit;
    end;
  end
  else begin
    TestLimit := SizingZone.Left + MinSize;
    if (TempPos.x <= TestLimit) then
    begin
      if (DockSiteOrient = doHorizontal) then
      begin
        if TempPos.x <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.x := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end else
        TempPos.x := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if (TempPos.x >= TestLimit) then
    begin
      if (DockSiteOrient = doHorizontal) then
      begin
        if TempPos.x >= DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.x := DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize;
      end else
        TempPos.x := TestLimit;
    end;
  end;
  SizePos := TempPos;

end;

procedure TCnVCDockTree.CustomLoadZone(Stream: TStream;
  var Zone: TCnDockZone);
begin
  Stream.Read(TCnVCDockZone(Zone).FZoneSizeStyle, sizeof(TZoneSizeStyle));
  inherited CustomLoadZone(Stream, Zone);
end;

procedure TCnVCDockTree.CustomSaveZone(Stream: TStream; Zone: TCnDockZone);
begin
  Stream.Write(TCnVCDockZone(Zone).FZoneSizeStyle, sizeof(TZoneSizeStyle));
  inherited CustomSaveZone(Stream, Zone);
end;

procedure TCnVCDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  if Zone <> nil then
    case TCnVCDockZone(Zone).ZoneSizeStyle of
      zssMinimum:
      begin
        Zone.ZoneLimit := Zone.LimitBegin + MinSize;
        Exit;
      end;
      zssMaximum:
      begin
        Zone.ZoneLimit := DockSiteSizeA - Zone.VisibleNextSiblingCount * MinSize;
        Exit;
      end;
    end;
  inherited ScaleChildZone(Zone);

  if (Zone <> nil) and (Zone.ParentZone <> nil) and Zone.Visibled and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    // Zone��LimitSize����С�ڹ涨����С�ߴ�MinSize��
    if (Zone.LimitSize < MinSize) then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;
    // Zone��LimitBegin��ǰһ���ɼ��Ľڵ��ZoneLimit��λ�ò���С�ڹ涨����С�ߴ�MinSize
    if (Zone.BeforeClosestVisibleZone <> nil) and (Zone.LimitBegin > DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
      (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2) then
      Zone.BeforeClosestVisibleZone.ZoneLimit := DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2;
  end;
end;

procedure TCnVCDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  inherited ScaleSiblingZone(Zone);
end;

procedure TCnVCDockTree.DoOtherHint(Zone: TCnDockZone;
  HTFlag: Integer; var HintStr: string);
begin
  inherited DoOtherHint(Zone, HTFlag, HintStr);
  if HTFlag = HTEXPAND then
    HintStr := gs_CnVCDockTreeExpandBtnHint;
end;

function TCnVCDockTree.GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign;
begin
  Result := inherited GetDockAlign(Client, DropCtl);
  case DockSite.Align of
    alLeft, alRight:
    begin
      if (Result in [alLeft, alRight]) and (DropCtl <> nil) then
        DropCtl := nil;
    end;
    alTop, alBottom:
    begin
      if (Result in [alTop, alBottom]) and (DropCtl <> nil) then
        DropCtl := nil;
    end;
  end;
end;

procedure TCnVCDockTree.GetCaptionRect(var Rect: TRect);
begin
  //inherited;
  case GrabbersPosition of
    gpTop:
      Rect.Bottom := Rect.Top + GrabberSize + 2;
    gpLeft:
      Rect.Right := Rect.Left + GrabberSize + 2;
  end;
end;

{ TCnVCDockZone }

constructor TCnVCDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
  { Ĭ�����������״̬ }
  FZoneSizeStyle := zssNormal;

  FExpandBtnDown := False;
end;

destructor TCnVCDockZone.Destroy;
begin
  inherited Destroy;
end;

procedure TCnVCDockZone.DoSetChildSizeStyle(ZoneSizeStyle: TZoneSizeStyle);
var AZone: TCnVCDockZone;
begin
  AZone := TCnVCDockZone(ChildZones);
  while AZone <> nil do
  begin
    AZone.ZoneSizeStyle := ZoneSizeStyle;
    AZone := TCnVCDockZone(AZone.AfterClosestVisibleZone);
  end;
end;

procedure TCnVCDockZone.Insert(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVCDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone = nil);// div 2;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if ((BeforeVisibleZone <> nil) and (TCnVCDockZone(BeforeVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) or
      ((AfterVisibleZone <> nil) and (TCnVCDockZone(AfterVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) then
    begin
      { ���ParentZone����һ��ͣ���ͻ������״̬,�͵���ResetZoneSizeStyle������������ZoneSizeStyle }
      ZoneSizeStyle := zssMinimum;
      TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, nil);
      Visibled := True;
      Exit;
    end;
    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TCnVCDockTree(Tree).MinSize then
      DockSize := (TempSize - (ParentZone.VisibleChildCount) * TCnVCDockTree(Tree).MinSize) div 2;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    begin
      PrevShift := 0;
      NextShift := 0;
    end else
    if BeforeVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ����ǰ�� }
      PrevShift := 0;
      NextShift := DockSize + BorderSize;
      ZoneLimit := DockSize + BorderSize;
      if ParentZone.VisibleChildCount = 1 then
        AfterVisibleZone.ZoneLimit := TempSize;
    end else if AfterVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ������� }
      PrevShift := DockSize + BorderSize;
      NextShift := 0;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end else
    begin
      { Ҫ����Ľڵ����ڸ��ڵ���м� }
      PrevShift := Round((BeforeVisibleZone.ZoneLimit) * (DockSize + BorderSize) / TempSize);
      NextShift := DockSize - PrevShift;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
    end;

    Visibled := True;
    { ����½ڵ�����һ���ֵܽڵ� }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { ���������½ڵ����һ���ֵܽڵ��ʱ�򣬾�ֹͣ���� }
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { ��PrevSibling��ZoneLimit���е��� }
      if BeforeVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { ����½ڵ�����һ���ֵܽڵ� }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;
  end else
  begin
    with TCnVCDockTree(Tree) do
    begin

      TempSize := DockHeightWidth[DockSiteOrient] - BorderSize;

      { ����ֵܽڵ���nil����Ҫ���µ���DockSite�Ŀ�Ⱥ͸߶� }
      if (BeforeVisibleZone <> nil) then
      begin
        if (Tree.TopZone.VisibleChildCount = 2) and Visibled then
          BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit + BorderSize;
        if Visibled then
          ZoneLimit := BeforeVisibleZone.ZoneLimit + TempSize
        else
          ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize + BorderSize;

        TempSize := ZoneLimit;
      end;

      if (AfterVisibleZone <> nil) then
      begin
        { �µĽڵ��ZoneLimit����������Ŀؼ���LRDockWidth����BorderSize }
        if Visibled then
          ZoneLimit := LimitBegin + TempSize
        else
          ZoneLimit := LimitBegin + DockSize - BorderSize;
        { ��Ҫ������ƫ���������µĽڵ��ZoneLimit }
        ShiftBy := ZoneLimit;
        { ����ƫ�Ʒ���Ϊ��ֱ�� }
        ShiftScaleOrient := DockSiteOrient;
        { ��ʼƫ�������� }
        ForEachAt(AfterVisibleZone, ShiftZone, tskForward);
        { ���¸�DockSite�Ŀ�Ȼ��߸߶ȸ�ֵ }
        TempSize := DockSiteSize + ZoneLimit - LimitBegin;
      end;
      Visibled := True;

      DockSiteSize := TempSize;
      { ����GetClientAlignControl����DockServer��GetClientAlignSize�¼� }
      TCnDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
    end;
  end;
  Visibled := True;
end;

procedure TCnVCDockZone.Remove(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVCDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone = nil);// div 2;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if (ZoneSizeStyle in [zssMaximum, zssMinimum]) then
    begin
      if ZoneSizeStyle = zssMinimum then
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, self)
      else if ZoneSizeStyle = zssMaximum then
      begin
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssNormal, self);
        TCnVCDockZone(ParentZone).DoSetChildSizeStyle(zssNormal);
      end;
      Visibled := False;
      Exit;
    end;

    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
      Exit;

    if BeforeVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ����ǰ�� }
      PrevShift := 0;
      NextShift := -DockSize + BorderSize;
      ZoneLimit := -DockSize + BorderSize;
    end else if AfterVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ������� }
      PrevShift := -DockSize + BorderSize;
      NextShift := 0;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end else
    begin
      { Ҫ����Ľڵ����ڸ��ڵ���м� }
      PrevShift := -Round((BeforeVisibleZone.ZoneLimit) * (DockSize + BorderSize) / (TempSize - (DockSize + BorderSize)));
      NextShift := -DockSize - PrevShift;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit;
    end;

    { ����½ڵ�����һ���ֵܽڵ� }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { ���������½ڵ����һ���ֵܽڵ��ʱ�򣬾�ֹͣ���� }
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { ��PrevSibling��ZoneLimit���е��� }
      if BeforeVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { ����½ڵ�����һ���ֵܽڵ� }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;
  end else
  begin
    Visibled := False;
    with TCnVCDockTree(Tree) do
    begin
      { ������ڵ��ͣ�������DockSite��ͣ��������ͬ�Ļ����ͽ�����Ӧ�ı������� }
      ZoneLimit := LimitBegin - BorderSize;
      ShiftBy := -DockSize - BorderSize;
      ShiftScaleOrient := DockSiteOrient;
      { ���Zone��ǰһ���ֵܴ��ڣ��ʹ�Zone��ǰһ���ֵܿ�ʼ���� }
      if BeforeClosestVisibleZone <> nil then
        DockSiteSize := DockSiteSize - DockSize - BorderSize
      { ���ߣ����Zone�ĺ�һ���ֵܴ��ڣ��ʹ�Zone�ĺ�һ���ֵܿ�ʼ���� }
      else if AfterClosestVisibleZone <> nil then
      begin
        ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
        { ���濪ʼ����DockSite�Ŀ�Ⱥ͸߶� }
        DockSiteSize := DockSiteSize - DockSize - BorderSize;
      end;
    end;
  end;
end;

procedure TCnVCDockZone.InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderWidth: Integer;
begin
  if not Insert then
    Visibled := False;

  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) and (ParentZone <> Tree.TopZone) then
  begin
    if Insert then
      TempSize := ParentZone.VisibleSize
    else
      TempSize := ParentZone.LimitSize;

    ParentZone.InsertOrRemove(TempSize, Insert, Hide);
  end;
  if ParentZone = nil then Exit;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if (TCnVCDockZone(ParentZone.ChildZones).ZoneSizeStyle in [zssMaximum, zssMinimum]) then//or
    begin
      { ���ParentZone����һ��ͣ���ͻ������״̬,�͵���ResetZoneSizeStyle������������ZoneSizeStyle }
      if Insert then
      begin
        ZoneSizeStyle := zssMinimum;
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, nil);
      end
      else
      begin
        if ZoneSizeStyle = zssMinimum then
          TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, self)
        else if ZoneSizeStyle = zssMaximum then
        begin
          TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssNormal, self);
          TCnVCDockZone(ParentZone).DoSetChildSizeStyle(zssNormal);
        end;
      end;
      Exit;
    end;

    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize;

    BorderWidth := TCnVCDockTree(Tree).BorderWidth;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderWidth;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    if BeforeClosestVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ����ǰ�� }
      PrevShift := 0;
      NextShift := (2 * Integer(Insert) - 1) * (DockSize + BorderWidth);

      ZoneLimit := Integer(Insert) * (DockSize + BorderWidth);
      if ParentZone.VisibleChildCount = 2 then
        NextSibling.ZoneLimit := TempSize;
    end else if AfterClosestVisibleZone = nil then
    begin
      { Ҫ����Ľڵ����ڸ��ڵ������� }
      PrevShift := (2 * Integer(Insert) - 1) * (DockSize + BorderWidth);
      NextShift := 0;
      begin
        if ParentZone.ChildCount = 2 then
          PrevSibling.ZoneLimit := TempSize - Integer(Insert) * PrevShift
        else
          PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      end;
      ZoneLimit := TempSize;

    end else
    begin
      { Ҫ����Ľڵ����ڸ��ڵ���м� }
      PrevShift := (2 * Integer(Insert) - 1) * Round((PrevSibling.ZoneLimit) * (DockSize + BorderWidth) / (TempSize - Integer(not Insert) * (DockSize + BorderWidth)));
      NextShift := (2 * Integer(Insert) - 1) * DockSize - PrevShift;
      PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      ZoneLimit := Integer(Insert) * (DockSize + BorderWidth) + PrevSibling.ZoneLimit;
    end;

    { ����½ڵ�����һ���ֵܽڵ� }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { ���������½ڵ����һ���ֵܽڵ��ʱ�򣬾�ֹͣ���� }
        ReplacementZone := PrevSibling;
        try
          if PrevSibling.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (PrevSibling.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { ��PrevSibling��ZoneLimit���е��� }
      if PrevSibling.LimitSize < TCnVCDockTree(Tree).MinSize then
        PrevSibling.ZoneLimit := PrevSibling.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { ����½ڵ�����һ���ֵܽڵ� }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(NextSibling, ScaleSiblingZone, tskForward);
      end;
    end;

    ParentZone.Update;

  end else
  begin
    with TCnVCDockTree(Tree) do
    begin
      { ����ǲ������ }
      if Insert then
      begin
        { ����ֵܽڵ���nil����Ҫ���µ���DockSite�Ŀ�Ⱥ͸߶� }
        if AfterClosestVisibleZone = nil then
        begin
          ZoneLimit := LimitBegin + DockHeightWidth[DockSiteOrient];// + DockSize;
          DockSiteSize := ZoneLimit;
        end else if BeforeClosestVisibleZone = nil then
        begin
          { �µĽڵ��ZoneLimit����������Ŀؼ���LRDockWidth����BorderWidth }
          ZoneLimit := DockHeightWidth[DockSiteOrient] + BorderWidth;// + DockSize;
          { ��Ҫ������ƫ���������µĽڵ��ZoneLimit }
          ShiftBy := ZoneLimit;
          { ����ƫ�Ʒ���Ϊ��ֱ�� }
          ShiftScaleOrient := DockSiteOrient;
          { ��ʼƫ�������� }
          ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
          { ���¸�DockSite�Ŀ�Ȼ��߸߶ȸ�ֵ }
          DockSiteSize := DockSiteSize + ZoneLimit;
        end;
        { ����GetClientAlignControl����DockServer��GetClientAlignSize�¼� }
        TCnDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
      end else
      { �������ɾ������ }
      begin
        { ������ڵ��ͣ�������DockSite��ͣ��������ͬ�Ļ����ͽ�����Ӧ�ı������� }
        ZoneLimit := LimitBegin;
        ShiftBy := -DockSize;
        ShiftScaleOrient := DockSiteOrient;
        { ���Zone��ǰһ���ֵܴ��ڣ��ʹ�Zone��ǰһ���ֵܿ�ʼ���� }
        if PrevSibling <> nil then
          DockSiteSize := DockSiteSize - DockSize - 5
        { ���ߣ����Zone�ĺ�һ���ֵܴ��ڣ��ʹ�Zone�ĺ�һ���ֵܿ�ʼ���� }
        else if NextSibling <> nil then
        begin
          ForEachAt(NextSibling, ShiftZone, tskForward);
          { ���濪ʼ����DockSite�Ŀ�Ⱥ͸߶� }
          DockSiteSize := DockSiteSize - DockSize;
        end;
      end;
    end;
  end;
  if Insert then
    Visibled := True;
end;

procedure TCnVCDockZone.SetZoneSize(Size: Integer; Show: Boolean);
begin
  inherited SetZoneSize(Size, Show);
end;

{ TCnVCDragDockObject }

constructor TCnVCDragDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FrameWidth := DefaultFrameWidth;

  FDockOverFrameWidth := DefaultDockOverFrameWidth;

  FDockOverBrush := TBrush.Create;
  SetDefaultBrushStyle;

  CurrState := dsDragEnter;
  OldState := CurrState;
end;

destructor TCnVCDragDockObject.Destroy;
begin
  FDockOverBrush.Free;
  inherited;
end;

function TCnVCDragDockObject.DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := 0;
end;

procedure TCnVCDragDockObject.GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);

var  DockOver: Boolean;

  { ��û����ľ���Ϳ�ܿ�� }
  procedure GetBrushAndFrameWidth;
  begin
    if DockOver then
    begin
      PenSize := FDockOverFrameWidth;
      ABrush := FDockOverBrush;
    end else
    begin
      PenSize := FrameWidth;
      ABrush := Brush;
    end;
  end;

begin
              { DockOver��ʼ��ʱ�� }                                                   { ��ֱ�Ӱ�Control�϶����ٽ���ͣ����������ʱ�� }
  DockOver := ((OldState = dsDragEnter) and (CurrState = dsDragMove) and (not Erase or (OldTarget <> nil)))
              { DockOver;�е�ʱ�� }
              or ((OldState = dsDragMove) and (CurrState = dsDragMove))
              { DockOver������ʱ�� }
              or ((OldState = dsDragMove) and (CurrState = dsDragLeave) and Erase);
              { Control������Ѿ���ͣ���� }

  GetBrushAndFrameWidth;

  if (OldState = dsDragMove) and (CurrState = dsDragLeave) then
  begin
    { ���DockOver�����Ļ���������OldStateΪdsDragEnter��ʹ��������������� }
    OldState := dsDragEnter;
    OldTarget := nil;
  end else OldTarget := DragTarget;

  if Erase then DrawRect := EraseDockRect
  else DrawRect := DockRect;

end;

function TCnVCDragDockObject.GetDropCtl: TControl;
begin
  Result := DropOnControl;
end;

procedure TCnVCDragDockObject.SetCurrState(const Value: TDragState);
begin
  FCurrState := Value;
end;

procedure TCnVCDragDockObject.SetDefaultBrushStyle;
begin
  FDockOverBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FDockOverBrush.Style := bsSolid;
end;

procedure TCnVCDragDockObject.SetOldState(const Value: TDragState);
begin
  FOldState := Value;
end;

{ TCnVCDockSplitter }

constructor TCnVCDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldSize := MinSize;
end;

function TCnVCDockSplitter.DoCanResize(var NewSize: Integer): Boolean;
var DockPanel: TCnDockPanel;
  Limit, MinSize: Integer;
begin
  Result := inherited DoCanResize(NewSize);
  if (Result) and (FOldSize > NewSize) then
  begin
    DockPanel := DockServer.DockPanel[Integer(Align) - 1];
    Limit := DockPanel.CnDockManager.GetDockClientLimit(Cn_GetControlOrient(DockPanel),
      Align in [alLeft, alTop]);
    MinSize := DockPanel.CnDockManager.MinSize;

    if DockPanel.Align in [alLeft, alTop] then
    begin
      if NewSize < Limit + MinSize then
        Result := False;
    end else
    begin
      if NewSize < Cn_GetControlSize(DockPanel) - Limit + MinSize then
        Result := False;
    end;
  end;
  if Result then
    FOldSize := NewSize;
end;

procedure TCnVCDockSplitter.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var DockPanel: TCnDockPanel;
begin
  DockPanel := DockServer.DockPanel[Integer(Align) - 1];
  DockPanel.CnDockManager.BeginResizeDockSite;
  try
    inherited MouseUp(Button, Shift, X, Y);
  finally
    DockPanel.CnDockManager.EndResizeDockSite;
  end;
end;

procedure TCnVCDockSplitter.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  Inc(Rect.Right, 2);
  case Align of
    alLeft:
    begin
      InflateRect(Rect, 0, 2);
    end;
    alRight:
    begin
      OffsetRect(Rect, -1, 0);
      InflateRect(Rect, 0, 2);
    end;
    alTop:
    begin
      Inc(Rect.Bottom, 2);
      InflateRect(Rect, 2, 0);
    end;
    alBottom:
    begin
      Dec(Rect.Top, 2);
      InflateRect(Rect, 2, 1);
    end;
  end;
  Canvas.Brush.Color := Color;
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

{ TCnVCConjoinServerOption }

procedure TCnVCConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnVCConjoinServerOption then
  begin
    FBorderWidth := TCnVCConjoinServerOption(Source).BorderWidth;
  end;
  inherited;
end;

constructor TCnVCConjoinServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited;
  BorderWidth := 4;
end;

destructor TCnVCConjoinServerOption.Destroy;
begin
  inherited;

end;

procedure TCnVCConjoinServerOption.ResetDockControlOption;
begin
  inherited;

end;

procedure TCnVCConjoinServerOption.SetBorderWidth(const Value: Integer);
begin
  FBorderWidth := Value;
end;

end.

