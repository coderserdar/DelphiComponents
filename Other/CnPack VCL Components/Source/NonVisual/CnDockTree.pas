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
{       ����ͣ���ؼ��Ĺ�����                            }
{       CnDockTree ��Ԫ                                 }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockTree;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�����ͣ���ؼ��Ĺ����� 
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Consts, CnDockSupportClass;

const
  { ���λ���ڷָ������� }
  HTSPLITTER = 30;
  { û�����ض���λ���� }
  HTNONE = 31;

type

  TCnDockTree = class;

  // ICnDockManager�ӿڼ̳���IDockManager���û�������ICnDockManager���ٶ���һЩ���������������ԡ�
  ICnDockManager = interface(IDockManager)
    ['{7B0AACBC-E9BF-42F8-9629-E551067090B2}']
    function GetActiveControl: TControl;               //��ý���Ŀؼ�
    procedure SetActiveControl(const Value: TControl); //����ĳ���ؼ���ý���
    function GetGrabberSize: Integer;                  //��ð��ֵĴ�С
    procedure SetGrabberSize(const Value: Integer);    //���ð��ֵĴ�С
    function GetSplitterWidth: Integer;                //��÷ָ����Ŀ��
    procedure SetSplitterWidth(const Value: Integer);  //���÷ָ����Ŀ��
    function GetBorderWidth: Integer;                  //��ñ߿�Ŀ��
    procedure SetBorderWidth(const Value: Integer);    //���ñ߿�Ŀ��
    function GetDockRect: TRect;                       //���ͣ������
    procedure SetDockRect(const Value: TRect);         //����ͣ������

    function GetDockSiteSize: Integer;                 //���ͣ���������Ŀ�Ȼ��߸߶�
    procedure SetDockSiteSize(const Value: Integer);   //����ͣ���������Ŀ�Ȼ��߸߶�

    function GetMinSize: Integer;                      //���ͣ���ͻ�֮�����С����

    procedure BeginResizeDockSite;
    procedure EndResizeDockSite;

    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl;  //���ͣ��Ԥ�����������Control;

    function GetHTFlag(MousePos: TPoint): Integer;

    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);

    procedure ShowControl(Control: TControl);           //��ʾControl
    procedure HideControl(Control: TControl);           //����Control

    procedure ShowAllControl;                           // ��ʾ���е�Control
    procedure HideAllControl;                           // �������е�Control
    procedure ShowSingleControl(Control: TControl);     // ֻ��ʾһ��Control�������Ķ�����
    procedure HideSingleControl(Control: TControl);     // ֻ����һ��Control�������Ķ���ʾ

    { ���µ�NewControl�滻�ϵ�OldControl }
    procedure ReplaceZoneChild(OldControl, NewControl: TControl);

    { ���������Control��������Zone������ҵ��ͷ���True�����򷵻�False }
    function HasZoneWithControl(Control: TControl): Boolean;

    { ���ͣ���������ϵ�ͣ���ͻ��ļ���, �����DockSite }
    function GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer;

    function GetFrameRect(Control: TControl): TRect;   //���ͣ���ؼ��ľ��δ�С�������DockSite��
    function GetFrameRectEx(Control: TControl): TRect; //���ͣ���ؼ��ľ��δ�С���������Ļ��
    property ActiveControl: TControl read GetActiveControl
      write SetActiveControl;
    property GrabberSize: Integer read GetGrabberSize
      write SetGrabberSize;
    property SplitterWidth: Integer read GetSplitterWidth
      write SetSplitterWidth;
    property BorderWidth: Integer read GetBorderWidth
      write SetBorderWidth;

    property DockSiteSize: Integer read GetDockSiteSize write SetDockSiteSize;

    property DockRect: TRect read GetDockRect write SetDockRect;

    property MinSize: Integer read GetMinSize;
  end;

  TCnDockZone = class
  private
    FChildControl: TWinControl;     //����Ŀؼ�
    FChildZones: TCnDockZone;       //����Ů
    FNextSibling: TCnDockZone;      //���ֵ�
    FOrientation: TDockOrientation; //ͣ���ķ�ʽ����ˮƽ���Ǵ�ֱ������û��
    FParentZone: TCnDockZone;       //����
    FPrevSibling: TCnDockZone;      //��һ���ֵ�
    FTree: TCnDockTree;             //�����Ŀ���
    FZoneLimit: Integer;            //�ڵ���±߻����ұߵ�����
                                    //���FOrientation��ˮƽ�ģ��ͱ�ʾ�±ߵ����꣬
                                    //���FOrientation�Ǵ�ֱ�ģ��ͱ�ʾ�ұߵ����ꡣ
    FVisibleSize: Integer;          //�洢�ϵ�Zone�ɼ���ʱ���ZoneSize��ֵ,
    FVisibled: Boolean;             //�Ƿ�ɼ�
    FControlVisibled: Boolean;      //ChildControl�Ƿ�ɼ�
    FIsInside: Boolean;             //�Ƿ���DockSite������
    function GetFirstSibling: TCnDockZone;//�����ǰһ���ֵ�
    function GetLastSibling: TCnDockZone; //������һ���ֵ�
    function GetFirstChild: TCnDockZone;  //�����ǰһ����Ů
    function GetLastChild: TCnDockZone;   //������һ����Ů
    function GetTopLeftArr(Orient: TDockOrientation): Integer;
    function GetHeightWidthArr(Orient: TDockOrientation): Integer;
    function GetAfterClosestVisibleZone: TCnDockZone;
    function GetBeforeClosestVisibleZone: TCnDockZone;
    function GetAfterApoapsisVisibleZone: TCnDockZone;
    function GetBeforeApoapsisVisibleZone: TCnDockZone;
    function GetNextSiblingCount: Integer;
    function GetPrevSiblingCount: Integer;
    procedure SetVisibled(const Value: Boolean);
    procedure SetZoneLimit(const Value: Integer);
    function GetVisibleNextSiblingCount: Integer;
    function GetVisibleNextSiblingTotal: Integer;
    function GetVisiblePrevSiblingCount: Integer;
    function GetVisiblePrevSiblingTotal: Integer;
    function GetFirstVisibleChildZone: TCnDockZone;
    function GetLastVisibleChildZone: TCnDockZone;
    procedure SetIsInside(const Value: Boolean);
  protected
    procedure AdjustZoneLimit(Value: Integer); virtual;//�ı�FZoneLimitֵ���ҵ��������ֵ�
    procedure LButtonDbClkMothed; virtual;    // ��������˫����ʱ���ж�Zone�ϵ�ChildControl��ζ���
    function GetChildCount: Integer;          //�����Ů�ĸ���
    function GetVisibleChildCount: Integer;   //��ÿɼ���Ů�ĸ���
    function GetChildTotal: Integer;          //���ĩ����Ů������
    function GetVisibleChildTotal: Integer;   //���ĩ�οɼ���Ů�ĸ���
    function GetLimitBegin: Integer;          //��ýڵ���ϱ߻�����ߵ�����
    function GetLimitSize: Integer;           //��ýڵ�ĸ߶Ȼ��߿��
    function GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
    function GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
    function GetControlName: string;//��ÿؼ������ƣ����û�пؼ��ͷ��ؿ��ַ���
    { ��õ�ǰ�Ľڵ��ڵ��ӽڵ�ķָ�����ZoneLimit, ���IsMinΪTrue, ˵����ȡ��Сֵ����֮��ȡ���ֵ }
    function GetSplitterLimit(IsMin: Boolean): Integer; virtual;
    function DoGetSplitterLimit(Orientation: TDockOrientation;
      IsMin: Boolean; var LimitResult: Integer): Integer; virtual;
    {��������ΪValue�Ŀؼ�������ҵ��Ͱ���ͣ����FTree��}
    function SetControlName(const Value: string): Boolean;
    procedure DoCustomSetControlName; virtual;
    procedure SetChildControlVisible(Client: TControl; AViisible: Boolean); virtual;
  public
    constructor Create(Tree: TCnDockTree); virtual;
    { ����һ����СΪDockSize�Ľڵ� }
    procedure Insert(DockSize: Integer; Hide: Boolean); virtual;
    { ȥ��һ����СΪDockSize�Ľڵ� }
    procedure Remove(DockSize: Integer; Hide: Boolean); virtual;
    { ����һ���ؼ�ͣ��������ʱ�򣬵���InsertOrRemove���µ���ParentZone�ϵ��ӽڵ�,
      ����Insertָʾ�Ƿ��ǲ�����������Insert=True���ǲ�����������߾���ɾ������ }
    procedure InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean); virtual;
    procedure ResetChildren(Exclude: TCnDockZone); virtual;//����������Ů�����ԣ��������
    { ���µ�ǰ�ڵ�(�������ӽڵ�)��������ǰ��ChildControl��λ�úʹ�С }
    procedure Update; virtual;
    { �������ڵ�ľ���λ�� }
    function GetFrameRect: TRect; virtual;
    { ���ýڵ�Ĵ�С }
    procedure SetZoneSize(Size: Integer; Show: Boolean); virtual;
    { �뱾�ڵ�����Ŀɼ���ǰ�ֵܽڵ� }
    property BeforeClosestVisibleZone: TCnDockZone read GetBeforeClosestVisibleZone;
    { �뱾�ڵ�����Ŀɼ��ĺ��ֵܽڵ� }
    property AfterClosestVisibleZone: TCnDockZone read GetAfterClosestVisibleZone;
    { �뱾�ڵ���Զ�Ŀɼ���ǰ�ֵܽڵ� }
    property BeforeApoapsisVisibleZone: TCnDockZone read GetBeforeApoapsisVisibleZone;
    { �뱾�ڵ���Զ�Ŀɼ��ĺ��ֵܽڵ� }
    property AfterApoapsisVisibleZone: TCnDockZone read GetAfterApoapsisVisibleZone;
    { ��һ���ɼ�����Ů�ڵ� }
    property FirstVisibleChildZone: TCnDockZone read GetFirstVisibleChildZone;
    { ��һ���ɼ�����Ů�ڵ� }
    property LastVisibleChildZone: TCnDockZone read GetLastVisibleChildZone;
    { ��Ů���� }
    property ChildCount: Integer read GetChildCount;
    { ĩ����Ů���� }
    property ChildTotal: Integer read GetChildTotal;
    { ��һ����Ů }
    property ChildZones: TCnDockZone read FChildZones write FChildZones;
    { �ڽڵ��ϵ�Control�ؼ� }
    property ChildControl: TWinControl read FChildControl write FChildControl;
    { ��һ����Ů }
    property FirstChild: TCnDockZone read GetFirstChild;
    { ��һ���ֵ� }
    property FirstSibling: TCnDockZone read GetFirstSibling;
    { �߶� }
    property Height: Integer index Ord(doHorizontal) read GetHeightWidth;
    { ����ͣ����������ø߶ȺͿ�� }
    property HeightWidth[Orient: TDockOrientation]: Integer read GetHeightWidthArr;
    { ���һ����Ů }
    property LastChild: TCnDockZone read GetLastChild;
    { ���һ���ֵ� }
    property LastSibling: TCnDockZone read GetLastSibling;
    { ���λ�� }
    property Left: Integer index Ord(doVertical) read GetTopLeft;
    { ��ʼλ�õ����� }
    property LimitBegin: Integer read GetLimitBegin;
    { ��С }
    property LimitSize: Integer read GetLimitSize;
    { ��һ���ֵ� }
    property NextSibling: TCnDockZone read FNextSibling write FNextSibling;
    { ��ú��ֵܵĸ��� }
    property NextSiblingCount: Integer read GetNextSiblingCount;
    { ͣ������ }
    property Orientation: TDockOrientation read FOrientation write FOrientation;
    { ���ڵ� }
    property ParentZone: TCnDockZone read FParentZone write FParentZone;
    { ��һ���ֵ� }
    property PrevSibling: TCnDockZone read FPrevSibling write FPrevSibling;
    { ���ǰ�ֵܵĸ��� }
    property PrevSiblingCount: Integer read GetPrevSiblingCount;
    { �ϱ�λ�� }
    property Top: Integer index Ord(doHorizontal) read GetTopLeft;
    { ����ͣ�������������ߺ��ϱ�λ�� }
    property TopLeft[Orient: TDockOrientation]: Integer read GetTopLeftArr;
    { �����ĸ��� }
    property Tree: TCnDockTree read FTree write FTree;
    { �ɼ���Ů�ڵ�ĸ��� }
    property VisibleChildCount: Integer read GetVisibleChildCount;
    { �ɼ�ĩ�˽ڵ�ĸ��� }
    property VisibleChildTotal: Integer read GetVisibleChildTotal;
    { �ɼ������ֵܵĸ��� }
    property VisiblePrevSiblingCount: Integer read GetVisiblePrevSiblingCount;
    { �ɼ������ֵܵ�ĩ�˽ڵ�ĸ��� }
    property VisiblePrevSiblingTotal: Integer read GetVisiblePrevSiblingTotal;
    { �ɼ������ֵܵĸ��� }
    property VisibleNextSiblingCount: Integer read GetVisibleNextSiblingCount;
    { �ɼ������ֵܵ�ĩ�˽ڵ�ĸ��� }
    property VisibleNextSiblingTotal: Integer read GetVisibleNextSiblingTotal;
    { �ɼ��Ĵ�С }
    property VisibleSize: Integer read FVisibleSize write FVisibleSize;
    { ��� }
    property Width: Integer index Ord(doVertical) read GetHeightWidth;
    { �����DockSite�ľ���λ�� }
    property ZoneLimit: Integer read FZoneLimit write SetZoneLimit;
    { �Ƿ�ɼ� }
    property Visibled: Boolean read FVisibled write SetVisibled;
    { �Ƿ������� }
    property IsInside: Boolean read FIsInside write SetIsInside;
  end;

  TCnAdvDockZone = class(TCnDockZone)
  private
    FCloseBtnDown: Boolean;
    FMouseDown: Boolean;
  protected
    procedure LButtonDbClkMothed; override;
  public
    constructor Create(Tree: TCnDockTree); override;
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
    property CloseBtnDown: Boolean read FCloseBtnDown write FCloseBtnDown;
    property MouseDown: Boolean read FMouseDown write FMouseDown;
  end;

  { ���ı�����ʽ���ֱ���ǰ������ͺ������ }
  TTreeScanKind = (tskForward, tskMiddle, tskBackward);

  { ���ı������ȼ��𣬷ֱ����ȱ����ֵܣ��ȱ�����Ů }
  TTreeScanPriority = (tspSibling, tspChild);

  TCnForEachZoneProc = procedure(Zone: TCnDockZone) of object;

  { ���ֵ�λ�ã��������ĸ�λ�ã��ֱ����������� }
  TGrabbersPosition = (gpTop, gpBottom, gpLeft, gpRight);

  TCnDockZoneClass = class of TCnDockZone;

  TCnDockTree = class(TInterfacedObject, ICnDockManager)
  private

    {�ڵ��������}
    FCnDockZoneClass: TCnDockZoneClass;
    {�������ĸ��ؼ�����˽���}
    FActiveControl: TControl;

    FBorderWidth: Integer;         //�߿�Ŀ��

    FSplitterWidth: Integer;       //�ָ����Ŀ��
    FBrush: TBrush;                //���������ֵ�ˢ��
    FDockSite: TWinControl;        //ͣ���ķ���ؼ�
    FGrabberSize: Integer;         //���ֵĴ�С
    FOldRect: TRect;               //��DockSite�Ĵ�С������ʱ��
                                   //���ֵ�洢���һ�ε�DockSite�Ĵ�С
    FDockRect: TRect;

    FOldWndProc: TWndMethod;
    FReplacementZone: TCnDockZone;
    FResizeCount: Integer;         //����DockSite��С�ļ�����
    FScaleBy: Double;              //�����Ĵ�С

    { ������ƫ����������ʱ������ָʾ������ʲôͣ�������Zone��Ҫ���� }
    FShiftScaleOrient: TDockOrientation;
    FShiftBy: Integer;             //ƫ����
    FSizePos: TPoint;              //��������ָ���ʱ����¼����������
    FSizingDC: HDC;                //���ָ������豸������
    FSizingWnd: HWND;
    FSizingZone: TCnDockZone;      //�ָ��������ĸ��ڵ�
    FTopZone: TCnDockZone;         //���ڵ�
    FTopXYLimit: Integer;
    FUpdateCount: Integer;         //���¼�����
    FVersion: Integer;             //�汾
    FOldHTFlag: Integer;           //�ϵ�����λ��
    FParentLimit: Integer;         //���ֵ�������ڵ���ZoneLimit��ʱ���õģ�
                                   //���幫ʽ��ScaleChildZone����
    FMinSize: Integer;             //ͣ���ؼ�֮�����С����

    FCanvas: TControlCanvas;       //������DockSite�Ļ���

    procedure SetTopZone(const Value: TCnDockZone);
    procedure SetTopXYLimit(const Value: Integer);
    { ����TCnDockZone�������� }
    procedure SetCnDockZoneClass(const Value: TCnDockZoneClass);

    { ��÷ָ����Ŀ�� }
    function GetSplitterWidth: Integer;
    { ��ñ߿�Ŀ�� }
    function GetBorderWidth: Integer;
    { ���÷ָ����Ŀ�� }
    procedure SetSplitterWidth(const Value: Integer);
    { ���ñ߿�Ŀ�� }
    procedure SetBorderWidth(const Value: Integer);
    function GetDockSiteOrient: TDockOrientation;
    function GetDockSiteSize: Integer;
    procedure SetDockSiteSize(const Value: Integer);
    procedure SetMinSize(const Value: Integer);
    function GetDockSiteBegin: Integer;
    procedure SetDockSiteBegin(const Value: Integer);
    function GetDockSiteSizeA: Integer;
    procedure SetDockSiteSizeA(const Value: Integer);
    procedure SetVersion(const Value: Integer);
    function GetDockSiteSizeWithOrient(Orient: TDockOrientation): Integer;
    procedure SetDockSiteSizeWithOrient(Orient: TDockOrientation;
      const Value: Integer);
    function GetDockRect: TRect;                       //���ͣ������
    procedure SetDockRect(const Value: TRect);        //����ͣ������
    function GetMinSize: Integer;
  protected
    function HasZoneWithControl(Control: TControl): Boolean;
    { �����DockSite�Ĵ�����Ϣ }
    procedure WindowProc(var Message: TMessage); virtual;
    { ------------------------------------------------------------------------ }
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); virtual;
    { ------------------------------------------------------------------------ }
    function DoMouseEvent(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): TWMNCHitMessage; virtual;
    { ------------------------------------------------------------------------ }
    { ��DockSite��������ƶ���ʱ�����DoMouseMove���� }
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite�������������µ�ʱ�����DoLButtonDown���� }
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; virtual;
    { ��DockSite�����������ͷŵ�ʱ�����DoLButtonUp���� }
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite����������˫����ʱ�����DoLButtonDbClk���� }
    procedure DoLButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite��������м����µ�ʱ�����DoLButtonDown���� }
    procedure DoMButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite��������м��ͷŵ�ʱ�����DoLButtonUp���� }
    procedure DoMButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite����������˫����ʱ�����DoMButtonDbClk���� }
    procedure DoMButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite��������Ҽ����µ�ʱ�����DoLButtonDown���� }
    procedure DoRButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite��������Ҽ��ͷŵ�ʱ�����DoLButtonUp���� }
    procedure DoRButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite����������˫����ʱ�����DoRButtonDbClk���� }
    procedure DoRButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ------------------------------------------------------------------------ }
    { ����AZone�е�ChildControl }
    procedure DoHideZoneChild(AZone: TCnDockZone); virtual;
    { ------------------------------------------------------------------------ }
    { ��DockSite��Ҫ���ù����״��ʱ�����DoSetCursor���� }
    procedure DoSetCursor(var Message: TWMSetCursor;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��DockSite�ϳ�����ʾ���ʱ�����DoHintShow���� }
    procedure DoHintShow(var Message: TCMHintShow;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ��������ʾ��Ϣ }
    procedure DoOtherHint(Zone: TCnDockZone;
      HTFlag: Integer; var HintStr: string); virtual;
    { ------------------------------------------------------------------------ }
    procedure CustomSaveZone(Stream: TStream;
      Zone: TCnDockZone); virtual;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TCnDockZone); virtual;
    procedure DoSaveZone(Stream: TStream;
      Zone: TCnDockZone; Level: Integer); virtual;
    procedure DoLoadZone(Stream: TStream); virtual;
    { ------------------------------------------------------------------------ }
    { ����Control�ؼ��Ĵ�С }
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); virtual;
    { ��ʼ����DockSite�Ĵ�С���򵥵�ʹFResizeCount��һ }
    procedure BeginResizeDockSite;
    { ��ʼ���£��򵥵�ʹFUpdateCount��һ }
    procedure BeginUpdate;
    { ���㲢�����Ʒָ�����λ�� }
    procedure CalcSplitterPos; virtual;
    { ��Control�ؼ���Visible���Ըı��ʱ�򣬵���ControlVisibilityChanged���� }
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean); virtual;
    { ���Client��DropCtl�е�λ�� }
    function GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign; virtual;
    { ���������ȷ���������DockSite��ʲôλ�� }
    function DoFindZone(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    { ���ָ����ƶ���ʱ������� }
    procedure DrawSizeSplitter; virtual;
    { ��������DockSite�Ĵ�С����FResizeCount��һ }
    procedure EndResizeDockSite;
    { �������£���FUpdateCount��һ�����FUpdateCountС�ڵ������ʱ�򣬾͵���UpdateAll�������� }
    procedure EndUpdate;
    ////////////////////////////////////////////////////////////////////////////
    { ���������Control�������ҵ���Ӧ��Zone }
    function FindControlZone(Control: TControl; IncludeHide: Boolean = False): TCnDockZone; virtual;
    { ���������Control�������ҵ���Ӧ��Zone�����ҷ������Zone��Level }
    function FindControlZoneAndLevel(Control: TControl;
      var CtlLevel: Integer; IncludeHide: Boolean = False): TCnDockZone; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { �����������б��� }
    procedure ForEachAt(Zone: TCnDockZone; Proc: TCnForEachZoneProc;
      ScanKind: TTreeScanKind = tskForward; ScanPriority: TTreeScanPriority = tspSibling); virtual;
    { �����DockSite�еĻ��Control�ؼ� }
    function GetActiveControl: TControl; virtual;
    { ��ð��ֵĴ�С }
    function GetGrabberSize: Integer; virtual;
    ////////////////////////////////////////////////////////////////////////////
    function GetBorderHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetRightGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetBottomGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { ���ͣ��Ԥ�����������Control }
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { ���ͣ���������ϵ�ͣ���ͻ��ļ���, �����DockSite }
    function GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer; virtual;
    { ��ð��ֵľ��δ�С }
    function GetFrameRect(Control: TControl): TRect; virtual;
    function GetFrameRectEx(Control: TControl): TRect; virtual;
    { ��÷ָ����ľ��δ�С }
    function GetSpiltterRect(Zone: TCnDockZone): TRect; virtual;
    { ���ð�����ʲôλ�� }
    function GetGrabbersPosition: TGrabbersPosition; virtual;
    { ���Control�ؼ��Ĵ�С }
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect); virtual;
    { ��÷ָ�����Limit }
    function GetSplitterLimit(AZone: TCnDockZone; IsCurrent, IsMin: Boolean): Integer; virtual;
    procedure DoGetNextLimit(Zone, AZone: TCnDockZone; var LimitResult: Integer); virtual;
    { ���MousePosλ�õ�HTFlag }
    function GetHTFlag(MousePos: TPoint): Integer; virtual;
    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    { ������������λ���ж��������Control�ؼ� }
    function HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl; virtual;
    { ������������λ���ж��������Zone }
    function InternalHitTest(const MousePos: TPoint;
      out HTFlag: Integer): TCnDockZone; virtual;
    { ����һ���ؼ�������InsertAt�ǲ����λ�� }
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    { ------------------------------------------------------------------------ }
    { ����һ���µ�Zone��Ȼ���NewZone��SiblingZone��Ϊ����ChildZones }
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); virtual;
    { ��NewZone��ΪSiblingZone���ֵ� }
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
        InsertLast, Update: Boolean); virtual;
    { ------------------------------------------------------------------------ }
    { ������װ��ͣ����Ϣ }
    procedure LoadFromStream(Stream: TStream); virtual;
    { ��ͣ����Ϣ�洢������ }
    procedure SaveToStream(Stream: TStream); virtual;

    { ���º����ǻ�ͼ���� }
    {==========================================================================}
    { �ػ��������� }
    procedure PaintDockSite; virtual;
    { �ػ�HostDockSite�ľ������� }
    procedure DrawDockSiteRect; virtual;
    { �ػ�ÿһ���ڵ� }
    procedure DrawZone(Zone: TCnDockZone); virtual;
    { �ػ����� }
    procedure DrawZoneGrabber(Zone: TCnDockZone); virtual;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); virtual;
    { �ػ��ָ��� }
    procedure DrawZoneSplitter(Zone: TCnDockZone); virtual;
    procedure DrawSplitterRect(const ARect: TRect); virtual;
    { �ػ��߿� }
    procedure DrawZoneBorder(Zone: TCnDockZone); virtual;
    { R1Ϊ�ڿ��С��R2Ϊ����С }
    procedure DrawDockBorder(DockControl: TControl; R1, R2: TRect); virtual;

    {==========================================================================}
    { �õ��������Ĵ�С }
    procedure GetCaptionRect(var Rect: TRect); virtual;
    { ��λͣ��λ�� }
    procedure PositionDockRect(Client, DropCtl: TControl;
      DropAlign: TAlign; var DockRect: TRect); virtual;
    ////////////////////////////////////////////////////////////////////////////
    { ɾ��ȫ��Zone }
    procedure PruneZone(Zone: TCnDockZone); virtual;
    { ɾ������Zone }
    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean = True); virtual;
    { ����Zone�ı��� }
    procedure ScaleZone(Zone: TCnDockZone); virtual;

    procedure ScaleChildZone(Zone: TCnDockZone); virtual;   //������ʱ����������������
                                                            //��ŮZone��ZoneLimit��ֵ
    procedure ScaleSiblingZone(Zone: TCnDockZone); virtual; //������ʱ����������������
                                                            //�ֵ�Zone��ŮZone��ZoneLimit��ֵ
    { ����Zone��ƫ���� }
    procedure ShiftZone(Zone: TCnDockZone); virtual;
    { ����Zone }
    procedure UpdateZone(Zone: TCnDockZone); virtual;
    { ���ָ��� }
    procedure DrawSplitter(Zone: TCnDockZone); virtual;
    ////////////////////////////////////////////////////////////////////////////
    { ɾ��Control�ؼ� }
    procedure RemoveControl(Control: TControl); virtual;
    { ����DockSite�еĻControl�ؼ� }
    procedure SetActiveControl(const Value: TControl); virtual;
    { ���ð��ֵĴ�С }
    procedure SetGrabberSize(const Value: Integer); virtual;
    { ����Zone��ChildControl�Ĵ�С������Zone��ChildZones }
    procedure SetNewBounds(Zone: TCnDockZone); virtual;
    procedure SetReplacingControl(Control: TControl);
    { ��������ָ�����ʱ�����SplitterMouseDown���� }
    procedure SplitterMouseDown(OnZone: TCnDockZone; MousePos: TPoint); virtual;
    { ������ͷŷָ�����ʱ�����SplitterMouseUp���� }
    procedure SplitterMouseUp; virtual;
    { �������÷�Χ }
    procedure ResetBounds(Force: Boolean); virtual;
    { �ѿؼ�������д����Stream���� }
    procedure WriteControlName(Stream: TStream; ControlName: string);
    { ��Stream��������ؼ������� }
    procedure ReadControlName(Stream: TStream; var ControlName: string);
    ////////////////////////////////////////////////////////////////////////////
    procedure ShowControl(Control: TControl);           //��ʾControl
    procedure HideControl(Control: TControl);           //����Control
    procedure ShowAllControl;                           // ��ʾ���е�Control
    procedure HideAllControl;                           // �������е�Control
    procedure ShowSingleControl(Control: TControl);     // ֻ��ʾһ��Control�������Ķ�����
    procedure HideSingleControl(Control: TControl);     // ֻ����һ��Control�������Ķ���ʾ
    ////////////////////////////////////////////////////////////////////////////
    { ���µ�NewControl�滻�ϵ�OldControl }
    procedure ReplaceZoneChild(OldControl, NewControl: TControl);
    ////////////////////////////////////////////////////////////////////////////
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth;
    property Canvas: TControlCanvas read FCanvas;
    property DockSiteSize: Integer read GetDockSiteSize write SetDockSiteSize;
    property DockSiteSizeA: Integer read GetDockSiteSizeA write SetDockSiteSizeA;
    property DockSiteBegin: Integer read GetDockSiteBegin write SetDockSiteBegin;
    property DockSiteSizeWithOrient[Orient: TDockOrientation]: Integer
      read GetDockSiteSizeWithOrient write SetDockSiteSizeWithOrient;
    property GrabberSize: Integer read FGrabberSize write SetGrabberSize;
    property GrabbersPosition: TGrabbersPosition read GetGrabbersPosition;
    property MinSize: Integer read GetMinSize write SetMinSize;
    property DockRect: TRect read GetDockRect write SetDockRect;
    property OldRect: TRect read FOldRect write FOldRect;
    property ParentLimit: Integer read FParentLimit write FParentLimit;
    property ReplacementZone: TCnDockZone read FReplacementZone write FReplacementZone;
    property ResizeCount: Integer read FResizeCount write FResizeCount;
    property ScaleBy: Double read FScaleBy write FScaleBy;
    property ShiftBy: Integer read FShiftBy write FShiftBy;
    property ShiftScaleOrient: TDockOrientation read FShiftScaleOrient write FShiftScaleOrient;
    property SizePos: TPoint read FSizePos write FSizePos;
    property SizingDC: HDC read FSizingDC;
    property SizingWnd: HWND read FSizingWnd;
    property SizingZone: TCnDockZone read FSizingZone write FSizingZone;
    property SplitterWidth: Integer read GetSplitterWidth write SetSplitterWidth;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property Version: Integer read FVersion write SetVersion;
  public
    SplitterCanvas: TControlCanvas;
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); virtual;
    destructor Destroy; override;
    property DockSite: TWinControl read FDockSite write FDockSite;
    property DockSiteOrient: TDockOrientation read GetDockSiteOrient;
    { ���÷ָ����������״���û�������������������ı�������״ }
    procedure SetSplitterCursor(CursorIndex: TDockOrientation); virtual;
    { �ػ�DockSite�Ľ��� }
    procedure PaintSite(DC: HDC); virtual;
    property TopXYLimit: Integer read FTopXYLimit write SetTopXYLimit;
    property TopZone: TCnDockZone read FTopZone write SetTopZone;
    { ����ȫ�� }
    procedure UpdateAll;
    { ���µ�ǰZone����Ů }
    procedure UpdateChild(Zone: TCnDockZone);

    property CnDockZoneClass: TCnDockZoneClass read FCnDockZoneClass
      write SetCnDockZoneClass;
  end;

  TCnDockTreeClass = class of TCnDockTree;

  TCnAdvDockTree = class(TCnDockTree)
  private
    FButtonHeight,                  //�رհ�ť�ĸ߶�
    FButtonWidth,                   //�رհ�ť�Ŀ��
    FLeftOffset,                    //�رհ�ť�����ƫ����
    FRightOffset,                   //�رհ�ť���ұ�ƫ����
    FTopOffset,                     //�رհ�ť���ϱ�ƫ����
    FBottomOffset: Integer;         //�رհ�ť���±�ƫ����
    FButtonSplitter: Integer;       //��ť֮��ļ��
    FCloseBtnZone: TCnAdvDockZone;

    FDropDockSize: Integer;
    FDockHeightWidth: array[TDockOrientation] of Integer;
    FDockRectArr: array[TDockOrientation, Boolean] of Integer;

    procedure SetBottomOffset(const Value: Integer);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonSplitter(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetLeftOffset(const Value: Integer);
    procedure SetRightOffset(const Value: Integer);
    procedure SetTopOffset(const Value: Integer);

    function GetDockHeightWidth(Orient: TDockOrientation): Integer;
    procedure SetDockHeightWidth(Orient: TDockOrientation;
      const Value: Integer);

    function GetDockRectFromArr(Orient: TDockOrientation;
      AtLast: Boolean): Integer;
    procedure SetDockRectToArr(Orient: TDockOrientation; AtLast: Boolean;
      const Value: Integer);
    procedure SetDropDockSize(const Value: Integer);

  protected
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
      InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure SetDockHeightWidthArr(NoOrValue, HorValue, VerValue: Integer);
    procedure SetDockRectArr(ARect: TRect);

    procedure ScaleZone(Zone: TCnDockZone); override;
    procedure ScaleChildZone(Zone: TCnDockZone); override;
    procedure ScaleSiblingZone(Zone: TCnDockZone); override;
    procedure ShiftZone(Zone: TCnDockZone); override;

    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean); override;
  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
    property BottomOffset: Integer read FBottomOffset write SetBottomOffset;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ButtonSplitter: Integer read FButtonSplitter write SetButtonSplitter;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property LeftOffset: Integer read FLeftOffset write SetLeftOffset;
    property RightOffset: Integer read FRightOffset write SetRightOffset;
    property TopOffset: Integer read FTopOffset write SetTopOffset;
    property CloseBtnZone: TCnAdvDockZone read FCloseBtnZone write FCloseBtnZone;
    property DockHeightWidth[Orient: TDockOrientation]: Integer read GetDockHeightWidth write SetDockHeightWidth;
    property DockRectArr[Orient: TDockOrientation; AtLast: Boolean]: Integer read GetDockRectFromArr write SetDockRectToArr;
    property DropDockSize: Integer read FDropDockSize write SetDropDockSize;
  end;
  
var
  //�洢����Ϣ�����Ľ�����־
  TreeStreamEndFlag: Integer = -1;

implementation

uses
  Math, CnDockFormControl, CnDockSupportProc, CnDockGlobal, CnVSNETDockStyle;

type
  TCnWinControlAccess = class(TWinControl);

{ TCnDockZone }

constructor TCnDockZone.Create(Tree: TCnDockTree);
begin
//  FVisibleSize := 0;
  ParentZone := nil;
  PrevSibling := nil;
  NextSibling := nil;
  ChildZones := nil;
  ChildControl := nil;
  FTree := Tree;
  FVisibled := True;
end;

function TCnDockZone.GetChildCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := ChildZones;
  while Zone <> nil do
  begin
    Zone := Zone.NextSibling;
    Inc(Result);
  end;
end;

function TCnDockZone.GetLimitBegin: Integer;
var
  CheckZone: TCnDockZone;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.Orientation = doHorizontal then Result := Top
  else if CheckZone.Orientation = doVertical then Result := Left
  else Result := 0;//raise Exception.Create('');
end;

function TCnDockZone.GetLimitSize: Integer;
var
  CheckZone: TCnDockZone;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.Orientation = doHorizontal then Result := Height
  else if CheckZone.Orientation = doVertical then Result := Width
  else Result := Tree.TopXYLimit; //raise Exception.Create('');
end;

function TCnDockZone.GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TCnDockZone;
  R: TRect;
begin
  Zone := Self;
  while Zone <> FTree.FTopZone do
  begin
    // �ҵ��ɼ������ֵܡ�
    if (Zone.VisiblePrevSiblingCount > 0) and (Zone.ParentZone.Orientation = TDockOrientation(Orient)) then
    begin
      Result := Zone.BeforeClosestVisibleZone.ZoneLimit;
      Exit;
    end else Zone := Zone.ParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  TCnWinControlAccess(FTree.FDockSite).AdjustClientRect(R);
  case TDockOrientation(Orient) of
    doVertical: Result := R.Left;
    doHorizontal: Result := R.Top;
  else
    Result := 0;
  end;
end;

function TCnDockZone.GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TCnDockZone;
  R: TRect;
begin
  if (Self = FTree.FTopZone) or ((FParentZone = FTree.FTopZone) and
    (ChildControl <> nil) and (FTree.FTopZone.ChildCount = 1)) then
  begin
    R := FTree.FDockSite.ClientRect;
    TCnWinControlAccess(FTree.FDockSite).AdjustClientRect(R);
    if TDockOrientation(Orient) = doHorizontal then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end
  else begin
    Zone := Self;
    while (Zone <> FTree.FTopZone) and (Zone.ParentZone <> nil) do
    begin
      // �洢
//      BeginLimit := Zone.LimitBegin;
//      while (Zone.NextSibling <> nil) and (not Zone.NextSibling.Visibled) do
//        Zone := Zone.NextSibling;
      if {(Zone.VisiblePrevSiblingCount > 0) and }(Zone.ParentZone.Orientation = TDockOrientation(Orient)) then
      begin
        Result := Zone.ZoneLimit - Zone.LimitBegin;
        Exit;
      end
      else
        Zone := Zone.ParentZone;
    end;
    if FTree.FTopZone.Orientation = TDockOrientation(Orient) then
      Result := FTree.TopXYLimit
    else
      Result := FTree.FTopZone.ZoneLimit;
  end;
end;

procedure TCnDockZone.ResetChildren(Exclude: TCnDockZone);
var
  SumLimit,
  NewLimit,
  FirstChildBegin,
  OldPrevLimit: Integer;
  ChildNode: TCnDockZone;  // ��ǰ����Ů�ڵ�(�ɼ���)
  PrevNode: TCnDockZone;   // ��ǰ����Ů�ڵ��ǰ�ֵ�(�ɼ���)
begin
  //
  case Orientation of
    doHorizontal: NewLimit := Height;
    doVertical: NewLimit := Width;
  else
    Exit;
  end;
  // �õ���һ���ɼ�����Ů�ڵ㣬����ȷ�����Ǵ��ڵġ�
  ChildNode := FirstVisibleChildZone;
  if ChildNode = nil then Exit;
  // �õ�ƽ�ֵ�ZoneLimit��ֵ
  SumLimit := NewLimit;
  NewLimit := NewLimit div VisibleChildCount;

  FirstChildBegin := ChildNode.LimitBegin;

  Tree.ShiftScaleOrient := Orientation;
  Tree.ParentLimit := 0;
  if ChildNode.ZoneLimit - FirstChildBegin > 0 then
    Tree.ScaleBy := NewLimit / (ChildNode.ZoneLimit - FirstChildBegin)
  else Tree.ScaleBy := 1;
  if (Tree.ScaleBy <> 1) and (ChildNode.VisibleChildCount > 0) then
    Tree.ForEachAt(ChildNode.ChildZones, Tree.ScaleChildZone, tskMiddle, tspChild);

  if ChildNode <> Exclude then
    OldPrevLimit := ChildNode.ZoneLimit
  else OldPrevLimit := FirstChildBegin;


  // ����һ���ɼ�����Ů�ڵ��ZoneLimit��ֵ
  ChildNode.ZoneLimit := FirstChildBegin + NewLimit;
  ChildNode.Update;
  // ����ChildNode����Ϊ�ں���ĳ�����Ҫ�õ�
  PrevNode := ChildNode;
  ChildNode := ChildNode.AfterClosestVisibleZone;

  // һֱ���ϵ�ѭ����ֱ�����һ���ɼ��Ŀɼ�����Ů�ڵ�Ϊֹ��
  while ChildNode <> nil do
  begin
    if ChildNode.ZoneLimit - OldPrevLimit > 0 then
      Tree.ScaleBy := NewLimit / (ChildNode.ZoneLimit - OldPrevLimit)
    else Tree.ScaleBy := 1;

    Tree.ShiftBy := PrevNode.ZoneLimit - OldPrevLimit;
    if (Tree.ShiftBy <> 0) and (ChildNode.VisibleChildCount > 0){ and (PrevNode <> Exclude) }then
      Tree.ForEachAt(ChildNode.ChildZones, Tree.ShiftZone, tskForward);

    Tree.ParentLimit := PrevNode.ZoneLimit;

    if (Tree.ScaleBy <> 1) and (ChildNode.VisibleChildCount > 0) then
      Tree.ForEachAt(ChildNode.ChildZones, Tree.ScaleChildZone, tskForward);

    if ChildNode <> Exclude then
      OldPrevLimit := ChildNode.ZoneLimit;
//    else OldPrevLimit := PrevNode.ZoneLimit;

    ChildNode.ZoneLimit := PrevNode.ZoneLimit + NewLimit;

    if ChildNode.AfterClosestVisibleZone = nil then
    begin
      // ȡ���������
      if NewLimit = 0 then
        NewLimit := 1;
      ChildNode.ZoneLimit := ChildNode.ZoneLimit + (SumLimit mod NewLimit);
    end;
    ChildNode.Update;
    PrevNode := ChildNode;
    ChildNode := ChildNode.AfterClosestVisibleZone;
  end;
end;

function TCnDockZone.GetControlName: string;
begin
  Result := '';
  if ChildControl <> nil then
  begin
    if ChildControl.Name = '' then
      raise Exception.CreateRes(@SDockedCtlNeedsName);
    Result := ChildControl.Name;
  end;
end;

function TCnDockZone.SetControlName(const Value: string): Boolean;
var
  Client: TControl;
begin
  Client := nil;
  with FTree do
  begin
    TCnWinControlAccess(FDockSite).ReloadDockedControl(Value, Client);
    Result := Client <> nil;
    if Result then
    begin
      FReplacementZone := Self;
      ChildControl := TWinControl(Client);
      DoCustomSetControlName;
      try
        if IsInside then
        begin
          Client.ManualDock(FDockSite, nil, alNone);
//          CnGlobalDockPresident.CalcDockSizes(Client);
        end;
//        ResetBounds(True);
      finally
        SetChildControlVisible(Client, FControlVisibled);
        FReplacementZone := nil;
      end;
    end;
  end;
end;

procedure TCnDockZone.Update;

  function ParentNotLast: Boolean;
  var
    Parent: TCnDockZone;
  begin
    Result := False;
    Parent := FParentZone;
    while Parent <> nil do
    begin
      if (Parent.VisibleNextSiblingCount > 0) and (Parent.Orientation = ParentZone.Orientation) then
      begin
        Result := True;
        Exit;
      end;
      Parent := Parent.FParentZone;
    end;
  end;

var
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if Visibled and (ChildControl <> nil) and (FTree.FUpdateCount = 0) then
  begin
    ChildControl.DockOrientation := FParentZone.Orientation;
    NewWidth := Width;
    NewHeight := Height;
    if ParentNotLast then
    begin
      if FParentZone.Orientation = doHorizontal then
        Dec(NewWidth, FTree.SplitterWidth)
      else
        Dec(NewHeight, FTree.SplitterWidth);
    end;

    if ((NextSibling <> nil) and (VisibleNextSiblingTotal > 0)) or ((FParentZone <> FTree.FTopZone) and
      ((FParentZone.Orientation = FTree.FTopZone.Orientation) and
      (FZoneLimit < FTree.TopXYLimit)) or
      ((FParentZone.Orientation <> FTree.FTopZone.Orientation) and
      (FZoneLimit < FTree.FTopZone.ZoneLimit))) then
    begin
      if FParentZone.Orientation = doHorizontal then
        Dec(NewHeight, FTree.SplitterWidth)
      else
        Dec(NewWidth, FTree.SplitterWidth);
    end;
    R := Bounds(Left, Top, NewWidth, NewHeight);
    FTree.AdjustDockRect(ChildControl, R);
    ChildControl.BoundsRect := R;
  end;
end;

function TCnDockZone.GetFrameRect: TRect;
var
  ALeft, ATop, ARight, ABottom, BorderWidth: Integer;
begin
  ALeft := Left;
  ATop := Top;
  if NextSibling <> nil then
    BorderWidth := Tree.BorderWidth
  else
  BorderWidth := 0;
  ARight := ALeft + Width - BorderWidth;
  ABottom := ATop + Height - BorderWidth;
  Result := Rect(ALeft, ATop, ARight, ABottom);
end;

function TCnDockZone.GetFirstSibling: TCnDockZone;
begin
  Result := Self;
  while Result.PrevSibling <> nil do
    Result := Result.PrevSibling;
end;

function TCnDockZone.GetLastSibling: TCnDockZone;
begin
  Result := Self;
  while (Result <> nil) and (Result.NextSibling <> nil) do
    Result := Result.NextSibling;
end;

function TCnDockZone.GetFirstChild: TCnDockZone;
begin
  Result := ChildZones;
end;

function TCnDockZone.GetLastChild: TCnDockZone;
begin
  Result := ChildZones;
  if Result <> nil then
    Result := Result.LastSibling;
end;

function TCnDockZone.GetTopLeftArr(Orient: TDockOrientation): Integer;
begin
  Result := 0;
  case Orient of
    doHorizontal: Result := Top;
    doVertical: Result := Left;
  else
//    raise Exception.Create('');
  end;
end;

function TCnDockZone.GetHeightWidthArr(Orient: TDockOrientation): Integer;
begin
  Result := 0;
  case Orient of
    doHorizontal: Result := Height;
    doVertical: Result := Width;
  else
//    raise Exception.Create('');
  end;
end;

procedure TCnDockZone.AdjustZoneLimit(Value: Integer);
begin
  FZoneLimit := Value;
  if PrevSibling <> nil then
    PrevSibling.ZoneLimit := PrevSibling.ZoneLimit + Value;
//  else if NextSibling <> nil then
//    NextSibling.ZoneLimit := NextSibling.ZoneLimit
end;

procedure TCnDockZone.SetZoneSize(Size: Integer; Show: Boolean);
begin
  InsertOrRemove(Size, Show, False);
end;

procedure TCnDockZone.InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean);
begin
end;

procedure TCnDockZone.Insert(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, True, Hide);
  // ���ParentZone��VisibleChildCount����0��
  // ˵�����ڵ�Ҳ�����صģ���Ҫ���ø��ڵ��Insert������
  // ע�⣬����һ���ݹ麯����һֱ���õ����ڵ�ɼ�Ϊֹ��
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  Visibled := True;
  if ParentZone <> nil then
    ParentZone.ResetChildren(Self);

  // ���¸���ParentZone�ϵ���Ů�ڵ��λ��
  Tree.SetNewBounds(ParentZone);
  Tree.UpdateChild(ParentZone);
end;

procedure TCnDockZone.Remove(DockSize: Integer; Hide: Boolean);
var Zone: TCnDockZone;
begin
  InsertOrRemove(DockSize, False, Hide);
  // ��������Visibled��
  Visibled := not Hide;

  // �����ParentZone���������ParentZone��û�пɼ�����Ů���Ͱ�ParentZoneҲRemove,
  // ����һ���ݹ麯����Ŀ����ʹ���������ϲ��Zone��ʼ��
  if (ParentZone <> Tree.TopZone) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  // ����Ѿ�û�пɼ��ĺ�����ֵܣ��Ͱ�ǰ�ֵܵ�ZoneLimit��ɵ�ǰZone��ZoneLimit��
  if AfterClosestVisibleZone = nil then
  begin
    // �ҵ��뵱ǰZone����Ŀɼ���ǰ�ֵܡ�
    Zone := BeforeClosestVisibleZone;
    if Zone <> nil then
    begin
      // ����пɼ���ǰ�ֵܣ��͵�������ֵܵ�������Ů��λ�á�
      Zone.ZoneLimit := ZoneLimit;
      Tree.SetNewBounds(Zone);
    end;
  end;
  // ��Ϊ��ǰ��Zone��������������ZoneLimit����ɺ�LimitBeginһ���ġ�
  ZoneLimit := LimitBegin;
end;

function TCnDockZone.GetVisibleChildCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := ChildZones;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.NextSibling;
  end;
end;

function TCnDockZone.GetChildTotal: Integer;

  procedure DoFindChildCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindChildCount(Zone.NextSibling);
      DoFindChildCount(Zone.ChildZones);
      {if Zone.Orientation = doNoOrient then}
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindChildCount(ChildZones);
end;

function TCnDockZone.GetVisibleChildTotal: Integer;

  procedure DoFindVisibleChildCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindVisibleChildCount(Zone.NextSibling);
      DoFindVisibleChildCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleChildCount(ChildZones);
end;

function TCnDockZone.GetAfterClosestVisibleZone: TCnDockZone;
begin
  Result := NextSibling;
  while Result <> nil do
  begin
    if Result.Visibled then
      Exit;
    Result := Result.NextSibling;
  end;
end;

function TCnDockZone.GetBeforeClosestVisibleZone: TCnDockZone;
begin
  Result := PrevSibling;
  while Result <> nil do
  begin
    if Result.Visibled then
      Exit;
    Result := Result.PrevSibling;
  end;
end;

function TCnDockZone.GetAfterApoapsisVisibleZone: TCnDockZone;
begin
  Result := LastSibling;
  if Result <> nil then
    Result := Result.BeforeClosestVisibleZone;
  if Self = Result then
    Result := nil;
end;

function TCnDockZone.GetBeforeApoapsisVisibleZone: TCnDockZone;
begin
  Result := ParentZone.ChildZones;
  if Result <> Self then
    Result := Result.AfterClosestVisibleZone;
  if Self = Result then
    Result := nil;
end;

function TCnDockZone.GetNextSiblingCount: Integer;
var AZone: TCnDockZone;
begin
  Result := 0;
  AZone := NextSibling;
  while AZone <> nil do
  begin
    Inc(Result);
    AZone := AZone.NextSibling;
  end;
end;

function TCnDockZone.GetPrevSiblingCount: Integer;
var AZone: TCnDockZone;
begin
  Result := 0;
  AZone := PrevSibling;
  while AZone <> nil do
  begin
    Inc(Result);
    AZone := AZone.PrevSibling;
  end;
end;

procedure TCnDockZone.SetVisibled(const Value: Boolean);
begin
  FVisibled := Value;
  if (not FVisibled) and (Self <> Tree.TopZone) then
  begin
    if ParentZone.Orientation = doNoOrient then
      VisibleSize := Tree.TopXYLimit
    else VisibleSize := LimitSize;
  end else
  begin

  end;
end;

function TCnDockZone.GetVisibleNextSiblingCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := NextSibling;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.NextSibling;
  end;
end;

function TCnDockZone.GetVisibleNextSiblingTotal: Integer;

  procedure DoFindVisibleNextSiblingCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindVisibleNextSiblingCount(Zone.NextSibling);
      DoFindVisibleNextSiblingCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleNextSiblingCount(NextSibling);
end;

function TCnDockZone.GetVisiblePrevSiblingCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := PrevSibling;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.PrevSibling;
  end;
end;

function TCnDockZone.GetVisiblePrevSiblingTotal: Integer;

  procedure DoFindVisibleNextSiblingCount(Zone: TCnDockZone);
  begin
    if (Zone <> nil) and (Zone <> Self) then
    begin
      DoFindVisibleNextSiblingCount(Zone.NextSibling);
      DoFindVisibleNextSiblingCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleNextSiblingCount(ParentZone);
end;

procedure TCnDockZone.SetZoneLimit(const Value: Integer);
begin
  FZoneLimit := Value;
end;

function TCnDockZone.GetFirstVisibleChildZone: TCnDockZone;
begin
  Result := ChildZones;
  while (Result <> nil) and (not Result.Visibled) do
    Result := Result.NextSibling;
end;

function TCnDockZone.GetSplitterLimit(IsMin: Boolean): Integer;
begin
  if IsMin then
    Result := ZoneLimit
  else Result := LimitBegin;
  
  if ChildZones <> nil then
    ChildZones.DoGetSplitterLimit(ParentZone.Orientation, IsMin, Result);
end;

function TCnDockZone.DoGetSplitterLimit(Orientation: TDockOrientation;
  IsMin: Boolean; var LimitResult: Integer): Integer;
begin
  Result := 0;
  if (ParentZone <> nil) and (ParentZone.Orientation = Orientation) and Visibled then
  begin
    if IsMin then
      LimitResult := Min(LimitResult, ZoneLimit)
    else
    begin
      if AfterClosestVisibleZone <> nil then
        LimitResult := Max(LimitResult, ZoneLimit);
    end;
  end;

  if NextSibling <> nil then
    NextSibling.DoGetSplitterLimit(Orientation, IsMin, LimitResult);

  if ChildZones <> nil then
    ChildZones.DoGetSplitterLimit(Orientation, IsMin, LimitResult);
end;

function TCnDockZone.GetLastVisibleChildZone: TCnDockZone;
var Zone: TCnDockZone;
begin
  Result := nil;
  Zone := ChildZones;
  while (Zone <> nil) and Zone.Visibled do
  begin
    Result := Zone;
    Zone := Zone.NextSibling;
  end;
end;

procedure TCnDockZone.DoCustomSetControlName;
begin
  { û���� }
end;

procedure TCnDockZone.LButtonDbClkMothed;
begin
  if ChildControl <> nil then
    ChildControl.ManualDock(nil, nil, alTop);
end;

procedure TCnDockZone.SetIsInside(const Value: Boolean);
begin
  FIsInside := Value;
end;

procedure TCnDockZone.SetChildControlVisible(Client: TControl; AViisible: Boolean);
begin
  if Client <> nil then
  begin
    Client.Visible := {(not IsInside) or }FControlVisibled;
  end;
end;

{ TCnDockTree }

constructor TCnDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
var
  I: Integer;
begin
  FCnDockZoneClass := CnDockZoneClass;
  FBorderWidth := 0;
  FSplitterWidth := 4;
  FDockSite := TWinControl(DockSite);
  FDockSite.ShowHint := True;
  FVersion := gs_BaseDockTreeVersion;
  GrabberSize := 12;
  FMinSize := 12;
  FTopZone := FCnDockZoneClass.Create(Self);
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  // �����Ѿ����ڵĿؼ�������
  BeginUpdate;
  try
    for I := 0 to DockSite.ControlCount - 1 do
      InsertControl(DockSite.Controls[I], alLeft, nil);
    FTopZone.ResetChildren(nil);
  finally
    EndUpdate;
  end;
  if not (csDesigning in DockSite.ComponentState) then
  begin
    FOldWndProc := FDockSite.WindowProc;
    FDockSite.WindowProc := WindowProc;
  end;
end;

destructor TCnDockTree.Destroy;
begin
  if @FOldWndProc <> nil then
    FDockSite.WindowProc := FOldWndProc;
  PruneZone(FTopZone);
  FBrush.Free;
  inherited Destroy;
end;

procedure TCnDockTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
  { Ϊ�ؼ�����ռ� }
  { ���ȼ�ȥ�߿�Ŀ�� }
  InflateRect(ARect, -BorderWidth, -BorderWidth);
  { Ȼ���ټ�ȥ���ֵĿ�� }
  case GrabbersPosition of
    gpTop:
      Inc(ARect.Top, GrabberSize);
    gpBottom:
      Dec(ARect.Bottom, GrabberSize);
    gpLeft:
      Inc(ARect.Left, GrabberSize);
    gpRight:
      Dec(ARect.Right, GrabberSize);
  end;
end;

procedure TCnDockTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCnDockTree.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateAll;
  end;
end;

function TCnDockTree.FindControlZone(Control: TControl; IncludeHide: Boolean): TCnDockZone;
var
  CtlZone: TCnDockZone;

  procedure DoFindControlZone(StartZone: TCnDockZone);
  begin
    if (StartZone.ChildControl = Control) and (StartZone.Visibled or IncludeHide) then
      CtlZone := StartZone
    else begin
      // �������ֵ�
      if (CtlZone = nil) and (StartZone.NextSibling <> nil) then
        DoFindControlZone(StartZone.NextSibling);
      // ��������Ů
      if (CtlZone = nil) and (StartZone.ChildZones <> nil) then
        DoFindControlZone(StartZone.ChildZones);
    end;
  end;

begin
  CtlZone := nil;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone);
  Result := CtlZone;
end;

procedure TCnDockTree.ForEachAt(Zone: TCnDockZone; Proc: TCnForEachZoneProc;
  ScanKind: TTreeScanKind; ScanPriority: TTreeScanPriority);

  { ǰ����� }
  procedure DoForwardForEach(Zone: TCnDockZone);
  begin
    Proc(Zone);
    if ScanPriority = tspSibling then
    begin
      // �������ֵ�
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
      // ��������Ů
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
    end else
    begin
      // ��������Ů
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
      // �������ֵ�
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
    end;
  end;

  { ������� }
  procedure DoMiddleForEach(Zone: TCnDockZone);
  begin
    if ScanPriority = tspSibling then
    begin
      // �������ֵ�
      if Zone.NextSibling <> nil then DoMiddleForEach(Zone.NextSibling);
    end else
    begin
      // ��������Ů
      if Zone.ChildZones <> nil then DoMiddleForEach(Zone.ChildZones);
    end;

    Proc(Zone);

    if ScanPriority = tspSibling then
    begin
      // ��������Ů
      if Zone.ChildZones <> nil then DoMiddleForEach(Zone.ChildZones);
    end else
      // �������ֵ�
      if Zone.NextSibling <> nil then DoMiddleForEach(Zone.NextSibling);
  end;

  { ������� }
  procedure DoBackwardForEach(Zone: TCnDockZone);
  begin
    if ScanPriority = tspSibling then
    begin
      // �������ֵ�
      if Zone.NextSibling <> nil then DoBackwardForEach(Zone.NextSibling);
      // ��������Ů
      if Zone.ChildZones <> nil then DoBackwardForEach(Zone.ChildZones);
    end else
    begin
      // ��������Ů
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
      // �������ֵ�
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
    end;
    Proc(Zone);
  end;

begin
  { ����������Ĳ���Zone��nil,�ʹӸ�Ŀ¼��ʼ���� }
  if Zone = nil then
  begin
    if FTopZone = nil then
      FTopZone := FCnDockZoneClass.Create(Self);
    Zone := FTopZone;
  end;
  { ����ScanKind�������и��Եı��� }
  case ScanKind of
    tskForward: DoForwardForEach(Zone);
    tskMiddle:  DoMiddleForEach(Zone);
    tskBackward:DoBackwardForEach(Zone);
  end;
end;

procedure TCnDockTree.GetControlBounds(Control: TControl; out CtlBounds: TRect);
var
  Z: TCnDockZone;
begin
  Z := FindControlZone(Control);
  if Z = nil then
    FillChar(CtlBounds, SizeOf(CtlBounds), 0)
  else
    with Z do
    begin
      CtlBounds := Bounds(Left, Top, Width, Height);
//      AdjustRect(
    end;
end;

function TCnDockTree.HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl;
var
  Zone: TCnDockZone;
begin
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then Result := Zone.ChildControl
  else Result := nil;
end;

procedure TCnDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
const
{ Delphi6.0 }
{$IFDEF COMPILER6_UP}
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient); { alCustom }
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False, False);  { alCustom }
{$ELSE}
{ Delphi5.0 OR LAST }
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient);
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False);
{$ENDIF}

var
  Sibling,                  // ��һ���ֵ�
  Me: TCnDockZone;   // ��ǰ��Ҫ�������Ľڵ�
  InsertOrientation,        // ����ķ���
  CurrentOrientation: TDockOrientation;// ��ǰ�ķ���
  NewWidth, NewHeight: Integer;
  R: TRect;                 // �ؼ��ľ��δ�С
begin
//  if not Control.Visible then Exit;
  if FReplacementZone <> nil then
  begin
    { ���FReplacementZone <> nil��˵��������ִ��װ��ͣ����Ϣ�Ĳ��� }
    FReplacementZone.ChildControl := TWinControl(Control);
    FReplacementZone.Update;
    Exit;
  end
  else if FTopZone <> nil then
  begin
    if FTopZone.ChildZones = nil then
    begin
      // ������ǿյģ���Ҫ��ӵ�һ����Ů
      R := FDockSite.ClientRect;
      TCnWinControlAccess(FDockSite).AdjustClientRect(R);
      NewWidth := R.Right - R.Left;
      NewHeight := R.Bottom - R.Top;
      if TCnWinControlAccess(FDockSite).AutoSize then
      begin
        if NewWidth = 0 then NewWidth := Control.UndockWidth;
        if NewHeight = 0 then NewHeight := Control.UndockHeight;
      end;
      R := Bounds(R.Left, R.Top, NewWidth, NewHeight);
      AdjustDockRect(Control, R);
      Control.BoundsRect := R;
      Me := FCnDockZoneClass.Create(Self);
      FTopZone.ChildZones := Me;
      Me.FParentZone := FTopZone;
      Me.ChildControl := TWinControl(Control);
    end
    else begin
      // Ĭ����ͣ�����ұ�
      if InsertAt in [alClient, alNone] then InsertAt := alRight;
      { ����Control�Ƿ��Ѿ���ͣ������DockSite�У�
        ����ǵĻ���ɾ������ڵ� }
      Me := FindControlZone(Control, True);
      if Me <> nil then RemoveZone(Me, False);
      { ���ҵ�DropCtl���ڵĽڵ� }
      Sibling := FindControlZone(DropCtl);
      { ���ò���ķ��� }
      InsertOrientation := OrientArray[InsertAt];
      if FTopZone.ChildCount = 1 then
      begin
        // �����ֻ��һ����Ů�����ҵڶ������ڱ���ӽ�ȥ��
        // ���Է����λ�ñ��뱻��������
        FTopZone.Orientation := InsertOrientation;
        case InsertOrientation of
          doHorizontal:
            begin
              FTopZone.ZoneLimit := FTopZone.ChildZones.Width;
              TopXYLimit := FTopZone.ChildZones.Height;
            end;
          doVertical:
            begin
              FTopZone.ZoneLimit := FTopZone.ChildZones.Height;
              TopXYLimit := FTopZone.ChildZones.Width;
            end;
        end;
      end;
      { ����һ���ڵ㣬���Ұ�Control��ֵ������ڵ��ChildControl }
      Me := FCnDockZoneClass.Create(Self);
      Me.ChildControl := TWinControl(Control);
      { ����CurrentOrientation�ķ��� }
      if Sibling <> nil then
        { ��DropCtl���ڵĽڵ�ĸ��ڵ�ķ��� }
        CurrentOrientation := Sibling.FParentZone.Orientation
        { �Ǹ��ڵ�ķ��� }
      else CurrentOrientation := FTopZone.Orientation;
      if InsertOrientation = doNoOrient then
        InsertOrientation := CurrentOrientation;

      // �ؼ����ڱ�ͣ����һ��������ͬ����Ľڵ��ʱ��
      // ��Ҫ���Լ���ӵ��ֵܵ�ǰ������
      if InsertOrientation = CurrentOrientation then
        InsertSibling(Me, Sibling, MakeLast[InsertAt], True)
      else
      // �ؼ����ڱ�ͣ����һ��������ͬ����Ľڵ��ʱ��
      // ��Ҫ����һ�����ڵ㣬���ҽ��Լ����ֵ���Ϊ������ڵ����Ů��
      // ������ڵ�ķ���Ͳ��뷽��(InsertOrientation)��ͬ
        InsertNewParent(Me, Sibling, InsertOrientation, MakeLast[InsertAt], True);
    end;
    { ���»��ͻ�����ͣ����� }
    FDockSite.Invalidate;
  end;

(*  if FTopStoreZone <> nil then
  begin
    if FTopStoreZone.ChildZones = nil then
    begin
      Store := TCnDockZone.Create(Self, True);
      FTopStoreZone.ChildZones := Store;
      Store.FParentZone := FTopStoreZone;
      Store.ChildControl := TWinControl(Control);
    end else
    begin
      // Ĭ����ͣ�����ұ�
      if InsertAt in [alClient, alNone] then InsertAt := alRight;
      { ����Control�Ƿ��Ѿ���ͣ������DockSite�У�
        ����ǵĻ���ɾ������ڵ� }
      Store := FindControlZone(Control, True);
      if Store <> nil then RemoveZone(Store, True);
      { ���ҵ�DropCtl���ڵĽڵ� }
      Sibling := FindControlZone(DropCtl, True);
      { ���ò���ķ��� }
      InsertOrientation := OrientArray[InsertAt];
      if FTopStoreZone.ChildCount = 1 then
      begin
        // �����ֻ��һ����Ů�����ҵڶ������ڱ���ӽ�ȥ��
        // ���Է����λ�ñ��뱻��������
        FTopStoreZone.Orientation := InsertOrientation;
      end;

      { ����һ���ڵ㣬���Ұ�Control��ֵ������ڵ��ChildControl }
      Store := TCnDockZone.Create(Self, True);
      Store.ChildControl := TWinControl(Control);
      { ����CurrentOrientation�ķ��� }
      if Sibling <> nil then
        { ��DropCtl���ڵĽڵ�ĸ��ڵ�ķ��� }
        CurrentOrientation := Sibling.FParentZone.Orientation
        { �Ǹ��ڵ�ķ��� }
      else CurrentOrientation := FTopStoreZone.Orientation;
      if InsertOrientation = doNoOrient then
        InsertOrientation := CurrentOrientation;

      // �ؼ����ڱ�ͣ����һ��������ͬ����Ľڵ��ʱ��
      // ��Ҫ���Լ���ӵ��ֵܵ�ǰ������
      if InsertOrientation = CurrentOrientation then
        InsertSibling(Store, Sibling, MakeLast[InsertAt], False, True)
      else
      // �ؼ����ڱ�ͣ����һ��������ͬ����Ľڵ��ʱ��
      // ��Ҫ����һ�����ڵ㣬���ҽ��Լ����ֵ���Ϊ������ڵ����Ů��
      // ������ڵ�ķ���Ͳ��뷽��(InsertOrientation)��ͬ
        InsertNewParent(Store, Sibling, InsertOrientation, MakeLast[InsertAt], False, True);
    end;
  end;*)
end;

procedure TCnDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
var
  NewParent: TCnDockZone;
begin
  NewParent := FCnDockZoneClass.Create(Self);

  NewParent.Orientation := ParentOrientation;
  if SiblingZone = nil then
  begin
    // ���SiblingZone�ǿյĻ���������Ҫ������ڵ���Ϊ���ڵ����Ů��������
    NewParent.ZoneLimit := TopXYLimit;
    TopXYLimit := FTopZone.ZoneLimit;
    ShiftScaleOrient := ParentOrientation;
    ScaleBy := 0.5;
    if InsertLast then
    begin
      FTopZone.Visibled := FTopZone.VisibleChildCount > 0;
      NewParent.ChildZones := FTopZone;
      FTopZone.ParentZone := NewParent;
      FTopZone.NextSibling := NewZone;
      NewZone.PrevSibling := FTopZone;
      NewZone.ParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.ChildZones, ScaleZone, tskForward);
    end
    else begin
      NewParent.ChildZones := NewZone;
      FTopZone.ParentZone := NewParent;
      FTopZone.PrevSibling := NewZone;
      NewZone.NextSibling := FTopZone;
      NewZone.ParentZone := NewParent;
      FTopZone := NewParent;

      if ParentOrientation <> FTopZone.Orientation then
        NewZone.ZoneLimit := FTopZone.ZoneLimit div 2
      else NewZone.ZoneLimit := TopXYLimit div 2;

      ForEachAt(NewZone.NextSibling, ScaleZone, tskForward);
      if ParentOrientation <> FTopZone.Orientation then
        ShiftBy := FTopZone.ZoneLimit div 2
      else ShiftBy := TopXYLimit div 2;
      ForEachAt(NewZone.NextSibling, ShiftZone, tskForward);

    end;
    ForEachAt(nil, UpdateZone, tskForward);
  end
  else begin
    // ���SiblingZone���ǿյģ����Ǿ�Ҫ����һ���ڵ㣬
    // ����ڵ���Me��SiblingZone��ͬ�ĸ���
    NewParent.ZoneLimit := SiblingZone.ZoneLimit;
    NewParent.ParentZone := SiblingZone.ParentZone;
    NewParent.PrevSibling := SiblingZone.PrevSibling;
    if NewParent.PrevSibling <> nil then
      NewParent.PrevSibling.NextSibling := NewParent;
    NewParent.NextSibling := SiblingZone.NextSibling;
    if NewParent.NextSibling <> nil then
      NewParent.NextSibling.PrevSibling := NewParent;
    if NewParent.ParentZone.ChildZones = SiblingZone then
      NewParent.ParentZone.ChildZones := NewParent;
    NewZone.ParentZone := NewParent;
    SiblingZone.ParentZone := NewParent;
    if InsertLast then
    begin
      // ���뵽SiblingZone�ĺ���
      NewParent.ChildZones := SiblingZone;
      SiblingZone.ZoneLimit := NewParent.ParentZone.ZoneLimit;
      SiblingZone.PrevSibling := nil;
      SiblingZone.NextSibling := NewZone;
      NewZone.PrevSibling := SiblingZone;
    end
    else begin
      // ���뵽SiblingZone��ǰ��
      NewParent.ChildZones := NewZone;
      SiblingZone.PrevSibling := NewZone;
      SiblingZone.NextSibling := nil;
      NewZone.NextSibling := SiblingZone;
    end;
  end;
  if Update then
  begin
    // ������������Ů�ķ�Χ
    NewParent.ResetChildren(nil);
    ForEachAt(nil, UpdateZone, tskForward);
  end;
end;

procedure TCnDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
begin
  if (NewZone <> nil) and (SiblingZone <> nil) and
    (NewZone.ChildControl = SiblingZone.ChildControl) then
    SiblingZone := nil;
  if SiblingZone = nil then
  begin
    SiblingZone := FTopZone.ChildZones;
    if InsertLast then
      SiblingZone := SiblingZone.LastSibling;
  end;
  if InsertLast then
  begin
    // ��NewZone���뵽SiblingZone��
    NewZone.ParentZone := SiblingZone.ParentZone;
    NewZone.PrevSibling := SiblingZone;
    NewZone.NextSibling := SiblingZone.NextSibling;
    if NewZone.NextSibling <> nil then
      NewZone.NextSibling.PrevSibling := NewZone;
    SiblingZone.NextSibling := NewZone;
  end
  else begin
    // ��NewZone���뵽SiblingZoneǰ
    NewZone.NextSibling := SiblingZone;
    NewZone.PrevSibling := SiblingZone.PrevSibling;
    if NewZone.PrevSibling <> nil then
      NewZone.PrevSibling.NextSibling := NewZone;
    SiblingZone.PrevSibling := NewZone;
    NewZone.ParentZone := SiblingZone.ParentZone;
    if NewZone.ParentZone.ChildZones = SiblingZone then
      NewZone.ParentZone.ChildZones := NewZone;
  end;
  if Update then
  begin
    // �����������е��ֵ�
    SiblingZone.ParentZone.ResetChildren(nil);
    UpDateChild(SiblingZone.ParentZone);
  end;
end;

function TCnDockTree.DoFindZone(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
const HTFlagArr : array[Boolean] of Integer = (HTCLIENT, HTSPLITTER);
begin
  Result := nil;
  // ����Ƿ��ڷָ����ĵײ�...
  if (Zone.ParentZone.Orientation = doHorizontal) and
    (Zone.NextSibling <> nil) and
    ((MousePos.Y <= Zone.FZoneLimit) and
    (MousePos.Y >= Zone.FZoneLimit - SplitterWidth)) and
    ((MousePos.X <= Zone.ParentZone.FZoneLimit) and
    (MousePos.X >= Zone.ParentZone.LimitBegin)) then
  begin
    HTFlag := HTFlagArr[Zone.VisibleNextSiblingTotal > 0];
    Result := Zone;
  end

  // ����Ƿ��ڷָ�������...
  else if (Zone.FParentZone.Orientation = doVertical) and
    (Zone.NextSibling <> nil) and
    ((MousePos.X <= Zone.FZoneLimit) and
    (MousePos.X >= Zone.FZoneLimit - SplitterWidth)) and
    ((MousePos.Y <= Zone.ParentZone.FZoneLimit) and
    (MousePos.Y >= Zone.ParentZone.LimitBegin)) then
  begin
    HTFlag := HTFlagArr[Zone.VisibleNextSiblingTotal > 0];
    Result := Zone;
  end

  // ����Ƿ��ڰ�����...
  else if Zone.ChildControl <> nil then
  begin
    { ����Ƿ��ڱ߿���... }
    case GrabbersPosition of
      gpTop:    Result := GetTopGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpLeft:   Result := GetLeftGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpBottom: Result := GetBottomGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpRight:  Result := GetRightGrabbersHTFlag(MousePos, HTFlag, Zone);
    end;
    { ������ڰ����ϣ��Ͳ����Ƿ��ڱ߿���... }
    if Result = nil then
      Result := GetBorderHTFlag(MousePos, HTFlag, Zone);
  end else Result := nil;

  if (Result <> nil) and (not Result.Visibled) then Result := nil;
  
  // ������Ľڵ�...
  if (Result = nil) and (Zone.NextSibling <> nil) then
    Result := DoFindZone(MousePos, HTFlag, Zone.NextSibling);
  if (Result = nil) and (Zone.ChildZones <> nil) then
    Result := DoFindZone(MousePos, HTFlag, Zone.ChildZones);
end;

function TCnDockTree.InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TCnDockZone;
var
  ResultZone: TCnDockZone;

  function FindControlAtPos(const Pos: TPoint): TControl;
  var
    I: Integer;
    P: TPoint;
  begin
    for I := FDockSite.ControlCount - 1 downto 0 do
    begin
      Result := FDockSite.Controls[I];
      with Result do
      begin
        { �ؼ����뱻��ʾ����... }
        if not Result.Visible or ((Result is TWinControl) and
           not TWinControl(Result).Showing) then continue;
        P := Point(Pos.X - Left, Pos.Y - Top);
        if PtInRect(ClientRect, P) then Exit;
      end;
    end;
    Result := nil;
  end;

var
  CtlAtPos: TControl;
begin
  ResultZone := nil;
  HTFlag := HTNOWHERE;
  CtlAtPos := FindControlAtPos(MousePos);
  if (CtlAtPos <> nil) and (CtlAtPos.HostDockSite = FDockSite) then
  begin
    ResultZone := FindControlZone(CtlAtPos);
    if ResultZone <> nil then HTFlag := HTCLIENT;
  end
  else if (FTopZone <> nil) and (FTopZone.ChildZones <> nil) and (FTopZone.ChildCount >= 1) and
    (CtlAtPos = nil) then
    ResultZone := DoFindZone(MousePos, HTFlag, FTopZone.ChildZones);
  Result := ResultZone;
end;

procedure TCnDockTree.LoadFromStream(Stream: TStream);
var
//  CompName: string;
//  Client: TControl;
//  I, InVisCount: Integer;
  I: Integer;
begin
  PruneZone(FTopZone);
  // ���汾, ����汾��ͬ,���˳�
  Stream.Read(I, SizeOf(I));
  if I <> Version then
    Exit;

  BeginUpdate;
  try
    // �����ڵ������
    Stream.Read(FTopXYLimit, SizeOf(FTopXYLimit));
    // ���������Ľڵ�
    DoLoadZone(Stream);
  finally
    EndUpdate;
  end;
end;

{procedure TCnDockTree.PaintDockGrabber(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);

  procedure DrawCloseButton(Left, Top: Integer);
  begin
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+FGrabberSize-2,
      Top+FGrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
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

begin
  with ARect do
  begin
    case GrabbersPosition of
      gpLeft:
      begin
        DrawCloseButton(Left+BorderWidth+BorderWidth+1, Top+BorderWidth+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+3, Top+FGrabberSize+BorderWidth+1, Left+BorderWidth+5, Bottom+BorderWidth-2);
        DrawGrabberLine(Left+BorderWidth+6, Top+FGrabberSize+BorderWidth+1, Left+BorderWidth+8, Bottom+BorderWidth-2);
      end;
      gpTop:
      begin
        DrawCloseButton(Right-FGrabberSize+BorderWidth+1, Top+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+3, Right-FGrabberSize+BorderWidth-2, Top+BorderWidth+5);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+6, Right-FGrabberSize+BorderWidth-2, Top+BorderWidth+8);
      end;
//      gpBottom:
//      gpRight:
    end;
  end;
end;}

procedure TCnDockTree.PaintSite(DC: HDC);
(*var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
  DockClient: TCnDockClient;
begin
  { ����һ��TControlCanvas���� }
  Canvas := TControlCanvas.Create;
  try
    { Ȼ���DockSite����Canvas��Control���ԣ�����Canvas�Ϳ�����DockSite�ϻ�ͼ���� }
    Canvas.Control := FDockSite;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        { ��ʼѭ���ػ�DockSite�ϵ�Clients }
        for I := 0 to FDockSite.ControlCount - 1 do
        begin
          Control := FDockSite.Controls[I];
          if Control.Visible and (Control.HostDockSite = FDockSite) then
          begin
            { ���ҵ�Control�����TCnDockClient�� }
            DockClient := FindDockClient(Control);
            { ��ð��ֵľ��δ�С }
            R := GetFrameRect(Control);
            { ������ }
            PaintDockGrabber(Canvas, Control, R);
            { �����û��Զ��廭�¼� }
            if DockClient <> nil then
              DockClient.DoPaintDockGrabber(Canvas, Control, R);
          end;
        end;
        SplitterCanvas := Canvas;
        SplitterCanvas.Brush.Color := TCnWinControlAccess(DockSite).Color;
        { ���ָ��� }
        ForEachAt(nil, DrawSplitter, tskBackward);
        SplitterCanvas := nil;
        { ��DockSite�ı߿� }
        PaintDockSiteRect(Canvas);
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;*)

begin
  { ����һ��TControlCanvas���� }
  FCanvas := TControlCanvas.Create;
  try
    { Ȼ���DockSite����Canvas��Control���ԣ�����Canvas�Ϳ�����DockSite�ϻ�ͼ���� }
    FCanvas.Control := FDockSite;
    FCanvas.Lock;
    try
      FCanvas.Handle := DC;
      try
        PaintDockSite;
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
    end;
  finally
    FCanvas.Free;
    FCanvas := nil;
  end;

end;

procedure TCnDockTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  VisibleClients,
  NewX, NewY, NewWidth, NewHeight: Integer;
begin
  VisibleClients := FDockSite.VisibleDockClientCount;
  { ��DockSiteС������ͣ���ؼ��������棬DockRect��Ӧ����Ϊ�����ó�DockSite�Ŀͻ����� }
  if (DropCtl = nil) or (DropCtl.DockOrientation = doNoOrient) or
     {(DropCtl = Client) or }(VisibleClients < 2) then
  begin
    DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    { ��������һ��ͣ���ͻ����ǰ�DockSite�Ŀͻ����ֳ�һ�� }
    if VisibleClients > 0 then
    with DockRect do
      case DropAlign of
        alLeft: Right := Right div 2;
        alRight: Left := Right div 2;
        alTop: Bottom := Bottom div 2;
        alBottom: Top := Bottom div 2;
      end;
  end
  else begin
    { ���ߣ����DockSite��������һ���ͻ���ʱ�� �����������Ŀؼ�����DockRect������}
    NewX := DropCtl.Left;
    NewY := DropCtl.Top;
    NewWidth := DropCtl.Width;
    NewHeight := DropCtl.Height;
    if DropAlign in [alLeft, alRight] then
      NewWidth := DropCtl.Width div 2
    else if DropAlign in [alTop, alBottom] then
      NewHeight := DropCtl.Height div 2;
    case DropAlign of
      alRight: Inc(NewX, NewWidth);
      alBottom: Inc(NewY, NewHeight);
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
    if DropAlign = alClient then
      DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
  end;
  MapWindowPoints(FDockSite.Handle, 0, DockRect, 2);
end;

procedure TCnDockTree.PruneZone(Zone: TCnDockZone);

  procedure DoPrune(Zone: TCnDockZone);
  begin
    // �������ֵ�
    if Zone.NextSibling <> nil then
      DoPrune(Zone.NextSibling);
    // ��������Ů
    if Zone.ChildZones <> nil then
      DoPrune(Zone.ChildZones);
    // ɾ���ڵ�
    Zone.Free;
  end;

begin
  if Zone = nil then Exit;
  // ���ȵݹ��ɾ����Ů
  if Zone.ChildZones <> nil then DoPrune(Zone.ChildZones);
  // �������Zone
  if Zone.FPrevSibling <> nil then
    Zone.FPrevSibling.NextSibling := Zone.NextSibling
  else if Zone.FParentZone <> nil then
    Zone.FParentZone.ChildZones := Zone.NextSibling;
  if Zone.NextSibling <> nil then
    Zone.NextSibling.FPrevSibling := Zone.FPrevSibling;
  // ɾ�����Zone
  if Zone = FTopZone then FTopZone := nil;
  Zone.Free;
end;

procedure TCnDockTree.RemoveControl(Control: TControl);
var
  Z: TCnDockZone;
begin
  Z := FindControlZone(Control, True);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.ChildControl := nil
    else
    begin
      if (Z.ParentZone.Orientation <> doNoOrient) and Z.Visibled then
        Z.Remove(Z.LimitSize, False);
      RemoveZone(Z, False);
      SetNewBounds(nil);
      UpdateAll;
    end;
    Control.DockOrientation := doNoOrient;
    { �ػ�DockSite }
    FDockSite.Invalidate;
  end;
end;

procedure TCnDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
var
  Sibling, LastChild: TCnDockZone;
  VisibleZoneChildCount,
  ZoneChildCount: Integer;
  label LOOP;
begin
  if not Hide then
  begin
    if Zone = nil then
      raise Exception.Create(SDockTreeRemoveError + SDockZoneNotFound);
    if Zone.ChildControl = nil then
      raise Exception.Create(SDockTreeRemoveError + SDockZoneHasNoCtl);
    VisibleZoneChildCount := Zone.ParentZone.VisibleChildCount;
    ZoneChildCount := Zone.ParentZone.ChildCount;
    if VisibleZoneChildCount <= 1 then
    begin
      // �����ǰ�ڵ�ĸ��ڵ�ֻ��һ���ɼ��Ľڵ㣬����Ҫ�Ѹ��ڵ����ص�

      // ��������ǰ�ֵܺͺ��ֵ�
      if Zone.PrevSibling = nil then
      begin
        Zone.ParentZone.ChildZones := Zone.NextSibling;
        if Zone.NextSibling <> nil then
          Zone.NextSibling.PrevSibling := nil;
      end else if Zone.NextSibling = nil then
        Zone.PrevSibling.NextSibling := nil
      else
      begin
        Zone.PrevSibling.NextSibling := Zone.NextSibling;
        Zone.NextSibling.PrevSibling := Zone.PrevSibling;
      end;
      // ���ص����ڵ㣬ע�⣬����һ���ݹ麯����������ڵ�ĸ��ڵ�Ŀɼ���Ů����Ҳ��0��
      // ��Ҫ���ص����ڵ�ĸ��ڵ�
{      if Zone.ParentZone <> TopZone then
        Zone.ParentZone.Remove(Zone.ParentZone.LimitSize, True)
      else}
//      goto LOOP;
//      Exit;
    end;
    if ZoneChildCount = 2 then
    begin
      // ����ڵ�ֻ��һ���ֵܽڵ�
      if Zone.PrevSibling = nil then Sibling := Zone.NextSibling
      else Sibling := Zone.PrevSibling;
      if Sibling.ChildControl <> nil then
      begin
        // �ֵܽڵ���һ����ChildControl����û���ӽڵ�Ľڵ�
        if Zone.ParentZone = FTopZone then
        begin
          // ������ڵ��Ǹ��ڵ㣬��ɾ������ڵ�
          FTopZone.ChildZones := Sibling;
          Sibling.PrevSibling := nil;
          Sibling.NextSibling := nil;
          Sibling.ZoneLimit := FTopZone.LimitSize;
          Sibling.Update;
        end
        else begin
          // ���򣬾Ͱ��ֵܽڵ��ChildControl�ƶ������ڵ��ϣ�Ȼ��ɾ������ֵܽڵ�
          Zone.ParentZone.Orientation := doNoOrient;
          Zone.ParentZone.ChildControl := Sibling.ChildControl;
          Zone.ParentZone.ChildZones := nil;
          Sibling.Free;
        end;
        // ���¸��ڵ�
        ForEachAt(Zone.ParentZone, UpdateZone, tskForward);
      end
      else begin
        // �ֵܽڵ���һ�����ӽڵ�Ľڵ㣬�����ֵܽڵ���뱻�ŵ����ڵ��ϣ�
        // ���߷ŵ���һ��
        if Zone.ParentZone = FTopZone then
        begin
          // �ڵ��Ǹ��ڵ���ӽڵ㣬�����ֵܽڵ�����Ϊ���ڵ�
          Sibling.ZoneLimit := TopXYLimit;
          TopXYLimit := FTopZone.ZoneLimit;
          FTopZone.Free;
          FTopZone := Sibling;
          Sibling.NextSibling := nil;
          Sibling.PrevSibling := nil;
          Sibling.ParentZone := nil;
        end
        else
        begin
          // �ڵ�ĸ��ڵ㲻�Ǹ��ڵ㣬�����ӽڵ���뱻�ŵ����ڵ���
          Sibling.ChildZones.PrevSibling := Zone.ParentZone.PrevSibling;
          if Sibling.ChildZones.PrevSibling = nil then
            Zone.ParentZone.ParentZone.ChildZones := Sibling.ChildZones
          else
            Sibling.ChildZones.PrevSibling.NextSibling := Sibling.ChildZones;
          LastChild := Sibling.ChildZones;
          LastChild.ParentZone := Zone.ParentZone.ParentZone;
          repeat
            LastChild := LastChild.NextSibling;
            if LastChild <> nil then
              LastChild.ParentZone := Zone.ParentZone.ParentZone
            else
              Break;
          until LastChild.NextSibling = nil;
          if LastChild <> nil then
          begin
          LastChild.NextSibling := Zone.ParentZone.NextSibling;
            if LastChild.NextSibling <> nil then
              LastChild.NextSibling.PrevSibling := LastChild;
            ForEachAt(LastChild.ParentZone, UpdateZone, tskForward);
          end;
          Zone.ParentZone.Free;
          Sibling.Free;
        end;
      end;
    end
    else begin
      // ����ڵ��ж���ֵܽڵ�
      if Zone.PrevSibling = nil then
      begin
        // ���ڸ��ڵ�ĵ�һ���ڵ㣬����ʹ��һ���ֵܽڵ���Ϊ���ڵ����Ů��
        // ���Ұѱ���ɾ����
        Zone.ParentZone.ChildZones := Zone.NextSibling;
        if Zone.NextSibling <> nil then
        begin
          Zone.NextSibling.PrevSibling := nil;
          Zone.NextSibling.Update;
        end;
      end
      else begin
        // �����ڸ��ڵ�ĵ�һ���ڵ㣬����ɾ������ڵ㣬���ҵ����ֵܽڵ�
        Zone.PrevSibling.NextSibling := Zone.NextSibling;
        if Zone.NextSibling <> nil then
          Zone.NextSibling.PrevSibling := Zone.PrevSibling;
        Zone.PrevSibling.ZoneLimit := Zone.ZoneLimit;
        Zone.PrevSibling.Update;
      end;
      ForEachAt(Zone.ParentZone, UpdateZone, tskForward);
    end;
LOOP:
    Zone.Free;
  end;
  SetNewBounds(nil);
  UpdateAll;
end;

procedure TCnDockTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in FDockSite.ComponentState) and
    (FTopZone <> nil) and (FDockSite.DockClientCount > 0) then
  begin
    R := FDockSite.ClientRect;
    TCnWinControlAccess(FDockSite).AdjustClientRect(R);
    if Force or (not CompareMem(@R, @FOldRect, SizeOf(TRect))) then
    begin
      FOldRect := R;
      case FTopZone.Orientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := R.Right - R.Left;
            // ��R.Bottom = R.Top��ʱ��˵��DockSite�ĸ߶�Ϊ0��
            // ��Ϊ�������ڴ������ø߶Ⱥ��ٵ���ZoneLimit�ģ�
            // �������������ʹ���߶����ó�0��ʱ���ܵ���TopXYLimit
            if R.Bottom - R.Top > 0 then
              TopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := R.Bottom - R.Top;
            // ��R.Right = R.Left��ʱ��˵��DockSite�Ŀ��Ϊ0��
            // ��Ϊ�������ڴ������ÿ�Ⱥ��ٵ���ZoneLimit�ģ�
            // �������������ʹ��������ó�0��ʱ���ܵ���TopXYLimit
            if R.Right - R.Left > 0 then
              TopXYLimit := R.Right - R.Left;
          end;
      end;
      SetNewBounds(nil);
      if FUpdateCount = 0 then ForEachAt(nil, UpdateZone, tskForward);
    end;
  end;
end;

procedure TCnDockTree.ScaleZone(Zone: TCnDockZone);
begin
  { ScaleZone�Ĺ�ʽ��ScaleChildZone��ʽ���������,
    ��FParentLimit��0��ʱ��,��������ʽ��� }
  FParentLimit := 0;
  ScaleChildZone(Zone);
end;

procedure TCnDockTree.SaveToStream(Stream: TStream);
begin
  // д���İ汾
  Stream.Write(FVersion, SizeOf(FVersion));
  // д���ڵ�����
  Stream.Write(FTopXYLimit, SizeOf(FTopXYLimit));
  // ������д���нڵ������
  DoSaveZone(Stream, FTopZone, 0);
  Stream.Write(TreeStreamEndFlag, SizeOf(TreeStreamEndFlag));
end;

procedure TCnDockTree.SetNewBounds(Zone: TCnDockZone);

  procedure DoSetNewBounds(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      if (Zone.VisibleNextSiblingCount = 0) and (Zone <> FTopZone) then
      begin
        if Zone.ParentZone = FTopZone then
          Zone.ZoneLimit := FTopXYLimit
        else
          Zone.ZoneLimit := Zone.ParentZone.ParentZone.FZoneLimit;
      end;
      if Zone.ChildZones <> nil then DoSetNewBounds(Zone.ChildZones);
      if Zone.NextSibling <> nil then DoSetNewBounds(Zone.NextSibling);
    end;
  end;

begin
  if IsLoading then Exit;
  if Zone = nil then Zone := FTopZone.ChildZones;
  DoSetNewBounds(Zone);
  { ���»�ͣ����� }
  FDockSite.Invalidate;
end;

procedure TCnDockTree.SetReplacingControl(Control: TControl);
begin
  FReplacementZone := FindControlZone(Control);
end;

procedure TCnDockTree.ShiftZone(Zone: TCnDockZone);
begin
  if (Zone <> nil) and (Zone <> FTopZone) and
    (Zone.ParentZone.Orientation = FShiftScaleOrient) then
  begin
    Inc(Zone.FZoneLimit, FShiftBy);
    if Zone.LimitSize < FMinSize then
      Zone.FZoneLimit := Zone.LimitBegin + FMinSize;
  end;
end;

procedure TCnDockTree.SplitterMouseDown(OnZone: TCnDockZone; MousePos: TPoint);
begin
  FSizingZone := OnZone;
  Mouse.Capture := FDockSite.Handle;
  FSizingWnd := FDockSite.Handle;
  FSizingDC := GetDCEx(FSizingWnd, 0, DCX_CACHE or DCX_CLIPSIBLINGS or
    DCX_LOCKWINDOWUPDATE);
  FSizePos := MousePos;
  DrawSizeSplitter;
end;

procedure TCnDockTree.SplitterMouseUp;

  procedure SetSiblingZoneSize(PosXY: Integer);
  var AZone: TCnDockZone;
    PrevCount, NextCount: Integer;
  begin
    { ����PrevSibling }
    PrevCount := FSizingZone.PrevSiblingCount;
    AZone := FSizingZone.ParentZone.ChildZones;
    while (AZone <> nil) and (AZone <> FSizingZone) do
    begin
      if AZone.ZoneLimit >= PosXY - PrevCount * MinSize +
        Integer(AZone.PrevSibling = nil) * (SplitterWidth div 2) then
      begin
        AZone.ZoneLimit := PosXY - PrevCount * MinSize +
          Integer(AZone.PrevSibling = nil) * (SplitterWidth div 2);
        Break;
      end;
      Dec(PrevCount);
      AZone := AZone.NextSibling;
    end;

    AZone := AZone.NextSibling;
    while PrevCount > 0 do
    begin
      Dec(PrevCount);
      AZone.ZoneLimit := AZone.LimitBegin + MinSize;
      AZone := AZone.NextSibling;
    end;

    { ����NextSibling }
    NextCount := 1;
    AZone := FSizingZone.NextSibling;
    while (AZone <> nil) do
    begin
      if AZone.ZoneLimit <= PosXY + NextCount * MinSize +
        Integer(AZone.NextSibling <> nil) * (SplitterWidth div 2) then
        AZone.ZoneLimit := PosXY + NextCount * MinSize +
          Integer(AZone.NextSibling <> nil) * (SplitterWidth div 2);
      Inc(NextCount);
      AZone := AZone.NextSibling;
    end;
  end;

begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(FSizingWnd, FSizingDC);
  if FSizingZone.ParentZone.Orientation = doHorizontal then
  begin
    FSizingZone.ZoneLimit := FSizePos.y + (SplitterWidth div 2);
    SetSiblingZoneSize(FSizePos.y);
  end else
  begin
    FSizingZone.ZoneLimit := FSizePos.x + (SplitterWidth div 2);
    SetSiblingZoneSize(FSizePos.x);
  end;
  SetNewBounds(FSizingZone.ParentZone);
  ForEachAt(FSizingZone.ParentZone, UpdateZone, tskForward);
  FSizingZone := nil;
end;

procedure TCnDockTree.UpdateAll;
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(nil, UpdateZone, tskForward);
end;

procedure TCnDockTree.UpdateZone(Zone: TCnDockZone);
begin
  if (FUpdateCount = 0) then
    Zone.Update;
end;

procedure TCnDockTree.DrawSizeSplitter;
var
  R: TRect;
  PrevBrush: HBrush;
begin
  if FSizingZone <> nil then
  begin
    with R do
    begin
      if FSizingZone.ParentZone.Orientation = doHorizontal then
      begin
        Left := FSizingZone.Left;
        Top := FSizePos.Y - (SplitterWidth div 2);
        Right := Left + FSizingZone.Width;
        Bottom := Top + SplitterWidth;
      end
      else begin
        Left := FSizePos.X - (SplitterWidth div 2);
        Top := FSizingZone.Top;
        Right := Left + SplitterWidth;
        Bottom := Top + FSizingZone.Height;
      end;
    end;
    PrevBrush := SelectObject(FSizingDC, FBrush.Handle);
    with R do
      PatBlt(FSizingDC, Left, Top, Right - Left, Bottom - Top, PATINVERT);
    SelectObject(FSizingDC, PrevBrush);
  end;
end;

function TCnDockTree.GetSplitterLimit(AZone: TCnDockZone; IsCurrent, IsMin: Boolean): Integer;
begin
  if IsCurrent then
  begin
    Result := AZone.GetSplitterLimit(False);
  end else
  begin
    if AZone.AfterClosestVisibleZone <> nil then
      Result := AZone.AfterClosestVisibleZone.GetSplitterLimit(True)
    else
      Result := AZone.ZoneLimit + AZone.LimitSize;
  end;
end;

procedure TCnDockTree.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);
begin
  if Visible then
  begin
    ShowControl(Control);
  end else
    HideControl(Control);
end;

procedure TCnDockTree.WindowProc(var Message: TMessage);
var TempZone: TCnDockZone;
  HitTestValue: Integer;
begin
  case Message.Msg of
    CM_DOCKNOTIFICATION:
      with TCMDockNotification(Message) do
        if (NotifyRec.ClientMsg = CM_VISIBLECHANGED) then
          ControlVisibilityChanged(Client, Boolean(NotifyRec.MsgWParam));
    WM_MOUSEMOVE:
      { ��������ƶ�����Ϣ }
      DoMouseMove(TWMMouse(Message), TempZone, HitTestValue);
    WM_LBUTTONDBLCLK:
      { ����������˫������Ϣ }
      DoLButtonDbClk(TWMMouse(Message), TempZone, HitTestValue);
    WM_LBUTTONDOWN:
      { �������������µ���Ϣ }
      if DoLButtonDown(TWMMouse(Message), TempZone, HitTestValue) then Exit;
    WM_LBUTTONUP:
      { �����������ſ�����Ϣ }
      DoLButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_MBUTTONDOWN:
      { ��������м����µ���Ϣ }
      DoMButtonDown(TWMMouse(Message), TempZone, HitTestValue);
    WM_MBUTTONUP:
      { ��������м��ſ�����Ϣ }
      DoMButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_RBUTTONDOWN:
      { ��������Ҽ����µ���Ϣ }
      DoRButtonDown(TWMMouse(Message), TempZone, HitTestValue);
    WM_RBUTTONUP:
      { ��������Ҽ��ſ�����Ϣ }
      DoRButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_SETCURSOR:
      begin
        { �������ù�����Ϣ }
        DoSetCursor(TWMSetCursor(Message), TempZone, HitTestValue);
        if Message.Result = 1 then Exit;
      end;
  end;

  { �����ϵ�WndProc }
  FOldWndProc(Message);

  if Message.Msg = CM_HINTSHOW then
    { ������ʾ������ʾ����Ϣ }
    DoHintShow(TCMHintShow(Message), TempZone, HitTestValue);
end;

procedure TCnDockTree.SetGrabberSize(const Value: Integer);
begin
  if FGrabberSize <> Value then
  begin
    FGrabberSize := Value;
    UpdateAll;
    Docksite.Invalidate;
  end;
end;

function TCnDockTree.GetGrabbersPosition: TGrabbersPosition;
begin
  if DockSite.Align in [alTop, alBottom] then
    Result := gpLeft
  else Result := gpTop;
end;

function TCnDockTree.GetBottomGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  Result := nil;
end;

function TCnDockTree.GetBorderHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
var ARect: TRect;
begin
  Result := nil;
  { ��ýڵ�ľ��δ�С }
  ARect := Zone.GetFrameRect;
  { ������������ھ����� }
  if PtInRect(ARect, MousePos) then
  begin
    { ��ȥ�߿�Ŀ�� }
    InflateRect(ARect, -BorderWidth, -BorderWidth);
    if not PtInRect(ARect, MousePos) then
    begin
      Result := Zone;
      HTFlag := HTBORDER;
    end;
  end;
end;

function TCnDockTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + FGrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    if MousePos.Y < Zone.ChildControl.Top + FGrabberSize + 3 then HTFlag := HTCLOSE
    else HTFlag := HTCAPTION;
  end else Result := nil;
end;

function TCnDockTree.GetRightGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  Result := nil;
end;

function TCnDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + FGrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
      if MousePos.X > Left + Width - FGrabberSize - 3 then HTFlag := HTCLOSE
      else HTFlag := HTCAPTION;
  end else Result := nil;
end;

function TCnDockTree.GetActiveControl: TControl;
begin
  Result := FActiveControl;
end;

procedure TCnDockTree.SetActiveControl(const Value: TControl);
begin
  FActiveControl := Value;
end;

function TCnDockTree.GetGrabberSize: Integer;
begin
  Result := FGrabberSize;
end;

function TCnDockTree.FindControlZoneAndLevel(Control: TControl;
  var CtlLevel: Integer; IncludeHide: Boolean): TCnDockZone;
var
  CtlZone: TCnDockZone;

  procedure DoFindControlZone(StartZone: TCnDockZone; Level: Integer);
  begin
    if (StartZone.ChildControl = Control) and (StartZone.Visibled or IncludeHide) then
    begin
      CtlZone := StartZone;
      CtlLevel := Level;
    end
    else begin
      // �������ֵ�
      if (CtlZone = nil) and (StartZone.NextSibling <> nil) then
        DoFindControlZone(StartZone.NextSibling, Level);
      // ��������Ů
      if (CtlZone = nil) and (StartZone.ChildZones <> nil) then
        DoFindControlZone(StartZone.ChildZones, Level + 1);
      if (CtlZone <> nil) and (not CtlZone.Visibled) then CtlZone := nil;
    end;
  end;

begin
  CtlZone := nil;
  CtlLevel := 0;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone, 0);
  Result := CtlZone;
end;

procedure TCnDockTree.SetSplitterWidth(const Value: Integer);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    if FUpdateCount <= 0 then
      UpdateAll;
  end;
end;

procedure TCnDockTree.SetTopZone(const Value: TCnDockZone);
begin
  FTopZone := Value;
end;

procedure TCnDockTree.SetTopXYLimit(const Value: Integer);
begin
  FTopXYLimit := Value;
end;

procedure TCnDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);

var Control: TControl;
  DockClient: TCnDockClient;
begin
  { �����갴�µĵط��Ƿָ�����λ���� }
  if FSizingZone <> nil then
  begin
    { ���ָ�������״ }
    DrawSizeSplitter;
    { ������λ�ñ������� }
    FSizePos := SmallPointToPoint(Message.Pos);
    { ��������λ�ã�������һЩ���� }
    CalcSplitterPos;
    { ���ָ�������״ }
    DrawSizeSplitter;
  end;
  { ��������λ�� }
  Zone := InternalHitTest(SmallPointToPoint(Message.Pos), HTFlag);
  if Zone <> nil then
  begin
    { ���ȵ���TCnDockClient�е�DoNCMouseMove���� }
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      DockClient.DoNCMouseMove(Cn_CreateNCMessage(
        DockSite, WM_NCMOUSEMOVE, HTFlag, FSizePos), msConjoin);
    Control := Zone.ChildControl;
  end
  else Control := nil;
  if (Control <> nil) and (HTFlag <> FOldHTFlag) then
  begin
    { �������λ�ú�ԭ����λ�ò�һ�����͸�����ʾ���� }
    Application.HideHint;
    Application.HintMouseMessage(Control, TMessage(Message));
    Application.ActivateHint(SmallPointToPoint(Message.Pos));
    FOldHTFlag := HTFlag;
  end;
end;

function TCnDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var P: TPoint;
  Msg: TMsg;
//  DockClient: TCnDockClient;
begin
  Result := False;
  P := SmallPointToPoint(Message.Pos);
  { ��������λ�ú͵�ǰ�Ľڵ� }
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) then
  begin
    if HTFlag = HTSPLITTER then
      { �������λ�øպ��ڷָ�����λ���ϣ�
      �͵���SplitterMouseDown��������һЩ��Ҫ�Ĵ��� }
      SplitterMouseDown(Zone, P)
    else if (HTFlag = HTCAPTION) or (HTFlag = HTBORDER) then
    begin
      { �������λ�����ڱ������� }
      { ��ȫ�ֱ���GlobalDockClient��ֵ }
      GlobalDockClient := FindDockClient(Zone.ChildControl);
      if GlobalDockClient <> nil then
        { ���ȵ���TCnDockClient�е�DoNCButtonDown���� }
        GlobalDockClient.DoNCButtonDown(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONDOWN, HTFlag, P), mbLeft, msConjoin);

      if (not PeekMessage(Msg, FDockSite.Handle, WM_LBUTTONDBLCLK,
        WM_LBUTTONDBLCLK, PM_NOREMOVE)) and
        (Zone.ChildControl is TWinControl) then
        { ʹ�ڵ�����Ŀؼ���ý��� }
//        if not Zone.ChildControl.Focused then
//          TWinControl(Zone.ChildControl).SetFocus;
        if (GetActiveControl <> Zone.ChildControl) and Zone.ChildControl.CanFocus then
          Zone.ChildControl.SetFocus;
        if (TCnWinControlAccess(Zone.ChildControl).DragKind = dkDock) and
        (TCnWinControlAccess(Zone.ChildControl).DragMode = dmAutomatic)then
        begin
          { ��ʼ�϶� }
          BeginDrag(Zone.ChildControl, True);
        end;
      Result := True;
    end;
  end;
end;

procedure TCnDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  if FSizingZone = nil then
  begin
    { �����갴�µ�λ�ò����ڷָ����ϣ�
    Ҳ����˵���ڱ��������棬���ж����������ڱ��������ĸ�����λ�� }
    P := SmallPointToPoint(Message.Pos);
    Zone := InternalHitTest(P, HTFlag);
    if (Zone <> nil) then
    begin
      if (HTFlag <> HTSPLITTER) and (Zone.ChildControl <> nil) then
      begin
        DockClient := FindDockClient(Zone.ChildControl);
        if DockClient <> nil then
          { ���ȵ���TCnDockClient�е�DoNCButtonDown���� }
          DockClient.DoNCButtonUp(Cn_CreateNCMessage(
            DockSite, WM_NCLBUTTONUP, HTFlag, P), mbLeft, msConjoin);
        if (HTFlag = HTCLOSE) then
        begin
          if (DockClient <> nil) and (not DockClient.EnableCloseBtn) then Exit;
          DoHideZoneChild(Zone);
        end;
      end;
    end;
  end
  else
    { �����갴�µ�λ�����ڷָ����ϣ��͵���SplitterMouseUp��������һЩ��Ҫ������}
    SplitterMouseUp;
end;

procedure TCnDockTree.DoLButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var //AControl: TWinControl;
  P: TPoint;
begin
  { �����ж�����λ���������� }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil)
    and (HTFlag = HTCAPTION) or (HTFlag = HTBORDER) then
  begin
    if (HTFlag <> HTSPLITTER) then
      { ���ȵ���TCnDockClient�е�DoNCButtonDown���� }
      GlobalDockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
        DockSite, WM_NCLBUTTONUP, HTFlag, P), mbLeft, msConjoin);
    if GlobalDockClient.CanFloat then
    begin
      { �������λ�����ڱ��������棬��ʹ�ڵ�����Ŀؼ����� }
      CnGlobalDockPresident.CancelDrag;
      Zone.LButtonDbClkMothed;
    end;
    Zone := nil;
  end;
end;

procedure TCnDockTree.DoSetCursor(var Message: TWMSetCursor;
  var Zone: TCnDockZone; out HTFlag: Integer);
var
  P: TPoint;
begin
  { �������λ�ò��ҽ�������ת�� }
  GetCursorPos(P);
  P := FDockSite.ScreenToClient(P);
  with Message do
    if (Smallint(HitTest) = HTCLIENT) and (CursorWnd = FDockSite.Handle)
      and (FDockSite.VisibleDockClientCount > 0) then
    begin
      { ȷ��������ĸ�λ�� }
      Zone := InternalHitTest(P, HTFlag);
      if (Zone <> nil) and (HTFlag = HTSPLITTER) then
      begin
        { ���������ڷָ����ϣ��͵���SetSplitterCursor���ù�����״ }
        SetSplitterCursor(Zone.ParentZone.Orientation);
        Result := 1;
      end;
    end;
end;

procedure TCnDockTree.DoHintShow(var Message: TCMHintShow;
  var Zone: TCnDockZone; out HTFlag: Integer);
var
  Control: TWinControl;
  R: TRect;
  ADockClient: TCnDockClient;
  CanShow: Boolean;
begin
  with Message do
  begin
    if Result = 0 then
    begin
      { ȷ��������ĸ�λ�� }
      Zone := InternalHitTest(Message.HintInfo.CursorPos, HTFlag);
      if Zone <> nil then
        Control := Zone.ChildControl
      else Control := nil;
      { ���ҵ�Control�����TCnDockClient���������ShowHint����ΪFalse�����˳� }
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and (not ADockClient.ShowHint) then
        Exit;

      if HTFlag = HTSPLITTER then
        HintInfo^.HintStr := ''
      else if (Control <> nil){ and (HTFlag in [HTCAPTION, HTCLOSE]) }then
      begin
        { ������ʾ����Ĵ�С }
        R := GetFrameRect(Control);
        if HTFlag = HTCAPTION then
        begin
          { ����������ʾ��Ϣ }
          HintInfo^.HintStr := TCnWinControlAccess(Control).Caption;
        end else if HTFlag = HTCLOSE then
          { �رհ�ť����ʾ��Ϣ }
          HintInfo^.HintStr := gs_CnDockTreeCloseBtnHint
        else DoOtherHint(Zone, HTFlag, HintInfo^.HintStr);

        HintInfo^.CursorRect := R;
        { ����TCnDockClient��DoFormShowHint������}
        CanShow := True;
        if ADockClient <> nil then
          ADockClient.DoFormShowHint(HTFlag, HintInfo^.HintStr, CanShow);
        if not CanShow then
          HintInfo^.HintStr := '';
      end;
    end;
  end;
end;

procedure TCnDockTree.SetSplitterCursor(CursorIndex: TDockOrientation);
const
  SizeCursors: array[TDockOrientation] of TCursor = (crDefault, crVSplit, crHSplit);
begin
  Windows.SetCursor(Screen.Cursors[SizeCursors[CursorIndex]]);
end;

procedure TCnDockTree.SetCnDockZoneClass(const Value: TCnDockZoneClass);
begin
  FCnDockZoneClass := Value;
end;

procedure TCnDockTree.DoMButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { ���ҵ��ؼ��ϵ�TCnDockClient�����ҵ���TCnDockClient��DoNCButtonDown���� }
      DockClient.DoNCButtonDown(Msg, mbMiddle, msConjoin);
  end;
end;

procedure TCnDockTree.DoMButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { ���ҵ��ؼ��ϵ�TCnDockClient�����ҵ���TCnDockClient��DoNCButtonUp���� }
      DockClient.DoNCButtonUp(Msg, mbMiddle, msConjoin);
  end;
end;

procedure TCnDockTree.DoRButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { ���ҵ��ؼ��ϵ�TCnDockClient�����ҵ���TCnDockClient��DoNCButtonDown���� }
      DockClient.DoNCButtonDown(Msg, mbRight, msConjoin);
  end;
end;

procedure TCnDockTree.DoRButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { ���ҵ��ؼ��ϵ�TCnDockClient�����ҵ���TCnDockClient��DoNCButtonUp���� }
      DockClient.DoNCButtonUp(Msg, mbRight, msConjoin);
  end;
end;

function TCnDockTree.DoMouseEvent(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): TWMNCHitMessage;
var APoint: TPoint;
begin
  Result.Result := 0;
  APoint := SmallPointToPoint(Message.Pos);
  { ���ȼ�⵽����λ�������ĸ��ؼ��ı����� }
  Zone := InternalHitTest(APoint, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag <> HTSPLITTER) then
  begin
    { ����һ��TWMNCHitMessage�ṹ������'Message.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST'��
      ������ΪWM_MOUSExxx��WM_NCMOUSExxx��һһ��Ӧ�ģ����������WM_NCMOUSEFIRST - WM_MOUSEFIRST�� }
    Result := Cn_CreateNCMessage(DockSite, Message.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST, HTFlag, APoint);
    Result.Result := 1;
  end;
end;

procedure TCnDockTree.DoMButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  { �����ж�����λ���������� }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag = HTCAPTION) then
  begin
    if (HTFlag <> HTSPLITTER) then
    begin
      DockClient := FindDockClient(Zone.ChildControl);
      if DockClient <> nil then
        { ���ȵ���TCnDockClient�е�DoNCButtonDown���� }
        DockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONUP, HTFlag, P), mbMiddle, msConjoin);
    end;
  end;
end;

procedure TCnDockTree.DoRButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  { �����ж�����λ���������� }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag = HTCAPTION) then
  begin
    if (HTFlag <> HTSPLITTER) then
    begin
      DockClient := FindDockClient(Zone.ChildControl);
      if DockClient <> nil then
        { ���ȵ���TCnDockClient�е�DoNCButtonDown���� }
        DockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONUP, HTFlag, P), mbRight, msConjoin);
    end;
  end;
end;

function TCnDockTree.GetFrameRect(Control: TControl): TRect;
var ALeft, ATop: Integer;
begin
  if Control <> nil then
  begin
    Result := Control.BoundsRect;
    ALeft := Result.Left;
    Atop := Result.Top;
    AdjustDockRect(Control, Result);
    Dec(Result.Left, 2 * (Result.Left - Control.Left) + 1);
    Dec(Result.Top, 2 * (Result.Top - Control.Top));
    Dec(Result.Right, 2 * (Result.Right - ALeft - Control.Width));
    Dec(Result.Bottom, 2 * (Result.Bottom - ATop - Control.Height));
  end else raise Exception.Create(gs_ControlCannotIsNil);
end;

function TCnDockTree.GetSpiltterRect(Zone: TCnDockZone): TRect;
var A, B, C, D: Integer;
begin
  if (Zone <> nil) and Zone.Visibled and (Zone.ParentZone <> nil)
    and (Zone.VisibleNextSiblingCount >= 1)
    and (Zone.ParentZone.Orientation <> doNoOrient) then
  begin
    A := Zone.ParentZone.LimitBegin;
    B := Zone.ParentZone.ZoneLimit;
    C := Zone.ZoneLimit - SplitterWidth;
    D := C + 1 * SplitterWidth;
    if Zone.ParentZone.Orientation = doHorizontal then
      Result := Rect(A, C, B, D)
    else if Zone.ParentZone.Orientation = doVertical then
      Result := Rect(C, A, D, B);
  end else Result := Rect(0, 0, 0, 0);
end;

{procedure TCnDockTree.PaintDockSplitter(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);
begin
  with Canvas do
  begin
    FillRect(ARect);
  end;
end;}

procedure TCnDockTree.BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer);
var ADockClient: TCnDockClient;
begin
  ADockClient := FindDockClient(Control);
  { ��ʼ�϶� }
  if ADockClient <> nil then
    CnGlobalDockPresident.BeginDrag(Control, ADockClient.DirectDrag, Threshold);
end;

function TCnDockTree.GetFrameRectEx(Control: TControl): TRect;
begin
  if Control <> nil then
  begin
    Result := GetFrameRect(Control);
    MapWindowPoints(DockSite.Handle, 0, Result, 2);
  end;
end;

procedure TCnDockTree.DrawDockSiteRect;
begin
  { û���� }
end;

procedure TCnDockTree.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if FUpdateCount <= 0 then
      UpdateAll;
  end;
end;

function TCnDockTree.GetBorderWidth: Integer;
begin
  Result := FBorderWidth;
end;

function TCnDockTree.GetSplitterWidth: Integer;
begin
  Result := FSplitterWidth;
end;

procedure TCnDockTree.DrawSplitter(Zone: TCnDockZone);
var R: TRect;
begin
  { ��÷ָ����ľ��δ�С }
  R := GetSpiltterRect(Zone);
  { ���ָ��� }
  DrawSplitterRect(R);
end;

function TCnDockTree.GetDockEdge(DockRect: TRect; MousePos: TPoint;
  var DropAlign: TAlign; Control: TControl): TControl;
begin
  Result := nil;
  { û������ }
end;

function TCnDockTree.GetDockSiteOrient: TDockOrientation;
begin
  Result := Cn_GetControlOrient(DockSite);
end;

procedure TCnDockTree.BeginResizeDockSite;
begin
  Inc(FResizeCount);
end;

procedure TCnDockTree.EndResizeDockSite;
begin
  Dec(FResizeCount);
  if FResizeCount < 0 then
    FResizeCount := 0;
end;

procedure TCnDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  // �����ǿɼ��ģ�ͣ�������ShiftScaleOrientҪ��ͬ��
  if (Zone <> nil) and (Zone.ParentZone <> nil) and Zone.Visibled and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    // ���ݼ��㹫ʽ�õ�Zone��ZoneLimit��
    Zone.ZoneLimit := Integer(Round(Zone.ZoneLimit * ScaleBy + FParentLimit * (1 - ScaleBy)));
    // Zone��LimitSize����С�ڹ涨����С�ߴ�MinSize��
(*    if (Zone.LimitSize < FMinSize) then
      Zone.FZoneLimit := Zone.LimitBegin + FMinSize;
    // Zone��LimitBegin��ǰһ���ɼ��Ľڵ��ZoneLimit��λ�ò���С�ڹ涨����С�ߴ�MinSize
    if (Zone.BeforeClosestVisibleZone <> nil) and (Zone.LimitBegin > DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
      (Zone.VisibleNextSiblingCount + 1) * MinSize + {Integer(Zone.NextSibling <> nil) * }SplitterWidth div 2) then
      Zone.BeforeClosestVisibleZone.ZoneLimit := DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + {Integer(Zone.NextSibling <> nil) * }SplitterWidth div 2;
*)  end;
end;

procedure TCnDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  {��ScaleChildZone�ļ��㹫ʽ��һ����}
  ScaleChildZone(Zone);
{  if Zone = nil then Exit;
  if (Zone <> nil) and (Zone.ParentZone <> nil) and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
      Zone.ZoneLimit := Integer(Round(Zone.ZoneLimit * ScaleBy + FParentLimit * (1 - ScaleBy)));
}
end;

function TCnDockTree.GetDockSiteSize: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Width;
    doHorizontal: Result := DockSite.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteSize(const Value: Integer);
begin
  DockSite.Parent.DisableAlign;
  try
    // ���DockSite�����ұ߻����±ߣ���Ҫ��������DockSiteBegin(Top����Left),
    // ���������ڴ��ҿؼ�֮��ԭ�����еĴ���
    if DockSite.Align in [alRight, alBottom] then
      DockSiteBegin := DockSiteBegin - (Value - DockSiteSize);
    case DockSiteOrient of
      doVertical: DockSite.Width := Value;
      doHorizontal: DockSite.Height := Value;
    else
      raise Exception.Create(gs_CannotSetValueWithNoOrient);
    end;
  finally
    DockSite.Parent.EnableAlign;
  end;
end;

procedure TCnDockTree.SetMinSize(const Value: Integer);
begin
  FMinSize := Value;
end;

function TCnDockTree.GetDockSiteBegin: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Left;
    doHorizontal: Result := DockSite.Top;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteBegin(const Value: Integer);
begin
  case DockSiteOrient of
    doVertical: DockSite.Left := Value;
    doHorizontal: DockSite.Top := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

function TCnDockTree.GetDockSiteSizeA: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Height;
    doHorizontal: Result := DockSite.Width;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteSizeA(const Value: Integer);
begin
  case DockSiteOrient of
    doVertical: DockSite.Height := Value;
    doHorizontal: DockSite.Width := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.CalcSplitterPos;
var
  MinWidth,
  TestLimit: Integer;
begin
  MinWidth := MinSize;
  if (FSizingZone.ParentZone.Orientation = doHorizontal) then
  begin
    TestLimit := GetSplitterLimit(FSizingZone, True, False) + MinWidth;
    if FSizePos.y <= TestLimit then FSizePos.y := TestLimit;
    TestLimit := GetSplitterLimit(FSizingZone, False, True) - MinWidth - SplitterWidth;
    if FSizePos.y >= TestLimit then FSizePos.y := TestLimit;
  end
  else begin
    TestLimit := GetSplitterLimit(FSizingZone, True, False) + MinWidth;
    if FSizePos.x <= TestLimit then FSizePos.x := TestLimit;
    TestLimit := GetSplitterLimit(FSizingZone, False, True) - MinWidth - SplitterWidth;
    if FSizePos.x >= TestLimit then FSizePos.x := TestLimit;
  end;
end;

procedure TCnDockTree.SetVersion(const Value: Integer);
begin
  FVersion := Value;
end;

procedure TCnDockTree.DoSaveZone(Stream: TStream;
  Zone: TCnDockZone; Level: Integer);
begin
  with Stream do
  begin
    { �ڵ�ĵȼ�, TopZoneΪ0 }
    Write(Level, SizeOf(Level));
    CustomSaveZone(Stream, Zone);
  end;
  // ��������Ů
  if Zone.ChildZones <> nil then
    DoSaveZone(Stream, Zone.ChildZones, Level + 1);
  // �������ֵ�
  if Zone.NextSibling <> nil then
    DoSaveZone(Stream, Zone.NextSibling, Level);
end;

procedure TCnDockTree.WriteControlName(Stream: TStream; ControlName: string);
var
  NameLen: Integer;
begin
  NameLen := Length(ControlName);
  Stream.Write(NameLen, SizeOf(NameLen));
  if NameLen > 0 then Stream.Write(Pointer(ControlName)^, NameLen * SizeOf(Char));
end;

procedure TCnDockTree.DoLoadZone(Stream: TStream);
var
  Level, LastLevel, I: Integer;
  Zone, LastZone, NextZone: TCnDockZone;
begin
  LastLevel := 0;
  LastZone := nil;
  while True do
  begin
    with Stream do
    begin
      Read(Level, SizeOf(Level));
      if Level = TreeStreamEndFlag then Break;
      Zone := FCnDockZoneClass.Create(Self);
      CustomLoadZone(Stream, Zone);
      if Zone = nil then
        Continue;
    end;
    if Level = 0 then FTopZone := Zone
    else if Level = LastLevel then
    begin
      LastZone.NextSibling := Zone;
      Zone.FPrevSibling := LastZone;
      Zone.FParentZone := LastZone.FParentZone;
    end
    else if Level > LastLevel then
    begin
      LastZone.ChildZones := Zone;
      Zone.FParentZone := LastZone;
    end
    else if Level < LastLevel then
    begin
      NextZone := LastZone;
      for I := 1 to LastLevel - Level do NextZone := NextZone.FParentZone;
      NextZone.NextSibling := Zone;
      Zone.FPrevSibling := NextZone;
      Zone.FParentZone := NextZone.FParentZone;
    end;
    LastLevel := Level;
    LastZone := Zone;
  end;
end;

procedure TCnDockTree.ReadControlName(Stream: TStream;
  var ControlName: string);
var
  Size: Integer;
begin
  ControlName := '';
  Size := 0;
  Stream.Read(Size, SizeOf(Size));
  if Size > 0 then
  begin
    SetLength(ControlName, Size);
    Stream.Read(Pointer(ControlName)^, Size * SizeOf(Char));
  end;
end;

procedure TCnDockTree.CustomLoadZone(Stream: TStream; var Zone: TCnDockZone);
var CompName: string;
begin
  with Stream do
  begin
    Read(Zone.FOrientation, SizeOf(Zone.Orientation));
    Read(Zone.FZoneLimit, SizeOf(Zone.FZoneLimit));
    Read(Zone.FVisibled, SizeOf(Zone.Visibled));
    Read(Zone.FControlVisibled, SizeOf(Zone.FControlVisibled));
    Read(Zone.FVisibleSize, SizeOf(Zone.VisibleSize));
    Read(Zone.FIsInside, SizeOf(Zone.FIsInside));
    ReadControlName(Stream, CompName);
    if CompName <> '' then
    begin
      if not Zone.SetControlName(CompName) then
      begin
        { ���û���ҵ�����ؼ��Ͱѽڵ�ɾ�� }
        Zone.Free;
        Zone := nil;
        Exit;
      end;
    end;
  end;
end;

procedure TCnDockTree.CustomSaveZone(Stream: TStream; Zone: TCnDockZone);
var AVisible: Boolean;
begin
  with Stream do
  begin
    { �ڵ�ķ��� }
    Write(Zone.Orientation, SizeOf(Zone.Orientation));
    { �ڵ�ľ������� }
    Write(Zone.ZoneLimit, SizeOf(Zone.ZoneLimit));
    { �ڵ��Ƿ�ɼ� }
    if Zone.ChildControl <> nil then
      AVisible := Zone.ChildControl.Visible;
    Write(Zone.Visibled, SizeOf(Zone.Visibled));
    { �ؼ��Ƿ�ɼ� }
    AVisible := False;
    if Zone.ChildControl <> nil then
      AVisible := Zone.ChildControl.Visible;
    Write(AVisible, SizeOf(AVisible));
    { �ڵ��ڿɼ���ʱ���LimitSize }
    Write(Zone.VisibleSize, SizeOf(Zone.VisibleSize));
    { �Ƿ���DockSite������ }
    Zone.IsInside := True;
    if (Zone.ChildControl <> nil) and (Zone.ChildControl.HostDockSite <> DockSite)
      and not (DockSite is TCnVSPopupPanel) then
      Zone.IsInside := False;
    Write(Zone.IsInside, SizeOf(Zone.IsInside));
    { �ڵ������е�ChildControl������ }
    WriteControlName(Stream, Zone.GetControlName);
  end;
end;

procedure TCnDockTree.SetDockSiteSizeWithOrient(Orient: TDockOrientation;
  const Value: Integer);
begin
  case Orient of
    doVertical: DockSite.Width := Value;
    doHorizontal: DockSite.Height := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.DoOtherHint(Zone: TCnDockZone;
  HTFlag: Integer; var HintStr: string);
begin
  { û���� }
end;

function TCnDockTree.GetHTFlag(MousePos: TPoint): Integer;
var Zone: TCnDockZone;
begin
  Zone := InternalHitTest(MousePos, Result);
  if Zone = nil then Result := HTNONE;
end;

procedure TCnDockTree.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(DockSite.Handle, InfluenceRect);
  InflateRect(InfluenceRect, DefExpandoRect, DefExpandoRect);
end;

function TCnDockTree.GetDockRect: TRect;
begin
  Result := FDockRect;
end;

procedure TCnDockTree.SetDockRect(const Value: TRect);
begin
  FDockRect := Value;
end;

function TCnDockTree.GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign;
var
  CRect, DRect: TRect;
begin
  Result := alRight;
  if DropCtl <> nil then
  begin
    CRect := Client.BoundsRect;
    DRect := DropCtl.BoundsRect;
    if (CRect.Top <= DRect.Top) and (CRect.Bottom < DRect.Bottom) and
       (CRect.Right >= DRect.Right) then
      Result := alTop
    else if (CRect.Left <= DRect.Left) and (CRect.Right < DRect.Right) and
       (CRect.Bottom >= DRect.Bottom) then
      Result := alLeft
    else if CRect.Top >= ((DRect.Top + DRect.Bottom) div 2) then
      Result := alBottom;
  end;
end;

procedure TCnDockTree.HideControl(Control: TControl);
var
  Z: TCnDockZone;
begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  Z := FindControlZone(Control);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.ChildControl := nil
    else
    begin
      if TopZone.VisibleChildTotal = 1 then
        Z.Remove(TopXYLimit, True)
      else Z.Remove(Z.LimitSize, True);
//      UpdateAll;
    end;
    Control.DockOrientation := doNoOrient;
    SetNewBounds(nil);
    UpdateAll;
    { �ػ�DockSite }
    FDockSite.Invalidate;
  end;
end;

procedure TCnDockTree.ShowControl(Control: TControl);
var Z: TCnDockZone;
begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  Z := FindControlZone(Control, True);
  if Z <> nil then
    Z.Insert(Z.VisibleSize, False);

  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.DoGetNextLimit(Zone, AZone: TCnDockZone; var LimitResult: Integer);
begin
  if (Zone <> AZone) and
    (Zone.ParentZone.Orientation = AZone.ParentZone.Orientation) and
    (Zone.ZoneLimit > AZone.FZoneLimit) and ((Zone.ChildControl = nil) or
    ((Zone.ChildControl <> nil) and (Zone.ChildControl.Visible))) then
    LimitResult := Min(LimitResult, Zone.ZoneLimit);
  if Zone.NextSibling <> nil then DoGetNextLimit(Zone.NextSibling, AZone, LimitResult);
  // ����������ע�͵�����䣬��ʾ���Ժ��Ե�����ͬһ�����ڵ�Ľڵ㣬
  // ʹ�����ڷָ����������ɡ�
  if (Zone.ChildZones <> nil){ and (Zone = AZone.AfterClosestVisibleZone) }then
    DoGetNextLimit(Zone.ChildZones, AZone, LimitResult);
end;

procedure TCnDockTree.UpdateChild(Zone: TCnDockZone);
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(Zone, UpdateZone, tskForward);
end;

function TCnDockTree.GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer;
var Zone: TCnDockZone;
begin
  Result := 0;
  if TopZone.ChildCount = 1 then
    Result := Integer(not IsMin) * DockSiteSizeWithOrient[Orient]
  else
  begin
    if IsMin then
    begin
      if TopZone.Orientation = Orient then
        Zone := TopZone.LastVisibleChildZone
      else Zone := TopZone;
      if Zone <> nil then
        Result := Zone.LimitBegin;
    end else
    begin
      if TopZone.Orientation = Orient then
        Zone := TopZone.FirstVisibleChildZone
      else Zone := TopZone;
      if Zone <> nil then
        Result := Zone.ZoneLimit;
    end;

    TopZone.DoGetSplitterLimit(Orient, IsMin, Result);
  end;

{  if TopZone <> nil then
  begin
    if TopZone.ChildCount = 1 then
      Result := Integer(not IsMin) * DockSiteSizeWithOrient[Orient]
    else
      TopZone.DoGetSplitterLimit(Orient, IsMin, Result);
  end;}
end;

function TCnDockTree.GetDockSiteSizeWithOrient(
  Orient: TDockOrientation): Integer;
begin
  case Orient of
    doVertical: Result := DockSite.Width;
    doHorizontal: Result := DockSite.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

function TCnDockTree.GetMinSize: Integer;
begin
  Result := FMinSize;
end;

procedure TCnDockTree.GetCaptionRect(var Rect: TRect);
begin
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := 0;
  Rect.Bottom := 0;
end;

procedure TCnDockTree.HideAllControl;

  procedure DoHideAllControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoHideAllControl(AZone.NextSibling);
      DoHideAllControl(AZone.ChildZones);
      if (AZone.ChildControl <> nil) and (AZone.visibled) then
        AZone.Remove(AZone.LimitSize, True);
    end;
  end;

begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  DoHideAllControl(TopZone.ChildZones);
  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.HideSingleControl(Control: TControl);

  procedure DoHideSingleControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoHideSingleControl(AZone.NextSibling);
      DoHideSingleControl(AZone.ChildZones);
      if AZone.ChildControl <> nil then
      begin
        if (AZone.ChildControl = Control) then
        begin
          if (AZone.ChildControl.Visible) then
          begin
            AZone.Remove(AZone.LimitSize, True);
            AZone.ChildControl.Visible := False;
          end;
        end else
        begin
          AZone.Insert(AZone.VisibleSize, False);
          AZone.ChildControl.Visible := True;
        end;
      end;
    end;
  end;

begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  if Control <> nil then
  begin
    DoHideSingleControl(TopZone.ChildZones);
    SetNewBounds(nil);
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

procedure TCnDockTree.ShowAllControl;

  procedure DoShowAllControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoShowAllControl(AZone.NextSibling);
      DoShowAllControl(AZone.ChildZones);
      if (AZone.ChildControl <> nil) and (not AZone.visibled) then
        AZone.Insert(AZone.VisibleSize, True);
    end;
  end;

begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  DoShowAllControl(TopZone.ChildZones);
  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.ShowSingleControl(Control: TControl);

  procedure DoShowSingleControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoShowSingleControl(AZone.NextSibling);
      DoShowSingleControl(AZone.ChildZones);
      if AZone.ChildControl <> nil then
      begin
        if (AZone.ChildControl = Control) then
        begin
          if (not AZone.ChildControl.Visible) then
          begin
            AZone.Insert(AZone.VisibleSize, False);
            AZone.ChildControl.Visible := True;
          end;
        end else
        begin
          AZone.Remove(AZone.LimitSize, True);
          AZone.ChildControl.Visible := False;
        end;
      end;
    end;
  end;

begin
  { ���ReplacementZone <> nil����˵�����ڽ���Load����������ִ������Ĳ�����ֱ���˳��Ϳ����� }
  if ReplacementZone <> nil then Exit;
  if Control <> nil then
  begin
    DoShowSingleControl(TopZone.ChildZones);
    SetNewBounds(nil);
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

procedure TCnDockTree.DrawDockBorder(DockControl: TControl; R1, R2: TRect);
begin

end;

procedure TCnDockTree.DrawDockGrabber(Control: TControl;
  const ARect: TRect);
  procedure DrawCloseButton(Left, Top: Integer);
  var ADockClient: TCnDockClient;
  begin
    ADockClient := FindDockClient(Control);
    if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+GrabberSize-2,
      Top+GrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
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

begin
  with ARect do
  begin
    case GrabbersPosition of
      gpLeft:
      begin
        DrawCloseButton(Left+BorderWidth+BorderWidth+1, Top+BorderWidth+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+3, Top+GrabberSize+BorderWidth+1, Left+BorderWidth+5, Bottom+BorderWidth-2);
        DrawGrabberLine(Left+BorderWidth+6, Top+GrabberSize+BorderWidth+1, Left+BorderWidth+8, Bottom+BorderWidth-2);
      end;
      gpTop:
      begin
        DrawCloseButton(Right-GrabberSize+BorderWidth+1, Top+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+3, Right-GrabberSize+BorderWidth-2, Top+BorderWidth+5);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+6, Right-GrabberSize+BorderWidth-2, Top+BorderWidth+8);
      end;
//      gpBottom:
//      gpRight:
    end;
  end;
end;

procedure TCnDockTree.DrawSplitterRect(const ARect: TRect);
begin
  Canvas.Brush.Color := TCnWinControlAccess(DockSite).Color;
  Canvas.FillRect(ARect);
end;

procedure TCnDockTree.DrawZone(Zone: TCnDockZone);
begin
  { ���ε���DrawZoneBorder,DrawGrabber��DrawSplitter������
    �������������������ı��ػ������� }
  DrawZoneBorder(Zone);
  DrawZoneGrabber(Zone);
  DrawZoneSplitter(Zone);
  DrawDockSiteRect;
end;

procedure TCnDockTree.DrawZoneBorder(Zone: TCnDockZone);
var ChildControl: TControl;
//  R1, R2: TRect;
begin
  if Zone = nil then Exit;
  ChildControl := Zone.ChildControl;
  if (ChildControl <> nil) and ChildControl.Visible and
    (ChildControl.HostDockSite = DockSite) then
  begin
//    R := GetFrameRect(ChildControl);
//    DrawDockGrabber(ChildControl, R);
  end;
end;

procedure TCnDockTree.DrawZoneGrabber(Zone: TCnDockZone);
var ChildControl: TControl;
  R: TRect;
begin
  if Zone = nil then Exit;
  ChildControl := Zone.ChildControl;
  if (ChildControl <> nil) and ChildControl.Visible and
    (ChildControl.HostDockSite = DockSite) then
  begin
    R := GetFrameRect(ChildControl);
    DrawDockGrabber(ChildControl, R);
  end;
end;

procedure TCnDockTree.DrawZoneSplitter(Zone: TCnDockZone);
var R: TRect;
begin
  { ��÷ָ����ľ��δ�С }
  R := GetSpiltterRect(Zone);
  { ���ָ��� }
  DrawSplitterRect(R);
end;

procedure TCnDockTree.PaintDockSite;
begin
  ForEachAt(nil, DrawZone, tskBackward);
end;

function TCnDockTree.HasZoneWithControl(Control: TControl): Boolean;
begin
  Result := FindControlZone(Control, True) <> nil;
end;

procedure TCnDockTree.ReplaceZoneChild(OldControl, NewControl: TControl);
var Zone: TCnDockZone;
begin
  Zone := FindControlZone(OldControl, True);
  if Zone <> nil then
  begin
    Zone.ChildControl := TWinControl(NewControl);
    UpdateAll;
  end;
end;

procedure TCnDockTree.DoHideZoneChild(AZone: TCnDockZone);
var AForm: TCustomForm;
begin
  if (AZone <> nil) and (AZone.ChildControl <> nil) then
  begin
    if AZone.ChildControl.InheritsFrom(TCustomForm) then
    begin
      { ������Close������AZone�����ͷ� }
      AForm := TCustomForm(AZone.ChildControl);
      AForm.Close;
    end else
      AZone.ChildControl.Visible := False;
  end;
end;

{ TCnAdvDockZone }

constructor TCnAdvDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
  FCloseBtnDown := False;
  FMouseDown := False;
end;

destructor TCnAdvDockZone.Destroy;
begin
  if Self = TCnAdvDockTree(Tree).CloseBtnZone then
    TCnAdvDockTree(Tree).CloseBtnZone := nil;
  inherited Destroy;
end;

procedure TCnAdvDockZone.Insert(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, True, Hide);
end;

procedure TCnAdvDockZone.LButtonDbClkMothed;
begin
  if GlobalDockClient <> nil then
    GlobalDockClient.RestoreChild;
end;

procedure TCnAdvDockZone.Remove(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, False, Hide);
end;

{ TCnAdvDockTree }

constructor TCnAdvDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited Create(DockSite, CnDockZoneClass);
  GrabberSize     := 15;
  FButtonHeight   := 12;
  FButtonWidth    := 12;
  FLeftOffset     := 0;
  FRightOffset    := 0;
  FTopOffset      := 0;
  FBottomOffset   := 0;
  FButtonSplitter := 2;
end;

function TCnAdvDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var TempZone: TCnAdvDockZone;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTCLOSE) then
  begin
    TempZone := TCnAdvDockZone(Zone);
    TempZone.CloseBtnDown := True;
    TempZone.MouseDown := True;
    FCloseBtnZone := TempZone;
    DockSite.Invalidate;
  end;
end;

procedure TCnAdvDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
//var TempZone: TCnAdvDockZone;
begin
  inherited DoLButtonUp(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    FCloseBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTCLOSE) then
    begin
      // ��������е�ַƫ�TCnAdvDockZone(Zone).CloseBtnDown
      // ��Ī�������ָ��һ�����ָ�룬��Ҫע�⡣
//      TempZone := TCnAdvDockZone(Zone);
      TCnAdvDockZone(Zone).CloseBtnDown := False;
    end;
  end;
end;

procedure TCnAdvDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnAdvDockZone;
begin
  inherited DoMouseMove(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TCnAdvDockZone(Zone);
    if ((TempZone <> nil) and (TempZone.CloseBtnDown <> (HTFlag = HTCLOSE))
      and ((FCloseBtnZone = TempZone) and FCloseBtnZone.MouseDown)) then
    begin
      TempZone.CloseBtnDown := (HTFlag = HTCLOSE) and FCloseBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnAdvDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  try
    inherited;
    if NewZone.ChildControl <> nil then
      SetDockHeightWidthArr(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
        NewZone.ChildControl.LRDockWidth + BorderWidth)
    else SetDockHeightWidthArr(0, 0, 0);
  finally
    Update := TempUpdate;
  end;

  if Update then
  begin
    NewZone.Insert(FDropDockSize, False);
    SetNewBounds(NewZone.ParentZone);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
  end;
end;

procedure TCnAdvDockTree.SetBottomOffset(const Value: Integer);
begin
  FBottomOffset := Value;
end;

procedure TCnAdvDockTree.SetButtonHeight(const Value: Integer);
begin
  FButtonHeight := Value;
end;

procedure TCnAdvDockTree.SetButtonSplitter(const Value: Integer);
begin
  FButtonSplitter := Value;
end;

procedure TCnAdvDockTree.SetButtonWidth(const Value: Integer);
begin
  FButtonWidth := Value;
end;

procedure TCnAdvDockTree.SetLeftOffset(const Value: Integer);
begin
  FLeftOffset := Value;
end;

procedure TCnAdvDockTree.SetRightOffset(const Value: Integer);
begin
  FRightOffset := Value;
end;

procedure TCnAdvDockTree.SetTopOffset(const Value: Integer);
begin
  FTopOffset := Value;
end;

function TCnAdvDockTree.GetDockHeightWidth(
  Orient: TDockOrientation): Integer;
begin
  Result := FDockHeightWidth[Orient];
end;

procedure TCnAdvDockTree.SetDockHeightWidth(Orient: TDockOrientation;
  const Value: Integer);
begin
  FDockHeightWidth[Orient] := Value;
end;

function TCnAdvDockTree.GetDockRectFromArr(Orient: TDockOrientation;
  AtLast: Boolean): Integer;
begin
  Result := FDockRectArr[Orient, Atlast];
end;

procedure TCnAdvDockTree.SetDockRectToArr(Orient: TDockOrientation;
  AtLast: Boolean; const Value: Integer);
begin
  FDockRectArr[Orient, Atlast] := Value;
end;

procedure TCnAdvDockTree.SetDockRectArr(ARect: TRect);
begin
  FDockRectArr[doNoOrient, False] := 0;
  FDockRectArr[doNoOrient, True] := 0;
  FDockRectArr[doHorizontal, False] := ARect.Top;
  FDockRectArr[doHorizontal, True] := ARect.Bottom;
  FDockRectArr[doVertical, False] := ARect.Left;
  FDockRectArr[doVertical, True] := ARect.Right;
end;

procedure TCnAdvDockTree.SetDockHeightWidthArr(NoOrValue, HorValue,
  VerValue: Integer);
begin
  FDockHeightWidth[doNoOrient] := NoOrValue;
  FDockHeightWidth[doHorizontal] := HorValue;
  FDockHeightWidth[doVertical] := VerValue;
end;

procedure TCnAdvDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleChildZone(Zone);
end;

procedure TCnAdvDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleSiblingZone(Zone);
end;

procedure TCnAdvDockTree.ScaleZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleZone(Zone);
end;

procedure TCnAdvDockTree.ShiftZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ShiftZone(Zone);
end;

procedure TCnAdvDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  if NewZone.ChildControl <> nil then
    SetDockHeightWidthArr(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
      NewZone.ChildControl.LRDockWidth + BorderWidth)
  else SetDockHeightWidthArr(0, 0, 0);

  if SiblingZone = nil then
  begin
    if InsertLast then
      ReplacementZone := TopZone
    else ReplacementZone := NewZone;
  end;

  try
    inherited;
  finally
    Update := TempUpdate;
    ReplacementZone := nil;
  end;

  if Update then
  begin
    NewZone.Insert(DropDockSize, False);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
    SetNewBounds(NewZone.ParentZone);
  end;
end;

procedure TCnAdvDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
begin
  { ����Ĭ�ϵ�RemoveZone���� }
  inherited;
end;

procedure TCnAdvDockTree.SetDropDockSize(const Value: Integer);
begin
  FDropDockSize := Value;
end;

end.
