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
{       ͣ������Ϳͻ��ؼ�                              }
{       CnDockFormControl ��Ԫ                          }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockFormControl;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�ͣ������Ϳͻ��ؼ���Ԫ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.00
    ����: ���沨
    E-mail: zhouyibo2000@sina.com
    QQ: 51571811
    ��ʼʱ�� -- 2002-01-14
    ����ʱ�� -- 2002-01-20
    ��Ҫ���ܣ�
    ʹ�������ͣ�����ܣ����ҹ���ͣ�����塣
    ���ʹ�ã�
    �������������Ϸ�һ��TCnDockServer����һ��Ĺ��ߴ����Ϸ�һ��TCnDockClient
    �ؼ������������ߴ���Ϳ���ͣ�������������ˣ�����ͣ�������������������
    �ĸ������ϡ�ͬʱ���������ߴ��廹���໥ͣ���Ĺ��ܣ����ǿ���ͣ����ƽ���ͺ�
    ��ҳ�͵Ĵ��塣

    CnDockFormControl��Ԫ���ṩ��һЩ�����͹���������ͣ�����壬���ǵĶ���������
    ע�͡�

    ������Ĵ����У�������������������ã��������û��Ϳ������Զ�������滻��ԭ��
    ���࣬�ı����ǵ�Ĭ�����ԡ�

    BUG--
    1.�ڽ���ͣ������ʱ��ͣ������ĵ�һ��Windows��׼�ؼ�����������п��ܱ���գ�

    2.ͣ�������Caption�п��ܱ���ա�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.01
    ����ʱ�� -- 2002-02-09
    �����˱���ͣ����Ϣ�ͻ�ԭͣ����Ϣ�Ĺ��ܣ�
    �û��������԰�ͣ����Ϣ�洢��INI�ļ��У������Ա�����ע����С�
    ��Ϊ�ڻ�ԭͣ����Ϣ��ʱ���������ס�ˣ����Բ��������˸����

    �����˶�ͣ��ѡ��Ĵ�����
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.02
    ����ʱ�� -- 2002-02-20
    �����˸����ؼ�TCnBasicDockStyle�������ı�ͣ���ķ���磺
    ͣ����Ԥ��Ч����ͣ������ֵ���۵ȡ�
    �����û����Լ̳�TCnBasicDockStyle�������Զ�����ۣ��ṩ���������
    �û����Էֱ�̳�TCnDockSplitter��TCnDockPanel��TCnConjoinPanel��
    TCnTabPageControl��TCnDockZone��TCnDockTree�����ı�ͣ�����
    Ŀǰ���ӵ�ͣ���������CnDelphiDockStyle��CnVCDockStyle��
    ���Ƿֱ��ʾ����Delphi������VC++�ķ��
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.03
    ����ʱ�� -- 2002-03-10
    ������0.00���е�����BUG���������еĸ����ȶ����ɿ��ˡ�
    ������TCnVIDDockStyle�࣬����ģ��Visual InderDev��ͣ���ķ�񣬻�û����ȫ��ɣ�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.031
    ����ʱ�� -- 2002-03-17
    �����һ�������ѶȵĹ��ܣ����ǵ�ͣ���ؼ���TCnConjoinDockHostForm��ʱ��
    �Ͱ��������TCnConjoinPanel��ͣ����Ϣ����������Ȼ���ٰ�����ԭ��DropOnControl�С�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.04
    ����ʱ�� -- 2002-04-17
    �Գ���ĵײ��������˴���ȵ��޸ģ����ڣ����׿ؼ��Ѿ�������Delphi��VCL����
    �����ˣ�������ȫ���Ը����Լ�����Ը������ͣ���ķ���ˡ�
    ����������TCnDockPresident�࣬��������ڵ�����϶��ؼ���ʱ����ͣ���¼���
    ��������TCnCustomDockControl�࣬������TCustomControl�̳й������������涨��
    ��һЩ��ͣ����صĺ����ͷ����������TCnDockPanel, TCnConjoinPanel��
    TCnTabPageControl����TCnCustomDockControl�̳С�������ֻҪ����������������ͬ��
    ���붼����TCnCustomDockControl�У������˴������õ����ã����У�����������
    �ļ����ʱ��Ҳ�ܷ��㡣���ڴ������ȵ��޸ģ���ʱͣ������໹������ȫʹ�ã�
    ����Ӧ�����Ͽ����޸ĺõġ�����ͣ������Ժ�Ҫ���ӣ�
    TCnWPSDockStyle(����WPS��ͣ�����)��
    TCnPhotoShopDockStyle(����PhotoShop��ͣ�����),
    TCnTancentDockStyle(������Ѷ�������ͣ�����),
    TCnOfficeDockStyle(����Office XP��ͣ�����),
    TCnNETDockStyle(����.NET��ͣ�����),
    TCnVBDockStyle(����Visual Basic��ͣ�����)��
    TCnCustomDockStyle(�Զ����ͣ�����)��
    ...
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.041
    ����ʱ�� -- 2002-04-24
    ������һЩTCnDockClient�е����ԣ��磺
    DirectDrag(���������������ʱ���Ƿ����̽���ͣ������)��
    NCPopupMenu(������Ҽ������������ʱ���Ƿ���������˵�)��
    ShowHint(������ƶ����������ϵ�ʱ���Ƿ���ʾ��ʾ��Ϣ)��
    ��������һЩTCnDockClient���¼����磺
    OnFormShowHint(����ʾ��Ϣ��ʾ��ʱ�򴥷�)
    OnNCButtonDown(������ڱ������ϰ��µ�ʱ�򴥷�)
    OnNCButtonUp(������ڱ��������ͷŵ�ʱ�򴥷�)
    OnNCMouseMove(������ڱ��������ƶ���ʱ�򴥷�)
    OnNCButtonDblClk(������ڱ�������˫����ʱ�򴥷�)
    OnPaintDockGrabber(���ػ����ֵ�ʱ�򴥷�)
    OnPaintDockSplitter(���ػ��ָ�����ʱ�򴥷�)

    ��TCnVCDockStyle�������һЩ�Ľ���ʹ������ȥ����VC��ͣ����񣬲�����Щ�ط�
    ���д��Ľ�����ֻ��ʱ�����⡣

    �������׿ؼ�����ȥ����Խ��Խǿ���ˣ������������һ������ŷ�������ϣ��������������
    ��ȱ���ٷ�����
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.041
    ����ʱ�� -- 2002-05-10
    ��TCnVCDockStyle������˽�һ���ĸĽ����������������Ѿ���������VC++��ͣ������ˡ�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.042
    ����ʱ�� -- 2002-05-24
    ����һ������Visual C++����ĳ���,������ʾTCnVCDockStyle�ؼ�,���ĺܿ�Ŷ.
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.050
    ����ʱ�� -- 2002-06-13
    ���������Visual InterDev�е�ͣ����ҳ�Ľ��档������һ��������TCnVIDTabPageControl
    �ؼ���������Ϊһ��ķ�ҳ�ؼ���ʹ�á�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.051
    ����ʱ�� -- 2002-07-03
    �����������Visual InterDev�е�ͣ����ҳ�Ľ��档
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.060
    ����ʱ�� -- 2002-08-11
    �Ѿ����Visual InterDev�е�ͣ����ҳ�Ľ��棬��TCnBasicDockStyle��������������
    ���ԣ�TCnBasicConjoinServerOption��TCnBasicTabServerOption���û����Լ̳���
    ��������ʵ���Լ���ͣ���������ԡ�
    ����������һ�������ļ�������ȥ�Ϳ�ʼ�����ˣ�׼����ʮ�·ݷ������׿ؼ���
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.065
    ����ʱ�� -- 2002-08-20
    �ҵ���һЩBUG,�����Ѿ�����,��������,�ɿ���������ĸ�����
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.070
    ����ʱ�� -- 2002-09-14
    ������һ������Ҫ�Ĺ��ܣ����ǵ��û��ر�һ���Ѿ�ͣ���˵Ĵ���󣬿��԰��������
    ��ͣ���������е�ԭ����λ�ñ����������û��ٴ���ʾ����ʱ�򣬿��Ի�ԭ��ԭ����λ��
    �ʹ�С�����������Visual C++��Visual InterDev�ж��У�������Delphi��û�У�
    �����Ұ��������Ҳ�ӵ�CnDelphiDockStyle���ˡ�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.100
    ����ʱ�� -- 2002-11-20
    ��������ð��Ѿ������ˣ����컹�����û�������һЩBug�������Ѿ��޸��ˡ�
    ������������һ������Visual InterDev��Demo������������һ��˫����������ԭ��
    ���ܣ�����İ��ģ���������ָĲ���ȥ�ˣ�Ϊʲô��˵�������������ڸտ�ʼ��
    ����ؼ���ʱ��û���뵽�����������ڹ�����ôǿ�󣬳���ṹû����ƺã�������
    �����ٸ���ȥ�Ļ�������ͻᷢչ�����ɿ��ƵĻ��ҳ̶ȡ����ֻ����ʹ�����ʱ
    ��˫����ԭ�Ĺ���ȥ����Ȼ����������ȵ�����������VS.NET��ͣ������ͷ�����ʽ
    �棬�����⼸����æ��������VS.NET��ͣ�����TCnVSNETDockStyle�ء�����ܵ�12����
    �����꣬Ȼ��Ҫ���ԣ�����ֻ�ܵȵ�2003��1�·ݲ��ܷ����ˡ�

    ����ʽ�淢����ʱ���һ�Ҫ�Ѱ����ļ������English��������Ҳ���������й��˵�
    �ؼ��������һ���֪������Ϊ���������ǹ��������������ʱ����˵�ɡ�

    ���˳��������DockPresident��ֲ��C#���棬������֪������ʲôʱ��ʼ����Ϊ��
    ��C#Ҳ��һ֪��⣬ǰһ����ѧ��һ�£������ַ����ˣ�����ûʱ�䰡��
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.0110
    ����ʱ�� -- 2002-12-14
    �������������VS.NETͣ��������TCnVSNETDockStyle,���ܻ����Ϻ�VS.NETͣ�����
    ��࣬��������Delphi�������ԣ���Щ�ط�û�а취��VS.NETͣ�������ȫһ�������
    �������˽�VS.NET��ͣ�������ο���VS.NET��ͣ����񡷡�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.0112
    ����ʱ�� -- 2002-12-20
    ����һ������MSDN2002ͣ������Demo��Ч���������Ǻǡ�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.0120
    ����ʱ�� -- 2003-1-4
    �°汾�Ŀؼ��Ѿ����������,���ڽ�����Խ׶�,Ԥ�ƴ��ں󷢲�������ȥ��ʱ����Ҫ
    ��������,�����޸�һЩϸ�ڵĶ���,���ư���,��ȫ�ĵ��ȵȡ�
    ����Ĵ������Ѿ�ͻ��25000�У����ȥ��ע�Ͳ���Ӧ��Ҳ��20000���У�Ҳ�����еȹ�ģ
    �Ŀؼ��ˣ��Ǻǣ����㻨����һ���ҵ��ʱ�䣬��;Ҳ����������ùģ�������ҧ�����
    ����������������ؼ��Ժ�����˻���ô�����ܹ��������ʵ�Ѿ���һ��ʤ���ˣ��Ǻǡ�
    ����ȥ���������Ҫ�������󣬻���ʱ�����ƹ����������λ���Ѿ���Dock President
    ���������������������Ƽ�һ�£�������³С����������ʾ��л�ˡ�
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- �汾 0.0121
    ����ʱ�� -- 2003-2-16
    ������һ�����⣬�Ǿ�����WinXP����װ��ͣ����Ϣ��ʱ����Ļ����˸����ʵ���
    ������98����2000������û�еģ���Ϊ�����Ѿ�������LockWindowUpdate������
    ������XP���治֪����ʲôԭ�򣬻��ǻ����������˸�����������û�취��ֻ����
    װ�ص�ʱ����˵�CM_SHOWINGCHANGED��Ϣ��Ȼ����װ�ص�����Ӧ����ʾ����û��
    ��ʾ�Ŀͻ�������ʾ������
+------------------------------------------------------------------------------}

{$P+,S-,W-,R-,T-,H+,X+}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Extctrls, Comctrls, Stdctrls, Consts, Inifiles, CommCtrl, Registry, CnDockTree,
  CnDockSupportClass, CnDockSupportControl, CnClasses, CnConsts, CnCompConsts;

const
  { ͣ����״̬ }
  DS_Unknow   = 0;  // ��֪����ʲô״̬
  DS_Docking  = 1;  // ͣ��״̬
  DS_Floating = 2;  // ����״̬

type
  TCnSplitterSize = 0..32767;

  TCnDockBaseControl = class;
  TCnDockServer = class;
  TCnDockClient = class;
  TCnConjoinPanel = class;
  TCnTabPageControl = class;
  TCnConjoinDockHostForm = class;
  TCnTabDockHostForm = class;

  { ������ͣ��������е����ͻ��˵ķָ��� }
  TCnDockSplitter = class({TSplitter)//}TCustomDockPanelSplitter)
  private
    { ��ʾ���ĸ�TDockServerӵ�� }
    FDockServer: TCnDockServer;
    function GetSplitterIndex: Integer;
  protected
    function FindControl: TControl; override;
    property DockServer: TCnDockServer read FDockServer write FDockServer;
  public
    constructor Create(AOwner: TComponent); override;
    property SplitterIndex: Integer read GetSplitterIndex;
  end;

  // ������ShowDockPanel��������DockPanel�Ŀ�Ȼ��߸߶ȵ�ʱ��
  // ����ָ�������ǰ���DockPanel�����TBDockHeight��LRDockWidth
  // ���ǰ���Client��TBDockHeight��LRDockWidth�����á�
  TSetDockPanelSizeFrom = (sdfDockPanel, sdfClient);

  { ͣ�������������ͣ���ͻ��˵�Panel }
  TCnDockPanel = class(TCnCustomDockPanel)
  private
    { ��ʾ���ĸ�TDockServerӵ�� }
    FDockServer: TCnDockServer;
    function GetPanelIndex: Integer;
  protected
    procedure SetDockServer(const Value: TCnDockServer); virtual;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    { ��ʾͣ���Ĵ��� }
    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TSetDockPanelSizeFrom = sdfClient); virtual;
    procedure ResetPosition;
    property PanelIndex: Integer read GetPanelIndex;
    property DockServer: TCnDockServer read FDockServer write SetDockServer;
  end;

  TCnAdvDockPanel = class(TCnDockPanel)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnDockPanelClass = class of TCnDockPanel;
  TCnDockSplitterClass = class of TCnDockSplitter;
  TCnConjoinPanelClass = class of TCnConjoinPanel;
  TCnTabDockClass = class of TCnTabPageControl;

  { �ָ����ķ�� }
  TCnSplitterStyle = class(TPersistent)
  private
    FSplitter: TCnDockSplitter;
    FDockServer: TCnDockServer;
    FColor: TColor;
    FCursor: TCursor;
    FParentColor: Boolean;
    FResizeStyle: TResizeStyle;
    FSize: TCnSplitterSize;          // ��С������[����]ʱ���ǿ�ȣ�����[�ϣ���]ʱ���Ǹ߶ȡ�
    FMinSize: TCnSplitterSize;       // ���븸�ؼ���С��λ��
    procedure SetColor(const Value: TColor);
    procedure SetCursor(const Value: TCursor);
    procedure SetParentColor(const Value: Boolean);
    procedure SetResizeStyle(const Value: TResizeStyle);
    procedure SetSize(const Value: TCnSplitterSize);
    procedure SetMinSize(const Value: TCnSplitterSize);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToSplitter(Dest: TCnDockSplitter);
    procedure SetSplitterStyle;
    property Splitter: TCnDockSplitter read FSplitter write FSplitter;
  public
    constructor Create(ASplitter: TCnDockSplitter; ACursor: TCursor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write SetCursor;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ResizeStyle: TResizeStyle read FResizeStyle write SetResizeStyle
      default rsPattern;
    property Size: TCnSplitterSize read FSize write SetSize default 3;
    property MinSize: TCnSplitterSize read FMinSize write SetMinSize default 30;
  end;

  TCnBasicDockStyle = class;

  { TCnBasicConjoinServerOption��TCnBasicTabServerOption��ͬ�ĸ��� }
  TCnBasicServerOption = class(TPersistent)
  private
    FDockStyle: TCnBasicDockStyle;
  protected
    { �������ú�DockStyle����ϵ��TCnDockBaseControl������ }
    procedure ResetDockControlOption; virtual; abstract;
    { ��������ADockServer������ }
    procedure ResetDockServerOption(ADockServer: TCnDockServer); virtual;//  abstract;
    { ��������ADockClient������ }
    procedure ResetDockClientOption(ADockClient: TCnDockClient); virtual;//  abstract;
    property DockStyle: TCnBasicDockStyle read FDockStyle write FDockStyle;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TGrabbersSize = 1..MaxInt;
  TSplitterWidth = 1..MaxInt;

  { ƽ�̷�������ѡ���� }
  TCnBasicConjoinServerOption = class(TCnBasicServerOption)
  private
    FGrabbersSize: TGrabbersSize;
    FSplitterWidth: TSplitterWidth;
  protected
    procedure SetGrabbersSize(const Value: TGrabbersSize); virtual;
    procedure SetSplitterWidth(const Value: TSplitterWidth); virtual;
    { ------------------------------------------------------------------------ }
    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TCnDockServer); override;
    procedure ResetDockClientOption(ADockClient: TCnDockClient); override;
    { ��������TCnDockPanel������ }
    procedure ResetDockPanel(APanel: TCnDockPanel); virtual;
    { ��������TCnConjoinPanel������ }
    procedure ResetConjoinPanel(APanel: TCnConjoinPanel); virtual;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetGrabbersSize_WithoutChangeSystemInfo(const Value: TGrabbersSize);
    procedure SetSplitterWidth_WithoutChangeSystemInfo(const Value: TSplitterWidth);
  published
    property GrabbersSize: TGrabbersSize read FGrabbersSize write SetGrabbersSize;
    property SplitterWidth: TSplitterWidth read FSplitterWidth write SetSplitterWidth;
  end;

  { ��ҳ��������ѡ���� }
  TCnBasicTabServerOption = class(TCnBasicServerOption)
  private
    FTabPosition: TTabPosition;
    FHotTrack: Boolean;
  protected
    procedure SetTabPosition(const Value: TTabPosition); virtual;
    procedure SetHotTrack(const Value: Boolean); virtual;
    { ------------------------------------------------------------------------ }
    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TCnDockServer); override;
    procedure ResetDockClientOption(ADockClient: TCnDockClient); override;
    { ��������TCnTabPageControl������ }
    procedure ResetTabPageControl(APage: TCnTabPageControl); virtual;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
  end;

  TCnBasicConjoinServerOptionClass = class of TCnBasicConjoinServerOption;
  TCnBasicTabServerOptionClass = class of TCnBasicTabServerOption;

  { ����ͣ�����Ļ��� }
  TCnBasicDockStyle = class(TCnComponent)
  private
    FCnDockPanelClass: TCnDockPanelClass;      // TCnDockServer���ĸ�TCnDockPanel���������
    FCnDockSplitterClass: TCnDockSplitterClass;// TCnDockServer���ĸ�TSplitter���������
    FCnConjoinPanelClass: TCnConjoinPanelClass;// ƽ�̷��������������
    FCnTabDockClass: TCnTabDockClass;          // ��ҳ���������������
    FCnDockPanelTreeClass: TCnDockTreeClass;   // TCnDockPanel��IDockManager���������
    FCnDockPanelZoneClass: TCnDockZoneClass;   // TCnDockPanel��IDockManager�Ľڵ��������
    FCnConjoinPanelTreeClass: TCnDockTreeClass;// TCnConjoinPanel��IDockManager���������
    FCnConjoinPanelZoneClass: TCnDockZoneClass;
    FCnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass;
    FCnTabServerOptionClass: TCnBasicTabServerOptionClass;

    FCnConjoinServerOption: TCnBasicConjoinServerOption;
    FCnTabServerOption: TCnBasicTabServerOption;

    FParentForm: TForm;         // ������
    FOldWindowProc: TWndMethod;

    { һ���б������洢����������TCnBasicDockStyle�����TCnDockBaseControl���� }
    FDockBaseControlList: TList;

    function GetDockBaseControlListCount: Integer;
    function GetDockBaseControlLists(Index: Integer): TCnDockBaseControl;

  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject); virtual;
    procedure FormGetSiteInfo(Source: TCnDragDockObject; DockClient: TCnDockClient; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormPositionDockRect(DockClient: TCnDockClient; Source: TCnDragDockObject); virtual;
    procedure FormDockDrop(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(DockClient: TCnDockClient; Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(DockClient: TCnDockClient; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetDockEdge(DockClient: TCnDockClient; Source: TCnDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); virtual;
    { ------------------------------------------------------------------------ }
    { ����ƽ��ͣ���ָ��ѡ���� }
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); virtual;
    { ������ҳͣ���ָ��ѡ���� }
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); virtual;
    { ����ƽ�̺ͷ�ҳͣ���ָ��ѡ���� }
    procedure CreateServerOption; virtual;
    { �ͷ�ƽ�̺ͷ�ҳͣ���ָ��ѡ���� }
    procedure FreeServerOption; virtual;
    { ------------------------------------------------------------------------ }

    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TCnDockBaseControl); virtual;

    { ����TCnDockServer��WindowProc��Ϣ�����Ҫ��Ҫִ��Ĭ�ϵ���Ϣ����ͷ���False,����ͷ���True }
    function DockServerWindowProc(DockServer: TCnDockServer; var Message: TMessage): Boolean; virtual;
    { ����TCnDockClient��WindowProc��Ϣ�����Ҫ��Ҫִ��Ĭ�ϵ���Ϣ����ͷ���False,����ͷ���True }
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; virtual;

    { ���񸸿ؼ���WindowProc��Ϣ }
    procedure ParentFormWindowProc(var Message: TMessage); virtual;

    { ��ADockBaseControl��ӵ�FDockBaseControlList�У�
      ����Ѿ������˾Ͳ����룬��֮���뵽�б�Ľ�β�� }
    procedure AddDockBaseControl(ADockBaseControl: TCnDockBaseControl); virtual;
    { ��ADockBaseControl��FDockBaseControlList��ɾ����
      ����������˾Ͳ�ɾ������֮ɾ�� }
    procedure RemoveDockBaseControl(ADockBaseControl: TCnDockBaseControl); virtual;
    { ------------------------------------------------------------------------ }
    procedure SetConjoinServerOption(
      const Value: TCnBasicConjoinServerOption); virtual;
    procedure SetTabServerOption(const Value: TCnBasicTabServerOption); virtual;
    function GetConjoinServerOption: TCnBasicConjoinServerOption; virtual;
    function GetTabServerOption: TCnBasicTabServerOption; virtual;
    { ����ǰ��APanel��ƽ�̵ķ��ֵ,����������������������µ����� }
    procedure AssignConjoinServerOption(APanel: TCnCustomDockPanel); virtual;
    { ����ǰ��APage�ķ�ҳ�ķ��ֵ,����������������������µ����� }
    procedure AssignTabServerOption(APage: TCnTabPageControl); virtual;
    procedure Loaded; override;
    property DockBaseControlList: TList read FDockBaseControlList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function CanSetEnableDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetLeftDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetRightDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetTopDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetBottomDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function GetControlName: string; virtual;
    function GetDockStyleVersion: string; virtual;
    { �������ù�����״ }
    procedure ResetCursor(Source: TCnDragDockObject); virtual;
    { ���DockClient������ͣ��״̬ }
    function GetDockState(DockClient: TCnDockClient): Integer; virtual;
    procedure ResetDockControlConjoinOption;
    procedure ResetDockControlTabOption;
    { ------------------------------------------------------------------------ }
    procedure ShowDockForm(ADockClient: TCnDockClient); virtual;// ��ʾADockClient�е�ParentForm;
    procedure HideDockForm(ADockClient: TCnDockClient); virtual;// ����ADockClient�е�ParentForm;
    function GetDockFormVisible(ADockClient: TCnDockClient): Boolean; virtual;// �õ�ADockClient�е�ParentForm�Ƿ�ɼ�.
    { ------------------------------------------------------------------------ }
    property DockBaseControlListCount: Integer read GetDockBaseControlListCount;
    property DockBaseControlLists[Index: Integer]: TCnDockBaseControl read GetDockBaseControlLists;
    { ��ԭԭ�ȵĿͻ���״̬ }
    procedure RestoreClient(DockClient: TCnDockClient); virtual;
    { ------------------------------------------------------------------------ }
    property Version: string read GetDockStyleVersion;// �汾��
    property ControlName: string read GetControlName; // �ؼ�����
    { ------------------------------------------------------------------------ }
    property CnDockPanelClass: TCnDockPanelClass
      read FCnDockPanelClass write FCnDockPanelClass;
    property CnDockSplitterClass: TCnDockSplitterClass
      read FCnDockSplitterClass write FCnDockSplitterClass;
    property CnConjoinPanelClass: TCnConjoinPanelClass
      read FCnConjoinPanelClass write FCnConjoinPanelClass;
    property CnTabDockClass: TCnTabDockClass
      read FCnTabDockClass write FCnTabDockClass;
    property CnDockPanelTreeClass: TCnDockTreeClass
      read FCnDockPanelTreeClass write FCnDockPanelTreeClass;
    property CnDockPanelZoneClass: TCnDockZoneClass
      read FCnDockPanelZoneClass write FCnDockPanelZoneClass;
    property CnConjoinPanelTreeClass: TCnDockTreeClass
      read FCnConjoinPanelTreeClass write FCnConjoinPanelTreeClass;
    property CnConjoinPanelZoneClass: TCnDockZoneClass
      read FCnConjoinPanelZoneClass write FCnConjoinPanelZoneClass;
    { ------------------------------------------------------------------------ }
    property CnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass
      read FCnConjoinServerOptionClass write FCnConjoinServerOptionClass;
    property CnTabServerOptionClass: TCnBasicTabServerOptionClass
      read FCnTabServerOptionClass write FCnTabServerOptionClass;
    { ------------------------------------------------------------------------ }
    property ParentForm: TForm read FParentForm write FParentForm;
    { ------------------------------------------------------------------------ }
    property ConjoinServerOption: TCnBasicConjoinServerOption
      read GetConjoinServerOption write SetConjoinServerOption;
    property TabServerOption: TCnBasicTabServerOption
      read GetTabServerOption write SetTabServerOption;
  end;

  TCnAdvDockStyle = class(TCnBasicDockStyle)
  protected
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; override;
  end;

  { ͣ������˺Ϳͻ��˹�ͬ�ĸ��� }
  TCnDockBaseControl = class(TCnComponent)
  private
    FEnableDock,            // �Ƿ����ͣ���ڷ����
    FLeftDock,              // �Ƿ����ͣ���ڷ���˵����
    FTopDock,               // �Ƿ����ͣ���ڷ���˵��ϱ�
    FRightDock,             // �Ƿ����ͣ���ڷ���˵��ұ�
    FBottomDock,            // �Ƿ����ͣ���ڷ���˵��±�
    FEachOtherDock: Boolean;// �Ƿ�����໥ͣ��
    FDockStyle: TCnBasicDockStyle;

    FOldOnClose: TCloseEvent;
    FOldOnCreate: TNotifyEvent;

    FParentForm: TForm;

    FOldWindowProc: TWndMethod;

  protected
    procedure Loaded; override;
    procedure SetParentComponent(Value: TComponent); override;
    { ------------------------------------------------------------------------ }
    function CanSetEnableDocked: Boolean; virtual;
    function CanSetLeftDocked: Boolean; virtual;
    function CanSetRightDocked: Boolean; virtual;
    function CanSetTopDocked: Boolean; virtual;
    function CanSetBottomDocked: Boolean; virtual;
    function CanSetEachOtherDocked: Boolean; virtual;
    { ------------------------------------------------------------------------ }
    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;
    { ------------------------------------------------------------------------ }
    procedure SetBottomDock(const Value: Boolean); virtual;
    procedure SetEachotherDock(const Value: Boolean); virtual;
    procedure SetEnableDock(const Value: Boolean); virtual;
    procedure SetLeftDock(const Value: Boolean); virtual;
    procedure SetRightDock(const Value: Boolean); virtual;
    procedure SetTopDock(const Value: Boolean); virtual;
    procedure SetDockStyle(const Value: TCnBasicDockStyle); virtual;
    { ------------------------------------------------------------------------ }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WindowProc(var Message: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ParentForm: TForm read FParentForm;
    { ------------------------------------------------------------------------ }
//    class function GetDockStyleVersion: string; virtual;
    function GetDockStyleVersion: string; virtual;
    { ------------------------------------------------------------------------ }
    property EnableDock: Boolean read FEnableDock write SetEnableDock default True;
    property LeftDock: Boolean read FLeftDock write SetLeftDock default True;
    property TopDock: Boolean read FTopDock write SetTopDock default True;
    property RightDock: Boolean read FRightDock write SetRightDock default True;
    property BottomDock: Boolean read FBottomDock write SetBottomDock default True;
    property EachOtherDock: Boolean read FEachotherDock write SetEachotherDock default True;
    { ------------------------------------------------------------------------ }
    property Version: string read GetDockStyleVersion; // �汾��
    { ------------------------------------------------------------------------ }
    property DockStyle: TCnBasicDockStyle read FDockStyle write SetDockStyle;
  end;

  TGetClientAlignSizeEvent = procedure(Align: TAlign; var Value: Integer) of object;
  TFinishSetDockPanelSizeEvent = procedure(DockPanel: TCnDockPanel) of object;

  { ͣ������� }
  TCnDockServer = class(TCnDockBaseControl)
  private
    FDockPanelClass: TCnDockPanelClass;
    FLeftDockPanel,                         // ��ߵ�ͣ�����
    FTopDockPanel,                          // �ϱߵ�ͣ�����
    FBottomDockPanel,                       // �±ߵ�ͣ�����
    FRightDockPanel: TCnDockPanel;          // �ұߵ�ͣ�����

    FDockSplitterClass: TCnDockSplitterClass;
    FLeftSplitter,                          // ��ߵķָ���
    FTopSplitter,                           // �ϱߵķָ���
    FBottomSplitter,                        // �±ߵķָ���
    FRightSplitter: TCnDockSplitter;        // �ұߵķָ���

    FLeftSplitterStyle,                     // ��ߵķָ������
    FTopSplitterStyle,                      // �ϱߵķָ������
    FRightSplitterStyle,                    // �±ߵķָ������
    FBottomSplitterStyle: TCnSplitterStyle; // �ұߵķָ������

    FOnGetClientAlignSize: TGetClientAlignSizeEvent;
    FOnFinishSetDockPanelSize: TFinishSetDockPanelSizeEvent;

    procedure CreateDockPanelAndSplitter; // �����ĸ�ͣ�����ͷָ���
//    procedure DestroyDockPanelAndSplitter;// �ͷ��ĸ�ͣ�����ͷָ���

    procedure CreateSplitterStyle;  // ����������ķ��
    procedure DestroySplitterStyle; // �ͷŷ�����ķ��

    { ���÷ָ����ķ�� }
    procedure SetSplitterStyle;     // ���÷�����ķ��
    { ������ߵķ�����ķ�� }
    procedure SetLeftSplitterStyle(const Value: TCnSplitterStyle);
    { �����ϱߵķ�����ķ�� }
    procedure SetTopSplitterStyle(const Value: TCnSplitterStyle);
    { �����ұߵķ�����ķ�� }
    procedure SetRightSplitterStyle(const Value: TCnSplitterStyle);
    { �����±ߵķ�����ķ�� }
    procedure SetBottomSplitterStyle(const Value: TCnSplitterStyle);
    { �������ΪIndex��TCnDockPanel }
    function GetDockPanel(Index: Integer): TCnDockPanel;
    { �������ΪIndex��TCnDockSplitter }
    function GetDockSplitter(Index: Integer): TCnDockSplitter;
    procedure DoGetClientAlignControl(Align: TAlign; var Value: Integer);
    function GetDockPanelWithAlign(Index: TAlign): TCnDockPanel;
    function GetDockSplitterWithAlign(Index: TAlign): TCnDockSplitter;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    { ------------------------------------------------------------------------ }
    procedure DoFinishSetDockPanelSize(DockPanel: TCnDockPanel);
    procedure DoFloatDockClients(DockPanel: TCnDockPanel);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;
    { ------------------------------------------------------------------------ }
   procedure WMActivate(var Message: TWMActivate);
   procedure WindowProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetClientAlignControl(Align: TAlign): Integer;

    property LeftDockPanel: TCnDockPanel read FLeftDockPanel;
    property RightDockPanel: TCnDockPanel read FRightDockPanel;
    property TopDockPanel: TCnDockPanel read FTopDockPanel;
    property BottomDockPanel: TCnDockPanel read FBottomDockPanel;

    property LeftSplitter: TCnDockSplitter read FLeftSplitter;
    property RightSplitter: TCnDockSplitter read FRightSplitter;
    property TopSplitter: TCnDockSplitter read FTopSplitter;
    property BottomSplitter: TCnDockSplitter read FBottomSplitter;

    property DockPanel[Index: Integer]: TCnDockPanel read GetDockPanel;
    property DockPanelWithAlign[Index: TAlign]: TCnDockPanel read GetDockPanelWithAlign;
    property DockSplitter[Index: Integer]: TCnDockSplitter read GetDockSplitter;
    property DockSplitterWithAlign[Index: TAlign]: TCnDockSplitter read GetDockSplitterWithAlign;
    property Version: string read GetDockStyleVersion; // �汾��
  published
    property LeftSplitterStyle: TCnSplitterStyle read FLeftSplitterStyle write SetLeftSplitterStyle;
    property TopSplitterStyle: TCnSplitterStyle read FTopSplitterStyle write SetTopSplitterStyle;
    property RightSplitterStyle: TCnSplitterStyle read FRightSplitterStyle write SetRightSplitterStyle;
    property BottomSplitterStyle: TCnSplitterStyle read FBottomSplitterStyle write SetBottomSplitterStyle;

    property OnGetClientAlignSize: TGetClientAlignSizeEvent
      read FOnGetClientAlignSize write FOnGetClientAlignSize;
    property OnFinishSetDockPanelSize: TFinishSetDockPanelSizeEvent
      read FOnFinishSetDockPanelSize write FOnFinishSetDockPanelSize;

    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property DockStyle;
  end;

  { ����λ�ã��ֱ���'�ڸ���������'��'��ƽ�̷�������'��'�ڷ�ҳ��������' }
  TMouseStation = (msFloat, msConjoin, msTabPage);

  TNCButtonEvent = procedure(DockClient: TCnDockClient; Button: TMouseButton;
    X, Y: Smallint; HitTest: Longint; MouseStation: TMouseStation) of object;

  TNCButtonDownEvent = TNCButtonEvent;

  TNCButtonUpEvent = TNCButtonEvent;

  TNCButtonDblClkEvent = TNCButtonEvent;

  TNCMouseMoveEvent = procedure(DockClient: TCnDockClient;
    X, Y: Smallint; HitTest: Longint; MouseStation: TMouseStation) of object;

  TPaintDockEvent = procedure(Canvas: TCanvas;
    Control: TControl; const ARect: TRect) of object;

  TPaintDockGrabberEvent = TPaintDockEvent;

  TPaintDockSplitterEvent = TPaintDockEvent;

  TFormHintEvent = procedure(HTFlag: Integer; var HintStr: string; var CanShow: Boolean) of object;

  { ͣ���ͻ��� }
  TCnDockClient = class(TCnDockBaseControl)
  private
    FConjoinPanelClass: TCnConjoinPanelClass;
    FTabDockClass: TCnTabDockClass;
    FParentVisible: Boolean;    // �������Ƿ��ǿɼ��ġ�
    FNCPopupMenu: TPopupMenu;   // ���ڴ���ı��������һ�����Ҽ�ʱ�������Ĳ˵���
    FDirectDrag: Boolean;       // ����갴�µ�ʱ���Ƿ���������ͣ��������
    FShowHint: Boolean;         // �Ƿ���ʾ��ʾ��Ϣ

    FCanFloat: Boolean;             // �Ƿ���Ը���,���������Ӧһ���û���Ҫ����ӵ�,
                                    // Ϊ�˷�ֹ����Ľ����������,��ʱ���û�����ϣ��
                                    // ͣ���Ĵ��帡������,�����������������,�����True,
                                    // ��������,���߾Ͳ�����,ֻ��ͣ������������,
                                    // ��Ȼ�ڷ�����֮���ǿ����໥�϶���,Ĭ��ΪTrue.
    FRelativeServer: TCnDockServer; // �й�ϵ��TCnDockServer��ʵ��,�������Ҳ��Ӧһ��
                                    // �û���Ҫ����ӵ�,��һ���ͻ��������ͣ����������
                                    // �����ʱ��,��Ҫ�ж�FRelativeServer�͵�ǰ����ͣ��
                                    // �ķ������Ƿ���ͬһ��,�������,�Ͳ���ͣ��,���߾�
                                    // �ܹ�ͣ��,Ĭ��ֵΪnil,�����������nil��ʱ��,��˵��
                                    // ����ͣ�������еķ���������.
    FDockLevel: Integer;            // ͣ���ļ���,ֻ����ͬ����Ŀͻ�����ſ����໥ͣ��

    FEnableCloseBtn: Boolean;       // �Ƿ�����رհ�ť����.

    FOnNCButtonDown:      TNCButtonDownEvent;
    FOnNCButtonUp:        TNCButtonUpEvent;
    FOnNCMouseMove:       TNCMouseMoveEvent;
    FOnNCButtonDblClk:    TNCButtonDblClkEvent;

    FOnPaintDockGrabber:  TPaintDockGrabberEvent;
    FOnPaintDockSplitter: TPaintDockSplitterEvent;

    FOnFormShowHint: TFormHintEvent;

    FOnFormShow: TNotifyEvent;
    FOnFormHide: TNotifyEvent;

    FCurrentDockSite,
    FLastDockSite:  TWinControl;// ��һ��ͣ���ķ�����
    FUnDockLeft,                // ����ʱ�����ߵ�λ��
    FUnDockTop: Integer;        // ����ʱ����ұߵ�λ��

    FVSPaneWidth: Integer;      // ���Ŀ��

    procedure SetParentVisible(const Value: Boolean);
    function GetLRDockWidth: Integer;
    function GetTBDockHeight: Integer;
    procedure SetLRDockWidth(const Value: Integer);
    procedure SetTBDockHeight(const Value: Integer);
    procedure SetNCPopupMenu(const Value: TPopupMenu);
    { ------------------------------------------------------------------------ }
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCMButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCMButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCMButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCRButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCRButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCRButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCMouseMove(var Message: TWMNCHitMessage);
    procedure CMVisibleChanged(var Message: TMessage);
    procedure SetCurrentDockSite(const Value: TWinControl);
    procedure SetLastDockSite(const Value: TWinControl);
    procedure SetVSPaneWidth(const Value: Integer);
    procedure SetUnDockLeft(const Value: Integer);
    procedure SetUnDockTop(const Value: Integer);
    function GetDockState: Integer;
    procedure SetCanFloat(const Value: Boolean);
    procedure SetRelativeServer(const Value: TCnDockServer);
    procedure SetDockLevel(const Value: Integer);
    procedure SetEnableCloseBtn(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    procedure DoMenuPopup(X, Y: Integer); virtual;
    { ------------------------------------------------------------------------ }
    procedure Deactivate; virtual;
    procedure Activate; virtual;
    { ------------------------------------------------------------------------ }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { ------------------------------------------------------------------------ }
    procedure DoFloatDockClients(PanelAlign: TAlign);
    procedure DoFloatDockEachOther;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEachotherDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;
    { ------------------------------------------------------------------------ }
    procedure DoFormOnClose(Sender: TObject;
      var Action: TCloseAction); override;
    { ------------------------------------------------------------------------ }
    procedure WMSize(var Message: TWMSize);
    procedure WMActivate(var Message: TWMActivate);
//    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged)
    procedure WindowProc(var Message: TMessage); override;
    property RelativeServer: TCnDockServer read FRelativeServer write SetRelativeServer default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { ------------------------------------------------------------------------ }
    procedure FormStartDock(var Source: TCnDragDockObject); virtual;
    procedure FormPositionDockRect(Source: TCnDragDockObject); virtual;
    procedure FormDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormDockDrop(Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure FormGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;
    { ------------------------------------------------------------------------ }
    procedure Assign(Source: TPersistent); override;
    { ����OnFormShow�¼� }
    procedure MakeShowEvent;
    { ����OnFormClose�¼� }
    procedure MakeHideEvent;
    { ����DefaultConjoinPanelClass��ʵ�� }
    function CreateConjoinPanelClass(ConjoinHost: TForm): TCnConjoinPanel;
    { ����DefaultTabDockClass��ʵ�� }
    function CreateTabDockClass(TabHost: TForm): TCnTabPageControl;
    { ------------------------------------------------------------------------ }
    function CreateConjoinHostAndDockControl(Control1,
      Control2: TControl; DockType: TAlign): TCnConjoinDockHostForm; virtual;
    function CreateTabHostAndDockControl(Control1,
      Control2: TControl): TCnTabDockHostForm; virtual;
    { ------------------------------------------------------------------------ }
    procedure DoNCButtonDown(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    procedure DoNCButtonUp(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    procedure DoNCMouseMove(Message: TWMNCHitMessage;
      MouseStation: TMouseStation); virtual;
    procedure DoNCButtonDblClk(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    { ------------------------------------------------------------------------ }
    procedure DoPaintDockGrabber(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoPaintDockSplitter(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoFormShowHint(HTFlag: Integer; var HintStr: string; var CanShow: Boolean);
    { ------------------------------------------------------------------------ }
    procedure ShowParentForm;   // ��ʾParentForm
    procedure HideParentForm;   // ����ParentForm
    { ��ԭ���е�ͣ���ͻ� }
    procedure RestoreChild;
    { ------------------------------------------------------------------------ }
    property VSPaneWidth: Integer read FVSPaneWidth write SetVSPaneWidth;
    { ------------------------------------------------------------------------ }
    { ��ʾ���ؼ��Ŀɼ��� }
    property ParentVisible: Boolean read FParentVisible write SetParentVisible;
    { ------------------------------------------------------------------------ }
    { ��ǰ��ͣ�������� }
    property CurrentDockSite: TWinControl read FCurrentDockSite write SetCurrentDockSite;
    { ��ǰ��ͣ�������� }
    property LastDockSite: TWinControl read FLastDockSite write SetLastDockSite;
    property UnDockLeft: Integer read FUnDockLeft write SetUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write SetUnDockTop;

    { ͣ���ͻ���״̬�����ڸ���״̬����ͣ��״̬�������������״̬ }
    property DockState: Integer read GetDockState;

  published
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnFormHide: TNotifyEvent read FOnFormHide write FOnFormHide;
    property OnNCButtonDown: TNCButtonDownEvent read FOnNCButtonDown write FOnNCButtonDown;
    property OnNCButtonUp: TNCButtonUpEvent read FOnNCButtonUp write FOnNCButtonUp;
    property OnNCMouseMove: TNCMouseMoveEvent read FOnNCMouseMove write FOnNCMouseMove;
    property OnNCButtonDblClk: TNCButtonDblClkEvent read FOnNCButtonDblClk write FOnNCButtonDblClk;
    property OnPaintDockGrabber: TPaintDockGrabberEvent read FOnPaintDockGrabber write FOnPaintDockGrabber;
    property OnPaintDockSplitter: TPaintDockSplitterEvent read FOnPaintDockSplitter write FOnPaintDockSplitter;
    property OnFormShowHint: TFormHintEvent read FOnFormShowHint write FOnFormShowHint;
    { ------------------------------------------------------------------------ }
    property LRDockWidth: Integer read GetLRDockWidth write SetLRDockWidth;
    property TBDockHeight: Integer read GetTBDockHeight write SetTBDockHeight;
    property NCPopupMenu: TPopupMenu read FNCPopupMenu write SetNCPopupMenu;
    property DirectDrag: Boolean read FDirectDrag write FDirectDrag;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property CanFloat: Boolean read FCanFloat write SetCanFloat default True;
    property DockLevel: Integer read FDockLevel write SetDockLevel default 0;
    property EnableCloseBtn: Boolean read FEnableCloseBtn write SetEnableCloseBtn;
    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property EachOtherDock;
    property DockStyle;
  end;

  TCnConjoinPanel = class(TCnCustomDockPanel)
  private
    FDockClient: TCnDockClient;
    function GetParentForm: TCnConjoinDockHostForm;
//    procedure SetUnDockControl(const Value: TControl);
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property ParentForm: TCnConjoinDockHostForm read GetParentForm;
  end;

  TCnAdvConjoinPanel = class(TCnConjoinPanel)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnTabPageControl = class(TCnDockPageControl)
  private
    FDockClient: TCnDockClient;
    FVersion: Integer;
    function GetParentForm: TCnTabDockHostForm;
  protected
    { ------------------------------------------------------------------------ }
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property ParentForm: TCnTabDockHostForm read GetParentForm;
    property TabPosition;
  end;

  TCnAdvTabPageControl = class(TCnTabPageControl)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  // TCnDockableForm��TCnConjoinDockHostForm��TCnTabDockHostForm��ͬ�ĸ���
  // ����һЩ�������๲ͬ���¼���
  // ������Ҳ��һ��TCnDockClient�ؼ���Ҳ����˵��Ҳ��һ����ͣ���ؼ���
  // ��һ��ķ���TCnDockClient�ؼ��Ĵ�����һ�������ԡ�
  TCnDockableForm = class(TForm)
  private
    FDockClient: TCnDockClient;
    FDockableControl: TWinControl;
    FUnDockControl: TControl;
    FFloatingChild: TControl;
    function GetDockableControl: TWinControl;
    procedure SetDockableControl(const Value: TWinControl);
    procedure SetUnDockControl(const Value: TControl);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property DockableControl: TWinControl read GetDockableControl write SetDockableControl;
    property UnDockControl: TControl read FUnDockControl write SetUnDockControl;
    property FloatingChild: TControl read FFloatingChild;
  published
  end;

  { ƽ�̵ķ����� }
  TCnConjoinDockHostForm = class(TCnDockableForm)
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    Panel: TCnConjoinPanel;
    constructor Create(AOwner: TComponent); override;
    property DockClient;
  published
  end;

  { ��ҳ�ķ����� }
  TCnTabDockHostForm = class(TCnDockableForm)
  public
    PageControl: TCnTabPageControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetActiveDockForm: TForm;
    property DockClient;

  published
  end;

var
  { Ĭ�ϵ�TDockServerͣ��Panel��TCnDockPanel }
  DefaultDockPanelClass: TCnDockPanelClass = TCnDockPanel;

  { Ĭ�ϵ�ͣ���ָ�����TCnDockSplitter }
  DefaultDockSplitterClass: TCnDockSplitterClass = TCnDockSplitter;

  { Ĭ�ϵ�ƽ��ͣ������TCnConjoinPanel }
  DefaultConjoinPanelClass: TCnConjoinPanelClass = TCnConjoinPanel;

  { Ĭ�ϵķ�ҳͣ������TCnTabPageControl }
  DefaultTabDockClass: TCnTabDockClass = TCnTabPageControl;

  { Ĭ�ϵ�ͣ���������ڵ���TCnDockZone }
  DefaultDockZoneClass: TCnDockZoneClass = TCnDockZone;

  { Ĭ�ϵ�ͣ����������TCnDockTree }
  DefaultDockTreeClass: TCnDockTreeClass = TCnDockTree;

//  FCnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass;
//  FCnTabServerOptionClass: TCnBasicTabServerOptionClass;

{ ��ʾ���� }
procedure ShowDockForm(DockWindow: TWinControl);
{ ���ش��� }
procedure HideDockForm(DockWindow: TControl);
{ ʹ��Host�ϵ�TDockClient�ؼ�����OnShowEvent��OnHideEvent���ݹ���� }
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
{ ��ô���Ŀɼ��� }
function GetFormVisible(DockWindow: TWinControl): Boolean;
{ ���÷�ҳ�ķ������е�PageControl��PopMenu }
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
{ ���÷�ҳ�ķ������е�PageControl��HotTrack }
procedure SetDockPageControlHotTrack(Value: Boolean);
{ ���÷�ҳ�ķ��������ʾ��� }
procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
{ ����ƽ�̵ķ��������ʾ��� }
procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
{ ��ͣ���������Ϣ������ļ� }
procedure SaveDockTreeToFile(FileName: string);
{ ���ļ������ͣ���������Ϣ }
procedure LoadDockTreeFromFile(FileName: string);
{ ��ͣ���������Ϣ�����ע��� }
procedure SaveDockTreeToReg(RootKey: DWORD; RegPath: string);
{ ��ע��������ͣ���������Ϣ }
procedure LoadDockTreeFromReg(RootKey: DWORD; RegPath: string);
{ �ڴ����ϲ���TDockBaseControl�ؼ� }
function FindDockBaseControl(Client: TControl): TCnDockBaseControl;
{ �ڴ����ϲ���TDockClient�ؼ� }
function FindDockClient(Client: TControl): TCnDockClient;
{ �ڴ����ϲ���TDockServer�ؼ� }
function FindDockServer(Client: TControl): TCnDockServer;
{ ���ͣ���������Ϳͻ��˻���ͣ���ͻ���֮���Ƿ���ͣ����ϵ }
function IsDockable(Sender: TWinControl; Client: TControl;
  DropCtl: TControl = nil; DockAlign: TAlign = alNone): Boolean;
{ ����ͣ�������� }
function ComputeDockingRect(AControl: TControl;
  var DockRect: TRect; MousePos: TPoint): TAlign;
{ ʹͣ������AControl��Sender���ϳ��� }
procedure DoFloat(Sender, AControl: TControl);
{ ���ÿؼ���DockSite���� }
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
{ ʹDockForm�ؼ������������������DockClients }
procedure DoFloatForm(DockForm: TControl);
{ ɾ���������õ�TCnDockableForm���� }
procedure FreeAllDockableForm;
{ ʹӦ�ó�������еĴ��嶼���� }
procedure DoFloatAllForm;
{ �õ�AControl�ͻ�����Align�������е��ӿؼ��ۼӵĴ�С��AControl�߿�ľ��� }
function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl = nil): Integer;
procedure ResetDockClient(Control: TControl; NewTarget: TControl); overload;
procedure ResetDockClient(DockClient: TCnDockClient; NewTarget: TControl); overload;
procedure ReshowAllVisibleWindow;
{ �ͷ�����û�пͻ��ķ����� }
//procedure FreeAllHostOfNotClient;



implementation

uses Dialogs, Math, CnDockSupportProc, CnDockGlobal, CnDockInfo, CnVSNETDockStyle;//, CnVSNETDockStyle;

var { ͣ����PageConrol�ĵ����˵���Ĭ��Ϊû�� }
    DockPageControlPopupMenu: TPopupMenu = nil;
    { ͣ����PageConrol�Ƿ������ʾ��Ĭ��ΪFalse }
    DockPageControlHotTrack: Boolean = False;
    { ��ҳʽ���ڵķ��Ĭ��ΪbsSizeToolWin }
    TabDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
    { ƽ��ʽ����ķ��Ĭ��ΪbsSizeToolWin }
    ConjoinDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
    { ����ϵͳ�Ƿ���WinXP }
    IsWinXP: Boolean;

type
  TCnControlAccess = class(TControl);
  TCnWinControlAccess = class(TWinControl);

{$R CnDockableForm.DFM}
{$R CnConjoinDockHost.DFM}
{$R CnTabDockHost.DFM}

{ �ڴ����ϲ���TDockBaseControl�ؼ� }
function FindDockBaseControl(Client: TControl): TCnDockBaseControl;
var i: Integer;
begin
  Result := nil;
  if Client <> nil then
  begin
    for i := 0 to Client.ComponentCount - 1 do
      if (Client.Components[i] is TCnDockBaseControl) then
      begin
        Result := TCnDockBaseControl(Client.Components[i]);
        Exit;
      end;
  end;
end;

{ �ڴ����ϲ���TDockServer�ؼ� }
function FindDockServer(Client: TControl): TCnDockServer;
var ADockControl: TCnDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TCnDockServer then
    Result := TCnDockServer(ADockControl)
  else Result := nil;
end;

{ �ڴ����ϲ���TDockClient�ؼ� }
function FindDockClient(Client: TControl): TCnDockClient;
var ADockControl: TCnDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TCnDockClient then
    Result := TCnDockClient(ADockControl)
  else Result := nil;
end;

{ ���ͣ���������Ϳͻ��˻���ͣ���ͻ���֮���Ƿ���ͣ����ϵ }
function IsDockable(Sender: TWinControl; Client: TControl; DropCtl: TControl = nil;
  DockAlign: TAlign = alNone): Boolean;
var ADockClient: TCnDockClient;
  i: Integer;
  SenderDockStyle,
  ClientDockStyle: TCnBasicDockStyle;
  SenderStyleName,
  ClientStyleName: string;
//  Level_1: Integer;
Label JudgeRelation;

begin
  ADockClient := FindDockClient(Client);
  Result := False;
  if (ADockClient <> nil) and (ADockClient.EnableDock) then
  begin
    if Sender is TCnDockPanel then
    begin
      with TCnDockPanel(Sender) do
      begin
        Result := DockServer.EnableDock and
//          ((DockAlign <> alClient) or (ADockClient.EachOtherDock and (DockAlign = alClient))) and
          (((Align = alLeft) and DockServer.LeftDock and (ADockClient.LeftDock)) or
          ((Align = alTop) and DockServer.TopDock and (ADockClient.TopDock)) or
          ((Align = alRight) and DockServer.RightDock and (ADockClient.RightDock)) or
          ((Align = alBottom) and DockServer.BottomDock and (ADockClient.BottomDock)));
        SenderDockStyle := DockServer.DockStyle;
      end;
    end else
    begin
      if (Sender <> nil) and (Sender.Parent is TCnDockableForm) then
      begin
        with TCnDockableForm(Sender.Parent).DockableControl do
          for i := 0 to DockClientCount - 1 do
            if DockClients[i] = Client then
              Exit;
      end;
      Result := ADockClient.EachOtherDock;
      if Sender <> nil then
        ADockClient := FindDockClient(Sender.Parent);
      if ADockClient <> nil then
        Result := Result and ADockClient.EachOtherDock;
//      else Result := False;
      if ADockClient <> nil then
        SenderDockStyle := ADockClient.DockStyle
      else Exit;
    end;

    ADockClient := FindDockClient(Client);
    if ADockClient <> nil then
      ClientDockStyle := ADockClient.DockStyle
    else Exit;

    if SenderDockStyle = nil then
      SenderStyleName := ''
    else SenderStyleName := SenderDockStyle.ClassName;

    if ClientDockStyle = nil then
      ClientStyleName := ''
    else ClientStyleName := ClientDockStyle.ClassName;

    Result := Result and (SenderStyleName = ClientStyleName);

JudgeRelation:
{    if Result and (GlobalDockClient <> nil) then
    begin
      ADockClient := nil;
      Level_1 := GlobalDockClient.DockLevel;
      if DropCtl <> nil then
        ADockClient := FindDockClient(DropCtl);
      if ADockClient = nil then
      begin
        if Sender.Parent is TCnDockableForm then
          ADockClient := FindDockClient(Sender.Parent)
        else
          ADockClient := FindDockClient(Sender);
      end;
      if ADockClient <> nil then
        Result := Level_1 = ADockClient.DockLevel;
    end;}
(*    ADockClient := GlobalDockClient;
    if Result and (Sender <> nil) and (ADockClient <> nil) then
    begin
      ADockServer := FindDockServer(Sender.Parent);
      if (ADockServer <> nil) and (ADockClient.RelativeServer <> nil) then
        Result := Result and (ADockServer = ADockClient.RelativeServer)
      else
      begin
        ADockServer := ADockClient.RelativeServer;
        if Sender.Parent is TCnDockableForm then
          ADockClient := FindDockClient(Sender.Parent)
        else
          ADockClient := FindDockClient(Sender);
        if ((ADockClient <> nil) and (ADockClient.RelativeServer = nil)) or (ADockServer = nil) then
          Exit;
        if (ADockClient <> nil){ or (ADockServer <> nil) }then
          Result := Result and (ADockServer = ADockClient.RelativeServer);
      end;
    end;*)
  end;
end;

{ ���´����Caption��ͬʱ���¸����ڵ�Caption���ݹ���� }
procedure UpdateCaption(Source: TWinControl; Exclude: TControl);
var i: Integer;
  Host: TCnDockableForm;
begin
  if (Source <> nil) and (Source.Parent is TCnDockableForm) then
  begin
    Host := TCnDockableForm(Source.Parent);
    Host.Caption := '';
    { ���±����Caption }
    for I := 0 to Source.DockClientCount - 1 do
    begin
      if Source.DockClients[I].Visible and (Source.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Source.DockClients[I]).Caption + gs_CnStringSplitter;
    end;
    { ����TCnTabPageControl�ı�ǩ }
    if (Host.HostDockSite is TCnTabPageControl) then
    begin
      with TCnTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Source) then
          ActivePage.Caption := Host.Caption;
    end;
    UpdateCaption(Host.HostDockSite, nil);
  end;
end;

{ ʹͣ������AControl��Sender���ϳ��� }
procedure DoFloat(Sender, AControl: TControl);
var
  ARect: TRect;
  CH{ CaptionHeight }, BW{ BorderWidth }: Integer;
begin
  { ��ñ������ĸ߶Ⱥͱ߿�Ŀ�� }
  BW := Cn_GetSysBorderWidth;
  CH := Cn_GetSysCaptionHeight;
  { ��������ת�� }
  ARect.TopLeft := Sender.ClientToScreen(Point(
    -(BW + 3), -(CH + BW + 1)));
  ARect.BottomRight := Sender.ClientToScreen(Point(
    Sender.UndockWidth - (BW + 3),
    Sender.UndockHeight - (BW + CH + 1)));
//  Cn_LockWindow(TWinControl(Sender));
  try
    { ʹAControl���� }
    AControl.ManualFloat(ARect);
  finally
//    Cn_UnLockWindow;
  end;
  if (AControl.Left <> ARect.Left) or (AControl.Top <> ARect.Top) then
  begin
    AControl.Left := ARect.Left;
    AControl.Top := ARect.Top;
  end;
//  SetDockSite(AControl, True);
end;

{ ����ͣ�������� }
function ComputeDockingRect(AControl: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect,
  DockLeftRect,
  DockBottomRect,
  DockRightRect,
  DockCenterRect: TRect;
begin
  Result := alNone;
  // ����ͣ������
  if AControl = nil then Exit;
  with AControl do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5 * 4);
    DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight);

    DockCenterRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5);
    DockCenterRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5 * 4);

    // ����������ĸ�ͣ������
    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := ClientWidth div 2;
    end else if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := ClientHeight div 2;
    end else if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := ClientWidth div 2;
    end else if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Top := ClientHeight div 2;
    end else if PtInRect(DockCenterRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
    end;
    if Result = alNone then Exit;

    // DockRect����Ļ����
    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

{ ��ʾ���壬���������Ҳ�ǲ��ɼ��ģ�����ʾ�����ڣ��ݹ���� }
procedure ShowDockForm(DockWindow: TWinControl);
  procedure ShowClient(Client, DockParent: TWinControl);
  var ADockClient: TCnDockClient;
    i: Integer;
  begin
    { ����������滹��ͣ�����壬�ͷֱ�������ǵ�MakeDockClientEvent }
    if (DockParent is TCnDockableForm) and (Client <> nil) then
    begin
      with TCnDockableForm(DockParent).DockableControl do
        for i := 0 to DockClientCount - 1 do
        begin
          if DockClients[i] <> Client then
            MakeDockClientEvent(DockClients[i], True);
        end;
      if Client.HostDockSite is TCnCustomDockControl then
        TCnCustomDockControl(Client.HostDockSite).UpdateCaption(nil);
    end else
    begin
      { ����͵����Լ���MakeShowEvent }
      ADockClient := FindDockClient(DockParent);
      if ADockClient <> nil then
      begin
        ADockClient.DockStyle.ShowDockForm(ADockClient);
        if DockParent.CanFocus then
          DockParent.SetFocus;
      end;
    end;
    if DockParent.Parent = nil then
      SetForegroundWindow(DockParent.Handle);
  end;

  { ��ʾTCnDockPanel }
  function ShowDockPanel(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
    begin
      if Client.HostDockSite is TCnDockPanel then
      begin
        { ���ݾ��������ʾTCnDockPanel }
        TCnDockPanel(Client.HostDockSite).ShowDockPanel(True, Client, sdfDockPanel);
        Result := nil;
      end;
    end;
  end;

  { ��ʾ��ҳ���� }
  function ShowTabDockHost(Client: TWinControl): TWinControl;
  var i: Integer;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if Client.HostDockSite is TCnTabPageControl then
      begin
        with TCnTabPageControl(Client.HostDockSite) do
          for i:=0 to PageCount - 1 do
          begin
            if Pages[i].Controls[0] = Client then
            begin
              Pages[i].Show;
              Break;
            end;
          end;
        if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TCnDockPanel) then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TCnTabPageControl) then
            Result := ShowTabDockHost(Result);
        end;
      end;
    end;
  end;

  { ��ʾƽ�̴��� }
  function ShowConjoinDockHost(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TCnDockPanel) then
      begin
        if Client.HostDockSite.Parent <> nil then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TCnConjoinPanel) then
            Result := ShowConjoinDockHost(Result);
        end;
      end;
    end;
  end;

  procedure ShowPopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TCnVSPopupPanel then
      TCnVSPopupPanel(Client.HostDockSite).VSChannel.PopupDockForm(Client)
    else if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
        TCnVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.PopupDockForm(Client)
      else if (Client.HostDockSite.Parent.HostDockSite is TCnDockPanel) then
        Client.HostDockSite.Parent.HostDockSite.Invalidate;
    end;
  end;

var TmpDockWindow: TWinControl;
begin
  TmpDockWindow := DockWindow;
  repeat
    DockWindow := ShowTabDockHost(DockWindow);
    DockWindow := ShowConjoinDockHost(DockWindow);
    DockWindow := ShowDockPanel(DockWindow);
  until (DockWindow = nil) or (DockWindow.Parent = nil);
  ShowPopupPanel(TmpDockWindow);
end;

{ ���ش��壬�ݹ���� }
procedure HideDockForm(DockWindow: TControl);
  { ����ͣ���ͻ����� }
  procedure HideDockChild(DockWindow: TControl);
  var i: Integer;
    DockClient: TCnDockClient;
  begin
    if DockWindow = nil then Exit;
    if (DockWindow is TCnDockableForm) and (DockWindow.Visible) then
    begin
      { ��������ϻ����Ӵ��岢�Ҵ����ǿɼ��ģ��͵����Ӵ����DockStyle��HideDockForm }
      with TCnDockableForm(DockWindow).DockableControl do
        for i := 0 to DockClientCount - 1 do
          HideDockChild(DockClients[i]);
    end;
    DockClient := FindDockClient(DockWindow);
    if (DockWindow is TForm) and (TForm(DockWindow).FormStyle <> fsMDIChild) then
      DockClient.DockStyle.HideDockForm(DockClient);
  end;

  { ����ͣ�������� }
  procedure HideDockParent(DockWindow: TControl);
  var
    Host: TWinControl;
    DockClient: TCnDockClient;
  begin
    if (DockWindow <> nil) and (DockWindow.HostDockSite <> nil) then
    begin
      Host := DockWindow.HostDockSite;
      if Host.VisibleDockClientCount = 0 then
      begin
        { ���ͣ������ĸ�������Ŀɼ�ͣ������ĸ���Ϊ0����Ӧ���������ͣ������ĸ� }
        if Host is TCnDockPanel then
          { ����TCnDockPanel�Ĵ�С }
          TCnDockPanel(Host).ShowDockPanel(False, nil)
        else begin
          if Host.Parent <> nil then
          begin
            { ʹͣ������ĸ����أ����ҵ����������TCnDockClient��DockStyle��HideDockForm���� }
            DockClient := FindDockClient(Host.Parent);
            DockClient.DockStyle.HideDockForm(DockClient);
            HideDockParent(Host.Parent);
          end;
        end
      end;
    end;
  end;

  procedure HidePopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TCnVSPopupPanel then
      TCnVSPopupPanel(Client.HostDockSite).VSChannel.HidePopupPanel(Client)
    else if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
        TCnVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.HidePopupPanel(Client)
      else if (Client.HostDockSite.Parent.HostDockSite is TCnDockPanel) then
        Client.HostDockSite.Parent.HostDockSite.Invalidate
    end;
  end;

var TmpDockWindow: TWinControl;

begin
  TmpDockWindow := TWinControl(DockWindow);
  HideDockChild(DockWindow);
  HideDockParent(DockWindow);
  if (DockWindow.HostDockSite is TCnCustomDockControl) then
    TCnCustomDockControl(DockWindow.HostDockSite).UpdateCaption(DockWindow);
  HidePopupPanel(TmpDockWindow);
end;

{ ʹ��Host�ϵ�TDockClient�ؼ�����OnShowEvent��OnHideEvent���ݹ���� }
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
var i: Integer;
  ADockClient: TCnDockClient;
begin
  ADockClient := FindDockClient(Host);
  if ADockClient <> nil then
  begin
    if Visible then ADockClient.MakeShowEvent
    else ADockClient.MakeHideEvent;
    if (Host is TCnDockableForm) and (Host.Visible) then
    begin
      with TCnDockableForm(Host).DockableControl do
        for i := 0 to DockClientCount - 1 do
          MakeDockClientEvent(DockClients[i], Visible);
    end;
  end;
end;

{ ��ô���Ŀɼ��ԣ��ݹ���� }
function GetFormVisible(DockWindow: TWinControl): Boolean;
var
  ADockClient: TCnDockClient;
begin
  Result := True;
  ADockClient := FindDockClient(DockWindow);
  if ADockClient <> nil then
  begin
    Result := ADockClient.DockStyle.GetDockFormVisible(ADockClient);
  end;
end;

{ ���÷�ҳ�ķ������е�PageControl��PopMenu }
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
var i: Integer;
begin
  DockPageControlPopupMenu := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.CustomForms[i] is TCnTabDockHostForm then
      TCnTabDockHostForm(Screen.CustomForms[i]).PageControl.PopupMenu := Value;
  end;
end;

{ ���÷�ҳ�ķ������е�PageControl��HotTrack }
procedure SetDockPageControlHotTrack(Value: Boolean);
var i: Integer;
begin
  DockPageControlHotTrack := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.CustomForms[i] is TCnTabDockHostForm then
      TCnTabDockHostForm(Screen.CustomForms[i]).PageControl.HotTrack := Value;
  end;
end;

{ ���÷�ҳ�ķ��������ʾ��� }
procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
var i: Integer;
begin
  TabDockHostBorderStyle := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.CustomForms[i] is TCnTabDockHostForm) and (Screen.CustomForms[i].HostDockSite = nil) then
      TCnTabDockHostForm(Screen.CustomForms[i]).BorderStyle := Value;
  end;
end;

{ ����ƽ�̵ķ��������ʾ��� }
procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
var i: Integer;
begin
  ConjoinDockHostBorderStyle := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.CustomForms[i] is TCnConjoinDockHostForm) and (Screen.CustomForms[i].HostDockSite = nil) then
      TCnConjoinDockHostForm(Screen.CustomForms[i]).BorderStyle := Value;
  end;
end;

{ ��ͣ���������Ϣ������ļ� }
procedure SaveDockTreeToFile(FileName: string);
var CnDockInfoTree: TCnDockInfoTree;
  i: Integer;
begin
  HideAllPopupPanel(nil);
  { ����ͣ����Ϣ�� }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  try
    { ����Ӧ�ó�������д��� }
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      ((FindDockClient(Screen.CustomForms[i]) <> nil) or (FindDockServer(Screen.CustomForms[i]) <> nil)) then
        { �ѷ��������Ĵ����ͣ����Ϣ���浽��Ϣ���� }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;
    { ��INI�ļ� }
    CnDockInfoTree.DockInfoIni := TIniFile.Create(FileName);
    try
      { ����Ϣ���е�ͣ����Ϣд��INI�ļ��� }
      CnDockInfoTree.WriteInfoToIni;
    finally
      { �ر�INI�ļ� }
      CnDockInfoTree.DockInfoIni.Free;
    end;
  finally
    { �ͷ�ͣ����Ϣ�� }
    CnDockInfoTree.Free;
  end;
end;

procedure ReshowAllVisibleWindow;
var i: Integer;
begin
  if IsWinXP then
  begin
    for i := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[i].Visible then
        Windows.ShowWindow(Screen.Forms[i].Handle, SW_SHOW)
      else
        Windows.ShowWindow(Screen.Forms[i].Handle, SW_HIDE);
    end;
  end;
end;

{ ���ļ������ͣ���������Ϣ }
procedure LoadDockTreeFromFile(FileName: string);
var CnDockInfoTree: TCnDockInfoTree;
  Form: TForm;
begin
  HideAllPopupPanel(nil);
  // ����һ��û�б߿򣬴�СΪ0�Ĵ��壬�������������Ƿ�ֹ��Ļ��˸
  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0,0,0,0);
  Form.Visible := True;
  Form.Name := 'A_B_C_D_E_F_G_H_I_J_K_L_M_N';
  { ����ͣ����Ϣ�� }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  { ��סWindows���� }
  Cn_LockWindow(nil);
  try
    { ��INI�ļ� }
    CnDockInfoTree.DockInfoIni := TIniFile.Create(FileName);
    try
      IsLoading := True;
      { ��INI�ļ���������ݲ��һ�ԭͣ����Ϣ }
      CnDockInfoTree.ReadInfoFromIni;
    finally
      { �ر�INI�ļ� }
      CnDockInfoTree.DockInfoIni.Free;
      IsLoading := False;
    end;
  finally
    Form.Release;
    { ����Windows���� }
    Cn_UnLockWindow;
    { �ͷ�ͣ����Ϣ�� }
    CnDockInfoTree.Free;
  end;
  ReshowAllVisibleWindow;
end;

{ ��ͣ���������Ϣ�����ע��� }
procedure SaveDockTreeToReg(RootKey: DWORD; RegPath: string);
var CnDockInfoTree: TCnDockInfoTree;
  i: Integer;
begin
  HideAllPopupPanel(nil);
  { ����ͣ����Ϣ�� }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  try
    { ����Ӧ�ó�������д��� }

    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      (FindDockClient(Screen.CustomForms[i]) <> nil) then
        { �ѷ��������Ĵ����ͣ����Ϣ���浽��Ϣ����(������ͣ���ͻ�) }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;

    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      (FindDockServer(Screen.CustomForms[i]) <> nil) then
        { �ѷ��������Ĵ����ͣ����Ϣ���浽��Ϣ����(������ͣ��������) }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;

    { ��TRegistry }
    CnDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      { ����Ϣ���е�ͣ����Ϣд��ע����� }
      CnDockInfoTree.DockInfoReg.RootKey := RootKey;
      CnDockInfoTree.WriteInfoToReg(RegPath);
    finally
      { �ر�ע��� }
      CnDockInfoTree.DockInfoReg.Free;
    end;
  finally
    { �ͷ�ͣ����Ϣ�� }
    CnDockInfoTree.Free;
  end;
end;

{ ��ע��������ͣ���������Ϣ }
procedure LoadDockTreeFromReg(RootKey: DWORD; RegPath: string);
var CnDockInfoTree: TCnDockInfoTree;
  Form: TForm;
begin
  HideAllPopupPanel(nil);
  // ����һ��û�б߿򣬴�СΪ0�Ĵ��壬�������������Ƿ�ֹ��Ļ��˸
  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0,0,0,0);
  Form.Visible := True;
  Form.Name := 'A_B_C_D_E_F_G_H_I_J_K_L_M_N';
  { ����ͣ����Ϣ�� }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  { ��סWindows���� }
  Cn_LockWindow(nil);
  try
    { ��TRegistry }
    CnDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      IsLoading := True;
      { ��ע�����������ݲ��һ�ԭͣ����Ϣ }
      CnDockInfoTree.DockInfoReg.RootKey := RootKey;
      CnDockInfoTree.ReadInfoFromReg(RegPath);
    finally
      { �ر�TRegistry }
      CnDockInfoTree.DockInfoReg.Free;
      IsLoading := False;
    end;
  finally
    { ����Windows���� }
    Cn_UnLockWindow;
    { �ͷ�ͣ����Ϣ�� }
    CnDockInfoTree.Free;
    Form.Release;
  end;
  ReshowAllVisibleWindow;
end;

{ ���ÿؼ���DockSite���� }
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
begin
  TCnWinControlAccess(Control).DockSite := SiteValue;
  if (not (csDesigning in Control.ComponentState)) and (CnGlobalDockPresident <> nil) then
    CnGlobalDockPresident.RegisterDockSite(Control, SiteValue);
end;

{ ʹDockForm�ؼ������������������DockClients }
procedure DoFloatForm(DockForm: TControl);
var i, j: Integer;
  ADockServer: TCnDockServer;
  ARect: TRect;
begin
  if (DockForm is TCnDockableForm) then
  begin
    {���������TCnDockableForm����ʹ�������ͣ�����帡��}
    with TCnDockableForm(DockForm).DockableControl do
    begin
      for i := DockClientCount - 1 downto 0 do
      begin
        DoFloatForm(DockClients[i]);
      end;
//      DockForm.Hide;
      DockForm.ManualDock(nil);
    end;
  end else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      {�����������TCnDockServer����ʹ���ĸ������TCnDockPanel����Ĵ��帡��}
      for i := 0 to 3 do
      begin
        for j := ADockServer.DockPanel[i].DockClientCount - 1 downto 0 do
          DoFloatForm(ADockServer.DockPanel[i].DockClients[j]);
        if ADockServer.DockPanel[i] is TCnVSNETDockPanel then
        begin
          with TCnVSNETDockPanel(ADockServer.DockPanel[i]).VSChannel do
          begin
            RemoveAllBlock;
            HidePopupPanel(ActiveDockForm);
          end;
        end;
      end;
    end else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        {���������ͣ��״̬�ģ���ת������ϵ}
        if (DockForm.HostDockSite.Parent is TCnDockableForm) and
          (DockForm.HostDockSite.DockClientCount <= 2) then
          PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end else ARect := DockForm.BoundsRect;
//      DockForm.Hide;
      {ʹ���帡��}
      if DockForm.HostDockSite is TCnVSPopupPanel then
      begin
        TCnVSPopupPanel(DockForm.HostDockSite).VSNETDockPanel.VSChannel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(nil, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end else
        DockForm.ManualDock(nil);
    end;
  end;
end;

{ ɾ���������õ�TCnDockableForm���� }
procedure FreeAllDockableForm;
var i: Integer;
begin
  Assert(CnGlobalDockPresident <> nil);
  for i := CnGlobalDockPresident.DockableFormList.Count - 1 downto 0 do
  begin
    if TCnDockableForm(CnGlobalDockPresident.DockableFormList[i]).DockableControl.DockClientCount = 0 then
      TCnDockableForm(CnGlobalDockPresident.DockableFormList[i]).Free;
  end;
end;

{ ʹӦ�ó�������еĴ��嶼���� }
procedure DoFloatAllForm;
var i: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      // ��������Ӧ�ó�������д��壬Ȼ���һ��Ĵ��屣�浽TempList��
      if not (Screen.CustomForms[i] is TCnDockableForm) then
        TempList.Add(Screen.CustomForms[i]);
    end;
    {����Ӧ�ó���Ĵ��ڣ�ʹ���Ǹ���}
    for i := 0 to TempList.Count - 1 do
      DoFloatForm(TempList[i]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

{ �õ�AControl�ͻ�����Align�������е��ӿؼ��ۼӵĴ�С��AControl�߿�ľ��� }
function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to AControl.ControlCount - 1 do
  begin
    if (AControl.Controls[i].Align = Align) and AControl.Controls[i].Visible and (AControl.Controls[i] <> Exclude) and
      not ((AControl.Controls[i] is TCnDockSplitter) or (AControl.Controls[i] is TCnDockPanel)) then
    begin
      if Align in [alLeft, alRight] then
        Inc(Result, AControl.Controls[i].Width)
      else Inc(Result, AControl.Controls[i].Height);
    end;
  end;
end;

{******************************************************************************}
{           ����ĺ����ͷ��������ڲ�����                                       }
{******************************************************************************}

function GetActiveControl(AForm: TCustomForm): TWinControl;
var AControl: TWinControl;
begin
  Result := nil;
  AControl := AForm.ActiveControl;
  while AControl <> nil do
  begin
    if (AControl.HostDockSite <> nil) then
    begin
      Result := AControl;
      Exit;
    end;
    AControl := AControl.Parent;
  end;
end;

function GetHostDockParent(AControl: TWinControl): TWinControl;
begin
  Result := nil;
  while AControl <> nil do
  begin
    if (AControl.HostDockSite <> nil) then
    begin
      Result := AControl.HostDockSite;
      Exit;
    end;
    AControl := AControl.Parent;
  end;
end;

{ ����������Control�ϵ�TCnDockClint��������� }
procedure ResetDockClient(Control: TControl; NewTarget: TControl);
begin
  ResetDockClient(FindDockClient(Control), NewTarget);
end;

procedure ResetDockClient(DockClient: TCnDockClient; NewTarget: TControl);
var point: TPoint;
begin
  if DockClient <> nil then
  begin
    if (DockClient.ParentForm.HostDockSite is TCnDockPanel) and (NewTarget is TCnDockPanel) then
    begin

    end else
    begin

      if (DockClient.LastDockSite is TCnDockPanel) and (NewTarget is TCnDockPanel) and (DockClient.LastDockSite <> NewTarget) then
      begin
        with TCnDockPanel(DockClient.LastDockSite) do
        begin
          if UseDockManager and (CnDockManager <> nil) then
            CnDockManager.RemoveControl(DockClient.ParentForm);
        end;
      end;

      if (DockClient.ParentForm.HostDockSite is TCnDockPanel) then
      begin
//        if (NewTarget is TCnTabPageControl) or (NewTarget is TCnConjoinPanel) then
//          DockClient.LastDockSite := nil
//        else
          DockClient.LastDockSite := DockClient.ParentForm.HostDockSite;
      end
      else //if not (DockClient.LastDockSite is TCnDockPanel) then
        DockClient.LastDockSite := nil;

      if DockClient.ParentForm.HostDockSite = nil then
      begin
        DockClient.UnDockLeft := DockClient.ParentForm.BoundsRect.TopLeft.x;
        DockClient.UnDockTop := DockClient.ParentForm.BoundsRect.TopLeft.y;
      end else
      begin
        point := DockClient.ParentForm.BoundsRect.TopLeft;
        point := DockClient.ParentForm.HostDockSite.ClientToScreen(point);
        DockClient.UnDockLeft := point.x;
        DockClient.UnDockTop := point.y;
      end;
    end;
  end;
end;

{ TCnDockPanel }

constructor TCnDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  Width := 10;
  Height := 10;
end;

destructor TCnDockPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCnDockPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  // ���Source.Control��TCnDockableForm,���п��ܱ��ͷŵ���������Ѿ����ͷŵ��ˣ�
  // �Ͳ�Ҫ�ٵ���ShowDockPanel�����ˣ���Ȼ��ʹTCnDockPanel��Ȼ��߸߶�Ϊ0��
  if Source.Control <> nil then
    ShowDockPanel(True, Source.Control);
end;

procedure TCnDockPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
    inherited CustomDockOver(Source, X, Y, State, Accept);
end;

procedure TCnDockPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TCnDockPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnDockPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
var
  ARect: TRect;
//  Tmp: Integer;
{  function GetParentClientW(Control: TWinControl): Integer;
  var i: Integer;
  begin
    Result := Control.ClientWidth;
    for i := 0 to Control.ControlCount - 1 do
    begin
      if (Control.Controls[i].Align in [alLeft, alRight])
        and (Control.Controls[i] <> Self)
        and (Control.Controls[i].Visible) then
        Result := Result - Control.Controls[i].Width;
    end;
  end;

  function GetParentClientH(Control: TWinControl): Integer;
  var i: Integer;
  begin
    Result := Control.ClientHeight;
    for i := 0 to Control.ControlCount - 1 do
    begin
      if (Control.Controls[i].Align in [alTop, alBottom])
        and (Control.Controls[i] <> Self)
        and (Control.Controls[i].Visible) then
        Result := Result - Control.Controls[i].Height;
    end;
  end;
var ParentClientW, ParentClientH: Integer;}
begin
  inherited CustomPositionDockRect(Source, X, Y);
  if VisibleDockClientCount = 0 then
  begin
    // ��סDockRectԤ��ͣ������.
    if GlobalDockClient <> nil then
    begin
      case Align of
        alTop:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(
            Point(Width, Source.Control.TBDockHeight));
        end;
        alBottom:
        begin
          ARect.TopLeft := ClientToScreen(
            Point(0, -Source.Control.TBDockHeight));
          ARect.BottomRight := ClientToScreen(
            Point(Width, 0));
        end;
        alLeft:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(
            Point(Source.Control.LRDockWidth, Height));
        end;
        alRight:
        begin
          ARect.TopLeft := ClientToScreen(Point(-Source.Control.LRDockWidth, 0));
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
      end;
      Source.DockRect := ARect;
    end;
  end;
end;

procedure TCnDockPanel.CustomStartDock(var Source: TCnDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TCnDockPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ShowDockPanel(False, nil);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
end;

procedure TCnDockPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  { ������Ӧ�ó����в�������ΪAControlName�Ĵ��� }
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnDockPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TSetDockPanelSizeFrom);
const
  DefaultDockSize = 100; // Ĭ�ϵĿ�Ȼ��ߴ�С
var
  DockHeight, DockWidth: Integer;
//Label Loop;
begin
  if (not MakeVisible and (VisibleDockClientCount > 1)) or
    (GlobalDockClient = nil){ or IsLoading }then Exit;
  { ����TCnDockSplitter������ }
  DockServer.DockSplitter[Integer(Align) - 1].Visible := MakeVisible;

  if (MakeVisible and (Client <> nil)) then// or ((not MakeVisible) and (Client <> nil)) then
  begin
    if (Width * Height = 0) then
    begin
      { �����Ⱥ͸߶���һ����0 }

      if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
      begin
        DockHeight := TBDockHeight;
        DockWidth := LRDockWidth;
      end else
      begin
        DockHeight := Client.TBDockHeight;
        DockWidth := Client.LRDockWidth;
      end;

      { ���ܵ���0����Ȼ��ʾ�᲻���� }
      if DockHeight = 0 then
        DockHeight := DefaultDockSize;
      if DockWidth = 0 then
        DockWidth := DefaultDockSize;

      Parent.DisableAlign;
      try
        case Align of
          alTop:
          begin
            { ���������Ŀͻ������ϱ��б�Ŀؼ����Ͱ�����߶ȼ������� }
            Top := DockServer.GetClientAlignControl(alTop);
            Height := DockHeight;
            DockServer.TopSplitter.Top := Top + Height;
          end;
          alBottom:
          begin
            { ���������Ŀͻ������±��б�Ŀؼ����Ͱ�����߶ȼ������� }
            Top := Parent.ClientHeight - DockServer.GetClientAlignControl(alBottom) - DockHeight + 1;
            Height := DockHeight;
            DockServer.BottomSplitter.Top := Top + DockServer.BottomSplitter.Height;
          end;
          alLeft:
          begin
            { ���������Ŀͻ���������б�Ŀؼ����Ͱ������ȼ������� }
            Left := DockServer.GetClientAlignControl(alLeft);
            Width := DockWidth;
            DockServer.LeftSplitter.Left := Left + Width;
          end;
          alRight:
          begin
            { ���������Ŀͻ������ұ��б�Ŀؼ����Ͱ������ȼ������� }
            Width := DockWidth;
            Left := Parent.ClientWidth - DockServer.GetClientAlignControl(alRight) - DockWidth + 1;
            DockServer.RightSplitter.Left := Left - DockServer.RightSplitter.Width;
          end;
        end;
      finally
        Parent.EnableAlign;
        if UseDockManager and (CnDockManager <> nil) then
          CnDockManager.ResetBounds(True);
      end;
      DockServer.DoFinishSetDockPanelSize(Self);
    end;
  end else
  begin
//    if (Client <> nil) and (not Client.Visible) then goto Loop;
    if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
    begin
      if Height > 0 then
        TBDockHeight := Height;
      if Width > 0 then
        LRDockWidth := Width;
    end else
    begin
      if Height > 0 then
        Client.TBDockHeight := Height;
      if Width > 0 then
        Client.LRDockWidth := Width;
    end;
    if Align in [alLeft, alRight] then
      Width := 0
    else Height := 0;
    { ��������λ�� }
    ResetPosition;
  end;
  { ��������Ļ�����ʾClient }
//Loop:
  if MakeVisible and (Client <> nil) then
  begin
    if (not Client.Visible) then
      Client.Show;
    if (not TWinControl(Client).Focused) and (TWinControl(Client).CanFocus) then
      TWinControl(Client).SetFocus;
  end;
end;

function TCnDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := IsDockable(Self, Client);
  ShowDockPanel(False, nil);
  Result := Result and (Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0);
  if Result then
  begin
    if not (Client is TCnDockableForm) then
      SetDockSite(TForm(Client), True);
  end;
end;

procedure TCnDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      { ------------------------------------------------------------------------ }
      SetDockSite(TForm(Source.Control), False);
    end;
    UpdateCaption(nil);
  end;
  ShowDockPanel(TWinControl(Source.DragTarget).VisibleDockClientCount > 0, Source.Control);
end;

function TCnDockPanel.GetPanelIndex: Integer;
begin
  case Align of
    alTop:    Result := 0;
    alBottom: Result := 1;
    alLeft:   Result := 2;
    alRight:  Result := 3;
  else
    Result := -1;
  end;
end;

procedure TCnDockPanel.Resize;
begin
  inherited;

end;

procedure TCnDockPanel.SetDockServer(const Value: TCnDockServer);
begin
  FDockServer := Value;
end;

procedure TCnDockPanel.ResetPosition;
begin
  { ����TCnDockPanel��λ�ã�ʹ�������ڷ�����Ŀͻ��������ڲ� }
  case Align of
    alLeft:
      Left := GetClientAlignControlArea(Parent, Align) + 1;
    alRight:
      Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align) - Width - 1;
    alTop:
      Top := GetClientAlignControlArea(Parent, Align) + 1;
    alBottom:
      Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align) - Height - 1;
  end;
end;

{ TCnAdvDockPanel }

//procedure TCnAdvDockPanel.CMDockClient(var Message: TCMDockClient);
//var DockClient: TCnDockClient;
//begin
{  with Message do
  begin
    DockClient := FindDockClient(DockSource.Control);
    if DockClient <> nil then
    begin
      if (DockClient.LastDockSite is TCnDockPanel) then
      begin
        with TCnDockPanel(DockClient.LastDockSite) do
        begin
          if UseDockManager and (CnDockManager <> nil) then
            CnDockManager.RemoveControl(DockSource.Control);

        end;
      end else
      begin

      end;
    end;
  end;}
//end;

procedure TCnAdvDockPanel.CMUnDockClient(var Message: TCMUnDockClient);
var DockClient: TCnDockClient;
begin
  if IsLoading then Exit;
  with Message do
  begin
    Result := 0;
    if UseDockManager and (CnDockManager <> nil) then
    begin
      DockClient := FindDockClient(Client);
      if (NewTarget <> nil) or
        ((Client <> nil) and (csDestroying in Client.ComponentState)) then
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := nil;
        CnDockManager.RemoveControl(Client);
      end else
      begin
        if (DockClient <> nil) then
          DockClient.LastDockSite := Self;
        CnDockManager.HideControl(Client);
      end;
    end;
  end;
end;

procedure TCnAdvDockPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvDockPanel.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TCnAdvDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ TCnSplitterStyle }

procedure TCnSplitterStyle.Assign(Source: TPersistent);
begin
  if Source is TCnSplitterStyle then
  begin
    Color := TCnSplitterStyle(Source).Color;
    Cursor := TCnSplitterStyle(Source).Cursor;
    ParentColor := TCnSplitterStyle(Source).ParentColor;
    ResizeStyle := TCnSplitterStyle(Source).ResizeStyle;
    Size := TCnSplitterStyle(Source).Size;
  end;
  inherited Assign(Source);
end;

procedure TCnSplitterStyle.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnSplitterStyle then
    with TCnSplitterStyle(Dest) do
    begin
      Color := Self.Color;
      Cursor := Self.Cursor;
      ParentColor := Self.ParentColor;
      ResizeStyle := Self.ResizeStyle;
      Size := Self.Size;
    end
  else inherited AssignTo(Dest);
end;

procedure TCnSplitterStyle.AssignToSplitter(Dest: TCnDockSplitter);
begin
  Dest.Color := Color;
  Dest.Cursor := Cursor;
  Dest.ParentColor := ParentColor;
  Dest.ResizeStyle := ResizeStyle;
  if Dest.Align in [alTop, alBottom] then
    Dest.Height := Size
  else Dest.Width := Size;
end;

constructor TCnSplitterStyle.Create(ASplitter: TCnDockSplitter; ACursor: TCursor);
begin
  inherited Create;
  FSplitter := ASplitter;
  Color := clBtnFace;
  Cursor := ACursor;
  ParentColor := False;
  ResizeStyle := rsPattern;
  FSize := 3;
  FMinSize := 30;
end;

procedure TCnSplitterStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ParentColor := False;
    if Assigned(FSplitter) then
      FSplitter.Color := Value;
  end;
end;

procedure TCnSplitterStyle.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  if Assigned(FSplitter) then
    FSplitter.Cursor := Value;
end;

procedure TCnSplitterStyle.SetMinSize(const Value: TCnSplitterSize);
begin
  FMinSize := Value;
  if Assigned(FSplitter) then
    FSplitter.MinSize := Value;
end;

procedure TCnSplitterStyle.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then FColor := FDockServer.ParentForm.Color;
    if Assigned(FSplitter) then
      FSplitter.ParentColor := Value;
  end;
end;

procedure TCnSplitterStyle.SetResizeStyle(const Value: TResizeStyle);
begin
  FResizeStyle := Value;
  if Assigned(FSplitter) then
    FSplitter.ResizeStyle := Value;
end;

procedure TCnSplitterStyle.SetSize(const Value: TCnSplitterSize);
begin
  FSize := Value;
  if Assigned(FSplitter) then
  begin
    if FSplitter.Align in [alTop, alBottom] then
      FSplitter.Height := Value
    else FSplitter.Width := Value;
  end;
end;

procedure TCnSplitterStyle.SetSplitterStyle;
begin
  AssignToSplitter(FSplitter);
end;

{ TCnDockBaseControl }

destructor TCnDockBaseControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if @FOldWindowProc <> nil then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
    if Assigned(FDockStyle) and not (FDockStyle is TCnBasicDockStyle) then
      FDockStyle.SetDockBaseControl(False, Self);
  end;
  if FDockStyle <> nil then
    FDockStyle.RemoveDockBaseControl(Self);
  inherited Destroy;
end;

constructor TCnDockBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentForm := TForm(AOwner);
  FEnableDock := True;
  FLeftDock := True;
  FTopDock := True;
  FRightDock := True;
  FBottomDock := True;
  FEachOtherDock := True;
  FDockStyle := nil;
  if not (csDesigning in ComponentState) then
  begin
    FOldOnClose := FParentForm.OnClose;
    ParentForm.OnClose := DoFormOnClose;
    FOldOnCreate := FParentForm.OnCreate;
    ParentForm.OnCreate := DoFormOnCreate;
    { �����ϵĴ��ڹ��� }
    FOldWindowProc := FParentForm.WindowProc;
    { ���ش��ڹ��� }
    FParentForm.WindowProc := WindowProc;
  end;
end;

procedure TCnDockBaseControl.SetBottomDock(const Value: Boolean);
begin
  if CanSetBottomDocked then
    FBottomDock := Value;
end;

procedure TCnDockBaseControl.SetEachotherDock(const Value: Boolean);
begin
  if CanSetEachOtherDocked then
    FEachotherDock := Value;
end;

procedure TCnDockBaseControl.SetEnableDock(const Value: Boolean);
begin
  if CanSetEnableDocked then
    FEnableDock := Value;
end;

procedure TCnDockBaseControl.SetLeftDock(const Value: Boolean);
begin
  if CanSetLeftDocked then
    FLeftDock := Value;
end;

procedure TCnDockBaseControl.SetRightDock(const Value: Boolean);
begin
  if CanSetRightDocked then
    FRightDock := Value;
end;

procedure TCnDockBaseControl.SetTopDock(const Value: Boolean);
begin
  if CanSetTopDocked then
    FTopDock := Value;
end;

procedure TCnDockBaseControl.Assign(Source: TPersistent);
begin
  if Source is TCnDockBaseControl then
  begin
    FEnableDock := TCnDockBaseControl(Source).EnableDock;
    FLeftDock := TCnDockBaseControl(Source).LeftDock;
    FTopDock := TCnDockBaseControl(Source).TopDock;
    FRightDock := TCnDockBaseControl(Source).RightDock;
    FBottomDock := TCnDockBaseControl(Source).BottomDock;
    FEachOtherDock := TCnDockBaseControl(Source).EachOtherDock;
    FDockStyle := TCnDockBaseControl(Source).DockStyle;
    if FDockStyle <> nil then
      FDockStyle.AddDockBaseControl(Self);
  end else inherited Assign(Source);
end;

procedure TCnDockBaseControl.SetDockStyle(const Value: TCnBasicDockStyle);
begin
  { ����ԭ����FDockStyle�������Value���TCnDockBaseControl���� }
  if FDockStyle <> nil then
    FDockStyle.RemoveDockBaseControl(Self);
  if Value <> nil then
    Value.AddDockBaseControl(Self);

  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    // ��ǰ������ڣ����޸�FDockStyle��ֵ��
    if Value <> nil then
    begin
      Value.SetDockBaseControl(True, Self);
      // ����FreeNotification������ʹһ�������TCnDockBaseControl����DockStyle
      // ���ԣ����DockStyle��������һ�������ϵĿؼ�����������쳣��
      Value.FreeNotification(ParentForm);
    end;
    FDockStyle := Value;
  end else
  begin
    // �������ڲ�������DockStyle���ԡ�
    ShowMessage(gs_CannotChangeDockStyleProperty);
  end;
end;

procedure TCnDockBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  // ��DockStyle���ԵĿؼ���������ͷŵ�ʱ�򣬻����Notification������
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    // ������ͷţ���Ҫ��DockStyle�������nil��
    if AComponent = FDockStyle then
    begin
      FDockStyle.SetDockBaseControl(False, Self);
      FDockStyle := nil;
    end;
end;

procedure TCnDockBaseControl.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Ĭ�ϵ�DockStyle������TCnBasicDockStyle�ࡣ
    if not Assigned(DockStyle) then
      DockStyle := TCnBasicDockStyle.Create(ParentForm);
    if Assigned(DockStyle) then
    begin
      DockStyle.SetDockBaseControl(True, Self);
    end;
  end;
  inherited Loaded;
end;

function TCnDockBaseControl.GetDockStyleVersion: string;
begin
  Result := gs_CnDockManagerVersion;
end;

procedure TCnDockBaseControl.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (Action = caFree) and (CnGlobalDockPresident <> nil) then
  begin
    { ��ParentForm��ӵ�CnDockManager�� }
    if Self is TCnDockServer then
      CnGlobalDockPresident.RemoveDockServerFromDockManager(ParentForm)
    else if Self is TCnDockClient then
      CnGlobalDockPresident.RemoveDockClientFromDockManager(ParentForm);
  end;
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TCnDockBaseControl.DoFormOnCreate(Sender: TObject);
begin
  if CnGlobalDockPresident <> nil then
  begin
    { ��CnDockManager���ParentForm }
    if Self is TCnDockServer then
      CnGlobalDockPresident.AddDockServerToDockManager(ParentForm)
    else if Self is TCnDockClient then
      CnGlobalDockPresident.AddDockClientToDockManager(ParentForm);
  end;
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TCnDockBaseControl.WindowProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Message);
  end;
end;

procedure TCnDockBaseControl.SetParentComponent(Value: TComponent);
var ADockBaseControl: TCnDockBaseControl;
begin
  ADockBaseControl := FindDockBaseControl(ParentForm);
  if (Assigned(ADockBaseControl)) and (ADockBaseControl <> Self) then
    raise EInvalidOperation.Create(Format(gs_CannotLayAnother, [ADockBaseControl.ClassName, ClassName]));
  inherited SetParentComponent(Value);
end;

function TCnDockBaseControl.CanSetBottomDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetBottomDocked(Self);
end;

function TCnDockBaseControl.CanSetEachOtherDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetEachOtherDocked(Self);
end;

function TCnDockBaseControl.CanSetEnableDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetEnableDocked(Self);
end;

function TCnDockBaseControl.CanSetLeftDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetLeftDocked(Self);
end;

function TCnDockBaseControl.CanSetRightDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetRightDocked(Self);
end;

function TCnDockBaseControl.CanSetTopDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetTopDocked(Self);
end;

{ TCnDockServer }

constructor TCnDockServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSplitterStyle;
end;

destructor TCnDockServer.Destroy;
begin
//  if not (csDesigning in ComponentState) then
//    DestroyDockPanelAndSplitter;
  DestroySplitterStyle;
  inherited Destroy;
end;

procedure TCnDockServer.CreateDockPanelAndSplitter;
var ControlList: TList;

  function CreatePanel(Align: TAlign; Name: string): TCnDockPanel;
  begin
    if (FDockPanelClass <> nil) and
      (FDockPanelClass <> TCnDockPanelClass(ClassType)) then
    begin
      Result := FDockPanelClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Caption := '';
      Result.Align := Align;
      Result.DockServer := Self;
      Result.ResetPosition;
      if Align in [alTop, alBottom] then
        Result.Height := 0
      else
        Result.Width := 0;
      SetDockSite(Result, True);
      { ����DockStyle��AssignConjoinServerOption������������ƽ������ }
      if DockStyle <> nil then
        DockStyle.AssignConjoinServerOption(Result);
    end else Result := nil;
  end;

  function CreateSplitter(Align: TAlign; Name: string): TCnDockSplitter;
  begin
    if (FDockSplitterClass <> nil) and
      (FDockSplitterClass <> TCnDockSplitterClass(ClassType)) then
    begin
      Result := FDockSplitterClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Visible := False;
      Result.Align := Align;
      Result.DockServer := Self;
    end else Result := nil;
  end;

begin
  ControlList := TList.Create;
  try
    FLeftDockPanel    := CreatePanel(alLeft, 'LeftDockPanel_A_B_C_D_E_F_G');
    FLeftSplitter     := CreateSplitter(alLeft, 'LeftSplitter_A_B_C_D_E_F_G');
    FRightDockPanel   := CreatePanel(alRight, 'RightDockPanel_A_B_C_D_E_F_G');
    FRightSplitter    := CreateSplitter(alRight, 'RightSplitter_A_B_C_D_E_F_G');
    FTopDockPanel     := CreatePanel(alTop, 'TopDockPanel_A_B_C_D_E_F_G');
    FTopSplitter      := CreateSplitter(alTop, 'TopSplitter_A_B_C_D_E_F_G');
    FBottomDockPanel  := CreatePanel(alBottom, 'BottomDockPanel_A_B_C_D_E_F_G');
    FBottomSplitter   := CreateSplitter(alBottom, 'BottomSplitter_A_B_C_D_E_F_G');
  finally
    ControlList.Free;
  end;
end;

procedure TCnDockServer.CreateSplitterStyle;
begin
  FLeftSplitterStyle    := TCnSplitterStyle.Create(FLeftSplitter, crHSplit);
  FTopSplitterStyle     := TCnSplitterStyle.Create(FTopSplitter, crVSplit);
  FRightSplitterStyle   := TCnSplitterStyle.Create(FRightSplitter, crHSplit);
  FBottomSplitterStyle  := TCnSplitterStyle.Create(FBottomSplitter, crVSplit);

  FLeftSplitterStyle.FDockServer := Self;
  FTopSplitterStyle.FDockServer := Self;
  FRightSplitterStyle.FDockServer := Self;
  FBottomSplitterStyle.FDockServer := Self;
end;

procedure TCnDockServer.DestroySplitterStyle;
begin
  FLeftSplitterStyle.Free;
  FTopSplitterStyle.Free;
  FRightSplitterStyle.Free;
  FBottomSplitterStyle.Free;
end;

procedure TCnDockServer.SetLeftSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FLeftSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetTopSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FTopSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetRightSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FRightSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetBottomSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FBottomSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetSplitterStyle;
begin
  LeftSplitterStyle.Splitter := FLeftSplitter;
  LeftSplitterStyle.SetSplitterStyle;
  TopSplitterStyle.Splitter := FTopSplitter;
  TopSplitterStyle.SetSplitterStyle;
  RightSplitterStyle.Splitter := FRightSplitter;
  RightSplitterStyle.SetSplitterStyle;
  BottomSplitterStyle.Splitter := FBottomSplitter;
  BottomSplitterStyle.SetSplitterStyle;
end;

procedure TCnDockServer.WindowProc(var Message: TMessage);
begin
  { ִ��FDockStyle����Ϣ���� }
  if Assigned(FDockStyle) then
  begin
    if FDockStyle.DockServerWindowProc(Self, Message) then
      { ���DockStyle�Ѿ�ִ������Ϣ�����Ͳ�ִ��Ĭ�ϵĴ��� }
      Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    if Message.Msg = WM_ACTIVATE then
      WMActivate(TWMActivate(Message));
  end;
  inherited WindowProc(Message);
end;

function TCnDockServer.GetDockPanel(Index: Integer): TCnDockPanel;
begin
  Result := nil;
  case Index of
    0: Result := FTopDockPanel;
    1: Result := FBottomDockPanel;
    2: Result := FLeftDockPanel;
    3: Result := FRightDockPanel;
  end;
end;

function TCnDockServer.GetDockSplitter(Index: Integer): TCnDockSplitter;
begin
  Result := nil;
  case Index of
    0: Result := FTopSplitter;
    1: Result := FBottomSplitter;
    2: Result := FLeftSplitter;
    3: Result := FRightSplitter;
  end;
end;

procedure TCnDockServer.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(BottomDockPanel);
  inherited SetBottomDock(Value);
end;

procedure TCnDockServer.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(TopDockPanel);
    DoFloatDockClients(BottomDockPanel);
    DoFloatDockClients(LeftDockPanel);
    DoFloatDockClients(RightDockPanel);
  end;
  inherited SetEnableDock(Value);
end;

procedure TCnDockServer.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(LeftDockPanel);
  inherited SetLeftDock(Value);
end;

procedure TCnDockServer.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(RightDockPanel);
  inherited SetRightDock(Value);
end;

procedure TCnDockServer.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(TopDockPanel);
  inherited SetTopDock(Value);
end;

procedure TCnDockServer.DoFloatDockClients(DockPanel: TCnDockPanel);
var
  i: Integer;
  ADockClient: TCnDockClient;
begin
  if not (csDesigning in ComponentState) and (DockPanel <> nil) then
  begin
    for i := DockPanel.DockClientCount - 1 downto 0 do
    begin
      ADockClient := FindDockClient(DockPanel.DockClients[i]);
      if ADockClient <> nil then
        ADockClient.RestoreChild;
    end;
  end;
end;

procedure TCnDockServer.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnDockServerName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDockServerComment;
end;

procedure TCnDockServer.Loaded;
begin
  if Assigned(DockStyle) and Assigned(DockStyle.CnDockPanelClass) then
    FDockPanelClass := DockStyle.CnDockPanelClass
  else FDockPanelClass := DefaultDockPanelClass;
  { ------------------------------------------------------------------------ }
  if Assigned(DockStyle) and Assigned(DockStyle.CnDockSplitterClass) then
    FDockSplitterClass := DockStyle.CnDockSplitterClass
  else FDockSplitterClass := DefaultDockSplitterClass;
  { ------------------------------------------------------------------------ }
  if not (csDesigning in ComponentState) then
  begin
    CreateDockPanelAndSplitter;
    SetSplitterStyle;
  end;
  inherited Loaded;
end;

procedure TCnDockServer.WMActivate(var Message: TWMActivate);
var i: Integer;
  Control: TWinControl;
begin
  if (Message.Active = WA_INACTIVE) then
  begin
    for i := 0 to 3 do
      DockPanel[i].CnDockManager.ActiveControl := nil;
  end else
  begin
    Control := GetActiveControl(ParentForm);
    for i := 0 to 3 do
    begin
      if GetHostDockParent(Control) = DockPanel[i] then
      begin
        DockPanel[i].CnDockManager.ActiveControl := Control;
        if Control.CanFocus then
          Control.SetFocus;
      end;
    end;
  end;
end;

procedure TCnDockServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TCnDockServer.DoGetClientAlignControl(Align: TAlign; var Value: Integer);
begin
  if Assigned(FOnGetClientAlignSize) then
    FOnGetClientAlignSize(Align, Value);
end;

procedure TCnDockServer.DoFinishSetDockPanelSize(DockPanel: TCnDockPanel);
begin
  if Assigned(FOnFinishSetDockPanelSize) then
    FOnFinishSetDockPanelSize(DockPanel);
end;

function TCnDockServer.GetClientAlignControl(Align: TAlign): Integer;
begin
  Result := GetClientAlignControlArea(ParentForm, Align);
  DoGetClientAlignControl(Align, Result);
end;

function TCnDockServer.GetDockPanelWithAlign(Index: TAlign): TCnDockPanel;
begin
  Result := nil;
  case Index of
    alLeft:   Result := FLeftDockPanel;
    alRight:  Result := FRightDockPanel;
    alTop:    Result := FTopDockPanel;
    alBottom: Result := FBottomDockPanel;
  end;
end;

function TCnDockServer.GetDockSplitterWithAlign(
  Index: TAlign): TCnDockSplitter;
begin
  Result := nil;
  case Index of
    alLeft:   Result := FLeftSplitter;
    alRight:  Result := FRightSplitter;
    alTop:    Result := FTopSplitter;
    alBottom: Result := FBottomSplitter;
  end;
end;

{ TCnDockClient }

constructor TCnDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentVisible := ParentForm.Visible;
  ParentForm.DragKind := dkDock;
  ParentForm.DragMode := dmAutomatic;
  ParentForm.UseDockManager := False;
  if not (ParentForm is TCnDockableForm) then
    SetDockSite(ParentForm, True);
  LRDockWidth := 100;
  TBDockHeight := 100;
  if GlobalDockClient = nil then
    GlobalDockClient := Self;
  FDirectDrag := False;
  FShowHint := True;
  FCanFloat := True;
  FRelativeServer := nil;
  FDockLevel := 0;
  EnableCloseBtn := True;
end;

destructor TCnDockClient.Destroy;
begin
  if not (ParentForm is TCnDockableForm) then
    SetDockSite(ParentForm, False);
  ParentForm.DragKind := dkDrag;
  ParentForm.DragMode := dmManual;
  inherited Destroy;
end;

procedure TCnDockClient.Assign(Source: TPersistent);
begin
  if Source is TCnDockClient then
  begin
    FConjoinPanelClass := TCnDockClient(Source).FConjoinPanelClass;
    FTabDockClass := TCnDockClient(Source).FTabDockClass;
    FParentVisible := TCnDockClient(Source).FParentVisible;
    FNCPopupMenu := TCnDockClient(Source).FNCPopupMenu;
    FDirectDrag := TCnDockClient(Source).FDirectDrag;
    FShowHint := TCnDockClient(Source).FShowHint;
    FCanFloat := TCnDockClient(Source).FCanFloat;
    FRelativeServer := TCnDockClient(Source).FRelativeServer;
    FDockLevel := TCnDockClient(Source).DockLevel;
  end;
  inherited Assign(Source);
end;

procedure TCnDockClient.WindowProc(var Message: TMessage);
var OldOrient: TDockOrientation;
begin
  { ִ��FDockStyle����Ϣ���� }
  if Assigned(FDockStyle) then
  begin
    if FDockStyle.DockClientWindowProc(Self, Message) then
      { ���DockStyle�Ѿ�ִ������Ϣ�����Ͳ�ִ��Ĭ�ϵĴ��� }
      Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      CM_SHOWINGCHANGED:
      begin
        { ��WinXP�У�����װ�ص�ʱ�򣬻������˸�����������ʾ�ı����Ϣ�������
          ��98����2000����û����������ģ���Ϊ�Ѿ�������LockWindowUpdate������
          ������XP���治֪����ʲôԭ�򣬻��ǻ����������˸�������û�취��ֻ��
          ��װ�ص�ʱ���ȹ��˵�CM_SHOWINGCHANGED��Ϣ��Ȼ����װ�ص�����Ӧ����ʾ
          ����û����ʾ�Ŀͻ�������ʾ������}
        if IsWinXP and IsLoading then
          Exit;
      end;
      WM_NCLBUTTONDOWN:
      begin
        // �������WM_NCLBUTTONDOWN��Ϣ���͵���WMNCLButtonDown������
        WMNCLButtonDown(TWMNCHitMessage(Message));
        if (Message.Result = 1) then
          Exit;
      end;
      WM_NCLBUTTONUP:
        WMNCLButtonUp(TWMNCHitMessage(Message));
      WM_NCLBUTTONDBLCLK:
        WMNCLButtonDblClk(TWMNCHitMessage(Message));
      WM_NCMBUTTONDOWN:
        WMNCMButtonDown(TWMNCHitMessage(Message));
      WM_NCMBUTTONUP:
        WMNCMButtonUp(TWMNCHitMessage(Message));
      WM_NCMBUTTONDBLCLK:
        WMNCMButtonDblClk(TWMNCHitMessage(Message));
      WM_NCRBUTTONDOWN:
      begin
        WMNCRButtonDown(TWMNCHitMessage(Message));
        { ������Զ���ĵ����˵����Ͳ�Ҫ����ϵͳĬ�ϵĵ����˵���
          Ҫʵ��������ܣ�ֻҪ���������Ϣ���͸�Windows�Ϳ��԰쵽�� }
        if FNCPopupMenu <> nil then
          Exit;
      end;
      WM_NCRBUTTONUP:
        WMNCRButtonUp(TWMNCHitMessage(Message));
      WM_NCRBUTTONDBLCLK:
        WMNCRButtonDblClk(TWMNCHitMessage(Message));
      WM_NCMOUSEMOVE:
        WMNCMOUSEMOVE(TWMNCHitMessage(Message));

      WM_SIZE:
        WMSize(TWMSize(Message));
      WM_ACTIVATE:
        WMActivate(TWMActivate(Message));
      WM_WINDOWPOSCHANGED:
      begin
        //����һ�������������ʹTControl.CalcDockSizes����ʧЧ,
        //��ο�Controls.pas�ļ��е�4462--4466��(D5�汾),����4816--4820��(D6�汾)
        ParentForm.ControlState := ParentForm.ControlState + [csDocking];
        OldOrient := ParentForm.DockOrientation;
        ParentForm.DockOrientation := doNoOrient;
        try
          inherited WindowProc(Message);
        finally
          ParentForm.ControlState := ParentForm.ControlState - [csDocking];
          ParentForm.DockOrientation := OldOrient;
        end;
        Exit;
      end;
      CM_ENTER:
        Activate;
      CM_EXIT:
        Deactivate;
      CM_VISIBLECHANGED:
        CMVisibleChanged(Message);
    end;
  end;
  inherited WindowProc(Message);
end;

{ ����ƽ�̵ķ����岢�Ұѿؼ�ͣ����ȥ }
function TCnDockClient.CreateConjoinHostAndDockControl(
  Control1, Control2: TControl; DockType: TAlign): TCnConjoinDockHostForm;
var APanel: TCnConjoinPanel;
  OldDockWidth, OldDockHeight: Integer;
begin
  { ����һ��TCnConjoinDockHostForm������ΪTCnConjoinPanel��Parent }
  Result := TCnConjoinDockHostForm.Create(Application);
  { ����һ��TCnConjoinPanel�����Ұ�������Result�� }
  APanel := CreateConjoinPanelClass(Result);
  { Result��λ�ô�С��Control1����ͬ�������ڵ���CreateConjoinHostAndDockControl
    ��ʱ��Ҫע�⣺Control1Ϊͣ��Ŀ�괰�壬Control2Ϊͣ��Դ���� }
  Result.BoundsRect := Control1.BoundsRect;
  { ����Result�Ŀ�Ⱥ͸߶�ΪUndockWidth��UndockHeight }
  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;
  { �ֱ��Control1��Control2ͣ����APage }
  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APanel, nil, alNone);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;
  {============================================================================}
  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APanel, nil, DockType);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;
  { ��Result��DockSite�������ó�False }
  SetDockSite(Result, False);
end;

{ ������ҳ�ķ����岢�Ұѿؼ�ͣ����ȥ }
function TCnDockClient.CreateTabHostAndDockControl(
  Control1, Control2: TControl): TCnTabDockHostForm;
var APage: TCnTabPageControl;
  OldDockWidth, OldDockHeight: Integer;
begin
  { ����һ��TCnTabDockHostForm������ΪTCnTabPageControl��Parent }
  Result := TCnTabDockHostForm.Create(Application);
  { ����һ��TCnTabPageControl�����Ұ�������Result�� }
  APage := CreateTabDockClass(Result);
  { Result��λ�ô�С��Control1����ͬ�������ڵ���CreateTabHostAndDockControl
    ��ʱ��Ҫע�⣺Control1Ϊͣ��Ŀ�괰�壬Control2Ϊͣ��Դ���� }
  Result.BoundsRect := Control1.BoundsRect;
  { ����Result�Ŀ�Ⱥ͸߶�ΪUndockWidth��UndockHeight }
  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;
  { �ֱ��Control1��Control2ͣ����APage }
  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APage, nil, alClient);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;
  {============================================================================}
  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APage, nil, alClient);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;
  { ��Result��DockSite�������ó�False }
  SetDockSite(Result, False);
end;

procedure TCnDockClient.MakeHideEvent;
begin
  ParentVisible := False;
  if Assigned(FOnFormHide) then
    FOnFormHide(Self);
end;

procedure TCnDockClient.MakeShowEvent;
begin
  if {(not ParentVisible) and }ParentForm.Visible then
  begin
    if Assigned(FOnFormShow) then
      FOnFormShow(Self);
    ParentVisible := True;
  end;
end;

procedure TCnDockClient.SetParentVisible(const Value: Boolean);
begin
  FParentVisible := Value;
end;

procedure TCnDockClient.DoFloatDockClients(PanelAlign: TAlign);
begin
  if (ParentForm.HostDockSite is TCnDockPanel) and
    (PanelAlign = ParentForm.HostDockSite.Align) then
    RestoreChild;
end;

procedure TCnDockClient.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alBottom);
  inherited SetBottomDock(Value);
end;

procedure TCnDockClient.SetEachotherDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockEachOther;
  inherited SetEachotherDock(Value);
end;

procedure TCnDockClient.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(alTop);
    DoFloatDockClients(alBottom);
    DoFloatDockClients(alLeft);
    DoFloatDockClients(alRight);
    DoFloatDockEachOther;
  end;
  if ParentForm <> nil then
  begin
    if Value then
      ParentForm.DragKind := dkDock
    else
      ParentForm.DragKind := dkDrag;
  end;
  inherited SetEnableDock(Value);
end;

procedure TCnDockClient.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alLeft);
  inherited SetLeftDock(Value);
end;

procedure TCnDockClient.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alRight);
  inherited SetRightDock(Value);
end;

procedure TCnDockClient.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alTop);
  inherited SetTopDock(Value);
end;

procedure TCnDockClient.DoFloatDockEachOther;
begin
  if (ParentForm.HostDockSite <> nil) and
    (ParentForm.HostDockSite.Parent is TCnDockableForm) then
    RestoreChild;
end;

procedure TCnDockClient.WMSize(var Message: TWMSize);
begin
{  if (ParentForm.HostDockSite <> nil) and (Message.Width * Message.Height <> 0) then
  begin
    if ParentForm.DockOrientation = doVertical then // ��ֱ
      ParentForm.LRDockWidth := Message.Width
    else if ParentForm.DockOrientation = doHorizontal then
      ParentForm.TBDockHeight := Message.Height
    else
    begin
      if (ParentForm.HostDockSite is TCnDockPanel) and
        (ParentForm.HostDockSite.FindChildControl(ParentForm.Name) <> nil) then
      begin
        if ParentForm.HostDockSite.Align in [alLeft, alRight] then
          ParentForm.LRDockWidth := Message.Width
        else
          ParentForm.TBDockHeight := Message.Height;
      end;
    end;
  end;}
  inherited;
end;

procedure TCnDockClient.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnDockClientName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDockClientComment;
end;

procedure TCnDockClient.Loaded;
begin
  { ����FConjoinPanelClass }
  if Assigned(DockStyle) and Assigned(DockStyle.CnConjoinPanelClass) then
    FConjoinPanelClass := DockStyle.CnConjoinPanelClass
  else FConjoinPanelClass := DefaultConjoinPanelClass;
  { ����FTabDockClass }
  if Assigned(DockStyle) and Assigned(DockStyle.CnTabDockClass) then
    FTabDockClass := DockStyle.CnTabDockClass
  else FTabDockClass := DefaultTabDockClass;
  inherited Loaded; 
end;

function TCnDockClient.CreateConjoinPanelClass(
  ConjoinHost: TForm): TCnConjoinPanel;
begin
  Result := nil;
  TCnConjoinDockHostForm(ConjoinHost).DockClient.Assign(Self);
  if (FConjoinPanelClass <> nil) and
    (FConjoinPanelClass <> TCnConjoinPanelClass(ClassType)) then
  begin
    Result := FConjoinPanelClass.Create(ConjoinHost);
    Result.Align := alClient;
    TCnConjoinDockHostForm(ConjoinHost).DockableControl := Result;
    TCnConjoinDockHostForm(ConjoinHost).Panel := Result;
    SetDockSite(Result, True);
    { ����DockStyle.AssignConjoinServerOption������һЩ��DockStyle�����õ����� }
    DockStyle.AssignConjoinServerOption(TCnConjoinDockHostForm(ConjoinHost).Panel);
  end;
end;

function TCnDockClient.CreateTabDockClass(
  TabHost: TForm): TCnTabPageControl;
begin
  Result := nil;
  TCnTabDockHostForm(TabHost).DockClient.Assign(Self);
  if (FTabDockClass <> nil) and
    (FTabDockClass <> TCnTabDockClass(ClassType)) then
  begin
    Result := FTabDockClass.Create(TabHost);
    Result.Align := alClient;
    TCnTabDockHostForm(TabHost).DockableControl := Result;
    TCnTabDockHostForm(TabHost).PageControl := Result;
    SetDockSite(Result, True);
    { ����DockStyle.AssignTabServerOption������һЩ��DockStyle�����õ����� }
    DockStyle.AssignTabServerOption(TCnTabDockHostForm(TabHost).PageControl);
  end;
end;

procedure TCnDockClient.WMActivate(var Message: TWMActivate);
begin
  if (ParentForm is TCnConjoinDockHostForm) then
  begin
    if (Message.Active = WA_INACTIVE) then
      TCnConjoinPanel(TCnConjoinDockHostForm(ParentForm).Panel).CnDockManager.ActiveControl := nil
    else
    begin
      TCnConjoinPanel(TCnConjoinDockHostForm(ParentForm).Panel).CnDockManager.ActiveControl :=
        GetActiveControl(ParentForm);
    end;
  end;
end;

procedure TCnDockClient.Activate;
begin
  if ParentForm.HostDockSite is TCnCustomDockPanel then
  begin
    TCnCustomDockPanel(
      ParentForm.HostDockSite).CnDockManager.ActiveControl := ParentForm;
  end;
end;

procedure TCnDockClient.Deactivate;
begin
  if ParentForm.HostDockSite is TCnCustomDockPanel then
  begin
    if TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager <> nil then
      TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager.ActiveControl := nil;
  end;
end;

procedure TCnDockClient.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action = caHide then
  begin
    HideDockForm(ParentForm);
    FParentVisible := True;
  end;
end;

procedure TCnDockClient.FormDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormDockDrop(Self, Source, X, Y);
end;

procedure TCnDockClient.FormDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  FormPositionDockRect(Source);
  if Assigned(DockStyle) then
    DockStyle.FormDockOver(Self, Source, X, Y, State, Accept);
end;

procedure TCnDockClient.FormEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormEndDock(Self, Target, X, Y);
end;

procedure TCnDockClient.FormPositionDockRect(Source: TCnDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormPositionDockRect(Self, Source);
end;

procedure TCnDockClient.FormStartDock(var Source: TCnDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormStartDock(Self, Source);
end;

function TCnDockClient.FormUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if Assigned(DockStyle) then
    Result := DockStyle.FormUnDock(Self, Newtarget, Client)
  else Result := False;
end;

procedure TCnDockClient.FormGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(ParentForm.Handle, InfluenceRect);
  InflateRect(InfluenceRect, -4, -4);
  if Assigned(DockStyle) then
    DockStyle.FormGetSiteInfo(Source, Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TCnDockClient.GetLRDockWidth: Integer;
begin
  Result := ParentForm.LRDockWidth;
end;

function TCnDockClient.GetTBDockHeight: Integer;
begin
  Result := ParentForm.TBDockHeight;
end;

procedure TCnDockClient.SetLRDockWidth(const Value: Integer);
begin
  if ParentForm.LRDockWidth <> Value then
    ParentForm.LRDockWidth := Value;
end;

procedure TCnDockClient.SetTBDockHeight(const Value: Integer);
begin
  if ParentForm.TBDockHeight <> Value then
    ParentForm.TBDockHeight := Value;
end;

procedure TCnDockClient.DoNCButtonDown(Message: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonDown) then
    FOnNCButtonDown(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.DoNCButtonUp(Message: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonUp) then
    FOnNCButtonUp(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
  if Button = mbRight then
    { ���������Ҽ����͵����˵� }
    DoMenuPopup(Message.XCursor, Message.YCursor);
end;

procedure TCnDockClient.DoNCMouseMove(Message: TWMNCHitMessage;
  MouseStation: TMouseStation);
begin
  if Assigned(FOnNCMouseMove) then
    FOnNCMouseMove(Self, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.DoNCButtonDblClk(Message: TWMNCHitMessage; Button: TMouseButton;
  MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonDblClk) then
    FOnNCButtonDblClk(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.WMNCLButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbLeft, msFloat);

  GlobalDockClient := Self;
  
  if (Message.HitTest = HTCAPTION) and (ParentForm.DragKind = dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(ParentForm.Handle) then
  begin
      { Activate window since we override WM_NCLBUTTON behavior }
      SetWindowPos(ParentForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or
        SWP_NOSIZE);
      PostMessage(ParentForm.Handle, WM_NCLBUTTONUP, TMessage(Message).WParam,
        TMessage(Message).LParam);
      if ParentForm.Active then
        CnGlobalDockPresident.BeginDrag(ParentForm, DirectDrag, Integer(DirectDrag) * 2 - 1);
      Message.Result := 1;
  end else
    Message.Result := 0;
end;

procedure TCnDockClient.WMNCLButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbLeft, msFloat);
end;

procedure TCnDockClient.WMNCMButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCMButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCRButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbRight, msFloat);
end;

procedure TCnDockClient.WMNCRButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbRight, msFloat);
end;

procedure TCnDockClient.WMNCMOUSEMOVE(var Message: TWMNCHitMessage);
begin
  DoNCMouseMove(Message, msFloat);
end;

procedure TCnDockClient.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbLeft, msFloat);
end;

procedure TCnDockClient.WMNCMButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCRButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbRight, msFloat);
end;

procedure TCnDockClient.SetNCPopupMenu(const Value: TPopupMenu);
begin
  FNCPopupMenu := Value;
end;

procedure TCnDockClient.DoMenuPopup(X, Y: Integer);
begin
  if FNCPopupMenu <> nil then
  begin
    FNCPopupMenu.PopupComponent := ParentForm;
    FNCPopupMenu.Popup(X, Y);
  end;
end;

procedure TCnDockClient.DoPaintDockGrabber(
  Canvas: TCanvas; Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockGrabber) then
    FOnPaintDockGrabber(Canvas, Control, ARect);
end;

procedure TCnDockClient.DoPaintDockSplitter(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockSplitter) then
    FOnPaintDockSplitter(Canvas, Control, ARect);
end;

procedure TCnDockClient.FormGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  if Assigned(DockStyle) then
    DockStyle.FormGetDockEdge(Self, Source, MousePos, DropAlign)
  else DropAlign := alNone;
end;

procedure TCnDockClient.DoFormShowHint(HTFlag: Integer; var HintStr: string;
  var CanShow: Boolean);
begin
  if Assigned(FOnFormShowHint) then
   FOnFormShowHint(HTFlag, HintStr, CanShow);
end;

procedure TCnDockClient.SetCurrentDockSite(const Value: TWinControl);
begin
  FCurrentDockSite := Value;
end;

procedure TCnDockClient.SetLastDockSite(const Value: TWinControl);
begin
  FLastDockSite := Value;
end;

procedure TCnDockClient.SetVSPaneWidth(const Value: Integer);
begin
  FVSPaneWidth := Value;
end;

procedure TCnDockClient.RestoreChild;
begin
  DockStyle.RestoreClient(Self);
end;

procedure TCnDockClient.SetUnDockLeft(const Value: Integer);
begin
  FUnDockLeft := Value;
end;

procedure TCnDockClient.SetUnDockTop(const Value: Integer);
begin
  FUnDockTop := Value;
end;

procedure TCnDockClient.HideParentForm;
begin
  HideDockForm(ParentForm);
end;

procedure TCnDockClient.ShowParentForm;
begin
  ShowDockForm(ParentForm);
end;

function TCnDockClient.GetDockState: Integer;
begin
  Result := DS_Unknow;
  if DockStyle <> nil then
    Result := DockStyle.GetDockState(Self);
end;

procedure TCnDockClient.CMVisibleChanged(var Message: TMessage);
begin
end;

procedure TCnDockClient.SetCanFloat(const Value: Boolean);
begin
  FCanFloat := Value;
end;

procedure TCnDockClient.SetRelativeServer(const Value: TCnDockServer);
begin
  if (csDesigning in ComponentState){ or (csLoading in ComponentState) }then
  begin
    if Value <> nil then
      Value.FreeNotification(ParentForm);
  end;
  FRelativeServer := Value;
end;

procedure TCnDockClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRelativeServer) then
    // ������ͷţ���Ҫ��FRelativeServer�������nil��
    FRelativeServer := nil;
end;

procedure TCnDockClient.SetDockLevel(const Value: Integer);
begin
  if not ParentForm.Floating then
  begin
    if FDockLevel <> Value then
      DoFloatForm(ParentForm);
  end;
  FDockLevel := Value;
end;

procedure TCnDockClient.SetEnableCloseBtn(const Value: Boolean);
begin
  FEnableCloseBtn := Value;
end;

{ TCnDockableForm }

constructor TCnDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  FDockClient := TCnDockClient.Create(self);
  CnGlobalDockPresident.DockableFormList.Add(Self);
  FFloatingChild := nil;
  TBDockHeight := FDockClient.TBDockHeight;
  LRDockWidth := FDockClient.LRDockWidth;
end;

destructor TCnDockableForm.Destroy;
var Index: Integer;
begin
  if CnGlobalDockPresident <> nil then
  begin
    Index := CnGlobalDockPresident.DockableFormList.IndexOf(Self);
    if Index <> -1 then
      CnGlobalDockPresident.DockableFormList.Delete(Index);
  end;
  if DockClient.LastDockSite is TCnDockPanel then
    TCnDockPanel(DockClient.LastDockSite).CnDockManager.RemoveControl(Self);
  inherited Destroy;
  FFloatingChild := nil;
end;

procedure TCnDockableForm.DoClose(var Action: TCloseAction);
var i: Integer;
begin
  if DockableControl.DockClientCount = 1 then
  begin
    { ��������ͻ�,�����Ժ�Ҫ����ͣ�������������� }
    FFloatingChild := DockableControl.DockClients[0];
    { ���HostDockSite��Ϊ��,˵���Ժ�Ҫ����ͣ��������������,Ϊ�˷�ֹ��˸,�Ȱ�
      FFloatingChild���ص�,Ȼ��ȵ�����ͣ��������ʾ����,�Ϳ��Է�ֹ��˸,��ʾ��
      ������CnDockSupportControl.pas��TCnCustomDockPanel.WndProc���� }
    if HostDockSite <> nil then
      FFloatingChild.Visible := False;
    { ���ͣ���ͻ��ĸ���Ϊ1���Ͱ����ϳ��������ͷű��� }
    DoFloat(Self, DockableControl.DockClients[0]);
    Action := caFree;
  end else if DockableControl.DockClientCount = 0 then
    { ����Ѿ�û��ͣ���ͻ����ͰѴ����ͷŵ� }
    Action := caFree
  else
  begin
    { ��������� }
    Action := caHide;
    { ������ҵ�DockableControl�еĿͻ���һ����FUnDockControl���Ͳ������Լ� }
    if (FUnDockControl <> nil) and (DockableControl.DockClientCount = 2) then
    begin
      for i := 0 to DockableControl.DockClientCount - 1 do
        if FUnDockControl = DockableControl.DockClients[i] then
        begin
          Action := caNone;
          Break;
        end;
    end;
  end;
  if (HostDockSite is TCnDockPanel) and (HostDockSite.VisibleDockClientCount = 1) and
    (FFloatingChild = nil) then
    // ���ͣ����������TDockServer�е�DockPanel,�͵���ShowDockPanel������
    TCnDockPanel(HostDockSite).ShowDockPanel(False, Self);
    
  inherited DoClose(Action);
  FUnDockControl := nil;
end;

function TCnDockableForm.GetDockableControl: TWinControl;
begin
  Result := FDockableControl;
end;

procedure TCnDockableForm.SetDockableControl(const Value: TWinControl);
begin
  FDockableControl := Value;
end;

procedure TCnDockableForm.SetUnDockControl(const Value: TControl);
begin
  FUnDockControl := Value;
end;

{ TCnConjoinDockHostForm }

constructor TCnConjoinDockHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := ConjoinDockHostBorderStyle;
end;

procedure TCnConjoinDockHostForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
end;

{ TCnTabDockHostForm }

constructor TCnTabDockHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := TabDockHostBorderStyle;
end;

destructor TCnTabDockHostForm.Destroy;
begin
  inherited;
end;

function TCnTabDockHostForm.GetActiveDockForm: TForm;
begin
  if PageControl.ActivePage.ControlCount = 1 then
    Result := TForm(PageControl.ActivePage.Controls[0])
  else Result := nil;
end;

{ TCnConjoinPanel }

constructor TCnConjoinPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := TCnConjoinDockHostForm(AOwner).DockClient;
  Align := alClient;
  BevelOuter := bvNone;
  DoubleBuffered := True;
  ParentFont := False;
  Caption := '';
end;

procedure TCnConjoinPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      { �������д�����Ϊ�˷�ֹ����ı������ʹ�������Ŀؼ������ݱ���� }
      ParentForm.ActiveControl := nil;
      TForm(Source.Control).ActiveControl := nil;
      { ------------------------------------------------------------------------ }
      SetDockSite(TForm(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

procedure TCnConjoinPanel.DoDockOver(Source: TDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  { ���ͣ���ͻ���TDockableForm������ͣ���ͻ�������TDockClient�ؼ�����ͬ��ͣ�� }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

function TCnConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;
  { ���ͣ���ͻ���TDockableForm���ͻ�ԭͣ���ͻ���DockSite����ΪTrue }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2){ and (NewTarget <> Self) }then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TCnConjoinPanel.GetParentForm: TCnConjoinDockHostForm;
begin
  Result := TCnConjoinDockHostForm(Parent);
end;

procedure TCnConjoinPanel.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client);
  if CanDock then
  begin
    GetWindowRect(Handle, InfluenceRect);
  end;
end;

procedure TCnConjoinPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    { ���������Ϊ�˷�ֹ����ı������ʹ�������Ŀؼ������ݱ���� }
    ParentForm.ActiveControl := nil;
    if TForm(Source.Control).FormStyle = fsStayOnTop then
      TForm(Parent).FormStyle := fsStayOnTop;
  end;
  UpdateCaption(nil);
end;

procedure TCnConjoinPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  { ���ͣ���ͻ���TDockableForm������ͣ���ͻ�������TDockClient�ؼ�����ͬ��ͣ�� }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnConjoinPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);;
end;

procedure TCnConjoinPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
end;

procedure TCnConjoinPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TCnConjoinPanel.CustomStartDock(var Source: TCnDragDockObject);
begin
  ParentForm.FUnDockControl := nil;
  inherited CustomStartDock(Source);
end;

function TCnConjoinPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;
  { ���ͣ���ͻ���TDockableForm���ͻ�ԭͣ���ͻ���DockSite����ΪTrue }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if ((VisibleDockClientCount = 1) or
    (DockClientCount <= 2)) and (NewTarget <> ParentForm.DockableControl) then
  begin
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  end;
  UpdateCaption(Client);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TCnConjoinPanel.ReloadDockedControl(
  const AControlName: string; var AControl: TControl);
begin
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnConjoinPanel.CMUnDockClient(var Message: TCMUnDockClient);
begin
  inherited;
  { ���������ͣ���ͻ�����Ҫ�ͷ�ParentForm }
  if (DockClientCount = 2) and (VisibleDockClientCount = 1) then
    PostMessage(ParentForm.Handle, WM_CLOSE, 0, 0);
  if VisibleDockClientCount <= 2 then
    CnDockFormControl.UpdateCaption(Self, Message.Client);
  if UseDockManager and (CnDockManager <> nil) then
    CnDockManager.ResetBounds(True);
end;

{ TCnTabPageControl }

procedure TCnTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  case TabPosition of
    tpLeft:
      Inc(Rect.Left, 2);
    tpRight:
      Dec(Rect.Right, 2);
    tpBottom:
    begin
      Dec(Rect.Top, 1);
      Dec(Rect.Bottom, 2);
    end;
  end;
end;

constructor TCnTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := FindDockClient(Parent);
  SetDockSite(Self, True);
  PopupMenu := DockPageControlPopupMenu;
  HotTrack := DockPageControlHotTrack;
  DoubleBuffered := True;
  Caption := '';
  FVersion := $00040000;
end;

var TabPageStreamEndFlag: Integer = -10;

destructor TCnTabPageControl.Destroy;
begin
  SetDockSite(Self, False);
  inherited Destroy;
end;

procedure TCnTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      { ����һ�д�����Ϊ�˷�ֹ����ı������ʹ�������Ŀؼ������ݱ���� }
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TWinControl(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

function TCnTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  { ���ͣ���ͻ���TDockableForm���ͻ�ԭͣ���ͻ���DockSite����ΪTrue }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2){ and (NewTarget <> Self) }then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TCnTabPageControl.GetParentForm: TCnTabDockHostForm;
begin
  Result := TCnTabDockHostForm(Parent);
end;

procedure TCnTabPageControl.LoadFromStream(Stream: TStream);
var i, Count, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  AControl: TControl;
begin
  try
    Stream.Read(i, Sizeof(i));
    { �����ж�����ǩ�ĸ������������δ����ж������ǵ���Ϣ }
    Stream.Read(Count, Sizeof(Count));
    for i := 0 to Count - 1 do
    begin
      ControlName := '';
      { �������ֵĳ��� }
      Stream.Read(NameLen, SizeOf(NameLen));
      if NameLen > 0 then
      begin
        { �������ֵ��ַ��� }
        SetLength(ControlName, NameLen);
        Stream.Read(Pointer(ControlName)^, NameLen * SizeOf(Char));
      end;
      if ControlName <> '' then
      begin
        { �ҵ�����ΪControlName��ͣ�����壬���Ұ���ͣ����TCnTabPageControl }
        ReloadDockedControl(ControlName, AControl);
        if AControl <> nil then
          AControl.ManualDock(Self, nil, alClient);
      end;
      { ������ǩ��Visible }
      Stream.Read(SheetVisible, SizeOf(SheetVisible));
      DockClients[i].Visible := Boolean(SheetVisible);
    end;
    { ����������ı�ǩ }
    Stream.Read(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
    ActivePageIndex := ActiveSheetIndex;
  finally

  end;
end;

procedure TCnTabPageControl.SaveToStream(Stream: TStream);
var i, Count, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  CurrentControl: TControl;
begin
  { д�����İ汾 }
  Stream.Write(FVersion, SizeOf(FVersion));
  Count := PageCount;
  { д��TabSheet�ĸ��� }
  Stream.Write(Count, SizeOf(Count));
  for i := 0 to PageCount - 1 do
  begin
    CurrentControl := Pages[i].Controls[0];
    ControlName := CurrentControl.Name;
    NameLen := Length(ControlName);
    { д�����ֵĳ��� }
    Stream.Write(NameLen, SizeOf(NameLen));
    { д������ }
    if NameLen > 0 then Stream.Write(Pointer(ControlName)^, NameLen * SizeOf(Char));
    SheetVisible := 0;
    if (Self is TCnVSNETTabPageControl) and (ParentForm.HostDockSite is TCnDockPanel) then
      SheetVisible := Integer(TCnVSNETDockTabSheet(Pages[i]).OldVisible)
    else
      SheetVisible := SheetVisible + Integer(CurrentControl.Visible);
    { д���Ƿ�ɼ� }
    Stream.Write(SheetVisible, SizeOf(SheetVisible));
  end;
  ActiveSheetIndex := ActivePageIndex;
  { д�뼤���TabSheet������ }
  Stream.Write(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
  { д�������־ }
  Stream.Write(TabPageStreamEndFlag, SizeOf(TabPageStreamEndFlag));
end;

procedure TCnTabPageControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
var DragDockObject: TDragDockObject;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    DragDockObject := TDragDockObject.Create(Source.Control);
    try
      DragDockObject.DockRect := Source.DockRect;
      DragDockObject.Control := Source.Control;
      Perform(CM_DOCKCLIENT, Integer(DragDockObject), Integer(SmallPoint(X, Y)));
      UpdateCaption(nil);
    finally
      DragDockObject.Free;
    end;
  end else
  begin
    inherited CustomDockDrop(Source, X, Y);
  end;
  if Source.Control is TForm then
  begin
    { ����һ�д�����Ϊ�˷�ֹ����ı������ʹ�������Ŀؼ������ݱ���� }
    TForm(Source.Control).ActiveControl := nil;
    { ------------------------------------------------------------------------ }
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TCnTabPageControl.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var ARect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  { ���ͣ���ͻ�������TDockClient�ؼ�����ͬ��ͣ�� }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  // ��ͣ���ͻ���Ԥ��ͣ��λ��
  if Accept then
  begin
    ComputeDockingRect(self, ARect,
      Point(ClientWidth div 2, ClientHeight div 2));
    Source.DockRect := ARect;
  end;
end;

procedure TCnTabPageControl.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TCnTabPageControl.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnTabPageControl.CustomPositionDockRect(
  Source: TCnDragDockObject; X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TCnTabPageControl.CustomStartDock(var Source: TCnDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TCnTabPageControl.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  { ���ͣ���ͻ���TDockableForm���ͻ�ԭͣ���ͻ���DockSite����ΪTrue }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

procedure TCnTabPageControl.ReloadDockedControl(
  const AControlName: string; var AControl: TControl);
begin
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnTabPageControl.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var ARect: TRect;
begin
  ARect := Source.DockRect;
  DropAlign := ComputeDockingRect(Source.Control, ARect, MousePos);
end;

{ TCnBasicDockStyle }

constructor TCnBasicDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { ����һЩĬ�ϵ�������� }
  CnDockPanelClass := DefaultDockPanelClass;
  CnDockSplitterClass := DefaultDockSplitterClass;
  CnConjoinPanelClass := DefaultConjoinPanelClass;
  CnTabDockClass := DefaultTabDockClass;
  CnDockPanelTreeClass := DefaultDockTreeClass;
  CnDockPanelZoneClass := DefaultDockZoneClass;
  CnConjoinPanelTreeClass := DefaultDockTreeClass;
  CnConjoinPanelZoneClass := DefaultDockZoneClass;
  FCnConjoinServerOptionClass := TCnBasicConjoinServerOption;
  FCnTabServerOptionClass := TCnBasicTabServerOption;
  FDockBaseControlList := TList.Create;
  if AOwner is TCustomForm then
    FParentForm := TForm(AOwner)
  else FParentForm := nil;

  if not (csDesigning in ComponentState) then
  begin
    { �����ϵĴ��ڹ��� }
    FOldWindowProc := FParentForm.WindowProc;
    { ���ش��ڹ��� }
    FParentForm.WindowProc := ParentFormWindowProc;
  end;
end;

function TCnBasicDockStyle.GetControlName: string;
begin
  Result := gs_CnDockStyleName;
end;

function TCnBasicDockStyle.GetDockStyleVersion: string;
begin
  Result := gs_CnDockStyleVersion;
end;

procedure TCnBasicDockStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TCnDockBaseControl);
begin
  { û������ }
end;

procedure TCnBasicDockStyle.FormDockDrop(DockClient: TCnDockClient; Source: TCnDragDockObject; X,
  Y: Integer);
var
  ARect,DRect: TRect;
  DockType: TAlign;
  Host: TCustomForm;
  APanelDock: TWinControl;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;
    { ��סWindows���� }
    if not IsLoading then
      Cn_LockWindow(nil);
    try
      // ����ComputeDockingRect����֪��ͣ��������
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));

        if (ParentForm.HostDockSite is TCnDockPanel) then
        begin
          // ���ͣ����������TDockPanel����ͣ����TDockServer��DockPanel�ϡ�
          if DockType = alClient then
          begin
            // ���ͣ��������alClient
            if Source.Control is TCnTabDockHostForm then
            begin
              // ���ͣ���ͻ���TCnTabDockHostForm��
              // ���Ȱ�Parentͣ����TCnTabDockHostForm�У�
              // �ٰ�TCnTabDockHostFormͣ����TCnDockPanel�С�
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
              TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end else
            begin
              // ����ʹ���TCnTabDockHostForm��
              // �Ѱ�Parentͣ����TCnTabDockHostForm�У�
              // �ٰ�TCnTabDockHostFormͣ����TCnDockPanel�С�
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            // ���ͣ�����Ͳ���alClient,
            // �Ͱ�ͣ������ͣ����TCnDockPanel.
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        // ������ҳ�ķ�����
        if DockType = alClient then
        begin
          Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end
        // ����ƽ�̵ķ�����
        else if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      { ����Windows���� }
      if not IsLoading then
        Cn_UnLockWindow;
    end;
  end;
end;

procedure TCnBasicDockStyle.FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm.HostDockSite, Source.Control, Source.DropOnControl, Source.DropAlign);
    if Accept and (State = dsDragMove) and
      (ComputeDockingRect(ParentForm, ARect, Point(X, Y)) <> alNone) then
      Source.DockRect := ARect;
  end;
end;

procedure TCnBasicDockStyle.FormEndDock(DockClient: TCnDockClient; Target: TObject; X, Y: Integer);
begin
  { û���� }
end;

procedure TCnBasicDockStyle.FormPositionDockRect(DockClient: TCnDockClient;
  Source: TCnDragDockObject);
var
  NewWidth, NewHeight: Integer;
  TempX, TempY: Double;
begin
  with Source do
  begin
    if (DragTarget = nil) or (not TCnWinControlAccess(DragTarget).UseDockManager) then
    begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;
      // Drag position for dock rect is scaled relative to control's click point.
      TempX := DragPos.X - ((NewWidth) * MouseDeltaX);
      TempY := DragPos.Y - ((NewHeight) * MouseDeltaY);
      with DockRect do
      begin
        Left := Round(TempX);
        Top := Round(TempY);
        Right := Left + NewWidth;
        Bottom := Top + NewHeight;
      end;
      { Allow DragDockObject final say on this new dock rect }
      AdjustDockRect(DockRect);
    end
    else begin
      GetWindowRect(TargetControl.Handle, DockRect);
      if TCnWinControlAccess(DragTarget).UseDockManager then
      begin
        if TargetControl is TCnCustomDockPanel then
        begin
          if (TCnCustomDockPanel(DragTarget).CnDockManager <> nil) then
            TCnCustomDockPanel(DragTarget).CnDockManager.PositionDockRect(Control,
              DropOnControl, DropAlign, DockRect);
        end;
      end;
    end;
  end;
end;

procedure TCnBasicDockStyle.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
// ���಻��
end;

procedure TCnBasicDockStyle.FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject);
begin
  { û���� }
end;

function TCnBasicDockStyle.FormUnDock(DockClient: TCnDockClient; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := True;
end;

procedure TCnBasicDockStyle.FormGetSiteInfo(Source: TCnDragDockObject;
  DockClient: TCnDockClient;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  with DockClient do
    CanDock := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
end;

function TCnBasicDockStyle.DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean;
begin
  { Ĭ��Ҫִ��ԭ������Ϣ���� }
  Result := False;
end;

procedure TCnBasicDockStyle.FormGetDockEdge(DockClient: TCnDockClient;
  Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := TCnControlAccess(DockClient.ParentForm).GetDockEdge(MousePos);
end;

procedure TCnBasicDockStyle.SetConjoinServerOption(
  const Value: TCnBasicConjoinServerOption);
begin
  FCnConjoinServerOption.Assign(Value);
end;

procedure TCnBasicDockStyle.SetTabServerOption(
  const Value: TCnBasicTabServerOption);
begin
  FCnTabServerOption.Assign(Value);
end;

procedure TCnBasicDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnBasicConjoinServerOption.Create(Self);
end;

procedure TCnBasicDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnBasicTabServerOption.Create(Self);
end;

procedure TCnBasicDockStyle.CreateServerOption;
begin
  if FCnConjoinServerOption = nil then
    FCnConjoinServerOption := FCnConjoinServerOptionClass.Create(Self);
  if FCnTabServerOption = nil then
    FCnTabServerOption := FCnTabServerOptionClass.Create(Self);
end;

function TCnBasicDockStyle.GetConjoinServerOption: TCnBasicConjoinServerOption;
begin
  Result := FCnConjoinServerOption;
end;

function TCnBasicDockStyle.GetTabServerOption: TCnBasicTabServerOption;
begin
  Result := FCnTabServerOption;
end;

procedure TCnBasicDockStyle.FreeServerOption;
begin
  if FCnConjoinServerOption <> nil then
    FCnConjoinServerOption.Free;
  if FCnTabServerOption <> nil then
    FCnTabServerOption.Free;
end;

destructor TCnBasicDockStyle.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if @FOldWindowProc <> nil then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
  end;
  FDockBaseControlList.Free;
  FreeServerOption;
  inherited Destroy;
end;

procedure TCnBasicDockStyle.AssignConjoinServerOption(
  APanel: TCnCustomDockPanel);
begin
  APanel.CnDockManager.GrabberSize := ConjoinServerOption.GrabbersSize;
  APanel.CnDockManager.SplitterWidth := ConjoinServerOption.SplitterWidth;
end;

procedure TCnBasicDockStyle.AssignTabServerOption(
  APage: TCnTabPageControl);
begin
  APage.HotTrack := TabServerOption.HotTrack;
  APage.TabPosition := TabServerOption.TabPosition;
end;

procedure TCnBasicDockStyle.Loaded;
begin
  inherited;
end;

procedure TCnBasicDockStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateServerOption;
end;

procedure TCnBasicDockStyle.ParentFormWindowProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Message);
  end;
end;

procedure TCnBasicDockStyle.AddDockBaseControl(
  ADockBaseControl: TCnDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  if FDockBaseControlList.IndexOf(ADockBaseControl) = -1 then
  begin
    FDockBaseControlList.Add(ADockBaseControl);
    ConjoinServerOption.ResetDockControlOption;
    TabServerOption.ResetDockControlOption;
  end;
end;

procedure TCnBasicDockStyle.RemoveDockBaseControl(
  ADockBaseControl: TCnDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  FDockBaseControlList.Remove(ADockBaseControl);
end;

procedure TCnBasicDockStyle.ResetDockControlConjoinOption;
begin

end;

procedure TCnBasicDockStyle.ResetDockControlTabOption;
begin

end;

function TCnBasicDockStyle.GetDockBaseControlListCount: Integer;
begin
  Result := FDockBaseControlList.Count;
end;

function TCnBasicDockStyle.GetDockBaseControlLists(
  Index: Integer): TCnDockBaseControl;
begin
  Result := FDockBaseControlList[Index];
end;

function TCnBasicDockStyle.DockServerWindowProc(DockServer: TCnDockServer;
  var Message: TMessage): Boolean;
begin
  { Ĭ��Ҫִ��ԭ������Ϣ���� }
  Result := False;
end;

function TCnBasicDockStyle.GetDockState(
  DockClient: TCnDockClient): Integer;
begin
  Result := DS_Unknow;
  if (DockClient <> nil) and (DockClient.ParentForm <> nil) then
  begin
    if DockClient.ParentForm.Floating then
      Result := DS_Floating
    else Result := DS_Docking;
  end; 
end;

function TCnBasicDockStyle.CanSetBottomDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetEnableDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetLeftDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetRightDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetTopDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

procedure TCnBasicDockStyle.ResetCursor(Source: TCnDragDockObject);
begin
  if (Source.TargetControl = nil) and (Source.Control <> nil) and (Source.Control.Floating) then
    Windows.SetCursor(Screen.Cursors[crDefault])
  else if (Source.TargetControl = nil) and (not GlobalDockClient.CanFloat) then
    Windows.SetCursor(Screen.Cursors[crNo])
  else
    Windows.SetCursor(Screen.Cursors[crDefault]);
end;

procedure TCnBasicDockStyle.HideDockForm(ADockClient: TCnDockClient);
begin
  { ���ô����ϵ�TCnDockClient��MakeHideEvent���� }
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := False;
    ADockClient.MakeHideEvent;
  end;
end;

procedure TCnBasicDockStyle.ShowDockForm(ADockClient: TCnDockClient);
begin
  { ���ô����ϵ�TCnDockClient��MakeShowEvent���� }
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := True;
    ADockClient.MakeShowEvent;
  end;
end;

function TCnBasicDockStyle.GetDockFormVisible(ADockClient: TCnDockClient): Boolean;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if ADockClient.ParentForm.Visible then
    begin
      if ADockClient.ParentForm.HostDockSite <> nil then
      begin
        if ADockClient.ParentForm.HostDockSite is TCnDockPanel then
          Result := ADockClient.ParentForm.HostDockSite.Width * ADockClient.ParentForm.HostDockSite.Height > 0
        else
          Result := GetFormVisible(ADockClient.ParentForm.HostDockSite.Parent);
      end;
    end else Result := False;
  end;
end;

procedure TCnBasicDockStyle.RestoreClient(DockClient: TCnDockClient);
var TmpLastDockSite: TWinControl;
  TmpUnDockLeft, TmpUnDockTop: Integer;
  i: Integer;
  ADockClient: TCnDockClient;
  ADockServer: TCnDockServer;
  ARect: TRect;

  procedure DoFloatParentForm;
  begin
    with DockClient do
    begin
      if (ParentForm.HostDockSite <> nil) then
      begin
        ARect := Bounds(TmpUnDockLeft, TmpUnDockTop, ParentForm.UndockWidth, ParentForm.UndockHeight);
        ParentForm.ManualFloat(ARect);
        if (ParentForm.Left <> ARect.Left) or (ParentForm.Top <> ARect.Top) then
        begin
          ParentForm.Left := ARect.Left;
          ParentForm.Top := ARect.Top;
        end;
      end;
    end;
  end;

begin
  if DockClient = nil then Exit;
  if not DockClient.CanFloat then Exit;
  with DockClient do
  begin
    { �������ܹ�����ͣ�������Ĳ��� }
    if not EnableDock then Exit;
    if LastDockSite is TCnDockPanel then
    begin
      with TCnDockPanel(LastDockSite) do
      begin
        { ����ͣ���ͻ� }
        if ((not LeftDock) and (Align = alLeft)) or
          ((not RightDock) and (Align = alRight)) or
          ((not TopDock) and (Align = alTop)) or
          ((not BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;

        { ����ͣ�������� }
        ADockServer := DockServer;
        if ADockServer <> nil then
          if (not ADockServer.EnableDock) or
            ((not ADockServer.LeftDock) and (Align = alLeft)) or
            ((not ADockServer.RightDock) and (Align = alRight)) or
            ((not ADockServer.TopDock) and (Align = alTop)) or
            ((not ADockServer.BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;
      end;
    end;

    if ParentForm is TCnConjoinDockHostForm then
    begin
      with TCnConjoinDockHostForm(ParentForm).Panel do
      begin
        for i := DockClientCount - 1 downto 0 do
        begin
          ADockClient := FindDockClient(DockClients[i]);
          if (ADockClient <> nil) and (ADockClient.LastDockSite is TCnDockPanel) then
            ADockClient.RestoreChild;
        end;
      end;
      Exit;
    end;

    { ����ԭ�ȵ�LastDockSite����Ϊ�ڵ���һЩ������Ḳ�ǵ�ԭ�ȵ�LastDockSite }
    TmpLastDockSite := LastDockSite;
    TmpUnDockLeft := UnDockLeft;
    TmpUnDockTop := UnDockTop;

    { ��������DockClient��һЩ���� }
    ResetDockClient(DockClient, nil);

    DoFloatParentForm;

    if TmpLastDockSite is TCnDockPanel then
    begin
      with TCnDockPanel(TmpLastDockSite) do
      begin
        if UseDockManager and (CnDockManager <> nil) then
        begin
          if not CnDockManager.HasZoneWithControl(ParentForm) then Exit;
          DisableAlign;
          try
            { ����ParentForm��Dock������ParentFormͣ����LastDockSite�� }
            ParentForm.Dock(TmpLastDockSite, Rect(0,0,0,0));
            { ������LastDockSite��CnDockManager�ӿ����Ѿ���һ��ParentForm��Zone��
              ����ֻҪ����show�����Ϳ����� }
            CnDockManager.ShowControl(ParentForm);
            { ����һ�д�����Ϊ�˷�ֹ����ı������ʹ�������Ŀؼ������ݱ���� }
            ParentForm.ActiveControl := nil;
            SetDockSite(ParentForm, False);
            { ʹParentForm��ý��� }
            if ParentForm.Visible and ParentForm.CanFocus then
              ParentForm.SetFocus;
            ShowDockPanel(True, ParentForm, sdfDockPanel);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
  end;
end;

{ TCnAdvDockStyle }

function TCnAdvDockStyle.DockClientWindowProc(DockClient: TCnDockClient;
  var Message: TMessage): Boolean;
begin
  if (DockClient <> nil) and (Message.Msg = WM_NCLBUTTONDBLCLK) then
    if DockClient.CanFloat then
      DockClient.RestoreChild;
  Result := inherited DockClientWindowProc(DockClient, Message);
end;

{ TCnDockSplitter }

constructor TCnDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSnap := False;
end;

function TCnDockSplitter.FindControl: TControl;
begin
  if DockServer <> nil then
    Result := DockServer.GetDockPanelWithAlign(Align)
  else
    Result := inherited FindControl;
end;

function TCnDockSplitter.GetSplitterIndex: Integer;
begin
  case Align of
    alTop:    Result := 0;
    alBottom: Result := 1;
    alLeft:   Result := 2;
    alRight:  Result := 3;
  else
    Result := -1;
  end;
end;

{ TCnBasicServerOption }

procedure TCnBasicServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicServerOption then
  begin

  end;
  inherited Assign(Source);
end;

constructor TCnBasicServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  FDockStyle := ADockStyle;
end;

destructor TCnBasicServerOption.Destroy;
begin

  inherited Destroy;
end;

procedure TCnBasicServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
begin
// �������������һ�����麯�������ǲ�֪����ô���±���Ҫ����ֻ�øĳ�һ����麯��
end;

procedure TCnBasicServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
begin
// �������������һ�����麯�������ǲ�֪����ô���±���Ҫ����ֻ�øĳ�һ����麯��
end;

{ TCnBasicTabServerOption }

procedure TCnBasicTabServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicTabServerOption then
  begin
    FTabPosition := TCnBasicTabServerOption(Source).FTabPosition;
    FHotTrack := TCnBasicTabServerOption(Source).FHotTrack;
  end;
  inherited Assign(Source);

end;

constructor TCnBasicTabServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  FHotTrack := False;
  FTabPosition := tpTop;
end;

destructor TCnBasicTabServerOption.Destroy;
begin

  inherited Destroy;
end;

procedure TCnBasicTabServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
var PageControl: TCnTabPageControl;
begin
  if ADockClient = nil then Exit;
  { �õ����ADockClient���ڵĴ����PageControl }
  PageControl := TCnTabPageControl(TCnTabDockHostForm(ADockClient.ParentForm).PageControl);
  ResetTabPageControl(PageControl);
  if PageControl <> nil then
    PageControl.Invalidate;
end;

procedure TCnBasicTabServerOption.ResetDockControlOption;
var i: Integer;
  ADockClient: TCnDockClient;
begin
  { ѭ��DockStyle��DockBaseControlList�б�Ȼ���ÿһ��
    TCnDockClientȡ������Ȼ����ø��Եĺ��������������ǵ�ѡ�� }
  for i := 0 to DockStyle.DockBaseControlListCount - 1 do
  begin
    if DockStyle.DockBaseControlLists[i] is TCnDockClient then
    begin
      ADockClient := TCnDockClient(DockStyle.DockBaseControlLists[i]);
      if ADockClient.ParentForm is TCnTabDockHostForm then
      begin
        { ��������TCnDockClient��ѡ�� }
        ResetDockClientOption(ADockClient);
      end;
    end;
  end;
end;

procedure TCnBasicTabServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
begin
  { do nothing }
end;

procedure TCnBasicTabServerOption.ResetTabPageControl(
  APage: TCnTabPageControl);
begin
  if APage = nil then Exit;
  APage.HotTrack := FHotTrack;
  APage.TabPosition := FTabPosition;
end;

procedure TCnBasicTabServerOption.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicTabServerOption.SetTabPosition(
  const Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    ResetDockControlOption;
  end;
end;

{ TCnBasicConjoinServerOption }

procedure TCnBasicConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicConjoinServerOption then
  begin
    FGrabbersSize := TCnBasicConjoinServerOption(Source).FGrabbersSize;
    FSplitterWidth := TCnBasicConjoinServerOption(Source).FSplitterWidth;
  end;
  inherited Assign(Source);
end;

constructor TCnBasicConjoinServerOption.Create(
  ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := 12;
  SplitterWidth := 4;
end;

destructor TCnBasicConjoinServerOption.Destroy;
begin
  inherited Destroy;
end;

procedure TCnBasicConjoinServerOption.ResetConjoinPanel(
  APanel: TCnConjoinPanel);
begin
  APanel.CnDockManager.GrabberSize := FGrabbersSize;
  APanel.CnDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TCnBasicConjoinServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
var
  ConjoinPanel: TCnConjoinPanel;
begin
  if ADockClient = nil then Exit;
  { �õ����ADockClient���ڵĴ����Panel }
  ConjoinPanel := TCnConjoinPanel(TCnConjoinDockHostForm(ADockClient.ParentForm).Panel);
  ResetConjoinPanel(ConjoinPanel);
  ConjoinPanel.Invalidate;
end;

procedure TCnBasicConjoinServerOption.ResetDockControlOption;
var
  i: Integer;
  ADockServer: TCnDockServer;
  ADockClient: TCnDockClient;
begin
  if DockStyle = nil then Exit;
  { ѭ��DockStyle��DockBaseControlList�б�Ȼ���ÿһ��TCnDockServer����
    TCnDockClientȡ������Ȼ����ø��Եĺ��������������ǵ�ѡ�� }
  for i := 0 to DockStyle.DockBaseControlListCount - 1 do
  begin
    if DockStyle.DockBaseControlLists[i] is TCnDockServer then
    begin
      { ��������TCnDockServer��ѡ�� }
      ADockServer := TCnDockServer(DockStyle.DockBaseControlLists[i]);
      ResetDockServerOption(ADockServer);
    end else if DockStyle.DockBaseControlLists[i] is TCnDockClient then
    begin
      ADockClient := TCnDockClient(DockStyle.DockBaseControlLists[i]);
      if ADockClient.ParentForm.HostDockSite is TCnConjoinPanel then
      begin
        { ��������TCnDockClient��ѡ�� }
        ADockClient := FindDockClient(ADockClient.ParentForm.HostDockSite.Parent);
        ResetDockClientOption(ADockClient);
      end;
    end;
  end;
end;

procedure TCnBasicConjoinServerOption.ResetDockPanel(APanel: TCnDockPanel);
begin
  APanel.CnDockManager.GrabberSize := FGrabbersSize;
  APanel.CnDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TCnBasicConjoinServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
var
  i: Integer;
begin
  // Panel���벻��Ϊ��
  if ADockServer = nil then Exit;
  for i := 0 to 3 do
  begin
    if ADockServer.DockPanel[i] = nil then
      break;
    ResetDockPanel(ADockServer.DockPanel[i]);
    ADockServer.DockPanel[i].Invalidate;
  end;
end;

procedure TCnBasicConjoinServerOption.SetGrabbersSize(
  const Value: TGrabbersSize);
begin
  if FGrabbersSize <> Value then
  begin
    FGrabbersSize := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicConjoinServerOption.SetGrabbersSize_WithoutChangeSystemInfo(
  const Value: TGrabbersSize);
begin
  FGrabbersSize := Value;
end;

procedure TCnBasicConjoinServerOption.SetSplitterWidth(
  const Value: TSplitterWidth);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicConjoinServerOption.SetSplitterWidth_WithoutChangeSystemInfo(
  const Value: TSplitterWidth);
begin
  FSplitterWidth := Value;
end;

{ TCnAdvTabPageControl }

procedure TCnAdvTabPageControl.CMUnDockClient(
  var Message: TCMUnDockClient);
begin
  inherited;
end;

procedure TCnAdvTabPageControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvTabPageControl.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

destructor TCnAdvTabPageControl.Destroy;
var DockClient: TCnDockClient;
begin
  DockClient := FindDockClient(Parent);
  if (DockClient <> nil) and (DockClient.LastDockSite is TCnDockPanel) then
  begin
    with TCnDockPanel(DockClient.LastDockSite) do
    begin
      if UseDockManager and (CnDockManager <> nil) then
        CnDockManager.RemoveControl(Self.Parent);
    end;
  end;
  inherited;
end;

procedure TCnAdvTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ TCnAdvConjoinPanel }

procedure TCnAdvConjoinPanel.CMUnDockClient(var Message: TCMUnDockClient);
begin
  inherited;

end;

procedure TCnAdvConjoinPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvConjoinPanel.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TCnAdvConjoinPanel.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // ����Client�ϵ�TCnDockClient�ؼ���LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ Initialization and cleanup }

procedure InitDockPresident;
var OSVERSIONINFO: TOSVERSIONINFO;
begin
  { ����һ��TCnDockPresident��ʵ�� }
  if CnGlobalDockPresident <> nil then
    CnGlobalDockPresident.Free;
  CnGlobalDockPresident := TCnDockPresident.Create;
  { �жϵ�ǰ����ϵͳ�İ汾, ��WindowsXP��������, �Ժ���õ��� }
  OSVERSIONINFO.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
  GetVersionEx(OSVERSIONINFO);
  IsWinXP := (OSVERSIONINFO.dwMajorVersion = 5) and (OSVERSIONINFO.dwMinorVersion = 1);
end;

procedure DoneDockPresident;
begin
  { �ͷ����TCnDockPresident��ʵ�� }
  if CnGlobalDockPresident <> nil then
  begin
    CnGlobalDockPresident.Free;
    CnGlobalDockPresident := nil;
  end;
end;

initialization
  InitDockPresident;

finalization
  DoneDockPresident;

end.

