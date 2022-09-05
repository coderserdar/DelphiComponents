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
{       ����һЩȫ�ֵı���                              }
{       CnDockGlobal ��Ԫ                               }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockGlobal;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�ͣ�������һЩȫ�ֱ�����Ԫ��Ӣ���
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.17 V1.0
*                ���뵥Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {Component Names}
  gs_CnProductName = 'CnPack Dock Components';
  gs_CnDcokServerName = 'Dock Server Component';
  gs_CnDcokClientName = 'Dock Client Component';
  gs_CnDockStyleName = 'Dock Style Component';
  {Version}
  gs_CnDockManagerVersion = '1.0.0.0'; {NOT Fixed}
  gs_CnDockStyleVersion   = '1.0.0.0'; {NOT Fixed}
  {Time}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {Author}
  gs_CnAuthorName = 'Zhou Yibo';
  gs_CnComparyName = 'None';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {About}
  gs_CnAbout = 'About';
  gs_CnDockManagerAbout = 'It is %s, version %s,' + #10#13 +
                          'Copywrite: %s-%s, Author: %s, Company: %s,' + #10#13 +
                          'Website: %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   'It is %s, version %s,' + #10#13 +
                          'Copywright: %s-%s, Author: %s, Company: %s,' + #10#13 +
                          'Website: %s,' + #10#13 +
                          'Email: %s';
  {Splitter}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {Hint}
  gs_CnDockTreeCloseBtnHint = 'Close';
  gs_CnVCDockTreeExpandBtnHint = 'Expand';
  gs_CnVSNETDockTreeAutoHideBtnHint = 'Auto Hide';
  gs_CnDockTreeVSplitterHint = 'Vertical Splitter';
  gs_CnDockTreeHSplitterHint = 'Horizontal Splitter';

  { Hash Table }
  gs_CnTableIndexError = 'Table Index Error';
  gs_CnNodeExistedError = 'Node Existed';
  gs_CnComProcError = 'Compare Proc Pointer is nil';

  { CnDockTree }
  gs_ControlCannotIsNil = 'Control can NOT be nil';
  gs_CannotGetValueWithNoOrient = 'Can NOT Get Control Value without Dock Orient';
  gs_CannotSetValueWithNoOrient = 'Can NOT Set Control Value without Dock Orient';

  { CnDockFormControl }
  gs_CannotChangeDockStyleProperty = 'Can NOT Change DockStyle in Runtime';
  gs_CannotLayAnother = 'A %s already Exists, Can NOT put Another %s';

  { CnDelphiDockStyle }
  gs_LikeDelphiStyle = 'Delphi %s';

  { CnVCDockStyle }
  gs_LikeVCStyle = 'Visual C++ %s';

  { CnVIDDockStyle }
  gs_CannotSetTabPosition = 'Can NOT Set TabPosition to tpLeft/tpRight';
  gs_LikeVIDStyle = 'Visual InterDev %s';
  gs_TabPositionMustBetpBottom = 'TabPosition must be tpBottom';

  { CnVSNETDockStyle }
  gs_LikeVSNETStyle = 'Visual Studio.net %s';

  { CnEclipseDockStyle}
  gs_LikeEclipseStyle = 'Java Eclipse %s';

  { CnDcokInfo }
  gs_CannotFindWindow = '!@# Can NOT Find Window #@!';

  { DockTree Version }
  gs_BaseDockTreeVersion = $00040000;
  { DockTree VC Version }
  gs_VCDockTreeVersion = $00040010;

  { Dock Instance: DefExpandoRect }
  DefExpandoRect = 10;

  { WM_NCxxx Start and End }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { Is Loading from File or Registry }
  IsLoading: Boolean = False;

  {GlobalDockClient will be set to a TCnDockClient on a form when Caption clicked. }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
