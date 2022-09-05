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
* ��Ԫ���ƣ�ͣ�������һЩȫ�ֱ�����Ԫ 
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

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {�ؼ�����}
  gs_CnProductName = 'CnPack Dock Component';
  gs_CnDcokServerName = 'ͣ������ؼ�';
  gs_CnDcokClientName = 'ͣ���ͻ��ؼ�';
  gs_CnDockStyleName = 'ͣ�����ؼ�';
  {�汾}
  gs_CnDockManagerVersion = '1.0.0.0'; {���ǹ̶���}
  gs_CnDockStyleVersion   = '1.0.0.0'; {���ǹ̶���}
  {ʱ��}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {����}
  gs_CnAuthorName = '���沨';
  gs_CnComparyName = '��û����';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {����}
  gs_CnAbout = '����';
  gs_CnDockManagerAbout = '����һ�� %s, �汾�� %s,' + #10#13 +
                          '��Ȩ: %s-%s, ����: %s,��˾: %s,' + #10#13 +
                          '������ҳ: %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   '����һ�� %s, �汾�� %s,' + #10#13 +
                          '��Ȩ: %s-%s, ����: %s,��˾: %s,' + #10#13 +
                          '������ҳ: %s,' + #10#13 +
                          'Email: %s';
  {�ַ��ָ����}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {��ʾ��Ϣ}
  gs_CnDockTreeCloseBtnHint = '�ر�';
  gs_CnVCDockTreeExpandBtnHint = '��չ';
  gs_CnVSNETDockTreeAutoHideBtnHint = '�Զ�����';
  gs_CnDockTreeVSplitterHint = '��ֱ�ָ���';
  gs_CnDockTreeHSplitterHint = 'ˮƽ�ָ���';

  { Hash�����ʾ��Ϣ }
  gs_CnTableIndexError = 'Ͱ����������Χ';
  gs_CnNodeExistedError = '�ڵ��Ѿ�������';
  gs_CnComProcError = '�Ƚϵĺ���ָ��Ϊ��';

  { CnDockTree�Ĵ�����Ϣ }
  gs_ControlCannotIsNil = '����Control����Ϊnil';
  gs_CannotGetValueWithNoOrient = '���ܻ��û��ͣ�������Control������';
  gs_CannotSetValueWithNoOrient = '��������û��ͣ�������Control������';

  { CnDockFormControl�Ĵ�����Ϣ }
  gs_CannotChangeDockStyleProperty = '�����������ڸı�DockStyle����';
  gs_CannotLayAnother = '�ڴ������Ѿ�����һ��%s,�����ٷ���һ��%s��';

  { CnDelphiDockStyle����Ϣ }
  gs_LikeDelphiStyle = '���� Delphi ��%s';

  { CnVCDockStyle����Ϣ }
  gs_LikeVCStyle = '���� Visual C++ ��%s';

  { CnVIDDockStyle����Ϣ }
  gs_CannotSetTabPosition = '��������TabPositionΪtpLeft����tpRight';
  gs_LikeVIDStyle = '���� Visual InterDev ��%s';
  gs_TabPositionMustBetpBottom = 'TabPositionһ��Ҫ���ó�tpBottom';

  { CnVSNETDockStyle����Ϣ }
  gs_LikeVSNETStyle = '���� Visual Studio.net ��%s';

  { CnEclipseDockStyle����Ϣ }
  gs_LikeEclipseStyle = '���� Java eclipse ��%s';

  { CnDcokInfo����Ϣ }
  gs_CannotFindWindow = '!@#�Ҳ����������#@!';

  { DockTree�İ汾,������ͣ����Ϣ��װ�غʹ洢��ʱ��Ҫ�õ��� }
  gs_BaseDockTreeVersion = $00040000;
  { DockTree��VC�汾 }
  gs_VCDockTreeVersion = $00040010;

  { ��ͣ���ͻ��ͷ������ľ���С��DefExpandoRectʱ������ͣ������ }
  DefExpandoRect = 10;

  { WM_NCxxx�Ŀ�ʼ��WM_NCxxx�Ľ��� }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager��Ψһ��������������ͣ������ }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { �Ƿ����ڴ��ļ���ע�����װ��ͣ����Ϣ }
  IsLoading: Boolean = False;

  { �����������������ϵı�����ʱ���������TCnDockClient��ֵ��GlobalDockClient }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
