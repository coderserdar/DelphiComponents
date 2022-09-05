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

unit CnNetRegister;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ�����ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.1
*                ΪTCnRS232Dialog�����������Ա༭������
*           2002.04.08 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  {$IFNDEF BCB5} {$IFNDEF BCB6} CnUDP, {$ENDIF} {$ENDIF}
  CnConsts, CnRS232, CnModem, CnRS232Dialog, CnIP, CnPing, CnDNS, CnDialUp,
  CnCameraEye, CnIISCtrl, CnTwain, CnIocpSimpleMemPool, CnIocpSocketAdapter,
  CnNetPropEditor;

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
  RegisterComponents(SCnNetPalette, [TCnRS232]);
  RegisterComponents(SCnNetPalette, [TCnModem]);
  RegisterComponents(SCnNetPalette, [TCnRS232Dialog]);
  RegisterComponents(SCnNetPalette, [TCnDialUp]);
  RegisterComponents(SCnNetPalette, [TCnIP, TCnPing, TCnDNS]);
  RegisterComponents(SCnNetPalette, [TCnCameraEye, TCnIISCtrl, TCnTwain]);
  RegisterComponents(SCnNetPalette, [TCnIocpSimpleMemPool, TCnIocpSocketAdapter]);
{$IFNDEF BCB5} {$IFNDEF BCB6}
  RegisterComponents(SCnNetPalette, [TCnUDP]);
{$ENDIF} {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCnRS232Config), TCnRS232, '', TCnRS232ConfigProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Timeouts), TCnRS232, '', TCnRS232TimeoutsProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Config), TCnRS232Dialog, '', TCnRS232ConfigProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Timeouts), TCnRS232Dialog, '', TCnRS232TimeoutsProperty);
end;

end.
