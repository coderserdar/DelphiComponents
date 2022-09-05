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

unit CnNetConsts;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ���Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫ����������ͨѶ���õ�����Դ�ַ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.08 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnRS232
  SInvalidXonXoffChar = 'Xon�ַ���Xoff�ַ�������ͬ';
  SSerialPortAlreadyOpened = '�ô����Ѿ�����';
  SSerialPortOpenError = '���ڴ�ʧ��';
  SNotACommHandle = 'ָ�����豸����һ����Ч��ͨѶ�˿�';
  SSetupBuffFail = '���ô���ͨѶ������ʧ��';
  SCreateEventFail = '�����¼�ʧ��';
  SCreateReadFail = '�����������߳�ʧ��';
  SCreateWriteFail = '����д�����߳�ʧ��';
  SCnRS232Name = 'RS232����ͨѶ���';
  SCnRS232Comment = 'RS232����ͨѶ���';

  // CnModem
  SCnModemName = '��׼���ƽ����ͨѶ���';
  SCnModemComment = '��׼���ƽ����ͨѶ���';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232�������öԻ������';
  SCnRS232DialogComment = 'RS232�������öԻ������';
  SRS232Option = '��������';
  SRS232TimeoutsOption = '��ʱ����';
  SBaudRateError = '������ֻ����������';
  SInputASCIICode = '������ASCII����(0..255)';
  SInputInteger = '����������';

  // CnPing
  SCnPingName = 'Ping���';
  SCnPingComment = 'Ping���';
  SInitFailed = 'Winsock ��ʼ��ʧ�ܣ������ǰ汾����ȷ';
  SInvalidAddr = 'IP��ַ���Ϸ�';
  SNoResponse = '[%0:S] û����Ӧ';
  STimeOut = '��Ӧ��ʱ';
  SICMPRunError = 'ICMP���д���';
  SPingResultString = '[%0:S]: �ֽ���:%1:D ʱ��: %2:Dms TTL: %3:D';

  // CnIP
  SCnIPName = 'IP���';
  SCnIPComment = 'IP���';
  SCnErrorAddress = '�����IP��ַ';
  SCnErrorAddrRang = '����IP��ַ��Χ';

  // CnDNS
  SCnDNSName = 'DNS���';
  SCnDNSComment = 'DNS���';
  SCnDNSTooLong = '�ַ������ȳ���';
  SCnDNSInvalidHeadByteFmt = '�ַ������� %d λ�� %d ���Ƿ�';

implementation

end.
