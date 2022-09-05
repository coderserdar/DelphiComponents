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
* �޸ļ�¼��2005.12.24 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnRS232
  SInvalidXonXoffChar = 'XonChar must differs from XoffChar';
  SSerialPortAlreadyOpened = 'This serial port already opened';
  SSerialPortOpenError = 'Error opening serial port';
  SNotACommHandle = 'File handle is not a comm handle ';
  SSetupBuffFail = 'Cannot setup comm buffer';
  SCreateEventFail = 'Unable to create event';
  SCreateReadFail = 'Unable to create read thread';
  SCreateWriteFail = 'Unable to create write thread';
  SCnRS232Name = 'RS232 Cerial Port Communications Component';
  SCnRS232Comment = 'RS232 Serial Port Communications Component';

  // CnModem
  SCnModemName = 'Modem Communications Component';
  SCnModemComment = 'Modem Communications Component';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232 Serial Port Option Dialog Component';
  SCnRS232DialogComment = 'RS232 Serial Port Option Dialog Component';
  SRS232Option = 'Comm Option';
  SRS232TimeoutsOption = 'Timeout Settings';
  SBaudRateError = 'Baud Rate must be an Integer Value';
  SInputASCIICode = 'Please Enter an ASCII code (0..255)';
  SInputInteger = 'Please Enter an Integer';

  // CnPing
  SCnPingName = 'CnPing Component';
  SCnPingComment = 'CnPing Component';  
  SInitFailed = 'Init Failed. Maybe Winsock Verison Error';
  SInvalidAddr = 'IP Address Error';
  SNoResponse = '[%0:S] No Response';
  STimeOut = 'Time Out';    
  SICMPRunError = 'ICMP Run Error';
  SPingResultString = '[%0:S]: Bytes:%1:D Time: %2:Dms TTL:%3:D';
  
  // CnIP
  SCnIPName = 'CnIP Component';
  SCnIPComment = 'CnIP Component'; 
  SCnErrorAddress = 'Error IP Address';  
  SCnErrorAddrRang = 'IP Address Range Error';

  // CnDNS
  SCnDNSName = 'CnDNS Component';
  SCnDNSComment = 'CnDNS Component';
  SCnDNSTooLong = 'Too Long String Length Byte and Out of Bound.';
  SCnDNSInvalidHeadByteFmt = 'Invalid String Head Byte %d at %d.';

implementation

end.
