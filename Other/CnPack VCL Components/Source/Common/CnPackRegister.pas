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

unit CnPackRegister;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�����������ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.1
*                ����TCnCopyright���Ա༭������
*           2002.04.08 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// ע�͵�����ȡ����ϵͳĬ��������޸�
// Ĭ�Ϸ�ʽ��ʹ�����塢Size9 ����ȡ��ϵͳĬ�ϵ�����
// ��������Ӱ���½��Ĵ��塢IDE�в��ִ��ڵ�
// {$DEFINE CN_MODIFY_DEFAULT_FONT}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  CnClasses, CnPropEditors;

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
  {$IFDEF CN_MODIFY_DEFAULT_FONT}
  with DefFontData do    // �޸�ϵͳĬ������
  begin
    Handle := 0;
    Height := -12;
    Pitch := fpDefault;
    Style := [];
    Charset := GB2312_CHARSET;
    Name := '����';
  end;
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCnCopyright), nil, 'About', TCnCopyrightProperty);
end;

end.
