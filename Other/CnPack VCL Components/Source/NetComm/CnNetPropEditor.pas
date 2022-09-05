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

unit CnNetPropEditor;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ�����Ա༭����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.1
*                ΪTCnRS232ConfigProperty��TCnRS232TimeoutsProperty����
*                ��TRS232Dialog�����֧��
*           2002.04.08 V1.0
*                ������Ԫ
*                ����ע��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type

//------------------------------------------------------------------------------
// TCnRS232Config���Ա༭��
//------------------------------------------------------------------------------

{ TCnRS232ConfigProperty }

  TCnRS232ConfigProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

//------------------------------------------------------------------------------
// TCnRS232Timeouts���Ա༭��
//------------------------------------------------------------------------------

{ TCnRS232TimeoutsProperty }

  TCnRS232TimeoutsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

implementation

uses
  CnRS232, CnRS232Dialog, CnNetConsts;

//------------------------------------------------------------------------------
// TCnRS232Config���Ա༭��
//------------------------------------------------------------------------------

{ TCnRS232ConfigProperty }

// �༭����
procedure TCnRS232ConfigProperty.Edit;
var
  CommDlg: TCnRS232Dialog;
  CommConfig: TCnRS232Config;
begin
  if GetComponent(0) is TCnRS232 then
    CommConfig := TCnRS232(GetComponent(0)).CommConfig
  else if GetComponent(0) is TCnRS232Dialog then
    CommConfig := TCnRS232Dialog(GetComponent(0)).CommConfig
  else
    Exit;

  CommDlg := TCnRS232Dialog.Create(nil);
  try
    CommDlg.Kind := ckExtended;
    CommDlg.Pages := [cpNormal, cpXonXoff, cpHardware];
    CommDlg.BaudRateList := False;
    CommDlg.ShowHint := csCheckHint;
    CommDlg.CommConfig.Assign(CommConfig);
    if CommDlg.Execute then
    begin
      CommConfig.Assign(CommDlg.CommConfig);
    end;
    Designer.Modified;
  finally
    CommDlg.Free;
  end;
end;

// ȡ���Ա༭������
function TCnRS232ConfigProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

// ����������ʾ�ı�
function TCnRS232ConfigProperty.GetValue: string;
begin
  Result := SRS232Option;
end;

//------------------------------------------------------------------------------
// TCnRS232Timeouts���Ա༭��
//------------------------------------------------------------------------------

{ TCnRS232TimeoutsProperty }

// �༭����
procedure TCnRS232TimeoutsProperty.Edit;
var
  CommDlg: TCnRS232Dialog;
  CommTimeouts: TCnRS232Timeouts;
begin
  if GetComponent(0) is TCnRS232 then
    CommTimeouts := TCnRS232(GetComponent(0)).Timeouts
  else if GetComponent(0) is TCnRS232Dialog then
    CommTimeouts := TCnRS232Dialog(GetComponent(0)).Timeouts
  else
    Exit;

  CommDlg := TCnRS232Dialog.Create(nil);
  try
    CommDlg.Kind := ckExtended;
    CommDlg.Pages := [cpTimeouts];
    CommDlg.BaudRateList := False;
    CommDlg.ShowHint := csCheckHint;
    CommDlg.Timeouts.Assign(CommTimeouts);
    if CommDlg.Execute then
    begin
      CommTimeouts.Assign(CommDlg.Timeouts);
    end;
    Designer.Modified;
  finally
    CommDlg.Free;
  end;
end;

// ȡ���Ա༭������
function TCnRS232TimeoutsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

// ����������ʾ�ı�
function TCnRS232TimeoutsProperty.GetValue: string;
begin
  Result := SRS232TimeoutsOption;
end;

end.

