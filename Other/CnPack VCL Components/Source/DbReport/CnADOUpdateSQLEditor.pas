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

unit CnADOUpdateSQLEditor;
{* |<PRE>
================================================================================
* ������ƣ�CnPack�����
* ��Ԫ���ƣ�CnADOUpdateSQL���Ա༭����Ԫ
* ��Ԫ���ߣ�С��
* ��    ע��
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.04.25
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Controls,
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.ADODB {$ELSE} ADODB {$ENDIF},
  Provider,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  Dsgnintf
  {$ENDIF}
  ;

type
  TCnADOUpdateSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  CnADOUpdateSQLFrm, CnADOUpdateSQL;

procedure TCnADOUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  with TCnADOUpdateSQLForm.Create(nil) do
  try
    case TCnADOUpdateSQL(Component).ConnectionType of
      ctConnection:
        if TCnADOUpdateSQL(Component).Connection <> nil then
          Connection := TCnADOUpdateSQL(Component).Connection;
      ctDataSet:
        if TCnADOUpdateSQL(Component).DataSet <> nil then
          Connection := TCustomADODataSet(TCnADOUpdateSQL(Component).DataSet).Connection;
      ctProvider:
        if TCnADOUpdateSQL(Component).Provider <> nil then
          Connection := TCustomADODataSet(TDataSetProvider(TCnADOUpdateSQL(Component).Provider).DataSet).Connection;
    end;
    ModifySQL.Text := TCnADOUpdateSQL(Component).ModifySQL.Text;
    InsertSQL.Text := TCnADOUpdateSQL(Component).InsertSQL.Text;
    DeleteSQL.Text := TCnADOUpdateSQL(Component).DeleteSQL.Text;
    if ShowModal = mrOK then
    begin
      TCnADOUpdateSQL(Component).ModifySQL.Text := ModifySQL.Text;
      TCnADOUpdateSQL(Component).InsertSQL.Text := InsertSQL.Text;
      TCnADOUpdateSQL(Component).DeleteSQL.Text := DeleteSQL.Text;
      Self.Designer.Modified;
    end;
  finally
    Free;
  end;
end;

function TCnADOUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  if Index in [1] then
    Result := 'Generate SQL';
end;

function TCnADOUpdateSQLEditor.GetVerbCount:Integer;
begin
  Result := $00000005;
end;

{$ENDIF SUPPORT_ADO}
end.
