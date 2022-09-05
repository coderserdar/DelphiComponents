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

unit CnDBRegister;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ���ݿ⹤�������
* ��Ԫ���ƣ����ݿ������ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.12.12 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
{$IFDEF SUPPORT_ADO}
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.AdoConEd {$ELSE} AdoConEd {$ENDIF},
{$ENDIF}
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  CnCompRegister, {$IFNDEF COMPILER5}CnSQLAnalyzer, {$ENDIF} CnADOConPool, CnConsts,
  CnADOUpdateSQLEditor, CnADOUpdateSQL, CnPagedGrid {$IFDEF COMPILER6_UP},
  CnDHibernateConsts, CnDHibernateClasses, CnDHibernateMemData,
  CnDHibernateQueryAdv, CnDHibernateSubQuery, CnDhibernateSubQueryAdv,
  CnDHibernateAbout, CnDHibernateThread, CnDHibernateImport, CnDHibernateExport,
  CnDHibernateCalc, CnDHibernateBackupRestore, CnDHibernateNavigator,
  CnDHibernateBatchSQL{$ENDIF};

{$IFDEF SUPPORT_ADO}
type
  TADOConStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{$IFDEF COMPILER6_UP}
  TCnDHAboutEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
{$ENDIF}

{$ENDIF}

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
{$IFDEF SUPPORT_ADO}
{$IFNDEF COMPILER5}
  RegisterComponents(SCnDatabasePalette, [TCnSQLAnalyzer]);
  RegisterPropertyEditor(TypeInfo(string), TCnSQLAnalyzer,
    'ConnectionString', TADOConStringProperty);
{$ENDIF}
  RegisterComponents(SCnDatabasePalette, [TCnADOConPool]);
  RegisterPropertyEditor(TypeInfo(WideString), TCnADOConPool,
    'ConnectionString', TADOConStringProperty);
  RegisterComponents(SCnDatabasePalette, [TCnADOUpdateSQL]);
  RegisterComponentEditor(TCnADOUpdateSQL, TCnADOUpdateSQLEditor);

{$IFNDEF COMPILER5}
  RegisterComponents(SCnDatabasePalette, [
    TCnDHibernateQuery, TCnDHibernateQueryAdvance, {$IFNDEF DELPHI2009_UP}TCnDHibernateMemData, {$ENDIF}
      TCnDHibernateIdGenerator, TCnDHibernateSubQuery, TCnDHibernateSubQueryAdvance,
      TCnDHibernateThread, TCnDHibernateImport, TCnDHibernateExport,
      TCnDHibernateCalculator, TCnDHibernateBackupRestore, TCnDHibernateNavigator,
      TCnDHibernateBatchSQL]);

  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateQuery, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateQueryAdvance, 'About',
    TCnDHAboutEditor);

{$IFNDEF DELPHI2009_UP}
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateMemData, 'About',
    TCnDHAboutEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateIdGenerator, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateSubQuery, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateSubQueryAdvance, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateThread, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateImport, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateExport, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateCalculator, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateBackupRestore, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateNavigator, 'About',
    TCnDHAboutEditor);
  RegisterPropertyEditor(TypeInfo(string), TCnDHibernateBatchSQL, 'About',
    TCnDHAboutEditor);
{$ENDIF}
{$ENDIF}

{$IFDEF SUPPORT_DB}
  RegisterComponents(SCnDatabasePalette, [TCnPagedGrid]);
{$ENDIF}
end;

{ TADOConStringProperty }

{$IFDEF SUPPORT_ADO}
procedure TADOConStringProperty.Edit;
begin
  if EditConnectionString(GetComponent(0) as TComponent) then
    Modified;
end;

function TADOConStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFDEF COMPILER6_UP}

{ TCnDHAboutEditor }

procedure TCnDHAboutEditor.Edit;
begin
  inherited;
  with TCnFormDHibernateAbout.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

function TCnDHAboutEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

function TCnDHAboutEditor.GetValue: string;
begin
  Result := 'CnPack';
end;

{$ENDIF}

{$ENDIF}

end.
