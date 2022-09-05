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

unit CnDHibernateAbout; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate IDE��ؿ�
* ��Ԫ���ƣ����ڶԻ���Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CnDHibernateConsts, StdCtrls
  {$IFDEF DELPHI2007}, GIFImg {$ENDIF}, ShellApi;

type
  TCnFormDHibernateAbout = class(TForm)
    Label1: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    btnOK: TButton;
    procedure imgAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CnFormDHibernateAbout: TCnFormDHibernateAbout; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$R *.dfm}

procedure TCnFormDHibernateAbout.Image1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.cnpack.org', nil, nil, SW_SHOW);
end;

procedure TCnFormDHibernateAbout.imgAboutClick(Sender: TObject);
begin
  Close;
end;

procedure TCnFormDHibernateAbout.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := Format('Version: %s', [DH_VERSION]);
end; 

{$ENDIF SUPPORT_ADO}
end.
