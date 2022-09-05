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

unit CnCompAboutFrm;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�����������������ڴ��ڵ�Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����ݲ����ϱ��ػ�����ʽ
* �޸ļ�¼��2002.08.06 V1.0
                ������ýӿ�
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type

{ TCnCompAboutForm }

  TCnCompAboutForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  PCnAuthorInfo = ^TCnAuthorInfo;
  {* ���������Ϣָ�� }
  TCnAuthorInfo = record
  {* ���������Ϣ��¼ }
    Name: string;
    {* ������� }
    Email: string;
    {* ����������䣬����Ϊ�� }
  end;

  PCnAuthorInfoArray = ^TCnAuthorInfoArray;
  {* ���������Ϣ��̬����ָ�� }
  TCnAuthorInfoArray = array of TCnAuthorInfo;
  {* ���������Ϣ��̬���� }

function CnShowCompAbout(Sender: TObject; AuthorArray: TCnAuthorInfoArray;
  const Comment: string): Boolean; overload;
{* ��ʾ������ڴ��ڹ��̣��������ߣ�
 |<PRE>
   Sender: TObject                 - �����ߣ��������������������Ա༭��
   AuthorArray: TCnAuthorInfoArray - ���������Ϣ����̬���飬���Դ�nil
   Comment: string                 - �����˵����Ϣ������Ϊ��
   Result: Boolean                 - �����ģʽ���ڷ��� mrOK ��Ϊ�棬����Ϊ��
 |</PRE>}

function CnShowCompAbout(Sender: TObject; const AName, AEmail: string;
  const Comment: string): Boolean; overload;
{* ��ʾ������ڴ��ڹ��̣���һ���ߣ�
 |<PRE>
   Sender: TObject                 - �����ߣ��������������������Ա༭��
   AName: string                   - �������
   AEmail: string                  - �������Email
   Comment: string                 - �����˵����Ϣ������Ϊ��
   Result: Boolean                 - �����ģʽ���ڷ��� mrOK ��Ϊ�棬����Ϊ��
 |</PRE>}

implementation

uses
  CnConsts;

{$R *.DFM}

// ��ʾ������ڴ��ڹ��̣��������ߣ�
function CnShowCompAbout(Sender: TObject; AuthorArray: TCnAuthorInfoArray;
  const Comment: string): Boolean;
begin
  with TCnCompAboutForm.Create(nil) do
  try
    // �˴������û����룬�������ϲ���
    // ע���� Low(AuthorArray) �� High(AuthorArray) ��ȡ�������½�
    // ���Ϊ�������������ӣ�����Ϊ��ʱ�����
    // ��ʾʱ�õ��������ַ����������� CnConsts ��ȡ���ر��ǰ汾�ŵ�
    // �� CnConsts �е���Դ�ַ�������
    // ���ǵ����ػ�����Ԫ�У����Ǵ��壩�õ��������ַ�����÷ŵ� CnCosnts �ж���
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

// ��ʾ������ڴ��ڹ��̣���һ���ߣ�
function CnShowCompAbout(Sender: TObject; const AName, AEmail: string;
  const Comment: string): Boolean; overload;
var
  AuthorArray: TCnAuthorInfoArray;
begin
  SetLength(AuthorArray, 1);
  try
    AuthorArray[0].Name := AName;
    AuthorArray[0].Email := AEmail;
    Result := CnShowCompAbout(Sender, AuthorArray, Comment);
  finally
    AuthorArray := nil;
  end;
end;

end.
