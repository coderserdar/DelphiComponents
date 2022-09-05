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

unit CnConsole;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�����̨��� TCnConsole ��Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
*           ����
* ��    ע��Ϊ GUI �������ӿ���̨
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP + Delphi 5.0
* �� �� �����õ�Ԫ�����ַ�����Դ
* �޸ļ�¼��2008.10.14 v1.1
*               �����������ı���ɫ�Ĺ���
*           2006.10.05 v1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows,
  CnClasses, CnConsts, CnCompConsts;

  {����̨�ı�����ɫ����}
   const
      tfBlue  =1;
      tfGreen =2;
      tfRed   =4;
      tfIntensity = 8;
      tfWhite = $f;
      
      tbBlue  =$10;
      tbGreen =$20;
      tbRed   =$40;
      tbIntensity = $80;

type
  TCnConsole = class(TCnComponent)
  private
    FConsoleTitle: string;
    FEnabled: Boolean;
    FConsoleHandle:THandle;
    procedure SetConsoleTitle(const Value: string);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    procedure ResetConsole;
    {* ��λ����̨,������EnabledΪTrueʱ��Ч��}
    procedure SetTextColor(const aColor:WORD);
    {* ���ÿ���̨��ǰ����ɫ}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    {* ΪTrueʱ��������̨��ΪFalse�رտ���̨}
    property ConsoleTitle: string read FConsoleTitle write SetConsoleTitle;
    {* ����̨�ı���}
  end;

implementation

{ TCnConsole }

constructor TCnConsole.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
end;

destructor TCnConsole.Destroy;
begin
  if not (csDesigning in ComponentState) and FEnabled then
    FreeConsole;
  inherited;
end;

procedure TCnConsole.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnConsoleName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnConsoleComment;
end;

procedure TCnConsole.ResetConsole;
begin
  if csDesigning in ComponentState then
    Exit;

  if FEnabled then
  begin
    FreeConsole;
    AllocConsole;
    FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    if FConsoleTitle <> '' then
      Windows.SetConsoleTitle(PChar(FConsoleTitle));
  end;
end;

procedure TCnConsole.SetConsoleTitle(const Value: string);
begin
  FConsoleTitle := Value;
  if FEnabled and not (csDesigning in ComponentState) then
    if (FConsoleTitle <> '') or not (csLoading in ComponentState) then
      Windows.SetConsoleTitle(PChar(FConsoleTitle));
end;

procedure TCnConsole.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if csDesigning in ComponentState then
      Exit;
      
    if FEnabled then
    begin
      AllocConsole;
      SetConsoleTitle(PChar(FConsoleTitle));
      FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    end
    else
    begin
      FreeConsole;
    end;
  end;
end;

procedure TCnConsole.SetTextColor(const aColor: WORD);
begin
  if FEnabled then
  SetConsoleTextAttribute(FConsoleHandle, aColor);
end;

end.
