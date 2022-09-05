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

unit CnProgressFrm;
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ�ͨ�ý��������嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���ô����ɳ����ڲ����ƿ����͹رգ�����ֱ�Ӵ�������ʵ��
*           �õ�Ԫ�ṩ���¼�������������ʾ��̬��ʾ���壺
*             ShowProgress   - ��ʾ����������
*             HideProgress   - ���ؽ���������
*             UpdateProgress - ���µ�ǰ����
*             UpdateProgressTitle  - ���´������
* ʹ�÷���������Ҫ��ʾ��ʾ���ڵĵ�Ԫ��uses����Ԫ������Ҫ��ʾ��ʾ��Ϣʱֱ��
*           ֱ�ӵ���ShowXXXX���̼��ɡ�
* ע�����ͬһʱ����Ļ��ֻ����ʾһ�����ȴ��壬������ʾʱ�������д������
*           ��ʹ�ã�����ʾ�ô���Ĵ����Կ��Լ������С�
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����в����ϱ��ػ�����ʽ
* �޸ļ�¼��2008.03.08 V1.1
*                xierenxixi �޸�����÷�ʽ
*           2002.04.03 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ComCtrls;

type

{ TProgressForm }

  TProgressForm = class(TForm)
    SpeedButton1: TSpeedButton;
    lblTitle: TLabel;
    ProgressBar: TProgressBar;
    Label1: TLabel;
  private
    { Private declarations }
    FPerLabel: TLabel;
  public
    { Public declarations }
    procedure DoCreate; override;

  end;

procedure ShowProgress(const Title: string; AMax: Integer = 100);
{* ��ʾ���������壬����Ϊ��������Լ����ֵ��Ĭ�� 100���ٷֱ���ʽ�������Զ��������ֵ}
procedure HideProgress;
{* �رս���������}
procedure UpdateProgress(Value: Integer);
{* ���µ�ǰ���ȣ�����Ϊ����ֵ���� Max Ϊ 100 ʱ�ɽ��ܷ�ΧΪ 0..100����ʱ Value ����ٷֱ�}
procedure UpdateProgressTitle(const Title: string);
{* ���½�����������⣬����Ϊ����}
procedure UpdateProgressMax(Value: Integer);
{* ���½��������ֵ������Ϊ�µ����ֵ}

implementation

{$R *.DFM}

var
  ProgressForm: TProgressForm = nil;  // ����������ʵ��
  FormList: Pointer;   // �����õĴ����б�ָ��

// ��ʾ����
procedure ShowProgress(const Title: string; AMax: Integer = 100);
begin
  if not Assigned(ProgressForm) then
    ProgressForm := TProgressForm.Create(Application.MainForm)
  else
    ProgressForm.BringToFront;
  ProgressForm.lblTitle.Caption := Title;
  ProgressForm.ProgressBar.Max := AMax;
  ProgressForm.Show;

  // xierenxixi �޸�
  FormList := DisableTaskWindows(ProgressForm.Handle);
  ProgressForm.Update;
end;

// �رմ���
procedure HideProgress;
begin
  if not Assigned(ProgressForm) then Exit;

  // xierenxixi �޸�
  EnableTaskWindows(FormList);
  
  ProgressForm.Hide;
  Application.ProcessMessages;
  ProgressForm.Free;
  ProgressForm := nil;
end;

// ���½���
procedure UpdateProgress(Value: Integer);
begin
  if Assigned(ProgressForm) then
  begin
    ProgressForm.ProgressBar.Position := Value;
    ProgressForm.FPerLabel.Caption := Format('%-3d%%', [Round(Value/ProgressForm.ProgressBar.Max * 100)]);
    ProgressForm.Update;
    Application.ProcessMessages;
  end;
end;

// ���±���
procedure UpdateProgressTitle(const Title: string);
begin
  if Assigned(ProgressForm) then
  begin
    ProgressForm.lblTitle.Caption := Title;
    ProgressForm.Update;
    Application.ProcessMessages;
  end;
end;

// ���½��������ֵ
procedure UpdateProgressMax(Value: Integer);
begin
  if Assigned(ProgressForm) then
  begin
    ProgressForm.ProgressBar.Max:=Value;
    ProgressForm.Update;
    Application.ProcessMessages;
  end;
end;

{ TProgressForm }

procedure TProgressForm.DoCreate;
begin
  inherited;
  FPerLabel := TLabel.Create(Self);
  FPerLabel.Caption := '    '; // 100%
  FPerLabel.Parent := Label1.Parent;
  FPerLabel.Top := Label1.Top;
  FPerLabel.Left := ProgressBar.Left + ProgressBar.Width - FPerLabel.Width;
end;

end.
