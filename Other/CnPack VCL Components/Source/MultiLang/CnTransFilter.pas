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

unit CnTransFilter;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ������ IDE ����������õ�Ԫ
* ��Ԫ���ߣ�CnPack������ С�� (kending@21cn.com)
* ��    ע���õ�Ԫʵ���˶������ IDE ������˹���
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2006.10.15 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, CheckLst, CnLangUtils;

type
  TFrmTransFilter = class(TForm)
    chklstFilter: TCheckListBox;
    btnCancel: TButton;
    btnOK: TButton;
    lblFilter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure chklstFilterKeyPress(Sender: TObject; var Key: Char);
  private
    procedure GetFilters(var FilterOptions: TLangTransFilterSet);
    procedure SetFilters(const FilterOptions: TLangTransFilterSet);
    { Private declarations }
  public
    procedure Open(var FilterOptions: TLangTransFilterSet);
    { Public declarations }
  end;

implementation

uses
  CnLangConsts;

{$R *.dfm}

procedure TFrmTransFilter.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Caption := SCnFilterFrmCaption;
  lblFilter.Caption := SCnFilterCaption;
  btnOK.Caption := SCnOKCaption;
  btnCancel.Caption := SCnCancelCaption;

  with chklstFilter do
    for i := 0 to Items.Count - 1 do
      Checked[i] := True;
end;

procedure TFrmTransFilter.GetFilters(var FilterOptions: TLangTransFilterSet);
var
  i: Integer;
begin
  FilterOptions := [];
  with chklstFilter do
    for i := 0 to Items.Count - 1 do
      if Checked[i] then
        Include(FilterOptions, TLangTransFilter(i));
end;

procedure TFrmTransFilter.SetFilters(const FilterOptions: TLangTransFilterSet);
var
  i: Integer;
begin
  with chklstFilter do
    for i := 0 to Items.Count - 1 do
      Checked[i] := TLangTransFilter(i) in FilterOptions;
end;

procedure TFrmTransFilter.Open(var FilterOptions: TLangTransFilterSet);
begin
  if FilterOptions <> [] then
    SetFilters(FilterOptions);

  if ShowModal = mrOk then
    GetFilters(FilterOptions);
end;

procedure TFrmTransFilter.chklstFilterKeyPress(Sender: TObject;
  var Key: Char);
var
  I: Integer;
begin
  if Key = #1 then // Ctrl+A
  begin
    for I := 0 to chklstFilter.Items.Count - 1 do
      chklstFilter.Checked[I] := True;
  end
  else if Key = #4 then // Ctrl+D
  begin
    Key := #0;
    for I := 0 to chklstFilter.Items.Count - 1 do
      chklstFilter.Checked[I] := False;
  end
end;

end.

