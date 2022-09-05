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

unit TestSkipListUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, CnSkipList, StdCtrls, ComCtrls, ExtCtrls;

type
  TSkipListTestForm = class(TForm)
    Grid: TStringGrid;
    btnShow: TButton;
    btnAdd1: TButton;
    btnAdd2: TButton;
    btnAdd3: TButton;
    edtValue: TEdit;
    udValue: TUpDown;
    btnAdd: TButton;
    btnRandom: TButton;
    btnDel: TButton;
    bvl1: TBevel;
    bvl2: TBevel;
    bvl3: TBevel;
    btnSearch: TButton;
    bvl4: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnAdd1Click(Sender: TObject);
    procedure btnAdd2Click(Sender: TObject);
    procedure btnAdd3Click(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FSkipList: TCnSkipList;
    procedure UpdateSkipListToGrid;
    procedure AddValue(Value: Integer);
  public

  end;

var
  SkipListTestForm: TSkipListTestForm;

implementation

{$R *.dfm}

function Comp(const Value: Pointer; const Node: TCnSkipListNode): Integer;
var
  P1, P2: Integer;
begin
//  if Node = nil then
//  begin
//    Result := -1;
//    Exit;
//  end;

  P1 := Integer(Value);
  P2 := Integer(Node.Data);

  if P1 > P2 then
    Result := 1
  else if P1 = P2 then
    Result := 0
  else
    Result := -1;
end;

procedure TSkipListTestForm.FormCreate(Sender: TObject);
begin
  FSkipList := TCnSkipList.Create(Comp);
  FSkipList.Head.Text := 'Head';
  UpdateSkipListToGrid;
end;

procedure TSkipListTestForm.FormDestroy(Sender: TObject);
begin
  FSkipList.Free;
end;

procedure TSkipListTestForm.UpdateSkipListToGrid;
var
  I, Col: Integer;
  P: TCnSkipListNode;
begin
  Grid.RowCount := FSkipList.MaxLevel + 1; // 0 �� MaxLevel ��
  Grid.ColCount := FSkipList.Count + 2;    // ����һ���кţ�����һ�� nil

  for I := 0 to Grid.RowCount - 1 do
    for Col := 0 to Grid.ColCount - 1 do
      Grid.Cells[Col, I] := '';

  // ����ߵĲ����
  for I := 0 to Grid.RowCount - 1 do
    Grid.Cells[0, I] := IntToStr(Grid.RowCount - 1 - I);

  // �������ʣ�Head ��Ӧ�� 1��
  Col := 1;
  P := FSkipList.Head;
  while P <> nil do
  begin
    if P = FSkipList.Head then
    begin
      for I := 0 to Grid.RowCount - 1 do
        Grid.Cells[Col, I] := '>';
    end
    else
    begin
      // P ������Ҫ�� Col ����
      for I := 0 to P.Level do
      begin
        if (P.Forwards[I] <> nil) or (I = 0) then    // i = 0 ��ʾ���²㣬Ҳ���� RowCount - 1 ��
          Grid.Cells[Col, Grid.RowCount - 1 - I] := IntToStr(Integer(P.Data));
      end;
    end;
    Inc(Col);
    P := P.Forwards[0];
  end;

  for I := 0 to Grid.RowCount - 1 do
    Grid.Cells[Col, Grid.RowCount - 1 - I] := 'nil';
end;

procedure TSkipListTestForm.btnShowClick(Sender: TObject);
begin
  UpdateSkipListToGrid;
end;

procedure TSkipListTestForm.btnAdd1Click(Sender: TObject);
begin
  AddValue(1);
end;

procedure TSkipListTestForm.AddValue(Value: Integer);
var
  Node: TCnSkipListNode;
begin
  Node := FSkipList.Insert(Pointer(Value));
  if Node <> nil then
  begin
    Node.Data := Pointer(Value);
    Node.Text := IntToStr(Value);
    UpdateSkipListToGrid;
  end
  else
    ShowMessage('Insert Failed.');
end;

procedure TSkipListTestForm.btnAdd2Click(Sender: TObject);
begin
  AddValue(2);
end;

procedure TSkipListTestForm.btnAdd3Click(Sender: TObject);
begin
  AddValue(3);
end;

procedure TSkipListTestForm.btnAddClick(Sender: TObject);
begin
  AddValue(udValue.Position);
end;

procedure TSkipListTestForm.btnRandomClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 50 do
    AddValue(Trunc(Random * 1000) - 400);
end;

procedure TSkipListTestForm.btnDelClick(Sender: TObject);
var
  I, E: Integer;
  S: string;
begin
  S := Grid.Cells[Grid.Col, Grid.Row];
  if S = '' then
  begin
    for I := Grid.Row to Grid.RowCount - 1 do
    begin
      S := Grid.Cells[Grid.Col, I];
      if S <> '' then
        Break;
    end;
  end;

  Val(S, I, E);
  if E <> 0 then
  begin
    ShowMessage('Can NOT Delete: ' + S);
    Exit;
  end;

  if FSkipList.Delete(Pointer(I)) then
  begin
    ShowMessage('Delete OK. ' + S);
    UpdateSkipListToGrid;
  end
  else
    ShowMessage('Delete Failed. ' + S);
end;

procedure TSkipListTestForm.btnSearchClick(Sender: TObject);
var
  S: string;
  I, E: Integer;
  Node: TCnSkipListNode;
begin
  S := InputBox('Search', 'Enter a Value:', '0');
  Val(S, I, E);
  if E <> 0 then
  begin
    ShowMessage('Can NOT Search: ' + S);
    Exit;
  end;

  Node := FSkipList.Search(Pointer(I));
  if Node <> nil then
    ShowMessage('Found.')
  else
    ShowMessage('NOT Found.');
end;

end.
