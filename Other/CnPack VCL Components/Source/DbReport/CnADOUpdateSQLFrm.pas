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

unit CnADOUpdateSQLFrm;
{* |<PRE>
================================================================================
* ������ƣ�CnPack�����
* ��Ԫ���ƣ�CnADOUpdateSQL���Ա༭������
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.ADODB {$ELSE} ADODB {$ENDIF};

type
  TCnADOUpdateSQLForm = class(TForm)
    PageControl1: TPageControl;
    TabSheetOptions: TTabSheet;
    GroupBox1: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    cbbTables: TComboBox;
    btnGetTables: TButton;
    btnGetTableFields: TButton;
    btnGenerateSQL: TButton;
    lstKeyFields: TListBox;
    lstUpdateFields: TListBox;
    TabSheetSQL: TTabSheet;
    lbl4: TLabel;
    RadioGroupSQL: TRadioGroup;
    mmoSQLText: TMemo;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    pmKeyFields: TPopupMenu;
    mniSelectAll1: TMenuItem;
    mniClearAll1: TMenuItem;
    pmUpdateFields: TPopupMenu;
    mniSelectAll2: TMenuItem;
    mniClearAll2: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure btnGetTablesClick(Sender: TObject);
    procedure btnGetTableFieldsClick(Sender: TObject);
    procedure btnGenerateSQLClick(Sender: TObject);
    procedure RadioGroupSQLClick(Sender: TObject);
    procedure cbbTablesChange(Sender: TObject);
    procedure mmoSQLTextChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mniSelectAll1Click(Sender: TObject);
    procedure mniClearAll1Click(Sender: TObject);
    procedure mniSelectAll2Click(Sender: TObject);
    procedure mniClearAll2Click(Sender: TObject);    
  private
    { Private declarations }
    FConnection: TADOConnection;
    FModifySQL: TStrings;
    FInsertSQL: TStrings;
    FDeleteSQL: TStrings;
    FTableName: AnsiString;
    procedure ClearValue;
    function GetConnection: TADOConnection;
    function GetModifySQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetDeleteSQL: TStrings;
    procedure SetConnection(Value: TADOConnection);
    procedure SetModifySQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetDeleteSQL(Value: TStrings);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connection: TADOConnection read GetConnection write SetConnection;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$R *.dfm}

procedure CnListBoxSelectAll(ListBox: TListBox; Select: Boolean = True);
var
  I: Integer;
begin
  if ListBox.MultiSelect then
    for I := 0 to ListBox.Items.Count - 1 do
      ListBox.Selected[I] := Select
  else if not Select then
    ListBox.ItemIndex := -1;
end;

{ TCnADOUpdateSQLForm }

constructor TCnADOUpdateSQLForm.Create(AOwner: TComponent);
begin
  inherited;
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
end;

destructor TCnADOUpdateSQLForm.Destroy;
begin
  FModifySQL.Free;
  FInsertSQL.Free;
  FDeleteSQL.Free;
  inherited;
end;

procedure TCnADOUpdateSQLForm.FormShow(Sender: TObject);
var
  i, j: Integer;
begin
  if FConnection <> nil then
  begin
    FConnection.GetTableNames(cbbTables.Items);
    if Trim(FModifySQL.Text) <> '' then
    begin
      //��ModifySQL�ַ����л�ȡ����
      FTableName := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim(FModifySQL.Strings[0]));
      System.Delete(FTableName, 1, 7);
      FTableName := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim({$IFDEF UNICODE}String{$ENDIF}(FTableName)));
      cbbTables.ItemIndex := cbbTables.Items.IndexOf({$IFDEF UNICODE}String{$ENDIF}(FTableName));

      //����FTableName������ȡ�ֶ���
      FConnection.GetFieldNames({$IFDEF UNICODE}String{$ENDIF}(FTableName), lstKeyFields.Items);
      lstUpdateFields.Items.Text := lstKeyFields.Items.Text;

      //����FModifySQL��ѡ���б���ֶ�
      j := 0;
      for i := 2 to FModifySQL.Count - 1 do
      begin
        if Trim(UpperCase(FModifySQL.Strings[i])) = 'WHERE' then
        begin
          j := i;
          Break;
        end;
        lstUpdateFields.Selected[lstUpdateFields.Items.IndexOf(Trim(Copy(FModifySQL.Strings[i], 1, Pos('=', FModifySQL.Strings[i]) - 1)))] := True;
      end;
      for i := j + 1 to FModifySQL.Count - 1 do
      begin
        lstUpdateFields.Selected[lstUpdateFields.Items.IndexOf(Trim(Copy(FModifySQL.Strings[i], 1, Pos('=', FModifySQL.Strings[i]) - 1)))] := True;
      end;
    end;
  end;
  btnOK.Enabled := Trim(mmoSQLText.Lines.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.btnGetTablesClick(Sender: TObject);
var
  sText: string;
begin
  sText := cbbTables.Text;
  if FConnection <> nil then
    FConnection.GetTableNames(cbbTables.Items);
  cbbTables.ItemIndex := cbbTables.Items.IndexOf(sText);
  cbbTables.Hint := cbbTables.Text;
  ClearValue;
end;

procedure TCnADOUpdateSQLForm.ClearValue;
begin
  lstKeyFields.Clear;
  lstUpdateFields.Clear;
  FModifySQL.Clear;
  FInsertSQL.Clear;
  FDeleteSQL.Clear;
  mmoSQLText.Clear;
  btnOK.Enabled := Trim(mmoSQLText.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.btnGetTableFieldsClick(Sender: TObject);
begin
  if cbbTables.Text = '' then
  begin
    MessageDlg('Please select Table Name.', mtInformation, [mbOK], 0);
    Exit;
  end;
  FConnection.GetFieldNames(cbbTables.Text, lstKeyFields.Items);
  lstUpdateFields.Items.Text := lstKeyFields.Items.Text;
  CnListBoxSelectAll(lstKeyFields);
  CnListBoxSelectAll(lstUpdateFields);
end;

procedure TCnADOUpdateSQLForm.btnGenerateSQLClick(Sender: TObject);
var
  i, j: Integer;
  sFieldName, sFieldNames, sValuesParams: AnsiString;
begin
  //����ѡ����������Ϊִ��SQL����where������
  if lstKeyFields.SelCount = 0 then
  begin
    ClearValue;
    MessageDlg('Please specify Key Fields.', mtInformation, [mbOK], 0);
    Exit;
  end;

  //����ѡ��Ҫ���µ��ֶ�
  if lstUpdateFields.SelCount = 0 then
  begin
    MessageDlg('Please specify Update Fields.', mtInformation, [mbOK], 0);
    Exit;
  end;

  //����SQL���
  FModifySQL.Clear;
  FInsertSQL.Clear;
  FDeleteSQL.Clear;
  FModifySQL.Add('UPDATE ' + cbbTables.Text);
  FModifySQL.Add(' SET ');
  FInsertSQL.Add('INSERT INTO ' + cbbTables.Text);
  sFieldNames := '   (';
  sValuesParams := 'VALUES '#13#10'   (';

  //�����ֶ������ֶβ���
  j := 0;
  for i := 0 to lstUpdateFields.Items.Count - 1 do
  begin
    if lstUpdateFields.Selected[i] then
    begin
      sFieldName := {$IFDEF UNICODE}AnsiString{$ENDIF}(lstUpdateFields.Items.Strings[i]);
      if (lstUpdateFields.SelCount - 1) <> j then //����ѡ��������һ��ʱ
      begin
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ',');
        sFieldNames := sFieldNames + sFieldName + ',';
        sValuesParams := sValuesParams + ':' + sFieldName + ',';
      end
      else begin                                  //��ѡ��������һ��ʱ
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
        sFieldNames := sFieldNames + sFieldName;
        sValuesParams := sValuesParams + ':' + sFieldName;
      end;
      Inc(j);
    end;
  end;
  FInsertSQL.Add({$IFDEF UNICODE}String{$ENDIF}(sFieldNames) + ')' + #13 + #10 + {$IFDEF UNICODE}String{$ENDIF}(sValuesParams) + ')');
  FModifySQL.Add(' WHERE ');
  FDeleteSQL.Add('DELETE FROM ' + cbbTables.Text);
  FDeleteSQL.Add(' WHERE ');

  //����Where�����������ֶ�
  j := 0;
  for i := 0 to lstKeyFields.Items.Count - 1 do
  begin
    if lstKeyFields.Selected[i] then
    begin
      sFieldName := {$IFDEF UNICODE}AnsiString{$ENDIF}(lstKeyFields.Items.Strings[i]);
      if (lstKeyFields.SelCount - 1) <> j then //����ѡ��������һ��ʱ
      begin
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' and ');
        FDeleteSQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' and ');
      end
      else begin                              //��ѡ��������һ��ʱ
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
        FDeleteSQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
      end;
      Inc(j);
    end;
  end;

  RadioGroupSQLClick(Sender);
  TabSheetSQL.Show;
end;

procedure TCnADOUpdateSQLForm.RadioGroupSQLClick(Sender: TObject);
begin
  case RadioGroupSQL.ItemIndex of
    0: mmoSQLText.Lines.Text := FModifySQL.Text;
    1: mmoSQLText.Lines.Text := FInsertSQL.Text;
    2: mmoSQLText.Lines.Text := FDeleteSQL.Text;
  end;
end;

procedure TCnADOUpdateSQLForm.cbbTablesChange(Sender: TObject);
begin
  inherited;
  ClearValue;
end;

procedure TCnADOUpdateSQLForm.mmoSQLTextChange(Sender: TObject);
begin
  case RadioGroupSQL.ItemIndex of
    0: FModifySQL.Text := mmoSQLText.Lines.Text;
    1: FInsertSQL.Text := mmoSQLText.Lines.Text;
    2: FDeleteSQL.Text := mmoSQLText.Lines.Text;
  end;
  btnOK.Enabled := Trim(mmoSQLText.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
  begin
    mmoSQLText.Lines.Text := FModifySQL.Text;
  end;
end;

procedure TCnADOUpdateSQLForm.mniSelectAll1Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstKeyFields);
end;

procedure TCnADOUpdateSQLForm.mniClearAll1Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstKeyFields, False);
end;

procedure TCnADOUpdateSQLForm.mniSelectAll2Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstUpdateFields);
end;

procedure TCnADOUpdateSQLForm.mniClearAll2Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstUpdateFields);
end;

function TCnADOUpdateSQLForm.GetConnection: TADOConnection;
begin
  Result := FConnection;
end;

function TCnADOUpdateSQLForm.GetDeleteSQL: TStrings;
begin
  Result := FDeleteSQL;
end;

function TCnADOUpdateSQLForm.GetInsertSQL: TStrings;
begin
  Result := FInsertSQL;
end;

function TCnADOUpdateSQLForm.GetModifySQL: TStrings;
begin
  Result := FModifySQL;
end;

procedure TCnADOUpdateSQLForm.SetConnection(Value: TADOConnection);
begin
  if Assigned(Value) then
    FConnection := Value;
end;

procedure TCnADOUpdateSQLForm.SetDeleteSQL(Value: TStrings);
begin
  FDeleteSQL.Assign(Value);
end;

procedure TCnADOUpdateSQLForm.SetInsertSQL(Value: TStrings);
begin
  FInsertSQL.Assign(Value);
end;

procedure TCnADOUpdateSQLForm.SetModifySQL(Value: TStrings);
begin
  FModifySQL.Assign(Value);
end;

{$ENDIF SUPPORT_ADO}
end.
