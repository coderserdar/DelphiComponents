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

unit CnPODOFormMain;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate PODO ���ɹ���
* ��Ԫ���ƣ�PODO ���ɹ���
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnPODOFormMain.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdoConEd, StdCtrls, DB, ADODB, CnPODOConsts, ComCtrls, CnPODOUtils,
  CnDHibernateAbout;

type
  TfrmPodoMain = class(TForm)
    conn: TADOConnection;
    table: TADOTable;
    lblDB: TLabel;
    edtDB: TEdit;
    btnConn: TButton;
    lblTblName: TLabel;
    cbTableName: TComboBox;
    lblTblInfo: TLabel;
    lvInfo: TListView;
    lblPODO: TLabel;
    btnGenerate: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    sdPODO: TSaveDialog;
    mmPodo: TMemo;
    procedure btnConnClick(Sender: TObject);
    procedure cbTableNameSelect(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { get data field and type }
    procedure getFields;
    { generate podo file preview according to fields }
    procedure generatePODO;
  public
    { Public declarations }
  end;

var
  frmPodoMain: TfrmPodoMain;

implementation

{$R *.dfm}

procedure TfrmPodoMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPodoMain.btnConnClick(Sender: TObject);
begin
  conn.Close;
  // edtDB.Clear;
  cbTableName.Items.Clear;
  AdoConEd.EditConnectionString(Self.conn);
  edtDB.Text := conn.ConnectionString;
  try
    conn.Open;
    conn.GetTableNames(cbTableName.Items);
    MessageBox(Handle, PODO_CONNECT_SUCCESS, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  except
    MessageBox(Handle, PODO_CONNECT_FAIL, PODO_MSGBOX_TITLE, MB_OK or MB_ICONERROR);
  end;
end;

procedure TfrmPodoMain.btnGenerateClick(Sender: TObject);
begin
  // save podo file
  sdPODO.Filter := Format(FILTER_PODO, [Format(FILTER_FILE_NAME, [UpperCase(DeleteSpaces(cbTableName.Text))])]);
  sdPODO.FileName := sdPODO.Filter;
  if not sdPODO.Execute then
    Exit;
  try
    mmPODO.Lines.SaveToFile(sdPODO.FileName);
    MessageBox(Handle, PODO_SAVE_SUCCESS, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  except
    MessageBox(Handle, PODO_SAVE_FAIL, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  end;
end;

procedure TfrmPodoMain.btnHelpClick(Sender: TObject);
begin
  with TCnFormDHibernateAbout.Create(application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfrmPodoMain.cbTableNameSelect(Sender: TObject);
begin
  table.Close;
  lvInfo.Items.Clear;
  mmPODO.Lines.Clear;
  if cbTableName.Text <> EmptyStr then
  begin
    table.TableName := cbTableName.Text;
    table.Open;
    getFields;
    generatePODO;
  end;
end;

procedure TfrmPodoMain.generatePODO;
var
  i: Integer;
  tableName: string;
  fieldName: string;
begin
  // TODO : generate podo file preview
  // delete the spaces in table name
  tableName := DeleteSpaces(cbTableName.Text);
  with mmPODO.Lines do
  begin
    // comment
    Append(PREVIEW_UNIT_HEAD_COMMENT);
    Append(EmptyStr);
    // unit %s;
    Append(Format(PREVIEW_UNIT_NAME, [UpperCase(tableName)]));
    Append(EmptyStr);
    // {$M+}
    Append(PREVIEW_UNIT_MPLUS);
    Append(EmptyStr);
    // interface
    Append(PREVIEW_UNIT_INTERFACE);
    Append(EmptyStr);
    // uses
    Append(PREVIEW_UNIT_USES);
    // Classes, SysUtils, DHibernateBase;
    Append(PREVIEW_UNIT_BASE_UNIT);
    Append(EmptyStr);
    // type
    Append(PREVIEW_UNIT_TYPE);
    // T%s = class(TDHibernateBase)
    Append(Format(PREVIEW_UNIT_CLASS_NAME, [UpperCaseFirst(tableName)]));
    // private
    Append(PREVIEW_UNIT_PRIVATE);
    // F%s : %s
    for i := 0 to lvInfo.Items.Count - 1 do
    begin
      fieldName := UpperCaseFirst(DeleteSpaces(lvInfo.Items[i].Caption));
      Append(Format(PREVIEW_UNIT_PRIVATE_ATTR, [fieldName, lvInfo.Items[i].SubItems[0]]));
    end;
    // published
    Append(PREVIEW_UNIT_PUBLISHED);
    // property %s : %s read F%s write F%s;
    for i := 0 to lvInfo.Items.Count - 1 do
    begin
      fieldName := UpperCaseFirst(DeleteSpaces(lvInfo.Items[i].Caption));
      Append(Format(PREVIEW_UNIT_PUBLISHED_ATTR, [fieldName, lvInfo.Items[i].SubItems[0], fieldName, fieldName]));
    end;
    // end;
    Append(PREVIEW_UNIT_END);
    Append(EmptyStr);
    // implementation
    Append(PREVIEW_UNIT_IMPLEMENTATION);
    Append(EmptyStr);
    // initialization
    Append(PREVIEW_UNIT_INITIALIZATION);
    // registerClass
    Append(Format(PREVIEW_UNIT_REGISTER_CLASS, [UpperCaseFirst(tableName)]));
    Append(EmptyStr);
    // end.
    Append(PREVIEW_UNIT_FULL_END);
  end;
end;

procedure TfrmPodoMain.getFields;
var
  i: Integer;
  item: TListItem;
begin
  for i := 0 to table.FieldCount - 1 do
  begin
    item := lvInfo.Items.Add;
    item.Caption := table.Fields[i].FieldName;
    item.SubItems.Add(DataTypeToString(table.Fields[i].DataType));
  end;
end;

end.

