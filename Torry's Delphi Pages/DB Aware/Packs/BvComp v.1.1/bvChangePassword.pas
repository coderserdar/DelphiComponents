unit bvChangePassword;

interface

{$ifdef LINUX}
 ERROR: not compatible with LINUX (dbtables)
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, ExtCtrls, Buttons,bvLocalization;

type
  TChangePasswordForm = class(TForm)
    LabAlias: TLabel;
    EditAlias: TComboBox;
    LabTable: TLabel;
    EditTableName: TComboBox;
    Table: TTable;
    BitBtnChangePassword: TBitBtn;
    Bevel1: TBevel;
    LabNewPassword: TLabel;
    EditPassword: TEdit;
    LabMaskOfPassword: TLabel;
    LabConfirmPassword: TLabel;
    EditPassword1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure EditAliasChange(Sender: TObject);
    procedure BitBtnChangePasswordClick(Sender: TObject);
    procedure EditTableNameChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetAliasEnabled(Value:boolean);
    function GetAliasEnabled:boolean;
  public
    { Public declarations }
    function ChangePassword:boolean;
    property AliasEnabled:boolean read GetAliasEnabled write SetAliasEnabled;
  end;

var
  ChangePasswordForm: TChangePasswordForm;


implementation

uses bde,bvMessageUnit;

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TChangePasswordForm.SetAliasEnabled(Value:boolean);
begin
  LabAlias.Enabled:=Value;
  EditAlias.Enabled:=value;
end;

function TChangePasswordForm.GetAliasEnabled:boolean;
begin
  Result:=EditAlias.Enabled;
end;

function TChangePasswordForm.changePassword:boolean;
var
  hDb: hDbiDb;
  TblDesc: CRTblDesc;
  Dir: string;
  OpType: CROpType;
begin
  Result:=false;
  if Editpassword.text<>EditPassword1.text
  then bvMessage(StrErrorForConfirmPasswords)
  else begin

    table.active:=true;
    SetLength(Dir, dbiMaxNameLen + 1);
    FillChar(TblDesc, sizeof(CRTblDesc), #0);
    Check(DbiGetDirectory(table.DBHandle, False, PChar(Dir)));
    SetLength(Dir, StrLen(PChar(Dir)));
    table.active:=false;
    try

      Check(DbiOpenDatabase(nil, nil, dbiReadWrite, dbiOpenExcl, nil, 0, nil, nil,
        hDb));
      Check(DbiSetDirectory(hDb, PChar(Dir)));

  //    if trim(Editpassword.text)='' then optype:=crDROP
      {else} optype:=crdropadd;

      fillchar(tbldesc,sizeof(crtbldesc),#0);
      tbldesc.bProtected:=not (trim(Editpassword.text)='');
      tbldesc.pOptData:=@optype;
      strpcopy(tbldesc.szPassword,trim(Editpassword.Text));
      StrPCopy(tbldesc.szTblName, TABLE.tableName);
      StrCopy(tbldesc.szTblType, szParadox);

  //    check(DbiDoRestructure( thDatabase.Handle,1,@desc,nil,nil,nil,false));

      Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
      Result:=true;
    finally
      Check(DbiCloseDatabase(hDb));
    end;

  // check(DbiDoRestructure( dmKassa.dbKassa.Handle,1,@desc,nil,nil,nil,false));
  end;
end;


procedure TChangePasswordForm.FormCreate(Sender: TObject);
begin
   LabAlias.caption:=StrBDEAlias;
   LabTable.caption:=StrTable;
   LabNewPassword.caption:=StrNewPassword;
   LabMaskofpassword.caption:=StrMaskOfPassword;
   LabConfirmPassword.caption:=StrConfirmPassword;
   BitBtnChangePassword.caption:=StrChangepassword;
   self.caption:=StrChangePasswordForm;

   EditAlias.Items.Clear;
   Session.GetAliasNames(EditAlias.Items);
   EditAlias.ItemIndex:=0;
end;

procedure TChangePasswordForm.EditAliasChange(Sender: TObject);
begin
  EditTableName.Items.Clear;
  if (trim(EditAlias.text)<>'') and Session.IsAlias(editalias.text)
  then   begin
    Table.Active:=false;
    table.databaseName:=Editalias.text;
    Session.GetTableNames( EditAlias.text,'',true,true,EditTableName.Items);
  end;
end;

procedure TChangePasswordForm.BitBtnChangePasswordClick(Sender: TObject);
begin
   if changePassword then bvMessage(StrOkOperation);
end;

procedure TChangePasswordForm.EditTableNameChange(Sender: TObject);
begin
  Table.active:=false;
  table.tableName:=EditTableName.text;
end;

end.
