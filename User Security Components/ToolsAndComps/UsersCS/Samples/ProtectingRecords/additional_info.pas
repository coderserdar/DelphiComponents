unit additional_info;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, Grids, DBGrids, DBCtrls, ExtCtrls, Buttons, users_basic;

type
  TfrmAdditionalInfo = class(TForm)
    ThePanel: TPanel;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    Panel2: TPanel;
    dbUserName: TDBText;
    Panel3: TPanel;
    Label1: TLabel;
    DBGrid1: TDBGrid;
    qryUser: TQuery;
    qryUserUSERCS_NAME: TStringField;
    qryUserADDITIONAL_INFO: TStringField;
    qryUserUSER_ID: TFloatField;
    qryUserAPP_KEY: TStringField;
    dtsUser: TDataSource;
    updtUser: TUpdateSQL;
    qryCompanies: TQuery;
    qryCompaniesCompanyName: TStringField;
    qryCompaniesCompanyId: TIntegerField;
    qryCompaniesVisible: TStringField;
    dtsCompanies: TDataSource;
    UpdateSQL1: TUpdateSQL;
    Panel1: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure qryCompaniesVisibleGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure DBGrid1CellClick(Column: TColumn);
  private
    { Private declarations }
  public
    { Public declarations }
    GetActualUserId: TGetActualUserId;
    AppKey: String;
  end;

var
  frmAdditionalInfo: TfrmAdditionalInfo;

implementation

{$R *.DFM}

procedure TfrmAdditionalInfo.FormActivate(Sender: TObject);
var
  AdditionalInfo: String;
  User_Id: Integer;
begin
  if qryUser.Active then
    Exit;
  if Assigned(GetActualUserId) then
    GetActualUserId(User_Id);
  qryUser.ParamByName('USER_ID').AsInteger:=User_Id;
  qryUser.ParamByName('APP_KEY').AsString:=AppKey;
  qryUser.Open;
  AdditionalInfo:=Copy(qryUser.FieldByName('ADDITIONAL_INFO').AsString,Length('Company_Id not in (-1,')+1,Length(qryUser.FieldByName('ADDITIONAL_INFO').AsString)-Length('not Company_Id in (-1,')+1);
  qryCompanies.Open;
  qryCompanies.DisableControls;
  while not qryCompanies.EOF do
    begin
      if Pos(qryCompanies.FieldByName('Company_Id').AsString,AdditionalInfo)=0 then
        begin
          qryCompanies.Edit;
          qryCompaniesVisible.Value := '1';
          qryCompanies.Post;
        end;
      qryCompanies.Next;
    end;
  qryCompanies.EnableControls;
end;

procedure TfrmAdditionalInfo.FormDeactivate(Sender: TObject);
var
  Filter: String;
  Checked: Boolean;
begin
  qryCompanies.First;
  Filter:='not Company_Id in (-1,';
  while not qryCompanies.EOF do
    begin
      Checked:=qryCompaniesVisible.Value <> '0';
      if not Checked then
        Filter:=Filter+qryCompanies.FieldByName('Company_Id').AsString+',';
      qryCompanies.Next;
    end;
  Filter[Length(Filter)]:=')';
  if (qryUser.FieldByName('ADDITIONAL_INFO').AsString<>Filter) then
    begin
      if not (qryUser.State = dsEdit) then
        qryUser.Edit;
      qryUser.FieldByName('ADDITIONAL_INFO').AsString:=Filter;
      qryUser.Post;
      if qryUser.Database.InTransaction then
        begin
          qryUser.ApplyUpdates;
          qryUser.CommitUpdates;
        end
      else
        qryUser.Database.ApplyUpdates([qryUser]);
    end;
  qryUser.Close;
  qryCompanies.Close;
end;

procedure TfrmAdditionalInfo.qryCompaniesVisibleGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  if Sender.Value='0' then
    Text:=''
  else
    Text:='a';
end;

procedure TfrmAdditionalInfo.DBGrid1CellClick(Column: TColumn);
begin
  if Column.Index=1 then
    begin
      qryCompanies.Edit;
      if qryCompaniesVisible.Value = '0' then
        qryCompaniesVisible.Value := '1'
      else
        qryCompaniesVisible.Value := '0';
      qryCompanies.Post;
    end;
end;

end.
