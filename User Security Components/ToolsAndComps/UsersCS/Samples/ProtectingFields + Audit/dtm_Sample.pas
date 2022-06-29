unit dtm_Sample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, users_basic, users_cs;

type
  TSampleData = class(TDataModule)
    Companies: TQuery;
    dtsCompanies: TDataSource;
    CompaniesCompany_Name: TStringField;
    CompaniesCompany_Id: TIntegerField;
    CompaniesCompany_Address: TStringField;
    CompaniesCompany_Phone: TStringField;
    CompaniesCompany_FAX: TStringField;
    CompaniesCompanyUserName: TStringField;
    UsersCSReg1: TUsersCSReg;
    procedure CompaniesBeforePost(DataSet: TDataSet);
    procedure SampleDataCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SampleData: TSampleData;

implementation

uses main;

{$R *.DFM}

procedure TSampleData.CompaniesBeforePost(DataSet: TDataSet);
begin
  if CompaniesCOMPANY_NAME.Value<>CompaniesCOMPANY_NAME.OldValue then
    UsersCSReg1.Audit(CompaniesCOMPANY_NAME.Name,'Name Change','Old Name: '+CompaniesCOMPANY_NAME.OldValue+#10#13+'New Name: '+CompaniesCOMPANY_NAME.Value);
end;

procedure TSampleData.SampleDataCreate(Sender: TObject);
begin
  Companies.Open
end;

end.
