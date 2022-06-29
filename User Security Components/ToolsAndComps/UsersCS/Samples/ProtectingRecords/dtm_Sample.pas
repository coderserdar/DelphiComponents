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

end.
