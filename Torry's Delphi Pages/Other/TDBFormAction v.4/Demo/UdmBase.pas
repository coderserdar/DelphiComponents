{*********************************************************************}
{ Dynamic load library for Loader (http://visualdesigner.fatal.ru/)   }
{            (C) Copyright 2006 SVD, Pfaffenrot S.                    }
{*********************************************************************}

unit UdmBase;

interface

uses
  SysUtils, Classes, DBTables, DB;

type
  TdmBase = class(TDataModule)
    Database1: TDatabase;
    Session1: TSession;
    tbCustomer: TTable;
    tbCountry: TTable;
    tbVendors: TTable;
  published
    {please don't remove this declaration}
    class function CreateNecessary: boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmBase: TdmBase;

implementation

{$R *.dfm}

{ TDataModule1 }

{please don't remove this declaration}
class function TdmBase.CreateNecessary: boolean;
begin
  Result := True;
end;
{-------------------------------------}



{please don't remove this declaration}
initialization
  RegisterClass(TdmBase);


end.
