// This demo project shows, how to use NCOCI8 for building MIDAS server.
// It is standard procedure, with 3 exceptions ...
//
// 1) Because NCOCI8 uses for binding dataset with database DatabaseName
// property, NCOCI8 has the same problem as BDE. When new exemplar of
// remote datamodule is building, and it have TOCIDatabase, then all
// TOCIDataSet from it will be bind to TOCIDatabase not from the same
// datamodule, but to TOCIDatabase from FIRST datamodule. BDE's TSession
// has property AutoSessionName, if it is True, then TSession will handle
// this problem automatically - it will rename SessionName's in new
// datamodule.
//
// NCOCI8 has two choices for you:
// - use global variable NCOciDB.FOCIDatabaseNameExpandMode. If it equal
//   to deUseThread, then components, that are living in different THREADS
//   will have different DatabaseName's. If it equal deUseOwner, then
//   components, that are living in different DATAMODULE's, will have
//   different DatabaseName's.
// - use explicit control for how NCOCI8 will rename DatabaseName.
//   You can include in DatabaseName special symbols, that will be expanded
//   %T - thread id
//   %O - owner address
// The easiest way is to set FOCIDatabaseNameExpandMode := deUseThread in
// initialization section of remote datamodule. It is here :)
//
// 2) You must set TDataSetProvider.ResolveToDataSet = True allways.
// False does not work. This issue will exist until TOCIQuery will
// have TableName property.
//
// 3) You must set TOCIDatabase.SilentMode = True allways.
// False does not work.
 
unit SrvDM;

interface

uses
  Windows, Messages, SysUtils, Classes, ComServ, ComObj, VCLCom, DataBkr,
  DBClient, Srv_TLB, StdVcl, Db, NCOciDB, NCOci, NCOciWrapper, Provider;

type
  TNCOCI8MidasTest = class(TRemoteDataModule, INCOCI8MidasTest)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    DataSetProvider1: TDataSetProvider;
  private
    { Private declarations }
  protected
    class procedure UpdateRegistry(Register: Boolean; const ClassID, ProgID: string); override;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

class procedure TNCOCI8MidasTest.UpdateRegistry(Register: Boolean; const ClassID, ProgID: string);
begin
  if Register then
  begin
    inherited UpdateRegistry(Register, ClassID, ProgID);
    EnableSocketTransport(ClassID);
    EnableWebTransport(ClassID);
  end else
  begin
    DisableSocketTransport(ClassID);
    DisableWebTransport(ClassID);
    inherited UpdateRegistry(Register, ClassID, ProgID);
  end;
end;

initialization
  // !!! it is important for proper functionality of server !!!
  FOCIDatabaseNameExpandMode := deUseThread;
  TComponentFactory.Create(ComServer, TNCOCI8MidasTest,
    Class_NCOCI8MidasTest, ciMultiInstance, tmApartment);
end.
