{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgDBRes resource constants unit               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{I VG.INC}
{$D-,L-}

unit vgDBRes;

interface

const
  MaxDBStrID           = 50000;

  { Checkers }
  SConfirmDelete       = MaxDBStrID - 1;
  SOpenTableFailed     = MaxDBStrID - 2;
  SEqualStr            = MaxDBStrID - 3;
  SNotNullReqired      = MaxDBStrID - 4;
  SNullReqired         = MaxDBStrID - 5;
  SLengthTooSmall      = MaxDBStrID - 6;
  SIntFieldOutOfRange  = MaxDBStrID - 7;
  SFieldRequired       = MaxDBStrID - 8;
  SNonZeroValueReq     = MaxDBStrID - 9;
  SMoreThanReq         = MaxDBStrID - 10;
  SLessThanReq         = MaxDBStrID - 11;
  SEqualReq            = MaxDBStrID - 12;
  SNotEqualReq         = MaxDBStrID - 13;
  SValueRange          = MaxDBStrID - 14;
  SNumericSequenceReq  = MaxDBStrID - 15;
  SDateSequenceReq     = MaxDBStrID - 16;
  SRecordNotFound      = MaxDBStrID - 17;
  SRecordNotFoundTbl   = MaxDBStrID - 18;
  SInsertNotAvail      = MaxDBStrID - 19;
  SEditNotAvail        = MaxDBStrID - 20;
  SDeleteNotAvail      = MaxDBStrID - 21;

  { DB Exceptions }
  SInvalidUserName     = MaxDBStrID - 30;
  SDeadlock            = MaxDBStrID - 31;
  SKeyViol             = MaxDBStrID - 32;
  SKeyDeleted          = MaxDBStrID - 33;
  SFKViolation         = MaxDBStrID - 34;
  SErrorCodeFmt        = MaxDBStrID - 35;

  { TvgDBMenu captions }
  SDBMenuInsert        = MaxDBStrID - 36;
  SDBMenuEdit          = MaxDBStrID - 37;
  SDBMenuDelete        = MaxDBStrID - 38;
  SDBMenuFirst         = MaxDBStrID - 39;
  SDBMenuLast          = MaxDBStrID - 40;
  SDBMenuRefresh       = MaxDBStrID - 41;

  { ProviderItem errors }
  SGetProviderError    = MaxDBStrID - 48;
  SDuplicateProvNames  = MaxDBStrID - 49;
  SProvNotFound        = MaxDBStrID - 50;

  { DataSetServices }
  SServicesNotFound    = MaxDBStrID - 51;
  SServicesEmpty       = MaxDBStrID - 52;
  SCreateDataSetErr    = MaxDBStrID - 53;
  SCreateDataSetEditErr= MaxDBStrID - 54;

  { RemoteServer }
  SRemoteServerNeeded  = MaxDBStrID - 55;

  { SinglePointServer }
  SProviderListNeeded  = MaxDBStrID - 56;

  SInvalidDatabase     = MaxDBStrID - 57;
  SMissingDatabase     = MaxDBStrID - 58;
  SUnknownDatabase     = MaxDBStrID - 59;
  SDupProviderName     = MaxDBStrID - 60;
  SDBFactoryMissing    = MaxDBStrID - 61;
  SNoDataModule        = MaxDBStrID - 62;
  SInvalidDatabaseName = MaxDBStrID - 63;

implementation

{$R *.RES}

end.