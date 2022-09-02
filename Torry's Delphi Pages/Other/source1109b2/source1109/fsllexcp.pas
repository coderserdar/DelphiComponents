{$I fsdefine.inc}

Unit fsllexcp;

Interface

Uses
  SysUtils,
  fsconst,
  fsllbase,
  fssrmgr;

Var
  fsStrResGeneral: TfsStringResource; {in FFLLCNST.RC}
  fsStrResBDE: TfsStringResource;

  {===FlashFiler exception classes===}
Type
  {..the ancestor..}
  EfsException = Class(Exception)
  Private
    FErrorCode: Integer;
  Public
    Constructor CreateEx(StrRes: TfsStringResource;
      ErrorCode: Integer;
      Const ExtraData: Array Of Const);
    Constructor CreateNoData(StrRes: TfsStringResource;
      ErrorCode: Integer);
    Property ErrorCode: Integer
      Read FErrorCode;
  End;
  TfsExceptionClass = Class Of EfsException;

  {..the communications class exceptions..}
  EfsCommsException = Class(EfsException);

  {..the server exception..}
  EfsServerException = Class(EfsException);

  {..the client exception..}
  EfsClientException = Class(EfsException);

  {..the BDE exception..}
  EfsBDEException = Class(EfsException);

  {---Exception raising---}
Procedure FSRaiseException(ExceptionClass: TfsExceptionClass;
  StringRes {ource}: TfsStringResource; {!!.10}
  {conflict with StringResource directive fools some
   source parsing tools}
  ErrorCode: Integer;
  Const ExtraData: Array Of Const);
{-Raise an exception. ErrorCode is the Filer error code, ExtraData
  is an array of const values defining the extra data required by
  the error code's string resource}
Procedure FSRaiseExceptionNoData(ExceptionClass: TfsExceptionClass;
  StringRes {ource}: TfsStringResource; {!!.10}
  {conflict with StringResource directive fools some
   source parsing tools}
  ErrorCode: Integer);
{-Raise an exception. ErrorCode is the Filer error code}

Implementation

{===Filer exception generator========================================}

Constructor EfsException.CreateEx(StrRes: TfsStringResource;
  ErrorCode: Integer;
  Const ExtraData: Array Of Const);
  var
  Msg: String;
Begin
  FErrorCode := ErrorCode;
  Case ErrorCode Of
    50100:
      Begin
        Msg := 'FSSQL: Invalid password given.';
        Inherited CreateFmt(Msg, ExtraData);
      End;
      50101:
          Begin
            Msg := 'FSSQL: Invalid ID database.';
           Inherited CreateFmt(Msg, ExtraData);
          End;
    Else
      Begin
        Inherited CreateFmt(StrRes[ErrorCode], ExtraData);
      End;
  End;
End;
{--------}

Constructor EfsException.CreateNoData(StrRes: TfsStringResource;
  ErrorCode: Integer);
Begin
  Inherited Create(StrRes[ErrorCode]);
  FErrorCode := ErrorCode;
End;
{--------}

Procedure FSRaiseException(ExceptionClass: TfsExceptionClass;
  StringRes {ource}: TfsStringResource; {!!.10}
  ErrorCode: Integer;
  Const ExtraData: Array Of Const);
Begin
  Raise ExceptionClass.CreateEx(StringRes {ource}, ErrorCode, ExtraData) {!!.10}
End;
{--------}

Procedure FSRaiseExceptionNoData(ExceptionClass: TfsExceptionClass;
  StringRes {ource}: TfsStringResource; {!!.10}
  ErrorCode: Integer);
Begin
  Raise ExceptionClass.CreateNoData(StringRes {ource}, ErrorCode); {!!.10}
End;
{====================================================================}

Procedure FinalizeUnit;
Begin
  fsStrResGeneral.Free;
  fsStrResBDE.Free;
End;

Procedure InitializeUnit;
Begin
  fsStrResGeneral := Nil;
  fsStrResBDE := Nil;
  fsStrResGeneral := TfsStringResource.Create(hInstance, 'FS_GENERAL_STRINGS');
  fsStrResBDE := TfsStringResource.Create(hInstance, 'FS_BDE_ERROR_STRINGS');
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

