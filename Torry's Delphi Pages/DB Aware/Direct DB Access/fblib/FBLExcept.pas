{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLExcept.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

{$I fbl.inc}

{
@abstract(Fblib runtime exceptions)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLExcept.pas unit provides managing runtime exception
}

unit FBLExcept;


interface

uses
  SysUtils, ibase_h, FBLConst;

type
  {@abstract(encapsulates the properties and methods for fblib exceptions)}
  EFBLError = class(Exception)
  private
    FSQLCode: Long;
    FError_code: Long;
  public
    {Instantiates an instance of an EFBLError with a simple message string, SQLCode and Fb Isc_ErrorCode}
    constructor Create(ASQLCode: Long; AErrorCode: Long; AMsg: string); overload;
    {Instantiates an instance of an EFBLError with a formatted message string}
    constructor CreateFmt(AMsg: string; const AArgs: array of const); overload;
    {Instantiates an instance of an EFBLError with a simple message string}
    constructor Create(AMsg: string); overload;
    {SqlCode error see Firebird Manual for more info}
    property SQLCode: Long read FSQLCode write FSQLCode;
    {isc_errorcode firebird server error see firebird manual , all error code are definited in iberror_h.pas}
    property ISC_ErrorCode: Long read FError_Code;
  end;

  {Status vector is a vector of integer value for managing error status in firebird api calls }
  ISC_STATUS_VECTOR = array[0..19] of ISC_STATUS;
  {pointer to @link(ISC_STATUS_VECTOR)}
  PSTATUS_VECTOR = ^ISC_STATUS_VECTOR;
{Raise an EFBLError with a simple message string}
procedure FBLError(AErr: string; const AArgs: array of const); overload;
{Raise an EFBLError with a format message string}
procedure FBLError(AErr: string); overload;
{@exclude}
procedure FBLShowError(AStatusVectorPtr: PSTATUS_VECTOR);

implementation

procedure FBLShowError(AStatusVectorPtr: PSTATUS_VECTOR);
var
  MsgBuffer: array[0..511] of char;
  ErrMsg: string;
  LastMsg: string;
  ErrCode: ISC_STATUS;
  SQLCode: ISC_LONG;
  IbErr: LongInt;
begin
  ErrMsg := '';
  LastMsg := '';
  IbErr := AStatusVectorPtr[1];
  SQLCode := isc_sqlcode(@AStatusVectorPtr);
  repeat
    ErrCode := isc_interprete(@msgBuffer, @AStatusVectorPtr);
    if LastMsg <> string(MsgBuffer) then
    begin
      LastMsg := string(MsgBuffer);
      if Length(ErrMsg) <> 0 then
        ErrMsg := ErrMsg + NEW_LINE;
      ErrMsg := ErrMsg + LastMsg;
    end;
  until ErrCode = 0;
  raise EFBLError.Create(SQLCode, IbErr, ErrMsg);
end;

//EFBLEerror -------------------------------------------------------------------

constructor EFBLError.Create(ASQLCode: Long; AErrorCode: Long; AMsg: string);
begin
  inherited Create(AMsg);
  FSQLCode := ASQLCode;
  FError_Code := AErrorCode;
end;

//------------------------------------------------------------------------------

constructor EFBLError.CreateFmt(AMsg: string; const AArgs: array of const);
begin
  inherited CreateFmt(AMsg, AArgs);
  FSQLCode := 0;
  FError_Code := 0;
end;

//------------------------------------------------------------------------------

constructor EFBLError.Create(AMsg: string);
begin
  inherited Create(AMsg);
  FSQLCode := 0;
  FError_Code := 0;;
end;

//------------------------------------------------------------------------------

procedure FBLError(AErr: string; const AArgs: array of const);
begin
  raise EFBLError.CreateFmt(AErr, AArgs);
end;

//------------------------------------------------------------------------------

procedure FBLError(AErr: string);
begin
  raise EFBLError.Create(AErr);
end;

//------------------------------------------------------------------------------

end.
