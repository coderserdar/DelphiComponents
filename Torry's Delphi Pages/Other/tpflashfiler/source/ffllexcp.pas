{*********************************************************}
{* FlashFiler: FlashFiler exceptions                     *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllexcp;

interface

uses
  SysUtils,
  ffconst,
  ffllbase,
  ffsrmgr;

var
  ffStrResGeneral : TffStringResource; {in FFLLCNST.RC}
  ffStrResBDE : TffStringResource;                       


{===FlashFiler exception classes===}
type
  {..the ancestor..}
  EffException = class(Exception)
    private
      FErrorCode : integer;
    public
      constructor CreateEx(StrRes    : TffStringResource;
                           ErrorCode : integer;
                     const ExtraData : array of const);
      constructor CreateNoData(StrRes    : TffStringResource;
                               ErrorCode : integer);
      property ErrorCode : integer
         read FErrorCode;
  end;
  TffExceptionClass = class of EffException;

  {..the communications class exceptions..}
  EffCommsException = class(EffException);

  {..the server exception..}
  EffServerException = class(EffException);

  {..the client exception..}
  EffClientException = class(EffException);

  {..the BDE exception..}
  EffBDEException = class(EffException);


{---Exception raising---}
procedure FFRaiseException(ExceptionClass : TffExceptionClass;
                           StringRes{ource} : TffStringResource;       {!!.10}
                           {conflict with StringResource directive fools some
                            source parsing tools}
                           ErrorCode      : integer;
                     const ExtraData      : array of const);
  {-Raise an exception. ErrorCode is the Filer error code, ExtraData
    is an array of const values defining the extra data required by
    the error code's string resource}
procedure FFRaiseExceptionNoData(ExceptionClass : TffExceptionClass;
                                 StringRes{ource} : TffStringResource; {!!.10}
                                 {conflict with StringResource directive fools some
                                  source parsing tools}
                                 ErrorCode      : integer);
  {-Raise an exception. ErrorCode is the Filer error code}

implementation

{===Filer exception generator========================================}
constructor EffException.CreateEx(StrRes    : TffStringResource;
                                  ErrorCode : integer;
                            const ExtraData : array of const);
begin
  inherited CreateFmt(StrRes[ErrorCode], ExtraData);
  FErrorCode := ErrorCode;
end;
{--------}
constructor EffException.CreateNoData(StrRes    : TffStringResource;
                                      ErrorCode : integer);
begin
  inherited Create(StrRes[ErrorCode]);
  FErrorCode := ErrorCode;
end;
{--------}
procedure FFRaiseException(ExceptionClass : TffExceptionClass;
                           StringRes{ource} : TffStringResource;       {!!.10}
                           ErrorCode      : integer;
                     const ExtraData      : array of const);
begin
  raise ExceptionClass.CreateEx(StringRes{ource}, ErrorCode, ExtraData) {!!.10}
end;
{--------}
procedure FFRaiseExceptionNoData(ExceptionClass : TffExceptionClass;
                                 StringRes{ource} : TffStringResource; {!!.10}
                                 ErrorCode      : integer);
begin
  raise ExceptionClass.CreateNoData(StringRes{ource}, ErrorCode);      {!!.10}
end;
{====================================================================}

procedure FinalizeUnit;
begin
  ffStrResGeneral.Free;
  ffStrResBDE.Free;
end;

procedure InitializeUnit;
begin
  ffStrResGeneral := nil;
  ffStrResBDE := nil;
  ffStrResGeneral := TffStringResource.Create(hInstance, 'FF_GENERAL_STRINGS');
  ffStrResBDE := TffStringResource.Create(hInstance, 'FF_BDE_ERROR_STRINGS');
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
