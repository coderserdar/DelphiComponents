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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: XpExcept.PAS                              *}
{*********************************************************}
{* XMLPartner: XMLPartner's exception classes            *}
{*********************************************************}

{$I XpDefine.inc}

unit XpExcept;

interface

uses
  SysUtils,
  XpBase;

{Include error strings}
{$I XpExcept.inc}

type
  EXpException = class(Exception);

  EXpStreamError = class(EXpException)
  private
    seFilePos : Longint;
  public
    constructor CreateError(const FilePos : Longint;
                            const Reason  : DOMString);

    property FilePos : Longint
       read seFilePos;
  end;

  EXpFilterError = class(EXpStreamError)
  private
    feReason  : DOMString;
    feLine    : Longint;
    feLinePos : Longint;
  public
    constructor CreateError(const FilePos, Line, LinePos : Longint;
                            const Reason : DOMString);

    property Reason : DOMString
       read feReason;
    property Line : Longint
       read feLine;
    property LinePos : Longint
       read feLinePos;
  end;

  EXpParserError = class(EXpFilterError)
  protected
    peURL : DOMString;
  public
    constructor CreateError({const FilePos,} Line, LinePos : Longint;
                            const Url, Reason : DOMString);

    property URL : DOMString
       read peURL;
  end;

implementation

constructor EXpStreamError.CreateError(const FilePos : Longint;
                                       const Reason  : DOMString);
begin
  inherited Create(Reason);

  seFilePos := FilePos;
end;
{******************************************************************************}
constructor EXpFilterError.CreateError(const FilePos,
                                             Line,
                                             LinePos : Longint;
                                       const Reason  : DOMString);
begin
  inherited CreateError(FilePos, Reason);

  feLine := Line;
  feLinePos := LinePos;
  feReason := Reason;
end;
{******************************************************************************}
constructor EXpParserError.CreateError(Line,
                                       LinePos : Longint;
                                 const Url,
                                       Reason  : DOMString);
begin
  inherited CreateError(FilePos, Line, LinePos, Reason);
  peUrl := Url;
end;

end.
