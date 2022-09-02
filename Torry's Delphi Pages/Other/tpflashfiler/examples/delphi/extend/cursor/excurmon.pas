{*********************************************************}
{* FlashFiler: Example Cursor Monitor & Extender         *}
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
 
unit ExCurMon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FFLLBase, ffllcomp, FFLLEng;

type
  TffCursorMonitor = class(TffBaseEngineMonitor)
  protected
    procedure bemSetServerEngine(anEngine : TffBaseServerEngine); override;
  public
    function Interested(aServerObject : TffObject) : TffBaseEngineExtender; override;
  end;

  TffCursorExtender = class(TffBaseEngineExtender)
  protected
  public
    constructor Create(aOwner : TffBaseEngineMonitor); override;

    function Notify(serverObject : TffObject;
                    action : TffEngineAction) : TffResult; override;
  end;

procedure Register;

implementation

uses
  FFSrBDE,
  FFSrEng;

{===TffCursorMonitor=================================================}
procedure TffCursorMonitor.bemSetServerEngine(anEngine : TffBaseServerEngine);
begin
  inherited;
  AddInterest(TffSrBaseCursor);
end;
{--------}
function TffCursorMonitor.Interested(aServerObject : TffObject) : TffBaseEngineExtender;
begin
  Result := nil;
  if (aServerObject is TffSrBaseCursor) then
    Result := TffCursorExtender.Create(Self);
end;
{====================================================================}

{===TffCursorExtender================================================}
constructor TffCursorExtender.Create(aOwner : TffBaseEngineMonitor);
begin
  inherited;
  FActions := [ffeaAfterRecDelete, ffeaAfterRecInsert, ffeaAfterRecUpdate];
end;
{--------}
function TffCursorExtender.Notify(serverObject : TffObject;
                                  action : TffEngineAction) : TffResult;
begin
  Result := DBIERR_NONE;
  if serverObject is TffSrBaseCursor then
    case action of
      ffeaAfterRecDelete :
        begin
          beep;
          sleep(100);
          beep;
          sleep(100);
          beep;
        end;
      ffeaAfterRecInsert :
        beep;
      ffeaAfterRecUpdate :
        begin
          beep;
          sleep(200);
          beep;
        end;
    end;  { case }
end;
{====================================================================}

procedure Register;
begin
  RegisterComponents('FlashFiler Examples', [TffCursorMonitor]);
end;

end.
