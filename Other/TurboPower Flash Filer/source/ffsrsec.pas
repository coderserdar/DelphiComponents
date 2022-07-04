{*********************************************************}
{* FlashFiler: Client Security Monitor/Extender          *}
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

unit ffsrsec;

interface

uses
  SysUtils,
  fflleng,
  ffllbase,
  ffsreng,
  ffsrbase,
  ffsrbde;

type
  TffSecurityExtender = class(TffBaseEngineExtender)
  public
    constructor Create(aOwner : TffBaseEngineMonitor); override;
    function  Notify(aServerObject : TffObject;
                      aAction      : TffEngineAction) : TffResult; override;
  protected
  end;

  TffSecurityMonitor = class(TffBaseEngineMonitor)
  public
    function  Interested(aServerObject : TffObject) : TffBaseEngineExtender; override;
  protected
    procedure bemSetServerEngine(anEngine : TffBaseServerEngine); override;
  end;

implementation

uses
  ffsrcvex;

{===TffSecurityMonitor===============================================}
function TffSecurityMonitor.Interested(aServerObject : TffObject) : TffBaseEngineExtender;
{Rewritten !!.11}
begin
  { This should always be a TffSrBaseCursor, TffSrClient or TffDatabase,
    but we need to check to be sure. }
  Result := nil;
  if (aServerObject is TffSrBaseCursor) then begin
    { Do not create extenders for temporary files. Temporary files are
      created by the SQL engine when it needs to build a result table. }
    if not (fffaTemporary in
            TffSrBaseCursor(aServerObject).Table.Files[0].fiAttributes) then
      Result := TffSecurityExtender.Create(Self);
  end
  else if (aServerObject is TffSrDatabase) or
          (aServerObject is TffSrClient) then
    Result := TffSecurityExtender.Create(Self);
end;
{--------}
procedure TffSecurityMonitor.bemSetServerEngine(anEngine : TffBaseServerEngine);
begin
  inherited bemSetServerEngine(anEngine);
  AddInterest(TffSrClient);
  AddInterest(TffSrDatabase);
  AddInterest(TffSrBaseCursor);
end;
{====================================================================}

{===TffSecurityExtender==============================================}
constructor TffSecurityExtender.Create(aOwner: TffBaseEngineMonitor);
begin
  inherited Create(aOwner);
  FActions := [ffeaBeforeAddInx,
               ffeaBeforeBLOBCreate,
               ffeaBeforeBLOBDelete,
               ffeaBeforeBLOBGetLength,
               ffeaBeforeBLOBRead,
               ffeaBeforeBLOBTruncate,
               ffeaBeforeBLOBWrite,
               ffeaBeforeChgAliasPath,
               ffeaBeforeDBDelete,
               ffeaBeforeDBInsert,
               ffeaBeforeDBRead,
               ffeaBeforeFileBLOBAdd,
               ffeaBeforeRebuildInx,
               ffeaBeforeRecDelete,
               ffeaBeforeRecInsert,
               ffeaBeforeRecRead,
               ffeaBeforeRecUpdate,
               ffeaBeforeTabDelete,
               ffeaBeforeTabInsert,
               ffeaBeforeTableLock,
               ffeaBeforeTabPack,
               ffeaBeforeTabRead,
               ffeaBeforeTabRestruct,
               ffeaBeforeTabUpdate];
end;

function TffSecurityExtender.Notify(aServerObject : TffObject;
                                    aAction       : TffEngineAction) : TffResult;
var
  ReqRights : TffUserRights;
begin
  try
    Result := DBIERR_NONE;
    ReqRights := [];

    {don't check rights if this isn't a secure server}
    if not TffServerEngine(FParent.ServerEngine).Configuration.GeneralInfo^.giIsSecure then
      Exit;
    { Ignore if this is not the right kind of server object. }
    if (not (aServerObject is TffSrBaseCursor)) and                      {!!.01}
       (not (aServerObject is TffSrDatabase)) and                        {!!.01}
       (not (aServerObject is TffSrClient)) then
      Exit;
    {find what rights are needed for aAction}
    case aAction of
      {grouped by unique subsets of TffUserRights}
      {record actions include actions for BLOBs - for example,
       reading a BLOB would require the client to have the same
       privileges as reading a record}

      {reading a record, BLOB, table, or database requires read privileges}
      ffeaBeforeDBRead,
      ffeaBeforeBLOBRead,
      ffeaBeforeBLOBGetLength,
      ffeaBeforeRecRead,
      ffeaBeforeTabRead     : ReqRights := [arRead];

      {inserting a record, BLOB, table, or database requires insert privileges}
      ffeaBeforeDBInsert,
      ffeaBeforeBLOBCreate,
      ffeaBeforeFileBLOBAdd,
      ffeaBeforeRecInsert,
      ffeaBeforeTabInsert   : ReqRights := [arInsert];

      {updating a table, writing to a BLOB, or truncating a BLOB requires
       updates privileges}
      ffeaBeforeBLOBWrite,
      ffeaBeforeBLOBTruncate,
      ffeaBeforeTabUpdate   : ReqRights := [arUpdate];

      {updating a record requires read and update privileges}
      ffeaBeforeRecUpdate   : ReqRights := [arRead, arUpdate];

      {deleting a record, BLOB, table, or database requires delete privileges}
      ffeaBeforeDBDelete,
      ffeaBeforeBLOBDelete,
      ffeaBeforeRecDelete,
      ffeaBeforeTabDelete   : ReqRights := [arDelete];

      {restructuring a table requires the client to have delete,
       insert, read, and update privileges}
      ffeaBeforeTabRestruct : ReqRights := [arDelete, arInsert, arRead, arUpdate];

      {packing a table requires delete and update privileges}
      ffeaBeforeTabPack     : ReqRights := [arDelete, arUpdate];

      {adding or rebuilding an index requires insert, read, and
       update privileges}
      ffeaBeforeAddInx,
      ffeaBeforeRebuildInx  : ReqRights := [arInsert, arRead, arUpdate];

      {locking a table or changing an alias requires read and
       update privileges}
      ffeaBeforeChgAliasPath,
      ffeaBeforeTableLock   : ReqRights := [arRead, arUpdate];
    end; {case}

    { If no rights required then exit. }
    if ReqRights = [] then
      exit;

    { Now that we know what rights are required, lets see if the
      client has sufficient rights. }
    if aServerObject is TffSrClient then begin
      if (TffSrClient(aServerObject).Rights * ReqRights) <> ReqRights then
        Result := DBIERR_NOTSUFFTABLERIGHTS;
    end
    else
      if ((TffSrClient(TffServerObject(aServerObject).Client).Rights) *
           ReqRights) <> ReqRights then
      Result := DBIERR_NOTSUFFTABLERIGHTS;
  except
    on E : Exception do begin
      Result := ConvertServerException(E, nil);
    end;
  end;{try..except}
end;
{====================================================================}

end.
