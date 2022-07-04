{*********************************************************}
{*             VPSQLPARADOXDIALECT.PAS 1.03              *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*         Hannes Danzl                                                       *}   
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{This unit was provided by Hannes Danzl and is used here with permission       }


// a sql dialect class for paradox
unit VPSQLParadoxDialect;

interface

uses
  VPSQLDialect, VPDbIntf, dbtables, db, sysutils;

type
  // a sql dialect class for paradox
  TVPParadoxSQLDialect = class(TBaseSQLDialect)
  protected
    // override to use 'create table "%TableName%" (%Fields%)' since paradox
    // can have the fileextension in the tablename
    function GetCreateSyntax: String; override;
    // return the right strings
    function SQLGetColumnDef(const aFieldDef: TFieldDef): String; override;
    // override to avoid abstract error
    procedure EnsureDatabaseExists; virtual; 
  end;

implementation

procedure TVPParadoxSQLDialect.EnsureDatabaseExists;
begin
  // do nothing
end;

function TVPParadoxSQLDialect.GetCreateSyntax: String;
begin
  result:='create table "%TableName%" (%Fields%)';
end;

function TVPParadoxSQLDialect.SQLGetColumnDef(
  const aFieldDef: TFieldDef): String;
var
  aTypeName: String;
begin
  case aFieldDef.DataType of
    ftInteger: aTypeName:='INTEGER';
    ftFloat: aTypeName:='NUMERIC';
    ftString:
      if aFieldDef.Size<256 then
        aTypeName:='VARCHAR('+inttostr(aFieldDef.Size)+')'
      else
        aTypeName:='BLOB(1,1)';
    ftBoolean: aTypeName:='BOOLEAN';
    ftDate: aTypeName:='DATE';
    ftTime: aTypeName:='TIME';
    ftDateTime: aTypeName:='TIMESTAMP';
  end;    // case
  result:=aFieldDef.Name+' '+aTypeName;
end;

function CreateParadoxDialect(InterfaceClass: String): TObject;
begin
  result:=TVPParadoxSQLDialect.Create(nil);
end;

initialization
  // IMPORTANT: register it
  sSQLDialectFactory.RegisterInterfaceType('Paradox', @CreateParadoxDialect);
end.
