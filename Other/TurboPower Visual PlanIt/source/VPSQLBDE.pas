{*********************************************************}
{*                  VPSQLBDE.PAS 1.03                    *}
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

unit VPSQLBDE;

interface

uses
  classes, VPDbIntf, dbtables, db, sysutils;

type
  // implements the ISQLDataSet interface for TQuery
  TshwBDEDataset = class(TQuery, ISQLDataSet)
  protected
    // see ISQLDataset.iConnectionParams
    fConnectionParams: TStringlist;
    // see ISQLDataset.iSQL
    procedure SetiSQL(const value: String); virtual;
    // see ISQLDataset.iSQL
    function GetiSQL:String; virtual;
    // see ISQLDataset.iExecSQL
    procedure IExecSQL; virtual;
    // see ISQLDataset.iConnectionParams
    procedure SetiConnectionParams(const value: String); virtual;
    // see ISQLDataset.iConnectionParams
    function GetiConnectionParams:String; virtual;
  public
    // constructor
    constructor Create(aOwner: TComponent); override;
    // destructor
    destructor Destroy; override;
  end;

implementation

constructor TshwBDEDataset.Create(aOwner: TComponent);
begin
  inherited;
  fConnectionParams:=TStringlist.Create;
  RequestLive:=true;
end;

destructor TshwBDEDataset.Destroy;
begin
  fConnectionParams.free;
  inherited;
end;

function TshwBDEDataset.GetiConnectionParams: String;
begin
  result:=fConnectionParams.Text;
end;

function TshwBDEDataset.GetiSQL: String;
begin
  result:=sql.text;
end;

procedure TshwBDEDataset.IExecSQL;
begin
  ExecSQL;
end;

procedure TshwBDEDataset.SetiConnectionParams(const value: String);
begin
  fConnectionParams.Text:=value;
  Close;
  DatabaseName:=fConnectionParams.Values['DatabaseName'];
end;

procedure TshwBDEDataset.SetiSQL(const value: String);
begin
  sql.text:=value;
end;

function CreateBDESQLDataset(InterfaceClass: String): TDataset;
begin
  result:=TshwBDEDataset.Create(nil);
end;

initialization
  // IMPORTANT: register it
  sSQLDatasetFactory.RegisterInterfaceType('BDE', @CreateBDESQLDataset);
end.
