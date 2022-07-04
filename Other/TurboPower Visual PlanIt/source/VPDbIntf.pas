{*********************************************************}
{*                  VPDBINTF.PAS 1.03                    *}
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
{*      Hannes Danzl                                                          *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{ This unit was provided by Hannes Danzl and is used here with permission      }

// base unit for all interfaced tdatasets<br>
// defines interfaces for connecting to db independent datasets
unit VPDbIntf;

interface

uses classes, db, sysutils;

type
  // interface for sql capable datasets
  ISQLDataSet = interface
    ['{5855A2B8-8568-4AA5-86BC-6DDB06833F8E}']
    // see iSQL
    procedure SetiSQL(const value: String);
    // see iSQL
    function GetiSQL:String;
    // see iConnectionParams
    procedure SetiConnectionParams(const value: String);
    // see iConnectionParams
    function GetiConnectionParams:String;
    // interface to the ExecSQL method
    procedure IExecSQL;
    // interface for setting the SQL statement
    property iSQL: String read GetiSQL write SetiSQL;
    // interface for optional connection parameters for the dataset
    property iConnectionParams: String read GetiConnectionParams write SetiConnectionParams;
  end;

  // interface for datasets capable of creating "tables"
  ICreateTableDataSet = interface
    ['{83FC58AD-C245-4F03-A2B8-D1B737BC1955}']
    // should create the given table
    procedure iCreateTable(const aTableName: String; const aFieldDefs: TFieldDefs; const aIndexDefs: TIndexDefs);
  end;

  // internal storage type
  TCreateInstance = function (InterfaceClass: String): TObject;

  // factory for creating classes that implement ISQLDataset
  TDBFactory = class(TObject)
  protected
    // list of registered creation methods; a method must be of type TCreateInstance
    fStringlist: TStringlist;
  public
    // constructor
    constructor Create; virtual;
    // destructor
    destructor Destroy; override;
    // registers a class that implements ISQLDataSet. aproc is a function,
    // that creates an instance of a TDataset descendant with ISQLDataSet
    // implementation and returns it.
    procedure RegisterInterfaceType(InterfaceClass: String; aProc: TCreateinstance);
    // calls the appropriate creation method and returns the dataset; nil if the
    // classtype is not known.
    function CreateInstance(InterfaceClass: String): TObject;
  end;

  // the single instance of TSQLDatasetFactory for each application
  // use it to register and create datasets
  function sSQLDatasetFactory: TDBFactory;

implementation

{ TSQLDatasetFactory }

constructor TDBFactory.Create;
begin
  inherited;
  fStringlist:=TStringlist.Create;
end;

function TDBFactory.CreateInstance(InterfaceClass: String): TObject;
var
  anindex: integer;
begin
  result:=nil;
  anindex:=fStringlist.IndexOf(InterfaceClass);
  if anindex>-1 then
    result:=TCreateinstance(fStringlist.Objects[anIndex])(InterfaceClass)
  else
    assert(false, 'DB type "'+InterfaceClass+'" not registered with factory.');
end;

destructor TDBFactory.Destroy;
begin
  fStringlist.Free;
  inherited;
end;

procedure TDBFactory.RegisterInterfaceType(InterfaceClass: String;
  aProc: TCreateinstance);
begin
  fStringlist.AddObject(InterfaceClass, TObject(@aProc))
end;

var
  fSQLDatasetFactory: TDBFactory;

function sSQLDatasetFactory: TDBFactory;
begin
  if fSQLDatasetFactory=nil then
    fSQLDatasetFactory:=TDBFactory.Create;
  result:=fSQLDatasetFactory;
end;

initialization
  fSQLDatasetFactory:=nil;
end.

