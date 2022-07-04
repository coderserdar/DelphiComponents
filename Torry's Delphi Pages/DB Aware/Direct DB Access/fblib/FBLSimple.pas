{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLSimple.pas

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
@abstract(Managing database connection , transaction and dsql queries)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLSimple.pas unit provides connection transaction and queries to firebird database
}
unit FBLSimple;

interface

uses
  SysUtils, Classes, FBLDsql, FBLDatabase, FBLTransaction;

type
  {@abstract(encapsulates the properties and methods for connect and request dsql queries from firebird database)}
  TFBLSimple = class(TFBLDsql)
  private
    FDb : TFBLDatabase;
    FTr : TFBLTransaction;
    function GetInTransaction: boolean;
    function GetConnected: boolean;
    function GetUser: string;
    function GetPassword: string;
    function GetConnectionString: string;
    function GetCharacterSet: string;
    procedure SetUser(const AValue: string);
    procedure SetPassword(const AValue: string);
    procedure SetConnectionString(const AValue: string);
    procedure SetCharacterSet(const AValue: string);
  public
    {Create an instance  of a TFBLSimple}
    constructor Create(AOwner: TComponent); reintroduce; overload;
    {Create an instance  of a TFBLSimple}
    constructor Create; reintroduce; overload;
    {Create an instance  of a TFBLSimple}
    constructor Create(const AConnectionString :string);reintroduce; overload;
    {Create an instance  of a TFBLSimple

    }
    constructor Create(const AConnectionString,AUserName,APassword:string); reintroduce; overload;
    {Free up  all resources associated with this instance}
    destructor Destroy;override;
    {Connect to database
      @longCode(#
    //Examples ...
     program testsimple;
     uses
       SysUtils, FBLSimple;
     var
       fbdb: TFBLSimple;
       conn,user,passwd: string;  //connection string
     begin
       conn := 'localhost:c:\db\juventus.fdb';
       user := 'sysdba';
       passwd := 'masterkey';
       fbdb := TFBLSimple.Create(conn,user,passwd);
       try
         try
           fbdb.Connect;
           fbdb.StartTransaction;
           fbdb.SQL.Text := 'SELECT * FROM PLAYERS';
           fbdb.ExecSQL;
           while not fbdb.Eof do
           begin
             WriteLn(fbdb.FieldbyNameAsString('player_id'));
             WriteLn(fbdb.FieldbyNameAsString('name'));
             WriteLn(fbdb.FieldbyNameAsString('number'));
             WriteLn('');
             fbdb.Next;
           end;
          fbdb.Close;
          fbdb.Commit;
          fbdb.Disconnect;
        except
          on E:Exception do
            WriteLn(E.Message);
        end;
      finally
       fbdb.Free;
      end;
    end.
    #)}
    procedure Connect;
    {disconnect a connected database @html(<br>)
    see also @link(Connect)}
    procedure Disconnect;
    {Start transaction, when transaction is active property @link(InTransaction) := True}
    procedure StartTransaction;
    {Commit an active transaction}
    procedure Commit;
    {RollBack an active transaction}
    procedure RollBack;
    {CommitRetaining an active transaction}
    procedure CommitRetaining;
    {True if transaction is active}
    property InTransaction: boolean read GetInTransaction;
    {True if database  is connected}
    property Connected: boolean read GetConnected;
    {User name}
    property User: string read GetUser write SetUser;
    {Password}
    property Password: string read GetPassword write SetPassword;
    {Connection string
    @longCode(#
    //Examples ...
    fbdb.ConnectionString := 'localhost:c:\dbdb\mydb.fdb'
    #)
    }
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property CharacterSet: string read GetCharacterSet write SetCharacterSet;
  end;


implementation

constructor  TFBLSimple.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fdb := TFBLDatabase.Create(nil);
  Ftr := TFBLTransaction.Create(nil);
  Ftr.Database := FDb;
  self.Transaction := Ftr;
end;

//------------------------------------------------------------------------------


constructor  TFBLSimple.Create;
begin
  inherited Create(nil);
  Fdb := TFBLDatabase.Create(nil);
  Ftr := TFBLTransaction.Create(nil);
  Ftr.Database := FDb;
  self.Transaction := Ftr;
end;

//------------------------------------------------------------------------------

constructor  TFBLSimple.Create(const AConnectionString:string);
begin
  inherited Create(nil);
  Fdb := TFBLDatabase.Create(nil);
  Ftr := TFBLTransaction.Create(nil);
  Ftr.Database := FDb;
  self.Transaction := Ftr;
  fdb.DBFile := AConnectionString;
end;

//------------------------------------------------------------------------------

constructor TFBLSimple.Create(const AConnectionString,AUserName,APassword:string);
begin
  inherited Create(nil);
  Fdb := TFBLDatabase.Create(nil);
  Ftr := TFBLTransaction.Create(nil);
  self.Transaction := Ftr;
  Ftr.Database := FDb;
  fdb.DBFile := AConnectionString;
  Fdb.User := AUserName;
  Fdb.Password := APassword;
end;

//------------------------------------------------------------------------------

destructor  TFBLSimple.Destroy;
begin
  Ftr.Free;
  Fdb.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetInTransaction: boolean;
begin
  Result := FTr.InTransaction;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetConnected: boolean;
begin
  Result := FDb.Connected;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetUser: string;
begin
  Result := Fdb.User;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetPassword: string;
begin
  Result := Fdb.Password;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetConnectionString;
begin
  Result := Fdb.DBFile;
end;

//------------------------------------------------------------------------------

function TFBLSimple.GetCharacterSet;
begin
  Result := Fdb.CharacterSet;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.SetUser(const AValue: string);
begin
  Fdb.User := AValue;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.SetPassword(const AValue: string);
begin
  FDb.Password := AValue;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.SetConnectionString(const AValue: string);
begin
  Fdb.DBFile := AValue;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.SetCharacterSet(const AValue: string);
begin
  Fdb.CharacterSet := AValue;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.Connect;
begin
  FDB.Connect;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.Disconnect;
begin
   FDB.Disconnect;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.StartTransaction;
begin
  Ftr.StartTransaction;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.Commit;
begin
  Ftr.Commit;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.RollBack;
begin
  Ftr.RollBack;
end;

//------------------------------------------------------------------------------

procedure TFBLSimple.CommitRetaining;
begin
 Ftr.CommitRetaining;
end;

//------------------------------------------------------------------------------

end.

