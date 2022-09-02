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
 
 unit ExOrderu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, StdCtrls, Grids, DBGrids, FFDB, FFDBBase, Menus, ffllbase, ffllcomm,
  fflllgcy, ffllcomp, fflleng, ffsrintm, ffclreng;

type
  TForm1 = class(TForm)
    ffSess: TffSession;
    CustomerData: TDataSource;
    LinesData: TDataSource;
    OrdersData: TDataSource;
    CustomerTable: TffTable;
    OrdersTable: TffTable;
    LinesTable: TffTable;
    ProductTable: TffTable;
    LinesTableLineID: TIntegerField;
    LinesTableOrderID: TIntegerField;
    LinesTableProductID: TIntegerField;
    LinesTableCount: TIntegerField;
    LinesTableTotal: TCurrencyField;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    GroupBox1: TGroupBox;
    DBGrid1: TDBGrid;
    GroupBox2: TGroupBox;
    DBGrid4: TDBGrid;
    DBGrid3: TDBGrid;
    LinesTableDescription: TStringField;
    LinesTablePrice: TCurrencyField;
    ffClient: TffClient;
    ffRSE: TFFRemoteServerEngine;
    ltMain: TffLegacyTransport;
    procedure LinesTableCalcFields(DataSet: TDataSet);
    procedure Open1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.LinesTableCalcFields(DataSet: TDataSet);
begin
  LinesTableTotal.AsFloat :=
    LinesTablePrice.AsFloat * LinesTableCount.AsInteger;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  ProductTable.Active := True;
  CustomerTable.Active := True;
  OrdersTable.Active := True;
  LinesTable.Active := True;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  LinesTable.Active := False;
  OrdersTable.Active := False;
  CustomerTable.Active := False;
  ProductTable.Active := False;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close1Click(nil);
  Close;
end;

end.
