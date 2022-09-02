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
 
unit ExCustu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Menus, Grids, DBGrids, Db, FFDB, FFDBBase, ffllcomm,
  fflllgcy, ffllcomp, fflleng, ffsrintm, ffclreng, ffllbase;

type
  TForm1 = class(TForm)
    ffSess: TffSession;
    CustomerTable: TffTable;
    CustomerData: TDataSource;
    CustomerGrid: TDBGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Navigate1: TMenuItem;
    First1: TMenuItem;
    Last1: TMenuItem;
    Next1: TMenuItem;
    Prior1: TMenuItem;
    Edit1: TMenuItem;
    Append1: TMenuItem;
    Post1: TMenuItem;
    Refresh1: TMenuItem;
    Insert1: TMenuItem;
    N2: TMenuItem;
    Cancel1: TMenuItem;
    DBNavigator1: TDBNavigator;
    ffClient: TffClient;
    ffRSE: TFFRemoteServerEngine;
    ltMain: TffLegacyTransport;
    procedure Open1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure First1Click(Sender: TObject);
    procedure Last1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure Prior1Click(Sender: TObject);
    procedure Append1Click(Sender: TObject);
    procedure Post1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure Insert1Click(Sender: TObject);
    procedure Cancel1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  csAlias = 'Tutorial';

{$R *.DFM}

procedure TForm1.Open1Click(Sender: TObject);
begin
  CustomerTable.Active := True;
  Close1.Enabled := True;
  Navigate1.Enabled := True;
  Edit1.Enabled := True;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  CustomerTable.Active := False;
  Close1.Enabled := False;
  Navigate1.Enabled := False;
  Edit1.Enabled := False;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.First1Click(Sender: TObject);
begin
  CustomerTable.First;
end;

procedure TForm1.Last1Click(Sender: TObject);
begin
  CustomerTable.Last;
end;

procedure TForm1.Next1Click(Sender: TObject);
begin
  CustomerTable.Next;
end;

procedure TForm1.Prior1Click(Sender: TObject);
begin
  CustomerTable.Prior;
end;

procedure TForm1.Append1Click(Sender: TObject);
begin
  CustomerTable.Append;
end;

procedure TForm1.Post1Click(Sender: TObject);
begin
  CustomerTable.Post;
end;

procedure TForm1.Refresh1Click(Sender: TObject);
begin
  CustomerTable.Refresh;
end;

procedure TForm1.Insert1Click(Sender: TObject);
begin
  CustomerTable.Insert;
end;

procedure TForm1.Cancel1Click(Sender: TObject);
begin
  CustomerTable.Cancel;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  aPath : string;
begin
  ffSess.Open;
  if not ffSess.IsAlias(csAlias) then begin
    aPath := ExtractFilePath(Application.ExeName);
    if aPath[Length(aPath)] <> '\' then
      aPath := aPath + '\';
    { Path should point to the folder containing the Mythic tables. }
    ffSess.AddAlias(csAlias, aPath + '..', False);
  end;
end;

end.
