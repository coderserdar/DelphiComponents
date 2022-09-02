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
 
 unit ExBlobu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Menus, Grids, DBGrids, Db, FFDB, FFDBBase, StdCtrls,
  FFLLBase, ffllcomm, fflllgcy, ffllcomp, fflleng, ffsrintm, ffclreng;

type
  TForm1 = class(TForm)
    ffSess: TffSession;
    BlobSource: TDataSource;
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
    Panel1: TPanel;
    DbImage1: TDBImage;
    DbText1: TDBText;
    Images1: TMenuItem;
    Bmp1: TMenuItem;
    Jpeg1: TMenuItem;
    All1: TMenuItem;
    DbText2: TDBText;
    DbText3: TDBText;
    BlobTable: TffTable;
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
    procedure All1Click(Sender: TObject);
    procedure Bmp1Click(Sender: TObject);
    procedure Jpeg1Click(Sender: TObject);
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
  BlobTable.Active := True;
  Close1.Enabled := True;
  Navigate1.Enabled := True;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  BlobTable.Active := False;
  Close1.Enabled := False;
  Navigate1.Enabled := False;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.First1Click(Sender: TObject);
begin
  BlobTable.First;
end;

procedure TForm1.Last1Click(Sender: TObject);
begin
  BlobTable.Last;
end;

procedure TForm1.Next1Click(Sender: TObject);
begin
  BlobTable.Next;
end;

procedure TForm1.Prior1Click(Sender: TObject);
begin
  BlobTable.Prior;
end;

procedure TForm1.All1Click(Sender: TObject);
begin
  BlobTable.Filtered := False;
end;

procedure TForm1.Bmp1Click(Sender: TObject);
begin
  Bmp1.Checked := True;
  BlobTable.Filter := 'Type = ''Bmp''';
  BlobTable.Filtered := True;
end;

procedure TForm1.Jpeg1Click(Sender: TObject);
begin
  Jpeg1.Checked := True;
  BlobTable.Filter := 'Type = ''Jpeg''';
  BlobTable.Filtered := True;
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
