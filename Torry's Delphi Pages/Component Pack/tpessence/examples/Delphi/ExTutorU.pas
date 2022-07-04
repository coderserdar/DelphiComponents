
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
 * The Original Code is TurboPower Essentials Vol I
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

unit ExTutorU;

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EsRollUp, StdCtrls, EsEdPop, EsEdCal, EsBase, EsCal, Buttons, ExtCtrls,
  EsLabel, EsMarque, EsTile, EsEdCalc;

type
  TForm1 = class(TForm)
    EsRollUp1: TEsRollUp;
    MoreBtn: TBitBtn;
    LessBtn: TBitBtn;
    Panel1: TPanel;
    EsScrollingMarquee1: TEsScrollingMarquee;
    ListBox1: TListBox;
    EsTile1: TEsTile;
    EsDateEdit1: TEsDateEdit;
    EsNumberEdit1: TEsNumberEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EsRollUp1RollDown(Sender: TObject);
    procedure EsRollUp1RollUp(Sender: TObject);
    procedure EsScrollingMarquee1Cycle(Sender: TObject);
    procedure EsDateEdit1Exit(Sender: TObject);
    procedure EsNumberEdit1Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {these can be set at design-time}
  BorderStyle := bsDialog;
  AutoScroll := False;

  {move "less" button into same positon as "more" button}
  LessBtn.Top := MoreBtn.Top;
  LessBtn.Left := MoreBtn.Left;

  {load text to display}
  ListBox1.Items.LoadFromFile('EXTUTOR.TXT');

  {set initial state}
  LessBtn.Visible := False;
  MoreBtn.Visible := True;
  ESRollUp1.MinHeight := MoreBtn.Top + MoreBtn.Height + 4;
  ESRollUp1.RolledUp := True;
  ListBox1.ItemIndex := 0;
  EsScrollingMarquee1.Caption := ListBox1.Items[0];
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EsRollUp1.RolledUp := not EsRollUp1.RolledUp;
  if EsRollUp1.RolledUp then begin
    MoreBtn.Visible := True;
    LessBtn.Visible := False;
  end else begin
    LessBtn.Visible := True;
    MoreBtn.Visible := False;
  end;
end;

procedure TForm1.EsRollUp1RollDown(Sender: TObject);
begin
  EsScrollingMarquee1.Active := True;
end;

procedure TForm1.EsRollUp1RollUp(Sender: TObject);
begin
  EsScrollingMarquee1.Active := False;
end;

procedure TForm1.EsScrollingMarquee1Cycle(Sender: TObject);
var
  I : Integer;
begin
  {pick the next string to display}
  I := ListBox1.ItemIndex+1;
  if I > ListBox1.Items.Count-1 then
    I := 0;
  ListBox1.ItemIndex := I;
  EsScrollingMarquee1.Caption := ListBox1.Items[I];
end;

procedure TForm1.EsDateEdit1Exit(Sender: TObject);
begin
  Label1.Caption := FormatDateTime(LongDateFormat, EsDateEdit1.Date);
end;

procedure TForm1.EsNumberEdit1Exit(Sender: TObject);
begin
  Label2.Caption := EsNumberEdit1.AsString;
end;

end.
