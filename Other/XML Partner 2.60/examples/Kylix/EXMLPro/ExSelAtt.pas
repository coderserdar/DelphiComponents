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
 * The Original Code is TurboPower XMLPartner
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

{*********************************************************}
{* XMLPartner: ExSelAtt.PAS 2.57                         *}
{*********************************************************}
{* XMLPartner: XML Editor Attribute Selection form       *}
{*********************************************************}
unit ExSelAtt;

interface

uses
{$IFDEF WIN32}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
  QStdCtrls,
  QComCtrls,
  QControls,
{$ENDIF}
  SysUtils,
  Classes,
  XpDOM;

type
  TSelAttrsForm = class(TForm)
    Label1: TLabel;
    AttrsListView: TListView;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure AttrsListViewData(Sender: TObject; Item: TListItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    { Private declarations }
    FList: TXpNodeList;
    FNode: TXpElement;

  public
    { Public declarations }
    procedure Run(oList: TXpNodeList; oNode: TXpElement);
  end;

var
  SelAttrsForm: TSelAttrsForm;

implementation

{$R *.dfm}

procedure TSelAttrsForm.Run(oList: TXpNodeList; oNode: TXpElement);
var
  oItem: TListItem;
  i: Integer;
  sName: String;
begin
  FList := oList;
  FNode := oNode;
  for i := 0 to FList.Length - 1 do
  begin
    oItem := AttrsListView.Items.Add;
    sName := TXpElement(FList.Item(i)).GetAttribute('name');
    oItem.Caption := sName;
    oItem.Checked := (TXpElement(FList.Item(i)).GetAttribute('checked') = 'true') or
      (FNode.GetAttribute(sName) <> '');
  end;
  ShowModal;
end;

procedure TSelAttrsForm.AttrsListViewData(Sender: TObject;
  Item: TListItem);
begin
  Item.Caption := TXpElement(FList.Item(Item.Index)).GetAttribute('name');
  Item.Checked := TXpElement(FList.Item(Item.Index)).GetAttribute('checked') = 'true';
end;

procedure TSelAttrsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  AttrsListView.Items.Clear;
end;

procedure TSelAttrsForm.FormShow(Sender: TObject);
begin
  AttrsListView.SetFocus;
end;

procedure TSelAttrsForm.OkBtnClick(Sender: TObject);
var
  i: Integer;
  sName, sValue: String;
begin
  for i := 0 to FList.Length - 1 do
  begin
    sName := TXpElement(FList.Item(i)).GetAttribute('name');
    if AttrsListView.Items[i].Checked then
    begin
      if FNode.GetAttribute(sName) = '' then
      begin
        sValue := TXpElement(FList.Item(i)).GetAttribute('value');
        FNode.SetAttribute(sName, sValue);
      end;
    end
    else
      FNode.RemoveAttribute(sName);
  end;
end;

end.
