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

unit uCustEnt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls, Mask, ExtCtrls, db;

type
  TfrmCustomerEntry = class(TForm)
    pnlBottom: TPanel;
    btnClose: TButton;
    dbeCompany: TDBEdit;
    Label1: TLabel;
    bdeContact: TDBEdit;
    bdeAddress1: TDBEdit;
    dbeAddress2: TDBEdit;
    dbeCity: TDBEdit;
    dbeState: TDBEdit;
    dbeZip: TDBEdit;
    dbeCountry: TDBEdit;
    dbePhone: TDBEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    dbeDeliveryMethod: TDBComboBox;
    procedure btnCloseClick(Sender: TObject);
  private
    function GetDataset: TDataset;
    { Private declarations }
  public
    { property used to simplify access to the customer table }  
    property Dataset : TDataset
       read GetDataset;
  end;

var
  frmCustomerEntry: TfrmCustomerEntry;

implementation

uses dMythic;

{$R *.DFM}

procedure TfrmCustomerEntry.btnCloseClick(Sender: TObject);
begin
  { make sure that all the changes have been saved }
  Dataset.CheckBrowseMode;

  Close;
end;

function TfrmCustomerEntry.GetDataset: TDataset;
begin
  Result := dbeCompany.Datasource.Dataset;
end;

end.
