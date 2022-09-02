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

{$I ffdefine.inc}
unit uEmpCfg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, StdCtrls, ExtCtrls, db, FFDB, ExtDlgs, DBCtrls;

type
  TfrmEmployees = class(TForm)
    pnlBottom: TPanel;
    btnClose: TButton;
    btnNew: TButton;
    btnDelete: TButton;
    tblEmployees: TffTable;
    dtsemployees: TDataSource;
    pnlBack: TPanel;
    grdEmp: TDBGrid;
    Splitter1: TSplitter;
    pnlPhotoTools: TPanel;
    btnSelectPhoto: TButton;
    btnClearPhoto: TButton;
    dlgOpenPicture: TOpenPictureDialog;
    dbiEmployee: TDBImage;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnClearPhotoClick(Sender: TObject);
    procedure btnSelectPhotoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetDataset: TDataset;
  public
    { property used to simplify access to the employee table }
    property Dataset : TDataset
      read GetDataset;
  end;

var
  frmEmployees: TfrmEmployees;

implementation

uses
  dMythic
  {$IFDEF DCC6OrLater}                                                          {!!.05}
  ,Variants                                                                     {!!.05}
  {$ENDIF}                                                                      {!!.05}
  ;

{$R *.DFM}

procedure TfrmEmployees.btnDeleteClick(Sender: TObject);
begin
  { make sure the dataset is in browse mode }
  Dataset.CheckBrowseMode;

  { confirm before deleting the record }
  if MessageDlg(Format('Delete %s?', [Dataset.FieldByName('Name').AsString]),
                mtConfirmation, mbOKCancel, 0) = mrOK then
    Dataset.Delete;
end;

procedure TfrmEmployees.btnNewClick(Sender: TObject);
var
  NameStr : string;
begin
  { make sure the dataset is in browse mode }
  Dataset.CheckBrowseMode;
  { retrieve the new employee name, and add it to the table }
  if InputQuery('Create new employee', 'Name', NameStr) then
    Dataset.AppendRecord([null, NameStr]);
end;

function TfrmEmployees.GetDataset: TDataset;
begin
  Result := grdEmp.Datasource.Dataset;
end;

procedure TfrmEmployees.btnClearPhotoClick(Sender: TObject);
var
  BS : TffBlobStream;
begin
  if not (Dataset.State in [dsEdit, dsInsert]) then
    Dataset.Edit;
  BS := TffBlobStream.Create(TBlobField(Dataset.FieldByName('Picture')), bmWrite);
  try
    BS.Position := 0;
    BS.Truncate;
    Dataset.Post;
  finally
    BS.Free;
  end;
end;

procedure TfrmEmployees.btnSelectPhotoClick(Sender: TObject);
var
  BS : TffBlobStream;
  FS : TFileStream;
begin
  if dlgOpenPicture.Execute then begin
    if not (Dataset.State in [dsEdit, dsInsert]) then
      Dataset.Edit;
    FS := TFileStream.Create(dlgOpenPicture.FileName, fmOpenRead or fmShareDenyWrite);
    try
      BS := TffBlobStream(Dataset.CreateBlobStream(Dataset.FieldByName('Picture'),
                          bmReadWrite));
      try
        BS.CopyFrom(FS, FS.Size);
        Dataset.Post;
      finally
        BS.Free;
      end;
    finally
      FS.Free;
    end;
  end;
end;

procedure TfrmEmployees.FormShow(Sender: TObject);
begin
  tblEmployees.Open;
end;

end.
