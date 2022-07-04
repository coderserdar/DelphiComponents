{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit unClassLister;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmClassLister = class(TForm)
    lbList: TListBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    function Execute(AOwner: TComponent; AClass: TComponentClass): TComponent;
  end;

function SelectComponentByClass(AOwner: TComponent; AClass: TComponentClass): TComponent;

implementation

{$R *.dfm}

uses
  Consts;

function SelectComponentByClass(AOwner: TComponent; AClass: TComponentClass): TComponent;
var
  ClassLst: TfrmClassLister;
begin
  ClassLst := TfrmClassLister.Create(nil);
  try
    Result := ClassLst.Execute(AOwner, AClass);
  finally
    ClassLst.Free;
  end;
end;

procedure TfrmClassLister.btnOkClick(Sender: TObject);
begin
  if lbList.ItemIndex = -1 Then
    raise Exception.Create('Must select a item')
  else
    ModalResult := mrOk;
end;

function TfrmClassLister.Execute(AOwner: TComponent; AClass: TComponentClass): TComponent;
var
  i: Integer;
begin
  Caption := AOwner.Name + ' ' + AClass.ClassName + ' List';
  for i := 0 to AOwner.ComponentCount -1 do
    if AOwner.Components[i] is AClass then
      lbList.Items.AddObject(AOwner.Components[i].Name, AOwner.Components[i]);

  if lbList.Items.Count = 0 then
  begin
    ShowMessage(AClass.ClassName + ' not found in ' + AOwner.Name);
    Result := nil;
  end else if (ShowModal = mrOk) Then
    Result := TComponent(lbList.Items.Objects[lbList.ItemIndex])
  else
    Result := nil;
end;

procedure TfrmClassLister.FormCreate(Sender: TObject);
begin
  btnOk.Caption := SOKButton;
  btnCancel.Caption := SCancelButton;
end;

end.
