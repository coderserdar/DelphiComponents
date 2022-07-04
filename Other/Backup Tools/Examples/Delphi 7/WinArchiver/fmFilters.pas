unit fmFilters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, unTranslation;

type
  TFilters = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel5: TPanel;
    lbExtensions: TListBox;
    Panel3: TPanel;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnClearAll: TBitBtn;
    rgKindOfFilter: TRadioGroup;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
    function  CleanExt( const str : String ) : String;
  public
    { Déclarations publiques }
  end;

var
  Filters: TFilters;

implementation
uses ArchiverMisc;
{$R *.DFM}

procedure TFilters.WMTranslate( var Message : TMessage );
begin
  rgKindOfFilter.Items.Text := GetStr(2207);
end;

procedure TFilters.btnClearAllClick(Sender: TObject);
begin
  lbExtensions.Items.Clear;
end;

procedure TFilters.btnDeleteClick(Sender: TObject);
begin
  with lbExtensions do
    if ItemIndex >= 0 then
      Items.Delete( ItemIndex );
end;

function TFilters.CleanExt( const str : String ) : String;
begin
  Result := UpperCase(str);
end;

procedure TFilters.btnEditClick(Sender: TObject);
var
  value : String;
begin
  with lbExtensions do
    if ItemIndex >= 0 then
      begin
        value := Items.Strings[ItemIndex];
        if InputQuery( GetStr(2200), GetStr(2208), value ) then
          Items.Strings[ItemIndex] := CleanExt(value);
      end;
end;

procedure TFilters.btnAddClick(Sender: TObject);
var
  value : String;
begin
  with lbExtensions do
    begin
      value := '';
      if InputQuery( GetStr(2200), GetStr(2209), value ) then
        if value <> '' then
          Items.Add( CleanExt(value) );
    end;
end;

end.
