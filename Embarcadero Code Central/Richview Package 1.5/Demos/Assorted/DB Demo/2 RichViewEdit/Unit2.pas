unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, RVScroll, RichView, RVEdit, DB, DBTables,
  Unit1;

type
  TForm2 = class(TForm)
    btnBold: TSpeedButton;
    btnPost: TSpeedButton;
    btnCancel: TSpeedButton;
    btnClose: TSpeedButton;
    RichViewEdit1: TRichViewEdit;
    Panel1: TPanel;
    procedure RichViewEdit1Change(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure RichViewEdit1CurTextStyleChanged(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FFieldName: String;
    FTable: TTable;
    procedure SetModified(Value: Boolean);
    procedure Load;
    procedure Save;
    property Modified: Boolean read FModified write SetModified;
  public
    { Public declarations }
    procedure SetField(const AFieldName: String; ATable: TTable);
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}
{==============================================================================}
// Simple, but quite useful functions.
// Earlier version of Delphi do not support TTable.CreateBlobStream...
{
function SaveRVFToField(rv: TRichView; tbl: TTable;
                        const FieldName: String): Boolean;
var Stream: TStream;
begin
  Stream := tbl.CreateBlobStream(tbl.FieldByName(FieldName), bmWrite);
  try
    Result := rv.SaveRVFToStream(Stream, False);
  finally
    Stream.Free;
  end;
end;

function LoadRVFFromField(rv: TRichView; tbl: TTable;
                          const FieldName: String): Boolean;
var Stream: TStream;
begin
  Stream := tbl.CreateBlobStream(tbl.FieldByName(FieldName), bmRead);
  try
    Result := rv.LoadRVFFromStream(Stream);
  finally
    Stream.Free;
  end;
  rv.Format;
end;
}
{==============================================================================}
// So below is alternative solution:
function SaveRVFToField(rv: TRichView; tbl: TTable;
                        const FieldName: String): Boolean;
var Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := rv.SaveRVFToStream(Stream, False);
    Stream.Position := 0;
    TBlobField(tbl.FieldByName(FieldName)).LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function LoadRVFFromField(rv: TRichView; tbl: TTable;
                          const FieldName: String): Boolean;
var Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    TBlobField(tbl.FieldByName(FieldName)).SaveToStream(Stream);
    Stream.Position := 0;
    Result := rv.LoadRVFFromStream(Stream);
  finally
    Stream.Free;
  end;
  rv.Format;
end;
{==============================================================================}
procedure TForm2.SetField(const AFieldName: String; ATable: TTable);
begin
  FTable := ATable;
  FFieldName := AFieldName;
  Load;
  Caption := FTable.FieldByName('Caption').AsString;
end;

procedure TForm2.Load;
begin
  LoadRVFFromField(RichViewEdit1, FTable, FFieldName);
  Modified := False;
end;

procedure TForm2.Save;
begin
  FTable.Edit;
  SaveRVFToField(RichViewEdit1, FTable, FFieldName);
  FTable.Post;
  Modified := False;
end;

procedure TForm2.RichViewEdit1Change(Sender: TObject);
begin
  Modified := True;
end;

procedure TForm2.SetModified(Value: Boolean);
begin
  if FModified<>Value then begin
    FModified := Value;
    if FModified then
      Panel1.Caption := 'Modified'
    else
      Panel1.Caption := '';
  end;
end;

procedure TForm2.btnPostClick(Sender: TObject);
begin
  Save;
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  Load;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified then
    case Application.MessageBox('Save changes?', 'Text was modified',
         MB_YESNOCANCEL or MB_ICONQUESTION) of
      IDYES:
        begin
          Save;
          CanClose := True;
        end;
      IDNO:
        CanClose := True;
      IDCANCEL:
        CanClose := False;
    end;
end;

procedure TForm2.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.RichViewEdit1CurTextStyleChanged(Sender: TObject);
begin
  btnBold.Down := RichViewEdit1.CurTextStyleNo<>0;
end;

procedure TForm2.btnBoldClick(Sender: TObject);
begin
  // switching 1-st and 0-th styles
  if btnBold.Down then
    RichViewEdit1.ApplyTextStyle(1)
  else
    RichViewEdit1.ApplyTextStyle(0);
end;

end.
