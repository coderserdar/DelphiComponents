unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RVScroll, RichView, RVEdit, RVMisc, RVStyle, ImgList,
  RVTable;

type
  TForm1 = class(TForm)
    rve: TRichViewEdit;
    Panel1: TPanel;
    bnnOpen: TButton;
    btnFind: TButton;
    btnReplace: TButton;
    fd: TFindDialog;
    rd: TReplaceDialog;
    RVStyle1: TRVStyle;
    OpenDialog1: TOpenDialog;
    il: TImageList;
    procedure btnReplaceClick(Sender: TObject);
    procedure rdFind(Sender: TObject);
    procedure rdReplace(Sender: TObject);
    procedure rveRVFImageListNeeded(Sender: TRichView;
      ImageListTag: Integer; var il: TImageList);
    procedure btnFindClick(Sender: TObject);
    procedure fdFind(Sender: TObject);
    procedure bnnOpenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure ShowInfo(const msg,cpt: String);
begin
  Application.MessageBox(PChar(msg),PChar(cpt),MB_OK or MB_ICONINFORMATION);
end;

{=============================== REPLACE ======================================}
procedure TForm1.btnReplaceClick(Sender: TObject);
var s: String;
    p: Integer;
begin
  rve.SetFocus;
  fd.CloseDialog;
  if rve.SelectionExists then begin
    s := rve.GetSelText;
    p := Pos(#13,s);
    if p<>0 then s := Copy(s,1,p-1);
    rd.FindText := s;
  end;
  rd.Execute;
end;
{------------------------------------------------------------------------------}
procedure TForm1.rdFind(Sender: TObject);
begin
  if not rve.SearchText(rd.FindText,GetRVESearchOptions(rd.Options)) then
    ShowInfo('String not found','Search and Replace');
end;
{------------------------------------------------------------------------------}
procedure TForm1.rdReplace(Sender: TObject);
var c: Integer;
begin
  if frReplace in rd.Options then begin
    if rve.GetSelText=rd.FindText then rve.InsertText(rd.ReplaceText);
    if not rve.SearchText(rd.FindText,GetRVESearchOptions(rd.Options)) then
      ShowInfo('String not found','Search and Replace');
    end
  else if frReplaceAll in rd.Options then begin
    c := 0;
    if rve.GetSelText=rd.FindText then begin
      rve.InsertText(rd.ReplaceText);
      inc(c);
    end;
    while rve.SearchText(rd.FindText,GetRVESearchOptions(rd.Options)) do begin
      rve.InsertText(rd.ReplaceText);
      inc(c);
    end;
    ShowInfo(Format('There were %d replacements',[c]),'Replace');
  end;
end;
{================================= FIND =======================================}
procedure TForm1.btnFindClick(Sender: TObject);
var s: String;
    p: Integer;
begin
  rve.SetFocus;
  fd.CloseDialog;
  if rve.SelectionExists then begin
    s := rve.GetSelText;
    p := Pos(#13,s);
    if p<>0 then s := Copy(s,1,p-1);
    fd.FindText := s;
  end;
  fd.Execute;
end;
{------------------------------------------------------------------------------}
procedure TForm1.fdFind(Sender: TObject);
begin
  if not rve.SearchText(fd.FindText,GetRVESearchOptions(fd.Options)) then
    ShowInfo('String not found','Search');
end;
{==============================================================================}
procedure TForm1.rveRVFImageListNeeded(Sender: TRichView;
  ImageListTag: Integer; var il: TImageList);
begin
  il := Self.il;
end;
{------------------------------------------------------------------------------}
procedure TForm1.bnnOpenClick(Sender: TObject);
var
    r: Boolean;
begin
  if OpenDialog1.Execute then begin
    case OpenDialog1.FilterIndex of
      1: // RVF
        r := rve.LoadRVF(OpenDialog1.FileName);
      2: // ANSI text
        r := rve.LoadText(OpenDialog1.FileName,0,0,False);
      else
        r := False;
    end;
    if not r then
      Application.MessageBox('Error during loading', 'Error', 0);
    rve.Format;
    rve.SetFocus;
  end;
end;

initialization

  RegisterClasses([TEdit,TButton]);

end.
