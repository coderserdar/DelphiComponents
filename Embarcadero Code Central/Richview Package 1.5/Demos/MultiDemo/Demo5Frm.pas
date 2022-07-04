unit Demo5Frm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView, StdCtrls, ExtCtrls, Menus, ShellApi;

type
  TfrmDemo5 = class(TForm)
    pan: TPanel;
    edit: TEdit;
    rv: TRichView;
    rvs: TRVStyle;
    pm: TPopupMenu;
    mitFreezescrolling: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure editKeyPress(Sender: TObject; var Key: Char);
    procedure rvSelect(Sender: TObject);
    procedure pmPopup(Sender: TObject);
    procedure mitFreezescrollingClick(Sender: TObject);
    procedure rvJump(Sender: TObject; id: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo5: TfrmDemo5;

implementation

{$R *.DFM}
{--------------------------------------------------------------}
function IsAddress(s: String): Boolean;
begin
  // Checks for prefix.
  // For better results, it should check for lengths...
  s := UpperCase(s);
  Result :=
        (Pos('HTTP://',   s)=1) or
        (Pos('FTP://',    s)=1) or
        (Pos('FILE://',   s)=1) or
        (Pos('GOPHER://', s)=1) or
        (Pos('MAILTO://', s)=1) or        
        (Pos('HTTPS://',  s)=1) or
        (Pos('MAILTO:',   s)=1) or
        (Pos('NEWS:',     s)=1) or
        (Pos('TELNET:',   s)=1) or
        (Pos('WAIS:',     s)=1) or
        (Pos('WWW.',      s)=1) or
        (Pos('FTP.',      s)=1);
end;
{--------------------------------------------------------------}
function IsEmail(const s: String): Boolean;
var p1, p2: Integer;
   pchr: PChar;
begin
  //'@' must exist and '.' must be after it. This is not comprehensive test,
  //but I think that it's ok 
  Result := False;
  p1 := Pos('@', s);
  if p1=0 then exit;
  pchr := StrRScan(PChar(s),'.');
  if pchr = nil then exit;
  p2 := pchr - PChar(s)+1;
  if p1>p2 then exit;
  Result := True;
end;
{--------------------------------------------------------------}
procedure AddWithURLs(s: String; rv: TRichView; DefStyle, UrlStyle: Integer);
var Before, CurrentWord, Space: String;
    p: Integer;
    ParaNo: Integer;
begin
   ParaNo := 0;
   Before := '';
   if s = '' then begin
     rv.AddNL('', DefStyle, ParaNo);
     exit;
   end;
   while s<>'' do begin
     p := Pos(' ', s);
     if p=0 then p := Length(s)+1;
     CurrentWord := Copy(s, 1, p-1);
     Space := Copy(s, p, 1);
     s := Copy(s, p+1, Length(s));
     if IsAddress(CurrentWord) or IsEmail(CurrentWord) then begin
        if Before<>'' then begin
          rv.AddNL(Before, DefStyle, ParaNo);
          ParaNo := -1;
          Before := '';
        end;
        rv.AddNL(CurrentWord, UrlStyle, ParaNo);
        ParaNo := -1;
        if Space<>'' then rv.Add(Space, DefStyle);
        end
     else
       Before := Before + CurrentWord+Space;
   end;
   if Before<>'' then
     rv.AddNL(Before, DefStyle, ParaNo);
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.FormCreate(Sender: TObject);
begin
  pan.ClientHeight := edit.Height;
  edit.SetBounds(0,0,pan.ClientWidth,pan.ClientHeight);
  rv.AddNL('Use right-click menu to freeze scrolling when appending text', 2, 0);
  rv.AddNL('Try quick-copy: selection is copied automatically when done', 2, 0);
  AddWithURLs('You can use URLs and e-mail ( like www.trichview.com )',
              rv, 2, 1);
  rv.Format;
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.FormResize(Sender: TObject);
begin
 edit.Width := pan.ClientWidth;
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.editKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then begin
    AddWithURLS(edit.Text,rv,0,1);
    rv.FormatTail;
    Key := #0;
    edit.Text := '';
  end;
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.rvSelect(Sender: TObject);
begin
  // Quick-copy
  if rv.SelectionExists then begin
    rv.CopyDef;
    rv.Deselect;
    rv.Invalidate;
  end;
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.pmPopup(Sender: TObject);
begin
  mitFreezeScrolling.Checked := not (rvoScrollToEnd in rv.Options);
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.mitFreezescrollingClick(Sender: TObject);
begin
  if (rvoScrollToEnd in rv.Options) then
    rv.Options := rv.Options-[rvoScrollToEnd]
  else
    rv.Options := rv.Options+[rvoScrollToEnd];
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.rvJump(Sender: TObject; id: Integer);
var ItemNo: Integer;
    s: String;
    ItemTag: Integer;
begin
  ItemNo := rv.GetJumpPointItemNo(id);
  rv.GetTextInfo(ItemNo, s, ItemTag);
  if not IsAddress(s) and IsEmail(s) then
    s := 'mailto:'+s;
  ShellExecute(Application.Handle, 'open', PChar(s), nil, nil, SW_NORMAL);
end;
{--------------------------------------------------------------}
procedure TfrmDemo5.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;



end.
