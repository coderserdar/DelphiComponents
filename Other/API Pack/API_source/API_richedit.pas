unit API_richedit;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.07, 03072009, ari pikivirta
//  * added GetFirstVisibleLine and GetLastVisibleLine function
//
// r1.06, 22062009, ari pikivirta
//  * added function OpenFileFast to open even big files really fast
//  * removed Perform(EM_EXLIMITTEXT, 0, $7FFFFFF0); from create
//
// r1.05, 06052008, ari pikivirta
//  * fixed bug "no parent" when inserting component onto the form
//    SendMessage function should not be used internally inside the compoennt! 
//
// r1.04, 04022008, ari pikivirta
//  * overloaded markwords function for start end end position on marking
//  * added basic features to support syntax highlighting (example updated)
//
// r1.03, 31012008, ari pikivirta
//  * fixed markwords function (was stuck on previous)
//
// r1.02, 25052007, ari pikivirta
//  * added ScrollToBottom and ScrollToTop procedures
//
// r1.01/30072006/ari pikivirta
//  * added onmouseleave and onmouseenter events
//  * changed getposition functions
//  * added maxlines property (removes lines depending on caret pos)
//
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ComCtrls, RichEdit,
  Graphics;

type
  // highlighting
  TAPI_syntaxitem = record
    words: tstringlist;
    font: tfont;
  end;

  TAPI_syntax = record
    count: integer;
    item: array of TAPI_syntaxitem;
    number: tfont;
    special: tfont;
  end;

  // richedit component
  TAPI_richedit = class(TRichEdit)
  private
    fversion: string;
    fsearchtype: tsearchtypes;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fmaxlines: integer;
    fsyntax: TAPI_syntax;

    procedure SetSearchType (t: tsearchtypes);
    procedure dummys(s: string);
    procedure setmaxlines(i: integer);
    function  SyntaxFontIndex(f: tfont): integer;
    function  SyntaxWordIndex(s: string): integer;
    procedure ClearSyntaxes;

  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Change; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  GetCaretPos: TPoint; override;
    procedure SetCaretPos(const Value: TPoint); override;
    function  PosY: integer;
    function  PosX: integer;

    function  MousePosX: integer;
    function  MousePosY: Integer;
    function  GetWordAtPoint (x, y: integer): string;

    procedure AddSyntax(word: string; font: tfont);
    procedure DeleteSyntax(word: string);
    procedure MarkWords(TheWord: string; PosStart, PosEnd: integer; font: tfont); overload;
    procedure MarkWords(TheWord: string; font: tfont); overload;
    function  GetWord: string;

    procedure ScrollToTop;
    procedure ScrollToBottom;

    function  OpenFileFast(Const Filename: string): boolean;

    function  GetLastVisibleLine: Integer;
    function  GetFirstVisibleLine: Integer;

  published
    property  Version: string read fversion write dummys stored false;
    property  SearchType: tsearchtypes read fsearchtype write setsearchtype;
    property  OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property  OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property  MaxLines: integer read fmaxlines write setmaxlines;
    property  Font;

  end;

procedure Register;

implementation

uses
  API_files;

{$r *.res}

const
  VERSIONINFOSTRING: string = 'r1.07/ari.pikivirta@kolumbus.fi';

procedure TAPI_richedit.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_richedit.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=VERSIONINFOSTRING;
  fsearchtype:=[stWholeWord];
  // syntax highlighting
  fsyntax.special:= tfont.create;
  fsyntax.special.Assign(font);
  fsyntax.number:= tfont.create;
  fsyntax.number.Assign(font);
  fsyntax.count:= 0;
  setlength(fsyntax.item, fsyntax.count);
  ControlStyle:= ControlStyle + [csOpaque];

  // limit maximum text length to 2Gb
  // instead of default 64k
  //Perform(EM_EXLIMITTEXT, 0, $7FFFFFF0);
  // uncommented 22062009api, since should already handle big files!
end;

//------------------------------------------------------------------------------
destructor TAPI_richedit.destroy;
begin
  ClearSyntaxes;
  fsyntax.special.free;
  fsyntax.number.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.OpenFileFast(Const Filename: string): boolean;
begin
  result:= FALSE;
  if fileexists(Filename) then
  try
    self.Lines.BeginUpdate;
    try
      self.Lines.Clear; // clear existing
      self.MaxLength:= api_files.GetFileSize(Filename) * 2; // set max length
      self.Lines.LoadFromFile(Filename); // load lines from file
    finally
      self.lines.EndUpdate;  
    end;
    result:= TRUE;
  except
    // ignore exception
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Richedit.GetLastVisibleLine: Integer;
const
  EM_EXLINEFROMCHAR = WM_USER + 54;
var
  r: TRect;
  i: Integer;
begin
  {
   The EM_GETRECT message retrieves the formatting rectangle
   of an edit control.
  }
  Perform(EM_GETRECT, 0, Longint(@r));
  r.Left := r.Left + 1;
  r.Top  := r.Bottom - 2;
  {
    The EM_CHARFROMPOS message retrieves information about the character
    closest to a specified point in the client area of an edit control
  }
  i := Perform(EM_CHARFROMPOS, 0, Integer(@r.topleft));
  {
    The EM_EXLINEFROMCHAR message determines which
    line contains the specified character in a rich edit control
  }
  Result := Perform(EM_EXLINEFROMCHAR, 0, i);
end;


//------------------------------------------------------------------------------
function TAPI_Richedit.GetFirstVisibleLine: Integer;
begin
{
  Sending the EM_GETFIRSTVISIBLELINE message to a multi-line edit control
  finds out which line is the first line visible.
  This is the line that is currently displayed at the top of the control.
}
  Result := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
end;

//------------------------------------------------------------------------------
function TAPI_richedit.SyntaxFontIndex(f: tfont): integer;
var
  i: integer;
begin
  result:= -1;
  for i:=0 to fsyntax.count-1 do
    if fsyntax.item[i].font=f then
    begin
      result:= i;
      break;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.SyntaxWordIndex(s: string): integer;
var
  i: integer;
begin
  result:= -1;
  for i:=0 to fsyntax.count-1 do
    if fsyntax.item[i].words.IndexOf(s)>-1 then
    begin
      result:= i;
      break;
    end;
end;

//------------------------------------------------------------------------------
procedure TAPi_richedit.ClearSyntaxes;
var
  i: integer;
begin
  // free all stuff
  for i:=0 to fsyntax.count-1 do
  begin
    fsyntax.item[i].words.free;
    fsyntax.item[i].font.free;
  end;
  // set size to zero
  fsyntax.count:= 0;
  setlength(fsyntax.item, fsyntax.count);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.AddSyntax(word: string; font: tfont);
var
  w, f: integer;
begin
  // check for current indexes
  f:= syntaxfontindex(font);
  w:= syntaxwordindex(word);
  // already ok, exit immediately
  if (f=w) then exit;
  // delete existing
  DeleteSyntax(word);
  // check font once again
  f:= syntaxfontindex(font);
  // add new font
  if (f<0) then
  begin
    // new font with new word
    fsyntax.count:= fsyntax.count + 1;
    setlength(fsyntax.item, fsyntax.count);
    fsyntax.item[fsyntax.count-1].words:= tstringlist.create;
    fsyntax.item[fsyntax.count-1].words.text:= word;
    fsyntax.item[fsyntax.count-1].font:= tfont.create;
    fsyntax.item[fsyntax.count-1].font.assign( font );
  end else
  // font exists, just add new word
  begin
    fsyntax.item[f].words.add(word);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.DeleteSyntax(word: string);
var
  w,i: integer;
begin
  w:= syntaxwordindex(word);
  if (w>-1) then
  begin
    // delete from words list
    fsyntax.item[w].words.Delete(fsyntax.item[w].words.IndexOf(word));
    // remove whole font?
    if fsyntax.item[w].words.Count<1 then
    begin
      fsyntax.item[w].words.Free;
      fsyntax.item[w].font.Free;
      for i:=w to fsyntax.count-2 do
        fsyntax.item[i]:= fsyntax.item[i+1];
      fsyntax.count:= fsyntax.count - 1;
      setlength(fsyntax.item, fsyntax.count);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.SetSearchType (t: tsearchtypes);
begin
  if t<>fsearchtype then
    fsearchtype:=t;
end;

//------------------------------------------------------------------------------
procedure TAPI_Richedit.ScrollToTop;
begin
  Perform(WM_VSCROLL, SB_TOP, 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.ScrollToBottom;
begin
  Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.Change;
var
  pt: tpoint;
  l,s,i,j: integer;
begin
  if not assigned(owner) then exit;

  pt:= getcaretpos;                           // get cursor position

  // syntax highlighiting
  if (fsyntax.count>0) then
  begin
    // get current length
    l:= LongRec(Perform(EM_GETSEL, 0, 0)).Hi;
    // start from beginning of this line
    s:= l - pt.x;
    // if length exists
    if l>0 then
      // go trough fonts
      for i:=0 to fsyntax.count-1 do
        // check words under this font
        for j:= 0 to fsyntax.item[i].words.count-1 do
          // mark words
          markwords(fsyntax.item[i].words[j], s, l, fsyntax.item[i].font);
  end;

  // max lines checking
  if (fmaxlines>0) and (lines.count>fmaxlines) then
  begin
    if (pt.Y<3) then                          // if below first line
    begin
      while lines.count>fmaxlines do          // while more than needded
        lines.delete(lines.count-1);          // delete last line
      //selstart:= 0;                           // set cursor to beginning
    end else
    begin
      while lines.Count>fmaxlines do          // while more than needed
        lines.Delete(0);                      // remove from beginning
      //selstart:= length(lines.text);          // goto last pos
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.setmaxlines(i: Integer);
begin
  if (i<>fmaxlines) and ((i>2) or (i=0)) then
  begin
    fmaxlines:= i;
    change;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.GetCaretPos: TPoint;
begin
  Result.X:= LongRec(Perform(EM_GETSEL, 0, 0)).Hi;
  Result.Y:= Perform(EM_LINEFROMCHAR, Result.X, 0);
  Result.X:= Result.X - Perform(EM_LINEINDEX, -1, 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.SetCaretPos(const Value: TPoint);
var
  CharIdx: Integer;
begin
  CharIdx := Perform(EM_LINEINDEX, Value.y, 0) + Value.x;
  Perform(EM_SETSEL, CharIdx, CharIdx);
end;

//------------------------------------------------------------------------------
function TAPI_richedit.MousePosY: Integer;
var
  pt: tpoint;
begin
  GetCursorPos(Pt);
  result:= pt.y;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.MousePosX: Integer;
var
  pt: tpoint;
begin
  GetCursorPos(Pt);
  result:= pt.x;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.PosY: Integer;
var
  pt: tpoint;
begin
  pt:= getcaretpos;
  result:= pt.y;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.PosX: Integer;
var
  pt: tpoint;
begin
  pt:= getcaretpos;
  result:= pt.x;
end;

//------------------------------------------------------------------------------
function TAPI_richedit.GetWord: string;
var
  pt: tpoint;
  x,y: integer;
  i,j: Integer;
  len: integer;
  s: string;
begin
  result:='';

  pt:= getcaretpos;
  x:= pt.x;
  y:= pt.y;

  s:= Lines[y];
  len:= length(s);

  if len<1 then exit;
  if (x<0) or (x>len-1) then exit;
  if (y<0) or (y>lines.count-1) then exit;

  // find word start
  i:=x+1;
  if (i>0) and (i<=len) then
    while (i>0) and (s[i]<>' ') do
      i:=i-1;

  // find word end
  j:=x+1;
  if (j>0) and (j<=len) then
    while (j<=len) and (s[j]<>' ') do
      j:=j+1;

  result:=Copy(s, i, j-i);
end;

//------------------------------------------------------------------------------
function TAPI_richedit.GetWordAtPoint (x, y: integer): string;
var
  pt: tpoint;
  icindex: integer;
  icoffset: integer;
  xx,yy: integer;
  i,j: Integer;
  len: integer;
  s: string;
begin
  result:='';

  pt:=point(x,y);

  icindex:=Perform(EM_CHARFROMPOS, 0, Integer(@Pt));
  if icindex<0 then Exit;

  yy:=Perform(EM_EXLINEFROMCHAR, 0, icindex);
  if lines.count<yy then Exit;

  icoffset:=icindex-Perform(EM_LINEINDEX, yy, 0);
  xx:=icoffset;

  s:=Lines[yy];
  len:=length(s);
  if (xx<0) or (xx>len-1) then exit;
  if (yy<0) or (yy>lines.count-1) then exit;

  // find word start
  i:=xx+1;
  if (i>0) and (i<=len) then
    while (i>0) and (s[i]<>' ') do
      i:=i-1;

  // find word end
  j:=xx+1;
  if (j>0) and (j<=len) then
    while (j<=len) and (s[j]<>' ') do
      j:=j+1;

  result:=Copy(s, i, j-i);
end;

//------------------------------------------------------------------------------
procedure TAPI_richedit.MarkWords(TheWord: string; PosStart, PosEnd: integer; font: tfont);
var
  pt: tpoint;
  next: integer;
begin
  pt:= getcaretpos;
  next:= FindText(TheWord, PosStart, PosEnd, fsearchtype);
  while next>-1 do
  begin
    SelStart:= next;
    sellength:= length(theword);
    selattributes.assign( font );
    Selstart:= next + length(theword);
    sellength:= 0;
    SelAttributes:=DefAttributes;
    next:= FindText(Theword, next+1, PosEnd, fsearchtype);
  end;
  setcaretpos(pt);
end;

procedure TAPI_richedit.MarkWords(TheWord: string; font: tfont);
begin
  Markwords(theword, 0, length(lines.text), font);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_richedit]);
end;

end.
