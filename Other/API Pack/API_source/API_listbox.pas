unit API_listbox;

//------------------------------------------------------------------------------
// listbox with new features like odd/even coloring, progressbars on each row,
// and columns made easy.
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
// 01062009, r1.22, ari pikivirta
//  * added AsDateTimeString option to SortByColumn function
//
// 01072008, r1.21, ari pikivirta
//  * bug fixed on formatcolumns function (invalidate when resizing)
//
// 21062008, r1.20, ari pikivirta
//  * columncount can be now (again) set even there's no lines on the listbox
//
// 13062008, r1.19, ari pikivirta
//  * added findtext, findnext and findprev functions
//
// 18122007, r1.17, ari pikivirta
//  * added disabled line string and font, default is "<!--" from html world
//
// 16052007, r1.17, ari pikivirta
//  * added export to html text function with possibility to have labels as
//    separate list
//
// 05032007, r1.16, ari pikivirta
//  * fixed problem with multiselect
//
// 18022007, r1.15, ari pikivirta
//  * added sortbycolumn function (ascending / descending)
//
// 02122006, r1.14, ari pikivirta
//  * added selected as individual font (was previously only font color) 
//
// 27112006, r1.13, ari pikivirta
//  * fixed bug in setcolvalue function
//
// 29082006, r1.12, ari pikivirta
//  * added formatcolumns procedure
//  * changed columncount property to use parent's columns instead
//
// 26032006, r1.11, ari pikivirta
//  * added possibility to add background picture into the listbox,
//    if assigned only progressbar will be shown..
//
// 20032006, r1.10, ari pikivirta
//  * overloaded get item, get name and get get value functions
//  * added set column value functions overloaded
//
// 22112004, r1.09, ari pikivirta
// * added get column value function
//
// 11102004, r1.08, ari pikivirta
// * added possibility to synchronize (force) all font names same with main font
//
// r1.07, 12082004, ari pikivirta
// * changed mouseoverfont property name to fontmouseover to get it next
//   to real font property for easy changing of both
//
// r1.06, ari pikivirta
// * added onmousenter and onmouselave events
// * added mouseover color and font
//
// r1.05, ari pikivirta
// * added custom column widths
// * removed "normal" columns property
// * fixed get selected function
//
// r1.04, ari pikivirta
// * removed inedit...
//
// r1.03, ari pikivirta
// * added inedit possibility
//
// r1.02, ari pikivirta
// * added api_listbox synchronization possibility
//
// r1.01, ari pikivirta
// * added progressbar stuff to show something special..
// * added refreshwidths to rezise the column according to max textwidth
//
// r1.00, ari pikviirta

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, Types,
  ComCtrls;

type
  TAPI_ListEditLeave = procedure(
    sender: tobject;
    item: integer;
    text: string) of Object;

  TAPI_listbox = class(TListBox)
  private
    fversion: string;
    fvisiblelines: integer;
    flinecolorselected: tcolor;
    fuselinecoloring: boolean;
    flinecolorodd: tcolor;
    flinecoloreven: tcolor;
    fshowprogressbar: boolean;
    fprogress: tstringlist;
    fprogresscolor: tcolor;
    ffontselected: tfont;
    fmouseisover: boolean;
    fmouseoverfont: tfont;
    fmouseovercolor: tcolor;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fcolumnseparator: string;
    fcolumncount: integer;
    fdefaultcolumnwidth: integer;
    fcolumnwidth: array of integer;
    fsynchronizedlist: tapi_listbox;
    fpicture: tbitmap;
    fenabledlinestring: string;
    fdisabledfont: tfont;
    ffindpos: integer;

//    procedure settextcolorselected(c: tcolor);
    procedure setlinecolorselected(c: tcolor);
    procedure setuselinecoloring(b: boolean);
    procedure setlinecolorodd(c: tcolor);
    procedure setlinecoloreven(c: tcolor);
    function  locateprogress(itemindex: integer): integer;
    procedure setshowprogressbar(b: boolean);
    procedure setprogresscolor(c: tcolor);
    procedure setdefaultcolumnwidth ( i: integer );
    procedure setcolumncount ( i: integer );
    procedure setcolumnseparator ( s: string );
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure setsynchronizedlist(al: tapi_listbox);
    procedure dummys(s: string);
    procedure setmouseoverfont( f: tfont );
    procedure setselectedfont( f: tfont );
    procedure setdisabledfont(f: tfont);
    procedure setenabledlinestring(s: string);
    procedure setbitmap(bmp: tbitmap);
    procedure Int_QuickSort(aList: TAPI_listbox; L, R: Integer);
    function  getcolumncount: integer;


  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawItem(Index: Integer; Rec: TRect; State: TOwnerDrawState); override;
//    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
//    procedure MeasureItem(Index: Integer; var Height: Integer); override;

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure RefreshWidths;

    procedure GetSelected(List: TStrings);
    function MoveItemUp (pos: integer): boolean;
    function MoveItemDown (pos: integer): boolean;

    function GetItem(item: string): integer; overload;
    function GetItem(index: integer): string; overload;
    function GetItemAsInt(index: integer): integer;
    function GetItemAsFloat(index: integer): double;

    function GetName(name: string): integer; overload;
    function GetName(index: integer): string; overload;
    function GetNameAsInt(index: integer): integer; overload;
    function GetNameAsFloat(index: integer): double; overload;

    function GetValue(index: integer): string; overload;
    function GetValue(value: string): integer;overload;
    function GetValueAsInt(name: string): integer; overload;
    function GetValueAsInt(index: integer): integer; overload;
    function GetValueAsFloat(name: string): double; overload;
    function GetValueAsFloat(index: integer): double; overload;

    procedure SetProgress(item: integer; position: double);
    function  GetProgress(item: integer): double;
    procedure DeleteProgress(item: integer);

    function  GetColValue(index, column: integer): string;
    function  SetColValue(index, column: integer; value: string): boolean; overload;
    function  SetColValue(index, column: integer; value: integer): boolean; overload;
    function  SetColValue(index, column: integer; value: double): boolean; overload;
    function  SetColWidth(column, width: integer): boolean;
    function  GetColWidth(column: integer): integer;
    function  CountColumns(row: integer): integer;

    procedure FormatColumns(hdr: theadercontrol);
    procedure SortByColumn(Const col: integer; Const ascending: boolean = TRUE; Const AsDatetimeString: Boolean = FALSE );

    function  ExportHtmlText (var s: string; Labels: tstringlist ): boolean; overload;
    function  ExportHtmlText (var s: string): boolean; overload;

    procedure QuickSort(First, Last: integer);

    function  FindText(TextToFind: string): integer; // returns row
    function  FindNext(TextToFind: string): integer; // returns row
    function  FindPrev(TextToFind: string): integer; // returns row

  published
    property Version: string read fversion write dummys stored false;
    property VisibleLinesCount: integer read fvisiblelines;
    property LineColorSelected: tcolor read flinecolorselected write setlinecolorselected;
    property LineColoring: boolean read fuselinecoloring write setuselinecoloring;
    property LineColorOdd: tcolor read flinecolorodd write setlinecolorodd;
    property LineColorEven: tcolor read flinecoloreven write setlinecoloreven;
    property MouseIsOver: boolean read fmouseisover write fmouseisover;
    property MouseOverColor: tcolor read fmouseovercolor write fmouseovercolor;
    property FontMouseOver: tfont read fmouseoverfont write setmouseoverfont;
    property FontSelected: tfont read ffontselected write setselectedfont;
    property FontDisabled: tfont read fdisabledfont write setdisabledfont;
    property DisableString: string read fenabledlinestring write setenabledlinestring;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property Picture: tbitmap read fpicture write setbitmap;
    property ProgressExists: boolean read fshowprogressbar write setshowprogressbar;
    property ProgressColor: tcolor read fprogresscolor write setprogresscolor;
    property ColumnSeparator: string read fcolumnseparator write setcolumnseparator;
    property ColumnDefaultWidth: integer read fdefaultcolumnwidth write setdefaultcolumnwidth;
    property SynchronizedList: tapi_listbox read fsynchronizedlist write setsynchronizedlist;
    property Columns: integer read getcolumncount write setcolumncount;
    property Font;

  end;

procedure Register;

implementation

uses
  //api_strings; //strutils;
  strutils;

{$r *.res}

const
  versioninfostring: string = 'r1.22/ari.pikivirta@kolumbus.fi';

procedure TAPI_listbox.dummys(s: string); begin end;

//------------------------------------------------------------------------------
function TAPI_listbox.locateprogress(itemindex: integer): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to fprogress.Count-1 do
    if fprogress.Names[i]=inttostr(itemindex) then
    begin
      result:=i;
      break;
    end;
end;

//------------------------------------------------------------------------------
constructor TAPI_listbox.create(aowner: tcomponent);
begin
  inherited create(aowner);
  Style:=lbOwnerDrawFixed;
  color:=clwhite;
  fversion:=versioninfostring;
  fvisiblelines:=0;
  flinecolorselected:=clyellow;
//  ftextcolorselected:=clblack;
  fuselinecoloring:=true;
  flinecoloreven:=clwhite;
  flinecolorodd:=$cccccc;

  fshowprogressbar:=false;
  fprogresscolor:=cllime;
  fprogress:=tstringlist.create;
  fprogress.clear;

  fmouseoverfont:= tfont.create;
  fmouseoverfont.Assign( font );

  ffontselected:= tfont.create;
  ffontselected.assign( font );
  fmouseovercolor:= color;
  fcolumnseparator:='||';
  fmouseisover:= falsE;
  fenabledlinestring:= '<!--';

  fdefaultcolumnwidth:= 92;
  fcolumncount:= 0;
  setlength( fcolumnwidth, fcolumncount );

  fpicture:= tbitmap.create;

  fdisabledfont:= tfont.create;
  fdisabledfont.assign( font );

  ffindpos:= -1;
end;

//------------------------------------------------------------------------------
destructor TAPI_listbox.destroy;
begin
  fprogress.Free;
  fprogress:= nil;
  fmouseoverfont.free;
  fmouseoverfont:= nil;
  ffontselected.free;
  ffontselected:= nil;
  fdisabledfont.free;
  fdisabledfont:= nil;
  fpicture.free;
  fpicture:= nil;
  inherited destroy;
end;

{
//------------------------------------------------------------------------------
procedure TAPI_listbox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  msg.Result:=0;
end;
}

//------------------------------------------------------------------------------
procedure TAPI_listbox.MouseEnter(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseisover:=true;
    if assigned(fonmouseenter) then
      fonmouseenter(self);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.MouseLeave(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseisover:=false;
    if assigned(fonmouseleave) then
      fonmouseleave(self);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.Int_QuickSort(aList: TAPI_listbox; L, R: Integer);
var
  I, J, P: Integer;
  Obj: string;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while CompareStr(aList.GetItem(I), aList.GetItem(P)) < 0 do Inc(I);
      while CompareStr(AList.GetItem(J), AList.GetItem(P)) > 0 do Dec(J);
      if I<=J then
      begin
        // swap strings
        Obj:= AList.Items[i];
        AList.items[i]:= alist.items[j];
        alist.items[j]:= obj;
        if P = I then P := J
          else if P = J then P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      int_QuickSort(aList, L, J);
    L := I;
  until I >= R;
end;

procedure TAPI_listbox.QuickSort(First, Last: integer);
begin
  int_quicksort(self, first, last);
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.FormatColumns(hdr: theadercontrol);
var
  i: integer;
begin
  fcolumncount:= hdr.Sections.Count;
  setlength(fcolumnwidth, fcolumncount);
  for i:=0 to fcolumncount-1 do
    fcolumnwidth[i]:= hdr.Sections[i].Width;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setbitmap(bmp: tbitmap);
begin
  fpicture.assign(bmp);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setmouseoverfont( f: tfont );
begin
  if f<>fmouseoverfont then
  begin
    fmouseoverfont.assign( f );
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setselectedfont( f: tfont );
begin
  if f<>ffontselected then
  begin
    ffontselected.assign(f);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setdisabledfont(f: tfont);
begin
  if f<>fdisabledfont then
  begin
    fdisabledfont.assign(f);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setenabledlinestring(s: string);
begin
  if fenabledlinestring<>s then
  begin
    fenabledlinestring:= s;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  tlbVisible: Boolean;
begin
  tlbVisible := (Parent <> nil) and IsWindowVisible(Handle);
  if tlbVisible then ShowWindow(Handle, SW_HIDE);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if tlbVisible then ShowWindow(Handle, SW_SHOW);
end;

//------------------------------------------------------------------------------
(*
function TAPI_listbox.textflags: cardinal;
begin
  result:= DT_NOPREFIX;
  //flags:= flags or DT_WORDBREAK; //if fwordwrap then flags:= flags or DT_WORDBREAK;
  result:= result or DT_LEFT; //if alignment = taLeftJustify then flags:= flags or DT_LEFT;
  //if alignment = taRightJustify then flags:= flags or DT_RIGHT;
  //if alignment = taCenter then flags:= flags or DT_CENTER;
  //flags:= flags or DT_VCENTER;  // vertical center..
  result:= result or DT_TOP;
end;
*)

//------------------------------------------------------------------------------
(*
function TAPI_listbox.SplitLineText(Index: integer): tstringlist;
begin
  result:= tstringlist.create;
  result.clear;
end;
*)

//------------------------------------------------------------------------------
(*
procedure TAPI_listbox.MeasureItem(Index: Integer; var Height: Integer);
begin
  //temprect:= rect(rec.Left+posi, rec.top, rec.Left+posi+fcolumnwidth[currentcolumn], rec.Bottom);
  //windows.DrawText(canvas.Handle, pchar(temptext), length(temptext), temprect, textflags);
  height:= 50;
end;
*)

//------------------------------------------------------------------------------
procedure TAPI_listbox.DrawItem(Index: Integer; Rec: TRect; State: TOwnerDrawState);
var
  i: integer;
  d: double;
  d2: integer;
  textheight: integer;
//  sel: boolean;
  texti: string;
  posi: integer;
  Seppos: integer;
  temptext: string;
  currentcolumn: integer;
  ImageRect1, imagerect2, ResRect, NewRect: trect;
//  temprect: trect;
begin
  texti:=Items[Index];

  // solid color
  if fmouseisover then
  begin
    Canvas.Brush.Color:= fmouseoverColor;
    canvas.Font.Assign( fmouseoverfont );
  end else
  begin
    canvas.Brush.color:= color;
    canvas.font.Assign( font );
  end;
  canvas.Brush.Style:=bssolid;
  Canvas.FillRect(Rec);

  textheight:=canvas.TextHeight(texti);
  if textheight>0 then
    fvisiblelines:=round(height/textheight)
    else fvisiblelines:=0;

  if (odSelected in state) then
  begin
    // selected
    Canvas.Brush.Color:= FLineColorSelected;
    canvas.Font.Assign( ffontselected );
    Canvas.FillRect(Rec);
  end else
  begin
    // normal painting
    Canvas.Font.Color := Font.Color;
    // even lines coloring
    if fuselinecoloring then
    begin
      if (Index mod 2) = 0 then
        canvas.Brush.Color := FLineColorEven else
        Canvas.Brush.Color := FLineColorOdd;
      Canvas.FillRect(Rec);
    end;
  end;

  // background picture
  if assigned(fpicture) then
  begin
    ImageRect1 := Classes.Rect(0,0,
      fpicture.Width,
      fpicture.Height);
    ImageRect2 := ImageRect1;
    IntersectRect(ResRect, ImageRect2, Rec);
    NewRect.Left:=ImageRect1.Left;
    NewRect.Top:=ImageRect1.Top+ResRect.Top-ImageRect2.Top;
    NewRect.Right:=ImageRect1.Right;
    NewRect.Bottom:=ImageRect1.Bottom+ResRect.Bottom-ImageRect2.Bottom;
    Canvas.CopyRect(ResRect,fpicture.Canvas,NewRect);
    Canvas.Brush.Style:=bsClear;
  end;

  // progressbars
  if fshowprogressbar then
  begin
    i:=locateprogress(index);
    if i>-1 then
    begin
      d:=getprogress(i);
      d2:=rec.left+round(d*((rec.Right-rec.Left)/100));
      if d>-1 then
      begin
        canvas.Brush.color:= fprogresscolor;
        canvas.FillRect(rect(rec.left,rec.top,d2-1,rec.Bottom-1));
      end;
    end else
      // failed to locate progress value
  end;

  // check for enabled line beginning
  if (fenabledlinestring<>'') and (assigned(fdisabledfont)) then
    if (copy(texti,1,length(fenabledlinestring))=fenabledlinestring) then
    begin
      canvas.font.assign(fdisabledfont);
      delete(texti,1,length(fenabledlinestring));
    end;

  // draw text..
  canvas.Brush.style:=bsclear;
  posi:=0;
  currentcolumn:=0;
  repeat

    // find first column separator
    seppos:=pos(fcolumnseparator, texti);
    if seppos>0 then
    begin

      // get text upto the separator and define the area
      temptext:=copy(texti,1,seppos-1);
      delete(texti,1,seppos+length(fcolumnseparator)-1);

      // check that there is columns defined
      if currentcolumn<fcolumncount then
      begin

        // limit text to columnwidth..
        //temprect:= rect(rec.Left+posi, rec.top, rec.Left+posi+fcolumnwidth[currentcolumn], rec.Bottom);
        //windows.DrawText(canvas.Handle, pchar(temptext), length(temptext), temprect, textflags);
        canvas.TextRect(rect( rec.Left+posi, rec.top, rec.Left+posi+fcolumnwidth[ currentcolumn], rec.Bottom), rec.left+posi+1, rec.Top, temptext);
        posi:=posi+fcolumnwidth[ currentcolumn ];

      end else
      begin

        // no limitation for text
        //temprect:= rect(rec.Left+posi, rec.top, rec.Right, rec.Bottom);
        //windows.DrawText(canvas.Handle, pchar(temptext), length(temptext), temprect, textflags);
        canvas.textout(rec.left+posi, rec.top, temptext);
        posi:=posi+fdefaultcolumnwidth;

      end;

      currentcolumn:= currentcolumn+1;
    end else

    // no separators found
    begin
      //temprect:= rect(rec.Left+posi, rec.top, rec.Right, rec.Bottom);
      //windows.DrawText(canvas.Handle, pchar(texti), length(texti), temprect, textflags);
      Canvas.TextOut(Rec.Left+posi, Rec.Top, texti);
    end;

  until seppos=0;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:=Params.Style or LBS_USETABSTOPS;
//  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setdefaultcolumnwidth ( i: integer );
begin
  if fdefaultcolumnwidth<> i then
  begin
    fdefaultcolumnwidth:=i;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.CountColumns(row: integer): integer;
var
  p1, p2: integer;
begin
  result:= 0;

  p1:= 1;
  p2:= posex( fcolumnseparator, items[row], p1+1 );
  while (p2>0) do
  begin
    p1:= p2;
    p2:= posex( fcolumnseparator, items[row], p1+1 );
    result:= Result + 1;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.getcolvalue(index, column: integer): string;
var
  p1, p2, l, c: integer;
  value: string;
begin
  l:= length(fcolumnseparator);
  p1:= 1;
  c:= 0;
  if (index>-1) and (index<items.count) then
  begin
    repeat
      p2:= posex( fcolumnseparator, items[index], p1 );
      if p2>0 then
      begin
        value:= copy(items[index], p1, p2-p1);
        p1:= p2+l;
      end else
        value:= copy(items[index], p1, length(items[index]));
      c:= c + 1;
    until (p2<1) or (c>column);
  end else
    value:='';
  result:= value;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.SortByColumn(Const col: integer; Const ascending: boolean = TRUE; Const AsDatetimeString: Boolean = FALSE );
var
  i, j: integer;
  s1, s2: string;
  d1, d2: Tdatetime;
begin
  items.BeginUpdate;
  try
    for i:=0 to (items.count-2) do    // at least two rows must exist
      for j:=i+1 to (items.count-1) do  // go trough rest of them
      begin
        s1:= getcolvalue(i, col);     // item from beginning
        s2:= getcolvalue(j, col);     // item from end
        if (AsDateTimeString) and (trystrtodatetime(s1, d1)) and (trystrtodatetime(s2, d2)) then
        begin
          s1:= FloatToStr(d1);
          s2:= FloatToStr(d2);
        end;
        if ascending then
        begin                         // ascending
          if s1>s2 then               // if [i] is greater
            items.exchange(i,j);      // do exchange lines
        end else
        begin                         // descending
          if s2>s1 then               // if [j] is greater
            items.exchange(i,j);      // do exchange items
        end;
      end;
  finally
    items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.SetColValue(index, column: integer; value: string): boolean;
var
  col, p, p2: integer;
  s: string;
begin
  if (index>-1) and (index<items.count) then
  begin
    s:= items[index];

    // find the correct column
    col:= 1;
    p:= pos(fcolumnseparator,s);                      // find first separator
    while (col<column) and (p>0) do                   // and there is columns left
    begin
      p:= posex(fcolumnseparator,s,p+1);              // next separator position
      col:= col + 1;                                  // increase column count
    end;

    // insert new value into the found position
    p:= p + length(fcolumnseparator);
    insert(value, s, p);                              // add value
    p:= p + length(value);

    // delete old value
    p2:= posex(fcolumnseparator,s,p);                 // find ending of this column
    if p2>0 then                                      // if ending separator found
      delete(s,p,p2-p)                                // delete old value
      else delete(s,p,length(s));                     // delete rest

    items[index]:= s;                                 // assign to list
    result:= true;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.SetColValue(index, column: integer; value: integer): boolean;
begin
  result:= setcolvalue(index, column, inttostr(value));
end;

//------------------------------------------------------------------------------
function TAPI_listbox.SetColValue(index, column: integer; value: double): boolean;
begin
  result:= setcolvalue(index, column, floattostr(value));
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setcolumncount ( i: integer );
var
  j: integer;
begin
  if i<=0 then
  begin
    fcolumncount:=0;
    setlength ( fcolumnwidth, fcolumncount);
    invalidate;
  end else
  if i<fcolumncount then
  begin
    fcolumncount:=i;
    setlength ( fcolumnwidth, fcolumncount);
    invalidate;
  end else
  if i>fcolumncount then
  begin
    setlength( fcolumnwidth, i );
    for j:=fcolumncount-1 to i-1 do
      fcolumnwidth[j]:= fdefaultcolumnwidth;
    fcolumncount:= i;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.getcolumncount: integer;
var
  i, v: integer;
begin
  result:= 0;
  v:= 0;
  for i:= 0 to items.count-1 do
  begin
    v:= CountColumns(i);
    if v>result then result:= v;
  end;
  if v<1 then
    result:= fcolumncount;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setcolumnseparator ( s: string );
begin
  if fcolumnseparator<> s then
  begin
    fcolumnseparator:= s;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.setcolwidth ( column, width: integer ): boolean;
begin
  result:=False;
  if (column<0) or (column>fcolumncount-1) then exit;
  fcolumnwidth[ column ]:= width;
  invalidate;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.getcolwidth ( column: integer ): integer;
begin
  result:=-1;
  if (column<0) or (column>fcolumncount-1) then exit;
  result:= fcolumnwidth [column];
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.RefreshWidths; // not checking custom separator...
var
  i: integer;
  width: integer;
  maxwidth: integer;
  s: string;
begin
  maxwidth:=0;
  for i:=0 to count-1 do
  begin
    s:=items[i];
    width:=canvas.TextWidth(s);
    if width>maxwidth then
      maxwidth:=width;
  end;
  self.TabWidth:=maxwidth;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setuselinecoloring(b: boolean);
begin
  if b<>fuselinecoloring then
  begin
    fuselinecoloring:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setshowprogressbar(b: boolean);
begin
  if b<>fshowprogressbar then
  begin
    fshowprogressbar:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setprogresscolor(c: tcolor);
begin
  if fprogresscolor<>c then
  begin
    fprogresscolor:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure Tapi_listbox.GetSelected(List: TStrings);
var
  i: integer;
begin
  list.clear;
  for i:=0 to Items.count-1 do
    if self.Selected[i] then
      list.add(self.Items[i]);
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setprogress(item: integer; position: double);
var
  i: integer;
begin
  if position>100 then position:=100;
  if position<0 then position:=0;
  if item>self.Items.Count-1 then exit;
  if item<0 then exit;

  i:=locateprogress(item);
  if i>-1 then
  begin
    // update existing item
    fprogress.Values[inttostr(item)]:=floattostr(position);
  end else
  begin
    // add new item
    fprogress.Add(inttostr(item)+'='+floattostr(position));
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_listbox.getprogress(item: integer): double;
var
  i: integer;
begin
  result:=-1;
  if item>self.items.count-1 then exit;
  if item<0 then exit;
  i:=locateprogress(item);
  if i>-1 then
  begin
    result:=strtofloat(fprogress.Values[inttostr(item)]);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.deleteprogress(item: integer);
var
  i: integer;
begin
  if item>items.count-1 then exit;
  if item<0 then exit;
  i:=locateprogress(item);
  if i>-1 then
    fprogress.Delete(i);
end;

//------------------------------------------------------------------------------
function TAPI_listbox.MoveItemUp (pos: integer): boolean;
begin
  result:=false;
  if pos<1 then exit;
  if pos>self.items.count-1 then exit;
  self.Items.Exchange(pos, pos-1);
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.MoveItemDown (pos: integer): boolean;
begin
  result:=False;
  if pos>items.count-2 then exit;
  if pos<0 then exit;
  items.exchange(pos, pos+1);
  result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setlinecolorselected(c: tcolor);
begin
  if flinecolorselected<>c then
  begin
    flinecolorselected:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
(*
procedure TAPI_listbox.settextcolorselected(c: tcolor);
begin
  if textcolorselected<>c then
  begin
    ftextcolorselected:=c;
    invalidate;
  end;
end;
*)

//------------------------------------------------------------------------------
procedure TAPI_listbox.setlinecolorodd(c: tcolor);
begin
  if flinecolorodd<>c then
  begin
    flinecolorodd:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setlinecoloreven(c: tcolor);
begin
  if flinecoloreven<>c then
  begin
    flinecoloreven:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetItem(item: string): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to items.count-1 do
    if items[i]=item then
    begin
      result:=i;
      break;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetItem(index: integer): string;
begin
  if (index<items.count) then
    result:= items[index]
    else result:= '';
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetItemAsInt(index: integer): integer;
begin
  result:=-1;
  if index>-1 then
  try
    result:=strtoint(Items[index]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetItemAsFloat(index: integer): double;
begin
  result:=-1;
  if index>-1 then
  try
    result:=strtofloat(Items[index]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetName(name: string): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to items.count-1 do
    if Items.Names[i]=name then
    begin
      result:=i;
      break;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetName(index: integer): string;
begin
  if (index < items.count) then
    result:= items.names[index]
    else result:= '';
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetNameAsFloat(index: integer): double;
begin
  result:=-1;
  if (index>-1) and (index<items.count) then
  try
    result:=strtofloat(items.Names[index]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetNameAsInt(index: integer): integer;
begin
  result:=-1;
  if (index>-1) and (index<items.count) then
  try
    result:=strtoint(items.names[index]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValue(value: string): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to items.count-1 do
    if Items.Values[Items.names[i]]=value then
    begin
      result:=i;
      break;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValue(index: integer): string;
begin
  if (index>-1) and (index<items.count) then
    result:= items.Values[items.names[index]]
    else result:= '';
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValueAsInt(index: integer): integer;
begin
  result:=-1;
  if (index>-1) and (index<items.count) then
  try
    result:=strtoint(items.Values[items.names[index]]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValueAsInt(name: string): integer;
var
  index: integer;
begin
  result:=-1;
  index:= GetName(name);
  if (index>-1) and (index<items.count) then
  try
    result:=strtoint(items.Values[items.names[index]]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValueAsFloat(index: integer): double;
begin
  result:=-1;
  if (index>-1) and (index<items.count) then
  try
    result:=strtofloat(items.Values[items.names[index]]);
  except
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.GetValueAsFloat(name: string): double;
var
  index: integer;
begin
  result:=-1;
  index:= GetName(name);
  if (index>-1) and (index<items.count) then
  try
    result:=strtofloat(items.Values[items.names[index]]);
  except
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.WMVScroll(var Msg: TMessage);
begin
  if assigned(fsynchronizedlist) then
    fsynchronizedlist.Perform( WM_VSCROLL, Msg.wParam, Msg.lParam );
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.WMHScroll(var Msg: TMessage);
begin
  if assigned(fsynchronizedlist) then
    fsynchronizedlist.Perform( WM_HSCROLL, Msg.wParam, Msg.lParam );
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.setsynchronizedlist(al: tapi_listbox);
begin
  fsynchronizedlist:=al;
end;

//------------------------------------------------------------------------------
procedure TAPI_listbox.Click;
begin
  if assigned(fsynchronizedlist) then
  begin
    if (fsynchronizedlist.count>ItemIndex) then
      fsynchronizedlist.itemindex:=itemindex;
    fsynchronizedlist.Click;
    fsynchronizedlist.Invalidate;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.ExportHtmlText (var s: string; Labels: tstringlist ): boolean;
var
  rows: integer;
  cols: integer;
  i: integer;
  j: integer;
  text: string;
  colsadded: boolean;
begin
  s:='<html>'+#13#10
    +'<body background=#cccccc>'+#13#10
    +'<table width=100% border=0>'+#13#10;

  // count columns
  cols:= CountColumns(0);
  rows:= count;

  // add labels
  if labels.count>0 then
  begin
    s:=s+'<tr>';
    for i:=0 to labels.count-1 do
      s:=s+'<td bgcolor=#000000><font size=2 color=#ffffff>'+labels[i]+'</td>';
    s:=s+'</tr>'+#13#10;
  end;

  // export as html text
  colsadded:= false;
  for j:=0 to rows-1 do
  begin
    s:=s+'<tr><a name="row'+inttostr(j)+'"></a>';
    for i:=0 to cols-1 do
    begin
      text:= self.GetColValue(j,i);
      s:=s+'<td bgcolor=#ffffff>';
      if not colsadded then
        s:=s+'<a name="col'+inttostr(i)+'"></a>';
      s:=s+'<font size=2 color=#000000>';
      s:=s+text+'</td>';
    end;
    s:=s+'</tr>'+#13#10;
    colsadded:= true;
  end;

  s:=s+'</table>'+#13#10
    +'</body>'+#13#10
    +'</html>';

  result:=true;
end;

function TAPI_listbox.ExportHtmlText (var s: string): boolean;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.clear;
    result:= exporthtmltext(s, sl);
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_listbox.FindText(TextToFind: string): integer; // returns row
var
  i: integer;
begin
  result:= -1;
  for i:=0 to items.count-1 do
    if pos(texttofind, items[i])>-1 then
    begin
      result:= i;
      ffindpos:= i;
      break;
    end;
end;

function TAPI_listbox.FindNext(TextToFind: string): integer; // returns row
var
  i: integer;
begin
  result:= -1;
  for i:=ffindpos+1 to items.count-1 do
    if pos(texttofind, items[i])>-1 then
    begin
      result:= i;
      ffindpos:= i;
      break;
    end;
end;

function TAPI_listbox.FindPrev(TextToFind: string): integer; // returns row
var
  i: integer;
begin
  result:= -1;
  for i:= ffindpos-1 downto 0 do
    if pos(texttofind, items[i])>-1 then
    begin
      result:= i;
      ffindpos:= i;
      break;
    end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_listbox]);
end;

end.
