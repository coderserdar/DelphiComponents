unit API_stringgrid;

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
// r1.07, 02112009, ari pikivirta
//  * fixed bug that caused stringgrid to no longer follow anhors defined
//  * doublebuffered set to true by default
//  * font colors should follow font definition in case they're equal with orig
//
// r1.06, 01062009, ari pikivirta
//  * added parameter to SortByCol to sort as datetime ("1.6.2009"..)  
//
// r1.05, 22012009, ari pikivirta
//  * added function AutoWidthColumn(column, maxwidth)
//  * added function AutoWidthAllColumns(maxwidth)
//
// r1.04/16012008, ari pikivirta
//  * added SortByColumn procedure
//  * removed html and body tags from html export function
//  * added text alignment properties (vertical & horizontal)
//
// r1.03/15052007, ari pikivirta
//  * added export to html text and file functions (similar than in workbook)
//
// r1.02/08122006, ari pikivirta
//  * added FormatColumns(headercontrol) function
//  * added Clear(ZeroCountsAlso) function, zerocounts will remove all rows
//    upto fixed rows
//  * added background colors and font colors for selected and focused cells
//  * added word wrapping as an default option
//
// r1.01/15082006, ari pikivirta
//  * fixed selected row and col property

interface

uses
  Windows, SysUtils, Classes, Controls, messages, ComCtrls,
  Types, Graphics, Grids;

type
  TMouseCellEvent = procedure (sender: tobject; col, row: integer) of object;
  TVerticalAlignment = (taVerticalCenter, taAlignTop, taAlignBottom);

  TAPI_stringgrid = class(TStringGrid)
  private
    fversion: string;
    fondestroy: tnotifyevent;
    fmousecol: integer;
    fmouserow: integer;
    fselectedcol: integer;
    fselectedrow: integer;
    fmultiline: boolean;
    fwordwrap: boolean;
    ffixfont: tcolor;
    fselfont: tcolor;
    ffocfont: tcolor;
    fbacksel: tcolor;
    fbackfoc: tcolor;
    fvalign: TVerticalAlignment;
    fhalign: TAlignment;
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure setfixfont(c: tcolor);
    procedure setfocfont(c: tcolor);
    procedure setselfont(c: tcolor);
    procedure setbacksel(c: tcolor);
    procedure setbackfoc(c: tcolor);
    function  textflags: cardinal;
    procedure setwordwrap(b: boolean);
    procedure setmultiline(b: boolean);
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

  protected
    fonmouseleavecell: TMouseCellEvent;
		fonmouseentercell: TMouseCellEvent;
  	fonmouseleave: TMouseCellEvent;
		fonmouseenter: TMouseCellEvent;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure DblClick; override;
    function  SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; State: TGridDrawState); override;
    procedure DoEnter; override;
    procedure DoExit; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetAsInteger(Const Acol, Arow: Integer): integer;
    function    GetAsFloat(Const Acol, Arow: Integer): double;
    procedure   FormatColumns(Const HDR: THeadercontrol);
    procedure   Clear(Const ZeroRowAndColAmount: boolean = FALSE);
    procedure   Loaded; override;
    procedure   CalculateRowHeight(Const ARow: integer);
    procedure   AutoWidthColumn(Const ColumnIndex: Integer; Const MaxWidth: Integer = -1);
    procedure   AutoWidthAllColumns(Const MaxWidth: Integer = -1);
    procedure   SortByColumn(Const ACol: Integer; Const Ascending: Boolean = TRUE; Const AsDateTime: Boolean = FALSE);
    function    ExportHtmlText (var S: string; NameLinks: boolean = false): boolean;
    function    ExportHtmlFile ( Filename: string; NameLinks: boolean = false): boolean;

  published
    property Version: string read fversion write dummys stored false;
    property SelectedRow: integer read fselectedrow write dummyi stored false;
    property SelectedCol: integer read fselectedcol write dummyi stored false;
    property MouseRow: integer read fmouserow write dummyi stored false;
    property MouseCol: integer read fmousecol write dummyi stored false;
    property OnDestroy: tnotifyevent read fondestroy write fondestroy;
    property OnMouseEnterCell: TMouseCellEvent read fonmouseentercell write fonmouseentercell;
    property OnMouseLeaveCell: TMouseCellEvent read fonmouseleavecell write fonmouseleavecell;
    property OnMouseEnter: TMouseCellEvent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: TMouseCellEvent read fonmouseleave write fonmouseleave;
    property MultilineCells: boolean read fmultiline write setmultiline;
    property WordWrap: boolean read fwordwrap write setwordwrap;
    property FontFixed: tcolor read ffixfont write setfixfont;
    property FontFocused: tcolor read ffocfont write setfocfont;
    property FontSelected: tcolor read fselfont write setselfont;
    property ColorFocused: tcolor read fbackfoc write setbackfoc;
    property ColorSelected: tcolor read fbacksel write setbacksel;
    property VerAlign: tverticalalignment read fvalign write fvalign;
    property HorAlign: talignment read fhalign write fhalign;

  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfostring: string = 'r1.07/ari.pikivirta)at(kolumbus.fi';

procedure TAPI_stringgrid.dummys(s: string); begin end;
procedure TAPI_stringgrid.dummyi(i: integer); begin end;

//------------------------------------------------------------------------------
constructor TAPI_stringgrid.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  fmousecol:= -1;
  fmouserow:= -1;
  fselectedcol:= -1;
  fselectedrow:= -1;
  fmultiline:= true;
  fwordwrap:= true;
  ffixfont:= font.Color;
  fselfont:= font.color;
  ffocfont:= font.color;
  fbacksel:= color;
  fbackfoc:= clyellow;
  fvalign:= taAlignTop;
  fhalign:= taLeftJustify;
  doublebuffered:= TRUE;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.CMParentFontChanged(var Message: TMessage); 
var
  ffix, fsel, ffoc: boolean;
begin
  ffix:= (color=ffixfont);
  fsel:= (color=fselfont);
  ffoc:= (color=ffocfont);
  inherited;
  if (ffix) then ffixfont:= color;
  if (fsel) then fselfont:= color;
  if (ffoc) then ffocfont:= color;
end;

//------------------------------------------------------------------------------
destructor TAPI_stringgrid.destroy;
begin
  if assigned(fondestroy) then fondestroy(self);
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.Loaded;
begin
  // does nothing (at least yet..)
  if font.Height>defaultrowheight then
  begin
    defaultrowheight:= font.height;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setfixfont(c: tcolor);
begin
  if c<>ffixfont then
  begin
    ffixfont:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.DoEnter;
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.DoExit;
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setfocfont(c: tcolor);
begin
  if c<>ffocfont then
  begin
    ffocfont:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setselfont(c: tcolor);
begin
  if c<>fselfont then
  begin
    fselfont:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setbacksel(c: tcolor);
begin
  if fbacksel<>c then
  begin
    fbacksel:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setbackfoc(c: tcolor);
begin
  if fbackfoc<>c then
  begin
    fbackfoc:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.Clear(Const ZeroRowAndColAmount: boolean = FALSE);
var
  i: integer;
begin
  // clear all rows
  for i:=0 to rowcount-1 do
  begin
    Rows[i].Clear;
  end;

  // adjust counts..
  if ZeroRowAndColAmount then
  begin
    // priority to rows..
    if FixedRows>0 then rowcount:= fixedrows
      else if fixedcols>0 then colcount:= fixedcols
        else  begin
                rowcount:= 0;
                colcount:= 0;
              end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setwordwrap(b: boolean);
begin
  if b<>fwordwrap then
  begin
    fwordwrap:= b;
    invalidate; // repaint
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.setmultiline(b: boolean);
begin
  if b<>fmultiline then
  begin
    fmultiline:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_Stringgrid.SortByColumn(Const ACol: Integer; Const Ascending: Boolean = TRUE; Const AsDateTime: Boolean = FALSE);
// last updated: 01062009api
var
  i,j: integer;
  temp: tstringlist;
  d1, d2: tdatetime;
  s1, s2: string;
begin
  if (Acol>-1) and (ACol<RowCount) then
  begin
    temp:= tstringlist.create;
    try
      for i:=0 to (RowCount-1) do
        for j:=0 to (i-1) do
        begin
          s1:= Cells[ACol, i];
          s2:= Cells[ACol, j];
          if (AsDateTime) and (trystrtodatetime(s1, d1)) and (trystrtodatetime(s2, d2)) then
          begin
            s1:= FloatToStr(d1);
            s2:= FloatToStr(d2);
          end;
          if ((Ascending) and (s1<s2) or (not Ascending) and (s1>s2)) then
          begin
            temp.Assign(rows[i]);
            rows[i]:= rows[j];
            rows[j].Assign(temp);
          end;
        end;
    finally
      temp.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.AutoWidthColumn(Const ColumnIndex: integer; Const MaxWidth: Integer = -1);
var
  AWidth: integer;
  I: integer;
  text: string;
  temprect: trect;
begin
  if (ColumnIndex<0) and (ColumnIndex>ColCount-1) then exit;
  AWidth:= 0;
  for i:=0 to rowcount-1 do
  begin
    text:= cells[ColumnIndex, i];
    if text<>'' then
    begin
      temprect:= rect(0, 0, self.Width, self.DefaultRowHeight);
      windows.DrawText(canvas.Handle, pchar(text), length(text), temprect, textflags + DT_CALCRECT);
      if temprect.right-temprect.left>AWidth then AWidth:= temprect.right-temprect.left;
    end;
  end;
  if (maxwidth>-1) then
    if Awidth>maxwidth then
      awidth:= maxwidth;
  self.ColWidths[columnindex]:= AWidth;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.AutoWidthAllColumns(Const MaxWidth: Integer = -1);
var
  i: integer;
begin
  for i:=0 to colcount-1 do autowidthcolumn(i, maxwidth);
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.CMMouseEnter(var message: TMessage);
begin
  fmousecol:=-1;
  fmouserow:=-1;
  if assigned(fonmouseleave) then fonmouseleave(self, fmousecol, fmouserow);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.Click;
begin
//  fselectedcol:= mousecol;
//  fselectedrow:= mouserow;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.FormatColumns(Const hdr: theadercontrol);
var
  i: integer;
begin
  ColCount:= hdr.Sections.Count;
  for i:=0 to colcount-1 do
    colwidths[i]:= hdr.sections[i].Width - self.GridLineWidth;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.DblClick;
begin
//  fselectedcol:= mousecol;
//  fselectedrow:= mouserow;
  inherited;
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  fselectedcol:= acol;
  fselectedrow:= arow;
  result:= inherited SelectCell(ACol, ARow);
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.CMMouseLeave(var message: TMessage);
begin
  fmousecol:=-1;
  fmouserow:=-1;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  newcol: integer;
  newrow: integer;
begin
  MouseToCell(x,y,newcol,newrow);
  if ((newcol<>fmousecol) or (newrow<>fmouserow))
    and (newcol>-1) and (newcol<colcount)
    and (newrow>-1) and (newrow<rowcount) then
  begin
    if Fmousecol<>-1 then
      if assigned(fonmouseleavecell) then	fonmouseleavecell(self,Fmousecol,Fmouserow)
      else
			if assigned(fonmouseenter) then	fonmouseenter(self,Fmousecol,Fmouserow);
   	Fmousecol:=newcol;
    Fmouserow:=newrow;
		if assigned(fonmouseentercell) then	fonmouseentercell(self,Fmousecol,Fmouserow);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.GetAsInteger(Const Acol, Arow: integer): integer;
begin
  if (Acol>-1) and (Arow>-1) and (Acol<colcount) and (Arow<rowcount) then
  begin
    try
      result:=strtoint(cells[Acol, Arow]);
    except
      result:=0;
    end;
  end else
    result:=0;
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.GetAsFloat(Const Acol, Arow: integer): double;
begin
  if (Acol>-1) and (Arow>-1) and (Acol<colcount) and (Arow<rowcount) then
  begin
    try
      result:=strtofloat(cells[Acol, Arow]);
    except
      result:=0;
    end;
  end else
    result:=0;
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.textflags: cardinal;
begin
  result:= DT_NOPREFIX;
  if fwordwrap then result:= result or DT_WORDBREAK;

  //result:= result or DT_LEFT;
  if fhalign = taLeftJustify then result:= result or DT_LEFT;
  if fhalign = taRightJustify then result:= result or DT_RIGHT;
  if fhalign = taCenter then result:= result or DT_CENTER;

  // taVerticalCenter, taAlignTop, taAlignBottom
  if fvalign = taVerticalCenter then result:= result or DT_VCENTER;
  if fvalign = taAlignTop then result:= result or DT_TOP;
  if fvalign = taAlignBottom then result:= result or DT_BOTTOm;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.CalculateRowHeight(Const ARow: integer);
var
  i, maxheight: integer;
  temprect: trect;
begin
  maxheight:= DefaultRowHeight;
  for i:=0 to colcount-1 do
  begin
    text:= cells[i, arow];
    if text<>'' then
    begin
      temprect:= rect(0, 0, colwidths[i], maxheight);
      windows.DrawText(canvas.Handle, pchar(text), length(text), temprect, textflags + DT_CALCRECT);
      if temprect.Bottom-temprect.top>maxheight then
        maxheight:= temprect.bottom-temprect.top;
    end;
  end;
  rowheights[arow]:= maxheight;
end;

//------------------------------------------------------------------------------
procedure TAPI_stringgrid.DrawCell(ACol, ARow: Integer; ARect: TRect; State: TGridDrawState);
var
  text: string;
  temprect: trect;
begin
  if canvas.Font<>font then canvas.font.Assign(font);

  CalculateRowHeight(arow);

  if gdfixed in state then
  begin
    canvas.Font.Color:= ffixfont;
    canvas.Brush.Color:= fixedcolor;
  end else
  if gdFocused in state then
  begin
    canvas.font.color:= ffocfont;
    canvas.Brush.Color:= fbackfoc;
  end else
  if gdselected in state then
  begin
    canvas.font.color:= fselfont;
    canvas.Brush.Color:= fbacksel;
  end else
  begin
    canvas.font.color:= font.color;
    canvas.brush.color:= color;
  end;

  canvas.Brush.Style:= bssolid;
  canvas.FillRect(arect);

  canvas.Brush.style:= bsclear;
  text:= cells[acol, arow];
  temprect:= rect(arect.Left, arect.Top, arect.Right, arect.Bottom);
  windows.DrawText(canvas.Handle, pchar(text), length(text), temprect, textflags);
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.exporthtmltext (var s: string; NameLinks: boolean = false): boolean; // this is always true
var
  rows: integer;
  cols: integer;
  i: integer;
  j: integer;
  text: string;
begin
  s:= '<table width=100% border=0>'+#13#10;
  rows:= self.RowCount;
  cols:= self.ColCount;
  for j:=0 to rows-1 do
  begin
    s:=s+'<tr>';
    if namelinks then s:=s+'<a name="row'+inttostr(j)+'"></a>';
    for i:=0 to cols-1 do
    begin
      s:=s+'<td width=100% bgcolor=#ffffff><font size=2 color=#000000>';
      if namelinks then
      begin
        if j=0 then
          s:=s+'<a name="col'+inttostr(i)+'"></a>';
        s:=s+'<a name="r'+inttostr(j)+'c'+inttostr(i)+'"></a>';
      end;
      s:=s+text+'</td>';
    end;
    s:=s+'</tr>'+#13#10;
  end;
  s:=s+'</table>'+#13#10;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_stringgrid.exporthtmlfile ( filename: string; Namelinks: boolean = false ): boolean;
var
  sl: tstringlist;
  s: string;
begin
  result:=false;
  sl:= tstringlist.create;
  try
    exporthtmltext(s, namelinks);
    sl.text:= s;
    try
      sl.SaveToFile(filename);
      result:=true;
    except
      // failed
    end;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_stringgrid]);
end;

end.
