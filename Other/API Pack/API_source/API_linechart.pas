unit API_linechart;

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
// r1.15, 23062009, ari pikivirta
//  * drawing rectangle around area in case grid is being drawn
//
// r1.14, 20052009, ari pikivirta
//  * does not erase background anymore -> removes some of the flicker
//
// r.13, 09022009, ari pikivirta
//  * added zoommargin (%) property
//  * fixed bug on drawing zoom area margins -> now drawing correctly to middle
//    even there's min and max defined
//
// r1.12, 09122008, ari pikivirta
//  * fixed bug on setting history count on the fly
//
// r1.11, 23102007, ari pikivirta
//  * changed font background to clear on showing min/max values on graph
//  * overloaded linecolor function
//  * renamed numoflines to lines
//
// r1.10, 01082007, ari pikivirta
//  * control style changed to csopaque to remove flickering
//  * min/max/avg move to be as functions and overloaded them for individual lines
//  * clear function has possibility to reset to any value
//  * background color property removed, regular "color" is used instead
//  * removed limitation of maximum lines count
//
// r1.09, 10062006, ari pikivirta
//  * increased maximum line amount to 32
//  * added property for showing min and max value on the panel top and bottom
//
// r1.08, 25032006, ari pikivirta
//  * changed fixed arrays to variable length arrays to reduce memory
//    usage (limits remain the same)
//  * added clearall function
//  * overloaded add function to allow adding multiple y values simultaneously
//
// r1.07, 16082004, ari pikivirta
// * fixed min/max calculation if one is equal to zero
// * fixed filling of gradient to reach bottom line also
// * fixed setlinecolor and changed name to just linecolor
//
// r1.06, ari pikivirta
// * gradient fill included into the class
//
// r1.05, ari pikivirta
// * added gradient fill options if no picture assigned
//
// r1.04, ari pikivirta
// * line color function added (was missing in r1.03)
//

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  graphics, API_graphics, dialogs;

const
  // constants for drawing
  linechart_maxhistory = 2048;

type
  // THE component
  TAPI_linechart = class(tpaintbox)
  private
    fversion: string;
    fhistorycount: integer;
    fgrid: boolean;
    fgridcolor: tcolor;
    fbackgroundimage: tpicture;
    ftotalmax: double;
    ftotalmin: double;
    ftotalavg: double;
    fgridcounth: integer;
    fgridcountw: integer;

    fautozoom: boolean;
    fsetmin: double;
    fsetmax: double;
    fzoommargin: double; // as percentage
    fshowminmax: boolean;

    // data
    fnumoflines: integer;
    fhistory: array of array of double;
    fmin: array of double;
    favg: array of double;
    fmax: array of double;
    flinecolor: array of tcolor;

    // double buffered drawing
    bmp: tbitmap;

    // gradient fill
    fgradient: boolean;
    fgradientstyle: tgradientstyle;
    fstartcolor: tcolor;
    fendcolor: tcolor;

    procedure sethistorycount(Const i: integer);
    procedure setnumoflines(Const i: integer);
    procedure setbackgroundimage(p: tpicture);
    procedure setgrid(Const b: boolean);
    procedure setgridcolor(Const c: tcolor);
    procedure setgridcountw(Const i: integer);
    procedure setgridcounth(Const i: integer);
    procedure autozoom (Const b: boolean);
    procedure setmin (Const d: double);
    procedure setmax (Const d: double);
    procedure setshowminmax(Const b: boolean);

    // gradient
    procedure setgradient(Const b: boolean);
    procedure setstartcolor(Const c: tcolor);
    procedure setendcolor(Const c: tcolor);
    procedure setgradientstyle(Const i: tgradientstyle);
    procedure dummys(s: string);

  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  public
    // common
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;

    // lines
    procedure ClearAll(Const value: double = 0.0);
    procedure Clear(Const line: integer; Const value: double = 0.0);
    procedure Add(Const line: integer; Const yvalue: double; Const Color: TColor = clBlack); overload;
    procedure Add(YValue: array of Double); overload;
    function  Values(Const Line, Count: Integer): Double;
    procedure LineColor(Const Line: Integer; Const C: TColor); overload;
    function  LineColor(Const Line: integer): TColor; overload;

    // contents
    function Max: double; overload;
    function Max(Const line: integer): double; overload;
    function Min: double; overload;
    function Min(Const line: integer): double; overload;
    function Average: double; overload;
    function Average(Const line: integer): double; overload;

  published
    property Version: string read fversion write dummys stored false;
    property HistoryCount: integer read fhistorycount write sethistorycount;
    property Lines: integer read fnumoflines write setnumoflines;

    property BackgroundImage: tpicture read fbackgroundimage write setbackgroundimage;
    property Grid: boolean read fgrid write setgrid;
    property GridColor: tcolor read fgridcolor write setgridcolor;
    property GridCountH: integer read fgridcounth write setgridcounth;
    property GridCountV: integer read fgridcountw write setgridcountw;
    property ZoomAuto: boolean read fautozoom write autozoom;
    property ZoomMin: double read fsetmin write setmin;
    property ZoomMax: double read fsetmax write setmax;
    property ShowMinMax: boolean read fshowminmax write setshowminmax;
    property ZoomMargin: double read fzoommargin write fzoommargin;

    // gradient
    property Gradient: boolean read fgradient write setgradient;
    property GradientStart: tcolor read fstartcolor write setstartcolor;
    property GradientEnd: tcolor read fendcolor write setendcolor;
    property GradientStyle: tgradientstyle read fgradientstyle write setgradientstyle;

  end;

procedure Register;


implementation

uses
  math;

{$R *.RES}

const
  versioninfo = 'r1.15/ari.pikivirta[at]kolumbus.fi';

procedure TAPI_linechart.dummys(s: string); begin end;

// --------------------------------------------------------------------------
constructor tAPI_linechart.create(aowner:tcomponent);
var
  i: integer;
begin
  inherited create(aowner);

  fversion:=versioninfo;
  fbackgroundimage:=tpicture.create;
  bmp:=tbitmap.create;

  // init colors etc.
  color:=           clsilver;
  fgrid:=           true;
  fgridcolor:=      clgray;
  fgridcounth:=     3;
  fgridcountw:=     2;
  fgradient:=       false;
  fstartcolor:=     clwhite;
  fendcolor:=       clsilver;
  fgradientstyle:=  gshorizontal;
  fautozoom:=       true;
  fshowminmax:=     true;
  fzoommargin:=     10.0; // +-10%

  //
  sethistorycount(10);
  setnumoflines(1);
  clearall(0);
  linecolor(1, clred);
  randomize;
  for i:=0 to 9 do
    add(0, random(10)/2-2.5);

  // i hate flickering things..
  ControlStyle:= ControlStyle + [csOpaque];
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.setgradient(Const b: boolean);
begin
  if fgradient<>b then
  begin
    fgradient:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_linechart.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.setstartcolor(Const c: tcolor);
begin
  if fstartcolor<>c then
  begin
    fstartcolor:=c;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
function TAPI_linechart.Max: double;
begin
  result:= ftotalmax;
end;

// --------------------------------------------------------------------------
function TAPI_linechart.Max(Const line: integer): double;
begin
  result:= fmax[line];
end;

// --------------------------------------------------------------------------
function TAPI_linechart.Min: double;
begin
  result:= ftotalmin;
end;

function TAPI_linechart.Min(Const line: integer): double;
begin
  result:= fmin[line];
end;

// --------------------------------------------------------------------------
function TAPI_linechart.Average: double;
begin
  result:= ftotalavg;
end;

// --------------------------------------------------------------------------
function TAPI_linechart.Average(Const line: integer): double;
var
  i: integer;
begin
  // calculate average value
  favg[line]:=0;                                    // reset average
  for i:=0 to fhistorycount-1 do                    // go trough history
    favg[line]:=favg[line]+fhistory[line,i];        // sum all points
  if (fhistorycount<>0) and (favg[line]<>0) then    // check valid numbers
    favg[line]:=favg[line]/fhistorycount            // calc average
    else favg[line]:=0;                             // or force zero
  // return
  result:= favg[line];                              // return result
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.setendcolor(Const c: tcolor);
begin
  if fendcolor<>c then
  begin
    fendcolor:=c;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.setshowminmax(Const b: Boolean);
begin
  if b<>fshowminmax then
  begin
    fshowminmax:= b;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.setgradientstyle(Const i: tgradientstyle);
begin
  if fgradientstyle<>i then
  begin
    fgradientstyle:=i;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.clear(Const line: integer; Const value: double = 0);
var
  i: integer;
begin
  if (line>-1) and (line<fnumoflines) then            // check area is valid
  begin
    for i:=0 to self.fhistorycount-1 do               // go trough the history
      fhistory[line,i]:= value;                      // reset history value
    fmax[line]:= value;
    fmin[line]:= value;
    favg[line]:= value;
  end;
  invalidate;
end;

// --------------------------------------------------------------------------
procedure TAPI_linechart.ClearAll(Const value: double = 0.0);
var
  i: integer;
begin
  for i:=0 to fnumoflines-1 do
    clear(i, value);
  invalidate;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setgrid(Const b: boolean);
begin
  if b<>fgrid then
  begin
    fgrid:=b;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.autozoom(Const b: boolean);
begin
  if b<>fautozoom then
  begin
    fautozoom:=b;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setmax (Const d: double);
begin
  if (d<>fsetmax) and (d>=fsetmin) then
  begin
    fsetmax:=d;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setmin (Const d: double);
begin
  if (d<>fsetmin) and (d<=fsetmax) then
  begin
    fsetmin:=d;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setbackgroundimage(p:tpicture);
begin
  fbackgroundimage.Assign(p);
  invalidate;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setgridcounth(Const i: integer);
begin
  if i<>fgridcounth then
  begin
    fgridcounth:=i;
    if fgridcounth<0 then
      fgridcounth:=0;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setgridcountw(Const i: integer);
begin
  if i<>fgridcountw then
  begin
    fgridcountw:=i;
    if fgridcountw<0 then
      fgridcountw:=0;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setgridcolor(Const c: tcolor);
begin
  if c<>fgridcolor then
  begin
    fgridcolor:=c;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.add(Const line: integer; Const yvalue: double; Const Color: TColor = clBlack);
var
  i: integer;
begin
  if (line<0) or (line>fnumoflines-1) then exit;    // check valid line number

  // move all history one step back
  for i:=0 to fhistorycount-2 do fhistory[line,i]:= fhistory[line,i+1];

  // add new value
  fhistory[line,fhistorycount-1]:= yvalue;

  // check for new maximum and minimum
  if (yvalue<fmin[line]) then fmin[line]:= yvalue;
  if (yvalue>fmax[line]) then fmax[line]:= yvalue;
  average(line);                                    // calculate average

  invalidate;                                       // refresh view
end;

// --------------------------------------------------------------------------
procedure TAPI_Linechart.Add(YValue: array of Double);
var
  i: integer;
begin
  for i:=low(YValue) to high(Yvalue) do
    add(i, yvalue[i]);
end;

// --------------------------------------------------------------------------
function tAPI_linechart.values(Const line, count: integer): double;
begin
  result:=0;
  if (line<0) or (line>fnumoflines-1) then exit;
  result:= fhistory[line,count];
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.setnumoflines(Const i: integer);
var
  x: integer;
begin
  if (i>-1) then
  begin
    // set line statistics lengths
    fnumoflines:= i;
    setlength( fmin, i );
    setlength( fmax, i );
    setlength( favg, i );
    setlength( flinecolor, i );
    // set new history lengths,
    setlength(fhistory, i);
    for x:=0 to i-1 do
    begin
      setlength(fhistory[x], fhistorycount);
      clear(x);
    end;
    // repaint
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.paint;
var
  h, w: integer;
  fmin, fmax: extended;
  fheight: extended;
  favg, fh, fw: extended;
  i, j: integer;
  gridh, gridw: extended;
  line: integer;
  fzoffset: extended;
  fdrawmin, fdrawmax: extended;
begin
  h:= height;
  w:= width;
  if (h<1) or (w<1) then exit;

  // create bitmap
  bmp.Height:=h;
  bmp.Width:=w;

  // min, max and average
  if (fhistorycount>0) and (fnumoflines>0) then
  begin
    fmin:= MAXDOUBLE;
    fmax:= MINDOUBLE;
    favg:=0;
    for j:=0 to fnumoflines-1 do
      for i:=0 to fhistorycount-1 do
      begin
        if fhistory[j,i]<fmin then fmin:=fhistory[j,i];
        if fhistory[j,i]>fmax then fmax:=fhistory[j,i];
        favg:=favg+fhistory[j,i];
      end;
    if (favg<>0) then
      favg:=favg/fhistorycount;
  end else
  begin
    fmin:= 0;
    fmax:= 0;
    favg:= 0;
  end;

  // set scaling
  if not fautozoom then
  begin
    fmin:= fsetmin;
    fmax:= fsetmax;
  end;

  // drawing limits
  fdrawmin:= fmin;
  fdrawmax:= fmax;
  if (fdrawmax<0) then
  begin
    fdrawmin:= fdrawmin + abs(fdrawmax);
    fdrawmax:= fdrawmax + abs(fdrawmax);
  end;

  // scaling on display
  fheight:= (fdrawmax-fdrawmin)*(1+fzoommargin/100);
  if fheight<1 then fheight:= 1;
  fzoffset:= (fheight-(fdrawmax-fdrawmin))/2;
  fh:= (h/fheight);
  if (fhistorycount>0) then fw:=w/fhistorycount else fw:=1;

  // fill background
  bmp.canvas.Brush.Style:= bssolid;
  if not fbackgroundimage.Bitmap.Empty then
  begin
    // picture
    bmp.canvas.CopyRect(bmp.canvas.ClipRect,fbackgroundimage.Bitmap.Canvas,fbackgroundimage.Bitmap.Canvas.ClipRect);
  end else
  begin
    // gradient fill
    if fgradient then
    begin
      gradientfill(bmp.canvas,bmp.Canvas.ClipRect,fstartcolor,fendcolor,fgradientstyle);
    end else
    // normal fill
    begin
      bmp.Canvas.Brush.Color:= color;
      bmp.canvas.FillRect(bmp.Canvas.ClipRect);
    end;
  end;

  // draw grid if enabled
  if fgrid then
  begin
    if (w>1) and (fgridcountw>0) then gridw:=(w-1)/fgridcountw else gridw:=1;
    if (h>1) and (fgridcounth>0) then gridh:=(h-1)/fgridcounth else gridh:=1;
    bmp.canvas.pen.Style:=pssolid;
    bmp.canvas.Pen.Mode:=pmcopy;
    bmp.canvas.pen.Color:=fgridcolor;
    // draw rectangle around area
    bmp.canvas.moveto(0,0);
    bmp.canvas.lineto(0,height-1);
    bmp.canvas.lineto(width-1,height-1);
    bmp.canvas.lineto(width-1,0);
    //bmp.canvas.lineto(0,0); // not needed =)
    // draw grid lines
    for i:=0 to fgridcountw-1 do
    begin
      bmp.canvas.MoveTo(round(i*gridw),0);
      bmp.canvas.lineto(round(i*gridw),h);
    end;
    for i:=0 to fgridcounth-1 do
    begin
      bmp.Canvas.moveto(0,round(i*gridh));
      bmp.canvas.LineTo(w,round(i*gridh));
    end;
  end;

  // draw min and max if enabled
  if fshowminmax then
  begin
    bmp.Canvas.Brush.style:= bsclear;
    bmp.Canvas.TextOut(1,1,formatfloat('0.0', fmax));
    bmp.canvas.textout(1,height-bmp.canvas.TextHeight('1')-1, formatfloat('0.0',fmin));
  end;

  // draw all lines
  bmp.Canvas.Pen.Style:=pssolid;
  bmp.canvas.Pen.Mode:=pmcopy;
  for line:=0 to fnumoflines-1 do
  begin
    bmp.Canvas.Pen.Color:=flinecolor[line];
    bmp.canvas.MoveTo(0,trunc(fh*(fdrawmax+fzoffset-fhistory[line,0])));
    for i:=0 to fhistorycount-1 do
      bmp.Canvas.LineTo(trunc(i*fw),trunc(fh*(fdrawmax+fzoffset-fhistory[line,i])));
  end;

  // copy bitmap to canvas
  canvas.Draw(0,0,bmp);

  // total min, max and average
  ftotalmax:=fmax;
  ftotalmin:=fmin;
  ftotalavg:=favg;
end;

// --------------------------------------------------------------------------
destructor tAPI_linechart.destroy;
begin
  fbackgroundimage.Free;
  bmp.Free;
  inherited destroy;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.sethistorycount(Const i: integer);
begin
  if fhistorycount<>i then
  begin
    fhistorycount:=i;
    setnumoflines(fnumoflines);
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
procedure tAPI_linechart.linecolor(Const line: integer; Const c: tcolor);
begin
  if (line>-1) and (line<self.fnumoflines) and (flinecolor[line]<>c) then
  begin
    flinecolor[line]:=c;
    invalidate;
  end;
end;

// --------------------------------------------------------------------------
function  TAPI_linechart.LineColor(Const Line: integer): tcolor;
begin
  if (line>-1) and (line<fnumoflines) then
  begin
    result:= flinecolor[line];
  end else
    result:= clblack;
end;

// --------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_linechart]);
end;

end.
