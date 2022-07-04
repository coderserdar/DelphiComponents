unit API_chart;

//------------------------------------------------------------------------------
// chart component
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
// 12072007, r1.02, ari pikivirta
//    * added title property (was removed on some compile accidentally)
//    * removed unnecessary property "color back out", added default color
//      to use with line/bar unless they're separately defined on add 
//
// 26.9.2005, r1.01, ari pikivirta
//    * added Del(index) function
//    * changed default background out area color to clgray instead of black
//    * axis marks are only drawn if different from previous
//    * added invalidate after resize
//    * added Add(x,y,color) function
//    * added fmaxvalues property to limit amount of stored values
//
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Types, Classes, Controls, ExtCtrls, Graphics, Messages,
  DIalogs, API_base, API_graphics;

type
  Tcharttype = (ctLine, ctBar);

  Tdatapoint = record
    item: string;                       // item name (not visible by default)
    x: extended;                        // x value
    y: extended;                        // y value
    color: tcolor;                      // poimt color
  end;

  Tdata = record
    count: integer;                     // amount if datapoints
    values: array of tdatapoint;        // list of values to be drawn
    xext: string;                       // x values extension string
    yext: string;                       // y values extension string
  end;

  TAPI_chart = class(TAPI_Custom_Paintbox)
  private
    { Private declarations }
    fdata: tdata;
    fsorted: boolean;
    fxaxis: string;
    fxshowitem: boolean;
    fxshowext: boolean;
    fxformat: string;
    fyaxis: string;
    fyshowitem: boolean;
    fyminvalue, fxminvalue, fymaxvalue, fxmaxvalue: extended;
    fyshowext: boolean;
    fyformat: string;
    fdefaultcolor: tcolor;
    fxmin, fxmax, fymin, fymax, fyavg: extended;
    fcharttype: tcharttype;
    ftitle: string;
    ftitlecolor: tcolor;
    fmouseisover: boolean;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fdatapos: integer;
    fcolorback1: tcolor;
    fcolorback2: tcolor;
    fxaxisspacing: integer;
    fyaxisspacing: integer;
    fmaxvalues: integer;
    fgridcolor: tcolor;
    procedure dummyi(i: integer);
    procedure checkminmax;
    procedure settype(b: tcharttype);
    procedure setbcolor1(c: tcolor);
    procedure setbcolor2(c: tcolor);
    function  calcXtextwidth: integer;
    function  calcXtextheight: integer;
    function  calcYtextwidth: integer;
    function  calcYtextheight: integer;
    function  calctitleheight: integer;
    function  getXtext(index: integer): string; overload;
    function  getYtext(index: integer): string; overload;
    function  Xtext(value: extended): string; overload;
    function  Ytext(value: extended): string; overload;
    procedure settitle(s: string);
    procedure settitlecolor(c: tcolor);
    procedure setdefaultcolor(c: tcolor);
    procedure setgridcolor(c: tcolor);
    procedure SetDefaultData;
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    procedure ClearData;
    procedure Add(x, y: extended); overload;
    procedure Add(x, y: extended; color: tcolor); overload;
    procedure Add(item: string; x,y: extended; color: tcolor; xext, yext: string); overload;
    procedure Del(index: integer);
    function  XValue(index: integer): extended;
    function  YValue(index: integer): extended;
    function  XItem(index: integer): string;
    function  YItem(index: integer): string;
    procedure Sort;
    function  GetXMin: extended;
    function  GetXMax: extended;
    function  GetYMin: extended;
    function  GetYMax: extended;
    function  GetYAvg: extended;
  published
    { Published declarations }
    property Values: integer read fdata.count write dummyi stored false;          // datapoints in memory
    property Sorted: boolean read fsorted write fsorted;                        // sort values by x on add
    property Title: string read ftitle write settitle;
    property TitleColor: tcolor read ftitlecolor write settitlecolor;
    property XAxis: string read fxaxis write fxaxis;
    property YAxis: string read fyaxis write fyaxis;
    property XMinValue: extended read fxminvalue write fxminvalue;
    property XMaxValue: extended read fxmaxvalue write fxmaxvalue;
    property YMinValue: extended read fyminvalue write fyminvalue;
    property YMaxValue: extended read fymaxvalue write fymaxvalue;
    property ColorBackground1: tcolor read fcolorback1 write setbcolor1;
    property ColorBackground2: tcolor read fcolorback2 write setbcolor2;
    property ChartType: tcharttype read fcharttype write settype;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property DataPos: integer read fdatapos write dummyi stored false;
    property XAxisFormat: string read fxformat write fxformat;
    property YAxisFormat: string read fyformat write fyformat;
    property XAxisShowItem: boolean read fxshowitem write fxshowitem;
    property YAxisShowItem: boolean read fyshowitem write fyshowitem;
    property XAxisShowExt: boolean read fxshowext write fxshowext;
    property YAxisShowExt: boolean read fyshowext write fyshowext;
    property MaxValues: integer read fmaxvalues write fmaxvalues;
    property XAxisSpacing: integer read fxaxisspacing write fxaxisspacing;
    property YAxisSpacing: integer read fyaxisspacing write fyaxisspacing;
    property ColorDefault: tcolor read fdefaultcolor write setdefaultcolor;
    property ColorGrid: tcolor read fgridcolor write setgridcolor;
  end;

procedure Register;

implementation

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_chart.create(aowner: tcomponent);
begin
  inherited create(aowner);
  version:= 'r1.02/ari.pikivirta(at)kolumbus.fi';
  ftitle:= 'My Chart';
  fxshowitem:= false;
  fxshowext:= true;
  color:= clbtnface;
  fcolorback1:= clsilver;
  fcolorback2:= clwhite;
  fdefaultcolor:= clred;
  fxformat:= '0.0';
  fyshowitem:= false;
  fyshowext:= True;
  fyformat:= '0.0';
  fcharttype:= ctline;
  fmaxvalues:= 0 ;
  fxaxisspacing:= 5;
  fyaxisspacing:= 5;
  setdefaultdata;
  ControlStyle:= ControlStyle + [csOpaque];
end;

//------------------------------------------------------------------------------
destructor TAPI_chart.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.SetDefaultData;
var
  i: integer;
begin
  cleardata;
  for i:=0 to 5 do
    add(i, random(255));
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.dummyi(i: integer);
begin
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.settype(b: tcharttype);
begin
  if b<> fcharttype then
  begin
    fcharttype:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.settitle(s: string);
begin
  ftitle:= s;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.settitlecolor(c: tcolor);
begin
  if c<>ftitlecolor then
  begin
    ftitlecolor:= c;
    invalidate;
  end;  
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.setgridcolor(c: tcolor);
begin
  if c<>fgridcolor then
  begin
    fgridcolor:= c;
    invalidate;
  end;  
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.Resize;
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.setbcolor1(c: tcolor);
begin
  if c<> fcolorback1 then
  begin
    fcolorback1:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.setbcolor2(c: tcolor);
begin
  if c<> fcolorback2 then
  begin
    fcolorback2:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.setdefaultcolor(c: tcolor);
begin
  if c<>fdefaultcolor then
  begin
    fdefaultcolor:= c;
    if (csdesigning in componentstate) then
      setdefaultdata;
    invalidate;
  end;  
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.ClearData;
begin
  fdata.count:= 0;
  setlength( fdata.values, fdata.count );
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.Del(index: integer);
var
  j: integer;
begin
  if (index>-1) and (index<fdata.count) then
  begin
    for j:=index to fdata.count-2 do
      fdata.values[j]:= fdata.values[j+1];
    fdata.count:= fdata.count-1;
    setlength(fdata.values, fdata.count);
    checkminmax;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.Sort;
var
  i,j: integer;
  dp: tdatapoint;
begin
  for i:=0 to fdata.count-2 do
    for j:=0 to fdata.count-2 do
      if fdata.values[j].x > fdata.values[j+1].x then                           // compare x values
      begin
        dp:= fdata.values[j];
        fdata.values[j]:= fdata.values[j+1];
        fdata.values[j+1]:= dp;
      end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.checkminmax;
var
  i: integer;
begin
  fyavg:= 0;
  if fdata.count>0 then
  begin
    fxmin:= high(integer);
    fxmax:= low(integer);
    fymin:= high(integer);
    fxmax:= low(integer);
    for i:=0 to fdata.count-1 do
    begin
      if fdata.values[i].x < fxmin then fxmin:= fdata.values[i].x;
      if fdata.values[i].x > fxmax then fxmax:= fdata.values[i].x;
      if fdata.values[i].y < fymin then fymin:= fdata.values[i].y;
      if fdata.values[i].y > fymax then fymax:= fdata.values[i].y;
      fyavg:= fyavg + fdata.values[i].y;
    end;
    if (fyavg>0) then
      fyavg:= fyavg/fdata.count;
  end else
  begin
    fxmin:= -0.1;
    fxmax:= 0.1;
    fymin:= -0.1;
    fymax:= 0.1;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.Add(item: string; x,y: extended; color: tcolor; xext, yext: string);
begin
  if (fmaxvalues>0) and (fdata.count > fmaxvalues) then del(0);
  fdata.count:= fdata.count+1;
  setlength(fdata.values, fdata.count);
  fdata.values[fdata.count-1].item:= item;
  fdata.values[fdata.count-1].x:= x;
  fdata.values[fdata.count-1].y:= y;
  fdata.values[fdata.count-1].color:= color;
  fdata.xext:= xext;
  fdata.yext:= yext;
  if fsorted then sort;
  checkminmax;
end;

procedure TAPI_chart.Add(x, y: extended; color: tcolor);
begin
  add('',x,y,color,'','');
end;

procedure TAPI_chart.Add(x,y: extended);
begin
  add('',x,y,fdefaultcolor,'','');
end;

//------------------------------------------------------------------------------
function TAPI_chart.XValue(index: integer): extended;
begin
  if (index>-1) and (index<fdata.count) then result:= fdata.values[index].x
    else result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_chart.YValue(index: integer): extended;
begin
  if (index>-1) and (index<fdata.count) then result:= fdata.values[index].y
    else result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_chart.XItem(index: integer): string;
begin
  if (index>-1) and (index<fdata.count) then result:= fdata.values[index].item
    else result:= '';
end;

//------------------------------------------------------------------------------
function TAPI_chart.YItem(index: integer): string;
begin
  if (index>-1) and (index<fdata.count) then result:= fdata.values[index].item
    else result:= '';
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.MouseEnter(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseisover:=true;
    if assigned(fonmouseenter) then fonmouseenter(self);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.MouseLeave(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseisover:=false;
    if assigned(fonmouseleave) then fonmouseleave(self);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  xadd: extended;
  i: integer;
begin
  fdatapos:= -1;

  if (fcharttype = ctbar) and (fdata.count>0) then
  begin
    if (x>calcytextwidth) then
    begin
      xadd:= round((Width - calcytextwidth) / fdata.count);                 // calculate barwidth
      for i:=0 to fdata.count-1 do
        if (x>calcytextwidth+xadd*i) then
          fdatapos:= i;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------
function TAPI_chart.GetYAvg: extended;
begin
  result:= fyavg;
end;

//------------------------------------------------------------------------------
function TAPI_chart.GetXMin: extended;
begin
  result:= fxmin;
end;

//------------------------------------------------------------------------------
function TAPI_chart.GetXMax: extended;
begin
  result:= fxmax;
end;

//------------------------------------------------------------------------------
function TAPI_chart.GetYMin: extended;
begin
  result:= fymin;
end;

//------------------------------------------------------------------------------
function TAPI_chart.GetYMax: extended;
begin
  result:= fymax;
end;

//------------------------------------------------------------------------------
function TAPI_chart.Xtext(value: extended): string;
begin
  result:= '';
  if fxshowitem then result:= result + fdata.values[0].item;
  result:= result + formatfloat(fxformat, value);
  if fxshowext then result:= result + fdata.xext;
end;

function TAPI_chart.getXtext(index: integer): string;
begin
  result:= '';
  if (index>-1) and (index<fdata.count) then
    result:= xtext(fdata.values[index].x);
end;

//------------------------------------------------------------------------------
function TAPI_chart.Ytext(value: extended): string;
begin
  result:= '';
  if fyshowitem then result:= result + fdata.values[0].item;
  result:= result + formatfloat(fyformat, value);
  if fyshowext then result:= result + fdata.yext;
end;

function TAPI_chart.getYtext(index: integer): string;
begin
  result:= '';
  if (index>-1) and (index<fdata.count) then
    result:= Ytext(fdata.values[index].y);
end;

//------------------------------------------------------------------------------
function TAPI_chart.calcXtextwidth: integer;
var
  i, value, mlen: integer;
begin
  mlen:= canvas.textwidth('0');
  for i:=0 to fdata.count-1 do
  begin
    value:= canvas.TextWidth(getxtext(i));
    if value > mlen then
      mlen:= value;
  end;
  result:= mlen + fxaxisspacing + 2;
end;

//------------------------------------------------------------------------------
function TAPI_chart.calcYtextwidth: integer;
var
  i, mlen, value: integer;
begin
  mlen:= canvas.textwidth('0');
  for i:=0 to fdata.count-1 do
  begin
    value:= canvas.TextWidth(getytext(i));
    if value > mlen then
      mlen:= value;
  end;
  result:= mlen + canvas.textwidth('1') + 2;
end;

//------------------------------------------------------------------------------
function TAPI_chart.calcXtextheight: integer;
var
  i, value, mlen: integer;
begin
  mlen:= canvas.textheight('0');
  for i:=0 to fdata.count-1 do
  begin
    value:= canvas.Textheight(getxtext(i));
    if value > mlen then mlen:= value;
  end;
  result:= mlen + 2;
end;

//------------------------------------------------------------------------------
function TAPI_chart.calcYtextheight: integer;
var
  i, value, mlen: integer;
begin
  mlen:= canvas.textheight('0');
  for i:=0 to fdata.count-1 do
  begin
    value:= canvas.Textheight(getytext(i));
    if value > mlen then mlen:= value;
  end;
  result:= mlen + fyaxisspacing + 2;
end;

//------------------------------------------------------------------------------
function TAPI_chart.calctitleheight: integer;
begin
  result:= canvas.TextHeight(ftitle) + 2;
end;

//------------------------------------------------------------------------------
procedure TAPI_chart.Paint;
var
  bmp: tbitmap;
  i: integer;
  xratio, yratio: extended;
  ymin, ymax, xmax, xmin: extended;
  xadd: extended;
  yaxismarks, xaxismarks: integer;
  xtexth, xtextw, ytexth, ytextw, titleh: integer;
  s, last: string;
begin
  last:= '';

  bmp:= tbitmap.Create;
  try

    // fill background
    bmp.Height:=              height;
    bmp.Width:=               width;
    bmp.Canvas.Brush.Color:=  color;
    bmp.canvas.brush.style:=  bssolid;
    bmp.canvas.FillRect(bmp.Canvas.ClipRect);

    // assign same font to bmp
    bmp.canvas.Font.Assign(self.Font);

    // drawing area borders
    xtexth:= calcxtextheight;
    xtextw:= calcxtextwidth;
    ytexth:= calcytextheight;
    ytextw:= calcytextwidth;
    titleh:= calctitleheight;

    // background fill
    bmp.canvas.Brush.style:= bssolid;
    bmp.canvas.FillRect(bmp.Canvas.ClipRect);
    GradientFill(bmp.canvas, rect(ytextw, titleh, width-3, height - xtexth), fcolorback1, fcolorback2, gsvertical);
    bmp.canvas.brush.style:= bsclear;

    // draw title
    if ftitle<>'' then
    begin
      bmp.canvas.Font.Color:= ftitlecolor;
      bmp.canvas.TextOut(width div 2 - (canvas.textwidth(ftitle) div 2), 0, ftitle);
    end;

    // forced y or x
    if (fymaxvalue>fyminvalue) then
    begin
      ymax:= fymaxvalue;
      ymin:= fyminvalue;
    end else
    begin
      ymax:= fymax;
      ymin:= fymin;
    end;
    if (fxmaxvalue>fxminvalue) then
    begin
      xmax:= fxmaxvalue;
      xmin:= fxminvalue;
    end else
    begin
      xmax:= fxmax;
      xmin:= fxmin;
    end;

    // exit if unsafe
    if (ymax-ymin <> 0) and (xmax-xmin <> 0) then
    begin

      // LINECHART
      if fcharttype = ctline then
      begin

        // marks
        bmp.Canvas.Font.Color:= font.Color;
        bmp.canvas.Pen.Color:= fgridcolor;

        // Y text and line
        yaxismarks:= round((bmp.height - xtexth - titleh) / ytexth);
        for i:=0 to yaxismarks-1 do
        begin
          // text
          s:= ytext(ymin+(ymax-ymin)*i/yaxismarks);
          if s<>last then
          begin
            bmp.canvas.TextOut(0, bmp.height - xtexth - ytexth*i - (ytexth div 2), s);
            last:= s;
          end;
          // line
          if bmp.Height - xtexth - ytexth*i - ytexth > titleh then
          begin
            bmp.canvas.MoveTo(ytextw, bmp.height - xtexth - ytexth*i - ytexth);
            bmp.canvas.Lineto(width, bmp.height - xtexth - ytexth*i - ytexth);
          end;
        end;

        // X text and line
        xaxismarks:= round((bmp.Width - ytextw - 3) / xtextw);
        for i:=0 to xaxismarks-1 do
        begin
          // text
          s:= xtext(xmin+(xmax-xmin)*i/xaxismarks);
          if s<>last then
          begin
            bmp.canvas.textout(ytextw + xtextw*i, bmp.Height - xtexth, s);
            last:= s;
          end;
          // line
          bmp.canvas.MoveTo(ytextw + xtextw*i, titleh);
          bmp.canvas.Lineto(ytextw + xtextw*i, bmp.height - xtexth);
        end;

        // line chart
        xratio:= (bmp.width - ytextw - 3) / (xmax-xmin);
        yratio:= (bmp.height - ytexth - titleh) / (ymax-ymin);
        xadd:= -xmin;
        if fdata.count>0 then
        begin
          // line
          bmp.canvas.MoveTo(ytextw + round(xratio*(xadd + fdata.values[0].x)), titleh + round(yratio*(ymax - fdata.values[0].y)));
          for i:=1 to fdata.count-1 do
          begin
            bmp.canvas.Pen.Color:= fdata.values[i].color;
            bmp.canvas.LineTo(ytextw + round(xratio*(xadd + fdata.values[i].x)), titleh + round(yratio*(ymax - fdata.values[i].y)));
          end;
        end;

      end else

      // BARCHART
      if fcharttype = ctbar then
      begin

        // marks
        bmp.Canvas.Font.Color:= font.Color;
        bmp.canvas.Pen.Color:= fgridcolor;

        yaxismarks:= round((bmp.height - xtexth - titleh) / ytexth);
        for i:=0 to yaxismarks-1 do
        begin
          // text
          s:= ytext(ymin+(ymax-ymin)*i/yaxismarks);
          if s<>last then
          begin
            bmp.canvas.TextOut(0, bmp.height - xtexth - ytexth*i - (xtexth div 2), s);
            last:= s;
          end;
          // line
          if bmp.Height - xtexth - ytexth*i - ytexth > titleh then
          begin
            bmp.canvas.MoveTo(ytextw, bmp.height - xtexth - ytexth*i - ytexth);
            bmp.canvas.Lineto(width, bmp.height - xtexth - ytexth*i - ytexth);
          end;
        end;

        // draw bars
        yratio:= (bmp.height - ytexth - titleh) / (ymax-ymin);
        xadd:= round((bmp.Width - ytextw - 3) / fdata.count);                     // calculate barwidth
        for i:=0 to fdata.count-1 do
        begin
          bmp.Canvas.Brush.Color:= fdata.values[i].color;
          bmp.Canvas.FillRect(rect(round(ytextw + xadd*i)+ 1, bmp.Height - xtexth, round(ytextw + xadd*i + xadd) - 1, titleh + round(yratio*(ymax-fdata.values[i].y))));
          bmp.canvas.brush.style:= bsclear;
          bmp.canvas.textout(round(ytextw + xadd*i)+ 1, bmp.Height - xtexth, xtext(fdata.values[i].x));
        end;

      end;

    end; // minmax check

    canvas.Draw(0,0,bmp);
    //canvas.CopyMode:=cmSrcCopy;
    //canvas.CopyRect(ClientRect, bmp.Canvas, ClientRect);
  finally
    bmp.free;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_chart]);
end;

end.
