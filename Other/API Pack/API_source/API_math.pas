unit API_math;

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
// 15052005, r1.02, ari pikivirta
//  * added moving average filter
//
// 23082004, r1.01, ari pikivirta
//  * fixed serie delete area (end was -1)
//  * fixed liniearize (end was -1)
//
// r1.00, azi pikivirta
//  * added most needed serie functions

interface

uses
  SysUtils, Classes, dialogs;

type
  TAPI_math = class(TComponent)
  private
    fversion: string;
    flist: tstringlist;
    procedure dummys(s: string);

  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    function max(b1, b2: byte): byte; overload;
    function max(w1, w2: word): word; overload;
    function max(i1, i2: integer): integer; overload;
    function max(c1, c2: cardinal): cardinal; overload;
    function max(r1, r2: real): real; overload;
    function max(d1, d2: double): double; overload;

    function min(b1, b2: byte): byte; overload;
    function min(w1, w2: word): word; overload;
    function min(i1, i2: integer): integer; overload;
    function min(c1, c2: cardinal): cardinal; overload;
    function min(r1, r2: real): real; overload;
    function min(d1, d2: double): double; overload;

    function equal(b1, b2: byte): boolean; overload;
    function equal(w1, w2: word): boolean; overload;
    function equal(i1, i2: integer): boolean; overload;
    function equal(c1, c2: cardinal): boolean; overload;
    function equal(r1, r2: real): boolean; overload;
    function equal(d1, d2: double): boolean; overload;

    function smaller(b1, b2: byte): boolean; overload;
    function smaller(w1, w2: word): boolean; overload;
    function smaller(i1, i2: integer): boolean; overload;
    function smaller(c1, c2: cardinal): boolean; overload;
    function smaller(r1, r2: real): boolean; overload;
    function smaller(d1, d2: double): boolean; overload;

    function greater(b1, b2: byte): boolean; overload;
    function greater(w1, w2: word): boolean; overload;
    function greater(i1, i2: integer): boolean; overload;
    function greater(c1, c2: cardinal): boolean; overload;
    function greater(r1, r2: real): boolean; overload;
    function greater(d1, d2: double): boolean; overload;

    // serie functions
    function  serie: tstringlist;                                 // get whole list
    procedure serie_clear;                                        // clear list
    function  serie_count: integer;                               // item count
    function  serie_set(pos: integer; v: double): boolean;        // set/modify values
    function  serie_add(v: double): boolean;                      // add row
    function  serie_del(pos: integer): boolean;                   // delete row
    function  serie_delarea(start, stop: integer): boolean;       // delete area
    function  serie_asinteger(pos: integer): integer;             // get as integer
    function  serie_asfloat(pos: integer): double;                // get as float
    function  serie_average: double;                              // calculate average
    function  serie_min: double;                                  // get minimum
    function  serie_max: double;                                  // get maximum
    function  serie_sum: double;                                  // sum of serie values
    function  serie_move (y: double): boolean;                    // move all values
    function  serie_multiply (m: double): boolean;                // multiply all
    function  serie_linearize (starty, stopy: double): boolean;   // linearize all values
    function  serie_findends (var start: integer;
      var stop: integer; dif: double; step: integer): boolean;    // find ends (above dif)
    function  serie_medianfilter: boolean;                        // apply 2r+1 filter
    function  serie_movingaverage(ncount: integer): boolean;      // apply ncount moving average to table
    function  serie_sort_asc: boolean;                            // sort ascending
    function  serie_sort_desc: boolean;                           // sorf descending

  published
    property Version: string read fversion write dummys stored false;

  end;

procedure Register;

implementation

uses
  math;

const
  versioninfostring = 'r1.01/ari.pikivirta(at)kolumbus.fi';

{$r *.res}

procedure TAPI_math.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_math.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfostring;
  flist:=Tstringlist.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_math.destroy;
begin
  flist.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie: tstringlist;
begin
  result:=flist;
end;

//------------------------------------------------------------------------------
procedure TAPI_math.serie_clear;
begin
  flist.Clear;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_count: integer;
begin
  result:=flist.count;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_average: double;
var
  i: integer;
begin
  result:=0;
  if flist.count<1 then exit;
  for i:=0 to flist.count-1 do
  begin
    result:=result+strtofloat(flist[i]);
  end;
  if result<>0 then
    result:=result/flist.count;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_min: double;
var
  d: double;
  i: integer;
begin
  result:=0;
  if flist.count<1 then exit;
  result:=maxdouble;
  for i:=0 to flist.count-1 do
  begin
    d:=strtofloat(flist[i]);
    if d<result then
      result:=d;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_max: double;
var
  d: double;
  i: integer;
begin
  result:=0;
  if flist.count<1 then exit;
  result:=mindouble;
  for i:=0 to flist.count-1 do
  begin
    d:=strtofloat(flist[i]);
    if d>result then
      result:=d;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_add(v: double): boolean;
begin
  flist.add(floattostr(v));
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_set(pos: integer; v: double): boolean;
begin
  result:= false;
  if flist.count<1 then exit;
  if pos<0 then exit;
  if pos>flist.count-1 then exit;
  flist[pos]:=floattostr(v);
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_del(pos: integer): boolean;
begin
  result:= false;
  if flist.count<1 then exit;
  if pos<0 then exit;
  if pos>flist.count-1 then exit;
  flist.Delete(pos);
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_delarea(start, stop: integer): boolean;
var
  c: integer;
  i: integer;
begin
  result:=false;
  if flist.count<1 then exit;
  if start>stop then exit;
  if start<0 then start:= 0;
  if stop>flist.count-1 then stop:=flist.count-1;
  c:=stop-start;
  for i:=0 to c do
    flist.Delete(start);
  result:=true;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_asinteger(pos: integer): integer;
begin
  result:=0;
  if flist.count<1 then exit;
  if pos>flist.count-1 then exit;
  if pos<0 then exit;
  result:=trunc(strtofloat(flist[pos]));
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_asfloat(pos: integer): double;
begin
  result:=0;
  if flist.count<1 then exit;
  if pos>flist.count-1 then exit;
  if pos<0 then exit;
  result:=strtofloat(flist[pos]);
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_multiply (m: double): boolean;
var
  i: integer;
  d: double;
begin
  result:=false;
  if flist.count<1 then exit;
  for i:=0 to flist.count-1 do
  begin
    d:=strtofloat(flist[i]);
    d:=d*m;
    flist[i]:=floattostr(d);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_move (y: double): boolean;
var
  i: integer;
  d: double;
begin
  result:=False;
  if flist.count<1 then exit;
  for i:=0 to flist.count-1 do
  begin
    d:=strtofloat(flist[i]);
    d:=d+y;
    flist[i]:=floattostr(d);
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
//Vstart = Vdev(0):
//Vstop = Vdev(N)
//For i = 0 To N
//     y = Vstop - (N - i) * (Vstop - Vstart) / N
//     Vdev(i) = Vdev(i) - y
//Next i
function TAPI_math.serie_linearize (starty, stopy: double): boolean;
var
  i: integer;
  y: double;
  d: double;
begin
  result:=False;
  if flist.count<1 then exit;
  for i:=0 to flist.count-1 do
  begin
    d:= strtofloat(flist[i]);
    y:= stopy - (flist.count - i) * (stopy - starty) / flist.count;
    d:= d - y;
    flist[i]:= floattostr(d);
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
function  TAPI_math.serie_sum: double;
var
  d: double;
  i: integer;
begin
  d:=0;
  for i:=0 to flist.count-1 do
  begin
    d:=d+strtofloat(flist[i]);
  end;
  result:=d;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_findends (var start: integer;
  var stop: integer; dif: double; step: integer): boolean;
var
  i: integer;
  d1, d2: double;
begin
  result:=false;
  start:=0;
  stop:=flist.count-1;
  if flist.count<0 then exit;

  // go through ends from middle
  for i:=step to round(flist.count/2)-step do
  begin
    // beginning
    d1:=strtofloat(flist[i]);
    d2:=strtofloat(flist[i-step]);
    if abs(d1-d2)>dif then
      start:=i-step;
    // ending
    d1:=strtofloat(flist[flist.count-i-1]);
    d2:=strtofloat(flist[flist.count-i+step-1]);
    if abs(d1-d2)>dif then
      stop:=flist.count-i+step;
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_medianfilter: boolean;
var
  r: integer;
  i: integer;
  p: integer;
  j: integer;
  z: double;
  temp: string;
  newlist: array [-5..5] of string;
begin
  result:=False;
  if flist.count<1 then exit;
  r:=2;
  for i:=r+2 to flist.Count-(r+1) do
  begin
    for p:=-r to r do
      newlist[p]:=flist[i+p];
    for p:=-r+1 to r do
      for j:=p downto -r+1 do
      begin
        if strtofloat(newlist[j])>strtofloat(newlist[j-1]) then
        begin
          // switch places
          temp:=newlist[j];
          newlist[j]:=newlist[j-1];
          newlist[j-1]:=temp;
        end;
      end;
    flist[i]:=newlist[0];
    z:=strtofloat(flist[i-2])
      +2*strtofloat(flist[i-1])
      +strtofloat(flist[i]);
    flist[i-1]:=floattostr(z/4);
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_movingaverage(ncount: integer): boolean;
var
  sl: tstringlist;
  i,j: integer;
  total: extended;
begin
  result:= false;                                   // set default result to false
  if flist.count < 1 then exit;                     // if no items in list exit
  if ncount < 1 then exit;                          // if ncount is not defined, break as error

  sl:= tstringlist.create;                          // create temporary list
  try
    sl.clear;                                       // clear temporary list

    for i:=ncount to flist.count-1 do               // go trough the real list from ncount
    begin
      total:= 0;                                    // set total to zero
      for j:=i-ncount to i do                       // go trough previous ncount items
        total:= total + strtofloat(flist[j]);       // add value to total
      if (total<>0) then                            // if total is  not zero
          sl.add( floattostr ( total / ncount ) )   // add average to temporary list
          else sl.add( '0' );                       // otherwise add zero
    end;

    j:= flist.count;                                // store original list size
    flist.clear;                                    // clear it
    for i:=0 to ncount-1 do                         // write original lists first ncount numbers
      flist.Add( sl[0] );                           // to same than first was
    for i:=0 to sl.count-1 do                       // rest will be
      flist.add( sl[i] );                           // as calculated via this function

    result:=(j = flist.count);                      // return true if item count is still same
  finally
    sl.free;                                        // free temporary list
  end;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_sort_asc: boolean;
begin
  result:=false;
  if flist.count<1 then exit;
  flist.Sort;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.serie_sort_desc: boolean;
var
  i: integer;
  j: integer;
begin
  result:=false;
  if flist.count<1 then exit;
  // this is very slow, but works..
  for j:=0 to flist.count-2 do
  for i:=0 to flist.count-2 do
  begin
    if flist[i]>flist[i+1] then
    begin
      flist.Exchange(i,i+1);
    end;
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_math.max(b1, b2: byte): byte;
begin
  if b1>b2 then result:=b1 else result:=b2;
end;

function TAPI_math.max(w1, w2: word): word;
begin
  if w1>w2 then result:=W1 else result:=w2;
end;

function TAPI_math.max(i1, i2: integer): integer;
begin
  if i1>i2 then result:=i1 else result:=i2;
end;

function TAPI_math.max(c1, c2: cardinal): cardinal;
begin
  if c1>c2 then result:=c1 else result:=c2;
end;

function TAPI_math.max(r1, r2: real): real;
begin
  if r1>r2 then result:=r1 else result:=r2;
end;

function TAPI_math.max(d1, d2: double): double;
begin
  if d1>d2 then result:=d1 else result:=d2;
end;

//------------------------------------------------------------------------------
function TAPI_math.min(b1, b2: byte): byte;
begin
  if b1<b2 then result:=b1 else result:=b2;
end;

function TAPI_math.min(w1, w2: word): word;
begin
  if w1<w2 then result:=w1 else result:=w2;
end;

function TAPI_math.min(i1, i2: integer): integer;
begin
  if i1<i2 then result:=i1 else result:=i2;
end;

function TAPI_math.min(c1, c2: cardinal): cardinal;
begin
  if c1<c2 then result:=c1 else result:=c2;
end;

function TAPI_math.min(r1, r2: real): real;
begin
  if r1<r2 then result:=r1 else result:=r2;
end;

function TAPI_math.min(d1, d2: double): double;
begin
  if d1<d2 then result:=d1 else result:=d2;
end;

//------------------------------------------------------------------------------
function TAPI_math.equal(b1, b2: byte): boolean;
begin
  result:=b1=b2;
end;

function TAPI_math.equal(w1, w2: word): boolean;
begin
  result:=w1=w2;
end;

function TAPI_math.equal(i1, i2: integer): boolean;
begin
  result:=i1=i2;
end;

function TAPI_math.equal(c1, c2: cardinal): boolean;
begin
  result:=c1=c2;
end;

function TAPI_math.equal(r1, r2: real): boolean;
begin
  result:=r1=r2;
end;

function TAPI_math.equal(d1, d2: double): boolean;
begin
  result:=d1=d2;
end;

//------------------------------------------------------------------------------
function TAPI_math.smaller(b1, b2: byte): boolean;
begin
  result:=b1<b2;
end;

function TAPI_math.smaller(w1, w2: word): boolean;
begin
  result:=w1<w2;
end;

function TAPI_math.smaller(i1, i2: integer): boolean;
begin
  result:=i1<i2;
end;

function TAPI_math.smaller(c1, c2: cardinal): boolean;
begin
  result:=c1<c2;
end;

function TAPI_math.smaller(r1, r2: real): boolean;
begin
  result:=r1<r2;
end;

function TAPI_math.smaller(d1, d2: double): boolean;
begin
  result:=d1<d2;
end;

//------------------------------------------------------------------------------
function TAPI_math.greater(b1, b2: byte): boolean;
begin
  result:=b1>b2;
end;

function TAPI_math.greater(w1, w2: word): boolean;
begin
  result:=w1>w2;
end;

function TAPI_math.greater(i1, i2: integer): boolean;
begin
  result:=i1>i2;
end;

function TAPI_math.greater(c1, c2: cardinal): boolean;
begin
  result:=c1>c2;
end;

function TAPI_math.greater(r1, r2: real): boolean;
begin
  result:=r1>r2;
end;

function TAPI_math.greater(d1, d2: double): boolean;
begin
  result:=d1>d2;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_math]);
end;

end.

