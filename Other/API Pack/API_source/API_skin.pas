unit API_skin;

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
// r1.02/15092006/ari pikivirta
//  * added active property
//  * blur and colorscale put outside the component to be possible to use
//    those outside by just adding this unit to uses clause
//
// r1.01/30.8.2006/ari pikivirta
//  * added possibility to have background image (is used for doing regions
//    for the form), in case background image is defined form will not be
//    blurred and blended - note that even the background image is drawn
//    transparent i was not able to put owner form's background transparent
//    and that must be done manually.

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Messages, forms, ExtCtrls;

type
  TAPI_skin = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    factive: boolean;
    fscreenshot: tbitmap;                 // for blend & blur
    fbackground: tbitmap;                 // for regions
    foldborder: tborderstyle;
    foldregion: hrgn;
    fownerform: tform;
    fmoving: boolean;
    foldform_x: integer;
    foldform_y: integer;
    foldform_w: integer;
    foldform_h: integer;
    fblurradius: integer;
    fblurcolor: tcolor;
    fupdate: boolean;
    PrevParentWndProc: TWndMethod;
    factivated: boolean;
    ftakingscreenshot: boolean;
    foldtransparentcolor: boolean;
    foldtransparentcolorvalue: tcolor;

    procedure CallDefault(var Msg:TMessage);
    procedure ScreenShot;
    procedure setblurradius(r: integer);
    procedure setblurcolor(c: tcolor);
    procedure dummys(s: string);
    procedure setbackground(b: tbitmap);
    function CreateReqionFromBitmap(Bitmap: TBitmap): HRGN;
    procedure setactive(b: boolean);

  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure CaptureScreenRect(var bmp: tbitmap; ARect: TRect);
    procedure NewParentWndProc(var Msg: TMessage);

  public
    { Public declarations }
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property BlurRadius: integer read fblurradius write setblurradius;
    property BlurColor: tcolor read fblurcolor write setblurcolor;
    property BackImage: tbitmap read fbackground write setbackground;
    property Active: boolean read factive write setactive;

  end;

procedure Register;

// CUSTOM FUNCTIONS TO BE USED OUTSIDE THIS COMPONENT
//------------------------------------------------------------------------------
function InitScreen: boolean;
function FreeScreen: boolean;
function ScreenCanvas: tcanvas;
procedure ScreenCapture(var bmp: tbitmap; ARect: TRect);
Function BitmapBlurGaussian(Bitmap: TBitmap; radius: double): Boolean;
function ColorScale(Bmp: TBitmap; Color: TColor): boolean;

implementation

{$R *.RES}

// internal global variables:
var
  _ScreenDC: HDC;
  _ScreenCanvas: tcanvas;

function InitScreen: boolean;
begin
  if _ScreenCanvas=nil then
  begin
    _ScreenDC:= GetDC(0);
    _ScreenCanvas:= tcanvas.create;
    _ScreenCanvas.Handle:= _ScreenDC;
    result:= true;
  end else
    result:= false;
end;

function FreeScreen: boolean;
begin
  if _ScreenCanvas<>nil then
  begin
    ReleaseDC(0, _ScreenDC);
    _ScreenCanvas.Free;
    _ScreenCanvas:= nil;
    result:= true;
  end else
    result:= false;
end;

function ScreenCanvas: tcanvas;
begin
  result:= _ScreenCanvas;
end;

procedure ScreenCapture(var bmp: tbitmap; ARect: TRect);
var
  ScreenDC: HDC;
begin
  bmp.Width:= arect.Right - arect.Left;
  bmp.Height:= arect.Bottom - arect.Top;
  screenDC:=GetDC(0);
  try
    bmp.PixelFormat:= pf24bit;
    bitblt(bmp.Canvas.Handle,0,0,bmp.Width,bmp.Height,screenDC,arect.Left,arect.Top,SRCCOPY);
  finally
    releaseDC(0,ScreenDC);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.dummys(s: string);
begin
  //..
end;

//------------------------------------------------------------------------------
constructor TAPI_skin.create(aowner: tcomponent);
//var
//  p: pointer;
//  r: HRGN;
begin
  inherited create(aowner);

  fversion:= 'r1.02/ari.pikivirta@kolumbus.fi';
  fscreenshot:= tbitmap.create;
  fbackground:= tbitmap.create;
  fownerform:= tform(aowner as tform);
  fblurcolor:= clwhite;
  fblurradius:= 3;
  fmoving:= false;
  fupdate:= true;
  factivated:= false;
  ftakingscreenshot:= false;
  factive:= false;

  if (not (csdesigning in componentstate)) and (fownerform<>nil) then
  begin
    PrevParentWndProc:= fownerform.WindowProc;
    fownerform.WindowProc:= NewParentWndProc;
    //SetBkMode(fownerform.canvas.Handle, TRANSPARENT);
  end;

end;

//------------------------------------------------------------------------------
destructor TAPI_skin.Destroy;
begin
  if (not (csdesigning in componentstate)) and (fownerform<>nil) then
  begin
    fownerform.WindowProc:= PrevParentWndProc;
    PrevParentWndProc:= nil;
    (*
    fownerform.transparentcolor:= foldtransparentcolor;
    fownerform.transparentcolorvalue:= foldtransparentcolorvalue;
    fownerform.Width:= foldform_w;
    fownerform.Height:= foldform_h;
    fownerform.BorderStyle:= foldborder;
    SetWindowRgn(fownerform.Handle, foldregion, true);
    *)
  end;

  fscreenshot.free;
  fbackground.free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.Loaded;
var
  r: hrgn;
begin
  inherited;
  if (not (csdesigning in componentstate)) and (fownerform<>nil) and (not fbackground.empty) then
  begin
    //r:= CreateReqionFromBitmap(fbackground, fbackground.Canvas.ClipRect, fblurcolor);
    getwindowrgn(fownerform.Handle, foldregion);
    foldborder:= fownerform.BorderStyle;
    foldtransparentcolor:= fownerform.TransparentColor;
    foldtransparentcolorvalue:= fownerform.transparentcolorvalue;
    foldform_w:= fownerform.Width;
    foldform_h:= fownerform.Height;
    fownerform.TransparentColor:= true;
    fownerform.TransparentColorValue:= fownerform.Color;

    r:= CreateReqionFromBitmap(fbackground);
    setwindowrgn(fownerform.Handle, r, true);
    fownerform.BorderStyle:= bsnone;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.setactive(b: boolean);
begin
  if factive<>b then
  begin
    factive:= b;
    fupdate:= true;
    if fownerform<>nil then
      fownerform.Invalidate;
  end;  
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.setbackground(b: tbitmap);
var
  r: hrgn;
begin
  fbackground.assign(b);
  if (fownerform<>nil) and (not (csdesigning in componentstate)) then
  begin
    if (not fbackground.Empty) then
    begin
      fownerform.Width:= fbackground.Width;
      fownerform.Height:= fbackground.Height;
      //r:= CreateReqionFromBitmap(fbackground, fbackground.Canvas.ClipRect, fblurcolor);
      r:= CreateReqionFromBitmap(fbackground);
      setwindowrgn(fownerform.Handle, r, true);
      fownerform.BorderStyle:= bsnone;
    end else
    begin
      setwindowrgn(fownerform.handle, foldregion, true);
      fownerform.BorderStyle:= foldborder;
      fownerform.Width:= foldform_w;
      fownerform.Height:= foldform_h;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_skin.CreateReqionFromBitmap(Bitmap: TBitmap): HRGN;
var
  transColor: TColor;
  i, j: Integer;
  width, height: Integer;
  left, right: Integer;
  rectRgn: HRGN;
begin
  Result:= 0;
  width:= Bitmap.Width;
  height:= Bitmap.Height;
  transColor:= Bitmap.Canvas.Pixels[width - 1, height - 1];
  bitmap.TransparentColor:= transcolor;
  for i:= 0 to height - 1 do
  begin
    left:= -1;
    for j:= 0 to width - 1 do
    begin
      if left<0 then
      begin
        if Bitmap.Canvas.Pixels[j, i]<>transColor then
        begin
          left:=j;
        end else
        begin
          if Bitmap.Canvas.Pixels[j, i]=transColor then
          begin
            right:= j;
            rectRgn:= CreateRectRgn(left, i, right, i + 1);
            if Result = 0 then
            begin
              Result:= rectRgn;
            end else
            begin
              CombineRgn(Result, Result, rectRgn, RGN_OR);
              DeleteObject(rectRgn);
            end;
            left:= -1;
          end;
        end;
        if left>=0 then
        begin
          rectRgn:= CreateRectRgn(left, i, width, i + 1);
          if Result = 0 then
          begin
            Result:= rectRgn;
          end else
          begin
            CombineRgn(Result, Result, rectRgn, RGN_OR);
            DeleteObject(rectRgn);
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.setblurradius(r: integer);
begin
  if (fblurradius<>r) and (r>-1) and (r<20) then
  begin
    fblurradius:= r;
    if (fownerform<>nil) then
    begin
      fupdate:= true;
      fownerform.rePaint;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.CallDefault;
begin
  PrevParentWndProc(Msg);
  //Msg.Result:= CallWindowProc(PrevParentWndProc, fownerform.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.NewParentWndProc(var Msg: TMessage);
var
  PS: TPaintStruct;
//canvas: tcanvas;
begin
  case Msg.Msg of
    (*
    WM_Create:
      begin
        if (not (csdesigning in componentstate)) and (not fbackground.empty) and
          (fownerform<>nil) then
        begin
          fownerform.TransparentColorValue:= fownerform.Color;
          fownerform.TransparentColor:= true;
        end;
      end;
    *)
    WM_Activate:
      begin
        if (not ftakingscreenshot) and (msg.WParam<>WA_INACTIVE) then
        begin
          // activate form
          if not factivated then
          begin
            fupdate:= true;
            factivated:= true;
          end;
        end else
          // deactivate form
          factivated:= false;
        calldefault(msg);
      end;
    WM_EnterSizeMove:
      begin
        fmoving:= true;
        CallDefault(Msg)
      end;
    WM_ExitSizeMove:
      begin
        CallDefault(Msg);
        fmoving:= False;
        fupdate:= true;
        fownerform.Repaint;
      end;
    WM_Paint:
      begin
        if (csdesigning in componentstate) or (not fbackground.Empty) then
        begin
          CallDefault(Msg);
        end else
        begin
          if not fmoving then
          begin
            // compare state
            if (foldform_x<>fownerform.top) or (foldform_y<>fownerform.Left) or
              (foldform_w<>fownerform.width) or (foldform_h<>fownerform.height) or
              (fupdate) then
            begin
              // store old state
              fupdate:= false;
              foldform_x:= fownerform.top;
              foldform_y:= fownerform.Left;
              foldform_w:= fownerform.width;
              foldform_h:= fownerform.height;
              // get screenshot
              screenshot;
              // color blending
              ColorScale(fscreenshot, fblurcolor);
              // do gaussian blur
              if fblurradius>0 then
                bitmapblurgaussian(fscreenshot, fblurradius);
            end;
          end;
          // paint screenshot
          Msg.WParam:= BeginPaint(fownerform.Handle,PS);
          BitBlt(Msg.WParam,0,0,fscreenshot.Width,fscreenshot.Height,fscreenshot.Canvas.Handle,0,0,SRCCOPY);
          CallDefault(Msg);
          EndPaint(fownerform.Handle,PS);
        end;
      end;
    WM_EraseBkgnd:
      begin
        if (csDesigning in ComponentState) then CallDefault(Msg)
          else
          begin
            if (not fbackground.Empty) then
            begin
              fownerform.Canvas.BrushCopy(
                fbackground.Canvas.ClipRect,
                  fbackground,
                    fbackground.Canvas.ClipRect,
                      fbackground.TransparentColor);
              msg.Result:=1;
            end else
              Msg.Result:=1;
          end;
      end;
    WM_LButtonDown:
      if not (csdesigning in componentstate) then
      begin
        if (not fbackground.Empty) then
        begin
          ReleaseCapture;
          SendMessage(fownerform.Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
          msg.Result:= 0;
        end else
         calldefault(msg);
      end else
        calldefault(msg);
    else
      CallDefault(Msg);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.CaptureScreenRect(var bmp: tbitmap; ARect: TRect);
var
  ScreenDC: HDC;
begin
  bmp.Width:= arect.Right - arect.Left;
  bmp.Height:= arect.Bottom - arect.Top;
  screenDC:=GetDC(0);
  try
    bmp.PixelFormat:= pf24bit;
    bitblt(bmp.Canvas.Handle,0,0,bmp.Width,bmp.Height,screenDC,arect.Left,arect.Top,SRCCOPY);
  finally
    releaseDC(0,ScreenDC);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.ScreenShot;
var
  pt: tpoint;
  ticksnow: cardinal;
begin
  if fownerform=nil then exit;

  // get screenshot
  pt.x:= 0;
  pt.y:= 0;
  pt:= fownerform.ClientToScreen(pt);

  {*
  pt.x:= fownerform.left;
  pt.y:= fownerform.Top;

  // we need to get borders also
  // otherwise there will be some offset
  case fownerform.BorderStyle of
    bsSizeable: begin
      pt.X:= pt.X - GetSystemMetrics(SM_CXSIZEFRAME);
      pt.Y:= pt.Y - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    end;
    bsDialog: begin
      pt.X:= pt.X - GetSystemMetrics(SM_CXFIXEDFRAME);
      pt.Y:= pt.Y - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYFIXEDFRAME);
    end;
    bsSingle: begin
      pt.X:= pt.X - 1;
      pt.Y:= pt.Y - GetSystemMetrics(SM_CYCAPTION) - 1;
    end;
    bsToolWindow: begin
      pt.X:= pt.X - 1;
      pt.Y:= pt.Y - GetSystemMetrics(SM_CYSMCAPTION) - 1;
    end;
    bsSizeToolWin: begin
      pt.X:= pt.X - GetSystemMetrics(SM_CXSIZEFRAME);
      pt.Y:= pt.Y - GetSystemMetrics(SM_CYSMCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    end;
  end;
  *}

  ftakingscreenshot:= true;
  showwindow(fownerform.Handle, sw_hide);
  try
    //setactivewindow(0);
    TicksNow:=GetTickCount;
    while (GetTickCount-TicksNow)<20 do
      Application.ProcessMessages;
    capturescreenrect(fscreenshot,rect(pt.x,pt.y,pt.x+fownerform.Width,pt.y+fownerform.Height));
  finally
    showwindow(fownerform.handle, sw_show);
    ftakingscreenshot:= false;
    //setactivewindow(fownerform.Handle);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_skin.setblurcolor(c: tcolor);
begin
  if (fblurcolor<>c) then
  begin
    fblurcolor:= c;
    if (fownerform<>nil) then
    begin
      fupdate:= true;
      fownerform.rePaint;
    end;
  end;
end;

//------------------------------------------------------------------------------
type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    b: byte; {easier to type than rgbtBlue}
    g: byte;
    r: byte;
  end;
  PRow = ^TRow;
  TRow = array[0..1000000] of TRGBTriple;
  PPRows = ^TPRows;
  TPRows = array[0..1000000] of PRow;

//------------------------------------------------------------------------------
function ColorScale(Bmp: TBitmap; Color: TColor): boolean;
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y: Integer;
  Row: PRGBArray;
begin
  result:=false;
  if bmp=nil then exit;
  Bmp.PixelFormat:= pf24Bit;
  for y:= 0 to Bmp.Height-1 do
  begin
    Row:= Bmp.ScanLine[y];
    for x:= 0 to Bmp.Width-1 do
    begin
      //col:= (row[x].r + row[x].g + row[x].b) div 3;
      Row[x].r:= (row[x].r and getrvalue(color));
      Row[x].g:= (row[x].g and getgvalue(color));
      Row[x].b:= (row[x].b and getbvalue(color));
    end;
  end;
  result:= true;
end;

//------------------------------------------------------------------------------
// gaussian blur
const
  MaxKernelSize = 100;

type
  TKernelSize = 1..MaxKernelSize;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of single;
  end;
{the idea is that when using a TKernel you ignore the Weights except
for Weights in the range -Size..Size.}

procedure MakeGaussianKernel(var K: TKernel; radius: double; MaxData, DataGranularity: double);
{makes K into a gaussian kernel with standard deviation = radius. For the
 current application you set MaxData = 255 and DataGranularity = 1. Now
the procedure sets the value of K.Size so that when we use K we will 
ignore the Weights that are so small they can't possibly matter. (Small Size
is good because the execution time is going to be propertional to K.Size.)} 
var
  j: integer;
  temp, delta: double;
  KernelSize: TKernelSize;
begin
  for j := Low(K.Weights) to High(K.Weights) do 
  begin
    temp := j / radius;
    K.Weights[j] := exp(-temp * temp / 2);
  end; 
  {now divide by constant so sum(Weights) = 1:}
  temp := 0;
  for j := Low(K.Weights) to High(K.Weights) do 
    temp := temp + K.Weights[j];
  for j := Low(K.Weights) to High(K.Weights) do 
    K.Weights[j] := K.Weights[j] / temp; 
  {now discard (or rather mark as ignorable by setting Size) the entries that 
  are too small to matter - this is important, otherwise a blur with a small radius
  will take as long as with a large radius...}
  KernelSize := MaxKernelSize; 
  delta := DataGranularity / (2 * MaxData); 
  temp := 0;
  while (temp < delta) and (KernelSize > 1) do 
  begin
    temp := temp + 2 * K.Weights[KernelSize]; 
    dec(KernelSize); 
  end; 
  K.Size := KernelSize;
  {now just to be correct go back and jiggle again so the sum of the entries
  we'll be using is exactly 1} 
  temp := 0;
  for j := -K.Size to K.Size do 
    temp := temp + K.Weights[j]; 
  for j := -K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp; 
end; 
   
function TrimInt(Lower, Upper, theInteger: integer): integer; 
begin
  if (theInteger <= Upper) and (theInteger >= Lower) then
    result := theInteger
  else 
    if theInteger > Upper then
      result := Upper
    else 
      result := Lower;
end;
   
function TrimReal(Lower, Upper: integer; x: double): integer;
begin
  if (x < upper) and (x >= lower) then 
    result := trunc(x) 
  else 
    if x > Upper then
      result := Upper 
    else 
      result := Lower; 
end; 

procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
var 
  j, n: integer; 
  tr, tg, tb: double; {tempRed, etc}
  w: double;
begin 
  for j := 0 to High(theRow) do 
  begin 
    tb := 0; 
    tg := 0;
    tr := 0; 
    for n := -K.Size to K.Size do 
    begin
      w := K.Weights[n];
      {the TrimInt keeps us from running off the edge of the row...}
      with theRow[TrimInt(0, High(theRow), j - n)] do
      begin
        tb := tb + w * b;
        tg := tg + w * g;
        tr := tr + w * r;
      end;
    end;
    with P[j] do
    begin
      b := TrimReal(0, 255, tb);
      g := TrimReal(0, 255, tg);
      r := TrimReal(0, 255, tr);
    end;
  end;
  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriple));
end;

Function BitmapBlurGaussian(Bitmap: TBitmap; radius: double): Boolean;
Var
  Row      : integer;
  Col      : integer;
  theRows  : PPRows;
  K        : TKernel;
  ACol     : PRow;
  P        : PRow;
Begin
  result:= false;
  if bitmap=nil then exit;
  Try
    MakeGaussianKernel(K, radius, 255, 1);
    GetMem(theRows, Bitmap.Height * SizeOf(PRow));
    GetMem(ACol, Bitmap.Height * SizeOf(TRGBTriple));
    {record the location of the bitmap data:}
    for Row := 0 to Bitmap.Height - 1 do
      theRows[Row] := Bitmap.Scanline[Row];
    {blur each row:}
    P := AllocMem(Bitmap.Width * SizeOf(TRGBTriple));
    for Row := 0 to Bitmap.Height - 1 do
      BlurRow(Slice(theRows[Row]^, Bitmap.Width), K, P);
    {now blur each column}
    ReAllocMem(P, Bitmap.Height * SizeOf(TRGBTriple));
    for Col := 0 to Bitmap.Width - 1 do
    begin
      {first read the column into a TRow:}
      for Row := 0 to Bitmap.Height - 1 do
        ACol[Row] := theRows[Row][Col];
      BlurRow(Slice(ACol^, Bitmap.Height), K, P);
      {now put that row, um, column back into the data:}
      for Row := 0 to Bitmap.Height - 1 do
        theRows[Row][Col] := ACol[Row];
    end;
    FreeMem(theRows);
    FreeMem(ACol);
    ReAllocMem(P, 0);
    Result := True;
  Except
    Result := False;
  End;
end;


procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_skin]);
end;

end.
