unit API_bmpzoom;

// component revision history
// r1.01, ari pikivirta
// * updated email information in the version string

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics;

type
  TAPI_bmpzoom = class(tpanel)
  private
    fversion: string;
    frotated: tbitmap;
    fpicture: tpicture;
    fbackcolor: tcolor;
    fxpos: double;
    fypos: double;
    fzoom: double;
    foutside: boolean;
    fshowleft: integer;
    fshowright: integer;
    fshowtop: integer;
    fshowbottom: integer;
    fpictwidth: integer;
    fpictheight: integer;
    frotation: smallint;
    procedure setpicture(p: tpicture);
    procedure setxpos(d:double);
    procedure setypos(d:double);
    procedure setzoom(d:double);
    procedure setbackcolor(c: tcolor);
    procedure setrotation(a:smallint);
    procedure Rotatebitmap(Src: tbitmap; var Dst: tbitmap; cx, cy: Integer; Angle: Extended);
  protected
    procedure paint; override;
  public
    constructor create(aowner:tcomponent); override;
    destructor destroy; override;
  published
    property Version: string read fversion;
    property Picture: tpicture read fpicture write setpicture;
    property PicWidth: integer read fpictwidth;
    property PicHeight: integer read fpictheight;
    property Rotation: smallint read frotation write setrotation;
    property ShowLeft: integer read fshowleft;
    property ShowRight: integer read fshowright;
    property ShowTop: integer read fshowtop;
    property ShowBottom: integer read fshowbottom;
    property XPos: double read fxpos write setxpos;
    property YPos: double read fypos write setypos;
    property Zoom: double read fzoom write setzoom;
    property BackColor: tcolor read fbackcolor write setbackcolor;
    property Outside: boolean read foutside;
  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfo = 'r1.01/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
constructor tAPI_bmpzoom.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fpicture:=tpicture.create;
  frotated:=tbitmap.create;
  frotation:=0;
  fzoom:=1;
  fxpos:=width/2;
  fypos:=height/2;
  foutside:=false;
end;

//------------------------------------------------------------------------------
destructor tAPI_bmpzoom.destroy;
begin
  frotated.free;
  fpicture.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setpicture(p:tpicture);
begin
  fpicture.assign(p);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setbackcolor(c:tcolor);
begin
  if c<>fbackcolor then
  begin
    fbackcolor:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setrotation(a:smallint);
begin
  if frotation<>a then
  begin
    frotation:=a;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setxpos(d:double);
begin
  if d<>fxpos then
  begin
    fxpos:=d;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setypos(d:double);
begin
  if d<>fypos then
  begin
    fypos:=d;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.setzoom(d:double);
begin
  if d<>fzoom then
  begin
    fzoom:=d;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.paint;
var
  inrect: trect;
  wz: double;
  wz2: double;
  hz: double;
  hz2: double;
  outbmp: tbitmap;
begin
  // check if outside area
  foutside:=((fxpos<0) or (fxpos>fpicture.Width) or
    (fypos<0) or (fypos>fpicture.Height));

  // create temp bitmap
  outbmp:=tbitmap.Create;
  outbmp.Height:=height;
  outbmp.Width:=width;

  // fill background
  outbmp.Canvas.Brush.Color:=fbackcolor;
  outbmp.canvas.Brush.Style:=bssolid;
  outbmp.canvas.FillRect(outbmp.Canvas.ClipRect);

  // get zoomed picture size
  wz:=width*fzoom;
  wz2:=wz/2;
  hz:=width*fzoom;
  hz2:=hz/2;
  inrect.Left:=round((fxpos-wz2));
  inrect.Right:=round((fxpos+wz2));
  inrect.Top:=round((fypos-hz2));
  inrect.Bottom:=round((fypos+hz2));

  fshowleft:= inrect.left;
  fshowright:= inrect.right;
  fshowtop:= inrect.top;
  fshowbottom:= inrect.bottom;

  fpictwidth:= fpicture.Width;
  fpictheight:= fpicture.Height;

  if assigned(fpicture) then
  begin
    canvas.CopyMode:=cmSrcCopy;
    if frotation<>0 then
    begin
      // rotate original
      frotated.Height:=fpicture.Height;
      frotated.Width:=fpicture.Width;
      rotatebitmap(fpicture.bitmap,frotated,fpicture.width div 2, fpicture.Height div 2, frotation);
      // draw rotated
      Canvas.CopyRect(outbmp.canvas.ClipRect,frotated.Canvas,inrect);
    end else
    begin
      // draw original
      Canvas.CopyRect(outbmp.canvas.ClipRect,fpicture.Bitmap.Canvas,inrect);
    end;
  end;

  outbmp.Free;
end;

//------------------------------------------------------------------------------
procedure tAPI_bmpzoom.Rotatebitmap(Src: tbitmap; var Dst: tbitmap; cx, cy: Integer; Angle: Extended);
  function IntToByte(i:Integer):Byte;
  begin
    if i>255 then Result:=255
      else if i<0 then Result:=0
      else Result:=i;
  end;
  function TrimInt(i, Min, Max: Integer): Integer;
  begin
    if      i>Max then Result:=Max
    else if i<Min then Result:=Min
    else               Result:=i;
  end;
type
  TFColor  = record b,g,r:Byte end;
var
  Top,Bottom,Left,Right,eww,nsw,fx,fy,wx,wy: Extended;
  cAngle,sAngle: Double;
  xDiff,yDiff,ifx,ify,px,py,ix,iy,x,y: Integer;
  nw,ne,sw,se: TFColor;
  P1,P2,P3: Pbytearray;
begin
  // change pixel format
  src.pixelformat:=pf24bit;
  dst.PixelFormat:=pf24bit;

  Angle:=angle;
  Angle:=-Angle*Pi/180;
  sAngle:=Sin(Angle);
  cAngle:=Cos(Angle);
  xDiff:=(Dst.Width-Src.Width) div 2;
  yDiff:=(Dst.Height-Src.Height) div 2;
  for y:=0 to Dst.Height-1 do
  begin
    P3:=Dst.scanline[y];
    py:=2*(y-cy)+1;
    for x:=0 to Dst.Width-1 do
    begin
      px:=2*(x-cx)+1;
      fx:=(((px*cAngle-py*sAngle)-1)/ 2+cx)-xDiff;
      fy:=(((px*sAngle+py*cAngle)-1)/ 2+cy)-yDiff;
      ifx:=Round(fx);
      ify:=Round(fy);

      // check for picture limits
      if(ifx>-1)and(ifx<Src.Width)and(ify>-1)and(ify<Src.Height)then
      begin
        eww:=fx-ifx;
        nsw:=fy-ify;
        iy:=TrimInt(ify+1,0,Src.Height-1);
        ix:=TrimInt(ifx+1,0,Src.Width-1);
        P1:=Src.scanline[ify];
        P2:=Src.scanline[iy];
        nw.r:=P1[ifx*3];
        nw.g:=P1[ifx*3+1];
        nw.b:=P1[ifx*3+2];
        ne.r:=P1[ix*3];
        ne.g:=P1[ix*3+1];
        ne.b:=P1[ix*3+2];
        sw.r:=P2[ifx*3];
        sw.g:=P2[ifx*3+1];
        sw.b:=P2[ifx*3+2];
        se.r:=P2[ix*3];
        se.g:=P2[ix*3+1];
        se.b:=P2[ix*3+2];

        Top:=nw.b+eww*(ne.b-nw.b);
        Bottom:=sw.b+eww*(se.b-sw.b);
        P3[x*3+2]:=IntToByte(Round(Top+nsw*(Bottom-Top)));

        Top:=nw.g+eww*(ne.g-nw.g);
        Bottom:=sw.g+eww*(se.g-sw.g);
        P3[x*3+1]:=IntToByte(Round(Top+nsw*(Bottom-Top)));

        Top:=nw.r+eww*(ne.r-nw.r);
        Bottom:=sw.r+eww*(se.r-sw.r);
        P3[x*3]:=IntToByte(Round(Top+nsw*(Bottom-Top)));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_bmpzoom]);
end;

end.
