unit Ccube;  { Spinning Cube Component }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, ExtCtrls, Dialogs;

type
  TPoint3d = Record X,Y,Z :Integer; End;
  TCubeDispOpt = (cdoErase,
                  cdoHide,  { Hidden Line Removal? }
                  cdoDblBuf,
                  cdoAuto,
                  cdoFaces,
                  cdoShade,
                  cdoColor);
  TCubeDispOpts = Set of TCubeDispOpt;
  TCubeSpin = class(TPanel)
  private
    { Private declarations }
    Vertices :Array [0..7] of TPoint3d;
    VRotated :Array [0..7] of TPoint3d;
    XForm3d  :Array [0..3,0..3] of Single;
    Perspective :Integer;
    XP :Integer;
    YP :Integer;
    ScaleMe :Integer;  { Scaling up from 1 }
    ZScale :Integer;
    fContinuous :Boolean;
    fSpinInc :Integer;

    AngleX :Integer;  { Y/Z Rotation about X }
    AngleY :Integer;  { X/Z Rotation about Y }
    AngleZ :Integer;  { X/Y Rotation about Z }
    fXSpinOn :Boolean;
    fYSpinOn :Boolean;
    fZSpinOn :Boolean;

    fOptions :TCubeDispOpts;
    fOnSpin :TNotifyEvent;

    HideV  :Integer;  { Hidden vertex }
    LastV  :Integer;  { Last Drawn Vector }

    CubeBMP: HBITMAP; { Cache the bitmap for redraw speed }
    BMPCanvas :TCanvas;
    fUpdating :Boolean;  { Updating Controls }
    fTimer :TTimer;
    fForceErase :Boolean;

    function Rotate(P:TPoint; Rotation:Integer):TPoint;
    function Rotate2D(P:TPoint; Rotation:Integer):TPoint;
    function Rotate3D(Const P:TPoint3D):TPoint3D;
    function Rotate3D2(Const P:TPoint3D):TPoint3D;
    Procedure XFormBld; { Precompute Transformation Matrix }
    function Rotate3D3(Const P:TPoint3D):TPoint3D;
    function MapPt(Vertex:Integer):TPoint;
    procedure MovetoPt(dc:HDC; P:TPoint);
    procedure LinetoPt(dc:HDC; P:TPoint);
    procedure Connect(dc:HDC; V1,V2:Integer);
    procedure SetSize;
    procedure DrawCube;
    procedure BMPDraw;
    procedure RotateCube;
    procedure DrawEdges(dc:HDC);
    procedure DrawFaces(c:TCanvas);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMEnterIdle(var Message: TWMEnterIdle); message WM_ENTERIDLE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure TimerElapse(Sender: TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    Probs :TStrings;
    Constructor Create(AOwner:TComponent); Override;
    Destructor Destroy; Override;
    Procedure SetAngles( AX,AY,AZ :Integer);
    procedure SetXSpin( newValue :Integer);
    procedure SetYSpin( newValue :Integer);
    procedure SetZSpin( newValue :Integer);
    procedure SetOptions( newValue :TCubeDispOpts);
    procedure SetContinuous( newValue:Boolean);
  published
    { Published declarations }
    Property XSpin :Integer Read AngleX Write SetXSpin;
    Property YSpin :Integer Read AngleY Write SetYSpin;
    Property ZSpin :Integer Read AngleZ Write SetZSpin;

    Property Options :TCubeDispOpts Read fOptions Write SetOptions;
    Property Continuous :Boolean Read fContinuous Write SetContinuous;
    Property SpinInc :Integer Read fSpinInc Write fSpinInc Default 1;

    Property XSpinOn :Boolean Read fXSpinOn Write fXSpinOn Default True;
    Property YSpinOn :Boolean Read fYSpinOn Write fYSpinOn Default True;
    Property ZSpinOn :Boolean Read fZSpinOn Write fZSpinOn Default True;

    Property OnSpin :TNotifyEvent Read fOnSpin Write fOnSpin;
  private
    Sins :Array[0..359] of Single;
    Function FastSin(Degrees:Integer):Single;
    Function FastCos(Degrees:Integer):Single;
    Procedure InitSin;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lib', [TCubeSpin]);
end;

{ Computing SIN and COS are VERY slow.  Because we do this
  A LOT, and we only work in even degrees we precompute the
  SIN for all 360 degrees (which also saves a degrees -> Radians
  conversion).  This dramatically speeds up processing cube
  Rotations. }

Function TCubeSpin.FastSin(Degrees:Integer):Single;
Begin
  While Degrees < 0 do Inc(Degrees,360);
  While Degrees >= 360 do Dec(Degrees,360);
  Result := Sins[Degrees];
End;
Function TCubeSpin.FastCos(Degrees:Integer):Single;
Begin
  Result := FastSin(90-Degrees);
End;
Procedure TCubeSpin.InitSin;
Var Degrees:Integer;
Begin
  For Degrees := 0 to 359 do
      Sins[Degrees] := Sin(Degrees * PI /180);
  {$IFDEF DEBUGs}
  Degrees := 0;
  While Degrees <= 360 do Begin
      If ABS(FastCos(Degrees) - Cos(Degrees * PI /180)) > 0.000001 Then
         Raise Exception.Create(
               StrVal(['No match on ',Degrees,': ',FastCos(Degrees),' <> ',Cos(Degrees * PI /180)]));
      Inc(Degrees,5);
  End;
  {$ENDIF}
End;

{ Take the X,Y, and Z coordinates and return a 3D Point }
Function Point3d(X1,Y1,Z1:Integer):TPoint3d;
Begin
  Result.X := X1;
  Result.Y := Y1;
  Result.Z := Z1;
End;

{ Compare two (2D) points for equality }
Function ComparePt(A,B:TPoint):Boolean;
Begin
  Result := (A.X = B.X) and (A.Y = B.Y);
End;

{ Take a 3D Point (vertex #) and map it to the 2D screen }
function TCubeSpin.MapPt(Vertex:Integer):TPoint;
Var NewPt   :TPoint;
    New3DPt :TPoint3D;
Begin
  With VRotated[Vertex] do
    if cdoHide in fOptions Then Begin { Hidden Line Removal? }
       { No Perspective }
       Result.X := X+XP;
       Result.Y := Y+YP;
    End Else Begin { Perspective }
       Result.X := X+XP + X * Z div ZScale;
       Result.Y := Y+YP + Y * Z div ZScale;
     End;
End;

{ Rotate a point by transforming it from rectangular to polar,
  adjusting the polar angle, and transforming it back.
  This was WAY too slow and was replaced with the
  2D transformation matrix approach below }
function TCubeSpin.Rotate(P:TPoint; Rotation:Integer):TPoint;
Var   D     :Single;
      Angle :Integer;
Begin
  { Compute Polar }
  With P Do Begin
    D   := Sqrt(X*X + Y*Y);
    If Y <> 0 Then
       Angle := Trunc(ArcTan(X/Y) * 180 / PI)
    Else
       If X < 0 Then
          Angle := -90
       Else
          Angle := 90;
    If Y < 0 Then
       Angle := Angle + 180;
  End;
  { Compute New X,Y }
  Angle :=Angle + Rotation;
  Result.X := Trunc(D * FastSin(Angle));
  Result.Y := Trunc(D * FastCos(Angle));
End;

{ 2D Rotation via transform matrix:
           [ cos A,  sin A, 0 ]  general [ a  b  0 ]
           [ -sin A, cos A, 0 ]          [ c  d  0 ]
           [ 0,      0,     0 ]          [ tx ty 1 ]
   or X' := aX + bY + tx := (cos A)X  + (sin A)Y;
      Y' := cX + dY + ty := (-sin A)X + (cos A)Y;
}
function TCubeSpin.Rotate2D(P:TPoint; Rotation:Integer):TPoint;
Var   sinA, cosA :Single;
Begin
  sinA := FastSin(Rotation);
  cosA := FastCos(Rotation);
  Result.X := Trunc(cosA * P.X  + sinA * P.Y);
  Result.Y := Trunc(-sinA * P.X + cosA * P.Y);
(*  If Not ComparePt(Result, Rotate(P, Rotation)) Then
     Raise Exception.Create(
           StrVal(['No match on ',Rotation,': ',StrPt(P),' -> ',
                       StrPt(Result),' ',
                       StrPt(Rotate(P, Rotation))])); *)
End;

{ Do a 3D rotate by performing 2D rotates around each axis.
  This is probably much slower than using a 3D transformation
  Matrix, but I've been too lazy to figure out what a three-D
  transformation Matrix would look like. }
function TCubeSpin.Rotate3D(Const P:TPoint3D):TPoint3D;
Var NewPt  :TPoint;
Begin
  Result := P;
  With Result do Begin
    NewPt := Rotate2D(Point(X,Y), AngleZ); { X/Y Rotate about Z }
    X := NewPt.X;  Y := NewPt.Y;
    NewPt := Rotate2D(Point(X,Z), AngleY); { X/Z Rotate about Y }
    X := NewPt.X;  Z := NewPt.Y;
    NewPt := Rotate2D(Point(Y,Z), AngleX); { Y/Z Rotate about X }
    Y := NewPt.X;  Z := NewPt.Y;
  End;
End;

{ Do a 3D rotate by using a 3D transformation Matrix:
   [ cos Az * cos Ay, sin Az, -sin Ay, 0 ]  general [ a  b  c  0 ]
   [ -sin Az, cos Az * cos Ax, sin Ax, 0 ]          [ d  e  f  0 ]
   [ sin Ay, -sin Ax, cos Ax * cos Ay, 0 ]          [ g  h  i  0 ]
   [ 0,      0,       0,               1 ]          [ tx ty tz 1 ]
   or X' := aX + bY + cZ + tx := (cos Az)(cos Ay)X  + (sin Az)Y - (sin Ay)Z;
      Y' := dX + eY + fZ + ty := (-sin Az)X + (cos Az)(cos Ax)Y + (sin Ax)Z;
      Z' := fX + gY + hZ + ty := (sin Ay)X -(sin Ax)Y + (cos Ax)(cos Ay)Z;

 [ cosY*cosZ                 cosY*-sinZ                 -sinY      0  ]
 [ -sinXsinY*cosZ+cosX*sinZ  -sinXsinY*-sinZ+cosX*cosZ  -sinXcosY  0  ]
 [ cosXsinY*cosZ+sinX*sinZ   cosXsinY*-sinZ+sinX*cosZ   cosXcosY   0  ]
 [ 0                         0                          0          1  ]

or

X' = cosAY*cosAZ*X - cosAY*sinAZ*Y - sinAY*Z
Y' = (-sinAX*sinAY*cosAZ + cosAX*sinAZ)*X + (sinAX*sinAY*sinAZ+cosAX*cosAZ)Y
	- sinAX*cosAY*Z
Z' = (cosAX*sinAY*cosAZ+sinAX*sinAZ)*X + (-cosAX*sinAY*sinAZ+sinAX*cosAZ)Y
	+ (cosAX*cosAY)Z
}
function TCubeSpin.Rotate3D2(Const P:TPoint3D):TPoint3D;
Var NewPt  :TPoint3d;
    sinAx, cosAx :Single;
    sinAy, cosAy :Single;
    sinAz, cosAz :Single;
Begin
  sinAx := FastSin(AngleX);  cosAx := FastCos(AngleX);
  sinAy := FastSin(AngleY);  cosAy := FastCos(AngleY);
  sinAz := FastSin(AngleZ);  cosAz := FastCos(AngleZ);
  With P do Begin
    Result.X := Trunc( cosAY*cosAZ*X - cosAY*sinAZ*Y - sinAY*Z);
    Result.Y := Trunc( (-sinAX*sinAY*cosAZ + cosAX*sinAZ)*X
                       + (sinAX*sinAY*sinAZ+cosAX*cosAZ)*Y
	               - sinAX*cosAY*Z);
    Result.Z := Trunc( (cosAX*sinAY*cosAZ+sinAX*sinAZ)*X
                       + (-cosAX*sinAY*sinAZ+sinAX*cosAZ)*Y
	               + (cosAX*cosAY)*Z);
  End;
(*  NewPt := Rotate3d(P);
  If (Abs(NewPt.X-Result.X)>2) or (Abs(NewPt.Y-Result.Y)>2) or (Abs(NewPt.Z-Result.Z)>2) Then
     { Raise Exception.Create( }
     Probs.Add( Format( 'No match on X %3d:%4d ->%4d %4d, Y %3d:%4d ->%4d %4d, Z %3d:%4d ->%4d %4d',
                        [AngleX,P.X,Result.X,NewPt.X,
                         AngleY,P.Y,Result.Y,NewPt.Y,
                         AngleZ,P.Z,Result.Z,NewPt.Z ]));   (**)
End;

Procedure TCubeSpin.XformBld; { Build the 3D Transformation Matrix }
Var sinAx, cosAx :Single;
    sinAy, cosAy :Single;
    sinAz, cosAz :Single;
Begin
  sinAx := FastSin(AngleX);  cosAx := FastCos(AngleX);
  sinAy := FastSin(AngleY);  cosAy := FastCos(AngleY);
  sinAz := FastSin(AngleZ);  cosAz := FastCos(AngleZ);
  XForm3d[0,0] := cosAY*cosAZ;       { X' }
  XForm3d[0,1] := - cosAY*sinAZ;
  XForm3d[0,2] := -sinAY;
  XForm3d[1,0] := -sinAX*sinAY*cosAZ + cosAX*sinAZ; { Y' }
  XForm3d[1,1] := sinAX*sinAY*sinAZ+cosAX*cosAZ;
  XForm3d[1,2] := - sinAX*cosAY;
  XForm3d[2,0] := cosAX*sinAY*cosAZ+sinAX*sinAZ; { Z' }
  XForm3d[2,1] := -cosAX*sinAY*sinAZ+sinAX*cosAZ;
  XForm3d[2,2] := cosAX*cosAY;
End;
{ Now use it! }
function TCubeSpin.Rotate3D3(Const P:TPoint3D):TPoint3D;
Var NewPt  :TPoint3d;
Begin
  With P do Begin
    Result.X := Trunc( XForm3d[0,0]*X + XForm3D[0,1]*Y + XForm3D[0,2]*Z);
    Result.Y := Trunc( XForm3d[1,0]*X + XForm3D[1,1]*Y + XForm3D[1,2]*Z);
    Result.Z := Trunc( XForm3d[2,0]*X + XForm3D[2,1]*Y + XForm3D[2,2]*Z);
  End;
(*  NewPt := Rotate3d(P);
  If (Abs(NewPt.X-Result.X)>2) or (Abs(NewPt.Y-Result.Y)>2) or (Abs(NewPt.Z-Result.Z)>2) Then
     { Raise Exception.Create( }
     Probs.Add( Format( 'No match on X %3d:%4d ->%4d %4d, Y %3d:%4d ->%4d %4d, Z %3d:%4d ->%4d %4d',
                        [AngleX,P.X,Result.X,NewPt.X,
                         AngleY,P.Y,Result.Y,NewPt.Y,
                         AngleZ,P.Z,Result.Z,NewPt.Z ]));   (**)
End;

{ Does a GDI moveto using a point instead of X,Y coords }
procedure TCubeSpin.MovetoPt(dc:HDC; P:TPoint);
Begin
  Moveto(dc,P.X,P.Y);
End;

{ Does a GDI lineto using a point instead of X,Y coords }
procedure TCubeSpin.LinetoPt(dc:HDC; P:TPoint);
Begin
  Lineto(dc,P.X,P.Y);
End;

{ Draw a line from the last point to this point.
  Also does 3D to 2D mapping and limited hidden
  line removal. }
procedure TCubeSpin.Connect(dc:HDC; V1,V2:Integer);
Begin
  If (cdoHide in fOptions) and ( (V1=HideV) or (V2=HideV) ) Then Exit;
  If V1 <> LastV Then
     MovetoPt(dc, MapPt(V1));
  LineToPt(dc, MapPt(V2));
  LastV := V2;
End;

{ Windows calls here to ask us to repaint }
procedure TCubeSpin.Paint;
Begin
  if CubeBMP = 0 Then
     CubeBMP := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
  { MUST be false if not Double Buffering to avoid endless loop }
  { Must be true for double buffering so we don't get trash! }
  DrawCube;
End;

{ Rotate the Cube to the correct angles, then
  Check for hidden line removal and process }
procedure TCubeSpin.RotateCube;
Var I   :Integer;
begin
  XFormBld; { Precompute Transformation Matrix }
  For I := Low(Vertices) to High(Vertices) do
      VRotated[I] := Rotate3D3(Vertices[I]);
  if (cdoHide in fOptions) Then Begin  { Find furthest back vector and hide it }
     HideV  := Low(Vertices);
     For I := Low(Vertices)+1 to High(Vertices) do
         If VRotated[HideV].Z > VRotated[I].Z Then
            HideV := I;
  End;
end;

{ Come here to draw the cube by it's edges }
procedure TCubeSpin.DrawEdges(dc:HDC);
begin
    MovetoPT(dc,MapPt(0));  { Draw the Top }
    LastV  := 0;  { Last Drawn Vector }
    Connect(dc,0,1);
    Connect(dc,1,2);
    Connect(dc,2,3);
    Connect(dc,3,0);
    Connect(dc,4,5);  { Draw the Bottom }
    Connect(dc,5,6);
    Connect(dc,6,7);
    Connect(dc,7,4);
    Connect(dc,4,0);  { Draw the sides }
    Connect(dc,5,1);
    Connect(dc,6,2);
    Connect(dc,7,3);
end;

{ Come here to draw the cube by it's faces }
procedure TCubeSpin.DrawFaces(C:TCanvas);
  { Computes Color Brightness for a Face }
  Function Intensity(V:Array of Integer):Integer;
  Var  I :Integer;
       MinZ, MaxZ :Integer;
  Begin
    MinZ := Low(V);
    MaxZ := Low(V);
    For I := Low(V)+1 to High(V) do Begin
        If VRotated[V[I]].Z < VRotated[V[MinZ]].Z Then
           MinZ := I;
        If VRotated[V[I]].Z > VRotated[V[MaxZ]].Z Then
           MaxZ := I;
    End;
    { Faces flat to observer get highest value,
      Faces on edge get lowest.  This will actually make
      the back face bright, but who cares since it's never seen.
      }
    Result := Abs(VRotated[V[MinZ]].Z - VRotated[V[MaxZ]].Z);
    { Use LongInt for intermediate calc,
      When descaled, longest distance is 2
    }
    Result := 255 - LongInt(Result) * 64 Div ScaleMe;
    If Result < 0 Then Result := 0;
  End;
  procedure OneFace(V1,V2,V3,V4:Integer);
  Var  I :Integer;
  Begin
    If HideV in [V1,V2,V3,V4] Then Exit; { Hidden }
    if cdoShade in Options Then Begin
       I := Intensity([V1,V2,V3,V4]); { How bright }
       if cdoColor in Options Then
          C.Brush.Color := RGB(I div 2,I div 4,I)
       Else
          C.Brush.Color := RGB(I, I, I);  { Gray Scale }
    End Else Begin
       if cdoColor in Options Then
          C.Brush.Color := RGB(V1*64,V2*64,V3*64)
       Else Begin
          I := (V1 * 64 + V2 * 16 + V3) Mod 256;
          C.Brush.Color := RGB(I, I, I);
       End;
    End;
    {C.Brush.Color := RGB((A SHR 1) and 255, A and 63, -A and 63);}
    C.Polygon([ MapPt(V1), MapPt(V2), MapPt(V3), MapPt(V4) ]);
  End;
begin
    OneFace(0,1,2,3);  { Draw the Top }
    OneFace(4,5,6,7);  { Draw the Bottom }
    OneFace(0,1,5,4);  { Draw the Sides }
    OneFace(1,2,6,5);
    OneFace(2,3,7,6);
    OneFace(3,0,4,7);
end;

procedure TCubeSpin.DrawCube;
Var dc  :HDC;
    I   :Integer;
begin
  RotateCube;
  Canvas.Brush.Color := Color;
  if (cdoDblBuf in fOptions) Then
     BMPDraw
  Else
    With Canvas Do Begin
      If fForceErase or (cdoErase in fOptions) Then
         FillRect(ClientRect);
      Pen.Color := clBlack;
      if (cdoFaces in fOptions) Then
         DrawFaces(Canvas)
      Else
         DrawEdges(Canvas.Handle);
    End;
  fForceErase := False;
end;

procedure TCubeSpin.BMPDraw;
Var
  ScreenDC, BMPDC: HDC;
  SaveBMP: HBITMAP;
  I :Integer;
  CliRect :TRect;
begin
  CliRect := ClientRect;
{  ScreenDC := pDraw.Canvas.Handle; {GetDC(0); }
  BMPDC    := CreateCompatibleDC(Canvas.Handle);
  try
    SaveBMP  := SelectObject(BMPDC, CubeBMP);
    try
      BMPCanvas.Handle := BMPdc;
      { Clear the contents of the bitmap }
      If fForceErase or (cdoErase in fOptions) Then
         FillRect(BMPdc, CliRect, Canvas.Brush.Handle);

    if (cdoFaces in fOptions) Then
       DrawFaces(BMPCanvas)
    Else
       DrawEdges(BMPdc);

      { Paint the BMP onto the canvas and release }
      Canvas.CopyRect(CliRect, BMPCanvas, Rect(0,0,ClientWidth,ClientHeight));
    finally
      SelectObject(BMPDC, SaveBMP);
    End;
  Finally
    DeleteDC(BMPDC);
    { ReleaseDC(0, ScreenDC); }
  end;
end;

Constructor TCubeSpin.Create(AOwner:TComponent);
Begin
  Inherited Create(AOwner);
  fSpinInc := 5;
  fXSpinOn := True;
  fYSpinOn := False;
  fZSpinOn := True;
  fOptions := [cdoErase, cdoHide, cdoDblBuf, cdoFaces, cdoColor, cdoShade];
  InitSin;
  BMPCanvas := TCanvas.Create;
  Probs := TStringList.Create;
  {SetSize; }
End;

Destructor TCubeSpin.Destroy;
begin
  If CubeBMP <> 0 Then
     DeleteObject(CubeBMP);
  BMPCanvas.Free;
  Probs.Free;
  Inherited Destroy;
end;

{ Builds a cube of the proper size, and allocates a
  bitmap of the correct size for double buffering }
Procedure TCubeSpin.SetSize;
Var I        :Integer;
begin
  Vertices[0] := Point3d(-1,-1,-1);
  Vertices[1] := Point3d(-1, 1,-1);
  Vertices[2] := Point3d( 1, 1,-1);
  Vertices[3] := Point3d( 1,-1,-1);
  Vertices[4] := Point3d(-1,-1, 1);
  Vertices[5] := Point3d(-1, 1, 1);
  Vertices[6] := Point3d( 1, 1, 1);
  Vertices[7] := Point3d( 1,-1, 1);
  If ClientWidth <= ClientHeight Then
     ScaleMe := ClientWidth div 4
  Else
     ScaleMe := ClientHeight div 4;
  For I := 0 to High(Vertices) do
      with Vertices[I] do Begin
           X := X * ScaleMe;
           Y := Y * ScaleMe;
           Z := Z * ScaleMe;
      End;
  Perspective := 8;
  XP := ClientWidth div 2;
  YP := ClientHeight div 2;
  ZScale := ScaleMe * Perspective;  { Downscale Z-Axis values by this }

  { Create a bitmap to handle smooth redraws }
  If CubeBMP <> 0 Then
     DeleteObject(CubeBMP);
  If Parent <> Nil Then
     CubeBMP := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
  fForceErase := True;
end;

Procedure TCubeSpin.SetAngles( AX,AY,AZ :Integer);
Begin
  If (AngleZ = AZ) and (AngleY = AY) and (AngleX = AX) Then
     Exit;
  AngleX := AX MOD 360;
  AngleY := AY MOD 360;
  AngleZ := AZ MOD 360;
  If AngleX < 0 Then Inc(AngleX, 360);
  If AngleY < 0 Then Inc(AngleY, 360);
  If AngleZ < 0 Then Inc(AngleZ, 360);
  Invalidate; { Request a Redraw }
  If Assigned(fOnSpin) Then
     fOnSpin(Self);
end;

procedure TCubeSpin.SetXSpin( newValue :Integer);
Begin
  If newValue = AngleX then Exit;
  SetAngles( newValue, AngleY, AngleZ);
End;

procedure TCubeSpin.SetYSpin( newValue :Integer);
Begin
  If newValue = AngleY then Exit;
  SetAngles( AngleX, newValue, AngleZ);
End;

procedure TCubeSpin.SetZSpin( newValue :Integer);
Begin
  If newValue = AngleZ then Exit;
  SetAngles( AngleX, AngleY, newValue);
End;

procedure TCubeSpin.SetOptions( newValue :TCubeDispOpts);
Begin
  if cdoFaces in newValue Then { Faces requires hidden line removal }
     Include(newValue, cdoHide);
  if newValue = fOptions Then Exit;
  fOptions := newValue;
  Invalidate;
End;

procedure TCubeSpin.SetContinuous( newValue:Boolean);
Begin
  If newValue = fContinuous then Exit;
  fContinuous := newValue;
  If fContinuous Then Begin
    fTimer := TTimer.Create(Self);
    fTimer.Interval := 1;
    fTimer.OnTimer := TimerElapse;
    fTimer.Enabled := True;
  End Else Begin
    fTimer.Free;
    fTimer := Nil;
  End;
End;

{ Suppress Windows normal background erasure.
  If we're double buffering, then we always paint the whole
  area, so erasing the background would cause unnecessary
  flicker.
  If we're not double buffering, then we let the normal
  erase occur (which DOES cause flicker), unless the
  user has turned off erasing.
}
procedure TCubeSpin.WMEraseBkgnd(var Message: TWMEraseBkgnd);
Begin
{  if (cdoDblBuf in fOptions) Then }
     Message.Result := 1
{  Else
     Inherited; }
End;

procedure TCubeSpin.WMEnterIdle(var Message: TWMEnterIdle);
Var AX,AY,AZ :Integer;
Begin
  If fContinuous Then Begin
     AX := AngleX;
     AY := AngleY;
     AZ := AngleZ;
     If fXSpinOn Then Inc(AX, fSpinInc);
     If fYSpinOn Then Inc(AY, fSpinInc);
     If fZSpinOn Then Inc(AZ, fSpinInc);
     SetAngles( AX, AY, AZ);
  End;
End;

{ Trap the Windows message requesting our size change,
  let it, then recreate the bitmap drawing buffer,
  resize the cube, and request a redraw }
procedure TCubeSpin.WMSize(var Message: TWMSize);
Begin
  Inherited;
  if Message.SizeType in [SIZE_MAXHIDE,SIZE_MAXSHOW] Then
     Exit;  { Not our window that was resized }
  SetSize;
  Invalidate;
end;

procedure TCubeSpin.TimerElapse(Sender: TObject);
Var AX,AY,AZ :Integer;
Begin
  If fContinuous Then Begin
     AX := AngleX;
     AY := AngleY;
     AZ := AngleZ;
     If fXSpinOn Then Inc(AX, fSpinInc);
     If fYSpinOn Then Inc(AY, fSpinInc);
     If fZSpinOn Then Inc(AZ, fSpinInc);
     SetAngles( AX, AY, AZ);
  End;
End;

end.
