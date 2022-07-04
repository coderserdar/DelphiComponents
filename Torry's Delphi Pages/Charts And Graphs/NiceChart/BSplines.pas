Unit BSplines;
{------------------------------------------------------------------------------}
{                                                                              }
{ This code was written by : M. v. Engeland                                    }
{                                                                              }
{ This code is copyright 2000 by                                               }
{       M. v. Engeland                                                         }
{                                                                              }
{                                                                              }
{   This is the followup of the B-splines component posted in 1997.            }
{   Some bugs were fixed, and some changes were made. The splines now use      }
{   dynamic allocation of memory when needed when adding points to the spline. }
{   If the number of points to be added is known in advance, the the precise   }
{   amount of memory can be allocated setting the capacity property.           }
{                                                                              }
{   The conditions for using this component however are not changed:           }
{   You may use this component in any way you want to, whether it is for       }
{   commercial purposes or not, as long as you don't hold me responsible for   }
{   any disasters it may cause. But I would appreciate it if you would send me }
{   an e-mail (martijn@dutw38.wbmt.tudelft.nl) to tell me what you use it for  }
{   because over the last three years I've learned that it has been used for   }
{   a wide variaty of interesting applications which it was initially never    }
{   intended for. Besides, it's nice to know how your offspring's doing.       }
{   Also all comments and/or remarks are welcome.                              }
{                                                                              }
{   See the demo program for how to use the component.                         }
{   Special attention is payed on the possibility of interpolating the         }
{   vertices. As you may or may not know, B-Splines normally do not            }
{   interpolate the used controlpoints. Thanks to simple matrix calculation    }
{   however it is possible to interpolate the controlpoints by calculating     }
{   new vertices in such a way that the spline interpolates the original ones. }
{------------------------------------------------------------------------------}

interface

uses windows,
     sysutils,
     classes,
     Graphics,
     dialogs,
     Math;

const MaxFragments            = 600; // The maximum of straight line segments allowed for drawing the spline
      MaxResults              = MaxFragments+10; // Max. number of calculated intersections
      MaxInterpolatedVertices = 250; // The maximum number of vertices that can be interpolated, up to 16000 allowed
      MaxCalcSteps            = 150; // Number of steps for numerical intersection calculating
      MaxError                = 1e-5;// Max error for intersection calculating
      MaxIterations           = 80;
      VerticesIncrement       = 25;  // Number of vertices to allocate memory for when the count property exceeds the current capacity

type TDataType              = single;
     TVertex              = record
                                 X,Y : TDataType;
                              end;
     // The following dynamic array is used to store the desired user-specified controlpoints
     T2DPointList           = array[1..1] of TVertex;
     P2DPointList           = ^T2DPointList;

     // The vertexlist is used internally to make the spline interpolate the controlpoints
     TVertexList          = array[0..0] of TVertex;
     P2DVertexList          = ^TVertexList;

     // The knuckle list stores a flag to see whether a point is a knuckle or not
     TKnuckleList           = array[1..1] of Boolean;
     PKnuckleList           = ^TKnuckleList;

     // T2DResults is a record with calculatedvalues at a specific point when for ex. the x-value is known
     T2DResults             = record
                                 NumberOfHits : integer;
                                 Parameter    : array[1..MaxResults] of TDataType;
                                 Point        : array[1..MaxResults] of TVertex;
     end;

     // 2D B-spline class:
     TBSpline             = class         // 2D B-Spline object
                                 private
                                    FColor         : TColor;
                                    FNoPoints      : Integer;
                                    FCapacity      : Integer;
                                    FPointList     : P2DPointList;
                                    FVertexList    : P2DVertexList;
                                    FKnuckleList   : PKnuckleList;
                                    FBuild         : Boolean;
                                    FNoVertices    : Integer;
                                    FInterpolated  : boolean;
                                    FMin,FMax      : TVertex;
                                    FFragments     : Integer;
                                    FShowVertices  : Boolean;
                                    procedure   FSetBuild(val:boolean);
                                    procedure   FSetCapacity(Val:Integer);                              virtual;
                                    procedure   FSetInterpolated(const value:boolean);
                                    procedure   FSetFragments(Const value:integer);
                                    function    FGetNumberOfPoints:integer;
                                    function    FGetPoint(Index:Integer):TVertex;                      virtual;
                                    procedure   FSetPoint(Index:Integer;Value:TVertex);                virtual;
                                    function    FGetVertex(Index:Integer):TVertex;                     virtual;
                                    procedure   FSetVertex(Index:Integer;Value:TVertex);               virtual;
                                    function    FGetKnuckle(Index:integer):Boolean;                     virtual;
                                    procedure   FSetKnuckle(Index:integer;Value:Boolean);               virtual;
                                    function    FGetNumberOfVertices:integer;
                                    procedure   FInterpolate;                                           virtual;
                                    procedure   FFillMatrix;                                            virtual;
                                    procedure   FPhantomPoints;
                                 public
                                    procedure   AddPoint(Vertex:TVertex);                              virtual;
                                    procedure   Extents(Var Min,Max : TVertex);                        virtual;
                                    procedure   Clear;                                                 virtual;
                                    constructor Create;
                                    destructor  Destroy;                                               override;
                                    procedure   Draw(Canvas:TCanvas);
                                    procedure   DeletePoint(Index:Integer);
                                    function    FirstDerive(Parameter:extended):TVertex;               virtual;
                                    procedure   InsertPoint(Index:Integer;Vertex:TVertex);
                                    procedure   Invert;// Inverse the controlpoints, eg the last point first and vice versa
                                    function    SecondDerive(Parameter:extended):TVertex;              virtual;// second derivative in a point
                                    function    KnownX(XValue:TDataType;var Results:T2DResults):Boolean;virtual;
                                    function    KnownY(YValue:TDataType;var Results:T2DResults):Boolean;virtual;
                                    function    Value(Parameter:extended):TVertex;                     virtual;
                                    procedure   Rebuild;                                                virtual;
                                    property    Build                  : Boolean read FBuild write FSetBuild;
                                    property    NumberOfPoints         : Integer read FGetNumberOfPoints;
                                    property    Capacity               : Integer read FCapacity write FSetCapacity;
                                    property    Fragments              : integer read FFragments write FSetFragments;
                                    property    Interpolated           : Boolean read FInterpolated write FSetInterpolated;
                                    property    NumberOfVertices       : Integer read FGetNumberOfVertices;
                                    property    Point [Index:Integer]  : TVertex read FGetPoint write FSetPoint;
                                    property    Vertex[Index:Integer]  : TVertex read FGetVertex write FSetVertex;
                                    property    Knuckle[Index:integer] : Boolean  read FGetKnuckle write FSetKnuckle;
                                    property    Min                    : TVertex read FMin;
                                    property    Max                    : TVertex read FMax;
                                    property    ShowVertices           : boolean read FShowVertices write FShowVertices;
                                    property    Color                  : TColor read FColor write FColor;
                              end;

implementation

// The following tpes are used for the interpolation routines
Type TMatrixRow      = Array[1..1] of TDataType;
     PMatrixRow      = ^TMatrixRow;
     TMatrix         = array[1..MaxInterpolatedVertices] of PMatrixRow;

var  Matrix          : TMatrix;


function DistPP2D(P1,P2:TVertex):TDataType;
begin
   Result:=Sqrt(Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y));
end;{DistPP2D}

{--------------------------------------------------------------------------------------------------}
{                                           TBSpline                                             }
{--------------------------------------------------------------------------------------------------}
procedure TBSpline.FSetBuild(val:boolean);
begin
   if not val then
   begin
      // Release allocated memory for vertices
      if (FVertexList<>nil) and (FBuild) then Freemem(FVertexList,(FNoVertices+2)*SizeOf(TVertex));
      FNoVertices:=0;
      FVertexList:=nil;
      // Clear extents
      FMin.X:=0;
      FMin.Y:=0;
      FMax.X:=1;
      FMax.Y:=1;
   end;
   FBuild:=Val;
end;{TBSpline.FSetBuild}

procedure TBSpline.FSetCapacity(Val:Integer);
var CurrentSize : Word;
    NewSize     : Word;
    OldPoints   : P2DPointList;
    OldKnuckle  : PKnuckleList;
begin
   if Val<>FCapacity then
   begin
      CurrentSize:=Capacity*SizeOf(TVertex);
      NewSize:=Val*SizeOf(TVertex);
      OldPoints:=FPointList;
      FPointList:=nil;
      OldKnuckle:=FKnuckleList;
      FKnuckleList:=Nil;
      if Val>0 then
      begin
         GetMem(FPointList,NewSize);
         GetMem(FKnuckleList,Val);
         FillChar(FKnuckleList^,Val,0);
         if Capacity<>0 then
         begin
            Move(OldKnuckle^,FKnuckleList^,Capacity);
            Move(OldPoints^,FPointList^,CurrentSize);
         end;
      end;
      if CurrentSize<>0 then
      begin
         Freemem(OldPoints,CurrentSize);
         Freemem(OldKnuckle,Capacity);
      end;
      FCapacity:=Val;
   end;
end;{TBSpline.FSetCapacity}

procedure TBSpline.FSetFragments(Const value:integer);
begin
   if FFragments<>value then
   begin
      FFragments:=value;
      if FFragments>MaxFragments then FFragments:=MaxFragments;
   end;
end;{TBSpline.FSetFragments}

procedure TBSpline.FSetInterpolated(const value:boolean);
begin
   if Value<>FInterpolated then
   begin
      FInterpolated:=value;
      Build:=false;
   end;
end;{TBSpline.FSetInterpolated}

function TBSpline.FGetNumberOfPoints:integer;
begin
   Result:=FNoPoints;
end;{TBSpline.FGetNumberOfPoints}

function TBSpline.FGetPoint(Index:Integer):TVertex;
begin
   if (Index>=1) and (Index<=FNoPoints) then Result:=FPointList^[Index] else
   begin
      Result.X:=0;
      Result.Y:=0;
      Result.Y:=0;
      Raise Exception.Create('List index out of bounds in '+Self.ClassName+'.FGetPoint. ('+IntToStr(Index)+').');
   end;
end;{TBSpline.FGetPoint}

procedure TBSpline.FSetPoint(Index:Integer;Value:TVertex);
begin
   if (Index>=1) and (Index<=FNoPoints) then
   begin
      FPointList^[Index]:=Value;
      Build:=False;
   end else Raise Exception.Create('List index out of bounds in '+Self.ClassName+'.FSetPoint. ('+IntToStr(Index)+').');
end;{TBSpline.FSetPoint}

function TBSpline.FGetVertex(Index:Integer):TVertex;
begin
   Result.X:=0;
   Result.Y:=0;
   if FBuild=False then if FNoPoints>1 then Rebuild
                                       else exit;
   if (Index>=0) and (Index<=NumberOfVertices+1) then Result:=FVertexList^[Index]
                                                 else Raise Exception.Create('List index out of bounds in '+Self.ClassName+'.FGetVertex. ('+IntToStr(Index)+').');
end;{TBSpline.FGetVertex}

procedure TBSpline.FSetVertex(Index:Integer;Value:TVertex);
begin
   if (Index>=0) and (Index<=NumberOfVertices+1) then FVertexList^[Index]:=Value
                                                 else Raise Exception.Create('List index out of bounds in '+Self.ClassName+'.FSetVertex. ('+IntToStr(Index)+').');
end;{TBSpline.FSetVertex}

function TBSpline.FGetKnuckle(Index:integer):Boolean;
begin
   if (Index=1) or (Index=FNoPoints) then Result:=false else
      if (Index>0) and (Index<=FNoPoints) then Result:=FKnuckleList^[Index]
                                           else Raise Exception.Create('List index out of bounds in '+Self.ClassName+'FGetKnuckle. ('+IntToStr(Index)+').');
end;{TBSpline.FGetKnuckle}

procedure TBSpline.FSetKnuckle(Index:integer;Value:Boolean);
begin
   if (Index>0) and (Index<=FNoPoints) then
   begin
      FKnuckleList^[Index]:=Value;
      Build:=false;
   end else Raise Exception.Create('List index out of bounds in '+Self.ClassName+'.FSetKnuckle. ('+IntToStr(Index)+').');
end;{TBSpline.FSetKnuckle}

function TBSpline.FGetNumberOfVertices:integer;
begin
   if Not FBuild then Rebuild;
   Result:=FNoVertices;
end;{TBSpline.FGetNumberOfVertices}

procedure TBSpline.Rebuild;
var I,J      : integer;
    Vertex2D : TVertex;
begin
   if FNoPoints>1 then
   begin
      if FVertexList<>nil then
      begin
         Freemem(FVertexList,(FNoVertices+2)*SizeOf(TVertex));
         FVertexList:=nil;
      end;
      FNoVertices:=0;
      For I:=1 to FNoPoints do if Knuckle[I] then inc(FNoVertices,3)
                                             else inc(FNoVertices,1);
      GetMem(FVertexList,(FNoVertices+2)*SizeOf(TVertex));
      J:=0;
      For I:=1 to FNoPoints do
      begin
         Vertex2D:=Point[I];
         if Knuckle[I] then
         begin
            FVertexList^[J+1]:=Vertex2D;
            FVertexList^[J+2]:=Vertex2D;
            Inc(J,2);
         end;
         FVertexList^[J+1]:=FPointList^[I];
         if I=1 then
         begin
            FMin:=Vertex2D;
            FMax:=FMin;
         end else
         begin
            if Vertex2D.X<FMin.X then FMin.X:=Vertex2D.X;
            if Vertex2D.Y<FMin.Y then FMin.Y:=Vertex2D.Y;
            if Vertex2D.X>FMax.X then FMax.X:=Vertex2D.X;
            if Vertex2D.Y>FMax.Y then FMax.Y:=Vertex2D.Y;
         end;

         Inc(J);
      end;
      if Interpolated then
      begin
         For I:=1 to FNoVertices do
         begin
            GetMem(Matrix[I],FNovertices*SizeOf(TDatatype));
            FillChar(Matrix[I]^,FNovertices*SizeOf(TDatatype),0);
         end;
         FFillMatrix;
         Finterpolate;
         For I:=1 to FNoVertices do
         begin
            FreeMem(Matrix[I],FNovertices*SizeOf(TDatatype));
            Matrix[I]:=nil;
         end;
      end;
   end;
   FBuild:=true;
   FPhantomPoints;
end;{TBSpline.Rebuild}

procedure TBSpline.FInterpolate;
var I,J,K      : integer;
    Factor     : extended;
    Tmp        : P2DVertexList;
begin
   if (FNoVertices<MaxInterpolatedVertices) and (FNoVertices>2) then
   begin
      GetMem(Tmp,(FNoVertices+2)*SizeOf(TVertex));
      for I:=1 to FNoVertices do
      begin
         for J:=I+1 to FNoVertices do
         begin
            factor:=Matrix[J]^[I]/Matrix[I]^[I];
            for K:=1 to FNoVertices do Matrix[J]^[K]:=Matrix[J]^[K]-factor*Matrix[I]^[K];
            FVertexList^[J].x:=FVertexList^[J].x-factor*FVertexList^[J-1].x;
            FVertexList^[J].y:=FVertexList^[J].y-factor*FVertexList^[J-1].y;
         end;
      end;
      Tmp^[FNoVertices].x:=FVertexList^[FNoVertices].x/Matrix[FNoVertices]^[FNoVertices];
      Tmp^[FNoVertices].y:=FVertexList^[FNoVertices].y/Matrix[FNoVertices]^[FNoVertices];
      for I:=FNoVertices-1 downto 1 do
      begin
         Tmp^[I].x:=(1/Matrix[I]^[I])*(FVertexList^[I].x-Matrix[I]^[I+1]*Tmp^[I+1].x);
         Tmp^[I].y:=(1/Matrix[I]^[I])*(FVertexList^[I].y-Matrix[I]^[I+1]*Tmp^[I+1].y);
      end;
      if FVertexList<>nil then
      begin
         Freemem(FVertexList,(FNoVertices+2)*SizeOf(TVertex));
         FVertexList:=nil;
      end;
      FVertexList:=Tmp;
   end;
end;{TBSpline.FInterpolate}

function TBSpline.KnownX(XValue:TDataType;var Results:T2DResults):Boolean;
var UpperLimit   : integer;
    Counter      : integer;
    Min,Max      : extended;
    Parameter    : extended;
    Error        : extended;
    Finished     : boolean;
    P1,P2        : TVertex;
    Output       : TVertex;
begin
   Result:=false;
   if not FBuild then
   begin
      Rebuild;
      if not FBuild then exit;
   end;
   Results.NumberOfHits:=0;
   if NumberOfPoints=0 then exit;
   if NumberOfPoints=2 then
   begin
      P1:=Point[1];
      P2:=Point[2];
      if P1.X>P2.X then
      begin
         Output:=P1;
         P1:=P2;
         P2:=Output;
      end;
      if (P1.X<=XValue) and (P2.X>=XValue) then
      begin
         if abs(P1.X-P2.X)<1e6 then Parameter:=0.5
                               else Parameter:=(XValue-P1.X)/(P2.X-P1.X);
         Results.NumberOfHits:=Results.NumberOfHits+1;
         Results.Parameter[Results.NumberOfHits]:=Parameter;
         Results.Point[Results.NumberOfHits]:=Value(Parameter);
         Results.Point[Results.NumberOfHits].X:=XValue;
      end;
   end else
   begin
      UpperLimit:=1;
      P1:=Point[1];
      repeat
         Finished:=false;
         repeat
            P2:=Value(UpperLimit/MaxCalcSteps);
            if ((P1.X<=XValue) and (P2.X>=XValue)) or
               ((P1.X>=XValue) and (P2.X<=XValue)) then Finished:=true;
            if not Finished then
            begin
               if UpperLimit=MaxCalcSteps then
               begin
                  Result:=Results.NumberOfHits>0;
                  exit;
               end else
               begin
                  inc(UpperLimit);
                  P1:=P2;
               end;
            end;
         until Finished;
         Max:=UpperLimit/MaxCalcSteps;
         Min:=(UpperLimit-1)/MaxCalcSteps;
         Counter:=0;
         repeat
            if abs(P1.X-P2.X)<1e-6 then Parameter:=0.5*(Min+Max) else
            if (P1.X<=XValue) and (P2.X>=XValue) then Parameter:=Min+(Max-Min)*((XValue-P1.X)/(P2.X-P1.X))
                                                 else Parameter:=Min+((P1.X-XValue)*(Max-Min)/(P1.X-P2.X));
            if Parameter<min then Parameter:=min;
            if Parameter>max then Parameter:=max;
            Output:=Value(parameter);
            if XValue=0 then Error:=abs(Output.X-XValue) else
                             Error:=abs((XValue-Output.X)/XValue);
            if Output.X>XValue then
            begin
               if (P1.X<=XValue) and (P2.X>=XValue) then
               begin
                  Max:=parameter;
                  P2:=Output;
               end else
               begin
                  min:=parameter;
                  P1:=Output;
               end;
            end else
            begin
               if (P1.X<=XValue) and (P2.X>=XValue) then
               begin
                  Min:=parameter;
                  P1:=Output;
               end else
               begin
                  Max:=parameter;
                  P2:=Output;
               end;
            end;
            inc(Counter);
         until (Error<MaxError) or (Counter>MaxIterations);
         if Results.NumberOfHits>=MaxResults then Raise Exception.Create('Max. number of results exceeded in TBSpline.KnownX');
         Results.NumberOfHits:=Results.NumberOfHits+1;
         Results.Parameter[Results.NumberOfHits]:=Parameter;
         Results.Point[Results.NumberOfHits]:=Value(Parameter);
         Results.Point[Results.NumberOfHits].X:=XValue;
         Max:=UpperLimit/MaxCalcSteps;
         P1:=Value(Max);
         Inc(UpperLimit);
      until UpperLimit>MaxCalcSteps;
   end;
   Result:=Results.NumberOfHits>0;
end;{TBSpline.KnownX}

function TBSpline.KnownY(YValue:TDataType;var Results:T2DResults):Boolean;
var UpperLimit   : integer;
    Counter      : integer;
    Min,Max      : extended;
    Parameter    : extended;
    Error        : extended;
    Finished     : boolean;
    P1,P2        : TVertex;
    Output       : TVertex;
begin
   Result:=false;
   if not FBuild then
   begin
      Rebuild;
      if not FBuild then exit;
   end;
   Results.NumberOfHits:=0;
   if NumberOfPoints=0 then exit;
   if NumberOfPoints=2 then
   begin
      P1:=Point[1];
      P2:=Point[2];
      if P1.Y>P2.Y then
      begin
         Output:=P1;
         P1:=P2;
         P2:=Output;
      end;
      if (P1.Y<=YValue) and (P2.Y>=YValue) then
      begin
         if abs(P1.Y-P2.Y)<1e6 then Parameter:=0.5
                               else Parameter:=(YValue-P1.Y)/(P2.Y-P1.Y);
         Results.NumberOfHits:=Results.NumberOfHits+1;
         Results.Parameter[Results.NumberOfHits]:=Parameter;
         Results.Point[Results.NumberOfHits]:=Value(Parameter);
         Results.Point[Results.NumberOfHits].Y:=YValue;
      end;
   end else
   begin
      UpperLimit:=1;
      P1:=Point[1];
      repeat
         Finished:=false;
         repeat
            P2:=Value(UpperLimit/MaxCalcSteps);
            if ((P1.Y<=YValue) and (P2.Y>=YValue)) or
               ((P1.Y>=YValue) and (P2.Y<=YValue)) then Finished:=true;
            if not Finished then
            begin
               if UpperLimit=MaxCalcSteps then
               begin
                  Result:=Results.NumberOfHits>0;
                  exit;
               end else
               begin
                  inc(UpperLimit);
                  P1:=P2;
               end;
            end;
         until Finished;
         Max:=UpperLimit/MaxCalcSteps;
         Min:=(UpperLimit-1)/MaxCalcSteps;
         Counter:=0;
         repeat
            if abs(P1.Y-P2.Y)<1e-6 then Parameter:=0.5*(Min+Max) else
            if (P1.Y<=YValue) and (P2.Y>=YValue) then Parameter:=Min+(Max-Min)*((YValue-P1.Y)/(P2.Y-P1.Y))
                                                 else Parameter:=Min+((P1.Y-YValue)*(Max-Min)/(P1.Y-P2.Y));
            if Parameter<min then Parameter:=min;
            if Parameter>max then Parameter:=max;
            Output:=Value(parameter);
            if YValue=0 then Error:=abs(Output.Y-YValue) else
                             Error:=abs((YValue-Output.Y)/YValue);
            if Output.Y>YValue then
            begin
               if (P1.Y<=YValue) and (P2.Y>=YValue) then
               begin
                  Max:=parameter;
                  P2:=Output;
               end else
               begin
                  min:=parameter;
                  P1:=Output;
               end;
            end else
            begin
               if (P1.Y<=YValue) and (P2.Y>=YValue) then
               begin
                  Min:=parameter;
                  P1:=Output;
               end else
               begin
                  Max:=parameter;
                  P2:=Output;
               end;
            end;
            inc(Counter);
         until (Error<MaxError) or (Counter>MaxIterations);
         if Results.NumberOfHits>=MaxResults then Raise Exception.Create('Max. number of results exceeded in TBSpline.KnownY');
         Results.NumberOfHits:=Results.NumberOfHits+1;
         Results.Parameter[Results.NumberOfHits]:=Parameter;
         Results.Point[Results.NumberOfHits]:=Value(Parameter);
         Results.Point[Results.NumberOfHits].Y:=YValue;
         Max:=UpperLimit/MaxCalcSteps;
         P1:=Value(Max);
         Inc(UpperLimit);
      until UpperLimit>MaxCalcSteps;
   end;
   Result:=Results.NumberOfHits>0;
end;{TBSpline.KnownY}

procedure TBSpline.AddPoint(Vertex:TVertex);
begin
   if NumberOfPoints=Capacity then Capacity:=Capacity+VerticesIncrement;
   inc(FNoPoints);
   Point[NumberOfPoints]:=Vertex;
   Build:=false;
end;{TBSpline.AddPoint}

constructor TBSpline.Create;
begin
   inherited Create;
   FPointList:=nil;
   FVertexList:=nil;
   FKnuckleList:=nil;
   FCapacity:=0;
   Clear;
end;{TBSpline.Create}

procedure TBSpline.DeletePoint(Index:Integer);
var I : integer;
begin
   if NumberOfPoints>0 then
   begin
      dec(FNoPoints);
      for I:=Index to NumberOfPoints do
      begin
         FPointList^[I]:=FPointList^[I+1];
         FKnuckleList^[I]:=FKnuckleList^[I+1];
      end;
      Build:=false;
   end;
   if NumberOfPoints=0 then Clear;
end;{TBSpline.DeletePoint}

destructor TBSpline.Destroy;
begin
   Clear;
   inherited Destroy;
end;{TBSpline.Destroy}

procedure TBSpline.Draw(Canvas:TCanvas);
var J     : Integer;
    ParameterValue : single;
    V       : TVertex;
begin
   //Canvas.Pen.Color:=Color;
   For J:=0 to Fragments do     {Draw the spline in 200 steps}
   begin
      ParameterValue:=(J/Fragments); // parameter value must be in the range 0.0-1.0
      V:=Value(ParameterValue);
      // Use used moveto/lineto method for demo-drawing.
      // using the Canvas.polyline method is SIGNIFICANTLY FASTER though!!
      if J=0 then Canvas.MoveTo(Round(V.X),Round(V.Y))
             else Canvas.LineTo(Round(V.X),Round(V.Y));
   end;
   if ShowVertices then For J:=1 to NumberOfPoints do     {Draw the vertices}
   begin
      V:=Point[J];
      Canvas.Pen.Color:=clRed;
      Canvas.Ellipse(Round(V.X)-2,Round(V.Y)-2,Round(V.X)+2,Round(V.Y)+2);
   end;
end;{TBSpline.Draw}

procedure TBSpline.Extents(Var Min,Max : TVertex);
var I : Integer;
    P : TVertex;
begin
   for I:=1 to NumberOfPoints do
   begin
      P:=Point[I];
      if P.X<Min.X then Min.X:=P.X;
      if P.X>Max.X then Max.X:=P.X;
      if P.Y<Min.Y then Min.Y:=P.Y;
      if P.Y>Max.Y then Max.Y:=P.Y;
   end;
end;{TBSpline.Extents}

function TBSpline.FirstDerive(Parameter:extended):TVertex;
var c,S,E : integer;
    Dist    : extended;
    Mix     : extended;
    Mid     : Extended;
begin
   Result.X:=0;
   Result.Y:=0;
   if FNoPoints<2 then exit;
   if not FBuild then Rebuild;
   Mid:=(NumberOfVertices-1)*Parameter+1;
   S:=Trunc(Mid-1);
   if S<0 then S:=0;
   E:=S+3;
   if S>FNovertices+1 then S:=FNovertices+1;
   for c:=S to E do
   begin
      dist:=C-Mid;
      if (dist>-2) and (dist<=-1) then Mix:=(2+dist)*(2+dist)*0.5 else
        if (dist>=-1) and (dist<=0) then Mix:=(-2*dist-1.5*dist*dist) else
          if (dist>=0) and (dist<=1) then Mix:=(-2*dist+1.5*dist*dist) else
            if (dist>=1) and (dist<2) then Mix:=-(2-dist)*(2-dist)*0.5 else
             mix:=0;
      Result.x:=Result.x+FVertexList^[c].x*mix;
      Result.y:=Result.y+FVertexList^[c].y*mix;
   end;
end;{TBSpline.FirstDerive}

procedure TBSpline.InsertPoint(Index:Integer;Vertex:TVertex);
var I : integer;
begin
   if (Index>=0) and (Index<=NumberOfPoints) then
   begin
      if NumberOfPoints=Capacity then Capacity:=Capacity+VerticesIncrement;
      inc(FNoPoints);
      for I:=NumberOfPoints downto Index+1 do
      begin
         FPointList^[I]:=FPointList^[I-1];
         FKnuckleList^[I]:=FKnuckleList^[I-1];
      end;
      FPointList^[Index]:=Vertex;
      FKnuckleList^[Index]:=False;
      Build:=false;
   end else raise Exception.Create('Index out of range');
end;{TBSpline.InsertPoint}

procedure TBSpline.Invert;
var OldPoints   : P2DPointList;
    OldVertices : P2DVertexList;
    OldKnuckle  : PKnuckleList;
    I           : Integer;
begin
exit;   // Backup current data
   OldPoints:=FPointList;
   FPointList:=nil;
   OldKnuckle:=FKnuckleList;
   FKnuckleList:=Nil;
   OldVertices:=FVertexList;
   FVertexList:=nil;
   // Prepare new arrays
   GetMem(FPointList,Capacity*SizeOf(TVertex));
   GetMem(FKnuckleList,Capacity);
   GetMem(FVertexList,(Capacity+2)*SizeOf(TVertex));
   // Initialize knuckle list
   FillChar(FKnuckleList^,Capacity,0);
   // Copy controlpoints
   for I:=1 to NumberOfPoints do
   begin
      FPointList^[I]:=OldPoints^[NumberOfPoints-I+1];
      FKnuckleList^[I]:=OldKnuckle^[NumberOfPoints-I+1];
   end;
   // Copy vertices
   if (OldVertices<>nil) and (FNoVertices<>0) and (Build) then for I:=0 to NumberOfVertices+1 do FVertexList^[I]:=OldVertices^[FNoVertices-I+1];
   // Destroy old arrays
   FreeMem(OldPoints,Capacity*SizeOf(TVertex));
   if (Oldvertices<>nil) and (FNoVertices<>0) and (build) then FreeMem(OldVertices,(Capacity+2)*SizeOf(TVertex));
   FreeMem(OldKnuckle,Capacity);
end;{TBSpline.Invert}

procedure TBSpline.Clear;
begin
   FColor:=clBlack;
   if (FVertexList<>nil) and (NumberOfVertices>0) then
   begin
      Freemem(FVertexList,(FNoVertices+2)*SizeOf(TVertex));
      FVertexList:=nil;
   end;
   FShowvertices:=False;
   FNoPoints:=0;
   FNoVertices:=0;
   Build:=False;
   Capacity:=0;
   FInterpolated:=false;
   FFragments:=100;
end;{TBSpline.Clear}

procedure TBSpline.FPhantomPoints;
var I : integer;
begin
   if NumberOfVertices>1 then
   begin
      I:=0;
      FVertexList^[I].X:=2*FVertexList^[I+1].X-FVertexList^[I+2].X;
      FVertexList^[I].Y:=2*FVertexList^[I+1].Y-FVertexList^[I+2].Y;
      FVertexList^[NumberOfVertices+1].X:=2*FVertexList^[NumberOfVertices].X-FVertexList^[NumberOfVertices-1].X;
      FVertexList^[NumberOfVertices+1].Y:=2*FVertexList^[NumberOfVertices].Y-FVertexList^[NumberOfVertices-1].Y;
   end;
end;{TBSpline.FPhantomPoints}

procedure TBSpline.FFillMatrix;
var I,J : integer;
begin
   if (FNoVertices>2) and (FNoVertices<=MaxInterpolatedVertices) then
   begin
      for I:=2 to FNoVertices-1 do
      begin
         Matrix[I]^[I-1]:=1/6;
         Matrix[I]^[I]:=2/3;
         Matrix[I]^[I+1]:=1/6;
      end;
      Matrix[1]^[1]:=1;
      Matrix[FNoVertices]^[FNoVertices]:=1;
      I:=3;
      while I<FNoVertices-1 do
      begin
         if (abs(FVertexList^[I].X-FVertexList^[I-1].X)<1e-5) and (abs(FVertexList^[I+1].X-FVertexList^[I].X)<1e-5) and
            (abs(FVertexList^[I].Y-FVertexList^[I-1].Y)<1e-5) and (abs(FVertexList^[I+1].Y-FVertexList^[I].Y)<1e-5) then
         begin
            for J:=I-1 to I+1 do
            begin
               Matrix[J]^[J-1]:=0;
               Matrix[J]^[J]:=1;
               Matrix[J]^[J+1]:=0;
            end;
            inc(I,2);
         end else inc(I);
      end;
   end;
end;{TBSpline.FFillMatrix}

function TBSpline.Value(Parameter:extended):TVertex;
var c,S,E   : integer;
    Dist    : extended;
    Mix     : extended;
    Mid     : TDataType;
begin
   Result.X:=0;
   Result.Y:=0;
   if FNoPoints<2 then exit;
   if not FBuild then Rebuild;
   Mid:=(NumberOfVertices-1)*Parameter+1;
   S:=Trunc(Mid-1);
   if S<0 then S:=0;
   E:=S+3;
   if S>FNovertices+1 then S:=FNovertices+1;
   for c:=S to E do
   begin
      dist:=abs(C-Mid);
      if dist<2 then
      begin
         if dist<1 then mix:=4/6-dist*dist+0.5*dist*dist*dist else
                        mix:=(2-dist)*(2-dist)*(2-dist)/6;
         Result.x:=Result.x+FVertexList^[c].x*mix;
         Result.y:=Result.y+FVertexList^[c].y*mix;
      end;
   end;
end;{TBSpline.Value}

function TBSpline.secondDerive(Parameter:extended):TVertex;
var c,S,E   : integer;
    Dist    : extended;
    Mix     : extended;
    Mid     : Extended;
begin
   Result.X:=0;
   Result.Y:=0;
   if FNoPoints<2 then exit;
   if not FBuild then Rebuild;
   Mid:=(NumberOfVertices-1)*Parameter+1;
   S:=Trunc(Mid-1);
   if S<0 then S:=0;
   E:=S+3;
   if S>FNovertices+1 then S:=FNovertices+1;
   for c:=S to E do
   begin
      dist:=C-Mid;
      if (dist>=-2) and (dist<=-1) then Mix:=2+dist else
         if (dist>=-1) and (dist<=0) then Mix:=-2-3*dist else
            if (dist>=0) and (dist<=1) then Mix:=-2+3*dist else
               if (dist>=1) and (dist<=2) then Mix:=2-dist else Mix:=0;
      Result.x:=Result.x+FVertexList^[c].x*mix;
      Result.y:=Result.y+FVertexList^[c].y*mix;
   end;
end;{TBSpline.secondDerive}

end.
