// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  20271: mcmTrace.pas
//
//    Rev 1.8    2014-04-06 13:06:58  mcm
//
//    Rev 1.7    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.6    20-08-2007 21:02:34  mcm
// Added support for Delphi 2007
//
//    Rev 1.5    05-06-2006 22:45:24  mcm    Version: IMG 3.0
// Renamed Resize to ResizeData on TmcmCustomPolygon.
//
//    Rev 1.4    21-05-2006 12:42:00  mcm
// Clean-up
//
//    Rev 1.3    09-05-2006 21:54:12  mcm
//
//    Rev 1.2    01-05-2006 17:45:12  mcm
// Added TmcmTraceObject, Hit test, Calculation of shapes.
// Improved trace method.
//
//   Rev 1.1    13-02-2005 19:57:20  mcm

//
//   Rev 1.0    12-05-2003 15:35:18  mcm

unit mcmTrace;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, Classes, Controls, Forms, Graphics,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Graphics, Vcl.Forms,
      Vcl.Controls,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmImageKernel,
     mcmGraphics;

const
  HitDistance : single = 1.0; // Distance from line that constitute as a hit on
                              // the polygon.

type
  TmcmTraceDir  = (TCD_RIGHT, TCD_HORIZ, TCD_ALLDIR); // Search direction for initial point.
  TmcmTraceType = (TCT_4CONNECT, TCT_8CONNECT);       // Not used.

  TmcmTracePolygon = class(TmcmPolygon);

  TmcmTraceList = class(TList)
  public
     function   AddTrace : TmcmPolygon;
     procedure  Clear; {$IFNDEF DCB3} override; {$ENDIF DCB3}
     procedure  Delete(Index : integer);
  end;


  TmcmTrace = class(TmcmImageKernel)
  private
    // Private declarations
    FBuffer      : PMatrixB; // Pointer to lines in the image.
    FMemSize     : longint;  // Initial memory size of TmcmPolygon.
    FMinPoints   : integer;
    FOnlyClose   : boolean;  // Return only closed polygon objects.
    FTraceObj    : TmcmTracePolygon;
    FTraceDir    : TmcmTraceDir;
    FTraceOnEdge : boolean;

    FTraceList   : TmcmTraceList;
  protected
    // Protected declarations
    procedure   SetSourceImage(Index : word; Image : TmcmImage); override;
    function    GetTraceObject(Index : integer) : TmcmPolygon;
    procedure   SetMinimumPoints(Value : integer);
    procedure   SetTraceDir(Value : TmcmTraceDir);
    procedure   SetTraceObject(Index : integer; Value : TmcmPolygon);
    procedure   SetTraceOnEdge(Value : boolean);

    procedure   StartChainCode(var Index : integer; var sx, sy : longint; TraceValue : byte);
    procedure   NextChainCode(var Index : integer; var nx, ny : longint; TraceValue : byte);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; override;

    procedure   ChainCode(var x, y : longint; TraceValue : byte);
    procedure   ChainCodeAll(TraceValue : byte);
    procedure   ChainCodeRemove(var x, y : longint; TraceValue : byte);
    function    Count : longint;
    function    ReleaseTraceObject(Index : integer) : TmcmPolygon;
    procedure   ShowOn(Form : TWinControl);

    property    MinimumPoints : integer
      read      FMinPoints
      write     SetMinimumPoints default 2;
    property    TraceObject[Index : integer] : TmcmPolygon
      read      GetTraceObject;
      //write     SetTraceObject;
  published
    // Published declarations
    property    OnlyClose : boolean
      read      FOnlyClose
      write     FOnlyClose default False;
    property    TraceDir : TmcmTraceDir
      read      FTraceDir
      write     SetTraceDir default TCD_HORIZ;
    property    TraceOnEdge : boolean
      read      FTraceOnEdge
      write     SetTraceOnEdge default True;
  end;


implementation

uses {$IFNDEF GE_DXE2}
      SysUtils;
     {$ELSE}
      System.SysUtils;
     {$ENDIF}

// Direction arrays used when tracing the next point.
const dx : array[0..8] of longint = (-1, -1,  0,  1, 1, 1, 0, -1, -1);
      dy : array[0..8] of longint = ( 0, -1, -1, -1, 0, 1, 1,  1,  0);
//    Direction LIndex            = ( 0,  1,  2,  3, 4, 5, 6,  7,  8);

type
  // This class is added to access the hidden DibInfo property on TmcmImage.
  TmcmTraceImage = class(TmcmImage)
  public
    property DibInfo;
  end;

//------------------------------------------------------------------------------
// TmcmTraceList.
//------------------------------------------------------------------------------

function TmcmTraceList.AddTrace : TmcmPolygon;
var NewTrace : TmcmPolygon;
begin
  NewTrace := TmcmPolygon.Create(Nil);
  Add(NewTrace);
  Result := NewTrace;
end; // TmcmTraceList.AddTrace.


procedure TmcmTraceList.Clear;
var i : integer;
begin
  {$IFNDEF DCB3} Inherited Clear; {$ENDIF DCB3}
  for i := Count - 1 downto 0
  do begin
     TmcmPolygon(Self.Items[i]).Free;
     Self.Items[i] := Nil;
  end;
  Inherited Clear;
end; // TmcmTraceList.Clear.


procedure TmcmTraceList.Delete(Index : integer);
begin
  TmcmPolygon(Items[Index]).Free;
  Items[Index] := Nil;
  Inherited Delete(Index);
end; // TmcmTraceList.Delete.


//------------------------------------------------------------------------------
// TmcmTrace.
//------------------------------------------------------------------------------

constructor TmcmTrace.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FBuffer   := Nil;
  FTraceObj := Nil;
  FTraceList := TmcmTraceList.Create;
  Clear;

  FOnlyClose   := False;
  FTraceDir    := TCD_HORIZ;
  FTraceOnEdge := True;
  FMinPoints   := 2;
end; // TmcmTrace.Create.


destructor TmcmTrace.Destroy;
begin
  Clear;
  FTraceList.Free;
  FTraceList := Nil;
  Inherited Destroy;
end; // TmcmTrace.Destroy.


procedure TmcmTrace.Clear;
begin
  Inherited Clear;
  if Assigned(FBuffer)
  then FreeMem(FBuffer);
  FBuffer := Nil;
  FMemSize := 1024;
  FTraceList.Clear;
end; // TmcmTrace.Clear.


procedure TmcmTrace.ShowOn(Form : TWinControl);
var i : integer;
begin
  // Changes all TmcmPolygon parent to the "Form" parameter.
  if (Form is TWinControl) and Assigned(FTraceList)
  then begin
       for i := 0 to (FTraceList.Count - 1)
       do begin
          TmcmPolygon(FTraceList.Items[i]).BoundingRect;
          TmcmPolygon(FTraceList.Items[i]).Parent := Form; // TCustomForm(Self.Owner);
       end;
  end;

end; // TmcmTrace.ShowOn.


function TmcmTrace.ReleaseTraceObject(Index : integer) : TmcmPolygon;
begin
  if (Index < FTraceList.Count)
  then begin
       Result := TmcmPolygon(FTraceList.Items[Index]);
       FTraceList.Items[Index] := Nil;
  end
  else Result := Nil;
end; // TmcmTrace.ReleaseTraceObject.


function TmcmTrace.GetTraceObject(Index : integer) : TmcmPolygon;
begin
  if (Index < FTraceList.Count)
  then Result := FTraceList.Get(Index)
  else Result := Nil;
end; // TmcmTrace.GetTraceObject.


procedure TmcmTrace.SetTraceObject(Index : integer; Value : TmcmPolygon);
begin // Adding objects is not a good idea!
end; // TmcmTrace.SetTraceObject.


procedure TmcmTrace.SetSourceImage(Index : word; Image : TmcmImage);
begin
  Inherited SetSourceImage(Index, Image);
  if Assigned(FBuffer)
  then FreeMem(FBuffer);
  FBuffer := Nil;
  GetMem(FBuffer, FSrcHeight * SizeOf(PVectorB));
  FillChar(FBuffer^, FSrcHeight * SizeOf(PVectorB), 0);
end; // TmcmTrace.SetSourceImage.


procedure TmcmTrace.SetMinimumPoints(Value : integer);
begin
  FMinPoints := Value;
end; // TmcmTrace.SetMinimumPoints.


procedure TmcmTrace.SetTraceDir(Value : TmcmTraceDir);
begin
  FTraceDir := Value;
end; // TmcmTrace.SetTraceDir.


procedure TmcmTrace.SetTraceOnEdge(Value : boolean);
begin
  FTraceOnEdge := Value;
end; // TmcmTrace.SetTraceOnEdge.


function TmcmTrace.Count : longint;
begin
  if Assigned(FTraceList)
  then Result := FTraceList.Count
  else Result := 0;
end; // TmcmTrace.Count.

//------------------------------------------------------------------------------
// Chain Code routines.
//------------------------------------------------------------------------------

procedure TmcmTrace.StartChainCode(var Index : integer; var sx, sy : longint; TraceValue : byte);
var i, j    : longint;
    pBuffer : PVectorB;
begin
  // Locate the starting point in Horizontal direction ONLY.
  if (FBuffer^[sy] = Nil)
  then FBuffer^[sy] := FSrcImage[0].ScanLine[sy];
  pBuffer := FBuffer^[sy];

  if (pBuffer^[sx] <> TraceValue)
  then begin
       // Search in right direction.
       i := 0;
       while (pBuffer^[sx+i] <> TraceValue) and
             (sx + i < FSrcWidth - 1)
       do inc(i);
       if (pBuffer^[sx+i] <> TraceValue)
       then i := 65536;

       // Search in left direction.
       if (FTraceDir <> TCD_RIGHT)
       then begin
            j := 0;
            while (pBuffer^[sx-j] <> TraceValue) and (sx - j > 1)
            do inc(j);
            if (pBuffer^[sx-j] <> TraceValue)
            then j := 65536;
       end
       else j := 65536;

       if (i < 65536) and (j < 65536)
       then begin
            if (pBuffer^[sx+i] = TraceValue) or
               (pBuffer^[sx-j] = TraceValue)
            then begin
                 if (i <= j)
                 then begin
                      sx := sx + i;
                      Index := 4;
                 end
                 else begin
                      sx := sx - j;
                      Index := 0;
                 end;
            end
            else sx := -1;
       end
       else begin
            if (i = 65536) and (j = 65536)
            then sx := -1
            else begin
                 if (i < 65536)
                 then begin
                      Index := 4;
                      if (pBuffer^[sx+i] = TraceValue)
                      then sx := sx + i
                      else sx := -1;
                 end;
                 if (j < 65536)
                 then begin
                      Index := 0;
                      if (pBuffer^[sx-j] = TraceValue)
                      then sx := sx - j
                      else sx := -1;
                 end;
            end;
       end;
  end
  else begin
       // Search in right direction.
       i := 0;
       while (pBuffer^[sx+i] = TraceValue) and
             (sx + i < FSrcWidth - 1)
       do inc(i);
       if (pBuffer^[sx+i] = TraceValue)
       then i := 65536
       else if (pBuffer^[sx+i] <> TraceValue)
            then dec(i);

       // Search in left direction.
       if (FTraceDir <> TCD_RIGHT)
       then begin
            j := 0;
            while (pBuffer^[sx-j] = TraceValue) and
                  (sx - j > 1)
            do inc(j);
            if (pBuffer^[sx-j] = TraceValue)
            then j := 65536
            else if (pBuffer^[sx-j] <> TraceValue)
                 then dec(j);
       end
       else j := 65536;

       if (i <= j)
       then begin
            if (sx + i < FSrcWidth - 1)
            then begin
                 sx := sx + i;
                 Index := 0;
            end
            else sx := -1;
       end
       else begin
            if (sx - j > 0)
            then begin
                 sx := sx - j;
                 Index := 4;
            end
            else sx := -1;
       end;
  end;
end; // TmcmTrace.StartChainCode.


procedure TmcmTrace.NextChainCode(var Index : integer; var nx, ny : longint; TraceValue : byte);
var   i, j     : longint;
      Int      : integer;
      MaxIndex : integer;
      DoSearch : boolean;
begin
  for i := (ny - 1) to (ny + 1)
  do if (FBuffer^[i] = Nil) and (i >= 0)
     then FBuffer^[i] := FSrcImage[0].ScanLine[i];

  i := (Index + 1) mod 8;
  MaxIndex := Index + 7;
  DoSearch := True;

  if (0 < nx) and (nx < FSrcWidth - 1) and
     (0 < ny) and (ny < FSrcHeight - 1)
  then begin
       // The obejct is not touching the edge.
       while DoSearch and (i <= MaxIndex)
       do begin
          j := (i + 4) mod 8;
          Int := FBuffer^[ny+dy[j]]^[nx+dx[j]];
          if (Int = TraceValue)
          then begin
               Index := j;
               DoSearch := False;
          end;
          inc(i);
       end;
  end
  else begin
       // The object is touching the edge.
       if FTraceOnEdge
       then begin
            while DoSearch and (i <= MaxIndex)
            do begin
               j := (i + 4) mod 8;
               if (0 <= nx + dx[j]) and (nx + dx[j] < FSrcWidth) and
                  (0 <= ny + dy[j]) and (ny + dy[j] < FSrcHeight)
               then begin
                    Int := FBuffer^[ny+dy[j]]^[nx+dx[j]];
                    if (Int = TraceValue)
                    then begin
                         Index := j;
                         DoSearch := False;
                    end;
               end;
               inc(i);
            end;
       end;
  end;

  if DoSearch
  then begin
       nx := -1;
       ny := -1;
  end
  else begin
       nx := nx + dx[Index];
       ny := ny + dy[Index];
  end;
end; // TmcmTrace.NextChainCode.


procedure TmcmTrace.ChainCode(var x, y : longint; TraceValue : byte);
var sx, sy : longint;
    nx, ny : longint;
    ox, oy : longint;
    LIndex : integer;
begin
  // This method traces the grey level value TraceValue
  FError := EC_OK;
  FTraceObj := Nil;

  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8])
  then begin
       if (y < 0)
       then y := 0;
       if (y > FSrcHeight - 1)
       then y := FSrcHeight - 1;

       // Locate first point.
       sx := x;
       sy := y;
       LIndex  := 0;
       StartChainCode(LIndex, sx, sy, TraceValue);

       if (sx >= 0) // Did we find a point, sx = -1 if not.
       then begin
            nx := sx;
            ny := sy;
            ox := -16384;
            oy := -16384;

            FTraceObj := TmcmTracePolygon(FTraceList.AddTrace);
            //FTraceObj.FTraceType := TCT_8CONNECT;
            if (FTraceObj.ResizeData(FMemSize) = 0)
            then begin
                 ox := sx;
                 oy := sy;
            end;

            FTraceObj.FTrace^[0] := mcmPoint(sx, sy);
            inc(FTraceObj.FCount);

            while ((sx <> ox) or (sy <> oy)) and
                  ((0 <= nx) and (nx <= FSrcWidth - 1) and
                   (0 <= ny) and (ny <= FSrcHeight - 1))
            do begin
               // Locate next point.
               NextChainCode(LIndex, nx, ny, TraceValue);
               ox := nx;
               oy := ny;
               if (nx >= 0)
               then begin
                    FTraceObj.FTrace^[FTraceObj.FCount] := mcmPoint(nx, ny);
                    inc(FTraceObj.FCount);
                    if (FTraceObj.FCount >= FTraceObj.FMemSize)
                    then if (FTraceObj.ResizeData(-1) = 0)
                         then begin
                              ox := sx;
                              oy := sy;
                         end;
               end;
            end;

            if FOnlyClose
            then begin
                 if Not(FTraceObj.IsClosed)
                 then FTraceObj.Clear;
            end;

            if (FTraceObj.FCount >= FMinPoints)
            then begin // Resize memory allocation.
                 FTraceObj.ResizeData(FTraceObj.FCount);
                 FTraceObj.Image := FSrcImage[1];
            end
            else begin
                 // Object length/perimeter is too small, we'll delete it.
                 FTraceList.Delete(FTraceList.IndexOf(FTraceObj));
                 FTraceObj := Nil;
            end;
       end;
       x := sx;
       y := sy;
  end;
end; // TmcmTrace.ChainCode.


procedure TmcmTrace.ChainCodeRemove(var x, y : longint; TraceValue : byte);
var sx, sy : longint;
    nx, ny : longint;
    ox, oy : longint;
    LIndex : integer;
    Swaped : integer;
begin
  // This method changes the image values, to ensure that no point is re-visited.
  // It is assumed that the image contains the outline of each obejct to be traced.
  FError := EC_OK;
  FTraceObj := Nil;

  // Make sure we have a valid Result Image.
  if CheckSource(0, [IF_GREY8])
  then begin
       if (y < 0)
       then y := 0;
       if (y > FSrcHeight - 1)
       then y := FSrcHeight - 1;

       sx := x;
       sy := y;
       LIndex  := 0;
       StartChainCode(LIndex, sx, sy, TraceValue);

       if (sx >= 0) // Did we find a point, sx = -1 if not.
       then begin
            nx := sx;
            ny := sy;
            ox := -16384;
            oy := -16384;

            FTraceObj := TmcmTracePolygon(FTraceList.AddTrace);
            // FTraceObj.FTraceType := TCT_8CONNECT;
            if (FTraceObj.ResizeData(FMemSize) = 0)
            then begin
                 ox := sx;
                 oy := sy;
            end;

            FTraceObj.FTrace^[0] := mcmPoint(sx, sy);
            inc(FTraceObj.FCount);
            FBuffer^[sy]^[sx] := 254; // Start point.

            Swaped := 0;
            while (Swaped < 2)
            do begin
               inc(Swaped);
               while ((sx <> ox) or (sy <> oy)) and
                     ((0 <= nx) and (nx <= FSrcWidth - 1) and
                      (0 <= ny) and (ny <= FSrcHeight - 1))
               do begin
                  // Locate next point.
                  NextChainCode(LIndex, nx, ny, TraceValue);
                  ox := nx;
                  oy := ny;
                  if (nx >= 0)
                  then begin
                       FBuffer^[ny]^[nx] := 127; // Path point.
                       FTraceObj.FTrace^[FTraceObj.FCount] := mcmPoint(nx, ny);
                       inc(FTraceObj.FCount);
                       if (FTraceObj.FCount >= FTraceObj.FMemSize)
                       then if (FTraceObj.ResizeData(-1) = 0)
                            then begin
                                 ox := sx;
                                 oy := sy;
                            end;
                  end;
               end;

               // If object isn't closed, swap traced points and re-try a trace
               // from the start point.
               if (Swaped = 1) and (FTraceObj.FCount > 1) and
                  ((FTraceObj.FTrace^[0].x <> FTraceObj.FTrace^[FTraceObj.FCount-1].x) or
                   (FTraceObj.FTrace^[0].y <> FTraceObj.FTrace^[FTraceObj.FCount-1].y))
               then begin
                    FTraceObj.SwapTraceData;
                    nx := sx;
                    ny := sy;

                    // Determine the direction after reversing the polygon order.
                    // This is used to trace the next point.
                    LIndex := FTraceObj.FCount;
                    ox := FTraceObj.FTrace^[LIndex-1].x - FTraceObj.FTrace^[LIndex-2].x;
                    oy := FTraceObj.FTrace^[LIndex-1].y - FTraceObj.FTrace^[LIndex-2].y;
                    case ox of
                    -1 : case oy of
                         -1 : LIndex := 1;
                          0 : LIndex := 0;
                          1 : LIndex := 7;
                         end;
                     0 : case oy of
                         -1 : LIndex := 2;
                          //0 : NOT Possible;
                          1 : LIndex := 6;
                         end;
                     1 : case oy of
                         -1 : LIndex := 3;
                          0 : LIndex := 4;
                          1 : LIndex := 5;
                         end;
                    end;
                    ox := -16384;
                    oy := -16384;
               end
               else inc(Swaped);

               if (FTraceObj.FCount > 1) and (Swaped > 1)
               then FBuffer^[sy]^[sx] := 192; // End Point.
            end;

            if FOnlyClose
            then begin
                 if Not(FTraceObj.IsClosed)
                 then FTraceObj.Clear;
            end;

            if (FTraceObj.FCount >= FMinPoints)
            then begin // Resize memory allocation.
                 FTraceObj.ResizeData(FTraceObj.FCount);
                 FTraceObj.Image := FSrcImage[1];
            end
            else begin
                 // Object length/perimeter is too small, we'll delete it.
                 FTraceList.Delete(FTraceList.IndexOf(FTraceObj));
                 FTraceObj := Nil;
            end;
       end;
       x := sx;
       y := sy;
  end;
end; // TmcmTrace.ChainCodeRemove.


procedure TmcmTrace.ChainCodeAll(TraceValue : byte);
var x, y : longint;
begin
  // ChainCodeAll finds all "outlined" objects in the 8-bit BW image.
  // Note, all white (255) values are modified.
  FError := EC_OK;
  y := 0;
  FTraceDir := TCD_RIGHT;
  while (y < FSrcHeight) and (FError = EC_OK)
  do begin
     x := 0;
     while (0 <= x) and (x < FSrcWidth)
     do begin
        ChainCodeRemove(x, y, TraceValue);
        if (x >= 0)
        then inc(x);
     end;
     inc(y);
  end;
end; // TmcmTrace.ChainCodeAll.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
