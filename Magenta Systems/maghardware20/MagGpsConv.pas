unit MagGpsConv;

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 14th January 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This unit includes functions to convert World Geodetic System WGS84 longitude
and latitude in degrees (as double) into the British Ordnance Survey National
Grid reference system of eastings and northings in metres, within a box covering
Great Britain, zeros being the south west corner.

If you are using WGS84 lat/long then you need only the two functions
declared in the interface section. If you want OSGB lat/long you'll need to
access the implementation ones.

OSGB does WGS84<>OSGB lat/long datum conversion, LatLongToGR is a
generalised projection function, and LatLongToNGR plugs in the constants for
the Ordnance Survey grid.

For WGS84LatLongToNGR and NGRToWGS84LatLong, Lat/long are in degrees, some
of the other functions pass their parameters in radians.

Angus - June 2014 - added WGS84LatLongToNGREx with validation
                    changed eastings and northings to integers
                    renamed so we can find it again

}

interface

function WGS84LatLongToNGR(Latitude: Double; Longitude: Double;
                          var Altitude, Eastings, Northings: integer): Boolean;
function WGS84LatLongToNGREx(Latitude, Longitude: Double;
                          var Altitude, Eastings, Northings: integer): Boolean;
function NGRToWGS84LatLong(const Eastings, Northings: integer;
                          var Latitude, Longitude: double; var Altitude: integer): Boolean;

implementation


function IntPower(const Number: Double; Power: Integer): Double; forward;
function OSGB(const Latitude: Double; const Longitude: Double;
     var DeltaLat: Double; var DeltaLong: Double; var DeltaAlt: Double): Boolean; forward;
function LatLongToNGR(const Latitude, Longitude: Double;
                      var Eastings, Northings: integer): Boolean; forward;
function LatLongToGR(const Latitude,Longitude: Double;
                     var Eastings, Northings: Double;
                     const LongBase: Double; const LatBase: Double;
                     const EastOffset: Double; const NorthOffset: Double;
                     const RA: Double; const RB: Double): Boolean; forward;
function Radians(const Deg: Double): Double; forward;

// Lat/long in degrees
function WGS84LatLongToNGR(Latitude: Double; Longitude: Double;
                        var Altitude, Eastings, Northings: integer): Boolean;
var
  DeltaLat:  Double;
  DeltaLong: Double;
  DeltaAlt:  Double;
begin
  // Convert WGS84 to OSGB lat-long
  if not OSGB(Latitude, Longitude, DeltaLat, DeltaLong, DeltaAlt) then begin
    Result := False;
    Exit;
  end;
  Latitude  := Latitude  - DeltaLat;
  Longitude := Longitude - DeltaLong;
  Altitude  := Altitude  - Integer (Trunc (DeltaAlt));

  // Convert OSGB lat-long to NGR
  if not LatLongToNGR(Radians(Latitude), Radians(Longitude), Eastings, Northings) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

// ensure location is within Great Britain

function WGS84LatLongToNGREx(Latitude, Longitude: Double;
                          var Altitude, Eastings, Northings: integer): Boolean;
begin
    Eastings := 0;
    Northings := 0;
    result := false ;
    if (Latitude < 49) or (Latitude > 61) then exit ;
    if (Longitude < -8) or (Longitude > 3) then exit ;     // should be -2, but try and cover Ireland
    result := WGS84LatLongToNGR(Latitude, Longitude,
                            Altitude, Eastings, Northings);
end;

function NGRToWGS84LatLong(const Eastings, Northings: integer;
          var Latitude, Longitude: Double; var Altitude: integer): Boolean;
var
  E:     integer;
  N:     integer;
  A:     integer;
  dx:    Double;
  dy:    Double;
  Count: Integer;
  t:     Integer;
begin
  Result := False;

  E := Eastings;
  N := Northings;

  Latitude  := 51.0;
  Longitude := -2.0;
  Count     := 0;
  for t := 1 to 100 do begin
    A := Altitude;
    if not WGS84LatLongToNGR(Latitude, Longitude, A, E, N) then begin
      Exit;
    end;
    dy := ((Northings - N) * 0.00001);
    dx := ((Eastings - E) * 0.00001);
    if abs(dy) < 0.00000000000000001 then begin
      Count := Count or 1;
    end
    else begin
      Latitude  := Latitude + dy;
    end;
    if abs(dx) < 0.00000000000000001 then begin
      Count := Count or 2;
    end
    else begin
      Longitude := Longitude + dx;
    end;
    if Count = 3 then begin
      Break;
    end;
  end;
  Altitude := Altitude + (Altitude - Integer (Trunc (A)));
  Result := True;
end;

// Lat/long in degrees
function OSGB(const Latitude: Double; const Longitude: Double;
      var DeltaLat: Double; var DeltaLong: Double; var DeltaAlt: Double): Boolean;
var
  U: Double;
  V: Double;
  K: Double;
begin
  try
    K := 0.15707963;
    U := K * (Latitude - 54);
    V := K * (Longitude - 0);

    DeltaLat := (
       0.86569
     - 2.84325 * U + 0.42841 * V
     - 0.38129 * IntPower(U, 2)
     + 0.10976 * IntPower(V, 2)
     + 0.94422 * IntPower(U, 3)
     + 0.48727 * IntPower(U, 2) * V
     + 2.60383 * IntPower(U, 3) * V
     + 0.45210 * IntPower(U, 2) * IntPower(V, 2)
     - 4.67120 * IntPower(U, 5) * V
     + 5.92805 * IntPower(U, 8) * V
     - 1.57065 * IntPower(U, 4) * IntPower(V, 5)
     + 9.93871 * IntPower(U, 9) * IntPower(V, 2)
     + 4.46776 * IntPower(U, 7) * IntPower(V, 9))
     / 3600;

     DeltaLong := (
     -  6.33331
     -  1.37857 * U
     -  3.08334 * V
     -  2.36779 * U * V
     -  0.31544 * IntPower(V, 2)
     +  0.16916 * IntPower(U, 2) * V
     -  3.30597 * U * IntPower(V, 2)
     -  0.89564 * IntPower(U, 2) * IntPower(V, 2)
     -  2.20892 * U * IntPower(V, 3)
     + 10.50912 * IntPower(U, 8) * IntPower(V, 2)
     - 14.7534  * IntPower(U, 9) * IntPower(V, 2))
     / 3600;

     DeltaAlt := (
       48.125
     +  6.154 * U
     -  3.952 * V
     - 13.733 * IntPower(U, 2)
     +  7.185 * U * V
     + 18.661 * IntPower(U, 4)
     - 12.149 * IntPower(U, 5)
     - 25.508 * IntPower(U, 3) * IntPower(V, 3)
     - 60.708 * IntPower(U, 7) * IntPower(V, 6));
  except
//    ErrorMsg('Invalid FP operation in OSGB');
    Result := False;
    Exit;
  end;
  Result := True;
end;

function IntPower(const Number: Double; Power: Integer): Double;
var
  Value: Double;
begin
  Value := Number;
  while Power > 1 do begin
    Value := Value * Number;
    Power := Power - 1;
  end;
  Result := Value;
end;

// Lat/long in Radians
function LatLongToNGR(const Latitude, Longitude: Double;
                              var Eastings, Northings: integer): Boolean;
var
    XEastings, XNorthings: Double ;
begin
  Result := LatLongToGR(Latitude, Longitude, XEastings, XNorthings,
  -0.03490659, // -2 deg
  0.85521133,  // 49 deg
  400000, -100000, 6375020.481, 6353722.49);
  if Result then  // Angus
  begin
      Eastings := Integer (Trunc (XEastings));
      Northings := Integer (Trunc (XNorthings));
  end;
end;

// Lat/long in Radians
function LatLongToGR(const Latitude: Double; const Longitude: Double;
       var Eastings: Double; var Northings: Double;
       const LongBase: Double; const LatBase: Double;
       const EastOffset: Double;   // Of false origin from true, in metres
       const NorthOffset: Double;  // Of false origin from true, in metres
       const RA: Double;           // Major semi-axis (radius of earth at poles)
       const RB: Double): Boolean; // Minor semi-axis (radius of earth at equator)
var
  R2: Double;
  D:  Double;
  G3: Double;
  G4: Double;
  A:  Double;
  S:  Double;
  S2: Double;
  C:  Double;
  C2: Double;
  T:  Double;
  T2: Double;
  T4: Double;
  R:  Double;
  V:  Double;
  U:  Double;
  W:  Double;
  P:  Double;
  P2: Double;
  X:  Double;
  Z:  Double;
begin
  try
    R2 := (RA * RA - RB * RB) / (RA * RA);
    D  := (RA - RB) / (RA + RB);

    G3 := Latitude - LatBase;
    G4 := Latitude + LatBase;

    A  := RB * (1 + D) * (G3 - D * (3 * sin(G3) * cos(G4) - D *
          (1.875 * sin(2 * G3) * cos(2 * G4) - D * 1.46 *
          sin(3 * G3) * cos(3 * G4)))) + NorthOffset;

    S  := sin(Latitude);
    S2 := S * S;
    C  := cos(Latitude);
    C2 := C * C;

    T  := S / C;
    T2 := T * T;
    T4 := T2 * T2;

    R  := 1.0 - R2 * S2;
    V  := RA / sqrt(R);
    U  := R / (1 - R2);
    W  := U - 1;

    P  := Longitude - LongBase;
    P2 := P * P;
    X  := P * V * C;
    Z  := P2 * C2;

    Eastings := EastOffset + X * (1 + Z / 6 * (U - T2 + Z / 20 * (5 - 18 * T2 + T4 + 14 * W - 58 * T2 + W)));

    X := X * S * P;

    Northings := A + X * (0.5 + Z / 24 * (5 - T2 + 9 * W + Z / 30 * (61 - 58 * T2 + T4)));
  except
//    ErrorMsg('Invalid FP operation in LatLongToGR');
    Result := False;
    Exit;
  end;
  Result := True;
end;

function Radians(const Deg: Double): Double;
begin
  Result := Deg / (180 / pi);
end;

end.
