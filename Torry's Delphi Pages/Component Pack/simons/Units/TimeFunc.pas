unit TimeFunc;

{ Modifizierte Version 2.00

  Die Routinen aus dieser Unit stammen aus der TMoon-Komponente
  von Andreas Hörstemeier : http://www.hoerstemeier.com.

  Die Unit wurde von Simon Reinhardt für die Komponente TSRCalendar
  modifiziert. Vielen Dank an Thomas Stahmer <TStahmer@pdv-online.de>
  für die Korrektur der Jahreszeit-Berechnungen. }

{$I SRDefine.inc}

interface

uses SRCal, SysUtils;

type
  E_NoRiseSet=class(Exception);
  E_OutOfAlgorithRange=class(Exception);

function Julian_Date(ADate:TDateTime):extended;

function sun_distance(date:TDateTime): extended;
function moon_distance(date:TDateTime): extended;
function age_of_moon(date:TDateTime): extended;

function last_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
function next_phase(date:TDateTime; phase:TMoonPhase):TDateTime;

function current_phase(date:TDateTime):extended;
function lunation(date:TDateTime):integer;

function sun_diameter(date:TDateTime):extended;
function moon_diameter(date:TDateTime):extended;

function star_time(date:TDateTime):extended;
function StartSeason(year: integer; season:TSeason):TDateTime;

function Sun_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
function Sun_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
function Sun_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
function Moon_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;

function nextperigee(date:TDateTime):TDateTime;
function nextapogee(date:TDateTime):TDateTime;

function NextEclipse(var date:TDateTime; sun:boolean):TEclipse;

implementation

uses MyMath {$IFDEF SR_Delphi3_Up}, Windows{$ENDIF};

const
  AU=149597869;             (* astronomical unit in km *)
  mean_lunation=29.530589;  (* Mean length of a month *)
  earth_radius=6378.15;     (* Radius of the earth *)

type
t_coord = record
  longitude, latitude, radius: extended;
  rektaszension, declination: extended;
  parallax: extended;
  end;
  T_RiseSet=(_rise,_set,_transit);

{$IFDEF SR_Delphi2_Up}
function TimeZoneBias:longint;
var
  tz_info: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(tz_info) of
    1: result:=tz_info.StandardBias+tz_info.Bias;
    2: result:=tz_info.DaylightBias+tz_info.Bias;
    else result:=0;
  end;
end;

function TransformDate(aDatum:TDatetime): TDateTime;
var
  bias, jetzt,
  start_time,
  first_now   : TDateTime;
begin
  first_now:=now;

  bias:=TimeZoneBias/(60*24);
  start_time:=aDatum-(now-first_now)-bias;

  jetzt:=(now-first_now)+start_time;
  result:=jetzt;
end;
{$ENDIF}

function put_in_360(x:extended):extended;
begin
  result:=x-round(x/360)*360;
  while result<0 do result:=result+360;
  end;

{ Angular functions with degrees }
function sin_d(x:extended):extended;
begin
  sin_d:=sin(put_in_360(x)*pi/180);
end;

function cos_d(x:extended):extended;
begin
  cos_d:=cos(put_in_360(x)*pi/180);
end;

function tan_d(x:extended):extended;
begin
  tan_d:=tan(put_in_360(x)*pi/180);
end;

function arctan2_d(a,b:extended):extended;
begin
  result:=arctan2(a,b)*180/pi;
end;

function arcsin_d(x:extended):extended;
begin
  result:=arcsin(x)*180/pi;
end;

function arccos_d(x:extended):extended;
begin
  result:=arccos(x)*180/pi;
end;

function arctan_d(x:extended):extended;
begin
  result:=arctan(x)*180/pi
end;

{ Julian date }
function Julian_Date(ADate:TDateTime):extended;
var FirstJulian,
    FirstOf2k : TDateTime;
begin
  FirstJulian:=EncodeDate(1582, 10, 15);
  if ADate>=FirstJulian then begin
    FirstOf2k:=EncodeDate(2000, 1, 1);
    Result:=2451544.5-FirstOf2k+ADate;
  end
  else
    Result:=0;   { not yet implemented !!! }
end;

function Delphi_Date(JulDat:extended):TDateTime;
var FirstJulian,
    FirstOf2k : TDateTime;
begin
  FirstJulian:=EncodeDate(1582, 10, 15);
  if JulDat>=Julian_Date(FirstJulian) then begin
    FirstOf2k:=EncodeDate(2000, 1, 1);
    Result:=JulDat-2451544.5+FirstOf2k;
  end
  else
    Result:=0;    { not yet implemented !!! }
end;

function star_time(date:TDateTime):extended;
var
  jd, t: extended;
begin
  jd:=julian_date(date);
  t:=(jd-2451545.0)/36525;
  result:=put_in_360(280.46061837+360.98564736629*(jd-2451545.0)+
                     t*t*(0.000387933-t/38710000));
end;

{ Coordinate functions }

{ Based upon Chapter 12 and 21 of Meeus }
procedure calc_geocentric(var coord:t_coord; date:TDateTime);
const
arg_mul:array[0..30,0..4] of shortint =
  (( 0, 0, 0, 0, 1),
   (-2, 0, 0, 2, 2),
   ( 0, 0, 0, 2, 2),
   ( 0, 0, 0, 0, 2),
   ( 0, 1, 0, 0, 0),
   ( 0, 0, 1, 0, 0),
   (-2, 1, 0, 2, 2),
   ( 0, 0, 0, 2, 1),
   ( 0, 0, 1, 2, 2),
   (-2,-1, 0, 2, 2),
   (-2, 0, 1, 0, 0),
   (-2, 0, 0, 2, 1),
   ( 0, 0,-1, 2, 2),
   ( 2, 0, 0, 0, 0),
   ( 0, 0, 1, 0, 1),
   ( 2, 0,-1, 2, 2),
   ( 0, 0,-1, 0, 1),
   ( 0, 0, 1, 2, 1),
   (-2, 0, 2, 0, 0),
   ( 0, 0,-2, 2, 1),
   ( 2, 0, 0, 2, 2),
   ( 0, 0, 2, 2, 2),
   ( 0, 0, 2, 0, 0),
   (-2, 0, 1, 2, 2),
   ( 0, 0, 0, 2, 0),
   (-2, 0, 0, 2, 0),
   ( 0, 0,-1, 2, 1),
   ( 0, 2, 0, 0, 0),
   ( 2, 0,-1, 0, 1),
   (-2, 2, 0, 2, 2),
   ( 0, 1, 0, 0, 1));

arg_phi:array[0..30,0..1] of longint =
  ((-171996,-1742),
   ( -13187,  -16),
   (  -2274,   -2),
   (   2062,    2),
   (   1426,  -34),
   (    712,    1),
   (   -517,   12),
   (   -386,   -4),
   (   -301,    0),
   (    217,   -5),
   (   -158,    0),
   (    129,    1),
   (    123,    0),
   (     63,    0),
   (     63,    1),
   (    -59,    0),
   (    -58,   -1),
   (    -51,    0),
   (     48,    0),
   (     46,    0),
   (    -38,    0),
   (    -31,    0),
   (     29,    0),
   (     29,    0),
   (     26,    0),
   (    -22,    0),
   (     21,    0),
   (     17,   -1),
   (     16,    0),
   (    -16,    1),
   (    -15,    0));

arg_eps:array[0..30,0..1] of longint =
  (( 92025,   89),
   (  5736,  -31),
   (   977,   -5),
   (  -895,    5),
   (    54,   -1),
   (    -7,    0),
   (   224,   -6),
   (   200,    0),
   (   129,   -1),
   (   -95,    3),
   (     0,    0),
   (   -70,    0),
   (   -53,    0),
   (     0,    0),
   (   -33,    0),
   (    26,    0),
   (    32,    0),
   (    27,    0),
   (     0,    0),
   (   -24,    0),
   (    16,    0),
   (    13,    0),
   (     0,    0),
   (   -12,    0),
   (     0,    0),
   (     0,    0),
   (   -10,    0),
   (     0,    0),
   (    -8,    0),
   (     7,    0),
   (     9,    0));

var
  t,omega: extended;
  d,m,ms,f,s: extended;
  i: integer;
  epsilon,epsilon_0,delta_epsilon: extended;
  delta_phi: extended;
  alpha,delta: extended;
begin
  t:=(julian_date(date)-2451545.0)/36525;

  (* longitude of rising knot *)
  omega:=put_in_360(125.04452+(-1934.136261+(0.0020708+1/450000*t)*t)*t);

  { delta_phi and delta_epsilon }
  (* mean elongation of moon to sun *)
  d:=put_in_360(297.85036+(445267.111480+(-0.0019142+t/189474)*t)*t);

  (* mean anomaly of the sun *)
  m:=put_in_360(357.52772+(35999.050340+(-0.0001603-t/300000)*t)*t);

  (* mean anomly of the moon *)
  ms:=put_in_360(134.96298+(477198.867398+(0.0086972+t/56250)*t)*t);

  (* argument of the latitude of the moon *)
  f:=put_in_360(93.27191+(483202.017538+(-0.0036825+t/327270)*t)*t);

  delta_phi:=0;
  delta_epsilon:=0;

  for i:=0 to 30 do begin
    s:= arg_mul[i,0]*d
       +arg_mul[i,1]*m
       +arg_mul[i,2]*ms
       +arg_mul[i,3]*f
       +arg_mul[i,4]*omega;
    delta_phi:=delta_phi+(arg_phi[i,0]+arg_phi[i,1]*t*0.1)*sin_d(s);
    delta_epsilon:=delta_epsilon+(arg_eps[i,0]+arg_eps[i,1]*t*0.1)*cos_d(s);
    end;

  delta_phi:=delta_phi*0.0001/3600;
  delta_epsilon:=delta_epsilon*0.0001/3600;

  (* angle of ecliptic *)
  epsilon_0:=84381.448+(-46.8150+(-0.00059+0.001813*t)*t)*t;

  epsilon:=(epsilon_0+delta_epsilon)/3600;

  coord.longitude:=put_in_360(coord.longitude+delta_phi);

  (* geocentric coordinates *)
  alpha:=arctan2_d( sin_d(coord.longitude)*cos_d(epsilon)
                   -tan_d(coord.latitude)*sin_d(epsilon)
                  ,cos_d(coord.longitude));
  delta:=arcsin_d( sin_d(coord.latitude)*cos_d(epsilon)
                  +cos_d(coord.latitude)*sin_d(epsilon)*sin_d(coord.longitude));

  coord.rektaszension:=alpha;
  coord.declination:=delta;
end;

{ Based upon Chapter 24 of Meeus }
function sun_coordinate(date:TDateTime):t_coord;
var
  t,e,m,c,nu: extended;
  l0,o,omega,lambda: extended;
begin
  t:=(julian_date(date)-2451545.0)/36525;

  (* geometrical mean longitude of the sun *)
  l0:=280.46645+(36000.76983+0.0003032*t)*t;

  (* excentricity of the erath orbit *)
  e:=0.016708617+(-0.000042037-0.0000001236*t)*t;

  (* mean anomaly of the sun *)
  m:=357.52910+(35999.05030-(0.0001559+0.00000048*t)*t)*t;

  (* mean point of sun *)
  c:= (1.914600+(-0.004817-0.000014*t)*t)*sin_d(m)
     +(0.019993-0.000101*t)*sin_d(2*m)
     +0.000290*sin_d(3*m);

  (* true longitude of the sun *)
  o:=put_in_360(l0+c);

  (* true anomaly of the sun *)
  nu:=m+c;

  (* distance of the sun in km *)
  result.radius:=(1.000001018*(1-e*e))/(1+e*cos_d(nu))*AU;

  (* apparent longitude of the sun *)
  omega:=125.04452+(-1934.136261+(0.0020708+1/450000*t)*t)*t;
  lambda:=put_in_360(o-0.00569-0.00478*sin_d(omega)
                     -20.4898/3600/(result.radius/AU));

  result.longitude:=lambda;
  result.latitude:=0;

  calc_geocentric(result,date);
end;

{ Based upon Chapter 45 of Meeus }
function moon_coordinate(date:TDateTime):t_coord;
const
arg_lr:array[0..59, 0..3] of shortint =
  (( 0, 0, 1, 0),
   ( 2, 0,-1, 0),
   ( 2, 0, 0, 0),
   ( 0, 0, 2, 0),
   ( 0, 1, 0, 0),
   ( 0, 0, 0, 2),
   ( 2, 0,-2, 0),
   ( 2,-1,-1, 0),
   ( 2, 0, 1, 0),
   ( 2,-1, 0, 0),
   ( 0, 1,-1, 0),
   ( 1, 0, 0, 0),
   ( 0, 1, 1, 0),
   ( 2, 0, 0,-2),
   ( 0, 0, 1, 2),
   ( 0, 0, 1,-2),
   ( 4, 0,-1, 0),
   ( 0, 0, 3, 0),
   ( 4, 0,-2, 0),
   ( 2, 1,-1, 0),
   ( 2, 1, 0, 0),
   ( 1, 0,-1, 0),
   ( 1, 1, 0, 0),
   ( 2,-1, 1, 0),
   ( 2, 0, 2, 0),
   ( 4, 0, 0, 0),
   ( 2, 0,-3, 0),
   ( 0, 1,-2, 0),
   ( 2, 0,-1, 2),
   ( 2,-1,-2, 0),
   ( 1, 0, 1, 0),
   ( 2,-2, 0, 0),
   ( 0, 1, 2, 0),
   ( 0, 2, 0, 0),
   ( 2,-2,-1, 0),
   ( 2, 0, 1,-2),
   ( 2, 0, 0, 2),
   ( 4,-1,-1, 0),
   ( 0, 0, 2, 2),
   ( 3, 0,-1, 0),
   ( 2, 1, 1, 0),
   ( 4,-1,-2, 0),
   ( 0, 2,-1, 0),
   ( 2, 2,-1, 0),
   ( 2, 1,-2, 0),
   ( 2,-1, 0,-2),
   ( 4, 0, 1, 0),
   ( 0, 0, 4, 0),
   ( 4,-1, 0, 0),
   ( 1, 0,-2, 0),
   ( 2, 1, 0,-2),
   ( 0, 0, 2,-2),
   ( 1, 1, 1, 0),
   ( 3, 0,-2, 0),
   ( 4, 0,-3, 0),
   ( 2,-1, 2, 0),
   ( 0, 2, 1, 0),
   ( 1, 1,-1, 0),
   ( 2, 0, 3, 0),
   ( 2, 0,-1,-2));

arg_b:array[0..59, 0..3] of shortint =
  (( 0, 0, 0, 1),
   ( 0, 0, 1, 1),
   ( 0, 0, 1,-1),
   ( 2, 0, 0,-1),
   ( 2, 0,-1, 1),
   ( 2, 0,-1,-1),
   ( 2, 0, 0, 1),
   ( 0, 0, 2, 1),
   ( 2, 0, 1,-1),
   ( 0, 0, 2,-1),  (* !!! Error in German Meeus *)
   ( 2,-1, 0,-1),
   ( 2, 0,-2,-1),
   ( 2, 0, 1, 1),
   ( 2, 1, 0,-1),
   ( 2,-1,-1, 1),
   ( 2,-1, 0, 1),
   ( 2,-1,-1,-1),
   ( 0, 1,-1,-1),
   ( 4, 0,-1,-1),
   ( 0, 1, 0, 1),
   ( 0, 0, 0, 3),
   ( 0, 1,-1, 1),
   ( 1, 0, 0, 1),
   ( 0, 1, 1, 1),
   ( 0, 1, 1,-1),
   ( 0, 1, 0,-1),
   ( 1, 0, 0,-1),
   ( 0, 0, 3, 1),
   ( 4, 0, 0,-1),
   ( 4, 0,-1, 1),
   ( 0, 0, 1,-3),
   ( 4, 0,-2, 1),
   ( 2, 0, 0,-3),
   ( 2, 0, 2,-1),
   ( 2,-1, 1,-1),
   ( 2, 0,-2, 1),
   ( 0, 0, 3,-1),
   ( 2, 0, 2, 1),
   ( 2, 0,-3,-1),
   ( 2, 1,-1, 1),
   ( 2, 1, 0, 1),
   ( 4, 0, 0, 1),
   ( 2,-1, 1, 1),
   ( 2,-2, 0,-1),
   ( 0, 0, 1, 3),
   ( 2, 1, 1,-1),
   ( 1, 1, 0,-1),
   ( 1, 1, 0, 1),
   ( 0, 1,-2,-1),
   ( 2, 1,-1,-1),
   ( 1, 0, 1, 1),
   ( 2,-1,-2,-1),
   ( 0, 1, 2, 1),
   ( 4, 0,-2,-1),
   ( 4,-1,-1,-1),
   ( 1, 0, 1,-1),
   ( 4, 0, 1,-1),
   ( 1, 0,-1,-1),
   ( 4,-1, 0,-1),
   ( 2,-2, 0, 1));

sigma_r: array[0..59] of longint =
 (-20905355,
   -3699111,
   -2955968,
    -569925,
      48888,
      -3149,
     246158,
    -152138,
    -170733,
    -204586,
    -129620,
     108743,
     104755,
      10321,
          0,
      79661,
     -34782,
     -23210,
     -21636,
      24208,
      30824,
      -8379,
     -16675,
     -12831,
     -10445,
     -11650,
      14403,
      -7003,
          0,
      10056,
       6322,
      -9884,
       5751,
          0,
      -4950,
       4130,
          0,
      -3958,
          0,
       3258,
       2616,
      -1897,
      -2117,
       2354,
          0,
          0,
      -1423,
      -1117,
      -1571,
      -1739,
          0,
      -4421,
          0,
          0,
          0,
          0,
       1165,
          0,
          0,
       8752);

sigma_l: array[0..59] of longint =
 (6288774,
  1274027,
   658314,
   213618,
  -185116,
  -114332,
    58793,
    57066,
    53322,
    45758,
   -40923,
   -34720,
   -30383,
    15327,
   -12528,
    10980,
    10675,
    10034,
     8548,
    -7888,
    -6766,
    -5163,
     4987,
     4036,
     3994,
     3861,
     3665,
    -2689,
    -2602,
     2390,
    -2348,
     2236,
    -2120,
    -2069,
     2048,
    -1773,
    -1595,
     1215,
    -1110,
     -892,
     -810,
      759,
     -713,
     -700,
      691,
      596,
      549,
      537,
      520,
     -487,
     -399,
     -381,
      351,
     -340,
      330,
      327,
     -323,
      299,
      294,
        0);

sigma_b: array[0..59] of longint =
 (5128122,
   280602,
   277693,
   173237,
    55413,
    46271,
    32573,
    17198,
     9266,
     8822,
     8216,
     4324,
     4200,
    -3359,
     2463,
     2211,
     2065,
    -1870,
     1828,
    -1794,
    -1749,
    -1565,
    -1491,
    -1475,
    -1410,
    -1344,
    -1335,
     1107,
     1021,
      833,
      777,
      671,
      607,
      596,
      491,
     -451,
      439,
      422,
      421,
     -366,
     -351,
      331,
      315,
      302,
     -283,
     -229,
      223,
      223,
     -220,
     -220,
     -185,
      181,
     -177,
      176,
      166,
     -164,
      132,
     -119,
      115,
      107);

var
  t,d,m,ms,f,e,ls : extended;
  sr,sl,sb,temp: extended;
  a1,a2,a3: extended;
  lambda,beta,delta: extended;
  i: integer;
begin
  t:=(julian_date(date)-2451545)/36525;

  (* mean elongation of the moon *)
  d:=297.8502042+(445267.1115168+(-0.0016300+(1/545868-1/113065000*t)*t)*t)*t;

  (* mean anomaly of the sun *)
  m:=357.5291092+(35999.0502909+(-0.0001536+1/24490000*t)*t)*t;

  (* mean anomaly of the moon *)
  ms:=134.9634114+(477198.8676313+(0.0089970+(1/69699-1/1471200*t)*t)*t)*t;

  (* argument of the longitude of the moon *)
  f:=93.2720993+(483202.0175273+(-0.0034029+(-1/3526000+1/863310000*t)*t)*t)*t;

  (* correction term due to excentricity of the earth orbit *)
  e:=1.0+(-0.002516-0.0000074*t)*t;

  (* mean longitude of the moon *)
  ls:=218.3164591+(481267.88134236+(-0.0013268+(1/538841-1/65194000*t)*t)*t)*t;

  (* arguments of correction terms *)
  a1:=119.75+131.849*t;
  a2:=53.09+479264.290*t;
  a3:=313.45+481266.484*t;

  { sr := ä r_i cos(d,m,ms,f);   !!!  gives different value than in Meeus }
  sr:=0;
  for i:=0 to 59 do begin
    temp:=sigma_r[i]*cos_d( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e;
    sr:=sr+temp;
    end;

  { sl := ä l_i sin(d,m,ms,f); }
  sl:=0;
  for i:=0 to 59 do begin
    temp:=sigma_l[i]*sin_d( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e;
    sl:=sl+temp;
  end;

  (* correction terms *)
  sl:=sl +3958*sin_d(a1)
         +1962*sin_d(ls-f)
          +318*sin_d(a2);

  { sb := ä b_i sin(d,m,ms,f); }
  sb:=0;
  for i:=0 to 59 do begin
    temp:=sigma_b[i]*sin_d( arg_b[i,0]*d
                           +arg_b[i,1]*m
                           +arg_b[i,2]*ms
                           +arg_b[i,3]*f);
    if abs(arg_b[i,1])=1 then temp:=temp*e;
    if abs(arg_b[i,1])=2 then temp:=temp*e;
    sb:=sb+temp;
  end;

  (* correction terms *)
  sb:=sb -2235*sin_d(ls)
          +382*sin_d(a3)
          +175*sin_d(a1-f)
          +175*sin_d(a1+f)
          +127*sin_d(ls-ms)
          -115*sin_d(ls+ms);

  lambda:=ls+sl/1000000;
  beta:=sb/1000000;
  delta:=385000.56+sr/1000;

  result.radius:=delta;
  result.longitude:=lambda;
  result.latitude:=beta;

  calc_geocentric(result,date);
end;

{ Based upon chapter 39 of Meeus }
procedure correct_position(var position:t_coord; date:TDateTime;
                           latitude,longitude,height:extended);
var
  u,h,delta_alpha: extended;
  rho_sin, rho_cos: extended;
const
  b_a=0.99664719;
begin
  u:=arctan_d(b_a*b_a*tan_d(longitude));
  rho_sin:=b_a*sin_d(u)+height/6378140*sin_d(longitude);
  rho_cos:=cos_d(u)+height/6378140*cos_d(longitude);

  position.parallax:=arcsin_d(sin_d(8.794/3600)/(moon_distance(date)/AU));
  h:=star_time(date)-longitude-position.rektaszension;
  delta_alpha:=arctan_d(
                (-rho_cos*sin_d(position.parallax)*sin_d(h))/
                (cos_d(position.declination)-
                  rho_cos*sin_d(position.parallax)*cos_d(h)));
  position.rektaszension:=position.rektaszension+delta_alpha;
  position.declination:=arctan_d(
      (( sin_d(position.declination)
        -rho_sin*sin_d(position.parallax))*cos_d(delta_alpha))/
      ( cos_d(position.declination)
       -rho_cos*sin_d(position.parallax)*cos_d(h)));
end;

{ Moon phases and age of the moon }
{ Based upon Chapter 47 of Meeus }
{ Both used for moon phases and moon and sun eclipses }
procedure calc_phase_data(date:TDateTime; phase:TMoonPhase; var jde,kk,m,ms,f,o,e: extended);
var
  t: extended;
  k: longint;
  ts: extended;
begin
  k:=round((date-encodedate(2000,1,1))/36525.0*1236.85);
  ts:=(date-encodedate(2000,1,1))/36525.0;
  kk:=int(k)+ord(phase)/4.0;
  t:=kk/1236.85;
  jde:=2451550.09765+29.530588853*kk
       +t*t*(0.0001337-t*(0.000000150-0.00000000073*t));
  m:=2.5534+29.10535669*kk-t*t*(0.0000218+0.00000011*t);
  ms:=201.5643+385.81693528*kk+t*t*(0.1017438+t*(0.00001239-t*0.000000058));
  f:= 160.7108+390.67050274*kk-t*t*(0.0016341+t*(0.00000227-t*0.000000011));
  o:=124.7746-1.56375580*kk+t*t*(0.0020691+t*0.00000215);
  e:=1-ts*(0.002516+ts*0.0000074);
end;

{ Based upon Chapter 47 of Meeus }
function nextphase(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  t: extended;
  kk: extended;
  jde: extended;
  m,ms,f,o,e: extended;
  korr,w,akorr: extended;
  a:array[1..14] of extended;
begin
  calc_phase_data(date,phase,jde,kk,m,ms,f,o,e);
  t:=kk/1236.85;
  case phase of
    mpNewmoon: begin
      korr:= -0.40720*sin_d(ms)
             +0.17241*e*sin_d(m)
             +0.01608*sin_d(2*ms)
             +0.01039*sin_d(2*f)
             +0.00739*e*sin_d(ms-m)
             -0.00514*e*sin_d(ms+m)
             +0.00208*e*e*sin_d(2*m)
             -0.00111*sin_d(ms-2*f)
             -0.00057*sin_d(ms+2*f)
             +0.00056*e*sin_d(2*ms+m)
             -0.00042*sin_d(3*ms)
             +0.00042*e*sin_d(m+2*f)
             +0.00038*e*sin_d(m-2*f)
             -0.00024*e*sin_d(2*ms-m)
             -0.00017*sin_d(o)
             -0.00007*sin_d(ms+2*m)
             +0.00004*sin_d(2*ms-2*f)
             +0.00004*sin_d(3*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(2*ms+2*f)
             -0.00003*sin_d(ms+m+2*f)
             +0.00003*sin_d(ms-m+2*f)
             -0.00002*sin_d(ms-m-2*f)
             -0.00002*sin_d(3*ms+m)
             +0.00002*sin_d(4*ms);
    end;
    mpFirstQuarter, mpLastQuarter: begin
      korr:= -0.62801*sin_d(ms)
             +0.17172*e*sin_d(m)
             -0.01183*e*sin_d(ms+m)
             +0.00862*sin_d(2*ms)
             +0.00804*sin_d(2*f)
             +0.00454*e*sin_d(ms-m)
             +0.00204*e*e*sin_d(2*m)
             -0.00180*sin_d(ms-2*f)
             -0.00070*sin_d(ms+2*f)
             -0.00040*sin_d(3*ms)
             -0.00034*e*sin_d(2*ms-m)
             +0.00032*e*sin_d(m+2*f)
             +0.00032*e*sin_d(m-2*f)
             -0.00028*e*e*sin_d(ms+2*m)
             +0.00027*e*sin_d(2*ms+m)
             -0.00017*sin_d(o)
             -0.00005*sin_d(ms-m-2*f)
             +0.00004*sin_d(2*ms+2*f)
             -0.00004*sin_d(ms+m+2*f)
             +0.00004*sin_d(ms-2*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(3*m)
             +0.00002*sin_d(2*ms-2*f)
             +0.00002*sin_d(ms-m+2*f)
             -0.00002*sin_d(3*ms+m);
      w:=0.00306-0.00038*e*cos_d(m)
                +0.00026*cos_d(ms)
                -0.00002*cos_d(ms-m)
                +0.00002*cos_d(ms+m)
                +0.00002*cos_d(2*f);
      if phase = mpFirstQuarter then
        korr:=korr+w
      else
        korr:=korr-w;
    end;
    mpFullmoon: begin
      korr:= -0.40614*sin_d(ms)
             +0.17302*e*sin_d(m)
             +0.01614*sin_d(2*ms)
             +0.01043*sin_d(2*f)
             +0.00734*e*sin_d(ms-m)
             -0.00515*e*sin_d(ms+m)
             +0.00209*e*e*sin_d(2*m)
             -0.00111*sin_d(ms-2*f)
             -0.00057*sin_d(ms+2*f)
             +0.00056*e*sin_d(2*ms+m)
             -0.00042*sin_d(3*ms)
             +0.00042*e*sin_d(m+2*f)
             +0.00038*e*sin_d(m-2*f)
             -0.00024*e*sin_d(2*ms-m)
             -0.00017*sin_d(o)
             -0.00007*sin_d(ms+2*m)
             +0.00004*sin_d(2*ms-2*f)
             +0.00004*sin_d(3*m)
             +0.00003*sin_d(ms+m-2*f)
             +0.00003*sin_d(2*ms+2*f)
             -0.00003*sin_d(ms+m+2*f)
             +0.00003*sin_d(ms-m+2*f)
             -0.00002*sin_d(ms-m-2*f)
             -0.00002*sin_d(3*ms+m)
             +0.00002*sin_d(4*ms);
    end;
    else
      korr:=0;   (* Delphi 2 shut up! *)
  end;
  { Additional Corrections due to planets }
  a[1]:=299.77+0.107408*kk-0.009173*t*t;
  a[2]:=251.88+0.016321*kk;
  a[3]:=251.83+26.651886*kk;
  a[4]:=349.42+36.412478*kk;
  a[5]:= 84.66+18.206239*kk;
  a[6]:=141.74+53.303771*kk;
  a[7]:=207.14+2.453732*kk;
  a[8]:=154.84+7.306860*kk;
  a[9]:= 34.52+27.261239*kk;
  a[10]:=207.19+0.121824*kk;
  a[11]:=291.34+1.844379*kk;
  a[12]:=161.72+24.198154*kk;
  a[13]:=239.56+25.513099*kk;
  a[14]:=331.55+3.592518*kk;
  akorr:=   +0.000325*sin_d(a[1])
            +0.000165*sin_d(a[2])
            +0.000164*sin_d(a[3])
            +0.000126*sin_d(a[4])
            +0.000110*sin_d(a[5])
            +0.000062*sin_d(a[6])
            +0.000060*sin_d(a[7])
            +0.000056*sin_d(a[8])
            +0.000047*sin_d(a[9])
            +0.000042*sin_d(a[10])
            +0.000040*sin_d(a[11])
            +0.000037*sin_d(a[12])
            +0.000035*sin_d(a[13])
            +0.000023*sin_d(a[14]);
  korr:=korr+akorr;
  nextphase:=delphi_date(jde+korr);
end;

function last_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date+28;
  result:=temp_date;
  while result>date do begin
    result:=nextphase(temp_date,phase);
    temp_date:=temp_date-28;
  end;
end;

function next_phase(date:TDateTime; phase:TMoonPhase):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextphase(temp_date,phase);
    temp_date:=temp_date+28;
  end;
end;

{ Based upon Chapter 46 of Meeus }
function moon_phase_angle(date: TDateTime):extended;
var
  sun_coord,moon_coord: t_coord;
  phi,i: extended;
begin
  sun_coord:=sun_coordinate(date);
  moon_coord:=moon_coordinate(date);
  phi:=arccos(cos_d(moon_coord.latitude)
             *cos_d(moon_coord.longitude-sun_coord.longitude));
  i:=arctan(sun_coord.radius*sin(phi)/
            (moon_coord.radius-sun_coord.radius*cos(phi)));
  if i<0 then  result:=i/pi*180+180
         else  result:=i/pi*180;

  if put_in_360(moon_coord.longitude-sun_coord.longitude)>180 then
    result:=-result;

end;

function age_of_moon(date: TDateTime):extended;
var
  sun_coord,moon_coord: t_coord;
begin
  sun_coord:=sun_coordinate(date);
  moon_coord:=moon_coordinate(date);
  result:=put_in_360(moon_coord.longitude-sun_coord.longitude)/360*mean_lunation;
end;

function current_phase(date:TDateTime):extended;
begin
  result:=(1+cos_d(moon_phase_angle(date)))/2;
end;

function lunation(date:TDateTime):integer;
begin
  result:=round((last_phase(date,mpNewMoon)-delphi_date(2423436))/mean_lunation)+1;
end;

{ The distances }
function sun_distance(date: TDateTime): extended;
begin
  result:=sun_coordinate(date).radius/au;
end;

function moon_distance(date: TDateTime): extended;
begin
  result:=moon_coordinate(date).radius;
end;

{ The angular diameter (which is 0.5 of the subtent in moontool) }
function sun_diameter(date:TDateTime):extended;
begin
  result:=959.63/(sun_coordinate(date).radius/au)*2;
end;

function moon_diameter(date:TDateTime):extended;
begin
  result:=358473400/moon_coordinate(date).radius*2;
end;

{ Perigee and Apogee }
function nextXXXgee(date:TDateTime; apo: boolean):TDateTime;
const
arg_apo:array[0..31,0..2] of shortint =
   { D  F  M }
  (( 2, 0, 0),
   ( 4, 0, 0),
   ( 0, 0, 1),
   ( 2, 0,-1),
   ( 0, 2, 0),
   ( 1, 0, 0),
   ( 6, 0, 0),
   ( 4, 0,-1),
   ( 2, 2, 0),
   ( 1, 0, 1),
   ( 8, 0, 0),
   ( 6, 0,-1),
   ( 2,-2, 0),
   ( 2, 0,-2),
   ( 3, 0, 0),
   ( 4, 2, 0),
   ( 8, 0,-1),
   ( 4, 0,-2),
   (10, 0, 0),
   ( 3, 0, 1),
   ( 0, 0, 2),
   ( 2, 0, 1),
   ( 2, 0, 2),
   ( 6, 2, 0),
   ( 6, 0,-2),
   (10, 0,-1),
   ( 5, 0, 0),
   ( 4,-2, 0),
   ( 0, 2, 1),
   (12, 0, 0),
   ( 2, 2,-1),
   ( 1, 0,-1));

arg_per:array[0..59,0..2] of shortint =
   { D  F  M }
  (( 2, 0, 0),
   ( 4, 0, 0),
   ( 6, 0, 0),
   ( 8, 0, 0),
   ( 2, 0,-1),
   ( 0, 0, 1),
   (10, 0, 0),
   ( 4, 0,-1),
   ( 6, 0,-1),
   (12, 0, 0),
   ( 1, 0, 0),
   ( 8, 0,-1),
   (14, 0, 0),
   ( 0, 2, 0),
   ( 3, 0, 0),
   (10, 0,-1),
   (16, 0, 0),
   (12, 0,-1),
   ( 5, 0, 0),
   ( 2, 2, 0),
   (18, 0, 0),
   (14, 0,-1),
   ( 7, 0, 0),
   ( 2, 1, 0),
   (20, 0, 0),
   ( 1, 0, 1),
   (16, 0,-1),
   ( 4, 0, 1),
   ( 2, 0,-2),
   ( 4, 0,-2),
   ( 6, 0,-2),
   (22, 0, 0),
   (18, 0,-1),
   ( 6, 0, 1),
   (11, 0, 0),
   ( 8, 0, 1),
   ( 4,-2, 0),
   ( 6, 2, 0),
   ( 3, 0, 1),
   ( 5, 0, 1),
   (13, 0, 0),
   (20, 0,-1),
   ( 3, 0, 2),
   ( 4, 2,-2),
   ( 1, 0, 2),
   (22, 0,-1),
   ( 0, 4, 0),
   ( 6,-2, 0),
   ( 2,-2, 1),
   ( 0, 0, 2),
   ( 0, 2,-1),
   ( 2, 4, 0),
   ( 0, 2,-2),
   ( 2,-2, 2),
   (24, 0, 0),
   ( 4,-4, 0),
   ( 9, 0, 0),
   ( 4, 2, 0),
   ( 2, 0, 2),
   ( 1, 0,-1));

koe_apo:array[0..31,0..1] of longint =
   {    1   T }
  (( 4392,  0),
   (  684,  0),
   (  456,-11),
   (  426,-11),
   (  212,  0),
   ( -189,  0),
   (  144,  0),
   (  113,  0),
   (   47,  0),
   (   36,  0),
   (   35,  0),
   (   34,  0),
   (  -34,  0),
   (   22,  0),
   (  -17,  0),
   (   13,  0),
   (   11,  0),
   (   10,  0),
   (    9,  0),
   (    7,  0),
   (    6,  0),
   (    5,  0),
   (    5,  0),
   (    4,  0),
   (    4,  0),
   (    4,  0),
   (   -4,  0),
   (   -4,  0),
   (    3,  0),
   (    3,  0),
   (    3,  0),
   (   -3,  0));

koe_per:array[0..59,0..1] of longint =
   {     1   T }
  ((-16769,  0),
   (  4589,  0),
   ( -1856,  0),
   (   883,  0),
   (  -773, 19),
   (   502,-13),
   (  -460,  0),
   (   422,-11),
   (  -256,  0),
   (   253,  0),
   (   237,  0),
   (   162,  0),
   (  -145,  0),
   (   129,  0),
   (  -112,  0),
   (  -104,  0),
   (    86,  0),
   (    69,  0),
   (    66,  0),
   (   -53,  0),
   (   -52,  0),
   (   -46,  0),
   (   -41,  0),
   (    40,  0),
   (    32,  0),
   (   -32,  0),
   (    31,  0),
   (   -29,  0),
   (   -27,  0),
   (    24,  0),
   (   -21,  0),
   (   -21,  0),
   (   -21,  0),
   (    19,  0),
   (   -18,  0),
   (   -14,  0),
   (   -14,  0),
   (   -14,  0),
   (    14,  0),
   (   -14,  0),
   (    13,  0),
   (    13,  0),
   (    11,  0),
   (   -11,  0),
   (   -10,  0),
   (    -9,  0),
   (    -8,  0),
   (     8,  0),
   (     8,  0),
   (     7,  0),
   (     7,  0),
   (     7,  0),
   (    -6,  0),
   (    -6,  0),
   (     6,  0),
   (     5,  0),
   (    27,  0),
   (    27,  0),
   (     5,  0),
   (    -4,  0));

var
  k, jde, t: extended;
  d,m,f,v: extended;
  i: integer;
begin
  k:=round(((date-encodedate(1999,1,1))/365.25-0.97)*13.2555);
  if apo then
    k:=k+0.5;
  t:=k/1325.55;
  jde:=2451534.6698+27.55454988*k+(-0.0006886+
       (-0.000001098+0.0000000052*t)*t)*t*t;
  d:=171.9179+335.9106046*k+(-0.0100250+(-0.00001156+0.000000055*t)*t)*t*t;
  m:=347.3477+27.1577721*k+(-0.0008323-0.0000010*t)*t*t;
  f:=316.6109+364.5287911*k+(-0.0125131-0.0000148*t)*t*t;
  v:=0;
  if apo then
    for i:=0 to 31 do
      v:=v+sin_d(arg_apo[i,0]*d+arg_apo[i,1]*f+arg_apo[i,2]*m)*
         (koe_apo[i,0]*0.0001+koe_apo[i,1]*0.00001*t)
  else
    for i:=0 to 59 do
      v:=v+sin_d(arg_per[i,0]*d+arg_per[i,1]*f+arg_per[i,2]*m)*
         (koe_per[i,0]*0.0001+koe_per[i,1]*0.00001*t);
  result:=delphi_date(jde+v);
end;

function nextperigee(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXgee(temp_date,false);
    temp_date:=temp_date+28;
    end;
end;

function nextapogee(date:TDateTime):TDateTime;
var
  temp_date: TDateTime;
begin
  temp_date:=date-28;
  result:=temp_date;
  while result<date do begin
    result:=nextXXXgee(temp_date,true);
    temp_date:=temp_date+28;
    end;
end;

{ The seasons }
{ Based upon chapter 26 of Meeus }
function StartSeason(year: integer; season:TSeason):TDateTime;
var
  y: extended;
  jde0: extended;
  t, w, dl, s: extended;
  i: integer;
const
a: array[0..23] of integer = (
  485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50,
  45, 44, 29, 18, 17, 16, 14, 12, 12, 12, 9, 8 );

bc : array[0..23, 1..2] of extended =
  (( 324.96,   1934.136 ),
   ( 337.23,  32964.467 ),
   ( 342.08,     20.186 ),
   (  27.85, 445267.112 ),
   (  73.14,  45036.886 ),
   ( 171.52,  22518.443 ),
   ( 222.54,  65928.934 ),
   ( 296.72,   3034.906 ),
   ( 243.58,   9037.513 ),
   ( 119.81,  33718.147 ),
   ( 297.17,    150.678 ),
   (  21.02,   2281.226 ),
   ( 247.54,  29929.562 ),
   ( 325.15,  31555.956 ),
   (  60.93,   4443.417 ),
   ( 155.12,  67555.328 ),
   ( 288.79,   4562.452 ),
   ( 198.04,  62894.029 ),
   ( 199.76,  31436.921 ),
   (  95.39,  14577.848 ),
   ( 287.11,  31931.756 ),
   ( 320.81,  34777.259 ),
   ( 227.73,   1222.114 ),
   (  15.45,  16859.074 ));

begin
  case year of
    -1000..+999: begin
      y:=year/1000;
      case season of
        seSpring: jde0:=1721139.29189+(365242.13740+( 0.06134+( 0.00111-0.00071*y)*y)*y)*y;
        seSummer: jde0:=1721223.25401+(365241.72562+(-0.05323+( 0.00907+0.00025*y)*y)*y)*y;
        seAutumn: jde0:=1721325.70455+(365242.49558+(-0.11677+(-0.00297+0.00074*y)*y)*y)*y;
        seWinter: jde0:=1721414.39987+(365242.88257+(-0.00769+(-0.00933-0.00006*y)*y)*y)*y;
        else    jde0:=0;   (* this can't happen *)
      end;
    end;
    +1000..+3000: begin
      y:=(year-2000)/1000;
      case season of
        seSpring: jde0:=2451623.80984+(365242.37404+( 0.05169+(-0.00411-0.00057*y)*y)*y)*y;
        seSummer: jde0:=2451716.56767+(365241.62603+( 0.00325+( 0.00888-0.00030*y)*y)*y)*y;
        seAutumn: jde0:=2451810.21715+(365242.01767+(-0.11575+( 0.00337+0.00078*y)*y)*y)*y;
        seWinter: jde0:=2451900.05952+(365242.74049+(-0.06223+(-0.00823+0.00032*y)*y)*y)*y;
        else    jde0:=0;   (* this can't happen *)
      end;
    end;
    else raise E_OutOfAlgorithRange.Create('Out of range of the algorithm');
  end;
  t:=(jde0-2451545.0)/36525;
  w:=35999.373*t-2.47;
  dl:=1+0.0334*cos_d(w)+0.0007*cos_d(2*w);
  s:=0;
  for i:=0 to 23 do
    s:=s+a[i]*cos_d(bc[i,1]+bc[i,2]*t);
  {$IFDEF SR_Delphi1}
  Result:=Delphi_Date(jde0+(0.00001*s)/dl);
  {$ELSE}
  Result:=TransformDate(Delphi_Date(jde0+(0.00001*s)/dl));
  {$ENDIF}
end;

{ Rising and setting of moon and sun }
{ Based upon chapter 14 of Meeus }
function Calc_Set_Rise(date:TDateTime; latitude, longitude:extended;
                       sun: boolean; kind: T_RiseSet):TDateTime;
var
  h: Extended;
  pos1, pos2, pos3: t_coord;
  h0, theta0, cos_h0, cap_h0: extended;
  m0,m1,m2: extended;

  function interpolation(y1,y2,y3,n: extended):extended;
  var
    a,b,c: extended;
  begin
    a:=y2-y1;
    b:=y3-y2;
    if a>100 then  a:=a-360;
    if a<-100 then  a:=a+360;
    if b>100 then  b:=b-360;
    if b<-100 then  b:=b+360;
    c:=b-a;
    result:=y2+0.5*n*(a+b+n*c);
  end; {interpolation}

  function correction(m:extended; kind:integer):extended;
  var
    alpha,delta,h, height: extended;
  begin
    alpha:=interpolation(pos1.rektaszension,
                         pos2.rektaszension,
                         pos3.rektaszension,
                         m);
    delta:=interpolation(pos1.declination,
                         pos2.declination,
                         pos3.declination,
                         m);
    h:=put_in_360((theta0+360.985647*m)-longitude-alpha);
    if h>180 then h:=h-360;

    height:=arcsin_d(sin_d(latitude)*sin_d(delta)
                     +cos_d(latitude)*cos_d(delta)*cos_d(h));

    case kind of
      0:   result:=-h/360;
      1,2: result:=(height-h0)/(360*cos_d(delta)*cos_d(latitude)*sin_d(h));
      else result:=0;   (* this cannot happen *)
    end;
  end; {correction}

begin
  if sun then
    h0:=-0.8333
  else begin
    pos1:=moon_coordinate(date);
    correct_position(pos1,date,latitude,longitude,0);
    h0:=0.7275*pos1.parallax-34/60;
  end;

  h:=int(date);
  theta0:=star_time(h);
  if sun then begin
    pos1:=sun_coordinate(h-1);
    pos2:=sun_coordinate(h);
    pos3:=sun_coordinate(h+1);
  end
  else begin
    pos1:=moon_coordinate(h-1);
    correct_position(pos1,h-1,latitude,longitude,0);
    pos2:=moon_coordinate(h);
    correct_position(pos2,h,latitude,longitude,0);
    pos3:=moon_coordinate(h+1);
    correct_position(pos3,h+1,latitude,longitude,0);
  end;

  cos_h0:=(sin_d(h0)-sin_d(latitude)*sin_d(pos2.declination))/
          (cos_d(latitude)*cos_d(pos2.declination));
  if (cos_h0<-1) or (cos_h0>1) then
    raise E_NoRiseSet.Create('No rises or sets calculable');
  cap_h0:=arccos_d(cos_h0);

  m0:=(pos2.rektaszension+longitude-theta0)/360;
  m1:=m0-cap_h0/360;
  m2:=m0+cap_h0/360;

  m0:=frac(m0);
  if m0<0 then m0:=m0+1;
  m1:=frac(m1);
  if m1<0 then m1:=m1+1;
  m2:=frac(m2);
  if m2<0 then m2:=m2+1;

  m0:=m0+correction(m0,0);
  m1:=m1+correction(m1,1);
  m2:=m2+correction(m2,2);

  case kind of
    _rise:    result:=h+m1;
    _set:     result:=h+m2;
    _transit: result:=h+m0;
    else      result:=0;    (* this can't happen *)
  end;
end;

function Sun_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_rise);
end;

function Sun_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_set);
end;

function Sun_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,true,_transit);
end;

function Moon_Rise(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_rise);
end;

function Moon_Set(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_set);
end;

function Moon_Transit(date:TDateTime; latitude, longitude:extended):TDateTime;
begin
  result:=Calc_Set_Rise(date,latitude,longitude,false,_transit);
end;

{ Checking for eclipses }
function Eclipse(var date:TDateTime; sun:boolean):TEclipse;
var
  jde,kk,m,ms,f,o,e: extended;
  t,f1,a1: extended;
  p,q,w,gamma,u: extended;
begin
  if sun then
    calc_phase_data(date,mpNewMoon,jde,kk,m,ms,f,o,e)
  else
    calc_phase_data(date,mpFullMoon,jde,kk,m,ms,f,o,e);
  t:=kk/1236.85;
  if abs(sin_d(f))>0.36 then
    result:=ecNone
  else begin
    f1:=f-0.02665*sin_d(o);
    a1:=299.77+0.107408*kk-0.009173*t*t;
    if sun then
      jde:=jde - 0.4075     * sin_d(ms)
               + 0.1721 * e * sin_d(m)
    else
      jde:=jde - 0.4065     * sin_d(ms)
               + 0.1727 * e * sin_d(m);
    jde:=jde   + 0.0161     * sin_d(2*ms)
               - 0.0097     * sin_d(2*f1)
               + 0.0073 * e * sin_d(ms-m)
               - 0.0050 * e * sin_d(ms+m)
               - 0.0023     * sin_d(ms-2*f1)
               + 0.0021 * e * sin_d(2*m)
               + 0.0012     * sin_d(ms+2*f1)
               + 0.0006 * e * sin_d(2*ms+m)
               - 0.0004     * sin_d(3*ms)
               - 0.0003 * e * sin_d(m+2*f1)
               + 0.0003     * sin_d(a1)
               - 0.0002 * e * sin_d(m-2*f1)
               - 0.0002 * e * sin_d(2*ms-m)
               - 0.0002     * sin_d(o);
    p:=        + 0.2070 * e * sin_d(m)
               + 0.0024 * e * sin_d(2*m)
               - 0.0392     * sin_d(ms)
               + 0.0116     * sin_d(2*ms)
               - 0.0073 * e * sin_d(ms+m)
               + 0.0067 * e * sin_d(ms-m)
               + 0.0118     * sin_d(2*f1);
    q:=        + 5.2207
               - 0.0048 * e * cos_d(m)
               + 0.0020 * e * cos_d(2*m)
               - 0.3299     * cos_d(ms)
               - 0.0060 * e * cos_d(ms+m)
               + 0.0041 * e * cos_d(ms-m);
    w:=abs(cos_d(f1));
    gamma:=(p*cos_d(f1)+q*sin_d(f1))*(1-0.0048*w);
    u:= + 0.0059
        + 0.0046 * e * cos_d(m)
        - 0.0182     * cos_d(ms)
        + 0.0004     * cos_d(2*ms)
        - 0.0005     * cos_d(m+ms);

    if sun then begin
      if abs(gamma)<0.9972 then begin
        if u<0 then
          result:=ecTotal
        else if u>0.0047 then
          result:=ecCircular
        else if u<0.00464*sqrt(1-gamma*gamma) then
          result:=ecCirculartotal
        else
          result:=ecCircular;
      end
      else if abs(gamma)>1.5433+u then
        result:=ecNone
      else if abs(gamma)<0.9972+abs(u) then
        result:=ecNoncentral
      else
        result:=ecPartial;
    end
    else begin
      if (1.0128 - u - abs(gamma)) / 0.5450 > 0 then
        result:=ecTotal
      else if (1.5573 + u - abs(gamma)) / 0.5450 > 0 then
        result:=ecHalfshadow
      else
        result:=ecNone;
    end;
  end;
  date:=delphi_date(jde);
end;

function NextEclipse(var date:TDateTime; sun:boolean):TEclipse;
var
  temp_date: TDateTime;
begin
  result:=ecNone;    (* just to make Delphi 2/3 shut up, not needed really *)
  temp_date:=date-28*2;
  while temp_date<date do begin
    temp_date:=temp_date+28;
    result:=Eclipse(temp_date,sun);
  end;
  date:=temp_date;
end;

end.
