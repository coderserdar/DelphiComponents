
// Panu Niva 3.12.2000
// Biljardi-pelin komponetti
// Peliss� olevat pallot on
// Luotu t�h�n luokkaan.
// P��ohjelmassa on luotu FImagelist:n pallo ja p��ohjelmasta on
// tehty viittaus TDXBall:iin (ts. pallo[i].imagelist := Imagelist)
// Projektiin mukaan v�hint��n Main.pas-p��ohjelma (ja DXBall).

unit DXBall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DIB, DXClass, DXDraws, Math;

type
  TDXBall = class(TDXDIB)
  private
    CofE:real;                       // Energian s�ilymiskerroin t�rm�yksess�
  protected
    FMass:real;                      // Pallon massa
    FX:real;                         // Pallon x-koordinaatti
    FY:real;                         // Pallon y-koordinaatti
    FImagelist:TDXImagelist;
  public
    dx:real;          // Lis�ys X-suunnassa eli  nopeus X-akselin suuntaan
    dy:real;          // Lis�ys Y-suunnassa eli  nopeus Y-akselin suuntaan
    ballcolour:integer;              // Pallon v�ri imagelistin indeksin�
                                     // Voidaan poistaa my�hemmin.
    constructor Create(AOwner:TComponent);      override;
    destructor Destroy;                         override;
    procedure Move;                             virtual;
    procedure SetX(const Value:real);           virtual;
    procedure SetY(const Value:real);           virtual;
    procedure SetMass(Value:real);              virtual;
    procedure React(pallob:TDXBall);            virtual;
  published
    property Imagelist:TDXImagelist read FImagelist write FImagelist;
    property X:real read FX write SetX;
    property Y:real read FY write SetY;
    property Mass:real read FMass write SetMass;
end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pool', [TDXBall]);
end;

Constructor TDXBall.Create(AOwner:TComponent);
begin
  inherited;
  X := 0;
  Y := 0;
  dx := 0;
  dy := 0;
  ballcolour := 0; // oletusv�ri punainen pallo
  Mass := 1;   // Massa oletuksena = 1
  CofE := 1;          { Coefficient of restitution                  }
                      { Jos Cofe = 1 energiaa s�ilyy t�rm�yksess�   }
                      { Jos Cofe < 1 energiaa h�vi�� t�rm�yksess�   }
                      { Jos CoeF > 1 energia lis��ntyy t�rm�yksess� }
                      { Ei mahdollinen luonnossa.                   }
end;

destructor TDXBall.Destroy;
begin
  inherited;
end;

procedure TDXBall.Move;
begin
  X := X + dx;
  Y := Y + dy;
end;

procedure TDXBall.SetX(const Value:real);
begin
  FX := Value;
end;

procedure TDXBall.SetY(Const Value:real);
begin
  FY := Value;
end;

procedure TDXBall.SetMass(Value:real);
begin
  if (Value < 0) then Value := abs(Value); // Ei negeatiivisi� massoja
  if (Value > 100) then Value := 100;    // Suurin massa 100 yksikk��
  if (Value = 0) then Value := 1;          // Jokaisella objektilla pit��
  FMass := Value;                          // olla massa.
end;

procedure TDXBall.React(pallob:TDXBall);

// T�m� funktio selvitt�� ovatko mitk��n kaksi palloa t�rm�nneet:
// Jos t�rm�ys on tapahtunut, funktio laskee loppunopeudet
// kummankin pallon massan ja alkunopeuden avulla.

// Perustuu kaavaan va2 = (e+1)*mb*vb1 + va1(ma - e*mb)/(ma+mb)
// Perustuu kaavaan vb2 = (e+1)*ma*va1 + vb1(ma - e*mb)/(ma+mb)
// Jossa
//        va2 = pallo a:n loppunopeus
//        vb2 = pallo b:n loppunopeus
//        va1 = pallo a:n alkunopeus
//        vb1 = pallo b:n alkunopeus
//         ma = pallo a:n massa ( = 1 jos halutaan yksinkertaistaa kaavaa )
//         mb = pallo b:n massa ( = 1 jos halutaan yksinkertaistaa kaavaa )
//          e = t�rm�yksen energian s�ilymisen kerroin
//          ( = 1 jos halutaan yksinkertaistaa kaavaa )
//
// Ts Kaava yksinkertaistettuna :
// ( eli t�rm�yksess� energia ei muutu ja
//   t�rm��vien kappaleiden massat ovat samat. )
// va2 = (1+1)*1*vb1 + va1(1 - 1*1)/(1+1) eli
// va2 = 2*vb1                            ja
// vb2 = (1+1)*1*va1 + va1(1 - 1*1)/(1+1) eli
// vb2 = 2*va1

// Proseduuri ottaa huomioon my�s sen t�rm��k� kaksi
// tai useampia palloja samanaikaisesti.

var
  nabx:real;      // Pallo A:n normaalivektori
  naby:real;      // Pallo B:n normaalivektori
  Distance:real;  // Pallojen v�linen et�isyys
  tabx:real;      // Pallo A:n tangentiaalinen vektori
  taby:real;      // Pallo B:n tangentiaalinen vektori
  vait:real;      // Pallo A:n alkunopeus t-akselin avulla ilmoitettuna
  vain:real;      // Pallo A:n alkunopeus n-akselin avulla ilmoitettuna
  vbit:real;      // Pallo B:n alkunopeus t-akselin avulla ilmoitettuna
  vbin:real;      // Pallo B:n alkunopeus n-akselin avulla ilmoitettuna
  xva:real;       // Pallo A:n Alkunopeus X-akselin suuntaan
  yva:real;       // Pallo A:n Alkunopeus Y-akselin suuntaan
  xvb:real;       // Pallo B:n Alkunopeus X-akselin suuntaan
  yvb:real;       // Pallo B:n Alkunopeus Y-akselin suuntaan
  ma:real;        // Pallo A:n massa
  mb:real;        // Pallo B:n massa
  vafn:real;      // Pallo A:n loppunopeus n-akselin avulla ilmoitettuna
  vbfn:real;      // Pallo B:n loppunopeus n-akselin avulla ilmoitettuna
  vaft:real;      // Pallo A:n loppunopeus t-akselin avulla ilmoitettuna
  vbft:real;      // Pallo B:n loppunopeus t-akselin avulla ilmoitettuna
  xfa:real;       // Pallo A:n loppunopeus X-akselin suuntaan
  yfa:real;       // Pallo A:n loppunopeus Y-akselin suuntaan
  xfb:real;       // Pallo B:n loppunopeus X-akselin suuntaan
  yfb:real;       // Pallo B:n loppunopeus Y-akselin suuntaan
begin
         Tag := 0;  // Jos t�rm�ys tapahtuu Tag:in arvo muuttuu 1:ksi
         // Laske normaalivektori a->b
         nabx := (pallob.X-X);
         naby := (pallob.Y-Y);

         // T�rm��k� pallot?
         Distance := sqrt(nabx*nabx + naby*naby);
         if (Distance <= Imagelist.Items[0].Width*1.00) then
         begin
            Tag := 1;
            // Peruutetaan t�rm�nneit� palloja
            // kunnes ne eiv�t ole sis�kk�in
            while (Distance < Imagelist.Items[0].Width*1.00) do
            begin
              X := X - (dx/100);
              Y := Y - (dy/100);
              pallob.X := pallob.X - (pallob.dx/100);
              pallob.Y := pallob.Y - (pallob.dy/100);
              nabx := (pallob.X-X);
              naby := (pallob.Y-Y);
              Distance := sqrt(nabx*nabx + naby*naby);
            end;

            // Jos pallot koskettavat toisiaan, laske seuraus
            // Normalisoi normaalivektori
            nabx := nabx/Distance;
            naby := naby/Distance;

            // Laske tangentiaalinen veektori, joka on kohtisuorassa
            // normaaliin n�hden. Ts kierr� vektoria 90 astetta.
            tabx := -naby;
            taby := nabx;

            // Tangentiaalinen on my�s normalisoitu, koska se on
            // vain kierretty normaalivektori

            // Laske kaikki alkunopeudet
            // Pallojen merkinn�t:
            // initial = i, final = f,
            // n = normal suunta t = tangentin suunta

            // nopeudet X- ja Y- suunnassa kulman mukaan
            xva := dx;
            yva := dy;
            xvb := pallob.dx;
            yvb := pallob.dy;

            //  Pistetulot
            vait := xva*tabx + yva*taby;
            vain := xva*nabx + yva*naby;
            vbit := xvb*tabx + yvb*taby;
            vbin := xvb*nabx + yvb*naby;

            // Nyt tiedet��n kaikki alkunopeudet n- ja t-akselien avulla
            // ilmoitettuna. Seuraavaksi lasketaan t�rm�yksen j�lkeiset
            // loppunopeudet.

            ma := Mass;
            mb := pallob.Mass;

            vafn := (mb*vbin*(cofE+1) + vain*(ma - cofE*mb)) / (ma + mb);
            vbfn := (ma*vain*(cofE+1) - vbin*(ma - cofE*mb)) / (ma + mb);

            // Tangentiaaliset komponentit ovat samat ennen ja j�lkeen
            // t�rm�yksen:
            vaft := vait;
            vbft := vbit;

            // Nopeusvektorit ovat
            // kappale a (vafn, vaft)
            // kappale b (vbfn, vbft)

            // Nyt pit�� siirty� takaisin alkuper�iseen
            // x,y- koordinaattij�rjestelm��n.

            xfa := vafn*nabx + vaft*tabx;
            yfa := vafn*naby + vaft*taby;

            xfb := vbfn*nabx + vbft*tabx;
            yfb := vbfn*naby + vbft*taby;

            dx:=xfa;
            dy:=yfa;

            Pallob.dx:=xfb;
            Pallob.dy:=yfb;
          end;
end;

end.
