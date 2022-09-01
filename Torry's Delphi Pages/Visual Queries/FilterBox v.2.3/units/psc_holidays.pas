{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_holidays;

interface
{$I psc_defines.inc}

Uses
  myla_system,
  myla_interfaces,
  psc_procs,
  psc_const;

type
  TPSCEnumCountryHolidaysProc=procedure(const AName:String;
    const ADate:TDateTime) of Object;

  IPSCHolidays = interface(IPSCInterface)
    ['{93C49DF8-79A6-4C4F-BA1B-2ADB08F01B29}']
    Function AnyHolidays(ACountryID:TPSCCountryID):Boolean;
    Procedure EnumCountryHolidays(ACountryID:TPSCCountryID;
      const ABaseDate:TDateTime;AProc:TPSCEnumCountryHolidaysProc);
    function EnumCountryHolidaysForDate(ACountryID:TPSCCountryID;
      const ADate:TDateTime;const ANames:IPSCStrings):Boolean;
    Procedure SetCountryHolidays(ACountry: TPSCCountryID;
      const AHolidays: Array of TPSCDayConst);
  end;

Function PSCGetHolidays: IPSCHolidays;
Function PSCGetEasterFromBaseDate(Const BaseDate: TDateTime): TDateTime;
Function PSCGetEasterMonday(Const BaseDate: TDateTime): TDateTime;
Function PSCGetEasterDate(Year: Word): TDateTime;
Function PSCGetYearGoldenNumber(Year: Word): Integer;

{
 (c) 1990-1999 by John Bitros Athens - Greece
 jbitros@hotmail.com Or jbitros@x-treme.gr
 -----------------------------------------------------------------------------
 The first function is calculating the Greek Easter.
 The two other functions are for holidays that are
 based on Easter (Clean Monday & Holy Spirit)
 The Dates Of This Holidays Are Moving Between The Years
}
Function PSCGetGreekEasterDate(Const BaseDate: TDateTime): TDateTime;
Function PSCGetGreekCleanMondayDate(Const BaseDate: TDateTime): TDateTime;
Function PSCGetGreekHolySpiritDate(Const BaseDate: TDateTime): TDateTime;

procedure PSCEnumCountriesWithHolidays(const S:IPSCStrings);

{------------------------------------------------------------------------------}

implementation

{------------------------------------------------------------------------------}

Function PSCGetGreekCleanMondayDate(Const BaseDate: TDateTime): TDateTime;
Begin
  Result := PSCIncDay(PSCGetGreekEasterDate(BaseDate), -48);
End;

{------------------------------------------------------------------------------}

Function PSCGetGreekHolySpiritDate(Const BaseDate: TDateTime): TDateTime;
Begin
  Result := PSCIncDay(PSCGetGreekEasterDate(BaseDate),50);
End;

{------------------------------------------------------------------------------}

Function PSCGetGreekEasterDate(Const BaseDate: TDateTime): TDateTime;
Var
  nEasterMonth,nThisDay,nThisMonth,nThisYear: Word;
  nEasterDay,Y1,A,AA,B,BB,C,CC,D,DD,E,EE,P,PP: Real;
Begin

  PSCDeCodeDate(BaseDate,nThisYear,nThisMonth,nThisDay);

  Y1 := nThisYear;
  AA := Int(Y1 / 19);
  A := Y1 - AA * 19;
  BB := INT(Y1 / 4);
  B := Y1 - BB * 4;
  CC := Int(Y1 / 7);
  C := Y1 - CC * 7;
  DD := Int((19 * A + 16) / 30);
  D := 19 * A + 16 - DD * 30;
  EE := Int((2 * B + 4 * C + 6 * D) / 7);
  E := 2 * B + 4 * C + 6 * D - EE * 7;
  P := 3 + D + E;

  If P > 30 Then
    PP := P - 30
  Else
    PP := P;

  If ((P - PP) > 0) Or ((P - PP) < 0) Then
    nEasterMonth := 5
  Else
    nEasterMonth := 4;

  nEasterDay := Int(PP);

  Result := PSCEnCodeDate(nThisYear,nEasterMonth,Round(nEasterDay));

End;

{-------------------------------------------------------------------------}

Function PSCGetYearGoldenNumber(Year: Word): Integer;
Begin
  Result := (Year Mod 19) + 1;
End;

{-------------------------------------------------------------------------}

Function PSCGetEasterDate(Year: Word): TDateTime;
Var
  C,I,J,H,G,L: Integer;
  D,M: Word;
Begin
  G := PSCGetYearGoldenNumber(Year) - 1;
  C := Year Div 100;
  H := (C - C Div 4 - (8 * C + 13) Div 25 + 19 * G + 15) Mod 30;
  I := H - (H Div 28) * (1 - (H Div 28) * (29 Div (H + 1)) * ((21 - G) Div 11));
  J := (Year + Year Div 4 + I + 2 - C + C Div 4) Mod 7;

  L := I - J;
  M := 3 + (L + 40) Div 44;
  D := L + 28 - 31 * (M Div 4);
  Result := PSCEnCodeDate(Year,M,D);
End;

{-------------------------------------------------------------------------}

Function PSCGetEasterMonday(Const BaseDate: TDateTime): TDateTime;
Begin
  Result := PSCGetEasterFromBaseDate(BaseDate) + 1;
End;

{-------------------------------------------------------------------------}

Function PSCGetEasterFromBaseDate(Const BaseDate: TDateTime): TDateTime;
Var
  Year,Month,Day: Word;
Begin
  PSCDeCodeDate(BaseDate,Year,Month,Day);
  Result := PSCGetEasterDate(Year);
End;

{-------------------------------------------}

Resourcestring
//BeginSkipConst
  SPSCHolCzech1stJan = 'Nov² rok';
  SPSCHolCzech1stMay = 'Svßtek prßce';
  SPSCHolCzech8thMay = 'Stßtnf svßtek - Den osvobozenf od faÜismu (1945)';
  SPSCHolCzech5thJul =
    'Stßtnf svßtek - Den slovansk²ch v8rozv8st· Cyrila a Metod8je';
  SPSCHolCzech6thJul = 'Stßtnf svßtek - mistr Jan Hus (1415)';
  SPSCHolCzech28thOct =
    'Stßtnf svßtek - Den vzniku samostatnTho +eskoslovenskTho stßtu (1918)';
  SPSCHolCzech24thDec = 'èt8dr² den';
  SPSCHolCzech25thDec = '1. svßtek vßnoFnf';
  SPSCHolCzech26thDec = '2. svßtek vßnoFnf';
  SPSCHolCzechEasterSunday = 'Ned8le velikonoFnf';
  SPSCHolCzechEasterMonday = 'Pond8lf velikonoFnf';
  SPSCHolCzech28thSep = 'Statni svatek - Den ceske statnosti';
  SPSCHolCzech17thNov = 'Statni svatek - Den boje za svobodu a demokracii';

  SPSCHolHungary1stJan  = 'Újév';
  SPSCHolHungary15thMar = 'Nemzeti ünnep';
  SPSCHolHungaryEasterSunday = 'Húsvét';
  SPSCHolHungaryEasterMonday = 'Húsvét hétfõ';
  SPSCHolHungary1stMay  = 'A munka ünnepe';
  SPSCHolHungary20thAug = 'Szent István ünnepe';
  SPSCHolHungary1stNov  = 'Mindenszentek napja';
  SPSCHolHungary23thOct = 'A köztársaság napja';
  SPSCHolHungary24thDec = 'Jézuska';
  SPSCHolHungary25thDec = 'Karácsony';
  SPSCHolHungary26thDec = 'Karácsony másnapja';
  SPSCHolHungary31stDec = 'Szilveszter';
  SPSCHolHungary1stSnMay= 'Anyák napja';

  SPSCHolAustralia1stJan = 'New Year''s Day';
  SPSCHolAustralia25thApr = 'ANZAC Day';
  SPSCHolAustralia8thJun = 'Queen''s Birthday';
  SPSCHolAustralia1stMonOct = 'Labour Day';
  SPSCHolAustralia25thDec = 'Christmas';
  SPSCHolAustralia26thDec = 'Boxing Day';

  SPSCHolAustria1stJan = 'Neujahr';
  SPSCHolAustria6thJan = 'Heilige Drei Könige';
  SPSCHolAustria1stMay = 'Staatsfeiertag';
  SPSCHolAustria15thAug = 'Mariä Himmelfahrt';
  SPSCHolAustria26thOct = 'Nationalfeiertag';
  SPSCHolAustria1stNov = 'Allerheiligen';
  SPSCHolAustria8thDec = 'Mariä Empfängnis';
  SPSCHolAustria25thDec = 'Weihnachtstag';
  SPSCHolAustria26thDec = 'Stephanstag';

  SPSCHolBelgium1stJan = 'Nieuwjaar/Jour de l''An';
  SPSCHolBelgium1stMay = 'Feest va de Arbeid/Fête du Travail';
  SPSCHolBelgium11thJul = 'Feest Ned. Cultuurgem./Fête Comm. cult. néerl.';
  SPSCHolBelgium21stJul = 'Nationale Feestdag/Fête Nationale';
  SPSCHolBelgium15thAug = 'O.L.V.-Hemelvaart./Assomption';
  SPSCHolBelgium27thSep = 'Feest. Franst. Cultuurgem./Fête Comm. cult. franç';
  SPSCHolBelgium1stNov = 'Allerheiligen/Toussaint';
  SPSCHolBelgium11thNov = 'Wapenstilstand 1918/Armistice 1918';
  SPSCHolBelgium25thDec = 'Kerstmis/Noël';

  SPSCHolBrazil1stJan = 'Confr. Universal';
  SPSCHolBrazil6thJan = 'Santos Reis';
  SPSCHolBrazil21stApr = 'Tiradentes';
  SPSCHolBrazil1stMay = 'Festa do Trabalho';
  SPSCHolBrazil29thJun = 'São Pedro';
  SPSCHolBrazil15thAug = 'Assunção N. Senhora';
  SPSCHolBrazil7thSep = 'Indep. do Brasil';
  SPSCHolBrazil12thOct = 'N. Sra. Aparecida';
  SPSCHolBrazil1stNov = 'Todos os Santos';
  SPSCHolBrazil2ndNov = 'Finados';
  SPSCHolBrazil15thNov = 'Procl. da Rep.';
  SPSCHolBrazil8thDec = 'Imaculada Conceição';
  SPSCHolBrazil25thDec = 'Natal';
  SPSCHolBrazil31stDec = 'São Silvestre';

  SPSCHolCanada1stJan = 'New Year''s Day/Jour de l''An';
  SPSCHolCanada3rdMonMay = 'Victoria Day/Fête de Dollard';
  SPSCHolCanada24thJun = ' /Fête Nationale des Québecois';
  SPSCHolCanada1stJul = 'Canada Day/Jour de la Confédération';
  SPSCHolCanada1stMonAug = 'Civic Holiday/ ';
  SPSCHolCanada1stMonSep = 'Labour Day/Fête du Travail';
  SPSCHolCanada12thOct = 'Thanksgiving Day/Jour d''Action de Grâces';
  SPSCHolCanada11thNov = 'Remembrance Day/Jour du Souvenir';
  SPSCHolCanada25thDec = 'Christmas/Noël';
  SPSCHolCanada26thDec = 'Boxing Day/Lendemain de Noël';

  SPSCHolDenmarkJan1st = 'Nytår';
  SPSCHolDenmarkMay1st = '1. Maj';
  SPSCHolDenmarkJun5th = 'Grundlovsdag';
  SPSCHolDenmarkDec25th = 'Juledag';
  SPSCHolDenmarkDec26th = '2. Juledag';

  SPSCHolFinlandJan1st = 'Uudenvuodenpäivä';
  SPSCHolFinlandJan6th = 'Loppiainen';
  SPSCHolFinlandMay1st = 'Vappu';
  SPSCHolFinlandDec6th = 'Itsenäisyyspäivä';
  SPSCHolFinlandDec24th = 'Jouluaato';
  SPSCHolFinlandDec25th = 'Jouluaato';
  SPSCHolFinlandDec26th = '2. joulupäivä';

  SPSCHolFranceJan1st = 'Jour de l''An';
  SPSCHolFranceMay1st = 'Fête du Travail';
  SPSCHolFranceMay8th = 'Armistice 1945';
  SPSCHolFranceJul14th = 'Fête Nationale';
  SPSCHolFranceAug15th = 'Assomption';
  SPSCHolFranceNov1st = 'Toussaint';
  SPSCHolFranceNov11th = 'Armistice 1918';
  SPSCHolFranceDec25th = 'Noël';

  SPSCHolGermany1stJan = 'Neujahr';
  SPSCHolGermany6thJan = 'Heilige Drei Könige';
  SPSCHolGermanyMay1st = 'Maifeiertag';
  SPSCHolGermanyOct3rd = 'Tag der deutschen Einheit';
  SPSCHolGermany1stNov = 'Allerheiligen';
  SPSCHolGermany3rdWnNov = 'Buß- und Bettag';
  SPSCHolGermany4thSnNov = 'Totensonntag';
  SPSCHolGermanyDec24th = 'Heiligabend';
  SPSCHolGermanyDec25th = '1. Weihnachtstag';
  SPSCHolGermanyDec26th = '2. Weihnachtstag';
  SPSCHolGermanyDec31st = 'Silvester';

  SPSCHolGreeceJan1th = 'ÐÑÙÔÏ×ÑÏÍÉÁ';
  SPSCHolGreeceJan6th = 'ÈÅÏÖÁÍÅÉÁ';
  SPSCHolGreeceMar25th = '25ç ÌÁÑÔÉÏÕ-ÅÕÁÃÃÅËÉÓÌÏÓ';
  SPSCHolGreeceMay1th = 'ÐÑÙÔÏÌÁÃÉÁ';
  SPSCHolGreeceAug15th = 'ÊÏÉÌÇÓÇÓ ÔÇÓ ÈÅÏÔÏÊÏÕ';
  SPSCHolGreeceOkt28th = 'Ï×É - ÅÈÍÉÊÇ ÅÐÅÔÅÉÏÓ';
  SPSCHolGreeceDec25th = '×ÑÉÓÔÏÕÃÅÍÍÁ';
  SPSCHolGreeceDec26th = '2ç ÌÅÑÁ ×ÑÉÓÔÏÕÃÅÍÍÙÍ';
  SPSCHolGreeceEaster = 'ÁÃÉÏ ÐÁÓ×Á';
  SPSCHolGreeceHSpirit = 'ÁÃÉÏ ÐÍÅÕÌÁ';
  SPSCHolGreeceCleanMon = 'ÊÁÈÁÑÁ ÄÅÕÔÅÑÁ';

  SPSCHolIreland1stJan = 'New Year''s Day';
  SPSCHolIrelandMar17th = 'St. Patrick''s Day';
  SPSCHolIreland1stMonJun = 'June Holiday';
  SPSCHolIreland1stMonAug = 'August Holiday';
  SPSCHolIrelandLastMonOct = 'October Holiday';
  SPSCHolIrelandDec24th = 'Christmas Eve';
  SPSCHolIreland25thDec = 'Christmas';
  SPSCHolIreland26thDec = 'Boxing Day';
  SPSCHolIrelandDec31st = 'New Year''s Eve';

  SPSCHolItalyJan1st = 'Capodanno';
  SPSCHolItalyJan6th = 'Epifania';
  SPSCHolItalyApr25th = 'Anniv. della Liberazione';
  SPSCHolItalyMay1st = 'Festa del Lavoro';
  SPSCHolItalyAug15th = 'Assunzione M.V.';
  SPSCHolItalyNov1st = 'Ognissanti';
  SPSCHolItalyDec8th = 'Immacolata Concezione';
  SPSCHolItalyDec25th = 'Natale';
  SPSCHolItalyDec26th = 'Santo Stefano';

  SPSCHolJapan1stJan = 'New Year''s Day';
  SPSCHolJapanJan15th = 'Adult''s Day';
  SPSCHolJapanFeb11th = 'Commemoration of the Founding of the Nation';
  SPSCHolJapanMar20th = 'Vernal Equinox Day';
  SPSCHolJapanMay3rd = 'Constitution Day';
  SPSCHolJapanMay4th = 'Greenary Day';
  SPSCHolJapanMay5th = 'Childrens'' Day';
  SPSCHolJapanSep15th = 'Respect for the Aged Day';
  SPSCHolJapanSep23rd = 'Autumnal Equinox Day';
  SPSCHolJapanOct10th = 'Health-Sports Day';
  SPSCHolJapanNov23rd = 'Labour Thanksgiving Day';
  SPSCHolJapanNov3rd = 'Culture Day';
  SPSCHolJapanDec23rd = 'The Emperor''s Birthday';

  SPSCHolNetherlandsJan1st = 'Nieuwjaar';
  SPSCHolNetherlandsApr30th = 'Koninginnedag';
  SPSCHolNetherlandsMay5th = 'Bevrijdingsdag';
  SPSCHolNetherlandsDec25th = 'Kerstmis';
  SPSCHolNetherlandsDec26th = '2de Kerstdag';

  SPSCHolNewZealand1stJan = 'New Year''s Day';
  SPSCHolNewZealandFeb6th = 'Waitangi Day';
  SPSCHolNewZealand25thApr = 'ANZAC Day';
  SPSCHolNewZealand1stMonJun = 'Queen''s Birthday';
  SPSCHolNewZealand_4thMonOct = 'Labour Day';
  SPSCHolNewZealand25thDec = 'Christmas';
  SPSCHolNewZealand26thDec = 'Boxing Day';

  SPSCHolNorwayJan1st = 'Nyttårsdag';
  SPSCHolNorwayMay1st = '1. mai';
  SPSCHolNorwayMay17th = '17. mai';
  SPSCHolNorwayDec25th = 'Juledag';
  SPSCHolNorwayDec26th = '2. Juledag';

  SPSCHolPortugalJan1st = 'Circuncisão';
  SPSCHolPortugalApr25th = 'Dia da Liberdade';
  SPSCHolPortugalMay1st = 'Dia do Trabalho';
  SPSCHolPortugalJun10th = 'Dia de Portugal';
  SPSCHolPortugalAug15th = 'Assunção de Nossa Senhora';
  SPSCHolPortugalOct5th = 'Proclamação da República';
  SPSCHolPortugalNov1st = 'Todos-os-Santos';
  SPSCHolPortugalDec1st = 'Restaur. de Independ';
  SPSCHolPortugalDec8th = 'Imaculada Conceiçao de Nossa Senhora';
  SPSCHolPortugalDec25th = 'Dia de Natal';

  SPSCHolSouthAfrica1stJan = 'New Year''s Day';
  SPSCHolSouthAfricaApr6th = 'Founders'' Day';
  SPSCHolSouthAfricaMay1st = 'Workers'' Day';
  SPSCHolSouthAfricaMay31st = 'Republic Day';
  SPSCHolSouthAfricaOct10th = 'Kruger Day';
  SPSCHolSouthAfricaDec16th = 'Day of Vow';
  SPSCHolSouthAfrica25thDec = 'Christmas';
  SPSCHolSouthAfricaDec26th = 'Day of Goodwill';

  SPSCHolSpainJan1st = 'Año Nuevo';
  SPSCHolSpainJan6th = 'Epifanía';
  SPSCHolSpainJan23rd = 'San Ildefonso';
  SPSCHolSpainFeb28th = 'Día de Andalucía';
  SPSCHolSpainMar19th = 'San José';
  SPSCHolSpainApr2nd = 'Fiesta de la Primavera';
  SPSCHolSpainApr23rd = 'Día de Aragón/Día de la Comunidad';
  SPSCHolSpainMay1st = 'Fiesta Trabajo';
  SPSCHolSpainMay2nd = 'Día de la Comunidad';
  SPSCHolSpainMay15th = 'San Isidro';
  SPSCHolSpainMay30th = 'Día de Canarias';
  SPSCHolSpainMay31st = 'Día de Castilla-La Mancha';
  SPSCHolSpainJun9th = 'Día de la Región/Día de la Rioja';
  SPSCHolSpainJun13th = 'San Antonio';
  SPSCHolSpainJul7th = 'San Fermín';
  SPSCHolSpainJul25th = 'Santiago';
  SPSCHolSpainAug15th = 'Asunción';
  SPSCHolSpainAug25th = 'Batalla de las Flores';
  SPSCHolSpainSep8th = 'Día de Asturias/V. de San Lorenzo/V. Guadalupe';
  SPSCHolSpainSep9th = 'Sta. María de la Cabeza';
  SPSCHolSpainSep13th = 'Viernes Santo';
  SPSCHolSpainSep21st = 'San Mateo';
  SPSCHolSpainSep24th = 'Sta. Mercedes';
  SPSCHolSpainOct9th = 'Día de la Comunidad';
  SPSCHolSpainOct12th = 'Hispanidad';
  SPSCHolSpainOct15th = 'N. Sra. Bien Aparecida';
  SPSCHolSpainNov7th = 'N. Sra. del Rosario';
  SPSCHolSpainDec8th = 'Día de la Constitución';
  SPSCHolSpainDec25th = 'Navidad';
  SPSCHolSpainDec26th = 'San Esteban';

  SPSCHolSwedenJan1st = 'Nyårsdagen';
  SPSCHolSwedenJan6th = 'Trettondagen';
  SPSCHolSwedenMay1st = 'Första maj';
  SPSCHolSwedenLastSnMay = 'Mors dag';
  SPSCHolSwedenJun24th = 'Johannes Döparens dag';
  SPSCHolSwedenOct24th = 'FN-dagen';
  SPSCHolSweden2ndSnNov = 'Fars dag';
  SPSCHolSwedenDec25th = 'Juldagen';
  SPSCHolSwedenDec26th = 'Annandag jul';

  SPSCHolSwitzerlandJan1st = 'Nouvel An/Neujahr/Capo d''anno';
  SPSCHolSwitzerlandJan2nd = 'Berchtoldstag';
  SPSCHolSwitzerlandJan6th = 'Les 3 Rois/Heilige 3 Könige/Epifania';
  SPSCHolSwitzerlandJan24th = 'Indépendance vaudoise';
  SPSCHolSwitzerlandMar1st = 'République neuchâteloise';
  SPSCHolSwitzerlandAug1st = 'Fête Nationale Suisse/Bundesfeier/Festa Federale';
  SPSCHolSwitzerlandAug15th = 'Assomption de N.D./Mariä Himmelfahrt/Assunzione';
  SPSCHolSwitzerlandDec8th =
    'Imm. Concept. de N.D./Mariä Empfängnis/Immacolata';
  SPSCHolSwitzerlandDec25th = 'Noël/Weihnachten/Natale';
  SPSCHolSwitzerlandDec26th = 'St.-Etienne/Stephanstag/Santo Stefano';
  SPSCHolSwitzerlandDec31st = 'Sylvestre/Silvester/S. Silvestro';

  SPSCHolUnitedKingdom1stJan = 'New Year''s Day';
  SPSCHolUnitedKingdom1stMonMay = 'May Day';
  SPSCHolUnitedKingdomLastMonMay = 'Spring Bank Holiday';
  SPSCHolUnitedKingdom2ndMonJul = 'Public Holiday, Northern Ireland';
  SPSCHolUnitedKingdomLastMonAug = 'Summer Bank Holiday';
  SPSCHolUnitedKingdomDec24th = 'Christmas Eve';
  SPSCHolUnitedKingdom25thDec = 'Christmas';
  SPSCHolUnitedKingdom26thDec = 'Boxing Day';
  SPSCHolUnitedKingdomDec31st = 'New Year''s Eve';

  SPSCHolUnitedStates1stJan = 'New Year''s Day';
  SPSCHolUnitedStates3rdMonJan = 'Martin Luther King, Jr.';
  SPSCHolUnitedStatesFeb2nd = 'Groundhog Day';
  SPSCHolUnitedStatesFeb12th = 'Lincoln''s Birthday';
  SPSCHolUnitedStatesFeb14th = 'Valentine''s Day';
  SPSCHolUnitedStates3rdMonFeb = 'President''s Day';
  SPSCHolUnitedStatesFeb22nd = 'Washington''s Birthday';
  SPSCHolUnitedStatesMar17th = 'St. Patrick''s Day';
  SPSCHolUnitedStatesApr1st = 'April Fool''s Day';
  SPSCHolUnitedStates1stSnApr = 'Daylight Savings Time Begins';
  SPSCHolUnitedStates2ndSnMay = 'Mother''s Day';
  SPSCHolUnitedStates3rdStMay = 'Armed Forces Day';
  SPSCHolUnitedStatesLastMonMay = 'Memorial Day (Observed)';
  SPSCHolUnitedStatesJun14th = 'Flag Day';
  SPSCHolUnitedStates3rdSnJun = 'Father''s Day';
  SPSCHolUnitedStatesJul4th = 'Independence Day';
  SPSCHolUnitedStates1stMonSep = 'Labour Day';
  SPSCHolUnitedStates2ndMonOct = 'Columbus Day (Observed)';
  SPSCHolUnitedStatesLastSnOct = 'Daylight Savings Time Ends';
  SPSCHolUnitedStatesOct31st = 'Halloween';
  SPSCHolUnitedStatesNov11th = 'Veterans Day';
  SPSCHolUnitedStates4thThuNov = 'Thanksgiving';
  SPSCHolUnitedStates25thDec = 'Christmas';
  SPSCHolUnitedStatesDec31st = 'New Year''s Eve';
//EndSkipConst

Const

  cPSCCzechCount = 13;
  cPSCCzech: Array[0..cPSCCzechCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: TPSCMonth(1); Name: SPSCHolCzech1stJan),
    (Kind: dckDate; Day: 1; Month: TPSCMonth(5); Name: SPSCHolCzech1stMay),
    (Kind: dckDate; Day: 8; Month: TPSCMonth(5); Name: SPSCHolCzech8thMay),
    (Kind: dckDate; Day: 5; Month: TPSCMonth(7); Name: SPSCHolCzech5thJul),
    (Kind: dckDate; Day: 6; Month: TPSCMonth(7); Name: SPSCHolCzech6thJul),
    (Kind: dckDate; Day: 28; Month: TPSCMonth(10); Name: SPSCHolCzech28thOct),
    (Kind: dckDate; Day: 24; Month: TPSCMonth(12); Name: SPSCHolCzech24thDec),
    (Kind: dckDate; Day: 25; Month: TPSCMonth(12); Name: SPSCHolCzech25thDec),
    (Kind: dckDate; Day: 26; Month: TPSCMonth(12); Name: SPSCHolCzech26thDec),
    (Kind: dckProc; Name: SPSCHolCzechEasterSunday; DateProc:
      PSCGetEasterFromBaseDate),
    (Kind: dckProc; Name: SPSCHolCzechEasterMonday; DateProc:
      PSCGetEasterMonday),
    (Kind: dckDate; Day: 28; Month: TPSCMonth(9); Name: SPSCHolCzech28thSep),
    (Kind: dckDate; Day: 17; Month: TPSCMonth(11); Name: SPSCHolCzech17thNov)
    );

  cPSCHungaryCount = 13;
  cPSCHungary: Array[0..cPSCHungaryCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day:  1; Month: TPSCMonth(1); Name: SPSCHolHungary1stJan),
    (Kind: dckDate; Day:  1; Month: TPSCMonth(5); Name: SPSCHolHungary1stMay),
    (Kind: dckDate; Day: 15; Month: TPSCMonth(3); Name: SPSCHolHungary15thMar),
    (Kind: dckDate; Day: 20; Month: TPSCMonth(8); Name: SPSCHolHungary20thAug),
    (Kind: dckDate; Day: 23; Month: TPSCMonth(10); Name: SPSCHolHungary23thOct),
    (Kind: dckDate; Day:  1; Month: TPSCMonth(11); Name: SPSCHolHungary1stNov),
    (Kind: dckDate; Day: 24; Month: TPSCMonth(12); Name: SPSCHolHungary24thDec),
    (Kind: dckDate; Day: 25; Month: TPSCMonth(12); Name: SPSCHolHungary25thDec),
    (Kind: dckDate; Day: 26; Month: TPSCMonth(12); Name: SPSCHolHungary26thDec),
    (Kind: dckDate; Day: 31; Month: TPSCMonth(12); Name: SPSCHolHungary31stDec),
    (Kind: dckProc; Name: SPSCHolHungaryEasterSunday; DateProc: PSCGetEasterFromBaseDate),
    (Kind: dckProc; Name: SPSCHolHungaryEasterMonday; DateProc: PSCGetEasterMonday),
    (Kind: dckDayNumber; Day: 2; Month: mMay; WeekDay: dwSunday; Name: SPSCHolHungary1stSnMay)
  );

  cPSCGreeceCount = 11;
  cPSCGreece: Array[0..cPSCGreeceCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolGreeceJan1th),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolGreeceJan6th),
    (Kind: dckDate; Day: 25; Month: mMarch; Name: SPSCHolGreeceMar25th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolGreeceMay1th),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolGreeceAug15th),
    (Kind: dckDate; Day: 28; Month: mOctober; Name: SPSCHolGreeceOkt28th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolGreeceDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolGreeceDec26th),

    (Kind: dckProc; Name: SPSCHolGreeceEaster; DateProc: PSCGetGreekEasterDate),
    (Kind: dckProc; Name: SPSCHolGreeceHSpirit; DateProc:
      PSCGetGreekHolySpiritDate),
    (Kind: dckProc; Name: SPSCHolGreeceCleanMon; DateProc:
      PSCGetGreekCleanMondayDate)
    );

  cPSCAustraliaCount = 6;
  cPSCAustralia: Array[0..cPSCAustraliaCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolAustralia1stJan),
    (Kind: dckDate; Day: 25; Month: mApril; Name: SPSCHolAustralia25thApr),
    (Kind: dckDate; Day: 8; Month: mJune; Name: SPSCHolAustralia8thJun),
    (Kind: dckDayNumber; Day: 1; Month: mOctober; WeekDay: dwMonday; Name:
      SPSCHolAustralia1stMonOct),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolAustralia25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolAustralia26thDec)
    );

  cPSCAustriaCount = 9;
  cPSCAustria: Array[0..cPSCAustriaCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolAustria1stJan),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolAustria6thJan),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolAustria1stMay),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolAustria15thAug),
    (Kind: dckDate; Day: 26; Month: mOctober; Name: SPSCHolAustria26thOct),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolAustria1stNov),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolAustria8thDec),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolAustria25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolAustria26thDec)
    );

  cPSCBelgiumCount = 9;
  cPSCBelgium: Array[0..cPSCBelgiumCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolBelgium1stJan),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolBelgium1stMay),
    (Kind: dckDate; Day: 11; Month: mJuly; Name: SPSCHolBelgium11thJul),
    (Kind: dckDate; Day: 21; Month: mJuly; Name: SPSCHolBelgium21stJul),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolBelgium15thAug),
    (Kind: dckDate; Day: 27; Month: mSeptember; Name: SPSCHolBelgium27thSep),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolBelgium1stNov),
    (Kind: dckDate; Day: 11; Month: mNovember; Name: SPSCHolBelgium11thNov),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolBelgium25thDec)
    );

  cPSCBrazilCount = 14;
  cPSCBrazil: Array[0..cPSCBrazilCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolBrazil1stJan),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolBrazil6thJan),
    (Kind: dckDate; Day: 21; Month: mApril; Name: SPSCHolBrazil21stApr),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolBrazil1stMay),
    (Kind: dckDate; Day: 29; Month: mJune; Name: SPSCHolBrazil29thJun),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolBrazil15thAug),
    (Kind: dckDate; Day: 7; Month: mSeptember; Name: SPSCHolBrazil7thSep),
    (Kind: dckDate; Day: 12; Month: mOctober; Name: SPSCHolBrazil12thOct),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolBrazil1stNov),
    (Kind: dckDate; Day: 2; Month: mNovember; Name: SPSCHolBrazil2ndNov),
    (Kind: dckDate; Day: 15; Month: mNovember; Name: SPSCHolBrazil15thNov),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolBrazil8thDec),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolBrazil25thDec),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolBrazil31stDec)
    );

  cPSCCanadaCount = 10;
  cPSCCanada: Array[0..cPSCCanadaCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolCanada1stJan),
    (Kind: dckDayNumber; Day: 3; Month: mMay; Name: SPSCHolCanada3rdMonMay),
    (Kind: dckDate; Day: 24; Month: mJune; Name: SPSCHolCanada24thJun),
    (Kind: dckDate; Day: 1; Month: mJuly; Name: SPSCHolCanada1stJul),
    (Kind: dckDayNumber; Day: 1; Month: mAugust; WeekDay: dwMonday; Name:
      SPSCHolCanada1stMonAug),
    (Kind: dckDayNumber; Day: 1; Month: mSeptember; WeekDay: dwMonday; Name:
      SPSCHolCanada1stMonSep),
    (Kind: dckDate; Day: 12; Month: mOctober; Name: SPSCHolCanada12thOct),
    (Kind: dckDate; Day: 11; Month: mNovember; Name: SPSCHolCanada11thNov),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolCanada25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolCanada26thDec)
    );

  cPSCDenmarkCount = 5;
  cPSCDenmark: Array[0..cPSCDenmarkCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolDenmarkJan1st),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolDenmarkMay1st),
    (Kind: dckDate; Day: 5; Month: mJune; Name: SPSCHolDenmarkJun5th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolDenmarkDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolDenmarkDec26th)
    );

  cPSCFinlandCount = 7;
  cPSCFinland: Array[0..cPSCFinlandCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolFinlandJan1st),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolFinlandJan6th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolFinlandMay1st),
    (Kind: dckDate; Day: 6; Month: mDecember; Name: SPSCHolFinlandDec6th),
    (Kind: dckDate; Day: 24; Month: mDecember; Name: SPSCHolFinlandDec24th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolFinlandDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolFinlandDec26th)
    );

  cPSCFranceCount = 8;
  cPSCFrance: Array[0..cPSCFranceCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolFranceJan1st),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolFranceMay1st),
    (Kind: dckDate; Day: 8; Month: mMay; Name: SPSCHolFranceMay8th),
    (Kind: dckDate; Day: 14; Month: mJuly; Name: SPSCHolFranceJul14th),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolFranceAug15th),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolFranceNov1st),
    (Kind: dckDate; Day: 11; Month: mNovember; Name: SPSCHolFranceNov11th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolFranceDec25th)
    );

  cPSCGermanyCount = 11;
  cPSCGermany: Array[0..cPSCGermanyCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolGermany1stJan),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolGermany6thJan),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolGermanyMay1st),
    (Kind: dckDate; Day: 3; Month: mOctober; Name: SPSCHolGermanyOct3rd),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolGermany1stNov),
    (Kind: dckDayNumber; Day: 3; Month: mNovember; WeekDay: dwWednesday; Name:
      SPSCHolGermany3rdWnNov),
    (Kind: dckDayNumber; Day: 4; Month: mNovember; WeekDay: dwSunday; Name:
      SPSCHolGermany4thSnNov),
    (Kind: dckDate; Day: 24; Month: mDecember; Name: SPSCHolGermanyDec24th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolGermanyDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolGermanyDec26th),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolGermanyDec31st)
    );

  cPSCIrelandCount = 9;
  cPSCIreland: Array[0..cPSCIrelandCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolIreland1stJan),
    (Kind: dckDate; Day: 17; Month: mMarch; Name: SPSCHolIrelandMar17th),
    (Kind: dckDayNumber; Day: 1; Month: mJune; WeekDay: dwMonday; Name:
      SPSCHolIreland1stMonJun),
    (Kind: dckDayNumber; Day: 1; Month: mAugust; WeekDay: dwMonday; Name:
      SPSCHolIreland1stMonAug),
    (Kind: dckDayNumber; Day: - 1; Month: mOctober; WeekDay: dwMonday; Name:
      SPSCHolIrelandLastMonOct),
    (Kind: dckDate; Day: 24; Month: mDecember; Name: SPSCHolIrelandDec24th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolIreland25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolIreland26thDec),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolIrelandDec31st)
    );

  cPSCItalyCount = 9;
  cPSCItaly: Array[0..cPSCItalyCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolItalyJan1st),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolItalyJan6th),
    (Kind: dckDate; Day: 25; Month: mApril; Name: SPSCHolItalyApr25th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolItalyMay1st),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolItalyAug15th),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolItalyNov1st),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolItalyDec8th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolItalyDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolItalyDec26th)
    );

  cPSCJapanCount = 13;
  cPSCJapan: Array[0..cPSCJapanCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolJapan1stJan),
    (Kind: dckDate; Day: 15; Month: mJanuary; Name: SPSCHolJapanJan15th),
    (Kind: dckDate; Day: 11; Month: mFebruary; Name: SPSCHolJapanFeb11th),
    (Kind: dckDate; Day: 20; Month: mMarch; Name: SPSCHolJapanMar20th),
    (Kind: dckDate; Day: 3; Month: mMay; Name: SPSCHolJapanMay3rd),
    (Kind: dckDate; Day: 4; Month: mMay; Name: SPSCHolJapanMay4th),
    (Kind: dckDate; Day: 5; Month: mMay; Name: SPSCHolJapanMay5th),
    (Kind: dckDate; Day: 15; Month: mSeptember; Name: SPSCHolJapanSep15th),
    (Kind: dckDate; Day: 23; Month: mSeptember; Name: SPSCHolJapanSep23rd),
    (Kind: dckDate; Day: 10; Month: mOctober; Name: SPSCHolJapanOct10th),
    (Kind: dckDate; Day: 3; Month: mNovember; Name: SPSCHolJapanNov3rd),
    (Kind: dckDate; Day: 23; Month: mNovember; Name: SPSCHolJapanNov23rd),
    (Kind: dckDate; Day: 23; Month: mDecember; Name: SPSCHolJapanDec23rd)
    );

  cPSCNetherlandsCount = 5;
  cPSCNetherlands: Array[0..cPSCNetherlandsCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolNetherlandsJan1st),
    (Kind: dckDate; Day: 30; Month: mApril; Name: SPSCHolNetherlandsApr30th),
    (Kind: dckDate; Day: 5; Month: mMay; Name: SPSCHolNetherlandsMay5th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolNetherlandsDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolNetherlandsDec26th)
    );

  cPSCN_ZealandCount = 7;
  cPSCN_Zealand: Array[0..cPSCN_ZealandCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolNewZealand1stJan),
    (Kind: dckDate; Day: 6; Month: mFebruary; Name: SPSCHolNewZealandFeb6th),
    (Kind: dckDate; Day: 25; Month: mApril; Name: SPSCHolNewZealand25thApr),
    (Kind: dckDayNumber; Day: 1; Month: mJune; WeekDay: dwMonday; Name:
      SPSCHolNewZealand1stMonJun),
    (Kind: dckDayNumber; Day: 4; Month: mOctober; WeekDay: dwMonday; Name:
      SPSCHolNewZealand_4thMonOct),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolNewZealand25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolNewZealand26thDec)
    );

  cPSCNorwayCount = 5;
  cPSCNorway: Array[0..cPSCNorwayCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolNorwayJan1st),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolNorwayMay1st),
    (Kind: dckDate; Day: 17; Month: mMay; Name: SPSCHolNorwayMay17th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolNorwayDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolNorwayDec26th)
    );

  cPSCPortugalCount = 10;
  cPSCPortugal: Array[0..cPSCPortugalCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolPortugalJan1st),
    (Kind: dckDate; Day: 25; Month: mApril; Name: SPSCHolPortugalApr25th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolPortugalMay1st),
    (Kind: dckDate; Day: 10; Month: mJune; Name: SPSCHolPortugalJun10th),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolPortugalAug15th),
    (Kind: dckDate; Day: 5; Month: mOctober; Name: SPSCHolPortugalOct5th),
    (Kind: dckDate; Day: 1; Month: mNovember; Name: SPSCHolPortugalNov1st),
    (Kind: dckDate; Day: 1; Month: mDecember; Name: SPSCHolPortugalDec1st),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolPortugalDec8th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolPortugalDec25th)
    );

  cPSCS_AfricaCount = 8;
  cPSCS_Africa: Array[0..cPSCS_AfricaCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolSouthAfrica1stJan),
    (Kind: dckDate; Day: 6; Month: mApril; Name: SPSCHolSouthAfricaApr6th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolSouthAfricaMay1st),
    (Kind: dckDate; Day: 31; Month: mMay; Name: SPSCHolSouthAfricaMay31st),
    (Kind: dckDate; Day: 10; Month: mOctober; Name: SPSCHolSouthAfricaOct10th),
    (Kind: dckDate; Day: 16; Month: mDecember; Name: SPSCHolSouthAfricaDec16th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolSouthAfrica25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolSouthAfricaDec26th)
    );

  cPSCSpainCount = 30;
  cPSCSpain: Array[0..cPSCSpainCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolSpainJan1st),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolSpainJan6th),
    (Kind: dckDate; Day: 23; Month: mJanuary; Name: SPSCHolSpainJan23rd),
    (Kind: dckDate; Day: 28; Month: mFebruary; Name: SPSCHolSpainFeb28th),
    (Kind: dckDate; Day: 19; Month: mMarch; Name: SPSCHolSpainMar19th),
    (Kind: dckDate; Day: 2; Month: mApril; Name: SPSCHolSpainApr2nd),
    (Kind: dckDate; Day: 23; Month: mApril; Name: SPSCHolSpainApr23rd),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolSpainMay1st),
    (Kind: dckDate; Day: 2; Month: mMay; Name: SPSCHolSpainMay2nd),
    (Kind: dckDate; Day: 15; Month: mMay; Name: SPSCHolSpainMay15th),
    (Kind: dckDate; Day: 30; Month: mMay; Name: SPSCHolSpainMay30th),
    (Kind: dckDate; Day: 31; Month: mMay; Name: SPSCHolSpainMay31st),
    (Kind: dckDate; Day: 9; Month: mJune; Name: SPSCHolSpainJun9th),
    (Kind: dckDate; Day: 13; Month: mJune; Name: SPSCHolSpainJun13th),
    (Kind: dckDate; Day: 7; Month: mJuly; Name: SPSCHolSpainJul7th),
    (Kind: dckDate; Day: 25; Month: mJuly; Name: SPSCHolSpainJul25th),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolSpainAug15th),
    (Kind: dckDate; Day: 25; Month: mAugust; Name: SPSCHolSpainAug25th),
    (Kind: dckDate; Day: 8; Month: mSeptember; Name: SPSCHolSpainSep8th),
    (Kind: dckDate; Day: 9; Month: mSeptember; Name: SPSCHolSpainSep9th),
    (Kind: dckDate; Day: 13; Month: mSeptember; Name: SPSCHolSpainSep13th),
    (Kind: dckDate; Day: 21; Month: mSeptember; Name: SPSCHolSpainSep21st),
    (Kind: dckDate; Day: 24; Month: mSeptember; Name: SPSCHolSpainSep24th),
    (Kind: dckDate; Day: 9; Month: mOctober; Name: SPSCHolSpainOct9th),
    (Kind: dckDate; Day: 12; Month: mOctober; Name: SPSCHolSpainOct12th),
    (Kind: dckDate; Day: 15; Month: mOctober; Name: SPSCHolSpainOct15th),
    (Kind: dckDate; Day: 7; Month: mNovember; Name: SPSCHolSpainNov7th),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolSpainDec8th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolSpainDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolSpainDec26th)
    );

  cPSCSwedenCount = 9;
  cPSCSweden: Array[0..cPSCSwedenCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolSwedenJan1st),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolSwedenJan6th),
    (Kind: dckDate; Day: 1; Month: mMay; Name: SPSCHolSwedenMay1st),
    (Kind: dckDayNumber; Day: - 1; Month: mMay; WeekDay: dwSunday; Name:
      SPSCHolSwedenLastSnMay),
    (Kind: dckDate; Day: 24; Month: mJune; Name: SPSCHolSwedenJun24th),
    (Kind: dckDate; Day: 24; Month: mOctober; Name: SPSCHolSwedenOct24th),
    (Kind: dckDayNumber; Day: 2; Month: mNovember; WeekDay: dwSunday; Name:
      SPSCHolSweden2ndSnNov),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolSwedenDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolSwedenDec26th)
    );

  cPSCSwitzerlandCount = 11;
  cPSCSwitzerland: Array[0..cPSCSwitzerlandCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolSwitzerlandJan1st),
    (Kind: dckDate; Day: 2; Month: mJanuary; Name: SPSCHolSwitzerlandJan2nd),
    (Kind: dckDate; Day: 6; Month: mJanuary; Name: SPSCHolSwitzerlandJan6th),
    (Kind: dckDate; Day: 24; Month: mJanuary; Name: SPSCHolSwitzerlandJan24th),
    (Kind: dckDate; Day: 1; Month: mMarch; Name: SPSCHolSwitzerlandMar1st),
    (Kind: dckDate; Day: 1; Month: mAugust; Name: SPSCHolSwitzerlandAug1st),
    (Kind: dckDate; Day: 15; Month: mAugust; Name: SPSCHolSwitzerlandAug15th),
    (Kind: dckDate; Day: 8; Month: mDecember; Name: SPSCHolSwitzerlandDec8th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolSwitzerlandDec25th),
    (Kind: dckDate; Day: 26; Month: mDecember; Name: SPSCHolSwitzerlandDec26th),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolSwitzerlandDec31st)
    );

  cPSCUKCount = 9;
  cPSCUK: Array[0..cPSCUKCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolUnitedKingdom1stJan),
    (Kind: dckDayNumber; Day: 1; Month: mMay; WeekDay: dwMonday; Name:
      SPSCHolUnitedKingdom1stMonMay),
    (Kind: dckDayNumber; Day: - 1; Month: mMay; WeekDay: dwMonday; Name:
      SPSCHolUnitedKingdomLastMonMay),
    (Kind: dckDayNumber; Day: 2; Month: mJuly; WeekDay: dwMonday; Name:
      SPSCHolUnitedKingdom2ndMonJul),
    (Kind: dckDayNumber; Day: - 1; Month: mAugust; WeekDay: dwMonday; Name:
      SPSCHolUnitedKingdomLastMonAug),
    (Kind: dckDate; Day: 24; Month: mDecember; Name:
      SPSCHolUnitedKingdomDec24th),
    (Kind: dckDate; Day: 25; Month: mDecember; Name:
      SPSCHolUnitedKingdom25thDec),
    (Kind: dckDate; Day: 26; Month: mDecember; Name:
      SPSCHolUnitedKingdom26thDec),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolUnitedKingdomDec31st)
    );

  cPSCUSCount = 24;
  cPSCUS: Array[0..cPSCUSCount - 1] Of TPSCDayConst =
  (
    (Kind: dckDate; Day: 1; Month: mJanuary; Name: SPSCHolUnitedStates1stJan),
    (Kind: dckDayNumber; Day: 3; Month: mJanuary; WeekDay: dwMonday; Name:
      SPSCHolUnitedStates3rdMonJan),
    (Kind: dckDate; Day: 2; Month: mFebruary; Name: SPSCHolUnitedStatesFeb2nd),
    (Kind: dckDate; Day: 12; Month: mFebruary; Name: SPSCHolUnitedStatesFeb12th),
    (Kind: dckDate; Day: 14; Month: mFebruary; Name: SPSCHolUnitedStatesFeb14th),
    (Kind: dckDayNumber; Day: 3; Month: mFebruary; WeekDay: dwMonday; Name:
      SPSCHolUnitedStates3rdMonFeb),
    (Kind: dckDate; Day: 22; Month: mFebruary; Name: SPSCHolUnitedStatesFeb22nd),
    (Kind: dckDate; Day: 17; Month: mMarch; Name: SPSCHolUnitedStatesMar17th),
    (Kind: dckDate; Day: 1; Month: mApril; Name: SPSCHolUnitedStatesApr1st),
    (Kind: dckDayNumber; Day: 1; Month: mApril; WeekDay: dwSunday; Name:
      SPSCHolUnitedStates1stSnApr),
    (Kind: dckDayNumber; Day: 2; Month: mMay; WeekDay: dwSunday; Name:
      SPSCHolUnitedStates2ndSnMay),
    (Kind: dckDayNumber; Day: 3; Month: mMay; WeekDay: dwSaturday; Name:
      SPSCHolUnitedStates3rdStMay),
    (Kind: dckDayNumber; Day: - 1; Month: mMay; WeekDay: dwMonday; Name:
      SPSCHolUnitedStatesLastMonMay),
    (Kind: dckDate; Day: 14; Month: mJune; Name: SPSCHolUnitedStatesJun14th),
    (Kind: dckDayNumber; Day: 3; Month: mJune; WeekDay: dwSunday; Name:
      SPSCHolUnitedStates3rdSnJun),
    (Kind: dckDate; Day: 4; Month: mJuly; Name: SPSCHolUnitedStatesJul4th),
    (Kind: dckDayNumber; Day: 1; Month: mSeptember; WeekDay: dwMonday; Name:
      SPSCHolUnitedStates1stMonSep),
    (Kind: dckDayNumber; Day: 2; Month: mOctober; WeekDay: dwMonday; Name:
      SPSCHolUnitedStates2ndMonOct),
    (Kind: dckDayNumber; Day: - 1; Month: mOctober; WeekDay: dwSunday; Name:
      SPSCHolUnitedStatesLastSnOct),
    (Kind: dckDate; Day: 31; Month: mOctober; Name: SPSCHolUnitedStatesOct31st),
    (Kind: dckDate; Day: 11; Month: mNovember; Name: SPSCHolUnitedStatesNov11th),
    (Kind: dckDayNumber; Day: 4; Month: mNovember; WeekDay: dwThursday; Name:
      SPSCHolUnitedStates4thThuNov),
    (Kind: dckDate; Day: 25; Month: mDecember; Name: SPSCHolUnitedStates25thDec),
    (Kind: dckDate; Day: 31; Month: mDecember; Name: SPSCHolUnitedStatesDec31st)
    );

{------------------------------------------------------------------------------}

type
  TPSCCountryHolidays = class
  public
    Country: TPSCCountryID;
    Holidays: Array of TPSCDayConst;
  End;

  TPSCHolidaysCriteria=class(TInterfacedObject,IPSCCompareObjects)
  private
    function CompareObjects(AObject1,AObject2:TObject):Integer;
  end;

  TPSCHolidays = class(TInterfacedObject,IPSCHolidays)
  private
    FList:IPSCObjectList;

    function FindCountryHolidays(ACountryID:TPSCCountryID):TPSCCountryHolidays;

    Procedure EnumCountryHolidays(ACountryID:TPSCCountryID;
      const ABaseDate:TDateTime;AProc:TPSCEnumCountryHolidaysProc);
    function EnumCountryHolidaysForDate(ACountryID:TPSCCountryID;
      const ADate:TDateTime;const ANames:IPSCStrings):Boolean;
    Procedure SetCountryHolidays(ACountry: TPSCCountryID;
      const AHolidays: Array of TPSCDayConst);
    Function AnyHolidays(ACountryID:TPSCCountryID):Boolean;
  public
    constructor Create;
  end;

{------------------------------------------------------------------------------}

Function TPSCHolidays.AnyHolidays(ACountryID:TPSCCountryID):Boolean;
begin
  Result:=FindCountryHolidays(ACountryID)<>nil;
end;

{------------------------------------------------------------------------------}

constructor TPSCHolidays.Create;
begin
  inherited;
  FList:=PSCCreateObjectList(ioOwned);
  FList.SetSortCriteria(TPSCHolidaysCriteria.Create);
end;

{------------------------------------------------------------------------------}

Procedure TPSCHolidays.SetCountryHolidays(ACountry: TPSCCountryID;
  const AHolidays: Array of TPSCDayConst);
var
  MyItem:TPSCCountryHolidays;
  MyIndex:Integer;
  i:Integer;
begin
  MyItem:=TPSCCountryHolidays.Create;
  MyItem.Country:=ACountry;
  If FList.Find(MyItem,MyIndex) then
    begin
      MyItem.Free;
      MyItem:=TPSCCountryHolidays(FList.Items[MyIndex]);
    end
  else
    FList.Add(MyItem);

  SetLength(MyItem.Holidays,Length(AHolidays));
  for i:=Low(AHolidays) to High(AHolidays) do
    MyItem.Holidays[i]:=AHolidays[i];
end;

{------------------------------------------------------------------------------}

function TPSCHolidaysCriteria.CompareObjects(AObject1,AObject2:TObject):Integer;
begin
  Result:=TPSCCountryHolidays(AObject1).Country-
    TPSCCountryHolidays(AObject2).Country;
end;

{------------------------------------------------------------------------------}

Function PSCGetHolidayHint(Const CountryName,HolidayName: String): String;
Begin
  Result := PSCFormat('%s: %s', [CountryName,HolidayName]); //don't resource
End;

{------------------------------------------------------------------------------}

Function PSCAddHolidays(ACountryRec: TPSCCountryHolidays; Const ADate: TDateTime;
  const HolidayList: IPSCStrings): boolean;
Var
  i: integer;
  CountryName: String;
Begin
  Result := False;
  With ACountryRec Do
    Begin
      CountryName := PSCCountryIDToName(Country);
      For i := Low(Holidays) To High(Holidays) Do
        If PSCDatesCompare(PSCDayConstToDateTime(ADate,Holidays[i]),ADate,cpkDate) = 0
          Then
          Begin
            Result := True;
            If HolidayList <> Nil Then
              HolidayList.Add(PSCGetHolidayHint(CountryName,Holidays[i].Name))
            Else
              exit;
          End;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCHolidays.FindCountryHolidays(ACountryID:TPSCCountryID):TPSCCountryHolidays;
var
  MyIndex:Integer;
begin
  Result:=TPSCCountryHolidays.Create;
  Result.Country:=ACountryID;
  If FList.Find(Result,MyIndex) then
    begin
      Result.Free;
      Result:=TPSCCountryHolidays(FList.Items[MyIndex]);
    end
  else
    begin
      Result.Free;
      Result:=nil;
    end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCHolidays.EnumCountryHolidays(ACountryID:TPSCCountryID;
  const ABaseDate:TDateTime;AProc:TPSCEnumCountryHolidaysProc);
Var
  i: Integer;
  MyDate:TDateTime;
  MyHolidays:TPSCCountryHolidays;
begin
  MyHolidays:=FindCountryHolidays(ACountryID);
  If MyHolidays<>nil Then
    With MyHolidays do
      For i := Low(Holidays) To High(Holidays) Do
      begin
        MyDate:=PSCDayConstToDateTime(ABaseDate,Holidays[i]);
        AProc(Holidays[i].Name,MyDate);
      end;
end;

{------------------------------------------------------------------------------}

function TPSCHolidays.EnumCountryHolidaysForDate(ACountryID:TPSCCountryID;
  const ADate:TDateTime;const ANames:IPSCStrings):Boolean;
var
  MyHolidays:TPSCCountryHolidays;
begin
  MyHolidays:=FindCountryHolidays(ACountryID);
  If MyHolidays<>nil Then
    Result := PSCAddHolidays(MyHolidays,ADate,ANames)
  Else
    Result := False;
end;

{------------------------------------------------------------------------------}

Var
  FPSCHolidayList: IPSCHolidays;

{------------------------------------------------------------------------------}

Function PSCGetHolidays: IPSCHolidays;

  Procedure Initialize;
  Begin
    FPSCHolidayList := TPSCHolidays.Create;

    With FPSCHolidayList do
    begin
      SetCountryHolidays(cPSCCountryGreece,        cPSCGreece);
      SetCountryHolidays(cPSCCountryCzech,         cPSCCzech);
      SetCountryHolidays(cPSCCountryHungary,       cPSCHungary);
      SetCountryHolidays(cPSCCountryAustralia,     cPSCAustralia);
      SetCountryHolidays(cPSCCountryAustria,       cPSCAustria);
      SetCountryHolidays(cPSCCountryBelgium,       cPSCBelgium);
      SetCountryHolidays(cPSCCountryBrazil,        cPSCBrazil);
      SetCountryHolidays(cPSCCountryCanada,        cPSCCanada);
      SetCountryHolidays(cPSCCountryDenmark,       cPSCDenmark);
      SetCountryHolidays(cPSCCountryFinland,       cPSCFinland);
      SetCountryHolidays(cPSCCountryFrance,        cPSCFrance);
      SetCountryHolidays(cPSCCountryGermany,       cPSCGermany);
      SetCountryHolidays(cPSCCountryIreland,       cPSCIreland);
      SetCountryHolidays(cPSCCountryItaly,         cPSCItaly);
      SetCountryHolidays(cPSCCountryJapan,         cPSCJapan);
      SetCountryHolidays(cPSCCountryNetherlands,   cPSCNetherlands);
      SetCountryHolidays(cPSCCountryN_Zealand,     cPSCN_Zealand);
      SetCountryHolidays(cPSCCountryNorway,        cPSCNorway);
      SetCountryHolidays(cPSCCountryPortugal,      cPSCPortugal);
      SetCountryHolidays(cPSCCountrySouth_Africa,  cPSCS_Africa);
      SetCountryHolidays(cPSCCountrySpain,         cPSCSpain);
      SetCountryHolidays(cPSCCountrySweden,        cPSCSweden);
      SetCountryHolidays(cPSCCountrySwitzerland,   cPSCSwitzerland);
      SetCountryHolidays(cPSCCountryUK,            cPSCUK);
      SetCountryHolidays(cPSCCountryUS,            cPSCUS);
    end;
  End;

Begin
  If FPSCHolidayList = Nil Then
    Initialize;
  Result := FPSCHolidayList;
End;

{------------------------------------------------------------------------------}

procedure PSCEnumCountriesWithHolidays(const S:IPSCStrings);
var
  i:Integer;
begin
  for i:=Low(cPSCCountryArray) to High(cPSCCountryArray) do
    If PSCGetHolidays.AnyHolidays(cPSCCountryArray[i]) then
      S.Add(PSCCountryIDToName(cPSCCountryArray[i]));
end;

{------------------------------------------------------------------------------}

end.
