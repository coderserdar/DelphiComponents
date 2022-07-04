{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  This component allow translate a number into verbal string.
  For using you need:
   - drop it on form
   - define a desirable number in Value property, which you want translate into verbal string

  Also you can define in properties:
   - Currency: currency for translation
                 1. ukrainian UAH
                 2. ukrainian karbobanets
                 3. US dollars
                 4. russian rubles
                 5. deutsche marks
                 6. british pounds
                 7. euro
                 8. Rands (South Africa)
                 9. Belgian Franc
                10. Dominican Republic peso
                11. Rupiah
   - Language: language for translation
                 1. ukrainian
                 2. russian
                 3. english
                 4. german
                 5. spain
                 6. Indonesia
   - Expand:
       exWholeSum: True - expand in verbal string a whole part of number ('fourty five/45')
       exWholeCurrency: True - expand in verbal string a currency for whole part of number ('rubles/rub')
       exFractionSum: True - expand in verbal string a fractioanl part of number ('ten/10')
       exFractionCurrency: True - expand a currency for fractional part of number ('centes/cnt')
   - CharCase:
       ccLower: all characters in result string is lower
       ccProper: first character is upper, all rest is lower
       ccUpper: all characters in result string is upper

  Also you can use it without component creation thought
  run-time call a function, which will result a verbal string:
  function GetMoneyStr(Value: Double; Expand: TExpand; Language: TLanguage; Currency: TCurrency; CharCase: TCharCase): string;

  Big thanks to Lars Paprotny (paprotny@gkss.de, paprotny@gmx.de).
  Without Lars help I couldn't create a english and german language support.

  If anybody want to help me in new languages support then send
  a message to me. But component is freeware and I cann't offer a fee.

}

unit MoneyStr;

interface

uses
  Classes, StdCtrls;

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

type
  TExpand = set of (exWholeSum, exWholeCurrency, exFractionSum, exFractionCurrency);
  TCurrency = (tcUAH, tcKarbovanec, tcUSD, tcRUR, tcDM, tcPound, tcEuro, tsRand, tsBelgianFranc, tsPeso, tsRupiah);
  TLanguage = (tlUkrainian, tlRussian, tlEnglish, tlGerman, tlSpain, tlIndonesia);
  TCharCase = (ccLower, ccProper, ccUpper);

const Thousands: array[TLanguage] of array[1..4, 1..3] of string =
      ((('тис€ча',   'тис€чi',    'тис€ч'),
        ('мiльйон',  'мiльйони',  'мiльйонiв'),
        ('мiлiард',  'мiлiарди',  'мiлiардiв'),
        ('трилiон',  'трилiони',  'трилiонiв')),

       (('тыс€ча',   'тыс€чи',    'тыс€ч'),
        ('миллион',  'миллиона',  'миллионов'),
        ('миллиард', 'миллиарда', 'миллиардов'),
        ('триллион', 'триллиона', 'триллионов')),

       (('thousand', 'thousand',  'thousands'),
        ('million',  'million',   'millions'),
        ('billion',  'billion',   'billions'),
        ('trillion', 'trillion',  'trillions')),

       (('Tausend',  'Tausend',   'Tausend'),
        ('Million',  'Million',   'Millionen'),
        ('Milliarde',  'Milliarde',   'Milliarden'),
        ('Billion', 'Billion',  'Billionen')),

       (('mil',      'mil',       'mil'),
        ('millуn',   'millуn',    'millуn'),
        ('billуn',   'billуn',    'billуn'),
        ('trillon',  'trillon',   'trillon')),

       (('ribu',     'ribu',      'ribu'),
        ('juta',     'juta',      'juta'),
        ('milyar',   'milyar',    'milyar'),
        ('trilyun',  'trilyun',   'trilyun')));

const ShortCurrency: array[TLanguage] of array[0..10, 1..2] of string =
      ((('грн', 'коп'),
        ('крб', 'коп'),
        ('дол', 'цнт'),
        ('руб', 'коп'),
        ('мар', 'пфн'),
        ('фнт', 'пнс'),
        ('евр', 'цнт'),
        ('рнд', 'цнт'),
        ('бф',  ''),
        ('пес', 'сен'),
        ('Rp.', 'sen')),

       (('грн', 'коп'),
        ('крб', 'коп'),
        ('дол', 'цнт'),
        ('руб', 'коп'),
        ('мар', 'пфн'),
        ('фнт', 'пнс'),
        ('евр', 'цнт'),
        ('рнд', 'цнт'),
        ('бф',  ''),
        ('пес', 'сен'),
        ('Rp.', 'sen')),

       (('grn', 'cop'),
        ('krb', 'cop'),
        ('$',   'cen'),
        ('rub', 'cop'),
        ('dm',  'pfg'),
        ('pnd', 'p'),
        ('eur', 'cen'),
        ('R',   'c'),
        ('BF',  ''),
        ('RD', 'cts'),
        ('Rp.', 'sen')),

       (('grn', 'cop'),
        ('krb', 'cop'),
        ('$',   'cen'),
        ('rub', 'cop'),
        ('dm',  'pfg'),
        ('pnd', 'p'),
        ('eur', 'cen'),
        ('R',   'c'),
        ('BF',  ''),
        ('RD', 'cts'),
        ('Rp.', 'sen')),

       (('grn', 'cop'),
        ('krb', 'cop'),
        ('$',   'cen'),
        ('rub', 'cop'),
        ('dm',  'pfg'),
        ('pnd', 'p'),
        ('eur', 'cen'),
        ('R',   'c'),
        ('BF',  ''),
        ('RD', 'cts'),
        ('Rp.', 'sen')),

       (('grn', 'cop'),
        ('krb', 'cop'),
        ('$',   'cen'),
        ('rub', 'cop'),
        ('dm',  'pfg'),
        ('pnd', 'p'),
        ('eur', 'cen'),
        ('R',   'c'),
        ('BF',  ''),
        ('RD', 'cts'),
        ('Rp.', 'sen')));

const FullCurrency: array[TLanguage] of array[0..10, 1..6] of string =
      ((('гривн€',      'гривнi',      'гривень',     'копiйка', 'копiйки',  'копiйок'),
        ('карбованець', 'карбованц€',  'карбованцев', 'копiйка', 'копiйки',  'копiйок'),
        ('долар',       'долара',      'доларiв',     'цент',    'цента',    'центiв'),
        ('рубль',       'рубл€',       'рублiв',      'копiйка', 'копiйки',  'копiйок'),
        ('марка',       'марки',       'марок',       'пфенiг',  'пфенiга',  'пфенiгов'),
        ('фунт',        'фунта',       'фунтiв',      'пенс',    'пенса',    'пенсiв'),
        ('евро',        'евро',        'евро',        'цент',    'цента',    'центiв'),
        ('ренд',        'ренда',       'рендов',      'цент',    'цента',    'центiв'),
        ('франк',       'франка',      'франков',     '',        '',         ''),
        ('песо',        'песо',        'песо',        'сентаво', 'сентаво',  'сентаво'),
        ('rupiah',      'rupiah',      'rupiah',      'sen',     'sen',      'sen')),

       (('гривн€',     'гривни',     'гривен',      'копейка', 'копейки',  'копеек'),
        ('карбованец', 'карбованца', 'карбованцев', 'копейка', 'копейки',  'копеек'),
        ('доллар',     'доллара',    'долларов',    'цент',    'цента',    'центов'),
        ('рубль',      'рубл€',      'рублей',      'копейка', 'копейки',  'копеек'),
        ('марка',      'марки',      'марок',       'пфениг',  'пфенига',  'пфенигов'),
        ('фунт',       'фунта',      'фунтов',      'пенс',    'пенса',    'пенсов'),
        ('евро',       'евро',       'евро',        'цент',    'цента',    'центов'),
        ('ренд',       'ренда',      'рендов',      'цент',    'цента',    'центiв'),
        ('франк',      'франка',     'франков',     '',        '',         ''),
        ('песо',       'песо',       'песо',        'сентаво', 'сентаво',  'сентаво'),
        ('rupiah',      'rupiah',      'rupiah',      'sen',     'sen',      'sen')),

       (('gruvnya',    'gruvnya',    'gruvnies',    'kopeck',  'kopeck',   'kopecks'),
        ('karbovanec', 'karbovanec', 'karbovanecs', 'kopeck',  'kopeck',   'kopecks'),
        ('dollar',     'dollar',     'dollars',     'cent',    'cent',     'cents'),
        ('ruble',      'ruble',      'rubles',      'kopeck',  'kopeck',   'kopecks'),
        ('deutsche mark', 'deutsche mark', 'deutsche marks', 'pfennig', 'pfennig', 'pfennigs'),
        ('pound',      'pound',      'pounds',      'penny',   'penny',    'pence'),
        ('euro',       'euro',       'euros',       'cent',    'cent',     'cents'),
        ('Rand',       'Rand',       'Rand',        'cent',    'cent',     'cents'),
        ('Belgian Franc',       'Belgian Franc',      'Belgian Francs',     '',        '',         ''),
        ('peso',       'peso',       'pesos',       'centavo', 'centavo',  'centavos'),
        ('rupiah',      'rupiah',      'rupiahs',      'sen',     'sen',      'sens')),

       (('gruvnya',    'gruvnya',    'gruvnies',    'kopeck',  'kopeck',   'kopecks'),
        ('karbovanec', 'karbovanec', 'karbovanecs', 'kopeck',  'kopeck',   'kopecks'),
        ('dollar',     'dollar',     'dollars',     'cent',    'cent',     'cents'),
        ('ruble',      'ruble',      'rubles',      'kopeck',  'kopeck',   'kopecks'),
        ('Deutsche Mark', 'Deutsche Mark', 'Deutsche Marks', 'Pfennig', 'Pfennig', 'Pfennige'),
        ('pound',      'pound',      'pounds',      'penny',   'penny',    'pence'),
        ('euro',       'euro',       'euros',       'cent',    'cent',     'cents'),
        ('Rand',       'Rand',       'Rand',        'cent',    'cent',     'cents'),
        ('Belgian Franc',       'Belgian Franc',      'Belgian Francs',     '',        '',         ''),
        ('peso',       'peso',       'pesos',       'centavo', 'centavo',  'centavos'),
        ('rupiah',      'rupiah',      'rupiah',      'sen',     'sen',      'sen')),

       (('gruvnya',    'gruvnya',    'gruvnies',    'kopeck',  'kopeck',   'kopecks'),
        ('karbovanec', 'karbovanec', 'karbovanecs', 'kopeck',  'kopeck',   'kopecks'),
        ('dollar',     'dollar',     'dollars',     'cent',    'cent',     'cents'),
        ('ruble',      'ruble',      'rubles',      'kopeck',  'kopeck',   'kopecks'),
        ('deutsche mark', 'deutsche mark', 'deutsche marks', 'pfennig', 'pfennig', 'pfennigs'),
        ('pound',      'pound',      'pounds',      'pence',   'pence',    'pences'),
        ('euro',       'euro',       'euros',       'cent',    'cent',     'cents'),
        ('Rand',       'Rand',       'Rand',        'cent',    'cent',     'cents'),
        ('Belgian Franc',       'Belgian Franc',      'Belgian Francs',     '',        '',         ''),
        ('peso',       'peso',       'pesos',       'centavo', 'centavo',  'centavos'),
        ('rupiah',      'rupiah',      'rupiah',      'sen',     'sen',      'sen')),

       (('gruvnya',    'gruvnya',    'gruvnies',    'kopeck',  'kopeck',   'kopecks'),
        ('karbovanec', 'karbovanec', 'karbovanecs', 'kopeck',  'kopeck',   'kopecks'),
        ('dollar',     'dollar',     'dollars',     'cent',    'cent',     'cents'),
        ('ruble',      'ruble',      'rubles',      'kopeck',  'kopeck',   'kopecks'),
        ('deutsche mark', 'deutsche mark', 'deutsche marks', 'pfennig', 'pfennig', 'pfennigs'),
        ('pound',      'pound',      'pounds',      'penny',   'penny',    'pence'),
        ('euro',       'euro',       'euros',       'cent',    'cent',     'cents'),
        ('Rand',       'Rand',       'Rand',        'cent',    'cent',     'cents'),
        ('Belgian Franc',       'Belgian Franc',      'Belgian Francs',     '',        '',         ''),
        ('peso',       'peso',       'pesos',       'centavo', 'centavo',  'centavos'),
        ('rupiah',      'rupiah',      'rupiahs',      'sen',     'sen',      'sens')));

const Literal: array[TLanguage] of array[0..19, 1..4] of string =
      ((('',                '',              '',           'нуль'),
        ('одна',           'один',           '',           'сто'),
        ('двi',            'два',            'двадц€ть',   'двiстi'),
        ('три',            'три',            'тридц€ть',   'триста'),
        ('чотири',         'чотири',         'сорок',      'чотиреста'),
        ('п''€ть',         'п''€ть',         'п''€тдес€т', 'п''€тсот'),
        ('шiсть',          'шiсть',          'шiстдес€т',  'шiстьсот'),
        ('сiм',            'сiм',            'сiмдес€т',   'сiмсот'),
        ('вiсiм',          'вiсiм',          'вiсiмдес€т', 'вiсiмсот'),
        ('дев''€ть',       'дев''€ть',       'дев€носто',  'дев''€тсот'),
        ('дес€ть',         'дес€ть',         '',           ''),
        ('одинадц€ть',     'одинадц€ть',     '',           ''),
        ('дванадц€ть',     'дванадц€ть',     '',           ''),
        ('тринадц€ть',     'тринадц€ть',     '',           ''),
        ('чотирнадц€ть',   'чотирнадц€ть',   '',           ''),
        ('п''€тнадц€ть',   'п''€тнадц€ть',   '',           ''),
        ('шiстнадц€ть',    'шiстнадц€ть',    '',           ''),
        ('сiмнадц€ть',     'сiмнадц€ть',     '',           ''),
        ('вiсiмнадц€ть',   'вiсiмнадц€ть',   '',           ''),
        ('дев''€тнадц€ть', 'дев''€тнадц€ть', '',           '')),

       (('',              '',            '',           'ноль'),
        ('одна',         'один',         '',           'сто'),
        ('две',          'два',          'двадцать',   'двести'),
        ('три',          'три',          'тридцать',   'триста'),
        ('четыре',       'четыре',       'сорок',      'четыреста'),
        ('п€ть',         'п€ть',         'п€тьдес€т',   'п€тьсот'),
        ('шесть',        'шесть',        'шестьдес€т',  'шестьсот'),
        ('семь',         'семь',         'семьдес€т',   'семьсот'),
        ('восемь',       'восемь',       'восемьдес€т', 'восемьсот'),
        ('дев€ть',       'дев€ть',       'дев€носто',  'дев€тьсот'),
        ('дес€ть',       'дес€ть',       '',           ''),
        ('одинадцать',   'одинадцать',   '',           ''),
        ('двенадцать',   'двенадцать',   '',           ''),
        ('тринадцать',   'тринадцать',   '',           ''),
        ('четырнадцать', 'четырнадцать', '',           ''),
        ('п€тнадцать',   'п€тнадцать',   '',           ''),
        ('шестнадцать',  'шестнадцать',  '',           ''),
        ('семнадцать',   'семнадцать',   '',           ''),
        ('восемнадцать', 'восемнадцать', '',           ''),
        ('дев€тнадцать', 'дев€тнадцать', '',           '')),

       (('',              '',            '',           'zero'),
        ('one',          'one',         '',            'one hundred'),
        ('two',          'two',         'twenty',      'two hundred'),
        ('three',        'three',       'thirty',      'three hundred'),
        ('four',         'four',        'forty',       'four hundred'),
        ('five',         'five',        'fifty',       'five hundred'),
        ('six',          'six',         'sixty',       'six hundred'),
        ('seven',        'seven',       'seventy',     'seven hundred'),
        ('eight',        'eight',       'eighty',      'eight hundred'),
        ('nine',         'nine',        'ninety',      'nine hundred'),
        ('ten',          'ten',         '',            ''),
        ('eleven',       'eleven',      '',            ''),
        ('twelve',       'tweleve',     '',            ''),
        ('thirteen',     'thirteen',    '',            ''),
        ('fourteen',     'fourteen',    '',            ''),
        ('fifteen',      'fifteen',     '',            ''),
        ('sixteen',      'sixteen',     '',            ''),
        ('seventeen',    'seventeen',   '',            ''),
        ('eighteen',     'eighteen',    '',            ''),
        ('nineteen',     'nineteen',    '',            '')),

       (('',              '',            '',           'null'),
        ('eins',           'ein',         '',           'einhundert'),
        ('zwei',          'zwei',        'zwanzig',    'zweihundert'),
        ('drei',          'drei',        'dreissig',   'dreihundert'),
        ('vier',          'vier',        'vierzig',    'vierhundert'),
        ('fuenf',         'fuenf',       'fuenfzig',   'fuenfhundert'),
        ('sechs',         'sechs',       'sechzig',    'sechshundert'),
        ('sieben',        'sieben',      'siebzig',    'siebenhundert'),
        ('acht',          'acht',        'achtzig',    'achthundert'),
        ('neun',          'neun',        'neunzig',    'neunhundert'),
        ('zehn',          'zehn',        '',           ''),
        ('elf',           'elf',         '',           ''),
        ('zwoelf',        'zwoelf',      '',           ''),
        ('dreizehn',      'dreizehn',    '',           ''),
        ('vierzehn',      'vierzehn',    '',           ''),
        ('fuenfzehn',     'fuenfzehn',   '',           ''),
        ('sechzehn',      'sechzehn',    '',           ''),
        ('siebzehn',      'siebzehn',    '',           ''),
        ('achtzehn',      'achtzehn',    '',           ''),
        ('neunzehn',      'neunzehn',    '',           '')),

       (('',              '',            '',           'cero'),
        ('uno',           'una',         '',           'cien'),
        ('dos',           'dos',         'veinte',     'doscientos'),
        ('tres',          'tres',        'treinta',    'trescientos'),
        ('cuatro',        'cuatro',      'cuarenta',   'cuatrocientos'),
        ('cinco',         'cinco',       'cincuenta',  'quinientos'),
        ('seis',          'seis',        'sesenta',    'seiscientos'),
        ('siete',         'siete',       'setenta',    'setecientos'),
        ('ocho',          'ocho',        'ochenta',    'ochocientos'),
        ('nueve',         'nueve',       'noventa',    'novecientos'),
        ('diez',          'diez',        '',           ''),
        ('once',          'once',        '',           ''),
        ('doce',          'doce',        '',           ''),
        ('trece',         'trece',       '',           ''),
        ('catorce',       'catorce',     '',           ''),
        ('quince',        'quince',      '',           ''),
        ('diecisйis',     'diecisйis',   '',           ''),
        ('diecisiete',    'diecisiete',  '',           ''),
        ('dieciocho',     'dieciocho',   '',           ''),
        ('diecinueve',    'diecinueve',  '',           '')),

       (('',               '',               '',               'nol'),
        ('satu',           'satu',           '',               'seratus'),
        ('dua',            'dua',            'dua puluh',      'dua ratus'),
        ('tiga',           'tiga',           'tiga puluh',     'tiga ratus'),
        ('empat',          'empat',          'empat puluh',    'empat ratus'),
        ('lima',           'lima',           'lima puluh',     'lima ratus'),
        ('enam',           'enam',           'enam puluh',     'enam ratus'),
        ('tujuh',          'tujuh',          'tujuh puluh',    'tujuh ratus'),
        ('delapan',        'delapan',        'delapan puluh',  'delapan ratus'),
        ('sembilan',       'sembilan',       'sembilan puluh', 'sembilan ratus'),
        ('sepuluh',        'sepuluh',        '',               ''),
        ('sebelas',        'sebelas',        '',               ''),
        ('dua belas',      'dua belas',      '',               ''),
        ('tiga belas',     'tiga belas',     '',               ''),
        ('empat belas',    'empat belas',    '',               ''),
        ('lima belas',     'lima belas',     '',               ''),
        ('enam belas',     'enam belas',     '',               ''),
        ('tujuh belas',    'tujuh belas',    '',               ''),
        ('delapan belas',  'delapan belas',  '',               ''),
        ('sembilan belas', 'sembilan belas', '',               '')));

type
  TMoneyString = class(TCustomLabel)
  private
    { Private declarations }
    FCharCase: TCharCase;
    FValue: Double;
    FCurrency: TCurrency;
    FLanguage: TLanguage;
    FExpand: TExpand;

    procedure BuildMoneyString;
  protected
    { Protected declarations }
    procedure SetCharCase(Val: TCharCase);
    procedure SetValue(Val: Double);
    procedure SetCurrency(Val: TCurrency);
    procedure SetLanguage(Val: TLanguage);
    procedure SetExpand(Val: TExpand);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: Double read FValue write SetValue;
    property Currency: TCurrency read FCurrency write SetCurrency default tcUSD;
    property Language: TLanguage read FLanguage write SetLanguage default tlEnglish;
    property Expand: TExpand read FExpand write SetExpand;
    property CharCase: TCharCase read FCharCase write SetCharCase;

    property Align;
    property Alignment;
//    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

function GetMoneyStr(Value: Double; Expand: TExpand; Language: TLanguage; Currency: TCurrency; CharCase: TCharCase): string;

implementation
uses SysUtils;

function GetDeclension(Digit: Char): Integer;
begin
  Result := 3;
  if (Digit = '1') then
    Result := 1
  else
    if {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Digit {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}['2', '3', '4']) then
      Result := 2
end;

function AddSpaces(s1, s2, s3, s4: string): string;
begin
  Result := '';
  if s1 <> '' then Result := Result + s1 + ' ';
  if s2 <> '' then Result := Result + s2 + ' ';
  if s3 <> '' then Result := Result + s3 + ' ';
  if s4 <> '' then Result := Result + s4 + ' ';
end;

function GetMoneyString(IntPart: Boolean; strValue: string; Expand: TExpand; Language: TLanguage; Currency: TCurrency): string;
var
  strRevert, PP: string;
  PK: array[1..3] of string;
  DL, k, PG: Integer;
  N: Double;
begin
  strRevert := '000000000000000';

  DL := Length(strValue);
// claculate amount of triads in number
  if (DL mod 3 = 0) then
    N := DL/3
  else
    N := DL/3 + 1;
  N := Int(N);

// create a reverse string with additional '0' in right (up to 15 characters)
  k := 1;
  while (k <= 255) do
  begin
    if (k <= DL) then
      strRevert[k] := strValue[DL - k + 1]
    else
      strRevert[k] := '0';
    Inc(k)
  end;

  Result := '';
  if ((IntPart = True) and (exWholeSum in Expand)) or
     ((IntPart = False) and (exFractionSum in Expand)) then
  begin
    { if number is zero }
    if (StrToInt(strValue) = 0) then
      Result := Literal[Language][0, 4] + ' '
    else
    begin
      k := 0;
      while (k < N) do
      begin
        PP := '';
        PK[1] := '';
        PK[2] := '';
        PK[3] := '';

        if (strRevert[3 + 3*k] <> '0') then
          PK[1] := Literal[Language][StrToInt(strRevert[3+3*k]), 4]
        else
          PK[1] := '';

        if (strRevert[2 + 3*k] <> '0') then
        begin
          if (strRevert[2 + 3*k] = '1') then
          begin
            PK[2] := Literal[Language][StrToInt(strRevert[1+3*k])+10, 2];
            PK[3] := '';
          end
          else
          begin
            PK[2] := Literal[Language][StrToInt(strRevert[2+3*k]), 3];
            if ((k = 0) or (k = 1)) and (Currency in [tcUAH, tcDM, tcEuro]) then
              PK[3] := Literal[Language][StrToInt(strRevert[1+3*k]), 1]
            else
              PK[3] := Literal[Language][StrToInt(strRevert[1+3*k]), 2]
          end
        end
        else
        begin
          PK[2] := '';
          if (strRevert[1+3*k] = '0') then
            PK[3] := ''
          else
            if ((k = 0) or (k = 1)) and
               (Currency in [tcUAH, tcDM, tcEuro]) then
              PK[3] := Literal[Language][StrToInt(strRevert[1+3*k]), 1]
            else
              PK[3] := Literal[Language][StrToInt(strRevert[1+3*k]), 2];
        end;

        if (Language = tlGerman) and
           (strRevert[2 + 3*k] > '1') then
        begin
          PK[2] := PK[3] + 'und' + PK[2];
          PK[3] := ''
        end;

        if k > 0 then
        begin
          if (strRevert[1 + 3*k] <> '0') or
             (strRevert[2 + 3*k] <> '0') or
             (strRevert[3 + 3*k] <> '0') then
          begin
            { if it isn't a first tens (11..19) }
            if (strRevert[2 + 3*k] <> '1') then
              PP := Thousands[Language][k, GetDeclension(strRevert[1 + 3*k])]
            else
              PP := Thousands[Language][k, 3];
          end
        end;
        Result := AddSpaces(PK[1], PK[2], PK[3], PP) + Result;
        Inc(k)
      end
    end
  end
  else
    Result := strValue + ' ';

  { add a currency }
  if strValue[DL - 1] = '1' then
    PG := 3
  else
    PG := GetDeclension(strValue[DL]);

  if (IntPart = True) then
  begin
    if (exWholeCurrency in Expand) then
      Result := Result + FullCurrency[Language][Ord(Currency), PG]
    else
      Result := Result + ShortCurrency[Language][Ord(Currency), 1] + '.';
  end
  else
  begin
    if (exFractionCurrency in Expand) then
      Result := Result + FullCurrency[Language][Ord(Currency), 3+PG]
    else
      Result := Result + ShortCurrency[Language][Ord(Currency), 2] + '.';
  end;
end;

function GetMoneyStr(Value: Double; Expand: TExpand; Language: TLanguage; Currency: TCurrency; CharCase: TCharCase): string;
var
  s: string;
begin
  s := GetMoneyString(True, IntToStr(Trunc(Value)), Expand, Language, Currency) +
       ' ' +
       GetMoneyString(False, IntToStr(Round(Frac(Value)*100)), Expand, Language, Currency);

  case CharCase of
    ccProper: s[1] := AnsiUpperCase(s[1])[1];
    ccUpper:  s := AnsiUpperCase(s);
  end;
  Result := s;
end;

{ TMoneyString }
constructor TMoneyString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCurrency := tcUSD;
  FLanguage := tlEnglish;

  WordWrap := True;
  AutoSize := False;

  FExpand := [exWholeSum, exWholeCurrency, exFractionSum, exFractionCurrency];
  Value := 1;
end;

procedure TMoneyString.BuildMoneyString;
begin
  Caption := GetMoneyStr(FValue, Expand, FLanguage, FCurrency, FCharCase);
end;

procedure TMoneyString.SetValue(Val: Double);
begin
  if (FValue <> Val) then
  begin
    FValue := Val;
    BuildMoneyString;
  end;
end;

procedure TMoneyString.SetCurrency(Val: TCurrency);
begin
  if (FCurrency <> Val) then
  begin
    FCurrency := Val;
    BuildMoneyString;
  end;
end;

procedure TMoneyString.SetLanguage(Val: TLanguage);
begin
  if (FLanguage <> Val) then
  begin
    FLanguage := Val;
    BuildMoneyString;
  end;
end;

procedure TMoneyString.SetExpand(Val: TExpand);
begin
  if (FExpand <> Val) then
  begin
    FExpand := Val;
    BuildMoneyString;
  end;
end;

procedure TMoneyString.SetCharCase(Val: TCharCase);
begin
  if (FCharCase <> Val) then
  begin
    FCharCase := Val;
    BuildMoneyString;
  end;
end;

end.
