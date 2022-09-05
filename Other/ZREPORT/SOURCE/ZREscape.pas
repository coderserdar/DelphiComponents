(*****************************************************************************)
(*                                                                           *)
(*  Special thanks to:                                                       *)
(*  - David Moretti (dave@rimini.com);                                       *)
(*  - Alexey Gorbunov - AlGo (algo_kov@chat.ru);                             *)
(*  - Paul Barry (barryp@itcarlow.ie)                                        *)
(*                                                                           *)
(*****************************************************************************)

unit ZREscape;

interface

{$I ZRDefine.inc}

uses
  Windows, Messages,                               // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes,                                         // Delphi VCL
  ZRStream;                                        // ZReport

type
  { Esc-codes }
  TZREscapeCode = (ecReset, ecFormFeed, ecPica, ecElite, ecCondensedOn, ecCondensedOff,
                   ecBoldOn, ecBoldOff, ecItalicOn, ecItalicOff, ecUnderlineOn, ecUnderlineOff,
                   ecSuperScriptOn, ecSuperScriptOff, ecSubScriptOn, ecSubScriptOff,
                   ecReportStart, ecReportFinish, ecPageStart, ecPageFinish);
  TZREscapeCodes = set of TZREscapeCode;

  { Esc-models }
  TZREscapeModel = (emCustom,                   emNone,                 emEpson,                        emCanonF60,
                    emCanonLaser,               emDiablo1600,           emEpsonCR420,                   emEpsonDXSeries,
                    emEpsonEX800,               emEpsonFXSeries,        emEpsonGQ3500,                  emEpsonHS80,
                    emEpsonJX80,                emEpsonLQSeries,        emEpsonLQ2550,                  emEpsonLXSeries,
                    emEpsonLettertype,          emEpsonMXSeries,        emEpsonRX80,                    emEpsonSQ2000,
                    emHPDeskJet,                emHPInkJet2225C,        emHPLaserJet,                   emHPLaserJet2000,
                    emHPLaserJet500Plus,        emHPLaserJet8LPI,       emHPLaserJetII,                 emHPLaserJetIID,
                    emHPLaserJetIIP,            emHPLaserJetLand,       emHPLaserJetLandscape8LPI,      emHPLaserJetPlus,
                    emHPLaserJetPort,           emHPLaserJetIIEuropA4,  emHPLaserJetIILegal,            emHPLaserJetIIDLongEdge,
                    emHPLaserJetIIDSEdge,       emHPThinkJet,           emIBM4216PersonalPageprinter,   emIBMColorJetPrinter,
                    emIBMPCCompact,             emIBMPCConvert,         emIBMPCGraphics,                emIBMProPrinter,
                    emIBMProPrinter2X24XL,      emIBMProPrinterIII,     emIBMQuickWriter,               emIBMQuietWriterIandII,
                    emIBMQuietWriterIII,        emIBMWheelPrinter,      emMannesmanTally,               emMannesmanTallyMT160,
                    emMannesmanTallyMT85MT86,   emMannesmanTallySpirit, emNEC2050,                      emNEC3500,
                    emNEC3515,                  emNEC3550,              emNEC7710,                      emNEC7730,
                    emNEC8023,                  emNEC8810,              emNEC8815,                      emNEC8850,
                    emNECElf350,                emNECElf360,            emNECElf370JR,                  emNECElf370NEC,
                    emNECPinwriter2200,         emNECPinwriterP2P32,    emNECPinwriterP2P33,            emNECPinwriterP3,
                    emNECPinwriterP5,           emNECPinwriterP6P7,     emNECPinwriterP9XL,             emNECPinwriterP5200,
                    emOkidata192193,            emOkidata292293,        emOkidata82,                    emOkidata82A,
                    emOkidata84,                emOkidata84Std,         emOkidata9293,                  emOkidata9293Std,
                    emOkidataMicroline182,      emOkidataMicro182Std,   emOkidataOkimate20,             emOkidataPacemark2350,
                    emOkidataPacemark2410,      emOkidataPace2410Std,   emOlympia,                      emOlympiaCompactRO,
                    emPanasonic3151,            emPanasonicE708,        emPanasonicIXP1080,             emPanasonicKXP1090,
                    emPanasonicKXP1091,         emPanasonicKXP1092,     emStarMicronicsNX10,            emStarMicronicsPlus,
                    emStarPowertypePM,          emStarPowertypeWP,      emStarRadixGemini10X15X,        emStarSDSGStarMode
                   );


  TZREscapeString     = String[32];
  TZREscapeStrings    = array[TZREscapeCode] of TZREscapeString;
  TZREscapeSpecifiers = array[TZREscapeCode] of Char;

const
  EscapeModelNames: array[TZREscapeModel] of String[64] = (
    '<custom>',                 '<none>',               'Default Epson',        'CanonF-60',
    'CanonLaser',               'Diablo1600',           'EpsonCR-420',          'EpsonDXSeries',
    'EpsonEX-800',              'EpsonFXSeries',        'EpsonGQ-3500',         'EpsonHS-80',
    'EpsonJX-80',               'EpsonLQSeries',        'EpsonLQ-2550(EpsonLQ)','EpsonLXSeries',
    'EpsonLettertype',          'EpsonMXSeries',        'EpsonRX-80',           'EpsonSQ2000',
    'HPDeskJet',                'HPInkJet2225C',        'HPLaserJet',           'HPLaserJet2000',
    'HPLaserJet500Plus',        'HPLaserJet8LPI',       'HPLaserJetII',         'HPLaserJetIID',
    'HPLaserJetIIP',            'HPLaserJetLand',       'HPLaserJetLands8LPI',  'HPLaserJetPlus',
    'HPLaserJetPort',           'HPLaserJetIIEuropA4',  'HPLaserJetIILegal',    'HPLaserJetIIDLongEdge',
    'HPLaserJetIIDSEdge',       'HPThinkJet',           'IBM4216PPageprinter',  'IBMColorJetPrinter',
    'IBMPCCompact',             'IBMPCConvert',         'IBMPCGraphics',        'IBMProPrinter',
    'IBMProPrinter2/XL24',      'IBMProPrinterIII',     'IBMQuickWriter',       'IBMQuietWriterI&II',
    'IBMQuietWriterIII',        'IBMWheelPrinter',      'MannesmanTally',       'MannesmanTallyMT160',
    'MannesmanTallyMT85/MT86',  'MannesmanTallySpirit', 'NEC2050',              'NEC3500',
    'NEC3515,5515',             'NEC3550',              'NEC7710,3510',         'NEC7730,3530',
    'NEC8023',                  'NEC8810,8830',         'NEC8815,8835',         'NEC8850',
    'NECElf350',                'NECElf360',            'NECElf370JR',          'NECElf370NEC',
    'NECPinwriter2200',         'NECPinwriterP2/P3-2',  'NECPinwriterP2/P3-3',  'NECPinwriterP3',
    'NECPinwriterP5',           'NECPinwriterP6/P7',    'NECPinwriterP9XL',     'NECPinwriterP5200',
    'Okidata192/193(IBM)',      'Okidata292/293(IBM)',  'Okidata82',            'Okidata82A',
    'Okidata84(IBM)',           'Okidata84(Standard)',  'Okidata92/93(IBM)',    'Okidata92/93(Std)',
    'OkidataMline182(IBM)',     'OkidataMicro182(Std)', 'OkidataOkimate20',     'OkidataPace2350(Std)',
    'OkidataPacemark2410',      'OkidataPacemark2410',  'Olympia',              'OlympiaCompactRO',
    'Panasonic3151',            'PanasonicE708',        'Panasonic-P1080(IBM)', 'PanasonicKX-P1090',
    'PanasonicKX-P1091',        'PanasonicKX-P1092',    'StarMicronicsNX-10',   'StarMicronicsPlus',
    'StarPowertypePM',          'StarPowertypeWP',      'StarRadix 10X/15X',    'StarSD/SGStarMode');

type
  { TZREscapes }
  TZREscapes = class(TPersistent)
  private
    fModel : TZREscapeModel;
    fValues: TZREscapeStrings;
    procedure SetModel(Value: TZREscapeModel);
    function  GetValue(Index: TZREscapeCode): String;
    procedure SetValue(Index: TZREscapeCode; const Value: String);
    procedure ReadValues(Reader: TReader);
    procedure WriteValues(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property Values : TZREscapeStrings read fValues;
  published
    property Model: TZREscapeModel read fModel write SetModel default emEpson;
    property BoldOn        : String index ecBoldOn         read GetValue write SetValue stored False;
    property BoldOff       : String index ecBoldOff        read GetValue write SetValue stored False;
    property CondensedOn   : String index ecCondensedOn    read GetValue write SetValue stored False;
    property CondensedOff  : String index ecCondensedOff   read GetValue write SetValue stored False;
    property Elite         : String index ecElite          read GetValue write SetValue stored False;
    property FormFeed      : String index ecFormFeed       read GetValue write SetValue stored False;
    property ItalicOn      : String index ecItalicOn       read GetValue write SetValue stored False;
    property ItalicOff     : String index ecItalicOff      read GetValue write SetValue stored False;
    property Pica          : String index ecPica           read GetValue write SetValue stored False;
    property Reset         : String index ecReset          read GetValue write SetValue stored False;
    property SubScriptOn   : String index ecSubScriptOn    read GetValue write SetValue stored False;
    property SubScriptOff  : String index ecSubScriptOff   read GetValue write SetValue stored False;
    property SuperScriptOn : String index ecSuperScriptOn  read GetValue write SetValue stored False;
    property SuperScriptOff: String index ecSuperScriptOff read GetValue write SetValue stored False;
    property UnderlineOn   : String index ecUnderlineOn    read GetValue write SetValue stored False;
    property UnderlineOff  : String index ecUnderlineOff   read GetValue write SetValue stored False;
    property ReportStart   : String index ecReportStart    read GetValue write SetValue stored False;
    property ReportFinish  : String index ecReportFinish   read GetValue write SetValue stored False;
    property PageStart     : String index ecPageStart      read GetValue write SetValue stored False;
    property PageFinish    : String index ecPageFinish     read GetValue write SetValue stored False;
  end;

  { TZREscapeStream }
  TZREscapeStream = class(TZStringStream)
  private
    fEscapes : TZREscapeStrings;
  public
    constructor Create(aStream: TStream; aEscapes: TZREscapeStrings);
    function  ReadString(var Value: String): Boolean; override;
    procedure WriteString(const Value: String); override;
    property Escapes : TZREscapeStrings read fEscapes;
  end;

type
  TZREscapeStyle = (esCondensed, esBold, esItalic, esUnderline, esSuperScript, esSubScript);
  TZREscapeStyles  = set of TZREscapeStyle;

{ Utility functions and classes }
function EscapeLength(const S: String): Integer;
function EscapeStyles(const S: String; Pos: Integer): TZREscapeStyles;
function EscapeFormat(const S: String; Codes: TZREscapeCodes; Styles: TZREscapeStyles): String;
function EscapeDeformat(const S: String): String;

function EscapePadLeft  (const S: String; N: Integer): String;
function EscapePadRight (const S: String; N: Integer): String;
function EscapePadCenter(const S: String; N: Integer): String;

function EscapeCopy(const S: String; Index, Count: Integer): String;
function EscapeInsert(const Pattern, S: String; Pos: Integer): String;
function EscapeDelete(const S: String; Index, Count: Integer): String;
function EscapeStuff(const Pattern, S: String; Pos: Integer): String;

function Escape2String(const Value: String; const Quote: Char = ''''): String;
function String2Escape(const Value: String; const Quote: Char = ''''): String;

type
  TZREscapeTokenizer = class(TObject)
  private
    fLine  : String;
    fCodes : TZREscapeCodes;
    fStyles: TZREscapeStyles;
    fStart : Integer;
    fFinish: Integer;
    fToken : String;
    procedure SetLine(const Value: String);
  public
    constructor Create;
    function EOL: Boolean;
    function NextToken: String;
    property Line  : String read fLine write SetLine;
    property Codes : TZREscapeCodes read fCodes;
    property Styles: TZREscapeStyles read fStyles;
    property Token : String read fToken;
  end;

const
  EscapeChar: Char = '\';
  EscapeSpecifiers: TZREscapeSpecifiers =
    ('@', '=', 'P', 'E', 'C', 'c', 'B', 'b', 'I', 'i', 'U', 'u', 'H', 'h', 'L', 'l', '<', '>', '{', '}');

implementation

uses
  ZRConst, ZRStrUtl;                                // ZReport

function Escape2String(const Value: String; const Quote: Char): String;

  function IsCode(i: Integer; var S: String): Boolean;
  begin
    Result:= Value[i] < #32;
    if Result then
      S:= '#' + IntToStr(Byte(Value[i]))
    else
      S:= Value[i];
  end;

var
  i: Integer;
  S: String;
  C: Boolean;
begin
  Result:= '';
  i:= 1;
  C:= True;
  while i <= Length(Value) do
    begin
      if IsCode(i, S) then
        begin
          if not C then Result:= Result + Quote;
          C:= True;
        end
      else
        if C then
          begin
            Result:= Result + Quote;
            C     := False;
          end;
      Result:= Result + S;
      Inc(i);
    end;
  if not C then Result:= Result + Quote;
end;

function String2Escape(const Value: String; const Quote: Char): String;
var
  i, pc, ps: Integer;
begin
  Result:= '';
  i:= 1;
  while i<=Length(Value) do
    begin
      if (Value[i] = '#') or (Value[i] = Quote) then
        begin
          ps:= Pos(Quote, copy(Value, i+1, Length(Value)));
          pc:= Pos('#' , copy(Value, i+1, Length(Value)));
          if (pc = 0) or (ps > 0) and (ps < pc) then pc:= ps;
          if pc = 0 then pc:= Length(Value);
          if Value[i] = '#' then
            Result:= Result + Char(StrToInt(copy(Value, i+1, pc-1)))
          else
            begin
              Result:= Result + copy(Value, i+1, pc-1);
              inc(i);
            end;
          Inc(i, pc);
        end
      else
        begin
          Inc(i);
          Result:= Result + Value[i];
        end;
    end;
end;

type
  TZRStyleCodes = record
    On, Off: TZREscapeCode;
  end;

const
  StyleCodes : array[TZREscapeStyle] of TZRStyleCodes = (
{esCondensed  } ( On: ecCondensedOn  ; Off: ecCondensedOff   ),
{esBold       } ( On: ecBoldOn       ; Off: ecBoldOff        ),
{esItalic     } ( On: ecItalicOn     ; Off: ecItalicOff      ),
{esUnderline  } ( On: ecUnderlineOn  ; Off: ecUnderlineOff   ),
{esSuperScript} ( On: ecSuperScriptOn; Off: ecSuperScriptOff ),
{esSubScript  } ( On: ecSubScriptOn  ; Off: ecSubScriptOff   ) );

var
  CharIsCodeMap      : array [Char] of Boolean;                 //Является ли символ esc-спецификатором
  CharToCodeMap      : array [Char] of TZREscapeCode;           //Отображение символов на коды

  CodeIsSwitchOnMap  : array [TZREscapeCode] of Boolean;        //Является ли код включателем стиля
  CodeIsSwitchOffMap : array [TZREscapeCode] of Boolean;        //Является ли код выключателем стиля
  CodeToStyleMap     : array [TZREscapeCode] of TZREscapeStyle; //Отображение кодов на стили

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                       Utility functions and classes                    !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

// Являются ли символы строки S в физической позиции Pos esc-спецификатором?
function CharIsCode(const S: String; Pos: Integer): Boolean;
begin
  Result:= (Pos < length(S)) and (S[Pos] = EscapeChar) and CharIsCodeMap[S[Pos+1]];
end;

// Является ли код переключателем esc-стиля
function CodeIsStyle(Code: TZREscapeCode; var Style: TZREscapeStyle; var On: Boolean): Boolean;
begin
  Result := (CodeIsSwitchOnMap[Code] or CodeIsSwitchOffMap[Code]);
  if Result then begin
    Style := CodeToStyleMap[Code];
    On    := CodeIsSwitchOnMap[Code];
  end;
end;

// Определить стили по набору спецификаторов
function CodesToStyles(Codes: TZREscapeCodes): TZREscapeStyles;
var
  c: TZREscapeCode;
  s: TZREscapeStyle;
  o: Boolean;
begin
  Result := [];
  for c := Low(TZREscapeCode) to High(TZREscapeCode) do
    if (c in Codes) and CodeIsStyle(c, s, o) then begin
      if o then Include(Result, s) else Exclude(Result, s);
    end;
end;

// Количество esc-спецификаторов в строке S
function EscapeCount(const S: String): Integer;
var
  i: Integer;
begin
  Result:= 0;
  i     := 1;
  while i <= length(S) do begin
    if CharIsCode(S, i) then begin
      Inc(i);
      Inc(Result);
    end;
    Inc(i);
  end;
end;

// Длина строки S без учета esc-спецификаторов
function EscapeLength(const S: String): Integer;
begin
  Result:= length(S) - 2 * EscapeCount(S);
end;

// Получить физическую позицию символов
// строки S по логической позиции EscPos
// (пропустить возможные esc-спецификаторы)
function EscapePos(const S: String; EscPos: Integer): Integer;
var
  l: Integer;
  i: Integer;
  p: Integer;
begin
  Result:= EscPos;
  l     := length(S);
  i     := 1;
  p     := 1;
  while (i < l) and (p < EscPos) do begin
    if CharIsCode(S, i) then begin
      Inc(Result, 2);
      Inc(i);
    end else
      Inc(p);
    Inc(i);
  end;
  while (i < l) and CharIsCode(S, i) do begin
    Inc(Result, 2);
    Inc(i, 2);
  end;
end;



// Получить набор действующих esc-спецификаторов для строки S в логической позиции Pos
function EscapeCodes(const S: String; Pos: Integer): TZREscapeCodes;
var
  i: Integer;
  p: Integer;
  C: TZREscapeCode;
  E: TZREscapeStyle;
  O: Boolean;
begin
  Result:= [];
  i:= 1;
  p:= EscapePos(S, Pos);
  while (i < length(S)) and (i < p) do begin
    if CharIsCode(S, i) then begin
        Inc(i);
        C := CharToCodeMap[S[i]];
        if CodeIsStyle(C, E, O) then begin
          if O then begin
            Include(Result, StyleCodes[E].On );
            Exclude(Result, StyleCodes[E].Off);
          end else begin
            Exclude(Result, StyleCodes[E].On );
            Include(Result, StyleCodes[E].Off);
          end;
        end else
          Include(Result, C);
      end;
    Inc(i);
  end;
end;


// Получить состояние строки S в логической позиции Pos
function EscapeStyles(const S: String; Pos: Integer): TZREscapeStyles;
begin
  Result := CodesToStyles(EscapeCodes(S, Pos));
end;
{var
  i: Integer;
  p: Integer;
  C: TZREscapeCode;
  E: TZREscapeStyle;
  O: Boolean;
begin
  Result:= [];
  i:= 1;
  p:= EscapePos(S, Pos);
  while (i <= length(S)) and (i <= p) do begin
    if CharIsCode(S, i) then begin
        Inc(i);
        C := CharToCodeMap[S[i]];
        if CodeIsStyle(C, E, O) then begin
          if O then
            Include(Result, E)
          else
            Exclude(Result, E);
        end;
      end;
    Inc(i);
  end;
end;}


function EscapePrefix(const Codes: TZREscapeCodes; const Styles: TZREscapeStyles): String;
var
  C : TZREscapeCode;
  E : TZREscapeStyle;
  O : Boolean;
begin
  Result := '';
  for C:= Low(TZREscapeCode) to High(TZREscapeCode) do
    if (C in Codes) and not (CodeIsStyle(C, E, O) and (E in Styles)) then Result := Result + EscapeChar + EscapeSpecifiers[C];
  for E:= High(TZREscapeStyle) downto Low(TZREscapeStyle) do
    if E in Styles then Result := Result + EscapeChar + EscapeSpecifiers[StyleCodes[E].On];
end;

function EscapeSuffix(const Styles: TZREscapeStyles): String;
var
  E : TZREscapeStyle;
begin
  Result := '';
  for E:= Low(TZREscapeStyle) to High(TZREscapeStyle) do
    if E in Styles then Result := Result + EscapeChar + EscapeSpecifiers[StyleCodes[E].Off];
end;

// Расформатировать строку S esc-спецификаторами по набору стилей Styles
function EscapeFormat(const S: String; Codes: TZREscapeCodes; Styles: TZREscapeStyles): String;
(*var
  C      : TZREscapeCode;
  E      : TZREscapeStyle;
  Start,
  Finish : String[80];*)
begin
  (*Start := EscapePrefix(Codes, Styles);
  Finish:= EscapeSuffix(Styles);
  for C:= Low(TZREscapeCode) to High(TZREscapeCode) do
    if (C in Codes) {and not CodeIsStyle(C, E, O)} then Start := Start + EscapeChar + EscapeSpecifiers[C];
  for E:= High(TZREscapeStyle) downto Low(TZREscapeStyle) do
    if E in Styles then Start := Start + EscapeChar + EscapeSpecifiers[StyleCodes[E].On];
  for E:= Low(TZREscapeStyle) to High(TZREscapeStyle) do
    if E in Styles then Finish := Finish + EscapeChar + EscapeSpecifiers[StyleCodes[E].Off];
  Result:= Start + S + Finish;*)
  Result:= EscapePrefix(Codes, Styles) + S + EscapeSuffix(Styles);
end;

// Удалить из строки все esc-спицификаторы
function EscapeDeformat(const S: String): String;
var
  i: Integer;
begin
  Result:= S;
  i     := 1;
  while i <= length(Result) do
    if CharIsCode(Result, i) then
      Delete(Result, i, 2)
    else
      Inc(i);
end;


// Нормализовать строку с esc-спецификаторами (устранить дублирование)
function EscapeNormalize(const S: String): String;
var
  Codes  : TZREscapeCodes;
  Styles : TZREscapeStyles;
  E      : TZREscapeStyle;
  C      : TZREscapeCode;
  O      : Boolean;
  i      : Integer;
begin
  Result := S;
  Codes  := [];
  Styles := [];
  i      := 1;
  while i < length(Result) do
    if CharIsCode(Result, i) then begin
      C := CharToCodeMap[Result[i+1]];
      if CodeIsStyle(C, E, O) then begin
        if (E in Styles) then
          if O then
            Delete(Result, i, 2)
          else begin
            Exclude(Styles, E);
            Inc(i, 2);
          end
        else
          if O then begin
            Include(Styles, E);
            Inc(i, 2);
          end else
            Delete(Result, i, 2);
      end else
      if C in Codes then
        Delete(Result, i, 2)
      else begin
        Include(Codes, C);
        Inc(i, 2);
      end;
    end else
      Inc(i);
end;

function EscapePadLeft(const S: String; N: Integer): String;
var
  EL: Integer;
begin
  Result:= EscapeNormalize(S);
  EL    := EscapeLength(Result);
  if EL < N then Result:= Space(N-EL) + Result;
end;

function EscapePadRight(const S: String; N: Integer): String;
var
  EL: Integer;
begin
  Result:= EscapeNormalize(S);
  EL    := EscapeLength(Result);
  if EL < N then Result:= Result + Space(N-EL);
end;

function EscapePadCenter(const S: String; N: Integer): String;
var
  EL: Integer;
begin
  Result:= EscapeNormalize(S);
  EL    := EscapeLength(Result);
  if EL < N then begin
    Result := Space((N - EL) div 2) + S;
    Result := Result + Space(N - EscapeLength(Result));
  end;
end;



// Скопировать из esc-строки S часть, начинающуюся с
// логической позиции Index и имеющую логическую длину Count
function EscapeCopy(const S: String; Index, Count: Integer): String;
{var
  si, ei : Integer;
  SC, EC : TZREscapeCodes;
  SE, EE : TZREscapeStyles;
  C      : TZREscapeCode;
  E      : TZREscapeStyle;
begin
  Result:= '';
  if Index > EscapeLength(S) then Exit;
  SetLength(Result, 255);
  si:= EscapePos(S, Index);
  ei:= EscapePos(S, Index+Count-1);
  SC:= EscapeCodes(S, Index);
  EC:= EscapeCodes(S, Index+Count-1);
  SE:= EscapeStyles(S, Index);
  EE:= EscapeStyles(S, Index+Count-1);
  Result := EscapeNormalize(EscapeFormat(copy(S, si, ei-si), SC, []));
end;}
{var
  Buffer  : array[0..1023] of Char;
  si, l, i: Integer;
begin
  Result:= '';
  if Index > EscapeLength(S) then Exit;
  si:= EscapePos(S, Index);
  i := 0;
  l := length(S);
  while (si+i <= l) and (Count > 0) do begin
    if CharIsCode(S, si+i) then begin
      Buffer[i]:= S[si+i];
      Inc(i);
    end else begin
      Dec(Count);
    end;
    Buffer[i] := S[si+i];
    Inc(i);
  end;
  Buffer[i] := #0;
  Result:= EscapeNormalize(Buffer);
end;}
{var
  Buffer  : array[0..1023] of Char;
  si, l, i: Integer;
begin
  Result:= '';
  if Index > EscapeLength(S) then Exit;
  si:= EscapePos(S, Index);
  while (si > 1) and CharIsCode(S, si-2) do Dec(si, 2);
  i := 0;
  l := length(S);
  while (si+i <= l) and (Count > 0) do begin
    if CharIsCode(S, si+i) then begin
      Buffer[i]:= S[si+i];
      Inc(i);
    end else begin
      Dec(Count);
    end;
    Buffer[i] := S[si+i];
    Inc(i);
  end;
  Buffer[i] := #0;
  Result:= EscapeNormalize(Buffer);
end;}
var
  EscLen : Integer;
  StartI : Integer;
  EndI   : Integer;
  StartC : TZREscapeCodes;
  EndS   : TZREscapeStyles;
begin
  EscLen  := EscapeLength(S);
  if Index > EscLen then
    Result:= ''
  else begin
    if Index+Count-1 > EscLen then Count := EscLen-Index+1;
    StartI := EscapePos(S, Index);
    StartC := EscapeCodes(S, Index);
    EndI   := EscapePos(S, Index+Count-1);
    EndS   := EscapeStyles(S, Index+Count-1);
    Result := {EscapeNormalize}(
                EscapePrefix(StartC, []) +
                copy(S, StartI, EndI-StartI+1) +
                EscapeSuffix(EndS) );
  end;
end;


// Удалить из esc-строки S часть, начинающуюся с
// логической позиции Index и имеющую логическую длину Count
function EscapeDelete(const S: String; Index, Count: Integer): String;
var
  Start,
  Finish: String;
begin
  Result := EscapePadRight(S, Index-1);
  Start  := EscapeCopy(Result, 1, Index-1);
  Finish := EscapeCopy(Result, Index+Count, length(Result));
  Result := EscapeNormalize(Start + Finish);
end;
{
var
  si, l, i: Integer;
begin
  Result := S;
  if Index > EscapeLength(S) then Exit;
  si:= EscapePos(S, Index);
  i := 0;
  l := length(S);
  while (si + i <= l) and (Count > 0) do begin
    if CharIsCode(S, si+i) then begin
      Inc(i, 2);
    end else begin
      Delete(Result, si+i, 1);
      Inc(i);
      Dec(Count);
    end;
  end;
  Result := EscapeNormalize(Result);
end;
}



// Вставить esc-строку Pattern в esc-строку S часть,
// начиная с логической позиции Pos
function EscapeInsert(const Pattern, S: String; Pos: Integer): String;
(*var
  SI: Integer;
  SF: TZREscapeCodes;
  C : TZREscapeCode;
  SS: String;
begin
  if Pos > EscapeLength(S) then
    Result := EscapePadRight(S, Pos)
  else
    Result := S;
  SI := EscapePos(Result, Pos);
  SF := EscapeCodes(Result, Pos);
  SS := Pattern; {EscapeNormalize(Pattern);}
  for C:= Low(TZREscapeCode) to High(TZREscapeCode) do
    if C in SF then SS:= EscapeChar + EscapeSpecifiers[C] + SS;
  for C:= High(TZREscapeCode) downto Low(TZREscapeCode) do
    if C in SF then SS:= SS + EscapeChar + EscapeSpecifiers[C];
  Insert(SS, Result, SI);
  Result := EscapeNormalize(Result);
end;*)
var
  Start,
  Finish: String;
begin
  Result := EscapePadRight(S, Pos-1);
  Start  := EscapeCopy(Result, 1, Pos-1);
  Finish := EscapeCopy(Result, Pos, length(Result));
  Result := EscapeNormalize(Start + Pattern + Finish);
end;



// Заместить esc-строкой Pattern в esc-строке S
// часть, начинающуюся с логической позиции Pos
function EscapeStuff(const Pattern, S: String; Pos: Integer): String;
begin
  Result := EscapePadRight(S, Pos-1);
  Result := EscapeDelete(Result, Pos, EscapeLength(Pattern));
  Result := EscapeInsert(Pattern, Result, Pos);
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                            TZREscapeTokenizer                          !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZREscapeTokenizer.Create;
begin
  inherited;
  SetLine('');
end;

procedure TZREscapeTokenizer.SetLine(const Value: String);
begin
  fLine   := Value;
  fCodes  := [];
  fToken  := '';
  fStart  := 1;
  fFinish := 1;
  NextToken;
end;

function TZREscapeTokenizer.EOL: Boolean;
begin
  Result := fStart > EscapeLength(Line);
end;

function TZREscapeTokenizer.NextToken: String;
var
  si, l, i: Integer;
begin
  if EOL then
    fToken := ''
  else begin
    fStart := fFinish;
    fCodes := EscapeCodes(fLine, fStart);
    fStyles:= EscapeStyles(fLine, fStart);
    si := EscapePos(fLine, fStart);
    l  := length(fLine);
    i  := 0;
    while (si+i <= l) and not CharIsCode(fLine, si+i) do Inc(i);
    fToken  := copy(fLine, si, i);
    fFinish := fStart + i;
  end;
  Result := fToken;
end;


{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                              TZREscapeStream                           !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZREscapeStream.Create(aStream: TStream; {aSpecifiers: TZREscapeSpecifiers; }aEscapes: TZREscapeStrings);
begin
  inherited Create(aStream);
//  fSpecifiers:= aSpecifiers;
  fEscapes   := aEscapes;
end;

function TZREscapeStream.ReadString(var Value: String): Boolean;
var
  e: TZREscapeCode;
  S: String;
begin
  Result:= inherited ReadString(S);
  if Result then
    for e:= Low(TZREscapeCode) to High(TZREscapeCode) do
      S:= StringReplace(S, Escapes[e], EscapeChar + EscapeSpecifiers[e], [rfReplaceAll]);
  Value:= S;
end;

procedure TZREscapeStream.WriteString(const Value: String);
var
  e: TZREscapeCode;
  S: String;
begin
  S:= Value;
  for e:= Low(TZREscapeCode) to High(TZREscapeCode) do
    S:= StringReplace(S, EscapeChar + EscapeSpecifiers[e], Escapes[e], [rfReplaceAll]);
  inherited WriteString(S);
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZREscapes                              !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZREscapes.Create;
begin
  inherited;
  Model:= emEpson;
end;

function TZREscapes.GetValue(Index: TZREscapeCode): String;
begin
  Result:= fValues[Index];
end;

procedure TZREscapes.SetValue(Index: TZREscapeCode; const Value: String);
begin
  if Value <> GetValue(Index) then begin
    fValues[Index]:= Value;
    Model:= emCustom;
  end;
end;

procedure TZREscapes.ReadValues(Reader: TReader);
var
  e: TZREscapeCode;
begin
  Reader.ReadListBegin;
  for e:= Low(TZREscapeCode) to High(TZREscapeCode) do fValues[e] := Reader.ReadString;
  Reader.ReadListEnd;
end;

procedure TZREscapes.WriteValues(Writer: TWriter);
var
  e: TZREscapeCode;
begin
  Writer.WriteListBegin;
  for e:= Low(TZREscapeCode) to High(TZREscapeCode) do Writer.WriteString(fValues[e]);
  Writer.WriteListEnd;;
end;

procedure TZREscapes.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Values', ReadValues, WriteValues, Model = emCustom);
end;

(*
procedure TZREscapes.SetModel(Value: TZREscapeModel);
const
  EscapeMap: array[TZREscapeModel] of TZREscapeStrings = (
                 {ecReset, ecFormFeed, ecPica, ecElite, ecCondensedOn, ecCondensedOff, ecBoldOn, ecBoldOff, ecItalicOn, ecItalicOff, ecUnderlineOn, ecUnderlineOff, ecSuperScriptOn, ecSuperScriptOff, ecSubScriptOn, ecSubScriptOff, ecReportStart, ecReportFinish, ecPageStart, ecPageFinish}
{emCustom       }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emNone         }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emCannonF60    }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emCannonLaser  }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emEpson        }(#27#64,  #12,        #27#80, #27#77,  #15,           #18,            #27#71,   #27#72,    #27#52,     #27#53,      #27#45#49,     #27#45#48,      #27#83#01,       #27#84,           #27#83#00,     #27#84        , ''           , ''            , ''         , ''           ),
{emHPDeskjet    }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emHPLaserjet   }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emHPThinkjet   }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emIBMColorJet  }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emIBMPCGraphics}('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emIBMProprinter}('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emNEC3500      }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           ),
{emNECPinwriter }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''           )
  );
var
  e: TZREscapeCode;
begin
  if Value <> fModel then begin
    fModel:= Value;
    if fModel = emCustom then
      for e:= Low(TZREscapeCode) to High(TZREscapeCode) do EscapeMap[fModel, e]:= fValues[e];
    for e:= Low(TZREscapeCode) to High(TZREscapeCode) do fValues[e]:= EscapeMap[fModel, e];
  end;
end;
*)

procedure TZREscapes.SetModel(Value: TZREscapeModel);
(*
const
    {
     * These codes added by PJB - Feb 15, 2000.
     *
     * Adding support for PCL5 printers (and compatibles) from HP.
     *
     * According to HP's web-site, these codes are common across their
     * range of printers, so we can use the same codes for the LaserJet,
     * DeskJet and ThinkJet.  Here's hoping ...
     }
    PCL_Rst         = #27#69;             { Reset                        }
    PCL_FF          = #27#38#108#48#72;   { Form Feed - "Eject Page"     }

    PCL_Pic         = #27#38#107#48#83;   { Pica ???Not Sure??? 10 pitch }
    PCL_Et          = #27#38#107#52#83;   { Elite                        }
    PCL_cOn         = #27#38#107#50#83;   { CondensedOn/Compressed       }
    PCL_cOff        = PCL_Pic;            { CondensedOff ???Not Sure???  }

    PCL_Bold        = #27#40#115#51#66;   { BoldOn                       }
    PCL_NoBold      = #27#40#115#48#66;   { BoldOff - "Medium Stroke"    }

    PCL_Italic      = #27#40#115#49#83;   { ItalicOn                     }
    PCL_ItalOff     = #27#40#115#48#83;   { ItalicOff - "Upright Style   }

    PCL_Underline   = #27#38#100#48#68;   { UnderlineOn                  }
    PCL_UnderlOff   = #27#38#100#64;      { UnderlineOff                 }

    PCL_SuperOn     = #27#38#97#45#46#50#53#82; { SuperScriptOn          }
    PCL_SubOn       = #27#38#97#43#46#50#53#82; { SubScriptOn            }
    PCL_SuperOff    = PCL_SubOn;                { SuperScriptOff         }
    PCL_SubOff      = PCL_SuperOn;              { SubScriptOff           }

  EscapeMap: array[TZREscapeModel] of TZREscapeStrings = (
                 {ecReset, ecFormFeed, ecPica, ecElite, ecCondensedOn, ecCondensedOff, ecBoldOn, ecBoldOff, ecItalicOn, ecItalicOff, ecUnderlineOn, ecUnderlineOff, ecSuperScriptOn, ecSuperScriptOff, ecSubScriptOn, ecSubScriptOff, ecReportStart, ecReportFinish, ecPageStart, ecPageFinish}
{emCustom       }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emNone         }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emCannonF60    }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emCannonLaser  }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emEpson        }(#27#64,  #12,        #27#80, #27#77,  #15,           #18,            #27#71,   #27#72,    #27#52,     #27#53,      #27#45#49,     #27#45#48,      #27#83#01,       #27#84,           #27#83#00,     #27#84        , ''           , ''            , ''         , ''          ),
{emHPDeskjet    }(PCL_Rst, PCL_FF,     PCL_Pic,PCL_Et,  PCL_cOn,       PCL_cOff,       PCL_Bold, PCL_NoBold,PCL_Italic, PCL_ItalOff, PCL_Underline, PCL_UnderlOff,  PCL_SuperOn,     PCL_SuperOff,     PCL_SubOn,     PCL_SubOff    , ''           , ''            , ''         , ''          ),
{emHPLaserjet   }(PCL_Rst, PCL_FF,     PCL_Pic,PCL_Et,  PCL_cOn,       PCL_cOff,       PCL_Bold, PCL_NoBold,PCL_Italic, PCL_ItalOff, PCL_Underline, PCL_UnderlOff,  PCL_SuperOn,     PCL_SuperOff,     PCL_SubOn,     PCL_SubOff    , ''           , ''            , ''         , ''          ),
{emHPThinkjet   }(PCL_Rst, PCL_FF,     PCL_Pic,PCL_Et,  PCL_cOn,       PCL_cOff,       PCL_Bold, PCL_NoBold,PCL_Italic, PCL_ItalOff, PCL_Underline, PCL_UnderlOff,  PCL_SuperOn,     PCL_SuperOff,     PCL_SubOn,     PCL_SubOff    , ''           , ''            , ''         , ''          ),
{emIBMColorJet  }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emIBMPCGraphics}('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emIBMProprinter}('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emNEC3500      }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          ),
{emNECPinwriter }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            ''            , ''           , ''            , ''         , ''          )
  );
*)

const
  EscapeMap: array[TZREscapeModel] of TZREscapeStrings = (
                 {ecReset, ecFormFeed, ecPica, ecElite, ecCondensedOn, ecCondensedOff, ecBoldOn, ecBoldOff, ecItalicOn, ecItalicOff, ecUnderlineOn, ecUnderlineOff, ecSuperScriptOn, ecSuperScriptOff, ecSubScriptOn, ecSubScriptOff, ecReportStart, ecReportFinish, ecPageStart, ecPageFinish}
{emCustom       }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            '',             '',            '',             '',          ''          ),
{emNone         }('',      '',         '',     '',      '',            '',             '',       '',        '',         '',          '',            '',             '',              '',               '',            '',             '',            '',             '',          ''          ),
{Epson          }(#27#64,  #12,        #27#80, #27#77,  #15,           #18,            #27#71,   #27#72,    #27#52,     #27#53,      #27#45#49,     #27#45#48,      #27#83#01,       #27#84,           #27#83#00,     #27#84,         '',            '',             '',          ''          ),
{CanonF-60      }('',      '',         '',     '',      #15,           #18,            #27#69,   #27#70,    #27#54#1,   #27#54#102,  '',            '',             #27#83#102,      #27#84,           #27#83#1,      #27#84,         '',            '',             '',          ''          ),
{CanonLaser     }('',      '',         '',     '',      #27#31#9,      #27#31#13,      #27#79,   #27#38,    '',         '',          '',            '',             #27#68,          #27#85,           #27#85,        #27#68,         '',            '',             '',          ''          ),
{Diablo1600     }(#27#26#73,'','','',#27#31#9,#27#31#13,#27#79,#27#38,#27#38,'','','',#27#68,'',#27#85,'','','','',''),
{EpsonCR-420    }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{EpsonDXSeries  }('','','','',#27#31#9,#27#31#13,#27#79,#27#38,'','','','',#27#68,#27#85,#27#85,#27#68,'','','',''),
{EpsonEX-800    }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonFXSeries  }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonGQ-3500   }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonHS-80     }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonJX-80     }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonLQSeries  }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonLQ-2550   }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonLXSeries  }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonLettertype}(#27#64,'','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonMXSeries  }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonRX-80     }(#27#64,'','','',#15,#18,#27#71,#27#72,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{EpsonSQ2000    }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{HPDeskJet      }('','','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#40#115#43#49#85,#27#40#115#48#85,#27#40#115#45#49#85,#27#40#115#48#85,'','','',''),
{HPInkJet2225C  }(#27#64,'','','',#15,#18#182,#27#69,#27#70,#27#70,'','','','','','','','','','',''),
{HPLaserJet     }('','','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJet2000 }(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJet500Pl}(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJet8LPI }(#27#38#108#56#68,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetII   }(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetIID  }(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetIIP  }(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetLand }(#27#38#108#49#79,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetLands}(#27#38#108#49#111#56#68,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetPlus }(#27#69,'','','',#27#40#115#49#54#46#54#54#72,#27#40#115#49#48#72,#27#40#115#51#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetPort }('','','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#83#102,'',#27#83#1,'','','','',''),
{HPLaserJetIIEur}(#27#38#108#50#54#65,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetIILeg}(#27#38#108#50#104#51#65,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetIIDLo}(#27#38#108#50#83,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPLaserJetIIDSh}(#27#38#108#49#83,'','','',#27#40#115#49#54#46#54#72,#27#40#115#49#48#72,#27#40#115#53#66,#27#40#115#48#66,#27#40#115#49#83,#27#40#115#48#83,'','',#27#38#97#45#46#53#82,#27#38#97#43#46#53#82,#27#38#97#43#46#53#82,#27#38#97#45#46#53#82,'','','',''),
{HPThinkJet     }('','','','',#15,#18,#27#69,#27#70,'','','','','','','','','','','',''),
{IBM4216Personal}('','','','',#15,#18,#27#69,#27#70,#27#70,'','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMColorJetPrin}('','','','',#15,#18,#27#71,#27#72,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMPCCompact   }('','','','',#15,#18,#18#182,'',#18#182,'','','','','','','','','','',''),
{IBMPCConvert   }(#27#64,'','','',#15,#18,#27#69,#27#70,#27#70,'','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMPCGraphics  }('','','','',#15,#18,#27#69,#27#70,#27#54,#27#55,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMProPrinter  }('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{IBMProPrinter2/}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMProPrinterII}('','','','',#15,#18,#27#69,#27#70,#27#73#11,#27#73#102,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMQuickWriter }('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMQuietWriterI}('','','','',#27#73#1,#27#73#102,#27#73#1,#27#73#102,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMQuietWriterI}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{IBMWheelPrinter}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{MannesmanTally }('','','','',#15,#18,#18,'',#18,'','','',#27#68,'',#27#85,'','','','',''),
{MannesmanTallyM}('','','','',#27#91#54#119,#27#91#52#119,#27#91#61#122,#27#91#62#122,'','','','',#27#91#48#122,#27#91#50#122,#27#91#49#122,#27#91#50#122,'','','',''),
{MannesmanTallyM}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#1,#27#84,#27#83#102,#27#84,'','','',''),
{MannesmanTallyS}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NEC2050        }('','','','',#15,#18,#27#71,#27#72,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NEC3500        }('','','','',#15,#18,#27#71,#27#72,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NEC3515,5515   }('','','','',#27#31#9,#27#31#13,#27#87,#27#38,'','','','',#27#68,#27#85,#27#85,#27#68,'','','',''),
{NEC3550        }('','','','',#15,#18,#27#71,#27#72,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NEC7710,3510   }('','','','',#27#93#76,#29#93#72,#27#43,#27#44,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NEC7730,3530   }('','','','',#27#93#72,#27#93#76,#27#93#76,'',#27#93#76,'','','','','','','','','','',''),
{NEC8023        }(#27#78,'','','',#27#81,#27#83,#27#33,#27#34,#27#34,'','','','','','','','','','',''),
{NEC8810,8830   }('','','','',#27#93#72,#27#93#76,#27#38#27#43,#27#38,#27#38,'','','',#27#59,'',#27#58,'','','','',''),
{NEC8815,8835   }('','','','',#27#31#9,#27#31#13,#27#88#27#87,#27#88#27#38,#27#88#27#38,'','','',#27#68,'',#27#85,'','','','',''),
{NEC8850        }(#27#35#65,'','','',#15,#18,#27#71#27#38,#27#72#27#38,#27#72#27#38,'','','',#27#59,'',#27#58,'','','','',''),
{NECElf350      }('','','','',#15,#18,#27#71,#27#72,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NECElf360      }('','','','',#27#93#76,#27#93#72,#27#43,#27#44,'','','','',#27#59,#27#58,#27#58,#27#59,'','','',''),
{NECElf370JR    }(#27#35#65,'','','',#27#15,#27#83,#27#69,#27#70,#27#70,'','','',#27#59,'',#27#58,'','','','',''),
{NECElf370NEC   }(#27#35#65,'','','',#27#15,#27#83,#27#69,#27#70#27#72,#27#70#27#72,'','','',#27#59,'',#27#58,'','','','',''),
{NECPinwriter220}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP2/}('','','','',#27#81,#27#72,#27#33,#27#34,'','','','',#27#71#102,#27#90,#27#71#1,#27#90,'','','',''),
{NECPinwriterP2/}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP3 }('','','','',#15,#18,#27#71,#27#72,#27#72,'','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP5 }('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP6/}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP9X}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{NECPinwriterP52}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{Okidata192/193(}('','','','',#15,#18,#27#69,#27#70,#27#37#71,#27#37#72,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{Okidata192/193(}('','','','',#29,#30,#27#72,#27#73,#27#33#47,#27#33#42,'','',#27#74,#27#76,#27#76,#27#74,'','','',''),
{Okidata292/293(}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#2,#27#84,#27#83#1,#27#84,'','','',''),
{Okidata292/293(}('','','','',#29,#27#48,#27#72,#27#73,#27#33#47,#27#33#42,'','',#27#74,#27#75,#27#76,#27#77,'','','',''),
{Okidata82      }('','','','',#29,#31,#27#84,#27#73,#27#73,#27#73,'','',#27#74,#27#75,#27#76,#27#77,'','','',''),
{Okidata82A     }(#24,'','','',#29,'',#29#31,#30,#30,'','','','','','','','','','',''),
{Okidata84(IBM) }('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{Okidata84(Stand}('','','','',#29,#30,#27#72,#27#73,'','','','',#27#74,#27#76,#27#76,#27#74,'','','',''),
{Okidata92/93(IB}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{OkidataMicrolin}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#48,#27#84,#27#83#49,#27#84,'','','',''),
{OkidataOkimate2}('','','','',#15,#18,'','',#27#37#71,#27#37#72,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{OkidataPacemark}('','','','',#27#66,#27#54,'','','','','','',#27#70,#27#69,#27#68,#27#69,'','','',''),
{OkidataPacemark}('','','','',#15,#18,#27#69,#27#70,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{OkidataPacemark}('','','','',#27#66,#27#54,'','','','','','',#27#70,#27#69,#27#68,#27#69,'','','',''),
{Olympia        }(#27#64,'','','',#27#77,#27#80,#27#87,#27#38,'','','','',#27#68,'',#27#85,'','','','',''),
{OlympiaCompactR}('','','','',#27#77,#27#80,#27#79,#27#38,'','','','',#27#68,#27#85,#27#85,#27#68,'','','',''),
{Panasonic3151  }('','','','',#27#31#9,#27#31#13,#27#79,#27#38,'','','','',#27#68,#27#85,#27#85,#27#68,'','','',''),
{PanasonicE708  }(#27#26#73,'','','','','',#27#66,'',#27#66,'','','',#27#68,'',#27#85,'','','','',''),
{PanasonicIX-P10}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{PanasonicKX-P10}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{PanasonicKX-P10}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{PanasonicKX-P10}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{StarMicronicsNX}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{StarMicronicsPl}(#27#64,'','','',#27#15,#27#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{StarPowertypePM}('','','','',#27#66#3,#27#66#1,#27#71,#27#72,'','','','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{StarPowertypeWP}('','','','',#27#31#9,#27#31#13,#27#75,#27#77,'','','','',#27#68,#27#85,#27#85,#27#68,'','','',''),
{StarRadix/Gemin}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','',''),
{StarSD/SGStarMo}('','','','',#15,#18,#27#69,#27#70,#27#52,#27#53,'','',#27#83#102,#27#84,#27#83#1,#27#84,'','','','')
  );
var
  e: TZREscapeCode;
begin
  if Value <> fModel then begin
    fModel:= Value;
    if fModel = emCustom then
      for e:= Low(TZREscapeCode) to High(TZREscapeCode) do EscapeMap[fModel, e]:= fValues[e];
    for e:= Low(TZREscapeCode) to High(TZREscapeCode) do fValues[e]:= EscapeMap[fModel, e];
  end;
end;


procedure TZREscapes.Assign(Source: TPersistent);
begin
  if Source is TZREscapes then begin
    fModel  := TZREscapes(Source).fModel;
    fValues := TZREscapes(Source).fValues;
  end else
    inherited;
end;



var
  ch: Char;
  ec: TZREscapeCode;
  es: TZREscapeStyle;
initialization
  EscapeModelNames[emCustom] := LoadStr(szrEscapeModelCustom);
  EscapeModelNames[emNone]   := LoadStr(szrEscapeModelNone);

  for ch:= #0 to #255 do begin
    CharIsCodeMap[ch] := False;
    for ec:= low(TZREscapeCode) to high(TZREscapeCode) do begin
      if ch = EscapeSpecifiers[ec] then begin
        CharIsCodeMap[ch]:= True;
        CharToCodeMap[ch]:= ec;
        continue;
      end;
    end;
  end;

  for ec:= low(TZREscapeCode) to high(TZREscapeCode) do begin
    CodeIsSwitchOnMap [ec] := False;
    CodeIsSwitchOffMap[ec] := False;
    for es:= low(TZREscapeStyle) to high(TZREscapeStyle) do
      if StyleCodes[es].On  = ec then begin
        CodeIsSwitchOnMap [ec] := True;
        CodeToStyleMap    [ec] := es;
      end else
      if StyleCodes[es].Off = ec then begin
        CodeIsSwitchOffMap[ec] := True;
        CodeToStyleMap    [ec] := es;
      end;
  end;
end.

