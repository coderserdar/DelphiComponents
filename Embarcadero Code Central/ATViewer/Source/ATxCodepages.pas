//
// Codepages info:
// http://msdn2.microsoft.com/en-us/library/ms776446.aspx
//

unit ATxCodepages;

interface

uses
  Windows;

type
  TATEncodingFamily = (
    EncFamSystem,
    EncFamWEuropean,
    EncFamCEuropean,
    EncFamBaltic,
    EncFamCyrillic,
    EncFamUS,
    EncFamGreek,
    EncFamLatin3,
    EncFamTurkish,
    EncFamArabic,
    EncFamHebrew,
    EncFamThai,
    EncFamVietnam,
    EncFamEBCDIC,
    EncFamUnicode
    );

var
  cATEncodingNewRow: set of TATEncodingFamily = [EncFamUS, EncFamEBCDIC];
    //Families which start new row in menu
  cATEncodingFamilyNames: array[TATEncodingFamily] of AnsiString = (
    '',
    'Western European',
    'Central European',
    'Baltic',
    'Cyrillic',
    'Inited States',
    'Greek',
    'Latin 3',
    'Turkish',
    'Arabic',
    'Hebrew',
    'Thai',
    'Vietnamese',
    'IBM EBCDIC',
    'Unicode'
    );

type
  TATEncodingRec = record
    ID: Integer;
    Family: TATEncodingFamily;
    Name: AnsiString;
  end;

type
  TATEncoding = (
    //ANSI and OEM should be first
    vEncANSI,
    vEncOEM,
    //Common codepages should be here
    vEncEBCDIC,
    vEncKOI8,
    vEncISO,
    vEncMac,
    //Misc codepages should be here
    vEncWEuroDOS,
    vEncWEuroISO1,
    vEncWEuroISO2,
    vEncWEuroMac,
    vEncWEuroWin,
    vEncCEuroDOS,
    vEncCEuroISO,
    vEncCEuroMac,
    vEncCEuroWin,
    vEncBalDOS,
    vEncBalISO1,
    vEncBalISO2,
    vEncBalWin,
    vEncCyrDOS,
    vEncCyrISO,
    vEncCyrKOI8R,
    vEncCyrKOI8U,
    vEncCyrMacC,
    vEncCyrMacU,
    vEncCyrWin,
    vEncUSOEM,
    vEncGreekDOS,
    vEncGreekISO,
    vEncGreekMac,
    vEncGreekWin,
    vEncLatin3,
    vEncTurDOS,
    vEncTurISO,
    vEncTurMac,
    vEncTurWin,
    vEncArDOS1,
    vEncArDOS2,
    vEncArDOS3,
    vEncArISO,
    vEncArMac,
    vEncArWin,
    vEncHebDOS,
    vEncHebISO1,
    vEncHebISO2,
    vEncHebMac,
    vEncHebWin,
    vEncThai,
    vEncViet,
    vEncEBUSCanada,
    vEncEBInternational,
    vEncEBLatin1,
    vEncEBLatin2,
    vEncEBGreek,
    vEncEBGreekModern,
    vEncEBGermany,
    vEncEBDenmarkNorway,
    vEncEBFinlandSweden,
    vEncEBUnitedKingdom,
    vEncEBFrance,
    vEncEBIcelandic,
    vEncEBCyrRussian,
    vEncEBCyrSerbian,
    vEncEBItaly,
    vEncEBLatinAmerica,
    vEncEBJapaneseKatExt,
    vEncEBKoreanExt,
    vEncEBArabic,
    vEncEBHebrew,
    vEncEBThai,
    vEncEBTurkish,
    vEncEBTurkishLatin5,
    //Unicode encodings should be last
    vEncUnicodeLE,
    vEncUnicodeBE
    );

const
  cATUnicodeEncodings: set of TATEncoding = [vEncUnicodeLE, vEncUnicodeBE];

const
  cATEncodings: array[TATEncoding] of TATEncodingRec = (
    (ID:       -1;  Family: EncFamSystem;  Name: 'ANSI (Windows)'),
    (ID: CP_OEMCP;  Family: EncFamSystem;  Name: 'OEM (DOS)'),
    //
    (ID:       -1;  Family: EncFamSystem;  Name: 'EBCDIC'),
    (ID:       -1;  Family: EncFamSystem;  Name: 'KOI8'),
    (ID:       -1;  Family: EncFamSystem;  Name: 'ISO'),
    (ID: CP_MACCP;  Family: EncFamSystem;  Name: 'Mac'),
    //
    (ID:   850;  Family: EncFamWEuropean;  Name: 'IBM-850'),
    (ID: 28591;  Family: EncFamWEuropean;  Name: 'ISO 8859-1'),
    (ID: 28605;  Family: EncFamWEuropean;  Name: 'ISO 8859-15'),
    (ID: 10000;  Family: EncFamWEuropean;  Name: 'MacRoman'),
    (ID:  1252;  Family: EncFamWEuropean;  Name: 'Windows-1252'),
    (ID:   852;  Family: EncFamCEuropean;  Name: 'IBM-852'),
    (ID: 28592;  Family: EncFamCEuropean;  Name: 'ISO 8859-2'),
    (ID: 10029;  Family: EncFamCEuropean;  Name: 'MacCE'),
    (ID:  1250;  Family: EncFamCEuropean;  Name: 'Windows-1250'),
    (ID:   775;  Family: EncFamBaltic;  Name: 'IBM-775'),
    (ID: 28594;  Family: EncFamBaltic;  Name: 'ISO 8859-4'),
    (ID: 28603;  Family: EncFamBaltic;  Name: 'ISO 8859-13'),
    (ID:  1257;  Family: EncFamBaltic;  Name: 'Windows-1257'),
    (ID:   866;  Family: EncFamCyrillic;  Name: 'IBM-866'),
    (ID: 28595;  Family: EncFamCyrillic;  Name: 'ISO 8859-5'),
    (ID: 20866;  Family: EncFamCyrillic;  Name: 'KOI8-R'),
    (ID: 21866;  Family: EncFamCyrillic;  Name: 'KOI8-U'),
    (ID: 10007;  Family: EncFamCyrillic;  Name: 'MacCyrillic'),
    (ID: 10017;  Family: EncFamCyrillic;  Name: 'MacUkrainian'),
    (ID:  1251;  Family: EncFamCyrillic;  Name: 'Windows-1251'),
    (ID:   437;  Family: EncFamUS;  Name: 'US OEM'),
    (ID:   737;  Family: EncFamGreek;  Name: 'IBM-737'),
    (ID: 28597;  Family: EncFamGreek;  Name: 'ISO 8859-7'),
    (ID: 10006;  Family: EncFamGreek;  Name: 'MacGreek'),
    (ID:  1253;  Family: EncFamGreek;  Name: 'Windows-1253'),
    (ID: 28593;  Family: EncFamLatin3;  Name: 'ISO 8859-3'),
    (ID:   857;  Family: EncFamTurkish;  Name: 'IBM-857'),
    (ID: 28599;  Family: EncFamTurkish;  Name: 'ISO 8859-9'),
    (ID: 10081;  Family: EncFamTurkish;  Name: 'MacTurkish'),
    (ID:  1254;  Family: EncFamTurkish;  Name: 'Windows-1254'),
    (ID:   708;  Family: EncFamArabic;  Name: 'ASMO-708'),
    (ID:   720;  Family: EncFamArabic;  Name: 'DOS-720'),
    (ID:   864;  Family: EncFamArabic;  Name: 'IBM-864'),
    (ID: 28596;  Family: EncFamArabic;  Name: 'ISO 8859-6'),
    (ID: 10004;  Family: EncFamArabic;  Name: 'MacArabic'),
    (ID:  1256;  Family: EncFamArabic;  Name: 'Windows-1256'),
    (ID:   862;  Family: EncFamHebrew;  Name: 'IBM-862'),
    (ID: 38598;  Family: EncFamHebrew;  Name: 'ISO 8859-8, ISO-Logical'),
    (ID: 28598;  Family: EncFamHebrew;  Name: 'ISO 8859-8, ISO-Visual'),
    (ID: 10005;  Family: EncFamHebrew;  Name: 'MacHebrew'),
    (ID:  1255;  Family: EncFamHebrew;  Name: 'Windows-1255'),
    (ID:   874;  Family: EncFamThai;  Name: 'Windows-874'),
    (ID:  1258;  Family: EncFamVietnam;  Name: 'Windows-1258'),
    //
    (ID:   037;  Family: EncFamEBCDIC;  Name: 'US-Canada'),
    (ID:   500;  Family: EncFamEBCDIC;  Name: 'International'),
    (ID:  1047;  Family: EncFamEBCDIC;  Name: 'Latin 1/Open System'),
    (ID:   870;  Family: EncFamEBCDIC;  Name: 'Multilingual Latin 2'),
    (ID: 20423;  Family: EncFamEBCDIC;  Name: 'Greek'),
    (ID:   875;  Family: EncFamEBCDIC;  Name: 'Greek Modern'),
    (ID: 20273;  Family: EncFamEBCDIC;  Name: 'Germany'),
    (ID: 20277;  Family: EncFamEBCDIC;  Name: 'Denmark-Norway'),
    (ID: 20278;  Family: EncFamEBCDIC;  Name: 'Finland-Sweden'),
    (ID: 20285;  Family: EncFamEBCDIC;  Name: 'United Kingdom'),
    (ID: 20297;  Family: EncFamEBCDIC;  Name: 'France'),
    (ID: 20871;  Family: EncFamEBCDIC;  Name: 'Icelandic'),
    (ID: 20880;  Family: EncFamEBCDIC;  Name: 'Cyrillic Russian'),
    (ID: 21025;  Family: EncFamEBCDIC;  Name: 'Cyrillic Serbian-Bulgarian'),
    (ID: 20280;  Family: EncFamEBCDIC;  Name: 'Italy'),
    (ID: 20284;  Family: EncFamEBCDIC;  Name: 'Latin America-Spain'),
    (ID: 20290;  Family: EncFamEBCDIC;  Name: 'Japanese Katakana Extended'),
    (ID: 20833;  Family: EncFamEBCDIC;  Name: 'Korean Extended'),
    (ID: 20420;  Family: EncFamEBCDIC;  Name: 'Arabic'),
    (ID: 20424;  Family: EncFamEBCDIC;  Name: 'Hebrew'),
    (ID: 20838;  Family: EncFamEBCDIC;  Name: 'Thai'),
    (ID: 20905;  Family: EncFamEBCDIC;  Name: 'Turkish'),
    (ID:  1026;  Family: EncFamEBCDIC;  Name: 'Turkish (Latin 5)'),
    //
    (ID:    -1;  Family: EncFamUnicode;  Name: 'UTF-16 LE'),
    (ID:    -1;  Family: EncFamUnicode;  Name: 'UTF-16 BE')
    );

function CodepageName(Enc: TATEncoding): AnsiString;
function IsCodepageSupported(Enc: TATEncoding): Boolean;
function SCodepageToUnicode(const S: AnsiString; Enc: TATEncoding): WideString;


implementation

uses
  SysUtils;

var
  CodepagesSupported: array[TATEncoding] of Boolean;

//-------------------------------------------------
function CodepageID(Enc: TATEncoding): Integer;
begin
  Result := cATEncodings[Enc].ID;
  Assert(Result >= 0, 'Invalid codepage specified: CodepageID');
end;

//-------------------------------------------------
function CodepageName(Enc: TATEncoding): AnsiString;
begin
  with cATEncodings[Enc] do
    if Family = EncFamSystem then
      Result := Name
    else
      Result := cATEncodingFamilyNames[Family] + ' (' + Name + ')';
end;

//-------------------------------------------------
function EnumCodePagesProc(S: PAnsiChar): Integer; stdcall;
var
  ID: Integer;
  Enc: TATEncoding;
begin
  Result := 1;
  ID := StrToIntDef(S, 0);
  for Enc := Low(TATEncoding) to High(TATEncoding) do
    if cATEncodings[Enc].ID = ID then
    begin
      CodepagesSupported[Enc] := True;
      Break;
    end;
end;

procedure InitCodepagesSupported;
begin
  FillChar(CodepagesSupported, SizeOf(CodepagesSupported), 0);
  EnumSystemCodePagesA(@EnumCodePagesProc, CP_INSTALLED);

  CodepagesSupported[vEncANSI] := True;
  CodepagesSupported[vEncOEM] := True;
  CodepagesSupported[vEncEBCDIC] := True;
  CodepagesSupported[vEncKOI8] := True;
  CodepagesSupported[vEncISO] := True;
  CodepagesSupported[vEncMac] := True;
end;

//-------------------------------------------------
function IsCodepageSupported(Enc: TATEncoding): Boolean;
begin
  Result := CodepagesSupported[Enc];
end;

//-------------------------------------------------
function IsCodepageID1Byte(ID: Integer): Boolean;
var
  Info: TCPInfo;
begin
  Result := True;
  if GetCPInfo(ID, Info) then
    Result := Info.MaxCharSize = 1;
end;

//-------------------------------------------------
function SCodepageToUnicodeAPI(const S: AnsiString; Enc: TATEncoding): WideString;
var
  WS: WideString;
  N: Integer;
  ID: Integer;
begin
  Result := '?';
  if IsCodepageSupported(Enc) then
    case Enc of
      vEncANSI:
        begin
          Result := S;
        end;
      else
        begin
          WS := '';
          ID := CodepageID(Enc);
          N := MultiByteToWideChar(ID, 0, PAnsiChar(S), Length(S), nil, 0);
          if N = 0 then Exit;
          SetLength(WS, N);
          N := MultiByteToWideChar(ID, 0, PAnsiChar(S), Length(S), PWChar(WS), Length(WS));
          if N = 0 then Exit;
          Result := WS;
        end;
    end;
end;

//-------------------------------------------------
function SUnicodeToCodepage(const WS: WideString; Enc: TATEncoding): AnsiString;
var
  RS: AnsiString;
  N: Integer;
  ID: Integer;
begin
  Result := '?';
  if IsCodepageSupported(Enc) then
    case Enc of
      vEncANSI:
        begin
          Result := WS;
        end;
      else
        begin
          RS := '';
          ID := CodepageID(Enc);
          N := WideCharToMultiByte(ID, 0, PWChar(WS), Length(WS), nil, 0, nil, nil);
          if N = 0 then Exit;
          SetLength(RS, N);
          N := WideCharToMultiByte(ID, 0, PWChar(WS), Length(WS), PAnsiChar(RS), Length(RS), nil, nil);
          if N = 0 then Exit;
          Result := RS;
        end;
    end;
end;

//-------------------------------------------------
function SConvertOEMToANSI(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  OemToCharBuffA(PAnsiChar(S), PAnsiChar(Result), Length(S));
end;

//--------------------------------------------------
type
  TCodepageMap128 = array[0 .. 127] of AnsiChar;
  TCodepageMap256 = array[0 .. 255] of AnsiChar;

function SConvertByMap128(const S: AnsiString; const Map: TCodepageMap128): AnsiString;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(S) do
    if Ord(S[i]) >= 128 then
      Result[i] := Map[Ord(S[i]) - 128];
end;

function SConvertByMap256(const S: AnsiString; const Map: TCodepageMap256): AnsiString;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(S) do
    Result[i] := Map[Ord(S[i])];
end;

const
  //Cyrillic (KOI8-R) --> Cyrillic (Windows-1251)
  cMapKOI8ToANSI: TCodepageMap128 = (
    #$2D, #$A6, #$2D, #$AC, #$4C, #$2D, #$2B, #$2B, 
    #$54, #$2B, #$2B, #$2D, #$2D, #$2D, #$A6, #$A6, 
    #$2D, #$2D, #$2D, #$3F, #$A6, #$95, #$76, #$3F, 
    #$3F, #$3F, #$A0, #$3F, #$B0, #$3F, #$B7, #$3F, 
    #$3D, #$A6, #$2D, #$B8, #$E3, #$E3, #$AC, #$AC, 
    #$AC, #$4C, #$4C, #$4C, #$2D, #$2D, #$2D, #$A6, 
    #$A6, #$A6, #$A6, #$A8, #$A6, #$A6, #$54, #$54, 
    #$54, #$A6, #$A6, #$A6, #$2B, #$2B, #$2B, #$A9, 
    #$FE, #$E0, #$E1, #$F6, #$E4, #$E5, #$F4, #$E3, 
    #$F5, #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, 
    #$EF, #$FF, #$F0, #$F1, #$F2, #$F3, #$E6, #$E2, 
    #$FC, #$FB, #$E7, #$F8, #$FD, #$F9, #$F7, #$FA, 
    #$DE, #$C0, #$C1, #$D6, #$C4, #$C5, #$D4, #$C3, 
    #$D5, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, 
    #$CF, #$DF, #$D0, #$D1, #$D2, #$D3, #$C6, #$C2, 
    #$DC, #$DB, #$C7, #$D8, #$DD, #$D9, #$D7, #$DA
    );

  //From recode 3.5
  cMapMacToANSI: TCodepageMap128 = (
    #$C4, #$C5, #$C7, #$C9, #$D1, #$D6, #$DC, #$E1, 
    #$E0, #$E2, #$E4, #$E3, #$E5, #$E7, #$E9, #$E8,
    #$EA, #$EB, #$ED, #$EC, #$EE, #$EF, #$F1, #$F3, 
    #$F2, #$F4, #$F6, #$F5, #$FA, #$F9, #$FB, #$FC,
    #$A0, #$B0, #$A2, #$A3, #$A7, #$B4, #$B6, #$DF, 
    #$AE, #$A9, #$8E, #$82, #$8C, #$AD, #$C6, #$D8,
    #$8D, #$B1, #$B2, #$B3, #$A5, #$B5, #$A6, #$B7, 
    #$B8, #$B9, #$BC, #$AA, #$BA, #$BD, #$E6, #$F8,
    #$BF, #$A1, #$AC, #$92, #$80, #$81, #$A8, #$AB, 
    #$BB, #$83, #$BE, #$C0, #$C3, #$D5, #$91, #$93,
    #$D0, #$84, #$96, #$94, #$95, #$90, #$F7, #$D7, 
    #$FF, #$DD, #$98, #$97, #$86, #$99, #$DE, #$A4,
    #$88, #$87, #$89, #$8B, #$8A, #$C2, #$CA, #$C1, 
    #$CB, #$C8, #$CD, #$CE, #$CF, #$CC, #$D3, #$D4,
    #$F0, #$D2, #$DA, #$DB, #$D9, #$9B, #$9A, #$85, 
    #$8F, #$9D, #$9C, #$9E, #$9F, #$FD, #$FE, #$AF
    );

  //From recode 3.5
  cMapEBCDICToANSI: TCodepageMap256 = (
    #$00, #$01, #$02, #$03, #$F7, #$09, #$D2, #$7F, 
    #$F2, #$A8, #$93, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$D3, #$A1, #$08, #$D7, 
    #$18, #$19, #$96, #$D0, #$1C, #$1D, #$1E, #$1F,
    #$7C, #$D6, #$81, #$C0, #$D1, #$0A, #$17, #$1B, 
    #$D4, #$E9, #$E0, #$D5, #$92, #$05, #$06, #$07,
    #$F0, #$F1, #$16, #$F3, #$F4, #$F5, #$F6, #$04, 
    #$F8, #$F9, #$A9, #$94, #$14, #$15, #$95, #$1A,
    #$20, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, 
    #$C8, #$C9, #$5B, #$2E, #$3C, #$28, #$2B, #$21,
    #$26, #$D8, #$D9, #$E2, #$E3, #$E4, #$E5, #$E6, 
    #$E7, #$E8, #$5D, #$24, #$2A, #$29, #$3B, #$5E,
    #$2D, #$2F, #$82, #$83, #$84, #$85, #$86, #$87, 
    #$88, #$89, #$A6, #$2C, #$25, #$5F, #$3E, #$3F,
    #$97, #$98, #$99, #$A2, #$A3, #$A4, #$A5, #$91, 
    #$A7, #$60, #$3A, #$23, #$40, #$27, #$3D, #$22,
    #$80, #$61, #$62, #$63, #$64, #$65, #$66, #$67, 
    #$68, #$69, #$8A, #$8B, #$8C, #$8D, #$8E, #$8F,
    #$90, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F, #$70, 
    #$71, #$72, #$9A, #$9B, #$9C, #$9D, #$9E, #$9F,
    #$A0, #$7E, #$73, #$74, #$75, #$76, #$77, #$78, 
    #$79, #$7A, #$AA, #$AB, #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, 
    #$B8, #$B9, #$BA, #$BB, #$BC, #$BD, #$BE, #$BF,
    #$7B, #$41, #$42, #$43, #$44, #$45, #$46, #$47, 
    #$48, #$49, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$7D, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F, #$50, 
    #$51, #$52, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF,
    #$5C, #$E1, #$53, #$54, #$55, #$56, #$57, #$58, 
    #$59, #$5A, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, 
    #$38, #$39, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF
    );

const
  //Western European (ISO 8859-1) --> Western European (Windows-1252)
  cMapISOToANSI: TCodepageMap128 = (
    #$3F, #$81, #$3F, #$3F, #$3F, #$3F, #$3F, #$3F, 
    #$3F, #$3F, #$3F, #$3F, #$3F, #$8D, #$3F, #$8F, 
    #$90, #$3F, #$3F, #$3F, #$3F, #$3F, #$3F, #$3F, 
    #$3F, #$3F, #$3F, #$3F, #$3F, #$9D, #$3F, #$3F, 
    #$A0, #$A1, #$A2, #$A3, #$A4, #$A5, #$A6, #$A7, 
    #$A8, #$A9, #$AA, #$AB, #$AC, #$AD, #$AE, #$AF, 
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, 
    #$B8, #$B9, #$BA, #$BB, #$BC, #$BD, #$BE, #$BF, 
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, 
    #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF, 
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, 
    #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF, 
    #$E0, #$E1, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, 
    #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF, 
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, 
    #$F8, #$F9, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF
    );

//-------------------------------------------------
function SConvertKOI8ToANSI(const S: AnsiString): AnsiString;
begin
  Result := SConvertByMap128(S, cMapKOI8ToANSI);
end;

function SConvertMacToANSI(const S: AnsiString): AnsiString;
begin
  Result := SConvertByMap128(S, cMapMacToANSI);
end;

function SConvertEBCDICToANSI(const S: AnsiString): AnsiString;
begin
  Result := SConvertByMap256(S, cMapEBCDICToANSI);
end;

function SConvertISOToANSI(const S: AnsiString): AnsiString;
begin
  Result := SConvertByMap128(S, cMapISOToANSI);
end;

//-------------------------------------------------
function SCodepageToUnicode(const S: AnsiString; Enc: TATEncoding): WideString;
var
  IsoEnc: TATEncoding;
begin
  Result := '';

  Assert(not (Enc in cATUnicodeEncodings),
    'Unicode encodings can''t be passed to CodepageToUnicode');

  if (S <> '') then
    case Enc of
      vEncANSI:
        Result := S;

      vEncEBCDIC:
        if IsCodepageSupported(vEncEBUSCanada) then
          Result := SCodepageToUnicodeAPI(S, vEncEBUSCanada)
        else
          Result := SConvertEBCDICToANSI(S);

      vEncKOI8:
        if IsCodepageSupported(vEncCyrKOI8R) then
          Result := SCodepageToUnicodeAPI(S, vEncCyrKOI8R)
        else
          Result := SConvertKOI8ToANSI(S);

      vEncISO:
        begin
          //Since we don't have CP_ISOCP Windows constant,
          //we need to determine ISO codepage by hands:
          case GetACP of
            1250: IsoEnc := vEncCEuroISO;  //Eastern European 
            1251: IsoEnc := vEncCyrISO;    //Cyrillic
            1252: IsoEnc := vEncWEuroISO1; //US, Western Europe
            1253: IsoEnc := vEncGreekISO;  //Greek
            1254: IsoEnc := vEncTurISO;    //Turkish
            1255: IsoEnc := vEncHebISO1;   //Hebrew
            1256: IsoEnc := vEncArISO;     //Arabic
            1257: IsoEnc := vEncBalISO1;   //Baltic
            else IsoEnc := vEncWEuroISO1;
          end;

          if IsCodepageSupported(IsoEnc) then
            Result := SCodepageToUnicodeAPI(S, IsoEnc)
          else
            Result := SConvertISOToANSI(S);
        end;

      vEncMac:
        begin
          //CP_MACCP codepage supported only under NT:
          if (Win32Platform = VER_PLATFORM_WIN32_NT) then
            Result := SCodepageToUnicodeAPI(S, vEncMac)
          else
            Result := SConvertMacToANSI(S);
        end;

      else
        Result := SCodepageToUnicodeAPI(S, Enc);
    end;
end;


initialization

  InitCodepagesSupported;

end.
