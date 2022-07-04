unit CHCrypt;

{ ##############################################################################
  TCHCrypt

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 16.09.2002    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed


  ############################################################################ }

interface

uses
  SysUtils, Classes;

type
  TCryptKey = 1..100;
  TCryptBase = 1..10;
  TCryptMode = (cmStandard, cmExtended, cmIniStandard, cmIniExtended);
  TCryptChar = (ccAll, ccUpper, ccLower);
  
  TCHCrypt = class(TComponent)
  private
    FKey1: TCryptKey;
    FKey2: TCryptKey;
    FKey3: TCryptKey;
    FKey4: TCryptKey;
    FCryptNumber : Boolean;
    FCryptCharSmall : Boolean;
    FCryptCharLarge : Boolean;
    FCryptSonder : Boolean;
    FCryptExtSonder : Boolean;
    FCryptExtSmall : Boolean;
    FCryptExtLarge : Boolean;
    FCryptINI : Boolean;
    FMaxCryptNumber : Byte;
    FMaxCryptCharSmall : Byte;
    FMaxCryptCharLarge : Byte;
    FMaxCryptExtSonder : Byte;
    FMaxCryptExtSmall : Byte;
    FMaxCryptExtLarge : Byte;
    FMaxCryptSonder : Byte;
    FMaxCryptINI : Byte;
    FMaxNoPrint : Byte;
    FMaxCryptArray : Byte;
    FBase: TCryptBase;
    FCryptChar: TCryptChar;
    FCryptMode: TCryptMode;
    procedure BuildArray;
    procedure SetArrayWork;
    procedure SetCryptChar(const Value: TCryptChar);
    procedure SetCryptMode(const Value: TCryptMode);
  public
    constructor Create(AOwner : TComponent); override;

    function DoEncrypt(Text : string) : string;
    function DoDecrypt(Text : string) : string;
    function DoEncryptExt(Text : string; Base : TCryptBase; Key1, Key2, Key3, Key4 : TCryptKey;
      CryptMode : TCryptMode; CryptChar : TCryptChar) : string;
    function DoDecryptExt(Text : string; Base : TCryptBase; Key1, Key2, Key3, Key4 : TCryptKey;
      CryptMode : TCryptMode; CryptChar : TCryptChar) : string;
  published
    property Base : TCryptBase read FBase Write FBase;
    property CryptMode : TCryptMode read FCryptMode Write SetCryptMode;
    property CryptChar : TCryptChar read FCryptChar Write SetCryptChar;
    property Key1 : TCryptKey read FKey1 Write FKey1;
    property Key2 : TCryptKey read FKey2 Write FKey2;
    property Key3 : TCryptKey read FKey3 Write FKey3;
    property Key4 : TCryptKey read FKey4 Write FKey4;
  end;

procedure Register;

implementation

const
  // enthält Zahlen von 0 - 9
  CryptNumber : array[0..9] of Byte = (
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57);

  // enthält Zeichen a..z
  CryptCharSmall : array[0..25] of Byte = (
    97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
    117, 118, 119, 120, 121, 122);

  // enthält Zeichen A..Z
  CryptCharLarge : array[0..25] of Byte = (
    65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
    85, 86, 87, 88, 89, 90);

  // enthält Standardsonderzeichen
  CryptSonder : array[0..29] of Byte = (
    32, 64, 33, 63, 34, 62, 35, 92, 36, 60, 37, 59, 38, 58, 39, 47, 40, 46, 41, 45,
    42, 44, 94, 43, 123, 95, 124, 96, 125, 126);

  // enthält Extendedsonderzeichen
  CryptExtSonder : array[0..34] of Byte = (
    161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180,
    181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 215, 247, 223, 255);

  // enthält alle Kleinbuchstaben vom erweitertem Zeichensatz
  CryptExtSmall : array[0..29] of Byte = (
    224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243,
    244, 245, 246, 248, 249, 250, 251, 252, 253, 254);

  // enthält alle Großbuchstaben vom erweitertem Zeichensatz
  CryptExtLarge : array[0..29] of Byte = (
    192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211,
    212, 213, 214, 216, 217, 218, 219, 220, 221, 222);

  // enthält Sonderzeichen für die Verarbeitung und INI-Dateien
  CryptINI : array[0..2] of Byte = (61, 91, 93);


  // nicht druckbare Steuerzeichen
  NoPrint : array[0..31] of Byte = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 127);

var
  CryptArray : array of Byte;

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHCrypt]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCrypt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKey1 := 10;
  FKey2 := 20;
  FKey3 := 30;
  FKey4 := 40;
  FBase := 2;

  FMaxCryptNumber := Length(CryptNumber);
  FMaxCryptCharSmall := Length(CryptCharSmall);
  FMaxCryptCharLarge := Length(CryptCharLarge);
  FMaxCryptExtSonder := Length(CryptExtSonder);
  FMaxCryptExtSmall := Length(CryptExtSmall);
  FMaxCryptExtLarge := Length(CryptExtLarge);
  FMaxCryptSonder := Length(CryptSonder);
  FMaxCryptINI := Length(CryptINI);
  FMaxNoPrint := Length(NoPrint);

  FCryptMode := cmStandard;
  FCryptChar := ccAll;

  FCryptNumber := True;
  FCryptCharSmall := True;
  FCryptCharLarge := True;
  FCryptSonder := True;
  FCryptExtSonder := False;
  FCryptExtSmall := False;
  FCryptExtLarge := False;
  FCryptINI := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCrypt.BuildArray;
var
  I : Integer;
begin
  CryptArray := nil;
  FMaxCryptArray := 0;
  // Number
  if FCryptNumber then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptNumber);
    for I := 0 to High(CryptNumber) do
      CryptArray[FMaxCryptArray + I] := CryptNumber[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Charactar small
  if FCryptCharSmall then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptCharSmall);
    for I := 0 to High(CryptCharSmall) do
      CryptArray[FMaxCryptArray + I] := CryptCharSmall[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Charactar small
  if FCryptCharLarge then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptCharLarge);
    for I := 0 to High(CryptCharLarge) do
      CryptArray[FMaxCryptArray + I] := CryptCharLarge[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Sonderzeichen
  if FCryptSonder then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptSonder);
    for I := 0 to High(CryptSonder) do
      CryptArray[FMaxCryptArray + I] := CryptSonder[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Extended small
  if FCryptExtSmall then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptExtSmall);
    for I := 0 to High(CryptExtSmall) do
      CryptArray[FMaxCryptArray + I] := CryptExtSmall[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Extended sonder
  if FCryptExtSonder then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptExtSonder);
    for I := 0 to High(CryptExtSonder) do
      CryptArray[FMaxCryptArray + I] := CryptExtSonder[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // Extended large
  if FCryptExtLarge then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptExtLarge);
    for I := 0 to High(CryptExtLarge) do
      CryptArray[FMaxCryptArray + I] := CryptExtLarge[I];
  end;
  FMaxCryptArray := Length(CryptArray);
  // INI
  if FCryptINI then
  begin
    SetLength(CryptArray, FMaxCryptArray + FMaxCryptINI);
    for I := 0 to High(CryptINI) do
      CryptArray[FMaxCryptArray + I] := CryptINI[I];
  end;

  FMaxCryptArray := Length(CryptArray);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCrypt.DoDecryptExt(Text: string; Base : TCryptBase; Key1, Key2, Key3, Key4 : TCryptKey;
  CryptMode : TCryptMode; CryptChar : TCryptChar): string;
begin
  FKey1 := Key1;
  FKey2 := Key2;
  FKey3 := Key3;
  FKey4 := Key4;
  FBase := Base;
  FCryptMode := CryptMode;
  FCryptChar := CryptChar;
  Result := DoDecrypt(Text);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCrypt.DoEncryptExt(Text: string; Base : TCryptBase; Key1, Key2, Key3, Key4 : TCryptKey;
  CryptMode : TCryptMode; CryptChar : TCryptChar): string;
begin
  FKey1 := Key1;
  FKey2 := Key2;
  FKey3 := Key3;
  FKey4 := Key4;
  FBase := Base;
  FCryptMode := CryptMode;
  FCryptChar := CryptChar;
  Result := DoEncrypt(Text);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ verschlüsseln }
function TCHCrypt.DoEncrypt(Text : string) : string;
var
  sOutput, sInput : string;
  sChar : Char;
  nStrCount, nDeChar, nArrayIdx, nIdx, nNewIdx, nKeyValue : Integer;
  nChar : Word;
  bOk : Boolean;
begin
  sOutput := '';
  nIdx := 0;
  sInput := Text;

  if Length(sInput) > 0 then
  begin

    if FCryptChar = ccUpper then
      sInput := AnsiUpperCase(sInput)
    else if FCryptChar = ccLower then
      sInput := AnsiLowerCase(sInput);

    // einzelne Zeichen verschlüsseln
    for nStrCount := 1 to Length(sInput) do
    begin
      bOk := True;
      sChar := sInput[nStrCount];
      nChar := Ord(sChar);

      // nicht codierbare Sonderzeichen suchen
      for nArrayIdx := 0 to (FMaxNoPrint - 1) do
      begin
        if nChar = NoPrint[nArrayIdx] then
        begin
          bOk := False;
        end;
      end;

      if (FCryptMode = cmIniStandard) or (FCryptMode = cmIniExtended) then
      begin
        for nArrayIdx := 0 to (FMaxCryptINI - 1) do
        begin
          if nChar = CryptINI[nArrayIdx] then
          begin
            bOk := False;
          end;
        end;
      end;

      // je nach Zeichensatz, Zeichenindex in Array suchen
      if bOk then
      begin
        for nArrayIdx := 0 to (FMaxCryptArray - 1) do
        begin
          if nChar = CryptArray[nArrayIdx] then
            nIdx := nArrayIdx;
        end;

        nKeyValue := FKey1 + (FKey2 * 2) + (FKey3 * 3) + (FKey4 * 4);
        nNewIdx := nIdx + nKeyValue + (nStrCount * FBase);

        while nNewIdx > (FMaxCryptArray - 1) do
        begin
          nNewIdx := nNewIdx - (FMaxCryptArray - 1);
        end;
        nDeChar := CryptArray[nNewIdx];
      end
      else
      begin
        nDeChar := nChar;
      end;

      sOutput := sOutput + Chr(nDeChar);
    end;
  end;
  Result := sOutput;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ entschlüsseln }
function TCHCrypt.DoDecrypt(Text : string) : string;
var
  sCrypt : string;
  sChar : Char;
  nStrCount, nEnChar, nArrayIdx, nIdx, nNewIdx, nKeyValue : Integer;
  nChar : Word;
  bOk : Boolean;
begin
  sCrypt := '';
  nIdx := 0;

  if Length(Text) > 0 then
  begin
    // einzelne Zeichen entschlüsseln
    for nStrCount := 1 to Length(Text) do
    begin
      bOk := True;
      sChar := Text[nStrCount];
      nChar := Ord(sChar);

      // nicht codierbare Sonderzeichen suchen
      for nArrayIdx := 0 to (FMaxNoPrint - 1) do
      begin
        if nChar = NoPrint[nArrayIdx] then
        begin
          bOk := False;
        end;
      end;

      if (FCryptMode = cmIniStandard) or (FCryptMode = cmIniExtended) then
      begin
        for nArrayIdx := 0 to (FMaxCryptINI - 1) do
        begin
          if nChar = CryptINI[nArrayIdx] then
          begin
            bOk := False;
          end;
        end;
      end;

      // je nach Zeichensatz, Zeichenindex in Array suchen
      if bOk then
      begin
        for nArrayIdx := 0 to (FMaxCryptArray - 1) do
        begin
          if nChar = CryptArray[nArrayIdx] then
            nIdx := nArrayIdx;
        end;

        nKeyValue := FKey1 + (FKey2 * 2) + (FKey3 * 3) + (FKey4 * 4);
        nNewIdx := nIdx - nKeyValue - (nStrCount * FBase);

        while nNewIdx < 0 do
        begin
          nNewIdx := nNewIdx + (FMaxCryptArray - 1);
        end;
        nEnChar := CryptArray[nNewIdx];

      end
      else
      begin
        nEnChar := nChar; 
      end;

      sCrypt := sCrypt + Chr(nEnChar);
    end;
  end;
  Result := sCrypt;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCrypt.SetCryptChar(const Value: TCryptChar);
begin
  if Value <> FCryptChar then
  begin
    FCryptChar := Value;
    SetArrayWork;
    BuildArray;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCrypt.SetCryptMode(const Value: TCryptMode);
begin
  if FCryptMode <> Value then
  begin
    FCryptMode := Value;
    SetArrayWork;
    BuildArray;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCrypt.SetArrayWork;
begin
  // Standard
  if FCryptMode = cmStandard then
  begin
    FCryptNumber := True;
    FCryptSonder := True;
    FCryptExtSonder := False;
    FCryptINI := True;
  end
  // Extended
  else if FCryptMode = cmExtended then
  begin
    FCryptNumber := True;
    FCryptSonder := True;
    FCryptExtSonder := True;
    FCryptINI := True;
  end
  // IniStandard
  else if FCryptMode = cmIniStandard then
  begin
    FCryptNumber := True;
    FCryptSonder := True;
    FCryptExtSonder := False;
    FCryptINI := False;
  end
  // IniExtended
  else if FCryptMode = cmIniExtended then
  begin
    FCryptNumber := True;
    FCryptSonder := True;
    FCryptExtSonder := True;
    FCryptINI := False;
  end;

  // All
  if FCryptChar = ccAll then
  begin
    if (FCryptMode = cmStandard) or (FCryptMode = cmIniStandard) then
    begin
      FCryptCharSmall := True;
      FCryptCharLarge := True;
      FCryptExtSmall := False;
      FCryptExtLarge := False;
    end
    else if (FCryptMode = cmExtended) or (FCryptMode = cmIniExtended) then
    begin
      FCryptCharSmall := True;
      FCryptCharLarge := True;
      FCryptExtSmall := True;
      FCryptExtLarge := True;
    end;
  end
  // Upper
  else if FCryptChar = ccUpper then
  begin
    if (FCryptMode = cmStandard) or (FCryptMode = cmIniStandard) then
    begin
      FCryptCharSmall := False;
      FCryptCharLarge := True;
      FCryptExtSmall := False;
      FCryptExtLarge := False;
    end
    else if (FCryptMode = cmExtended) or (FCryptMode = cmIniExtended) then
    begin
      FCryptCharSmall := False;
      FCryptCharLarge := True;
      FCryptExtSmall := False;
      FCryptExtLarge := True;
    end;
  end
  // Lower
  else if CryptChar = ccLower then
  begin
    if (FCryptMode = cmStandard) or (FCryptMode = cmIniStandard) then
    begin
      FCryptCharSmall := True;
      FCryptCharLarge := False;
      FCryptExtSmall := False;
      FCryptExtLarge := False;
    end
    else if (FCryptMode = cmExtended) or (FCryptMode = cmIniExtended) then
    begin
      FCryptCharSmall := True;
      FCryptCharLarge := False;
      FCryptExtSmall := True;
      FCryptExtLarge := False;
    end;
  end;
end;

end.
