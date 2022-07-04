unit CHCryptPro;

{ ##############################################################################
  TCHCryptPro

  Version   		:   1.1.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 04.04.2004    - First Release
  1.1.0 - 02.01.2005    - NEW: property 'Options' define the output/input crypt char.


  ############################################################################ }

interface

uses
  SysUtils, Classes, Dialogs, StrUtils, Types;

type
  TCHCryptPro = class;

  TCryptChar = set of (UpperChar, LowerChar, NumberChar, AdvancedChar, SpezialChar, Space);
  TCryptModus = (cmAutomatic, cmCryptChar);

  TCHFileCrypt = class(TPersistent)
  private
    FOwner : TCHCryptPro;
    FInputfile: string;
    FOutputfile: string;
    procedure SetInputfile(const Value: string);
    procedure SetOutputfile(const Value: string);
  protected

  public
    constructor Create(AOwner: TCHCryptPro); virtual;
    destructor Destroy; override;
  published
    property Inputfile : string read FInputfile Write SetInputfile;
    property Outpufile : string read FOutputfile Write SetOutputfile;
  end;


  TCHCryptPro = class(TComponent)
  private
    FKey1: Byte;
    FKey4: Byte;
    FKey2: Byte;
    FKey3: Byte;
    FFileCrypt : TCHFileCrypt;
    FNotCryptChar: string;
    FAllArrayInt : Integer;
    FNotCryptStrings: TStrings;
    FCryptChar: TCryptChar;
    FCryptModus: TCryptModus;
    function BuildArrays(sText : string) : Boolean;
    function InitCryptChar : Boolean;
    procedure SetNotCryptStrings(const Value: TStrings);
  protected
    { Protected-Deklarationen }
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    function DoEncrypt(Text : string) : string; overload;
    function DoDecrypt(Text : string) : string; overload;
    function DoEncrypt(Text, NoCryptChar : string; Key1, Key2, Key3, Key4 : Byte) : string; overload;
    function DoDecrypt(Text, NoCryptChar  : string; Key1, Key2, Key3, Key4 : Byte) : string; overload;
    function DoEncryptFile : Boolean;
    function DoDecryptFile : Boolean;
  published
    property Key1 : Byte read FKey1 Write FKey1;
    property Key2 : Byte read FKey2 Write FKey2;
    property Key3 : Byte read FKey3 Write FKey3;
    property Key4 : Byte read FKey4 Write FKey4;
    property NotCryptChar : string read FNotCryptChar Write FNotCryptChar;
    property NotCryptStrings : TStrings read FNotCryptStrings Write SetNotCryptStrings;
    property FileCrypt : TCHFileCrypt read FFileCrypt Write FFileCrypt;
    property CryptChar : TCryptChar read FCryptChar Write FCryptChar;
    property CryptCharMode : TCryptModus read FCryptModus Write FCryptModus;
  end;

var
  CryptArray : array of Byte;
  AsciiArray : array of Byte;

  CharExtArray : array of Byte;
  NumberArray : array of Byte;
  CharUpperArray : array of Byte;
  CharLowerArray : array of Byte;
  CharSpezialArray : array of Byte;
  SpaceArray : array of Byte;
  NotCryptArray : array of Byte;


procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHCryptPro]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCryptPro.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKey1 := 10;
  FKey2 := 20;
  FKey3 := 30;
  FKey4 := 40;
  FCryptChar := [UpperChar, LowerChar, NumberChar, AdvancedChar, Space];
  FCryptModus := cmAutomatic;
  FFileCrypt := TCHFileCrypt.Create(Self);

  InitCryptChar;
  FNotCryptStrings := TStringList.Create;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.InitCryptChar : Boolean;
var
  I, X : Integer;
begin
  Result := True;
  try
    // NumberArray
    X := 0;
    for I := 48 to 57 do
    begin
      Inc(X);
      SetLength(NumberArray, X);
      NumberArray[X -1] := I;
    end;

    // CharUpperArray
    X := 0;
    for I := 65 to 90 do
    begin
      Inc(X);
      SetLength(CharUpperArray, X);
      CharUpperArray[X -1] := I;
    end;

    // CharLowerArray
    X := 0;
    for I := 97 to 122 do
    begin
      Inc(X);
      SetLength(CharLowerArray, X);
      CharLowerArray[X -1] := I;
    end;

    // CharExtArray
    X := 0;
    for I := 161 to 255 do
    begin
      Inc(X);
      SetLength(CharExtArray, X);
      CharExtArray[X -1] := I;
    end;

    // SpaceArray
    SetLength(SpaceArray, 1);
    SpaceArray[0] := 32;

    // CharSpezialArray
    X := 0;
    for I := 33 to 47 do
    begin
      Inc(X);
      SetLength(CharSpezialArray, X);
      CharSpezialArray[X -1] := I;
    end;
    for I := 58 to 64 do
    begin
      Inc(X);
      SetLength(CharSpezialArray, X);
      CharSpezialArray[X -1] := I;
    end;
    for I := 91 to 96 do
    begin
      Inc(X);
      SetLength(CharSpezialArray, X);
      CharSpezialArray[X -1] := I;
    end;
    for I := 123 to 126 do
    begin
      Inc(X);
      SetLength(CharSpezialArray, X);
      CharSpezialArray[X -1] := I;
    end;
  except
    Result := False;
    ShowMessage('Error: INITIALIZATION');
  end;

  FAllArrayInt := Length(NumberArray) + Length(CharExtArray) + Length(CharUpperArray) +
    Length(CharLowerArray) + Length(CharSpezialArray) + Length(CharExtArray);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.BuildArrays(sText : string) : Boolean;
var
  I, B, X : Byte;
  bOk, bLower, bUpper, bAdvanced, bSpezial, bNumber : Boolean;
  nMax : Word;
  sNumber, sLower, sUpper, sExt, sSpezial : string;

  procedure FillCharInArray;
  var
    X, Y : Byte;
  begin
    bOk := True;
    for X := 1 to Length(NotCryptChar) do
    begin
      Y := Ord(NotCryptChar[X]);
      if B = Y then
        bOk := False;
    end;

    if bOk then
    begin
      SetLength(CryptArray, Length(CryptArray) +1);
      CryptArray[Length(CryptArray) -1] := B;
    end;
  end;

  { gibt die größere der beiden Zahlen zurück }
  function GetMaxInt(A, B : Longint) : Longint;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

begin
  CryptArray := nil;
  Result := True;

  sNumber := '0123456789';
  sLower := 'abcdefghijklmnopqrstuvwxyzüöäß';
  sUpper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZÜÖÄ';
  sExt := '!"#$%&()*+,-./:;<=>?@[\]^_{|}~€¦§‘°';
  sSpezial := '¡¢£¤¥¨©ª«¬­®¯±²³´µ¶·¸¹º»¼½¾¿ÏÎÍÌËÊÉÈÇÆÅÄÃÂÁÀÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞïîíìëêéèçæåãâáàÿþýûúùø÷õôóòñð';

  try

    bNumber := False;
    bLower := False;
    bUpper := False;
    bAdvanced := False;
    bSpezial := False;
    for I := 0 to Length(sNumber) do
    begin
      if Pos(sNumber[I], sText) > 1 then
      begin
        bNumber := True;
        Break;
      end;
    end;
    for I := 0 to Length(sLower) do
    begin
      if Pos(sLower[I], sText) > 1 then
      begin
        bLower := True;
        Break;
      end;
    end;
    for I := 0 to Length(sUpper) do
    begin
      if Pos(sUpper[I], sText) > 1 then
      begin
        bUpper := True;
        Break;
      end;
    end;

    for I := 0 to Length(sSpezial) do
    begin
      if Pos(sSpezial[I], sText) > 1 then
      begin
        bSpezial := True;
        Break;
      end;
    end;

    // grösstes Array finden
    nMax := GetMaxInt(Length(NumberArray), Length(CharExtArray));
    nMax := GetMaxInt(nMax, Length(CharUpperArray));
    nMax := GetMaxInt(nMax, Length(CharLowerArray));
    nMax := GetMaxInt(nMax, Length(CharSpezialArray));


    // Cryptarray aufbauen
    for I := 0 to nMax do
    begin
      if ((FCryptModus = cmCryptChar) and (NumberChar in FCryptChar)) or
      ((FCryptModus = cmAutomatic) and (bNumber)) then
      begin
        if I <= Length(NumberArray) -1 then
        begin
          B := NumberArray[I];
          FillCharInArray;
        end;
      end;
      if ((FCryptModus = cmCryptChar) and (AdvancedChar in FCryptChar)) or
      ((FCryptModus = cmAutomatic) and (bAdvanced)) then
      begin
        if I <= Length(CharSpezialArray) -1 then
        begin
          B := CharSpezialArray[I];
          FillCharInArray;
        end;
      end;
      if ((FCryptModus = cmCryptChar) and (LowerChar in FCryptChar)) or
      ((FCryptModus = cmAutomatic) and (bLower)) then
      begin
        if I <= Length(CharLowerArray) -1 then
        begin
          B := CharLowerArray[I];
          FillCharInArray;
        end;
      end;
      if ((FCryptModus = cmCryptChar) and (SpezialChar in FCryptChar)) or
      ((FCryptModus = cmAutomatic) and (bSpezial)) then
      begin
        if I <= Length(CharExtArray) -1 then
        begin
          B := CharExtArray[I];
          FillCharInArray;
        end;
      end;
      if ((FCryptModus = cmCryptChar) and (UpperChar in FCryptChar)) or
      ((FCryptModus = cmAutomatic) and (bUpper)) then
      begin
        if I <= Length(CharUpperArray) -1 then
        begin
          B := CharUpperArray[I];
          FillCharInArray;
        end;
      end;
      if ((FCryptModus = cmCryptChar) and (Space in FCryptChar)) or
      (FCryptModus = cmAutomatic) then
      begin
        if I <= Length(SpaceArray) -1 then
        begin
          B := SpaceArray[I];
          FillCharInArray;
        end;
      end;
    end;

    // Array mit nicht zu ver/entschlüssenden Zeichen aufbauen
    X := 0;
    for I := 1 to Length(FNotCryptChar) do
    begin
      Inc(X);
      SetLength(NotCryptArray, X);
      NotCryptArray[X -1] := Ord(FNotCryptChar[I]);
    end;

    // array prüfen
    if Length(CryptArray) = 0 then
    begin
      ShowMessage('Error: INITIALIZATION');
      Result := False;
    end;
  except
    ShowMessage('Error: INITIALIZATION');
    Result := False;
  end;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCryptPro.Destroy;
begin
  NumberArray := nil;
  CharExtArray := nil;
  CharUpperArray := nil;
  CharLowerArray := nil;
  CharSpezialArray := nil;
  CryptArray := nil;
  AsciiArray := nil;
  FNotCryptStrings.Free;
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
// verschlüsseln
function TCHCryptPro.DoDecrypt(Text: string): string;
var
  I, A, nIdx, nNewIdx, nChar, LenArray, nDeChar, nStr, nOffset : Integer;
  sInput, sOutput : string;
  sChar : Char;
  bDoCrypt : Boolean;
begin
  if BuildArrays(Text) = False then
    Exit;

  sOutput := '';
  I := 0;
  nOffset := 1;

  if Length(Text) > 0 then
  begin
    sInput := Text;
    LenArray := Length(CryptArray);
    while I <> Length(sInput) do
    begin
      bDoCrypt := True;
      Inc(I);

      // Strings, die nicht verschlüsselt werden sollen
      if FNotCryptStrings.Count > 0 then
      begin
        for A := 0 to FNotCryptStrings.Count -1 do
        begin
          nStr := PosEx(FNotCryptStrings[A], Text, nOffset);
          if nStr = I then
          begin
            sOutput := sOutput + FNotCryptStrings[A];
            I := I + Length(FNotCryptStrings[A]) -1;
            nOffset := I;
            bDoCrypt := False;
          end;
        end;
      end;

      if bDoCrypt = True then
      begin
        sChar := sInput[I];
        nChar := Ord(sChar);
        nDeChar := nChar;

        for nIdx := 0 to LenArray -1 do
        begin
          if nChar = CryptArray[nIdx] then
          begin
            nNewIdx := (FKey1 * FKey2) + (FKey3 * FKey4) + I;
            nNewIdx := nIdx + nNewIdx;

            while nNewIdx > (LenArray - 1) do
            begin
              nNewIdx := nNewIdx - (LenArray - 1);
            end;
            nDeChar := CryptArray[nNewIdx];
            Break;
          end;
        end;
        sOutput := sOutput + Chr(nDeChar);
      end;
    end;
  end;
  Result := sOutput;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
// entschlüsseln
function TCHCryptPro.DoEncrypt(Text: string): string;
var
  I, A, nIdx, nNewIdx, nChar, LenArray, nEnChar, nStr, nOffset : Integer;
  sInput, sOutput : string;
  sChar : Char;
  bDoCrypt : Boolean;
begin
  if BuildArrays(Text) = False then
    Exit;

  sOutput := '';
  I := 0;
  nOffset := 1;

  if Length(Text) > 0 then
  begin
    sInput := Text;
    LenArray := Length(CryptArray);
    while I <> Length(sInput) do
    begin
      bDoCrypt := True;
      Inc(I);

      // Strings, die nicht entschlüsselt werden sollen
      if FNotCryptStrings.Count > 0 then
      begin
        for A := 0 to FNotCryptStrings.Count -1 do
        begin
          nStr := PosEx(FNotCryptStrings[A], Text, nOffset);
          if nStr = I then
          begin
            sOutput := sOutput + FNotCryptStrings[A];
            I := I + Length(FNotCryptStrings[A]) -1;
            nOffset := I;
            bDoCrypt := False;
          end;
        end;
      end;

      if bDoCrypt = True then
      begin
        sChar := sInput[I];
        nChar := Ord(sChar);
        nEnChar := nChar;

        for nIdx := 0 to LenArray -1 do
        begin
          if nChar = CryptArray[nIdx] then
          begin
            nNewIdx := (FKey1 * FKey2) + (FKey3 * FKey4) + I;
            nNewIdx := nIdx - nNewIdx;

            while nNewIdx < 0 do
            begin
              nNewIdx := nNewIdx + (LenArray - 1);
            end;
            nEnChar := CryptArray[nNewIdx];
            Break;
          end;
        end;
        sOutput := sOutput + Chr(nEnChar);
      end;
    end;
  end;
  Result := sOutput;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.DoDecrypt(Text, NoCryptChar : string; Key1, Key2, Key3,
  Key4: Byte): string;
begin
  FKey1 := Key1;
  FKey2 := Key2;
  FKey3 := Key3;
  FKey4 := Key4;
  FNotCryptChar := NoCryptChar;
  Result := DoDecrypt(Text);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.DoEncrypt(Text, NoCryptChar : string; Key1, Key2, Key3,
  Key4: Byte): string;
begin
  FKey1 := Key1;
  FKey2 := Key2;
  FKey3 := Key3;
  FKey4 := Key4;
  FNotCryptChar := NoCryptChar;
  Result := DoEncrypt(Text);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCryptPro.SetNotCryptStrings(const Value: TStrings);
begin
  FNotCryptStrings.Assign(Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.DoDecryptFile: Boolean;
var
  InputStream, OutputStream : TFileStream;
  InputStr, OutputStr : string;
  Handle : Integer;
  bOk : Boolean;
begin
  Result := False;
  bOk := True;
  InputStr := EmptyStr;
  OutputStr := EmptyStr;

  // check input
  if not FileExists(FFileCrypt.FInputfile) then
  begin
    bOk := False;
    ShowMessage('Error: Inputfile "'+FFileCrypt.FInputfile+'" not found!');
  end;

  // check output
  if bOk then
  begin
    if not FileExists(FFileCrypt.FOutputfile) then
    begin
      Handle := FileCreate(FFileCrypt.FOutputfile);
      if Handle = -1 then
      begin
        bOk := False;
        ShowMessage('Error: Outputfile "'+FFileCrypt.FOutputfile+'" could not be created!');
      end;
    end;
  end;

  // read file and decrypt
  if bOk then
  begin
    InputStream := TFileStream.Create(FFileCrypt.FInputfile, fmOpenRead);
    OutputStream := TFileStream.Create(FFileCrypt.FOutputfile, fmOpenWrite);
    try
      if InputStream.Size > 0 then
      begin
        SetLength(InputStr, InputStream.Size);
        SetLength(OutputStr, InputStream.Size);
        InputStream.Read(InputStr[1], InputStream.Size);
        OutputStr := DoDecrypt(InputStr);
        OutputStream.Write(OutputStr[1], InputStream.Size);
        Result := True;
      end;
    finally
      InputStream.Free;
      OutputStream.Free;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCryptPro.DoEncryptFile: Boolean;
var
  InputStream, OutputStream : TFileStream;
  InputStr, OutputStr : string;
  Handle : Integer;
  bOk : Boolean;
begin
  Result := False;
  bOk := True;
  InputStr := EmptyStr;
  OutputStr := EmptyStr;

  // check input
  if not FileExists(FFileCrypt.FInputfile) then
  begin
    bOk := False;
    ShowMessage('Error: Inputfile "'+FFileCrypt.FInputfile+'" not found!');
  end;

  // check output
  if bOk then
  begin
    if not FileExists(FFileCrypt.FOutputfile) then
    begin
      Handle := FileCreate(FFileCrypt.FOutputfile);
      if Handle = -1 then
      begin
        bOk := False;
        ShowMessage('Error: Outputfile "'+FFileCrypt.FOutputfile+'" could not be created!');
      end;
    end;
  end;

  // read file and encrypt
  if bOk then
  begin
    InputStream := TFileStream.Create(FFileCrypt.FInputfile, fmOpenRead);
    OutputStream := TFileStream.Create(FFileCrypt.FOutputfile, fmOpenWrite);
    try
      if InputStream.Size > 0 then
      begin
        SetLength(InputStr, InputStream.Size);
        SetLength(OutputStr, InputStream.Size);
        InputStream.Read(InputStr[1], InputStream.Size);
        OutputStr := DoEncrypt(InputStr);
        OutputStream.Write(OutputStr[1], InputStream.Size);
        Result := True;
      end;
    finally
      InputStream.Free;
      OutputStream.Free;
    end;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHFileCrypt }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFileCrypt.Create(AOwner: TCHCryptPro);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHFileCrypt.Destroy;
begin

  inherited Destroy;
end;

procedure TCHFileCrypt.SetInputfile(const Value: string);
begin
  if FInputfile <> Value then
  begin
    FInputfile := Value;
  end;
end;

procedure TCHFileCrypt.SetOutputfile(const Value: string);
begin
  if FOutputfile <> Value then
  begin
    FOutputfile := Value;
  end;
end;



end.
