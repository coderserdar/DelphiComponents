{*******************************************************}
{File:      NCOciUtil.PAS                               }
{Revision:  0.02.00 / 4.11.2001                         }
{Comment:   NC OCI8 VCL: utility unit                   }
{Copyright: (c) 1999-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciUtil;

interface

Uses SysUtils, Windows;

type
    TOciChars = set of Char;

    POCIThunk = ^TOCIThunk;
    TOCIThunk = packed record
        MOVEAX: Byte;
        SelfPtr: Pointer;
        PUSHEAX: Byte;
        JMP: Byte;
        JmpOffset: Integer;
    end;

    TOCIHack = class
    private
        FOrigProcAddr: PByte;
        FReplProcAddr: PByte;
        FHacked: Boolean;
        FSavedCode: PByte;
        procedure Call;
    public
        constructor Create(AOrigProcAddr: Pointer; AReplProcAddr: Pointer);
        destructor Destroy; override;
        procedure Hack;
        procedure Unhack;
    end;

{$IFDEF OCI_D4}
function StrLike(const AStr, AMask: String; ANoCase: Boolean = False;
    AManyCharsMask: Char = '%'; AOneCharMask: Char = '_'; AEscapeChar: Char = '\'): Boolean;
{$ELSE}
function StrLike(const AStr, AMask: String; ANoCase: Boolean;
    AManyCharsMask: Char; AOneCharMask: Char; AEscapeChar: Char): Boolean;
{$ENDIF}
function StrRPos(Str1, Str2: PChar): PChar;
function StrToken(const cString: string; AChars: TOciChars; var iFrom: Integer): string;
function StrIPos(const Sub: string; const S: string; fromI: Integer): Integer;
function VerStr2Int(const AVersion: String): Integer;
function VarNearEqual(const V1, V2: Variant): Boolean;
function UCOraName(const AName: String): String;
function NormOraName(const AName: String): String;

implementation

{$IFDEF OCI_D6}
Uses Variants;
{$ENDIF}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIHack

constructor TOCIHack.Create(AOrigProcAddr: Pointer; AReplProcAddr: Pointer);
begin
    inherited Create;
    FOrigProcAddr := AOrigProcAddr;
    FReplProcAddr := AReplProcAddr;
    FHacked := False;
end;

destructor TOCIHack.Destroy;
begin
    if FHacked then
        Unhack;
    inherited Destroy;
end;

procedure TOCIHack.Hack;
var
    SaveFlag: DWORD;
begin
    if not FHacked then
        if VirtualProtect(FOrigProcAddr, SizeOf(TOCIThunk), PAGE_EXECUTE_READWRITE, @SaveFlag) then
            try
                FHacked := True;
                GetMem(FSavedCode, SizeOf(TOCIThunk));
                Move(FOrigProcAddr^, FSavedCode^, SizeOf(TOCIThunk));
                with POCIThunk(FOrigProcAddr)^ do begin
                    MOVEAX := $B8;
                    SelfPtr := Self;
                    PUSHEAX := $50;
                    JMP := $E9;
                    JmpOffset := Integer(@TOCIHack.Call) - Integer(@POCIThunk(FOrigProcAddr)^.JMP) - 5;
                end;
            finally
                VirtualProtect(FOrigProcAddr, SizeOf(TOCIThunk), SaveFlag, @SaveFlag);
            end;
end;

procedure TOCIHack.Unhack;
var
    SaveFlag: DWORD;
begin
    if FHacked then
        if VirtualProtect(FOrigProcAddr, SizeOf(TOCIThunk), PAGE_EXECUTE_READWRITE, @SaveFlag) then
            try
                FHacked := False;
                Move(FSavedCode^, FOrigProcAddr^, SizeOf(TOCIThunk));
                FreeMem(FSavedCode, SizeOf(TOCIThunk));
            finally
                VirtualProtect(FOrigProcAddr, SizeOf(TOCIThunk), SaveFlag, @SaveFlag);
            end;
end;

procedure TOCIHack.Call;
asm
    POP EAX
    MOV EAX, [EAX].FReplProcAddr
    JMP EAX
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

{$IFDEF OCI_D4}
function StrLike(const AStr, AMask: String; ANoCase: Boolean = False;
    AManyCharsMask: Char = '%'; AOneCharMask: Char = '_'; AEscapeChar: Char = '\'): Boolean;
{$ELSE}
function StrLike(const AStr, AMask: String; ANoCase: Boolean;
    AManyCharsMask: Char; AOneCharMask: Char; AEscapeChar: Char): Boolean;
{$ENDIF}

    function InternalScan(pStr, pMask: PChar): Boolean;
    begin
        Result := True;
        while ((pStr^ = pMask^) and (pMask^ <> AManyCharsMask) or
               (pMask^ = AOneCharMask) or (pMask^ = AEscapeChar)) and
              (pStr^ <> #0) and (pMask^ <> #0) do begin
            if pMask^ = AEscapeChar then begin
                Inc(pMask);
                if pStr^ <> pMask^ then
                    Break;
            end;
            Inc(pMask);
            Inc(pStr);
        end;
        if pMask^ = AManyCharsMask then begin
            while (pMask^ = AManyCharsMask) or (pMask^ = AOneCharMask) do begin
                if pMask^ = AOneCharMask then
                    if pStr^ = #0 then begin
                        Result := False;
                        Exit;
                    end
                    else
                        Inc(pStr);
                Inc(pMask);
            end;
            if pMask^ <> #0 then begin
                while (pStr^ <> #0) and ((pMask^ <> pStr^) or not InternalScan(pStr, pMask)) do
                    Inc(pStr);
                Result := pStr^ <> #0;
            end;
        end
        else
            Result := (pMask^ = #0) and (pStr^ = #0);
    end;

begin
    if AMask = AManyCharsMask then
        Result := True
    else if (AMask = '') or (AStr = '') then
        Result := False
    else begin
        if ANoCase then
            Result := InternalScan(PChar(AnsiUpperCase(AStr)), PChar(AnsiUpperCase(AMask)))
        else
            Result := InternalScan(PChar(AStr), PChar(AMask));
    end;
end;

function StrRPos(Str1, Str2: PChar): PChar;
var
    p1, p2, pBSub, pESub, pBS, pES: PChar;
begin
    Result := nil;
    if (Str1 = nil) or (Str2 = nil) then
        Exit;
    pBSub := Str2;
    pESub := Str2 + StrLen(Str2) - 1;
    pBS := Str1 + StrLen(Str2) - 1;
    pES := Str1 + StrLen(Str1) - 1;
    while pES >= pBS do begin
        if pES^ = pESub^ then begin
            p1 := pES;
            p2 := pESub;
            while (p2 >= pBSub) and (p1^ = p2^) do begin
                Dec(p1);
                Dec(p2);
            end;
            if p2 < pBSub then begin
                Result := p1 + 1;
                Exit;
            end;
        end;
        Dec(pES);
    end;
end;

function StrToken(const cString: string; AChars: TOciChars; var iFrom: Integer): string;
var
    pS, pFrom: PChar;
begin
    if iFrom <= Length(cString) then begin
        pS := PChar(cString) + iFrom - 1;
        pFrom := pS;
        while (pS^ <> #0) and not (pS^ in AChars) do
            Inc(pS);
        Inc(iFrom, pS - pFrom + 1);
        SetString(Result, pFrom, pS - pFrom);
    end
    else
        Result := '';
end;

function StrIPos(const Sub: string; const S: string; fromI: Integer): Integer;
var
    P: PChar;
begin
    if fromI < 1 then
        fromI := 1;
    if fromI > Length(S) then
        Result := 0
    else begin
        P := StrPos(PChar(S) + (fromI - 1), PChar(Sub));
        if P = nil then
            Result := 0
        else
            Result := P - PChar(S) + 1;
    end;
end;

function VerStr2Int(const AVersion: String): Integer;
var
    i, i1: Integer;
    s: String;
begin
    Result := 0;
    s := AVersion;
    for i := 1 to 5 do begin
        if s = '' then
            Result := Result * 100
        else begin
            i1 := Pos('.', s);
            if i1 = 0 then
                i1 := Length(s) + 1;
            Result := Result * 100 + StrToInt(Trim(Copy(s, 1, i1 - 1)));
            s := Copy(s, i1 + 1, Length(s));
        end;            
    end;
end;

function VarNearEqual(const V1, V2: Variant): Boolean;
var
    vt1, vt2: Integer;
    dt1, dt2: TDateTime;
    d1, d2: Double;
begin
    vt1 := VarType(V1) and varTypeMask;
    vt2 := VarType(V2) and varTypeMask;
    if vt1 = vt2 then
        if vt1 = varDate then begin
            dt1 := V1;
            dt2 := V2;
            Result := Abs(dt1 - dt2) <= 1.0 / 86400.0;
        end
        else if vt1 in [varSingle, varDouble] then begin
            d1 := V1;
            d2 := V2;
            Result := Abs(d1 - d2) <= 1e-10;
        end
        else if vt1 in [varEmpty, varNull] then
            Result := True
        else
            Result := V1 = V2
    else
        Result := False;
end;

function UCOraName(const AName: String): String;
begin
    Result := TrimLeft(TrimRight(AName));
    if Result <> '' then
        if (Result[1] = '"') and (Result[Length(Result)] = '"') then
            Result := Copy(Result, 2, Length(Result) - 2)
        else
            Result := AnsiUpperCase(Result);
end;

function NormOraName(const AName: String): String;
var
    i: Integer;
begin
    Result := AName;
    for i := 1 to Length(AName) do
        if (i = 1) and not (AName[1] in ['a' .. 'z', 'A' .. 'Z']) or
           (i > 1) and not (AName[i] in ['a' .. 'z', 'A' .. 'Z',
                                         '0' .. '9', '#', '$', '_']) then begin
            Result := '"' + AName + '"';
            Break;
        end;
end;

end.
