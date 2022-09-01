{+--------------------------------------------------------------------------+
 | Component:   TmPasParser
 | Created:     11.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: A Pascal parser.
 | Version:     1.91
 | Status:      FreeWare
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risk.
 +--------------------------------------------------------------------------+}

unit mwPasParser;

interface

uses
  Windows, SysUtils;

type
  TTokenKind = (tkAbsolute, tkAbstract, tkAnd, tkAnsiComment, tkArray, tkAs,
    tkAsciiChar, tkAsm, tkAssembler, tkAssign, tkAutomated, tkBegin, tkBadString,
    tkBorComment, tkCase, tkCdecl, tkClass, tkColon, tkComma, tkCompDirect,
    tkConst, tkConstructor, tkCRLF, tkCRLFCo, tkDefault, tkDestructor, tkDispid,
    tkDispinterface, tkDiv, tkDo, tkDotDot, tkDownto, tkDynamic, tkElse, tkEnd,
    tkEqual, tkError, tkExcept, tkExport, tkExports, tkExternal, tkFar, tkFile,
    tkFinalization, tkFinally, tkFloat, tkFor, tkForward, tkFunction, tkGoto,
    tkGreater, tkGreaterEqual, tkIdentifier, tkIf, tkImplementation, tkIn,
    tkIndex, tkInherited, tkInitialization, tkInline, tkInteger, tkInterface,
    tkIs, tkKeyString, tkLabel, tkLibrary, tkLower, tkLowerEqual, tkMessage,
    tkMinus, tkMod, tkName, tkNear, tkNil, tkNodefault, tkNone, tkNot,
    tkNotEqual, tkNull, tkNumber, tkObject, tkOf, tkOr, tkOut, tkOverride,
    tkPacked, tkPascal, tkPlus, tkPoint, tkPrivate, tkProcedure, tkProgram,
    tkProperty, tkProtected, tkPublic, tkPublished, tkRaise, tkRead, tkReadonly,
    tkRecord, tkRegister, tkRepeat, tkResident, tkResourcestring, tkRoundClose,
    tkRoundOpen, tkSafecall, tkSemiColon, tkSet, tkShl, tkShr, tkSlash,
    tkSlashesComment, tkSquareClose, tkSquareOpen, tkSpace, tkStar, tkStdcall,
    tkStored, tkString, tkStringresource, tkSymbol, tkThen, tkThreadvar, tkTo,
    tkTry, tkType, tkUnit, tkUnknown, tkUntil, tkUses, tkVar, tkVirtual, tkWhile,
    tkWith, tkWrite, tkWriteonly, tkXor, tkReintroduce, tkOverload, tkImplements,
    tkInt64, tkLongWord); // D4 items added by Erik Berry

  TIdentDirect = Set of TTokenKind;

  TCommentState = (csAnsi, csBor, csNo, csSlashes);

  TmPasParser = class;

  TmPasToken = class(TObject)
  private
    function GetData: String;
  public
    ID: TTokenKind;
    LineNumber: Integer;
    LinePos: Integer;
    Position: Integer;
    Length: Integer;
    Origin: PChar;
    constructor Create;
    property Data: String read GetData;
  end;

  TmPasParser = class(TObject)
  private
    FToken: TmPasToken;
    FComment: TCommentState;
    FEndCount: Integer;
    FImplementationsPos: LongInt;
    FLastComment: LongInt;
    FLastIdentPos: LongInt;
    FLastSemiColon: LongInt;
    fOrigin: PChar;
    Run: LongInt;
    FRoundCount: ShortInt;
    FSquareCount: ShortInt;
    FVisibility: TTokenKind;
    function GetIsJunk: Boolean;
    function IdentKind: TTokenKind;
    procedure SetOrigin(value: PChar);
    procedure SetRunPos(NewPos: LongInt);
    procedure HandleComments;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetSubString(StartPos, EndPos: LongInt): String;
    procedure NextClassLine;
    procedure NextObjectLine;
    procedure NextID(ID: TTokenKind);
    procedure NextNonComment;
    procedure NextNonJunk;
    procedure NextNonSpace;
    procedure NextToken;
    procedure ToLineStart;
    function GetMethodImpLine(ClassName: String; MethodName: String): LongInt;
    property Comments: TCommentState read FComment write FComment;
    property EndCount: Integer read FEndCount write FEndCount;
    property ImplementationsPos: LongInt read FImplementationsPos;
    property IsJunk: Boolean read GetIsJunk;
    property LastComment: LongInt read FLastComment;
    property LastIdentPos: LongInt read FLastIdentPos;
    property LastSemiColon: LongInt read FLastSemiColon;
    property Origin: PChar read fOrigin write SetOrigin;
    property RunPos: LongInt read Run write SetRunPos;
    property RoundCount: ShortInt read FRoundCount write FRoundCount;
    property SquareCount: ShortInt read FSquareCount write FSquareCount;
    property Token: TmPasToken read FToken;
    property Visibility: TTokenKind read FVisibility write FVisibility;
  published
  end;

Const
  IdentDirect: TIdentDirect = [tkAbsolute, tkAbstract, tkAssembler, tkCdecl,
    tkDefault, tkDispid, tkDynamic, tkExport, tkExternal, tkFar, tkForward,
    tkIdentifier, tkIndex, tkMessage, tkName, tkNear, tkNodefault, tkOverride,
    tkPascal, tkRead, tkReadonly, tkRegister, tkResident, tksafecall, tkstdcall,
    tkStored, tkVirtual, tkWrite, tkWriteonly, tkReintroduce, tkOverload, tkImplements];

  BigIdentDirect: TIdentDirect = [tkAbsolute, tkAbstract, tkAssembler,
    tkAutomated, tkCdecl, tkDefault, tkDispid, tkDynamic, tkExport, tkExternal,
    tkFar, tkForward, tkIdentifier, tkIndex, tkMessage, tkName, tkNear,
    tkNodefault, tkOverride, tkPascal, tkPrivate, tkProtected, tkPublic,
    tkPublished, tkRead, tkReadonly, tkRegister, tkResident, tksafecall,
    tkstdcall, tkStored, tkVirtual, tkWrite, tkWriteOnly, tkReintroduce, tkOverload, tkImplements];

implementation

constructor TmPasToken.Create;
begin
  inherited Create;
  ID := tkNone;
  LineNumber := 0;
  LinePos := 0;
  Position := 0;
  Length := 0;
end; { Create }

function TmPasToken.GetData;
begin
  SetString(Result, (Origin + Position), Length);
end; { GetData }


destructor TmPasParser.Destroy;
begin
  Token.Free;
  inherited Destroy;
end; { Destroy }

constructor TmPasParser.Create;
begin
  inherited Create;
  FComment := csNo;
  FEndCount := 0;
  FImplementationsPos := 0;
  FToken := TmPasToken.Create;
  Visibility := tkUnknown;
end; { Create }

function TmPasParser.GetSubString(StartPos, EndPos: LongInt): String;
var
  SubLen: Integer;
begin
  if FOrigin[EndPos] = #10 then inc(EndPos);
  SubLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), SubLen);
end; { GetSubString }

procedure TmPasParser.SetOrigin(value: PChar);
begin
  FOrigin := value;
  Run := 0;
  FToken.Origin := value;
end; { SetOrigin }

procedure TmPasParser.SetRunPos(NewPos: LongInt);
begin
  Run := NewPos;
  NextToken;
end; { SetRunPos }

procedure TmPasParser.HandleComments;
begin
  Case FComment of
    csAnsi:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkAnsiComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                Case FToken.Position = Run of
                  True:
                    begin
                      FToken.Id := tkCRLFCo;
                      FToken.Length := 2;
                      FToken.Position := Run;
                      inc(Run);
                    end;
                  False:
                    begin
                      FToken.Length := Run - FToken.Position;
                    end;
                end;
                break;
              end;

            #10:
              begin
                inc(Run);
                inc(FToken.LineNumber);
                FToken.LinePos := Run;
                HandleComments;
                break;
              end;

            '*': if FOrigin[Run + 1] = ')' then
              begin
                inc(Run, 2);
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                break;
              end;
          end;
          inc(Run);
        end;
      end;

    csBor:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkBorComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                Case FToken.Position = Run of
                  True:
                    begin
                      FToken.Id := tkCRLFCo;
                      FToken.Length := 2;
                      FToken.Position := Run;
                      inc(Run);
                    end;
                  False:
                    begin
                      FToken.Length := Run - FToken.Position;
                    end;
                end;
                break;
              end;

            #10:
              begin
                inc(Run);
                inc(FToken.LineNumber);
                FToken.LinePos := Run;
                HandleComments;
                break;
              end;

            '}':
              begin
                inc(Run);
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                break;
              end;
          end;
          inc(Run);
        end;
      end;

    csSlashes:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkSlashesComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                break;
              end;
          end;
          inc(Run);
        end;
      end;

  end;
end; { HandleComments }

function TmPasParser.IdentKind: TTokenKind;
var
  HashKey: Integer;
  aToken: String;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to Length(aToken) do Result := Result + Ord(aToken[i]);
  end; { KeyHash }

begin
  Result := tkIdentifier;
  aToken := UpperCase(FToken.GetData);
  HashKey := KeyHash;
  Case HashKey of
    143: if aToken = 'IF' then Result := tkIf;
    147: if aToken = 'DO' then Result := tkDo;
    148: if aToken = 'AS' then Result := tkAs;
    149: if aToken = 'OF' then Result := tkOf;
    151: if aToken = 'IN' then Result := tkIn;
    156: if aToken = 'IS' then Result := tkIs;
    161: if aToken = 'OR' then Result := tkOr;
    163: if aToken = 'TO' then Result := tkTo;
    211: if aToken = 'AND' then Result := tkAnd;
    215: if aToken = 'END' then Result := tkEnd;
    217: if aToken = 'FAR' then Result := tkFar;
    224: if aToken = 'MOD' then Result := tkMod;
    225: if aToken = 'ASM' then Result := tkAsm;
    227:
      begin
        if aToken = 'DIV' then Result := tkDiv;
        if aToken = 'NIL' then Result := tkNil;
      end;
    231:
      begin
        if aToken = 'FOR' then Result := tkFor;
        if aToken = 'SHL' then Result := tkShl;
      end;
    233: if aToken = 'VAR' then Result := tkVar;
    236: if aToken = 'SET' then Result := tkSet;
    237: if aToken = 'SHR' then Result := tkShr;
    241: if aToken = 'NOT' then Result := tkNot;
    248: if aToken = 'OUT' then Result := tkOut;
    249: if aToken = 'XOR' then Result := tkXor;
    255: if aToken = 'TRY' then Result := tkTry;
    284:
      begin
        if aToken = 'CASE' then Result := tkCase;
        if aToken = 'READ' then Result := tkRead;
      end;
    288: if aToken = 'FILE' then Result := tkFile;
    289: if aToken = 'NAME' then Result := tkName;
    294: if aToken = 'NEAR' then Result := tkNear;
    297: if aToken = 'ELSE' then Result := tkElse;
    303: if aToken = 'THEN' then Result := tkThen;
    313: if aToken = 'GOTO' then Result := tkGoto;
    316: if aToken = 'WITH' then Result := tkWith;
    320:
      begin
        if aToken = 'UNIT' then Result := tkUnit;
        if aToken = 'USES' then Result := tkUses;
      end;
    322: if aToken = 'TYPE' then Result := tkType;
    341: if aToken = 'INT64' then Result := tkInt64;
    347: if aToken = 'CDECL' then Result := tkCdecl;
    352: if aToken = 'LABEL' then Result := tkLabel;
    357: if aToken = 'BEGIN' then Result := tkBegin;
    372: if aToken = 'RAISE' then Result := tkRaise;
    374: if aToken = 'CLASS' then Result := tkClass;
    376: if aToken = 'INDEX' then Result := tkIndex;
    377: if aToken = 'WHILE' then Result := tkWhile;
    383: if aToken = 'ARRAY' then Result := tkArray;
    391: if aToken = 'CONST' then Result := tkConst;
    395: if aToken = 'WRITE' then Result := tkWrite;
    396: if aToken = 'UNTIL' then Result := tkUntil;
    424: if aToken = 'PACKED' then Result := tkPacked;
    436: if aToken = 'PASCAL' then Result := tkPascal;
    439: if aToken = 'OBJECT' then Result := tkObject;
    445: if aToken = 'DISPID' then Result := tkDispid;
    447:
      begin
        if aToken = 'INLINE' then Result := tkInline;
        if aToken = 'PUBLIC' then Result := tkPublic;
        if aToken = 'RECORD' then Result := tkRecord;
      end;
    449: if aToken = 'REPEAT' then Result := tkRepeat;
    457: if aToken = 'EXCEPT' then Result := tkExcept;
    465: if aToken = 'STORED' then Result := tkStored;
    471: if aToken = 'STRING' then Result := tkKeyString;
    475: if aToken = 'DOWNTO' then Result := tkDownto;
    482: if aToken = 'EXPORT' then Result := tkExport;
    517:
      begin
        if aToken = 'DEFAULT' then Result := tkDefault;
        if aToken = 'DYNAMIC' then Result := tkDynamic;
        if aToken = 'MESSAGE' then Result := tkMessage;
      end;
    519: if aToken = 'STDCALL' then Result := tkStdcall;
    527: if aToken = 'FINALLY' then Result := tkFinally;
    533:
      begin
        if aToken = 'FORWARD' then Result := tkForward;
        if aToken = 'LIBRARY' then Result := tkLibrary;
      end;
    536: if aToken = 'PROGRAM' then Result := tkProgram;
    539: if aToken = 'PRIVATE' then Result := tkPrivate;
    551: if aToken = 'VIRTUAL' then Result := tkVirtual;
    565: if aToken = 'EXPORTS' then Result := tkExports;
    571: if aToken = 'SAFECALL' then Result := tkSafecall;
    596: if aToken = 'ABSTRACT' then Result := tkAbstract;
    604: if aToken = 'OVERLOAD' then Result := tkOverload;
    606:
      begin
        if aToken = 'READONLY' then Result := tkReadonly;
        if aToken = 'RESIDENT' then Result := tkResident;
      end;
    607: if aToken = 'ABSOLUTE' then Result := tkAbsolute;
    608: if aToken = 'OVERRIDE' then Result := tkOverride;
    611: if aToken = 'EXTERNAL' then Result := tkExternal;
    613: if aToken = 'REGISTER' then Result := tkRegister;
    614: if aToken = 'FUNCTION' then Result := tkFunction;
    620: if aToken = 'LONGWORD' then Result := tkLongWord;
    645: if aToken = 'PROPERTY' then Result := tkProperty;
    657: if aToken = 'INTERFACE' then Result := tkInterface;
    668: if aToken = 'INHERITED' then Result := tkInherited;
    670: if aToken = 'ASSEMBLER' then Result := tkAssembler;
    672: if aToken = 'PUBLISHED' then Result := tkPublished;
    673: if aToken = 'THREADVAR' then Result := tkThreadvar;
    674: if aToken = 'NODEFAULT' then Result := tkNodefault;
    676: if aToken = 'AUTOMATED' then Result := tkAutomated;
    681: if aToken = 'PROCEDURE' then Result := tkProcedure;
    682: if aToken = 'PROTECTED' then Result := tkProtected;
    717: if aToken = 'WRITEONLY' then Result := tkWriteonly;
    766: if aToken = 'IMPLEMENTS' then Result := tkImplements;
    783: if aToken = 'DESTRUCTOR' then Result := tkDestructor;
    836: if aToken = 'REINTRODUCE' then Result := tkReintroduce;
    870: if aToken = 'CONSTRUCTOR' then Result := tkConstructor;
    904: if aToken = 'FINALIZATION' then Result := tkFinalization;
    961: if aToken = 'DISPINTERFACE' then Result := tkDispinterface;
    1062: if aToken = 'IMPLEMENTATION' then Result := tkImplementation;
    1064: if aToken = 'INITIALIZATION' then Result := tkInitialization;
    1087:
      begin
        if aToken = 'RESOURCESTRING' then Result := tkResourcestring;
        if aToken = 'STRINGRESOURCE' then Result := tkStringresource;
      end;
  end;
  Case Result of
    tkIdentifier: FLastIdentPos := FToken.Position;
    tkImplementation: FImplementationsPos := FToken.Position;
    tkCase: inc(FEndCount);
    tkClass: FEndCount := 1;
    tkBegin: inc(FEndCount);
    tkEnd: dec(FEndCount);
    tkRecord: inc(FEndCount);
    tkObject: inc(FEndCount);
  end;
end; { IdentKind }

procedure TmPasParser.NextToken;
begin
  Case FOrigin[Run] of
    #0:
      begin
        FToken.Id := tkNull;
        FToken.Length := 0;
        FToken.Position := Run;
        Exit;
      end;
  end;

  FToken.Position := Run;
  if FComment <> csNo then HandleComments
  else
  begin
    Case FOrigin[Run] of
      #10:
        begin
          inc(Run);
          inc(FToken.LineNumber);
          FToken.LinePos := Run;
          NextToken;
        end;

      #13:
        begin
          FToken.Id := tkCRLF;
          FToken.Length := 2;
          FToken.Position := Run;
          if not (fOrigin[Run + 1] = #10) then
            Inc(FToken.LineNumber);
          inc(Run);
        end;

      #1..#9, #11, #12, #14..#32:
        begin
          FToken.Position := Run;
          while FOrigin[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
          FToken.ID := tkSpace;
          FToken.Length := Run - FToken.Position;
        end;

      '/':
        Case FOrigin[Run + 1] of
          '/':
            begin
              FComment := csSlashes;
              HandleComments;
            end;
        else
          begin
            FToken.Position := Run;
            inc(Run);
            FToken.ID := tkSymbol;
            FToken.Length := 1;
          end;
        end;

      '(':
        Case FOrigin[Run + 1] of
          '*':
            begin
              FComment := csAnsi;
              HandleComments;
            end;
          '.':
            begin
              FToken.Position := Run;
              inc(Run, 2);
              FToken.ID := tkSquareOpen;
              FToken.Length := 2;
            end;
        else
          begin
            FToken.Position := Run;
            inc(Run);
            FToken.Length := 1;
            FToken.ID := tkRoundOpen;
            inc(FRoundCount);
          end;
        end;

      'A'..'Z', 'a'..'z', '_':
        begin
          FToken.Position := Run;
          inc(Run);
          while FOrigin[Run] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do inc(Run);
          FToken.Length := Run - FToken.Position;
          FToken.ID := IdentKind;
        end;

      '0'..'9':
        begin
          FToken.Position := Run;
          inc(Run);
          FToken.ID := tkNumber;
          while FOrigin[Run] in ['0'..'9', '.', 'e', 'E'] do
          begin
            Case FOrigin[Run] of
              '.': if FOrigin[Run + 1] <> '.' then FToken.ID := tkFloat else break;
            end;
            inc(Run);
          end;
          FToken.Length := Run - FToken.Position;
        end;

      '{':
        begin
          FComment := csBor;
          HandleComments;
        end;

      '!', '"', '%', '&', ')'..'.', ':'..'@', '['..'^', '`', '~':
        begin
          FToken.Position := Run;
          Case FOrigin[Run] of
            ')':
              begin
                FToken.ID := tkRoundClose;
                dec(FRoundCount);
              end;

            '*': FToken.ID := tkStar;

            '+': FToken.ID := tkPlus;

            ',': FToken.ID := tkComma;

            '-': FToken.ID := tkMinus;

            '.':
              Case FOrigin[Run + 1] of
                ')':
                  begin
                    inc(Run);
                    FToken.ID := tkSquareClose;
                  end;
              else FToken.ID := tkPoint;
              end;

            ':':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    inc(Run);
                    FToken.ID := tkAssign;
                  end;
              else FToken.ID := tkColon;
              end;

            ';':
              begin
                FToken.ID := tkSemiColon;
                FLastSemiColon := Run;
              end;

            '<':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    inc(Run);
                    FToken.ID := tkLowerEqual;
                  end;
                '>':
                  begin
                    inc(Run);
                    FToken.ID := tkNotEqual;
                  end;
              else FToken.ID := tkLower;
              end;

            '=': FToken.ID := tkEqual;

            '>':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    inc(Run);
                    FToken.ID := tkGreaterEqual;
                  end;
              else FToken.ID := tkGreater;
              end;

            '[':
              begin
                FToken.ID := tkSquareOpen;
                inc(FSquareCount);
              end;

            ']':
              begin
                FToken.ID := tkSquareClose;
                dec(FSquareCount);
              end;

          else FToken.ID := tkSymbol;

          end;
          inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

      #39:
        begin
          FToken.ID := tkString;
          if (FOrigin[Run + 1] = #39) and (FOrigin[Run + 2] = #39) then inc(Run, 2);
          repeat
            Case FOrigin[Run] of
              #0, #10, #13:
                begin
                  FToken.ID := tkBadString;
                  break;
                end;
            end;
            inc(Run);
          until FOrigin[Run] = #39;
          if FOrigin[Run] <> #0 then inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

      '#':
        begin
          FToken.Position := Run;
          FToken.ID := tkAsciiChar;
          inc(Run);
          while FOrigin[Run] in ['0'..'9'] do inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

      '$':
        begin
          FToken.Position := Run;
          FToken.ID := tkInteger;
          inc(Run);
          while FOrigin[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

    else
      begin
        FToken.Position := Run;
        inc(Run);
        FToken.ID := tkUnknown;
        FToken.Length := Run - FToken.Position;
      end;
    end;
  end;
end; {NextToken}

procedure TmPasParser.NextID(ID: TTokenKind);
begin
  NextToken;
  while FToken.ID <> ID do
  begin
    Case FToken.ID of
      tkNull: break;
    else NextToken;
    end;
  end;
end; { NextID }

function TmPasParser.GetIsJunk: Boolean;
begin
  Case FToken.ID of
    tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo, tkSlashesComment, tkSpace:
      Result := True;
  else Result := False;
  end;
end;

procedure TmPasParser.NextNonComment;
begin
  repeat
    NextToken;
  until not (Token.ID in [tkAnsiComment, tkBorComment, tkCRLFCo,
    tkSlashesComment]);
end; { NextNonComCRLF }

procedure TmPasParser.NextNonJunk;
begin
  repeat
    NextToken;
  until not (Token.ID in [tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo,
    tkSlashesComment, tkSpace]);
end; { NextNonJunk }

procedure TmPasParser.NextNonSpace;
begin
  NextToken;
  while FToken.ID = tkSpace do NextToken;
end; { NextNonSpace }

procedure TmPasParser.ToLineStart;
begin
  Run := FToken.LinePos;
  NextToken;
end; { ToLineStart }

procedure TmPasParser.NextClassLine;
begin
  while FToken.ID <> tkNull do
  begin
    if FToken.ID in BigIdentDirect then
    begin
      FLastIdentPos := FToken.Position;
      NextNonJunk;
      if FToken.ID = tkEqual then
      begin
        NextNonJunk;
        if FToken.ID = tkClass then break;
      end;
    end;
    NextNonJunk;
  end;
end; { NextClassLine }

procedure TmPasParser.NextObjectLine;
begin
  while FToken.ID <> tkNull do
  begin
    if FToken.ID in BigIdentDirect then
    begin
      FLastIdentPos := FToken.Position;
      NextNonJunk;
      if FToken.ID = tkEqual then
      begin
        NextNonJunk;
        if FToken.ID in [tkClass, tkDispInterface, tkInterface] then break;
      end;
    end;
    NextNonJunk;
  end;
end; { NextObjectLine }

function TmPasParser.GetMethodImpLine(ClassName: String; MethodName: String): LongInt;
begin
  Result := -1;
  while FToken.ID <> tkNull do
  begin
    NextToken;
    if FToken.ID in [tkClass, tkConstructor, tkDestructor, tkFunction, tkProcedure] then
    begin
      // For class methods, skip past the "class" keyword: function/procedure is next
      if FToken.ID = tkClass then
        NextNonJunk;
      NextNonJunk;
      if UpperCase(FToken.Data) = UpperCase(ClassName) then
      begin
        NextNonJunk;
        if FToken.ID = tkPoint then
        begin
          NextNonJunk;
          if UpperCase(FToken.Data) = UpperCase(MethodName) then
          begin
            Result := FToken.LineNumber;
            break;
          end;
        end;
      end;
    end;
  end;
end; { GetMethodImpLine }


end.

