
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit PasLexer;

interface

{$I STD.INC}

uses
  Classes, SysUtils, PasParser, WinTools;

{ The TPascalLexer class is a basic lexical analysis tool. When used in
  conjunction with a source code file, it is assumed that the source is in a
  perfect state. That is to say, the source file MUST conform exactly to Object
  Pascal grammer before being handed off to the TPascalLexer class. The result
  of a less than perfect source file could lead to an endless looping condition. }

type
  TSourceKind = (skArray, skClass, skClassReference, skConstant, skConstructor,
    skDestructor, skEnumeration, skEvent, skFunction, skFunctionEvent,
    skFunctionInterfaceMethod, skFunctionMethod, skFunctionPointer, skInterface,
    skInterfaceMethod, skMember, skMethod, skProcedure, skProcedureEvent,
    skProcedureInterfaceMethod, skProcedureMethod, skProcedurePointer, skProperty,
    skRecord, skResourceString, skSet, skThreadVariable, skType, skVariable);
  TSourceKinds = set of TSourceKind;

  TRoutineKind = (rkConstructor, rkDestructor, rkFunction, rkProcedure);
  TCallingConvention = (ccDefault, ccCdecl, ccPascal, ccRegister, ccStdcall,
    ccSafecall);

  TMemberVisibility = (mvPrivate, mvProtected, mvPublic, mvPublished);
  TMemberVisibilities = set of TMemberVisibility;

  TPropertyAccess = set of (paRead, paWrite);
  TVirtualKind = (vkStatic, vkDynamic, vkMessage, vkOverride, vkVirtual);

  TSourceInfo = packed record
    Line: Integer;
    Column: Integer;
    Name: ShortString;
    Kind: TSourceKind;
    case TSourceKind of
      skClass: (
        ClassParent: ShortString;
        InterfaceCount: Integer);
      skFunction, skProcedure: (
        RoutineKind: TRoutineKind;
        RoutineConvention: TCallingConvention);
      skInterface: (
        InterfaceParent: ShortString);
      skInterfaceMethod: (
        InterfaceName: ShortString;
        InterfaceMethodKind: TRoutineKind;
        InterfaceMethodConvention: TCallingConvention);
      skMember: (
        ClassName: ShortString;
        Visibility: TMemberVisibility;
        case TSourceKind of
          skMethod: (
            ClassMethod: Boolean;
            ClassMethodKind: TRoutineKind;
            VirtualKind: TVirtualKind;
            ClassMethodConvention: TCallingConvention);
          skProperty: (
            Access: TPropertyAccess;
            Default: Boolean));
  end;

  TLexicalClassEvent = procedure(Sender: TObject; const Body: string;
    const Info: TSourceInfo; const InheritedName: string;
    InterfaceList: TStrings) of object;
  TLexicalCommentEvent = procedure(Sender: TObject; const Comment: string) of object;
  TLexicalEvent = procedure(Sender: TObject; const Body: string;
    const Info: TSourceInfo) of object;
  TLexicalInterfaceEvent = procedure(Sender: TObject; const Body: string;
    const Info: TSourceInfo; const InheritedName: string) of object;
  TLexicalUsesEvent = procedure(Sender: TObject; UsesList: TStrings) of object;

  TPascalLexer = class(TPascalParser)
  private
    FFileName: string;
    FIdentifier: TPascalToken;
    FInterfaceList: TStrings;
    FMap: TMemoryMappedFile;
    FScratchToken: TPascalToken;
    FSection: TPascalTokenKind;
    FUnitName: string;
    FUnitPath: string;
    FOnArray: TLexicalEvent;
    FOnClass: TLexicalClassEvent;
    FOnClassReference: TLexicalEvent;
    FOnMethod: TLexicalEvent;
    FOnComment: TLexicalCommentEvent;
    FOnConstant: TLexicalEvent;
    FOnFunction: TLexicalEvent;
    FOnEnumeration: TLexicalEvent;
    FOnEvent: TLexicalEvent;
    FOnInterface: TLexicalInterfaceEvent;
    FOnInterfaceMethod: TLexicalEvent;
    FOnProcedure: TLexicalEvent;
    FOnProperty: TLexicalEvent;
    FOnRecord: TLexicalEvent;
    FOnRoutinePointer: TLexicalEvent;
    FOnSet: TLexicalEvent;
    FOnType: TLexicalEvent;
    FOnUses: TLexicalUsesEvent;
    FOnVariable: TLexicalEvent;
    procedure ReadArray;
    function ReadCallingConvention: TCallingConvention;
    procedure ReadClass;
    procedure ReadClassReference;
    procedure ReadComment;
    procedure ReadConstant;
    procedure ReadEnumeration;
    procedure ReadInterface;
    procedure ReadRecord;
    procedure ReadRoutine;
    procedure ReadRoutinePointer(Kind: TRoutineKind);
    procedure ReadSet;
    procedure ReadType;
    procedure ReadUses;
    procedure ReadVariable;
    procedure SetFileName(Value: string);
  protected
    procedure InvokeEvent(var SourceInfo: TSourceInfo; Event: TLexicalEvent);
    procedure StandardEvent(Kind: TSourceKind; Event: TLexicalEvent);
    property Identifier: TPascalToken read FIdentifier;
    property Section: TPascalTokenKind read FSection;
  public
    constructor Create(Buffer: PChar = nil; Size: Integer = 0); override;
    destructor Destroy; override;
    procedure Analyze;
    procedure Reset;
    property FileName: string read FFileName write SetFileName;
    property UnitName: string read FUnitName;
    property UnitPath: string read FUnitPath;
    property OnArray: TLexicalEvent read FOnArray write FOnArray;
    property OnClass: TLexicalClassEvent read FOnClass write FOnClass;
    property OnClassReference: TLexicalEvent read FOnClassReference write FOnClassReference;
    property OnComment: TLexicalCommentEvent read FOnComment write FOnComment;
    property OnConstant: TLexicalEvent read FOnConstant write FOnConstant;
    property OnFunction: TLexicalEvent read FOnFunction write FOnFunction;
    property OnEvent: TLexicalEvent read FOnEvent write FOnEvent;
    property OnEnumeration: TLexicalEvent read FOnEnumeration write FOnEnumeration;
    property OnInterface: TLexicalInterfaceEvent read FOnInterface write FOnInterface;
    property OnInterfaceMethod: TLexicalEvent read FOnInterfaceMethod write FOnInterfaceMethod;
    property OnMethod: TLexicalEvent read FOnMethod write FOnMethod;
    property OnProcedure: TLexicalEvent read FOnProcedure write FOnProcedure;
    property OnProperty: TLexicalEvent read FOnProperty write FOnProperty;
    property OnRecord: TLexicalEvent read FOnRecord write FOnRecord;
    property OnRoutinePointer: TLexicalEvent read FOnRoutinePointer write FOnRoutinePointer;
    property OnSet: TLexicalEvent read FOnSet write FOnSet;
    property OnType: TLexicalEvent read FOnType write FOnType;
    property OnUses: TLexicalUsesEvent read FOnUses write FOnUses;
    property OnVariable: TLexicalEvent read FOnVariable write FOnVariable;
  end;

const
  ClassMembers = [skConstructor, skDestructor, skFunctionMethod,
    skProcedureMethod, skProperty];
  InterfaceMembers = [skFunctionInterfaceMethod, skProcedureInterfaceMethod];

function SourceKindToString(SourceKind: TSourceKind): string;

implementation

uses
  StrConst;

function KindToRoutine(Kind: TPascalTokenKind): TRoutineKind;
begin
  case Kind of
    tkConstructor:
      Result := rkConstructor;
    tkDestructor:
      Result := rkDestructor;
    tkFunction:
      Result := rkFunction;
  else
    Result := rkProcedure;
  end;
end;

const
  SourceKinds: array[TSourceKind] of TIdentMapEntry = (
    (Value: Ord(skArray); Name: 'array'),
    (Value: Ord(skClass); Name: 'class'),
    (Value: Ord(skClassReference); Name: 'class reference'),
    (Value: Ord(skConstant); Name: 'constant'),
    (Value: Ord(skConstructor); Name: 'constructor'),
    (Value: Ord(skDestructor); Name: 'destructor'),
    (Value: Ord(skEnumeration); Name: 'enumeration'),
    (Value: Ord(skEvent); Name: 'event'),
    (Value: Ord(skFunction); Name: 'function'),
    (Value: Ord(skFunctionEvent); Name: 'function event'),
    (Value: Ord(skFunctionInterfaceMethod); Name: 'function interface method'),
    (Value: Ord(skFunctionMethod); Name: 'function method'),
    (Value: Ord(skFunctionPointer); Name: 'function pointer'),
    (Value: Ord(skInterface); Name: 'interface'),
    (Value: Ord(skInterfaceMethod); Name: 'interface method'),
    (Value: Ord(skMember); Name: 'member'),
    (Value: Ord(skMethod); Name: 'method'),
    (Value: Ord(skProcedure); Name: 'procedure'),
    (Value: Ord(skProcedureEvent); Name: 'procedure event'),
    (Value: Ord(skProcedureInterfaceMethod); Name: 'procedure interface method'),
    (Value: Ord(skProcedureMethod); Name: 'procedure method'),
    (Value: Ord(skProcedurePointer); Name: 'procedure pointer'),
    (Value: Ord(skProperty); Name: 'property'),
    (Value: Ord(skRecord); Name: 'record'),
    (Value: Ord(skresourceString); Name: 'resource string'),
    (Value: Ord(skSet); Name: 'set'),
    (Value: Ord(skThreadVariable); Name: 'thread variable'),
    (Value: Ord(skType); Name: 'type'),
    (Value: Ord(SkVariable); Name: 'variable'));

function SourceKindToString(SourceKind: TSourceKind): string;
begin
  IntToIdent(Ord(SourceKind), Result, SourceKinds);
end;

{ TPascalLexer }

constructor TPascalLexer.Create(Buffer: PChar; Size: Integer);
begin
  inherited Create(Buffer, Size);
  ExceptionTokens := [tkImplementation, tkNull];
  FIdentifier := TPascalToken.Create(Self);
  FInterfaceList := TStringList.Create;
  FScratchToken := TPascalToken.Create(Self);
  FSection := tkNull;
end;

destructor TPascalLexer.Destroy;
begin
  FIdentifier.Free;
  FInterfaceList.Free;
  FScratchToken.Free;
  FileName := '';
  inherited Destroy;
end;

procedure TPascalLexer.Reset;
begin
  Initialize(InternalBuffer, InternalSize);
  FSection := tkNull;
end;

procedure TPascalLexer.Analyze;
var
  InterfaceSection: Boolean;
begin
  if Position <> 0 then
    Reset;
  InterfaceSection := False;
  repeat
    Next;
    case Token.Kind of
      tkConst, tkResourcestring, tkThreadvar, tkType, tkVar: FSection := Token.Kind;
      tkIdentifier:
        Identifier.Copy(Token);
      tkArray: ReadArray;
      tkFunction, tkProcedure:
        if Peek(CommentTokens) = tkIdentifier then
        begin
          FSection := Token.Kind;
          ReadRoutine;
        end
        else
          ReadRoutinePointer(KindToRoutine(Token.Kind));
      tkLeftParenthesis:
        ReadEnumeration;
      tkRecord:
        ReadRecord;
      tkSet:
        ReadSet;
      tkEqual:
        case FSection of
          tkConst, tkResourcestring: ReadConstant;
          tkType:
            if (Peek(CommentTokens) in [tkIdentifier, tkString, tkType]) then
              ReadType;
        end;
      tkColon:
        if (FSection in [tkConst, tkResourcestring, tkThreadvar,  tkVar]) and
          (Peek(CommentTokens) in [tkIdentifier,  tkString]) then
          case FSection of
            tkConst, tkResourcestring:
              ReadConstant;
            tkThreadvar, tkVar:
              ReadVariable;
          end;
      tkClass:
        case Peek(CommentTokens) of
          tkSemiColon:
            Scan([tkSemiColon]);
          tkOf:
            ReadClassReference;
        else
          ReadClass;
        end;
      tkInterface:
        if InterfaceSection then
          case Peek(CommentTokens) of
            tkSemiColon:
              Scan([tkSemiColon]);
          else
            ReadInterface;
          end
        else
          InterfaceSection := True;
      tkAnsiComment, tkCComment, tkPascalComment:
        ReadComment;
      tkUses:
        ReadUses;
    end;
  until Token.Kind in [tkImplementation, tkNull];
end;

procedure TPascalLexer.InvokeEvent(var SourceInfo: TSourceInfo;
  Event: TLexicalEvent);
var
  Body: string;
begin
  case FSection of
    tkConst:
      begin
        SourceInfo.Kind := skConstant;
        Event := FOnConstant;
      end;
    tkResourceString:
      begin
        SourceInfo.Kind := skResourceString;
        Event := FOnVariable;
      end;
    tkThreadVar:
      begin
        SourceInfo.Kind := skThreadVariable;
        Event := FOnVariable;
      end;
    tkVar:
      begin
        SourceInfo.Kind := skVariable;
        Event := FOnVariable;
      end;
  end;
  if Assigned(Event) then
  begin
    Body := CopyText(Identifier.Position, Token.Position + Token.Length -
      Identifier.Position);
    with SourceInfo do
    begin
      Line := Identifier.Row;
      Column := Identifier.Col;
      Name := Identifier.Text;
    end;
    Event(Self, Body, SourceInfo);
  end;
end;

procedure TPascalLexer.StandardEvent(Kind: TSourceKind; Event: TLexicalEvent);
var
  SourceInfo: TSourceInfo;
  I: Integer;
begin
  I := 0;
  repeat
    case Scan([tkEnd, tkRecord, tkLeftParenthesis, tkRightParenthesis,
      tkSemiColon]) of
      tkLeftParenthesis:
        Inc(I);
      tkRightParenthesis:
        Dec(I);
      tkEnd:
        begin
          Scan([tkSemiColon]);
          Dec(I)
        end;
      tkRecord:
        Inc(I);
    end;
  until (I = 0) and (Token.Kind = tkSemiColon);
  SourceInfo.Kind := Kind;
  InvokeEvent(SourceInfo, Event);
end;

procedure TPascalLexer.ReadArray;
var
  SourceInfo: TSourceInfo;
  I: Integer;
begin
  I := 0;
  repeat
    case Scan([tkLeftParenthesis, tkRightParenthesis, tkSemiColon]) of
      tkLeftParenthesis:
        Inc(I);
      tkRightParenthesis:
        Dec(I);
    end;
  until (I = 0) and (Token.Kind = tkSemiColon);
  SourceInfo.Kind := skArray;
  InvokeEvent(SourceInfo, FOnArray);
end;

function TPascalLexer.ReadCallingConvention: TCallingConvention;
begin
  Result := ccDefault;
  if (Peek(CommentTokens, FScratchToken) = tkIdentifier) then
    case StrToDirectiveKind(FScratchToken.Text) of
      dkCdecl: Result := ccCdecl;
      dkPascal: Result := ccPascal;
      dkRegister: Result := ccRegister;
      dkSafecall: Result := ccSafecall;
      dkStdcall: Result := ccStdcall;
    end;
  if Result <> ccDefault then
    Scan([tkSemiColon]);
end;

procedure TPascalLexer.ReadClass;
var
  Name: string;
  InheritedName: string;
  Visibility: TMemberVisibility;
  ClassMethod: Boolean;

  procedure ReadMethod;
  var
    SourceInfo: TSourceInfo;
    StartToken: TPascalToken;
    Start: Integer;
    Body: string;
  begin
    Start := 0;
    if Assigned(FOnMethod) then
    begin
      SourceInfo.ClassMethod := ClassMethod;
      if ClassMethod then
        StartToken := FScratchToken
      else
        StartToken := Token;
      SourceInfo.Line := StartToken.Row;
      SourceInfo.Column := StartToken.Col;
      Start := StartToken.Position;
      SourceInfo.ClassMethodKind := KindToRoutine(Token.Kind);
      case SourceInfo.ClassMethodKind of
        rkConstructor:
          SourceInfo.Kind := skConstructor;
        rkDestructor:
          SourceInfo.Kind := skDestructor;
        rkFunction:
          SourceInfo.Kind := skFunctionMethod;
        rkProcedure:
          SourceInfo.Kind := skProcedureMethod;
      end;
      Scan([tkIdentifier]);
      SourceInfo.Name := Token.Text;
      SourceInfo.ClassName := Name;
      SourceInfo.Visibility := Visibility;
    end;
    if Peek(CommentTokens) = tkLeftParenthesis then
      Scan([tkRightParenthesis]);
    Scan([tkSemiColon]);
    SourceInfo.VirtualKind := vkStatic;
    if Peek(CommentTokens, FScratchToken) = tkIdentifier then
      case StrToDirectiveKind(FScratchToken.Text) of
        dkDynamic:
          SourceInfo.VirtualKind := vkDynamic;
        dkMessage:
          SourceInfo.VirtualKind := vkMessage;
        dkOverride:
          SourceInfo.VirtualKind := vkOverride;
        dkVirtual:
          SourceInfo.VirtualKind := vkVirtual;
      end;
    if SourceInfo.VirtualKind <> vkStatic then
      Scan([tkSemiColon]);
    SourceInfo.ClassMethodConvention := ReadCallingConvention;
    if Assigned(FOnMethod) then
    begin
      Body := CopyText(Start, Token.Position + Token.Length - Start);
      FOnMethod(Self, Body, SourceInfo);
    end;
  end;

  procedure ReadProperty;
  var
    Start: Integer;
    SourceInfo: TSourceInfo;
    ColonFound: Boolean;
    Body: string;
    I: Integer;
  begin
    Start := 0;
    if Assigned(FOnProperty) then
    begin
      Start := Token.Position;
      SourceInfo.Line :=  Token.Row;
      SourceInfo.Column := Token.Col;
      Scan([tkIdentifier]);
      SourceInfo.Name := Token.Text;
      SourceInfo.Kind := skProperty;
      SourceInfo.ClassName := Name;
      SourceInfo.Visibility := Visibility;
      SourceInfo.Access := [];
    end;
    ColonFound := False;
    I := 0;
    repeat
      case Scan([tkLeftBracket, tkRightBracket, tkIdentifier, tkColon,
        tkSemicolon]) of
        tkLeftBracket:
          Inc(I);
        tkRightBracket:
          Dec(I);
        tkIdentifier:
          if ColonFound and Assigned(FOnProperty) then
            case StrToDirectiveKind(Token.Text) of
              dkRead:
                Include(SourceInfo.Access, paRead);
              dkWrite:
                Include(SourceInfo.Access, paWrite);
            end;
        tkColon:
          if I = 0 then
           ColonFound := True;
      end;
    until (I = 0) and (Token.Kind = tkSemicolon);
    if ColonFound then
    begin
      SourceInfo.Default := False;
      if Peek(CommentTokens, FScratchToken) = tkIdentifier then
        SourceInfo.Default := StrToDirectiveKind(FScratchToken.Text) = dkDefault;
      if SourceInfo.Default then
        Scan([tkSemiColon]);
      if Assigned(FOnProperty) then
      begin
        Body := CopyText(Start, Token.Position + Token.Length - Start);
        FOnProperty(Self, Body, SourceInfo);
      end;
    end;
  end;

var
  SourceInfo: TSourceInfo;
  Body: string;
  I: Integer;
const
  ClassTokens = [tkIdentifier, tkProperty, tkClass, tkConstructor, tkDestructor,
    tkFunction, tkProcedure, tkRecord, tkEnd];
begin
  Name := Identifier.Text;
  InheritedName := 'TObject';
  FInterfaceList.Clear;
  if Peek(CommentTokens) = tkLeftParenthesis then
  begin
    Scan([tkIdentifier]);
    InheritedName := Token.Text;
    while Scan([tkIdentifier, tkRightParenthesis]) = tkIdentifier do
      FInterfaceList.Add(Token.Text);
  end;
  ClassMethod := False;
  Visibility := mvPublished;
  I := 1;
  if Peek(CommentTokens) = tkSemiColon then
  begin
    Scan([tkSemiColon]);
    Dec(I);
  end;
  while I > 0 do
  begin
    case Scan(ClassTokens) of
      tkIdentifier:
        case StrToDirectiveKind(Token.Text) of
          dkPrivate:
            Visibility := mvPrivate;
          dkProtected:
            Visibility := mvProtected;
          dkPublic:
            Visibility := mvPublic;
          dkPublished:
            Visibility := mvPublished;
        end;
      tkClass:
        begin
          ClassMethod := True;
          FScratchToken.Copy(Token);
          Continue;
        end;
      tkConstructor, tkDestructor, tkFunction, tkProcedure: ReadMethod;
      tkProperty: ReadProperty;
      tkRecord:
        Inc(I);
      tkEnd:
        begin
          Scan([tkSemiColon]);
          Dec(I);
        end;
    end;
    ClassMethod := False;
  end;
  if Assigned(FOnClass) then
  begin
    Body := CopyText(Identifier.Position, Token.Position + Token.Length -
      Identifier.Position);
    with SourceInfo do
    begin
      Line := Identifier.Row;
      Column := Identifier.Col;
      Name := Identifier.Text;
      Kind := skClass;
      ClassParent := InheritedName;
      InterfaceCount := FInterfaceList.Count;
    end;
    FOnClass(Self, Body, SourceInfo, InheritedName, FInterfaceList);
  end;
end;

procedure TPascalLexer.ReadComment;
begin
  if Assigned(FOnComment) then
    FOnComment(Self, Token.Text);
end;

procedure TPascalLexer.ReadConstant;
var
  SourceInfo: TSourceInfo;
  I: Integer;
begin
  I := 0;
  repeat
    case Scan([tkLeftParenthesis, tkRightParenthesis, tkSemiColon]) of
      tkLeftParenthesis:
        Inc(I);
      tkRightParenthesis:
        Dec(I);
    end;
  until (I = 0) and (Token.Kind = tkSemiColon);
  SourceInfo.Kind := skConstant;
  InvokeEvent(SourceInfo, FOnConstant);
end;

procedure TPascalLexer.ReadInterface;
var
  Name: string;
  InheritedName: string;

  procedure ReadInterfaceMethod;
  var
    SourceInfo: TSourceInfo;
    Start: Integer;
    Body: string;
  begin
    Start := 0;
    if Assigned(FOnMethod) then
    begin
      SourceInfo.Line := Token.Row;
      SourceInfo.Column := Token.Col;
      Start := Token.Position;
      SourceInfo.InterfaceName := Name;
      SourceInfo.InterfaceMethodKind := KindToRoutine(Token.Kind);
      case SourceInfo.InterfaceMethodKind of
        rkFunction:
          SourceInfo.Kind := skFunctionInterfaceMethod;
        rkProcedure:
          SourceInfo.Kind := skProcedureInterfaceMethod;
      end;
      Scan([tkIdentifier]);
      SourceInfo.Name := Token.Text;
    end;
    if Peek(CommentTokens) = tkLeftParenthesis then
      Scan([tkRightParenthesis]);
    Scan([tkSemiColon]);
    SourceInfo.InterfaceMethodConvention := ReadCallingConvention;
    if Assigned(FOnMethod) then
    begin
      Body := CopyText(Start, Token.Position + Token.Length - Start);
      FOnInterfaceMethod(Self, Body, SourceInfo);
    end;
  end;

var
  SourceInfo: TSourceInfo;
  Body: string;
  I: Integer;
const
  ClassTokens = [tkIdentifier, tkProperty, tkClass, tkFunction, tkProcedure,
    tkRecord, tkEnd];
begin
  Name := Identifier.Text;
  InheritedName := 'IUnknown';
  if Peek(CommentTokens) = tkLeftParenthesis then
  begin
    Scan([tkIdentifier]);
    InheritedName := Token.Text;
    Scan([tkRightParenthesis]);
  end;
  I := 1;
  if Peek(CommentTokens) = tkSemiColon then
  begin
    Scan([tkSemiColon]);
    Dec(I);
  end;
  while I > 0 do
    case Scan(ClassTokens) of
      tkFunction, tkProcedure: ReadInterfaceMethod;
      tkEnd:
        begin
          Scan([tkSemiColon]);
          Dec(I);
        end;
    end;
  if Assigned(FOnClass) then
  begin
    Body := CopyText(Identifier.Position, Token.Position + Token.Length -
      Identifier.Position);
    with SourceInfo do
    begin
      Line := Identifier.Row;
      Column := Identifier.Col;
      Name := Identifier.Text;
      Kind := skInterface;
      InterfaceParent := InheritedName;
    end;
    FOnInterface(Self, Body, SourceInfo, InheritedName);
  end;
end;

procedure TPascalLexer.ReadClassReference;
var
  SourceInfo: TSourceInfo;
begin
  Scan([tkSemiColon]);
  SourceInfo.Kind := skClassReference;
  InvokeEvent(SourceInfo, FOnClassReference);
end;

procedure TPascalLexer.ReadEnumeration;
var
  SourceInfo: TSourceInfo;
begin
  Scan([tkSemiColon]);
  SourceInfo.Kind := skEnumeration;
  InvokeEvent(SourceInfo, FOnEnumeration);
end;

procedure TPascalLexer.ReadRecord;
var
  SourceInfo: TSourceInfo;
  I: Integer;
begin
  I := 1;
  while I > 0 do
    case Scan([tkEnd, tkRecord]) of
      tkEnd:
        begin
          Scan([tkSemiColon]);
          Dec(I)
        end;
      tkRecord:
        Inc(I);
    end;
  SourceInfo.Kind := skRecord;
  InvokeEvent(SourceInfo, FOnRecord);
end;

procedure TPascalLexer.ReadRoutine;
var
  SourceInfo: TSourceInfo;
  Event: TLexicalEvent;
  Start: Integer;
begin
  SourceInfo.RoutineKind := KindToRoutine(Section);
  Event := nil;
  case SourceInfo.RoutineKind of
    rkFunction:
      begin
        SourceInfo.Kind := skFunction;
        Event := FOnFunction;
      end;
    rkProcedure:
      begin
        SourceInfo.Kind := skProcedure;
        Event := FOnProcedure;
      end;
  end;
  if Assigned(Event) then
  begin
    Start := Token.Position;
    SourceInfo.Line := Token.Row;
    SourceInfo.Column := Token.Col;
  end
  else
    Start := 0;
  Scan([tkIdentifier]);
  if Assigned(Event) then
    SourceInfo.Name := Token.Text;
  if Peek(CommentTokens) = tkLeftParenthesis then
    Scan([tkRightParenthesis]);
  Scan([tkSemiColon]);
  SourceInfo.RoutineConvention := ReadCallingConvention;
  if Assigned(Event) then
    Event(Self, CopyText(Start, Token.Position + Token.Length - Start),
      SourceInfo);
end;

procedure TPascalLexer.ReadRoutinePointer(Kind: TRoutineKind);
var
  SourceInfo: TSourceInfo;
  MethodPointer: Boolean;
begin
  if Peek(CommentTokens) = tkLeftParenthesis then
    Scan([tkRightParenthesis]);
  MethodPointer := Scan([tkOf, tkSemiColon]) = tkOf;
  SourceInfo.RoutineKind := Kind;
  if MethodPointer then
  begin
    Scan([tkSemiColon]);
    case Kind of
      rkFunction: SourceInfo.Kind := skFunctionEvent;
      rkProcedure: SourceInfo.Kind := skProcedureEvent;
    end;
    SourceInfo.RoutineConvention := ReadCallingConvention;
    InvokeEvent(SourceInfo, FOnEvent);
  end
  else
  begin
    case Kind of
      rkFunction: SourceInfo.Kind := skFunctionPointer;
      rkProcedure: SourceInfo.Kind := skProcedurePointer;
    end;
    SourceInfo.RoutineConvention := ReadCallingConvention;
    InvokeEvent(SourceInfo, FOnRoutinePointer);
  end;
end;

procedure TPascalLexer.ReadSet;
var
  SourceInfo: TSourceInfo;
begin
  Scan([tkImplementation, tkNull, tkSemiColon]);
  SourceInfo.Kind := skSet;
  InvokeEvent(SourceInfo, FOnSet);
end;

procedure TPascalLexer.ReadType;
begin
  StandardEvent(skType, FOnType);
end;

procedure TPascalLexer.ReadUses;
var
  UsesList: TStrings;
begin
  if Assigned(FOnUses) then
  begin
    UsesList := TStringList.Create;
    try
      while Scan([tkIdentifier, tkSemiColon]) = tkIdentifier do
        UsesList.Add(Token.Text);
      FOnUses(Self, UsesList);
    finally
      UsesList.Free;
    end;
  end;
end;

procedure TPascalLexer.ReadVariable;
begin
  StandardEvent(skVariable, FOnVariable);
end;

procedure TPascalLexer.SetFileName(Value: string);
begin
  FMap.Free;
  if Value = '' then
    FMap := nil
  else
  begin
    FMap := TMemoryMappedFile.Create(Value);
    Initialize(PChar(FMap.ViewStart), Integer(FMap.ViewEnd - FMap.ViewStart));
    Scan([tkUnit]);
    Scan([tkIdentifier]);
    FUnitPath := Value;
    FUnitName := Token.Text;
  end;
end;

{ root
   + units
   |  + name1
   |  |  + path
   |  |  + uses
   |  |     + unit1
   |  |     + unit2 ...
   |  |  + classes
   |  |     + class1
   |  |     + class2 ...
   |  + name2 ...
   + unknown ...
   WinTools
   }

end.
