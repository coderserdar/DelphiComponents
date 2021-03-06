(*
 *   InstantObjects
 *   Query Language
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantCommand;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, InstantClasses, InstantTextFiler;

type
  TInstantIQLObject= class;

  TInstantIQLReader = class(TInstantTextReader)
  private
    procedure DoReadObject(AObject: TInstantIQLObject);
  protected
    procedure AfterReadObject(AObject: TInstantIQLObject); virtual;
    procedure BeforeReadObject(AObject: TInstantIQLObject); virtual;
    procedure InternalReadObject(AObject: TInstantIQLObject); virtual;
  public
    procedure ReadObject(AObject: TInstantIQLObject);
  end;

  TInstantIQLWriter = class(TInstantTextWriter)
  protected
    procedure AfterWriteKeyword(const Keyword: string); virtual;
    procedure AfterWriteObject(AObject: TInstantIQLObject); virtual;
    procedure BeforeWriteKeyword(const Keyword: string); virtual;
    procedure BeforeWriteObject(AObject: TInstantIQLObject); virtual;
    procedure InternalWriteKeyword(const Keyword: string); virtual;
    procedure InternalWriteObject(AObject: TInstantIQLObject); virtual;
  public
    procedure WriteObject(AObject: TInstantIQLObject);
    procedure WriteKeyword(const Keyword: string);
    procedure WriteSpace(Count: Integer = 1);
  end;

  TInstantIQLObjectClass = class of TInstantIQLObject;

  TInstantIQLObject = class(TPersistent)
  private
    FObjectList: TObjectList;
    FOwner: TInstantIQLObject;
    function GetObjectCount: Integer;
    function GetObjectList: TObjectList;
    function GetObjects(Index: Integer): TInstantIQLObject;
    function GetRoot: TInstantIQLObject;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure CheckIsIdentifierToken(const Token: string);
    procedure CheckToken(const ActualToken, ExpectedToken: string;
      IgnoreCase: Boolean = True);
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; virtual;
    procedure InternalClear; virtual;
    procedure InvalidTokenError(const Token: string);
    function ReadChild(Child: Pointer; Reader: TInstantIQLReader;
      Classes: array of TInstantIQLObjectClass): Boolean;
    procedure ReadObject(Reader: TInstantIQLReader); virtual;
    procedure UnexpectedTokenError(const ActualToken, ExpectedToken: string);
    function WriteChild(Child: TInstantIQLObject;
      Writer: TInstantIQLWriter; Space: Boolean = False): Boolean;
    procedure WriteObject(Writer: TInstantIQLWriter); virtual;
    property ObjectList: TObjectList read GetObjectList;
  public
    constructor Create(AOwner: TInstantIQLObject); virtual;
    destructor Destroy; override;
    class function AtInstance(Reader: TInstantIQLReader): Boolean;
    procedure Clear;
    procedure Read(Reader: TInstantIQLReader);
    procedure Write(Writer: TInstantIQLWriter);
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TInstantIQLObject read GetObjects; default;
    property Owner: TInstantIQLObject read FOwner;
    property Root: TInstantIQLObject read GetRoot;
  published
    property Text: string read GetText write SetText;
  end;

  TInstantIQLOperatorType = (otEQ, otGT, otLT, otNE, otEN, otEG, otGE, otLE,
    otEL, otLike, otIs, otIn, otAdd, otSub, otOr, otXor, otMul, otDiv, otFDiv,
    otMod, otAnd);
  TInstantIQLOperatorTypes = set of TInstantIQLOperatorType;

  TInstantIQLOperator = class(TInstantIQLObject)
  private
    FOperatorType: TInstantIQLOperatorType;
  protected
    class function GetOperatorType(const Token: string;
      var OperatorType: TInstantIQLOperatorType): Boolean;
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  public
    class function OperatorTypes: TInstantIQLOperatorTypes; virtual; abstract;
    property OperatorType: TInstantIQLOperatorType read FOperatorType;
  end;

  TInstantIQLRelOp = class(TInstantIQLOperator)
  public
    class function OperatorTypes: TInstantIQLOperatorTypes; override;
  end;

  TInstantIQLAddOp = class(TInstantIQLOperator)
  public
    class function OperatorTypes: TInstantIQLOperatorTypes; override;
  end;

  TInstantIQLMulOp = class(TInstantIQLOperator)
  public
    class function OperatorTypes: TInstantIQLOperatorTypes; override;
  end;

  TInstantIQLSignOp = class(TInstantIQLOperator)
  public
    class function OperatorTypes: TInstantIQLOperatorTypes; override;
  end;

  TInstantIQLExpression = class;

  TInstantIQLParameters = class(TInstantIQLObject)
  private
    FExpression: TInstantIQLExpression;
    FNextParameters: TInstantIQLParameters;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Expression: TInstantIQLExpression read FExpression;
    property NextParameters: TInstantIQLParameters read FNextParameters;
  end;

  TInstantIQLFactor = class(TInstantIQLObject)
  end;

  TInstantIQLClause = class;

  TInstantIQLClauseFactor = class(TInstantIQLFactor)
  private
    FClause: TInstantIQLClause;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Clause: TInstantIQLClause read FClause;
  end;

  TInstantIQLNotFactor = class(TInstantIQLFactor)
  private
    FFactor: TInstantIQLFactor;
    class function IsNotToken(const Token: string): Boolean;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Factor: TInstantIQLFactor read FFactor;
  end;

  TInstantIQLOperand = class(TInstantIQLFactor)
  end;

  TInstantIQLPath = class(TInstantIQLOperand)
  private
    FAttributeList: TStringList;
    function GetAttributeCount: Integer;
    function GetAttributeList: TStringList;
    function GetAttributes(Index: Integer): string;
    function GetHasAttributes: Boolean;
    function GetSubPath(Index: Integer): string;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
    property AttributeList: TStringList read GetAttributeList;
  public
    destructor Destroy; override;
    property AttributeCount: Integer read GetAttributeCount;
    property Attributes[Index: Integer]: string read GetAttributes; default;
    property SubPath[Index: Integer]: string read GetSubPath;
    property HasAttributes: Boolean read GetHasAttributes;
  end;

  TInstantIQLConstant = class(TInstantIQLOperand)
  private
    FValue: string;
    class function IsConstantToken(const Token: string): Boolean;
    function GetIsSelf: Boolean;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  public
    property IsSelf: Boolean read GetIsSelf;
  published
    property Value: string read FValue;
  end;

  TInstantIQLFunction = class(TInstantIQLOperand)
  private
    FParameters: TInstantIQLParameters;
    FFunctionName: string;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Parameters: TInstantIQLParameters read FParameters;
    property FunctionName: string read FFunctionName;
  end;

  TInstantIQLParam = class(TInstantIQLOperand)
  private
    FParamName: string;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property ParamName: string read FParamName;
  end;

  TInstantIQLCondition = class(TInstantIQLObject)
  end;

  TInstantIQLTerm = class(TInstantIQLCondition)
  private
    FFactor: TInstantIQLFactor;
    FMulOp: TInstantIQLMulOp;
    FNextTerm: TInstantIQLTerm;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Factor: TInstantIQLFactor read FFactor;
    property MulOp: TInstantIQLMulOp read FMulOp;
    property NextTerm: TInstantIQLTerm read FNextTerm;
  end;

  TInstantIQLExpression = class(TInstantIQLCondition)
  private
    FAddOp: TInstantIQLAddOp;
    FNextExpression: TInstantIQLExpression;
    FSignOp: TInstantIQLSignOp;
    FTerm: TInstantIQLTerm;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property AddOp: TInstantIQLAddOp read FAddOp;
    property NextExpression: TInstantIQLExpression read FNextExpression;
    property SignOp: TInstantIQLSignOp read FSignOp;
    property Term: TInstantIQLTerm read FTerm;
  end;

  TInstantIQLClause = class(TInstantIQLCondition)
  private
    FExpression: TInstantIQLExpression;
    FNextClause: TInstantIQLClause;
    FRelOp: TInstantIQLRelOp;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Expression: TInstantIQLExpression read FExpression;
    property NextClause: TInstantIQLClause read FNextClause;
    property RelOp: TInstantIQLRelOp read FRelOp;
  end;

  TInstantIQLOrderDirection = (odAsc, odDesc);

  TInstantIQLOrderSpec = class(TInstantIQLObject)
  private
    FExpression: TInstantIQLExpression;
    FOrderDirection: TInstantIQLOrderDirection;
    function GetOrderDirectionText: string;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
    property OrderDirectionText: string read GetOrderDirectionText;
  published
    property Expression: TInstantIQLExpression read FExpression;
    property OrderDirection: TInstantIQLOrderDirection read FOrderDirection;
  end;

  TInstantIQLOrder = class(TInstantIQLObject)
  private
    FNextOrder: TInstantIQLOrder;
    FOrderSpec: TInstantIQLOrderSpec;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property OrderSpec: TInstantIQLOrderSpec read FOrderSpec;
    property NextOrder: TInstantIQLOrder read FNextOrder;
  end;

  TInstantIQLSpecifier = class(TInstantIQLObject)
  private
    FOperand: TInstantIQLOperand;
    function GetIsPath: Boolean;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Operand: TInstantIQLOperand read FOperand;
    property IsPath: Boolean read GetIsPath;
  end;

  TInstantIQLClassRef = class(TInstantIQLObject)
  private
    FAny: Boolean;
    FObjectClassName: string;
  protected
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Any: Boolean read FAny;
    property ObjectClassName: string read FObjectClassName;
  end;

  TInstantIQLCommand = class(TInstantIQLObject)
  private
    FClassRef: TInstantIQLClassRef;
    FClause: TInstantIQLClause;
    FDistinct: Boolean;
    FOrder: TInstantIQLOrder;
    FSpecifier: TInstantIQLSpecifier;
    function GetAny: Boolean;
    function GetObjectClassName: string;
  protected
    function GetResultClassName: string; virtual;
    class function InternalAtInstance(Reader: TInstantIQLReader): Boolean; override;
    procedure InternalClear; override;
    procedure ReadObject(Reader: TInstantIQLReader); override;
    procedure WriteObject(Writer: TInstantIQLWriter); override;
  published
    property Any: Boolean read GetAny;
    property Clause: TInstantIQLClause read FClause;
    property ClassRef: TInstantIQLClassRef read FClassRef;
    property Distinct: Boolean read FDistinct;
    property ObjectClassName: string read GetObjectClassName;
    property Order: TInstantIQLOrder read FOrder;
    property ResultClassName: string read GetResultClassName;
    property Specifier: TInstantIQLSpecifier read FSpecifier;
  end;

  TInstantIQLCommandTranslator = class(TPersistent)
  private
    FCommand: TInstantIQLCommand;
    FCommandText: string;
    procedure SetCommandText(const Value: string);
    function GetCommand: TInstantIQLCommand;
  protected
    procedure AfterTranslate; virtual;
    procedure BeforeTranslate; virtual;
    procedure Clear; virtual;
    function GetResultClassName: string; virtual;
    function CreateCommand: TInstantIQLCommand; virtual;
    procedure Translate; virtual;
    property Command: TInstantIQLCommand read GetCommand;
  public
    property CommandText: string read FCommandText write SetCommandText;
    property ResultClassName: string read GetResultClassName;
  end;

  TInstantIQLTranslator = class;

  TInstantIQLStatementWriter = class(TInstantIQLWriter)
  private
    FTranslator: TInstantIQLTranslator;
  protected
    procedure InternalWriteKeyword(const Keyword: string); override;
    procedure InternalWriteObject(AObject: TInstantIQLObject); override;
  public
    constructor Create(ATranslator: TInstantIQLTranslator; AStream: TStream;
      FreeStream: Boolean = False);
    property Translator: TInstantIQLTranslator read FTranslator;
  end;

  TInstantIQLTranslator = class(TInstantIQLCommandTranslator)
  private
    FStatementText: string;
  protected
    procedure Translate; override;
    function TranslateKeyword(const Keyword: string; Writer: TInstantIQLWriter): Boolean; virtual;
    function TranslateObject(AObject: TInstantIQLObject;
      Writer: TInstantIQLWriter): Boolean; virtual;
    procedure WriteObject(AObject: TInstantIQLObject;
      Writer: TInstantIQLWriter);
  public
    property StatementText: string read FStatementText;
  end;

  EInstantIQLError = class(EInstantError)
  end;

implementation

uses
  InstantUtils, InstantConsts;

const
  OperatorTokens: array[TInstantIQLOperatorType] of string = ('=', '>', '<',
    '<>', '><', '=>', '>=', '<=', '=<', 'LIKE', 'IS', 'IN', '+', '-', 'OR',
    'XOR', '*', 'DIV', '/', 'MOD', 'AND');
  OrderTokens: array[TInstantIQLOrderDirection] of string = ('ASC', 'DESC');

{ TInstantIQLReader }

procedure TInstantIQLReader.AfterReadObject(AObject: TInstantIQLObject);
begin
end;

procedure TInstantIQLReader.BeforeReadObject(AObject: TInstantIQLObject);
begin
end;

procedure TInstantIQLReader.DoReadObject(AObject: TInstantIQLObject);
begin
  BeforeReadObject(AObject);
  InternalReadObject(AObject);
  AfterReadObject(AObject);
end;

procedure TInstantIQLReader.InternalReadObject(AObject: TInstantIQLObject);
begin
  AObject.ReadObject(Self);
end;

procedure TInstantIQLReader.ReadObject(AObject: TInstantIQLObject);
begin
  if Assigned(AObject) then
    DoReadObject(AObject);
end;

{ TInstantIQLWriter }

procedure TInstantIQLWriter.AfterWriteKeyword(const Keyword: string);
begin
end;

procedure TInstantIQLWriter.AfterWriteObject(AObject: TInstantIQLObject);
begin
end;

procedure TInstantIQLWriter.BeforeWriteKeyword(const Keyword: string);
begin
end;

procedure TInstantIQLWriter.BeforeWriteObject(AObject: TInstantIQLObject);
begin
end;

procedure TInstantIQLWriter.InternalWriteKeyword(const Keyword: string);
begin
  WriteString(Keyword);
  WriteSpace;
end;

procedure TInstantIQLWriter.InternalWriteObject(AObject: TInstantIQLObject);
begin
  AObject.WriteObject(Self);
end;

procedure TInstantIQLWriter.WriteKeyword(const Keyword: string);
begin
  BeforeWriteKeyword(Keyword);
  InternalWriteKeyword(Keyword);
  AfterWriteKeyword(Keyword);
end;

procedure TInstantIQLWriter.WriteObject(AObject: TInstantIQLObject);
begin
  if Assigned(AObject) then
  begin
    BeforeWriteObject(AObject);
    InternalWriteObject(AObject);
    AfterWriteObject(AObject);
  end;
end;

procedure TInstantIQLWriter.WriteSpace(Count: Integer);
begin
  if Count > 0 then
    WriteString(StringOfChar(' ', Count));
end;

{ TInstantIQLObject }

class function TInstantIQLObject.AtInstance(
  Reader: TInstantIQLReader): Boolean;
var
 SavePos: TInstantTextPos;
begin
  if Assigned(Reader) then
  begin
    SavePos := Reader.Position;
    try
      if Reader.SkipSpace then
        Result := False
      else
        Result := InternalAtInstance(Reader)
    finally
      Reader.Position := SavePos;
    end;
  end else
    Result := False;
end;

procedure TInstantIQLObject.CheckIsIdentifierToken(const Token: string);
begin
  if not InstantIsIdentifier(Token) then
    InvalidTokenError(Token);
end;

procedure TInstantIQLObject.CheckToken(const ActualToken,
  ExpectedToken: string; IgnoreCase: Boolean);
begin
  if not InstantSameText(ActualToken, ExpectedToken, IgnoreCase) then
    UnexpectedTokenError(ActualToken, ExpectedToken);
end;

procedure TInstantIQLObject.Clear;
begin
  ObjectList.Clear;
  InternalClear;
end;

constructor TInstantIQLObject.Create(
  AOwner: TInstantIQLObject);
begin
  inherited Create;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.ObjectList.Add(Self);
end;

destructor TInstantIQLObject.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.ObjectList.Remove(Self);
  FObjectList.Free;
  inherited;
end;

function TInstantIQLObject.GetObjectCount: Integer;
begin
  Result := ObjectList.Count;
end;

function TInstantIQLObject.GetObjectList: TObjectList;
begin
  if not Assigned(FObjectList) then
    FObjectList := TObjectList.Create;
  Result := FObjectList;
end;

function TInstantIQLObject.GetObjects(Index: Integer): TInstantIQLObject;
begin
  Result := ObjectList[Index] as TInstantIQLObject
end;

function TInstantIQLObject.GetRoot: TInstantIQLObject;
begin
  if Assigned(Owner) then
    Result := Owner.Root
  else
    Result := Self;
end;

function TInstantIQLObject.GetText: string;
var
  Stream: TStringStream;
  Writer: TInstantIQLWriter;
begin
  Stream := TStringStream.Create('');
  try
    Writer := TInstantIQLWriter.Create(Stream);
    try
      Write(Writer);
      Result := Stream.DataString;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

class function TInstantIQLObject.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := False;
end;

procedure TInstantIQLObject.InternalClear;
begin
end;

procedure TInstantIQLObject.InvalidTokenError(const Token: string);
begin
  raise EInstantError.CreateFmt(SInvalidToken, [Token]);
end;

procedure TInstantIQLObject.Read(Reader: TInstantIQLReader);
begin
  Clear;
  if Assigned(Reader) and not Reader.SkipSpace then
    Reader.ReadObject(Self);
end;

function TInstantIQLObject.ReadChild(Child: Pointer; Reader: TInstantIQLReader;
  Classes: array of TInstantIQLObjectClass): Boolean;
var
  I: Integer;
  ObjectClass: TInstantIQLObjectClass;
begin
  for I := Low(Classes) to High(Classes) do
  begin
    ObjectClass := Classes[I];
    Result := ObjectClass.AtInstance(Reader);
    if Result then
    begin
      TInstantIQLObject(Child^) := ObjectClass.Create(Self);
      TInstantIQLObject(Child^).Read(Reader);
      Exit;
    end;
  end;
  Result := False;
end;

procedure TInstantIQLObject.ReadObject(Reader: TInstantIQLReader);
begin
end;

procedure TInstantIQLObject.SetText(const Value: string);
var
  Reader: TInstantIQLReader;
begin
  Reader := TInstantIQLReader.Create(Value);
  try
    Read(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TInstantIQLObject.UnexpectedTokenError(const ActualToken,
  ExpectedToken: string);
begin
  raise EInstantError.CreateFmt(SUnexpectedToken,
    [ActualToken, ExpectedToken]);
end;

procedure TInstantIQLObject.Write(Writer: TInstantIQLWriter);
begin
  if Assigned(Writer) then
    Writer.WriteObject(Self);
end;

function TInstantIQLObject.WriteChild(Child: TInstantIQLObject;
  Writer: TInstantIQLWriter; Space: Boolean): Boolean;
begin
  Result := Assigned(Child);
  if Result then
  begin
    if Space then
      Writer.WriteSpace;
    Child.Write(Writer);
  end;
end;

procedure TInstantIQLObject.WriteObject(Writer: TInstantIQLWriter);
begin
end;

{ TInstantIQLOperator }

class function TInstantIQLOperator.GetOperatorType(const Token: string;
  var OperatorType: TInstantIQLOperatorType): Boolean;
var
  OpType: TInstantIQLOperatorType;
begin
  for OpType := Low(OperatorTokens) to High(OperatorTokens) do
    if (OpType in OperatorTypes) and
      SameText(Token, OperatorTokens[OpType])  then
    begin
      Result := True;
      OperatorType := OpType;
      Exit;
    end;
  Result := False;
end;

class function TInstantIQLOperator.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
var
  OpType: TInstantIQLOperatorType;
begin
  Result := GetOperatorType(Reader.ReadToken, OpType);
end;

procedure TInstantIQLOperator.InternalClear;
begin
  inherited;
  FOperatorType := Low(FOperatorType);
end;

procedure TInstantIQLOperator.ReadObject(Reader: TInstantIQLReader);
var
  Token, OpStr: string;
  OpType: TInstantIQLOperatorType;
begin
  inherited;
  Token := Reader.ReadToken;
  OpStr := '';
  while GetOperatorType(Token, OpType) do
  begin
    OpStr := OpStr + Token;
    Token := Reader.ReadToken;
  end;
  if Token <> '' then
    Reader.UnreadToken;
  if not GetOperatorType(OpStr, FOperatorType) then
    InvalidTokenError(OpStr);
end;

procedure TInstantIQLOperator.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  Writer.WriteString(OperatorTokens[FOperatorType]);
end;

{ TInstantIQLRelOp }

class function TInstantIQLRelOp.OperatorTypes: TInstantIQLOperatorTypes;
begin
  Result := [otEQ, otGT, otLT, otNE, otEN, otEG, otGE, otLE, otEL, otLike,
    otIs, otIn];
end;

{ TInstantIQLAddOp }

class function TInstantIQLAddOp.OperatorTypes: TInstantIQLOperatorTypes;
begin
  Result := [otAdd, otSub, otOr, otXor];
end;

{ TInstantIQLMulOp }

class function TInstantIQLMulOp.OperatorTypes: TInstantIQLOperatorTypes;
begin
  Result := [otMul, otDiv, otFDiv, otMod, otAnd];
end;

{ TInstantIQLSignOp }

class function TInstantIQLSignOp.OperatorTypes: TInstantIQLOperatorTypes;
begin
  Result := [otAdd, otSub];
end;

{ TInstantIQLParameters }

class function TInstantIQLParameters.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLExpression.AtInstance(Reader);
end;

procedure TInstantIQLParameters.InternalClear;
begin
  inherited;
  FExpression := nil;
  FNextParameters := nil;
end;

procedure TInstantIQLParameters.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  if ReadChild(@FExpression, Reader, [TInstantIQLExpression]) then
  begin
    Token := Reader.ReadToken;
    if Token = ',' then
      ReadChild(@FNextParameters, Reader, [TInstantIQLParameters])
    else if Token <> '' then
      Reader.UnreadToken;
  end;
end;

procedure TInstantIQLParameters.WriteObject(Writer: TInstantIQLWriter);
begin
  if Assigned(FExpression) then
  begin
    FExpression.Write(Writer);
    if Assigned(FNextParameters) then
    begin
      Writer.WriteString(', ');
      FNextParameters.Write(Writer);
    end;
  end;
end;

{ TInstantIQLClauseFactor }

class function TInstantIQLClauseFactor.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := Reader.ReadToken = '(';
end;

procedure TInstantIQLClauseFactor.InternalClear;
begin
  inherited;
  FClause := nil;
end;

procedure TInstantIQLClauseFactor.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  Token := Reader.ReadToken;
  if Token <> '(' then
    UnexpectedTokenError(Token, '(');
  ReadChild(@FClause, Reader, [TInstantIQLClause]);
  Token := Reader.ReadToken;
  if Token <> ')' then
    UnexpectedTokenError(Token, ')');
end;

procedure TInstantIQLClauseFactor.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  Writer.WriteChar('(');
  WriteChild(FClause, Writer);
  Writer.WriteChar(')');
end;

{ TInstantIQLNotFactor }

class function TInstantIQLNotFactor.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := IsNotToken(Reader.ReadToken);
end;

procedure TInstantIQLNotFactor.InternalClear;
begin
  inherited;
  FFactor := nil;
end;

class function TInstantIQLNotFactor.IsNotToken(const Token: string): Boolean;
begin
  Result := SameText(Token, 'NOT');
end;

procedure TInstantIQLNotFactor.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  Token := Reader.ReadToken;
  if not IsNotToken(Token) then
    InvalidTokenError(Token);
  ReadChild(@FFactor, Reader, [TInstantIQLNotFactor, TInstantIQLConstant,
    TInstantIQLPath, TInstantIQLClauseFactor]);
end;

procedure TInstantIQLNotFactor.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  Writer.WriteKeyword('NOT');
  WriteChild(FFactor, Writer);
end;

{ TInstantIQLPath }

destructor TInstantIQLPath.Destroy;
begin
  FAttributeList.Free;
  inherited;
end;

function TInstantIQLPath.GetAttributeCount: Integer;
begin
  Result := AttributeList.Count;
end;

function TInstantIQLPath.GetAttributeList: TStringList;
begin
  if not Assigned(FAttributeList) then
    FAttributeList := TStringList.Create;
  Result := FAttributeList;
end;

function TInstantIQLPath.GetAttributes(Index: Integer): string;
begin
  Result := AttributeList[Index];
end;

function TInstantIQLPath.GetHasAttributes: Boolean;
begin
  Result := AttributeCount > 0;
end;

function TInstantIQLPath.GetSubPath(Index: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Index do
  begin
    if I > 0 then
      Result := Result + InstantDot;
    Result := Result + Attributes[I];
  end
end;

class function TInstantIQLPath.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := InstantIsIdentifier(Reader.ReadToken);
end;

procedure TInstantIQLPath.InternalClear;
begin
  inherited;
  FreeAndNil(FAttributeList);
end;

procedure TInstantIQLPath.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  repeat
    Token := Reader.ReadToken;
    CheckIsIdentifierToken(Token);
    AttributeList.Add(Token);
    if Reader.SkipSpace then
      Exit;
    Token := Reader.ReadToken;
  until Token <> InstantDot;
  if Token <> '' then
    Reader.UnreadToken;
end;

procedure TInstantIQLPath.WriteObject(Writer: TInstantIQLWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(AttributeCount) do
  begin
    if I > 0 then
      Writer.WriteChar(InstantDot);
    Writer.WriteString(Attributes[I]);
  end;
end;

{ TInstantIQLConstant }

function TInstantIQLConstant.GetIsSelf: Boolean;
begin
  Result := SameText(Value, 'SELF') or (Value = '*');
end;

class function TInstantIQLConstant.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
var
  Token: string;
begin
  Token := Reader.ReadToken;
  Result := IsConstantToken(Token) or InstantIsNumeric(Token) or
    ((Length(Token) > 0) and (Token[1] in ['"', '''', '[']));
end;

procedure TInstantIQLConstant.InternalClear;
begin
  inherited;
  FValue := '';
end;

class function TInstantIQLConstant.IsConstantToken(
  const Token: string): Boolean;
const
  ConstantTokens: array[0..5] of string = ('SELF', '*', 'NIL',
  'NULL', 'TRUE', 'FALSE');
var
  I: Integer;
begin
  for I := Low(ConstantTokens) to High(ConstantTokens) do
  begin
    Result := SameText(Token, ConstantTokens[I]);
    if Result then
      Exit;
  end;
  Result := False;
end;

procedure TInstantIQLConstant.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  Token := Reader.ReadToken;
  if Token = '[' then
  begin
    FValue := Token;
    repeat
      Token := Reader.ReadToken;
      FValue := FValue + Token;
    until Token = ']'
  end else
    FValue := Token;
end;

procedure TInstantIQLConstant.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  Writer.WriteString(Value);
end;

{ TInstantIQLFunction }

class function TInstantIQLFunction.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := InstantIsIdentifier(Reader.ReadToken) and (Reader.ReadToken = '(');
end;

procedure TInstantIQLFunction.InternalClear;
begin
  inherited;
  FParameters := nil;
  FFunctionname := '';
end;

procedure TInstantIQLFunction.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  Token := Reader.ReadToken;
  CheckIsIdentifierToken(Token);
  FFunctionName := Token;
  CheckToken(Reader.ReadToken, '(');
  ReadChild(@FParameters, Reader, [TInstantIQLParameters]);
  CheckToken(Reader.ReadToken, ')');
end;

procedure TInstantIQLFunction.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  with Writer do
  begin
    WriteString(FFunctionName);
    Writer.WriteChar('(');
    if Assigned(FParameters) then
      FParameters.Write(Writer);
    Writer.WriteChar(')');
  end;
end;

{ TInstantIQLParam }

class function TInstantIQLParam.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := Reader.ReadChar = ':';
end;

procedure TInstantIQLParam.InternalClear;
begin
  inherited;
  FParamName := '';
end;

procedure TInstantIQLParam.ReadObject(Reader: TInstantIQLReader);
begin
  inherited;
  Reader.ReadChar;
  FParamName := Reader.ReadToken;
end;

procedure TInstantIQLParam.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  Writer.WriteChar(':');
  Writer.WriteString(FParamName);
end;

{ TInstantIQLTerm }

class function TInstantIQLTerm.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLFunction.AtInstance(Reader) or
    TInstantIQLConstant.AtInstance(Reader) or
    TInstantIQLPath.AtInstance(Reader) or
    TInstantIQLParam.AtInstance(Reader) or
    TInstantIQLClauseFactor.AtInstance(Reader) or
    TInstantIQLNotFactor.AtInstance(Reader);
end;

procedure TInstantIQLTerm.InternalClear;
begin
  inherited;
  FFactor := nil;
  FMulOp := nil;
  FNextTerm := nil;
end;

procedure TInstantIQLTerm.ReadObject(Reader: TInstantIQLReader);
begin
  inherited;
  if ReadChild(@FFactor, Reader, [TInstantIQLNotFactor, TInstantIQLFunction,
    TInstantIQLConstant, TInstantIQLPath, TInstantIQLParam,
    TInstantIQLClauseFactor]) and
    ReadChild(@FMulOp, Reader, [TInstantIQLMulOp]) then
    ReadChild(@FNextTerm, Reader, [TInstantIQLTerm]);
end;

procedure TInstantIQLTerm.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  if WriteChild(FFactor, Writer) and WriteChild(FMulOp, Writer, True) then
    WriteChild(FNextTerm, Writer, True);
end;

{ TInstantIQLExpression }

class function TInstantIQLExpression.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLSignOp.AtInstance(Reader) or
    TInstantIQLTerm.AtInstance(Reader) or (Reader.ReadToken = '(');
end;

procedure TInstantIQLExpression.InternalClear;
begin
  inherited;
  FAddOp := nil;
  FNextExpression := nil;
  FSignOp := nil;
  FTerm := nil;
end;

procedure TInstantIQLExpression.ReadObject(Reader: TInstantIQLReader);
begin
  inherited;
  ReadChild(@FSignOp, Reader, [TInstantIQLSignOp]);
  if ReadChild(@FTerm, Reader, [TInstantIQLTerm]) and
    ReadChild(@FAddOp, Reader, [TInstantIQLAddOp]) then
    ReadChild(@FNextExpression, Reader, [TInstantIQLExpression]);
end;

procedure TInstantIQLExpression.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  WriteChild(FSignOp, Writer);
  if WriteChild(FTerm, Writer) and WriteChild(FAddOp, Writer, True) then
    WriteChild(FNextExpression, Writer, True);
end;

{ TInstantIQLClause }

class function TInstantIQLClause.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLExpression.AtInstance(Reader);
end;

procedure TInstantIQLClause.InternalClear;
begin
  inherited;
  FExpression := nil;
  FNextClause := nil;
  FRelOp := nil;
end;

procedure TInstantIQLClause.ReadObject(Reader: TInstantIQLReader);
begin
  inherited;
  if ReadChild(@FExpression, Reader, [TInstantIQLExpression]) and
    ReadChild(@FRelOp, Reader, [TInstantIQLRelOp]) then
    ReadChild(@FNextClause, Reader, [TInstantIQLClause]);
end;

procedure TInstantIQLClause.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  if WriteChild(FExpression, Writer) and WriteChild(FRelOp, Writer, True) then
    WriteChild(FNextClause, Writer, True);
end;

{ TInstantIQLOrderSpec }

function TInstantIQLOrderSpec.GetOrderDirectionText: string;
begin
  Result := OrderTokens[OrderDirection]
end;

class function TInstantIQLOrderSpec.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLExpression.AtInstance(Reader);
end;

procedure TInstantIQLOrderSpec.InternalClear;
begin
  inherited;
  FExpression := nil;
  FOrderDirection := Low(FOrderDirection);
end;

procedure TInstantIQLOrderSpec.ReadObject(Reader: TInstantIQLReader);

  function TokenToOrderDirection(const Token: string;
    var IsorderToken: Boolean): TInstantIQLOrderDirection;
  begin
    for Result := Low(TInstantIQLOrderDirection) to High(TInstantIQLOrderDirection) do
      if SameText(OrderTokens[Result], Token) then
      begin
        IsOrderToken := True;
        Exit;
      end;
    IsOrderToken := False;
    Result := Low(TInstantIQLOrderDirection);
  end;

var
  Token: string;
  IsOrderToken: Boolean;
begin
  inherited;
  ReadChild(@FExpression, Reader, [TInstantIQLExpression]);
  with Reader do
  begin
    Token := ReadToken;
    if Token = ',' then
    begin
      FOrderDirection := odAsc;
      UnreadToken;
    end else if Token <> '' then
    begin
      FOrderDirection := TokenToOrderDirection(Token, IsOrderToken);
      if not IsOrderToken then
        InvalidTokenError(Token);
    end
  end;
end;

procedure TInstantIQLOrderSpec.WriteObject(Writer: TInstantIQLWriter);
begin
  if Assigned(FExpression) then
  begin
    FExpression.Write(Writer);
    if OrderDirection = odDesc then
    begin
      Writer.WriteSpace;
      Writer.WriteString(OrderTokens[FOrderDirection]);
    end;
  end;
end;

{ TInstantIQLOrder }

class function TInstantIQLOrder.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLOrderSpec.AtInstance(Reader)
end;

procedure TInstantIQLOrder.InternalClear;
begin
  inherited;
  FNextOrder := nil;
  FOrderSpec := nil;
end;

procedure TInstantIQLOrder.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  ReadChild(@FOrderSpec, Reader, [TInstantIQLOrderSpec]);
  Token := Reader.ReadToken;
  if Token = ',' then
    ReadChild(@FNextOrder, Reader, [TInstantIQLOrder])
  else if Token <> '' then
    Reader.UnreadToken;
end;

procedure TInstantIQLOrder.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  if Assigned(FOrderSpec) then
  begin
    FOrderSpec.Write(Writer);
    if Assigned(FNextOrder) then
    begin
      Writer.WriteString(', ');
      FNextOrder.Write(Writer);
    end;
  end;
end;

{ TInstantIQLSpecifier }

function TInstantIQLSpecifier.GetIsPath: Boolean;
begin
  Result := Operand is TInstantIQLPath;
end;

class function TInstantIQLSpecifier.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := TInstantIQLConstant.AtInstance(Reader) or
    TInstantIQLPath.AtInstance(Reader);
end;

procedure TInstantIQLSpecifier.InternalClear;
begin
  inherited;
  FOperand := nil;
end;

procedure TInstantIQLSpecifier.ReadObject(Reader: TInstantIQLReader);
begin
  inherited;
  ReadChild(@FOperand, Reader, [TInstantIQLConstant, TInstantIQLPath]);
end;

procedure TInstantIQLSpecifier.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  if Assigned(FOperand) then
    FOperand.Write(Writer);
end;

{ TInstantIQLClassRef }

class function TInstantIQLClassRef.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := InstantIsIdentifier(Reader.ReadToken);
end;

procedure TInstantIQLClassRef.InternalClear;
begin
  inherited;
  FObjectClassName := '';
end;

procedure TInstantIQLClassRef.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  Token := Reader.ReadToken;
  FAny := SameText(Token, 'ANY');
  if FAny then
    Token := Reader.ReadToken;
  FObjectClassName := Token;
end;

procedure TInstantIQLClassRef.WriteObject(Writer: TInstantIQLWriter);
begin
  inherited;
  if FAny then
    Writer.WriteKeyword('ANY');
  Writer.WriteString(FObjectClassName);
end;

{ TInstantIQLCommand }

function TInstantIQLCommand.GetAny: Boolean;
begin
  Result := Assigned(ClassRef) and ClassRef.Any;
end;

function TInstantIQLCommand.GetObjectClassName: string;
begin
  if Assigned(ClassRef) then
    Result := ClassRef.ObjectClassName
  else
    Result := '';
end;

function TInstantIQLCommand.GetResultClassName: string;
begin
  Result := ObjectClassName;
end;

class function TInstantIQLCommand.InternalAtInstance(
  Reader: TInstantIQLReader): Boolean;
begin
  Result := SameText(Reader.ReadToken, 'SELECT');
end;

procedure TInstantIQLCommand.InternalClear;
begin
  inherited;
  FClassRef := nil;
  FClause := nil;
  FOrder := nil;
  FSpecifier := nil;
end;

procedure TInstantIQLCommand.ReadObject(Reader: TInstantIQLReader);
var
  Token: string;
begin
  inherited;
  with Reader do
  begin
    Token := ReadToken;
    if not SameText(Token, 'SELECT') then
      UnexpectedTokenError(Token, 'SELECT');
    if SameText(ReadToken, 'DISTINCT') then
      FDistinct := True
    else
      UnreadToken;
    if not ReadChild(@FSpecifier, Reader, [TInstantIQLSpecifier]) then
      raise EInstantIQLError.Create(SSpecifierMissing);
    Token := ReadToken;
    if not SameText(Token, 'FROM') then
      UnexpectedTokenError(Token, 'FROM');
    if not ReadChild(@FClassRef, Reader, [TInstantIQLClassRef]) then
      raise EInstantIQLError.Create(SClassReferenceMissing);
    Token := ReadToken;
    if SameText(Token, 'WHERE') then
    begin
      ReadChild(@FClause, Reader, [TInstantIQLClause]);
      Token := ReadToken;
    end;
    if SameText(Token, 'ORDER') then
    begin
      Token := ReadToken;
      if not SameText(Token, 'BY') then
        InvalidTokenError(Token);
      ReadChild(@FOrder, Reader, [TInstantIQLOrder]);
      Token := ReadToken;
    end;
    if Token <> '' then
      InvalidTokenError(Token);
  end;
end;

procedure TInstantIQLCommand.WriteObject(Writer: TInstantIQLWriter);
begin
  if Assigned(FSpecifier) then
    with Writer do
    begin
      WriteKeyword('SELECT');
      if Distinct then
        WriteKeyword('DISTINCT');
      FSpecifier.Write(Writer);
      WriteSpace;
      WriteKeyword('FROM');
      if Assigned(FClassRef) then
        FClassRef.Write(Writer);
      if Assigned(FClause) then
      begin
        WriteSpace;
        WriteKeyword('WHERE');
        FClause.Write(Writer);
      end;
      if Assigned(FOrder) then
      begin
        WriteSpace;
        WriteKeyword('ORDER BY');
        FOrder.Write(Writer);
      end;
    end;
end;

{ TInstantIQLTranslator }

procedure TInstantIQLCommandTranslator.AfterTranslate;
begin
end;

procedure TInstantIQLCommandTranslator.BeforeTranslate;
begin
end;

procedure TInstantIQLCommandTranslator.Clear;
begin
end;

function TInstantIQLCommandTranslator.CreateCommand: TInstantIQLCommand;
begin
  Result := TInstantIQLCommand.Create(nil);
end;

function TInstantIQLCommandTranslator.GetCommand: TInstantIQLCommand;
begin
  Result := FCommand;
  if not Assigned(Result) then
    raise EInstantError.Create(SUnassignedCommandObject);
end;

function TInstantIQLCommandTranslator.GetResultClassName: string;
begin
  Result := '';
end;

procedure TInstantIQLCommandTranslator.SetCommandText(const Value: string);
begin
  if Value <> FCommandText then
  begin
    FCommand := CreateCommand;
    try
      FCommand.Text := Value;
      Clear;
      Beforetranslate;
      Translate;
      AfterTransLate;
      FCommandText := Value;
    finally
      FreeAndNil(FCommand);
    end;
  end;
end;

procedure TInstantIQLCommandTranslator.Translate;
begin
end;

{ TInstantIQLStatementWriter }

constructor TInstantIQLStatementWriter.Create(
  ATranslator: TInstantIQLTranslator; AStream: TStream;
  FreeStream: Boolean);
begin
  if not Assigned(ATranslator) then
    raise EInstantIQLError.Create(SUnassignedTranslator);
  FTranslator := ATranslator;
  inherited Create(AStream, FreeStream);
end;

procedure TInstantIQLStatementWriter.InternalWriteKeyword(
  const Keyword: string);
begin
  if not Translator.TranslateKeyword(Keyword, Self) then
    inherited;
end;

procedure TInstantIQLStatementWriter.InternalWriteObject(
  AObject: TInstantIQLObject);
begin
  if not Translator.TranslateObject(AObject, Self) then
    inherited;
end;

{ TInstantIQLTranslator }         

procedure TInstantIQLTranslator.Translate;
var
  Stream: TStringStream;
  Writer: TInstantIQLStatementWriter;
begin
  Stream := TStringStream.Create('');
  try
    Writer := TInstantIQLStatementWriter.Create(Self, Stream);
    try
      Command.Write(Writer);
      FStatementText := Stream.DataString;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TInstantIQLTranslator.TranslateKeyword(const Keyword: string;
  Writer: TInstantIQLWriter): Boolean;
begin
  Result := False;
end;

function TInstantIQLTranslator.TranslateObject(
  AObject: TInstantIQLObject; Writer: TInstantIQLWriter): Boolean;
begin
  Result := False;
end;

procedure TInstantIQLTranslator.WriteObject(AObject: TInstantIQLObject;
  Writer: TInstantIQLWriter);
begin
  if Assigned(AObject) and Assigned(Writer) then
    AObject.WriteObject(Writer);
end;

end.
