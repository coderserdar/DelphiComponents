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
unit myla_parser;

interface
{$I psc_defines.inc}

uses
  system.SysUtils,
  myla_system,
  myla_interfaces,

  psc_const;

{-------------------------------------------------------------}

const
  tok_None   = -1;
  tok_EOF    = 0;
  tok_String = 1;
  tok_Comment = 2;
  tok_Symbol = 3;
  tok_Integer = 4;
  tok_Float = 5;
  tok_ResWord = 6;
  tok_WhiteChar = 7;

  ps_Normal = 0;
  ps_Comment = 1;
  ps_Comment2 = 2;

type
  TPSCParserProc=function:Integer of object;

  TPSCParserProcs=class
  public
    Procs:Array[0..255] of TPSCParserProc;
  end;

  TPSCAbstractParser=class(TInterfacedObject)
  private
    FParserProcs:IPSCObjectList;
    FToken:Integer;
    FParserState:Integer;
    FResWordID:Integer;
    FResWords:IPSCStrings;
  protected
    function SkipToCharOrEol(C:Char):boolean;
    function ParseWhiteChar:Integer;
    function ParseIdentifier:Integer;
    function ParseIdentifierEx(Charset : TPSCCharSet):Integer;
    function RestIsComment:Integer;
    function ParseUniComplexCommentEnd(C:Char):Integer;
    function ParseJScrCommentEnd:Integer;
    function ParseJscrComment:Integer;
    function ParseStrConst:Integer;
    function ParseUniStrConst(C:Char):Integer;
    function ParseVBStrConst:Integer;
    function IsTokenResWord(ResWord:Integer):boolean;
    function ParseComment:Integer;virtual;
    function Parse1310Eol:Integer;
    function Parse1013Eol:Integer;
    function ParseEol(C:Char):Integer;virtual;
    function ParseNumConst:Integer;
    function GetNextChar:Char;
    function SimpleNextToken:Integer;

    procedure SkipDigits;
    procedure InitCommon;virtual;
    procedure InitEolState(State:Integer);

    function GetChar:Char;virtual;abstract;
    procedure Next;virtual;abstract;
    procedure Prior;virtual;abstract;
    procedure SetTokenEmpty;virtual;abstract;
  public
    function TokenString: string;virtual;abstract;

    procedure RegisterProc(const AInStates: Array of Integer;
      ACharSet:TPSCCharSet;AProc:TPSCParserProc);
    procedure ClearParserProcs;

    function NextToken:Integer;virtual;

    constructor Create;

    property Token:Integer Read FToken Write FToken;
    property ParserState:Integer Read FParserState Write FParserState;
    property ResWords:IPSCStrings Read FResWords;
    property ResWordID:Integer Read FResWordID;
  end;

  TPSCParser=class(TPSCAbstractParser)
  private
    FSource:IPSCParserStream;
    FTokenString:String;
  protected
    function GetChar:Char;override;
    procedure Next;override;
    procedure Prior;override;
    procedure SetTokenEmpty;override;
  public
    function TokenString: string;override;
    constructor Create(const ASource:IPSCParserStream);
  end;

  TPSCSqlParser = Class(TPSCParser)
  private
    FParseBrackets: boolean;
    Function ParseFieldName: Integer;
  public
    Procedure InitSqlSyntax;
    Constructor Create(const ASource:IPSCParserStream);
    Property ParseBrackets: boolean read FParseBrackets write FParseBrackets;
  End;

Procedure PSCPrepareLikeMask(Const Mask: String; EscapeChar: Char;
  const PreparedMask:IPSCObjectList);
Function PSCSQLContainsWhere(Const S: String): boolean;
function PSCCreateParserStream(const S:String):IPSCParserStream;
Procedure PSCGetUsedFields(Const AExpr: String; const AFields: IPSCStrings);

{-------------------------------------------------------------}

implementation

{-------------------------------------------------------------}

type
  TPSCParserStreamString=class(TInterfacedObject,IPSCParserStream)
  private
    FString:String;
    FIndex:Integer;
    function GetChar:Char;
    procedure Next;
    procedure Prior;
    function EOF:Boolean;
  public
    constructor Create(const S:String);
  end;

{-------------------------------------------------------------}

function TPSCParserStreamString.EOF:Boolean;
begin
  Result:=(FIndex>Length(FString));
end;

{-------------------------------------------------------------}

function PSCCreateParserStream(const S:String):IPSCParserStream;
begin
  Result:=TPSCParserStreamString.Create(S);
end;

{-------------------------------------------------------------}

constructor TPSCParserStreamString.Create(const S:String);
begin
  inherited Create;
  FString:=S;
  FIndex:=1;
end;

{-------------------------------------------------------------}

function TPSCParserStreamString.GetChar:Char;
begin
  If (FIndex<1) or (FIndex>Length(FString)) then
    Result:=#0
  else
    Result:=FString[FIndex];
end;

{-------------------------------------------------------------}

procedure TPSCParserStreamString.Next;
begin
  If FIndex<=Length(FString) then
    inc(FIndex);
end;

{-------------------------------------------------------------}

procedure TPSCParserStreamString.Prior;
begin
  If FIndex>1 then
    dec(FIndex);
end;

{-------------------------------------------------------------}

Constructor TPSCSqlParser.Create(const ASource:IPSCParserStream);
Begin
  Inherited;
  InitSQLSyntax;
End;

{-----------------------------------------------------------}

Function TPSCSqlParser.ParseFieldName: Integer;
Begin
  Next;

  If FParseBrackets Then
    Begin
      If SkipToCharOrEol(']') Then
        Next;
      Result := tok_Symbol;
    End
  Else
    Result := tok_WhiteChar;
End;

{-----------------------------------------------------------}

Procedure TPSCSqlParser.InitSqlSyntax;
Begin
  PSCStrArrayToStrings(ResWords,PSCSqlResWords);
  InitCommon;
  RegisterProc([ps_Normal], ['"'],ParseVBStrConst);
  RegisterProc([ps_Normal], [''''],ParseStrConst);
  RegisterProc([ps_Normal],cPSCFirstIdentChar,ParseIdentifier);
  RegisterProc([ps_Normal],cPSCDigit,ParseNumConst);
  RegisterProc([ps_Normal], ['/'],ParseJScrComment);
  RegisterProc([ps_Normal], ['['],ParseFieldName);
  RegisterProc([ps_Comment2],cPSCWhiteChar,ParseJscrCommentEnd);
  InitEolState(ps_Comment2);
End;

{-------------------------------------------}

Function PSCSQLContainsWhere(Const S: String): boolean;
Var
  Parser: TPSCSQLParser;
  ResWordWhere: Integer;
Begin
  Result := False;

  Parser := TPSCSQLParser.Create(PSCCreateParserStream(S));
  With Parser Do
  Try
    ResWordWhere := ResWords.IndexOf('WHERE'); //don't resource
    While Token <> tok_EOF Do
      Begin
        If IsTokenResWord(ResWordWhere) Then
          Begin
            Result := True;
            exit;
          End;
        NextToken;
      End;
  Finally
    Free;
  End;
End;

{-------------------------------------------------------------}

const
  pslike_normal = 0;
  pslike_escape = 1;

type
  TPSCLikeMaskParser = Class(TPSCParser)
  private
    FEscapeChar: Char;
    Procedure SetEscapeChar(V: Char);
  protected
    Procedure InitLikeMaskSyntax;
    Function LikeParseString: Integer;
    Function LikeParseAnyChars: Integer;
    Function LikeParseAnyChar: Integer;
    Function LikeParseCharSet: Integer;
    Function LikeParseEscapeChar: Integer;
  public
    Property EscapeChar: Char read FEscapeChar write SetEscapeChar;
    Constructor Create(const ASource:IPSCParserStream);
    Procedure PrepareMask(const PreparedMask: IPSCObjectList);
  End;

{-------------------------------------------------------------}

Procedure TPSCLikeMaskParser.PrepareMask(const PreparedMask:IPSCObjectList);

  Function _ParseCharSet(Const S: String; StartChar: Integer): TPSCCharSet;
  Var
    L: Integer;
  Begin
    L := Length(S);
    If (L > 0) And (S[L] = ']') Then
      dec(L);
    Result := PSCParseLikeCharSetDef(Copy(S,StartChar + 1,L - StartChar));
  End;

  Procedure AddItem(AItemType: Integer; Const S: String; CharSet:
    TPSCCharSet);
  Var
    Item: TPSCPreparedMaskItem;
  Begin
    Item := TPSCPreparedMaskItem.Create;
    With Item Do
      Begin
        ItemType := AItemType;
        StrData := S;
        CharSetData := CharSet;
      End;
    PreparedMask.Add(Item);
  End;

Begin
  PreparedMask.Clear;
  While NextToken <> tok_Eof Do
    Case Token Of
      tok_like_Str:
        AddItem(Token,TokenString, []);
      tok_like_AnyChar,tok_like_AnyChars:
        AddItem(Token,TokenString, []);
      tok_like_CharSet:
        AddItem(Token, '',_ParseCharSet(TokenString,1));
      tok_like_NotCharSet:
        AddItem(Token, '',_ParseCharSet(TokenString,2));
      tok_like_EscapeChar:
        Begin
        End;
    End;
End;

{-------------------------------------------------------------}

Procedure TPSCLikeMaskParser.SetEscapeChar(V: Char);
Begin
  If FEscapeChar <> V Then
    Begin
      FEscapeChar := V;
      InitLikeMaskSyntax;
    End;
End;

{-------------------------------------------------------------}

Constructor TPSCLikeMaskParser.Create(const ASource:IPSCParserStream);
Begin
  Inherited;
  FEscapeChar := '^';
  InitLikeMaskSyntax;
End;

{-------------------------------------------------------------}

Function TPSCLikeMaskParser.LikeParseString: Integer;
Begin
  Repeat
    Next;
  //Until (GetChar In [FEscapeChar,#0, '[', '%', '_']);
  Until (CharInSet(GetChar,[FEscapeChar,#0, '[', '%', '_']));
  Result := tok_like_Str;
  ParserState := pslike_normal;
End;

{-------------------------------------------------------------}

Function TPSCLikeMaskParser.LikeParseAnyChars: Integer;
Begin
  Next;
  Result := tok_like_AnyChars;
End;

{-------------------------------------------------------------}

Function TPSCLikeMaskParser.LikeParseAnyChar: Integer;
Begin
  Next;
  Result := tok_like_AnyChar;
End;

{-------------------------------------------------------------}

Function TPSCLikeMaskParser.LikeParseCharSet: Integer;
Begin
  Next;
  If GetChar = '^' Then
    Result := tok_like_NotCharSet
  Else
    Result := tok_like_CharSet;
  Repeat
    Next;
  //Until (GetChar In [#0, ']']);
  Until (CharInSet(GetChar,[#0, ']']));
  If GetChar = ']' Then
    Next;
End;

{-------------------------------------------------------------}

Function TPSCLikeMaskParser.LikeParseEscapeChar: Integer;
Begin
  Next;
  ParserState := pslike_escape;
  Result := tok_like_EscapeChar;
End;

{-------------------------------------------------------------}

Procedure TPSCLikeMaskParser.InitLikeMaskSyntax;
Begin
  ClearParserProcs;

  RegisterProc([pslike_Normal], [#1..#255],LikeParseString);
  RegisterProc([pslike_Normal], ['%'],LikeParseAnyChars);
  RegisterProc([pslike_Normal], ['_'],LikeParseAnyChar);
  RegisterProc([pslike_Normal], ['['],LikeParseCharSet);
  RegisterProc([pslike_Normal], [FEscapeChar],LikeParseEscapeChar);
  RegisterProc([pslike_escape], [#1..#255],LikeParseString);
End;

{-------------------------------------------------------------}

Procedure PSCPrepareLikeMask(Const Mask: String; EscapeChar: Char;
  const PreparedMask:IPSCObjectList);
Var
  Parser: TPSCLikeMaskParser;
Begin
  Parser := TPSCLikeMaskParser.Create(PSCCreateParserStream(Mask));
  Try
    Parser.EscapeChar := EscapeChar;
    Parser.PrepareMask(PreparedMask);
  Finally
    Parser.Free;
  End;
End;

{-------------------------------------------------------------}

procedure TPSCAbstractParser.ClearParserProcs;
begin
  FParserProcs.Clear;
end;

{-------------------------------------------------------------}

procedure TPSCAbstractParser.RegisterProc(const AInStates: Array of Integer;
            ACharSet:TPSCCharSet;AProc:TPSCParserProc);
Var
  i,j:Integer;
  MyMaxState:Integer;
begin
  MyMaxState:=PSCMaxInArray(AInStates);
  While MyMaxState>=FParserProcs.Count do
    FParserProcs.Add(TPSCParserProcs.Create);

  for i:=Low(AInStates) to High(AInStates) do
    for j:=0 to 255 do
      if CharInSet(Char(j),ACharSet) then //if Char(j) in ACharSet then
        TPSCParserProcs(FParserProcs[AInStates[i]]).Procs[j]:=AProc;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.SkipToCharOrEol(C:Char):boolean;
begin
  While (GetChar<>C) and (CharInSet(GetChar,cPSCValidChar)) do  //(GetChar in cPSCValidChar) do
    Next;
  Result:=GetChar=C;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseComment:Integer;
begin
  Result:=tok_Comment;
  If SkipToCharOrEol('}') then
    begin
      Next;
      FParserState:=ps_Normal;
    end
  else
    FParserState:=ps_Comment;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseNumConst:Integer;

  function ParseScaleFactor:boolean;
  begin
    Result:=CharInSet(GetChar,['E','e']); //GetChar in ['E','e'];
    If Result then
    begin
      Next;
      If CharInSet(GetChar,['+','-']) then  //GetChar in ['+','-'] then
        Next;
      SkipDigits;
    end;
  end;

begin
  Next;
  SkipDigits;
  if (GetChar='.') and CharInSet(GetNextChar,cPSCDigit) then  //(GetNextChar in cPSCDigit) then
  begin
    Next;
    SkipDigits;
    Result:=tok_Float;
    ParseScaleFactor;
  end else
    if ParseScaleFactor then
      Result:=tok_Float
    else
      Result := tok_Integer;
end;

{-------------------------------------------------------------}

procedure TPSCAbstractParser.SkipDigits;
begin
  while CharInSet(GetChar,cPSCDigit) do   //GetChar in cPSCDigit do
    Next;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseWhiteChar:Integer;
begin
  Result:=tok_WhiteChar;
  Next;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseIdentifier:Integer;
begin
  result := ParseIdentifierEx(CPSCOtherIdentChars);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseIdentifierEx(Charset : TPSCCharSet):Integer;
begin
  repeat
    Next;
  until not CharInSet(GetChar,Charset); //(GetChar in Charset);

  if Reswords.Find(TokenString,FResWordID) then
    Result:=tok_ResWord
  else
    Result:=tok_Symbol;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.RestIsComment:Integer;
begin
  While CharInSet(GetChar, cPSCValidChar) do   //GetChar in cPSCValidChar do
    Next;
  Result:=tok_Comment;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseEol(C:Char):Integer;
begin
  Next;
  If GetChar=C then
    Next;
  Result:=tok_None;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.Parse1013Eol:Integer;
begin
  Result:=ParseEol(#13);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.Parse1310Eol:Integer;
begin
  Result:=ParseEol(#10);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseJscrComment:Integer;
begin
  Next;
  case GetChar of
   '*':
     begin
       Next;
       Result:=ParseJScrCommentEnd;
     end;
   '/':
     Result:=RestIsComment
   else
     Result:=tok_WhiteChar;
  end;
end;

{-------------------------------------------------------------}

procedure TPSCAbstractParser.InitEolState(State:Integer);
begin
  RegisterProc([State],[#13],Parse1310Eol);
  RegisterProc([State],[#10],Parse1013Eol);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.GetNextChar:Char;
begin
  Next;
  Result:=GetChar;
  Prior;
end;

{-------------------------------------------------------------}

procedure TPSCAbstractParser.InitCommon;
begin
  RegisterProc([ps_Normal],cPSCWhiteChar,ParseWhiteChar);
  InitEolState(ps_Normal);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseJScrCommentEnd:Integer;
begin
  Result:=ParseUniComplexCommentEnd('/');
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseUniComplexCommentEnd(C:Char):Integer;
begin
  While true do
  begin
    If SkipToCharOrEol('*') then
    begin
      Next;
      if GetChar=C then
      begin
        FParserState:=ps_Normal;
        Next;
        break;
      end;
    end else
      begin
        FParserState:=ps_Comment2;
        break;
      end
  end;
  Result:=tok_Comment;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseStrConst:Integer;
begin
  Result:=ParseUniStrConst('''');
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseUniStrConst(C:Char):Integer;
var
  cfound : boolean;
begin
  Result:=tok_String;
  repeat
    Next;
    cfound := SkipToCharOrEol(C);
    if cfound then
      Next;
  until not cfound or (GetChar <> C);
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.ParseVBStrConst:Integer;
begin
  Result:=ParseUniStrConst('"');
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.NextToken: Integer;
begin
  Result:=SimpleNextToken;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.SimpleNextToken:Integer;
Var
  Method:TPSCParserProc;
begin
  Result:=tok_None;
  repeat
    SetTokenEmpty;
    If GetChar=#0 then
    begin
      Result:=tok_EOF;
      break;
    end;
    Method:=TPSCParserProcs(FParserProcs.Items[FParserState]).Procs[Integer(GetChar)];
    If Assigned(Method) then
      Result:=Method
    else
      Next;
  until Result<>tok_None;

  FToken:=Result;
end;

{-------------------------------------------------------------}

constructor TPSCAbstractParser.Create;
begin
  inherited;
  FParserProcs:=PSCCreateObjectList(ioOwned);
  FToken:=tok_None;
  FResWords:=PSCCreateSortedStringList;
end;

{-------------------------------------------------------------}

function TPSCAbstractParser.IsTokenResWord(ResWord:Integer):boolean;
begin
  Result:=(Token=tok_ResWord) and (FResWordID=ResWord);
end;

{-------------------------------------------------------------}

function TPSCParser.GetChar:Char;
begin
  Result:=FSource.GetChar;
end;

{-------------------------------------------------------------}

procedure TPSCParser.Next;
begin
  If GetChar<>#0 then
    FTokenString:=FTokenString+GetChar;
  FSource.Next;
end;

{-------------------------------------------------------------}

procedure TPSCParser.Prior;
begin
  If FTokenString<>'' then
    Delete(FTokenString,Length(FTokenString),1);
  FSource.Prior;
end;

{-------------------------------------------------------------}

procedure TPSCParser.SetTokenEmpty;
begin
  FTokenString:='';
end;

{-------------------------------------------------------------}

function TPSCParser.TokenString: string;
begin
  Result:=FTokenString;
end;

{-------------------------------------------------------------}

constructor TPSCParser.Create(const ASource:IPSCParserStream);
begin
  inherited Create;
  FSource:=ASource;
end;

{-------------------------------------------------------------------------}

Procedure PSCGetUsedFields(Const AExpr: String; const AFields: IPSCStrings);
Var
  SQLParser: TPSCSqlParser;
Begin
  SQLParser := TPSCSqlParser.Create(PSCCreateParserStream(AExpr));
  try
    Try
      With SQLParser Do
        Begin
          ParseBrackets := True;
          While NextToken <> tok_Eof Do
            If Token = tok_Symbol Then
              AFields.Add(PSCTrimSeparators(TokenString, ['[', ']']));
        End;

    Except
      AFields.Clear;
    End;
  finally
    SQLParser.Free;
  end;
End;

{-------------------------------------------------------------}

end.



