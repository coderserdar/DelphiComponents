{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for SQLite             }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZSqLiteToken;

interface

{$I ZParseSql.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZTokenizer, ZGenericSqlToken;

type

  {** Implements a SQLite-specific number state object. }
  TZSQLiteNumberState = class (TZNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a SQLite-specific quote string state object. }
  TZSQLiteQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeString(const Value: string; QuoteChar: Char): string; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZSQLiteCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZSQLiteSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZSQLiteWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZSQLiteTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

uses SysUtils, ZCompatibility{$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

{ TZSQLiteNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZSQLiteNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  FloatPoint: Boolean;
  LastChar: Char;

  procedure ReadDecDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
    begin
      if CharInSet(LastChar, ['0'..'9']) then begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end else begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
    end;
  end;

begin
  FloatPoint := FirstChar = '.';
  Result.Value := '';
  InitBuf(FirstChar);
  Result.TokenType := ttUnknown;
  LastChar := #0;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then begin
    ReadDecDigits;
    FloatPoint := LastChar = '.';
    if FloatPoint then
    begin
      Stream.Read(TempChar{%H-}, SizeOf(Char));
      ToBuf(TempChar, Result.Value);
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    ReadDecDigits;

  { Reads a power part of the number }
  if (Ord(LastChar) or $20) = ord('e') then //CharInSet(LastChar, ['e','E']) then
  begin
    Stream.Read(TempChar, SizeOf(Char));
    ToBuf(TempChar, Result.Value);
    FloatPoint := True;

    Stream.Read(TempChar, SizeOf(Char));
    if CharInSet(TempChar, ['0'..'9','-','+']) then begin
      ToBuf(TempChar, Result.Value);
      ReadDecDigits;
    end else begin
      FlushBuf(Result.Value);
      Result.Value := Copy(Result.Value, 1, Length(Result.Value) - 1);
      Stream.Seek(-2*SizeOf(Char), soFromCurrent);
    end;
  end;
  FlushBuf(Result.Value);

  { Prepare the result }
  if Result.Value = '.' then begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end else
    if FloatPoint then
      Result.TokenType := ttFloat else
      Result.TokenType := ttInteger;
end;

{ TZSQLiteQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZSQLiteQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := '';
  InitBuf(FirstChar);
  LastChar := #0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if ((LastChar = FirstChar) and (ReadChar <> FirstChar)
      and (FirstChar <> '[')) or ((FirstChar = '[') and (LastChar = ']')) then
    begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    ToBuf(ReadChar, Result.Value);
    if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);

  if CharInSet(FirstChar, ['"', '[']) then
    Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZSQLiteQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if QuoteChar = '[' then
    Result := '[' + Value + ']'
  else if CharInSet(QuoteChar, [#39, '"']) then
    Result := QuoteChar + Value + QuoteChar
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZSQLiteQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
begin
  Result := Value;
  if Length(Value) >= 2 then
  begin
    if CharInSet(QuoteChar, [#39, '"']) and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = QuoteChar) then
    begin
      if Length(Value) > 2 then
        Result := AnsiDequotedStr(Value, QuoteChar)
      else Result := '';
    end
    else if (QuoteChar = '[') and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = ']') then
      Result := Copy(Value, 2, Length(Value) - 2)
  end;
end;

{ TZSQLiteCommentState }

{**
  Gets a SQLite specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZSQLiteCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  InitBuf(FirstChar);
  Result.Value := '';
  Result.TokenType := ttUnknown;

  if FirstChar = '-' then
  begin
    ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '-') then begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetSingleLineComment(Stream, Result.Value);
    end else begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end else if FirstChar = '/' then begin
    ReadNum := Stream.Read(ReadChar, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '*') then
    begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetMultiLineComment(Stream, Result.Value);
    end else begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else
    FlushBuf(Result.Value);
end;

{ TZSQLiteSymbolState }

{**
  Creates this SQLite-specific symbol state object.
}
constructor TZSQLiteSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
  Add('==');
  Add('<<');
  Add('>>');
  Add('||');
end;

{ TZSQLiteWordState }

{**
  Constructs this SQLite-specific word state object.
}
constructor TZSQLiteWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
end;

{ TZSQLiteTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
procedure TZSQLiteTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZSQLiteSymbolState.Create;
  NumberState := TZSQLiteNumberState.Create;
  QuoteState := TZSQLiteQuoteState.Create;
  WordState := TZSQLiteWordState.Create;
  CommentState := TZSQLiteCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('[', '[', QuoteState);
  SetCharacterState(']', ']', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

end.

