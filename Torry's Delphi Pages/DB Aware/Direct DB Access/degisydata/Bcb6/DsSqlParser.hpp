// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsSqlParser.pas' rev: 6.00

#ifndef DsSqlParserHPP
#define DsSqlParserHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dssqlparser
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTokenType { tkNone, tkKeyWord, tkIdent, tkNumber, tkString, tkComment, tkTerm, tkComma, tkBracket, tkOperator, tkWhiteSpace, tkOther };
#pragma option pop

#pragma option push -b-
enum TSQLKeyWord { kwNone, kwAlter, kwDistinct, kwFrom, kwGroupBy, kwInsert, kwInto, kwOrderBy, kwSelect, kwSet, kwTable, kwTerm, kwWhere };
#pragma option pop

#pragma option push -b-
enum TSQLSchema { ssUNKNOWN, ssEMPTY, ssSELECT, ssINSERT, ssALTER, ssCREATE, ssUPDATE };
#pragma option pop

#pragma pack(push, 1)
struct TToken
{
	int Offset;
	int Len;
	TTokenType TokenType;
	AnsiString TokenText;
	TSQLKeyWord KeyWord;
} ;
#pragma pack(pop)

#pragma option push -b-
enum TParseState { psNormal, psIdent, psKeyword, psNumber, psString, psComment, psBracket, psOperator, psTerminator, psComma, psWhiteSpace, psOther };
#pragma option pop

class DELPHICLASS TCustomSQLParser;
class PASCALIMPLEMENTATION TCustomSQLParser : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TStringList* FKeyWords;
	AnsiString FInput;
	char FTermChar;
	char Ch;
	char Next;
	TParseState FState;
	AnsiString FOutPut;
	bool FPrior;
	int FIndex;
	int StrDelimCount;
	void __fastcall PushChar(char Ch);
	void __fastcall Mark(void);
	bool __fastcall IsKeyWord(AnsiString Ident);
	DYNAMIC TSQLKeyWord __fastcall GetKeywordType(AnsiString Ident);
	void __fastcall SetInput(AnsiString Value);
	TToken __fastcall GetNextToken();
	
protected:
	__property TToken NextToken = {read=GetNextToken};
	__property char TerminatorChar = {read=FTermChar, write=FTermChar, nodefault};
	__property AnsiString Input = {read=FInput, write=SetInput};
	__property Classes::TStringList* KeyWords = {read=FKeyWords, write=FKeyWords};
	
public:
	__fastcall TCustomSQLParser(void);
	__fastcall virtual ~TCustomSQLParser(void);
	void __fastcall Reset(void);
};


class DELPHICLASS TSQLParser;
class PASCALIMPLEMENTATION TSQLParser : public TCustomSQLParser 
{
	typedef TCustomSQLParser inherited;
	
private:
	DYNAMIC TSQLKeyWord __fastcall GetKeywordType(AnsiString Ident);
	
public:
	__property Input ;
	__property NextToken ;
	__fastcall TSQLParser(void);
public:
	#pragma option push -w-inl
	/* TCustomSQLParser.Destroy */ inline __fastcall virtual ~TSQLParser(void) { }
	#pragma option pop
	
};


#pragma pack(push, 4)
struct TStatementInfo
{
	int Offset;
	AnsiString Statement;
} ;
#pragma pack(pop)

class DELPHICLASS TSQLScriptParser;
class PASCALIMPLEMENTATION TSQLScriptParser : public TSQLParser 
{
	typedef TSQLParser inherited;
	
private:
	TStatementInfo __fastcall GetNextStatement();
	
public:
	__property Input ;
	__property TerminatorChar ;
	__property TStatementInfo NextStatement = {read=GetNextStatement};
public:
	#pragma option push -w-inl
	/* TSQLParser.Create */ inline __fastcall TSQLScriptParser(void) : TSQLParser() { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomSQLParser.Destroy */ inline __fastcall virtual ~TSQLScriptParser(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char *SQLKeyWordArray[13];

}	/* namespace Dssqlparser */
using namespace Dssqlparser;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsSqlParser
