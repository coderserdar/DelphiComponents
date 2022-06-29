// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CSV.pas' rev: 5.00

#ifndef CSVHPP
#define CSVHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Csv
{
//-- type declarations -------------------------------------------------------
typedef WideString WString;

class DELPHICLASS ECsvError;
class PASCALIMPLEMENTATION ECsvError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ECsvError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ECsvError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ECsvError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ECsvError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ECsvError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ECsvError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ECsvError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ECsvError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ECsvError(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TCsvEndOfLine { ceCr, ceLf, ceCrLf };
#pragma option pop

#pragma pack(push, 4)
struct TCsvLine
{
	AnsiString Line;
	int FieldCount;
} ;
#pragma pack(pop)

typedef DynamicArray<TCsvLine >  TCsvLines;

typedef DynamicArray<WideString >  TCsvFields;

class DELPHICLASS TCsv;
class PASCALIMPLEMENTATION TCsv : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	DynamicArray<TCsvLine >  FLines;
	int FCurrentLine;
	DynamicArray<WideString >  FCurrentFields;
	bool FEnclosing;
	wchar_t FEnclosingChar;
	wchar_t FFieldSeparator;
	WideString __fastcall GetField(int LineIndex, int FieldIndex);
	int __fastcall GetFieldCount(int LineIndex);
	int __fastcall GetLineCount(void);
	void __fastcall CheckLineIndex(int LineIndex);
	void __fastcall CheckFieldIndex(int LineIndex, int FieldIndex);
	void __fastcall SetEnclosing(bool Value);
	void __fastcall SetEnclosingChar(wchar_t Value);
	void __fastcall SetField(int LineIndex, int FieldIndex, const WideString Value);
	void __fastcall SetFieldCount(int LineIndex, int Value);
	void __fastcall SetLineCount(int Value);
	void __fastcall SetFieldSeparator(wchar_t Value);
	void __fastcall ReadLine(int LineIndex);
	void __fastcall WriteLine(void);
	
public:
	__fastcall TCsv(wchar_t FieldSeparator, bool Enclosing, wchar_t EnclosingChar);
	__fastcall virtual ~TCsv(void);
	__property int LineCount = {read=GetLineCount, write=SetLineCount, nodefault};
	__property bool Enclosing = {read=FEnclosing, write=SetEnclosing, nodefault};
	__property wchar_t EnclosingChar = {read=FEnclosingChar, write=SetEnclosingChar, nodefault};
	__property int FieldCount[int LineIndex] = {read=GetFieldCount, write=SetFieldCount};
	__property WideString Fields[int LineIndex][int FieldIndex] = {read=GetField, write=SetField};
	__property wchar_t FieldSeparator = {read=FFieldSeparator, write=SetFieldSeparator, nodefault};
	void __fastcall DeleteField(int LineIndex, int FieldIndex);
	void __fastcall DeleteLine(int LineIndex);
	void __fastcall InsertField(int LineIndex, int FieldIndex);
	void __fastcall InsertLine(int LineIndex);
	void __fastcall LoadString(const WideString Text);
	void __fastcall LoadWideString(const WideString Text);
	void __fastcall LoadAnsiString(const AnsiString Text);
	void __fastcall LoadAnsiFile(const AnsiString FileName);
	void __fastcall LoadUtf8(const AnsiString Text);
	void __fastcall LoadUtf8File(const AnsiString FileName);
	WideString __fastcall ToString(TCsvEndOfLine EndOfLine);
	AnsiString __fastcall ToUtf8(TCsvEndOfLine EndOfLine);
	void __fastcall ToUtf8File(const AnsiString FileName, TCsvEndOfLine EndOfLine);
};


class DELPHICLASS TCsvWriter;
class PASCALIMPLEMENTATION TCsvWriter : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TStream* FStream;
	bool FDestroyStream;
	bool FEnclosing;
	wchar_t FEnclosingChar;
	wchar_t FFieldSeparator;
	AnsiString FFieldSeparatorUtf8;
	AnsiString FEndOfLine;
	bool FWasSeparator;
	void __fastcall WriteSeparator(void);
	
public:
	__fastcall TCsvWriter(Classes::TStream* Stream, wchar_t FieldSeparator, TCsvEndOfLine EndOfLine, bool Enclosing, wchar_t EnclosingChar)/* overload */;
	__fastcall TCsvWriter(const AnsiString FileName, wchar_t FieldSeparator, TCsvEndOfLine EndOfLine, bool Enclosing, wchar_t EnclosingChar)/* overload */;
	__fastcall virtual ~TCsvWriter(void);
	void __fastcall NextLine(void);
	void __fastcall Write(const WideString Field);
};


//-- var, const, procedure ---------------------------------------------------
#define CsvMIME "text/csv"
#define CsvFileExtension ".csv"

}	/* namespace Csv */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Csv;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CSV
