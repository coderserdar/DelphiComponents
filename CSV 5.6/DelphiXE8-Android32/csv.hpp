// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CSV.pas' rev: 29.00 (Android)

#ifndef CsvHPP
#define CsvHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Csv
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ECsvError;
struct TCsvLine;
class DELPHICLASS TCsv;
class DELPHICLASS TCsvWriter;
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString WString;

typedef System::TArray__1<System::Byte> UTF8String;

#pragma pack(push,4)
class PASCALIMPLEMENTATION ECsvError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ECsvError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECsvError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ECsvError(System::PResStringRec ResStringRec) : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ECsvError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ECsvError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECsvError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECsvError(System::PResStringRec ResStringRec, int AHelpContext) : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECsvError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECsvError(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TCsvEndOfLine : unsigned char { ceCr, ceLf, ceCrLf };

struct DECLSPEC_DRECORD TCsvLine
{
public:
	System::UnicodeString Line;
	int FieldCount;
};


typedef System::DynamicArray<TCsvLine> TCsvLines;

typedef System::DynamicArray<System::UnicodeString> TCsvFields;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCsv : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCsvLines FLines;
	int FCurrentLine;
	TCsvFields FCurrentFields;
	bool FEnclosing;
	System::WideChar FEnclosingChar;
	System::WideChar FFieldSeparator;
	System::UnicodeString __fastcall GetField(int LineIndex, int FieldIndex);
	int __fastcall GetFieldCount(int LineIndex);
	int __fastcall GetLineCount(void);
	void __fastcall CheckLineIndex(int LineIndex);
	void __fastcall CheckFieldIndex(int LineIndex, int FieldIndex);
	void __fastcall SetEnclosing(bool Value);
	void __fastcall SetEnclosingChar(System::WideChar Value);
	void __fastcall SetField(int LineIndex, int FieldIndex, const System::UnicodeString Value);
	void __fastcall SetFieldCount(int LineIndex, int Value);
	void __fastcall SetLineCount(int Value);
	void __fastcall SetFieldSeparator(System::WideChar Value);
	void __fastcall ReadLine(int LineIndex);
	void __fastcall WriteLine(void);
	
public:
	__fastcall TCsv(System::WideChar FieldSeparator, bool Enclosing, System::WideChar EnclosingChar);
	__fastcall virtual ~TCsv(void);
	__property int LineCount = {read=GetLineCount, write=SetLineCount, nodefault};
	__property bool Enclosing = {read=FEnclosing, write=SetEnclosing, nodefault};
	__property System::WideChar EnclosingChar = {read=FEnclosingChar, write=SetEnclosingChar, nodefault};
	__property int FieldCount[int LineIndex] = {read=GetFieldCount, write=SetFieldCount};
	__property System::UnicodeString Fields[int LineIndex][int FieldIndex] = {read=GetField, write=SetField};
	__property System::WideChar FieldSeparator = {read=FFieldSeparator, write=SetFieldSeparator, nodefault};
	void __fastcall DeleteField(int LineIndex, int FieldIndex);
	void __fastcall DeleteLine(int LineIndex);
	void __fastcall InsertField(int LineIndex, int FieldIndex);
	void __fastcall InsertLine(int LineIndex);
	void __fastcall LoadString(const System::UnicodeString Text);
	void __fastcall LoadUtf8(const System::TArray__1<System::Byte> Text);
	void __fastcall LoadUtf8File(const System::UnicodeString FileName);
	HIDESBASE System::UnicodeString __fastcall ToString(TCsvEndOfLine EndOfLine = (TCsvEndOfLine)(0x2));
	System::TArray__1<System::Byte> __fastcall ToUtf8(TCsvEndOfLine EndOfLine = (TCsvEndOfLine)(0x2));
	void __fastcall ToUtf8File(const System::UnicodeString FileName, TCsvEndOfLine EndOfLine = (TCsvEndOfLine)(0x2));
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCsvWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FStream;
	bool FDestroyStream;
	bool FEnclosing;
	System::WideChar FEnclosingChar;
	System::WideChar FFieldSeparator;
	System::TArray__1<System::Byte> FFieldSeparatorUtf8;
	System::TArray__1<System::Byte> FEndOfLine;
	bool FWasSeparator;
	void __fastcall WriteSeparator(void);
	
public:
	__fastcall TCsvWriter(System::Classes::TStream* Stream, System::WideChar FieldSeparator, TCsvEndOfLine EndOfLine, bool Enclosing, System::WideChar EnclosingChar)/* overload */;
	__fastcall TCsvWriter(const System::UnicodeString FileName, System::WideChar FieldSeparator, TCsvEndOfLine EndOfLine, bool Enclosing, System::WideChar EnclosingChar)/* overload */;
	__fastcall virtual ~TCsvWriter(void);
	void __fastcall NextLine(void);
	void __fastcall Write(const System::UnicodeString Field);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define CsvMIME u"text/csv"
#define CsvFileExtension u".csv"
}	/* namespace Csv */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CSV)
using namespace Csv;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CsvHPP
