// CodeGear C++ Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Winjson.pas' rev: 11.00

#ifndef WinjsonHPP
#define WinjsonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Winjson
{
//-- type declarations -------------------------------------------------------
typedef DynamicArray<Byte >  TBytes;

class DELPHICLASS EJsonError;
class PASCALIMPLEMENTATION EJsonError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EJsonError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EJsonError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EJsonError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EJsonError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EJsonError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EJsonError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EJsonError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EJsonError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EJsonError(void) { }
	#pragma option pop
	
};


class DELPHICLASS TJson;
class DELPHICLASS TStringBuilder;
class PASCALIMPLEMENTATION TJson : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	TJson* operator[](WideString Name) { return Items[Name]; }
	
private:
	virtual TJson* __fastcall Clone(void) = 0 ;
	virtual TJson* __fastcall GetAt(int Index);
	virtual TJson* __fastcall GetItem(const WideString Name);
	virtual void __fastcall SetAt(int Index, TJson* Value);
	virtual void __fastcall SetItem(const WideString Name, TJson* Value);
	virtual void __fastcall Write(TStringBuilder* StringBuilder) = 0 /* overload */;
	virtual void __fastcall Write(TStringBuilder* StringBuilder, WideChar TabChar, int TabSize, int Indentation)/* overload */;
	
public:
	virtual bool __fastcall AsBoolean(void);
	virtual System::TDateTime __fastcall AsDateTime(void);
	virtual int __fastcall AsDateTimeOffset(void);
	virtual double __fastcall AsNumber(void);
	virtual WideString __fastcall AsString();
	__property TJson* At[int Index] = {read=GetAt, write=SetAt};
	__property TJson* Items[WideString Name] = {read=GetItem, write=SetItem/*, default*/};
	virtual bool __fastcall IsArray(void);
	virtual bool __fastcall IsBoolean(void);
	virtual bool __fastcall IsDateTime(void);
	virtual bool __fastcall IsLiteral(void);
	virtual bool __fastcall IsNull(void);
	virtual bool __fastcall IsNumber(void);
	virtual bool __fastcall IsObject(void);
	virtual bool __fastcall IsString(void);
	WideString __fastcall ToString(bool PrettyPrint = true, bool UseTabChar = false, int TabSize = 0x2);
	AnsiString __fastcall ToUtf8(bool PrettyPrint = true, bool UseTabChar = false, int TabSize = 0x2);
	void __fastcall ToUtf8File(const AnsiString FileName, bool PrettyPrint = true, bool UseTabChar = false, int TabSize = 0x2);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJson(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJson(void) { }
	#pragma option pop
	
};


class DELPHICLASS TJsonLiteral;
class PASCALIMPLEMENTATION TJsonLiteral : public TJson 
{
	typedef TJson inherited;
	
private:
	bool FFreeInstance;
	
public:
	virtual bool __fastcall IsLiteral(void);
	virtual void __fastcall FreeInstance(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonLiteral(void) : TJson() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonLiteral(void) { }
	#pragma option pop
	
};


class DELPHICLASS TJsonNull;
class PASCALIMPLEMENTATION TJsonNull : public TJsonLiteral 
{
	typedef TJsonLiteral inherited;
	
private:
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	
public:
	virtual bool __fastcall IsNull(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonNull(void) : TJsonLiteral() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonNull(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
};


class DELPHICLASS TJsonFalse;
class PASCALIMPLEMENTATION TJsonFalse : public TJsonLiteral 
{
	typedef TJsonLiteral inherited;
	
private:
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	
public:
	virtual bool __fastcall AsBoolean(void);
	virtual bool __fastcall IsBoolean(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonFalse(void) : TJsonLiteral() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonFalse(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
};


class DELPHICLASS TJsonTrue;
class PASCALIMPLEMENTATION TJsonTrue : public TJsonLiteral 
{
	typedef TJsonLiteral inherited;
	
private:
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	
public:
	virtual bool __fastcall AsBoolean(void);
	virtual bool __fastcall IsBoolean(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonTrue(void) : TJsonLiteral() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonTrue(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
};


class DELPHICLASS TJsonNumber;
class PASCALIMPLEMENTATION TJsonNumber : public TJson 
{
	typedef TJson inherited;
	
private:
	double FValue;
	__fastcall TJsonNumber(double Value);
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	
public:
	virtual double __fastcall AsNumber(void);
	virtual bool __fastcall IsNumber(void);
	__property double Value = {read=FValue};
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonNumber(void) : TJson() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonNumber(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
};


class DELPHICLASS TJsonString;
class PASCALIMPLEMENTATION TJsonString : public TJson 
{
	typedef TJson inherited;
	
private:
	WideString FValue;
	__fastcall TJsonString(const WideString Value);
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	System::TDateTime __fastcall GetDateTime(void);
	int __fastcall GetDateTimeOffset(void);
	
public:
	virtual WideString __fastcall AsString();
	virtual System::TDateTime __fastcall AsDateTime(void);
	virtual int __fastcall AsDateTimeOffset(void);
	virtual bool __fastcall IsDateTime(void);
	virtual bool __fastcall IsString(void);
	__property WideString Value = {read=FValue};
	__property System::TDateTime DateTime = {read=GetDateTime};
	__property int DateTimeOffset = {read=GetDateTimeOffset, nodefault};
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TJsonString(void) : TJson() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonString(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
};


typedef DynamicArray<TJson* >  WinJson__01;

class DELPHICLASS TJsonArray;
class DELPHICLASS TJsonObject;
class PASCALIMPLEMENTATION TJsonArray : public TJson 
{
	typedef TJson inherited;
	
public:
	TJson* operator[](int Index) { return Elements[Index]; }
	
private:
	DynamicArray<TJson* >  FElements;
	void __fastcall CheckIndex(int Index);
	virtual TJson* __fastcall GetAt(int Index);
	virtual void __fastcall SetAt(int Index, TJson* Value);
	int __fastcall GetElementCount(void);
	void __fastcall SetElementCount(int Value);
	TJson* __fastcall GetElement(int Index);
	void __fastcall SetElement(int Index, TJson* Element);
	void __fastcall SetRawElement(int Index, TJson* Element);
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	virtual void __fastcall Write(TStringBuilder* StringBuilder, WideChar TabChar, int TabSize, int Indentation)/* overload */;
	
public:
	__fastcall TJsonArray(int ElementCount);
	__fastcall virtual ~TJsonArray(void);
	__property TJson* Elements[int Index] = {read=GetElement, write=SetElement/*, default*/};
	__property int ElementCount = {read=GetElementCount, write=SetElementCount, nodefault};
	virtual bool __fastcall IsArray(void);
	void __fastcall SetNull(int Index);
	void __fastcall SetFalse(int Index);
	void __fastcall SetTrue(int Index);
	void __fastcall SetNumber(int Index, double Value);
	void __fastcall SetString(int Index, const WideString Value);
	void __fastcall SetDateTime(int Index, const System::TDateTime Value, int Offset = 0x0);
	TJsonArray* __fastcall SetArray(int Index, int ElementCount);
	TJsonObject* __fastcall SetObject(int Index);
	TJson* __fastcall SetJson(int Index, TJson* Value);
};


struct TJsonMember
{
	
public:
	WideString FName;
	TJson* FValue;
} ;

typedef DynamicArray<TJsonMember >  WinJson__21;

class PASCALIMPLEMENTATION TJsonObject : public TJson 
{
	typedef TJson inherited;
	
public:
	TJson* operator[](WideString Name) { return Members[Name]; }
	
private:
	DynamicArray<TJsonMember >  FMembers;
	int FMemberCount;
	int __fastcall GetMemberCapacity(void);
	void __fastcall CheckIndex(int Index);
	void __fastcall ClearMember(int Index);
	virtual TJson* __fastcall GetItem(const WideString Name);
	virtual void __fastcall SetItem(const WideString Name, TJson* Value);
	WideString __fastcall GetMemberName(int Index);
	void __fastcall SetMemberName(int Index, const WideString Value);
	TJson* __fastcall GetMemberValue(int Index);
	void __fastcall SetMemberValue(int Index, TJson* Value);
	virtual TJson* __fastcall Clone(void);
	virtual void __fastcall Write(TStringBuilder* StringBuilder)/* overload */;
	virtual void __fastcall Write(TStringBuilder* StringBuilder, WideChar TabChar, int TabSize, int Indentation)/* overload */;
	TJson* __fastcall GetMember(const WideString Name);
	void __fastcall SetMember(const WideString Name, TJson* Value);
	void __fastcall SetRawMember(int Index, const WideString Name, TJson* Value);
	__property int MemberCapacity = {read=GetMemberCapacity, nodefault};
	
public:
	__fastcall TJsonObject(void);
	__fastcall virtual ~TJsonObject(void);
	__property int MemberCount = {read=FMemberCount, nodefault};
	__property WideString MemberName[int Index] = {read=GetMemberName, write=SetMemberName};
	__property TJson* MemberValue[int Index] = {read=GetMemberValue, write=SetMemberValue};
	__property TJson* Members[WideString Name] = {read=GetMember, write=SetMember/*, default*/};
	int __fastcall Find(const WideString Name);
	void __fastcall DeleteAll(void);
	void __fastcall Delete(int Index)/* overload */;
	void __fastcall Delete(const WideString Name)/* overload */;
	virtual bool __fastcall IsObject(void);
	void __fastcall SetNull(const WideString Name);
	void __fastcall SetFalse(const WideString Name);
	void __fastcall SetTrue(const WideString Name);
	void __fastcall SetNumber(const WideString Name, double Value);
	void __fastcall SetString(const WideString Name, const WideString Value);
	void __fastcall SetDateTime(const WideString Name, const System::TDateTime Value, int Offset = 0x0);
	TJsonArray* __fastcall SetArray(const WideString Name, int ElementCount);
	TJsonObject* __fastcall SetObject(const WideString Name);
	TJson* __fastcall SetJson(const WideString Name, TJson* Value);
};


class PASCALIMPLEMENTATION TStringBuilder : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	WideString FText;
	int FLength;
	WideString __fastcall GetText();
	int __fastcall GetCapacity(void);
	void __fastcall SetCapacity(int Value);
	void __fastcall EnsureCapacity(int Value);
	
public:
	__fastcall TStringBuilder(int Capacity);
	void __fastcall Clear(void);
	void __fastcall Append(WideChar Value)/* overload */;
	void __fastcall Append(const WideString Value)/* overload */;
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property WideString Text = {read=GetText};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TStringBuilder(void) { }
	#pragma option pop
	
};


class DELPHICLASS TJsonCustomParser;
class PASCALIMPLEMENTATION TJsonCustomParser : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	WideString FText;
	int FLastColumn;
	int FLastPosition;
	int FLastRow;
	int FPosition;
	TStringBuilder* FStringBuilder;
	bool __fastcall IsEnd(void);
	WideChar __fastcall Current(void);
	void __fastcall Next(void);
	bool __fastcall IsWhiteSpace(void);
	void __fastcall SkipWhiteSpace(void);
	bool __fastcall IsStructuralCharacter(void);
	bool __fastcall IsDigit(void);
	void __fastcall PositionToRowColumn(int Position, int &Row, int &Column);
	void __fastcall Error(int Position, const AnsiString ErrorMessage);
	void __fastcall Read(WideChar C);
	int __fastcall ReadDigit(void);
	int __fastcall ReadHexaDigit(void);
	double __fastcall ParseNumber(void);
	WideString __fastcall ParseString();
	
protected:
	__fastcall TJsonCustomParser(void);
	virtual TJson* __fastcall Parse(const WideString Text);
	
public:
	__fastcall virtual ~TJsonCustomParser(void);
	TJson* __fastcall ParseUtf8(const TBytes Utf8)/* overload */;
	TJson* __fastcall ParseAnsi(const TBytes Ansi)/* overload */;
	TJson* __fastcall ParseUtf8(const AnsiString Utf8)/* overload */;
	TJson* __fastcall ParseAnsi(const AnsiString Ansi)/* overload */;
	TJson* __fastcall ParseUtf8(Classes::TStream* Stream)/* overload */;
	TJson* __fastcall ParseAnsi(Classes::TStream* Stream)/* overload */;
	TJson* __fastcall ParseUtf8File(const AnsiString FileName);
	TJson* __fastcall ParseAnsiFile(const AnsiString FileName);
};


class DELPHICLASS TJsonParser;
class PASCALIMPLEMENTATION TJsonParser : public TJsonCustomParser 
{
	typedef TJsonCustomParser inherited;
	
private:
	TJson* __fastcall ParseValue(void);
	TJson* __fastcall ParseLiteral(void);
	TJsonObject* __fastcall ParseObject(void);
	TJsonArray* __fastcall ParseArray(void);
	
public:
	__fastcall TJsonParser(void);
	virtual TJson* __fastcall Parse(const WideString Text);
public:
	#pragma option push -w-inl
	/* TJsonCustomParser.Destroy */ inline __fastcall virtual ~TJsonParser(void) { }
	#pragma option pop
	
};


typedef DynamicArray<bool >  WinJson__71;

class DELPHICLASS TJsonWriter;
class PASCALIMPLEMENTATION TJsonWriter : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	DynamicArray<bool >  FNesting;
	int FNestedLevel;
	bool FDestroyStream;
	bool FFirstItem;
	bool FPrettyPrint;
	Classes::TStream* FStream;
	int FTabSize;
	bool FUseTabChar;
	bool FUtf8;
	void __fastcall AddNesting(bool IsObject);
	bool __fastcall InArray(void);
	bool __fastcall InObject(void);
	void __fastcall WriteIndentation(void);
	void __fastcall WriteSeparator(void);
	void __fastcall WriteNamePart(const WideString Name);
	void __fastcall RequireObject(void);
	void __fastcall RequireArray(void);
	bool __fastcall IsEmpty(void);
	
public:
	__fastcall TJsonWriter(Classes::TStream* Stream, bool PrettyPrint, bool UseTabChar, int TabSize, bool Utf8)/* overload */;
	__fastcall TJsonWriter(const AnsiString FileName, bool PrettyPrint, bool UseTabChar, int TabSize)/* overload */;
	__fastcall virtual ~TJsonWriter(void);
	void __fastcall Check(void);
	void __fastcall BeginArray(void)/* overload */;
	void __fastcall BeginObject(void)/* overload */;
	void __fastcall BeginArray(const WideString Name)/* overload */;
	void __fastcall BeginObject(const WideString Name)/* overload */;
	void __fastcall EndArray(void);
	void __fastcall EndObject(void);
	void __fastcall WriteNull(void)/* overload */;
	void __fastcall Write(bool Value)/* overload */;
	void __fastcall Write(double Value)/* overload */;
	void __fastcall Write(const WideString Value)/* overload */;
	void __fastcall Write(const System::TDateTime Value, int Offset)/* overload */;
	void __fastcall WriteNull(const WideString Name)/* overload */;
	void __fastcall Write(const WideString Name, bool Value)/* overload */;
	void __fastcall Write(const WideString Name, double Value)/* overload */;
	void __fastcall Write(const WideString Name, const WideString Value)/* overload */;
	void __fastcall Write(const WideString Name, const System::TDateTime Value, int Offset)/* overload */;
};


#pragma option push -b-
enum TJsonItem { itEof, itNull, itFalse, itTrue, itNumber, itString, itBeginArray, itEndArray, itBeginObject, itEndObject };
#pragma option pop

typedef DynamicArray<bool >  WinJson__91;

class DELPHICLASS TJsonReader;
class PASCALIMPLEMENTATION TJsonReader : public TJsonCustomParser 
{
	typedef TJsonCustomParser inherited;
	
private:
	bool FFirstItem;
	bool FIsMember;
	int FItemPosition;
	WideString FMemberName;
	DynamicArray<bool >  FNesting;
	int FNestedLevel;
	double FNumberValue;
	WideString FStringValue;
	bool FWasJsonItem;
	void __fastcall AddNesting(bool IsObject);
	int __fastcall GetColumn(void);
	int __fastcall GetRow(void);
	bool __fastcall InArray(void);
	bool __fastcall InObject(void);
	TJsonItem __fastcall ParseLiteral(void);
	TJsonItem __fastcall ReadValue(void);
	
public:
	__fastcall TJsonReader(const WideString Text)/* overload */;
	__fastcall TJsonReader(const TBytes Data, bool Utf8)/* overload */;
	__fastcall TJsonReader(const AnsiString Data, bool Utf8)/* overload */;
	__fastcall TJsonReader(Classes::TStream* Stream, bool Utf8)/* overload */;
	__fastcall TJsonReader(const AnsiString FileName, bool Utf8, bool FromFile)/* overload */;
	HIDESBASE TJsonItem __fastcall Read(void);
	__property int Column = {read=GetColumn, nodefault};
	__property bool IsMember = {read=FIsMember, nodefault};
	__property int ItemPosition = {read=FItemPosition, nodefault};
	__property WideString MemberName = {read=FMemberName};
	__property double NumberValue = {read=FNumberValue};
	__property int Row = {read=GetRow, nodefault};
	__property WideString StringValue = {read=FStringValue};
public:
	#pragma option push -w-inl
	/* TJsonCustomParser.Destroy */ inline __fastcall virtual ~TJsonReader(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define JsonMIME "application/json"
#define JsonFileExtension ".json"

}	/* namespace Winjson */
using namespace Winjson;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Winjson
