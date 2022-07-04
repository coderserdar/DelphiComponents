// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDbFilter.pas' rev: 6.00

#ifndef DsDbFilterHPP
#define DsDbFilterHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsdbfilter
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum DsFilterType { fltUnknown, fltOpenBracket, fltCloseBracket, fltDelimiter, fltVariable, fltFunction, fltDataField, fltString, fltBool, fltInteger, fltFloat, fltEqual, fltNotEqual, fltGrThen, fltLeThen, fltGrEqThen, fltLeEqThen, fltAND, fltOR, fltXOR, fltNOT, fltPLUS, fltMINUS, fltMULTI, fltDIV, fltDate, fltTime, fltDateTime, fltNull };
#pragma option pop

struct DsFilterData;
typedef DsFilterData *pDsFilterData;

#pragma pack(push, 1)
struct DsFilterData
{
	DsFilterType ExprType;
	Word ExprPos;
	union
	{
		struct 
		{
			System::SmallStringBase<255>  _string;
			
		};
		struct 
		{
			bool _bool;
			
		};
		struct 
		{
			double _float;
			
		};
		struct 
		{
			int _integer;
			
		};
		struct 
		{
			void *_field;
			
		};
		
	};
} ;
#pragma pack(pop)

typedef DsFilterData DsFilterDataArray[256];

typedef DsFilterData *pDsFilterDataArray;

#pragma option push -b-
enum DsValueType { vtInteger, vtDouble, vtBoolean, vtString };
#pragma option pop

struct DsValue;
typedef DsValue *pDsValue;

#pragma pack(push, 1)
struct DsValue
{
	DsValueType ValType;
	Word ValSize;
	union
	{
		struct 
		{
			System::SmallStringBase<255>  _string;
			
		};
		struct 
		{
			bool _bool;
			
		};
		struct 
		{
			double _float;
			
		};
		struct 
		{
			int _integer;
			
		};
		
	};
} ;
#pragma pack(pop)

struct IDsVariable;
typedef IDsVariable *pDsVariable;

#pragma pack(push, 1)
struct IDsVariable
{
	System::SmallString<32>  VarName;
	DsValue Value;
} ;
#pragma pack(pop)

typedef DsFilterData __fastcall (*DsParserFunction)(System::TObject* Sender, pDsFilterDataArray pInput);

struct DsFunction;
typedef DsFunction *pDsFunction;

#pragma pack(push, 1)
struct DsFunction
{
	System::SmallString<64>  Name;
	Word Params;
	DsParserFunction Addr;
} ;
#pragma pack(pop)

typedef void __fastcall (*DsRegisterFunctionProc)(const AnsiString Name, int ParamCount, DsParserFunction Func);

typedef void __stdcall (*DsRegisterExternalFunctionsProc)(DsRegisterFunctionProc Proc);

class DELPHICLASS TDsDbFilter;
class PASCALIMPLEMENTATION TDsDbFilter : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Db::TDataSet* FOwner;
	DsFilterData *pExprBuf;
	DsFilterData *pExprPos;
	int FExprLen;
	AnsiString FExpression;
	Classes::TList* FVarList;
	Classes::TList* FFuncList;
	bool FPrepared;
	Db::TFilterOptions FFilterOptions;
	void __fastcall SetExpression(const AnsiString Value);
	
protected:
	DsFilterType __fastcall GetExprType(AnsiString Value);
	DsFilterData __fastcall Term();
	DsFilterData __fastcall SubTerm();
	DsFilterData __fastcall Calculate();
	DsFilterData __fastcall Operation(const DsFilterData &Val1, const DsFilterData &Val2, DsFilterType OperType);
	Db::TField* __fastcall Find_Field(AnsiString FldName);
	pDsVariable __fastcall Find_Variable(AnsiString VarName);
	pDsFunction __fastcall Find_Function(AnsiString FuncName);
	void __fastcall UnPrepare(void);
	void __fastcall Clear_DsVariables(void);
	void __fastcall Clear_Functions(void);
	
public:
	__fastcall TDsDbFilter(Db::TDataSet* AOwner);
	__fastcall virtual ~TDsDbFilter(void);
	DsFilterData __fastcall Evaluate();
	pDsValue __fastcall Add_DsVariable(AnsiString VarName, const DsValue &VarValue);
	bool __fastcall Del_DsVariable(AnsiString VarName);
	void __fastcall Prepare(void);
	void __fastcall RegisterFunction(const AnsiString Name, int ParamCount, DsParserFunction Func);
	__property AnsiString Expression = {read=FExpression, write=SetExpression};
	__property bool Prepared = {read=FPrepared, nodefault};
	__property Db::TFilterOptions Options = {read=FFilterOptions, write=FFilterOptions, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char *IExprTypeArray[29];
static const Shortint RES_INVALIDDATETIME = 0xffffffff;

}	/* namespace Dsdbfilter */
using namespace Dsdbfilter;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDbFilter
