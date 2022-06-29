// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DbUtilsEh.pas' rev: 5.00

#ifndef DbUtilsEhHPP
#define DbUtilsEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Windows.hpp>	// Pascal unit
#include <TypInfo.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dbutilseh
{
//-- type declarations -------------------------------------------------------
typedef AnsiString __fastcall (*TDateValueToSQLStringProcEh)(Db::TDataSet* DataSet, const Variant &Value
	);

typedef AnsiString DbUtilsEh__1[16];

typedef AnsiString DbUtilsEh__2[16];

typedef AnsiString __fastcall (*TOneExpressionFilterStringProcEh)(Dbgrideh::TSTFilterOperatorEh O, const 
	Variant &v, AnsiString FieldName, Db::TDataSet* DataSet, TDateValueToSQLStringProcEh DateValueToSQLStringProc
	, bool SupportsLike);

typedef TMetaClass*TDataSetClass;

class DELPHICLASS TDatasetFeaturesEh;
class PASCALIMPLEMENTATION TDatasetFeaturesEh : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TMetaClass*FDataSetClass;
	
public:
	__fastcall virtual TDatasetFeaturesEh(void);
	virtual void __fastcall ApplySorting(System::TObject* Sender, Db::TDataSet* DataSet, bool IsReopen)
		;
	virtual void __fastcall ApplyFilter(System::TObject* Sender, Db::TDataSet* DataSet, bool IsReopen);
		
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDatasetFeaturesEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TSQLDatasetFeaturesEh;
class PASCALIMPLEMENTATION TSQLDatasetFeaturesEh : public TDatasetFeaturesEh 
{
	typedef TDatasetFeaturesEh inherited;
	
private:
	bool FSortUsingFieldName;
	AnsiString FSQLPropName;
	TDateValueToSQLStringProcEh FDateValueToSQLString;
	bool FSupportsLocalLike;
	
public:
	__fastcall virtual TSQLDatasetFeaturesEh(void);
	virtual void __fastcall ApplySorting(System::TObject* Sender, Db::TDataSet* DataSet, bool IsReopen)
		;
	__property bool SortUsingFieldName = {read=FSortUsingFieldName, write=FSortUsingFieldName, nodefault
		};
	virtual void __fastcall ApplyFilter(System::TObject* Sender, Db::TDataSet* DataSet, bool IsReopen);
		
	__property AnsiString SQLPropName = {read=FSQLPropName, write=FSQLPropName};
	__property TDateValueToSQLStringProcEh DateValueToSQLString = {read=FDateValueToSQLString, write=FDateValueToSQLString
		};
	__property bool SupportsLocalLike = {read=FSupportsLocalLike, write=FSupportsLocalLike, nodefault};
		
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLDatasetFeaturesEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCommandTextDatasetFeaturesEh;
class PASCALIMPLEMENTATION TCommandTextDatasetFeaturesEh : public TSQLDatasetFeaturesEh 
{
	typedef TSQLDatasetFeaturesEh inherited;
	
public:
	__fastcall virtual TCommandTextDatasetFeaturesEh(void);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TCommandTextDatasetFeaturesEh(void) { }
	#pragma option pop
	
};


typedef TMetaClass*TDatasetFeaturesEhClass;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE AnsiString STFilterOperatorsStrMapEh[16];
extern PACKAGE Dbgrideh::TSTOperandTypeEh STFldTypeMapEh[36];
extern PACKAGE AnsiString STFilterOperatorsSQLStrMapEh[16];
extern PACKAGE AnsiString SQLFilterMarker;
extern PACKAGE System::ResourceString _SNotOperatorEh;
#define Dbutilseh_SNotOperatorEh System::LoadResourceString(&Dbutilseh::_SNotOperatorEh)
extern PACKAGE System::ResourceString _SAndOperatorEh;
#define Dbutilseh_SAndOperatorEh System::LoadResourceString(&Dbutilseh::_SAndOperatorEh)
extern PACKAGE System::ResourceString _SOrOperatorEh;
#define Dbutilseh_SOrOperatorEh System::LoadResourceString(&Dbutilseh::_SOrOperatorEh)
extern PACKAGE System::ResourceString _SLikePredicatEh;
#define Dbutilseh_SLikePredicatEh System::LoadResourceString(&Dbutilseh::_SLikePredicatEh)
extern PACKAGE System::ResourceString _SInPredicatEh;
#define Dbutilseh_SInPredicatEh System::LoadResourceString(&Dbutilseh::_SInPredicatEh)
extern PACKAGE System::ResourceString _SNullConstEh;
#define Dbutilseh_SNullConstEh System::LoadResourceString(&Dbutilseh::_SNullConstEh)
extern PACKAGE System::ResourceString _SQuoteIsAbsentEh;
#define Dbutilseh_SQuoteIsAbsentEh System::LoadResourceString(&Dbutilseh::_SQuoteIsAbsentEh)
extern PACKAGE System::ResourceString _SLeftBracketExpectedEh;
#define Dbutilseh_SLeftBracketExpectedEh System::LoadResourceString(&Dbutilseh::_SLeftBracketExpectedEh)
	
extern PACKAGE System::ResourceString _SRightBracketExpectedEh;
#define Dbutilseh_SRightBracketExpectedEh System::LoadResourceString(&Dbutilseh::_SRightBracketExpectedEh)
	
extern PACKAGE System::ResourceString _SErrorInExpressionEh;
#define Dbutilseh_SErrorInExpressionEh System::LoadResourceString(&Dbutilseh::_SErrorInExpressionEh)
	
extern PACKAGE System::ResourceString _SUnexpectedExpressionBeforeNullEh;
#define Dbutilseh_SUnexpectedExpressionBeforeNullEh System::LoadResourceString(&Dbutilseh::_SUnexpectedExpressionBeforeNullEh)
	
extern PACKAGE System::ResourceString _SUnexpectedExpressionAfterOperatorEh;
#define Dbutilseh_SUnexpectedExpressionAfterOperatorEh System::LoadResourceString(&Dbutilseh::_SUnexpectedExpressionAfterOperatorEh)
	
extern PACKAGE System::ResourceString _SIncorrectExpressionEh;
#define Dbutilseh_SIncorrectExpressionEh System::LoadResourceString(&Dbutilseh::_SIncorrectExpressionEh)
	
extern PACKAGE System::ResourceString _SUnexpectedANDorOREh;
#define Dbutilseh_SUnexpectedANDorOREh System::LoadResourceString(&Dbutilseh::_SUnexpectedANDorOREh)
	
extern PACKAGE void __fastcall ClearSTFilterExpression(Dbgrideh::TSTFilterExpressionEh &FExpression)
	;
extern PACKAGE void __fastcall InitSTFilterOperatorsStrMap(void);
extern PACKAGE bool __fastcall ParseSTFilterExpressionEh(AnsiString Exp, Dbgrideh::TSTFilterExpressionEh 
	&FExpression);
extern PACKAGE AnsiString __fastcall GetExpressionAsFilterString(Dbgrideh::TCustomDBGridEh* AGrid, TOneExpressionFilterStringProcEh 
	OneExpressionProc, TDateValueToSQLStringProcEh DateValueToSQLStringProc, bool UseFieldOrigin, bool 
	SupportsLocalLike);
extern PACKAGE AnsiString __fastcall GetOneExpressionAsLocalFilterString(Dbgrideh::TSTFilterOperatorEh 
	O, const Variant &v, AnsiString FieldName, Db::TDataSet* DataSet, TDateValueToSQLStringProcEh DateValueToSQLStringProc
	, bool SupportsLike);
extern PACKAGE AnsiString __fastcall GetOneExpressionAsSQLWhereString(Dbgrideh::TSTFilterOperatorEh 
	O, const Variant &v, AnsiString FieldName, Db::TDataSet* DataSet, TDateValueToSQLStringProcEh DateValueToSQLStringProc
	, bool SupportsLike);
extern PACKAGE AnsiString __fastcall DateValueToDataBaseSQLString(AnsiString DataBaseName, const Variant 
	&v);
extern PACKAGE void __fastcall ApplyFilterSQLBasedDataSet(Dbgrideh::TCustomDBGridEh* Grid, TDateValueToSQLStringProcEh 
	DateValueToSQLString, bool IsReopen, AnsiString SQLPropName);
extern PACKAGE bool __fastcall IsSQLBasedDataSet(Db::TDataSet* DataSet, Classes::TStrings* &SQL);
extern PACKAGE bool __fastcall IsDataSetHaveSQLLikeProp(Db::TDataSet* DataSet, AnsiString SQLPropName
	, WideString &SQLPropValue);
extern PACKAGE void __fastcall ApplySortingForSQLBasedDataSet(Dbgrideh::TCustomDBGridEh* Grid, Db::TDataSet* 
	DataSet, bool UseFieldName, bool IsReopen, AnsiString SQLPropName);
extern PACKAGE void __fastcall RegisterDatasetFeaturesEh(TMetaClass* DatasetFeaturesClass, TMetaClass* 
	DataSetClass);
extern PACKAGE void __fastcall UnregisterDatasetFeaturesEh(TMetaClass* DataSetClass);
extern PACKAGE TDatasetFeaturesEh* __fastcall GetDatasetFeaturesForDataSetClass(TMetaClass* DataSetClass
	);
extern PACKAGE TDatasetFeaturesEh* __fastcall GetDatasetFeaturesForDataSet(Db::TDataSet* DataSet);

}	/* namespace Dbutilseh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dbutilseh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbUtilsEh
