// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBGridEhImpExp.pas' rev: 6.00

#ifndef DBGridEhImpExpHPP
#define DBGridEhImpExpHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Clipbrd.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dbgridehimpexp
{
//-- type declarations -------------------------------------------------------
typedef DynamicArray<System::Currency >  TFooterValues;

class DELPHICLASS TDBGridEhExport;
class PASCALIMPLEMENTATION TDBGridEhExport : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Dbgrideh::TColCellParamsEh* FColCellParamsEh;
	Dbgrideh::TCustomDBGridEh* FDBGridEh;
	Dbgrideh::TColumnsEhList* FExpCols;
	Classes::TStream* FStream;
	AnsiString __fastcall GetFooterValue(int Row, int Col);
	void __fastcall CalcFooterValues(void);
	
protected:
	DynamicArray<System::Currency >  FooterValues;
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooter(Dbgrideh::TColumnsEhList* ColumnsList, int FooterNo);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
	__property Classes::TStream* Stream = {read=FStream, write=FStream};
	__property Dbgrideh::TColumnsEhList* ExpCols = {read=FExpCols, write=FExpCols};
	
public:
	__fastcall virtual TDBGridEhExport(void);
	__fastcall virtual ~TDBGridEhExport(void);
	virtual void __fastcall ExportToStream(Classes::TStream* AStream, bool IsExportAll);
	virtual void __fastcall ExportToFile(AnsiString FileName, bool IsExportAll);
	__property Dbgrideh::TCustomDBGridEh* DBGridEh = {read=FDBGridEh, write=FDBGridEh};
};


typedef TMetaClass*TDBGridEhExportClass;

class DELPHICLASS TDBGridEhExportAsText;
class PASCALIMPLEMENTATION TDBGridEhExportAsText : public TDBGridEhExport 
{
	typedef TDBGridEhExport inherited;
	
private:
	bool FirstRec;
	bool FirstCell;
	
protected:
	virtual void __fastcall CheckFirstRec(void);
	virtual void __fastcall CheckFirstCell(void);
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteFooter(Dbgrideh::TColumnsEhList* ColumnsList, int FooterNo);
	virtual void __fastcall WriteRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
	
public:
	virtual void __fastcall ExportToStream(Classes::TStream* Stream, bool IsExportAll);
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Create */ inline __fastcall virtual TDBGridEhExportAsText(void) : TDBGridEhExport() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsText(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhExportAsCSV;
class PASCALIMPLEMENTATION TDBGridEhExportAsCSV : public TDBGridEhExportAsText 
{
	typedef TDBGridEhExportAsText inherited;
	
private:
	char FSeparator;
	
protected:
	virtual void __fastcall CheckFirstCell(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
	
public:
	__fastcall virtual TDBGridEhExportAsCSV(void);
	__property char Separator = {read=FSeparator, write=FSeparator, nodefault};
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsCSV(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhExportAsHTML;
class PASCALIMPLEMENTATION TDBGridEhExportAsHTML : public TDBGridEhExport 
{
	typedef TDBGridEhExport inherited;
	
private:
	AnsiString __fastcall GetAlignment(Classes::TAlignment Alignment);
	AnsiString __fastcall GetColor(Graphics::TColor Color);
	void __fastcall PutText(Graphics::TFont* Font, AnsiString Text);
	void __fastcall Put(AnsiString Text);
	void __fastcall PutL(AnsiString Text);
	
protected:
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooter(Dbgrideh::TColumnsEhList* ColumnsList, int FooterNo);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Create */ inline __fastcall virtual TDBGridEhExportAsHTML(void) : TDBGridEhExport() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsHTML(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhExportAsRTF;
class PASCALIMPLEMENTATION TDBGridEhExportAsRTF : public TDBGridEhExport 
{
	typedef TDBGridEhExport inherited;
	
private:
	Classes::TMemoryStream* FCacheStream;
	Classes::TStrings* ColorTblList;
	Classes::TStrings* FontTblList;
	int __fastcall GetFontIndex(AnsiString FontName);
	int __fastcall GetColorIndex(Graphics::TColor Color);
	AnsiString __fastcall GetAlignment(Classes::TAlignment Alignment);
	Graphics::TColor __fastcall GetDataCellColor(Dbgrideh::TColumnsEhList* ColumnsList, int ColIndex);
	Graphics::TColor __fastcall GetFooterCellColor(Dbgrideh::TColumnsEhList* ColumnsList, int ColIndex, int FooterNo);
	void __fastcall PutText(Graphics::TFont* Font, AnsiString Text, Graphics::TColor Background);
	void __fastcall Put(AnsiString Text);
	void __fastcall PutL(AnsiString Text);
	
protected:
	void __fastcall WriteCellBorder(bool LeftBorder, bool TopBorder, bool BottomBorder, bool RightBorder);
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooter(Dbgrideh::TColumnsEhList* ColumnsList, int FooterNo);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
	
public:
	virtual void __fastcall ExportToStream(Classes::TStream* AStream, bool IsExportAll);
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Create */ inline __fastcall virtual TDBGridEhExportAsRTF(void) : TDBGridEhExport() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsRTF(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhExportAsXLS;
class PASCALIMPLEMENTATION TDBGridEhExportAsXLS : public TDBGridEhExport 
{
	typedef TDBGridEhExport inherited;
	
private:
	Word FCol;
	Word FRow;
	void __fastcall WriteBlankCell(void);
	void __fastcall WriteIntegerCell(const int AValue);
	void __fastcall WriteFloatCell(const double AValue);
	void __fastcall WriteStringCell(const AnsiString AValue);
	void __fastcall IncColRow(void);
	
protected:
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteTitle(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
	virtual void __fastcall WriteFooterCell(int DataCol, int Row, Dbgrideh::TColumnEh* Column, Graphics::TFont* AFont, Graphics::TColor Background, Classes::TAlignment Alignment, AnsiString Text);
	
public:
	virtual void __fastcall ExportToStream(Classes::TStream* AStream, bool IsExportAll);
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Create */ inline __fastcall virtual TDBGridEhExportAsXLS(void) : TDBGridEhExport() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsXLS(void) { }
	#pragma option pop
	
};


#pragma pack(push, 1)
struct TVCLDBIF_BOF
{
	char Signatura[7];
	Byte Version;
	int ColCount;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TVCLDBIF_INTEGER32
{
	Byte AType;
	int Value;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TVCLDBIF_FLOAT64
{
	Byte AType;
	double Value;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TVCLDBIF_STRING
{
	Byte AType;
	int Size;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TVCLDBIF_BINARY_DATA
{
	Byte AType;
	int Size;
} ;
#pragma pack(pop)

class DELPHICLASS TDBGridEhExportAsVCLDBIF;
class PASCALIMPLEMENTATION TDBGridEhExportAsVCLDBIF : public TDBGridEhExport 
{
	typedef TDBGridEhExport inherited;
	
private:
	void __fastcall WriteUnassigned(void);
	void __fastcall WriteNull(void);
	void __fastcall WriteInteger(int AValue);
	void __fastcall WriteFloat(double AValue);
	void __fastcall WriteString(AnsiString AValue);
	void __fastcall WriteBinaryData(AnsiString AValue);
	
protected:
	virtual void __fastcall WritePrefix(void);
	virtual void __fastcall WriteSuffix(void);
	virtual void __fastcall WriteDataCell(Dbgrideh::TColumnEh* Column, Dbgrideh::TColCellParamsEh* FColCellParamsEh);
public:
	#pragma option push -w-inl
	/* TDBGridEhExport.Create */ inline __fastcall virtual TDBGridEhExportAsVCLDBIF(void) : TDBGridEhExport() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDBGridEhExport.Destroy */ inline __fastcall virtual ~TDBGridEhExportAsVCLDBIF(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhImport;
class PASCALIMPLEMENTATION TDBGridEhImport : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Dbgrideh::TCustomDBGridEh* FDBGridEh;
	Classes::TStream* FStream;
	Dbgrideh::TColumnsEhList* FImpCols;
	
protected:
	bool Eos;
	virtual void __fastcall ReadPrefix(void);
	virtual void __fastcall ReadSuffix(void);
	virtual void __fastcall ReadRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall ReadDataCell(Dbgrideh::TColumnEh* Column);
	__property Classes::TStream* Stream = {read=FStream, write=FStream};
	__property Dbgrideh::TColumnsEhList* ImpCols = {read=FImpCols, write=FImpCols};
	
public:
	__fastcall virtual TDBGridEhImport(void);
	virtual void __fastcall ImportFromStream(Classes::TStream* AStream, bool IsImportAll);
	virtual void __fastcall ImportFromFile(AnsiString FileName, bool IsImportAll);
	__property Dbgrideh::TCustomDBGridEh* DBGridEh = {read=FDBGridEh, write=FDBGridEh};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDBGridEhImport(void) { }
	#pragma option pop
	
};


typedef TMetaClass*TDBGridEhImportClass;

#pragma option push -b-
enum TImportTextSreamState { itssChar, itssTab, itssNewLine, itssEof };
#pragma option pop

class DELPHICLASS TDBGridEhImportAsText;
class PASCALIMPLEMENTATION TDBGridEhImportAsText : public TDBGridEhImport 
{
	typedef TDBGridEhImport inherited;
	
private:
	char FLastChar;
	TImportTextSreamState FLastState;
	AnsiString FLastString;
	bool FIgnoreAll;
	bool __fastcall GetChar(char &ch);
	TImportTextSreamState __fastcall CheckState(void);
	TImportTextSreamState __fastcall GetString(AnsiString &Value);
	
protected:
	virtual void __fastcall ReadPrefix(void);
	virtual void __fastcall ReadRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall ReadDataCell(Dbgrideh::TColumnEh* Column);
	
public:
	virtual void __fastcall ImportFromStream(Classes::TStream* AStream, bool IsImportAll);
public:
	#pragma option push -w-inl
	/* TDBGridEhImport.Create */ inline __fastcall virtual TDBGridEhImportAsText(void) : TDBGridEhImport() { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDBGridEhImportAsText(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhImportAsVCLDBIF;
class PASCALIMPLEMENTATION TDBGridEhImportAsVCLDBIF : public TDBGridEhImport 
{
	typedef TDBGridEhImport inherited;
	
private:
	#pragma pack(push, 1)
	TVCLDBIF_BOF Prefix;
	#pragma pack(pop)
	
	bool FIgnoreAll;
	Variant LastValue;
	Classes::TStringList* FieldNames;
	bool UseFieldNames;
	void __fastcall ReadValue(void);
	
protected:
	virtual void __fastcall ReadPrefix(void);
	virtual void __fastcall ReadRecord(Dbgrideh::TColumnsEhList* ColumnsList);
	virtual void __fastcall ReadDataCell(Dbgrideh::TColumnEh* Column);
	
public:
	virtual void __fastcall ImportFromStream(Classes::TStream* AStream, bool IsImportAll);
public:
	#pragma option push -w-inl
	/* TDBGridEhImport.Create */ inline __fastcall virtual TDBGridEhImportAsVCLDBIF(void) : TDBGridEhImport() { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDBGridEhImportAsVCLDBIF(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint TVCLDBIF_TYPE_EOF = 0x0;
static const Shortint TVCLDBIF_TYPE_UNASSIGNED = 0x1;
static const Shortint TVCLDBIF_TYPE_NULL = 0x2;
static const Shortint TVCLDBIF_TYPE_INTEGER32 = 0x3;
static const Shortint TVCLDBIF_TYPE_FLOAT64 = 0x4;
static const Shortint TVCLDBIF_TYPE_STRING = 0x5;
static const Shortint TVCLDBIF_TYPE_BINARY_DATA = 0x6;
extern PACKAGE Word CF_VCLDBIF;
extern PACKAGE bool ExtendedVCLDBIFImpExpRowSelect;
extern PACKAGE char DBGridEhImpExpCsvSeparator;
extern PACKAGE void __fastcall WriteDBGridEhToExportStream(TMetaClass* ExportClass, Dbgrideh::TCustomDBGridEh* DBGridEh, Classes::TStream* Stream, bool IsSaveAll);
extern PACKAGE void __fastcall SaveDBGridEhToExportFile(TMetaClass* ExportClass, Dbgrideh::TCustomDBGridEh* DBGridEh, const AnsiString FileName, bool IsSaveAll);
extern PACKAGE void __fastcall LoadDBGridEhFromImportFile(TMetaClass* ImportClass, Dbgrideh::TCustomDBGridEh* DBGridEh, const AnsiString FileName, bool IsLoadToAll);
extern PACKAGE void __fastcall ReadDBGridEhFromImportStream(TMetaClass* ImportClass, Dbgrideh::TCustomDBGridEh* DBGridEh, Classes::TStream* Stream, bool IsLoadToAll);
extern PACKAGE void __fastcall DBGridEh_DoCutAction(Dbgrideh::TCustomDBGridEh* DBGridEh, bool ForWholeGrid);
extern PACKAGE void __fastcall DBGridEh_DoDeleteAction(Dbgrideh::TCustomDBGridEh* DBGridEh, bool ForWholeGrid);
extern PACKAGE void __fastcall DBGridEh_DoCopyAction(Dbgrideh::TCustomDBGridEh* DBGridEh, bool ForWholeGrid);
extern PACKAGE void __fastcall DBGridEh_DoPasteAction(Dbgrideh::TCustomDBGridEh* DBGridEh, bool ForWholeGrid);

}	/* namespace Dbgridehimpexp */
using namespace Dbgridehimpexp;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DBGridEhImpExp
