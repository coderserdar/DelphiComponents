// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsMemTable.pas' rev: 6.00

#ifndef DsMemTableHPP
#define DsMemTableHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbFilter.hpp>	// Pascal unit
#include <DsConsts.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsmemtable
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TMemBlobData;

typedef AnsiString TMemBlobArray[1];

typedef AnsiString *PMemBlobArray;

#pragma option push -b-
enum TLoadMode { lmCopy, lmAppend };
#pragma option pop

class DELPHICLASS TMemoryRecord;
typedef int __fastcall (__closure *TCompareRecords)(TMemoryRecord* Item1, TMemoryRecord* Item2);

class DELPHICLASS TDsCustomMemTable;
class PASCALIMPLEMENTATION TDsCustomMemTable : public Db::TDataSet 
{
	typedef Db::TDataSet inherited;
	
private:
	int FRecordPos;
	int FRecordSize;
	int FBookmarkOfs;
	int FBlobOfs;
	int FRecBufSize;
	Word *FOffsets;
	int FLastID;
	int FAutoInc;
	bool FActive;
	Classes::TList* FIndexList;
	bool FCaseInsensitiveSort;
	bool FDescendingSort;
	Dsdbfilter::TDsDbFilter* FFilterParser;
	HIDESBASE TMemoryRecord* __fastcall AddRecord(void);
	HIDESBASE TMemoryRecord* __fastcall InsertRecord(int Index);
	TMemoryRecord* __fastcall FindRecordID(int ID);
	void __fastcall CreateIndexList(const AnsiString FieldNames);
	void __fastcall FreeIndexList(void);
	void __fastcall QuickSort(int L, int R, TCompareRecords Compare);
	void __fastcall Sort(void);
	int __fastcall CalcRecordSize(void);
	void * __fastcall FindFieldData(void * Buffer, Db::TField* Field);
	TMemoryRecord* __fastcall GetMemoryRecord(int Index);
	int __fastcall GetCapacity(void);
	bool __fastcall RecordFilter(void);
	void __fastcall SetCapacity(int Value);
	void __fastcall ClearRecords(void);
	void __fastcall InitBufferPointers(bool GetProps);
	
protected:
	Classes::TList* FRecords;
	void __fastcall AssignMemoryRecord(TMemoryRecord* Rec, char * Buffer);
	virtual bool __fastcall GetActiveRecBuf(char * &RecBuf);
	HIDESBASE void __fastcall InitFieldDefsFromFields(void);
	void __fastcall RecordToBuffer(TMemoryRecord* Rec, char * Buffer);
	virtual void __fastcall SetMemoryRecordData(char * Buffer, int Pos);
	virtual void __fastcall SetAutoIncFields(char * Buffer);
	virtual int __fastcall CompareRecords(TMemoryRecord* Item1, TMemoryRecord* Item2);
	AnsiString __fastcall GetBlobData(Db::TField* Field, char * Buffer);
	void __fastcall SetBlobData(Db::TField* Field, char * Buffer, AnsiString Value);
	virtual char * __fastcall AllocRecordBuffer(void);
	virtual void __fastcall FreeRecordBuffer(char * &Buffer);
	virtual void __fastcall InternalInitRecord(char * Buffer);
	virtual void __fastcall ClearCalcFields(char * Buffer);
	virtual Db::TGetResult __fastcall GetRecord(char * Buffer, Db::TGetMode GetMode, bool DoCheck);
	virtual Word __fastcall GetRecordSize(void);
	virtual void __fastcall SetFiltered(bool Value);
	virtual void __fastcall SetFilterOptions(Db::TFilterOptions Value);
	virtual void __fastcall SetFilterText(const AnsiString Value);
	void __fastcall SetFilter(AnsiString FilterText, Db::TFilterOptions FilterOptions, bool IsActive);
	void __fastcall ActivateFilters(void);
	void __fastcall DeactivateFilters(void);
	virtual void __fastcall SetOnFilterRecord(const Db::TFilterRecordEvent Value);
	virtual void __fastcall SetFieldData(Db::TField* Field, void * Buffer)/* overload */;
	virtual void __fastcall CloseBlob(Db::TField* Field);
	virtual void __fastcall GetBookmarkData(char * Buffer, void * Data);
	virtual Db::TBookmarkFlag __fastcall GetBookmarkFlag(char * Buffer);
	virtual void __fastcall InternalGotoBookmark(void * Bookmark);
	virtual void __fastcall InternalSetToRecord(char * Buffer);
	virtual void __fastcall SetBookmarkFlag(char * Buffer, Db::TBookmarkFlag Value);
	virtual void __fastcall SetBookmarkData(char * Buffer, void * Data);
	virtual bool __fastcall GetIsIndexField(Db::TField* Field);
	virtual void __fastcall InternalFirst(void);
	virtual void __fastcall InternalLast(void);
	virtual void __fastcall InitRecord(char * Buffer);
	virtual void __fastcall InternalAddRecord(void * Buffer, bool Append);
	virtual void __fastcall InternalDelete(void);
	virtual void __fastcall InternalPost(void);
	virtual void __fastcall InternalClose(void);
	virtual void __fastcall InternalHandleException(void);
	virtual void __fastcall InternalInitFieldDefs(void);
	virtual void __fastcall InternalOpen(void);
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual bool __fastcall IsCursorOpen(void);
	virtual int __fastcall GetRecordCount(void);
	virtual int __fastcall GetRecNo(void);
	virtual void __fastcall SetRecNo(int Value);
	__property TMemoryRecord* Records[int Index] = {read=GetMemoryRecord};
	
public:
	__fastcall virtual TDsCustomMemTable(Classes::TComponent* AOwner);
	__fastcall virtual ~TDsCustomMemTable(void);
	virtual bool __fastcall BookmarkValid(void * Bookmark);
	virtual int __fastcall CompareBookmarks(void * Bookmark1, void * Bookmark2);
	virtual Classes::TStream* __fastcall CreateBlobStream(Db::TField* Field, Db::TBlobStreamMode Mode);
	virtual bool __fastcall GetFieldData(Db::TField* Field, void * Buffer)/* overload */;
	virtual bool __fastcall GetCurrentRecord(char * Buffer);
	virtual bool __fastcall IsSequenced(void);
	virtual bool __fastcall Locate(const AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
	void __fastcall SortOnFields(const AnsiString FieldNames, bool CaseInsensitive = true, bool Descending = false);
	void __fastcall EmptyTable(void);
	void __fastcall CopyStructure(Db::TDataSet* Source);
	int __fastcall LoadFromDataSet(Db::TDataSet* Source, int RecordCount, TLoadMode Mode);
	int __fastcall SaveToDataSet(Db::TDataSet* Dest, int RecordCount);
	__property int Capacity = {read=GetCapacity, write=SetCapacity, default=0};
	
/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetFieldData(Db::TField* Field, void * Buffer, bool NativeFormat){ TDataSet::SetFieldData(Field, Buffer, NativeFormat); }
	
public:
	inline bool __fastcall  GetFieldData(int FieldNo, void * Buffer){ return TDataSet::GetFieldData(FieldNo, Buffer); }
	inline bool __fastcall  GetFieldData(Db::TField* Field, void * Buffer, bool NativeFormat){ return TDataSet::GetFieldData(Field, Buffer, NativeFormat); }
	
};


class DELPHICLASS TDsMemTable;
class PASCALIMPLEMENTATION TDsMemTable : public TDsCustomMemTable 
{
	typedef TDsCustomMemTable inherited;
	
__published:
	__property Active  = {default=0};
	__property AutoCalcFields  = {default=1};
	__property Capacity  = {default=0};
	__property Filter ;
	__property Filtered  = {default=0};
	__property FilterOptions  = {default=0};
	__property FieldDefs ;
	__property ObjectView  = {default=0};
	__property BeforeOpen ;
	__property AfterOpen ;
	__property BeforeClose ;
	__property AfterClose ;
	__property BeforeInsert ;
	__property AfterInsert ;
	__property BeforeEdit ;
	__property AfterEdit ;
	__property BeforePost ;
	__property AfterPost ;
	__property BeforeCancel ;
	__property AfterCancel ;
	__property BeforeDelete ;
	__property AfterDelete ;
	__property BeforeScroll ;
	__property AfterScroll ;
	__property OnCalcFields ;
	__property OnDeleteError ;
	__property OnEditError ;
	__property OnFilterRecord ;
	__property OnNewRecord ;
	__property OnPostError ;
public:
	#pragma option push -w-inl
	/* TDsCustomMemTable.Create */ inline __fastcall virtual TDsMemTable(Classes::TComponent* AOwner) : TDsCustomMemTable(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDsCustomMemTable.Destroy */ inline __fastcall virtual ~TDsMemTable(void) { }
	#pragma option pop
	
};


class DELPHICLASS TMemBlobStream;
class PASCALIMPLEMENTATION TMemBlobStream : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	Db::TBlobField* FField;
	TDsCustomMemTable* FDataSet;
	char *FBuffer;
	Db::TBlobStreamMode FMode;
	bool FOpened;
	bool FModified;
	int FPosition;
	bool FCached;
	int __fastcall GetBlobSize(void);
	AnsiString __fastcall GetBlobFromRecord(Db::TField* Field);
	
public:
	__fastcall TMemBlobStream(Db::TBlobField* Field, Db::TBlobStreamMode Mode);
	__fastcall virtual ~TMemBlobStream(void);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Write(const void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
	void __fastcall Truncate(void);
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


class PASCALIMPLEMENTATION TMemoryRecord : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	TDsCustomMemTable* FMemoryData;
	int FID;
	void *FData;
	void *FBlobs;
	int __fastcall GetIndex(void);
	void __fastcall SetMemoryData(TDsCustomMemTable* Value, bool UpdateParent);
	
protected:
	virtual void __fastcall SetIndex(int Value);
	
public:
	__fastcall virtual TMemoryRecord(TDsCustomMemTable* MemoryData);
	__fastcall virtual TMemoryRecord(TDsCustomMemTable* MemoryData, bool UpdateParent);
	__fastcall virtual ~TMemoryRecord(void);
	__property TDsCustomMemTable* MemoryData = {read=FMemoryData};
	__property int ID = {read=FID, write=FID, nodefault};
	__property int Index = {read=GetIndex, write=SetIndex, nodefault};
	__property void * Data = {read=FData};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dsmemtable */
using namespace Dsmemtable;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsMemTable
