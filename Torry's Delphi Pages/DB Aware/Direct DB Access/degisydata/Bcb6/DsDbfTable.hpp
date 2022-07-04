// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDbfTable.pas' rev: 6.00

#ifndef DsDbfTableHPP
#define DsDbfTableHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDatabase.hpp>	// Pascal unit
#include <DsDbUtils.hpp>	// Pascal unit
#include <DsDbfApi.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
#include <DsConsts.hpp>	// Pascal unit
#include <DsCP.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsdbftable
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDsDbfTable;
class PASCALIMPLEMENTATION TDsDbfTable : public Db::TDataSet 
{
	typedef Db::TDataSet inherited;
	
private:
	Dsdbfapi::TDbfHandle* FHandle;
	Dsdbapi::TDsDbHandle* FDbHandle;
	bool FExclusive;
	bool FReadOnly;
	Word FRecordSize;
	Word FBookmarkOfs;
	Word FRecInfoOfs;
	Word FBlobCacheOfs;
	Word FRecBufSize;
	AnsiString FTableName;
	Dsdatabase::TDsDatabase* FDatabase;
	bool FCacheBlobs;
	int FBufferRecords;
	char *FFilterBuffer;
	Db::TDataSet* FParentDataSet;
	int FLastParentPos;
	Word FTableLevel;
	bool FShowDeleted;
	Dscp::CP_TYPE FCodePage;
	void __fastcall InitBufferPointers(bool GetProps);
	void __fastcall SetExclusive(bool Value);
	AnsiString __fastcall GetFileName();
	bool __fastcall GetActiveRecBuf(char * &RecBuf);
	void __fastcall SetTableName(const AnsiString Value);
	void __fastcall SetDatabase(const Dsdatabase::TDsDatabase* Value);
	AnsiString __fastcall GetBlobData(Db::TField* Field, char * Buffer);
	void __fastcall SetBlobData(Db::TField* Field, char * Buffer, AnsiString Value);
	void __fastcall SetBufferRecords(const int Value);
	void __fastcall SetReadOnly(const bool Value);
	void __fastcall SetTableLevel(const Word Value);
	void __fastcall SetShowDeleted(const bool Value);
	void __fastcall SetCodePage(const Dscp::CP_TYPE Value);
	void __fastcall CheckTableName(void);
	
protected:
	Dsdbfapi::TDbfHandle* __fastcall CreateHandle(void);
	void __fastcall DestroyHandle(void);
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall CloseCursor(void);
	virtual void __fastcall DataEvent(Db::TDataEvent Event, int Info);
	virtual void __fastcall CloseBlob(Db::TField* Field);
	virtual char * __fastcall AllocRecordBuffer(void);
	virtual void __fastcall FreeRecordBuffer(char * &Buffer);
	virtual void __fastcall ClearCalcFields(char * Buffer);
	virtual void __fastcall GetBookmarkData(char * Buffer, void * Data);
	virtual Db::TBookmarkFlag __fastcall GetBookmarkFlag(char * Buffer);
	virtual bool __fastcall FindRecord(bool Restart, bool GoForward);
	virtual Db::TGetResult __fastcall GetRecord(char * Buffer, Db::TGetMode GetMode, bool DoCheck);
	virtual Word __fastcall GetRecordSize(void);
	void __fastcall AddField(Dsdbfapi::TDbfField* Fld, int FieldID, Db::TFieldDefs* FieldDefs);
	virtual void __fastcall InitRecord(char * Buffer);
	virtual void __fastcall InternalAddRecord(void * Buffer, bool Append);
	virtual void __fastcall InternalClose(void);
	virtual void __fastcall InternalDelete(void);
	virtual void __fastcall InternalFirst(void);
	virtual void __fastcall InternalGotoBookmark(void * Bookmark);
	virtual void __fastcall InternalHandleException(void);
	virtual void __fastcall InitFieldDefs(void);
	virtual void __fastcall InternalInitFieldDefs(void);
	virtual void __fastcall InternalInitRecord(char * Buffer);
	virtual void __fastcall InternalInsert(void);
	virtual void __fastcall InternalLast(void);
	virtual void __fastcall InternalOpen(void);
	virtual void __fastcall InternalPost(void);
	virtual void __fastcall InternalSetToRecord(char * Buffer);
	virtual bool __fastcall GetCanModify(void);
	virtual bool __fastcall IsCursorOpen(void);
	virtual void __fastcall SetBookmarkFlag(char * Buffer, Db::TBookmarkFlag Value);
	virtual void __fastcall SetBookmarkData(char * Buffer, void * Data);
	virtual void __fastcall SetFieldData(Db::TField* Field, void * Buffer)/* overload */;
	virtual int __fastcall GetRecordCount(void);
	virtual int __fastcall GetRecNo(void);
	virtual void __fastcall SetRecNo(int Value);
	virtual void __fastcall SetFiltered(bool Value);
	virtual void __fastcall SetFilterOptions(Db::TFilterOptions Value);
	virtual void __fastcall SetFilterText(const AnsiString Value);
	void __fastcall SetFilter(AnsiString FilterText, Db::TFilterOptions FilterOptions, bool IsActive);
	void __fastcall ActivateFilters(void);
	void __fastcall DeactivateFilters(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall CheckDatabase(void);
	
public:
	__fastcall virtual TDsDbfTable(Classes::TComponent* AOwner);
	__fastcall virtual ~TDsDbfTable(void);
	void __fastcall CreateTable(void);
	void __fastcall BeginDataUpdate(void);
	void __fastcall EndDataUpdate(void);
	virtual Classes::TStream* __fastcall CreateBlobStream(Db::TField* Field, Db::TBlobStreamMode Mode);
	virtual int __fastcall GetBlobFieldData(int FieldNo, Db::TBlobByteData &Buffer);
	virtual bool __fastcall GetFieldData(Db::TField* Field, void * Buffer)/* overload */;
	virtual bool __fastcall GetFieldData(int FieldNo, void * Buffer)/* overload */;
	virtual bool __fastcall GetCurrentRecord(char * Buffer);
	virtual bool __fastcall Locate(const AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
	virtual Variant __fastcall Lookup(const AnsiString KeyFields, const Variant &KeyValues, const AnsiString ResultFields);
	virtual bool __fastcall IsSequenced(void);
	virtual bool __fastcall BookmarkValid(void * Bookmark);
	virtual int __fastcall CompareBookmarks(void * Bookmark1, void * Bookmark2);
	void __fastcall PackTable(void);
	void __fastcall EmptyTable(void);
	void __fastcall DeleteTable(void);
	__property Dsdbapi::TDsDbHandle* DbHandle = {read=FDbHandle};
	__property bool CacheBlobs = {read=FCacheBlobs, write=FCacheBlobs, default=1};
	__property Dsdbfapi::TDbfHandle* Handle = {read=FHandle};
	__property AnsiString FileName = {read=GetFileName};
	__property Word TableLevel = {read=FTableLevel, write=SetTableLevel, nodefault};
	
__published:
	__property Active  = {default=0};
	__property AutoCalcFields  = {default=1};
	__property int BufferRecords = {read=FBufferRecords, write=SetBufferRecords, default=10};
	__property Dscp::CP_TYPE CodePage = {read=FCodePage, write=SetCodePage, nodefault};
	__property Dsdatabase::TDsDatabase* Database = {read=FDatabase, write=SetDatabase};
	__property bool Exclusive = {read=FExclusive, write=SetExclusive, nodefault};
	__property FieldDefs ;
	__property Filter ;
	__property Filtered  = {default=0};
	__property FilterOptions  = {default=0};
	__property bool ShowDeleted = {read=FShowDeleted, write=SetShowDeleted, default=0};
	__property AnsiString TableName = {read=FTableName, write=SetTableName};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, nodefault};
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
	__property BeforeRefresh ;
	__property AfterRefresh ;
	__property OnCalcFields ;
	__property OnDeleteError ;
	__property OnEditError ;
	__property OnFilterRecord ;
	__property OnNewRecord ;
	__property OnPostError ;
	
/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetFieldData(Db::TField* Field, void * Buffer, bool NativeFormat){ TDataSet::SetFieldData(Field, Buffer, NativeFormat); }
	
public:
	inline bool __fastcall  GetFieldData(Db::TField* Field, void * Buffer, bool NativeFormat){ return TDataSet::GetFieldData(Field, Buffer, NativeFormat); }
	
};


class DELPHICLASS TBlobStream;
class PASCALIMPLEMENTATION TBlobStream : public Classes::TStream 
{
	typedef Classes::TStream inherited;
	
private:
	Db::TBlobField* FField;
	TDsDbfTable* FDataSet;
	char *FBuffer;
	Db::TBlobStreamMode FMode;
	int FFieldNo;
	bool FOpened;
	bool FModified;
	int FPosition;
	AnsiString FBlobData;
	bool FCached;
	int FCacheSize;
	int __fastcall GetBlobSize(void);
	
public:
	__fastcall TBlobStream(Db::TBlobField* Field, Db::TBlobStreamMode Mode);
	__fastcall virtual ~TBlobStream(void);
	virtual int __fastcall Read(void *Buffer, int Count);
	virtual int __fastcall Write(const void *Buffer, int Count);
	virtual int __fastcall Seek(int Offset, Word Origin)/* overload */;
	void __fastcall Truncate(void);
	
/* Hoisted overloads: */
	
public:
	inline __int64 __fastcall  Seek(const __int64 Offset, Classes::TSeekOrigin Origin){ return TStream::Seek(Offset, Origin); }
	
};


class DELPHICLASS ENoResultSet;
class PASCALIMPLEMENTATION ENoResultSet : public Db::EDatabaseError 
{
	typedef Db::EDatabaseError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ENoResultSet(const AnsiString Msg) : Db::EDatabaseError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ENoResultSet(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Db::EDatabaseError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ENoResultSet(int Ident)/* overload */ : Db::EDatabaseError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ENoResultSet(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Db::EDatabaseError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ENoResultSet(const AnsiString Msg, int AHelpContext) : Db::EDatabaseError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ENoResultSet(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Db::EDatabaseError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ENoResultSet(int Ident, int AHelpContext)/* overload */ : Db::EDatabaseError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ENoResultSet(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Db::EDatabaseError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ENoResultSet(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall DsGetTableNames(Dsdatabase::TDsDatabase* Db, Classes::TStrings* List);

}	/* namespace Dsdbftable */
using namespace Dsdbftable;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDbfTable
