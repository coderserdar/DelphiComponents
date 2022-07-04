// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDdaApi.pas' rev: 6.00

#ifndef DsDdaApiHPP
#define DsDdaApiHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbParser.hpp>	// Pascal unit
#include <DsDbUtils.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
#include <DsCrypt.hpp>	// Pascal unit
#include <DsCP.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <FMTBcd.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsddaapi
{
//-- type declarations -------------------------------------------------------
typedef int *pDdaBookmark;

typedef int DdaBookmark;

struct DdaFileInfo;
typedef DdaFileInfo *pDdaFileInfo;

#pragma pack(push, 1)
struct DdaFileInfo
{
	Word Version;
	Byte Build;
	bool Encrypted;
	int UnUsed1;
	int UnUsed2;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct DdaFileHeader
{
	DdaFileInfo FileInfo;
	System::TDateTime DateUpd;
	Word RecSize;
	Word HdrLen;
	Byte Keyed;
	int RecCount;
	int DelCount;
	Word FldCount;
	Word IdxCount;
	int CheckCode1;
	int CheckCode2;
	Byte UnUsed[103];
} ;
#pragma pack(pop)

struct RecHeader;
typedef RecHeader *pRecHeader;

#pragma pack(push, 1)
struct RecHeader
{
	Byte State;
	Byte Mode;
} ;
#pragma pack(pop)

struct FldHeader;
typedef FldHeader *pFldHeader;

#pragma pack(push, 1)
struct FldHeader
{
	Db::TFieldType FldType;
	Word FldSize;
	bool FldReq;
	int FldAutoInc;
	Word FldUnUsed;
} ;
#pragma pack(pop)

struct KeyHeader;
typedef KeyHeader *pKeyHeader;

#pragma pack(push, 1)
struct KeyHeader
{
	Byte NumComps;
	char KeyNams[16];
	Byte CompType;
	Byte CompLen;
} ;
#pragma pack(pop)

class DELPHICLASS TDdaField;
class DELPHICLASS TDdaHeader;
class PASCALIMPLEMENTATION TDdaHeader : public Dsdbapi::TDsCustomHeader 
{
	typedef Dsdbapi::TDsCustomHeader inherited;
	
private:
	#pragma pack(push, 1)
	DdaFileHeader FTableHeader;
	#pragma pack(pop)
	
	
protected:
	virtual void __fastcall SetArrCount(const Word Value);
	virtual void __fastcall SetFldCount(const Word Value);
	virtual void __fastcall SetIdxCount(const Word Value);
	virtual void __fastcall SetPicCount(const Word Value);
	virtual Dsdbapi::DsResult __fastcall Read(void);
	virtual Dsdbapi::DsResult __fastcall ReadHeader(void);
	virtual Dsdbapi::DsResult __fastcall ReadSubHeader(void);
	virtual Dsdbapi::DsResult __fastcall Write(void);
	virtual Dsdbapi::DsResult __fastcall WriteHeader(void);
	virtual Dsdbapi::DsResult __fastcall WriteSubHeader(void);
	virtual int __fastcall GetDataOffs(void);
	virtual void __fastcall SetDataOffs(const int Value);
	virtual int __fastcall GetDelCount(void);
	virtual void __fastcall SetDelCount(const int Value);
	virtual int __fastcall GetRecLen(void);
	virtual void __fastcall SetRecLen(const int Value);
	virtual int __fastcall GetRecCount(void);
	virtual void __fastcall SetRecCount(const int Value);
	virtual int __fastcall GetHdrSize(void);
	virtual void __fastcall SetHdrSize(const int Value);
	virtual AnsiString __fastcall GetFilePrfx();
	virtual void __fastcall SetFilePrfx(const AnsiString Value);
	virtual bool __fastcall GetIsEncrypted(void);
	virtual void __fastcall SetIsEncrypted(const bool Value);
	virtual bool __fastcall GetIsLocked(void);
	virtual void __fastcall SetIsLocked(const bool Value);
	virtual bool __fastcall GetIsMemoExists(void);
	virtual void __fastcall SetIsMemoExists(const bool Value);
	virtual Dsdbapi::DsResult __fastcall SetToBeginSubHeader(void);
	virtual void __fastcall PrepareHeader(void);
	virtual void __fastcall PrepareFields(void);
	virtual bool __fastcall CheckHeader(const Word Value);
	void __fastcall DecodeHeader(const Word Value);
	int __fastcall GetFldLen(void);
	int __fastcall GetKeyLen(void);
	int __fastcall CalcRecLen(void);
	Word __fastcall GetCheckSum(void);
	
public:
	__fastcall virtual TDdaHeader(Dsdbapi::TDsCustomHandle* Handle, int FileHandle);
	virtual bool __fastcall IsSupportedFldType(Db::TFieldType FldType);
public:
	#pragma option push -w-inl
	/* TDsCustomHeader.Destroy */ inline __fastcall virtual ~TDdaHeader(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TDdaField : public Dsdbapi::TDsCustomField 
{
	typedef Dsdbapi::TDsCustomField inherited;
	
private:
	TDdaHeader* FHeader;
	char FFldName[64];
	#pragma pack(push, 1)
	FldHeader FFldHeader;
	#pragma pack(pop)
	
	
protected:
	virtual AnsiString __fastcall GetFldName();
	virtual Db::TFieldType __fastcall GetFldType(void);
	virtual void __fastcall SetFldName(const AnsiString Value);
	virtual void __fastcall SetFldType(const Db::TFieldType Value);
	virtual void __fastcall SetFldSize(const Word Value);
	virtual void __fastcall SetFldDec(const Byte Value);
	virtual void __fastcall SetAutoInc(const int Value);
	virtual int __fastcall GetAutoInc(void);
	virtual Byte __fastcall GetFldDec(void);
	virtual Word __fastcall GetFldSize(void);
	virtual Word __fastcall GetFldDefSize(void);
	virtual Word __fastcall GetFldPhyType(void);
	virtual Byte __fastcall GetFldSig(void);
	virtual void __fastcall SetFldPhyType(const Word Value);
	virtual void __fastcall SetFldSig(const Byte Value);
	
public:
	virtual Dsdbapi::DsResult __fastcall SetAttr(AnsiString AName, Db::TFieldType AType, Word ASize, Word ADec, Word ASig, bool AReq);
	__fastcall TDdaField(TDdaHeader* AOwner, pFldHeader pBuf);
	__fastcall virtual ~TDdaField(void);
	void __fastcall Prepare(void);
	virtual void __fastcall GetData(void * pRecBuf, void * pDest, Dsdbapi::pBoolean pIsBlank, int AddOffset = 0x0);
	virtual void __fastcall SetData(void * pSrc, void * pRecBuf, int AddOffset = 0x0);
};


class DELPHICLASS TDdaIndex;
class PASCALIMPLEMENTATION TDdaIndex : public Dsdbapi::TDsCustomIndex 
{
	typedef Dsdbapi::TDsCustomIndex inherited;
	
private:
	TDdaHeader* FHeader;
	#pragma pack(push, 1)
	KeyHeader FKeyHeader;
	#pragma pack(pop)
	
	Classes::TList* FKeyList;
	void __fastcall ClearKeyList(void);
	
public:
	__fastcall TDdaIndex(TDdaHeader* AOwner, pKeyHeader pBuf);
	__fastcall virtual ~TDdaIndex(void);
};


class DELPHICLASS TDdaHandle;
class PASCALIMPLEMENTATION TDdaHandle : public Dsdbapi::TDsCustomHandle 
{
	typedef Dsdbapi::TDsCustomHandle inherited;
	
private:
	void *FCryptID;
	
protected:
	TDdaField* __fastcall FindField(AnsiString FldName);
	Dsdbapi::DsResult __fastcall BeginDataUpdate(void);
	Dsdbapi::DsResult __fastcall EndDataUpdate(void);
	Dsdbapi::DsResult __fastcall PackTable(void);
	Dsdbapi::DsResult __fastcall EmptyTable(void);
	virtual int __fastcall GetDelCount(void);
	virtual int __fastcall GetRecCount(void);
	virtual int __fastcall GetRecordSize(void);
	virtual Dsdbapi::DsRecStatus __fastcall GetRecStatus(void * pRecBuf);
	virtual void __fastcall SetRecStatus(void * pRecBuf, Dsdbapi::DsRecStatus Status);
	virtual Dsdbapi::DsResult __fastcall WriteRecStatus(Dsdbapi::DsRecStatus Status);
	virtual Dsdbapi::DsResult __fastcall EncryptBuf(void * pBuf, int BufLen);
	virtual Dsdbapi::DsResult __fastcall DecryptBuf(void * pBuf, int BufLen);
	
public:
	__fastcall virtual TDdaHandle(AnsiString FileName, bool ReadOnly, bool OpenExcl, Dsdbapi::pOpenParams pParams);
	__fastcall TDdaHandle(AnsiString FileName, Dsdbapi::pOpenParams pParams);
	__fastcall virtual ~TDdaHandle(void);
	Dsdbapi::DsResult __fastcall CreateTable(AnsiString Password);
	void __fastcall AddField(AnsiString FldName, Db::TFieldType FldType, Word FldSize, bool FldReq);
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo)/* overload */;
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo, int Value)/* overload */;
	int __fastcall GetAutoIncValue(int FieldNo);
	virtual int __fastcall BookmarkSize(void);
	int __fastcall DataSize(void);
	HIDESBASE int __fastcall RecordSize(void);
	void __fastcall DeleteField(int Index);
	__property int DelCount = {read=GetDelCount, nodefault};
	__property int RecCount = {read=GetRecCount, nodefault};
	__property ShowDeleted ;
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint FLD_LONG = 0x1;
static const Shortint FLD_REAL = 0x2;
static const Shortint FLD_STRING = 0x3;
static const Shortint FLD_PICTURE = 0x4;
static const Shortint FLD_BYTE = 0x5;
static const Shortint FLD_SHORT = 0x6;
static const Shortint FLD_GROUP = 0x7;
static const Shortint FLD_DECIMAL = 0x8;
static const Shortint SIGN_LOCKED = 0x1;
static const Shortint SIGN_OWNED = 0x2;
static const Shortint SIGN_ENCRYPTED = 0x4;
static const Shortint SIGN_MEMO = 0x8;
static const Shortint SIGN_COMPRESSED = 0x10;
static const Shortint SIGN_RECLAIM = 0x20;
static const Shortint SIGN_READONLY = 0x40;
static const Byte SIGN_CREATED = 0x80;
static const Shortint DATA_NEW = 0x1;
static const Shortint DATA_OLD = 0x2;
static const Shortint DATA_REV = 0x4;
static const Shortint DATA_DEL = 0x10;
static const Shortint DATA_HLD = 0x40;
static const Word VER_21_SIG = 0x3343;
static const Shortint FILL_CHAR = 0x0;
static const Word DDA_FILE_VERSION = 0x2b4a;
static const int DDA_CRYPT_KEY1 = 0x4e14cbf5;
static const int DDA_CRYPT_KEY2 = 0x3af18b4e;
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenTable(AnsiString FileName, bool bReadOnly, bool bExclusive, Dsdbapi::pOpenParams pParams, TDdaHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCloseTable(TDdaHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPackTable(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCreateTable(TDdaHandle* CreateDesc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBegin(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToEnd(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBookMark(TDdaHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBookMark(TDdaHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetFilter(TDdaHandle* Handle, AnsiString FilterExpr, Db::TFilterOptions FilterOptions);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDropFilter(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetNextRecord(TDdaHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetPriorRecord(TDdaHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecord(TDdaHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInitRecord(TDdaHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInsertRecord(TDdaHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsModifyRecord(TDdaHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteRecord(TDdaHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEmptyTable(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsReadBlock(TDdaHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsWriteBlock(TDdaHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsAppendRecord(TDdaHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecCount(TDdaHandle* Handle, int &iRecCount);
extern PACKAGE Dsdbapi::DsResult __fastcall DsBeginDataUpdate(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEndDataUpdate(TDdaHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFindRecord(TDdaHandle* Handle, AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetField(TDdaHandle* Handle, Word iField, void * pRecBuf, void * pDest, bool &bBlank);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutField(TDdaHandle* Handle, Word iField, void * pRecBuf, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenBlob(TDdaHandle* Handle, void * pRecBuf, Word iField, Dsdbapi::DsOpenMode eOpenMode);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobSize(TDdaHandle* Handle, void * pRecBuf, Word iField, int &iSize);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlob(TDdaHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutBlob(TDdaHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsTruncateBlob(TDdaHandle* Handle, void * pRecBuf, Word iField, int iLen);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFreeBlob(TDdaHandle* Handle, void * pRecBuf, Word iField);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobHeading(TDdaHandle* Handle, Word iField, void * pRecBuf, void * pDest);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCopyTable(TDdaHandle* SrcHandle, TDdaHandle* &DsTHandle, char * DestName, bool bOverWrite);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteTable(Dsdbapi::TDsDbHandle* DbHandle, char * TblName);

}	/* namespace Dsddaapi */
using namespace Dsddaapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDdaApi
