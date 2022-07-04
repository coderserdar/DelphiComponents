// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDbfApi.pas' rev: 6.00

#ifndef DsDbfApiHPP
#define DsDbfApiHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbParser.hpp>	// Pascal unit
#include <DsDbUtils.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
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

namespace Dsdbfapi
{
//-- type declarations -------------------------------------------------------
typedef int *pDbfBookmark;

typedef int DbfBookmark;

#pragma option push -b-
enum TDbfVersion { dvUNKNOWN, dvDBASE3, dvDBASE4, dvDBASE5, dvDBASE7, dvFOXPRO };
#pragma option pop

#pragma pack(push, 1)
struct TRecDate
{
	char Year[4];
	char Month[2];
	char Day[2];
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct DbfFileHeader
{
	Byte Version;
	Byte Year;
	Byte Month;
	Byte Day;
	int NumRecs;
	Word HdrLen;
	Word RecLen;
	Word Rsrv1;
	Byte Transaction;
	Byte Encrypted;
	char NetWork[12];
	Byte MdxFile;
	Byte LangDrv;
	Word Rsrv2;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct DbfFileSubHeader
{
	char Rsrv1[36];
} ;
#pragma pack(pop)

struct RecHeader;
typedef RecHeader *pRecHeader;

#pragma pack(push, 1)
struct RecHeader
{
	Byte Hdr;
} ;
#pragma pack(pop)

struct Fld3Header;
typedef Fld3Header *pFld3Header;

#pragma pack(push, 1)
struct Fld3Header
{
	char FldName[11];
	Byte FldType;
	Byte Rsrv1[4];
	Byte FldSize;
	Byte FldDec;
	Byte Rsrv2[2];
	Byte WorkId;
	Byte Rsrv3[10];
	Byte IsPart;
} ;
#pragma pack(pop)

struct Fld5Header;
typedef Fld5Header *pFld5Header;

#pragma pack(push, 1)
struct Fld5Header
{
	char FldName[32];
	Byte FldType;
	Byte FldSize;
	Byte FldDec;
	Word Rsrv1;
	Byte MDXFlag;
	unsigned Rsrv2;
	unsigned AutoInc;
	Word Rsrv3;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct FldHeader
{
	
	union
	{
		struct 
		{
			Fld5Header Hdr5;
			
		};
		struct 
		{
			Fld3Header Hdr3;
			
		};
		
	};
} ;
#pragma pack(pop)

typedef FldHeader *pFldHeader;

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

struct KeyItem;
typedef KeyItem *pKeyItem;

#pragma pack(push, 1)
struct KeyItem
{
	Byte FldType;
	Word FldNum;
	Word ElmOff;
	Byte ElmLen;
} ;
#pragma pack(pop)

struct PicHeader;
typedef PicHeader *pPicHeader;

#pragma pack(push, 1)
struct PicHeader
{
	Word PicLen;
	char PicStr[256];
} ;
#pragma pack(pop)

struct ArrHeader;
typedef ArrHeader *pArrHeader;

#pragma pack(push, 1)
struct ArrHeader
{
	Word NumDim;
	Word TotDim;
	Word ElmSiz;
} ;
#pragma pack(pop)

struct ArrItem;
typedef ArrItem *pArrItem;

#pragma pack(push, 1)
struct ArrItem
{
	Word MaxDim;
	Word LenDim;
} ;
#pragma pack(pop)

struct DbfDate;
typedef DbfDate *pDbfDate;

#pragma pack(push, 1)
struct DbfDate
{
	char Year[4];
	char Month[2];
	char Day[2];
} ;
#pragma pack(pop)

typedef char *pBlobFileHdr;

typedef char BlobFileHdr[512];

struct BlobDbtHdr;
typedef BlobDbtHdr *pBlobDbtHdr;

#pragma pack(push, 1)
struct BlobDbtHdr
{
	int NextBlock;
	Byte Dummy[4];
	Byte DbfFile[8];
	Byte bVer;
	Byte Dummy2[3];
	Word BlockLen;
	Byte Dummy3[490];
} ;
#pragma pack(pop)

struct BlobFptHdr;
typedef BlobFptHdr *pBlobFptHdr;

#pragma pack(push, 1)
struct BlobFptHdr
{
	int NextBlock;
	Byte Dummy[2];
	Word BlockLen;
	Byte Dummy3[504];
} ;
#pragma pack(pop)

struct BlobBlockHdr;
typedef BlobBlockHdr *pBlobBlockHdr;

#pragma pack(push, 1)
struct BlobBlockHdr
{
	unsigned MemoType;
	unsigned MemoSize;
} ;
#pragma pack(pop)

class DELPHICLASS TDbfField;
class DELPHICLASS TDbfHeader;
class PASCALIMPLEMENTATION TDbfHeader : public Dsdbapi::TDsCustomHeader 
{
	typedef Dsdbapi::TDsCustomHeader inherited;
	
private:
	#pragma pack(push, 1)
	DbfFileHeader FTableHeader;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	DbfFileSubHeader FTableSubHeader;
	#pragma pack(pop)
	
	
protected:
	virtual void __fastcall SetFldCount(const Word Value);
	virtual void __fastcall SetIdxCount(const Word Value);
	virtual void __fastcall SetArrCount(const Word Value);
	virtual void __fastcall SetPicCount(const Word Value);
	virtual Dsdbapi::DsResult __fastcall Read(void);
	virtual Dsdbapi::DsResult __fastcall ReadHeader(void);
	virtual Dsdbapi::DsResult __fastcall ReadSubHeader(void);
	virtual Dsdbapi::DsResult __fastcall Write(void);
	virtual Dsdbapi::DsResult __fastcall WriteHeader(void);
	virtual Dsdbapi::DsResult __fastcall WriteSubHeader(void);
	Dsdbapi::DsResult __fastcall WriteEnd(void);
	virtual AnsiString __fastcall GetFilePrfx();
	virtual void __fastcall SetFilePrfx(const AnsiString Value);
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
	int __fastcall GetPicLen(void);
	int __fastcall GetArrLen(void);
	int __fastcall CalcRecLen(void);
	Word __fastcall GetCheckSum(void);
	Byte __fastcall GetLangDrvCode(void);
	AnsiString __fastcall GetLangDrvName();
	TDbfVersion __fastcall Version(void);
	bool __fastcall HasBlob(void);
	
public:
	__fastcall virtual TDbfHeader(Dsdbapi::TDsCustomHandle* Handle, int FileHandle);
	virtual bool __fastcall IsSupportedFldType(Db::TFieldType FldType);
public:
	#pragma option push -w-inl
	/* TDsCustomHeader.Destroy */ inline __fastcall virtual ~TDbfHeader(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TDbfField : public Dsdbapi::TDsCustomField 
{
	typedef Dsdbapi::TDsCustomField inherited;
	
private:
	TDbfHeader* FHeader;
	#pragma pack(push, 1)
	FldHeader FFldHeader;
	#pragma pack(pop)
	
	
protected:
	virtual AnsiString __fastcall GetFldName();
	virtual Db::TFieldType __fastcall GetFldType(void);
	virtual void __fastcall SetFldName(const AnsiString Value);
	virtual void __fastcall SetFldType(const Db::TFieldType Value);
	Db::TFieldType __fastcall PhyToLog(Word PhyType);
	Word __fastcall LogToPhy(Db::TFieldType LogType);
	virtual void __fastcall SetFldSize(const Word Value);
	virtual void __fastcall SetFldDec(const Byte Value);
	virtual void __fastcall SetAutoInc(const int Value);
	virtual void __fastcall SetFldSig(const Byte Value);
	virtual int __fastcall GetAutoInc(void);
	virtual Byte __fastcall GetFldDec(void);
	virtual Word __fastcall GetFldPhyType(void);
	virtual Byte __fastcall GetFldSig(void);
	virtual Word __fastcall GetFldSize(void);
	virtual Word __fastcall GetFldDefSize(void);
	virtual void __fastcall SetFldPhyType(const Word Value);
	
public:
	virtual Dsdbapi::DsResult __fastcall SetAttr(AnsiString AName, Db::TFieldType AType, Word ASize, Word ADec, Word ASig, bool AReq);
	__fastcall TDbfField(TDbfHeader* AOwner, pFldHeader pBuf);
	__fastcall virtual ~TDbfField(void);
	void __fastcall Prepare(void);
	virtual void __fastcall GetData(void * pRecBuf, void * pDest, Dsdbapi::pBoolean pIsBlank, int AddOffset = 0x0);
	virtual void __fastcall SetData(void * pSrc, void * pRecBuf, int AddOffset = 0x0);
};


class DELPHICLASS TDbfIndex;
class PASCALIMPLEMENTATION TDbfIndex : public Dsdbapi::TDsCustomIndex 
{
	typedef Dsdbapi::TDsCustomIndex inherited;
	
private:
	TDbfHeader* FHeader;
	#pragma pack(push, 1)
	KeyHeader FKeyHeader;
	#pragma pack(pop)
	
	Classes::TList* FKeyList;
	void __fastcall ClearKeyList(void);
	
public:
	__fastcall TDbfIndex(TDbfHeader* AOwner, pKeyHeader pBuf);
	__fastcall virtual ~TDbfIndex(void);
	int __fastcall AddKeyItem(pKeyItem pBuf);
};


class DELPHICLASS TDbfArray;
class PASCALIMPLEMENTATION TDbfArray : public Dsdbapi::TDsCustomArray 
{
	typedef Dsdbapi::TDsCustomArray inherited;
	
private:
	TDbfHeader* FHeader;
	#pragma pack(push, 1)
	ArrHeader FArrHeader;
	#pragma pack(pop)
	
	Classes::TList* FArrList;
	void __fastcall ClearArrList(void);
	
public:
	__fastcall TDbfArray(TDbfHeader* AOwner, pArrHeader pBuf);
	__fastcall virtual ~TDbfArray(void);
	int __fastcall AddKeyItem(pArrItem pBuf);
};


class DELPHICLASS TDbfPicture;
class PASCALIMPLEMENTATION TDbfPicture : public Dsdbapi::TDsCustomPicture 
{
	typedef Dsdbapi::TDsCustomPicture inherited;
	
private:
	TDbfHeader* FHeader;
	#pragma pack(push, 1)
	PicHeader FPicHeader;
	#pragma pack(pop)
	
	
public:
	__fastcall TDbfPicture(TDbfHeader* AOwner, pPicHeader pBuf);
	__fastcall virtual ~TDbfPicture(void);
};


class DELPHICLASS TDbfHandle;
class DELPHICLASS TDbfBlobHandle;
class PASCALIMPLEMENTATION TDbfBlobHandle : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TDbfHandle* FHandle;
	char FHeader[512];
	int FileHandle;
	
public:
	__fastcall TDbfBlobHandle(TDbfHandle* Handle);
	__fastcall virtual ~TDbfBlobHandle(void);
	void __fastcall OpenBlob(void);
	void __fastcall CloseBlob(void);
	bool __fastcall BlobExists(void);
	bool __fastcall ReadHeader(void);
	Dsdbapi::DsResult __fastcall BlobSize(void * pRecBuf, Word iField, int &iSize);
	Dsdbapi::DsResult __fastcall GetBlob(void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
	int __fastcall BlockSize(void);
};


class PASCALIMPLEMENTATION TDbfHandle : public Dsdbapi::TDsCustomHandle 
{
	typedef Dsdbapi::TDsCustomHandle inherited;
	
private:
	TDbfBlobHandle* FBlobHandle;
	
protected:
	TDbfField* __fastcall FindField(AnsiString FldName);
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
	void __fastcall OpenBlobFile(void);
	void __fastcall CloseBlobFile(void);
	
public:
	__fastcall virtual TDbfHandle(AnsiString FileName, bool ReadOnly, bool OpenExcl, Dsdbapi::pOpenParams pParams);
	__fastcall TDbfHandle(AnsiString FileName, Dsdbapi::pOpenParams pParams);
	__fastcall virtual ~TDbfHandle(void);
	Dsdbapi::DsResult __fastcall CreateTable(void);
	void __fastcall AddField(AnsiString FldName, Db::TFieldType FldType, Word FldSize, Word FldDec, bool FldReq);
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
static const Shortint MAX_SMALLINT = 0x4;
static const Shortint MAX_INTEGER = 0x9;
static const Shortint MAX_LARGEINT = 0x12;
static const Byte FLD_CHARACTER = 0x43;
static const Byte FLD_NUMERIC = 0x4e;
static const Byte FLD_LOGICAL = 0x4c;
static const Byte FLD_DATE = 0x44;
static const Byte FLD_MEMO = 0x4d;
static const Byte FLD_FLOAT = 0x46;
static const Byte FLD_BINARY = 0x42;
static const Byte FLD_GENERAL = 0x47;
static const Byte FLD_DOUBLE = 0x4f;
static const Byte FLD_PICTURE = 0x50;
static const Byte FLD_CURRENCY = 0x59;
static const Byte FLD_TIME = 0x54;
static const Byte FLD_DATETIME = 0x40;
static const Byte FLD_INTEGER = 0x49;
static const Byte FLD_AUTOINC = 0x2b;
static const Byte DATA_DEL = 0x2a;
static const Byte DATA_REV = 0x20;
static const Byte DATA_NEW = 0x20;
static const Shortint VER_DBASE_01 = 0x3;
static const Byte VER_DBASE_02 = 0x83;
static const Byte VER_DBASE_03 = 0xf5;
static const Byte VER_DBASE_04 = 0x8b;
static const Byte VER_DBASE_05 = 0x8e;
static const Shortint FILL_CHAR = 0x20;
static const Shortint TERM_FIELDS = 0xd;
static const Shortint TERM_RECORDS = 0x1a;
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenTable(AnsiString FileName, bool bReadOnly, bool bExclusive, Dsdbapi::pOpenParams pParams, TDbfHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCloseTable(TDbfHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPackTable(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCreateTable(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBegin(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToEnd(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBookMark(TDbfHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBookMark(TDbfHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetFilter(TDbfHandle* Handle, AnsiString FilterExpr, Db::TFilterOptions FilterOptions);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDropFilter(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetNextRecord(TDbfHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetPriorRecord(TDbfHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecord(TDbfHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInitRecord(TDbfHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInsertRecord(TDbfHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsModifyRecord(TDbfHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteRecord(TDbfHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEmptyTable(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsReadBlock(TDbfHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsWriteBlock(TDbfHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsAppendRecord(TDbfHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecCount(TDbfHandle* Handle, int &iRecCount);
extern PACKAGE Dsdbapi::DsResult __fastcall DsBeginDataUpdate(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEndDataUpdate(TDbfHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFindRecord(TDbfHandle* Handle, AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetField(TDbfHandle* Handle, Word iField, void * pRecBuf, void * pDest, bool &bBlank);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutField(TDbfHandle* Handle, Word iField, void * pRecBuf, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenBlob(TDbfHandle* Handle, void * pRecBuf, Word iField, Dsdbapi::DsOpenMode eOpenMode);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobSize(TDbfHandle* Handle, void * pRecBuf, Word iField, int &iSize);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlob(TDbfHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutBlob(TDbfHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsTruncateBlob(TDbfHandle* Handle, void * pRecBuf, Word iField, int iLen);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFreeBlob(TDbfHandle* Handle, void * pRecBuf, Word iField);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobHeading(TDbfHandle* Handle, Word iField, void * pRecBuf, void * pDest);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCopyTable(TDbfHandle* SrcHandle, TDbfHandle* &DsTHandle, char * DestName, bool bOverWrite);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteTable(Dsdbapi::TDsDbHandle* DbHandle, char * TblName);

}	/* namespace Dsdbfapi */
using namespace Dsdbfapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDbfApi
