// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsPdxApi.pas' rev: 6.00

#ifndef DsPdxApiHPP
#define DsPdxApiHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbParser.hpp>	// Pascal unit
#include <DsDbUtils.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
#include <DsCP.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <FMTBcd.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dspdxapi
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TPdxVersion { PDX_VERSION_UNKNOWN, PDX_VERSION_30, PDX_VERSION_35, PDX_VERSION_4x, PDX_VERSION_5x, PDX_VERSION_7x };
#pragma option pop

struct PdxBookmark;
typedef PdxBookmark *pPdxBookmark;

#pragma pack(push, 1)
struct PdxBookmark
{
	int BlockNum;
	int FirstNum;
	int RecordNum;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PdxFileHeader
{
	Word RecordSize;
	Word HeaderSize;
	Byte FileType;
	Byte BlockSize;
	int NumRecords;
	Word NumBlocksUsed;
	Word NumBlocksTotal;
	Word BlockFirst;
	Word BlockLast;
	Word Unknown_01;
	Byte ModifiedFlags1;
	Byte IndexFieldNumber;
	void *PrimaryIndexWorkspace;
	void *Unknown_02;
	Byte Unknown_03[3];
	Byte NumFields;
	Byte Unknown_04;
	Byte PrimaryKeyFields;
	Byte Unknown_05;
	int Encryption1;
	Byte SortOrder;
	Byte ModifiedFlags2;
	Word Unknown_06;
	Byte ChangeCount1;
	Byte ChangeCount2;
	Byte Unknown_07;
	void *TableNamePtr;
	void *FldInfoPtr;
	Byte WriteProtected;
	Byte FileVersionID;
	Word MaxBlocks;
	Byte Unknown_08;
	Byte AuxPasswords;
	Word Unknown_09;
	void *CryptInfoStartPtr;
	void *CryptInfoEndPtr;
	Byte Unknown_10;
	int AutoInc;
	Word FreeBlockFirst;
	Byte IndexUpdateRequired;
	Byte Unknown_12;
	Word HdrSize;
	Word Unknown_13;
	Byte RefIntegrity;
	Word Unknown_14;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PdxFileSubHeader
{
	Word Unknown_01;
	Word Unknown_02;
	int Encryption2;
	int FileUpdateTime;
	Word HiFieldID;
	Word HiFieldID1;
	Word SometimesNumFields;
	Word DosGlobalCodePage;
	int Unknown_16;
	int ChangeCount4;
	int Unknown_17;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct RecHeader
{
	
} ;
#pragma pack(pop)

typedef RecHeader *pRecHeader;

struct FldHeader;
typedef FldHeader *pFldHeader;

#pragma pack(push, 1)
struct FldHeader
{
	Byte FldType;
	Byte FldSize;
} ;
#pragma pack(pop)

struct BlockHeader;
typedef BlockHeader *pBlockHeader;

#pragma pack(push, 1)
struct BlockHeader
{
	Word NextBlock;
	Word PrevBlock;
	short DataOffset;
} ;
#pragma pack(pop)

#pragma option push -b-
enum BlockAttr { baPrev, baNext, baRecsNum };
#pragma option pop

typedef BlockAttr *pBlockAttr;

class DELPHICLASS TPdxField;
class DELPHICLASS TPdxHeader;
class PASCALIMPLEMENTATION TPdxHeader : public Dsdbapi::TDsCustomHeader 
{
	typedef Dsdbapi::TDsCustomHeader inherited;
	
private:
	#pragma pack(push, 1)
	PdxFileHeader FTableHeader;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	PdxFileSubHeader FTableSubHeader;
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
	virtual int __fastcall GetRecLen(void);
	virtual void __fastcall SetRecLen(const int Value);
	virtual int __fastcall GetDelCount(void);
	virtual void __fastcall SetDelCount(const int Value);
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
	int __fastcall GetFieldNamesLen(void);
	int __fastcall GetKeyLen(void);
	int __fastcall CalcRecLen(void);
	Word __fastcall GetCheckSum(void);
	TPdxVersion __fastcall GetVersion(void);
	Word __fastcall GetHeaderLen(bool FullLen);
	
public:
	__fastcall virtual TPdxHeader(Dsdbapi::TDsCustomHandle* Handle, int FileHandle);
	virtual bool __fastcall IsSupportedFldType(Db::TFieldType FldType);
public:
	#pragma option push -w-inl
	/* TDsCustomHeader.Destroy */ inline __fastcall virtual ~TPdxHeader(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TPdxField : public Dsdbapi::TDsCustomField 
{
	typedef Dsdbapi::TDsCustomField inherited;
	
private:
	TPdxHeader* FHeader;
	char FFldName[64];
	#pragma pack(push, 1)
	FldHeader FFldHeader;
	#pragma pack(pop)
	
	Word FFldDec;
	
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
	virtual Word __fastcall GetPhySize(void);
	
public:
	virtual Dsdbapi::DsResult __fastcall SetAttr(AnsiString AName, Db::TFieldType AType, Word ASize, Word ADec, Word ASig, bool AReq);
	__fastcall TPdxField(TPdxHeader* AOwner, pFldHeader pBuf);
	__fastcall virtual ~TPdxField(void);
	void __fastcall Prepare(void);
	virtual void __fastcall GetData(void * pRecBuf, void * pDest, Dsdbapi::pBoolean pIsBlank, int AddOffset = 0x0);
	virtual void __fastcall SetData(void * pSrc, void * pRecBuf, int AddOffset = 0x0);
};


class DELPHICLASS TPdxIndex;
class PASCALIMPLEMENTATION TPdxIndex : public Dsdbapi::TDsCustomIndex 
{
	typedef Dsdbapi::TDsCustomIndex inherited;
	
private:
	TPdxHeader* FHeader;
	Classes::TList* FKeyList;
	void __fastcall ClearKeyList(void);
	
public:
	__fastcall TPdxIndex(TPdxHeader* AOwner);
	__fastcall virtual ~TPdxIndex(void);
};


class DELPHICLASS TPdxBlock;
class DELPHICLASS TPdxHandle;
class DELPHICLASS TPdxBlobHandle;
#pragma pack(push, 1)
struct BlobHeader
{
	Byte RecordType;
	Word BlockSize;
	Word ModifNum;
	Word Unknown_01;
	Word Unknown_02;
	Word Unknown_03;
	Word BaseSize;
	Word SubBlockSize;
	Byte Unknown_04;
	Byte SubChunkSize;
	Word SubBlockNums;
	Word SubThreshold;
} ;
#pragma pack(pop)

class PASCALIMPLEMENTATION TPdxBlobHandle : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TPdxHandle* FHandle;
	#pragma pack(push, 1)
	BlobHeader FHeader;
	#pragma pack(pop)
	
	int FileHandle;
	
public:
	__fastcall TPdxBlobHandle(TPdxHandle* Handle);
	__fastcall virtual ~TPdxBlobHandle(void);
	void __fastcall OpenBlob(void);
	void __fastcall CloseBlob(void);
	bool __fastcall BlobExists(void);
	bool __fastcall ReadHeader(void);
	Dsdbapi::DsResult __fastcall BlobSize(void * pRecBuf, Word iField, int &iSize);
	Dsdbapi::DsResult __fastcall GetBlob(void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
};


class PASCALIMPLEMENTATION TPdxHandle : public Dsdbapi::TDsCustomHandle 
{
	typedef Dsdbapi::TDsCustomHandle inherited;
	
private:
	int FKeyFileHandle;
	TPdxBlobHandle* FBlobHandle;
	Dsdbapi::ICryptType FCryptType;
	int FCurrBlock;
	int FNextBlock;
	int FPrevBlock;
	void __fastcall SetBlockSize(const Word Value);
	
protected:
	virtual void __fastcall SetLevel(const Word Value);
	TPdxField* __fastcall FindField(AnsiString FldName);
	Dsdbapi::DsResult __fastcall BeginDataUpdate(void);
	Dsdbapi::DsResult __fastcall EndDataUpdate(void);
	Dsdbapi::DsResult __fastcall PackTable(void);
	Dsdbapi::DsResult __fastcall EmptyTable(void);
	virtual int __fastcall GetSeek(int RecNum);
	virtual int __fastcall GetDelCount(void);
	virtual int __fastcall GetRecCount(void);
	virtual int __fastcall GetRecordSize(void);
	virtual Dsdbapi::DsResult __fastcall DelBufRecord(int RecNo);
	virtual Dsdbapi::DsRecStatus __fastcall GetRecStatus(void * pRecBuf);
	virtual void __fastcall SetRecStatus(void * pRecBuf, Dsdbapi::DsRecStatus Status);
	virtual Dsdbapi::DsResult __fastcall WriteRecStatus(Dsdbapi::DsRecStatus Status);
	Word __fastcall GetBlockSize(void);
	int __fastcall BlockCapacity(void);
	int __fastcall BlockToSeek(int Value);
	int __fastcall SeekToBlock(int Value);
	bool __fastcall ReadBlock(int Value);
	bool __fastcall ReadNextBlock(void);
	bool __fastcall ReadPrevBlock(void);
	bool __fastcall ReadBlockHeader(int BlockNum, pBlockHeader pBlockHdr);
	bool __fastcall WriteBlockHeader(int BlockNum, Word BlockNext, Word BlockPrev, Word BlockRecs);
	bool __fastcall WriteBlockAttr(int BlockNum, BlockAttr Attr, Word Value);
	bool __fastcall AppendBlock(int RecCount);
	bool __fastcall InitKeyFile(void);
	void __fastcall CloseKeyFile(void);
	void __fastcall OpenBlobFile(void);
	void __fastcall CloseBlobFile(void);
	
public:
	__fastcall virtual TPdxHandle(AnsiString FileName, bool ReadOnly, bool OpenExcl, Dsdbapi::pOpenParams pParams);
	__fastcall TPdxHandle(AnsiString FileName, Dsdbapi::pOpenParams pParams);
	__fastcall virtual ~TPdxHandle(void);
	Dsdbapi::DsResult __fastcall CreateTable(void);
	void __fastcall AddField(AnsiString FldName, Db::TFieldType FldType, Word FldSize, bool FldReq);
	virtual Dsdbapi::DsResult __fastcall AppendRecord(void * pRecBuf, bool WriteHdr);
	virtual Dsdbapi::DsResult __fastcall SetToBeginData(void);
	virtual Dsdbapi::DsResult __fastcall SetToEndData(void);
	virtual Dsdbapi::DsResult __fastcall SetToRecNo(int Value);
	virtual Dsdbapi::DsResult __fastcall GetRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	virtual Dsdbapi::DsResult __fastcall GetNextRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	virtual Dsdbapi::DsResult __fastcall GetPriorRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo)/* overload */;
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo, int Value)/* overload */;
	int __fastcall GetAutoIncValue(int FieldNo);
	virtual int __fastcall BookmarkSize(void);
	int __fastcall DataSize(void);
	HIDESBASE int __fastcall RecordSize(void);
	void __fastcall DeleteField(int Index);
	__property Word BlockSize = {read=GetBlockSize, write=SetBlockSize, nodefault};
	__property Dsdbapi::ICryptType CryptType = {read=FCryptType, write=FCryptType, nodefault};
	__property int DelCount = {read=GetDelCount, nodefault};
	__property int RecCount = {read=GetRecCount, nodefault};
};


class PASCALIMPLEMENTATION TPdxBlock : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TPdxHandle* FHandle;
	int FBlockSize;
	int FRecSize;
	void *FBuffer;
	
public:
	__fastcall TPdxBlock(TPdxHandle* Handle);
	__fastcall virtual ~TPdxBlock(void);
};


struct BlobInfo;
typedef BlobInfo *pBlobInfo;

#pragma pack(push, 1)
struct BlobInfo
{
	int FileLoc;
	int Length;
	Word ModifNum;
} ;
#pragma pack(pop)

struct BlobIndex;
typedef BlobIndex *pBlobIndex;

#pragma pack(push, 1)
struct BlobIndex
{
	Byte Offset;
	Byte Len16;
	Word ModifNum;
	Byte Len;
} ;
#pragma pack(pop)

struct zBlobBlock;
typedef zBlobBlock *pBlobBlock;

#pragma pack(push, 1)
struct zBlobBlock
{
	Byte RecordType;
	Word BlockSize;
	int BlobLen;
	Word ModifNum;
} ;
#pragma pack(pop)

typedef BlobHeader *pBlobHeader;

//-- var, const, procedure ---------------------------------------------------
static const Shortint FLD_PDX_ALPHA = 0x1;
static const Shortint FLD_PDX_DATE = 0x2;
static const Shortint FLD_PDX_INT16 = 0x3;
static const Shortint FLD_PDX_INT32 = 0x4;
static const Shortint FLD_PDX_MONEY = 0x5;
static const Shortint FLD_PDX_NUMBER = 0x6;
static const Shortint FLD_PDX_LOGICAL = 0x9;
static const Shortint FLD_PDX_MEMO = 0xc;
static const Shortint FLD_PDX_BINARY = 0xd;
static const Shortint FLD_PDX_FMEMO = 0xe;
static const Shortint FLD_PDX_OLE = 0xf;
static const Shortint FLD_PDX_GRAPHIC = 0x10;
static const Shortint FLD_PDX_TIME = 0x14;
static const Shortint FLD_PDX_TIMESTAMP = 0x15;
static const Shortint FLD_PDX_AUTOINC = 0x16;
static const Shortint FLD_PDX_BCD = 0x17;
static const Shortint FLD_PDX_BYTES = 0x18;
static const Shortint FILL_CHAR = 0x0;
static const Shortint FILE_DB_INDEXED = 0x0;
static const Shortint FILE_PX_INDEX = 0x1;
static const Shortint FILE_DB_NONIDX = 0x2;
static const Shortint FILE_XNN_NONINC = 0x3;
static const Shortint FILE_YNN_NONINC = 0x4;
static const Shortint FILE_XNN_INC = 0x5;
static const Shortint FILE_XGN_NONINC = 0x6;
static const Shortint FILE_YGN_TOTAL = 0x7;
static const Shortint FILE_XGN_INC = 0x8;
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenTable(AnsiString FileName, bool bReadOnly, bool bExclusive, Dsdbapi::pOpenParams pParams, TPdxHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCloseTable(TPdxHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPackTable(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCreateTable(Dsdbapi::pCrTblDesc pCrDesc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBegin(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToEnd(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToRecordNo(TPdxHandle* Handle, int RecNo);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBookMark(TPdxHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBookMark(TPdxHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetFilter(TPdxHandle* Handle, AnsiString FilterExpr, Db::TFilterOptions FilterOptions);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDropFilter(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetNextRecord(TPdxHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetPriorRecord(TPdxHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecord(TPdxHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInitRecord(TPdxHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInsertRecord(TPdxHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsModifyRecord(TPdxHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteRecord(TPdxHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEmptyTable(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsReadBlock(TPdxHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsWriteBlock(TPdxHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsAppendRecord(TPdxHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecCount(TPdxHandle* Handle, int &iRecCount);
extern PACKAGE Dsdbapi::DsResult __fastcall DsBeginDataUpdate(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEndDataUpdate(TPdxHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFindRecord(TPdxHandle* Handle, AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetField(TPdxHandle* Handle, Word iField, void * pRecBuf, void * pDest, bool &bBlank);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutField(TPdxHandle* Handle, Word iField, void * pRecBuf, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenBlob(TPdxHandle* Handle, void * pRecBuf, Word iField, Dsdbapi::DsOpenMode eOpenMode);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobSize(TPdxHandle* Handle, void * pRecBuf, Word iField, int &iSize);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlob(TPdxHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutBlob(TPdxHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsTruncateBlob(TPdxHandle* Handle, void * pRecBuf, Word iField, int iLen);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFreeBlob(TPdxHandle* Handle, void * pRecBuf, Word iField);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobHeading(TPdxHandle* Handle, Word iField, void * pRecBuf, void * pDest);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCopyTable(TPdxHandle* SrcHandle, TPdxHandle* &DstHandle, char * DestName, bool bOverWrite);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteTable(Dsdbapi::TDsDbHandle* DbHandle, char * TblName);

}	/* namespace Dspdxapi */
using namespace Dspdxapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsPdxApi
