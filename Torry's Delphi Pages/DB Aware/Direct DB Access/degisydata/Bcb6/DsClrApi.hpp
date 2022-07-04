// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsClrApi.pas' rev: 6.00

#ifndef DsClrApiHPP
#define DsClrApiHPP

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

namespace Dsclrapi
{
//-- type declarations -------------------------------------------------------
typedef int *pClrBookmark;

typedef int ClrBookmark;

#pragma pack(push, 1)
struct ClrFileHeader
{
	Word FileSIG;
	Word SFAtr;
	Byte NumKeys;
	int NumRecs;
	int NumDels;
	Word NumFlds;
	Word NumPics;
	Word NumArrs;
	Word RecLen;
	int Offset;
	int LogEOF;
	int LogBOF;
	int FreeRec;
	char RecName[12];
	char MemName[12];
	char FilPrefx[3];
	char RecPrefx[3];
	Word MemoLen;
	Word MemoWid;
	int LockCont;
	int ChgTime;
	int ChgDate;
	Word CheckSum;
} ;
#pragma pack(pop)

struct RecHeader;
typedef RecHeader *pRecHeader;

#pragma pack(push, 1)
struct RecHeader
{
	Byte Hdr;
	int Ptr;
} ;
#pragma pack(pop)

struct FldHeader;
typedef FldHeader *pFldHeader;

#pragma pack(push, 1)
struct FldHeader
{
	Byte FldType;
	char FldName[16];
	Word FOffset;
	Word Length;
	Byte DecSig;
	Byte DecDec;
	Word ArrNum;
	Word PicNum;
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

struct BlobHeader;
typedef BlobHeader *pBlobHeader;

#pragma pack(push, 1)
struct BlobHeader
{
	Word FileSign;
	int FirstDel;
} ;
#pragma pack(pop)

struct BlobBlock;
typedef BlobBlock *pBlobBlock;

#pragma pack(push, 1)
struct BlobBlock
{
	int NextBlock;
	char BlobValue[252];
} ;
#pragma pack(pop)

class DELPHICLASS TClrField;
class DELPHICLASS TClrHeader;
class PASCALIMPLEMENTATION TClrHeader : public Dsdbapi::TDsCustomHeader 
{
	typedef Dsdbapi::TDsCustomHeader inherited;
	
private:
	#pragma pack(push, 1)
	ClrFileHeader FTableHeader;
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
	bool __fastcall CrackHeader(void);
	void __fastcall DecodeHeader(Word ID);
	int __fastcall GetFldLen(void);
	int __fastcall GetKeyLen(void);
	int __fastcall GetPicLen(void);
	int __fastcall GetArrLen(void);
	int __fastcall CalcRecLen(void);
	Word __fastcall GetCheckSum(void);
	
public:
	__fastcall virtual TClrHeader(Dsdbapi::TDsCustomHandle* Handle, int FileHandle);
	virtual bool __fastcall IsSupportedFldType(Db::TFieldType FldType);
public:
	#pragma option push -w-inl
	/* TDsCustomHeader.Destroy */ inline __fastcall virtual ~TClrHeader(void) { }
	#pragma option pop
	
};


class DELPHICLASS TClrArray;
class PASCALIMPLEMENTATION TClrField : public Dsdbapi::TDsCustomField 
{
	typedef Dsdbapi::TDsCustomField inherited;
	
private:
	TClrHeader* FHeader;
	#pragma pack(push, 1)
	FldHeader FFldHeader;
	#pragma pack(pop)
	
	int FAutoInc;
	
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
	virtual Word __fastcall GetPhySize(void);
	
public:
	virtual Dsdbapi::DsResult __fastcall SetAttr(AnsiString AName, Db::TFieldType AType, Word ASize, Word ADec, Word ASig, bool AReq);
	bool __fastcall IsArray(void);
	Word __fastcall ArrayNumber(void);
	TClrArray* __fastcall GetArray(void);
	TClrHeader* __fastcall Header(void);
	__fastcall TClrField(TClrHeader* AOwner, pFldHeader pBuf);
	__fastcall virtual ~TClrField(void);
	void __fastcall Prepare(void);
	virtual void __fastcall GetData(void * pRecBuf, void * pDest, Dsdbapi::pBoolean pIsBlank, int AddOffset = 0x0);
	virtual void __fastcall SetData(void * pSrc, void * pRecBuf, int AddOffset = 0x0);
};


class DELPHICLASS TClrIndex;
class PASCALIMPLEMENTATION TClrIndex : public Dsdbapi::TDsCustomIndex 
{
	typedef Dsdbapi::TDsCustomIndex inherited;
	
private:
	TClrHeader* FHeader;
	#pragma pack(push, 1)
	KeyHeader FKeyHeader;
	#pragma pack(pop)
	
	Classes::TList* FKeyList;
	void __fastcall ClearKeyList(void);
	
public:
	__fastcall TClrIndex(TClrHeader* AOwner, pKeyHeader pBuf);
	__fastcall virtual ~TClrIndex(void);
	int __fastcall AddKeyItem(pKeyItem pBuf);
};


class PASCALIMPLEMENTATION TClrArray : public Dsdbapi::TDsCustomArray 
{
	typedef Dsdbapi::TDsCustomArray inherited;
	
private:
	TClrHeader* FHeader;
	#pragma pack(push, 1)
	ArrHeader FArrHeader;
	#pragma pack(pop)
	
	Classes::TList* FArrList;
	void __fastcall ClearArrList(void);
	Word __fastcall GetTotalCount(void);
	
public:
	__fastcall TClrArray(TClrHeader* AOwner, pArrHeader pBuf);
	__fastcall virtual ~TClrArray(void);
	int __fastcall AddKeyItem(pArrItem pBuf);
	pArrItem __fastcall GetItem(int Index);
	__property Word NumDim = {read=FArrHeader.NumDim, nodefault};
	__property Word TotDim = {read=FArrHeader.TotDim, nodefault};
	__property Word ElmSize = {read=FArrHeader.ElmSiz, nodefault};
	__property Word TotalCount = {read=GetTotalCount, nodefault};
};


class DELPHICLASS TClrPicture;
class PASCALIMPLEMENTATION TClrPicture : public Dsdbapi::TDsCustomPicture 
{
	typedef Dsdbapi::TDsCustomPicture inherited;
	
private:
	TClrHeader* FHeader;
	#pragma pack(push, 1)
	PicHeader FPicHeader;
	#pragma pack(pop)
	
	
public:
	__fastcall TClrPicture(TClrHeader* AOwner, pPicHeader pBuf);
	__fastcall virtual ~TClrPicture(void);
};


class DELPHICLASS TClrHandle;
class DELPHICLASS TClrBlobHandle;
class PASCALIMPLEMENTATION TClrBlobHandle : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TClrHandle* FHandle;
	#pragma pack(push, 1)
	BlobHeader FHeader;
	#pragma pack(pop)
	
	int FileHandle;
	
public:
	__fastcall TClrBlobHandle(TClrHandle* Handle);
	__fastcall virtual ~TClrBlobHandle(void);
	void __fastcall OpenBlob(void);
	void __fastcall CloseBlob(void);
	bool __fastcall BlobExists(void);
	bool __fastcall ReadHeader(void);
	Dsdbapi::DsResult __fastcall BlobSize(void * pRecBuf, Word iField, int &iSize);
	Dsdbapi::DsResult __fastcall GetBlob(void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
};


class PASCALIMPLEMENTATION TClrHandle : public Dsdbapi::TDsCustomHandle 
{
	typedef Dsdbapi::TDsCustomHandle inherited;
	
private:
	Word FPassId;
	bool FDecodeTime;
	bool FDecodeDate;
	TClrBlobHandle* FBlobHandle;
	
protected:
	TClrField* __fastcall FindField(AnsiString FldName);
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
	void __fastcall SetPassId(const AnsiString Password);
	virtual Dsdbapi::DsResult __fastcall EncryptBuf(void * pBuf, int BufLen);
	virtual Dsdbapi::DsResult __fastcall DecryptBuf(void * pBuf, int BufLen);
	virtual Dsdbapi::DsResult __fastcall GetRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	virtual Dsdbapi::DsResult __fastcall GetNextRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	virtual Dsdbapi::DsResult __fastcall GetPriorRecord(void * pRecBuf, Dsdbapi::pRecProps pRecProps);
	void __fastcall OpenBlobFile(void);
	void __fastcall CloseBlobFile(void);
	TClrField* __fastcall FieldByIndex(int Index, int &AddOffset);
	
public:
	virtual Dsdbapi::DsResult __fastcall AppendRecord(void * pRecBuf, bool WriteHdr);
	virtual Dsdbapi::DsResult __fastcall ModifyRecord(void * pRecBuf);
	__fastcall virtual TClrHandle(AnsiString FileName, bool ReadOnly, bool OpenExcl, Dsdbapi::pOpenParams pParams);
	__fastcall TClrHandle(AnsiString FileName, Dsdbapi::pOpenParams pParams);
	__fastcall virtual ~TClrHandle(void);
	Dsdbapi::DsResult __fastcall CreateTable(void);
	void __fastcall AddField(AnsiString FldName, Db::TFieldType FldType, Word FldSize, Word FldSig, bool FldReq);
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo)/* overload */;
	Dsdbapi::DsResult __fastcall SetAutoIncValue(int FieldNo, int Value)/* overload */;
	int __fastcall GetAutoIncValue(int FieldNo);
	virtual int __fastcall BookmarkSize(void);
	int __fastcall DataSize(void);
	HIDESBASE int __fastcall RecordSize(void);
	void __fastcall DeleteField(int Index);
	__property int DelCount = {read=GetDelCount, nodefault};
	__property int RecCount = {read=GetRecCount, nodefault};
	__property bool DecodeDate = {read=FDecodeDate, write=FDecodeDate, default=0};
	__property bool DecodeTime = {read=FDecodeTime, write=FDecodeTime, default=0};
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
static const Shortint FLD_MEMO = 0x50;
static const Shortint FLD_DATE = 0x51;
static const Shortint FLD_TIME = 0x52;
#define DEFAULT_EXT ".DAT"
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
static const Shortint FILL_CHAR = 0x20;
static const Word DELTA_DAYS = 0x8d41;
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenTable(AnsiString FileName, bool bReadOnly, bool bExclusive, Dsdbapi::pOpenParams pParams, TClrHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCloseTable(TClrHandle* &Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPackTable(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCreateTable(TClrHandle* CreateDesc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBegin(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToEnd(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBookMark(TClrHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetToBookMark(TClrHandle* Handle, void * pBkMark);
extern PACKAGE Dsdbapi::DsResult __fastcall DsSetFilter(TClrHandle* Handle, AnsiString FilterExpr, Db::TFilterOptions FilterOptions);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDropFilter(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetNextRecord(TClrHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetPriorRecord(TClrHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecord(TClrHandle* Handle, void * pRecBuf, Dsdbapi::pRecProps pRecProps);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInitRecord(TClrHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsInsertRecord(TClrHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsModifyRecord(TClrHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteRecord(TClrHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEmptyTable(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsReadBlock(TClrHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsWriteBlock(TClrHandle* Handle, int &iRecords, void * pBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsAppendRecord(TClrHandle* Handle, void * pRecBuf);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetRecCount(TClrHandle* Handle, int &iRecCount);
extern PACKAGE Dsdbapi::DsResult __fastcall DsBeginDataUpdate(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsEndDataUpdate(TClrHandle* Handle);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFindRecord(TClrHandle* Handle, AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetField(TClrHandle* Handle, Word iField, void * pRecBuf, void * pDest, bool &bBlank);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutField(TClrHandle* Handle, Word iField, void * pRecBuf, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsOpenBlob(TClrHandle* Handle, void * pRecBuf, Word iField, Dsdbapi::DsOpenMode eOpenMode);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobSize(TClrHandle* Handle, void * pRecBuf, Word iField, int &iSize);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlob(TClrHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pDest, int &iRead);
extern PACKAGE Dsdbapi::DsResult __fastcall DsPutBlob(TClrHandle* Handle, void * pRecBuf, Word iField, int iOffSet, int iLen, void * pSrc);
extern PACKAGE Dsdbapi::DsResult __fastcall DsTruncateBlob(TClrHandle* Handle, void * pRecBuf, Word iField, int iLen);
extern PACKAGE Dsdbapi::DsResult __fastcall DsFreeBlob(TClrHandle* Handle, void * pRecBuf, Word iField);
extern PACKAGE Dsdbapi::DsResult __fastcall DsGetBlobHeading(TClrHandle* Handle, Word iField, void * pRecBuf, void * pDest);
extern PACKAGE Dsdbapi::DsResult __fastcall DsCopyTable(TClrHandle* SrcHandle, TClrHandle* &DsTHandle, char * DestName, bool bOverWrite);
extern PACKAGE Dsdbapi::DsResult __fastcall DsDeleteTable(Dsdbapi::TDsDbHandle* DbHandle, char * TblName);

}	/* namespace Dsclrapi */
using namespace Dsclrapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsClrApi
