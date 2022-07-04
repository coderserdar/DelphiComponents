// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDbApi.pas' rev: 6.00

#ifndef DsDbApiHPP
#define DsDbApiHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbParser.hpp>	// Pascal unit
#include <DsConsts.hpp>	// Pascal unit
#include <DsCP.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <FMTBcd.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsdbapi
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum DsResult { RES_OK, RES_ACCESSDENIED, RES_CANNOTOPENFILE, RES_CANNOTSEEKFILE, RES_CANNOTREADFILE, RES_CANNOTCREATEFILE, RES_CANNOTWRITEFILE, RES_DUPLICATETABLENAME, RES_DUPLICATEUSERNAME, RES_DUPLICATEFIELDNAME, RES_DUPLICATEAUTOINCFIELD, RES_DUPLICATEINDEXNAME, RES_OBJNOTFOUND, RES_DATABASEFILENOTFOUND, RES_TABLENOTFOUND, RES_RECORDNOTFOUND, RES_FIELDNOTFOUND, RES_BLOCKNOTFOUND, RES_INDEXNOTFOUND, RES_RECDELETED, RES_TABLEREADONLY, RES_READONLYFLD, RES_NOTSUFFTABLERIGHTS, RES_INVALIDKEYWORD, RES_TABLEISBUSY, RES_OBJECTLOCKED, RES_NOTABLOBFIELD, RES_NOCURRREC, RES_INDEXOPEN, RES_BOF, RES_EOF, RES_BEGINOFBLOB, RES_ENDOFBLOB, RES_TOOMANYTABLES, RES_TOOMANYUSERS, RES_TOOMANYPROCS, RES_TOOMANYCHECKS, RES_TOOMANYFIELDS, RES_TOOMANYINDEXES, RES_DATABASECLOSED, RES_TABLECRYPTED, RES_INVALIDOBJECT, RES_INVALIDHANDLE, RES_INVALIDDBHANDLE, RES_INVALIDFILTERHANDLE, RES_INVALIDPARAMETER, RES_INVALIDBOOKMARK, RES_INVALIDBLOBOFFSET, RES_INVALIDDATABASEFILE, RES_INVALIDDATABASEVERSION, RES_INVALIDDATABASECHECKSUM
	, RES_INVALIDUSER, RES_INVALIDTABLENAME, RES_INVALIDUSERNAME, RES_INVALIDFIELDTYPE, RES_INVALIDFIELDNAME, RES_INVALIDFIELDSIZE, RES_INVALIDBLOCK, RES_INVALIDEXPRESSION, RES_INVALIDENCRYPTION, RES_UNKNOWNIDENTIFIER, RES_UNEXPECTEDIDENTIFIER };
#pragma option pop

class DELPHICLASS DsException;
class PASCALIMPLEMENTATION DsException : public Db::EDatabaseError 
{
	typedef Db::EDatabaseError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall DsException(const AnsiString Msg) : Db::EDatabaseError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall DsException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Db::EDatabaseError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall DsException(int Ident)/* overload */ : Db::EDatabaseError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall DsException(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Db::EDatabaseError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall DsException(const AnsiString Msg, int AHelpContext) : Db::EDatabaseError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall DsException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Db::EDatabaseError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall DsException(int Ident, int AHelpContext)/* overload */ : Db::EDatabaseError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall DsException(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Db::EDatabaseError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~DsException(void) { }
	#pragma option pop
	
};


typedef Byte *pByte;

typedef Word *pWord;

typedef bool *pBoolean;

typedef int *pInteger;

typedef short *pSmallInt;

typedef Word *pWordBool;

typedef System::TDateTime *pDateTime;

typedef double *pDouble;

typedef __int64 *pLargeInt;

typedef Sysutils::TTimeStamp *pTimeStamp;

typedef char Char3[3];

typedef char Char11[11];

typedef char Char12[12];

typedef char Char16[16];

typedef char Char24[24];

typedef char Char32[32];

typedef char Char36[36];

typedef char Char48[48];

typedef char Char64[64];

typedef char Char128[128];

typedef char Char252[252];

typedef char Char256[256];

typedef char Char512[512];

typedef char CharArray[1];

typedef char DS_MSG[256];

typedef Set<Byte, 0, 255>  TByteSet;

#pragma pack(push, 2)
struct TByteIdent
{
	Word iOffs;
	TByteSet iVals;
} ;
#pragma pack(pop)

struct IFldDesc;
typedef IFldDesc *pFldDesc;

#pragma pack(push, 1)
struct IFldDesc
{
	Word FldIndex;
	char FldName[36];
	Db::TFieldType FldType;
	Word FldSize;
	Byte FldDec;
	Byte FldSig;
	bool FldReq;
} ;
#pragma pack(pop)

struct ICrTblDesc;
typedef ICrTblDesc *pCrTblDesc;

#pragma pack(push, 1)
struct ICrTblDesc
{
	char TblName[256];
	Word Level;
	Dscp::CP_TYPE CodePage;
	Word FldCount;
	IFldDesc *pFldDescs;
} ;
#pragma pack(pop)

#pragma option push -b-
enum ICryptType { ctNone, ctXorRandom, ctPower };
#pragma option pop

typedef int *pDefBookmark;

typedef int IDefBookmark;

#pragma option push -b-
enum DsRecStatus { rsNormal, rsModified, rsInserted, rsDeleted };
#pragma option pop

struct IRecProps;
typedef IRecProps *pRecProps;

#pragma pack(push, 1)
struct IRecProps
{
	int RecNum;
	DsRecStatus RecStatus;
	bool DeleteFlag;
} ;
#pragma pack(pop)

struct IFileInfo;
typedef IFileInfo *pFileInfo;

#pragma pack(push, 1)
struct IFileInfo
{
	Word Version;
	Byte Build;
	ICryptType CryptType;
	int UnUsed1;
	int UnUsed2;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct IBlobFileHeader
{
	Word Version;
	Word Build;
	System::TDateTime DateUpd;
	int BlockSize;
	int BlockCount;
	int BlockFirst;
	int BlockFree;
	Byte UnUsed[996];
} ;
#pragma pack(pop)

struct IBlobFieldHeader;
typedef IBlobFieldHeader *pBlobFieldHeader;

#pragma pack(push, 1)
struct IBlobFieldHeader
{
	int BlockNum;
	int DataSize;
	Word UnUsed;
} ;
#pragma pack(pop)

struct IBlobBlockHeader;
typedef IBlobBlockHeader *pBlobBlockHeader;

#pragma pack(push, 1)
struct IBlobBlockHeader
{
	int BlockPred;
	int BlockNext;
} ;
#pragma pack(pop)

#pragma option push -b-
enum IBlobBlockType { btNormal, btNew, btFree };
#pragma option pop

#pragma option push -b-
enum IRecordType { rtNormal, rtDeleted };
#pragma option pop

struct IOpenParams;
typedef IOpenParams *pOpenParams;

#pragma pack(push, 1)
struct IOpenParams
{
	bool CreateFile;
	int BufRecords;
	bool AutoUpdate;
	bool KeepBlobOpen;
	bool ShowDeleted;
	Word BlockSize;
	Dscp::CP_TYPE CodePage;
	Word TableLevel;
	bool DecodeDate;
	bool DecodeTime;
	char Password[64];
} ;
#pragma pack(pop)

#pragma option push -b-
enum DsHandleMode { hmNORMAL, hmCREATE };
#pragma option pop

#pragma option push -b-
enum DsOpenMode { dsREADWRITE, dsREADONLY };
#pragma option pop

#pragma option push -b-
enum IUpdateLock { ulNoLock, ulUpdateLock };
#pragma option pop

typedef Set<Shortint, 0, 15>  TDBFlags;

class DELPHICLASS TDsDbHandle;
class PASCALIMPLEMENTATION TDsDbHandle : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	AnsiString FDatabaseName;
	AnsiString __fastcall GetDatabaseName();
	
public:
	__property AnsiString DatabaseName = {read=GetDatabaseName, write=FDatabaseName};
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDsDbHandle(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDsDbHandle(void) { }
	#pragma option pop
	
};


struct TRecInfo;
typedef TRecInfo *PRecInfo;

#pragma pack(push, 1)
struct TRecInfo
{
	int RecordNumber;
	Db::TUpdateStatus UpdateStatus;
	Db::TBookmarkFlag BookmarkFlag;
} ;
#pragma pack(pop)

typedef DynamicArray<AnsiString >  TBlobDataArray;

class DELPHICLASS TDsCustomField;
class PASCALIMPLEMENTATION TDsCustomField : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Word FIndex;
	Word FOffset;
	Word FPhySize;
	
protected:
	virtual int __fastcall GetAutoInc(void) = 0 ;
	virtual Byte __fastcall GetFldDec(void) = 0 ;
	virtual AnsiString __fastcall GetFldName(void) = 0 ;
	virtual Word __fastcall GetFldPhyType(void) = 0 ;
	virtual Byte __fastcall GetFldSig(void) = 0 ;
	virtual Word __fastcall GetFldSize(void) = 0 ;
	virtual Db::TFieldType __fastcall GetFldType(void) = 0 ;
	virtual void __fastcall SetAutoInc(const int Value) = 0 ;
	virtual void __fastcall SetFldDec(const Byte Value) = 0 ;
	virtual void __fastcall SetFldName(const AnsiString Value) = 0 ;
	virtual void __fastcall SetFldPhyType(const Word Value) = 0 ;
	virtual void __fastcall SetFldSig(const Byte Value) = 0 ;
	virtual void __fastcall SetFldSize(const Word Value) = 0 ;
	virtual void __fastcall SetFldType(const Db::TFieldType Value) = 0 ;
	virtual void __fastcall GetData(void * pRecBuf, void * pDest, pBoolean pIsBlank, int AddOffset = 0x0) = 0 ;
	virtual void __fastcall SetData(void * pSrc, void * pRecBuf, int AddOffset = 0x0) = 0 ;
	virtual Word __fastcall GetFldDefSize(void) = 0 ;
	virtual bool __fastcall GetAsBoolean(void);
	virtual double __fastcall GetAsFloat(void);
	virtual int __fastcall GetAsInteger(void);
	virtual AnsiString __fastcall GetAsString();
	virtual System::TDateTime __fastcall GetAsDateTime(void);
	virtual Word __fastcall GetPhySize(void);
	virtual bool __fastcall GetIsEmpty(void);
	void __fastcall SetAsBoolean(const bool Value);
	void __fastcall SetAsFloat(const double Value);
	void __fastcall SetAsInteger(const int Value);
	void __fastcall SetAsString(const AnsiString Value);
	void __fastcall SetAsDateTime(const System::TDateTime Value);
	
public:
	virtual DsResult __fastcall SetAttr(AnsiString AName, Db::TFieldType AType, Word ASize, Word ADec, Word ASig, bool AReq) = 0 ;
	__property bool AsBoolean = {read=GetAsBoolean, write=SetAsBoolean, nodefault};
	__property System::TDateTime AsDateTime = {read=GetAsDateTime, write=SetAsDateTime};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, nodefault};
	__property AnsiString AsString = {read=GetAsString, write=SetAsString};
	__property int AutoInc = {read=GetAutoInc, write=SetAutoInc, nodefault};
	__property Byte FldDec = {read=GetFldDec, write=SetFldDec, nodefault};
	__property AnsiString FldName = {read=GetFldName, write=SetFldName};
	__property Word FldPhyType = {read=GetFldPhyType, write=SetFldPhyType, nodefault};
	__property Byte FldSig = {read=GetFldSig, write=SetFldSig, nodefault};
	__property Word FldSize = {read=GetFldSize, write=SetFldSize, nodefault};
	__property Db::TFieldType FldType = {read=GetFldType, write=SetFldType, nodefault};
	__property Word Index = {read=FIndex, write=FIndex, nodefault};
	__property bool IsEmpty = {read=GetIsEmpty, nodefault};
	__property Word Offset = {read=FOffset, write=FOffset, nodefault};
	__property Word PhySize = {read=GetPhySize, write=FPhySize, nodefault};
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDsCustomField(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDsCustomField(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDsCustomIndex;
class PASCALIMPLEMENTATION TDsCustomIndex : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDsCustomIndex(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDsCustomIndex(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDsCustomPicture;
class PASCALIMPLEMENTATION TDsCustomPicture : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDsCustomPicture(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDsCustomPicture(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDsCustomArray;
class PASCALIMPLEMENTATION TDsCustomArray : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDsCustomArray(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDsCustomArray(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDsCustomHeader;
class DELPHICLASS TDsCustomHandle;
class PASCALIMPLEMENTATION TDsCustomHandle : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	DsResult FError;
	Dsdbparser::TDsDbParser* FParser;
	Dscp::CP_TYPE FCodePage;
	Word FLevel;
	IUpdateLock FUpdateLock;
	int FFileHandle;
	bool FShowDeleted;
	DsHandleMode FHandleMode;
	void __fastcall SetShowDeleted(const bool Value);
	void __fastcall SetCodePage(const Dscp::CP_TYPE Value);
	
protected:
	virtual int __fastcall GetDelCount(void) = 0 ;
	virtual int __fastcall GetRecCount(void) = 0 ;
	virtual int __fastcall GetRecordSize(void) = 0 ;
	virtual DsRecStatus __fastcall GetRecStatus(void * pRecBuf) = 0 ;
	virtual DsResult __fastcall WriteRecStatus(DsRecStatus Status) = 0 ;
	virtual void __fastcall SetRecStatus(void * pRecBuf, DsRecStatus Status) = 0 ;
	virtual void __fastcall SetLevel(const Word Value);
	virtual DsResult __fastcall OpenFile(void);
	virtual DsResult __fastcall CloseFile(void);
	virtual bool __fastcall AcceptRecord(void);
	virtual int __fastcall GetSeek(int RecNum);
	virtual DsResult __fastcall DelBufRecord(int RecNo);
	virtual DsResult __fastcall UnDelBufRecord(int RecNo);
	virtual DsResult __fastcall EncryptBuf(void * pBuf, int BufLen);
	virtual DsResult __fastcall DecryptBuf(void * pBuf, int BufLen);
	void __fastcall InitBuffer(int RecCount);
	void __fastcall FreeBuffer(void);
	int __fastcall BufferSize(void);
	bool __fastcall RecInBuffer(int Value);
	DsResult __fastcall GetBufRecord(int RecNo, void * pRecBuf);
	DsResult __fastcall SetBufRecord(int RecNo, void * pRecBuf);
	__property bool ShowDeleted = {read=FShowDeleted, write=SetShowDeleted, nodefault};
	
public:
	TDsCustomHeader* FHeader;
	void *FBuffer;
	int FBufFirst;
	int FBufCount;
	int FBufSize;
	int FBufCapacity;
	int FCurrentRec;
	void *pCurrentRec;
	AnsiString FFileName;
	bool FReadOnly;
	bool FExclusive;
	int FOpenMode;
	
protected:
	virtual DsResult __fastcall SetToBeginData(void);
	virtual DsResult __fastcall SetToEndData(void);
	virtual DsResult __fastcall SetToRecNo(int RecNo);
	virtual DsResult __fastcall GetRecord(void * pRecBuf, pRecProps pRecProps);
	virtual DsResult __fastcall GetNextRecord(void * pRecBuf, pRecProps pRecProps);
	virtual DsResult __fastcall GetPriorRecord(void * pRecBuf, pRecProps pRecProps);
	virtual DsResult __fastcall AppendRecord(void * pRecBuf, bool WriteHdr);
	virtual DsResult __fastcall ModifyRecord(void * pRecBuf);
	virtual DsResult __fastcall DeleteRecord(void * pRecBuf, bool WriteHdr);
	virtual int __fastcall BookmarkSize(void);
	virtual DsResult __fastcall Locate(AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions Options);
	virtual bool __fastcall IsEncrypted(void);
	
public:
	__fastcall virtual TDsCustomHandle(AnsiString FileName, bool ReadOnly, bool OpenExcl, pOpenParams pParams);
	__fastcall virtual ~TDsCustomHandle(void);
	virtual DsResult __fastcall UnDeleteRecord(void * pRecBuf, bool WriteHdr);
	DsResult __fastcall AnsiToNative(void * pBuf, int BufLen);
	DsResult __fastcall NativeToAnsi(void * pBuf, int BufLen);
	DsResult __fastcall SetFilter(AnsiString Expr, Db::TFilterOptions Options);
	DsResult __fastcall DeleteFilter(void);
	__property void * ActiveRecord = {read=pCurrentRec};
	__property Dscp::CP_TYPE CodePage = {read=FCodePage, write=SetCodePage, nodefault};
	__property int DelCount = {read=GetDelCount, nodefault};
	__property DsResult Error = {read=FError, write=FError, nodefault};
	__property bool Exclusive = {read=FExclusive, write=FExclusive, nodefault};
	__property int FileHandle = {read=FFileHandle, nodefault};
	__property DsHandleMode HandleMode = {read=FHandleMode, write=FHandleMode, nodefault};
	__property Word Level = {read=FLevel, write=SetLevel, nodefault};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, nodefault};
	__property int RecCount = {read=GetRecCount, nodefault};
	__property int RecordSize = {read=GetRecordSize, nodefault};
	__property IUpdateLock UpdateLock = {read=FUpdateLock, write=FUpdateLock, nodefault};
};


class PASCALIMPLEMENTATION TDsCustomHeader : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TDsCustomHandle* FHandle;
	int FFileHandle;
	Classes::TList* FFldList;
	Classes::TList* FIdxList;
	Classes::TList* FArrList;
	Classes::TList* FPicList;
	Word __fastcall GetFldCount(void);
	Word __fastcall GetIdxCount(void);
	Word __fastcall GetArrCount(void);
	Word __fastcall GetPicCount(void);
	
protected:
	virtual void __fastcall SetArrCount(const Word Value) = 0 ;
	virtual void __fastcall SetFldCount(const Word Value) = 0 ;
	virtual void __fastcall SetIdxCount(const Word Value) = 0 ;
	virtual void __fastcall SetPicCount(const Word Value) = 0 ;
	virtual DsResult __fastcall Read(void) = 0 ;
	virtual DsResult __fastcall ReadHeader(void) = 0 ;
	virtual DsResult __fastcall ReadSubHeader(void) = 0 ;
	virtual DsResult __fastcall Write(void) = 0 ;
	virtual DsResult __fastcall WriteHeader(void) = 0 ;
	virtual DsResult __fastcall WriteSubHeader(void) = 0 ;
	virtual int __fastcall GetDataOffs(void) = 0 ;
	virtual void __fastcall SetDataOffs(const int Value) = 0 ;
	virtual int __fastcall GetDelCount(void) = 0 ;
	virtual void __fastcall SetDelCount(const int Value) = 0 ;
	virtual int __fastcall GetRecLen(void) = 0 ;
	virtual void __fastcall SetRecLen(const int Value) = 0 ;
	virtual int __fastcall GetRecCount(void) = 0 ;
	virtual void __fastcall SetRecCount(const int Value) = 0 ;
	virtual int __fastcall GetHdrSize(void) = 0 ;
	virtual void __fastcall SetHdrSize(const int Value) = 0 ;
	virtual AnsiString __fastcall GetFilePrfx(void) = 0 ;
	virtual void __fastcall SetFilePrfx(const AnsiString Value) = 0 ;
	virtual bool __fastcall GetIsEncrypted(void) = 0 ;
	virtual void __fastcall SetIsEncrypted(const bool Value) = 0 ;
	virtual bool __fastcall GetIsLocked(void) = 0 ;
	virtual void __fastcall SetIsLocked(const bool Value) = 0 ;
	virtual bool __fastcall GetIsMemoExists(void) = 0 ;
	virtual void __fastcall SetIsMemoExists(const bool Value) = 0 ;
	virtual DsResult __fastcall SetToBeginSubHeader(void) = 0 ;
	virtual void __fastcall PrepareHeader(void) = 0 ;
	virtual void __fastcall PrepareFields(void) = 0 ;
	virtual bool __fastcall CheckHeader(const Word Value) = 0 ;
	virtual bool __fastcall IsSupportedFldType(Db::TFieldType FldType) = 0 ;
	virtual DsResult __fastcall SetToBeginHeader(void);
	void __fastcall ClearFldList(void);
	void __fastcall ClearIdxList(void);
	void __fastcall ClearArrList(void);
	void __fastcall ClearPicList(void);
	void __fastcall ClearLists(void);
	
public:
	__fastcall virtual TDsCustomHeader(TDsCustomHandle* Handle, int AFileHandle);
	__fastcall virtual ~TDsCustomHeader(void);
	int __fastcall AddArray(TDsCustomArray* Arr);
	int __fastcall AddField(TDsCustomField* Fld);
	int __fastcall AddIndex(TDsCustomIndex* Idx);
	int __fastcall AddPicture(TDsCustomPicture* Pic);
	TDsCustomArray* __fastcall GetArray(const int Index);
	TDsCustomField* __fastcall GetField(const int Index);
	TDsCustomIndex* __fastcall GetIndex(const int Index);
	TDsCustomPicture* __fastcall GetPicture(const int Index);
	TDsCustomField* __fastcall FindField(const AnsiString FldName);
	__property Word ArrCount = {read=GetArrCount, write=SetArrCount, nodefault};
	__property int DataOffs = {read=GetDataOffs, write=SetDataOffs, nodefault};
	__property int DelCount = {read=GetDelCount, write=SetDelCount, nodefault};
	__property int FileHandle = {read=FFileHandle, nodefault};
	__property AnsiString FilePrfx = {read=GetFilePrfx, write=SetFilePrfx};
	__property Word FldCount = {read=GetFldCount, write=SetFldCount, nodefault};
	__property int HdrSize = {read=GetHdrSize, write=SetHdrSize, nodefault};
	__property TDsCustomHandle* Handle = {read=FHandle};
	__property Word IdxCount = {read=GetIdxCount, write=SetIdxCount, nodefault};
	__property bool IsEncrypted = {read=GetIsEncrypted, write=SetIsEncrypted, nodefault};
	__property bool IsLocked = {read=GetIsLocked, write=SetIsLocked, nodefault};
	__property bool IsMemoExists = {read=GetIsMemoExists, write=SetIsMemoExists, nodefault};
	__property Word PicCount = {read=GetPicCount, write=SetPicCount, nodefault};
	__property int RecCount = {read=GetRecCount, write=SetRecCount, nodefault};
	__property int RecLen = {read=GetRecLen, write=SetRecLen, nodefault};
};


typedef AnsiString DsDbApi__9[62];

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Byte DsTypeSizes[38];
extern PACKAGE AnsiString MSG_ARRAY[62];
static const int CRYPT_SEED = 0x4b81a0;
static const int CHECK_CODE = 0x2e1a3b2f;
extern PACKAGE void __fastcall Check(DsResult Status);
extern PACKAGE double __fastcall _BcdToDouble(const Fmtbcd::TBcd &Bcd);
extern PACKAGE int __fastcall _BcdToInteger(const Fmtbcd::TBcd &Bcd);

}	/* namespace Dsdbapi */
using namespace Dsdbapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDbApi
