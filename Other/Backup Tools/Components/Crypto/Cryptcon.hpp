// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cryptcon.pas' rev: 3.00

#ifndef CryptconHPP
#define CryptconHPP
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cryptcon
{
//-- type declarations -------------------------------------------------------
enum TSourceType { SourceFile, SourceByteArray, SourceString };

enum TCipherMode { ECBMode, CBCMode, CFBMode };

typedef int UWORD_32bits;

typedef Word UWORD_16bits;

typedef Byte UBYTE_08bits;

typedef Byte BArray[8];

typedef Byte *PByte;

typedef BArray *PArray;

typedef int LArray[2];

typedef int *PLong;

typedef LArray *PLArray;

#pragma pack(push, 1)
struct singleBytes
{
	Byte byte3;
	Byte byte2;
	Byte byte1;
	Byte byte0;
} ;
#pragma pack(pop)

struct aword
{
	
	union
	{
		singleBytes w;
		Byte fByte[4];
		int LWord;
		
	};
} ;

typedef aword *Paword;

class DELPHICLASS TCrypto;
class PASCALIMPLEMENTATION TCrypto : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	System::AnsiString FIVector;
	BArray *FIVTemp;
	System::AnsiString FKey;
	TSourceType FInputType;
	TCipherMode FCipherMode;
	System::AnsiString FInputFilePath;
	System::AnsiString FOutputFilePath;
	BArray *FInputArray;
	BArray *FOutputArray;
	System::AnsiString FInputString;
	Word FInputLength;
	Byte FBuffer[4097];
	Byte FSmallBuffer[64];
	bool FDoneFile;
	Byte FBLOCKSIZE;
	void __fastcall ShiftLeft(PByte pIV, PByte pNewData, Word Pos);
	int __fastcall MIN(int Aparam, int Bparam);
	void __fastcall GenIVector(void);
	void __fastcall InitIV(void);
	void __fastcall StartCipher(bool Continue);
	virtual void __fastcall EncipherBLOCK(void) = 0;
	virtual void __fastcall DecipherBLOCK(void) = 0;
	virtual void __fastcall SetKeys(void) = 0;
	void __fastcall Encipher_File(void);
	void __fastcall Decipher_File(void);
	virtual void __fastcall Encipher_Bytes(void);
	virtual void __fastcall Decipher_Bytes(void);
	void __fastcall EncipherECB(void);
	void __fastcall DecipherECB(void);
	void __fastcall EncipherCFB(void);
	void __fastcall DecipherCFB(void);
	void __fastcall EncipherCBC(void);
	void __fastcall DecipherCBC(void);
	
public:
	void __fastcall DecipherData(bool Continue);
	void __fastcall EncipherData(bool Continue);
	__fastcall virtual ~TCrypto(void);
	__property PArray pInputArray = {read=FInputArray, write=FInputArray};
	__property PArray pOutputArray = {read=FOutputArray, write=FOutputArray};
	
__published:
	__property System::AnsiString Key = {write=FKey, stored=false};
	__property TSourceType InputType = {read=FInputType, write=FInputType, nodefault};
	__property System::AnsiString InputFilePath = {read=FInputFilePath, write=FInputFilePath};
	__property System::AnsiString OutputFilePath = {read=FOutputFilePath, write=FOutputFilePath};
	__property System::AnsiString InputString = {read=FInputString, write=FInputString};
	__property Word InputLength = {read=FInputLength, write=FInputLength, nodefault};
	__property TCipherMode CipherMode = {read=FCipherMode, write=FCipherMode, nodefault};
	__property System::AnsiString IVector = {read=FIVector, write=FIVector, stored=false};
public:
	/* TComponent.Create */ __fastcall virtual TCrypto(Classes::TComponent* AOwner) : Classes::TComponent(
		AOwner) { }
	
};

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Cryptcon */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Cryptcon;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Cryptcon
