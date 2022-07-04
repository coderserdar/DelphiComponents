// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Blowunit.pas' rev: 3.00

#ifndef BlowunitHPP
#define BlowunitHPP
#include <Classes.hpp>
#include <Cryptcon.hpp>
#include <SysUtils.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Blowunit
{
//-- type declarations -------------------------------------------------------
struct twoAword
{
	Cryptcon::aword Xl;
	Cryptcon::aword Xr;
} ;

typedef twoAword *PtwoAword;

typedef int Tbf_PArray[18];

typedef Tbf_PArray *Pbf_P;

typedef int Tbf_SArray[4][256];

typedef Tbf_SArray *Pbf_S;

class DELPHICLASS TBlowFish;
class PASCALIMPLEMENTATION TBlowFish : public Cryptcon::TCrypto 
{
	typedef Cryptcon::TCrypto inherited;
	
private:
	Tbf_PArray *Fpbf_P;
	Tbf_SArray *Fpbf_S;
	Cryptcon::aword *FpXl;
	Cryptcon::aword *FpXr;
	Cryptcon::aword __fastcall bf_F(Cryptcon::Paword x);
	void __fastcall InitArray(void);
	void __fastcall ROUND(Cryptcon::Paword a, Cryptcon::Paword b, Byte n);
	void __fastcall BF_Initialize(void);
	void __fastcall BF_Encipher(void);
	void __fastcall BF_Decipher(void);
	
public:
	__fastcall virtual TBlowFish(Classes::TComponent* Owner);
	__fastcall virtual ~TBlowFish(void);
	virtual void __fastcall EncipherBLOCK(void);
	virtual void __fastcall DecipherBLOCK(void);
	virtual void __fastcall SetKeys(void);
};

typedef int uw;

//-- var, const, procedure ---------------------------------------------------
#define bf_N (Byte)(16)
#define KEYBYTES (Byte)(8)
#define MAXKEYBYTES (Byte)(56)
#define BF_MAXKEYLENGTH (Byte)(65)
#define BF_MINKEYLENGTH (Byte)(8)
extern PACKAGE int bf_P[18];
extern PACKAGE int bf_S[4][256];
extern PACKAGE void __fastcall Register(void);

}	/* namespace Blowunit */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Blowunit;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Blowunit
