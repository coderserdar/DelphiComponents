// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsCrypt.pas' rev: 6.00

#ifndef DsCryptHPP
#define DsCryptHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dscrypt
{
//-- type declarations -------------------------------------------------------
typedef void *TRC6ID;

#pragma option push -b-
enum TCryptMethod { cmRC6CBC, cmRC6CFB, cmRC6CFB128 };
#pragma option pop

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall CRP_InitCrypt(AnsiString Key, TRC6ID &ID);
extern PACKAGE void __fastcall CRP_Encrypt(TRC6ID ID, TCryptMethod Method, void * pBuf, unsigned Len);
extern PACKAGE void __fastcall CRP_Decrypt(TRC6ID ID, TCryptMethod Method, void * pBuf, unsigned Len);
extern PACKAGE void __fastcall CRP_FreeCrypt(TRC6ID ID);

}	/* namespace Dscrypt */
using namespace Dscrypt;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsCrypt
