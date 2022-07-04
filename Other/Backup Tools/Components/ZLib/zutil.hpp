// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZUtil.pas' rev: 3.00

#ifndef ZUtilHPP
#define ZUtilHPP
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zutil
{
//-- type declarations -------------------------------------------------------
typedef Byte Bytef;

typedef Byte charf;

typedef int int;

typedef int intf;

typedef Cardinal uInt;

typedef Cardinal uIntf;

typedef int Long;

typedef int uLong;

typedef int uLongf;

typedef void *voidp;

typedef void *voidpf;

typedef Byte *pBytef;

typedef int *pIntf;

typedef Cardinal *puIntf;

typedef int *puLong;

typedef Cardinal ptr2int;

typedef Byte zByteArray[2147483647];

typedef zByteArray *pzByteArray;

typedef int zIntfArray[536870911];

typedef zIntfArray *pzIntfArray;

typedef Cardinal zuIntArray[536870911];

typedef zuIntArray *PuIntArray;

typedef Byte uch;

typedef Byte uchf;

typedef Word ush;

typedef Word ushf;

typedef int ulg;

typedef Cardinal unsigned;

typedef Byte *pcharf;

typedef Byte *puchf;

typedef Word *pushf;

typedef Byte zuchfArray[2147483647];

typedef zByteArray *puchfArray;

typedef Word zushfArray[1073741823];

typedef zushfArray *pushfArray;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall zmemcpy(pBytef destp, pBytef sourcep, Cardinal len);
extern PACKAGE int __fastcall zmemcmp(pBytef s1p, pBytef s2p, Cardinal len);
extern PACKAGE void __fastcall zmemzero(pBytef destp, Cardinal len);
extern PACKAGE void __fastcall zcfree(void * opaque, void * ptr);
extern PACKAGE void * __fastcall zcalloc(void * opaque, Cardinal items, Cardinal size);

}	/* namespace Zutil */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zutil;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// ZUtil
