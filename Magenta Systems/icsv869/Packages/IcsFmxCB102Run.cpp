//---------------------------------------------------------------------------

#include <System.hpp>
#pragma hdrstop

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#ifdef _WIN64
  #pragma link "cryptui.a"
  #pragma link "crypt32.a"
  #pragma link "IcsCommonCB102Run.a"
#else
  #pragma link "cryptui.lib"
  #pragma link "crypt32.lib"
  #pragma link "IcsCommonCB102Run.lib"
#endif

#pragma argsused
extern "C" int _libmain(unsigned long reason)
{
	return 1;
}
//---------------------------------------------------------------------------


