/*> Ver: 2.0 ****************      History      ***************************\

V2.0		07/08/1998		Released

Legal issues: Copyright (C) 1997, 1998 by Boian Mitov
              <mitov@ec.rockwell.com>
              <http://members.xoom.com/mitov>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef FormIdleH
#define FormIdleH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>

//---------------------------------------------------------------------------
#if (__BORLANDC__ < 0x0530)
//---------------------------------------------------------------------------
// BCB 1.0
//---------------------------------------------------------------------------
  #define PACKAGE

#endif
//---------------------------------------------------------------------------

class PACKAGE TFormIdle : public TComponent
{
protected:
  static HHOOK  HookHandle;
  static TList *IdleList;
//  static TFormIdle *CurrentFirstIdle;

protected:
//  TFormIdle *NextFormIdle;
  TNotifyEvent FOnIdle;

private:
  static DWORD __stdcall ForegroundIdleProc(
      int code,	// hook code
      DWORD wParam,	// not used
      LONG lParam	// not used
     );
     
protected:
public : 
	virtual void __fastcall DoIdle ();

public:
	__fastcall TFormIdle(TComponent* Owner);
	__fastcall ~TFormIdle();
__published:
  __property TNotifyEvent OnIdle = { read=FOnIdle, write=FOnIdle };

};
//---------------------------------------------------------------------------
#endif
