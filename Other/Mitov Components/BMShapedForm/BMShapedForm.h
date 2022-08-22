/*> Ver: Beta V1.8 ****************      History      ***************************\

Beta V1.0		03/03/1999		Released
Beta V1.1		03/04/1999		Various bug fixes.
Beta V1.2		03/11/1999		Various bug fixes.
Beta V1.3		03/12/1999		Now working in True Color video mode.
Beta V1.4		03/20/1999		Resource leak fixed.
Beta V1.5		05/06/2001		Fixed a rtear bug on some images.
Beta V1.6		03/04/2008		Added C++ Builder 2006 and C++ Builder 2007 support.
Beta V1.7		03/04/2008		Added C++ Builder 2009 support.
Beta V1.8		03/03/2010		Added C++ Builder 2010 support.

Legal issues: Copyright (C) 1998, 2010 by Boian Mitov
              <mitov@mitov.com>
              <http://www.mitov.com>

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

****************************************************************************
Modifications by Eric DUPONT (roderic@roderic.com) marked as follow :
//---> Begin ED
// End ED
****************************************************************************

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef BMShapedFormH
#define BMShapedFormH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class PACKAGE TBMShapedForm : public TImage
{
	typedef Extctrls::TImage inherited;

private:
protected:
    bool  FMoveable;
    int   FMoveX;
    int   FMoveY;

//---> Begin ED
    bool  InMove;
// End ED

    int FImageHeight;
    int FImageWidth;

    bool    OldStretch;
    bool    OldAutoSize;
    int     OldWidth;
    int     OldHeight;
    int     OldFWidth;
    int     OldFHeight;

    bool    OSOwned;

    HRGN    FRegion;

    TBMShapedForm   *MyParent;

protected:
	virtual void __fastcall SetParent(Controls::TWinControl* Value);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Paint(void);

    virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);

	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X,
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
//---> Begin ED
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int
		Y);
// End ED

protected:
	void __fastcall PictureUpdate(System::TObject* Sender);
	void __fastcall RefreshForm(void);
	void __fastcall ObtainRegion (void);
    void __fastcall AddRegion( int x1, int x2, int y );

public:
    __fastcall TBMShapedForm (TComponent* Owner);
    __fastcall ~TBMShapedForm ();

__published:
	__property bool Moveable = {read=FMoveable, write=FMoveable, default = true };
};
//---------------------------------------------------------------------------
#endif
