/*> Ver: Beta V1.3 ****************      History      ***************************\

Beta V1.0       03/24/1999      Released
Beta V1.1       04/15/1999      BCB 4.0 Support.
Beta V1.2b1     04/21/1999      Uses with BMMediaDialogs
Beta V1.2b2     11/29/2003      Uses with BMMediaDialogs
Beta V1.3       11/23/2008      Added BCB 2009 support.

Legal issues: Copyright (C) 1998, 2008 by Boian Mitov
              <mitov@mitov.com>
              <http://www.mitov.com>
              <http://www.openwire.org>

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
#ifndef BMWaveH
#define BMWaveH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>

#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__

#elif (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif
//---------------------------------------------------------------------------
namespace Bmwaveeditors
{
    class TBMWaveDataProperty;
    class TBMWaveEditor;
};

namespace Bmwave
{
//---------------------------------------------------------------------------
class PACKAGE TBMWaveData : public Classes::TPersistent
{
    typedef Classes::TPersistent inherited;

    friend class Bmwaveeditors::TBMWaveDataProperty;
    friend class Bmwaveeditors::TBMWaveEditor;

private:
    Byte   *FData;
    int     FSize;
    bool    FStoredInExe;
    String  FileName;

private:
    bool __fastcall DoWrite(void);
    void __fastcall Fill( long ASize, Byte * AData);

protected:
    virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
    virtual void __fastcall ReadData(Classes::TStream* Stream);
    virtual void __fastcall WriteData(Classes::TStream* Stream);

public:
    void __fastcall Clear(void);
    virtual void __fastcall LoadFromFile(System::String Value);
    virtual void __fastcall Assign(Classes::TPersistent* Source);
    virtual void __fastcall Play(void);
    bool __fastcall Empty(void);

public:
    __fastcall TBMWaveData(void);
    __fastcall virtual ~TBMWaveData(void);

};
//---------------------------------------------------------------------------
class PACKAGE TBMCustomSound : public TComponent
{
  typedef TComponent inherited;

private:
protected:
public:
    __fastcall  TBMCustomSound (TComponent* Owner);

public:
    virtual void __fastcall Play (void);
    virtual bool __fastcall Empty (void);

__published:
};
//---------------------------------------------------------------------------
class PACKAGE TBMWave : public TBMCustomSound
{
  typedef TBMCustomSound inherited;
  
private:
protected:
  TBMWaveData  *FWave;

protected:
  void __fastcall SetWave ( TBMWaveData* Value );
  
public:
    __fastcall  TBMWave(TComponent* Owner);
    __fastcall ~TBMWave();

public:
    virtual void __fastcall Play (void);
    virtual bool __fastcall Empty (void);

__published:
    __property TBMWaveData     *Wave              = { read=FWave, write=SetWave };
};
//---------------------------------------------------------------------------
};
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmwave;
#endif
//---------------------------------------------------------------------------
#endif
