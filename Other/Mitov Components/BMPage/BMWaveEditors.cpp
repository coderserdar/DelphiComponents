//---------------------------------------------------------------------------
#pragma hdrstop


#if !(( __BORLANDC__ >= 0x0590 ) && ( __BORLANDC__ <= 0x0599 ))
  #define __USE_BMMEDIADIALOGS__
#endif

#ifdef __USE_BMMEDIADIALOGS__
  #include "BMMediaDialogs.h"
#else
  #include "Dialogs.hpp"
#endif

#if ( __BORLANDC__ >= 0x0560 )
    #include <DesignEditors.hpp>
#else
    #include <dsgnintf.hpp>
#endif

#include "BMWaveEditors.h"
#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__

#elif (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif

#include "BMWave.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

#ifdef __USE_BMMEDIADIALOGS__
 typedef TBMOpenMediaDialog TWOpenDialog;
 typedef TBMSaveMediaDialog TWSaveDialog;

#else
 typedef TOpenDialog TWOpenDialog;
 typedef TSaveDialog TWSaveDialog;

#endif
//---------------------------------------------------------------------------
using namespace Bmwave;

namespace Bmwaveeditors
{

class TBMWaveDataProperty : public TPropertyEditor
{
    typedef TPropertyEditor inherited;

private:
    virtual TPropertyAttributes __fastcall GetAttributes(void);
    virtual System::String __fastcall GetValue();
#if ( __BORLANDC__ >= 0x0570 )
    virtual void __fastcall SetValue(const WideString Value);
#endif
    virtual void __fastcall SetValue(const System::String Value);
    virtual void __fastcall Edit(void);
public:
    __fastcall virtual ~TBMWaveDataProperty(void) { }

public:
#ifdef __BCB_60__
  __fastcall TBMWaveDataProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TPropertyEditor( ADesigner, APropCount ) { }

#else
#ifdef __BCB_40__
    __fastcall TBMWaveDataProperty(const _di_IFormDesigner ADesigner, int APropCount) : TPropertyEditor( ADesigner, APropCount ) { }
#else
    __fastcall TBMWaveDataProperty(void) : Dsgnintf::TPropertyEditor() { }
#endif
#endif
};
//---------------------------------------------------------------------------
class TBMWaveEditor : public TComponentEditor 
{
    typedef TComponentEditor inherited;

private:
    virtual void __fastcall Edit(void);
public:
#ifdef __BCB_60__
  __fastcall virtual TBMWaveEditor(Classes::TComponent* AComponent, _di_IDesigner ADesigner) : TComponentEditor(AComponent, ADesigner) { }

#else
  __fastcall virtual TBMWaveEditor(Classes::TComponent* AComponent, _di_IFormDesigner ADesigner) : TComponentEditor(AComponent, ADesigner) { }
  
#endif
//    __fastcall virtual TBMWaveEditor(Classes::TComponent* AComponent, Dsgnintf::TFormDesigner*
//        ADesigner) : Dsgnintf::TComponentEditor(AComponent, ADesigner) { }

public:
    __fastcall virtual ~TBMWaveEditor(void) { }

};
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TPropertyAttributes __fastcall TBMWaveDataProperty::GetAttributes(void)
{
  return inherited::GetAttributes () << paDialog;
}
//---------------------------------------------------------------------------
System::String __fastcall TBMWaveDataProperty::GetValue()
{
  if ( (( TBMWaveData *)GetOrdValue ())->Empty () )
    return TEXT( "(Silence)" );

  else
    return (( TBMWaveData *)GetOrdValue ())->FileName;
}
//---------------------------------------------------------------------------
#if ( __BORLANDC__ >= 0x0570 )
void __fastcall TBMWaveDataProperty::SetValue(const WideString Value)
{
  if ( Value == TEXT( "" ))
    SetOrdValue ( NULL );

  inherited::SetValue( Value );
}
#endif
//---------------------------------------------------------------------------
void __fastcall TBMWaveDataProperty::SetValue(const System::String Value)
{
  if ( Value == TEXT( "" ))
    SetOrdValue ( NULL );
    
  inherited::SetValue( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveDataProperty::Edit ()
{
  TWOpenDialog  *OpenDialog = new TWOpenDialog (Application);
  OpenDialog->Filter = TEXT( "Wave files (*.wav)|*.wav|All files (*.*)|*.*" );
  OpenDialog->Options = OpenDialog->Options << ofFileMustExist;
  TBMWaveData *TmpData = (TBMWaveData*)GetOrdValue ();
  if ( ! TmpData->FileName.IsEmpty () )
    {
    OpenDialog->InitialDir = ExtractFileDir ( TmpData->FileName );
    OpenDialog->FileName = ExtractFileName ( TmpData->FileName );
    }

  if ( OpenDialog->Execute () )
    {
    TmpData = new TBMWaveData;
    TmpData->LoadFromFile( OpenDialog->FileName );
    if ( Application->MessageBox( TEXT( "Do you wand the sond data to be part of the executable ?" ), TEXT( "Option" ), MB_YESNO ) == IDYES )
      TmpData->FStoredInExe = true;

    else
      TmpData->FStoredInExe = false;

    SetOrdValue( ( long ) TmpData );
    Modified ();
    delete TmpData;
    }

  delete OpenDialog;
}
//---------------------------------------------------------------------------
void __fastcall TBMWaveEditor::Edit(void)
{
  TWOpenDialog  *OpenDialog = new TWOpenDialog (Application);
  OpenDialog->Filter = TEXT( "Wave files (*.wav)|*.wav|All files (*.*)|*.*" );
  OpenDialog->Options = OpenDialog->Options << ofFileMustExist;
  if ( ! ((TBMWave *)Component)->Wave->FileName.IsEmpty () )
    {
    OpenDialog->InitialDir = ExtractFileDir ( ((TBMWave *)Component)->Wave->FileName );
    OpenDialog->FileName = ExtractFileName ( ((TBMWave *)Component)->Wave->FileName );
    }

  if ( OpenDialog->Execute () )
    {
    TBMWaveData *TmpData = new TBMWaveData;
    TmpData->LoadFromFile( OpenDialog->FileName );
    if ( Application->MessageBox( TEXT( "Do you wand the sond data to be part of the executable ?" ), TEXT( "Option" ), MB_YESNO ) == IDYES )
      TmpData->FStoredInExe = true;

    else
      TmpData->FStoredInExe = false;

    ((TBMWave *)Component)->Wave->Assign(TmpData);

    Designer->Modified();
    delete TmpData;
    }

  delete OpenDialog;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
      TComponentClass classes[1] = {__classid(TBMWave)};
      RegisterPropertyEditor(__typeinfo(TBMWaveData), 0L, "", __classid(TBMWaveDataProperty));
      RegisterComponentEditor(classes[0],__classid(TBMWaveEditor));
    }
}

