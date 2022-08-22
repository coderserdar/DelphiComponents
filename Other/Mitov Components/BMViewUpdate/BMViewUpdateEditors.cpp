//---------------------------------------------------------------------------


#pragma hdrstop

#include "BMViewUpdateEditors.h"
#include "BMViewUpdate.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

#if ( __BORLANDC__ >= 0x0560 )
    #include <DesignEditors.hpp>
#else
    #include <dsgnintf.hpp>
#endif

#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__
  #include <DesignIntf.hpp>
  typedef _di_IDesigner TAFormDesigner;

#else
  typedef _di_IFormDesigner TAFormDesigner;

#endif


#if (__BORLANDC__ >= 0x0550)
  #define __BCB_50__
#endif

#if (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif

//---------------------------------------------------------------------------
namespace Bmviewupdateeditors
{
//---------------------------------------------------------------------------
class PACKAGE TGroupViewProperty :
  public TComponentProperty
{
  typedef TComponentProperty inherited;

public :
  virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
  virtual void __fastcall SetValue(const System::String Value);

public:
  inline void __fastcall  SetValue(const WideString Value){ TPropertyEditor::SetValue(Value); }

  bool  __fastcall IsItMe ( TBMGroupViewUpdate *GroupView );
  bool  __fastcall ContainsMe ( TBMGroupViewUpdate *GroupView );

   __fastcall virtual ~TGroupViewProperty (void) { }
#ifdef __BCB_40__
   __fastcall TGroupViewProperty (const TAFormDesigner ADesigner, int APropCount) : TComponentProperty( ADesigner, APropCount ) { }
#else
   __fastcall TGroupViewProperty (void) : Dsgnintf::TComponentProperty() { }
#endif
};
//---------------------------------------------------------------------------
bool  __fastcall TGroupViewProperty::IsItMe ( TBMGroupViewUpdate *GroupView )
{
  for ( int j = 0; j < PropCount; j ++ )
    if ( GroupView == GetComponent ( j ) )
      return true;

  return false;
}
//---------------------------------------------------------------------------
bool  __fastcall TGroupViewProperty::ContainsMe ( TBMGroupViewUpdate *GroupView )
{
  if ( ! GroupView->GroupViewUpdate )
    return false;

  if ( IsItMe ( GroupView->GroupViewUpdate ))
    return true;

  return ContainsMe ( GroupView->GroupViewUpdate ); 
}
//---------------------------------------------------------------------------
#ifdef     __BCB_50__
class TMyTmpPersistentClass :
  public TPersistent
{
public :
  DYNAMIC TPersistent* __fastcall GetOwner(void)
    {
    return TPersistent::GetOwner();
    }

};
#endif
//---------------------------------------------------------------------------
void __fastcall TGroupViewProperty::GetValues(Classes::TGetStrProc Proc)
{
#ifdef     __BCB_50__
  if ( GetComponent (0) && ((TMyTmpPersistentClass *)GetComponent (0))->GetOwner () )
    {
    TComponent *Owner = dynamic_cast <TComponent *> (((TMyTmpPersistentClass *)GetComponent (0))->GetOwner ());
    if ( Owner )
      {
      for ( int i = 0; i < Owner->ComponentCount; i ++ )
        {
        TBMGroupViewUpdate *ThreadGroup = dynamic_cast <TBMGroupViewUpdate *> ( Owner->Components [ i ] );
        if ( ThreadGroup )
          {
          if ( ! IsItMe ( ThreadGroup ) )
            if ( ! ContainsMe ( ThreadGroup ))
              Proc ( ThreadGroup->Name );
          }
        }
      }
    }
#else
  for ( int i = 0; i < Designer->Form->ComponentCount; i ++ )
    {
    TBMGroupViewUpdate *GroupView = dynamic_cast <TBMGroupViewUpdate *> ( Designer->Form->Components [ i ] );
    if ( GroupView )
      {
      if ( ! IsItMe ( GroupView ) )
        if ( ! ContainsMe ( GroupView ))
          Proc ( GroupView->Name );
          
      }
    }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TGroupViewProperty::SetValue( const System::String Value )
{
  inherited::SetValue(Value);
}
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
        RegisterPropertyEditor(__typeinfo(TBMCustomViewUpdate), 0L, "", __classid(TGroupViewProperty));
    }
}

