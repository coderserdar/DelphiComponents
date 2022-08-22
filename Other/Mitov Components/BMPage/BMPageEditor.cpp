//---------------------------------------------------------------------------


#pragma hdrstop

#include "BMPageEditor.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

#include "BMPAGE.h"
//---------------------------------------------------------------------------
#if ( __BORLANDC__ >= 0x0560 )
  #include <DesignEditors.hpp>
  #define __BCB_60__
  typedef _di_IDesigner TAFormDesigner;

#else
  #include <dsgnintf.hpp>
  typedef _di_IFormDesigner TAFormDesigner;

#endif

//---------------------------------------------------------------------------
namespace Bmpage
{
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//--- COMPONENT EDITOR ---------------------------------------------------------
// Provide a new PageControl component editor so that the glyphs are handled
// correctly.  Provides the same functions as the ancestor property editor:
// new, next and previous.  Enables glyphs to appear at design time.

class PACKAGE TBMPageControlEditor : public TDefaultEditor
//class PACKAGE TBMPageControlEditor : public TComponentEditor
{
public :
    __fastcall virtual TBMPageControlEditor ( Classes::TComponent* AComponent, TAFormDesigner ADesigner ) :
                TDefaultEditor(AComponent, ADesigner) { }
    virtual void __fastcall ExecuteVerb ( int Index );
    virtual String __fastcall GetVerb( int Index );
    virtual int __fastcall GetVerbCount();
};
//---------------------------------------------------------------------------
void __fastcall TBMPageControlEditor::ExecuteVerb ( int Index )
{
  TBMPageControl    *PageControl;
  TBMTabSheet       *Page;
  TAFormDesigner Designer;

  if ( dynamic_cast<TBMTabSheet *> ( Component ))
    PageControl = dynamic_cast<TBMPageControl*>( dynamic_cast<TBMTabSheet *> (Component)->PageControl );

  else
    PageControl = dynamic_cast<TBMPageControl*> ( Component );

  if ( PageControl )
    {
    Designer = TDefaultEditor::Designer;
    if ( Index == 0 )
      {

#if ( __BORLANDC__ >= 0x0560 )
      Page = new TBMTabSheet( Designer->Root );
#else
      Page = new TBMTabSheet ( Designer->Form );
#endif
      try
        {
        Page->Name = Designer->UniqueName( Page->ClassName() );
        Page->Parent = PageControl;
        Page->PageControl = PageControl;
        }

      catch ( ... )
        {
        delete Page;
        }

      PageControl->ActivePage = Page;
      }

    else
      {
      Page = (TBMTabSheet *)( PageControl->FindNextPage ( PageControl->ActivePage, Index == 1, false ));
      if ( Page && (Page != PageControl->ActivePage ))
        PageControl->ActivePage = Page;
      }

#if ( __BORLANDC__ >= 0x0570 )
    _di_IDesignObject ADesignObject;
    Page->GetInterface(ADesignObject);

    Designer->SelectComponent(ADesignObject);
#else
    Designer->SelectComponent(Page);
#endif
    Designer->Modified();
    }
}
//---------------------------------------------------------------------------
String __fastcall TBMPageControlEditor::GetVerb( int Index )
{
  switch ( Index )
    {
    case 0 : return "New (BM)page";
    case 1 : return "Next page";
    default :
    case 2 : return "Previous page";
    }
}
//---------------------------------------------------------------------------
int __fastcall TBMPageControlEditor::GetVerbCount()
{
  return 3;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
};
//---------------------------------------------------------------------------
namespace Bmpageeditor
{
//---------------------------------------------------------------------------
void __fastcall PACKAGE Register()
{
  RegisterComponentEditor(__classid(TBMPageControl), __classid(TBMPageControlEditor));
  RegisterComponentEditor(__classid(TBMTabSheet), __classid(TBMPageControlEditor));
}
//---------------------------------------------------------------------------
};
