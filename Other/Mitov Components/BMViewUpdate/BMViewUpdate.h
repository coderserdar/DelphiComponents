/*> Ver: V1.4 *********      History      ***************************\

Beta V1.0b0		10/22/1998		Released
Beta V1.0b1		11/06/1998		Property editor added.
Beta V1.0b2		11/16/1998		OnDelete handling has been added.
Beta V1.0b3		11/17/1998		OnChange fixes.
Beta V1.0b4		12/06/1998		ReactOnExternalChange's been added.
Beta V1.0b5		04/15/1999		BCB 4.0 Support.
Beta V1.0b6		11/15/2000		A new function Validate has been added.

V1.1		    03/18/2001		ValidateInterval has been added.
V1.2		    12/06/2004		BCB 6.0 Support.
V1.3		    03/24/2008		C++ Builder 2006 and C++ Builder 2007 Support.
V1.4		    11/21/2008		C++ Builder 2009 Support.


Legal issues: Copyright (C) 1997 - 2008 by Boian Mitov
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

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef BMViewUpdateH
#define BMViewUpdateH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <comctrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------

class TBMMessageHook;
class TBMGroupViewUpdate;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TFocusingEvent)(System::TObject* Sender, System::TObject* FocusFrom, System::TObject* OldItem, String OldCaption, System::TObject* FocusTo, System::TObject* NewItem, String NewCaption );

typedef void __fastcall (__closure *TValidationEvent)(System::TObject* Sender, System::TObject* Item, String Caption );
typedef void __fastcall (__closure *TUpdatingEvent)(System::TObject* Sender, System::TObject* OldItem, String OldCaption, System::TObject* NewItem, String NewCaption );
typedef void __fastcall (__closure *TItemsCompareEvent)(System::TObject* Sender, System::TObject* OldItem, String OldCaption, System::TObject* NewItem, String NewCaption, bool &Changed );

typedef void __fastcall (__closure *TTreeValidationEvent)(System::TObject* Sender, TTreeNode *Item );
typedef void __fastcall (__closure *TTreeUpdatingEvent)(System::TObject* Sender, TTreeNode *OldItem, TTreeNode *NewItem );
typedef void __fastcall (__closure *TTreeItemsCompareEvent)(System::TObject* Sender, TTreeNode *OldItem, TTreeNode *NewItem, bool &Changed );

typedef void __fastcall (__closure *TListValidationEvent)(System::TObject* Sender, TListItem *Item );
typedef void __fastcall (__closure *TListUpdatingEvent)(System::TObject* Sender, TListItem *OldItem, TListItem *NewItem );
typedef void __fastcall (__closure *TListItemsCompareEvent)(System::TObject* Sender, TListItem *OldItem, TListItem *NewItem, bool &Changed );
//---------------------------------------------------------------------------
class PACKAGE TBMCustomViewUpdate : public TComponent
{
private:
  typedef TComponent inherited;

protected:
  bool                  FEnabled;
  bool                  FValidated;

  TBMGroupViewUpdate   *FGroupViewUpdate;

  System::TObject      *FreezItem;
  String            FreezCaption;

  System::TObject      *OldItem;
  System::TObject      *NewItem;
  String            OldCaption;
  String            NewCaption;

  bool                  BeenRemoved;

protected:
  TTimer               *FUpdateTimer;
  TTimer               *FValidateTimer;

protected:
  virtual void __fastcall OnUpdateTimer (System::TObject* Sender);
  virtual void __fastcall OnValidateTimer (System::TObject* Sender);

  virtual void __fastcall WasChanged ( System::TObject* NewItem, String NewCaption );
  virtual void __fastcall WasRemoved ();

  virtual   void      __fastcall Notification(TComponent* AComponent, TOperation Operation );


  virtual   bool    __fastcall Assigned () { return false; }

  virtual   bool __fastcall DoCompare () { return false; }

  virtual   void __fastcall DoUpdate () {}
  virtual   void __fastcall DoValidate () {}
  virtual   void __fastcall DoInvalidate ( System::TObject* Item, String Caption ) = 0;

  virtual   String __fastcall GetCurrentCaption () { return ""; }
  virtual   System::TObject  * __fastcall GetCurrentItem () { return NULL; }
  virtual   System::TObject  * __fastcall GetCurrentSource () { return NULL; }

protected:
  void      __fastcall SetTimeInterval ( int TimeInterval );
  int       __fastcall GetTimeInterval ();
  void      __fastcall SetValidateInterval ( int TimeInterval );
  int       __fastcall GetValidateInterval ();

  void      __fastcall SetGroupViewUpdate ( TBMGroupViewUpdate *GroupViewUpdate );

  void      __fastcall SetEnabled ( bool Enabled );
  void      __fastcall SetValidated ( bool Value );

  virtual   void __fastcall DoEnable ( bool Enabled );


  void      __fastcall ResetTimers (); 

public:
    __fastcall TBMCustomViewUpdate(TComponent* Owner);
    __fastcall ~TBMCustomViewUpdate();

public:
  void __fastcall Validate();

public:
  __property int                ValidateInterval   = { read = GetValidateInterval, write = SetValidateInterval, default = 1000 };
  __property int                TimeInterval       = { read = GetTimeInterval, write = SetTimeInterval, default = 1000 };
  __property bool               Enabled            = { read = FEnabled, write = SetEnabled, default = true };
  __property bool               Validated          = { read = FValidated, write = SetValidated };

  __property TBMGroupViewUpdate *GroupViewUpdate    = { read = FGroupViewUpdate, write = SetGroupViewUpdate };
  
protected :
};
//---------------------------------------------------------------------------
class PACKAGE TBMCustomListTreeGroupViewUpdate : public TBMCustomViewUpdate
{
public:
  enum TCompareType { ctItem, ctText, ctCustom };

private:
protected:
  TCompareType      FCompareType;

protected:
  TBMMessageHook  *FMessageHook;

protected:
  virtual void __fastcall AppSendMessage ( System::TObject* Sender, HWND WindowHandle, TMessage &Message ) {}
  virtual void __fastcall OnSendMessage ( TObject *Sender, TMessage &Message );

  virtual   bool __fastcall DoCompare ();
  
  virtual   bool __fastcall HasCustomCompare () { return false; }

  virtual   bool __fastcall DoCustomCompare () { return false; }

public:
  __fastcall TBMCustomListTreeGroupViewUpdate(TComponent* Owner);
  __fastcall ~TBMCustomListTreeGroupViewUpdate();

public:
  __property TCompareType    CompareType = { read = FCompareType, write = FCompareType, default = ctItem };
};
//---------------------------------------------------------------------------
class PACKAGE TBMGroupViewUpdate : public TBMCustomListTreeGroupViewUpdate
{
protected:
  System::TObject          *FCurrentSource;

  bool                      FDetectChangeFocus;
  bool                      FInUpdate;

  TFocusingEvent            FOnSetFocus;
  TFocusingEvent            FOnLostFocus;

  TItemsCompareEvent        FOnCustomCompare;

  TValidationEvent          FOnInvalidate;
  TValidationEvent          FOnValidate;
  TUpdatingEvent            FOnUpdate;
  TUpdatingEvent            FOnChange;
  TValidationEvent          FOnDeleteItem;

protected:

  virtual   bool __fastcall Assigned () { return true; }

  virtual   bool __fastcall HasCustomCompare () { return FOnCustomCompare != NULL; }
  virtual   bool __fastcall DoCustomCompare ();
  virtual   void __fastcall DoUpdate ();
  virtual   void __fastcall DoValidate ();
  virtual   void __fastcall DoInvalidate ( System::TObject* Item, String Caption );

  virtual   String __fastcall GetCurrentCaption ();
  virtual   System::TObject  * __fastcall GetCurrentItem ();
  virtual   System::TObject  * __fastcall GetCurrentSource () { return FCurrentSource; }

public:
  virtual void __fastcall DoChange ( System::TObject* NewItem, String NewCaption );
  virtual void __fastcall DoRemove ();

  virtual void __fastcall DoGetFocus ( System::TObject* ToControl, System::TObject* NewItem, String NewCaption );
  virtual void __fastcall DoLostFocus ( System::TObject* ToControl, System::TObject* NewItem, String NewCaption );

public:
    __fastcall TBMGroupViewUpdate(TComponent* Owner);

__published:
  __property ValidateInterval;
  __property TimeInterval;
  __property Enabled;
  __property Validated;
  __property CompareType;
  __property GroupViewUpdate;
  __property bool                 DetectChangeFocus = { read=FDetectChangeFocus, write=FDetectChangeFocus };

  __property TFocusingEvent       OnSetFocus        = { read=FOnSetFocus,        write=FOnSetFocus        };
  __property TFocusingEvent       OnLostFocus       = { read=FOnLostFocus,       write=FOnLostFocus       };
  __property TValidationEvent     OnInvalidate      = { read=FOnInvalidate,      write=FOnInvalidate      };
  __property TValidationEvent     OnValidate        = { read=FOnValidate,        write=FOnValidate        };
  __property TUpdatingEvent       OnUpdate          = { read=FOnUpdate,          write=FOnUpdate          };
  __property TItemsCompareEvent   OnCustomCompare   = { read=FOnCustomCompare,   write=FOnCustomCompare   };
  __property TUpdatingEvent       OnChange          = { read=FOnChange,          write=FOnChange          };
  __property TValidationEvent     OnDeleteItem      = { read=FOnDeleteItem,      write=FOnDeleteItem      };
};
//---------------------------------------------------------------------------
class PACKAGE TBMCustomListTreeViewUpdate : public TBMCustomListTreeGroupViewUpdate
{
private:
  typedef TBMCustomListTreeGroupViewUpdate inherited;

protected:
  bool                     FReactOnExternalChange;

public:
    __fastcall TBMCustomListTreeViewUpdate(TComponent* Owner);

public:
  __property bool            ReactOnExternalChange = { read = FReactOnExternalChange, write = FReactOnExternalChange, default = false };

};
//---------------------------------------------------------------------------
class PACKAGE TBMTreeViewUpdate : public TBMCustomListTreeViewUpdate
{
private:
  typedef TBMCustomListTreeViewUpdate inherited;
  
protected:
  TCustomTreeView  *FChangeSource;

  TTreeItemsCompareEvent   FOnCustomCompare;

  TTreeValidationEvent     FOnTreeInvalidate;
  TTreeValidationEvent     FOnTreeValidate;
  TTreeUpdatingEvent       FOnTreeUpdate;

protected:

protected:
  virtual void __fastcall Notification(TComponent* AComponent, TOperation Operation);

  virtual void __fastcall AppSendMessage ( System::TObject* Sender, HWND WindowHandle, TMessage &Message );


  virtual   bool    __fastcall Assigned () { return FChangeSource != NULL; }

  virtual   bool __fastcall HasCustomCompare () { return FOnCustomCompare != NULL; }
  virtual   bool __fastcall DoCustomCompare ();
  virtual   void __fastcall DoUpdate ();
  virtual   void __fastcall DoValidate ();
  virtual   void __fastcall DoInvalidate ( System::TObject* Item, String Caption );

  virtual   String __fastcall GetCurrentCaption ();
  virtual   System::TObject  * __fastcall GetCurrentItem ();
  virtual   System::TObject  * __fastcall GetCurrentSource ();
  
protected:
  void      __fastcall SetChangeSource ( TCustomTreeView *ChangeSource );

public:
    __fastcall TBMTreeViewUpdate(TComponent* Owner);

__published:
  __property ValidateInterval;
  __property TimeInterval;
  __property Enabled;
  __property Validated;
  __property CompareType;
  __property GroupViewUpdate;
  __property ReactOnExternalChange;
  __property TCustomTreeView         *ChangeSource      = { read = FChangeSource, write = SetChangeSource };

  __property TTreeValidationEvent     OnTreeInvalidate  = {read=FOnTreeInvalidate,write=FOnTreeInvalidate};
  __property TTreeValidationEvent     OnTreeValidate    = {read=FOnTreeValidate,write=FOnTreeValidate};
  __property TTreeUpdatingEvent       OnTreeUpdate      = {read=FOnTreeUpdate,write=FOnTreeUpdate};
  __property TTreeItemsCompareEvent   OnCustomCompare   = {read=FOnCustomCompare,write=FOnCustomCompare};
};
//---------------------------------------------------------------------------
class PACKAGE TBMListViewUpdate : public TBMCustomListTreeViewUpdate
{
private:
  typedef TBMCustomListTreeViewUpdate inherited;

protected:
  TCustomListView          *FChangeSource;

  TListItemsCompareEvent    FOnCustomCompare;

  TListValidationEvent      FOnListInvalidate;
  TListValidationEvent      FOnListValidate;
  TListUpdatingEvent        FOnListUpdate;

protected:

protected:
  virtual void __fastcall Notification(TComponent* AComponent, TOperation Operation);

  virtual void __fastcall AppSendMessage ( System::TObject* Sender, HWND WindowHandle, TMessage &Message );

  virtual   bool __fastcall Assigned () { return FChangeSource != NULL; }

  virtual   bool __fastcall HasCustomCompare () { return FOnCustomCompare != NULL; }
  virtual   bool __fastcall DoCustomCompare ();
  virtual   void __fastcall DoUpdate ();
  virtual   void __fastcall DoValidate ();
  virtual   void __fastcall DoInvalidate ( System::TObject* Item, String Caption );

  virtual   String __fastcall GetCurrentCaption ();
  virtual   System::TObject  * __fastcall GetCurrentItem ();
  virtual   System::TObject  * __fastcall GetCurrentSource ();
  
protected:
  void      __fastcall SetChangeSource ( TCustomListView *ChangeSource );

public:
    __fastcall TBMListViewUpdate(TComponent* Owner);

__published:
  __property ValidateInterval;
  __property TimeInterval;
  __property Enabled;
  __property Validated;
  __property CompareType;
  __property GroupViewUpdate;
  __property ReactOnExternalChange;
  __property TCustomListView         *ChangeSource      = { read = FChangeSource, write = SetChangeSource };

  __property TListValidationEvent     OnListInvalidate  = {read=FOnListInvalidate,write=FOnListInvalidate};
  __property TListValidationEvent     OnListValidate    = {read=FOnListValidate,write=FOnListValidate};
  __property TListUpdatingEvent       OnListUpdate      = {read=FOnListUpdate,write=FOnListUpdate};
  __property TListItemsCompareEvent   OnCustomCompare   = {read=FOnCustomCompare,write=FOnCustomCompare};
};
//---------------------------------------------------------------------------
#endif
