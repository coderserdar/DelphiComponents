//---------------------------------------------------------------------------
#include <vcl.h>
#include <assert.h>
#pragma hdrstop

#include "BMViewUpdate.h"
#include "BMMessageHook.h"
#pragma package(smart_init)
#pragma link "BMMessageHook"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TBMTreeViewUpdate *)
{
    new TBMTreeViewUpdate(NULL);
}
//---------------------------------------------------------------------------
__fastcall TBMCustomViewUpdate::TBMCustomViewUpdate(TComponent* Owner)
    : TComponent (Owner)
{
  FUpdateTimer = new TTimer ( this );
  FUpdateTimer->Enabled = false;
  FUpdateTimer->OnTimer = OnUpdateTimer;         
  FUpdateTimer->Interval = 1000;

  FValidateTimer = new TTimer ( this );
  FValidateTimer->Enabled = false;
  FValidateTimer->OnTimer = OnValidateTimer;
  FValidateTimer->Interval = 1000;

  FEnabled = true;
  FValidated = true;

  FGroupViewUpdate = NULL;
  OldItem = NULL;
  NewItem = NULL;
  FreezItem = NULL;
  BeenRemoved = false;
}
//---------------------------------------------------------------------------
__fastcall TBMCustomViewUpdate::~TBMCustomViewUpdate ()
{
  delete FUpdateTimer;
  delete FValidateTimer;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::Validate()
{
  OnUpdateTimer ( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::OnUpdateTimer (System::TObject* Sender)
{
  if ( ComponentState.Contains( csDesigning ))
    return;
    
  if ( ! Assigned () )
    return;

  FUpdateTimer->Enabled = false;
  FValidateTimer->Enabled = false;

  if ( ! FEnabled )
    return;

  NewItem = GetCurrentItem ();
  NewCaption = GetCurrentCaption ();

  if ( BeenRemoved || DoCompare () )
    DoUpdate ();

  else
    DoValidate ();

  BeenRemoved = false;
  FValidated = true;
  OldItem = GetCurrentItem ();
  OldCaption = GetCurrentCaption (); 
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::ResetTimers ()
{
  FUpdateTimer->Enabled = false;
  FUpdateTimer->Enabled = true;
  FValidateTimer->Enabled = false;
  FValidateTimer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::OnValidateTimer (System::TObject* Sender)
{
  if ( ComponentState.Contains( csDesigning ))
    return;
    
  if ( ! Assigned () )
    return;

  FValidateTimer->Enabled = false;

  if ( ! FEnabled )
    return;

  NewItem = GetCurrentItem ();
  NewCaption = GetCurrentCaption ();

  if ( !( BeenRemoved || DoCompare () ))
    {
    DoValidate ();
    FValidateTimer->Enabled = false;
    
    BeenRemoved = false;
    FValidated = true;
    OldItem = GetCurrentItem ();
    OldCaption = GetCurrentCaption ();
    }
    
}
//---------------------------------------------------------------------------
void      __fastcall TBMCustomViewUpdate::SetGroupViewUpdate ( TBMGroupViewUpdate *GroupViewUpdate )
{
  if ( GroupViewUpdate == FGroupViewUpdate )
    return;

  FGroupViewUpdate = GroupViewUpdate;

  if ( FGroupViewUpdate )
    FGroupViewUpdate->FreeNotification ( this );

}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::Notification(TComponent* AComponent, TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  if ( dynamic_cast<TBMGroupViewUpdate *> ( AComponent ) && Operation == opRemove && AComponent == FGroupViewUpdate )
    SetGroupViewUpdate ( NULL );
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::SetEnabled ( bool Enabled )
{
  if ( FEnabled == Enabled )
    return;

  FEnabled = true;
  DoEnable ( Enabled );

  FEnabled = Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::SetValidated ( bool Value )
{
  if( Value == FValidated )
    return;
    
  if( Value )
    Validate();
    
  else
    {
    System::TObject *Item = GetCurrentItem();
    String Caption = GetCurrentCaption();
    DoInvalidate( Item, Caption ); 
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::DoEnable ( bool Enabled )
{
  if ( Enabled )
    DoInvalidate ( FreezItem, FreezCaption );

  else
    {
    OnUpdateTimer ( this );
    FreezItem = OldItem;
    FreezCaption = OldCaption;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::SetTimeInterval ( int TimeInterval )
{
  FUpdateTimer->Interval = (Cardinal)TimeInterval;
}
//---------------------------------------------------------------------------
int  __fastcall TBMCustomViewUpdate::GetTimeInterval ()
{
  return (int)FUpdateTimer->Interval;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::SetValidateInterval ( int TimeInterval )
{
  FValidateTimer->Interval = (Cardinal)TimeInterval;
}
//---------------------------------------------------------------------------
int  __fastcall TBMCustomViewUpdate::GetValidateInterval ()
{
  return (int)FValidateTimer->Interval;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::WasChanged ( System::TObject* NewItem, String NewCaption )
{
  if ( FGroupViewUpdate )
    FGroupViewUpdate->DoChange ( NewItem, NewCaption );
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomViewUpdate::WasRemoved ()
{
  BeenRemoved = true;
  if ( FGroupViewUpdate )
    FGroupViewUpdate->DoRemove ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMCustomListTreeGroupViewUpdate::TBMCustomListTreeGroupViewUpdate(TComponent* Owner)
    : TBMCustomViewUpdate (Owner)
{
  FMessageHook = new TBMMessageHook ( this );
  FMessageHook->OnAppSendMessage = AppSendMessage;
  FMessageHook->OnSendMessage = OnSendMessage;

  FCompareType = ctItem;
}
//---------------------------------------------------------------------------
__fastcall TBMCustomListTreeGroupViewUpdate::~TBMCustomListTreeGroupViewUpdate ()
{
  FMessageHook->OnAppSendMessage = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMCustomListTreeGroupViewUpdate::OnSendMessage ( TObject *Sender, TMessage &Message )
{
  if ( Message.Msg == WM_SETFOCUS )
    if ( FGroupViewUpdate )
      FGroupViewUpdate->DoGetFocus ( GetCurrentSource (), GetCurrentItem (), GetCurrentCaption () );

  if ( Message.Msg == WM_KILLFOCUS )
    if ( FGroupViewUpdate )
      FGroupViewUpdate->DoLostFocus ( GetCurrentSource (), GetCurrentItem (), GetCurrentCaption () );
}
//---------------------------------------------------------------------------
bool __fastcall TBMCustomListTreeGroupViewUpdate::DoCompare ()
{
  switch ( FCompareType )
    {
    case ctCustom:
      if ( HasCustomCompare () )
//      if ( FOnCustomCompare )
        return DoCustomCompare ();

    case ctItem:
      if ( OldItem != NewItem )
        return true;

      break;

    case ctText:
      if ( OldItem != NewItem && (( ! OldItem ) || ( ! NewItem ) ))
        return true;

      else if ( OldItem && NewItem )
        if ( OldCaption != NewCaption )
          return true;

      break;

    default :
      assert ( NULL ); // Never here !!!
    }
    
  return false;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMCustomListTreeViewUpdate::TBMCustomListTreeViewUpdate(TComponent* Owner)
    : TBMCustomListTreeGroupViewUpdate (Owner)
{
  FReactOnExternalChange = false;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMTreeViewUpdate::TBMTreeViewUpdate(TComponent* Owner)
    : TBMCustomListTreeViewUpdate (Owner)
{
}
//---------------------------------------------------------------------------
System::TObject  * __fastcall TBMTreeViewUpdate::GetCurrentSource ()
{
  return FChangeSource;
}
//---------------------------------------------------------------------------
bool __fastcall TBMTreeViewUpdate::DoCustomCompare ()
{
  bool NeedUpdate = false;
  if ( FOnCustomCompare )
    FOnCustomCompare ( this, (TTreeNode *)OldItem, (TTreeNode *)NewItem, NeedUpdate );

  return NeedUpdate; 
}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::DoUpdate ()
{
  if ( FOnTreeUpdate )
    FOnTreeUpdate ( this, (TTreeNode *)OldItem, (TTreeNode *)NewItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::DoValidate ()
{
  if ( FOnTreeValidate )
    FOnTreeValidate ( this, (TTreeNode *)NewItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::AppSendMessage ( System::TObject* Sender, HWND WindowHandle, TMessage &Message )
{
  if ( ComponentState.Contains( csDesigning ))
    return;

  if ( ! FChangeSource )
    return;

  if ( ! FUpdateTimer )
    return;

//  if ( ! FChangeSource->Parent->HandleAllocated () )
//    return;

//  if ( WindowHandle == FChangeSource->Parent->Handle )
    {
    if ( Message.Msg == WM_NOTIFY )
      {
      NM_TREEVIEW *pInf = (NM_TREEVIEW *)Message.LParam;
      if ( pInf->hdr.hwndFrom == FChangeSource->Handle )
        if ( pInf->hdr.code == TVN_SELCHANGED )
          {
          ResetTimers();
          if ( FEnabled )
            {
            NewItem = GetCurrentItem ();
            NewCaption = GetCurrentCaption ();

            if ( DoCompare () )
              {
              if ( FValidated )
                DoInvalidate ( OldItem, OldCaption );

              FValidated = false;
              }
            }

          if ( FReactOnExternalChange || pInf->action != TVC_UNKNOWN )
            WasChanged ( FChangeSource->Selected, FChangeSource->Selected ? FChangeSource->Selected->Text : (String)"" );
          }

        else if ( pInf->hdr.code == TVN_DELETEITEM )
          {
          if ( OldItem )
            {
            if ( pInf->itemOld.hItem == ((TTreeNode *)OldItem )->ItemId )
              {
              ResetTimers();
              if ( FEnabled )
                {
                NewItem = NULL;
                NewCaption = "";

                if ( DoCompare () )
                  {
                  if ( FValidated )
                    DoInvalidate ( OldItem, OldCaption );

                  FValidated = false;
                  }
                }

              WasRemoved ();
              OldItem = NULL;
              OldCaption = "";
              }
            }
          }

      }
    }
}
//---------------------------------------------------------------------------
System::TObject  * __fastcall TBMTreeViewUpdate::GetCurrentItem ()
{
  if ( FChangeSource )
    return FChangeSource->Selected;

  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::DoInvalidate ( System::TObject* Item, String Caption )
{
  if ( FOnTreeInvalidate )
    FOnTreeInvalidate ( this, (TTreeNode *)OldItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::SetChangeSource ( TCustomTreeView *ChangeSource )
{
  if ( ChangeSource == FChangeSource )
    return;

  FChangeSource = ChangeSource;
  FMessageHook->HookedTo = FChangeSource;

  if ( FChangeSource )
    FChangeSource->FreeNotification ( this );

  else
    {
    FUpdateTimer->Enabled = false;
    FValidateTimer->Enabled = false;
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMTreeViewUpdate::Notification(TComponent* AComponent, TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  
  if ( dynamic_cast<TCustomTreeView *> ( AComponent ) && Operation == opRemove && AComponent == FChangeSource )
    SetChangeSource ( NULL );
}
//---------------------------------------------------------------------------
String __fastcall TBMTreeViewUpdate::GetCurrentCaption ()
{
  System::TObject *Item = GetCurrentItem ();

  if ( Item )
    return ((TTreeNode *)Item )->Text;

  return "";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMListViewUpdate::TBMListViewUpdate(TComponent* Owner)
    : TBMCustomListTreeViewUpdate (Owner)
{
}
//---------------------------------------------------------------------------
System::TObject  * __fastcall TBMListViewUpdate::GetCurrentSource ()
{
  return FChangeSource;
}
//---------------------------------------------------------------------------
bool __fastcall TBMListViewUpdate::DoCustomCompare ()
{
  bool NeedUpdate = false;
  if ( FOnCustomCompare )
    FOnCustomCompare ( this, (TListItem *)OldItem, (TListItem *)NewItem, NeedUpdate );

  return NeedUpdate; 
}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::DoUpdate ()
{
  if ( FOnListUpdate )
    FOnListUpdate ( this, (TListItem *)OldItem, (TListItem *)NewItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::DoValidate ()
{
  if ( FOnListValidate )
    FOnListValidate ( this, (TListItem *)NewItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::AppSendMessage ( System::TObject* Sender, HWND WindowHandle, TMessage &Message )
{
  if ( ComponentState.Contains( csDesigning ))
    return;
    
  if ( ! FChangeSource )
    return;

  if ( ! FUpdateTimer )
    return;

  if ( Message.Msg != WM_NOTIFY )
    return;

//  if ( ! FChangeSource->Parent->HandleAllocated () )
//    return;

//  if ( WindowHandle == FChangeSource->Parent->Handle )
    {
    NM_LISTVIEW *pInf = (NM_LISTVIEW *)Message.LParam;
    if ( pInf->hdr.hwndFrom == FChangeSource->Handle )
      if ( pInf->hdr.code == LVN_ITEMCHANGED )
        {
        if ( pInf->uChanged == LVIF_STATE )
          {
          ResetTimers();
          if ( FEnabled )
            {
            NewItem = GetCurrentItem ();
            NewCaption = GetCurrentCaption ();

            if ( DoCompare () )
              {
              if ( FValidated )
                DoInvalidate ( OldItem, OldCaption );

              FValidated = false;
              }
            }

          if ( FReactOnExternalChange || FChangeSource->Focused () )
            WasChanged ( FChangeSource->ItemFocused, FChangeSource->ItemFocused ? FChangeSource->ItemFocused->Caption : (String)"" );
          }
        }

      else if ( pInf->hdr.code == LVN_DELETEITEM )
        {
        if ( OldItem )
          {
          if ( pInf->iItem == ((TListItem *)OldItem )->Index )
            {
            ResetTimers();
            if ( FEnabled )
              {
              NewItem = NULL;
              NewCaption = "";

              if ( DoCompare () )
                {
                if ( FValidated )
                  DoInvalidate ( OldItem, OldCaption );

                FValidated = false;
                }
              }

            WasRemoved ();
            OldItem = NULL;
            OldCaption = "";
            }
          }
        }

    }

}
//---------------------------------------------------------------------------
System::TObject  * __fastcall TBMListViewUpdate::GetCurrentItem ()
{
  if ( FChangeSource )
    return FChangeSource->ItemFocused;

  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::DoInvalidate ( System::TObject* Item, String Caption )
{
  if ( FOnListInvalidate )
    FOnListInvalidate ( this, (TListItem *)OldItem );
}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::SetChangeSource ( TCustomListView *ChangeSource )
{
  if ( ChangeSource == FChangeSource )
    return;

  FChangeSource = ChangeSource;
  FMessageHook->HookedTo = FChangeSource;

  if ( FChangeSource )
    FChangeSource->FreeNotification ( this );

  else
    {
    FUpdateTimer->Enabled = false;
    FValidateTimer->Enabled = false;
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMListViewUpdate::Notification(TComponent* AComponent, TOperation Operation )
{
  inherited::Notification( AComponent, Operation );

  if ( dynamic_cast<TCustomTreeView *> ( AComponent ) && Operation == opRemove && AComponent == FChangeSource )
    SetChangeSource ( NULL );
}
//---------------------------------------------------------------------------
String __fastcall TBMListViewUpdate::GetCurrentCaption ()
{
  System::TObject *Item = GetCurrentItem ();
  
  if ( Item )
    return ((TListItem *)Item)->Caption;

  return "";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMGroupViewUpdate::TBMGroupViewUpdate(TComponent* Owner)
    : TBMCustomListTreeGroupViewUpdate (Owner)
{
  FInUpdate = false;
  FCurrentSource = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoGetFocus ( System::TObject* ToControl, System::TObject* _NewItem, String _NewCaption )
{
  if ( FOnSetFocus )
    FOnSetFocus ( this, FCurrentSource, OldItem, OldCaption, ToControl, _NewItem, _NewCaption );

  NewItem        = _NewItem;
  NewCaption     = _NewCaption;
  FCurrentSource = ToControl;

  if ( FDetectChangeFocus )
    {
    if ( DoCompare () )
      {
      ResetTimers();
      if ( FEnabled )
        {
        if ( FValidated )
          DoInvalidate ( OldItem, OldCaption );

        FValidated = false;
        }
      }
    }

  if ( FValidated )
    {
    OldItem        = _NewItem;
    OldCaption     = _NewCaption;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoLostFocus ( System::TObject* ToControl, System::TObject* _NewItem, String _NewCaption )
{
  if ( FOnLostFocus )
    FOnLostFocus ( this, FCurrentSource, OldItem, OldCaption, ToControl, _NewItem, _NewCaption );
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoChange ( System::TObject* _NewItem, String _NewCaption )
{
  if ( ! FInUpdate )
    {
    NewItem = _NewItem;
    NewCaption = _NewCaption;


    if ( FEnabled )
      {
      if ( FOnChange )
        FOnChange ( this, OldItem, OldCaption, NewItem, NewCaption );

      if ( DoCompare () )
        {
        if ( FValidated )
          {
          if ( FOnInvalidate )
            FOnInvalidate ( this, OldItem, OldCaption );

          FValidated = false;
          }
        }
      }

    ResetTimers();
    }

  WasChanged ( _NewItem, _NewCaption );
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoRemove ()
{
  ResetTimers();

  NewItem = NULL;
  NewCaption = "";

  if ( FEnabled )
    if ( FOnDeleteItem )
      FOnDeleteItem ( this, OldItem, OldCaption );

  OldItem = NULL;
  OldCaption = "";

  WasRemoved ();
}
//---------------------------------------------------------------------------
bool __fastcall TBMGroupViewUpdate::DoCustomCompare ()
{
  bool NeedUpdate = false;
  if ( FOnCustomCompare )
    FOnCustomCompare ( this, OldItem, OldCaption, NewItem, NewCaption, NeedUpdate );

  return NeedUpdate;
}
//---------------------------------------------------------------------------
System::TObject  * __fastcall TBMGroupViewUpdate::GetCurrentItem ()
{
  return NewItem;
}
//---------------------------------------------------------------------------
String __fastcall TBMGroupViewUpdate::GetCurrentCaption ()
{
  return NewCaption;
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoUpdate ()
{
  FInUpdate = true;
  if ( FOnUpdate )
    FOnUpdate ( this, OldItem, OldCaption, NewItem, NewCaption );
    
  FInUpdate = false;
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoValidate ()
{
  if ( FOnValidate )
    FOnValidate ( this, NewItem, NewCaption );
}
//---------------------------------------------------------------------------
void __fastcall TBMGroupViewUpdate::DoInvalidate ( System::TObject* Item, String Caption )
{
  if ( FOnInvalidate )
    FOnInvalidate ( this, Item, Caption );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
namespace Bmviewupdate
{
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
        TComponentClass classes[3] =
          {
          __classid(TBMTreeViewUpdate),
          __classid(TBMListViewUpdate),
          __classid(TBMGroupViewUpdate)
          };

        RegisterComponents("BMitov", classes, 2);
    }
}
//---------------------------------------------------------------------------
