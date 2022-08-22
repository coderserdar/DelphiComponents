//---------------------------------------------------------------------------
#include <vcl.h>
#include <clipbrd.hpp>
#pragma hdrstop

#include "BMSynchroMethods.h"
#include "BMThread.h"
#ifdef __BCB_2007__
  #include "ToolsAPI.hpp"
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#ifdef __BCB_XE2__
#pragma link "system.syncobjs"
#else
#pragma link "syncobjs"
#endif

//#define _MY_TRACE

#ifdef _MY_TRACE
  #include <fstream.h>
#endif

#ifdef __BCB_80__
  #define MaxLongint MaxLongInt
#endif

namespace Bmsynchromethods
{
#ifdef _MY_TRACE
int DebugOffset = 0;

void __TraceFunc ( String _Message )
{
  ofstream of ( "c:\\Trece.txt", ios::app );
  of << _Message.c_str() << "\n";
}

class TMiniTrace
{
  String Message;

public :
  TMiniTrace ( String _Message ) :
    Message ( _Message )
    {
    ofstream of ( "c:\\Trece.txt", ios::app );
    for ( int i = 0; i < DebugOffset; i ++ )
      of << ' ';

    of << Message.c_str() << " >>\n";
    DebugOffset ++;
    }

  ~TMiniTrace()
    {
    DebugOffset --;
    ofstream of ( "c:\\Trece.txt", ios::app );

    for ( int i = 0; i < DebugOffset; i ++ )
      of << ' ';

    of << Message.c_str() << " <<\n";
    }
};

  #define TR(A)   TMiniTrace __Trace ( A )
  #define TRS(A) __TraceFunc ( A )
#else
  #define TRS(A)
  #define TR(A)
#endif

//---------------------------------------------------------------------------
static TSynchroMethodsForm  *DesignForm = NULL;

class TLocalInit
{

public :
  TLocalInit ()
    {
#ifdef _MY_TRACE
//    ofstream of ( "c:\\Trece.txt", ios::trunc );
#endif

    TR( "TLocalInit" );
    }                                              

  ~TLocalInit ()
    {
    TR( "~TLocalInit" );
      if ( DesignForm )
        {
        TR ( "deleting ...." );
        delete DesignForm;
        DesignForm = NULL;
        }
    }
};

TLocalInit LocalInit;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class PACKAGE TThreadGroupProperty :
  public TComponentProperty
{
  typedef TComponentProperty inherited;

public :
  virtual void __fastcall GetValues(Classes::TGetStrProc Proc);

  bool  __fastcall IsItMe ( TBMThreadGroup *ThreadGroup );
  bool  __fastcall ContainsMe ( TBMThreadGroup *ThreadGroup );

   __fastcall virtual ~TThreadGroupProperty (void) { }
#ifdef __BCB_40__
   __fastcall TThreadGroupProperty (const TAFormDesigner ADesigner, int APropCount) : inherited( ADesigner, APropCount ) { }
#else
   __fastcall TThreadGroupProperty (void) : Dsgnintf::TComponentProperty() { }
#endif
};
//---------------------------------------------------------------------------
bool  __fastcall TThreadGroupProperty::IsItMe ( TBMThreadGroup *ThreadGroup )
{
  for ( int j = 0; j < PropCount; j ++ )
    if ( ThreadGroup == GetComponent ( j ) )
      return true;

  return false;
}
//---------------------------------------------------------------------------
bool  __fastcall TThreadGroupProperty::ContainsMe ( TBMThreadGroup *ThreadGroup )
{
  if ( ! ThreadGroup->ThreadGroup )
    return false;

  if ( IsItMe ( ThreadGroup->ThreadGroup ))
    return true;

  return ContainsMe ( ThreadGroup->ThreadGroup );
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
void __fastcall TThreadGroupProperty::GetValues(Classes::TGetStrProc Proc)
{
#ifdef     __BCB_50__
  TR( "TThreadGroupProperty::GetValues" );
//  TRS( (String)"ComponentsCount : " + Designer->ContainerWindow->ComponentCount );
  if ( GetComponent (0) && ((TMyTmpPersistentClass *)GetComponent (0))->GetOwner () )
    {
    TRS( ((TMyTmpPersistentClass *)GetComponent (0))->GetOwner ()->ClassName() );
    TComponent *Owner = dynamic_cast <TComponent *> (((TMyTmpPersistentClass *)GetComponent (0))->GetOwner ());
    if ( Owner )
      {
      for ( int i = 0; i < Owner->ComponentCount; i ++ )
        {
        TRS( (String)"Component : " + Owner->Components [ i ]->Name );
        TBMThreadGroup *ThreadGroup = dynamic_cast <TBMThreadGroup *> ( Owner->Components [ i ] );
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
	TBMThreadGroup *ThreadGroup = dynamic_cast <TBMThreadGroup *> ( Designer->Form->Components [ i ] );
	if ( ThreadGroup )
	  {
	  if ( ! IsItMe ( ThreadGroup ) )
		if ( ! ContainsMe ( ThreadGroup ))
		  Proc ( ThreadGroup->Name );

	  }
	}
#endif
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TFileNotifyEvent)( TFileNotification NotifyCode, const String FileName, bool &Cancel );
typedef void __fastcall (__closure *TEventNotifyEvent)( TEventNotification NotifyCode, bool &Cancel );
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#ifdef __BCB_2007__
class TEventsEditorNotification : public TInterfacedObject, /*public IOTANotifier,*/ public IOTAIDENotifier
{
public:
    TNotifyEvent FOnUpdateNotify;
    TNotifyEvent FOnCloseNotify;

public:
	virtual HRESULT STDMETHODCALLTYPE QueryInterface( REFIID riid, void ** ppvObject) { return TInterfacedObject::QueryInterface( riid, ppvObject); }
	virtual ULONG STDMETHODCALLTYPE AddRef() { return TInterfacedObject::_AddRef(); }
	virtual ULONG STDMETHODCALLTYPE Release() { return TInterfacedObject::_Release(); }

public:
	virtual void __fastcall AfterSave(void)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}

	virtual void __fastcall BeforeSave(void)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}
	
	virtual void __fastcall Destroyed(void)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}

	virtual void __fastcall Modified(void)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}

public:
	virtual void __fastcall FileNotification(TOTAFileNotification NotifyCode, const String FileName, bool &Cancel);
	virtual void __fastcall BeforeCompile(const _di_IOTAProject Project, bool &Cancel)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}

	virtual void __fastcall AfterCompile(bool Succeeded)
	{
	  if( FOnUpdateNotify )
		FOnUpdateNotify(NULL);
	}

public:
//	virtual void __stdcall FileNotification(TFileNotification NotifyCode, const String FileName, bool &Cancel);

	virtual void __stdcall EventNotification(TEventNotification NotifyCode, bool &Cancel)
	{
	  if( FOnUpdateNotify)
	    FOnUpdateNotify(NULL);
	}
	
public:
	inline __fastcall TEventsEditorNotification(void)
		  {
		  }

};         
//---------------------------------------------------------------------------
void __fastcall TEventsEditorNotification::FileNotification(TOTAFileNotification NotifyCode, const String FileName, bool &Cancel)
{
  if(( NotifyCode == fnProjectClosing) || (NotifyCode == fnFileClosing))
	{
	if( FOnCloseNotify )
	  FOnCloseNotify( NULL );

	}

  else
	if ( FOnUpdateNotify )
	  FOnUpdateNotify( NULL );

}
//---------------------------------------------------------------------------
#else
class TEventsEditorNotification : public TIAddInNotifier
{
public:
  TFileNotifyEvent      FOnFileNotify;
  TEventNotifyEvent     FOnEventNotify;

public:
	virtual void __stdcall FileNotification(TFileNotification NotifyCode, const String FileName, bool &Cancel);

	virtual void __stdcall EventNotification(TEventNotification NotifyCode, bool &Cancel)
          {
          if ( FOnEventNotify )
            FOnEventNotify ( NotifyCode, Cancel );
          }
public:
	inline __fastcall TEventsEditorNotification (void)
          {
          ToolServices->AddNotifierEx( this );
          }

public:
	inline __fastcall virtual ~TEventsEditorNotification(void)
          {
          ToolServices->RemoveNotifier( this );
          }

};
//---------------------------------------------------------------------------
void __stdcall TEventsEditorNotification::FileNotification( TFileNotification NotifyCode, const String FileName, bool &Cancel )
{
  if ( FOnFileNotify )
    FOnFileNotify ( NotifyCode, FileName, Cancel );
}
#endif
//---------------------------------------------------------------------------
class TEmptyEntryPropertyEditor : public TPropertyEditor
{
public:
  virtual String __fastcall GetValue();
  virtual void __fastcall Edit(void);
  virtual TPropertyAttributes __fastcall GetAttributes(void);
};
//---------------------------------------------------------------------------
String __fastcall TEmptyEntryPropertyEditor::GetValue()
{
  return "Edit ...";
}
//---------------------------------------------------------------------------
void __fastcall TEmptyEntryPropertyEditor::Edit(void)
{
  if ( ! DesignForm )
	DesignForm = new TSynchroMethodsForm( NULL, Designer );

  DesignForm->SetDesigner ( Designer );


  DesignForm->Show ();
}
//---------------------------------------------------------------------------
TPropertyAttributes __fastcall TEmptyEntryPropertyEditor::GetAttributes(void)
{
  return TPropertyAttributes () << paDialog << paReadOnly;
}
//---------------------------------------------------------------------------
class PACKAGE TBMSynchroEventsEditorHelperClass : public TComponent
{
  TBMThreadSynchroNotifyEvent     __fastcall GetEmptyEvent();
  TBMThreadSynchroDataNotifyEvent __fastcall GetEmptyDataEvent();
  TNotifyEvent                    __fastcall GetEmptyNotifyEvent();
  TPersistent                    *__fastcall GetEmptyEntry();

__published:
  __property TBMThreadSynchroNotifyEvent       ThreadNotifyEventEmptyEvent = { read=GetEmptyEvent };
  __property TBMThreadSynchroDataNotifyEvent   ThreadDataNotifyEventEmptyEvent = { read=GetEmptyDataEvent };
  __property TNotifyEvent                      StdEmptyEvent = { read=GetEmptyNotifyEvent };
  __property TPersistent                      *SynchroMethods = { read=GetEmptyEntry };
};
//---------------------------------------------------------------------------
TBMThreadSynchroNotifyEvent __fastcall TBMSynchroEventsEditorHelperClass::GetEmptyEvent()
{
  return NULL;
}
//---------------------------------------------------------------------------
TBMThreadSynchroDataNotifyEvent __fastcall TBMSynchroEventsEditorHelperClass::GetEmptyDataEvent()
{
  return NULL;
}
//---------------------------------------------------------------------------
TNotifyEvent __fastcall TBMSynchroEventsEditorHelperClass::GetEmptyNotifyEvent()
{
  return NULL;
}
//---------------------------------------------------------------------------
TPersistent *__fastcall TBMSynchroEventsEditorHelperClass::GetEmptyEntry()
{
  return NULL;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
String __fastcall GenerateEventName ( TAFormDesigner Designer, String TamplateName )
{
  String GeneratedName = TamplateName;
  int i = 0;

  do
    {
    i ++;
    GeneratedName = TamplateName + i;
    }

  while ( Designer->MethodExists( GeneratedName ));

  return GeneratedName;
}
//---------------------------------------------------------------------------
String __fastcall GenerateEvent ( TAFormDesigner Designer, String EventPropertyName, String NameTamplate )
{
  PPropInfo PropInfo = ::GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), EventPropertyName );
  String GeneratedName = GenerateEventName ( Designer, NameTamplate );
#ifdef __BCB_2006__
  _di_IEventInfo Intf1( *new TEventInfo( GetTypeData( * ( PropInfo->PropType ))));
  Designer->CreateMethod( GeneratedName, Intf1 );
#else
  Designer->CreateMethod( GeneratedName, GetTypeData( * ( PropInfo->PropType )));
#endif
  Designer->ShowMethod( GeneratedName );

  return GeneratedName;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TThreadEventSruct : public TObject
{
public :
  String Parameter;
  String CallingFormat;
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSynchroMethodsForm::TSynchroMethodsForm(TComponent* Owner, TAFormDesigner _Designer )
        : TForm(Owner),
        Designer ( _Designer )
{
  TR( "TSynchroMethodsForm::TSynchroMethodsForm" );

  UpdateNotifier = new TEventsEditorNotification;
#ifdef __BCB_2007__
  _di_IOTAServices AToolServices( BorlandIDEServices ); 

  _di_IOTAIDENotifier AUpdateNotifier( UpdateNotifier );

  IDENotifierID = AToolServices->AddNotifier( AUpdateNotifier );

  UpdateNotifier->FOnUpdateNotify = OnUpdateNotify;
  UpdateNotifier->FOnCloseNotify = OnCloseNotify;
#else
  UpdateNotifier->FOnFileNotify = OnFileNotify;
  UpdateNotifier->FOnEventNotify = OnEventNotify;
#endif

  ThreadEventsList = new TStringList;
  StdEventsList = new TStringList;

  ValidateComponents();

#ifdef __BCB_40__
  Constraints->MinHeight = 350;
  Constraints->MinWidth = 400;
#endif
}
//---------------------------------------------------------------------------
__fastcall TSynchroMethodsForm::~TSynchroMethodsForm()
{
  TR( "TSynchroMethodsForm::~TSynchroMethodsForm" );

  ThreadEventsListView->OnChange = NULL;
  StdEventsListView->OnChange = NULL;

//  for ( int i = 0; i < StdEventsList->Count; i ++ )
//    delete ((TStdEventSruct)StdEventsList->Items [ i ] );

  ClearLists ();

  delete StdEventsList;
  delete ThreadEventsList;

#ifdef __BCB_2007__
  _di_IOTAServices AToolServices( BorlandIDEServices );
  AToolServices->RemoveNotifier( IDENotifierID );
#else
  delete UpdateNotifier;
#endif
}
//---------------------------------------------------------------------------
#ifdef __BCB_2007__
void __fastcall TSynchroMethodsForm::OnUpdateNotify( TObject *Sender )
{
  DesignForm->UpdateView();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::OnCloseNotify( TObject *Sender )
{
  Designer = NULL;
  Hide();
}
//---------------------------------------------------------------------------
#else
void __fastcall TSynchroMethodsForm::OnFileNotify  ( TFileNotification NotifyCode, const String FileName, bool &Cancel )
{
  if ( NotifyCode == fnProjectClosing || NotifyCode == fnFileClosing )
    {
    Designer = NULL;
    Hide ();
    }

  else
    DesignForm->UpdateView ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::OnEventNotify ( TEventNotification NotifyCode, bool &Cancel )
{
  DesignForm->UpdateView ();
}
//---------------------------------------------------------------------------
#endif
void __fastcall TSynchroMethodsForm::ClearLists ()
{
  for ( int i = 0; i < ThreadEventsList->Count; i ++ )
	delete ((TThreadEventSruct *)ThreadEventsList->Objects [ i ] );

  ThreadEventsList->Clear ();
  StdEventsList->Clear ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::Timer1Timer(TObject *Sender)
{
  UpdateView ();
  ValidateComponents ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::FormResize(TObject *Sender)
{
  ThreadEventsListView->Column [ 1 ]->Width = ThreadEventsListView->ClientWidth - ThreadEventsListView->Column [ 0 ]->Width;
  StdEventsListView->Column [ 1 ]->Width = StdEventsListView->ClientWidth - StdEventsListView->Column [ 0 ]->Width;
}
//---------------------------------------------------------------------------void __fastcall TSynchroMethodsForm::ThreadEventsListViewMouseUp(
void __fastcall TSynchroMethodsForm::ThreadEventsListViewMouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y)
{
  FormResize( Sender );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::FormShow(TObject *Sender)
{
  FormResize( Sender );
  ValidateComponents ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TListItem * __fastcall TSynchroMethodsForm::AddMethodNoDataItem ( const String S )
{
  TR ( "TSynchroMethodsForm::AddMethodNoDataItem" );

  TListItem *Item = ThreadEventsListView->Items->Add ();
  Item->Caption = S;
  Item->SubItems->Add ( "TBMExecuteThread *Thread" );
  Item->SubItems->Add ( "" );

  return Item;
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::AddMethodNoDataEntry ( const String S )
{
  TR ( "TSynchroMethodsForm::AddMethodNoDataEntry" );
  
  TThreadEventSruct *Struct = new TThreadEventSruct;
  Struct->Parameter = "TBMExecuteThread *Thread";
//  Struct->CallingFormat = "";
  ThreadEventsList->AddObject ( S, Struct );

//  AddMethodNoDataItem ( S );
}
//---------------------------------------------------------------------------
TListItem * __fastcall TSynchroMethodsForm::AddMethodDataItem ( const String S )
{
  TR ( "TSynchroMethodsForm::AddMethodDataItem" );

  TListItem *Item = ThreadEventsListView->Items->Add ();
  Item->Caption = S;
  Item->SubItems->Add ( "TBMExecuteThread *Thread, void *&Data" );
  Item->SubItems->Add ( ", & [%Data to pass here]" );

  return Item;
}
//---------------------------------------------------------------------------
TListItem * __fastcall TSynchroMethodsForm::AddStandardNotifyItem ( const String S )
{
  TR ( "TSynchroMethodsForm::AddStandardNotifyItem" );

  TListItem *Item = StdEventsListView->Items->Add ();
  Item->Caption = S;
  Item->SubItems->Add ( "TObject *Sender" );
  Item->SubItems->Add ( "" );

  return Item;
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::AddStandardNotifyEntry ( const String S )
{
  TR ( "TSynchroMethodsForm::AddStandardNotifyEntry" );

  StdEventsList->Add ( S );

//  AddStandardNotifyItem ( S );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::AddMethodDataEntry ( const String S )
{
  TR ( "TSynchroMethodsForm::AddMethodDataEntry" );
  
  TThreadEventSruct *Struct = new TThreadEventSruct;
  Struct->Parameter = "TBMExecuteThread *Thread, void *&Data";
  Struct->CallingFormat = ", & [%Data to pass here]";
  ThreadEventsList->AddObject ( S, Struct );
  
//  AddMethodDataItem ( S );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::ThreadEventsListViewKeyPress(
      TObject *Sender, char &Key)
{
  TR ( "TSynchroMethodsForm::EventsListViewKeyPress" );

  if ( Key == '\r' || Key == ' ' )
    ThreadEventsListViewDblClick ( Sender );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::RefreshButton1Click(TObject *Sender)
{
  UpdateView ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::ThreadEventsListViewDblClick ( TObject *Sender )
{
  TR ( "TSynchroMethodsForm::EventsListViewDblClick" );

  if ( !Designer )
    return;
    
  if ( ActiveListView->ItemFocused )
    {
    Designer->ShowMethod ( ActiveListView->ItemFocused->Caption );
    }

  Show ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::UpdateView ()
{
  TR ( "TSynchroMethodsForm::UpdateView" );

//  if ( ! Visible )
//    return;

  if ( !Designer )
    return;

  TLVChangeEvent OldOnChange;
  
  ClearLists ();

//  if ( PageControl->ActivePage == ThreadSynchroTabSheet )
  PPropInfo PropInfo;
  PropInfo = ::GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), "ThreadNotifyEventEmptyEvent" );

#ifdef __BCB_2006__
  _di_IEventInfo Intf1( *new TEventInfo( GetTypeData( * ( PropInfo->PropType ))));
  Designer->GetMethods( Intf1, AddMethodNoDataEntry );
#else
  Designer->GetMethods( GetTypeData( * ( PropInfo->PropType )), AddMethodNoDataEntry );
#endif
  
  PropInfo = ::GetPropInfo ( __typeinfo(TBMSynchroEventsEditorHelperClass), "ThreadDataNotifyEventEmptyEvent" );
#ifdef __BCB_2006__
  _di_IEventInfo Intf2( *new TEventInfo( GetTypeData( * ( PropInfo->PropType ))));
  Designer->GetMethods( Intf2, AddMethodDataEntry );
#else
  Designer->GetMethods( GetTypeData( * ( PropInfo->PropType )), AddMethodDataEntry );
#endif

  PropInfo = ::GetPropInfo ( __typeinfo(TButton), "OnClick" );
#ifdef __BCB_2006__
  _di_IEventInfo Intf3( *new TEventInfo( GetTypeData( * ( PropInfo->PropType ))));
  Designer->GetMethods( Intf3, AddStandardNotifyEntry );
#else
  Designer->GetMethods( GetTypeData( * ( PropInfo->PropType )), AddStandardNotifyEntry );
#endif

//  ThreadEventsListView->Items->Clear ();
  OldOnChange = ThreadEventsListView->OnChange;
  ThreadEventsListView->OnChange = NULL;

  bool Updated = false;

  for ( int i = 0; i < ThreadEventsListView->Items->Count; i ++ )
    {
    bool Found = false;
    for ( int j = 0; j < ThreadEventsList->Count; j ++ )
      {
      if ( ThreadEventsListView->Items->Item [ i ]->Caption == ThreadEventsList->Strings [ j ] )
        {
        if ( ThreadEventsListView->Items->Item [ i ]->SubItems->Strings [ 0 ] == ((TThreadEventSruct *)ThreadEventsList->Objects [ j ])->Parameter )
          {
          Found = true;
          delete ((TThreadEventSruct *)ThreadEventsList->Objects [ j ]);
          ThreadEventsList->Delete ( j );
          break;
          }
        }
      }

    if ( ! Found )
      {
      if ( ! Updated )
        ThreadEventsListView->Items->BeginUpdate ();
        
      Updated = true;
      delete ThreadEventsListView->Items->Item [ i ];
      i --;
      }
    }

  for ( int i = 0; i < ThreadEventsList->Count; i ++ )
    {
    TListItem *Item = ThreadEventsListView->Items->Add ();
    Item->Caption = ThreadEventsList->Strings [ i ];
    TThreadEventSruct *Struct = ((TThreadEventSruct *)ThreadEventsList->Objects [ i ]);
    Item->SubItems->Add ( Struct->Parameter     );
    Item->SubItems->Add ( Struct->CallingFormat );
    }

  ThreadEventsListView->OnChange = OldOnChange;
  if ( Updated )
    ThreadEventsListView->Items->EndUpdate ();

  Updated = false;
//  StdEventsListView->Items->Clear ();
  OldOnChange = StdEventsListView->OnChange;
  StdEventsListView->OnChange = NULL;

  for ( int i = 0; i < StdEventsListView->Items->Count; i ++ )
    {
    bool Found = false;
    for ( int j = 0; j < StdEventsList->Count; j ++ )
      {
      if ( StdEventsListView->Items->Item [ i ]->Caption == StdEventsList->Strings [ j ] )
        {
        Found = true;
        StdEventsList->Delete ( j );
        break;
        }
      }

    if ( ! Found )
      {
      if ( ! Updated )
        StdEventsListView->Items->BeginUpdate ();
        
      Updated = true;
      delete StdEventsListView->Items->Item [ i ];
      i --;
      }
    }

  for ( int i = 0; i < StdEventsList->Count; i ++ )
    {
    TListItem *Item = StdEventsListView->Items->Add ();
    Item->Caption = StdEventsList->Strings [ i ];
    Item->SubItems->Add ( "TObject *Sender" );
    Item->SubItems->Add ( "" );
    }

  StdEventsListView->OnChange = OldOnChange;
  if ( Updated )
    StdEventsListView->Items->EndUpdate ();

  ValidateComponents ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::PageControlChange(TObject *Sender)
{
  ValidateComponents ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::BtnDataEventClick(TObject *Sender)
{
  TR ( "TSynchroMethodsForm::BtnDataEventClick" );

  if ( !Designer )
    return;

  AddMethodDataItem ( GenerateEvent ( Designer, "ThreadDataNotifyEventEmptyEvent", "SynchroFuncData" ) )->EditCaption();
  FormResize( Sender );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::BtnNoDataEventClick(TObject *Sender)
{
  TR ( "TSynchroMethodsForm::BtnNoDataEventClick" );

  if ( !Designer )
    return;

  AddMethodNoDataItem ( GenerateEvent ( Designer, "ThreadNotifyEventEmptyEvent", "SynchroFunc" ) )->EditCaption();
  FormResize( Sender );
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::BtnNewStdEventClick(TObject *Sender)
{
  TR ( "TSynchroMethodsForm::BtnNoDataEventClick" );

  if ( !Designer )
    return;

  AddStandardNotifyItem ( GenerateEvent ( Designer, "StdEmptyEvent", "NotifyEvent" ) )->EditCaption();
  FormResize( Sender );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::ThreadEventsListViewEdited(TObject *Sender,
      TListItem *Item, String &S )
{
  TR ( "TSynchroMethodsForm::EventsListViewEdited" );

  if ( !Designer )
    return;

  if ( Item->Caption != S )
    {
    if ( Designer->MethodExists( S ))
      {
      S = GenerateEventName ( Designer, S );
      }

    Designer->RenameMethod ( Item->Caption, S );
    }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#ifdef __BCB_2007__
int GetCurrentEditView( IOTASourceEditor *EditorIntf )
{
  int Result = 0;
  
  if( EditorIntf->GetSubViewCount() == 1 )
    // If there is only one edit view, then it is clear that
    // the first one is the current edit view
    return 0;

  else
    {
    String EditorFileName = ExtractFileName( EditorIntf->FileName ).UpperCase();
    HWND WindowIterator = GetWindow(GetDesktopWindow(), GW_CHILD);
    // Iterate over all windows whose owner is the Desktop
    WindowIterator = GetWindow( WindowIterator, GW_HWNDNEXT );
    // If we find a window with the class name "TEditWindow"
    // and the filenames match then this is the currently active
    // edit view
    while(( WindowIterator ) && IsWindow( WindowIterator ))
      {
      TCHAR Buffer[256];
      if( GetClassName(WindowIterator, Buffer, sizeof(Buffer) - 1) == 0 )
        RaiseLastWin32Error();
        
	  if( StrPos( Buffer, TEXT("TEditWindow" )))
        {
        if( GetWindowText(WindowIterator, Buffer, sizeof(Buffer) - 1) == 0 )
          RaiseLastWin32Error();

        StrUpper( Buffer );
        if( StrPos(Buffer, EditorFileName.c_str() ))
          {
          // I the case where there are multiple edit windows
          // open the first one you come to in the iteration process
          // should always be the top-most (or most recently active) edit
          // window - JCH
          // Scan window caption from the end; if we started at the
          // beginning, we might run into the colon of C:\MyFile
          TCHAR *APos = StrRScan( Buffer, ':' );
          ++ APos;
          Result = StrToIntDef(StrPas(APos), -1);
          // Subtract 1 since we need 0..GetViewCount-1 rather than 1..GetViewCount
          if( Result != -1 )
            -- Result;

          break;
          }
        }

      WindowIterator = GetWindow( WindowIterator, GW_HWNDNEXT );
      }
    }

  return Result;
}
//---------------------------------------------------------------------------
#else
int GetCurrentEditView ( TIEditorInterface *EditorIntf )
{
  String EditorFileName;
  HWND WindowIterator;
  char Buffer [256];
  char *APos;

  int Result = -1;
  if ( EditorIntf->GetViewCount () == 1 )
    return 0;

  else
    {
    EditorFileName = ExtractFileName ( EditorIntf->FileName ()).UpperCase();
    WindowIterator = GetWindow(GetDesktopWindow (), GW_CHILD );
    // Iterate over all windows whose owner is the Desktop
    WindowIterator = GetWindow(WindowIterator, GW_HWNDNEXT );

    // If we find a window with the class name "TEditWindow"
    // and the filenames match then this is the currently active
    // edit view
    while ((WindowIterator != 0) && IsWindow(WindowIterator))
      {
      if ( GetClassName(WindowIterator, Buffer, sizeof(Buffer) - 1) == 0 )
        RaiseLastWin32Error ();

      if ( StrPos( Buffer, "TEditWindow" ) != NULL )
        {
        if ( GetWindowText(WindowIterator, Buffer, sizeof(Buffer) - 1) == 0 )
          RaiseLastWin32Error ();

        StrUpper(Buffer);
        if ( StrPos(Buffer, EditorFileName.c_str ()) != NULL )
          {
          // I the case where there are multiple edit windows
          // open the first one you come to in the iteration process
          // should always be the top-most (or most recently active) edit
          // window - JCH
          // Scan window caption from the end; if we started at the
          // beginning, we might run into the colon of C:\MyFile
          APos = StrRScan(Buffer, ':');
          APos ++;
          Result = StrToIntDef(StrPas(APos), -1);
          // Subtract 1 since we need 0..GetViewCount-1 rather than 1..GetViewCount
          if ( Result != -1 )
            Result --;

          break;
          }
        }

      WindowIterator = GetWindow(WindowIterator, GW_HWNDNEXT);
      }
    }

  return Result;
}
#endif
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::CopyCallButton1Click(TObject *Sender)
{
  Clipboard()->AsText = StatusBar->SimpleText;
}
//---------------------------------------------------------------------------
#ifdef __BCB_2007__
void __fastcall TSynchroMethodsForm::GenCallButton1Click(TObject *Sender)
{
  _di_IOTAModuleServices AModuleServices( BorlandIDEServices );

  _di_IOTAModule ModIntf = AModuleServices->CurrentModule();

  if ( ! ModIntf )
    {
    ShowMessage (
      "Please select the editor bofore pressing this button.\n"
      "For now the code will go to the clipboard.\n"
      "You can paste wherever you want."
     );

    Clipboard()->AsText = StatusBar->SimpleText;
    }

  else
    {
    // Get the interface to the source editor.
	IOTASourceEditor *pEditor;

	for( int i = 0; i < ModIntf->GetModuleFileCount(); i ++ )
	  {
	  _di_IOTAEditor EditorIntf = ModIntf->GetModuleFileEditor( i );
	  if( EditorIntf->QueryInterface(IID_IOTASourceEditor, &((void *)pEditor)) == S_OK )
		break;

	  }

	// If the file is not a source file, Editor is nil.
	if( ! pEditor )
	  return;

	_di_IOTASourceEditor Editor( pEditor );

	if( Editor->EditViewCount > 0 )
	  {
	  Editor->Show();
	  int CurrentView = GetCurrentEditView( Editor );
	  _di_IOTAEditView View = Editor->EditViews[ CurrentView ];
	  if( View )
		{
		try
		  {
			TOTAEditPos EditPos = View->CursorPos;
            TOTACharPos CharPos;
          
			View->ConvertPos(true, EditPos, CharPos);
			TOTABlockType OldBlockType = Editor->BlockType;
			Editor->BlockType = Toolsapi::btInclusive;
			Editor->BlockStart = CharPos;
			Editor->BlockAfter = CharPos;
			Editor->BlockVisible = true;
            if ( MessageDlg ( "Insert the code at this location ?", mtConfirmation , TMsgDlgButtons () << mbYes << mbNo, 0 ) == mrYes )
			  {
			  _di_IOTAEditWriter Writer = Editor->CreateUndoableWriter();
			  try
				{
				  Writer->CopyTo( View->CharPosToPos(CharPos));
				  if((CharPos.CharIndex == 0) && (EditPos.Col > 0))
					for( int i = 0; i < EditPos.Col - 1; i ++ )
					  Writer->Insert( " " );

				  AnsiString TmpStr( StatusBar->SimpleText );
				  Writer->Insert( TmpStr.c_str() );
				  Writer->Insert( "\r\n" );
				  for( int i = 0; i < EditPos.Col; i ++ )
                    Writer->Insert( " " );
                
				  Writer->CopyTo( MaxLongint );
				}

			  __finally
				{
				Writer = NULL;
				}

			  Editor->Show();
			  EditPos.Line ++;
			  View->CursorPos = EditPos;
			  }

			Editor->BlockType = OldBlockType;
			Editor->BlockStart = CharPos;
			Editor->BlockAfter = CharPos;
			Editor->BlockVisible = false;
          }

		catch(...)
		  {
		  }
		}
  //          View.Free();
	  }
	}

}
#else
void __fastcall TSynchroMethodsForm::GenCallButton1Click(TObject *Sender)
{
  TIModuleInterface *ModIntf = ToolServices->GetModuleInterface ( ToolServices->GetCurrentFile () );
  if ( ! ModIntf )
    {
    ShowMessage (
      "Please select the editor bofore pressing this button.\n"
      "For now the code will go to the clipboard.\n"
      "You can paste wherever you want."
     );

    Clipboard()->AsText = StatusBar->SimpleText;
    }

  else
    {
    ModIntf->ShowSource ();
	TIEditorInterface *EditorIntf = ModIntf->GetEditorInterface();
	if ( EditorIntf )
	  {
	  if ( EditorIntf->GetViewCount () > 0 )
		{
        int CurrentView = GetCurrentEditView ( EditorIntf );
        TIEditView *View = EditorIntf->GetView ( CurrentView );
        if ( View )
          {
          try
            {
            TCharPos CharPos;
            TEditPos EditPos = View->CursorPos;
            View->ConvertPos ( true, EditPos, CharPos );
            TBlockType OldBlockType = EditorIntf->BlockType;
            EditorIntf->BlockType = btInclusive;
            EditorIntf->BlockStart = CharPos;
            EditorIntf->BlockAfter = CharPos;
            EditorIntf->BlockVisible = true;

            if ( MessageDlg ( "Insert the code at this location ?", mtConfirmation , TMsgDlgButtons () << mbYes << mbNo, 0 ) == mrYes )
              {
              TIEditWriter *Writer = EditorIntf->CreateUndoableWriter ();
              try
                {
                Writer->CopyTo ( View->CharPosToPos ( CharPos ));
                if ( CharPos.CharIndex == 0 && EditPos.Col > 0 )
                for ( int i = 0; i < EditPos.Col - 1; i ++ )
                  Writer->Insert ( " " );

                Writer->Insert ( StatusBar->SimpleText.c_str () );
                Writer->Insert ( "\n\r" );
                for ( int i = 0; i < EditPos.Col; i ++ )
                  Writer->Insert ( " " );

                Writer->CopyTo ( MaxLongint );
                }

              __finally
                {
                Writer->Free ();
                }

              ModIntf->ShowSource ();
              EditPos.Line ++;
              View->CursorPos = EditPos;
              }

            EditorIntf->BlockType = OldBlockType;
            EditorIntf->BlockStart = CharPos;
            EditorIntf->BlockAfter = CharPos;
            EditorIntf->BlockVisible = false;
            }

          catch (...)
            {
            }

          View->Free ();
          }
        }

      EditorIntf->Free ();
      }

    ModIntf->Free ();
    }
    
}
#endif
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::RenameButton1Click(TObject *Sender)
{
  TR ( "TSynchroMethodsForm::RenameButtonClick" );

  if ( ActiveListView->ItemFocused )
    {
    ActiveListView->ItemFocused->EditCaption();
    }
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::ThreadEventsListViewChange(TObject *Sender,
      TListItem *Item, TItemChange Change)
{
  TR ( "TSynchroMethodsForm::EventsListViewChange" );

  ValidateComponents ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::SetDesigner ( TAFormDesigner _Designer )
{
  Designer = _Designer;
  if ( Designer )
    UpdateView ();
}
//---------------------------------------------------------------------------
void __fastcall TSynchroMethodsForm::ValidateComponents ()
{
  TR ( "TSynchroMethodsForm::ValidateComponents" );

  if ( PageControl->ActivePage == ThreadSynchroTabSheet )
    ActiveListView = ThreadEventsListView;

  else
    ActiveListView = StdEventsListView;

  ShowButton1->Enabled = ( ActiveListView->ItemFocused != NULL );
  ShowButton2->Enabled = ( ActiveListView->ItemFocused != NULL );
  RenameButton1->Enabled = ShowButton1->Enabled;
  RenameButton2->Enabled = ShowButton1->Enabled;
  GenCallButton1->Enabled = ShowButton1->Enabled;
  GenCallButton2->Enabled = ShowButton1->Enabled;
  CopyCallButton1->Enabled = ShowButton1->Enabled;
  CopyCallButton2->Enabled = ShowButton1->Enabled;
//  CreateGroupBox->Visible = ( TabControl->TabIndex == 0 );
//  RenameButton->Visible = CreateGroupBox->Visible;

  if ( ActiveListView->ItemFocused )
    StatusBar->SimpleText = (String)"Thread->Synchronize ( &" + ActiveListView->ItemFocused->Caption + ActiveListView->ItemFocused->SubItems->Strings [ 1 ] + " );";

  else
    StatusBar->SimpleText = "";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class PACKAGE TBMThreadControlEditor : public TComponentEditor
{
	typedef TComponentEditor inherited;

public :
	__fastcall virtual TBMThreadControlEditor ( Classes::TComponent* AComponent, TAFormDesigner ADesigner ) :
                inherited ( AComponent, ADesigner )
          {
          }

	__fastcall virtual ~TBMThreadControlEditor ()
          {
#ifndef __BCB_40__
          DesignForm->SetDesigner ( NULL );
          DesignForm->Hide ();
#endif
          }

    virtual void __fastcall Edit(void);
    virtual void __fastcall ExecuteVerb ( int Index );
    virtual String __fastcall GetVerb( int Index );
    virtual int __fastcall GetVerbCount ();
};
//---------------------------------------------------------------------------
void __fastcall TBMThreadControlEditor::Edit (void)
{
  TR ( "TBMThreadControlEditor::Edit" );

  if ( ! DesignForm )
    DesignForm = new TSynchroMethodsForm ( NULL, Designer );

  DesignForm->SetDesigner ( Designer );
  DesignForm->UpdateView ();
  DesignForm->Show ();
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadControlEditor::ExecuteVerb ( int Index )
{
  TR ( (String) "TBMThreadControlEditor::ExecuteVerb : " + Index );

  switch ( Index )
    {
    case 0:
	  GenerateEvent( Designer, "ThreadNotifyEventEmptyEvent", "SynchroFunc" );
	  break;

	case 1:
	  GenerateEvent( Designer, "ThreadDataNotifyEventEmptyEvent", "SynchroFuncData" );
	  break;

	default :
	case 2:
      Edit();
    }

}
//---------------------------------------------------------------------------
String __fastcall TBMThreadControlEditor::GetVerb( int Index )
{
  switch ( Index )
    {
    case 0 : return "New S.M. No Data";
    case 1 : return "New S.M. + Data";
    default :
    case 2 : return "Edit S.M. ...";
    }
}
//---------------------------------------------------------------------------
int __fastcall TBMThreadControlEditor::GetVerbCount ()
{
  return 3;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#ifdef __THREAD_EXPERT
class PACKAGE TBMThreadMethodsMenageExpert : public TIExpert
{
	typedef TIExpert inherited;

public:
	virtual String __stdcall GetName(void) { return "ThreadsSynchroEvents Wizard"; }
	virtual String __stdcall GetAuthor(void) { return "Boian Mitov"; }
	virtual String __stdcall GetComment(void) { return "Threads Synchro Events menagement."; }
	virtual String __stdcall GetPage(void) { return ""; }
	virtual HICON __stdcall GetGlyph(void) { return 0; }
	virtual TExpertStyle __stdcall GetStyle(void) { return esStandard; }
	virtual TExpertState __stdcall GetState(void) { return TExpertState () << esEnabled; }
	virtual String __stdcall GetIDString(void) { return "Boian Mitov.ThreadsSynchroEvents Wizard"; }
	virtual String __stdcall GetMenuText(void) { return "Synchro events..."; }
	virtual void __stdcall Execute(void);

public:
	inline __fastcall TBMThreadMethodsMenageExpert(void) : inherited()
          {
          }

public:
	inline __fastcall virtual ~TBMThreadMethodsMenageExpert(void)
          {
          }

};
//---------------------------------------------------------------------------
void __stdcall TBMThreadMethodsMenageExpert::Execute(void)
{
  TIModuleInterface *ModIntf = ToolServices->GetModuleInterface( ToolServices->GetCurrentFile () );
  if ( ModIntf )
    {
    TIFormInterface *FormIntf = ModIntf->GetFormInterface ();
    if ( FormIntf )
      {
      TIComponentInterface *FormCompIntf = FormIntf->GetFormComponent ();
      if ( FormCompIntf )
        {
        TIComponentInterface *CompIntf = FormIntf->CreateComponent ( FormCompIntf, "TBMThread", 0, 0, -1, 0 );
        if ( CompIntf )
          {
          String Str;
          TPersistent *Value;
          Invoking = true;

          CompIntf->GetPropValueByName ( "SynchroMethods", &Value );
          CompIntf->Delete();
          CompIntf->Free();
          }

        FormIntf->Free();
        }

      FormCompIntf->Free();
      }

    ModIntf->Free();
    }
}
#endif
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
    RegisterClass( __classid(TBMSynchroEventsEditorHelperClass) );
    RegisterPropertyEditor(__typeinfo(TBMThreadGroup), 0L, "", __classid(TThreadGroupProperty));
    RegisterPropertyEditor(__typeinfo(TPersistent), __classid(TBMThread), "SynchroMethods", __classid(TEmptyEntryPropertyEditor) );
    RegisterComponentEditor(__classid(TBMThread), __classid(TBMThreadControlEditor));
#ifdef __THREAD_EXPERT
    RegisterLibraryExpert ( new TBMThreadMethodsMenageExpert );
#endif
    }
//---------------------------------------------------------------------------
};













