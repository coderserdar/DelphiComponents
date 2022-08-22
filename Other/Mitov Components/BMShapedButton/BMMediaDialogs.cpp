//---------------------------------------------------------------------------
#include <vcl.h>
#include <consts.hpp>
#pragma hdrstop

#include "BMMediaDialogs.h"
#ifdef _USE_VISTA_DIALOGS_
  #include <AnsiStrings.hpp>
#endif
#pragma package(smart_init)
#pragma resource "BMMediaDialogs.res"
#pragma link "Dialogs" 

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
namespace Bmmediadialogs
{
static inline void ValidCtrCheck(TBMOpenMediaDialog *)
{
        new TBMOpenMediaDialog(NULL);
}
//---------------------------------------------------------------------------
#ifdef _USE_VISTA_DIALOGS_
class TFileDialogWrapper : public TObject
{
  protected:
    TCustomFileDialog *FFileDialog;
    TOpenDialog *FOpenDialog;
    virtual TCustomFileDialog *CreateFileDialog () = 0;

  private:
    void AssignFileTypes()
    {
        String FilterStr = FOpenDialog->Filter;
        int J = 1;
        int I = AnsiPos('|', FilterStr);
        while( I != 0 )
            {
            TFileTypeItem *AFileType = FFileDialog->FileTypes->Add();
            AFileType->DisplayName = FilterStr.SubString( J, I - J );
            if( ! SysLocale.FarEast )
                J = PosEx('|', FilterStr, I + 1);

            else
                {
                J = AnsiPos('|', FilterStr.SubString( I + 1, MAXINT));
                if( J != 0 )
                    J = J + (I + 1) - 1;

                }

            if( J == 0 )
                J = FilterStr.Length() + 1;

            AFileType->FileMask = FilterStr.SubString(I + 1, J - I - 1);
            ++J;

            if( ! SysLocale.FarEast )
                I = PosEx('|', FilterStr, J);

            else
                {
                I = AnsiPos('|', FilterStr.SubString( J, MAXINT ));
                if( I != 0 )
                    I += J - 1;

                }
            }
    }

    void AssignOptions()
    {
//  ofOverwritePrompt, [fdoOverWritePrompt]
      if( FOpenDialog->Options.Contains( ofOverwritePrompt ))
        FFileDialog->Options = FFileDialog->Options << fdoOverWritePrompt;

//  ofNoChangeDir, [fdoNoChangeDir]
      if( FOpenDialog->Options.Contains( ofNoChangeDir ))
        FFileDialog->Options = FFileDialog->Options << fdoNoChangeDir;

//  ofNoValidate, [fdoNoValidate]
      if( FOpenDialog->Options.Contains( ofNoValidate ))
        FFileDialog->Options = FFileDialog->Options << fdoNoValidate;

//  ofAllowMultiSelect, [fdoAllowMultiSelect]
      if( FOpenDialog->Options.Contains( ofAllowMultiSelect ))
        FFileDialog->Options = FFileDialog->Options << fdoAllowMultiSelect;

//  ofExtensionDifferent, [fdoStrictFileTypes]
      if( FOpenDialog->Options.Contains( ofExtensionDifferent ))
        FFileDialog->Options = FFileDialog->Options << fdoStrictFileTypes;

//  ofPathMustExist, [fdoPathMustExist]
      if( FOpenDialog->Options.Contains( ofPathMustExist ))
        FFileDialog->Options = FFileDialog->Options << fdoPathMustExist;

//  ofFileMustExist, [fdoFileMustExist]
      if( FOpenDialog->Options.Contains( ofFileMustExist ))
        FFileDialog->Options = FFileDialog->Options << fdoFileMustExist;

//  ofCreatePrompt, [fdoCreatePrompt]
      if( FOpenDialog->Options.Contains( ofCreatePrompt ))
        FFileDialog->Options = FFileDialog->Options << fdoCreatePrompt;

//  ofShareAware, [fdoShareAware]
      if( FOpenDialog->Options.Contains( ofShareAware ))
        FFileDialog->Options = FFileDialog->Options << fdoShareAware;

//  ofNoReadOnlyReturn, [fdoNoReadOnlyReturn]
      if( FOpenDialog->Options.Contains( ofNoReadOnlyReturn ))
        FFileDialog->Options = FFileDialog->Options << fdoNoReadOnlyReturn;

//  ofNoTestFileCreate, [fdoNoTestFileCreate]
      if( FOpenDialog->Options.Contains( ofNoTestFileCreate ))
        FFileDialog->Options = FFileDialog->Options << fdoNoTestFileCreate;

//  ofNoDereferenceLinks,  [fdoNoDereferenceLinks]
      if( FOpenDialog->Options.Contains( ofNoDereferenceLinks ))
        FFileDialog->Options = FFileDialog->Options << fdoNoDereferenceLinks;

//  ofDontAddToRecent, [fdoDontAddToRecent]
      if( FOpenDialog->Options.Contains( ofDontAddToRecent ))
        FFileDialog->Options = FFileDialog->Options << fdoDontAddToRecent;

//  ofForceShowHidden [fdoForceShowHidden]
      if( FOpenDialog->Options.Contains( ofForceShowHidden ))
        FFileDialog->Options = FFileDialog->Options << fdoForceShowHidden;

      if( FOpenDialog->OptionsEx.Contains( ofExNoPlacesBar ))
        FFileDialog->Options = FFileDialog->Options << fdoHidePinnedPlaces;
    }

    __fastcall void HandleShareViolation( TObject *Sender, TFileDialogShareViolationResponse & Response )
    {
        Response = fsrAccept;
    }

    __fastcall void OnFileOkEvent( TObject *Sender, bool &CanClose )
    {
        FOpenDialog->FileName = FFileDialog->FileName;
        FOpenDialog->Files->Assign( FFileDialog->Files );
        FOpenDialog->OnCanClose(FOpenDialog, CanClose);
    }

    __fastcall void OnFolderChangeEvent(TObject *Sender)
    {
        FOpenDialog->FileName = FFileDialog->FileName;
        FOpenDialog->OnFolderChange(FOpenDialog);
    }

    __fastcall void OnSelectionChangeEvent(TObject *Sender)
    {
        FOpenDialog->FileName = FFileDialog->FileName;
        FOpenDialog->Files->Assign(FFileDialog->Files);
        FOpenDialog->OnSelectionChange(FOpenDialog);
    }

    __fastcall void OnTypeChangeEvent(TObject *Sender)
    {
        FOpenDialog->FilterIndex = FFileDialog->FileTypeIndex;
        FOpenDialog->OnTypeChange(FOpenDialog);
    }

  public:
    virtual __fastcall TFileDialogWrapper( TOpenDialog *OpenDialog )
    {
        FOpenDialog = OpenDialog;
        FFileDialog = CreateFileDialog();
    }

    virtual __fastcall ~TFileDialogWrapper()
    {
        delete FFileDialog;
    }

  public:
    virtual __fastcall bool Execute( HWND ParentWnd )
    {
    //  with FOpenDialog do
    //  begin
        FFileDialog->DefaultExtension = FOpenDialog->DefaultExt;
        FFileDialog->DefaultFolder = FOpenDialog->InitialDir;
        FFileDialog->FileName = FOpenDialog->FileName;
        FFileDialog->FileTypeIndex = FOpenDialog->FilterIndex;
        FFileDialog->Title = FOpenDialog->Title;
        if( FOpenDialog->OnCanClose )
          FFileDialog->OnFileOkClick = OnFileOkEvent;

        if( FOpenDialog->OnFolderChange )
          FFileDialog->OnFolderChange = OnFolderChangeEvent;

        if( FOpenDialog->OnSelectionChange )
          FFileDialog->OnSelectionChange = OnSelectionChangeEvent;

        if( FOpenDialog->OnTypeChange )
          FFileDialog->OnTypeChange = OnTypeChangeEvent;

        // TOpenDialog/TSaveDialog ignore sharing violations when ofShareAware
        // is set. Assign an event handler to mimic that behavior.
        if( FOpenDialog->Options.Contains( ofShareAware ))
          FFileDialog->OnShareViolation = HandleShareViolation;

    //  end;
      AssignFileTypes();
      AssignOptions();

      bool Result = FFileDialog->Execute(ParentWnd);
      if( Result )
        {
        FOpenDialog->FileName = FFileDialog->FileName;
        FOpenDialog->Files->Assign(FFileDialog->Files);
        FOpenDialog->FilterIndex = FFileDialog->FileTypeIndex;
        }

      return Result;
    }

  };
//---------------------------------------------------------------------------
  class TFileOpenDialogWrapper : public TFileDialogWrapper
  {
  private:
    __fastcall void OnExecuteEvent(TObject *Sender)
    {
        DWORD LOptions;
        FFileDialog->Dialog->GetOptions( &LOptions );
        LOptions |= FOS_FORCEPREVIEWPANEON;
        FFileDialog->Dialog->SetOptions( LOptions );
    }

  public:
    virtual __fastcall TFileOpenDialogWrapper( TOpenDialog *OpenDialog ) :
      TFileDialogWrapper( OpenDialog )
    {
    }

  protected:
    virtual TCustomFileDialog *CreateFileDialog()
    {
        TFileOpenDialog *Result = new TFileOpenDialog( NULL );
        Result->OnExecute = OnExecuteEvent;
        return Result;
    }
};
//---------------------------------------------------------------------------
  class TFileSaveDialogWrapper : public TFileDialogWrapper
  {
  public:
    virtual __fastcall TFileSaveDialogWrapper( TOpenDialog *OpenDialog ) :
      TFileDialogWrapper( OpenDialog )
    {
    }

  protected:
    virtual TCustomFileDialog *CreateFileDialog()
    {
        return new TFileSaveDialog( NULL );
    }
  };
//---------------------------------------------------------------------------
#endif
class TBMPaintPanel : public TPanel
{
  typedef TPanel inherited;
  TImageList *FImageList;
  bool FImageVisible;

public:
  __fastcall virtual TBMPaintPanel(Classes::TComponent* AOwner) :
    TPanel(AOwner)
  {
    FImageList = new TImageList( this );
    Graphics::TBitmap *ABitmap = new Graphics::TBitmap();
    ABitmap->LoadFromResourceName((UINT)HInstance, "SOUNDGLYPH");
    FImageList->Width = ABitmap->Width;
    FImageList->Height = ABitmap->Height;
    FImageList->AddMasked( ABitmap, ABitmap->Canvas->Pixels[ 0 ][ 0 ] );
    delete ABitmap;
  }

  __fastcall virtual ~TBMPaintPanel()
  {
    delete FImageList;
  }

protected:
  virtual __fastcall void Paint()
  {
    inherited::Paint();
    if( FImageVisible )
      FImageList->Draw( Canvas, ( Width - FImageList->Width ) / 2, ( Height - FImageList->Height ) / 2, 0, true );
  }

public:
  void SetImageVisible( bool AValue )
  {
    FImageVisible = AValue;
    Invalidate();
  }

};
//---------------------------------------------------------------------------
String GetTmpFile ( String Extention )
{
  TCHAR Buffer [ MAX_PATH + 1 ] = TEXT("");
  GetTempPath ( MAX_PATH, Buffer );

  int i = 0;

  for (;;i++)
    {
    String FileName = Buffer;

    FileName += (String)"\\BMMD" + i + Extention;

    HANDLE Return = CreateFile(

      FileName.c_str (),    // pointer to name of the file
      GENERIC_WRITE,    // access (read-write) mode
      0,    // share mode
      SECURITY_ANONYMOUS,    // pointer to security attributes
      CREATE_NEW,    // how to create
      FILE_ATTRIBUTE_TEMPORARY,    // file attributes
      0     // handle to file with attributes to copy
      );

    if ( Return != INVALID_HANDLE_VALUE )
      {
      CloseHandle( Return );
      return FileName;
      }
    }
}
//---------------------------------------------------------------------------
__fastcall TBMMediaDialogImpl::TBMMediaDialogImpl( TOpenDialog* AOwner)
        : inherited (AOwner),
        OwnerDialog ( (TBMOpenMediaDialog *)AOwner )
{
  FTmpFile = "";

  InPreview = false;
  
  FPicturePanel = new TPanel( this );
  IsValidView = true;

  FPicturePanel->Name = "PicturePanel";
  FPicturePanel->Caption = "";
  FPicturePanel->SetBounds(204, 5, 169, 200);
  FPicturePanel->BevelOuter = bvNone;
  FPicturePanel->BorderWidth = 6;
  FPicturePanel->TabOrder = 1;
  FPictureLabel = new TLabel (this);

  FPictureLabel->Name = "PictureLabel";
  FPictureLabel->Caption = "";
  FPictureLabel->SetBounds(6, 6, 157, 23 + 10);
  FPictureLabel->Align = alTop;
  FPictureLabel->AutoSize = false;
  FPictureLabel->Parent = FPicturePanel;

  FCommandPanel = new TPanel ( this );
  FCommandPanel->Name = "CommandPanel";
  FCommandPanel->Caption = "";
  FCommandPanel->SetBounds( 0, 2, 150, 26);
  FCommandPanel->Align = alBottom;
#ifdef __BCB_40__
  FCommandPanel->AutoSize = false;
#endif
  FCommandPanel->Parent = FPicturePanel;

  FPreviewButton = new TSpeedButton (this);

  FPreviewButton->Name = "PreviewButton";
  FPreviewButton->SetBounds(77, 1, 23, 22);
  FPreviewButton->Enabled = false;
  FPreviewButton->Glyph->LoadFromResourceName((UINT)HInstance, "PREVIEWGLYPH");
  FPreviewButton->Hint = "Show Preview"; //Consts_SPictureDesc;
  FPreviewButton->ParentShowHint = false;
  FPreviewButton->ShowHint = true;
  FPreviewButton->OnClick = PreviewClick;
  FPreviewButton->Parent = FPicturePanel;

  FPaintPanel = new TBMPaintPanel(this);

  FPaintPanel->Name = "PaintPanel";
  FPaintPanel->Caption = "";
  FPaintPanel->SetBounds(6, 29, 157, 145);
  FPaintPanel->Align = alClient;
  FPaintPanel->BevelInner = bvRaised;
  FPaintPanel->BevelOuter = bvLowered;
  FPaintPanel->TabOrder = 0;
  FPaintPanel->Parent = FPicturePanel;

  FPlayer = new TMediaPlayer( this );
  FPlayer->Name = "MediaPlayer";
  FPlayer->TimeFormat = tfMilliseconds;
  FPlayer->Display = FPaintPanel;
  FPlayer->Visible = false;
  FPlayer->Top = -30;
  FPlayer->Left = -30;

  FTimer = new TTimer ( this );
  FTimer->OnTimer = MediaNotify;
  FTimer->Interval = 50;

  FUpdateTimer = new TTimer( this );
  FUpdateTimer->OnTimer = UpdateNotify;
  FUpdateTimer->Enabled = false;
  FUpdateTimer->Interval = 1500;

  FLoopCheckBox = new TCheckBox (this);
  FLoopCheckBox->Name = "LoopCheckBox";
  FLoopCheckBox->SetBounds( 100, 5, 50, 15);
  FLoopCheckBox->Checked = true;
  FLoopCheckBox->Caption = "Loop";
  FLoopCheckBox->Parent = FCommandPanel;

  FShowCheckBox = new TCheckBox (this);
  FShowCheckBox->Name = "ShowCheckBox";
  FShowCheckBox->OnClick = OnShowCheck;
  FShowCheckBox->Checked = true;
  FShowCheckBox->SetBounds( 6, 5, 90, 15);
  FShowCheckBox->Caption = "Show preview";
  FShowCheckBox->Parent = FCommandPanel;
//  FPlayer->SetBounds(0, 0, 153, 141);
//  FPlayer->VisibleButtons = Mplayer::TButtonSet () << btPlay << btStop << btPause;
  FPlayer->VisibleButtons = FPlayer->VisibleButtons >> btEject >> btRecord;
}
//---------------------------------------------------------------------------
bool __fastcall TBMMediaDialogImpl::ValidFile ( String FileName )
{
    return GetFileAttributes(PChar(FileName.c_str ())) != 0xFFFFFFFF;
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::OnShowCheck (System::TObject* Sender)
{
  FLoopCheckBox->Enabled = FShowCheckBox->Checked;

  if ( FShowCheckBox->Checked && FPlayer->Capabilities.Contains ( mpCanPlay ))
    FPlayer->Play ();
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::DoFolderChange(void)
{
  if ( ! FTmpFile.IsEmpty ())
    {
    FPlayer->Wait = true;
    FPlayer->Close ();
    DeleteFile ( FTmpFile );
    }

  FTmpFile = "";
  ((TBMPaintPanel *)FPaintPanel)->SetImageVisible( false );
  FPreviewButton->Enabled = false;
  FPictureLabel->Caption = "";
//  inherited::DoFolderChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::DoSelectionChange(void)
{
  FPreviewButton->Enabled = false;
  FUpdateTimer->Enabled = false;
  IsValidView = false;
  FPictureLabel->Enabled = false;
  FUpdateTimer->Enabled = true;
  if ( FPlayer->Mode == mpPlaying )
    FPlayer->Pause ();
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::DoClose(void)
{
  IsVisible = false;
  FPlayer->Wait = true;
  FPlayer->Close ();
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::DoShow(void)
{
  TRect PreviewRect, StaticRect;

  // Set preview area to entire dialog
  GetClientRect ( OwnerDialog->Handle, (RECT*)&PreviewRect );
  StaticRect = OwnerDialog->GetStaticRect ();
  // Move preview area to right of static area
  PreviewRect.Left = StaticRect.Left + (StaticRect.Right - StaticRect.Left);
  PreviewRect.Top += 4;
//  PreviewRect.Bottom -= FPlayer->Height;
  FPicturePanel->BoundsRect = PreviewRect;
  FPreviewButton->Left = FPaintPanel->BoundsRect.Right - FPreviewButton->Width - 2;
//  FPicture->Assign(NULL);
  FPicturePanel->ParentWindow = OwnerDialog->Handle;

  IsVisible = true;
//  FPlayer->DisplayRect = PreviewRect;

/*
  PreviewRect.Top = PreviewRect.Bottom;
  PreviewRect.Bottom += FPlayer->Height;
  FPlayer->BoundsRect = PreviewRect;
*/
  FPlayer->ParentWindow = OwnerDialog->Handle;
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::ExecuteEnter(void)
{
  if ( NewStyleControls && ! ( OwnerDialog->Options.Contains ( ofOldStyleDialog ) ))
    OwnerDialog->Template = TEXT("DLGTEMPLATE");

  else
    OwnerDialog->Template = NULL;

}
//---------------------------------------------------------------------------
bool IsVideo ( MCIDEVICEID DeviceID )
{
    MCI_GETDEVCAPS_PARMS DevCapParm;

    DWORD FFlags = MCI_WAIT | MCI_GETDEVCAPS_ITEM;

    DevCapParm.dwCallback = NULL;
    DevCapParm.dwItem = MCI_GETDEVCAPS_HAS_VIDEO;
    mciSendCommand( DeviceID, MCI_GETDEVCAPS, FFlags,  (long int) &DevCapParm );
    return (bool) DevCapParm.dwReturn;
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::UpdateNotify(System::TObject* Sender)
{
  FUpdateTimer->Enabled = false;
  IsValidView = true;

  FPictureLabel->Enabled = true;

//  String FullName;
  bool ValidPicture;

  ValidPicture = FileExists(OwnerDialog->FileName) && ValidFile(OwnerDialog->FileName);

  if ( CurrentFileName == OwnerDialog->FileName )
    {
    if ( FPlayer->Mode == mpPaused )
      FPlayer->Resume ();

    if( ! ComponentState.Contains( csDesigning ))
      FPreviewButton->Enabled = IsVideo( FPlayer->DeviceID );

    return;
    }

  CurrentFileName = OwnerDialog->FileName;
  FPaintPanel->Invalidate ();
  if ( ValidPicture )
  try
    {
    if ( ! FTmpFile.IsEmpty ())
      {
      FPlayer->Wait = true;
      FPlayer->Close ();
      DeleteFile ( FTmpFile );
      }

    FTmpFile = GetTmpFile ( ExtractFileExt ( CurrentFileName ));
    if ( CopyFile ( CurrentFileName.c_str (), FTmpFile.c_str (), false ))
      FPlayer->FileName = FTmpFile;

    else
      FPlayer->FileName = CurrentFileName;

    FPlayer->Open ();
    OriginalDisplayRect = FPlayer->DisplayRect;
    TRect DisplayRect = ::Rect ( 6, 6, FPaintPanel->Width - 2 * 6, FPaintPanel->Height - 2 * 6 );
    int VDisplay = DisplayRect.Bottom - DisplayRect.Top;
    int VOld = OriginalDisplayRect.Bottom - OriginalDisplayRect.Top;
    int HDisplay = DisplayRect.Right - DisplayRect.Left;
    int HOld = OriginalDisplayRect.Right - OriginalDisplayRect.Left;

    CalculatedRect = DisplayRect;
//    if ( FPlayer->Capabilities.Contains ( mpUsesWindow ) )
    if ( IsVideo ( FPlayer->DeviceID ))
      {
      if ( VOld * HDisplay > HOld * VDisplay )
        {
        int Hor =  VDisplay * HOld / VOld;
        CalculatedRect.Right = CalculatedRect.Left + Hor;
        }

      else
        {
        int Vert = HDisplay * VOld / HOld;
        CalculatedRect.Bottom = CalculatedRect.Top + Vert;
        }

      FPlayer->DisplayRect = CalculatedRect; //::Rect ( 6, 6, FPaintPanel->Width - 2 * 6, FPaintPanel->Height - 2 * 6 );
      CurrentRect = CalculatedRect;
      }

//    int TheLength = FPlayer->Length;
//    String LenString = IntToStr(LOBYTE(LOWORD(TheLength)) & 0xF ) + ":" + IntToStr(LOBYTE(LOWORD(TheLength)) >> 4 ) + ":" + IntToStr(HIBYTE(LOWORD(TheLength)) & 0xF );
//    String LenString = (String)( TheLength / 1000 ) + "." + ( TheLength % 1000 ) + " Sec";
//    if ( FPlayer->Capabilities.Contains ( mpUsesWindow ) )
    if ( IsVideo ( FPlayer->DeviceID ))
      {
      FPictureLabel->Caption = (String)"Movie : " + OriginalDisplayRect.Right + " : " + OriginalDisplayRect.Bottom; // + "\nTime : " +  LenString;
      }

    else
      {
//      FPictureLabel->Caption = (String)"Sound : " +  LenString;
      FPictureLabel->Caption = (String)"Sound";
      }

  if ( FShowCheckBox->Checked )
    FPlayer->Play ();
/*
    FPicture->LoadFromFile(FullName);
    TVarRec Rec [ 2 ];
    Rec [ 0 ] = FPicture->Width;
    Rec [ 1 ] = FPicture->Height;

    FPictureLabel->Caption = Format(Consts_SPictureDesc, Rec, 2);
*/
    if( ! ComponentState.Contains( csDesigning ))
      FPreviewButton->Enabled = IsVideo( FPlayer->DeviceID );
/*
    if ( ! FPreviewButton->Enabled )
      FPlayer->Display = NULL;

    else
      FPlayer->Display = FPaintPanel;
*/

    ((TBMPaintPanel *)FPaintPanel)->SetImageVisible( ! IsVideo( FPlayer->DeviceID ) );
    }

  catch (...)
    {
    ValidPicture = false;
    }

  if ( ! ValidPicture )
    {
    FPictureLabel->Caption = ""; //Consts_SPictureLabel;
    FPreviewButton->Enabled = false;
    ((TBMPaintPanel *)FPaintPanel)->SetImageVisible( false );
//    FPicture->Assign(NULL);
    }

//  FPaintBox->Invalidate ();
//  inherited::DoSelectionChange ();
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::ExecuteExit(void)
{
  if ( ! FTmpFile.IsEmpty ())
    DeleteFile ( FTmpFile );
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::MediaNotify(System::TObject* Sender)
{
  FPlayer->DisplayRect = CurrentRect; //::Rect ( 6, 6, FPaintPanel->Width - 2 * 6, FPaintPanel->Height - 2 * 6 );

  if( !IsValidView )
    return;

  if( InPreview )
    return;

  if( FShowCheckBox->Checked )
    {
    if( FLoopCheckBox->Checked )
      if( FPlayer->Mode == mpStopped )
        FPlayer->Play();

    }

  else
    if( FPlayer->Mode == mpStopped )
      FPaintPanel->Invalidate();

}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::OnResize (System::TObject* Sender)
{
  CurrentRect.Right = PreviewForm->ClientWidth - 4;//= OriginalDisplayRect.Right - OriginalDisplayRect.Left + 4; //FPicture->Width + (PreviewForm->ClientWidth - Panel->ClientWidth)+ 10;
  CurrentRect.Bottom = PreviewForm->ClientHeight - 4;//= OriginalDisplayRect.Bottom - OriginalDisplayRect.Top + 4;//FPicture->Height + (PreviewForm->ClientHeight - Panel->ClientHeight) + 10;

  FPlayer->DisplayRect = CurrentRect;
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::OnPreviewShow (System::TObject* Sender)
{
}
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::PreviewClick(System::TObject* Sender)
{
  TPanel *Panel;

  PreviewForm = new TForm (this);
//  with PreviewForm do
  try
    {
    PreviewForm->Name = "PreviewForm";
    PreviewForm->Caption = Consts_SPreviewLabel;
    PreviewForm->BorderStyle = bsSizeToolWin;
    PreviewForm->KeyPreview = true;
    PreviewForm->Position = poScreenCenter;
    PreviewForm->OnKeyPress = PreviewKeyPress;

    Panel = new TPanel ( PreviewForm );
    Panel->Name = "Panel";
    Panel->Caption = "";
    Panel->Align = alClient;
    Panel->BevelOuter = bvNone;
    Panel->BorderStyle = bsSingle;
    Panel->BorderWidth = 5;
    Panel->Color = clWindow;
    Panel->Parent = PreviewForm;
    Panel->OnResize = OnResize;

    PreviewForm->ClientWidth = OriginalDisplayRect.Right - OriginalDisplayRect.Left + 4; //FPicture->Width + (PreviewForm->ClientWidth - Panel->ClientWidth)+ 10;
    PreviewForm->ClientHeight = OriginalDisplayRect.Bottom - OriginalDisplayRect.Top + 4 + FPlayer->Height;//FPicture->Height + (PreviewForm->ClientHeight - Panel->ClientHeight) + 10;

    FPaintPanel->Invalidate ();
    InPreview = true;
    FPlayer->Display = Panel;
    FPlayer->ParentWindow = NULL;
    FPlayer->Parent = PreviewForm;
    FPlayer->Align = alBottom;
    FPlayer->Visible = true;
    FPlayer->Stop ();
    PreviewForm->OnShow = OnPreviewShow;
    CurrentRect = OriginalDisplayRect;
    FPlayer->Rewind ();
    PreviewForm->ShowModal ();
    InPreview = false;
    FPlayer->Parent = NULL;
    FPlayer->ParentWindow = OwnerDialog->Handle;
    FPlayer->Display = FPaintPanel;
    CurrentRect = CalculatedRect;
    }

  __finally
    {
    delete PreviewForm;
    }

}            
//---------------------------------------------------------------------------
void __fastcall TBMMediaDialogImpl::PreviewKeyPress(System::TObject* Sender, TCHAR &Key)
{
  if ( Key == 27 )
    (( TForm *)Sender)->Close ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMOpenMediaDialog::TBMOpenMediaDialog(TComponent* Owner)
        : inherited (Owner)
{
#ifdef _USE_VISTA_DIALOGS_
  FUseVistaDialogs = true;
#endif
  Options = Options << ofShareAware;

  FMediaDialogImpl = new TBMMediaDialogImpl( this );

  Filter = "All media (*.wav;*.mid;*.avi)|*.wav;*.mid;*.avi|Wave File (*.wav)|*.wav|MIDI File (*.mid)|*.mid|Video (*.avi)|*.avi|All files (*.*)|*.*";
}
//---------------------------------------------------------------------------
__fastcall TBMOpenMediaDialog::~TBMOpenMediaDialog(void)
{
  delete FMediaDialogImpl;
}
//---------------------------------------------------------------------------
bool __fastcall TBMOpenMediaDialog::GetAutoShow ()
{
  return FMediaDialogImpl->FShowCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::SetAutoShow ( bool Value )
{
  FMediaDialogImpl->FShowCheckBox->Checked = Value;
}
//---------------------------------------------------------------------------
bool __fastcall TBMOpenMediaDialog::GetLoop ()
{
  return FMediaDialogImpl->FLoopCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::SetLoop ( bool Value )
{
  FMediaDialogImpl->FLoopCheckBox->Checked = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::DoFolderChange(void)
{
  FMediaDialogImpl->DoFolderChange();
  inherited::DoFolderChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::WndProc(Messages::TMessage &Message)
{
    inherited::WndProc( Message );
    if( Message.Msg == WM_SIZE )
    {
        if( FMediaDialogImpl->IsVisible )
          {
          TRect ARect;
          GetClientRect( Handle, &ARect );
          FMediaDialogImpl->FPicturePanel->Height = ARect.Height() - FMediaDialogImpl->FPicturePanel->Top - FMediaDialogImpl->FCommandPanel->Height;
          }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::DoSelectionChange(void)
{
  FMediaDialogImpl->DoSelectionChange();
  inherited::DoSelectionChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::DoClose(void)
{
  FMediaDialogImpl->DoClose();
  inherited::DoClose ();
  // Hide any hint windows left behind
  Application->HideHint();
}
//---------------------------------------------------------------------------
void __fastcall TBMOpenMediaDialog::DoShow(void)
{
  FMediaDialogImpl->DoShow ();
  inherited::DoShow ();
}
//---------------------------------------------------------------------------
#if (__BORLANDC__ >= 0x0590)
bool __fastcall TBMOpenMediaDialog::Execute(HWND ParentWnd)
{
  FMediaDialogImpl->ExecuteEnter();
/*
  if( NewStyleControls && ! ( Options.Contains( ofOldStyleDialog )) && !
     ((Win32MajorVersion >= 6) && UseLatestCommonDialogs ))
    Template = TEXT( "DLGTEMPLATE" );

  else
    Template = NULL;
*/
  int Result;
  if ((Win32MajorVersion >= 6) && UseLatestCommonDialogs && FUseVistaDialogs &&
     ! ( OnIncludeItem || OnClose || OnShow ))

    {
    // This requires Windows Vista or later

    TFileDialogWrapper *FileDialogWrapper = new TFileOpenDialogWrapper( this );
    try
      {
      Result = FileDialogWrapper->Execute(ParentWnd);
      }
    __finally
    {
      delete FileDialogWrapper;
    };

    return Result;
    }

  else
    Result = inherited::Execute(ParentWnd);

  FMediaDialogImpl->ExecuteExit();

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TBMOpenMediaDialog::Execute(void)
{
  return inherited::Execute();
}
#else
//---------------------------------------------------------------------------
bool __fastcall TBMOpenMediaDialog::Execute(void)
{
  FMediaDialogImpl->ExecuteEnter();
  int Result = inherited::Execute();
  FMediaDialogImpl->ExecuteExit();

  return Result;
}
#endif
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMSaveMediaDialog::TBMSaveMediaDialog(TComponent* Owner)
        : inherited (Owner)
{
#ifdef _USE_VISTA_DIALOGS_
  FUseVistaDialogs = true;
#endif
  Options = Options << ofShareAware;

  FMediaDialogImpl = new TBMMediaDialogImpl ( this );

  Filter = "All media (*.wav;*.mid;*.avi)|*.wav;*.mid;*.avi|Wave File (*.wav)|*.wav|MIDI File (*.mid)|*.mid|Video (*.avi)|*.avi|All files (*.*)|*.*";
}
//---------------------------------------------------------------------------
__fastcall TBMSaveMediaDialog::~TBMSaveMediaDialog(void)
{
  delete FMediaDialogImpl;
}
//---------------------------------------------------------------------------
bool __fastcall TBMSaveMediaDialog::GetAutoShow ()
{
  return FMediaDialogImpl->FShowCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::SetAutoShow ( bool Value )
{
  FMediaDialogImpl->FShowCheckBox->Checked = Value;
}
//---------------------------------------------------------------------------
bool __fastcall TBMSaveMediaDialog::GetLoop ()
{
  return FMediaDialogImpl->FLoopCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::SetLoop ( bool Value )
{
  FMediaDialogImpl->FLoopCheckBox->Checked = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::DoFolderChange(void)
{
  FMediaDialogImpl->DoFolderChange();
  inherited::DoFolderChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::DoSelectionChange(void)
{
  FMediaDialogImpl->DoSelectionChange();
  inherited::DoSelectionChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::DoClose(void)
{
  FMediaDialogImpl->DoClose();
  inherited::DoClose();
  // Hide any hint windows left behind
  Application->HideHint();
}
//---------------------------------------------------------------------------
void __fastcall TBMSaveMediaDialog::DoShow(void)
{
  FMediaDialogImpl->DoShow ();
  inherited::DoShow ();
}
//---------------------------------------------------------------------------
#if (__BORLANDC__ >= 0x0590)
bool __fastcall TBMSaveMediaDialog::Execute(HWND ParentWnd)
{
  FMediaDialogImpl->ExecuteEnter();
/*
  if( NewStyleControls && ! ( Options.Contains( ofOldStyleDialog )) && !
     ((Win32MajorVersion >= 6) && UseLatestCommonDialogs ))
    Template = TEXT( "DLGTEMPLATE" );

  else
    Template = NULL;
*/
  int Result;
  if ((Win32MajorVersion >= 6) && UseLatestCommonDialogs && FUseVistaDialogs &&
     ! ( OnIncludeItem || OnClose || OnShow ))

    {
    // This requires Windows Vista or later
    TFileDialogWrapper *FileDialogWrapper = new TFileSaveDialogWrapper( this );
    try
      {
      Result = FileDialogWrapper->Execute(ParentWnd);
      }

    __finally
      {
      delete FileDialogWrapper;
      };

    return Result;
    }

  else
    Result = inherited::Execute(ParentWnd);

  FMediaDialogImpl->ExecuteExit();

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TBMSaveMediaDialog::Execute(void)
{
  return inherited::Execute();
}
#else
//---------------------------------------------------------------------------
bool __fastcall TBMSaveMediaDialog::Execute(void)
{
  FMediaDialogImpl->ExecuteEnter();
  int Result = inherited::Execute ();
  FMediaDialogImpl->ExecuteExit();

  return Result;
}
#endif
//---------------------------------------------------------------------------
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[2] = {__classid(TBMOpenMediaDialog), __classid(TBMSaveMediaDialog)};
    RegisterComponents("Dialogs", classes, 1);
  }
}
//---------------------------------------------------------------------------
