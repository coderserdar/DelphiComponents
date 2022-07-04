//---------------------------------------------------------------------------
#ifndef ExFormatEdU1H
#define ExFormatEdU1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "VpBase.hpp"
#include "VpBaseDS.hpp"
#include "VpPrtPrv.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
#define BaseCaption "Print Format Designer"
#define FileCaption "Print Format Designer - %s"
#define UnnamedFile "<Unnamed>"
//---------------------------------------------------------------------------
class TfrmPrnFormat : public TForm
{
__published:    // IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TButton *btnNewFormat;
        TPanel *Panel1;
        TVpPrintPreview *PrintPreview;
        TListBox *lbFormats;
        TListBox *lbElements;
        TButton *btnEditFormat;
        TButton *btnDeleteFormat;
        TButton *btnNewElement;
        TButton *btnEditElement;
        TButton *btnDeleteElement;
        TPanel *Panel2;
        TButton *btnLoadFile;
        TButton *btnSaveFile;
        TButton *btnClose;
        TButton *btnNewFile;
        TVpControlLink *VpControlLink;
        TOpenDialog *OpenDialog1;
        TSaveDialog *SaveDialog1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall lbElementsClick(TObject *Sender);
        void __fastcall lbFormatsClick(TObject *Sender);
        void __fastcall btnDeleteElementClick(TObject *Sender);
        void __fastcall btnCloseClick(TObject *Sender);
        void __fastcall btnNewFormatClick(TObject *Sender);
        void __fastcall btnEditFormatClick(TObject *Sender);
        void __fastcall btnDeleteFormatClick(TObject *Sender);
        void __fastcall btnNewElementClick(TObject *Sender);
        void __fastcall btnEditElementClick(TObject *Sender);
        void __fastcall btnNewFileClick(TObject *Sender);
        void __fastcall btnLoadFileClick(TObject *Sender);
        void __fastcall btnSaveFileClick(TObject *Sender);
private:        // User declarations

    String FileName;
    TVpControlLink* FLocalControlLink;
    bool IsDirty;

    int __fastcall DirtyPrompt();
    void __fastcall DoEditElement();
    void __fastcall DoEditFormat();
    void __fastcall DoNewFile();
    void __fastcall DoNewElement();
    void __fastcall DoNewFormat();
    void __fastcall DoSave();
    void __fastcall EnableElementButtons(bool Enable);
    void __fastcall EnableFormatButtons(bool Enable);
    void __fastcall UpdateCaption();

    TVpControlLink* __fastcall GetControlLink();
    void __fastcall SetControlLink(TVpControlLink* Value);
public:         // User declarations


        __fastcall TfrmPrnFormat(TComponent* Owner);

    __property TVpControlLink* LocalControlLink = {read=GetControlLink, write=SetControlLink};

    void __fastcall Execute();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmPrnFormat *frmPrnFormat;
//---------------------------------------------------------------------------
#endif
