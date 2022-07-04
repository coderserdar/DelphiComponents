//---------------------------------------------------------------------------
#ifndef ExVpAboutH
#define ExVpAboutH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <ShellApi.hpp>
#include "VpConst.hpp"
//---------------------------------------------------------------------------
#define cBrowserError "Unable to start web browser. Make sure you have it properly setup on your system."
//---------------------------------------------------------------------------
class TfrmAbout : public TForm
{
__published:    // IDE-managed Components
        TBevel *Bevel3;
        TBevel *Bevel2;
        TLabel *ProgramName;
        TLabel *VisitUsLabel;
        TLabel *GeneralNewsgroupsLabel;
        TLabel *lblTurboLink;
        TLabel *lblNewsGeneral;
        TLabel *SpecificNewsgroupLabel;
        TLabel *lblNewsSpecific;
        TLabel *StayUpToDateLabel;
        TLabel *InTheLabel;
        TLabel *CopyrightLabel;
        TLabel *RightsReservedLabel;
        TLabel *SignUpLabel;
        TLabel *ForThisProductLabel;
        TLabel *lblFreeUpdateCenter;
        TLabel *lblTurboPowerLive;
        TPanel *Panel1;
        TImage *Image1;
        TButton *OKButton;
        void __fastcall OKButtonClick(TObject *Sender);
        void __fastcall FormActivate(TObject *Sender);
        void __fastcall lblTurboLinkClick(TObject *Sender);
        void __fastcall lblTurboLinkMouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall FormMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y);
        void __fastcall lblFreeUpdateCenterClick(TObject *Sender);
        void __fastcall lblNewsGeneralClick(TObject *Sender);
        void __fastcall lblNewsSpecificClick(TObject *Sender);
        void __fastcall lblTurboPowerLiveClick(TObject *Sender);
private:        // User declarations
public:         // User declarations
        __fastcall TfrmAbout(TComponent* Owner);

        void __fastcall Execute();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAbout *frmAbout;
//---------------------------------------------------------------------------
#endif
