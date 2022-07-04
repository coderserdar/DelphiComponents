//---------------------------------------------------------------------------
#ifndef mainunitH
#define mainunitH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\IniFiles.hpp>
#include <vcl\Menus.hpp>
#include <vcl\ComCtrls.hpp>
#include "SpeedBar.hpp"
#include "Placemnt.hpp"
#include "DbPrgrss.hpp"
#include "AppEvent.hpp"
#include "AppUtils.hpp"
#include "RXShell.hpp"
#include "DbExcpt.hpp"
#include "RxMenus.hpp"
#include "VCLUtils.hpp"
#include "RxHints.hpp"
#include "PicClip.hpp"
#include "RxGrdCpt.hpp"
//---------------------------------------------------------------------------
class TRxDemoMainForm : public TForm
{
__published:    // IDE-managed Components
        TProgressBar *ProgressBar;
        TSpeedbarSection *SpeedbarSection1;
        TSpeedbarSection *SpeedbarSection2;
        TSpeedbarSection *SpeedbarSection3;
        TSpeedbarSection *SpeedbarSection4;
        TSpeedBar *Toolbar;
        TSpeedItem *CustomizeBtn;
        TSpeedItem *ExitBtn;
        TSpeedItem *View1Btn;
        TSpeedItem *View2Btn;
        TSpeedItem *View3Btn;
        TSpeedItem *WizardBtn;
        TSpeedItem *CascadeBtn;
        TSpeedItem *TileBtn;
        TSpeedItem *ArrangeBtn;
        TSpeedItem *AboutBtn;
        TMenuItem *CustomizeItem;
        TMenuItem *ExitItem;
        TMenuItem *FileMenu;
        TMenuItem *ViewMenu;
        TMenuItem *View1Item;
        TMenuItem *View3Item;
        TMenuItem *View4Item;
        TMenuItem *WindowMenu;
        TMenuItem *CascadeItem;
        TMenuItem *TileItem;
        TMenuItem *HelpMenu;
        TMenuItem *AboutItem;
        TPanel *MessagePanel;
        TPanel *StatusPanel;
        TPanel *ProgressPanel;
        TRxMainMenu *MainMenu1;
        TFormPlacement *FormPlacement1;
        TRxPopupMenu *ToolbarMenu;
        TDBProgress *DBProgress1;
        TAppEvents *AppEvents;
        TRxTrayIcon *TrayIcon;
        TRxPopupMenu *TrayMenu;
        TMenuItem *RXDemo1;
        TMenuItem *About1;
        TMenuItem *N1;
        TMenuItem *Exit1;
        TPicClip *TrayImg;
        TPicClip *ToolbarImg;
        TMenuItem *RXSiteItem;
        TRxGradientCaption *GradientCaption;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);

        void __fastcall FormPlacement1RestorePlacement(TObject *Sender);
        void __fastcall FormPlacement1SavePlacement(TObject *Sender);
        void __fastcall AppEventsMinimize(TObject *Sender);
        void __fastcall AppEventsRestore(TObject *Sender);

        void __fastcall CustomizeItemClick(TObject *Sender);
        void __fastcall Exit(TObject *Sender);
        void __fastcall View(TObject *Sender);
        void __fastcall WindowItemClick(TObject *Sender);
        void __fastcall AboutItemClick(TObject *Sender);

        void __fastcall SaveLayoutItemClick(TObject *Sender);
        void __fastcall RestoreLayoutItemClick(TObject *Sender);
        void __fastcall TrayClick(TObject *Sender);

        void __fastcall WizardBtnClick(TObject *Sender);
        void __fastcall AppEventsSettingsChanged(TObject *Sender);
        void __fastcall TrayMenuGetItemParams(TMenu *Sender, TMenuItem *Item,
        TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs);
        void __fastcall ToolbarMenuGetItemParams(TMenu *Sender, TMenuItem *Item,
        TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs);
        void __fastcall MainMenu1GetItemParams(TMenu *Sender, TMenuItem *Item,
        TMenuOwnerDrawState State, TFont *AFont, TColor &Color,
        TGraphic *&Graphic, int &NumGlyphs);
        void __fastcall TrayMenuDrawMargin(TMenu *Sender, TRect &Rect);
        void __fastcall RXSiteItemClick(TObject *Sender);
private:        // User declarations
        void __fastcall ActiveFormChange(TObject *Sender);
        void __fastcall UpdateToolbar();
public:         // User declarations
        __fastcall TRxDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TRxDemoMainForm *RxDemoMainForm;
//---------------------------------------------------------------------------
#endif
