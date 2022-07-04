//---------------------------------------------------------------------------
#ifndef ExDayPlannerU1H
#define ExDayPlannerU1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <TypInfo.hpp>
#include <Printers.hpp>
#include "VpBase.hpp"
#include "VpMisc.hpp"
#include "VpBaseDS.hpp"
#include "VpBDEDS.hpp"
#include "VpClock.hpp"
#include "VpContactGrid.hpp"
#include "VpDayView.hpp"
#include "VpDBDS.hpp"
#include "VpDlg.hpp"
#include "VpLEDLabel.hpp"
#include "VpMonthView.hpp"
#include "VpNavBar.hpp"
#include "VpPrtPrvDlg.hpp"
#include "VpResEditDlg.hpp"
#include "VpTaskList.hpp"
#include "VpEvntEditDlg.hpp"
#include "VpTaskEditDlg.hpp"
//---------------------------------------------------------------------------

#define CalendarPage      0
#define ContactsPage      1
#define TasksPage         2
#define MaintResourcePage 10
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:    // IDE-managed Components
        TPanel *PanelHeader;
        TLabel *LabelResource;
        TVpResourceCombo *VpResourceCombo1;
        TVpNavBar *VpNavBar1;
        TVpLEDLabel *LabelEventsLED;
        TVpLEDLabel *LabelContactsLED;
        TVpLEDLabel *LabelTasksLED;
        TLabel *LabelLoadedEvents;
        TLabel *LabelContacts;
        TLabel *LabelTasks;
        TVpClock *VpClock1;
        TStatusBar *StatusBar1;
        TPanel *PanelBase;
        TPanel *PanelContacts;
        TVpContactGrid *VpContactGrid1;
        TPanel *PanelTasks;
        TVpTaskList *VpTaskList1;
        TPanel *PanelFunction;
        TPanel *PanelDisplay;
        TImage *HeaderImage;
        TLabel *LabelHeader;
        TPanel *PanelCalendar;
        TSplitter *Splitter1;
        TPanel *PanelCalSplit2;
        TSplitter *Splitter2;
        TVpMonthView *VpMonthView1;
        TVpTaskList *VpTaskList2;
        TPanel *PanelCalSplit1;
        TVpDayView *VpDayView1;
        TPanel *PanelDayCount;
        TLabel *LabelDayCount;
        TTrackBar *TrackBar1;
        TPanel *PanelMaintResources;
        TBevel *Bevel1;
        TSpeedButton *BtnAddRes;
        TSpeedButton *BtnDelRes;
        TLabel *LabelResource2;
        TLabel *LabelResID;
        TLabel *LabelDescription;
        TLabel *LabelNotes;
        TSpeedButton *BtnApplyRes;
        TSpeedButton *BtnUndoRes;
        TEdit *EditResID;
        TMemo *MemoResNotes;
        TVpResourceCombo *VpResourceCombo2;
        TImageList *ImageList1;
        TVpBDEDataStore *VpBDEDataStore1;
        TVpResourceEditDialog *VpResourceEditDialog1;
        TMainMenu *MainMenu;
        TMenuItem *mnuFile;
        TMenuItem *mnuFileLoadPrintFormats;
        TMenuItem *Sep1;
        TMenuItem *mnuFilePrint;
        TMenuItem *mnuFilePrintPreview;
        TMenuItem *mnuFilePrintSetup;
        TMenuItem *mnuFileReportSetup;
        TMenuItem *Sep2;
        TMenuItem *mnuFileExit;
        TMenuItem *mnuMaintenance;
        TMenuItem *mnuMaintResources;
        TMenuItem *mnuHelp;
        TMenuItem *mnuHelpAbout;
        TVpPrintPreviewDialog *VpPrintPreviewDialog1;
        TVpControlLink *VpControlLink1;
        TOpenDialog *OpenDialog1;
        TPopupMenu *TaskPopup;
        TMenuItem *mnuNewTask;
        TMenuItem *mnuEditTask;
        TMenuItem *mnuDeleteTask;
        TPopupMenu *SchedulePopup;
        TMenuItem *mnuAddScheduleItem;
        TMenuItem *mnuEditScheduleItem;
        TMenuItem *mnuDeleteScheduleItem;
        TPrinterSetupDialog *PrinterSetupDialog1;
        TVpEventEditDialog *VpEventEditDialog1;
        TVpTaskEditDialog *VpTaskEditDialog1;
        TPrintDialog *PrintDialog1;
        TEdit *EditResDescr;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall BtnAddResClick(TObject *Sender);
        void __fastcall BtnDelResClick(TObject *Sender);
        void __fastcall VpNavBar1FolderChange(TObject *Sender, int Index,
          bool &AllowChange, bool Dragging);
        void __fastcall VpNavBar1ItemClick(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int Index);
        void __fastcall VpBDEDataStore1ResourceChange(TObject *Sender,
          TVpResource *Resource);
        void __fastcall TrackBar1Change(TObject *Sender);
        void __fastcall ResChange(TObject *Sender);
        void __fastcall EditResDescrChange(TObject *Sender);
        void __fastcall mnuAddScheduleItemClick(TObject *Sender);
        void __fastcall mnuDeleteScheduleItemClick(TObject *Sender);
        void __fastcall mnuDeleteTaskClick(TObject *Sender);
        void __fastcall mnuEditScheduleItemClick(TObject *Sender);
        void __fastcall mnuNewTaskClick(TObject *Sender);
        void __fastcall mnuEditTaskClick(TObject *Sender);
        void __fastcall mnuFileExitClick(TObject *Sender);
        void __fastcall mnuFileLoadPrintFormatsClick(TObject *Sender);
        void __fastcall mnuFilePrintClick(TObject *Sender);
        void __fastcall mnuFilePrintPreviewClick(TObject *Sender);
        void __fastcall mnuFilePrintSetupClick(TObject *Sender);
        void __fastcall mnuFileReportSetupClick(TObject *Sender);
        void __fastcall mnuHelpAboutClick(TObject *Sender);
        void __fastcall mnuMaintResourcesClick(TObject *Sender);
private:        // User declarations
public:         // User declarations
        __fastcall TMainForm(TComponent* Owner);

        void __fastcall SetActivePage(int Page);
        void __fastcall LoadMaintResFields();
        void __fastcall MakeDefaultFormats();
        void __fastcall LoadFormats();
        void __fastcall DoPrintCurrent();
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
