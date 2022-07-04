//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "PageDemo.h"
//---------------------------------------------------------------------------
#pragma link "RXDBCtrl"
#pragma link "Grids"
#pragma link "RXCtrls"
#pragma link "ToolEdit"
#pragma link "RxDBComb"
#pragma link "Placemnt"
#pragma link "PageMngr"
#pragma link "RxQuery"
#pragma link "DBUtils"
#pragma link "BdeUtils"
#pragma resource "*.dfm"
TClientAssistant *ClientAssistant;
//---------------------------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
const int pageToDo = 0;
const int pageSearchConfirm = 1;
const int pageSearchParams = 2;
const int pageClientsBrowse = 3;
const int pageClientEdit = 4;
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::DoSearch(void)
{
  SearchQuery->Close();
  SearchQuery->MacroByName("LAST_NAME")->AsString = FormatSQLCondition("LAST_NAME",
    "", SrchName->Text, ftString, ExactBtn->Checked);
  SearchQuery->MacroByName("FIRST_NAME")->AsString = FormatSQLCondition("FIRST_NAME",
    "", SrchFirstName->Text, ftString, ExactBtn->Checked);
  SearchQuery->MacroByName("CITY")->AsString = FormatSQLCondition("CITY",
    "", SrchCity->Text, ftString, ExactBtn->Checked);
  SearchQuery->MacroByName("OCCUPATION")->AsString = FormatSQLCondition("OCCUPATION",
    "", SrchOccupation->Text, ftString, ExactBtn->Checked);
  SearchQuery->MacroByName("STATE")->AsString = FormatSQLCondition("STATE",
    "", SrchState->Text, ftString, ExactBtn->Checked);
  SearchQuery->Open();
}
//---------------------------------------------------------------------------
__fastcall TClientAssistant::TClientAssistant(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::FormCreate(TObject *Sender)
{
  Clients->Open();
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::SaveBtnClick(TObject *Sender)
{
  Clients->Post();
  PageManager->SetPage(pageToDo, true);
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ToDoLeave(bool Next)
{
  if (NewBtn->Checked) {
    Mode = camNew;
  }
  else if (EditBtn->Checked) {
    Mode = camEdit;
  }
  else if (DeleteBtn->Checked) {
    Mode = camDelete;
  }
  else if (ViewBtn->Checked) {
    Mode = camView;
  }
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::SearchParamsLeave(bool Next)
{
  if (Next) {
    DoSearch();
  }
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ClientsBrowseLeave(bool Next)
{
  if (Next) {
    if ((IsDataSetEmpty(SearchQuery)) ||
      !(DataSetFindValue(Clients, SearchQuery->FieldByName("ACCT_NBR")->AsString,
      "ACCT_NBR"))) {
      DatabaseError("No data found");
    }
    if ((Mode == camDelete) && ConfirmDelete()) {
      Clients->Delete();
      RefreshQuery(SearchQuery);
    }
  }
  else {
    SearchQuery->Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ClientEditEnter(bool Next)
{
  if (Mode == camNew) {
    Clients->Append();
  }
  else if (Mode == camEdit) {
    Clients->Edit();
  }
  SaveBtn->Visible = (Clients->State == dsInsert) || (Clients->State == dsEdit);
  SaveBtn->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ClientEditLeave(bool Next)
{
  if (!Next) {
    ConfirmDataSetCancel(Clients);
  }
  SaveBtn->Visible = false;
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ClientsBrowseShow(bool Next)
{
  ActiveControl = Grid;
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::ClientEditShow(bool Next)
{
  ActiveControl = EditACCT_NBR;
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::FormCloseQuery(TObject *Sender,
        bool &CanClose)
{
  ConfirmDataSetCancel(Clients);
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::PageManagerGetNextPage(int CurrentPage,
        int &NewPage)
{
  switch (CurrentPage) {
    case pageToDo: {
      switch (Mode) {
        case camNew: {
          NewPage = pageSearchConfirm;
          break;
        }
        case camEdit:
        case camDelete:
        case camView: {
          NewPage = pageSearchParams;
          break;
        }
      }
      break;
    }
    case pageSearchConfirm: {
      if (DoSearchBtn->Checked) {
        NewPage = pageSearchParams;
      }
      else if (SkipSearchBtn->Checked) {
        NewPage = pageClientEdit;
      }
      break;
    }
    case pageSearchParams: {
      NewPage = CurrentPage + 1;
      break;
    }
    case pageClientsBrowse: {
      NewPage = CurrentPage + 1;
      if (Mode == camDelete) {
        NewPage = CurrentPage;
      }
      else if ((Mode == camEdit) || (Mode == camView)) {
        if (IsDataSetEmpty(SearchQuery)) {
          NewPage = -1;
        }
      }
      break;
    }
    case pageClientEdit: {
      NewPage = -1;
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::PageManagerGetPriorPage(int CurrentPage,
        int &NewPage)
{
  switch (CurrentPage) {
    case pageToDo: {
      NewPage = -1;
      break;
    }
    case pageSearchConfirm: {
      NewPage = CurrentPage - 1;
      break;
    }
    case pageSearchParams: {
      if (Mode == camNew) {
        NewPage = pageSearchConfirm;
      }
      else {
        NewPage = pageToDo;
      }
      break;
    }
    case pageClientsBrowse: {
      NewPage = CurrentPage - 1;
      break;
    }
    case pageClientEdit: {
      if (!DoSearchBtn->Checked && (Mode == camNew)) {
        NewPage = pageSearchConfirm;
      }
      else {
        NewPage = pageClientsBrowse;
      }
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::CloseBtnClick(TObject *Sender)
{
  Close();        
}
//---------------------------------------------------------------------------
void __fastcall TClientAssistant::EditChange(TObject *Sender)
{
  SaveBtn->Enabled = true;        
}
//---------------------------------------------------------------------------
