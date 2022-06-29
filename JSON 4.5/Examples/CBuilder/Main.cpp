//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ButtonOpenFileClick(TObject *Sender)
{
  if (OpenDialog->Execute())
  {
	TreeView->Items->BeginUpdate();
	try
	{
	  TreeView->Items->Clear();
	  TJsonParser *parser = new TJsonParser();
	  try
	  {
		TJson *jsonItem = parser->ParseUtf8File(OpenDialog->FileName);
		try
		{
		  Show(NULL, "", jsonItem);
		  TreeView->FullExpand();
		}
		__finally
		{
		  jsonItem->Free();
		}
	  }
	  __finally
	  {
		parser->Free();
	  }
	}
	__finally
	{
	  TreeView->Items->EndUpdate();
	}
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::Show(TTreeNode *parent, const String prefix, TJson *jsonItem)
{
	if (dynamic_cast<TJsonNull *>(jsonItem))
	  TreeView->Items->AddChild(parent, prefix + "null");
	else if (dynamic_cast<TJsonFalse *>(jsonItem))
	  TreeView->Items->AddChild(parent, prefix + "false");
	else if (dynamic_cast<TJsonTrue *>(jsonItem))
	  TreeView->Items->AddChild(parent, prefix + "true");
	else if (dynamic_cast<TJsonNumber *>(jsonItem))
	  TreeView->Items->AddChild(parent, prefix + FloatToStr(((TJsonNumber *)jsonItem)->Value));
	else if (dynamic_cast<TJsonString *>(jsonItem))
	  TreeView->Items->AddChild(parent, prefix + "\"" + ((TJsonString *)jsonItem)->Value + "\"");
	else if (dynamic_cast<TJsonArray *>(jsonItem))
	{
	  TTreeNode *child = TreeView->Items->AddChild(parent, prefix + "[]");
	  TJsonArray *jsonArray = (TJsonArray *)jsonItem;
	  for (int i = 0; i < jsonArray->ElementCount; ++i)
		Show(child, "[" + IntToStr(i) + "] ", jsonArray->Elements[i]);
	}
	else if (dynamic_cast<TJsonObject *>(jsonItem))
	{
	  TTreeNode *child = TreeView->Items->AddChild(parent, prefix + "{}");
	  TJsonObject *jsonObject = (TJsonObject *)jsonItem;
	  for (int i = 0; i < jsonObject->MemberCount; ++i)
		Show(child, jsonObject->MemberName[i] + ": ", jsonObject->MemberValue[i]);
	}
}
