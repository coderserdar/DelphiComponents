// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PropStorageEditEh.pas' rev: 6.00

#ifndef PropStorageEditEhHPP
#define PropStorageEditEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <TypInfo.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <CheckLst.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <PropFilerEh.hpp>	// Pascal unit
#include <PropStorageEh.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Propstorageediteh
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TNodeTypeEh { nthProperty, nthControl, nthPropNode };
#pragma option pop

class DELPHICLASS TNodeInfoEh;
class PASCALIMPLEMENTATION TNodeInfoEh : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	int Checked;
	System::TObject* Instance;
	TNodeTypeEh NodeType;
	bool IsVoidProperty;
	AnsiString Name;
	AnsiString Path;
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TNodeInfoEh(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TNodeInfoEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPropStorageEditEhForm;
class PASCALIMPLEMENTATION TPropStorageEditEhForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Buttons::TSpeedButton* spAddProp;
	Buttons::TSpeedButton* sbRemoveAllProps;
	Buttons::TSpeedButton* sbRemoveProp;
	Comctrls::TTreeView* TreeView1;
	Comctrls::TTreeView* TreeView2;
	Stdctrls::TButton* bOk;
	Stdctrls::TButton* bCancel;
	Extctrls::TBevel* Bevel1;
	Controls::TImageList* ImageList1;
	Checklst::TCheckListBox* cbPredifinedProps;
	Stdctrls::TLabel* lCompsAndProps;
	Stdctrls::TLabel* lStoredProps;
	Stdctrls::TEdit* Edit1;
	Stdctrls::TEdit* Edit2;
	Buttons::TSpeedButton* spSynchTrees;
	void __fastcall TreeView1MouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeView1MouseMove(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeView1MouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeView1GetSelectedIndex(System::TObject* Sender, Comctrls::TTreeNode* Node);
	void __fastcall TreeView1Deletion(System::TObject* Sender, Comctrls::TTreeNode* Node);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall spAddPropClick(System::TObject* Sender);
	void __fastcall sbRemovePropClick(System::TObject* Sender);
	void __fastcall TreeView1Change(System::TObject* Sender, Comctrls::TTreeNode* Node);
	void __fastcall TreeView2MouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall sbRemoveAllPropsClick(System::TObject* Sender);
	void __fastcall cbPredifinedPropsClickCheck(System::TObject* Sender);
	void __fastcall TreeView1Compare(System::TObject* Sender, Comctrls::TTreeNode* Node1, Comctrls::TTreeNode* Node2, int Data, int &Compare);
	void __fastcall TreeView1Expanding(System::TObject* Sender, Comctrls::TTreeNode* Node, bool &AllowExpansion);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall spSynchTreesClick(System::TObject* Sender);
	
protected:
	Comctrls::TTreeNode* __fastcall AddSortedChildObject(Comctrls::TTreeNode* Parent, const AnsiString S, TNodeInfoEh* Data);
	int __fastcall CompareNode(Comctrls::TTreeNode* Node1, Comctrls::TTreeNode* Node2);
	int __fastcall CompareNodeData(TNodeInfoEh* Data1, TNodeInfoEh* Data2);
	TNodeInfoEh* __fastcall CreateNodeInfo(Classes::TComponent* Component, AnsiString Name, AnsiString Path, TNodeTypeEh NodeType, bool IsVoidProperty);
	Comctrls::TTreeNode* __fastcall FindChildNodeInfo(Comctrls::TTreeNode* N2, Comctrls::TTreeNode* N1);
	Comctrls::TTreeNode* __fastcall GetChildNodeByText(Comctrls::TTreeNode* ParentNode, AnsiString Text);
	Ehlibvcl::TPropListArray __fastcall GetObjectPropList(System::TObject* AObject, int &ObjPropCount);
	bool __fastcall HaveCheckedChilds(Comctrls::TTreeNode* N);
	void __fastcall AddCollectionProperties(Comctrls::TTreeNode* N, Classes::TCollection* O, AnsiString Path);
	void __fastcall AddComponents(Comctrls::TTreeNode* N, Classes::TComponent* O, AnsiString Path);
	void __fastcall AddParentChecked(Comctrls::TTreeNode* N);
	void __fastcall AddProperties(Comctrls::TTreeNode* N, System::TObject* O, AnsiString Path, bool IsAddPropNode);
	void __fastcall AddPropertyNode(Comctrls::TTreeNode* N);
	void __fastcall AddVoidProperty(Comctrls::TTreeNode* N);
	void __fastcall ExchangeNode(Comctrls::TTreeNode* Parent, int L, int R);
	void __fastcall MainAddPropertyNode(Comctrls::TTreeNode* N)/* overload */;
	void __fastcall MainAddPropertyNode(AnsiString Path)/* overload */;
	void __fastcall MainDeletePropertyNode(Comctrls::TTreeNode* N)/* overload */;
	void __fastcall MainDeletePropertyNode(AnsiString Path)/* overload */;
	void __fastcall MainToggle(Comctrls::TTreeNode* N);
	void __fastcall QuickSort(Comctrls::TTreeNode* Parent, int L, int R);
	void __fastcall RemovePropertyNode(Comctrls::TTreeNode* N);
	void __fastcall ResetChildNodes(Comctrls::TTreeNode* N);
	void __fastcall ResetParentNodes(Comctrls::TTreeNode* N);
	void __fastcall SlaveDeleteNode(Comctrls::TTreeNode* SN);
	
public:
	int LeftBorderWidth;
	int RightBorderWidth;
	int ButtonSize;
	int VBottomMargin;
	Comctrls::TTreeNode* OnIconDownNode;
	Propstorageeh::TPropStorageEh* PropStorage;
	Comctrls::TTreeNode* RootNode;
	unsigned StartBuildTicks;
	void __fastcall BuildPredifinedProps(void);
	void __fastcall BuildPropertyList(void);
	void __fastcall BuildStoringPropertyList(Classes::TStrings* PropList);
	void __fastcall GetStoringPorps(Classes::TStrings* PropList);
	void __fastcall PropertyAdded(Comctrls::TTreeNode* DN);
	void __fastcall PropertyDeleting(Comctrls::TTreeNode* DN);
	void __fastcall UpdateButtonState(void);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TPropStorageEditEhForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TPropStorageEditEhForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TPropStorageEditEhForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TPropStorageEditEhForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TPredifinedPropsEh;
class PASCALIMPLEMENTATION TPredifinedPropsEh : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	bool FCkecked;
	TPropStorageEditEhForm* FEditForm;
	virtual AnsiString __fastcall Caprion();
	virtual bool __fastcall PropertyAdded(Classes::TComponent* Component, AnsiString PropPath);
	virtual bool __fastcall PropertyDeleted(Classes::TComponent* Component, AnsiString PropPath);
	virtual void __fastcall SetCkecked(bool AChecked);
	__fastcall virtual TPredifinedPropsEh(TPropStorageEditEhForm* EditForm);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TPredifinedPropsEh(void) { }
	#pragma option pop
	
};


typedef TMetaClass*TPredifinedPropsEhClass;

class DELPHICLASS TPredifinedActiveControlEh;
class PASCALIMPLEMENTATION TPredifinedActiveControlEh : public TPredifinedPropsEh 
{
	typedef TPredifinedPropsEh inherited;
	
protected:
	bool FActiveControlAdded;
	virtual AnsiString __fastcall Caprion();
	virtual bool __fastcall PropertyAdded(Classes::TComponent* Component, AnsiString PropPath);
	virtual bool __fastcall PropertyDeleted(Classes::TComponent* Component, AnsiString PropPath);
	virtual void __fastcall SetCkecked(bool AChecked);
protected:
	#pragma option push -w-inl
	/* TPredifinedPropsEh.Create */ inline __fastcall virtual TPredifinedActiveControlEh(TPropStorageEditEhForm* EditForm) : TPredifinedPropsEh(EditForm) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TPredifinedActiveControlEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPredifinedPosPropertiesEh;
class PASCALIMPLEMENTATION TPredifinedPosPropertiesEh : public TPredifinedPropsEh 
{
	typedef TPredifinedPropsEh inherited;
	
protected:
	bool FLeftAdded;
	bool FTopAdded;
	virtual AnsiString __fastcall Caprion();
	virtual bool __fastcall PropertyAdded(Classes::TComponent* Component, AnsiString PropPath);
	virtual bool __fastcall PropertyDeleted(Classes::TComponent* Component, AnsiString PropPath);
	virtual void __fastcall SetCkecked(bool AChecked);
protected:
	#pragma option push -w-inl
	/* TPredifinedPropsEh.Create */ inline __fastcall virtual TPredifinedPosPropertiesEh(TPropStorageEditEhForm* EditForm) : TPredifinedPropsEh(EditForm) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TPredifinedPosPropertiesEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPredifinedSizePropertiesEh;
class PASCALIMPLEMENTATION TPredifinedSizePropertiesEh : public TPredifinedPropsEh 
{
	typedef TPredifinedPropsEh inherited;
	
protected:
	bool FHeightAdded;
	bool FPixelsPerInchAdded;
	bool FWidthAdded;
	virtual AnsiString __fastcall Caprion();
	virtual bool __fastcall PropertyAdded(Classes::TComponent* Component, AnsiString PropPath);
	virtual bool __fastcall PropertyDeleted(Classes::TComponent* Component, AnsiString PropPath);
	virtual void __fastcall SetCkecked(bool AChecked);
protected:
	#pragma option push -w-inl
	/* TPredifinedPropsEh.Create */ inline __fastcall virtual TPredifinedSizePropertiesEh(TPropStorageEditEhForm* EditForm) : TPredifinedPropsEh(EditForm) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TPredifinedSizePropertiesEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPredifinedStatePropertiesEh;
class PASCALIMPLEMENTATION TPredifinedStatePropertiesEh : public TPredifinedPropsEh 
{
	typedef TPredifinedPropsEh inherited;
	
protected:
	bool FStateAdded;
	virtual AnsiString __fastcall Caprion();
	virtual bool __fastcall PropertyAdded(Classes::TComponent* Component, AnsiString PropPath);
	virtual bool __fastcall PropertyDeleted(Classes::TComponent* Component, AnsiString PropPath);
	virtual void __fastcall SetCkecked(bool AChecked);
protected:
	#pragma option push -w-inl
	/* TPredifinedPropsEh.Create */ inline __fastcall virtual TPredifinedStatePropertiesEh(TPropStorageEditEhForm* EditForm) : TPredifinedPropsEh(EditForm) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TPredifinedStatePropertiesEh(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool __fastcall EditPropStorage(Propstorageeh::TPropStorageEh* PropStorage);
extern PACKAGE void __fastcall RegisterPredifinedPropsClass(TMetaClass* PropsClass);

}	/* namespace Propstorageediteh */
using namespace Propstorageediteh;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PropStorageEditEh
