// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PropStorageEh.pas' rev: 4.00

#ifndef PropStorageEhHPP
#define PropStorageEhHPP

#pragma delphiheader begin
#pragma option push -w-
#include <TypInfo.hpp>	// Pascal unit
#include <IniFiles.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <PropFilerEh.hpp>	// Pascal unit
#include <Registry.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Propstorageeh
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPropStorageManagerEh;
class DELPHICLASS TPropStorageEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPropStorageManagerEh : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FWriteAsText;
	
protected:
	__property bool WriteAsText = {read=FWriteAsText, write=FWriteAsText, default=1};
	
public:
	__fastcall virtual TPropStorageManagerEh(Classes::TComponent* AOwner);
	virtual void __fastcall ReadProperties(TPropStorageEh* PropStorage);
	virtual void __fastcall ReadPropertiesStream(Classes::TStream* Stream, TPropStorageEh* PropStorage)
		;
	virtual void __fastcall WriteProperties(TPropStorageEh* PropStorage);
	virtual void __fastcall WritePropertiesStream(TPropStorageEh* PropStorage, Classes::TStream* Stream
		);
	virtual void __fastcall WritePropertiesText(TPropStorageEh* PropStorage, AnsiString Text);
public:
	#pragma option push -w-inl
	/* TComponent.Destroy */ inline __fastcall virtual ~TPropStorageManagerEh(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TIniPropStorageManEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TIniPropStorageManEh : public TPropStorageManagerEh 
{
	typedef TPropStorageManagerEh inherited;
	
private:
	AnsiString FIniFileName;
	
public:
	virtual void __fastcall ReadProperties(TPropStorageEh* PropStorage);
	virtual void __fastcall WritePropertiesStream(TPropStorageEh* PropStorage, Classes::TStream* Stream
		);
	virtual void __fastcall WritePropertiesText(TPropStorageEh* PropStorage, AnsiString Text);
	
__published:
	__property AnsiString IniFileName = {read=FIniFileName, write=FIniFileName};
	__property WriteAsText ;
public:
	#pragma option push -w-inl
	/* TPropStorageManagerEh.Create */ inline __fastcall virtual TIniPropStorageManEh(Classes::TComponent* 
		AOwner) : TPropStorageManagerEh(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TComponent.Destroy */ inline __fastcall virtual ~TIniPropStorageManEh(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

#pragma option push -b-
enum TRegistryKeyEh { rkClassesRootEh, rkCurrentUserEh, rkLocalMachineEh, rkUsersEh, rkPerformanceDataEh, 
	rkCurrentConfigEh, rkDynDataEh, rkCustomRegistryKeyEh };
#pragma option pop

class DELPHICLASS TRegPropStorageManEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TRegPropStorageManEh : public TPropStorageManagerEh 
{
	typedef TPropStorageManagerEh inherited;
	
private:
	HKEY FKey;
	AnsiString FPath;
	TRegistryKeyEh FRegistryKey;
	void __fastcall SerRegistryKey(const TRegistryKeyEh Value);
	void __fastcall SetKey(const HKEY Value);
	void __fastcall ReadPropertiesOld(TPropStorageEh* PropStorage);
	
public:
	__fastcall virtual TRegPropStorageManEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TRegPropStorageManEh(void);
	virtual void __fastcall ReadProperties(TPropStorageEh* PropStorage);
	virtual void __fastcall WritePropertiesStream(TPropStorageEh* PropStorage, Classes::TStream* Stream
		);
	virtual void __fastcall WritePropertiesText(TPropStorageEh* PropStorage, AnsiString Text);
	__property HKEY Key = {read=FKey, write=SetKey, default=-2147483647};
	
__published:
	__property TRegistryKeyEh RegistryKey = {read=FRegistryKey, write=SerRegistryKey, default=1};
	__property AnsiString Path = {read=FPath, write=FPath};
	__property WriteAsText ;
};

#pragma pack(pop)

typedef void __fastcall (__closure *TWriteCustomPropsEventEh)(System::TObject* Sender, Propfilereh::TPropWriterEh* 
	Writer);

typedef void __fastcall (__closure *TReadPropEventEh)(System::TObject* Sender, Propfilereh::TPropReaderEh* 
	Reader, AnsiString PropName, bool &Processed);

class DELPHICLASS TPropertyNamesEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPropertyNamesEh : public Classes::TStringList 
{
	typedef Classes::TStringList inherited;
	
private:
	Classes::TComponent* FRoot;
	void __fastcall SetRoot(const Classes::TComponent* Value);
	
protected:
	int __fastcall CompareStrings(const AnsiString S1, const AnsiString S2);
	bool __fastcall CheckPropertyPath(AnsiString Path);
	bool __fastcall CheckObjectPropertyPath(System::TObject* Instance, AnsiString PropPath);
	void __fastcall CheckPropertyNames(void);
	
public:
	virtual int __fastcall Add(const AnsiString S);
	__property Classes::TComponent* Root = {read=FRoot, write=SetRoot};
public:
	#pragma option push -w-inl
	/* TStringList.Destroy */ inline __fastcall virtual ~TPropertyNamesEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TPropertyNamesEh(void) : Classes::TStringList() { }
	#pragma option pop
	
};

#pragma pack(pop)

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPropStorageEh : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FActive;
	Classes::TNotifyEvent FAfterLoadProps;
	Classes::TNotifyEvent FAfterSaveProps;
	Classes::TNotifyEvent FBeforeLoadProps;
	Classes::TNotifyEvent FBeforeSaveProps;
	bool FDestroying;
	TReadPropEventEh FOnReadProp;
	TWriteCustomPropsEventEh FOnWriteCustomProps;
	Classes::TNotifyEvent FOnSavePlacement;
	bool FSaved;
	Forms::TCloseQueryEvent FSaveFormCloseQuery;
	Classes::TNotifyEvent FSaveFormDestroy;
	Classes::TNotifyEvent FSaveFormShow;
	AnsiString FSection;
	TPropStorageManagerEh* FStorageManager;
	TPropertyNamesEh* FStoredProps;
	Forms::TForm* __fastcall GetForm(void);
	AnsiString __fastcall GetSection();
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall RestoreEvents(void);
	void __fastcall SetEvents(void);
	void __fastcall SetSection(const AnsiString Value);
	void __fastcall SetStorageManager(const TPropStorageManagerEh* Value);
	void __fastcall SetStoredProps(const TPropertyNamesEh* Value);
	
protected:
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall Save(void);
	__property Forms::TForm* Form = {read=GetForm};
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	void __fastcall ReadProp(Propfilereh::TPropReaderEh* Reader, AnsiString PropName, bool &Processed);
		
	void __fastcall WriteCustomProps(Propfilereh::TPropWriterEh* Writer);
	
public:
	__fastcall virtual TPropStorageEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TPropStorageEh(void);
	void __fastcall LoadProperties(void);
	void __fastcall ReadPropValues(Classes::TStream* Stream);
	void __fastcall SaveProperties(void);
	void __fastcall WritePropValues(Classes::TStream* Stream);
	
__published:
	__property bool Active = {read=FActive, write=FActive, default=1};
	__property AnsiString Section = {read=GetSection, write=SetSection};
	__property TPropStorageManagerEh* StorageManager = {read=FStorageManager, write=SetStorageManager};
		
	__property TPropertyNamesEh* StoredProps = {read=FStoredProps, write=SetStoredProps};
	__property Classes::TNotifyEvent AfterLoadProps = {read=FAfterLoadProps, write=FAfterLoadProps};
	__property Classes::TNotifyEvent AfterSaveProps = {read=FAfterSaveProps, write=FAfterSaveProps};
	__property Classes::TNotifyEvent BeforeLoadProps = {read=FBeforeLoadProps, write=FBeforeLoadProps};
		
	__property Classes::TNotifyEvent BeforeSaveProps = {read=FBeforeSaveProps, write=FBeforeSaveProps};
		
	__property TWriteCustomPropsEventEh OnWriteCustomProps = {read=FOnWriteCustomProps, write=FOnWriteCustomProps
		};
	__property TReadPropEventEh OnReadProp = {read=FOnReadProp, write=FOnReadProp};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall GetDefinePropertyList(Classes::TPersistent* AObject, Classes::TStrings* 
	sl);
extern PACKAGE TPropStorageManagerEh* __fastcall DefaultPropStorageManager(void);
extern PACKAGE TPropStorageManagerEh* __fastcall SetDefaultPropStorageManager(TPropStorageManagerEh* 
	NewStorageManager);
extern PACKAGE bool __fastcall RegistryKeyToIdent(int RootKey, AnsiString &Ident);
extern PACKAGE bool __fastcall IdentToRegistryKey(const AnsiString Ident, int &RootKey);
extern PACKAGE void __fastcall GetRegistryKeyValues(Classes::TGetStrProc Proc);

}	/* namespace Propstorageeh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Propstorageeh;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PropStorageEh
