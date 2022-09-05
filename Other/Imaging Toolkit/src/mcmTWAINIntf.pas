// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  15896: mcmTWAINIntf.pas 
//
//    Rev 1.18    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.17    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.16    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.15    26-08-2009 22:39:50  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.14    08-01-2009 21:09:22  mcm    Version: DT 3.8
// Added support for Delphi 2009
//
//   Rev 1.13    28-10-2004 20:05:44  mcm    Version: DT 3.2
// Added override methods of OpenDSM.

//
//   Rev 1.12    08-07-2004 23:13:36  mcm    Version: DT3.0
// GetCapabilityMsg, fixed clean-up of pContainer.

//
//   Rev 1.11    04-11-2003 23:11:36  mcm    Version: DT3.0
// Moved code to the TmcmTWAINThread and TmcmTWAINQueue threads.

//
//   Rev 1.10    24-10-2003 13:03:28  mcm
// In DoOnXferNext, added check for whether or not to close the connection.

//
//   Rev 1.9    29-07-2003 11:33:48  mcm
// In DoMemTransfer OnMemXferBuf is only fired when received data count
// (MemXfer.BytesWritten) is greater than zero. 

//
//   Rev 1.8    06-07-2003 10:59:58  mcm    Version: DT 2.5
// Added LenientOnCaps property.

//
//   Rev 1.7    20-05-2003 17:03:50  mcm
// Fixed the problem when FileFormat is not a part of FileFormats selected file
// transfer reverts to native transfer.

//
//   Rev 1.6    15-04-2003 10:54:56  mcm    Version: DT 2.3
// Added a check for zero sized buffer values in DoOnMemXferSize.
// Modified DoMemTransfer to handle zero sized buffer values returned by the
// data source.

//
//   Rev 1.5    06-03-2003 10:49:20  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.
// Changed visibility of GetDefaultSource to public.
// Changed Count parameter in PendingXfers to smallint.

//
//   Rev 1.4    07-10-2002 15:56:36  mcm    Version: DT2.1
// Added ModalUI property (read only)

//
//   Rev 1.3    21-01-2002 11:51:24  mcm    Version: DT 2.0
// Fixed XferMech propery reference.

//
//   Rev 1.2    18-01-2002 15:11:56  mcm    Version: DT 2.0
// Access to all property members are through function/procedures.

//
//   Rev 1.1    11-01-2002 15:20:36  mcm    Version: DT 2.0
// Changed casing on TtwnXX variables to TTwnXX.

//
//   Rev 1.0    04-12-2001 16:49:08  mcm    Version: DT 2.0

unit mcmTWAINIntf;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, System.Classes, WinApi.Messages,
     {$IFDEF GE_DXE4}
     System.AnsiStrings,
     {$ENDIF}
     {$ELSE}
     Windows, Messages, Classes, SysUtils,
     {$ENDIF}
     twain,
     mcmTWAINKernel,
     mcmTWAINContainer;

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

type
  TmcmTWAINIntf = class(TmcmTWAINKernel)
  private
    { Private declarations }
    FConList        : TTwnContainerList; // List of negotiated capabilities.
    FLenientOnCaps  : boolean;           // When True capabilities are
                                         // negotiated disregarding what the
                                         // data source reported in
                                         // CAP_SUPPORTEDCAPS.
    FSupportCaps    : TTwnContainer;     // List of supported capabilities in
                                         // state 4
    FSupportCapsEx  : TTwnContainer;     // List of supported capabilities in
                                         // state 5 & 6.
    FNumCap         : integer;           // Number of capabilities in
                                         // FSupportCaps.
    FNumExCap       : integer;           // Number of extended capabilities in
                                         // FSupportCapsEx.
    FDeviceEvents   : TTwnDeviceEvents;  // Device events to react opun.
    FOnDeviceEvent  : TDeviceEvent;      // This event is fired when then data
                                         // source needs to alert the application
                                         // of some changes in the device.

    FOnXferProgress : TProgressEvent;    // Image transfer progress event.
  protected
    { Protected declarations }
    FShowUI         : boolean;           // Show TWAIN driver interface.
    FModalUI        : boolean;
    FIndicators     : boolean;           // If TRUE the source will display a
                                         // progress indicator during acquisition
                                         // regardless of whether the source's UI
                                         // is active.
                                         // If FALSE, the progress indicator will
                                         // be suppressed, if UI is inactive.
    procedure   Clear; virtual;
    // OBSOLITE, CreateContainer functions is canceled, and should not be used.
    function    CreateContainer  (    Cap           : word;
                                      ConType       : word;
                                      ItemType      : word;
                                      NumItems      : word) : TTwnContainer;
    procedure   DoOnDeviceEvent  (    Sender        : TObject); override;
    function    GetCapConSize    (    ItemType      : TW_UINT16) : integer;
    function    GetCapMsg        (    Cap           : TW_UINT16;
                                      Msg           : TW_UINT16;
                                  var ConType       : TW_UINT16;
                                  var pCapCon       : Pointer) : TW_UINT16;
    function    GetCIEColor      (    pCIE          : pTW_CIECOLOR) : TW_UINT16;
    function    GetContainers : TTwnContainerList;
    function    GetDeviceEvents : TTwnDeviceEvents;
    function    GetFirstSource   (var SourceName    : string) : TW_UINT16;
    function    GetImageFileFmt : TTwnFileFmt;
    function    GetModalUI : boolean;
    function    GetNextSource    (var SourceName    : string) : TW_UINT16;
    function    GetNumCap : integer;
    function    GetPixelFlavor : integer;
    function    GetShowIndicators  : boolean;
    function    GetShowUI : boolean;
    procedure   GetSupportedCaps;
    procedure   GetSupportedExCaps;
    function    GetUnits : integer;
    function    OpenDSM : TW_UINT16;

    function    SendBoolCap      (    Cap           : TW_UINT16;
                                      Msg           : TW_UINT16;
                                      Value         : boolean) : TW_UINT16;
    function    SendCapabilityMsg(    Cap           : TW_UINT16;
                                      Msg           : TW_UINT16;
                                      ptwCapability : pTW_CAPABILITY) : TW_INT16;
    function    SetCapMsg        (    Cap           : TW_UINT16;
                                      Msg           : TW_UINT16;
                                      ConType       : TW_UINT16;
                                      pCapCon       : Pointer) : TW_UINT16;
    function    SetDefaultSource (    SourceName    : string) : TW_UINT16;
    procedure   SetDeviceEvents  (    Value         : TTwnDeviceEvents);
    function    SetImageFileFmt  (    ItemValue     : TTwnFileFmt) : TW_UINT16;
    function    SetIndicators : TW_UINT16;
    procedure   SetPixelFlavor   (    Value         : integer);
    procedure   SetShowIndicators(    Value         : boolean);
    procedure   SetShowUI        (    Value         : boolean);
    procedure   SetUnits         (    Value         : integer);
    function    SetupXferMech : TW_INT16;
    function    UserInterface    (    Msg           : TW_UINT16;
                                      pUI           : pTW_USERINTERFACE) : TW_UINT16;
    function    XferGroup        (    Msg           : TW_UINT16;
                                  var Group         : TW_UINT32) : TW_UINT16;
    function    GrayResponse     (    pGrayResponse : pTW_GRAYRESPONSE;
                                      Msg           : TW_UINT16) : TW_UINT16;
    function    RGBResponse      (    pRGBResponse  : pTW_GRAYRESPONSE;
                                      Msg           : TW_UINT16) : TW_UINT16;
    function    JPEGCompression  (    pJPEGComp     : pTW_JPEGCOMPRESSION;
                                      Msg           : TW_UINT16) : TW_UINT16;
  public
    { Public declarations }
    constructor Create           (    AOwner        : TComponent); override;
    destructor  Destroy; override;

    function    GetCapabilityMsg (    Cap           : word;
                                      Msg           : word;
                                  var Container     : TTwnContainer) : integer; virtual;
    function    GetDefaultSource (var SourceName    : string) : TW_UINT16;
    function    IsCapSupported   (    Value         : integer) : boolean; virtual;
    function    IsExCapSupported (    Value         : integer) : boolean; virtual;
    function    SetCapabilityMsg (    Msg           : word;
                                      AsOneValue    : bool;
                                      Container     : TTwnContainer) : integer; virtual;

    property    Containers : TTwnContainerList
      read      GetContainers;
    property    DeviceEventTypes : TTwnDeviceEvents
      read      GetDeviceEvents
      write     SetDeviceEvents default [];
    property    LenientOnCaps  : boolean
      read      FLenientOnCaps
      write     FLenientOnCaps default False;
    property    NumCapabilities : integer
      read      GetNumCap;
    property    ShowIndicators : boolean
      read      GetShowIndicators
      write     SetShowIndicators default True;
    property    ShowUI : boolean
      read      GetShowUI
      write     SetShowUI default True;
    property    ModalUI : boolean
      read      GetModalUI;
    property    OnDeviceEvent : TDeviceEvent
      read      FOnDeviceEvent
      write     FOnDeviceEvent;
    property    OnXferProgress : TProgressEvent
      read      FOnXferProgress
      write     FOnXferProgress;
  published
    { Pulished declarations }
  end;

implementation

uses mcmTWAINFix;

type
  // Defined to allow access to protected members in TTwnContainer and
  // TTwnContainerList via type-cast.
  TTypeCastContainer = class(TTwnContainer);
  TTypeCastContainerList = class(TTwnContainerList);


{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}


constructor TmcmTWAINIntf.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  // Clear all event handles.
  FOnDeviceEvent  := Nil;
  FOnXferProgress := Nil;

  // Create ContainerList.
  FConList := TTwnContainerList.Create(Self);

  FSupportCaps   := Nil;
  FSupportCapsEx := Nil;
  FNumCap        := 0;
  FNumExCap      := 0;
  FShowUI        := True;
  FModalUI       := False;
  FIndicators    := True;
  FDeviceEvents  := [];
  FLenientOnCaps := False;

  // The TmcmTWAINThread cannot use TmcmContainer's so a backwards call is
  // performed during Memory transfers to the following methods.
  if Assigned(FTWAINThread)
  then begin
       FTWAINThread.GetUnits       := GetUnits;
       FTWAINThread.GetPixelFlavor := GetPixelFlavor;
  end;
end; // TmcmTWAINIntf.Create.


destructor TmcmTWAINIntf.Destroy;
begin
  // Free ContainerList.
  FConList.Free;
  Inherited Destroy;
end; // TmcmTWAINIntf.Destroy.


procedure TmcmTWAINIntf.Clear;
begin
  FSupportCaps   := Nil;
  FSupportCapsEx := Nil;
  FNumCap        := 0;
  FNumExCap      := 0;
  FConList.Clear;
end; // TmcmTWAINIntf.Clear.


function TmcmTWAINIntf.OpenDSM : TW_UINT16;
begin
  Result := Inherited OpenDSM;
  // The TmcmTWAINThread cannot use TmcmContainer's so a backwards call is
  // performed during Memory transfers to the following methods.
  //FTWAINThread.GetUnits       := GetUnits;
  //FTWAINThread.GetPixelFlavor := GetPixelFlavor;
end; //


function TmcmTWAINIntf.CreateContainer(Cap      : word;
                                       ConType  : word;
                                       ItemType : word;
                                       NumItems : word) : TTwnContainer;
// OBSOLITE, This functions is canceled, and should not be used.
var Container : TTwnContainer;
begin
  try
    Container := TTwnContainer.Create(Self);
    if Assigned(Container)
    then begin
         Container.Capability    := Cap;
         Container.ContainerType := ConType;
         Container.ItemType      := ItemType;
         Container.NumItems      := NumItems;
         TTypeCastContainerList(FConList).AddItem(Cap, Container);
    end;
  except
    on exception
    do Container := Nil;
  end;
  Result := Container;
end; // TmcmTWAINIntf.CreateContainer.                                      


function TmcmTWAINIntf.GetContainers : TTwnContainerList;
begin
  Result := FConList;
end; // TmcmTWAINIntf.GetContainer.                                         


function TmcmTWAINIntf.GetNumCap : integer;
begin
  Result := FNumCap;
end; // TmcmTWAINIntf.GetNumCap.


function TmcmTWAINIntf.SendBoolCap(Cap           : TW_UINT16;
                                   Msg           : TW_UINT16;
                                   Value         : boolean) : TW_UINT16;
var Container  : TTwnContainer;
begin
  Result := TWRC_FAILURE;
  Container := Containers.Items[Cap];
  if Not(Assigned(Container))
  then Container := Containers.CreateItem(Cap);
  if Assigned(Container)
  then begin
       Container.ItemType := TWTY_BOOL;
       Container.CurrentValue := Value;
       Result := SetCapabilityMsg(Msg, True, Container);
  end;
end; // TmcmTWAINIntf.SendBoolCap.


function TmcmTWAINIntf.SendCapabilityMsg(Cap           : TW_UINT16;
                                         Msg           : TW_UINT16;
                                         ptwCapability : pTW_CAPABILITY) : TW_INT16;
{------------------------------------------------------------------------------}
{ SendCapabilityMsg - Sends capability messages to the source and              }
{ returns the TW_CAPABILITY structure to the caller as a parameter.            }
{ If the DSM_Entry call fails, TWRC_FAILURE is returned.                       }
{------------------------------------------------------------------------------}
begin
  Result := TWRC_SUCCESS;

  // Fail if DSM isn't open.
  if Not(IsDSMOpen)
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M SendCapabilityMsg, DSM is not open.');
       {$ENDIF}
       Result := TWRC_FAILURE;
  end;

  // Fail if DS isn't open.
  if (Result = TWRC_SUCCESS)
  then begin
       if Not(IsDSOpen)
       then begin
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M SendCapabilityMsg, DS is not open.');
            {$ENDIF}
            Result := TWRC_FAILURE;
       end;
  end;

  if (Result = TWRC_SUCCESS)
  then begin
       ptwCapability^.Cap := Cap;
       case Msg of
       MSG_GET,
       MSG_GETDEFAULT,
       MSG_RESET        : begin
                            ptwCapability^.ConType    := TWON_DONTCARE16;
                            ptwCapability^.hContainer := 0;
                          end;
       MSG_QUERYSUPPORT : begin
                            ptwCapability^.ConType    := TWON_DONTCARE16; // TWON_ONEVALUE;
                            ptwCapability^.hContainer := 0;
                          end;
       end;
       Result := DSMEntry(@FAppIdentity,
                          @FSourceID,
                          DG_CONTROL,
                          DAT_CAPABILITY,
                          Msg,
                          TW_MEMREF(ptwCapability));
  end;
end; // TmcmTWAINIntf.SendCapabilityMsg.



function TmcmTWAINIntf.GetCapConSize(ItemType : TW_UINT16) : integer;
// GetCapConSize calculates the unpacked data size, ie. data sizes less
// than TW_UNIT32 are "re-scaled" to an TW_UNIT32.
var Size : integer;
begin
  Size := TWItemSize[ItemType];
  if (Size < SizeOf(TW_UINT32))
  then Size := SizeOf(TW_UINT32);
  Result := Size;
end; // TmcmTWAINIntf.GetCapConSize.                                        


function TmcmTWAINIntf.GetCapMsg(    Cap     : TW_UINT16;
                                     Msg     : TW_UINT16;
                                 var ConType : TW_UINT16;
                                 var pCapCon : Pointer) : TW_UINT16;
{------------------------------------------------------------------------------}
{ GetCapMsg                                                                    }
{ The GetCapMsg functions un-pack the data structures returned by the data     }
{ source.                                                                      }
{                                                                              }
{ TW_BOOL's must be treated as "0" = FALSE and "1" = TRUE.                     }
{ TW_STRxxx data types are treated as their respective data type.              }
{------------------------------------------------------------------------------}
(*
    procedure GetValue(pItem    : pointer;
                       Value    : pointer;
                       ItemType : TW_UINT16);
    begin
      // Add a check for valid type.
      // CAST to type of var caller wants.
      case ItemType of
      TWTY_INT8   : pTW_UINT32(Value)^ := TW_INT8(pItem^);
      TWTY_UINT8  : pTW_UINT32(Value)^ := TW_UINT8(pItem^);
      TWTY_INT16  : pTW_UINT32(Value)^ := TW_INT16(pItem^);
      TWTY_UINT16 : pTW_UINT32(Value)^ := TW_UINT16(pItem^);
      TWTY_INT32  : pTW_UINT32(Value)^ := TW_INT32(pItem^);
      TWTY_UINT32 : pTW_UINT32(Value)^ := TW_UINT32(pItem^);
      TWTY_BOOL   : pTW_UINT32(Value)^ := TW_BOOL(pItem^);
      TWTY_FIX32  : pTW_FIX32(Value)^  := TW_FIX32(pItem^);
      TWTY_STR32  : CopyMemory(pTW_STR32(Value), pTW_STR32(pItem), SizeOf(TW_STR32));
      TWTY_STR64  : CopyMemory(pTW_STR64(Value), pTW_STR64(pItem), SizeOf(TW_STR64));
      TWTY_STR128 : CopyMemory(pTW_STR128(Value), pTW_STR128(pItem), SizeOf(TW_STR128));
      TWTY_STR255 : CopyMemory(pTW_STR255(Value), pTW_STR255(pItem), SizeOf(TW_STR255));
      TWTY_FRAME  : CopyMemory(pTW_FRAME(Value), pTW_FRAME(pItem), SizeOf(TW_FRAME));
      {$IFDEF VER120}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ELSE}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ENDIF}
      end;
    end; // GetValue.                                                       
*)

    procedure ItemToUInt32(var pItem    : PAnsiChar;
                               Value    : pointer;
                               ItemType : TW_UINT16;
                               ItemSize : integer);
    begin
      case ItemType of
      TWTY_INT8   : pTW_UINT32(Value)^ := pTW_INT8(pItem)^;
      TWTY_INT16  : pTW_UINT32(Value)^ := pTW_INT16(pItem)^;
      TWTY_INT32  : pTW_UINT32(Value)^ := pTW_INT32(pItem)^;
      TWTY_UINT8  : pTW_UINT32(Value)^ := pTW_UINT8(pItem)^;
      TWTY_UINT16 : pTW_UINT32(Value)^ := pTW_UINT16(pItem)^;
      TWTY_UINT32 : pTW_UINT32(Value)^ := pTW_UINT32(pItem)^;
      TWTY_BOOL   : pTW_UINT32(Value)^ := pTW_BOOL(pItem)^;
      TWTY_FIX32  : pTW_FIX32(Value)^ := pTW_FIX32(pItem)^;
      TWTY_STR32  : CopyMemory(pTW_STR32(Value), pTW_STR32(pItem), SizeOf(TW_STR32));
      TWTY_STR64  : CopyMemory(pTW_STR64(Value), pTW_STR64(pItem), SizeOf(TW_STR64));
      TWTY_STR128 : CopyMemory(pTW_STR128(Value), pTW_STR128(pItem), SizeOf(TW_STR128));
      TWTY_STR255 : CopyMemory(pTW_STR255(Value), pTW_STR255(pItem), SizeOf(TW_STR255));
      TWTY_FRAME  : CopyMemory(pTW_FRAME(Value), pTW_FRAME(pItem), SizeOf(TW_FRAME));
      {$IFDEF VER120}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ELSE}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ENDIF}
      end;
      pItem := pItem + ItemSize;
    end; // ItemToUInt32.                                                   


var twCap        : TW_CAPABILITY;
    pArray       : pTW_ARRAY;
    pRange       : pTW_RANGE;
    pOneValue    : pTW_ONEVALUE;
    pEnumeration : pTW_ENUMERATION;

    pItem        : PAnsiChar;
    pItemList    : PAnsiChar;
    i, ItemSize  : integer;
    ItemSizes    : integer;
    UnpackSize   : integer;
begin
  pCapCon := Nil;
  // App ONLY fills in the CAP_ of interest.
  // Have Source build a container and fill it with the CAP_ of interest.
  twCap.hContainer := 0;
  Result := SendCapabilityMsg(Cap, Msg, @twCap);

  if (Result = TWRC_SUCCESS)
  then begin
       // Add code to do a switch on contype.  Then call the appropriate extract
       // routine from the common container code routines.
       case twCap.ConType of
       TWON_ARRAY       : begin
                            pArray := pTW_ARRAY(DSMMemLock(twCap.hContainer));
                            if (pArray <> Nil)
                            then begin
                                 UnpackSize := GetCapConSize(pArray^.ItemType);
                                 ItemSizes  := UnpackSize * pArray^.NumItems;
                                 GetMem(pCapCon, SizeOf(TW_ARRAY) + ItemSizes);
                                 FillChar(pCapCon^, SizeOf(TW_ARRAY) + ItemSizes, 0);
                                 if (pCapCon <> Nil)
                                 then begin
                                      ConType := TWON_ARRAY;
                                      with TW_ARRAY(pCapCon^)
                                      do begin
                                         ItemType  := pArray^.ItemType;
                                         NumItems  := pArray^.NumItems;
                                         ItemSize  := TWItemSize[ItemType];
                                         pItem     := @pArray^.ItemList;
                                         pItemList := @ItemList[0];
                                         for i := 0 to (NumItems - 1)
                                         do begin
                                            ItemToUInt32(pItem, pItemList, ItemType, ItemSize);
                                            pItemList := pItemList + UnpackSize;
                                         end;
                                      end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_ENUMERATION : begin
                            pEnumeration := pTW_ENUMERATION(DSMMemLock(twCap.hContainer));
                            if (pEnumeration <> Nil)
                            then begin
                                 UnpackSize := GetCapConSize(pEnumeration^.ItemType);
                                 ItemSizes  := UnpackSize * pEnumeration^.NumItems;
                                 GetMem(pCapCon, SizeOf(TW_ENUMERATION) + ItemSizes);

                                 if (pCapCon <> Nil)
                                 then begin
                                      ConType := TWON_ENUMERATION;
                                      with TW_ENUMERATION(pCapCon^)
                                      do begin
                                         ItemType     := pEnumeration^.ItemType;
                                         NumItems     := pEnumeration^.NumItems;
                                         CurrentIndex := pEnumeration^.CurrentIndex;
                                         DefaultIndex := pEnumeration^.DefaultIndex;
                                         pItem        := @pEnumeration^.ItemList;
                                         ItemSize     := TWItemSize[ItemType];
                                         pItemList    := @ItemList[0];
                                         for i := 0 to (NumItems - 1)
                                         do begin
                                            ItemToUInt32(pItem, pItemList, ItemType, ItemSize);
                                            pItemList := pItemList + UnpackSize;
                                         end;
                                      end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_ONEVALUE    : begin
                            pOneValue := pTW_ONEVALUE(DSMMemLock(twCap.hContainer));
                            if (pOneValue <> Nil)
                            then begin
                                 ItemSizes := SizeOf(TW_ONEVALUE) +
                                              GetCapConSize(pOneValue^.ItemType) -
                                              SizeOf(TW_UINT32);
                                 GetMem(pCapCon, ItemSizes);

                                 if (pCapCon <> Nil)
                                 then begin
                                      ConType := TWON_ONEVALUE;
                                      with TW_ONEVALUE(pCapCon^)
                                      do begin
                                         ItemType  := pOneValue^.ItemType;
                                         pItem     := @pOneValue^.Item;
                                         pItemList := @Item;
                                         ItemToUInt32(pItem, pItemList, ItemType, 0);
                                         // GetValue(@pOneValue^.Item, @Item, ItemType);
                                      end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_RANGE       : begin
                            pRange := pTW_RANGE(DSMMemLock(twCap.hContainer));
                            if (pRange <> Nil)
                            then begin
                                 GetMem(pCapCon, SizeOf(TW_RANGE));
                                 if (pCapCon <> Nil)
                                 then begin
                                      ConType := TWON_RANGE;
                                      with TW_RANGE(pCapCon^)
                                      do begin
                                         ItemType     := pRange^.ItemType;
                                         MinValue     := pRange^.MinValue;
                                         MaxValue     := pRange^.MaxValue;
                                         StepSize     := pRange^.StepSize;
                                         DefaultValue := pRange^.DefaultValue;
                                         CurrentValue := pRange^.CurrentValue;
                                      end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       end;
  end;

  // App is ALWAYS responsible for cleaning up the container.
  if (twCap.hContainer <> 0)
  then DSMMemFree(twCap.hContainer);
end; // TmcmTWAINIntf.GetCapMsg.                                            


function TmcmTWAINIntf.SetCapMsg(Cap     : TW_UINT16;
                                 Msg     : TW_UINT16;
                                 ConType : TW_UINT16;
                                 pCapCon : Pointer) : TW_UINT16;
{------------------------------------------------------------------------------}
{ SetCapMsg                                                                    }
{ The SetCapMsg functions pack the data structures to send to the data source. }
{                                                                              }
{ TW_BOOL's must be treated as "0" = FALSE and "1" = TRUE.                     }
{ TW_STRxxx data types are treated as their respective data type.              }
{------------------------------------------------------------------------------}

    procedure UInt32ToItem(var pItem    : PAnsiChar;
                               Value    : pointer;
                               ItemType : TW_UINT16;
                               ItemSize : integer);
    begin
      case ItemType of
      TWTY_INT8   : pTW_INT8(pItem)^   := pTW_UINT32(Value)^;
      TWTY_INT16  : pTW_INT16(pItem)^  := pTW_UINT32(Value)^;
      TWTY_INT32  : pTW_INT32(pItem)^  := pTW_UINT32(Value)^;
      TWTY_UINT8  : pTW_UINT8(pItem)^  := pTW_UINT32(Value)^;
      TWTY_UINT16 : pTW_UINT16(pItem)^ := pTW_UINT32(Value)^;
      TWTY_UINT32 : pTW_UINT32(pItem)^ := pTW_UINT32(Value)^;
      TWTY_BOOL   : pTW_BOOL(pItem)^   := pTW_UINT32(Value)^;
      TWTY_FIX32  : pTW_FIX32(pItem)^  := pTW_FIX32(Value)^;
      TWTY_STR32  : CopyMemory(pTW_STR32(pItem), pTW_STR32(Value), SizeOf(TW_STR32));
      TWTY_STR64  : CopyMemory(pTW_STR64(pItem), pTW_STR64(Value), SizeOf(TW_STR64));
      TWTY_STR128 : CopyMemory(pTW_STR128(pItem), pTW_STR128(Value), SizeOf(TW_STR128));
      TWTY_STR255 : CopyMemory(pTW_STR255(pItem), pTW_STR255(Value), SizeOf(TW_STR255));
      TWTY_FRAME  : CopyMemory(pTW_FRAME(pItem), pTW_FRAME(Value), SizeOf(TW_FRAME));
      end;
      pItem := pItem + ItemSize;
    end; // ItemToUInt32.                                                   


var twCap        : TW_CAPABILITY;
    pArray       : pTW_ARRAY;
    pRange       : pTW_RANGE;
    pOneValue    : pTW_ONEVALUE;
    pEnumeration : pTW_ENUMERATION;

    pItem        : PAnsiChar;
    pItemList    : PAnsiChar;
    i, ItemSize  : integer;
    ItemSizes    : integer;
    UnpackSize   : integer;
begin
  if (pCapCon <> Nil)
  then begin
       twCap.Cap        := Cap;     // id of cap you want.
       twCap.ConType    := ConType; // container type.
       twCap.hContainer := 0;

       case twCap.ConType of
       TWON_ARRAY       : begin
                            UnpackSize := GetCapConSize(TW_ARRAY(pCapCon^).ItemType);
                            ItemSize  := TWItemSize[TW_ARRAY(pCapCon^).ItemType];
                            ItemSizes :=  ItemSize * TW_ARRAY(pCapCon^).NumItems;
                            twCap.hContainer := DSMMemAlloc(SizeOf(TW_ARRAY) + ItemSizes);
                            pArray := DSMMemLock(twCap.hContainer);
                            if (pArray <> Nil)
                            then begin
                                 with TW_ARRAY(pCapCon^)
                                 do begin
                                    pArray^.ItemType := ItemType;
                                    pArray^.NumItems := NumItems;
                                    pItem     := @pArray^.ItemList;
                                    pItemList := @ItemList[0];
                                    for i := 0 to (NumItems - 1)
                                    do begin
                                       UInt32ToItem(pItem, pItemList, ItemType, ItemSize);
                                       pItemList := pItemList + UnpackSize;
                                    end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_ENUMERATION : begin
                            UnpackSize := GetCapConSize(TW_ARRAY(pCapCon^).ItemType);
                            ItemSize   := TWItemSize[TW_ENUMERATION(pCapCon^).ItemType];
                            ItemSizes  := ItemSize * TW_ENUMERATION(pCapCon^).NumItems;
                            twCap.hContainer := DSMMemAlloc(SizeOf(TW_ENUMERATION) + ItemSizes);
                            pEnumeration := DSMMemLock(twCap.hContainer);
                            if (pEnumeration <> Nil)
                            then begin
                                 with TW_ENUMERATION(pCapCon^)
                                 do begin
                                    pEnumeration^.ItemType     := ItemType;
                                    pEnumeration^.NumItems     := NumItems;
                                    pEnumeration^.CurrentIndex := CurrentIndex;
                                    pEnumeration^.DefaultIndex := DefaultIndex;
                                    pItem     := @pEnumeration^.ItemList;
                                    pItemList := @ItemList[0];
                                    for i := 0 to (NumItems - 1)
                                    do begin
                                       UInt32ToItem(pItem, pItemList, ItemType, ItemSize);
                                       pItemList := pItemList + UnpackSize;
                                    end;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_ONEVALUE    : begin
                            ItemSize := TWItemSize[TW_ONEVALUE(pCapCon^).ItemType];
                            twCap.hContainer := DSMMemAlloc(sizeof(TW_ONEVALUE) + ItemSize);
                            pOneValue := DSMMemLock(twCap.hContainer);
                            if (pOneValue <> Nil)
                            then begin
                                 with TW_ONEVALUE(pCapCon^)
                                 do begin
                                    pOneValue^.ItemType := ItemType;
                                    pItem := @pOneValue^.Item;
                                    UInt32ToItem(pItem, @Item, ItemType, ItemSize);
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       TWON_RANGE       : begin
                            twCap.hContainer := DSMMemAlloc(sizeof(TW_RANGE));
                            pRange := DSMMemLock(twCap.hContainer);
                            if (pRange <> Nil)
                            then begin
                                 with TW_RANGE(pCapCon^)
                                 do begin
                                    pRange^.ItemType     := ItemType;
                                    pRange^.MinValue     := MinValue;
                                    pRange^.MaxValue     := MaxValue;
                                    pRange^.StepSize     := StepSize;
                                    pRange^.DefaultValue := DefaultValue;
                                    pRange^.CurrentValue := CurrentValue;
                                 end;
                                 DSMMemUnlock(twCap.hContainer);
                            end;
                          end;
       end;

       // It is assumed that the Source will read the container NOW.
       if (twCap.hContainer <> 0)
       then Result := SendCapabilityMsg(Cap, Msg, @twCap)
       else Result := $FFFF;

       // NOTE: the App ALWAYS is required to Free the container.
       if (twCap.hContainer <> 0)
       then DSMMemFree(twCap.hContainer);
  end
  else Result := $FFFF;
end; // TmcmTWAINIntf.SetCapMsg.                                            


function TmcmTWAINIntf.GetCapabilityMsg(    Cap       : word;
                                            Msg       : word;
                                        var Container : TTwnContainer) : integer;
var ConType    : TW_UINT16;
    pContainer : pointer;
begin
  Result := TWRC_FAILURE;
  pContainer := Nil;
  case Msg of
  MSG_GET,
  MSG_GETCURRENT,
  MSG_GETDEFAULT,
  MSG_RESET        : begin
                       Result := GetCapMsg(Cap, Msg, ConType, pContainer);
                       if (Result = TWRC_SUCCESS) and (pContainer <> Nil)
                       then begin
                            if Not(Assigned(Container))
                            then begin
                                 Container := Containers.Items[Cap];
                                 if Not(Assigned(Container))
                                 then begin
                                      Container := TTwnContainer.Create(Self);
                                      TTypeCastContainerList(FConList).AddItem(Cap, Container);
                                 end;
                            end;
                            if Assigned(Container)
                            then TTypeCastContainer(Container).AssignContainerPtr(Cap, ConType, pContainer);
                       end
                       else begin
                            if (pContainer <> Nil)
                            then FreeMem(pContainer);
                            pContainer := Nil;
                            if (Result = TWRC_SUCCESS)
                            then Result := TWRC_FAILURE;
                       end;
                     end;
  MSG_QUERYSUPPORT : begin
                         Result := GetCapMsg(Cap, Msg, ConType, pContainer);
                         if (Result = TWRC_SUCCESS)
                         then begin
                              TTypeCastContainer(Container).FQuerySupport := integer($FFFFFFFF);
                              if (ConType = TWON_ONEVALUE)
                              then begin
                                   if (pTW_ONEVALUE(pContainer)^.ItemType = TWTY_INT32)
                                   then TTypeCastContainer(Container).FQuerySupport := pTW_ONEVALUE(pContainer)^.Item;
                              end;
                         end;
                         if (pContainer <> Nil)
                         then FreeMem(pContainer);
                         pContainer := Nil;
                     end;
  end;
end; // TmcmTWAINIntf.GetCapabilityMsg.


function TmcmTWAINIntf.SetCapabilityMsg(Msg        : word;
                                        AsOneValue : bool;
                                        Container  : TTwnContainer) : integer;
var ConType    : TW_UINT16;
    pContainer : pointer;
begin
  Result := -1;
  if Assigned(Container)
  then begin
       if (Msg = MSG_SET)
       then begin
            pContainer := TTypeCastContainer(Container).GetContainerPtr(AsOneValue);
            if AsOneValue
            then ConType := TWON_ONEVALUE
            else ConType := Container.ContainerType;
            Result := SetCapMsg(Container.Capability, Msg, ConType, pContainer);
            if (Result <> TWRC_SUCCESS)
            then ; // Error Handling missing !
       end;
  end;
end; // TmcmTWAINIntf.SetCapabilityMsg.                                     


procedure TmcmTWAINIntf.GetSupportedCaps;
begin
  if (GetCapabilityMsg(CAP_SUPPORTEDCAPS, MSG_GET, FSupportCaps) = TWRC_SUCCESS)
  then if Assigned(FSupportCaps)
       then begin
            if Not(IsCapSupported(ICAP_XFERMECH))
            then FSupportCaps.AddItem(ICAP_XFERMECH);
            FNumCap := FSupportCaps.NumItems;
       end;
end; // TmcmTWAINIntf.GetSupportedCaps.                                     


procedure TmcmTWAINIntf.GetSupportedExCaps;
begin
  if (GetCapabilityMsg(CAP_EXTENDEDCAPS, MSG_GET, FSupportCapsEx) = TWRC_SUCCESS)
  then if Assigned(FSupportCapsEx)
       then FNumExCap := FSupportCapsEx.NumItems;
end; // TmcmTWAINIntf.GetSupportedExCaps.


function TmcmTWAINIntf.IsCapSupported(Value : integer) : boolean;
var i : integer;
begin
  Result := False;
  if Assigned(FSupportCaps) and Not(FLenientOnCaps)
  then begin
       i := 0;
       while Not(Result) and (i < FSupportCaps.NumItems)
       do begin
          if (Value = FSupportCaps.Items[i])
          then Result := True;
          inc(i);
       end;
  end
  // Data source didn't support CAP_SUPPORTEDCAPS, therefore better accept
  // all other capabilities.
  else Result := True;
end; // TmcmTWAINIntf.IsCapSupported.                                       


function TmcmTWAINIntf.IsExCapSupported(Value : integer) : boolean;
var i : integer;
begin
  Result := False;
  if Assigned(FSupportCapsEx)
  then begin
       i := 0;
       while Not(Result) and (i < FSupportCapsEx.NumItems)
       do begin
          if (Value = FSupportCapsEx.Items[i])
          then Result := True;
          inc(i);
       end;
  end;
end; // TmcmTWAINIntf.IsExCapSupported.                                     


function TmcmTWAINIntf.GetFirstSource(var SourceName : string) : TW_UINT16;
{------------------------------------------------------------------------------}
{ FUNCTION: GetFirstSource                                                     }
{                                                                              }
{ ARGS:    none                                                                }
{                                                                              }
{ RETURNS: twRC TWAIN status return code                                       }
{                                                                              }
{ NOTES:   1). call the Source Manager to:                                     }
{              - have the SM put up a list of the available Sources            }
{              - get information about the user selected Source from           }
{                NewDSIdentity, filled by Source                               }
{------------------------------------------------------------------------------}
var NewDSIdentity : TW_IDENTITY;
begin
  if IsDSOpen
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot get first source. Source already open');
       {$ENDIF}
       Result := TWRC_FAILURE;
  end
  else begin
       // I will settle for the system default.  Shouldn't I get a highlight
       // on system default without this call?
       Result := DSMEntry(@FAppIdentity,
                          Nil,
                          DG_CONTROL,
                          DAT_IDENTITY,
                          MSG_GETFIRST,
                          TW_MEMREF(@NewDSIdentity));

       // - TWRC_SUCCESS, log in new Source
       // - TWRC_CANCEL,  keep the current Source
       // - default,      check the codes in a status message, display result
       case Result of
       TWRC_SUCCESS : begin
                        FSourceID := NewDSIdentity;
                        SourceName := string(NewDSIdentity.ProductName);
                      end;
       // TWRC_CANCEL,
       else SourceName := '';
       end;
  end;
end; // TmcmTWAINIntf.GetFirstSource.


function TmcmTWAINIntf.GetNextSource(var SourceName : string) : TW_UINT16;
{------------------------------------------------------------------------------}
{ FUNCTION: GetNextSource                                                      }
{                                                                              }
{ ARGS:    none                                                                }
{                                                                              }
{ RETURNS: twRC TWAIN status return code                                       }
{                                                                              }
{ NOTES:   1). call the Source Manager to:                                     }
{              - have the SM put up a list of the available Sources            }
{              - get information about the user selected Source from           }
{                NewDSIdentity, filled by Source                               }
{------------------------------------------------------------------------------}
var NewDSIdentity : TW_IDENTITY;
begin
  if IsDSOpen
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot get next source. Source already open');
       {$ENDIF}
       Result := TWRC_FAILURE;
  end
  else begin
       // I will settle for the system default.  Shouldn't I get a highlight
       // on system default without this call?
       Result := DSMEntry(@FAppIdentity,
                          Nil,
                          DG_CONTROL,
                          DAT_IDENTITY,
                          MSG_GETNEXT,
                          TW_MEMREF(@NewDSIdentity));

       // - TWRC_SUCCESS, log in new Source
       // - TWRC_CANCEL,  keep the current Source
       // - default,      check the codes in a status message, display result
       case Result of
       TWRC_SUCCESS   : begin
                          FSourceID := NewDSIdentity;
                          SourceName := string(NewDSIdentity.ProductName);
                        end;
       // TWRC_ENDOFLIST,
       // TWRC_CANCEL,
       else SourceName := '';
       end;
  end;
end; // TmcmTWAINIntf.GetNextSource.


function TmcmTWAINIntf.GetDefaultSource(var SourceName : string) : TW_UINT16;
var NewDSIdentity : TW_IDENTITY;
begin
  if IsDSOpen
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot get first source. Source already open');
       {$ENDIF}
       Result := TWRC_FAILURE;
  end
  else begin
       // I will settle for the system default.  Shouldn't I get a highlight
       // on system default without this call?
       Result := DSMEntry(@FAppIdentity,
                          Nil,
                          DG_CONTROL,
                          DAT_IDENTITY,
                          MSG_GETDEFAULT,
                          TW_MEMREF(@NewDSIdentity));

       // - TWRC_SUCCESS, log in new Source
       // - TWRC_CANCEL,  keep the current Source
       // - default,      check the codes in a status message, display result
       case Result of
       TWRC_SUCCESS : begin
                        FSourceID := NewDSIdentity;
                        SourceName := string(NewDSIdentity.ProductName);
                      end;
       // TWRC_CANCEL,
       else SourceName := '';
       end;
  end;
end; // TmcmTWAINIntf.GetDefaultSource.


function TmcmTWAINIntf.SetDefaultSource(SourceName : string) : TW_UINT16;
{------------------------------------------------------------------------------}
{ FUNCTION: SetDefaultSource                                                   }
{                                                                              }
{ ARGS:    none                                                                }
{                                                                              }
{ RETURNS: twRC TWAIN status return code                                       }
{------------------------------------------------------------------------------}
var TempName      : string;
    OldDSIdentity : TW_IDENTITY;
begin
  // Current code will only set a given data source as the "default" while the
  // data source manager is open!
  // It will not set the data source as the systems default source!
  // WHAT TO DO - WIN.INI on Windows before 2000 and registry after!?!
  // Or maintain a variable for ... i.e. an internal default source!
  if IsDSOpen
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot set default source. A source is already open');
       {$ENDIF}
       Result := TWRC_FAILURE;
  end
  else begin
       // I will settle for the system default.  Shouldn't I get a highlight
       // on system default without this call?
       OldDSIdentity := FSourceID;
       Result := GetFirstSource(TempName);
       while (Result = TWRC_SUCCESS) and
             (Length(TempName) > 0) and
             (TempName <> SourceName)
       do Result := GetNextSource(TempName);

       if Not((TempName = SourceName) and (Length(SourceName) > 0))
       then begin
            FSourceID := OldDSIdentity;
       end;
  end;
end; // TmcmTWAINIntf.SetDefaultSource.                                     


function TmcmTWAINIntf.GetShowIndicators : boolean;
begin
  Result := FIndicators;
end; // TmcmTWAINIntf.GetShowIndicators.                                    


procedure TmcmTWAINIntf.SetShowIndicators(Value : boolean);
begin
  FIndicators := Value;
end; // TmcmTWAINIntf.SetShowIndicators.                                    


function TmcmTWAINIntf.SetIndicators : TW_UINT16;
// Set show indicators to data source.
begin
  Result := SendBoolCap(CAP_INDICATORS, MSG_SET, FIndicators);
end; // TmcmTWAINIntf.SetIndicators.                                        


function TmcmTWAINIntf.GetShowUI : boolean;
begin
  Result := FShowUI;
end; // TmcmTWAINIntf.GetShowUI.                                            


procedure TmcmTWAINIntf.SetShowUI(Value : boolean);
begin
  FShowUI := Value;
end; // TmcmTWAINIntf.SetShowUI.                                            


function TmcmTWAINIntf.GetModalUI : boolean;
begin
  Result := FModalUI;
end; // TmcmTWAINIntf.GetModalUI.                                           


function TmcmTWAINIntf.GetImageFileFmt : TTwnFileFmt;
//------------------------------------------------------------------------------
// If more than one file format is supported, GetImageFileFmt will return
// the current format in the container.
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_IMAGEFILEFORMAT, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := TTwnFileFmt(TWFF_NONE);
end; // TmcmTWAINIntf.GetImageFileFmt.                                      


function TmcmTWAINIntf.SetImageFileFmt(ItemValue : TTwnFileFmt) : TW_UINT16;
var Container : TTwnContainer;
begin
  Container := FConList.Items[ICAP_IMAGEFILEFORMAT];
  if Not(Assigned(Container))
  then Container := FConList.CreateItem(ICAP_IMAGEFILEFORMAT);
  if Assigned(Container)
  then begin
       Container.ItemType     := TWTY_UINT16;
       Container.CurrentValue := TW_UINT16(ItemValue);
       Result := SetCapabilityMsg(MSG_SET, True, Container);
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINIntf.SetImageFileFmt.                                      


function TmcmTWAINIntf.SetupXferMech : TW_INT16;
{------------------------------------------------------------------------------}
{ SetupXferMech - Set the current transfer mechanism for Twain based on the    }
{ menus that have been checked by the user.                                    }
{                                                                              }
{ TransType = 0 Native Transfer.                                               }
{             1 File Transfer.                                                 }
{             2 Memory Transfer.                                               }
{------------------------------------------------------------------------------}
var Container    : TTwnContainer;
    SourceFormat : TTwnFileFmt;
    FileFormats  : TTwnFileFmts;
    FileExt      : string;
    i            : integer;
begin
  Container := FConList.Items[ICAP_XFERMECH];
  if Not(Assigned(Container))
  then Container := FConList.CreateItem(ICAP_XFERMECH);
  if Assigned(Container)
  then begin
       if (FSourceVersion >= 19)
       then begin
            if (XferMech = TWFX_FILES)
            then XferMech := TWFX_FILES2;
       end
       else begin
            if (XferMech = TWFX_FILES2)
            then XferMech := TWFX_FILES;
       end;

       Container.ItemType     := TWTY_UINT16;
       Container.CurrentValue := TW_UINT16(XferMech);
       Result := SetCapabilityMsg(MSG_SET, True, Container);

       // Special case to handle un-supported value = TWFX_FILES2, when both
       // application and source follow protocol equal to or greater than 1.9.
       if (Result <> TWRC_SUCCESS)
       then if (XferMech = TWFX_FILES2)
            then begin
                 XferMech := TWFX_FILES;
                 Container.CurrentValue := TW_UINT16(XferMech);
                 Result := SetCapabilityMsg(MSG_SET, True, Container);
            end;

       GetCapabilityMsg(ICAP_XFERMECH, MSG_GET, Container);
       if Assigned(Container)
       then begin
            if (XferMech = Container.CurrentValue)
            then Result := TWRC_SUCCESS
            else begin
                 // Error, Could not set-up requested transfer mechanism.
                 if LenientOnCaps
                 then Result := TWRC_SUCCESS;
                 XferMech := Container.CurrentValue;
            end;
       end;

       case XferMech of
       // File transfer.
       TWFX_FILES,
       TWFX_FILES2 : begin
                       FileFormats := FAllFormats;
                       if ReturnHandle
                       then FileFormats := FileFormats + [TWFF_BMP];

                       if Not(FileFormat in FileFormats)
                       then FileFormats := FileFormats + [FileFormat];
                       // Negotiate which file format to use.
                       // Start with the preferred file format (PrefFormat).
                       // If this is not supported by the data source check if
                       // any of the formats in FileFormats are supported.
                       // If non of the file formats are supported by the data
                       // source, switch to Native transfer mode.
                       repeat
                         SetImageFileFmt(FileFormat);
                         SourceFormat := GetImageFileFmt;
                         if (SourceFormat <> FileFormat)
                         then begin
                              if (SourceFormat in FileFormats)
                              then begin
                                   FileFormat := SourceFormat;
                                   FileExt    := FileFormat2Ext(FileFormat);
                                   FileName   := CreateFileName(FileName, FileExt);
                              end
                              else begin
                                   // Remove current format from FileFormats and
                                   // find next possible format.
                                   FileFormats := FileFormats - [FileFormat];
                                   i := integer(TWFF_TIFF);
                                   while (i <= integer(TWFF_PNG)) and
                                         Not(TTwnFileFmt(i) in FileFormats)
                                   do inc(i);
                                   FileFormat := TTwnFileFmt(i);
                              end;
                         end;
                       until (SourceFormat = FileFormat) or (FileFormats = []);

                       if Not(FileFormat in FileFormats)
                       then FileFormat := TTwnFileFmt(TWFF_NONE);

                       // If source doesn't support fileformat's, use native
                       // transfer.
                       FileExt := FileFormat2Ext(FileFormat);
                       if (Length(FileExt) > 0)
                       then FileName := CreateFileName(FileName, FileExt)
                       else begin
                            XferMech  := TWFX_NATIVE;
                            SetupXferMech;
                       end;
                     end;
       // Memory transfer.
       TWFX_MEMORY : ;
       // Native Tranfer.
       TWFX_NATIVE : ;
       end;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINIntf.SetupXferMech.


function TmcmTWAINIntf.XferGroup(    Msg   : TW_UINT16;
                                 var Group : TW_UINT32) : TW_UINT16;
// Parameters:
// Msg
//   MSG_GET        - Source returnes the Data Groups for the upcoming transfer.
//                    (State 4 - 6)
//   MSG_SET        - An application changes the Data Group to indicate that it
//                    wants to receive any audio associated with the image
//                    (State 6 ONLY).
//                    Note the Data Source must always default to DG_IMAGE.
// Group            - Bitwise combination of the supported Data Groups,
//                    i.e DG_CONTROL, DG_IMAGE, DG_AUDIO.
// Note:              Application should indicate in FAppIdentity.SupportedGroups
//                    which Data (Groups) it is interested in receiving.
var DataGroup : TW_UINT32;
begin
  if (Msg = MSG_SET)
  then DataGroup := Group;
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_CONTROL,
                     DAT_XFERGROUP,
                     Msg,
                     TW_MEMREF(@DataGroup));
  if (Result = TWRC_SUCCESS) and (Msg = MSG_GET)
  then Group := DataGroup;
end; // TmcmTWAINIntf.XferGroup.


function TmcmTWAINIntf.UserInterface(Msg     : TW_UINT16;
                                     pUI     : pTW_USERINTERFACE) : TW_UINT16;
begin
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_CONTROL,
                     DAT_USERINTERFACE,
                     Msg,
                     TW_MEMREF(pUI));
end; // TmcmTWAINIntf.UserInterface.


function TmcmTWAINIntf.GetCIEColor(pCIE : pTW_CIECOLOR) : TW_UINT16;
// State 4 - 6
begin
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_IMAGE,
                     DAT_CIECOLOR,
                     MSG_GET,
                     TW_MEMREF(pCIE));
end; // TmcmTWAINIntf.GetCIEColor.


function TmcmTWAINIntf.GrayResponse(pGrayResponse : pTW_GRAYRESPONSE;
                                    Msg           : TW_UINT16) : TW_UINT16;
// State 4
// MSG_RESET
// MSG_SET
begin
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_IMAGE,
                     DAT_GRAYRESPONSE,
                     Msg,
                     TW_MEMREF(pGrayResponse));
end; // TmcmTWAINIntf.GrayResponse.


function TmcmTWAINIntf.RGBResponse(pRGBResponse : pTW_GRAYRESPONSE;
                                   Msg          : TW_UINT16) : TW_UINT16;
begin
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_IMAGE,
                     DAT_RGBRESPONSE,
                     Msg,
                     TW_MEMREF(pRGBResponse));
end; // TmcmTWAINIntf.RGBResponse.


function TmcmTWAINIntf.JPEGCompression(pJPEGComp : pTW_JPEGCOMPRESSION;
                                       Msg       : TW_UINT16) : TW_UINT16;
begin
  Result := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_IMAGE,
                     DAT_JPEGCOMPRESSION,
                     Msg,
                     TW_MEMREF(pJPEGComp));
end; // TmcmTWAINIntf.JPEGCompression.


function TmcmTWAINIntf.GetPixelFlavor : integer;
{------------------------------------------------------------------------------}
{ PixelFlavor                                                                  }
{   MSG_GET container (Enumeration, OneValue)                                  }
{   ItemType = TW_UINT16                                                       }
{------------------------------------------------------------------------------}
var Container : TTwnContainer;
begin
  Container := Nil;
  Result := -1;
  if (GetCapabilityMsg(ICAP_PIXELFLAVOR, MSG_GET, Container) = TWRC_SUCCESS)
  then if Assigned(Container)
       then Result := Container.CurrentValue
end; // TmcmTWAINIntf.GetPixelFlavor.                                       


procedure TmcmTWAINIntf.SetPixelFlavor(Value : integer);
{------------------------------------------------------------------------------}
{ PixelFlavor                                                                  }
{   MSG_SET container (OneValue)                                               }
{   ItemType = TW_UINT16                                                       }
{------------------------------------------------------------------------------}
begin
  if (Containers.Items[ICAP_PIXELFLAVOR] = Nil)
  then GetPixelFlavor;
  if (Containers.Items[ICAP_PIXELFLAVOR] <> Nil)
  then begin
       Containers.Items[ICAP_PIXELFLAVOR].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_PIXELFLAVOR]);
  end;
end; // TmcmTWAINIntf.SetPixelFlavor.                                       


function TmcmTWAINIntf.GetUnits : integer;
{------------------------------------------------------------------------------}
{ Units                                                                        }
{   MSG_GET container (Enumeration, OneValue)                                  }
{   ItemType = TW_UINT16                                                       }
{------------------------------------------------------------------------------}
var Container : TTwnContainer;
begin
  Container := Nil;
  Result := -1;
  if (GetCapabilityMsg(ICAP_UNITS, MSG_GET, Container) = TWRC_SUCCESS)
  then if Assigned(Container)
       then Result := Container.CurrentValue;
end; // TmcmTWAINIntf.GetUnits.                                             


procedure TmcmTWAINIntf.SetUnits(Value : integer);
{------------------------------------------------------------------------------}
{ Units                                                                        }
{   MSG_SET container (Enumeration, OneValue)                                  }
{   ItemType = TW_UINT16                                                       }
{------------------------------------------------------------------------------}
begin
  if (Containers.Items[ICAP_UNITS] = Nil)
  then GetUnits;
  if (Containers.Items[ICAP_UNITS] <> Nil)
  then begin
       Containers.Items[ICAP_UNITS].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_UNITS]);
  end;
end; // TmcmTWAINIntf.SetUnits.


function TmcmTWAINIntf.GetDeviceEvents : TTwnDeviceEvents;
begin
  Result := FDeviceEvents;
end; // TmcmTWAINIntf.GetDeviceEvents.                                      


procedure TmcmTWAINIntf.SetDeviceEvents(Value : TTwnDeviceEvents);
var i         : integer;
    Container : TTwnContainer;
begin
  FDeviceEvents := Value;
  if IsDSOpen
  then begin
       Container := Containers.Items[CAP_DEVICEEVENT];

       // If the CAP_DEVICEEVENT container hasn't been created earlier in the
       // current TWAIN session and the FDeviceEvents SET is empty, then dont
       // negotiate device events with the data source.
       // As the data source shall default to "no" device events when opened
       // this omission shall have no effect on the session.
       if Not((Container = Nil) and (FDeviceEvents = []))
       then begin
            if Not(Assigned(Container))
            then Container := Containers.CreateItem(CAP_DEVICEEVENT);
            if Assigned(Container)
            then begin
                 Container.ContainerType := TWON_ARRAY;
                 Container.NumItems := 0;
                 for i := integer(TWDE_CHECKAUTOMATICCAPTURE) to (integer(TWDE_POWERSAVENOTIFY) - 1)
                 do if (TTwnDeviceEvent(i) in FDeviceEvents)
                    then Container.AddItem(TTwnDeviceEvent(i));
                 if (SetCapabilityMsg(MSG_SET, False, Container) <> TWRC_SUCCESS)
                 then ; // Error handling.
            end;
       end;
  end;
end; // TmcmTWAINIntf.SetDeviceEvents.                                      


procedure TmcmTWAINIntf.DoOnDeviceEvent(Sender : TObject);
var twRC          : TW_UINT16;
    twSourceEvent : TW_DEVICEEVENT;
    Event         : TTwnDeviceEvent;
    DeviceName    : string;
    A, B, C       : Variant;
begin
  // Call DG_CONTROL / DAT_DEVICEEVENT / MSG_GET to learn about event.
  try
    twRC := DSMEntry(@FAppIdentity,
                     @FSourceID,
                     DG_CONTROL,
                     DAT_DEVICEEVENT,
                     MSG_GET,
                     TW_MEMREF(@twSourceEvent));
  except
    twRC := TWRC_EXCEPTION;
  end;

  if (twRC = TWRC_SUCCESS)
  then begin
       // Get relevant information if any from TW_DEVICEEVENT structure and
       // fire OnDeviceEvent event.
       DeviceName := string(twSourceEvent.DeviceName);
       A := 0; B := 0; C := 0;
       Event := TTwnDeviceEvent(twSourceEvent.Event);
       case Event of
       TWDE_CHECKAUTOMATICCAPTURE  : begin
                                       A := twSourceEvent.AutomaticCapture;
                                       B := twSourceEvent.TimeBeforeFirstCapture;
                                       C := twSourceEvent.TimeBetweenCaptures;
                                     end;
       TWDE_CHECKBATTERY           : begin
                                       A := twSourceEvent.BatteryMinutes;
                                       B := twSourceEvent.BatteryPercentage;
                                     end;
       TWDE_CHECKDEVICEONLINE      : ;
       TWDE_CHECKFLASH             : begin
                                       A := twSourceEvent.FlashUsed2;
                                     end;
       TWDE_CHECKPOWERSUPPLY       : begin
                                       A := twSourceEvent.PowerSupply;
                                     end;
       TWDE_CHECKRESOLUTION        : begin
                                       A := FIX32ToFloat(twSourceEvent.XResolution);
                                       B := FIX32ToFloat(twSourceEvent.YResolution);
                                     end;
       // All other TWDE_xxx events does not provide data (A = B = C = 0)
       end;
       if Assigned(FOnDeviceEvent)
       then FOnDeviceEvent(Self, Event, DeviceName, A, B, C)
       else SetDeviceEvents([]);
  end;
end; // TmcmTWAINIntf.DoOnDeviceEvent.


{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
