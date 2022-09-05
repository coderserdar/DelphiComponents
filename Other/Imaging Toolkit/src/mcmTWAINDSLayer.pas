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
// $Log:  15892: mcmTWAINDSLayer.pas 
//
//    Rev 1.7    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.6    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.5    25-10-2009 16:44:28  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//   Rev 1.4    30-06-2005 22:04:58  mcm    Version: DT 3.5

//
//   Rev 1.3    02-06-2003 23:36:40  mcm
// In EnableDS, PostMessage to 1999 was exchanged with WM_DOEXITCLICK = WM_USER
// + 1999.
// In DisableDS, Application.CancelHint was added to overcome an error occuring
// when closing the driver/transferring an image as a result of a speedbutton or
// other components that shows hints.

//
//   Rev 1.2    06-03-2003 11:39:26  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.1    20-09-2002 12:34:28  mcm

//
//   Rev 1.0    04-12-2001 16:49:08  mcm    Version: DT 2.0

unit mcmTWAINDSLayer;

{$IFDEF WIN32}
  {$IFDEF VER100} {$A+} {$ELSE} {$A-} {$ENDIF}
{$ELSE}
  {$A+}
{$ENDIF}
{$F+,J+,N-,R-,W-,X+}

interface

{$INCLUDE mcmDefines.pas}

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.Classes, Vcl.Dialogs,
     {$ELSE}
     Windows, Dialogs, Classes,
     {$ENDIF}
     twain, twainproc,
     mcmTWAINLog,
     mcmTWAINContainer;

{ Local function prototypes -- need to move these to a header.                 }
const MAX_TWPATH  = 512;

var SourceControl : array[0..MAX_TWPATH] of char;

const // Image Layout.
      ImagePixFrame  : TRect
                     = (Left   : 0;
                        Top    : 0;
                        Right  : 0;
                        Bottom : 0);

      ImageLayout    : TW_IMAGELAYOUT
                     = (Frame          : (Left   : (Whole : 0;
                                                    Frac  : 0);
                                          Top    : (Whole : 0;
                                                    Frac  : 0);
                                          Right  : (Whole : 0;
                                                    Frac  : 0);
                                          Bottom : (Whole : 0;
                                                    Frac  : 0));
                        DocumentNumber : TW_UINT32(1);
                        PageNumber     : TW_UINT32(1);
                        FrameNumber    : TW_UINT32(1));

const MaxDeviceEvents = 10;

const MANUFACTURER   = 0;
      PRODFAMILY     = 1;
      PRODNAME       = 2;
      VERSION_INFO   = 3;

// Source State defines
const STATE1               = 100;
      STATE2               = 200;
      STATE3               = 300;
      STATE4               = 400;
      STATE5               = 500;
      STATE6               = 600;
      STATE7               = 700;

const TWN_APP       = 0;
      TWN_SRC       = 1;

// Internal structure to store TW messages.
type  TTWMessage = record
                   pSrc  : pTW_IDENTITY;
                   DG    : TW_UINT32;
                   DAT   : TW_UINT16;
                   MSG   : TW_UINT16;
                   pData : TW_MEMREF;
                   end;

      PTWMessage = ^TTWMessage;

type
  TOnDeviceEvent = procedure(Sender       : TObject;
                             pDeviceEvent : pTW_DEVICEEVENT) of object;

  TTwnsContainer = class(TTwnContainer)
  private
    procedure   SetStepValue(Value : Variant);
    function    GetStepValue : Variant;
  protected
    function    GetContainerPtr(AsOneValue : bool) : pointer;
  public
    property    QuerySupport : TW_INT32
      read      FQuerySupport
      write     FQuerySupport;
    property    StepValue : variant
      read      GetStepValue
      write     SetStepValue;
  end;

  TTWAINSource = class(TPersistent)
  private
    hAppWnd          : hWnd;        // Global handle to applications window.
    FFileName        : string; //array[0..1024] of char;
    FDSState         : integer;     // Global state machine place holder.
    FDSStatus        : TW_STATUS;   // Status structure for errors.
    FContainerList   : TtwnContainerList;
    FDeviceEventList : TList;       // List of Device Events.
    {$IFDEF TWNDEBUG}
    FmcmTWAINLog     : TmcmTWAINLog;
    {$ENDIF}
    FXferFirst       : bool;
    FXferDone        : boolean;
    FXferCount       : longint;
    FBytesOut        : longint;

    // Transfer image using Buffers.
    FMinBufSize      : integer;
    FMaxBufSize      : integer;

    FEventOverflow   : boolean;

    // Data source activation functions.
    function  EnableDS          (pUI     : pTW_USERINTERFACE) : TW_UINT16;
    function  DisableDS         (pUI     : pTW_USERINTERFACE) : TW_UINT16;
    function  OpenDS            (PTWMsg  : PTWMessage)        : TW_UINT16;
    function  CloseDS                                         : TW_UINT16;

    // Close event function.
    procedure OnNotifyXferReady(Sender : TObject);
    procedure OnNotifyCloseDSReq(Sender : TObject);
    procedure OnDeviceEvent(Sender : TObject; pDeviceEvent : pTW_DEVICEEVENT);

    // Bitmap swap colors.
    procedure FlipBitmap        (pBmp     : Pointer;
                                 pBmpInfo : PBitmapInfo);
    function IsPaletteGray : boolean;
    function GetImageHandle : THandle;

    function GetCapConSize(ItemType : TW_UINT16) : integer;
    function BuildUpOneValue(Creater  : TW_UINT16;
                             pData    : pTW_CAPABILITY;
                             ItemType : TW_UINT16;
                             Item     : pointer;
                             Msg      : TW_UINT16) : TW_UINT16;

    // Capability negotiation.
    function  ReceiveCapMsg     (pTWMsg  : PTWMessage) : TW_UINT16;
    function  ReturnCapMsg      (pTWMsg  : PTWMessage) : TW_UINT16;
    function  MergeCapabilities(Dest, Source: TTwnsContainer): TW_UINT16;
    procedure CreateContainers;
    function  IsExCapSupported(Value : word) : boolean;
  protected

  public
    FdsIdentity    : TW_IDENTITY; // Who we are.
    FappIdentity   : TW_IDENTITY; // To keep track of who's driving us.
    constructor Create;
    destructor Destroy; override;

    // Message input / Dispatch function.
    function DispatchMsg        (pTWMsg  : PTWMessage;
                                 DG      : TW_UINT32;
                                 DAT     : TW_UINT16)  : TW_UINT16;

    // DG_CONTROL
    function IdentityMsg        (PTWMsg  : PTWMessage) : TW_UINT16;
    function InterfaceMsg       (PTWMsg  : PTWMessage) : TW_UINT16;
    function CapabilityMsg      (PTWMsg  : PTWMessage) : TW_UINT16;
    function StatusMsg          (PTWMsg  : PTWMessage) : TW_UINT16;
    function PendingXferMsg     (PTWMsg  : PTWMessage) : TW_UINT16;
    function SetupMemXferMsg    (PTWMsg  : PTWMessage) : TW_UINT16;
    function SetupFileXferMsg   (PTWMsg  : PTWMessage) : TW_UINT16;
    function XferGroupMsg       (PTWMsg  : PTWMessage) : TW_UINT16;
    function DeviceEventMsg     (PTWMsg  : PTWMessage) : TW_UINT16;

    // DG_IMAGE
    function Palette8Msg        (PTWMsg  : PTWMessage) : TW_UINT16;
    function GrayResponseMsg    (PTWMsg  : PTWMessage) : TW_UINT16;
    function RGBResponseMsg     (PTWMsg  : PTWMessage) : TW_UINT16;
    function CIEColorMsg        (PTWMsg  : PTWMessage) : TW_UINT16;
    function JPEGCompressionMsg (PTWMsg  : PTWMessage) : TW_UINT16;
    function ImageInfoMsg       (PTWMsg  : PTWMessage) : TW_UINT16;
    function ImageLayoutMsg     (PTWMsg  : PTWMessage) : TW_UINT16;
    function ImageMemXferMsg    (PTWMsg  : PTWMessage) : TW_UINT16;
    function ImageFileXferMsg   (PTWMsg  : PTWMessage) : TW_UINT16;
    function ImageNativeXferMsg (PTWMsg  : PTWMessage) : TW_UINT16;
  published
  end;

var TWAINSource : TTWAINSource;

implementation

uses {$IFDEF GE_DXE2}
     System.SysUtils, Vcl.Forms, 
     {$ELSE}
     SysUtils, Forms,
     {$ENDIF}
     uDSUserInterface, // Data Source's User Interface.
     mcmTWAINDSEntry,
     mcmTWAINFix;


function TTwnsContainer.GetContainerPtr(AsOneValue : bool) : pointer;
begin
  Result := Inherited GetContainerPtr(AsOneValue);
end; { End TTwnsContainer.GetContainerPtr.                                     }


function TTwnsContainer.GetStepValue : Variant;
begin
  Result := Inherited GetStepValue;
end; { End TTwnsContainer.GetStepValue.                                        }


procedure TTwnsContainer.SetStepValue(Value : Variant);
var pRange : pTW_Range;
begin
  case ContainerType of
  TWON_RANGE : begin
                 pRange := GetContainerPtr(False);
                 if Assigned(pRange)
                 then with pRange^
                      do SetValue(Value, @StepSize, ItemType);
               end;
  end;
end; { End TTwnsContainer.SetStepValue.                                        }


constructor TTWAINSource.Create;
{
var Size              : Integer;
    RezBuffer         : String;
    MyHandle          : dword;
    VerString         : string;
    FormatVer         : string;
    SFInfo            : string;
    VerTranslation    : string;
    FormatStr         : string;
    TransTable        : PLongint;
    P                 : PChar;
    S, SBSize         : UInt;
}
begin
  FMinBufSize := 0;
  FMaxBufSize := 0;
  FappIdentity.Id := 0;

  // Set-up Source's identity and supported group.
  with FdsIdentity
  do begin
     Id               := 0;
     ProtocolMajor    := 1; // TWON_PROTOCOLMAJOR;
     ProtocolMinor    := 8; // TWON_PROTOCOLMINOR;
     Version.MajorNum := 1;
     Version.MinorNum := 0;
     Version.Language := TWLG_ENG;
     Version.Country  := TWCY_UNITEDKINGDOM;
     SupportedGroups  := DG_CONTROL or DG_IMAGE;
  end;

  // This section retreives version information.
  // Bugger - Delphi overrides ProductName, InternalName with the file name.
  (*
  Size := GetFileVersionInfoSize('twdsrc32.ds', MyHandle);
  SetLength(RezBuffer, Size);
  GetFileVersionInfo('twdsrc32.ds', MyHandle, Size, PChar(RezBuffer));

  VerQueryValue(PChar(RezBuffer), '\VarFileInfo\Translation',  pointer(TransTable), SBSize);

  SFInfo         := '\StringFileInfo\';
  VerTranslation := '\VarFileInfo\Translation';
  FormatStr      := '%s%.4x%.4x\%s%s';

  FormatVer := Format(FormatStr,[SfInfo,LoWord(TransTable^),HiWord(TransTable^),'TWAINName', #0]);
  VerQueryValue(PChar(RezBuffer), @FormatVer[1], Pointer(P), S);
  StrLCopy(FdsIdentity.ProductName, P, 32);

  FormatVer := Format(FormatStr,[SfInfo,LoWord(TransTable^),HiWord(TransTable^),'ProductVersion', #0]);
  VerQueryValue(PChar(RezBuffer), @FormatVer[1], Pointer(P), S);
  StrLCopy(FdsIdentity.Version.Info, P, 32);

  FormatVer := Format(FormatStr,[SfInfo,LoWord(TransTable^),HiWord(TransTable^),'CompanyName', #0]);
  VerQueryValue(PChar(RezBuffer), @FormatVer[1], Pointer(P), S);
  StrLCopy(FdsIdentity.Manufacturer, P, 32);

  FormatVer := Format(FormatStr,[SfInfo,LoWord(TransTable^),HiWord(TransTable^),'FileDescription', #0]);
  VerQueryValue(PChar(RezBuffer), @FormatVer[1], Pointer(P), S);
  StrLCopy(FdsIdentity.ProductFamily, P, 32);
  *)

  // Must use DRIVER32.RES to include correct version information.
  LoadStringA(hInstance, VERSION_INFO, FdsIdentity.Version.Info,  SizeOf(TW_STR32));
  LoadStringA(hInstance, MANUFACTURER, FdsIdentity.Manufacturer,  SizeOf(TW_STR32));
  LoadStringA(hInstance, PRODFAMILY,   FdsIdentity.ProductFamily, SizeOf(TW_STR32));
  LoadStringA(hInstance, PRODNAME,     FdsIdentity.ProductName,   SizeOf(TW_STR32));

  FDSState         := STATE3; // Global state machine place holder.

  FContainerList   := TTwnContainerList.Create(Nil); // Parent must be Nil.
  FDeviceEventList := TList.Create;
  FEventOverflow   := False;

  {$IFDEF TWNDEBUG}
    FmcmTWAINLog := TmcmTWAINLog.Create(Nil);
    FmcmTWAINLog.LogFilename := '.\DS-DELPHI.LOG';
    FmcmTWAINLog.DeleteLogFile;
  {$ENDIF}
end; { End TTWAINSource.Create.                                                }


destructor TTWAINSource.Destroy;
var i : integer;
begin
  for i := (FDeviceEventList.Count - 1) downto 0
  do FreeMem(pTW_DEVICEEVENT(FDeviceEventList.Items[i]));
  FDeviceEventList.Free;
  FContainerList.Free;
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Free;
  {$ENDIF}
  Inherited Destroy;
end; { End TTWAINSource.Destroy.                                               }


function TTWAINSource.GetCapConSize(ItemType : TW_UINT16) : integer;
// GetCapConSize calculates the unpacked data size, ie. data sizes less
// than TW_UNIT32 are "re-scaled" to an TW_UNIT32.                              
var Size : integer;
begin
  Size := TWItemSize[ItemType];
  if (Size < SizeOf(TW_UINT32))
  then Size := SizeOf(TW_UINT32);
  Result := Size;
end; { End TTWAINSource.GetCapConSize.                                         }


{------------------------------------------------------------------------------}
{ FUNCTION: BuildUpOneValue                                                    }
{                                                                              }
{ ARGS:    pData   pointer to a capability structure, details about container  }
{          ItemType constant that defines the type of the Item to follow       }
{          Item    the data to put into the OneValue container                 }
{                                                                              }
{ RETURNS: pData^.hContainer set to address of the container handle, ptr is    }
{          returned here                                                       }
{                                                                              }
{ NOTES:   This routine responds to a CAP_ call by creating a container of     }
{ type OneValue and returning with the hContainer value (excuse me) "pointing" }
{ to the container.  The container is filled with the values for ItemType      }
{ and Item requested by the caller.                                            }
{                                                                              }
{ NOTE: be sure to tell the APP the container type you have built, ConType.    }
{                                                                              }
{ Protocol: Used by MSG_GET.. calls were Source allocates the container and    }
{ the APP uses and then frees the container.                                   }
{                                                                              }
{ For generalization value of ItemType does not affect Item in anyway.         }
{ Caller should cast Item to TW_UINT32.                                        }
{------------------------------------------------------------------------------}

function TTWAINSource.BuildUpOneValue(Creater  : TW_UINT16;
                                      pData    : pTW_CAPABILITY;
                                      ItemType : TW_UINT16;
                                      Item     : pointer;
                                      Msg      : TW_UINT16) : TW_UINT16;
var pOneValue     : pTW_ONEVALUE;
    ContainerSize : integer;
  {$IFDEF VER70}
    Result : TW_UINT16;
  {$ENDIF}
begin
  Result := TWCC_SUCCESS;
  { MSG_SET allocates the structure at the app.                                }
  if ((Msg <> MSG_SET) and (Creater = TWN_SRC)) or
     ((Msg =  MSG_SET) and (Creater = TWN_APP))
  then begin
       case ItemType of
       TWTY_STR32  : ContainerSize := SizeOf(TW_ONEVALUE) + SizeOf(TW_STR32);
       TWTY_STR64  : ContainerSize := SizeOf(TW_ONEVALUE) + SizeOf(TW_STR64);
       TWTY_STR128 : ContainerSize := SizeOf(TW_ONEVALUE) + SizeOf(TW_STR128);
       TWTY_STR255 : ContainerSize := SizeOf(TW_ONEVALUE) + SizeOf(TW_STR255);
       else ContainerSize := SizeOf(TW_ONEVALUE);
       end;
       pData^.hContainer := GlobalAlloc(GMEM_MOVEABLE, ContainerSize);
       if (pData^.hContainer = 0)
       then Result := TWCC_LOWMEMORY;
  end;

  if (pData^.hContainer <> 0)
  then begin { tell APP the ConType returning.                                 }
       pData^.ConType := TWON_ONEVALUE;
       pOneValue := pTW_ONEVALUE(GlobalLock(pData^.hContainer));
       if (pOneValue <> Nil)
       then begin
            pOneValue^.ItemType := ItemType;
            pOneValue^.Item     := 0;
            case ItemType of
            TWTY_INT8   : pOneValue^.Item := TW_INT8(Item^);
	    TWTY_INT16  : pOneValue^.Item := TW_INT16(Item^);
    	    TWTY_INT32  : pOneValue^.Item := TW_INT32(Item^);
    	    TWTY_UINT8  : pOneValue^.Item := TW_UINT8(Item^);
    	    TWTY_UINT16 : pOneValue^.Item := TW_UINT16(Item^);
    	    TWTY_UINT32 : pOneValue^.Item := TW_UINT32(Item^);
    	    TWTY_BOOL   : pOneValue^.Item := TW_BOOL(Item^);
	    TWTY_FIX32  : begin
			    { fix32.Whole := @Item^.Whole;                     }
			    { fix32.Frac  := 32;                               }
			    { pOneValue^.Item := pTW_INT32(@fix32);            }
			    { pOneValue^.Item := pTW_INT32(@Item);             }
	                    pOneValue^.Item := TW_UINT32(Item^);
			  end;

            // TWTY_FRAME  : pOneValue^.Item  := TW_FRAME(TW_AFRAME(pList^)[j]);
            TWTY_STR32  : CopyMemory(@pOneValue^.Item, pTW_STR32(Item), SizeOf(TW_STR32));
            TWTY_STR64  : CopyMemory(@pOneValue^.Item, pTW_STR64(Item), SizeOf(TW_STR64));
            TWTY_STR128 : CopyMemory(@pOneValue^.Item, pTW_STR128(Item), SizeOf(TW_STR128));
            TWTY_STR255 : CopyMemory(@pOneValue^.Item, pTW_STR255(Item), SizeOf(TW_STR255));
            else          begin
	                    pOneValue^.Item := TW_UINT32(Item^);
                          end;
            end;
       end;
       GlobalUnlock(pData^.hContainer);
       Result := TWCC_SUCCESS;
  end;
  BuildUpOneValue := Result;
end; { End TTWAINSource.BuildUpOneValue.                                       }


{------------------------------------------------------------------------------}
{ FUNCTION: DispatchMsg                                                        }
{                                                                              }
{ ARGS:    pTWMsg  ptr to the message struct passed to DS_ENTRY                }
{          pMapper ptr to a struct containizng ptr's to functions              }
{          DAT     DAT_xxxx that you are 'switching' on                        }
{                                                                              }
{ RETURNS: condition code                                                      }
{                                                                              }
{ NOTES:   This function will dispatch the correct function based on the       }
{          struct identifier.  Searches down through the given Map for the     }
{          matching DAT_xxxx type.                                             }
{------------------------------------------------------------------------------}

function TTWAINSource.DispatchMsg(pTWMsg  : PTWMessage;
                                  DG      : TW_UINT32;
                                  DAT     : TW_UINT16) : TW_UINT16;
var twRc    : TW_UINT16;
    Proc_Ok : boolean;
begin
  Proc_Ok := True;
  case DG of
  DG_CONTROL : case DAT of
               DAT_IDENTITY      : twRC := IdentityMsg(pTWMsg);
               DAT_USERINTERFACE : twRC := InterfaceMsg(pTWMsg);
               DAT_CAPABILITY    : twRC := CapabilityMsg(pTWMsg);
               DAT_STATUS        : twRC := StatusMsg(pTWMsg);
               DAT_PENDINGXFERS  : twRC := PendingXferMsg(pTWMsg);
               DAT_SETUPMEMXFER  : twRC := SetupMemXferMsg(pTWMsg);
               DAT_SETUPFILEXFER : twRC := SetupFileXferMsg(pTWMsg);
               DAT_XFERGROUP     : twRC := XferGroupMsg(pTWMsg);
               DAT_DEVICEEVENT   : twRC := DeviceEventMsg(pTWMsg);
               else begin
                    Proc_Ok := False;
                    twRC    := TWRC_FAILURE;
               end;
               end;
  DG_IMAGE   : case DAT of
               DAT_IMAGEINFO       : twRC := ImageInfoMsg(pTWMsg);
               DAT_IMAGELAYOUT     : twRC := ImageLayoutMsg(pTWMsg);
               DAT_IMAGEMEMXFER    : twRC := ImageMemXferMsg(pTWMsg);
               DAT_IMAGENATIVEXFER : twRC := ImageNativeXferMsg(pTWMsg);
               DAT_IMAGEFILEXFER   : twRC := ImageFileXferMsg(pTWMsg);
               DAT_PALETTE8        : twRC := Palette8Msg(pTWMsg);
               DAT_GRAYRESPONSE    : twRC := GrayResponseMsg(pTWMsg);
               DAT_RGBRESPONSE     : twRC := RGBResponseMsg(pTWMsg);
               DAT_CIECOLOR        : twRC := CIEColorMsg(pTWMsg);
               DAT_JPEGCOMPRESSION : twRC := JPEGCompressionMsg(pTWMsg);
               else begin
                    Proc_Ok := False;
                    twRC    := TWRC_FAILURE;
               end;
               end;
  else begin
       FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
       twRC := TWRC_FAILURE;
  end;
  end;

  if Proc_Ok
  then Result := twRC
  else begin
       FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
       Result := TWRC_FAILURE;
  end;

  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Triplet2Str(1, DG, DAT, pTWMsg^.pData, pTWMsg^.MSG, twRC, FDSStatus.ConditionCode, ML_INFO);
  {$ENDIF}
end; { End TTWAINSource.DispatchMsg.                                           }


{------------------------------------------------------------------------------}
{ FUNCTION: IdentityMsg                                                        }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: twRC TWAIN status response                                          }
{                                                                              }
{ NOTES:   Handle the parse of: (from DAT_IDENTITY msg's)                      }
{              - MSG_OPENDS    open the indicated Source                       }
{              - MSG_CLOSEDS   close the indicated Source                      }
{              - MSG_GET       get identity struct of Source                   }
{              - error         error message                                   }
{                                                                              }
{------------------------------------------------------------------------------}

function TTWAINSource.IdentityMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRc : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('IdentityMsg');
  {$ENDIF}

  twRc := TWRC_SUCCESS;
  case pTWMsg^.MSG of
  MSG_OPENDS  : OpenDS(pTWMsg);
  MSG_CLOSEDS : CloseDS;

  { Tell the SM who you are valid 3--7 states.                                 }
  MSG_GET     : pTW_IDENTITY(pTWMsg^.pData)^ := FdsIdentity;
  else          begin
                  FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                  twRc := TWRC_FAILURE;
                end;
  end;
  Result := twRc;
end; { End TTWAINSource.IdentityMsg.                                           }


{------------------------------------------------------------------------------}
{ FUNCTION: InterfaceMsg                                                       }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: twRC TWAIN status response                                          }
{                                                                              }
{ NOTES:   Handle the parse of: (from DAT_USERINTERFACE msg's)                 }
{              - MSG_ENABLEDS  enable the open Source                          }
{              - MSG_DISABLEDS disable the open Source                         }
{              - errors        error message                                   }
{------------------------------------------------------------------------------}

function TTWAINSource.InterfaceMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRc : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('InterfaceMsg');
  {$ENDIF}

  case pTWMsg^.MSG of
  MSG_ENABLEDS  : twRc := EnableDS(pTW_USERINTERFACE(pTWMsg^.pData));
  MSG_DISABLEDS : twRc := DisableDS(pTW_USERINTERFACE(pTWMsg^.pData));
  else            begin
                    FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                    twRc := TWRC_FAILURE;
                  end;
  end;
  Result := twRc;
end; { End TTWAINSource.InterfaceMsg.                                          }


{------------------------------------------------------------------------------}
{ FUNCTION: EnableDS                                                           }
{                                                                              }
{ ARGS:    pUI ptr to user interface struct                                    }
{                                                                              }
{ RETURNS: twRC TWAIN status response                                          }
{              - TWRC_SUCCESS                                                  }
{              - TWRC_FAILURE                                                  }
{                  - TWCC_SEQERROR                                             }
{                  - TWCC_LOWMEMORY                                            }
{                                                                              }
{ NOTES:                                                                       }
{ COMMENTS:    Valid states: 4, transitions to state 5 iff successful.         }
{              ShowUI bit 1=Source user interface; 0=app supplied              }
{              hParent field should contain a valid windows handle if          }
{              you are implementing a modal user interface were the hparent    }
{              window will be disabled during access to the user IF dialog     }
{              via EnableWindow(hParent, False).  Else use hParent == NULL     }
{              for Modeless dialogs, see chapter 5.                            }
{------------------------------------------------------------------------------}

function TTWAINSource.EnableDS(pUI : pTW_USERINTERFACE) : TW_UINT16;
var twRC : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('EnableDS');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  if (FDSState = STATE4)
  then begin // Make this a global for later.
       hAppWnd := pUI^.hParent;  // Need to know the app window's handle.

       // If operation fails.
       if (FormSource = Nil)
       then begin
            FDSStatus.ConditionCode := TWCC_LOWMEMORY;
            twRC := TWRC_FAILURE;
       end

       // If operations succeed.
       else begin
            // Show the UI window, if the calling application asks for it!
            if (pUI^.ShowUI <> 0)
            then begin
                 // UI visible. User will control image acquisition.
                 if Assigned(FormSource)
                 then FormSource.UserInterface(True);
                 {$IFDEF TWNDEBUG}
                   FmcmTWAINLog.Str2File('Show DS window');
                 {$ENDIF}
            end
            else begin
                 // UI is not visible. Automatic image acquisition will commence.
                 {$IFDEF TWNDEBUG}
                   FmcmTWAINLog.Str2File('Keep DS window hidden');
                 {$ENDIF}

                 // Automatically get the image(s).
                 if Assigned(FormSource)
                 then FormSource.StartAutoScan;

                 PostMessage(FormSource.Handle, WM_DOEXITCLICK, 0, 0);
            end;
            FDSState := STATE5;
       end;
  end
  else begin
       FDSStatus.ConditionCode := TWCC_SEQERROR;
       twRC := TWRC_FAILURE;
  end;
  Result :=  twRC;
end; { End TTWAINSource.EnableDS.                                              }


{------------------------------------------------------------------------------}
{ FUNCTION: DisableDS                                                          }
{                                                                              }
{ ARGS:    pUI     ptr to user interface structure                             }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_SEQERROR                                                   }
{                                                                              }
{ NOTES:   Valid states: 5 through 7, transitions to 4 if successful. The App  }
{ sends down this call when the Source returns the TWMessage "MSG_CLOSEDSREQ". }
{                                                                              }
{ ShowUI bit 1=Source user interface; 0=app supplied                           }
{ hParent field should contain a valid windows handle                          }
{------------------------------------------------------------------------------}

function TTWAINSource.DisableDS(pUI : pTW_USERINTERFACE) : TW_UINT16;
var twRC : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('DisableDS');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE5,  // Hide UI if it's visible.
  STATE6,
  STATE7 : begin
             Application.CancelHint;
             {if (pUI^.ShowUI = 0)
             then} if Assigned(FormSource)
                  then FormSource.UserInterface(False);

             // Transition to new state.
             FDSState := STATE4;
           end;
      else begin
           FDSStatus.ConditionCode := TWCC_SEQERROR;
           twRC := TWRC_FAILURE;
      end;
  end;
  Result := twRC;
end; { End TTWAINSource.DisableDS.                                             }


{------------------------------------------------------------------------------}
{ FUNCTION: OpenDS                                                             }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:   This function will 'open' the Source, register it with the SM       }
{          Valid state: 3 transitions to state 4 on success.                   }
{ TODO: to support simultaneous connections to more than one app. the Source   }
{ should make a copy of it's states especially the identity structure of the   }
{ calling app.                                                                 }
{------------------------------------------------------------------------------}

function TTWAINSource.OpenDS(pTWMsg : PTWMessage) : TW_UINT16;
var twRC : TW_UINT16;
//    Aof  : TOFSTRUCT;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('OpenDS');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  if (FDSState = STATE3)
  then begin
       // Only allow one connection at a time.  If FAppIdentity.Id is set,
       // then there is already a connection.  If not, then remember who
       // we're connected to so we can send messages to it.
       if (FappIdentity.Id <> 0)
       then begin
            twRC := TWRC_FAILURE;
            FDSStatus.ConditionCode := TWCC_MAXCONNECTIONS;
       end
       else begin
            // This is to ensure that how we identify ourselves to the
            // SM is consistent with what it is keeping around.
            FappIdentity := pTW_IDENTITY(pTWMsg^.pSrc)^;
            FdsIdentity  := pTW_IDENTITY(pTWMsg^.pData)^;

            // Now create the user interface, but don't show it yet.
            FormSource := TFormSource.Create(Application);

            Application.DialogHandle := FormSource.Handle;

            if Not(Assigned(FormSource))
            then begin
                 twRC := TWRC_FAILURE;
                 FDSStatus.ConditionCode := TWCC_LOWMEMORY;
            end
            else begin
                 FormSource.SetAppID(@FappIdentity);
                 // Set-up required event handles.
                 FormSource.OnNotifyCloseDSRequest := OnNotifyCloseDSReq;
                 FormSource.OnNotifyXferReady := OnNotifyXferReady;
                 FormSource.OnDeviceEvent := OnDeviceEvent;

                 // Create capability containers.
                 CreateContainers;
            end;
            FDSState := STATE4;
       end;
  end
  else begin
       twRC := TWRC_FAILURE;
       FDSStatus.ConditionCode := TWCC_SEQERROR;
  end;
  Result := twRC;
end; { End TTWAINSource.OpenDS.                                                }


{------------------------------------------------------------------------------}
{ FUNCTION: CloseDS                                                            }
{                                                                              }
{ ARGS:    none                                                                }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:   This function will close the Source.                                }
{------------------------------------------------------------------------------}

function TTWAINSource.CloseDS : TW_UINT16;
var twRC : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('CloseDS');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6,
  STATE7 : begin // Clear out current connection.
             FappIdentity.Id := 0;
             try
               if Assigned(FormSource)
               then begin
                    FormSource.CloseAction := caFree;
                    FormSource.Close;
               end;
             finally
               // FormSource := Nil;
             end;

             // Set the DS state.
             FDSState := STATE3;
           end;
  else     begin
             twRC := TWRC_FAILURE;
             FDSStatus.ConditionCode := TWCC_SEQERROR;
           end;
  end;
  Result := twRC;
end; { End TTWAINSource.CloseDS.                                               }


{------------------------------------------------------------------------------}
{ FUNCTION: IsExCapSupported}
{------------------------------------------------------------------------------}

function TTWAINSource.IsExCapSupported(Value : word) : boolean;
var i         : integer;
    Container : TTwnsContainer;
begin
  Result := False;
  Container := TTwnsContainer(FContainerList.Items[CAP_EXTENDEDCAPS]);
  if Assigned(Container)
  then begin
       i := 0;
       while Not(Result) and (i < Container.NumItems)
       do begin
          {$IFOPT R+} {$DEFINE RANGE_CHECKING} {$R-} {$ENDIF}
          if (Value = Container.Items[i])
          then Result := True;
          {$IFDEF RANGE_CHECKING} {$R+} {$UNDEF RANGE_CHECKING} {$ENDIF}
          inc(i);
       end;
  end;
end; { End TTWAINSource.IsExCapSupported.                                      }


{------------------------------------------------------------------------------}
{ FUNCTION: CapabilityMsg                                                      }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES: for the MSG_SET side of Source                                        }
{ The application will create the container and destroy the container.         }
{ The Source will extract the value from the container 1st and use this value  }
{ to modify it's local value for the CAP.                                      }
{                                                                              }
{ The Source needs to be able to extract values from all of the different      }
{ container types and load as many as possible into it's local containers      }
{ for the particular capabilty.                                                }
{                                                                              }
{ NOTES: for the MSG_GET side of Source                                        }
{ The Source will create the container and the App will destroy container.     }
{ The App will extract the value from the container 1st and use this value     }
{ to modify it's local state or value for this CAP.                            }
{                                                                              }
{ The App extracts values from all different container types and also          }
{ matches up these cap's when appropriate to find the value or values which    }
{ best suit the current mode of the application.                               }
{                                                                              }
{ NOTES: for both App and Source in general:                                   }
{ You don't know what type of container you will be getting for any            }
{ particular cap until you get it.                                             }
{------------------------------------------------------------------------------}

function TTWAINSource.CapabilityMsg(pTWMsg : PTWMessage) : TW_UINT16;
var Cap        : TW_UINT16;
    twRC       : TW_UINT16;
    Ret        : TW_UINT16;
    Container  : TTwnsContainer;
    QueryValue : TW_INT32;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('CapabilityMsg');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  // Since the source control file only manages capabilities, it can be checked
  // here.  If this file is ever used for anything else, it would be good to
  // put these calls in a more general place.  DllMain is not a good idea though
  // because of the re-entrant nature of a TWAIN source.
  // always try to set the source control in case it is changed on the fly

  Cap := pTW_CAPABILITY(pTWMsg^.pData)^.Cap;
  case pTWMsg^.MSG of
  MSG_GET,
  MSG_GETCURRENT,
  MSG_GETDEFAULT   : begin
                       Ret := ReturnCapMsg(pTWMsg);
                       if (Ret <> TWCC_SUCCESS)
                       then twRc := TWRC_FAILURE;
                       FDSStatus.ConditionCode := Ret;
                     end;
  MSG_SET          : begin
                       // Check if CAP_EXTENDEDCAPS contains the requested
                       // capability. If not return TWCC_SEQERROR if data
                       // source is enabled.
                       if (FDSState < STATE5) or ((FDSState >= STATE5) and IsExCapSupported(Cap))
                       then Ret := ReceiveCapMsg(pTWMsg)
                       else Ret := TWCC_SEQERROR;
                       if (Ret <> TWRC_SUCCESS)
                       then twRC := TWRC_FAILURE;
                       FDSStatus.ConditionCode := Ret;
                     end;
  MSG_QUERYSUPPORT : begin
                       Container := TTwnsContainer(FContainerList.Items[Cap]);
                       if Assigned(Container)
                       then begin
                            QueryValue := Container.QuerySupport;
                            Ret := BuildUpOneValue(TWN_SRC,
                                                   pTW_CAPABILITY(pTWMsg^.pData),
                                                   TWTY_INT32,
                                                   @QueryValue,
                                                   MSG_GET);
                            FDSStatus.ConditionCode := Ret;
                            if (Ret <> TWRC_SUCCESS)
                            then twRC := TWRC_FAILURE;
                       end
                       else begin
                            FDSStatus.ConditionCode := TWCC_BADCAP;
                            twRc := TWRC_FAILURE;
                       end;
                     end;
  MSG_RESET        : begin
                       FormSource.ResetCapability(FContainerList, Cap);
                       Ret := FormSource.ChangeCapability(FContainerList, Cap);

                       FDSStatus.ConditionCode := Ret;
                       if (FDSStatus.ConditionCode = TWRC_SUCCESS)
                       then begin
                            pTWMsg^.MSG := MSG_GET;
                            Ret := ReturnCapMsg(pTWMsg);
                            FDSStatus.ConditionCode := Ret;
                       end;
                       if (Ret <> TWRC_SUCCESS)
                       then twRc := TWRC_FAILURE;
                     end;
  else               begin
                       FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                       twRC := TWRC_FAILURE;
                     end;
  end; // End of TWMsg.
  {$IFDEF TWNDEBUG}
//    LogTripletError(0, 1, DG_CONTROL, DAT_CAPABILITY, pTW_CAPABILITY(pTWMsg^.pData), pTWMsg^.MSG, '', twRC, ML_INFO);
  {$ENDIF}
  Result := twRC;
end; { End TTWAINSource.CapabilityMsg.                                         }


function TTWAINSource.ReceiveCapMsg(pTWMsg : PTWMessage) : TW_UINT16;

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
      {$IFDEF VER100}
        else begin
             pTW_UINT32(Value)^ := longint($FFFFFFFF);
        end;
      {$ELSE}
        else begin
             pTW_UINT32(Value)^ := longint($FFFFFFFF);
        end;
      {$ENDIF}
      end;
    end; { End GetValue.                                                       }


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
      // A special case for TW_STRXXX.
      TWTY_STR32  : CopyMemory(pTW_STR32(Value), pTW_STR32(pItem), SizeOf(TW_STR32));
      TWTY_STR64  : CopyMemory(pTW_STR64(Value), pTW_STR64(pItem), SizeOf(TW_STR64));
      TWTY_STR128 : CopyMemory(pTW_STR128(Value), pTW_STR128(pItem), SizeOf(TW_STR128));
      TWTY_STR255 : CopyMemory(pTW_STR255(Value), pTW_STR255(pItem), SizeOf(TW_STR255));
      {$IFDEF VER120}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ELSE}
        else pTW_UINT32(Value)^ := longint($FFFFFFFF);
      {$ENDIF}
      end;
      pItem := pItem + ItemSize;
    end; { End ItemToUInt32.                                                   }

var pArray       : pTW_ARRAY;
    pRange       : pTW_RANGE;
    pOneValue    : pTW_ONEVALUE;
    pEnumeration : pTW_ENUMERATION;

    pData        : pTW_CAPABILITY;
    Cap          : TW_UINT16;
    pCapCon      : Pointer;

    pItem        : PAnsiChar;
    pItemList    : PAnsiChar;
    i, ItemSize  : integer;
    ItemSizes    : integer;
    UnpackSize   : integer;

    OldContainer : TTwnsContainer;
    NewContainer : TTwnsContainer;
begin
  Result := TWRC_SUCCESS;

  // Get Capability.
  Cap := pTW_CAPABILITY(pTWMsg^.pData)^.Cap;
  OldContainer := TTwnsContainer(FContainerList.Items[Cap]);
  NewContainer := Nil;

  if Assigned(OldContainer)
  then begin
       if ((OldContainer.FQuerySupport and TWQC_SET) <> 0)
       then begin
            // Create a new container for incoming data.
            NewContainer := TTwnsContainer.Create(Nil);
            NewContainer.Capability := OldContainer.Capability;

            // Check that the capability exists and is supported.
            if Assigned(NewContainer)
            then begin
                 pData  := pTWMsg^.pData;
                 pCapCon := Nil;

                 // Retreive data from twain-container.
                 case pData^.ConType of
                 TWON_ARRAY       : begin
                                      pArray := pTW_ARRAY(GlobalLock(pData^.hContainer));
                                      if (pArray <> Nil)
                                      then begin
                                           UnpackSize := GetCapConSize(pArray^.ItemType);
                                           ItemSizes  := UnpackSize * pArray^.NumItems;
                                           GetMem(pCapCon, SizeOf(TW_ARRAY) + ItemSizes);
                                           if (pCapCon <> Nil)
                                           then begin
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
                                           end
                                           else Result := TWCC_LOWMEMORY;
                                           GlobalUnlock(pData^.hContainer);
                                      end;
                                    end;
                 TWON_ENUMERATION : begin
                                      pEnumeration := pTW_ENUMERATION(GlobalLock(pData^.hContainer));
                                      if (pEnumeration <> Nil)
                                      then begin
                                           UnpackSize := GetCapConSize(pEnumeration^.ItemType);
                                           ItemSizes  := UnpackSize * pEnumeration^.NumItems;
                                           GetMem(pCapCon, SizeOf(TW_ENUMERATION) + ItemSizes);
                                           if (pCapCon <> Nil)
                                           then begin
                                                with TW_ENUMERATION(pCapCon^)
                                                do begin
                                                   ItemType     := pEnumeration^.ItemType;
                                                   NumItems     := pEnumeration^.NumItems;
                                                   CurrentIndex := pEnumeration^.CurrentIndex;
                                                   DefaultIndex := pEnumeration^.DefaultIndex;
                                                   {$IFDEF EXTEC}
                                                   if (CurrentIndex >= NumItems)
                                                   then CurrentIndex := NumItems - 1;
                                                   if (DefaultIndex >= NumItems)
                                                   then DefaultIndex := NumItems - 1;
                                                   {$ENDIF}
                                                   pItem        := @pEnumeration^.ItemList;
                                                   ItemSize     := TWItemSize[ItemType];
                                                   pItemList    := @ItemList[0];
                                                   for i := 0 to (NumItems - 1)
                                                   do begin
                                                      ItemToUInt32(pItem, pItemList, ItemType, ItemSize);
                                                      pItemList := pItemList + UnpackSize;
                                                   end;
                                                end;
                                           end
                                           else Result := TWCC_LOWMEMORY;
                                           GlobalUnlock(pData^.hContainer);
                                      end;
                                    end;
                 TWON_ONEVALUE    : begin
                                      pOneValue := pTW_ONEVALUE(GlobalLock(pData^.hContainer));
                                      if (pOneValue <> Nil)
                                      then begin
                                           ItemSizes := SizeOf(TW_ONEVALUE) +
                                                        GetCapConSize(pOneValue^.ItemType) -
                                                        SizeOf(TW_UINT32);
                                           GetMem(pCapCon, ItemSizes);
                                           if (pCapCon <> Nil)
                                           then begin
                                                with TW_ONEVALUE(pCapCon^)
                                                do begin
                                                   ItemType := pOneValue^.ItemType;
                                                   GetValue(@pOneValue^.Item, @Item, ItemType);
                                                end;
                                           end
                                           else Result := TWCC_LOWMEMORY;
                                           GlobalUnlock(pData^.hContainer);
                                      end;
                                    end;
                 TWON_RANGE       : begin
                                      pRange := pTW_RANGE(GlobalLock(pData^.hContainer));
                                      if (pRange <> Nil)
                                      then begin
                                           GetMem(pCapCon, SizeOf(TW_RANGE));
                                           if (pCapCon <> Nil)
                                           then begin
                                                with TW_RANGE(pCapCon^)
                                                do begin
                                                   ItemType     := pRange^.ItemType;
                                                   MinValue     := pRange^.MinValue;
                                                   MaxValue     := pRange^.MaxValue;
                                                   StepSize     := pRange^.StepSize;
                                                   DefaultValue := pRange^.DefaultValue;
                                                   CurrentValue := pRange^.CurrentValue;
                                                end;
                                           end
                                           else Result := TWCC_LOWMEMORY;
                                           GlobalUnlock(pData^.hContainer);
                                      end;
                                    end;

                 else Result := TWCC_BUMMER; // Unknown container type.
                 end;

                 if (Result = TWCC_SUCCESS)
                 then begin
                      // Assign data to a Container.
                      NewContainer.AssignContainerPtr(Cap, pData^.ConType, pCapCon);

                      // Merge received data with existing container.
                      Result := MergeCapabilities(OldContainer, NewContainer);

                      // Let the user interface know that data has changed.
                      if (Result = TWCC_SUCCESS) or (Result = TWRC_CHECKSTATUS)
                      then FormSource.ChangeCapability(FContainerList, Cap);
                 end;
            end
            else Result := TWCC_LOWMEMORY;
       end
       else Result := TWCC_CAPBADOPERATION;
  end
  else Result := TWCC_CAPUNSUPPORTED;

  if Assigned(NewContainer)
  then NewContainer.Free;
end; { End TTWAINSource.ReceiveCapMsg.                                         }


function TTWAINSource.ReturnCapMsg(pTWMsg : PTWMessage) : TW_UINT16;

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
      end;
      pItem := pItem + ItemSize;
    end; { End ItemToUInt32.                                                   }


var pData        : pTW_CAPABILITY;
    Container    : TTwnsContainer;
    Cap          : TW_UINT16;
    ConType      : TW_UINT16;
    pArray       : pTW_ARRAY;
    pRange       : pTW_RANGE;
    pOneValue    : pTW_ONEVALUE;
    pEnumeration : pTW_ENUMERATION;
    pItem        : PAnsiChar;
    pItemList    : PAnsiChar;
    i, ItemSize  : integer;
    ItemSizes    : integer;
    UnpackSize   : integer;
    pCapCon      : Pointer;
    CurrentValue : Variant;
    DefaultValue : Variant;
begin
  Result := TWRC_SUCCESS;

  Cap := pTW_CAPABILITY(pTWMsg^.pData)^.Cap;
  Container := TTwnsContainer(FContainerList.Items[Cap]);
  if Assigned(Container)
  then begin
       pData  := pTWMsg^.pData;
       // Check if the application requests the capability returned in a
       // special container.

       if (pData^.ConType in [TWON_ARRAY,TWON_ENUMERATION,TWON_RANGE,TWON_ONEVALUE])
       then begin
            ConType := pData^.ConType;
            // Make sure that we support the requested container of this
            // capability.
            if (ConType <> Container.ContainerType) and (ConType <> TWON_ONEVALUE)
            then ConType := Container.ContainerType;
            case Cap of
            CAP_SUPPORTEDCAPS : ConType := TWON_ARRAY;
            CAP_EXTENDEDCAPS  : ConType := TWON_ARRAY;
            end;
       end
            // If requested container isn't valid use default container type.
       else ConType := Container.ContainerType;

       // If requested container value is the "Default" or "Current" return
       // data in a TW_ONEVALUE container.
       case pTWMsg^.MSG of
       MSG_GETCURRENT,
       MSG_GETDEFAULT : ConType := TWON_ONEVALUE;
       end;

       case ConType of
       TWON_ARRAY       : begin
                            pCapCon := Container.GetContainerPtr(False);
                            UnpackSize := GetCapConSize(TW_ARRAY(pCapCon^).ItemType);
                            ItemSize  := TWItemSize[TW_ARRAY(pCapCon^).ItemType];
                            ItemSizes :=  ItemSize * TW_ARRAY(pCapCon^).NumItems;
                            pData^.ConType := TWON_ARRAY;
                            pData^.hContainer := GlobalAlloc(GHND, SizeOf(TW_ARRAY) + ItemSizes);
                            pArray := GlobalLock(pData^.hContainer);
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
                                 GlobalUnlock(pData^.hContainer);
                            end
                            else Result := TWCC_LOWMEMORY;
                          end;
       TWON_ENUMERATION : begin
                            pCapCon := Container.GetContainerPtr(False);
                            UnpackSize := GetCapConSize(TW_ENUMERATION(pCapCon^).ItemType);
                            ItemSize   := TWItemSize[TW_ENUMERATION(pCapCon^).ItemType];
                            ItemSizes  := ItemSize * TW_ENUMERATION(pCapCon^).NumItems;
                            pData^.ConType := TWON_ENUMERATION;
                            pData^.hContainer := GlobalAlloc(GHND, SizeOf(TW_ENUMERATION) + ItemSizes);
                            pEnumeration := GlobalLock(pData^.hContainer);
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
                                 GlobalUnlock(pData^.hContainer);
                            end
                            else Result := TWCC_LOWMEMORY;
                          end;
       TWON_ONEVALUE    : begin
                            if (pTWMsg^.MSG = MSG_GETDEFAULT)
                            then begin
                                 // Set DefaultValue as CurrentValue.
                                 CurrentValue := Container.CurrentValue;
                                 DefaultValue := Container.DefaultValue;
                                 Container.CurrentValue := DefaultValue;
                            end;

                            pCapCon := Container.GetContainerPtr(True);
                            ItemSize   := TWItemSize[TW_ONEVALUE(pCapCon^).ItemType];
                            pData^.ConType := TWON_ONEVALUE;
                            pData^.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE) + ItemSize);
                            pOneValue := GlobalLock(pData^.hContainer);
                            if (pOneValue <> Nil)
                            then begin
                                 with TW_ONEVALUE(pCapCon^)
                                 do begin
                                    pOneValue^.ItemType := ItemType;
                                    pItem := @pOneValue^.Item;
                                    UInt32ToItem(pItem, @Item, ItemType, ItemSize);
                                 end;
                                 GlobalUnlock(pData^.hContainer);
                            end
                            else Result := TWCC_LOWMEMORY;

                            // Re-establish CurrentValue if MSG = MSG_GETDEFAULT.
                            if (pTWMsg^.MSG = MSG_GETDEFAULT)
                            then Container.CurrentValue := CurrentValue;
                          end;
       TWON_RANGE       : begin
                            pCapCon := Container.GetContainerPtr(False);
                            pData^.ConType := TWON_RANGE;
                            pData^.hContainer := GlobalAlloc(GHND,sizeof(TW_RANGE));
                            pRange := GlobalLock(pData^.hContainer);
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
                                 GlobalUnlock(pData^.hContainer);
                            end
                            else Result := TWCC_LOWMEMORY;
                          end;
       else begin // Error, non-supported type.
            Result := TWCC_BUMMER;
       end;
       end;
  end
  else Result := TWCC_CAPUNSUPPORTED;
end; { End TTWAINSource.ReturnCapMsg.                                          }


function TTWAINSource.MergeCapabilities(Dest, Source: TTwnsContainer) : TW_UINT16;

  function SetCurrentValue : TW_UINT16;
  begin
    Result := TWRC_SUCCESS;
    if (Source.CurrentValue < Dest.MinValue)
    then begin
         Source.CurrentValue := Dest.MinValue;
         Result := TWRC_CHECKSTATUS;
    end;
    if (Source.CurrentValue > Dest.MaxValue)
    then begin
         Source.CurrentValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    Dest.CurrentValue := Source.CurrentValue;
  end; { End SetCurrentValue.                                                  }


  function SetDefaultValue : TW_UINT16;
  begin
    Result := TWRC_SUCCESS;
    {
    if (Source.DefaultValue < Dest.MinValue)
    then begin
         Source.DefaultValue := Dest.MinValue;
         Result := TWRC_CHECKSTATUS;
    end;
    if (Source.DefaultValue > Dest.MaxValue)
    then begin
         Source.DefaultValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    Dest.DefaultValue := Source.DefaultValue;
    }
  end; { End SetDefaultValue.                                                  }


  function ValueInList(Value : Variant) : TW_UINT16;
  var i       : integer;
      ValueOK : boolean;
  begin
    Result := TWRC_CHECKSTATUS;
    ValueOK := False;
    i := 0;
    while (i < Dest.NumItems) and Not(ValueOK)
    do begin
       if (Dest.Items[i] = Value)
       then ValueOK := True;
       inc(i);
    end;
    if ValueOK
    then Result := TWRC_SUCCESS;
  end; { End ValueInList.                                                      }


  function SetEnumCurrentValue : TW_UINT16;
  begin
    Result := ValueInList(Source.CurrentValue);
    if (Result = TWRC_SUCCESS)
    then Dest.CurrentValue := Source.CurrentValue;
  end; { End SetEnumCurrentValue.                                              }


  function SetEnumDefaultValue : TW_UINT16;
  begin
    Result := ValueInList(Source.DefaultValue);
    if (Result = TWRC_SUCCESS)
    then Dest.DefaultValue := Source.DefaultValue;
    {
    if (Source.DefaultValue < Dest.MinValue)
    then begin
         Source.DefaultValue := Dest.MinValue;
         Result := TWRC_CHECKSTATUS;
    end;
    if (Source.DefaultValue > Dest.MaxValue)
    then begin
         Source.DefaultValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    Dest.DefaultValue := Source.DefaultValue;
    }
  end; { End SetEnumDefaultValue.                                              }


  function SetMinValue : TW_UINT16;
  begin
    Result := TWRC_SUCCESS;
    if (Source.MinValue < Dest.MinValue)
    then begin
         Source.MinValue := Dest.MinValue;
         Result := TWRC_CHECKSTATUS;
    end;
    if (Source.MinValue > Dest.MaxValue)
    then begin
         Source.MinValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    Dest.MinValue := Source.MinValue;
  end; { End SetMinValue.                                                      }


  function SetMaxValue : TW_UINT16;
  begin
    Result := TWRC_SUCCESS;
    if (Source.MaxValue > Dest.MaxValue)
    then begin
         Source.MaxValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    if (Source.MaxValue < Dest.MinValue)
    then begin
         Source.MinValue := Dest.MaxValue;
         Result := TWRC_CHECKSTATUS;
    end;
    Dest.MaxValue := Source.MaxValue;
  end; { End SetMaxValue.                                                      }

var i : integer;
begin
  Result := TWRC_SUCCESS;
  case Dest.ContainerType of
  TWON_ARRAY       : begin
                       case Source.ContainerType of
                       TWON_ARRAY       : begin
                                            // How to apply limits to arrays !?!
                                            if (Dest.Capability = CAP_DEVICEEVENT)
                                            then begin
                                                 Dest.NumItems := Source.NumItems;
                                                 for i := 0 to (Source.NumItems - 1)
                                                 do Dest.Items[i] := Source.Items[i];
                                            end;
                                          end;
                       else Result := TWCC_CAPUNSUPPORTED;
                       end;
                     end;
  TWON_ENUMERATION : begin
                       case Source.ContainerType of
                       TWON_ARRAY       : Result := TWRC_CHECKSTATUS;
                       TWON_ENUMERATION : begin
                                            Result := SetEnumCurrentValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetEnumDefaultValue;
                                          end;
                       TWON_ONEVALUE    : Dest.CurrentValue := Source.CurrentValue;
                       TWON_RANGE       : begin
                                            Result := SetCurrentValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetDefaultValue;
                                          end;
                       end;
                     end;
  TWON_ONEVALUE    : begin
                       case Source.ContainerType of
                       TWON_ARRAY : Result := TWRC_CHECKSTATUS;
                       else Dest.CurrentValue := Source.CurrentValue;
                       end;
                     end;
  TWON_RANGE       : begin
                       case Source.ContainerType of
                       TWON_ARRAY       : Result := TWRC_CHECKSTATUS;
                       TWON_ENUMERATION : begin
                                            Result := SetCurrentValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetDefaultValue;
                                          end;
                       TWON_ONEVALUE    : Dest.CurrentValue := Source.CurrentValue;
                       TWON_RANGE       : begin
                                            Result := SetCurrentValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetDefaultValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetMinValue;
                                            if (Result = TWRC_SUCCESS)
                                            then Result := SetMaxValue;
                                          end;
                       end;
                     end;
  end;
end; { End TTWAINSource.MergeCapabilities.                                     }


{------------------------------------------------------------------------------}
{ FUNCTION: StatusMsg                                                          }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:   Valid states 3 through 7.  Source fills in all fields of pDSMStatus }
{ and clears it's condition code.                                              }
{------------------------------------------------------------------------------}

function TTWAINSource.StatusMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC : TW_UINT16;
begin
  twRC := TWRC_SUCCESS;
  case pTWMsg^.MSG of
  MSG_GET : begin
              pTW_STATUS(pTWMsg^.pData)^ := FDSStatus; // copy all fields
              FDSStatus.ConditionCode := TWCC_SUCCESS; // clear condition code
            end;
  else      begin // Invalid triplet.
              FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
              twRC := TWRC_FAILURE;
            end;
  end;
  Result := twRC;
end; { End TTWAINSource.StatusMsg.                                             }


{------------------------------------------------------------------------------}
{ FUNCTION: Msg                                                                }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_SEQERROR                                                   }
{              TWCC_BADPROTOCOL        // debug ONLY                           }
{                                                                              }
{ NOTES:   Valid states: 4 -- 7.                                               }
{------------------------------------------------------------------------------}

function TTWAINSource.PendingXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC          : TW_UINT16;
    PendingImages : TW_INT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('PendingXferMsg');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case pTWMsg^.MSG of
  MSG_GET       : begin // Valid states 4 -- 7.
                    case FDSState of
                    // Single image supported, always 0.
                    STATE4,
                    STATE5,
                    STATE6,
                    STATE7 : pTW_PENDINGXFERS(pTWMsg^.pData)^.Count := 0;
                    else     begin // Fail get due to  seqerror.
                               FDSStatus.ConditionCode := TWCC_SEQERROR;
                               twRC := TWRC_FAILURE;
                             end;
                    end;
                  end;
    MSG_ENDXFER : begin // Clears ONLY current transfer, valid 6 or 7
                    // pending images needs to be set somewhere else --
                    // this is a patch
                    PendingImages := 0;
                    dec(PendingImages);
                    if (PendingImages < 0)
                    then PendingImages := 0;

                    pTW_PENDINGXFERS(pTWMsg^.pData)^.Count := TW_UINT16(PendingImages);
                  end;
    MSG_RESET   : // Clears ALL transfers, valid 6 or 7
                  if ((FDSState = STATE6) or (FDSState = STATE7))
                  then begin
                       // Always transition to state 5 since I only support
                       // single image.
                       FDSState := STATE5;
                       pTW_PENDINGXFERS(pTWMsg^.pData)^.Count := 0;
                  end
                  else begin
                       FDSStatus.ConditionCode := TWCC_SEQERROR;
                       twRC := TWRC_FAILURE;
                  end;
  else            begin
                    FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                    twRC := TWRC_FAILURE;
                  end;
  end;
  Result := twRC;
end; { End TTWAINSource.PendingXferMsg.                                        }


{------------------------------------------------------------------------------}
{ FUNCTION: SetupMemXferMsg                                                    }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_SEQERROR                                                   }
{              TWCC_BADPROTOCOL        // debug ONLY                           }
{                                                                              }
{ NOTES:   Valid states: 4 -- 6.                                               }
{ Source fills in the following:                                               }
{------------------------------------------------------------------------------}

function TTWAINSource.SetupMemXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC         : TW_UINT16;
    LongWidth    : integer;
    SetupMemXfer : pTW_SETUPMEMXFER;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('SetupMemXferMsg');
  {$ENDIF}

  FXferDone  := False;
  FXferFirst := True;
  FBytesOut  := 0;

  // for DEBUG only.
  FXferCount := 0;

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6 : case pTWMsg^.MSG of
           MSG_GET : begin
                       SetupMemXfer := pTW_SETUPMEMXFER(pTWMsg^.pData);
                       FMinBufSize := 0;
                       FMaxBufSize := 0;

                       if Assigned(FormSource)
                       then begin
                            if (FormSource.ImageStream.Size = 0)
                            then FormSource.CopyBmpToStream;
                            if (FormSource.ImageStream.Size > 0)
                            then begin
                                 with FormSource.pBmpInfo^.bmiHeader
                                 do begin
                                    LongWidth := (((biWidth * biBitCount) + 31) div 32) * 4;
                                    FMaxBufSize := biHeight * LongWidth;
                                    FMinBufSize := LongWidth;
                                 end;
                            end
                            else begin
                                 FMinBufSize := 1024;
                                 FMaxBufSize := 32768;
                            end;
                       end;
                       // else twRC := TWRC_FAILURE;

                       SetupMemXfer^.MinBufSize := FMinBufSize;
                       SetupMemXfer^.MaxBufSize := FMaxBufSize;
                       SetupMemXfer^.Preferred  := FMaxBufSize;
                     end;
           else      begin
                       FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                       twRC := TWRC_FAILURE;
                     end;
           end;
  else     begin
             FDSStatus.ConditionCode := TWCC_SEQERROR;
             twRC := TWRC_FAILURE;
           end;
  end;
  Result := twRC;
end; { End TTWAINSource.SetupMemXferMsg.                                       }


{------------------------------------------------------------------------------}
{ FUNCTION: SetupFileXferMsg                                                   }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_SEQERROR                                                   }
{              TWCC_BADVALUE                                                   }
{              TWCC_BADPROTOCOL        // not supportted                       }
{                                                                              }
{ NOTES:  Valid states: 4 -- 6.                                                }
{------------------------------------------------------------------------------}

function TTWAINSource.SetupFileXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC    : TW_UINT16;
    pSetup  : pTW_SETUPFILEXFER;
    hFXFile : {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.HFILE;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('SetupFileXferMsg');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6 : case pTWMsg^.MSG of
           MSG_SET   : begin
                         pSetup := pTW_SETUPFILEXFER(pTWMsg^.pData);
                         FFileName := string(pSetup^.FileName);
                         //lstrcpyn(FFileName, pSetup^.FileName, SizeOf(FFileName)); mxmxm

                         // Check that the file is valid.
                         if Not(FileExists(FFileName))
                         then begin
                              hFXFile := FileCreate(FFileName);
                              if (hFXFile = DWORD(-1))
                              then begin
                                   FDSStatus.ConditionCode := TWCC_BADVALUE;
                                   twRc := TWRC_FAILURE;
                              end
                              else FileClose(hFXFile);
                         end;
                       end;
           MSG_GETDEFAULT,
           MSG_GETCURRENT,
           MSG_GET   : begin
                         pSetup := pTW_SETUPFILEXFER(pTWMsg^.pData);
                         pSetup^.FileName := '.\twain.bmp';
                         pSetup^.Format  := word(TWFF_BMP);
                         pSetup^.VRefNum := 0;
                       end;
           else        begin
                         FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                         twRC := TWRC_FAILURE;
                       end;
           end;
  else     begin
             FDSStatus.ConditionCode := TWCC_SEQERROR;
             twRC := TWRC_FAILURE;
           end;
  end;
  Result := twRC;
end; { End TTWAINSource.SetupFileXferMsg.                                      }


{------------------------------------------------------------------------------}
{ FUNCTION: XferGroupMsg                                                       }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:   Valid states: 4 -- 6.  Since only the Data Group is supported       }
{ under Version 1.0 of the specification the application should only use       }
{ MSG_GET.                                                                     }
{------------------------------------------------------------------------------}

function TTWAINSource.XferGroupMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('XferGroupMsg');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6 : case pTWMsg^.MSG of
           MSG_GET,
           MSG_GETDEFAULT,
           MSG_GETCURRENT,
           MSG_RESET      : pTW_UINT16(pTWMsg^.pData)^ := DG_IMAGE;
           MSG_SET        : ; // ONLY images are define in current spec so you
                              // can not change DG_ type.
           else             begin
                              FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                              twRC := TWRC_FAILURE;
                            end;
           end;
  else     begin
             FDSStatus.ConditionCode := TWCC_SEQERROR;
             twRC := TWRC_FAILURE;
           end;
  end;
  Result := twRC;
end; { End TTWAINSource.XferGroupMsg.                                          }


function TTWAINSource.DeviceEventMsg(PTWMsg  : PTWMessage) : TW_UINT16;
var twRC         : TW_UINT16;
    pDeviceEvent : pTW_DEVICEEVENT;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('DeviceEventMsg');
  {$ENDIF}

  twRC := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6,
  STATE7 : case pTWMsg^.MSG of
           MSG_GET : begin
                       pDeviceEvent := pTW_DEVICEEVENT(pTWMsg^.pData);
                       if (FDeviceEventList.Count > 0)
                       then begin
                            CopyMemory(pDeviceEvent,
                                       pTW_DEVICEEVENT(FDeviceEventList.Items[0]),
                                       SizeOf(TW_DEVICEEVENT));
                            FreeMem(pTW_DEVICEEVENT(FDeviceEventList.Items[0]), SizeOf(TW_DEVICEEVENT));
                            FDeviceEventList.Delete(0);
                       end
                       else begin
                            if FEventOverflow
                            then begin
                                 FEventOverflow := False;
                                 FDSStatus.ConditionCode := TWCC_SEQERROR; // TWCC_DEVICEEVENTOVERFLOW;
                            end
                            else FDSStatus.ConditionCode := TWCC_SEQERROR;
                            twRC := TWRC_FAILURE;
                       end;
                     end;
           else      begin
                       FDSStatus.ConditionCode := TWCC_SEQERROR;
                       twRC := TWRC_FAILURE;
                     end;
           end;
  else     begin
             FDSStatus.ConditionCode := TWCC_SEQERROR;
             twRC := TWRC_FAILURE;
           end;
  end;
  Result := twRC;
end; { End TTWAINSource.DeviceEventMsg.                                        }

{------------------------------------------------------------------------------}
{ FUNCTION: Palette8Msg                                                        }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:                                                                       }
{------------------------------------------------------------------------------}

function TTWAINSource.Palette8Msg(pTWMsg : PTWMessage) : TW_UINT16;
var twRc      : TW_UINT16;
    pPal      : pTW_PALETTE8;
    pDib      : PBITMAPINFO;
    index     : integer;
    NumColors : longint;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('Palette8Msg');
  {$ENDIF}

  twRc := TWRC_SUCCESS;
  case FDSState of
  STATE4,
  STATE5,
  STATE6 : begin
             case pTWMsg^.MSG of
             MSG_GET : begin
                         // Palette info is stored in pBmpInfo (ImageStream).
                         if (FormSource.ImageStream.Size = 0)
                         then FormSource.CopyBmpToStream;
                         if (FormSource.pBmpInfo <> Nil)
                         then begin
                              pDib      := FormSource.pBmpInfo;
                              NumColors := pDib^.bmiHeader.biClrUsed;
                              if (NumColors = 0)
                              then begin
                                   if (pDib^.bmiHeader.biBitCount <= 8)
                                   then NumColors := (1 shl pDib^.bmiHeader.biBitCount)
                                   else NumColors := 0;
                              end;
                              if (pDib^.bmiHeader.biClrUsed  =  0) and
                                 (pDib^.bmiHeader.biBitCount <= 8)
                              then pDib^.bmiHeader.biClrUsed := NumColors;

                              pPal := pTW_PALETTE8(pTWMsg^.pData);
                              pPal^.NumColors   := TW_UINT16(NumColors);
                              pPal^.PaletteType := TWPA_RGB;

                              for index := 0 to (NumColors - 1)
                              do begin
                                 with pDib^.bmiColors[index]
                                 do begin
                                    pPal^.Colors[index].Channel1 := rgbRed;
                                    pPal^.Colors[index].Channel2 := rgbGreen;
                                    pPal^.Colors[index].Channel3 := rgbBlue;
                                    pPal^.Colors[index].Index    := index;
                                 end;
                              end;
                         end;
                       end;
             MSG_GETDEFAULT,
             MSG_RESET,
             MSG_SET : begin
                       end;
             else      begin
                         FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                         twRc := TWRC_FAILURE;
                       end;
             end;
           end;
  else     begin
             FDSStatus.ConditionCode := TWCC_SEQERROR;
             twRc := TWRC_FAILURE;
           end;
  end;
  Result := twRc;
end; { End TTWAINSource.Palette8Msg.                                           }


{------------------------------------------------------------------------------}
{ GrayResponseMsg -                                                            }
{------------------------------------------------------------------------------}

function TTWAINSource.GrayResponseMsg(pTWMsg : PTWMessage) : TW_UINT16;
(*
var x       : TW_UINT16;
    TmpStr  : array[0..19] of char;
    RespNum : array[0..19] of char;
    Color   : array[0..19] of char;
*)    
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('GrayResponseMsg');
  {$ENDIF}
  (*
  for x := 0 to (16 - 1)
  do begin
     StrCopy(RespNum, 'Response[');
     Str(x, TmpStr);
     StrCat(RespNum, TmpStr);
     StrCat(RespNum, ']');

     StrCopy(Color, '{');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel1, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, ',');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel2, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, ',');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel3, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, '}');
  end;
  *)
  Result := TWRC_FAILURE; // TWRC_SUCCESS;
end; { End TTWAINSource.GrayResponseMsg.                                       }


{------------------------------------------------------------------------------}
{ RGBResponse - Note: Uses the GrayResponse structure which is identical to    }
{ the RGBRESPONSE structure in TWAIN 1.5.4                                     }
{------------------------------------------------------------------------------}

function TTWAINSource.RGBResponseMsg(pTWMsg : PTWMessage) : TW_UINT16;
(*
var x  : TW_UINT16;
    TmpStr  : array[0..19] of char;
    RespNum : array[0..19] of char;
    Color   : array[0..19] of char;
*)
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('RGBResponseMsg');
  {$ENDIF}

  (*
  for x := 0 to (16 - 1)
  do begin
     StrCopy(RespNum, 'Response[');
     Str(x, TmpStr);
     StrCat(RespNum, TmpStr);
     StrCat(RespNum, ']');

     StrCopy(Color, '{');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel1, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, ',');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel2, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, ',');
     Str(pTW_GRAYRESPONSE(pTWMsg^.pData)^.Response[x].Channel3, TmpStr);
     StrCat(Color, TmpStr);
     StrCat(Color, '}');
  end;
  *)
  Result := TWRC_FAILURE; // TWRC_SUCCESS;
end; { End TTWAINSource.RGBResponseMsg.                                        }


{------------------------------------------------------------------------------}
{ CIEColorMsg -                                                                }
{------------------------------------------------------------------------------}

function TTWAINSource.CIEColorMsg(pTWMsg : PTWMessage) : TW_UINT16;
//var i : integer;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('CIEColorMsg');
  {$ENDIF}

  (*
  pTW_CIECOLOR(pTWMsg^.pData)^.ColorSpace      := TWPT_RGB;
  pTW_CIECOLOR(pTWMsg^.pData)^.LowEndian       := 0;
  pTW_CIECOLOR(pTWMsg^.pData)^.DeviceDependent := 1;
  pTW_CIECOLOR(pTWMsg^.pData)^.VersionNumber   := 0;

  // Transform stage ABC
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Decode[0].StartIn := FloatToFIX32(0.1);

  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[0,0] := FloatToFIX32(0.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[0,1] := FloatToFIX32(0.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[0,2] := FloatToFIX32(0.2);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[1,0] := FloatToFIX32(1.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[1,1] := FloatToFIX32(1.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[1,2] := FloatToFIX32(1.2);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[2,0] := FloatToFIX32(2.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[2,1] := FloatToFIX32(2.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageABC.Mix[2,2] := FloatToFIX32(2.2);

  // Transform Stage LMN
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Decode[0].StartIn := FloatToFIX32(1.1);

  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[0,0] := FloatToFIX32(0.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[0,1] := FloatToFIX32(0.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[0,2] := FloatToFIX32(0.2);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[1,0] := FloatToFIX32(1.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[1,1] := FloatToFIX32(1.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[1,2] := FloatToFIX32(1.2);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[2,0] := FloatToFIX32(2.0);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[2,1] := FloatToFIX32(2.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.StageLMN.Mix[2,2] := FloatToFIX32(2.2);

  // CIEPoint
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePoint.X := FloatToFIX32(4.4);
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePoint.Y := FloatToFIX32(5.5);
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePoint.Z := FloatToFIX32(6.6);

  // CIEPoint
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackPoint.X := FloatToFIX32(1.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackPoint.Y := FloatToFIX32(5.5);
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackPoint.Z := FloatToFIX32(6.6);

  // CIEPoint
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePaper.X := FloatToFIX32(1.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePaper.Y := FloatToFIX32(5.5);
  pTW_CIECOLOR(pTWMsg^.pData)^.WhitePaper.Z := FloatToFIX32(6.6);

  // CIEPoint
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackInk.X := FloatToFIX32(1.1);
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackInk.Y := FloatToFIX32(5.5);
  pTW_CIECOLOR(pTWMsg^.pData)^.BlackInk.Z := FloatToFIX32(6.6);

  i := 0;
  pTW_CIECOLOR(pTWMsg^.pData)^.Samples[i] := FloatToFIX32(99.99);
  i := 1;
  pTW_CIECOLOR(pTWMsg^.pData)^.Samples[i] := FloatToFIX32(98.98);
  i := 2;
  pTW_CIECOLOR(pTWMsg^.pData)^.Samples[i] := FloatToFIX32(97.97);
  i := 3;
  pTW_CIECOLOR(pTWMsg^.pData)^.Samples[i] := FloatToFIX32(96.96);
  *)
  Result := TWRC_FAILURE; // TWRC_SUCCESS;
end; { End TTWAINSource.CIEColorMsg.                                           }


{------------------------------------------------------------------------------}
{ JPEGCompressionMsg -                                                         }
{------------------------------------------------------------------------------}

function TTWAINSource.JPEGCompressionMsg(pTWMsg : PTWMessage) : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('JPEGCompressionMsg');
  {$ENDIF}

  Result := TWRC_FAILURE; // TWRC_SUCCESS;
end; { End TTWAINSource.JPEGCompressionMsg.                                    }


{------------------------------------------------------------------------------}
{ IsPaletteGray.                                                               }
{                                                                              }
{ MCM, This function is not a part of the original source code.                }
{                                                                              }
{------------------------------------------------------------------------------}

function TTWAINSource.IsPaletteGray : boolean;
var pDib      : PBITMAPINFO;
    index     : integer;
    NumColors : longint;
begin
  Result := False;
  if Assigned(FormSource)
  then begin
       if (FormSource.ImageStream.Size = 0)
       then FormSource.CopyBmpToStream;

       if (FormSource.pBmpInfo <> Nil)
       then begin
            pDib := FormSource.pBmpInfo;

            NumColors := pDib^.bmiHeader.biClrUsed;
            if (NumColors = 0)
            then if (pDib^.bmiHeader.biBitCount <= 8)
                 then NumColors := (1 shl pDib^.bmiHeader.biBitCount)
                 else NumColors := 0;

            index  := 0;
            Result := True;
            while (index < NumColors) and Result
            do begin
               with pDib^.bmiColors[index]
               do begin
                  if (rgbRed <> rgbGreen) or (rgbRed <> rgbBlue)
                  then Result := False
                  else inc(index);
               end;
            end;
            if (Index <> NumColors) and Result
            then Result := False;
       end;
  end;
  Result := Result;
end; { End TTWAINSource.IsPaletteGray.                                         }


{------------------------------------------------------------------------------}
{ FUNCTION: ImageInfoMsg                                                       }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{          TWCC_SEQERROR                                                       }
{ NOTES:  Valid states 6.  Fills in all fields of imageinfo struct.  Get       }
{ info on pending image transfer.                                              }
{ Fix the return codes, suppose to be pending, in which case it has already    }
{ been cleared for take off an none of the other message should occur at       }
{ this time !!!                                                                }
{------------------------------------------------------------------------------}

function TTWAINSource.ImageInfoMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC         : TW_UINT16;
    i            : integer;
    pII          : pTW_IMAGEINFO;
    BitPerSample : integer;
    XRes         : TTwnsContainer;
    YRes         : TTwnsContainer;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('ImageInfoMsg');
  {$ENDIF}

  if (FDSState = STATE6)
  then begin
       case pTWMsg^.MSG of
       MSG_GET : begin
                   if (FormSource.ImageStream.Size = 0)
                   then FormSource.CopyBmpToStream;

                   if (FormSource.pBmpInfo <> Nil)
                   then begin
                        // Set up local pointer to IMAGEINFO struct.
                        pII := pTW_IMAGEINFO(pTWMsg^.pData);

                        // Use data in .BMP file to best fill the ImageInfo
                        // struct.
                        with FormSource.pBmpInfo^.bmiHeader
                        do begin
                           xRes := TTwnsContainer(FContainerList.Items[ICAP_XRESOLUTION]);
                           pII^.XResolution  := FloatToFIX32(xRes.CurrentValue);
                           yRes := TTwnsContainer(FContainerList.Items[ICAP_YRESOLUTION]);
                           pII^.YResolution  := FloatToFIX32(yRes.CurrentValue);
                           pII^.ImageWidth   := biWidth;
                           pII^.ImageLength  := biHeight;
                           pII^.BitsPerPixel := biBitCount * biPlanes;
                           pII^.Planar       := 0; // BMP image is chunky.
                        end;

                        // Images are never compressed.
                        pII^.Compression  := word(TWCP_NONE);

                        // Clear first.
                        for i := 0 to 7
                        do pII^.BitsPerSample[i] := 0;

                        // Assumes color .BMP are 1, 4, 8, 24 as spec. by MS.
                        if (pII^.BitsPerPixel <= 8)
                        then begin
                             pII^.SamplesPerPixel     := 1;
                             pII^.BitsPerSample[0]    := pII^.BitsPerPixel;
                             if IsPaletteGray
                             then pII^.PixelType      := TWPT_GRAY
                             else pII^.PixelType      := TWPT_PALETTE;
                        end
                        else begin
                             pII^.SamplesPerPixel     := 3;
                             with FormSource.pBmpInfo^.bmiHeader
                             do begin
                                case biBitCount of
                                1  : BitPerSample := 1;
                                4  : BitPerSample := 4;
                                8  : BitPerSample := 8;
                                16 : BitPerSample := 5;
                                24 : BitPerSample := 8;
                                32 : BitPerSample := 8;
                                else BitPerSample := 8;
                                end;
                             end;
                             for i := 0 to 2
                             do pII^.BitsPerSample[i] := BitPerSample;
                             pII^.PixelType           := TWPT_RGB;
                        end;
                        twRC := TWRC_SUCCESS;
                   end
                   else twRC := TWRC_FAILURE;
                 end;
       else      begin
                   FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                   twRC := TWRC_FAILURE;
                 end;
       end;
  end
  else begin
       FDSStatus.ConditionCode := TWCC_SEQERROR;
       twRC := TWRC_FAILURE;
  end;
  Result := twRC;
end; { End TTWAINSource.ImageInfoMsg.                                          }


{------------------------------------------------------------------------------}
{ FUNCTION: ImageLayoutMsg                                                     }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: status code                                                         }
{                                                                              }
{ NOTES:   This function is supported to the extent that it will               }
{ return success for each call.                                                }
{------------------------------------------------------------------------------}

function TTWAINSource.ImageLayoutMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC       : TW_UINT16;
    pData      : pTW_IMAGELAYOUT;
    xyUnit     : TTwnsContainer;
    xRes       : TTwnsContainer;
    yRes       : TTwnsContainer;
    PixFrame   : TRect;
begin
  twRC := TWRC_SUCCESS;
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('');
  {$ENDIF}

  pData := pTWMsg^.pData;
  case pTWMsg^.MSG of
  MSG_GET,
  MSG_GETDEFAULT,
  MSG_GETCURRENT,
  MSG_RESET      : begin
                     if Assigned(FormSource)
                     then begin
                          if (pTWMsg^.MSG = MSG_RESET)
                          then begin
                               FormSource.ResetImageLayout(PixFrame);
		               ImageLayout.DocumentNumber := 1;
		               ImageLayout.PageNumber     := 1;
		               ImageLayout.FrameNumber    := 1;
                          end
                          else FormSource.GetImageLayout(PixFrame);
                     end
                     else PixFrame := Rect(0, 0, 0, 0);

                     xyUnit := TTwnsContainer(FContainerList.Items[ICAP_UNITS]);
                     xRes   := TTwnsContainer(FContainerList.Items[ICAP_XRESOLUTION]);
                     yRes   := TTwnsContainer(FContainerList.Items[ICAP_YRESOLUTION]);
                     case xyUnit.CurrentIndex of
                     TWUN_INCHES,
                     TWUN_CENTIMETERS,
                     TWUN_PICAS,
                     TWUN_TWIPS       : begin
                                          with ImageLayout
                                          do begin
                                             ImageLayout.Frame.Left   := FloatToFIX32(PixFrame.Left  / xRes.CurrentValue);
                                             ImageLayout.Frame.Top    := FloatToFIX32(PixFrame.Top / yRes.CurrentValue);
                                             ImageLayout.Frame.Right  := FloatToFIX32(PixFrame.Right  / xRes.CurrentValue);
                                             ImageLayout.Frame.Bottom := FloatToFIX32(PixFrame.Bottom / yRes.CurrentValue);
                                          end;
                                        end;
                     TWUN_POINTS,
                     TWUN_PIXELS      : begin
                                          with ImageLayout,
                                               FormSource.pBmpInfo^.bmiHeader
                                          do begin
                                             ImageLayout.Frame.Left.Long   := PixFrame.Left;
                                             ImageLayout.Frame.Top.Long    := PixFrame.Top;
                                             ImageLayout.Frame.Right.Long  := PixFrame.Right;
                                             ImageLayout.Frame.Bottom.Long := PixFrame.Bottom;
                                          end;
                                        end;
                     end;

        	     pData^.Frame.Left     := ImageLayout.Frame.Left;
		     pData^.Frame.Top      := ImageLayout.Frame.Top;
		     pData^.Frame.Right    := ImageLayout.Frame.Right;
		     pData^.Frame.Bottom   := ImageLayout.Frame.Bottom;
		     pData^.DocumentNumber := ImageLayout.DocumentNumber;
		     pData^.PageNumber     := ImageLayout.PageNumber;
		     pData^.FrameNumber    := ImageLayout.FrameNumber;
                   end;
  MSG_SET        : begin
        	     ImageLayout.Frame.Left     := pData^.Frame.Left;
		     ImageLayout.Frame.Top      := pData^.Frame.Top;
		     ImageLayout.Frame.Right    := pData^.Frame.Right;
		     ImageLayout.Frame.Bottom   := pData^.Frame.Bottom;
		     ImageLayout.DocumentNumber := pData^.DocumentNumber;
		     ImageLayout.PageNumber     := pData^.PageNumber;
		     ImageLayout.FrameNumber    := pData^.FrameNumber;

                     xyUnit := TTwnsContainer(FContainerList.Items[ICAP_UNITS]);
                     xRes   := TTwnsContainer(FContainerList.Items[ICAP_XRESOLUTION]);
                     yRes   := TTwnsContainer(FContainerList.Items[ICAP_YRESOLUTION]);
                     case xyUnit.CurrentIndex of
                     TWUN_INCHES,
                     TWUN_CENTIMETERS,
                     TWUN_PICAS,
                     TWUN_TWIPS       : begin
                                          with ImagePixFrame, pData^
                                          do begin
                                             Left   := Round(xRes.CurrentValue * FIX32ToFloat(Frame.Left));
                                             Top    := Round(yRes.CurrentValue * FIX32ToFloat(Frame.Top));
                                             Right  := Round(xRes.CurrentValue * FIX32ToFloat(Frame.Right));
                                             Bottom := Round(yRes.CurrentValue * FIX32ToFloat(Frame.Bottom));
                                          end;
                                        end;
                     TWUN_POINTS,
                     TWUN_PIXELS      : begin
                                          with ImagePixFrame
                                          do begin
                                             Left   := pData^.Frame.Left.Whole;
                                             Top    := pData^.Frame.Top.Whole;
                                             Right  := pData^.Frame.Right.Whole;
                                             Bottom := pData^.Frame.Bottom.Whole;
                                          end;
                                        end;
                     end;
                     if Assigned(FormSource)
                     then FormSource.SetImageLayout(ImagePixFrame);
                   end;
  else             begin
                     FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                     twRC := TWRC_FAILURE;
                   end;
  end;
  Result := twRC;
end; { End TTWAINSource.ImageLayoutMsg                                         }


{------------------------------------------------------------------------------}
{ FUNCTION GetImageHandle.                                                     }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{ RETURNS: Handle to Image, otherwise 0.                                       }
{                                                                              }
{ MCM, This function is not a part of the original source code.                }
{                                                                              }
{                                                                              }
{------------------------------------------------------------------------------}

function TTWAINSource.GetImageHandle : THandle;
type TLongType  = record
                  case Word of
        	  0  : (Ptr  : Pointer);
	          1  : (Long : Longint);
	 	  2  : (Lo   : Word;
	  	        Hi   : Word);
		  end;
     TAByte    = array[0..0] of byte;

var  hDIBs      : THandle;
     BitsS      : TLongType;
     BitsT      : TLongType;
     StartS     : TLongType;
     StartT     : TLongType;
     ToAddrS    : TLongType;
     ToAddrT    : TLongType;
     Count      : longint;
     BlockSize  : longint;
     NoColour   : word;
     HeaderSize : word;
begin
  hDIBs  := 0;
  if (FormSource.ImageStream.Size = 0)
  then FormSource.CopyBmpToStream;

  if (FormSource.pBmpInfo <> Nil)
  then begin
       with FormSource.pBmpInfo^.bmiHeader
       do begin
          if (biClrUsed > 0) and False
          then NoColour := biClrUsed
          else begin
               if (biBitCount <= 8)
               then NoColour := (1 shl biBitCount)
               else NoColour := 0;
          end;
          if (biBitCount <= 8)
          then HeaderSize := SizeOf(TBitmapInfoHeader) +
                             (NoColour * SizeOf(TRGBQuad))
          else HeaderSize := SizeOf(TBitmapInfoHeader);
       end;

       // Create return bitmap.
       hDIBs := GlobalAlloc(gmem_Moveable or gmem_Zeroinit,
                            FormSource.pBmpInfo^.bmiHeader.biSizeImage +
                            HeaderSize);
  end
  else HeaderSize := 0;

  if (hDIBs               <> 0)   and
     (FormSource.pBmp     <> Nil) and
     (FormSource.pBmpInfo <> Nil)
  then begin
       // Copy handle to bitmap.
       BitsS.Ptr := FormSource.pBmp;
       BitsT.Ptr := GlobalLock(hDIBs);

       // Place bitmap info in top of data block.
       CopyMemory(BitsT.Ptr, FormSource.pBmpInfo, HeaderSize);

       if (BitsS.Ptr <> Nil) and
          (BitsT.Ptr <> Nil)
       then begin
            StartS.Long := 0;
            StartT.Long := HeaderSize;
            BlockSize   := FormSource.pBmpInfo.bmiHeader.biSizeImage;
            while (BlockSize > 0)
            do begin
               Count := BlockSize;
               if (Count > 1024)
               then Count := 1024;

               // Calculate source address.
               ToAddrS.Long := BitsS.Long + StartS.Long;

               // Calculate target address.
               ToAddrT.Long := BitsT.Long + StartT.Long;

               // Move image data from source to target.
               CopyMemory(ToAddrT.Ptr, ToAddrS.Ptr, Count);

               // Increment source & target offset.
               inc(StartS.Long, Count);
               inc(StartT.Long, Count);

               { Decrement remaining bytes to transfer.                        }
               dec(BlockSize, Count);
            end;
       end;
       GlobalUnlock(hDIBs);
  end
  else hDIBs := 0;
  GetImageHandle := hDIBs;
end; { End TTWAINSource.GetImageHandle.                                        }


{------------------------------------------------------------------------------}
{ FUNCTION: ImageMemXferMsg                                                    }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_BADVALUE                                                   }
{              TWCC_SEQERROR                                                   }
{          TWRC_XFERDONE                                                       }
{          TWRC_CANCEL                                                         }
{------------------------------------------------------------------------------}

function TTWAINSource.ImageMemXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC        : TW_UINT16;
    pMem        : pTW_IMAGEMEMXFER;
    MaxRows     : TW_UINT32;
    RowsWritten : TW_UINT32;

    pDib        : PBITMAPINFO;
    DibStart    : TLongType;
    DibToAddr   : TLongType;
    DibBits     : TLongType;
    NumColors   : longint;
    LongWidth   : longint;
begin
  twRC := TWRC_SUCCESS;
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('ImageMemXferMsg');
  {$ENDIF}

  if FXferDone
  then begin
       // This is required so that the last time the transaction is processed,
       // the thunker won't try to write more data than is present.  Twacker
       // currently calls ImageMemXfer one time after all data has been
       // transfered.
       pMem := pTW_IMAGEMEMXFER(pTWMsg^.pData);
       pMem^.BytesWritten := integer(0);

       // Reset static variables.
       FBytesOut  := 0;
       FXferDone  := False;
       FXferFirst := True;
       FXferCount := 0;
       {$IFDEF TWNDEBUG}
         FmcmTWAINLog.Str2File('XferDone returned');
       {$ENDIF}
       twRC := TWRC_XFERDONE;
  end;

  if (twRC = TWRC_SUCCESS)
  then begin
       inc(FXferCount);

       // Abort if gets caught in an infinite loop.  The image should be
       // salvagable. 1000 should allow a very large number of transfers before
       // quitting. If the image is broken on an uneven byte boundry, we may
       // get caught in a loop.
       if (FXferCount > 1000)
       then begin
            (*
            if (TimerOut = 999)
            then MessageBox(0, 'The source aborts a memory transfer',
                               'Sample Source', MB_OK);
            *)

            pMem := pTW_IMAGEMEMXFER(pTWMsg^.pData);
            pMem^.BytesWritten := longint(0);

            // Reset static variables.
            FBytesOut  := 0;
            FXferDone  := False;
            FXferFirst := True;
            FXferCount := 0;
            {$IFDEF TWNDEBUG}
              FmcmTWAINLog.Str2File('XferDone Timed Out');
            {$ENDIF}
            twRC      := TWRC_XFERDONE;
       end;
  end;

  if (twRC = TWRC_SUCCESS)
  then begin
       case pTWMsg^.MSG of
       MSG_GET : begin
                   // Check that size is within the sources limits
                   pMem := pTW_IMAGEMEMXFER(pTWMsg^.pData);
                   if (FormSource.ImageStream.Size = 0) or
                      (FBytesOut = 0) // Fix problem from re-using FlipImage
                   then FormSource.CopyBmpToStream;
                   if (FMinBufSize > pMem^.Memory.Length)
                   then begin
                        Result := TWRC_FAILURE;
                        exit;
                   end;

                   pDib := FormSource.pBmpInfo;
                   if (pDib <> Nil)
                   then begin
                        if (FBytesOut = 0)
                        then FlipBitmap(FormSource.pBmp,  FormSource.pBmpInfo);

                        with FormSource.pBmpInfo.bmiHeader
                        do begin
                           if (biBitCount <= 8)
                           then NumColors := (1 shl biBitCount)
                           else NumColors := 0;
                           LongWidth := (((biWidth * biBitCount) + 31) div 32) * 4;
                        end;

                        // increment the pointer to the bitmap to the correct
                        // offset within the data area DibPtr will then point
                        // to the start of the current block of data to be
                        // copied.
                        DibBits.Ptr   := pDib;

                        DibStart.Long := SizeOf(TBITMAPINFOHEADER);
                        DibStart.Long := DibStart.Long +
                                         NumColors * SizeOf(TRGBQUAD);
                        DibStart.Long := DibStart.Long + FBytesOut;
                        DibToAddr.Long := DibBits.Long + DibStart.Long;

                        {$IFDEF TWNDEBUG}
                          FmcmTWAINLog.Str2File('Incremented pointer');
                        {$ENDIF}

                        // Set the attributes of the xfer.
                        pMem^.Compression := word(TWCP_NONE);
                        pMem^.BytesPerRow := LongWidth;

                        // Need to check that at least one entire strip can be
                        // transferred within the bounds of the buffer size.
                        // If the buffer is smaller than one strip, a tile
                        // transfer will have to be executed. This code will
                        // fail since tile is not supported.
                        if (pMem^.BytesPerRow > pMem^.Memory.Length)
                        then begin
                             // Need to set condition code also.
                             MessageBox(0, 'Buffer is too small for strip transfers',
                                           '', MB_OK);
                             ImageMemXferMsg := TWRC_FAILURE;
                             exit;
                        end;

                        pMem^.Columns := pDib^.bmiHeader.biWidth;
                        pMem^.Rows    := pDib^.bmiHeader.biHeight;

                        pMem^.XOffset := 0;
                        if (pMem^.BytesPerRow > 0)
                        then pMem^.YOffset := FBytesOut div pMem^.BytesPerRow
                        else pMem^.YOffset := 0;

                        {$IFDEF TWNDEBUG}
                          FmcmTWAINLog.Str2File('Set structure Y:');
                          FmcmTWAINLog.Str2File(IntToStr(pMem^.YOffset));
                        {$ENDIF}

                        // Determine the max number of rows that can be passed
                        // this time thru the transfer must end on a strip (row)
                        // boundry.
                        MaxRows := pMem^.Memory.Length div pMem^.BytesPerRow;

                        // Determine the number of rows of the bitmap that have
                        // been written thus far
                        RowsWritten := FBytesOut div pMem^.BytesPerRow;

                        // If the bitmap or the remaining bitmap is smaller
                        // than the memory allocation.
                        if (MaxRows >= (pMem^.Rows - RowsWritten))
                        then pMem^.BytesWritten := pMem^.BytesPerRow *
                                                  (pMem^.Rows - RowsWritten)
                        else begin
                             // It will use the entire buffer area.
                             pMem^.BytesWritten := pMem^.BytesPerRow * MaxRows;
                        end;
                        // pMem^.Rows = number of rows to transfer per call.
                        pMem^.Rows := pMem^.BytesWritten div pMem^.BytesPerRow;

                        FBytesOut := FBytesOut + pMem^.BytesWritten;

                        // Determine the number of rows of the bitmap that have
                        // been written thus far
                        RowsWritten := FBytesOut div pMem^.BytesPerRow;

                        // if the bytes written total are in excess of the
                        // bitmap image size, we are done.
                        // if (BytesOut >= pDib^.bmiHeader.biSizeImage)
                        //     done = TRUE;
                        if ((pDib^.bmiHeader.biHeight{pMem^.Rows} - RowsWritten) = 0)
                        then begin
                             FXferDone := True;
                             twRC     := TWRC_XFERDONE;
                             {$IFDEF TWNDEBUG}
                               FmcmTWAINLog.Str2File('Memory buffer Success');
                             {$ENDIF}
                        end
                        else twRC := TWRC_SUCCESS;

                        // Copy memory to the shared pointer
                        // (owned by the application)
                        CopyMemory(pMem^.Memory.TheMem,
                                   DibToAddr.Ptr,
                                   pMem^.BytesWritten);
                        {$IFDEF TWNDEBUG}
                          FmcmTWAINLog.Str2File('Copied buffer memory');
                        {$ENDIF}
                   end
                   else begin // MCM, Added in case returned hIBmp = 0.
                        pMem^.BytesWritten     := longint(0);
                        FDSStatus.ConditionCode := TWCC_BADVALUE;
                        twRC := TWRC_FAILURE;
                   end;
                 end;
       else      begin
                   FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                   twRc := TWRC_FAILURE;
                 end;
       end;
  end;
  if (twRC = TWRC_XFERDONE)
  then begin
       FormSource.ClearImageBuffer;
       inc(ImageLayout.DocumentNumber);
       inc(ImageLayout.PageNumber);
       inc(ImageLayout.FrameNumber);
  end;
  Result := twRC;
end; { End TTWAINSource.ImageMemXferMsg.                                       }


{------------------------------------------------------------------------------}
{ FUNCTION: ImageFileXferMsg                                                   }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_DONE                                                           }
{          TWRC_FAILURE                                                        }
{              TWCC_BADVALUE                                                   }
{              TWCC_SEQERROR                                                   }
{          TWRC_XFERDONE                                                       }
{              TWCC_CANCEL                                                     }
{                                                                              }
{ NOTES:   Valid states: 6 -- 7.  I do not support file transfers.             }
{------------------------------------------------------------------------------}

function TTWAINSource.ImageFileXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
var twRC : TW_UINT16;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('ImageFileXferMsg');
  {$ENDIF}
  case pTWMsg^.MSG of
  MSG_GET : begin
              // it has already been determined that the file exists and
              // can be opened, it is safe to copy it
              FormSource.SaveToFile(FFileName);

              twRc := TWRC_XFERDONE;
            end;
  else      begin
              FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
              twRC := TWRC_FAILURE;
            end;
  end;
  if (twRC = TWRC_XFERDONE)
  then begin
       inc(ImageLayout.DocumentNumber);
       inc(ImageLayout.PageNumber);
       inc(ImageLayout.FrameNumber);
       FormSource.ImageStream.Clear;
  end;
  Result := twRC;
end; { End TTWAINSource.ImageFileXferMsg.                                      }


{------------------------------------------------------------------------------}
{ FUNCTION: ImageNativeXferMsg                                                 }
{                                                                              }
{ ARGS:    pTWMsg  message struct, same as list of parms in DS_ENTRY call      }
{                                                                              }
{ RETURNS: TWRC_SUCCESS                                                        }
{          TWRC_FAILURE                                                        }
{              TWCC_BADVALUE                                                   }
{              TWCC_SEQERROR                                                   }
{          TWRC_XFERDONE                                                       }
{              TWCC_CANCEL                                                     }
{              TWCC_BADPROTOCOL         // debug only                          }
{                                                                              }
{ NOTES:  Valid states: 6 -- 7.  I only support native transfers.              }
{------------------------------------------------------------------------------}

function TTWAINSource.ImageNativeXferMsg(pTWMsg : PTWMessage) : TW_UINT16;
type LongType  = record
                 case Word of
		 0  : (Ptr  : Pointer);
		 1  : (Long : Longint);
		 2  : (Lo   : Word;
		       Hi   : Word);
		 end;
     TAByte    = array[0..0] of byte;
var  twRC       : TW_UINT16;
     hBits      : THandle;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('ImageNativeXferMsg');
  {$ENDIF}
  if ((FDSState = STATE6) or (FDSState = STATE7))
  then begin
       case pTWMsg^.MSG of
       MSG_GET : begin // return hDib in low word.
                   hBits := GetImageHandle;
                   pTW_UINT32(pTWMsg^.pData)^ := hBits;
                   if (hBits = 0)
                   then twRc := TWRC_FAILURE
                   else twRc := TWRC_XFERDONE;
                 end;
       else      begin
                   FDSStatus.ConditionCode := TWCC_BADPROTOCOL;
                   twRC := TWRC_FAILURE;
                 end;
       end;
  end
  else begin
       FDSStatus.ConditionCode := TWCC_SEQERROR;
       twRC := TWRC_FAILURE;
  end;
  if (twRC = TWRC_XFERDONE)
  then begin
       inc(ImageLayout.DocumentNumber);
       inc(ImageLayout.PageNumber);
       inc(ImageLayout.FrameNumber);
       FormSource.ImageStream.Clear;
  end;
  Result := twRC;
end; { End TTWAINSource.ImageNativeXferMsg.                                    }


procedure TTWAINSource.OnDeviceEvent(Sender       : TObject;
                                     pDeviceEvent : pTW_DEVICEEVENT);
var hDSMDLL      : THandle;
    lpDSM_Entry  : TDSM_Entry_Proc; // Entry point to the SM.
    pNewEvent    : pTW_DEVICEEVENT;
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('OnDeviceEvent');
  {$ENDIF}

  {$IFDEF WIN32}
    hDSMDLL := LoadLibrary('TWAIN_32.DLL');
    if (hDSMDLL <> 0)
  {$ELSE} // 16 bit Windows
    hDSMDLL := LoadLibrary('TWAIN.DLL');
    if (hDSMDLL <> 0)
  {$ENDIF}
   then begin
        @lpDSM_Entry := GetProcAddress(hDSMDLL, 'DSM_Entry');
        if (@lpDSM_Entry <> Nil)
        then begin
             if (FDeviceEventList.Count < MaxDeviceEvents)
             then begin
                  StrLCopy(pDeviceEvent^.DeviceName, FdsIdentity.ProductName, 255);
                  GetMem(pNewEvent, SizeOf(TW_DEVICEEVENT));
                  CopyMemory(pNewEvent, pDeviceEvent, SizeOf(TW_DEVICEEVENT));
                  FDeviceEventList.Add(pNewEvent);
                  lpDSM_Entry(@FdsIdentity,
                              @FappIdentity,
                              TW_UINT32(DG_CONTROL),
                              DAT_NULL,
                              MSG_DEVICEEVENT,
                              TW_MEMREF(Nil));
             end
             else begin
                  FEventOverflow := True;
             end;
        end;
        FreeLibrary(hDSMDLL);
   end;
end; { End TTWAINSource.OnDeviceEvent.                                         }


{------------------------------------------------------------------------------}
{ FUNCTION: OnNotifyCloseDSReq                                                 }
{                                                                              }
{ ARGS:    none                                                                }
{                                                                              }
{ RETURNS: Indirectly returns a MSG_CLOSEDSREQ to the Application, thru        }
{          DSM_Entry call.                                                     }
{------------------------------------------------------------------------------}

procedure TTWAINSource.OnNotifyCloseDSReq(Sender : TObject);
var hDSMDLL     : THandle;
    lpDSM_Entry : TDSM_Entry_Proc; // Entry point to the SM.
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('OnNotifyCloseDSReq');
  {$ENDIF}

  {$IFDEF WIN32}
    hDSMDLL := LoadLibrary('TWAIN_32.DLL');
    if (hDSMDLL <> 0)
  {$ELSE} // 16 bit Windows
    hDSMDLL := LoadLibrary('TWAIN.DLL');
    if (hDSMDLL <> 0)
  {$ENDIF}
   then begin
        @lpDSM_Entry := GetProcAddress(hDSMDLL, 'DSM_Entry');
        if (@lpDSM_Entry <> Nil)
        then begin
             lpDSM_Entry(@FdsIdentity,
                         @FappIdentity,
                         TW_UINT32(DG_CONTROL),
                         DAT_NULL,
                         MSG_CLOSEDSREQ,
                         TW_MEMREF(Nil));
        end;
        FreeLibrary(hDSMDLL);
   end;
end; { End TTWAINSource.OnNotifyCloseReq.                                      }


//------------------------------------------------------------------------------
// OnNotifyXferReady
//
// Return: Indirectly returns a MSG_XFERREADY to the Application, thrugh
//         DSM_Entry call.
//------------------------------------------------------------------------------

procedure TTWAINSource.OnNotifyXferReady(Sender : TObject);
var hDSMDLL     : THandle;
    lpDSM_Entry : TDSM_Entry_Proc; // Entry point to the SM.
begin
  {$IFDEF TWNDEBUG}
    FmcmTWAINLog.Str2File('OnNotifyXferReady');
  {$ENDIF}

  // Go to xferready state.
  FDSState := STATE6;

  {$IFDEF WIN32}
    hDSMDLL := LoadLibrary('TWAIN_32.DLL');
    if (hDSMDLL <> 0)
  {$ELSE} // 16 bit Windows
    hDSMDLL := LoadLibrary('TWAIN.DLL');
    if (hDSMDLL <> 0)
  {$ENDIF}
   then begin
        @lpDSM_Entry := GetProcAddress(hDSMDLL, 'DSM_Entry');
        if (@lpDSM_Entry <> Nil)
        then begin
             lpDSM_Entry(@FdsIdentity,
                         @FappIdentity,
                         TW_UINT32(DG_CONTROL),
                         DAT_NULL,
                         MSG_XFERREADY,
                         TW_MEMREF(Nil));
        end;
        FreeLibrary(hDSMDLL);
   end;
end; { End TTWAINSource.OnNotifyXferReady.                                     }


{------------------------------------------------------------------------------}
{ FlipBitMap - Takes a memory transfer buffer and changes it to a DIB format   }
{                                                                              }
{    i.e.    Memory Format                                                     }
{                         1  2  3  4  5                                        }
{                         6  7  8  9 10                                        }
{                        11 12 13 14 15                                        }
{                                                                              }
{            DIB bitmap Format                                                 }
{                        11 12 13 14 15                                        }
{                         6  7  8  9 10                                        }
{                         1  2  3  4  5                                        }
{                                                                              }
{    Memory RGBQuad order: RGB                                                 }
{    Windows DIB RGBQuad order: BGR                                            }
{                                                                              }
{ MCM 210996, This procedure was re-written in order to use less memory while  }
{ flipping the bitmap.                                                         }
{                                                                              }
{------------------------------------------------------------------------------}

procedure TTWAINSource.FlipBitmap(pBmp     : Pointer;
                                  pBmpInfo : PBitmapInfo);
var hBmpT       : THandle;
    pBmpT       : PVectorB;
    hBmpB       : THandle;
    pBmpB       : PVectorB;

    Height      : longint;
    BitCount    : word;
    NumColors   : integer;
    Offset      : longint;
    pixels      : TW_UINT16;
    items       : TW_INT32;
    i, j        : TW_UINT32;
    SaveRed     : byte;

    // Variables needed to handle large bitmaps.
    LongWidth   : longint;
    StartT      : TLongType;
    ToAddrT     : TLongType;
    BitsT       : TLongType;

    StartB      : TLongType;
    ToAddrB     : TLongType;
    BitsB       : TLongType;
begin
  Height    := pBmpInfo^.bmiHeader.biHeight;
  BitCount  := pBmpInfo^.bmiHeader.biBitCount;
  LongWidth := (((pBmpInfo^.bmiHeader.biWidth * bitCount) + 31) div 32) * 4;

  hBmpT := GlobalAlloc(GHND, LongWidth);
  hBmpB := GlobalAlloc(GHND, LongWidth);
  if (hBmpT <> 0) and
     (hBmpB <> 0)
  then begin
       pBmpT     := GlobalLock(hBmpT);
       pBmpB     := GlobalLock(hBmpB);

       BitsT.Ptr := pBmpInfo;
       BitsB.Ptr := pBmpInfo;

       if (pBmpInfo^.bmiHeader.biBitCount <= 8)
       then NumColors := (1 shl pBmpInfo^.bmiHeader.biBitCount)
       else NumColors := 0;

       // Calculate Offset to start of the bitmap data.
       Offset    := SizeOf(TBitmapInfoHeader);
       Offset    := Offset + NumColors * SizeOf(TRGBQUAD);

       StartT.Long := Offset;
       StartB.Long := Offset + (LongWidth * (Height - 1));

       // For each line.
       for i := 0 to ((Height - 1) div 2)
       do begin
          ToAddrT.Long := BitsT.Long + StartT.Long;
          ToAddrB.Long := BitsB.Long + StartB.Long;

          CopyMemory(pBmpT, ToAddrT.Ptr, LongWidth);
          CopyMemory(pBmpB, ToAddrB.Ptr, LongWidth);

          CopyMemory(ToAddrB.Ptr, pBmpT, LongWidth);
          CopyMemory(ToAddrT.Ptr, pBmpB, LongWidth);

          StartT.Long := StartT.Long + LongWidth;
          StartB.Long := StartB.Long - LongWidth;
       end;

       // Flip RGB color table.
       if (BitCount = 24)
       then begin
            Pixels      := TW_UINT16(pBmpInfo^.bmiHeader.biWidth);
            StartT.Long := Offset;
            for items := 0 to (Height - 1)
            do begin
               ToAddrT.Long := BitsT.Long + StartT.Long;
               CopyMemory(pBmpT, ToAddrT.Ptr, LongWidth);
               for i := 0 to (Pixels - 1)
               do begin
                  // Switch Red byte and Blue byte.
                  j := i * 3;
                  {$IFOPT R+} {$DEFINE RANGE_CHECKING} {$R-} {$ENDIF}
                  SaveRed     := pBmpT^[j];
                  pBmpT^[j]   := pBmpT^[j+2];
                  pBmpT^[j+2] := SaveRed;
                 {$IFDEF RANGE_CHECKING} {$R+} {$UNDEF RANGE_CHECKING} {$ENDIF}
               end;
               CopyMemory(ToAddrT.Ptr, pBmpT, LongWidth);
               StartT.Long := StartT.Long + LongWidth;
          end;
       end;

       // Unlock memory.
       GlobalUnlock(hBmpT);
       GlobalUnlock(hBmpB);
  end;

  // Free memory allocated for bitmap.
  if (hBmpT <> 0)
  then GlobalFree(hBmpT);
  if (hBmpB <> 0)
  then GlobalFree(hBmpB);
end; { End TTWAINSource.FlipBitmap.                                            }


procedure TTWAINSource.CreateContainers;
var i             : integer;
    Cap           : TW_UINT16;
    SupportedCaps : TTwnsContainer;
    ExtendedCaps  : TTwnsContainer;
begin
  // Get capabilities supported by the source.
  SupportedCaps := TTwnsContainer(FContainerList.CreateItem(CAP_SUPPORTEDCAPS));
  SupportedCaps.ItemType := TWTY_UINT16;
  SupportedCaps.ContainerType := TWON_ARRAY;
  SupportedCaps.QuerySupport := TWQC_GET;
  FormSource.GetSupportedCaps(SupportedCaps);

  // Create containers for each capability.
  for i := 0 to (SupportedCaps.NumItems - 1)
  do begin
     Cap := SupportedCaps.Items[i];
     FContainerList.CreateItem(Cap);
  end;

  // Initiate capability.
  for i := 0 to (SupportedCaps.NumItems - 1)
  do begin
     Cap := SupportedCaps.Items[i];
     FormSource.ResetCapability(FContainerList, Cap);
  end;

  // Get capabilities that are negotiable when the user interface is enabled.
  ExtendedCaps := TTwnsContainer(FContainerList.CreateItem(CAP_EXTENDEDCAPS));
  ExtendedCaps.ItemType := TWTY_UINT16;
  ExtendedCaps.ContainerType := TWON_ARRAY;
  ExtendedCaps.QuerySupport := TWQC_GET;
  FormSource.GetExtendedCaps(ExtendedCaps);
end; { End TTWAINSource.CreateContainers.                                      }

end.

