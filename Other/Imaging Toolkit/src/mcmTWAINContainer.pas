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
// $Log:  15888: mcmTWAINContainer.pas 
//
//    Rev 1.10    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.9    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.8    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.7    2013-11-26 18:38:46  mcm
// Fixed casting String to AnsiString in TTwnContainer.SetValue
//
//    Rev 1.6    25-10-2010 22:17:56  mcm
//
//    Rev 1.5    26-08-2009 22:39:50  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.4    06-07-2003 10:58:42  mcm    Version: DT 2.5
// Added compiler conditions.

//
//   Rev 1.3    15-04-2003 10:56:32  mcm    Version: DT 2.3
// Changed GetMaxValue and GetMinValue to support enimeration containers. 

//
//   Rev 1.2    06-03-2003 11:04:32  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.1    19-06-2002 11:23:26  mcm
// Modified SetItemType so that the input Value is disregarded when it's equal
// to 99. 
// This allows undefined (in the TWAIN standard) to be used.

//
//   Rev 1.0    04-12-2001 16:49:06  mcm    Version: DT 2.0

unit mcmTWAINContainer;

{$INCLUDE mcmDefines.pas}

interface

uses
     {$IFDEF GE_DXE4}
     WinApi.Windows, System.Classes, System.SysUtils,
     System.AnsiStrings,
     {$ELSE}
     {$IFDEF GE_DXE2}
     WinApi.Windows, System.Classes, System.SysUtils,
     {$ELSE}
     Windows, Classes, SysUtils,
     {$ENDIF}
     {$ENDIF}
     twain;

type
//------------------------------------------------------------------------------
// TTwnFrame.
//------------------------------------------------------------------------------

  TTwnFrame = class(TComponent)
  private
    FFrame  : pTW_FRAME;
  protected
    function  GetLeft : double;
    function  GetTop : double;
    function  GetRight : double;
    function  GetBottom : double;
    procedure SetLeft  (Value : double);
    procedure SetTop   (Value : double);
    procedure SetRight (Value : double);
    procedure SetBottom(Value : double);
  public
    constructor Create(AOwner: TComponent; NewFrame : pTW_FRAME); {$IFNDEF DCB3} reintroduce; {$ELSE} virtual; {$ENDIF}
    property Left : double
      read   GetLeft
      write  SetLeft;
    property Top : double
      read   GetTop
      write  SetTop;
    property Right : double
      read   GetRight
      write  SetRight;
    property Bottom : double
      read   GetBottom
      write  SetBottom;
  published
  end;

//------------------------------------------------------------------------------
// TTwnContainer.
//------------------------------------------------------------------------------

  TTwnContainer = class(TComponent)
  private
    { Private declarations }
    FCap         : word;
    FConType     : TW_UINT16;
    FItemType    : TW_UINT16;
    pArray       : pTW_Array;
    pEnumeration : pTW_Enumeration;
    pOneValue    : pTW_OneValue;
    pRange       : pTW_Range;
    FFrameList   : TList;

    procedure   ExtractOneValue(pData : pTW_CAPABILITY; pVoid : pointer);
    function    GetCapDataType(Cap : TW_UINT16) : TW_UINT16;
    procedure   SetCapability(Value : word);
    procedure   SetConType(Value : word);
    procedure   SetItemType(Value : word);
    function    GetItemSize(ItemType : TW_UINT16) : integer;
    function    GetItemListIndexFrom(Value : Variant) : integer;
    function    GetCurrentIndex : smallint;
    procedure   SetCurrentIndex(Value : smallint);
    function    GetDefaultIndex : smallint;
    procedure   SetDefaultIndex(Value : smallint);
    function    GetCurrentValue : variant;
    procedure   SetCurrentValue(Value : variant);
    function    GetDefaultValue : variant;
    procedure   SetDefaultValue(Value : variant);
    function    GetMaxFromList(ItemList : PAnsiChar; NumItems : smallint; ItemType : TW_UINT32) : Variant;
    function    GetMinFromList(ItemList : PAnsiChar; NumItems : smallint; ItemType : TW_UINT32) : Variant;
    function    GetMaxValue : Variant;
    procedure   SetMaxValue(Value : Variant);
    function    GetMinValue : Variant;
    procedure   SetMinValue(Value : Variant);
    function    GetNumItems : smallint;
    procedure   SetNumItems(Value : smallint);
    function    GetFrame(Index : smallint) : TTwnFrame;
    function    GetItem(Index : smallint) : variant;
    procedure   SetItem(Index : smallint; Value : Variant);
    function    GetQuerySupport : integer;
    procedure   SetupFrames(Count : integer);
  protected
    { Protected declarations }
    FQuerySupport : TW_INT32;
    procedure   AssignContainerPtr(Cap : word; ConType : word; pContainer : pointer); virtual;
    function    GetContainerPtr(AsOneValue : bool) : pointer;
    function    GetValue(pValue : pointer; ItemType : TW_UINT16) : Variant;
    procedure   SetValue(Value : Variant; pValue : pointer; ItemType  : TW_UINT16);
    function    GetStepValue : Variant;
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   AddFrame(Left, Top, Right, Bottom : double);
    procedure   AddItem(Value : Variant);
    procedure   Assign(Source : TPersistent); override;
    procedure   Clear;
    function    DeleteFrame(Index : integer) : smallint;
    function    DeleteItem(Value : Variant) : smallint;
    property    Capability : word
      read      FCap
      write     SetCapability;
    property    ContainerType : word
      read      FConType
      write     SetConType;
    property    CurrentIndex : smallint
      read      GetCurrentIndex
      write     SetCurrentIndex;
    property    CurrentValue : variant
      read      GetCurrentValue
      write     SetCurrentValue;
    property    DefaultIndex : smallint
      read      GetDefaultIndex
      write     SetDefaultIndex;
    property    DefaultValue : variant
      read      GetDefaultValue
      write     SetDefaultValue;
    property    Frames[index : smallint] : TTwnFrame
      read      GetFrame;
    property    Items[index : smallint] : variant
      read      GetItem
      write     SetItem;
    property    ItemType : word
      read      FItemType
      write     SetItemType;
    property    MaxValue : variant
      read      GetMaxValue
      write     SetMaxValue;
    property    MinValue : variant
      read      GetMinValue
      write     SetMinValue;
    property    NumItems : smallint
      read      GetNumItems
      write     SetNumItems;
    property    QuerySupport : integer
      read      GetQuerySupport;
    property    StepValue : variant
      read      GetStepValue;
     { write     SetStepValue; Applications may not change this member ! }
  published
    { published declarations }
  end;

//------------------------------------------------------------------------------
// TTwnContainerSrc.
//------------------------------------------------------------------------------

{$IFDEF DATASOURCE}
  TTwnContainerSrc = class(TTwnContainer)
  private
    procedure   SetStepValue(Value : Variant);
    function    GetStepValue : Variant;
  protected
  public
    procedure   AssignContainerPtr(Cap : word; ConType : word; pContainer : pointer); override;
    function    GetContainerPtr(AsOneValue : bool) : pointer;
    property    QuerySupport : TW_INT32
      read      FQuerySupport
      write     FQuerySupport;
    property    StepValue : variant
      read      GetStepValue
      write     SetStepValue;
  end;
{$ENDIF}

//------------------------------------------------------------------------------
// TTwnContainerList.
//------------------------------------------------------------------------------
type
  TTwnContainerList = class(TComponent)
  private
    { Private declarations   }
    FList : TList;
    function    GetItem(Index : word) : TTwnContainer;
    function    GetCount : integer;
  protected
    { Protected declarations }
    procedure   AddItem(Cap : word; Value : TtwnContainer);
  public
    { public declarations    }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear;
    function    CreateItem(Cap : word) : TtwnContainer;
    procedure   DeleteItem(Cap : word);
    property    Count : integer
      read      GetCount;
    property    Items[index : word] : TTwnContainer
      read      GetItem;
  published
    { published declarations }
  end;

implementation

uses {$IFNDEF DATASOURCE}
       mcmTWAINIntf,
     {$ENDIF}
     mcmTWAINFix;

{$IFNDEF DATASOURCE}
type
  TTypeCastTWAINIntf = class(TmcmTWAINIntf);
{$ENDIF}

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

//------------------------------------------------------------------------------
// TTwnFrame.
//------------------------------------------------------------------------------

constructor TTwnFrame.Create(AOwner : TComponent; NewFrame : pTW_FRAME);
begin
  Inherited Create(AOwner);
  FFrame := NewFrame;
end; { End TTwnFrame.Create.                                                   }


function TTwnFrame.GetLeft : double;
begin
  Result := FIX32ToFloat(FFrame^.Left);
end; { End TTwnFrame.GetLeft.                                                  }


function TTwnFrame.GetTop : double;
begin
  Result := FIX32ToFloat(FFrame^.Top);
end; { End TTwnFrame.GetTop.                                                   }


function TTwnFrame.GetRight : double;
begin
  Result := FIX32ToFloat(FFrame^.Right);
end; { End TTwnFrame.GetRight.                                                 }


function TTwnFrame.GetBottom : double;
begin
  Result := FIX32ToFloat(FFrame^.Bottom);
end; { End TTwnFrame.GetBottom.                                                }


procedure TTwnFrame.SetLeft(Value : double);
begin
  FFrame^.Left := FloatToFIX32(Value);
end; { End TTwnFrame.SetLeft.                                                  }


procedure TTwnFrame.SetTop(Value : double);
begin
  FFrame^.Top := FloatToFIX32(Value);
end; { End TTwnFrame.SetTop.                                                   }


procedure TTwnFrame.SetRight(Value : double);
begin
  FFrame^.Right := FloatToFIX32(Value);
end; { End TTwnFrame.SetRight.                                                 }


procedure TTwnFrame.SetBottom(Value : double);
begin
  FFrame^.Bottom := FloatToFIX32(Value);
end; { End TTwnFrame.SetBottom.                                                }


//------------------------------------------------------------------------------
// TTwnContainer.
//------------------------------------------------------------------------------

constructor TTwnContainer.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FCap      := 0;
  FConType  := TWON_ONEVALUE;
  FItemType := TWTY_UINT16;
  FQuerySupport := integer($FFFFFFFF); // TWQC_ALL;

  // Set all container types to Nil (not allocated).
  pArray       := Nil;
  pEnumeration := Nil;
  pOneValue    := Nil;
  pRange       := Nil;

  // Create a list for Frames (Only used when Cap = ICAP_FRAMES).
  FFrameList   := TList.Create;

  // Default to - and allocate memory for - a OneValue container.
  GetMem(pOneValue, SizeOf(TW_ONEVALUE) + SizeOf(TW_STR255));
  if Assigned(pOneValue)
  then begin
       pOneValue.ItemType := FItemType;
       pOneValue.Item     := 0;
  end;
end; { End TTwnContainer.Create.                                               }


destructor TTwnContainer.Destroy;
begin
  Clear;
  if Assigned(pOneValue)
  then FreeMem(pOneValue);
  pOneValue := Nil;
  if Assigned(FFrameList)
  then FFrameList.Free;
  FFrameList := Nil;

  Inherited Destroy;
end; { End TTwnContainer.Destroy.                                              }


procedure TTwnContainer.SetupFrames(Count : integer);
// SetupFrames will either add of delete TTwnFrame's from FrameList.
var i        : integer;
    NewFrame : TTwnFrame;
begin
  if (Count < 0)
  then Count := 0;
  if (FCAP = ICAP_FRAMES)
  then begin
       if (Count < FFrameList.Count)
       then begin //
            if (FFrameList.Count > 0)
            then begin
                 for i := (FFrameList.Count - 1) downto Count
                 do begin
                    try
                      if Assigned(FFrameList.Items[i])
                      then TTwnFrame(FFrameList.Items[i]).Free;
                      FFrameList.Items[i] := Nil;
                      FFrameList.Delete(i);
                    except
                      FFrameList.Items[i] := Nil;
                    end;
                 end;
            end;
       end
       else begin
            for i := FFrameList.Count to (Count - 1)
            do begin
               NewFrame := Nil;
               case FConType of
               TWON_ENUMERATION : begin
                                    if Assigned(pEnumeration)
                                    then NewFrame := TTwnFrame.Create(Self, pTW_Frame(@pEnumeration^.ItemList[4*i]));
                                  end;
               TWON_ONEVALUE    : begin
                                    if Assigned(pOneValue)
                                    then NewFrame := TTwnFrame.Create(Self, pTW_Frame(@pOneValue^.Item));
                                  end;
               end;
               if Assigned(NewFrame)
               then FFrameList.Add(NewFrame);
            end;
       end;
  end;
end; { End TTwnContainer.SetupFrames.                                          }


procedure TTwnContainer.Assign(Source : TPersistent);
var ItemSize  : integer;
    ItemSizes : integer;
    Src       : TTwnContainer;
begin
  Clear;
  Inherited Assign(Source);
  if (Source is TTwnContainer)
  then begin
       Src       := (Source as TTwnContainer);
       FCap      := Src.FCap;
       FConType  := Src.FConType;
       FItemType := Src.FItemType;
       ItemSize  := GetItemSize(FItemType);

       // Create and copy Array container.
       if Assigned(Src.pArray)
       then begin
            ItemSizes := SizeOf(TW_ARRAY) + ItemSize * Src.pArray^.NumItems;
            GetMem(pArray, ItemSizes);
            if Assigned(pArray)
            then CopyMemory(pArray, Src.pArray, ItemSizes);
       end;

       // Create and copy Enumeration container.
       if Assigned(Src.pEnumeration)
       then begin
            ItemSizes := SizeOf(TW_ENUMERATION) + ItemSize * Src.pEnumeration^.NumItems;
            GetMem(pEnumeration, ItemSizes);
            if Assigned(pEnumeration)
            then CopyMemory(pEnumeration, Src.pEnumeration, ItemSizes);
       end;

       // Create and copy OneValue container.
       if Assigned(Src.pOneValue)
       then begin
            ItemSize := ItemSize - SizeOf(TW_UINT32);
            if Not(Assigned(pOneValue))
            then GetMem(pOneValue, SizeOf(TW_ONEVALUE) + ItemSize);
            if Assigned(pOneValue)
            then CopyMemory(pOneValue, Src.pOneValue, SizeOf(TW_ONEVALUE) + ItemSize);
       end;

       // Create and copy Range container.
       if Assigned(Src.pRange)
       then begin
            GetMem(pRange, SizeOf(TW_RANGE));
            if Assigned(pRange)
            then CopyMemory(pRange, Src.pRange, SizeOf(TW_RANGE));
       end;

       // Create and copy frames.
       if (FCap = ICAP_FRAMES)
       then SetupFrames(NumItems);
  end;
end; { End TTwnContainer.Assign.                                               }


procedure TTwnContainer.Clear;
var i : integer;
begin
  // Clear and free array container.
  if Assigned(pArray)
  then FreeMem(pArray);
  pArray := Nil;

  // Clear and free enumeration container.
  if Assigned(pEnumeration)
  then FreeMem(pEnumeration);
  pEnumeration := Nil;

  // Clear and free range container.
  if Assigned(pRange)
  then FreeMem(pRange);
  pRange := Nil;

  // Clear frame list.
  for i := (FFrameList.Count - 1) downto 0
  do begin
     try
       if Assigned(FFrameList.Items[i])
       then TTwnFrame(FFrameList.Items[i]).Free;
       FFrameList.Items[i] := Nil;
       FFrameList.Delete(i);
     except
       FFrameList.Items[i] := Nil;
     end;
  end;
  FFrameList.Clear;

  // Sustain OneValue container, but re-set to "zero".
  if Assigned(pOneValue)
  then begin
       pOneValue.ItemType := FItemType;
       case FItemType of
       TWTY_STR32,
       TWTY_STR64,
       TWTY_STR128,
       TWTY_STR255,
       TWTY_FRAME  : FillChar(pOneValue.Item, TWItemSize[FItemType]-1, #0);
       else pOneValue.Item := -1;
       end;
  end;
end; { End TTwnContainer.Clear.                                                }


function TTwnContainer.GetItemSize(ItemType : TW_UINT16) : integer;
var TmpSize : integer;
begin
  TmpSize := TWItemSize[ItemType];
  if (TmpSize < SizeOf(TW_UINT32))
  then TmpSize := SizeOf(TW_UINT32);
  Result := TmpSize;
end; { End TTwnContainer.GetItemSize.                                          }


function TTwnContainer.GetCapDataType(Cap : TW_UINT16) : TW_UINT16;
//------------------------------------------------------------------------------
// GetCapDataType - Returns the data type of the specific cap based on the
// TWAIN spec.  This could be done from the twacker.ini file.
//------------------------------------------------------------------------------
begin
  // Neither CAP_CUSTOMBASE nor CAP_SUPPORTEDCAPSEXT is described in the TWAIN
  // manual, and is therefore left out intensionally.

  // mcm 11.07.1999, expanded to include all TWAIN ver 1.8 capablities.
  // mcm 20.05.2000, expanded to include all TWAIN ver 1.9 capablities.
  case Cap of
  // TWTY_BOOL
  CAP_AUTOFEED,
  CAP_AUTOSCAN,
  CAP_CAMERAPREVIEWUI,
  CAP_CLEARPAGE,
  CAP_CUSTOMDSDATA,
  CAP_DEVICEONLINE,
  CAP_DUPLEXENABLED,
  CAP_ENABLEDSUIONLY,
  CAP_FEEDERENABLED,
  CAP_FEEDERLOADED,
  CAP_FEEDPAGE,
  CAP_INDICATORS,
  CAP_PAPERDETECTABLE,
  CAP_PRINTERENABLED,
  CAP_REACQUIREALLOWED, // CHECK
  CAP_REWINDPAGE,
  CAP_THUMBNAILSENABLED,
  CAP_UICONTROLLABLE,
  ICAP_AUTOBRIGHT,
  ICAP_AUTOMATICBORDERDETECTION,
  ICAP_AUTOMATICDESKEW,
  ICAP_AUTOMATICROTATE,
  ICAP_BARCODEDETECTIONENABLED,
  ICAP_EXTIMAGEINFO,
  ICAP_FLASHUSED,
  ICAP_LAMPSTATE,
  ICAP_PATCHCODEDETECTIONENABLED,
  ICAP_TILES,
  ICAP_UNDEFINEDIMAGESIZE : Result := TWTY_BOOL;

  // TWTY_UINT8
  ICAP_CUSTHALFTONE       : Result := TWTY_UINT8;

  // TWTY_INT8

  // TWTY_UINT16
  ACAP_AUDIOFILEFORMAT,
  ACAP_XFERMECH,
  CAP_ALARMS,
  CAP_CLEARBUFFERS,
  CAP_DEVICEEVENT,
  CAP_DUPLEX,
  CAP_EXTENDEDCAPS,
  CAP_FEEDERALIGNMENT,
  CAP_FEEDERORDER,
  CAP_JOBCONTROL,
  CAP_LANGUAGE,
  CAP_POWERSUPPLY,
  CAP_PRINTER,
  CAP_PRINTERMODE,
  CAP_SUPPORTEDCAPS,
  ICAP_BARCODESEARCHMODE,
  ICAP_BARCODESEARCHPRIORITIES,
  ICAP_BITDEPTH,
  ICAP_BITDEPTHREDUCTION,
  ICAP_BITORDER,
  ICAP_BITORDERCODES,
  ICAP_CCITTKFACTOR,
  ICAP_COMPRESSION,
  ICAP_FILTER,
  ICAP_FLASHUSED2,
  ICAP_FLIPROTATION,
  ICAP_IMAGEFILEFORMAT,
  ICAP_IMAGEFILTER,
  ICAP_JPEGPIXELTYPE,
  ICAP_LIGHTPATH,
  ICAP_LIGHTSOURCE,
  ICAP_MAXFRAMES,
  ICAP_NOISEFILTER,
  ICAP_ORIENTATION,
  ICAP_OVERSCAN,
  ICAP_PATCHCODESEARCHMODE,
  ICAP_PATCHCODESEARCHPRIORITIES,
  ICAP_PIXELFLAVOR,
  ICAP_PIXELFLAVORCODES,
  ICAP_PIXELTYPE,
  ICAP_PLANARCHUNKY,
  ICAP_SUPPORTEDBARCODETYPES,
  ICAP_SUPPORTEDPATCHCODETYPES,
  ICAP_SUPPORTEDSIZES,
  ICAP_TIMEFILL,
  ICAP_UNITS,
  ICAP_XFERMECH           : Result := TWTY_UINT16;

  // TWTY_INT16
  CAP_BATTERYPERCENTAGE,
  CAP_XFERCOUNT,
  ICAP_JPEGQUALITY,
  ICAP_ZOOMFACTOR         : Result := TWTY_INT16;

  // TWTY_UINT32
  CAP_ENDORSER,
  CAP_MAXBATCHBUFFERS,
  CAP_PRINTERINDEX,
  ICAP_BARCODEMAXRETRIES,
  ICAP_BARCODEMAXSEARCHPRIORITIES,
  ICAP_BARCODETIMEOUT,
  ICAP_IMAGEDATASET,
  ICAP_PATCHCODEMAXRETRIES,
  ICAP_PATCHCODEMAXSEARCHPRIORITIES,
  ICAP_PATCHCODETIMEOUT   : Result := TWTY_UINT32;

  // TWTY_INT32
  CAP_ALARMVOLUME,
  CAP_AUTOMATICCAPTURE,
  CAP_BATTERYMINUTES,
  // CAP_POWERDOWNTIME, // CAP_POWERSAVETIME ???
  CAP_TIMEBEFOREFIRSTCAPTURE,
  CAP_TIMEBETWEENCAPTURES : Result := TWTY_INT32;

  // TWTY_FIX32
  ICAP_BRIGHTNESS,
  ICAP_CONTRAST,
  ICAP_EXPOSURETIME,
  ICAP_GAMMA,
  ICAP_HIGHLIGHT,
  ICAP_MINIMUMHEIGHT,
  ICAP_MINIMUMWIDTH,
  ICAP_PHYSICALHEIGHT,
  ICAP_PHYSICALWIDTH,
  ICAP_ROTATION,
  ICAP_SHADOW,
  ICAP_THRESHOLD,
  ICAP_XNATIVERESOLUTION,
  ICAP_XRESOLUTION,
  ICAP_XSCALING,
  ICAP_YNATIVERESOLUTION,
  ICAP_YRESOLUTION,
  ICAP_YSCALING           : Result := TWTY_FIX32;

  // TWTY_STR32
  CAP_DEVICETIMEDATE,
  CAP_TIMEDATE,
  ICAP_HALFTONES          : Result := TWTY_STR32;

  // TWTY_STR128
  CAP_AUTHOR              : Result := TWTY_STR128;

  // TWTY_STR255
  CAP_CAPTION,
  CAP_PRINTERSTRING,
  CAP_PRINTERSUFFIX,
  CAP_SERIALNUMBER        : Result := TWTY_STR255;

  // TWTY_FRAME
  ICAP_FRAMES             : Result := TWTY_FRAME;

  else                      Result := 99;
  end;
end; { End TTwnContainer.GetCapDataType.                                       }


procedure TTwnContainer.SetCapability(Value : word);
begin
  if (FCap <> Value)
  then Clear;
  FCap := Value;
  // Automatically assign data type (TWTY_xxx) based on assigned Capability.
  SetItemType(GetCapDataType(FCap));
end; { End TTwnContainer.SetCapability.                                        }


procedure TTwnContainer.SetConType(Value : word);
begin
  if (FConType <> Value)
  then Clear;
  FConType := Value;
  SetNumItems(10);
end; { End TTwnContainer.SetConType.                                           }


procedure TTwnContainer.SetItemType(Value : word);
begin
  if (Value <> 99)
  then begin
       FItemType := Value;

       // OneValue container.
       if Assigned(pOneValue)
       then pOneValue^.ItemType := Value;

       // Array container.
       if Assigned(pArray)
       then pArray^.ItemType := Value;

       // Enumeration container.
       if Assigned(pEnumeration)
       then pEnumeration^.ItemType := Value;

       // Range container.
       if Assigned(pRange)
       then pRange^.ItemType := Value;
  end;
end; { End TTwnContainer.SetItemType.                                          }


procedure TTwnContainer.AssignContainerPtr(Cap        : word;
                                           ConType    : word;
                                           pContainer : pointer);
// The pointer (pContainer) must NEVER be freed outside this object after
// assignment.
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  if (pContainer <> Nil)
  then begin
       Clear;
       FCap := Cap;
       FConType := ConType;
       case FConType of
       TWON_ARRAY       : begin
                            pArray := pContainer;
                            pOneValue^.ItemType := pArray^.ItemType;
                            pOneValue^.Item     := 0;
                          end;
       TWON_ENUMERATION : begin
                            pEnumeration := pContainer;
                            pOneValue^.ItemType := pEnumeration^.ItemType;
                            ItemSize  := GetItemSize(pEnumeration^.ItemType);
                            pItemList := PAnsiChar(@pEnumeration^.ItemList) +
                                         pEnumeration^.CurrentIndex * ItemSize;
                            CopyMemory(@pOneValue^.Item, pItemList, ItemSize);
                          end;
       TWON_ONEVALUE    : begin
                            if (pOneValue <> Nil)
                            then FreeMem(pOneValue);
                            pOneValue := pContainer;
                          end;
       TWON_RANGE       : begin
                            pRange := pContainer;
                            pOneValue^.ItemType := pRange^.ItemType;
                            pOneValue^.Item     := pRange^.CurrentValue;
                          end;
       end;
       FItemType := pOneValue^.ItemType;

       // Special case to handle Frames.
       if (FCap = ICAP_FRAMES)
       then SetupFrames(NumItems);
  end;
end; { End TTwnContainer.AssignContainerPtr.                                   }


function TTwnContainer.GetContainerPtr(AsOneValue : bool) : pointer;
// GetContainerPtr returns an internal pointer to one of the TW_XXX containers.
// The returned pointer must NEVER be freed outside this object.
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  Result := Nil;
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then begin
                          AsOneValue := False;
                          Result := pArray;
                     end;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then begin
                          Result := pEnumeration;
                          // Update pOneValue for setting the capability as a
                          // one value container.
                          pOneValue^.ItemType := pEnumeration^.ItemType;
                          ItemSize  := GetItemSize(FItemType);
                          pItemList := PAnsiChar(@pEnumeration^.ItemList) +
                                       pEnumeration^.CurrentIndex * ItemSize;
                          CopyMemory(@pOneValue^.Item, pItemList, ItemSize);
                     end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then begin
                          Result := pOneValue;
                     end;
  TWON_RANGE       : if Assigned(pRange)
                     then begin
                          Result := pRange;
                          pOneValue^.ItemType := pRange^.ItemType;
                          pOneValue^.Item     := pRange^.CurrentValue;
                     end;
  end;
  if AsOneValue
  then Result := pOneValue;
end; { End TTwnContainer.GetContainerPtr.                                      }


function TTwnContainer.GetItemListIndexFrom(Value : Variant) : integer;
var i         : integer;
    pItemList : PAnsiChar;
    ItemSize  : integer;
    Continue  : boolean;
    ReturnVal : Variant;
begin
  Result := -1;
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then begin
                          ItemSize  := GetItemSize(FItemType);
                          with pArray^
                          do begin
                             pItemList := @ItemList;
                             Continue := True;
                             i := 0;
                             while (i < NumItems) and Continue
                             do begin
                                if (Value = GetValue(pItemList, ItemType))
                                then begin
                                     Continue := False;
                                     Result := i;
                                end;
                                pItemList := pItemList + ItemSize;
                                inc(i);
                             end;
                          end;
                     end;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then begin
                          ItemSize  := GetItemSize(FItemType);
                          with pEnumeration^
                          do begin
                             pItemList := @ItemList;
                             Continue := True;
                             i := 0;
                             while (i < NumItems) and Continue
                             do begin
                                ReturnVal := GetValue(pItemList, ItemType);
                                if (Value = ReturnVal)
                                then begin
                                     Continue := False;
                                     Result := i;
                                end;
                                pItemList := pItemList + ItemSize;
                                inc(i);
                             end;
                          end;
                     end;
  end;
end; { End TTwnContainer.GetItemListIndexFrom.                                 }


function TTwnContainer.DeleteFrame(Index : integer) : smallint;
var j         : integer;
    ItemSize  : integer;
    pPreList  : PAnsiChar;
    pPostList : PAnsiChar;
begin
  Result := -1;
  if (0 <= Index) and (Index < FFrameList.Count)
  then begin
       ItemSize := GetItemSize(FItemType);
       case FConType of
       TWON_ENUMERATION : begin
                            TTwnFrame(FFrameList.Items[Index]).Free;
                            FFrameList.Items[Index] := Nil;
                            FFrameList.Delete(Index);
                            if Assigned(pEnumeration)
                            then begin
                                 with pEnumeration^
                                 do begin
                                    pPreList  := PAnsiChar(@ItemList) + Index * ItemSize;
                                    pPostList := pPreList + ItemSize;
                                    inc(Index);
                                    for j := Index to (NumItems - 1)
                                    do begin
                                       CopyMemory(pPreList, pPostList, ItemSize);
                                       pPreList  := pPreList + ItemSize;
                                       pPostList := pPostList + ItemSize;
                                    end;
                                    dec(NumItems);
                                    Result := Index;
                                 end;
                            end;
                          end;
       end;
  end;
end; { End TTwnContainer.DeleteFrame.                                          }


function TTwnContainer.DeleteItem(Value : Variant) : smallint;
// Returnes the index at which Value did reside. If Value is not found -1 is
// returned.
var i, j      : integer;
    ItemSize  : integer;
    pPreList  : PAnsiChar;
    pPostList : PAnsiChar;
begin
  Result := -1;
  i := GetItemListIndexFrom(Value);
  if (i >= 0)
  then begin
       ItemSize := GetItemSize(FItemType);
       case FConType of
       TWON_ARRAY       : if Assigned(pArray)
                          then begin
                               with pArray^
                               do begin
                                  pPreList  := PAnsiChar(@ItemList) + i * ItemSize;
                                  pPostList := pPreList + ItemSize;
                                  inc(i);
                                  for j := i to (NumItems - 1)
                                  do begin
                                     CopyMemory(pPreList, pPostList, ItemSize);
                                     pPreList  := pPreList + ItemSize;
                                     pPostList := pPostList + ItemSize;
                                  end;
                                  dec(NumItems);
                                  Result := i;
                               end;
                          end;
       TWON_ENUMERATION : if Assigned(pEnumeration)
                          then begin
                               with pEnumeration^
                               do begin
                                  pPreList  := PAnsiChar(@ItemList) + i * ItemSize;
                                  pPostList := pPreList + ItemSize;
                                  inc(i);
                                  for j := i to (NumItems - 1)
                                  do begin
                                     CopyMemory(pPreList, pPostList, ItemSize);
                                     pPreList  := pPreList + ItemSize;
                                     pPostList := pPostList + ItemSize;
                                  end;
                                  dec(NumItems);
                                  Result := i;
                               end;
                          end;
       end;
  end;
end; { End TTwnContainer.DeleteItem.                                           }


function TTwnContainer.GetValue(pValue   : pointer;
                                ItemType : TW_UINT16) : Variant;
begin
  case ItemType of
  TWTY_INT8    : Result := ShortInt(pValue^);
  TWTY_INT16   : Result := SmallInt(pValue^);
  TWTY_INT32   : Result := longint(pValue^);
  TWTY_UINT8   : Result := byte(pValue^);
  TWTY_UINT16  : Result := word(pValue^);
  TWTY_UINT32  : Result := longint(pValue^); // Should be LongWord!
  TWTY_BOOL    : Result := Bool(pValue^);
  TWTY_FIX32   : Result := FIX32ToFloat(TW_FIX32(pValue^));
  TWTY_FRAME   : Result := longint(0); // longint(pValue);
  TWTY_STR32   : Result := string(pTW_STR32(pValue)^);
  TWTY_STR64   : Result := string(pTW_STR64(pValue)^);
  TWTY_STR128  : Result := string(pTW_STR128(pValue)^);
  TWTY_STR255  : Result := string(pTW_STR255(pValue)^);
  end;
end; { End TTwnContainer.GetValue.                                             }


procedure TTwnContainer.SetValue(Value    : Variant;
                                 pValue   : pointer;
                                 ItemType : TW_UINT16);
begin
  case ItemType of
  TWTY_INT8,
  TWTY_INT16,
  TWTY_INT32   : TW_INT32(pValue^) := word(Value);
  TWTY_UINT8,
  TWTY_UINT16,
  TWTY_UINT32  : TW_UINT32(pValue^) := integer(Value);
  TWTY_BOOL    : TW_INT32(pValue^)  := integer(boolean(Value));
  TWTY_FIX32   : TW_FIX32(pValue^)  := FloatToFIX32(real(Value));
  // TWTY_FRAME   : ;
  {$IFDEF GE_DXE4}
  TWTY_STR32   : System.AnsiStrings.StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR64   : System.AnsiStrings.StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR128  : System.AnsiStrings.StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR255  : System.AnsiStrings.StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  {$ELSE}
  TWTY_STR32   : StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR64   : StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR128  : StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  TWTY_STR255  : StrPCopy(PAnsiChar(pValue), AnsiString(Value));
  {$ENDIF}
  else TW_INT32(pValue^) := 0;
  end;
end; { End TTwnContainer.SetValue.                                             }


function TTwnContainer.GetCurrentIndex : smallint;
begin
  Result := -1;
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then Result := pEnumeration.CurrentIndex;
  TWON_ONEVALUE    : Result := 0;
  end;
end; { End TTwnContainer.GetCurrentIndex.                                      }


procedure TTwnContainer.SetCurrentIndex(Value : smallint);
begin
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then if (Value < pEnumeration.NumItems)
                          then pEnumeration.CurrentIndex := Value;
  end;
end; { End TTwnContainer.SetCurrentIndex.                                      }


function TTwnContainer.GetDefaultIndex : smallint;
begin
  Result := -1;
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then Result := pEnumeration.DefaultIndex;
  TWON_ONEVALUE    : Result := 0;
  end;
end; { End TTwnContainer.GetDefaultIndex.                                      }


procedure TTwnContainer.SetDefaultIndex(Value : smallint);
begin
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then if (Value < pEnumeration.NumItems)
                          then pEnumeration.DefaultIndex := Value;
  end;
end; { End TTwnContainer.SetDefaultIndex.                                      }


function TTwnContainer.GetCurrentValue : variant;
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  case FItemType of
  TWTY_STR32,
  TWTY_STR64,
  TWTY_STR128,
  TWTY_STR255 : Result := '';
  else Result := -1;
  end;
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do if (0 <= CurrentIndex) and (CurrentIndex < NumItems)
                             then begin
                                  ItemSize  := CurrentIndex * GetItemSize(ItemType);
                                  pItemList := PAnsiChar(@ItemList) + ItemSize;
                                  Result := GetValue(pItemList, ItemType);
                             end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do Result := GetValue(@Item, ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then with pRange^
                          do Result := GetValue(@pRange^.CurrentValue, ItemType);
  end;
end; { End TTwnContainer.GetCurrentValue.                                      }


procedure TTwnContainer.SetCurrentValue(Value : variant);
var i       : integer;
    StepVal : variant;
begin
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then begin
                          i := GetItemListIndexFrom(Value);
                          if (i >= 0)
                          then pEnumeration^.CurrentIndex := i;
                     end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do SetValue(Value, pointer(@Item), ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then begin
                          if (Value < GetMinValue)
                          then Value := GetMinValue;
                          if (Value > GetMaxValue)
                          then Value := GetMaxValue;
                          with pRange^
                          do begin
                             StepVal := GetStepValue;
                             if (StepVal <> 0)
                             then begin
                                  i := Round((Value - GetMinValue) / GetStepValue);
                                  Value := GetStepValue * i + GetMinValue;
                                  if (Value > GetMaxValue)
                                  then Value := GetMaxValue;
                             end;
                             SetValue(Value, @CurrentValue, ItemType);
                          end;
                     end;
  end;
end; { End TTwnContainer.SetCurrentValue.                                      }


function TTwnContainer.GetDefaultValue : variant;
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  case FItemType of
  TWTY_STR32,
  TWTY_STR64,
  TWTY_STR128,
  TWTY_STR255 : Result := '';
  else Result := -1;
  end;
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do if (0 <= DefaultIndex) and (DefaultIndex < NumItems)
                             then begin
                                  ItemSize  := DefaultIndex * GetItemSize(ItemType);
                                  pItemList := PAnsiChar(@ItemList) + ItemSize;
                                  Result := GetValue(pItemList, ItemType);
                             end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do Result := GetValue(@Item, ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then with pRange^
                          do Result := GetValue(@DefaultValue, ItemType);
  end;
end; { End TTwnContainer.GetDefaultValue.                                      }


procedure TTwnContainer.SetDefaultValue(Value : variant);
var i       : integer;
    StepVal : variant;
begin
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then begin
                          i := GetItemListIndexFrom(Value);
                          if (i >= 0)
                          then pEnumeration^.DefaultIndex := i;
                     end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do SetValue(Value, @Item, ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then begin
                          if (Value < GetMinValue)
                          then Value := GetMinValue;
                          if (Value > GetMaxValue)
                          then Value := GetMaxValue;
                          with pRange^
                          do begin
                             StepVal := GetStepValue;
                             if (StepVal <> 0)
                             then begin
                                  i := Round((Value - GetMinValue) / GetStepValue);
                                  Value := GetStepValue * i + GetMinValue;
                                  if (Value > GetMaxValue)
                                  then Value := GetMaxValue;
                             end;
                             SetValue(Value, @DefaultValue, ItemType);
                          end;
                     end;
  end;
end; { End TTwnContainer.SetDefaultValue.                                      }


function TTwnContainer.GetMaxFromList(ItemList : PAnsiChar;
                                      NumItems : smallint;
                                      ItemType : TW_UINT32) : Variant;
var i         : integer;
    pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  Result := -1;
  pItemList := ItemList;
  if (NumItems > 0)
  then Result := GetValue(pItemList, ItemType);
  ItemSize  := GetItemSize(FItemType);
  i := 1;
  while (i < NumItems)
  do begin
     pItemList := ItemList + i * ItemSize;
     if (Result < GetValue(pItemList, ItemType))
     then Result := GetValue(pItemList, ItemType);
     inc(i);
  end;
end; { End TTwnContainer.GetMaxFromList.                                       }


function TTwnContainer.GetMaxValue : Variant;
begin
  Result := -1;
  if (FItemType <= TWTY_FIX32)
  then begin
       case FConType of
       TWON_ARRAY       : if Assigned(pArray)
                          then with pArray^
                               do Result := GetMaxFromList(@ItemList, NumItems, ItemType);
       TWON_ENUMERATION : if Assigned(pEnumeration)
                          then with pEnumeration^
                               do Result := GetMaxFromList(@ItemList, NumItems, ItemType);
       TWON_ONEVALUE    : if Assigned(pOneValue)
                          then with pOneValue^
                               do Result := GetValue(@Item, ItemType);
       TWON_RANGE       : if Assigned(pRange)
                          then with pRange^
                               do Result := GetValue(@MaxValue, ItemType);
       end;
  end;
end; { End TTwnContainer.GetMaxValue.                                          }


procedure TTwnContainer.SetMaxValue(Value : Variant);
begin
  case FConType of
  TWON_RANGE    : if Assigned(pRange)
                  then with pRange^
                       do SetValue(Value, @MaxValue, ItemType);
  end;
end; { End TTwnContainer.SetMaxValue.                                          }


function TTwnContainer.GetMinFromList(ItemList : PAnsiChar;
                                      NumItems : smallint;
                                      ItemType : TW_UINT32) : Variant;
var i         : integer;
    pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  Result := -1;
  pItemList := ItemList;
  if (NumItems > 0)
  then Result := GetValue(pItemList, ItemType);
  ItemSize  := GetItemSize(FItemType);
  i := 1;
  while (i < NumItems)
  do begin
     pItemList := ItemList + i * ItemSize;
     if (Result > GetValue(pItemList, ItemType))
     then Result := GetValue(pItemList, ItemType);
     inc(i);
  end;
end; { End TTwnContainer.GetMinFromList.                                       }


function TTwnContainer.GetMinValue : Variant;
begin
  Result := -1;
  if (FItemType <= TWTY_FIX32)
  then begin
       case FConType of
       TWON_ARRAY       : if Assigned(pArray)
                          then with pArray^
                               do Result := GetMinFromList(@ItemList, NumItems, ItemType);
       TWON_ENUMERATION : if Assigned(pEnumeration)
                          then with pEnumeration^
                               do Result := GetMinFromList(@ItemList, NumItems, ItemType);
       TWON_ONEVALUE    : if Assigned(pOneValue)
                          then with pOneValue^
                               do Result := GetValue(@Item, ItemType);
       TWON_RANGE       : if Assigned(pRange)
                          then with pRange^
                               do Result := GetValue(@MinValue, ItemType);
       end;
  end;
end; { End TTwnContainer.GetMinValue.                                          }


procedure TTwnContainer.SetMinValue(Value : Variant);
begin
  case FConType of
  TWON_RANGE    : if Assigned(pRange)
                  then with pRange^
                       do SetValue(Value, @MinValue, ItemType);
  end;
end; { End TTwnContainer.SetMinValue.                                          }


function TTwnContainer.GetStepValue : Variant;
begin
  case FConType of
  TWON_RANGE : if Assigned(pRange)
               then with pRange^
                    do Result := GetValue(@StepSize, ItemType);
  else Result := -1;
  end;
end; { End TTwnContainer.GetStepValue.                                         }


function TTwnContainer.GetNumItems : smallint;
begin
  Result := -1;
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then with pArray^
                          do Result := NumItems;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do Result := NumItems;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then Result := 1;
  TWON_RANGE       : if Assigned(pRange)
                     then Result := 1;
  end;
end; { End TTwnContainer.GetNumItems.                                          }


procedure TTwnContainer.SetNumItems(Value : smallint);
var ItemSizes   : integer;
begin
  if (Value >= 0)
  then begin
       case FConType of
       TWON_ARRAY       : begin
                            if Assigned(pArray)
                            then begin
                                 if (pArray^.NumItems <> Value)
                                 then begin
                                      ItemSizes  := GetItemSize(FItemType) * Value;
                                      ReallocMem(pArray, SizeOf(TW_ARRAY) + ItemSizes);
                                 end;
                            end
                            else begin
                                 ItemSizes  := GetItemSize(FItemType) * Value;
                                 GetMem(pArray, SizeOf(TW_ARRAY) + ItemSizes);
                                 FillMemory(pArray, SizeOf(TW_ARRAY) + ItemSizes, 0)
                            end;
                            if Assigned(pArray)
                            then begin
                                 pArray^.NumItems := Value;
                                 pArray^.ItemType := FItemType;
                            end
                            else OutOfMemoryError;
                          end;
       TWON_ENUMERATION : begin
                            if Assigned(pEnumeration)
                            then begin
                                 if (pEnumeration^.NumItems <> Value)
                                 then begin
                                      ItemSizes  := GetItemSize(FItemType) * Value;
                                      ReallocMem(pEnumeration, SizeOf(TW_ENUMERATION) + ItemSizes);
                                 end;
                            end
                            else begin
                                 ItemSizes  := GetItemSize(FItemType) * Value;
                                 GetMem(pEnumeration, SizeOf(TW_ENUMERATION) + ItemSizes);
                                 FillMemory(pEnumeration, SizeOf(TW_ARRAY) + ItemSizes, 0)
                            end;
                            if Assigned(pEnumeration)
                            then begin
                                 pEnumeration^.NumItems := Value;
                                 pEnumeration^.ItemType := FItemType;
                            end
                            else OutOfMemoryError;
                          end;
       TWON_RANGE       : begin
                            if Assigned(pRange)
                            then begin
                                 // A Range structure does not increase in size.
                                 // Don't do anything here.
                            end
                            else begin
                                 GetMem(pRange, SizeOf(TW_RANGE));
                                 FillMemory(pRange, SizeOf(TW_RANGE), 0)
                            end;
                            if Assigned(pRange)
                            then begin
                                 pRange^.ItemType := FItemType;
                            end;
                          end;
       end;
  end;
  if (FCAP = ICAP_FRAMES)
  then SetupFrames(NumItems);
end; { End TTwnContainer.SetNumItems.                                          }


function TTwnContainer.GetFrame(Index : smallint) : TTwnFrame;
begin
  if (0 <= Index) and (Index < FFrameList.Count) and (FCap = ICAP_FRAMES)
  then Result := TTwnFrame(FFrameList.Items[Index])
  else Result := Nil;
end; { End TTwnContainer.GetFrame.                                             }


function TTwnContainer.GetItem(Index : smallint) : variant;
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then with pArray^
                          do if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  Result := GetValue(pItemList, ItemType);
                             end;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  Result := GetValue(pItemList, ItemType);
                             end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do Result := GetValue(@Item, ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then with pRange^
                          do Result := GetValue(@CurrentValue, ItemType);
  else Result := -1;
  end;
end; { End TTwnContainer.GetItem.                                              }


procedure TTwnContainer.SetItem(Index : smallint; Value : Variant);
var pItemList : PAnsiChar;
    ItemSize  : integer;
begin
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then with pArray^
                          do begin
                             if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  SetValue(Value, pItemList, ItemType);
                             end;
                          end;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do begin
                             if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  SetValue(Value, pItemList, ItemType);
                             end;
                          end;
  TWON_ONEVALUE    : if Assigned(pOneValue)
                     then with pOneValue^
                          do SetValue(Value, @pOneValue^.Item, pOneValue^.ItemType);
  TWON_RANGE       : if Assigned(pRange)
                     then with pRange^
                          do SetValue(Value, @CurrentValue, ItemType);
  end;
end; { End TTwnContainer.SetItem.                                              }


procedure TTwnContainer.AddFrame(Left, Top, Right, Bottom : double);
var pItemList : PAnsiChar;
    ItemSize  : integer;
    Index     : smallint;
begin
  Index := GetNumItems;
  case FConType of
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then begin
                          SetNumItems(Index + 1);
                          with pEnumeration^
                          do begin
                             if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  pTW_Frame(pItemList)^.Left   := FloatToFIX32(Left);
                                  pTW_Frame(pItemList)^.Top    := FloatToFIX32(Top);
                                  pTW_Frame(pItemList)^.Right  := FloatToFIX32(Right);
                                  pTW_Frame(pItemList)^.Bottom := FloatToFIX32(Bottom);
                             end;
                          end;
                     end;
  end;
  SetupFrames(NumItems);
end; { End TTwnContainer.AddFrame.                                             }


procedure TTwnContainer.AddItem(Value : Variant);
var pItemList : PAnsiChar;
    ItemSize  : integer;
    Index     : smallint;
begin
  Index := GetNumItems;
  SetNumItems(Index + 1);
  case FConType of
  TWON_ARRAY       : if Assigned(pArray)
                     then with pArray^
                          do if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  SetValue(Value, pItemList, ItemType);
                             end;
  TWON_ENUMERATION : if Assigned(pEnumeration)
                     then with pEnumeration^
                          do if (0 <= Index) and (Index < NumItems)
                             then begin
                                  ItemSize  := GetItemSize(FItemType);
                                  pItemList := PAnsiChar(@ItemList) + Index * ItemSize;
                                  SetValue(Value, pItemList, ItemType);
                             end;
  end;
end; { End TTwnContainer.AddItem.                                              }


procedure TTwnContainer.ExtractOneValue(pData : pTW_CAPABILITY;
                                        pVoid : pointer);
//------------------------------------------------------------------------------
// FUNCTION: ExtractOneValue
//
// ARGS:    pData   pointer to a capability structure, details about container
//          pVoid   ptr will be set to point to the item on return
//
// RETURNS: pVoid pts to extracted value.
//
// NOTES:   This routine will open a container and extract the Item.  The Item
// will be returned to the caller in pVoid.  I will type cast the returned
// value to that of ItemType.
//
// Protocol: used by MSG_SET calls were Source empties then App frees.  It is
// also assumed that the APP allocates and fills the container BEFORE this
// call.
//------------------------------------------------------------------------------
var pOneValue : pTW_ONEVALUE;
begin
  pOneValue := pTW_ONEVALUE(GlobalLock(pData^.hContainer));
  if (pOneValue <> Nil)
  then begin
       // Add a check for valid type.
       // Cast to type of var caller wants.
       case pOneValue^.ItemType of
       TWTY_INT8   : TW_INT8(pVoid^)   := TW_INT8(pOneValue^.Item);
       TWTY_UINT8  : TW_UINT8(pVoid^)  := TW_UINT8(pOneValue^.Item);
       TWTY_INT16  : TW_INT16(pVoid^)  := TW_INT16(pOneValue^.Item);
       TWTY_UINT16 : TW_UINT16(pVoid^) := TW_UINT16(pOneValue^.Item);
       TWTY_INT32  : TW_INT32(pVoid^)  := TW_INT32(pOneValue^.Item);
       TWTY_UINT32 : TW_UINT32(pVoid^) := TW_UINT32(pOneValue^.Item);
       TWTY_BOOL   : TW_BOOL(pVoid^)   := TW_BOOL(pOneValue^.Item);
       TWTY_FIX32  : pTW_FIX32(pVoid)^ := TW_FIX32(pOneValue^.Item);
       TWTY_STR32  : CopyMemory(pTW_STR32(pVoid), @pOneValue^.Item, SizeOf(TW_STR32));
       TWTY_STR64  : CopyMemory(pTW_STR64(pVoid), @pOneValue^.Item, SizeOf(TW_STR64));
       TWTY_STR128 : CopyMemory(pTW_STR128(pVoid), @pOneValue^.Item, SizeOf(TW_STR128));
       TWTY_STR255 : CopyMemory(pTW_STR255(pVoid), @pOneValue^.Item, SizeOf(TW_STR255));
       TWTY_FRAME  : CopyMemory(pTW_FRAME(pVoid), @pOneValue^.Item, SizeOf(TW_FRAME));
       else          begin
                     end;
       end;
       GlobalUnlock(pData^.hContainer);
  end;
  // It is assumed that the App will free the container upon return.
end; { End TTwnContainer.ExtractOneValue.                                      }


function TTwnContainer.GetQuerySupport : integer;
var twCap  : TW_CAPABILITY;
    {$IFNDEF DATASOURCE}
    pQuery : pTW_ONEVALUE;
    {$ENDIF}
begin
  twCap.hContainer := 0;
  {$IFNDEF DATASOURCE}
  Result := TTypeCastTWAINIntf(Owner).SendCapabilityMsg(FCap, MSG_QUERYSUPPORT, @twCap);
  if (Result = TWRC_SUCCESS)
  then begin
       FQuerySupport := integer($FFFFFFFF);
       pQuery := pTW_ONEVALUE(GlobalLock(twCap.hContainer));
       if (pQuery <> Nil)
       then if (pQuery^.ItemType = TWTY_INT32)
            then if (twCap.ConType = TWON_ONEVALUE)
                 then ExtractOneValue(@twCap, @FQuerySupport);
  end
  else FQuerySupport := integer($FFFFFFFF);
  if (twCap.hContainer <> 0)
  then GlobalFree(twCap.hContainer);
  {$ELSE}
    FQuerySupport := integer($FFFFFFFF);
  {$ENDIF}
  Result := FQuerySupport;
end; { End TTwnContainer.GetQuerySupport.                                      }


{$IFDEF DATASOURCE}

function TTwnContainerSrc.GetContainerPtr(AsOneValue : bool) : pointer;
begin
  Result := Inherited GetContainerPtr(AsOneValue);
end; { End TTwnContainerSrc.GetContainerPtr.                                   }


function TTwnContainerSrc.GetStepValue : Variant;
begin
  Result := Inherited GetStepValue;
end; { End TTwnContainerSrc.GetStepValue.                                      }


procedure TTwnContainerSrc.SetStepValue(Value : Variant);
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
end; { End TTwnContainerSrc.SetStepValue.                                      }

procedure TTwnContainerSrc.AssignContainerPtr(Cap : word; ConType : word; pContainer : pointer);
begin
  Inherited AssignContainerPtr(Cap, ConType, pContainer);
end; { End TTwnContainerSrc.AssignContainerPtr.                                }


{$ENDIF}

//------------------------------------------------------------------------------
// TTwnContainerList.
//------------------------------------------------------------------------------

constructor TTwnContainerList.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FList := TList.Create;
end; { End TTwnContainerList.Create.                                           }


destructor TTwnContainerList.Destroy;
begin
  Clear;
  FList.Free;
  Inherited Destroy;
end; { End TTwnContainerList.Destroy.                                          }


procedure TTwnContainerList.Clear;
var i : integer;
begin
  for i := (FList.Count - 1) downto 0
  do begin
     try
       if Assigned(FList.Items[i])
       then TTwnContainer(FList.Items[i]).Free;
       FList.Items[i] := Nil;
       FList.Delete(i);
     except
       FList.Items[i] := Nil;
     end;
  end;
  FList.Clear;
end; { End TTwnContainerList.Clear.                                            }


function TTwnContainerList.GetItem(Index : word) : TTwnContainer;
var i : integer;
begin
  i := 0;
  Result := Nil;
  while (i < FList.Count)
  do begin
     if (TTwnContainer(FList.Items[i]).Capability = Index)
     then begin
          Result := TTwnContainer(FList.Items[i]);
          Exit;
     end;
     inc(i);
  end;
end; { End TTwnContainerList.GetItem.                                          }


procedure TTwnContainerList.AddItem(Cap : word; Value : TTwnContainer);
begin
  if Assigned(Value)
  then begin
       Value.Capability := Cap;
       // If a container holding Cap already exist, delete it!
       DeleteItem(Cap);
       FList.Add(Value);
  end;
end; { End TTwnContainerList.AddItem.                                          }


function TTwnContainerList.CreateItem(Cap : word) : TTwnContainer;
var NewItem : TTwnContainer;
begin
  try
    NewItem := TTwnContainer.Create(Self);
    if Assigned(NewItem)
    then begin
         NewItem.Capability := Cap;
         AddItem(Cap, NewItem);
         Result := NewItem;
    end
    else Result := Nil;
  except
    Result := Nil;
  end;
end; { ENd TTwnContainerList.CreateItem.                                       }


procedure TTwnContainerList.DeleteItem(Cap : word);
var i : integer;
begin
  i := 0;
  while (i < FList.Count)
  do begin
     if (TTwnContainer(FList.Items[i]).Capability = Cap)
     then begin
          try
            TTwnContainer(FList.Items[i]).Free;
            FList.Items[i] := Nil;
            FList.Delete(i);
          except
            FList.Items[i] := Nil;
          end;
          Exit;
     end;
     inc(i);
  end;
end; { End TTwnContainerList.DeleteItem.                                       }


function TTwnContainerList.GetCount : integer;
begin
  Result := FList.Count;
end; { End TTwnContainerList.GetCount.                                         }

{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

end.
