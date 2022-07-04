{*********************************************************}
{*                   VPBASE.PAS 1.03                     *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpBase;

{$R VpBASE.RES}

interface

uses
  Windows, Classes, Graphics, Controls, Dialogs, Forms, Messages, StdCtrls,
  ExtCtrls, SysUtils, VpConst,
  VpSR;                                                                  

const
  {Message base}
  Vp_First              = $7DF0;   {Sets base for all Vp messages}

const
  {Custom message types}
  Vp_PrintFormatChanged = Vp_First + 1;   {Print formats have changed}
  Vp_DataStoreChanged   = Vp_First + 2;   {Data Store has changed}
  Vp_DayViewInit        = Vp_First + 3;   {Initialize the DayView}     

type
  TVpRotationAngle = (ra0, ra90, ra180, ra270);
  TVpItemMeasurement = (imAbsolutePixel, imPercent, imInches);
  TVpItemType = (itDayView, itWeekView, itMonthView, itCalendar,
                  itShape, itCaption, itTasks, itContacts);

  TVpHours = (h_00, h_01, h_02, h_03, h_04, h_05, h_06, h_07, h_08,
               h_09, h_10, h_11, h_12, h_13, h_14, h_15, h_16, h_17,
               h_18, h_19, h_20, h_21, h_22, h_23);

  TVpGranularity = (gr05Min, gr06Min, gr10Min, gr15Min, gr20Min, gr30Min,
                    gr60Min);

  TVpEditorReturnCode = (rtCommit, rtAbandon);

  TVpCheckStyle = (csX, csCheck);

  TVpTimeFormat = (tf24Hour, tf12Hour);

  { XML definitions }
  DOMString = WideString;

  { miscellaneous stuff }
  TVpDrawingStyle = (dsFlat, ds3d);

  { event method types }
  TVpMouseWheelEvent = procedure(Sender : TObject; Shift : TShiftState;
    Delta, XPos, YPos : Word) of object;

  TVpOwnerDrawEvent = procedure(Sender: TObject; const Canvas: TCanvas;
    R: TRect; var Drawn: Boolean) of object;

  TVpOwnerDrawRowEvent = procedure(Sender: TObject; const Canvas: TCanvas;
    R: TRect; RowHeight: Integer; var Drawn: Boolean) of object;

  TVpOwnerDrawDayEvent = procedure(Sender: TObject; const Canvas: TCanvas;
    R: TRect; Day: Integer; var Drawn: Boolean) of object;

  TVpItemSelectedEvent = procedure(Sender : TObject;
    Index : Integer) of object;

  TVpGetEditorCaption = procedure(var Caption : string) of object;


  { XML exceptions }
  EXML = class (Exception);

  EVpStreamError = class(EXML)
  private
    seFilePos : Longint;
  public
    constructor CreateError(const FilePos : Longint;
                            const Reason  : DOMString);
    property FilePos : Longint
       read seFilePos;
  end;

  EVpFilterError = class(EVpStreamError)
  private
    feReason  : DOMString;
    feLine    : Longint;
    feLinePos : Longint;
  public
    constructor CreateError(const FilePos, Line, LinePos : Longint;
                            const Reason : DOMString);
    property Reason : DOMString
       read feReason;
    property Line : Longint
       read feLine;
    property LinePos : Longint
       read feLinePos;
  end;

  EVpParserError = class(EVpFilterError)
  protected
  public
    constructor CreateError(Line, LinePos : Longint;
                            const Reason : DOMString);
  end;

  { implements the Version property with its associated design time About box }
  TVpComponent = class(TComponent)
  protected { private }
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    { properties }
    property Version : string read GetVersion write SetVersion stored False;
  end;

  { Ancestor for all Visual PlanIt visual controls }
  TVpCustomControl = class(TCustomControl)
  protected { private }
    FAfterEnter    : TNotifyEvent;
    FAfterExit     : TNotifyEvent;
    FOnMouseWheel  : TVpMouseWheelEvent;
    FAutoScroll    : Boolean;
    function GetVersion : string;
    procedure SetVersion(const Value : string);
    procedure CMVisibleChanged(var Msg : TMessage); message CM_VISIBLECHANGED;
    procedure WMMouseWheel(var Msg : TMessage); message WM_MOUSEWHEEL;

  protected
    procedure DoOnMouseWheel(Shift : TShiftState;
      Delta, XPos, YPos : SmallInt); dynamic;
    procedure CreateWnd; override;
    property AfterEnter : TNotifyEvent read FAfterEnter write FAfterEnter;
    property AfterExit : TNotifyEvent read FAfterExit write FAfterExit;
    property OnMouseWheel : TVpMouseWheelEvent read FOnMouseWheel
      write FOnMouseWheel;

  public                                                                 
    constructor Create (AOwner : TComponent); override;                  

  published
    property Version : string read GetVersion write SetVersion stored False;

    {$IFDEF VERSION6}                                                    
    property BevelEdges;                                                 
    property BevelInner;                                                 
    property BevelOuter;                                                 
    property BevelKind;                                                  
    property BevelWidth;                                                 
    {$ENDIF}                                                             

    { The Hint property is published in TControl, but the ShowHint }     
    { property is left public. odd.                                }     
    { surfacing here will make it published in all our descendants }     
    property ShowHint;                                                   
  end;

  TVpPersistent = class(TPersistent)
  public
    procedure Invalidate; virtual; abstract;
  end;

  {TVpCategoryColorMap}
  TVpCategoryInfo= class(TPersistent)
  private
    FCategoryIndex: Integer;
  protected
    FBackgroundColor : TColor;                                                                                          
    FColor           : TColor;
    FDescription     : string;
    FIndex           : Integer;
    FBitmap          : TBitmap;                                          
    procedure SetBackgroundColor (const v : TColor);                     
    procedure SetBitmap (v : TBitmap);                                   
    procedure SetColor(Value: TColor);
    procedure SetDescription(Value: string);
  public                                                                 
    constructor Create;                                                  
    destructor Destroy; override;                                        
  published
    property BackgroundColor : TColor                                    
             read FBackgroundColor write SetBackgroundColor              
             default clWindow;                                           
    property Bitmap : TBitmap read FBitmap write SetBitmap;              
    property Color: TColor read FColor write SetColor;
    property Description: string read FDescription write SetDescription;
    property CategoryIndex: Integer read FCategoryIndex;
  end;

  TVpCategoryColorMap = class(TPersistent)
  protected
    FCat0 : TVpCategoryInfo;
    FCat1 : TVpCategoryInfo;
    FCat2 : TVpCategoryInfo;
    FCat3 : TVpCategoryInfo;
    FCat4 : TVpCategoryInfo;
    FCat5 : TVpCategoryInfo;
    FCat6 : TVpCategoryInfo;
    FCat7 : TVpCategoryInfo;
    FCat8 : TVpCategoryInfo;
    FCat9 : TVpCategoryInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function GetColor(Index: Integer): TColor;
    function GetName(Index: Integer):string;
  published
    property Category0 : TVpCategoryInfo read FCat0 write FCat0;
    property Category1 : TVpCategoryInfo read FCat1 write FCat1;
    property Category2 : TVpCategoryInfo read FCat2 write FCat2;
    property Category3 : TVpCategoryInfo read FCat3 write FCat3;
    property Category4 : TVpCategoryInfo read FCat4 write FCat4;
    property Category5 : TVpCategoryInfo read FCat5 write FCat5;
    property Category6 : TVpCategoryInfo read FCat6 write FCat6;
    property Category7 : TVpCategoryInfo read FCat7 write FCat7;
    property Category8 : TVpCategoryInfo read FCat8 write FCat8;
    property Category9 : TVpCategoryInfo read FCat9 write FCat9;
  end;

  { TVpFont }
  TVpFont = class(TFont)
  protected
    FOwner: TObject;
    procedure Changed; override;
  public
    constructor Create(AOwner: TObject); virtual;
    property Owner: TObject read FOwner write FOwner; 
  end;

  { Collections }
  TVpCollectionItem = class(TCollectionItem)
  protected { private }
    FName: String;
    FDisplayText: String;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetName(Value: String); virtual;
  public
    property DisplayText : string read FDisplayText write FDisplayText;
    property Name: String read FName write SetName;
  published
    property Version : String read GetVersion write SetVersion;
  end;

  TVpCollection = class(TCollection)
  protected { private }
    { property variables }
    FItemEditor     : TForm;
    FReadOnly       : Boolean;
    FOwner          : TPersistent;
    { event variables }
    FOnChanged      : TNotifyEvent;
    FOnItemSelected : TVpItemSelectedEvent;
    FOnGetEditorCaption : TVpGetEditorCaption;
    { Internal variables }
    InLoaded        : Boolean;
    IsLoaded        : Boolean;
    InChanged       : Boolean;
  protected
    function GetCount : Integer;
    procedure Loaded;
  public
    constructor Create(AOwner : TPersistent;
                       ItemClass : TCollectionItemClass); virtual;
    destructor Destroy; override;
    property ItemEditor : TForm read FItemEditor write FItemEditor;
    function Add : TVpCollectionItem; dynamic;
    {$IFNDEF VERSION4}
    function Insert(Index: Integer): TVpCollectionItem; dynamic;
    {$ENDIF}
    function GetItem(Index: Integer): TVpCollectionItem;
    function GetOwner: TPersistent; override;
    procedure SetItem(Index: Integer; Value: TVpCollectionItem);
    procedure DoOnItemSelected(Index : Integer);
    function GetEditorCaption : string;
    function ItemByName(const Name : string) : TVpCollectionItem;
    function ParentForm : TForm;
    property Count: Integer read GetCount;
    property Item[Index: Integer] : TVpCollectionItem
      read GetItem write SetItem; default;
    property OnGetEditorCaption : TVpGetEditorCaption
      read FOnGetEditorCaption write FOnGetEditorCaption;
    property ReadOnly : Boolean
      read FReadOnly write FReadOnly default False;
    property OnChanged : TNotifyEvent
      read FOnChanged write FOnChanged;
    property OnItemSelected : TVpItemSelectedEvent
      read FOnItemSelected write FOnItemSelected;
  end;

  TVpContainerList = class(TList)
  protected{ private }
    FOwner: TComponent;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  end;
  { End - Collections }

  TVpTimeRange = class(TPersistent)
  protected{private}
    FOwner: TObject;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FRangeBegin: TVpHours;
    FRangeEnd: TVpHours;
    procedure SetRangeBegin(const Value: TVpHours);
    procedure SetRangeEnd(const Value: TVpHours);
    procedure SetEndTime(const Value: TDateTime);
    procedure SetStartTime(const Value: TDateTime);
  public
    constructor Create(aOwner: TObject);
    destructor Destroy; override;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
  published
    property RangeBegin: TVpHours read FRangeBegin write SetRangeBegin;
    property RangeEnd: TVpHours read FRangeEnd write SetRangeEnd;
  end;

  TVpTimeSlotColor = class(TPersistent)
  protected { private }
    FOwner: TVpCustomControl;
    FActiveRange: TVpTimeRange;
    FInactive: TColor;
    FHoliday: TColor;
    FWeekend: TColor;
    FActive: TColor;
    FWeekday: TColor;
    procedure SetActive(const Value: TColor);
    procedure SetHoliday(const Value: TColor);
    procedure SetInactive(const Value: TColor);
    procedure SetWeekday(const Value: TColor);
    procedure SetWeekend(const Value: TColor);
  public
    constructor Create(AOwner: TVpCustomControl);
    destructor Destroy; override;
    procedure Changed;
  published
    property Active: TColor read FActive write SetActive;
    property Inactive: TColor read FInactive write SetInactive;
    property Holiday: TColor read FHoliday write SetHoliday;
    property Weekday: TColor read FWeekday write SetWeekday;
    property Weekend: TColor read FWeekend write SetWeekend;
    property ActiveRange: TVpTimeRange
      read FActiveRange write FActiveRange;
  end;

implementation

uses
  Math, CommCtrl;

{ EAdStreamError }

constructor EVpStreamError.CreateError(const FilePos: Integer;
  const Reason: DOMString);
begin
  inherited Create (Reason);
  seFilePos := FilePos;
end;

{ EAdFilterError }

constructor EVpFilterError.CreateError(const FilePos, Line,
  LinePos: Integer; const Reason: DOMString);
begin
  inherited CreateError(FilePos, Reason);

  feLine := Line;
  feLinePos := LinePos;
  feReason := Reason;
end;

{ EAdParserError }

constructor EVpParserError.CreateError(Line, LinePos: Integer;
  const Reason: DOMString);
begin
  inherited CreateError(FilePos, Line, LinePos, Reason);
end;

(*****************************************************************************)
{ TVpCustomControl }

constructor TVpCustomControl.Create (AOwner : TComponent);               
begin                                                                    
  inherited Create (AOwner);                                             
  TabStop := True;                                                       
end;                                                                     
{=====}

procedure TVpCustomControl.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if csLoading in ComponentState then
    Exit;
end;
{=====}

procedure TVpCustomControl.CreateWnd;
begin
  inherited CreateWnd;
end;
{=====}

procedure TVpCustomControl.DoOnMouseWheel(Shift: TShiftState; Delta, XPos,
  YPos: SmallInt);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, Delta, XPos, YPos);
end;
{=====}

function TVpCustomControl.GetVersion: string;
begin
  Result := VpVersionStr;
end;
{=====}

procedure TVpCustomControl.SetVersion(const Value: string);
begin
// This method left intentionally blank.
end;
{=====}

procedure TVpCustomControl.WMMouseWheel(var Msg: TMessage);
begin
  with Msg do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)) {fwKeys},
                   HIWORD(wParam) {zDelta},
                   LOWORD(lParam) {xPos},   HIWORD(lParam) {yPos});
end;
{=====}

(*****************************************************************************)
{ TVpCollection }

constructor TVpCollection.Create(AOwner : TPersistent;
                                  ItemClass : TCollectionItemClass);
begin
  FOwner := AOwner;
  Inherited Create(ItemClass);
end;
{=====}

destructor TVpCollection.Destroy;
begin
  ItemEditor.Free;
  Clear;
  inherited Destroy;
end;
{=====}

procedure TVpCollection.DoOnItemSelected(Index : Integer);
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, Index);
end;
{=====}

function TVpCollection.GetCount : Integer;
begin
  Result := inherited Count;
end;
{=====}

function TVpCollection.GetEditorCaption : string;
begin
  Result := 'Editing ' + ClassName;
  if Assigned(FOnGetEditorCaption) then
    FOnGetEditorCaption(Result);
end;
{=====}

function TVpCollection.Add : TVpCollectionItem;
begin
  Result := TVpCollectionItem(inherited Add);
  if ItemEditor <> nil then
    SendMessage(ItemEditor.Handle, Vp_PROPCHANGE, 0, 0);
end;
{=====}

{$IFNDEF VERSION4}
function TVpCollection.Insert(Index: Integer): TVpCollectionItem;
var
  I: Integer;
begin
  result := Add;
  for I := Index to Count - 2 do
    Items[I].Index := I + 1;
  Items[Count - 1].Index := Index;
end;
{=====}
{$ENDIF}

function TVpCollection.GetItem(Index : Integer) : TVpCollectionItem;
begin
  Result := TVpCollectionItem(inherited GetItem(Index));
end;

function TVpCollection.GetOwner: TPersistent;
begin
  result := FOwner;
end;
{=====}

procedure TVpCollection.SetItem(Index : Integer; Value : TVpCollectionItem);
begin
  inherited SetItem(Index, Value);
end;
{=====}

function TVpCollection.ItemByName(const Name : string) : TVpCollectionItem;
var
  i : Integer;
begin
  for i := 0 to pred(Count) do
    if Item[i].Name = Name then begin
      Result := Item[i];
      exit;
    end;
  Result := nil;
end;
{=====}

procedure TVpCollection.Loaded;
begin
  InLoaded := True;
  try
    Changed;
  finally
    InLoaded := False;
  end;
  IsLoaded := True;
end;
{=====}

function TVpCollection.ParentForm : TForm;
var
  Temp : TObject;
begin
  Temp := GetOwner;
  while (Temp <> nil) and not (Temp is TForm) do
    Temp := TComponent(Temp).Owner;
  Result := TForm(Temp);
end;
{=====}

(*****************************************************************************)
{ TVpCollectionItem }

function TVpCollectionItem.GetVersion: String;
begin
  Result := VpVersionStr;
end;
{=====}

procedure TVpCollectionItem.SetVersion(const Value: String);
begin
end;
{=====}

procedure TVpCollectionItem.SetName(Value: String);
begin
  FName := Value;
end;
{=====}

(*****************************************************************************)
{ TO32ContainerList }

constructor TVpContainerList.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := TComponent(AOwner);
end;
{=====}

destructor TVpContainerList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TPanel(Items[I]).Free;
  inherited;
end;
{=====}

(*****************************************************************************)
{ TVpComponent }

constructor TVpComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{=====}

function TVpComponent.GetVersion: string;
begin
  Result := VpVersionStr;
end;
{=====}

procedure TVpComponent.SetVersion(const Value: string);
begin
// This method left intentionally blank.
end;
{=====}

(*****************************************************************************)
{ VpFont }

procedure TVpFont.Changed;
begin
  inherited;
  Assert((FOwner is TControl) or (FOwner is TVpPersistent),
         Format('TVpFont.Changed: Unexpected parent class: %s',
                [FOwner.ClassName]));
  if FOwner is TControl then
    TControl(FOwner).Invalidate
  else if FOwner is TVpPersistent then
    TVpPersistent(FOwner).Invalidate;
end;
{=====}

constructor TVpFont.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;
{=====}

(*****************************************************************************)
{ TVpCategoryColorMap }

constructor TVpCategoryColorMap.Create;
begin
  inherited Create;

  FCat0 := TVpCategoryInfo.Create;
    FCat0.Color := clNavy;
    FCat0.Description := RSCategoryDesc0;                                
    FCat0.FIndex := 0;
  FCat1 := TVpCategoryInfo.Create;
    FCat1.Color := clRed;
    FCat1.Description := RSCategoryDesc1;                                
    FCat1.FIndex := 1;
  FCat2 := TVpCategoryInfo.Create;
    FCat2.Color := clYellow;
    FCat2.Description := RSCategoryDesc2;                                
    FCat2.FIndex := 2;
  FCat3 := TVpCategoryInfo.Create;
    FCat3.Color := clLime;
    FCat3.Description := RSCategoryDesc3;                                
    FCat3.FIndex := 3;
  FCat4 := TVpCategoryInfo.Create;
    FCat4.Color := clPurple;
    FCat4.Description := RSCategoryDesc4;                                
    FCat4.FIndex := 4;
  FCat5 := TVpCategoryInfo.Create;
    FCat5.Color := clTeal;
    FCat5.Description := RSCategoryDesc5;                                
    FCat5.FIndex := 5;
  FCat6 := TVpCategoryInfo.Create;
    FCat6.Color := clFuchsia;
    FCat6.Description := RSCategoryDesc6;                                
    FCat6.FIndex := 6;
  FCat7 := TVpCategoryInfo.Create;
    FCat7.Color := clOlive;
    FCat7.Description := RSCategoryDesc7;                                
    FCat7.FIndex := 7;
  FCat8 := TVpCategoryInfo.Create;
    FCat8.Color := clAqua;
    FCat8.Description := RSCategoryDesc8;                                
    FCat8.FIndex := 8;
  FCat9 := TVpCategoryInfo.Create;
    FCat9.Color := clMaroon;
    FCat9.Description := RSCategoryDesc9;                                
    FCat9.FIndex := 9;
end;
{=====}

destructor TVpCategoryColorMap.Destroy;
begin
  FCat0.Free;
  FCat1.Free;
  FCat2.Free;
  FCat3.Free;
  FCat4.Free;
  FCat5.Free;
  FCat6.Free;
  FCat7.Free;
  FCat8.Free;
  FCat9.Free;
  inherited;
end;
{=====}

function TVpCategoryColorMap.GetColor(Index: Integer): TColor;
begin
  case Index of
    0 : result := FCat0.Color;
    1 : result := FCat1.Color;
    2 : result := FCat2.Color;
    3 : result := FCat3.Color;
    4 : result := FCat4.Color;
    5 : result := FCat5.Color;
    6 : result := FCat6.Color;
    7 : result := FCat7.Color;
    8 : result := FCat8.Color;
    9 : result := FCat9.Color;
  else
    result := clBlack;
  end;
end;
{=====}

function TVpCategoryColorMap.GetName(Index: Integer): string;
begin
  case Index of
    0 : result := FCat0.Description;
    1 : result := FCat1.Description;
    2 : result := FCat2.Description;
    3 : result := FCat3.Description;
    4 : result := FCat4.Description;
    5 : result := FCat5.Description;
    6 : result := FCat6.Description;
    7 : result := FCat7.Description;
    8 : result := FCat8.Description;
    9 : result := FCat9.Description;
  else
    result := '';
  end;
end;
{=====}

(*****************************************************************************)
{ TVpCategoryInfo }

constructor TVpCategoryInfo.Create;                                      
begin                                                                    
  inherited Create;                                                      

  FBitmap          := TBitmap.Create;                                    
  FBackgroundColor := clWindow;                                          
end;                                                                     

destructor TVpCategoryInfo.Destroy;                                      
begin                                                                    
  FBitmap.Free;                                                          

  inherited Destroy;                                                     
end;                                                                     

procedure TVpCategoryInfo.SetBackgroundColor (const v : TColor);         
begin                                                                    
  if v <> FBackgroundColor then                                          
    FBackgroundColor := v;                                               
end;                                                                     

procedure TVpCategoryInfo.SetBitmap (v : TBitmap);                       
begin                                                                    
  FBitmap.Assign (v);                                                    
end;                                                                     

procedure TVpCategoryInfo.SetColor(Value: TColor);
begin
  if Value <> FColor then
    FColor := Value;
end;
{=====}

procedure TVpCategoryInfo.SetDescription(Value: string);
begin
  if Value <> FDescription then
    FDescription := Value;
end;
{=====}



{ TVpTimeRange }
(*****************************************************************************)
constructor TVpTimeRange.Create(aOwner: TObject);
begin
  inherited Create;
  FOwner := aOwner;
end;

destructor TVpTimeRange.Destroy;
begin
  inherited;
end;

procedure TVpTimeRange.SetRangeBegin(const Value: TVpHours);
begin
  { if the start time is being set to greater than the end, then force the }
  { end to be one hour later than the start }
  if FRangeEnd < Value then
    FRangeEnd := TVpHours(Ord(Value) + 1);

  FRangeBegin := Value;
  SetStartTime((Ord(Value) * 60) / MinutesInDay);
end;
{=====}

procedure TVpTimeRange.SetRangeEnd(const Value: TVpHours);
begin
  { if the end time is being set to less than the start, then force the }
  { start to be one hour earlier than the end }
  if FRangeBegin > Value then
    FRangeBegin := TVpHours(Ord(Value) - 1);

  FRangeEnd := Value;
  SetEndTime((Ord(Value) * 60) / MinutesInDay);
end;
{=====}

procedure TVpTimeRange.SetEndTime(const Value: TDateTime);
begin
  if Value < StartTime then
    StartTime := Value - (30 / MinutesInDay);
  FEndTime := Value;

  if FOwner is TVpTimeSlotColor then
    (FOwner as TVpTimeSlotColor).Changed;
end;
{=====}

procedure TVpTimeRange.SetStartTime(const Value: TDateTime);
begin
  if Value > EndTime then
    EndTime := Value + (30 / MinutesInDay);
  FStartTime := Value;

  if FOwner is TVpTimeSlotColor then
    (FOwner as TVpTimeSlotColor).Changed;
end;
{=====}




(*****************************************************************************)
{ TVpTimeSlotColor }

constructor TVpTimeSlotColor.Create(AOwner: TVpCustomControl);
begin
  inherited Create;
  FOwner := AOwner;
  FActiveRange   := TVpTimeRange.Create(Self);
  FInactive := $0080FFFF;
  FHoliday  := $00FF80FF;
  FWeekend  := $00FFFF80;
  FActive   := clWhite;
  FWeekday  := clWhite;
end;
{=====}

destructor TVpTimeSlotColor.Destroy;
begin
  FActiveRange.Free;
  inherited;
end;
{=====}

procedure TVpTimeSlotColor.Changed;
begin
  FOwner.Invalidate;
end;
{=====}

procedure TVpTimeSlotColor.SetActive(const Value: TColor);
begin
  if FActive <> Value then begin
    FActive := Value;
    Changed;
  end;
end;
{=====}

procedure TVpTimeSlotColor.SetHoliday(const Value: TColor);
begin
  if FHoliday <> Value then begin
    FHoliday := Value;
    Changed;
  end;
end;
{=====}

procedure TVpTimeSlotColor.SetInactive(const Value: TColor);
begin
  if FInactive <> Value then begin
    FInactive := Value;
    Changed;
  end;
end;
{=====}

procedure TVpTimeSlotColor.SetWeekday(const Value: TColor);
begin
  if FWeekday <> Value then begin
    FWeekday := Value;
    Changed;
  end;
end;
{=====}

procedure TVpTimeSlotColor.SetWeekend(const Value: TColor);
begin
  if FWeekend <> Value then begin
    FWeekend := Value;
    Changed;
  end;
end;
{=====}

end.

