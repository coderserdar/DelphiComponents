{*********************************************************}
{*                  VPPRTFMT.PAS 1.03                    *}
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

{
  This unit contains everything there is to define Visual PlanIt print
  formats.  Print formats are bit complicated.  Before looking in this unit,
  read the documentation on the print formats.  It will help things make
  sense here.

  The print formats are built as nested TCollections.

  The TVpPrinter class contains a TCollection descendent (TVpPrintFormat) that
  contains all the print formats.  Each item in this collection
  (TVpPrintFormatItem) contains a TCollection descendent
  (TVpPrintFormatElement) that contains all of the elements (DayViews,
  WeekViews, static text and the like) that make up the specific print format.
  The print element is defined in the TVpPrintFormatElementItem class.

  Shape and Caption elements are special - They do not use a Visual PlanIt
  control to handle their rendering.  The TVpPrintShape and TVpPrintCaption
  are used to store captions and elements as well as render them.  Each
  print element has a shape and caption class defined, even if that class
  is not used.

  When printing, appropriate components are found on the form to handle the
  rendering (this allows the user to print what they see).  However, if the
  components cannot be found, or UseFormComponents if false), internally
  cached copies of all the components are used.

  When printing the component, the print formats use the RenderToCanvas
  method of the Visual PlanIt visual controls.  It is important that
  RenderToCanvas properly handles rotation and rendering to arbitrary
  rectangles.

  -----------------------------------------------------------------------------

  Enabling printing for a new component is fairly complex.  These steps should
  handle it:
  1) Add the new component to the TVpItemType enumeration
  2) Add an internal cached version of the component in the TVpPrinter's
     private section.  Expose this as a published property.  Create the
     component in CreateWorkControls and free it in DestroyWorkControls.
  3) Modify RenderItem inside of TVpPrinter.PaintToCanvasRect to
     set the LinkableControl to the cached component for the appropriate
     value of the TVpItemType enumeration.
  4) If the component is date base (calendar, dayview, weekview and the
     like), set HaveDate to true at the end of RenderItem in
     TVpPrinter.PaintToCanvasRect.  Other changes may be required in
     RenderItem.
  5) Modify TVpPrinter.SaveToFile to save the definition of this element
     in XML.
  6) Modify TVpPrinter.xmlPrintFormatStartElement to handle loading this
     element from an XML configuration file.
  Examine how the other components are integrated into the printing system.

  Of course, the print format editor should be updated to contain the new
  element.
}

{$I Vp.INC}

unit VpPrtFmt;

interface

uses
  Windows,
  Classes,
  Dialogs,
  SysUtils,
  Graphics,
  StdCtrls,
  VpBase,
  VpMisc,
  VpData,
  VpXParsr,
  VpCanvasUtils,
  VpSR,
  VpException,
  Forms,
  Printers;

type
  TVpChangeVar = (cvRemove, cvIgnore, cvChange);
  TVpDayUnits = (duDay, duWeek, duMonth, duYear);
  TVpShapeType = (ustRectangle, ustTopLine, ustBottomLine,
                   ustLeftLine, ustRightLine, ustTLToBRLine,
                   ustBLToTRLine, ustEllipse);

  TVpWatcher = record
    Handle : THandle;
  end;
  PVpWatcher = ^TVpWatcher;

  { TVpAttributes and TVpAttributeItem
    a collection of attributes collected when parsing the xml file.
    This is also used to store variables }

  TVpAttributes = class;

  TVpAttributeItem = class (TVpCollectionItem)
    private
      FCollection  : TVpAttributes;
      FName        : string;
      FValue       : string;

    protected

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

    published
      property Collection : TVpAttributes read FCollection write FCollection;
      property Name : string read FName write FName;
      property Value : string read FValue write FValue;
  end;

  TVpAttributes = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpAttributeItem;
      function  GetOwner : TPersistent; override;
      procedure SetItem (Index : Integer; Value : TVpAttributeItem);

    public
      constructor Create (AOwner : TPersistent);
      {$IFNDEF VERSION5}
      procedure Delete (Item : integer);
      {$ENDIF}
      property Items[Index : Integer] : TVpAttributeItem
               read GetItem write SetItem;

  end;

  { Print Formats }

  TVpPrintShape = class (TPersistent)
    private
      FOwner : TPersistent;
      FShape : TVpShapeType;
      FBrush : TBrush;
      FPen   : TPen;

    protected
      function  GetOwner : TPersistent; override;
      procedure SetBrush (const v : TBrush);
      procedure SetPen (const v : TPen);

    public
      constructor Create (AOwner : TPersistent);
      destructor  Destroy; override;

      procedure PaintToCanvas (ACanvas  : TCanvas;
                               ARect    : TRect;
                               Angle    : TVpRotationAngle;
                               Viewport : TRect);

    published
      property Brush : TBrush read FBrush write SetBrush;
      property Pen : TPen read FPen write SetPen;
      property Shape : TVpShapeType read FShape write FShape;
  end;

  TVpPrintCaption = class (TPersistent)
    private
      FOwner   : TPersistent;
      FCaption : string;
      FFont    : TFont;

    protected
      function  GetOwner : TPersistent; override;
      procedure SetFont (const v : TFont);

    public
      constructor Create (AOwner : TPersistent);
      destructor  Destroy; override;

      procedure PaintToCanvas (ACanvas    : TCanvas;
                               ARect      : TRect;
                               Angle      : TVpRotationAngle;
                               Viewport   : TRect;
                               RealString : string);

    published
      property Caption : string read FCaption write FCaption;
      property Font : TFont read FFont write SetFont;
  end;

  TVpPrintFormatElement = class;

  TVpPrintFormatElementItem = class (TVpCollectionItem)
    private
      FCollection     : TVpPrintFormatElement;

      FRotation       : TVpRotationAngle;
      FItemType       : TVpItemType;
      FMeasurement    : TVpItemMeasurement;
      FHeight         : Extended;
      FLeft           : Extended;
      FTop            : Extended;
      FWidth          : Extended;
      FDayOffset      : Integer;
      FDayOffsetUnits : TVpDayUnits;
      FElementName    : string;
      FShape          : TVpPrintShape;
      FCaption        : TVpPrintCaption;
      FVisible        : Boolean;

    protected
      function  GetDisplayName : string; override;
      procedure SetCaption (const v : TVpPrintCaption);
      procedure SetDayOffset (const v : Integer);
      procedure SetDayOffsetUnits (const v : TVpDayUnits);
      procedure SetElementName (const v : string);
      procedure SetHeight (const v : Extended);
      procedure SetItemType (const v : TVpItemType);
      procedure SetLeft (const v : Extended);
      procedure SetMeasurement (const v : TVpItemMeasurement);
      procedure SetRotation (const v : TVpRotationAngle);
      procedure SetShape (const v : TVpPrintShape);
      procedure SetTop (const v : Extended);
      procedure SetWidth (const v : Extended);
      procedure SetVisible (const v : Boolean);

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

      property Collection : TVpPrintFormatElement
               read FCollection write FCollection;
    published
      property Caption : TVpPrintCaption read FCaption write SetCaption;
      property DayOffset : Integer read FDayOffset write SetDayOffset;
      property DayOffsetUnits : TVpDayUnits
               read FDayOffsetUnits write SetDayOffsetUnits;
      property ElementName : string read FElementName write SetElementName;
      property Height : Extended read FHeight write SetHeight nodefault;
      property ItemType : TVpItemType read FItemType write SetItemType
               default itDayView;
      property Left : Extended read FLeft write SetLeft nodefault;
      property Measurement : TVpItemMeasurement
               read FMeasurement write SetMeasurement default imPercent;
      property Rotation : TVpRotationAngle read FRotation write SetRotation
               default ra0;
      property Shape : TVpPrintShape read FShape write SetShape;
      property Top : Extended read FTop write SetTop nodefault;
      property Width : Extended read FWidth write SetWidth nodefault;
      property Visible : Boolean read FVisible write SetVisible default True;

  end;

  TVpPrintFormatElement = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpPrintFormatElementItem;
      function  GetOwner : TPersistent; override;
      procedure NotifyAll (Item : TCollectionItem);
      {$IFDEF VERSION6}
      procedure Notify (Item   : TCollectionItem;
                        Action : TCollectionNotification); override; 
      {$ENDIF}
      procedure SetItem (Index : Integer; Value : TVpPrintFormatElementItem);
      procedure Update (Item : TCollectionItem); override;

    public
      constructor Create (AOwner : TPersistent);

      property Items[Index : Integer] : TVpPrintFormatElementItem
               read GetItem write SetItem;
  end;

  TVpPrintFormat = class;

  TVpPrintFormatItem = class (TVpCollectionItem)
    private
      FCollection  : TVpPrintFormat;
      FElements    : TVpPrintFormatElement;

      FFormatName  : string;
      FDescription : string;

      FDayInc      : Integer;
      FDayIncUnits : TVpDayUnits;
      FVisible     : Boolean;

    protected
      function  GetDisplayName : string; override;
      procedure SetDayInc (const v : Integer);
      procedure SetDayIncUnits (const v : TVpDayUnits);
      procedure SetDescription (const v : string);
      procedure SetElements (const v : TVpPrintFormatElement);
      procedure SetFormatName (const v : string);
      procedure SetVisible (const v : Boolean);

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

      property Collection : TVpPrintFormat
               read FCollection write FCollection;
    published
      property DayInc : Integer read FDayInc write SetDayInc;
      property DayIncUnits : TVpDayUnits read FDayIncUnits write SetDayIncUnits;
      property Description : string read FDescription write SetDescription;
      property Elements : TVpPrintFormatElement
               read FElements write SetElements;
      property FormatName : string read FFormatName write SetFormatName;
      property Visible : Boolean read FVisible write SetVisible default True;

  end;

  TVpPrintFormat = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpPrintFormatItem;
      function  GetOwner : TPersistent; override;
      procedure NotifyAll (Item : TCollectionItem);
      {$IFDEF VERSION6}
      procedure Notify (Item   : TCollectionItem;
                        Action : TCollectionNotification); override; 
      {$ENDIF}
      procedure SetItem (Index : Integer; Value : TVpPrintFormatItem);
      procedure Update (Item : TCollectionItem); override;

    public
      constructor Create (AOwner : TPersistent);

      property Items[Index : Integer] : TVpPrintFormatItem
               read GetItem write SetItem;
  end;

  TVpPrinter = class (TPersistent)
    private
      FOwner             : TPersistent;
      FPrintFormats      : TVpPrintFormat;
      FCurFormat         : Integer;
      FAttributes        : TVpAttributes;
      FLoadingIndex      : Integer;
      FElementIndex      : Integer;
      FVariables         : TVpAttributes;
      FDayStart          : TVpHours;
      FDayEnd            : TVpHours;
      FGranularity       : TVpGranularity;
      FPrintJob          : Boolean;
      FHaveDate          : Boolean;
      FHaveTaskList      : Boolean;
      FLastTask          : Integer;
      FHaveContactGrid   : Boolean;
      FLastContact       : Integer;
      FLeftMargin        : Extended;
      FRightMargin       : Extended;
      FTopMargin         : Extended;
      FBottomMargin      : Extended;
      FMarginUnits       : TVpItemMeasurement;
      FUseFormComponents : Boolean;
      { Work copies of all the components - used if the components cannot
        be located when printing }
      FParentHandle      : THandle;
      FDayView           : TComponent;
      FWeekView          : TComponent;
      FMonthView         : TComponent;
      FCalendar          : TComponent;
      FContactGrid       : TComponent;
      FTaskList          : TComponent;
      { Notification Handles }
      FNotifiers         : TList;
      FDefaultXMLFileName: string;                                     

    protected
      procedure CreateWorkControls;
      procedure DestroyWorkControls;
      function  GetOwner : TPersistent; override;
      function  ReplaceVariables (const s : string) : string;
      procedure SetBottomMargin (const v : Extended);
      procedure SetCurFormat (const v : Integer);
      procedure SetDefaultXMLFileName (const v : string);              
      procedure SetLeftMargin (const v : Extended);
      procedure SetMarginUnits (const v : TVpItemMeasurement);
      procedure SetPrintFormats (const v : TVpPrintFormat);
      procedure SetRightMargin (const v : Extended);
      procedure SetTopMargin (const v : Extended);
      procedure SetUseFormComponents (const v : Boolean);
      procedure xmlPrintFormatAttribute (oOwner     : TObject;
                                         sName,
                                         sValue     : DOMString;
                                         bSpecified : Boolean);
      procedure xmlPrintFormatEndElement (oOwner : TObject;
                                          sValue : DOMString);
      procedure xmlPrintFormatStartElement (oOwner : TObject;
                                            sValue : DOMString);


    public
      constructor Create (AOwner : TComponent);
      destructor  Destroy; override;

      procedure AddDefaultVariables (Date : TDateTime);
      procedure AddVariable (VarName : string; Value : string);
      procedure ChangeVariable (VarName : string; NewValue : string);
      procedure CheckPrintFormat;                                                                                                                       
      procedure ClearVariables;
      function  DeleteVariable (VarName : string) : Boolean;
      procedure DeregisterAllWatchers;
      procedure DeregisterWatcher (Watcher : THandle);
      function  Find (const v : string) : Integer;
      function  HaveVariable (VarName : string) : Boolean;
      procedure LoadFromFile (FileName : string; Append : Boolean);    
      function  LookupVariable (VarName : string) : string;
      procedure NotifyLinked;
      procedure PaintToCanvasRect (ACanvas : TCanvas;
                                   ARect   : TRect;
                                   ADate   : TDateTime);
      procedure Print (APrinter      : TPrinter;
                       StartDate    : TDateTime;
                       EndDate      : TDateTime);
      procedure RegisterWatcher (Watcher : THandle);
      procedure RenderPage (    ACanvas      : TCanvas;
                                ARect        : TRect;
                                PageNum      : Integer;
                            var ADate        : TDateTime;
                                EndDate      : TDateTime;
                            var StartContact : Integer;
                            var StartTask    : Integer;
                            var LastPage     : Boolean);
      procedure SaveToFile (FileName : string);                        
      procedure UpdateDateVariables (Date : TDateTime);
      function ValidFormat (const v : Integer) : Boolean;


      property Calendar : TComponent read FCalendar write FCalendar;
      property ContactGrid : TComponent read FContactGrid write FContactGrid;
      property CurFormat : Integer read FCurFormat write SetCurFormat;
      property DayView : TComponent read FDayView write FDayView;
      property DefaultXMLFileName : string                             
               read FDefaultXMLFileName write SetDefaultXMLFileName;   
      property HaveDate : Boolean read FHaveDate;
      property HaveTaskList : Boolean read FHaveTaskList;
      property LastTask : Integer read FLastTask;
      property HaveContactGrid : Boolean read FHaveContactGrid;
      property LastContact : Integer read FLastContact;
      property MonthView : TComponent read FMonthView write FMonthView;
      property Printing : Boolean read FPrintJob;
      property TaskList : TComponent read FTaskList write FTaskList;
      property UseFormComponents : Boolean
               read FUseFormComponents write SetUseFormComponents default True;
      property WeekView : TComponent read FWeekView write FWeekView;

    published
      property BottomMargin : Extended
               read FBottomMargin write SetBottomMargin;
      property DayStart : TVpHours read FDayStart Write FDayStart;
      property DayEnd : TVpHours read FDayEnd write FDayEnd;
      property Granularity : TVpGranularity
               read FGranularity write FGranularity;
      property LeftMargin : Extended
               read FLeftMargin write SetLeftMargin;
      property MarginUnits : TVpItemMeasurement
               read FMarginUnits write SetMarginUnits default imInches;
      property PrintFormats : TVpPrintFormat
               read FPrintFormats write SetPrintFormats;
      property RightMargin : Extended
               read FRightMargin write SetRightMargin;
      property TopMargin : Extended
               read FTopMargin write SetTopMargin;
  end;

implementation

uses
  VpBaseDS,
  VpPrtFmtCBox,
  VpPrtPrv,
  VpDayView,
  VpWeekView,
  VpMonthView,
  VpCalendar,
  VpTaskList,
  VpContactGrid;

function XMLizeString (const s : string) : string;
var
  i : integer;
begin
  result := '';
  for i := 1 to Length (s) do
    case s[i] of
      '<' : result := result + '&lt;';
      '>' : result := result + '&gt;';
      {' ' : result := result + '&nbsp;';}
      '&' : result := result + '&amp;';
      '"' : result := result + '&quot;';
      else
        result := result + copy (s, i, 1);
    end;
end;

// TVpAttributeItem **********************************************************

constructor TVpAttributeItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TVpAttributes.Create (TVpAttributes (Collection).FOwner);

  FName  := '';
  FValue := '';
end;
{=====}

destructor TVpAttributeItem.Destroy;
begin
  FCollection.Free;
  FCollection := nil;

  inherited Destroy;
end;
{=====}



// TVpAttributes *************************************************************

constructor TVpAttributes.Create(AOwner : TPersistent);
begin
  inherited Create (TVpAttributeItem);
  FOwner := AOwner;
end;
{=====}

{$IFNDEF VERSION5}
procedure TVpAttributes.Delete(Item: integer);
begin
  GetItem(Item).Free;
end;
{=====}
{$ENDIF}

function TVpAttributes.GetItem (Index : Integer) : TVpAttributeItem;
begin
  Result := TVpAttributeItem (inherited GetItem (Index));
end;
{=====}

function TVpAttributes.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpAttributes.SetItem (Index : Integer; Value : TVpAttributeItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}



// TVpPrintShape *************************************************************

constructor TVpPrintShape.Create (AOwner : TPersistent);
begin
  inherited Create;

  FOwner := AOwner;
  FPen   := TPen.Create;
  FBrush := TBrush.Create;
  FShape := ustRectangle;
end;
{=====}

destructor TVpPrintShape.Destroy;
begin
  FPen.Free;
  FPen := nil;
  FBrush.Free;
  FBrush := nil;

  inherited Destroy;
end;
{=====}

function TVpPrintShape.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpPrintShape.PaintToCanvas (ACanvas  : TCanvas;
                                       ARect    : TRect;
                                       Angle    : TVpRotationAngle;
                                       Viewport : TRect);
var
  OldPen   : TPen;
  OldBrush : TBrush;

begin

  OldPen := TPen.Create;
  try
    OldBrush := TBrush.Create;
    try
      OldPen.Assign (ACanvas.Pen);
      OldBrush.Assign (ACanvas.Brush);
      case FShape of
        ustRectangle   :
        {$IFDEF VERSION5}
        ACanvas.Rectangle (ARect);
        {$ELSE}
        ACanvas.Rectangle (ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
        {$ENDIF}
        ustTopLine     : begin
          ACanvas.MoveTo (ARect.Left,  ARect.Top);
          ACanvas.LineTo (ARect.Right, ARect.Top);
        end;
        ustBottomLine  : begin
          ACanvas.MoveTo (ARect.Left,  ARect.Bottom);
          ACanvas.LineTo (ARect.Right, ARect.Bottom);
        end;
        ustLeftLine    : begin
          ACanvas.MoveTo (ARect.Left,  ARect.Top);
          ACanvas.LineTo (ARect.Left,  ARect.Bottom);
        end;
        ustRightLine   : begin
          ACanvas.MoveTo (ARect.Right, ARect.Top);
          ACanvas.LineTo (ARect.Right, ARect.Bottom);
        end;
        ustTLToBRLine  : begin
          ACanvas.MoveTo (ARect.Left,  ARect.Top);
          ACanvas.LineTo (ARect.Right, ARect.Bottom);
        end;
        ustBLToTRLine  : begin
          ACanvas.MoveTo (ARect.Left,  ARect.Bottom);
          ACanvas.LineTo (ARect.Right, ARect.Top);
        end;
        ustEllipse     :
          ACanvas.Ellipse (ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      end;
      ACanvas.Pen.Assign (OldPen);
      ACanvas.Brush.Assign (OldBrush);
    finally
      OldBrush.Free;
    end;
  finally
    OldPen.Free;
  end;
end;
{=====}

procedure TVpPrintShape.SetBrush (const v : TBrush);
begin
  FBrush.Assign (v);
end;
{=====}

procedure TVpPrintShape.SetPen (const v : TPen);
begin
  FPen.Assign (v);
end;
{=====}



// TVpPrintCaption ***********************************************************

constructor TVpPrintCaption.Create (AOwner : TPersistent);
begin
  inherited Create;

  FOwner   := AOwner;
  FFont    :=  TFont.Create;
  FCaption := '';
end;
{=====}

destructor TVpPrintCaption.Destroy;
begin
  FFont.Free;
  FFont := nil;

  inherited Destroy;
end;
{=====}

function TVpPrintCaption.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpPrintCaption.PaintToCanvas (ACanvas    : TCanvas;
                                          ARect      : TRect;
                                          Angle      : TVpRotationAngle;
                                          Viewport   : TRect;
                                          RealString : string);
var
  OldFont  : TFont;

begin
  OldFont := ACanvas.Font;
  ACanvas.Font := FFont;
  try
    TPSTextOutAtPoint (ACanvas, Angle, Viewport, ARect.Left, ARect.Top, RealString);
  finally
    ACanvas.Font := OldFont;
  end;
end;
{=====}

procedure TVpPrintCaption.SetFont (const v : TFont);
begin
  FFont.Assign (v);
end;
{=====}



// TVpPrintFormatElementItem *************************************************

constructor TVpPrintFormatElementItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TVpPrintFormatElement.Create (TVpPrintFormatElement (Collection).FOwner);

  FShape   := TVpPrintShape.Create (Self);
  FCaption := TVpPrintCaption.Create (Self);

  FRotation       := ra0;
  FElementName    := '';
  FItemType       := itDayView;
  FMeasurement    := imPercent;
  FHeight         := 100;
  FLeft           := 0;
  FTop            := 0;
  FWidth          := 100;
  FDayOffset      := 0;
  FDayOffsetUnits := duDay;
  FVisible        := True;
end;
{=====}

destructor TVpPrintFormatElementItem.Destroy;
begin
  FCollection.Free;
  FCollection := nil;

  FShape.Free;
  FShape := nil;
  FCaption.Free;
  FCaption := nil;

  inherited Destroy;
end;
{=====}

function TVpPrintFormatElementItem.GetDisplayName : string;
begin
  if FElementName <> '' then
    Result := '(' + FElementName + ') ' + inherited GetDisplayName
  else
    Result := inherited GetDisplayName;
end;
{=====}

procedure TVpPrintFormatElementItem.SetCaption (const v : TVpPrintCaption);
begin
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    try
      FCaption.Assign (v);
    finally
      FCollection.EndUpdate;
    end;
  end else
    FCaption.Assign (v);
end;
{=====}

procedure TVpPrintFormatElementItem.SetDayOffset (const v : Integer);
begin
  if v = FDayOffset then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FDayOffset := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FDayOffset := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetDayOffsetUnits (const v : TVpDayUnits);
begin
  if v = FDayOffsetUnits then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FDayOffsetUnits := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FDayOffsetUnits := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetElementName (const v : string);
begin
  if v = FElementName then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FElementName := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FElementName := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetHeight (const v : Extended);
begin
  if v = FHeight then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FHeight := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FHeight := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetItemType (const v : TVpItemType);
begin
  if v = FItemType then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FItemType := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FItemType := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetLeft (const v : Extended);
begin
  if v = FLeft then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FLeft := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FLeft := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetMeasurement (const v : TVpItemMeasurement);
begin
  if v = FMeasurement then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FMeasurement := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FMeasurement := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetRotation (const v : TVpRotationAngle);
begin
  if v = FRotation then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FRotation := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FRotation := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetShape (const v : TVpPrintShape);
begin
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    try
      FShape.Assign (v);
    finally
      FCollection.EndUpdate;
    end;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FShape.Assign (v);
end;
{=====}

procedure TVpPrintFormatElementItem.SetTop (const v : Extended);
begin
  if v = FTop then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FTop := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then begin
      Collection.NotifyAll (Self);
    end;
  end else
    FTop := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetWidth (const v : Extended);
begin
  if v = FWidth then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FWidth := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FWidth := v;
end;
{=====}

procedure TVpPrintFormatElementItem.SetVisible (const v : Boolean);
begin
  if v = FVisible then
    Exit;

  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FVisible := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FVisible := v;
end;
{=====}

// TVpPrintFormatElement *****************************************************

constructor TVpPrintFormatElement.Create(AOwner : TPersistent);
begin
  inherited Create (TVpPrintFormatElementItem);
  FOwner := AOwner;
end;
{=====}

function TVpPrintFormatElement.GetItem (Index : Integer) : TVpPrintFormatElementItem;
begin
  Result := TVpPrintFormatElementItem (inherited GetItem (Index));
end;

function TVpPrintFormatElement.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpPrintFormatElement.NotifyAll (Item : TCollectionItem);
var
  Notifier : TPersistent;

begin
  if not Assigned (FOwner) then
    Exit;

  if FOwner is TVpPrintFormatItem then begin
    if Assigned (TVpPrintFormatItem (FOwner).FCollection) then
      TVpPrintFormatItem (FOwner).FCollection.NotifyAll ((TVpPrintFormatItem (FOwner)));
  end;

  if FOwner is TVpPrintFormatItem then
    Notifier := (FOwner as TVpPrintFormatItem).GetOwner
  else if FOwner is TVpPrintFormat then
    Notifier := (FOwner as TVpPrintFormat).GetOwner
  else
    Notifier := nil;

  if not Assigned (Notifier) then
    Exit;

  if Notifier is TVpPrintFormat then
    Notifier := (Notifier as TVpPrintFormat).GetOwner;

  if Notifier is TVpPrinter then
    (Notifier as TVpPrinter).NotifyLinked
  else if Notifier is TVpControlLink then begin
    if not Assigned ((Notifier as TVpControlLink).Printer) then
      Exit;
    (Notifier as TVpControlLink).Printer.NotifyLinked;
  end;                                
end;
{=====}

{$IFDEF VERSION6}
procedure TVpPrintFormatElement.Notify (Item   : TCollectionItem;
                                        Action : TCollectionNotification);

begin
  inherited Notify (Item, Action);

  NotifyAll (Item);
end;
{$ENDIF}
{=====}

procedure TVpPrintFormatElement.SetItem (Index : Integer;
                                         Value : TVpPrintFormatElementItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}

procedure TVpPrintFormatElement.Update (Item : TCollectionItem);
var
  Notifier : TPersistent;

begin
  inherited Update (Item);

  if not Assigned (FOwner) then
    Exit;

  if FOwner is TVpPrintFormatItem then
    Notifier := (FOwner as TVpPrintFormatItem).GetOwner
  else if FOwner is TVpPrintFormat then
    Notifier := (FOwner as TVpPrintFormat).GetOwner
  else
    Notifier := nil;

  if not Assigned (Notifier) then
    Exit;

  if Notifier is TVpPrintFormat then 
    Notifier := (Notifier as TVpPrintFormat).GetOwner;

  if Notifier is TVpPrinter then
    (Notifier as TVpPrinter).NotifyLinked
  else if Notifier is TVpControlLink then begin
    if not Assigned ((Notifier as TVpControlLink).Printer) then
      Exit;
    (Notifier as TVpControlLink).Printer.NotifyLinked;
  end;
end;
{=====}

// TVpPrintFormatItem *************************************************

constructor TVpPrintFormatItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);

  FCollection := TVpPrintFormat.Create (TVpPrintFormat (Collection).FOwner);

  FElements := TVpPrintFormatElement.Create (Self);

  FFormatName  := 'Unknown';
  FDescription := '';
  FDayInc      := 0;
  FDayIncUnits := duDay;
  FVisible     := True;
end;
{=====}

destructor TVpPrintFormatItem.Destroy;
begin
  FElements.Free;
  FElements := nil;

  FCollection.Free;
  FCollection := nil;

  inherited Destroy;
end;
{=====}

function TVpPrintFormatItem.GetDisplayName : string;
begin
  if FFormatName <> '' then
    Result := '(' + FFormatName + ') ' + inherited GetDisplayName
  else
    Result := inherited GetDisplayName;
end;
{=====}

procedure TVpPrintFormatItem.SetDayInc (const v : Integer);
begin
  if v = FDayInc then
    Exit;
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FDayInc := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll  (Self);
  end else
    FDayInc := v;
end;
{=====}

procedure TVpPrintFormatItem.SetDayIncUnits (const v : TVpDayUnits);
begin
  if v = FDayIncUnits then
    Exit;
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FDayIncUnits := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FDayIncUnits := v;
end;
{=====}

procedure TVpPrintFormatItem.SetDescription (const v : string);
begin
  if v = FDescription then
    Exit;
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FDescription := v;
    FCollection.EndUpdate;
  end else
    FDescription := v;
end;
{=====}

procedure TVpPrintFormatItem.SetElements (const v : TVpPrintFormatElement);
begin
  FElements.Assign (v);
  if Assigned (Collection) then
    Collection.NotifyAll (Self);
end;
{=====}

procedure TVpPrintFormatItem.SetFormatName (const v : string);
begin
  if v = '' then
    raise EVpPrintFormatError.Create (RSNeedFormatName); 
  if v = FFormatName then
    Exit;
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FFormatName := v;
    FCollection.EndUpdate;
  end else
    FFormatName := v;
end;
{=====}

procedure TVpPrintFormatItem.SetVisible (const v : Boolean);
begin
  if v = FVisible then
    Exit;
  if Assigned (FCollection) then begin
    FCollection.BeginUpdate;
    FVisible := v;
    FCollection.EndUpdate;
    if Assigned (Collection) then
      Collection.NotifyAll (Self);
  end else
    FVisible := v;
end;
{=====}



// TVpPrintFormat ************************************************************

constructor TVpPrintFormat.Create (AOwner : TPersistent);
begin
  inherited Create (TVpPrintFormatItem);

  FOwner := AOwner;
end;
{=====}

function TVpPrintFormat.GetItem (Index : Integer) : TVpPrintFormatItem;
begin
  Result := TVpPrintFormatItem (inherited GetItem (Index));
end;
{=====}

function TVpPrintFormat.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpPrintFormat.NotifyAll (Item : TCollectionItem);
begin
  if not Assigned (FOwner) then
    Exit;

  if FOwner is TVpPrinter then
    (FOwner as TVpPrinter).NotifyLinked
  else if FOwner is TVpControlLink then begin
    if not Assigned ((FOwner as TVpControlLink).Printer) then
      Exit;
    (FOwner as TVpControlLink).Printer.NotifyLinked;
  end;
end;
{=====}

{$IFDEF VERSION6}
procedure TVpPrintFormat.Notify (Item   : TCollectionItem;
                                  Action : TCollectionNotification);
begin
  inherited Notify (Item, Action);

  NotifyAll (Item);
end;
{$ENDIF}
{=====}

procedure TVpPrintFormat.SetItem (Index : Integer;
                                          Value : TVpPrintFormatItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}

procedure TVpPrintFormat.Update (Item : TCollectionItem);
begin
  inherited Update (Item);

  if not Assigned (FOwner) then
    Exit;

  if FOwner is TVpPrinter then
    (FOwner as TVpPrinter).NotifyLinked
  else if FOwner is TVpControlLink then begin
    if not Assigned ((FOwner as TVpControlLink).Printer) then
      Exit;
    (FOwner as TVpControlLink).Printer.NotifyLinked;
  end;
end;
{=====}

// TVpPrinter ****************************************************************

constructor TVpPrinter.Create (AOwner : TComponent);
begin
  inherited Create;

  FPrintJob          := False;

  FPrintFormats      := TVpPrintFormat.Create (AOwner);
  FAttributes        := TVpAttributes.Create (Self);
  FVariables         := TVpAttributes.Create (Self);
  FNotifiers         := TList.Create;

  FOwner             := AOwner;

  FLoadingIndex      := -1;
  FElementIndex      := -1;
  FDayStart          := h_08;
  FDayEnd            := h_05;
  FGranularity       := gr30Min;
  FUseFormComponents := True;

  CreateWorkControls;
end;
{=====}

destructor TVpPrinter.Destroy;
begin
  DeregisterAllWatchers;
  FPrintFormats.Free;
  FAttributes.Free;
  FVariables.Free;
  FNotifiers.Free;
  DestroyWorkControls;
  inherited;
end;
{=====}

procedure TVpPrinter.AddDefaultVariables (Date : TDateTime);
  function HourToStr (Hour : TVpHours; Mil : Boolean) : string;
  begin
    if Mil then
      Result := IntToStr(ord(Hour))
    else
      case Hour of
        h_00 : Result := '12';
        h_01 : Result := '1';
        h_02 : Result := '2';
        h_03 : Result := '3';
        h_04 : Result := '4';
        h_05 : Result := '5';
        h_06 : Result := '6';
        h_07 : Result := '7';
        h_08 : Result := '8';
        h_09 : Result := '9';
        h_10 : Result := '10';
        h_11 : Result := '11';
        h_12 : Result := '12';
        h_13 : Result := '1';
        h_14 : Result := '2';
        h_15 : Result := '3';
        h_16 : Result := '4';
        h_17 : Result := '5';
        h_18 : Result := '6';
        h_19 : Result := '7';
        h_20 : Result := '8';
        h_21 : Result := '9';
        h_22 : Result := '10';
        h_23 : Result := '11';
      end;
  end;

  function GranularityToStr (Gran : TVpGranularity) : string;
  begin
    Result := '';
    case Gran of
      gr60Min : Result := '60';
      gr30Min : Result := '30';
      gr20Min : Result := '20';
      gr15Min : Result := '15';
      gr10Min : Result := '10';
      gr06Min  : Result := '6';
      gr05Min  : Result := '5';
    end;
  end;

  function HourToAMPM (Hour : TVpHours) : string;
  begin
    if (Hour >= H_00) and (Hour <= h_11) then
      Result := 'AM'
    else
      Result := 'PM';
  end;

  procedure AddDataStoreVars;
  var
    DataStore : TVpCustomDataStore;
    i         : Integer;
    TopLevel  : TComponent;

  begin
    if not Assigned (FOwner) then
      Exit;
    if not (FOwner is TVpControlLink) then
      Exit;
    TopLevel := (FOwner as TVpControlLink).Owner;
    if not Assigned (TopLevel) then
      Exit;

    DataStore := nil;

    for i := 0 to pred (TopLevel.ComponentCount) do
      if (TopLevel.Components[i] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore (TopLevel.Components[i]);
      end;

    if Assigned (DataStore) then begin
      AddVariable ('ResourceID', IntToStr (DataStore.ResourceID));
      if Assigned (DataStore.Resource) then begin
        AddVariable ('Resource', DataStore.Resource.Description);
        AddVariable ('ResourceNotes', DataStore.Resource.Notes);
      end;
    end;
  end;

begin
  { Variables for the date }
  UpdateDateVariables (Date);

  { Variables for the starting name }
  AddVariable ('StartHour12', HourToStr (FDayStart, False));
  AddVariable ('StartHour24', HourToStr (FDayStart, True));
  AddVariable ('StartHourAMPM', HourToAMPM (FDayStart));

  { Variables for the ending time }
  AddVariable ('StopHour12', HourToStr (FDayEnd, False));
  AddVariable ('StopHour24', HourToStr (FDayEnd, False));
  AddVariable ('StopHourAMPM', HourToAMPM (FDayEnd));

  { Variables for granularity }
  AddVariable ('Granularity', GranularityToStr (Granularity));

  AddDataStoreVars;
end;
{=====}

procedure TVpPrinter.AddVariable (VarName : string; Value : string);
var
  i      : Integer;
  NewVar : TVpAttributeItem;

begin
  for i := 0 to FVariables.Count - 1 do
    if FVariables.Items[i].Name = VarName then begin
      FVariables.Items[i].Value := Value;
      Exit;
    end;
  NewVar := TVpAttributeItem (FVariables.Add);
  NewVar.Name := VarName;
  NewVar.Value := Value;
end;
{=====}

procedure TVpPrinter.ChangeVariable (VarName : string; NewValue : string);
begin
  AddVariable (VarName, NewValue);
end;
{=====}

procedure TVpPrinter.CheckPrintFormat;                                   
begin                                                                    
  if PrintFormats.Count = 0 then                                         
    raise EVpPrintFormatError.Create (RSNoPrintFormats)                  
  else if (CurFormat < 0) or                                             
              (CurFormat >= PrintFormats.Count) then                     
    raise EVpPrintFormatError.Create (RSBadPrintFormat);                 
end;                                                                     
{=====}

procedure TVpPrinter.ClearVariables;
begin
  FVariables.Clear;
end;
{=====}

procedure TVpPrinter.CreateWorkControls;
begin
  FParentHandle := AllocateHWnd (nil);
  FDayView      := TVpDayView.CreateParented     (FParentHandle);
  FWeekView     := TVpWeekView.CreateParented    (FParentHandle);
  FMonthView    := TVpMonthView.CreateParented   (FParentHandle);
  FCalendar     := TVpCalendar.CreateParented    (FParentHandle); 
  FContactGrid  := TVpContactGrid.CreateParented (FParentHandle);
  FTaskList     := TVpTaskList.CreateParented    (FParentHandle);
end;
{=====}

procedure TVpPrinter.DestroyWorkControls;

begin
  DeallocateHWnd (FParentHandle);

  FDayView.Free;                                                       
  FWeekView.Free;                                                      
  FMonthView.Free;                                                     
  FCalendar.Free;                                                      
  FContactGrid.Free;                                                   
  FTaskList.Free;                                                      
end;
{=====}

function TVpPrinter.DeleteVariable (VarName : string) : Boolean;
var
  i : Integer;

begin
  Result := True;
  for i := 0 to FVariables.Count - 1 do
    if FVariables.Items[i].Name = VarName then begin
      FVariables.Delete(i);
      Exit;
    end;
  Result := False;
end;
{=====}

procedure TVpPrinter.DeregisterAllWatchers;
var
  i : Integer;

begin
  for i := FNotifiers.Count - 1 downto 0 do
    if Assigned (FNotifiers[i]) then begin
      FreeMem (FNotifiers[i]);
      FNotifiers.Delete (i);
    end;
end;
{=====}

procedure TVpPrinter.DeregisterWatcher (Watcher : THandle);
var
  i : Integer;

begin
  for i := FNotifiers.Count - 1 downto 0 do
    if Assigned (FNotifiers[i]) then
      if PVpWatcher (FNotifiers[i]).Handle = Watcher then begin
        FreeMem (FNotifiers[i]);
        FNotifiers.Delete (i);
        Exit;
      end;
end;
{=====}

function TVpPrinter.HaveVariable (VarName : string) : Boolean;
var
  i : Integer;

begin
  Result := True;
  for i := 0 to FVariables.Count - 1 do
    if FVariables.Items[i].Name = VarName then
      Exit;
  Result := False;
end;
{=====}

function TVpPrinter.Find (const v : string) : Integer;
var
  i : Integer;

begin
  Result := -1;
  for i := 0 to FPrintFormats.Count - 1 do
    if v = FPrintFormats.Items[i].FormatName then begin
      Result := i;
      Exit;
    end;
end;
{=====}

function TVpPrinter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpPrinter.LoadFromFile (FileName : string;                  
                                   Append   : Boolean);
var
  Parser : TVpParser;

begin
  if FileName = '' then                                                
    FileName := DefaultXMLFileName;                                    

  if not Append then
    FPrintFormats.Clear;

  FLoadingIndex := -1;
  FElementIndex := -1;
  Parser := TVpParser.Create (nil);
  Parser.OnAttribute := xmlPrintFormatAttribute;
  Parser.OnStartElement := xmlPrintFormatStartElement;
  Parser.OnEndElement := xmlPrintFormatEndElement;
  try
    Parser.ParseDataSource (FileName);
  finally
    Parser.Free;
  end;
  FLoadingIndex := -1;
  FElementIndex := -1;
  NotifyLinked;
end;
{=====}

function TVpPrinter.LookupVariable (VarName : string) : string;
var
  i : Integer;

begin
  Result := '';
  for i := 0 to FVariables.Count - 1 do
    if FVariables.Items[i].Name = VarName then begin
      Result := FVariables.Items[i].Value;
      Break;
    end;
end;
{=====}

procedure TVpPrinter.NotifyLinked;
var
  i        : Integer;
begin
  for i := 0 to FNotifiers.Count - 1 do
    if Assigned (FNotifiers[i]) then
      PostMessage (PVpWatcher (FNotifiers[i]).Handle,
                                Vp_PrintFormatChanged, 0, 0);
end;
{=====}

procedure TVpPrinter.PaintToCanvasRect (ACanvas : TCanvas;
                                         ARect   : TRect;
                                         ADate   : TDateTime);

var
  WidthInPixels  : Integer;
  HeightInPixels : Integer;
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;
  StartX         : Integer;
  StartY         : Integer;
  StopX          : Integer;
  StopY          : Integer;
  Scale          : Extended;
  StartLine      : Integer;
  EndLine        : Integer;

  procedure GetMeasurements;
  begin
    WidthInPixels  := ARect.Right - ARect.Left;
    HeightInPixels := ARect.Bottom - ARect.Top;

    PixelsPerInchX := GetDeviceCaps (ACanvas.Handle, LOGPIXELSX);
    PixelsPerInchY := GetDeviceCaps (ACanvas.Handle, LOGPIXELSY);

    Scale          := PixelsPerInchY / Screen.PixelsPerInch;

    StartLine      := HourToLine (DayStart, Granularity);
    EndLine        := HourToLine (DayEnd, Granularity);
  end;

  procedure GetPrintRectangle (Element : TVpPrintFormatElementItem);
  begin
    case Element.Measurement of
      imAbsolutePixel : begin
        StartX := Round (Element.Left);
        StartY := Round (Element.Top);
        StopX  := Round (Element.Left + Element.Width);
        StopY  := Round (Element.Top + Element.Height);
      end;

      imPercent : begin
        StartX := Round (Element.Left * WidthInPixels  / 100);
        StartY := Round (Element.Top  * HeightInPixels / 100);
        StopX  := Round ((Element.Left + Element.Width)  * WidthInPixels  / 100);
        StopY  := Round ((Element.Top  + Element.Height) * HeightInPixels / 100);
      end;

      imInches : begin
        StartX := Round (Element.Left * PixelsPerInchX);
        StartY := Round (Element.Top * PixelsPerInchX);
        StopX  := Round ((Element.Left + Element.Width) * PixelsPerInchX);
        StopY  := Round ((Element.Top + Element.Height) * PixelsPerInchX);
      end;
    end;

    Inc (StartX, ARect.Left);
    Inc (StartY, ARect.Top);
    Inc (StopX,  ARect.Left);
    Inc (StopY,  ARect.Top);
  end;

  function GetDate (Element : TVpPrintFormatElementItem) : TDateTime;
  begin
    Result := ADate;
    if Element.DayOffset <> 0 then begin
      case Element.DayOffsetUnits of
        duDay   : Result := Result + Element.DayOffset;
        duWeek  : Result := Result + Element.DayOffset * 7;
        duMonth : Result := IncMonth (Result, Element.DayOffset);
        duYear  : Result := IncYear (Result, Element.DayOffset);
      end;
    end;
  end;

  procedure RenderItem (Element : TVpPrintFormatElementItem);
  var
    i             : Integer;
    DI            : TVpDependentInfo;
    DependentList : TList;
    RenderControl : TVpLinkableControl;

  begin
    if not Element.Visible then
      Exit;

    RenderControl := nil;
    DependentList := (FOwner as TVpControlLink).GetDependentList;
    if FUseFormComponents then
      for i := 0 to DependentList.Count - 1 do begin
        DI := TVpDependentInfo (DependentList.List^[I]);
        if TVpLinkableControl (DI.Component).GetControlType = Element.ItemType then begin
          RenderControl := TVpLinkableControl (DI.Component);
          Break;
        end;
      end;

    if not Assigned (RenderControl) then begin
      case Element.ItemType of
        itDayView     : RenderControl := TVpLinkableControl (FDayView);
        itWeekView    : RenderControl := TVpLinkableControl (FWeekView);
        itMonthView   : RenderControl := TVpLinkableControl (FMonthView);
        itCalendar    : RenderControl := TVpLinkableControl (FCalendar);
        itContacts    : RenderControl := TVpLinkableControl (FContactGrid);
        itTasks       : RenderControl := TVpLinkableControl (FTaskList);
      end;
      if FOwner is TVpControlLink then
        RenderControl.DataStore := (FOwner as TVPControlLink).DataStore;
    end;

    if Assigned (RenderControl) then
      case Element.ItemType of
        itTasks    : begin
          FHaveTaskList := True;
          RenderControl.RenderToCanvas (ACanvas,
                               Rect (StartX, StartY, StopX, StopY),
                               Element.Rotation,
                               Scale,
                               GetDate (Element),
                               FLastTask,
                               EndLine,
                               Granularity,
                               True);
          FLastTask := RenderControl.GetLastPrintLine;
        end;

        itContacts : begin
          FHaveContactGrid := True;
          RenderControl.RenderToCanvas (ACanvas,
                               Rect (StartX, StartY, StopX, StopY),
                               Element.Rotation,
                               Scale,
                               GetDate (Element),
                               FLastContact,
                               EndLine,
                               Granularity,
                               True);
          FLastContact := RenderControl.GetLastPrintLine;
        end;

        else
          RenderControl.RenderToCanvas (ACanvas,
                               Rect (StartX, StartY, StopX, StopY),
                               Element.Rotation,
                               Scale,
                               GetDate (Element),
                               StartLine,
                               EndLine,
                               Granularity,
                               True);
      end;

      case Element.ItemType of
        itDayView,
        itMonthView,
        itWeekView,
        itCalendar   : FHaveDate := True;
      end;
  end;

var
  i : Integer;

begin
  CheckPrintFormat;                                                      
                                                                                 
  if not (FPrintJob) then begin
    FLastTask := 0;
    FLastContact := 0;
  end;

  AddDefaultVariables (ADate);

  if not (FOwner is TVpControlLink) then
    raise EVpPrintFormatError.Create (RSPrtControlOwner);

  GetMeasurements;

  if not ValidFormat (CurFormat) then
    raise EVpPrintFormatError.Create (RSBadPrintFormat + IntToStr (CurFormat)); 

  for i := 0 to FPrintFormats.Items[CurFormat].Elements.Count - 1 do begin
    GetPrintRectangle (FPrintFormats.Items[CurFormat].Elements.Items[i]);

    if FPrintFormats.Items[CurFormat].Elements.Items[i].ItemType = itCaption then begin
      if FPrintFormats.Items[CurFormat].Elements.Items[i].Visible then begin
        UpdateDateVariables (GetDate (FPrintFormats.Items[CurFormat].Elements.Items[i]));
        ACanvas.Font.Assign (FPrintFormats.Items[CurFormat].Elements.Items[i].FCaption.Font);
        FPrintFormats.Items[CurFormat].Elements.Items[i].FCaption.PaintToCanvas (ACanvas,
                                        Rect (StartX, StartY, StopX, StopY),
                                        FPrintFormats.Items[CurFormat].Elements.Items[i].Rotation,
                                        ARect,
                                        ReplaceVariables (FPrintFormats.Items[CurFormat].Elements.Items[i].FCaption.Caption));
      end;

    end else if FPrintFormats.Items[CurFormat].Elements.Items[i].ItemType = itShape then begin
      if FPrintFormats.Items[CurFormat].Elements.Items[i].Visible then begin
        ACanvas.Pen.Assign (FPrintFormats.Items[CurFormat].Elements.Items[i].FShape.Pen);
        ACanvas.Brush.Assign (FPrintFormats.Items[CurFormat].Elements.Items[i].FShape.Brush);
        FPrintFormats.Items[CurFormat].Elements.Items[i].FShape.PaintToCanvas (ACanvas,
                                      Rect (StartX, StartY, StopX, StopY),
                                      FPrintFormats.Items[CurFormat].Elements.Items[i].Rotation,
                                      ARect)
      end;

    end else
      RenderItem (FPrintFormats.Items[CurFormat].Elements.Items[i]);
  end;
end;
{=====}

procedure TVpPrinter.Print (APrinter     : TPrinter;
                             StartDate    : TDateTime;
                             EndDate      : TDateTime);
var
  ARect          : TRect;
  WidthInPixels  : Integer;
  HeightInPixels : Integer;
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;
  Scale          : Extended;

  procedure GetMeasurements;
  begin
    ARect.Left := 0;
    ARect.Top := 0;
    ARect.Right := APrinter.PageWidth;
    ARect.Bottom := APrinter.PageHeight;

    WidthInPixels  := ARect.Right - ARect.Left;
    HeightInPixels := ARect.Bottom - ARect.Top;

    PixelsPerInchX := GetDeviceCaps (APrinter.Canvas.Handle, LOGPIXELSX);
    PixelsPerInchY := GetDeviceCaps (APrinter.Canvas.Handle, LOGPIXELSY);

    Scale          := PixelsPerInchY / Screen.PixelsPerInch;
  end;

  procedure CalculateMargins;
  begin

    case MarginUnits of
      imAbsolutePixel : begin
        ARect.Left   := Round (LeftMargin);
        ARect.Top    := Round (TopMargin);
        ARect.Right  := ARect.Right - Round (RightMargin);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin);
      end;

      imPercent : begin
        ARect.Left   := Round (LeftMargin   * WidthInPixels  / 100);
        ARect.Top    := Round (TopMargin    * HeightInPixels / 100);
        ARect.Right  := ARect.Right  - Round (RightMargin  * WidthInPixels  / 100);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin * HeightInPixels / 100);
      end;

      imInches : begin
        ARect.Left   := Round (LeftMargin   * PixelsPerInchX);
        ARect.Top    := Round (TopMargin    * PixelsPerInchX);
        ARect.Right  := ARect.Right  - Round (RightMargin  * PixelsPerInchX);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin * PixelsPerInchX);
      end;
    end;
  end;

  function GetNextDate (ADate : TDateTime) : TDateTime;
  begin
    Result := ADate;
    if PrintFormats.Items[CurFormat].DayInc <> 0 then begin
      case PrintFormats.Items[CurFormat].DayIncUnits of
        duDay   : Result := Result + PrintFormats.Items[CurFormat].DayInc;
        duWeek  : Result := Result + PrintFormats.Items[CurFormat].DayInc * 7;
        duMonth : Result := IncMonth (Result, PrintFormats.Items[CurFormat].DayInc);
        duYear  : Result := IncYear (Result, PrintFormats.Items[CurFormat].DayInc);
      end;
    end else
      Result := Result + 1;
  end;

var
  CurDate       : TDateTime;
  RealStartDate : TDateTime;
  RealEndDate   : TDateTime;
  PageNum       : Integer;
  Done          : Boolean;

begin
  CheckPrintFormat;                                                                                                                                     

  FHaveDate        := False;
  FHaveContactGrid := False;
  FHaveTaskList    := False;

  FPrintJob := True;
  try
    AddDefaultVariables (StartDate);
    PageNum := 1;

    if not (FOwner is TVpControlLink) then
      raise EVpPrintFormatError.Create (RSPrtControlOwner);

    if not ValidFormat (CurFormat) then
      raise EVpPrintFormatError.Create (RSBadPrintFormat + IntToStr (CurFormat));

    GetMeasurements;
    CalculateMargins;

    CurDate := GetNextDate (StartDate);
    RealStartDate := StartDate;
    RealEndDate := EndDate;
    if CurDate < StartDate then begin
      if StartDate < EndDate then begin
        RealStartDate := EndDate;
        RealEndDate := StartDate;
      end;
    end else begin
      if StartDate > EndDate then begin
        RealStartDate := EndDate;
        RealEndDate := StartDate;
      end;
    end;
    CurDate := RealStartDate;

    Done := False;
    while not Done do begin
      { Update variables to reflect the current date }
      UpdateDateVariables (CurDate);
      ChangeVariable ('Page', IntToStr (PageNum));

      { Paint the page }
      if FOwner is TVpControlLink then
        with FOwner as TVpControlLink do
          TriggerOnPageStart (Self, PageNum, CurDate);

      PaintToCanvasRect (Printer.Canvas, ARect, CurDate);

      { Get the next date }

      CurDate := GetNextDate (CurDate);

      { Determine if the printing is done or not.
        This is a bit involved.  If only dates, captions and shapes are in the
        print format, doneness is determined when the date passes the end date.
        If task lists or contact grids are on the format, then doneness occurs
        when the date has bumped pass the last date and all the tasks and
        contacts have been printed.
      }
      Done := True;
      if (FHaveDate) and (CurDate <= RealEndDate) then
        Done := False;
      if (FHaveTaskList) and (FLastTask >= 0) then
        Done := False;
      if (FHaveContactGrid) and (FLastContact >= 0) then
        Done := False;

      if FOwner is TVpControlLink then
        with FOwner as TVpControlLink do
          TriggerOnPageEnd (Self, PageNum, CurDate, Done);

      { Go to the next page if not done }
      if not Done then begin
        Printer.NewPage;
        Inc (PageNum);
      end;
    end;
  finally
    FPrintJob := False;
  end;
end;
{=====}

procedure TVpPrinter.RegisterWatcher (Watcher : THandle);
var
  i          : Integer;
  NewHandle  : PVpWatcher;

begin
  for i := 0 to FNotifiers.Count - 1 do
    if Assigned (FNotifiers[i]) then
      if PVpWatcher (FNotifiers[i]).Handle = Watcher then
        Exit;
  GetMem (NewHandle, SizeOf (TVpWatcher));
  NewHandle.Handle := Watcher;
  FNotifiers.Add (NewHandle);
end;
{=====}

procedure TVpPrinter.RenderPage (    ACanvas      : TCanvas;
                                      ARect        : TRect;
                                      PageNum      : Integer;
                                  var ADate        : TDateTime;
                                      EndDate      : TDateTime;
                                  var StartContact : Integer;
                                  var StartTask    : Integer;
                                  var LastPage     : Boolean);
var
  WidthInPixels  : Integer;
  HeightInPixels : Integer;
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;
  Scale          : Extended;

  procedure GetMeasurements;
  begin
    WidthInPixels  := ARect.Right - ARect.Left;
    HeightInPixels := ARect.Bottom - ARect.Top;

    PixelsPerInchX := GetDeviceCaps (ACanvas.Handle, LOGPIXELSX);
    PixelsPerInchY := GetDeviceCaps (ACanvas.Handle, LOGPIXELSY);

    Scale          := PixelsPerInchY / Screen.PixelsPerInch;
  end;

  procedure CalculateMargins;
  begin

    case MarginUnits of
      imAbsolutePixel : begin
        ARect.Left   := Round (LeftMargin);
        ARect.Top    := Round (TopMargin);
        ARect.Right  := ARect.Right - Round (RightMargin);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin);
      end;

      imPercent : begin
        ARect.Left   := Round (LeftMargin   * WidthInPixels  / 100);
        ARect.Top    := Round (TopMargin    * HeightInPixels / 100);
        ARect.Right  := ARect.Right  - Round (RightMargin  * WidthInPixels  / 100);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin * HeightInPixels / 100);
      end;

      imInches : begin
        ARect.Left   := Round (LeftMargin   * PixelsPerInchX);
        ARect.Top    := Round (TopMargin    * PixelsPerInchX);
        ARect.Right  := ARect.Right  - Round (RightMargin  * PixelsPerInchX);
        ARect.Bottom := ARect.Bottom - Round (BottomMargin * PixelsPerInchX);
      end;
    end;
  end;

  function GetNextDate (ADate : TDateTime) : TDateTime;
  begin
    Result := ADate;
    if PrintFormats.Items[CurFormat].DayInc <> 0 then begin
      case PrintFormats.Items[CurFormat].DayIncUnits of
        duDay   : Result := Result + PrintFormats.Items[CurFormat].DayInc;
        duWeek  : Result := Result + PrintFormats.Items[CurFormat].DayInc * 7;
        duMonth : Result := IncMonth (Result, PrintFormats.Items[CurFormat].DayInc);
        duYear  : Result := IncYear (Result, PrintFormats.Items[CurFormat].DayInc);
      end;
    end else
      Result := Result + 1;
  end;

var
  OldTask    : Integer;
  OldContact : Integer;

begin
  CheckPrintFormat;                                                                                                                                     
  FHaveDate        := False;
  FHaveContactGrid := False;
  FHaveTaskList    := False;

  OldTask := FLastTask;
  OldContact := FLastContact;

  FPrintJob := True;
  try
    AddDefaultVariables (ADate);

    if not (FOwner is TVpControlLink) then
      raise EVpPrintFormatError.Create (RSPrtControlOwner);

    if FPrintFormats.Count = 0 then
      raise EVpPrintFormatError.Create (RSNeedFormatName)
    else if not ValidFormat (CurFormat) then
      raise EVpPrintFormatError.Create (RSBadPrintFormat + IntToStr (CurFormat));

    GetMeasurements;
    CalculateMargins;

    ChangeVariable ('Page', IntToStr (PageNum));

    FLastTask := StartTask;
    FLastContact := StartContact;
    PaintToCanvasRect (ACanvas, ARect, ADate);
    ADate := GetNextDate (ADate);

    LastPage := True;
    if (FHaveDate) and (ADate < EndDate) then
      LastPage := False;
    if (FHaveTaskList) and (FLastTask >= 0) then
      LastPage := False;
    if (FHaveContactGrid) and (FLastContact >= 0) then
      LastPage := False;
  finally
    FLastTask    := OldTask;
    FLastContact := OldContact;
  end;
end;
{=====}

function TVpPrinter.ReplaceVariables (const s : string) : string;
type
  TVpVariableState = (vsPlainText, vsCollectVarName, vsHaveVarName);

var
  State     : TVpVariableState;
  SLen      : Integer;
  i         : Integer;
  VarName   : string;
  ForceTerm : Boolean;
  VarsOk    : Boolean;
  Value     : string;
  Found     : Boolean;
  Change    : TVpChangeVar;

begin
  State     := vsPlainText;
  SLen      := Length (s);
  i         := 1;
  Result    := '';
  VarsOk    := True;
  ForceTerm := False;

  while (i <= SLen) do begin
    case State of
      vsCollectVarName :
        case s[i] of
          'A'..'Z', 'a'..'z', '0'..'9', '_' :
            VarName := VarName + s[i];
          ';' : begin
            State := vsHaveVarName;
            ForceTerm := True;
          end;
          else begin
            State := vsHaveVarName;
            ForceTerm := False;
          end;
        end;

      vsHaveVarName : begin
        State := vsPlainText;
        VarsOk := True;
        Found := HaveVariable (VarName);
        if Found then begin
          Change := cvChange;
          Value := LookupVariable (VarName);
        end else
          Change := cvRemove;

        if FOwner is TVpControlLink then
          with FOwner as TVpControlLink do
            TriggerOnGetVariable (Self, VarName, Found, Value, Change);

        case Change of
          cvChange :
            if ForceTerm then
              Result := Result + Value + s[i]
            else
              Result := Result + Value + s[i - 1] + s[i];
          cvIgnore :
            Result := Result + '$' + VarName + s[i - 1] + s[i];
          cvRemove : begin
          end;
        end;
      end;

      vsPlainText :
        case s[i] of
          '$' : begin
            VarsOk := False;
            State := vsCollectVarName;
            VarName := '';
          end;
        else
          Result := Result + s[i];
        end;

    end;
    Inc (i);
  end;

  if not VarsOk then begin
    Found := HaveVariable (VarName);
    if Found then begin
      Change := cvChange;
      Value := LookupVariable (VarName);
    end else
      Change := cvRemove;

    if FOwner is TVpControlLink then
      with FOwner as TVpControlLink do
        TriggerOnGetVariable (Self, VarName, Found, Value, Change);

    case Change of
      cvChange :
        Result := Result + LookupVariable (VarName);
      cvIgnore :
        Result := Result + '$' + VarName + s[i - 1];
      cvRemove : begin
      end;
    end;
  end;
end;
{=====}

procedure TVpPrinter.SaveToFile (FileName : string);                   
var
  fpOut : TextFile;
  i     : Integer;
  j     : Integer;

begin
  if FileName = '' then                                                
    FileName := DefaultXMLFileName;                                    

  AssignFile (fpOut, FileName);
  Rewrite (fpOut);
  try
    Writeln (fpOut, '<?xml version="1.0" encoding="UTF-8"?>');
    Writeln (fpOut, '<VpPrintFormats');
    Writeln (fpOut, '  Version = "0.0.1">');
    for i := 0 to FPrintFormats.Count - 1 do begin
      Writeln (fpOut, '  <PrintFormat');
      Writeln (fpOut, '    Name="' + XMLizeString (FPrintFormats.Items[i].FormatName) + '"');
      Writeln (fpOut, '    Description="' + XMLizeString (FPrintFormats.Items[i].Description) + '"');
      Writeln (fpOut, '    DayIncrement="' + IntToStr (FPrintFormats.Items[i].DayInc) + '"');
      if FPrintFormats.Items[i].Visible then
        Writeln (fpOut, '    Visble="True"')
      else
        Writeln (fpOut, '    Visble="False"');
      case FPrintFormats.Items[i].DayIncUnits of
        duDay   : Writeln (fpOut, '    DayIncrementUnits="Day">');
        duWeek  : Writeln (fpOut, '    DayIncrementUnits="Week">');
        duMonth : Writeln (fpOut, '    DayIncrementUnits="Month">');
        duYear  : Writeln (fpOut, '    DayIncrementUnits="Year">');
      end;
      for j := 0 to FPrintFormats.Items[i].Elements.Count - 1 do begin
        Writeln (fpOut, '    <Element');
        Writeln (fpOut, '      Name="' +
                 FPrintFormats.Items[i].Elements.Items[j].ElementName +
                 '"');
        if FPrintFormats.Items[i].Elements.Items[j].Visible then
          Writeln (fpOut, '      Visible="True"')
        else
          Writeln (fpOut, '      Visible="False"');
        case FPrintFormats.Items[i].Elements.Items[j].Rotation of
          ra0   : Writeln (fpOut, '      Rotation="0"');
          ra90  : Writeln (fpOut, '      Rotation="90"');
          ra180 : Writeln (fpOut, '      Rotation="180"');
          ra270 : Writeln (fpOut, '      Rotation="270"');
        end;
        case FPrintFormats.Items[i].Elements.Items[j].ItemType of
          itDayView   : Writeln (fpOut, '      Item="DayView"');
          itWeekView  : Writeln (fpOut, '      Item="WeekView"');
          itMonthView : Writeln (fpOut, '      Item="MonthView"');
          itCalendar  : Writeln (fpOut, '      Item="Calendar"');
          itShape     : Writeln (fpOut, '      Item="Shape"');
          itCaption   : Writeln (fpOut, '      Item="Caption"');
          itTasks     : Writeln (fpOut, '      Item="Tasks"');
          itContacts  : Writeln (fpOut, '      Item="Contacts"');
        end;
        case FPrintFormats.Items[i].Elements.Items[j].Measurement of
          imAbsolutePixel : Writeln (fpOut, '      Measurement="AbsolutePixel"');
          imPercent       : Writeln (fpOut, '      Measurement="Percent"');
          imInches        : Writeln (fpOut, '      Measurement="Inches"');
        end;
        Writeln (fpOut, '      Left="' + FloatToStr (FPrintFormats.Items[i].Elements.Items[j].Left) + '"');
        Writeln (fpOut, '      Top="' + FloatToStr (FPrintFormats.Items[i].Elements.Items[j].Top) + '"');
        Writeln (fpOut, '      Width="' + FloatToStr (FPrintFormats.Items[i].Elements.Items[j].Width) + '"');
        Writeln (fpOut, '      Height="' + FloatToStr (FPrintFormats.Items[i].Elements.Items[j].Height) + '"');
        Writeln (fpOut, '      DayOffset="' + IntToStr (FPrintFormats.Items[i].Elements.Items[j].DayOffset) + '"');
        case FPrintFormats.Items[i].Elements.Items[j].DayOffsetUnits of
          duDay   : Writeln (fpOut, '      DayOffsetUnits="Day">');
          duWeek  : Writeln (fpOut, '      DayOffsetUnits="Week">');
          duMonth : Writeln (fpOut, '      DayOffsetUnits="Month">');
          duYear  : Writeln (fpOut, '      DayOffsetUnits="Year">');
        end;

        if FPrintFormats.Items[i].Elements.Items[j].ItemType = itShape then begin
          Writeln (fpOut, '      <Shape');
          case FPrintFormats.Items[i].Elements.Items[j].FShape.Shape of
            ustRectangle  : Writeln (fpOut, '        Type="Rectangle">');
            ustTopLine    : Writeln (fpOut, '        Type="TopLine">');
            ustBottomLine : Writeln (fpOut, '        Type="BottomLine">');
            ustLeftLine   : Writeln (fpOut, '        Type="LeftLine">');
            ustRightLine  : Writeln (fpOut, '        Type="RightLine">');
            ustTLToBRLine : Writeln (fpOut, '        Type="TLToBRLine">');
            ustBLToTRLine : Writeln (fpOut, '        Type="BLToTRLine">');
            ustEllipse    : Writeln (fpOut, '        Type="Ellipse">');
          end;

          Writeln (fpOut, '        <Brush');
          Writeln (fpOut, '          Color="' +
                   IntToStr (FPrintFormats.Items[i].Elements.Items[j].FShape.Brush.Color) +
                   '"');
          case FPrintFormats.Items[i].Elements.Items[j].FShape.Brush.Style of
            bsSolid      : Writeln (fpOut, '          Style="Solid"/>');
            bsClear      : Writeln (fpOut, '          Style="Clear"/>');
            bsHorizontal : Writeln (fpOut, '          Style="Horizontal"/>');
            bsVertical   : Writeln (fpOut, '          Style="Vertical"/>');
            bsFDiagonal  : Writeln (fpOut, '          Style="FDiagonal"/>');
            bsBDiagonal  : Writeln (fpOut, '          Style="BDiagonal"/>');
            bsCross      : Writeln (fpOut, '          Style="Cross"/>');
            bsDiagCross  : Writeln (fpOut, '          Style="DiagCross"/>');
          end;
          Writeln (fpOut, '        <Pen');
          Writeln (fpOut, '          Color="' +
                   IntToStr (FPrintFormats.Items[i].Elements.Items[j].FShape.Pen.Color) +
                   '"');
          case FPrintFormats.Items[i].Elements.Items[j].FShape.Pen.Style of
            psSolid       : Writeln (fpOut, '          Style="Solid"');
            psDash        : Writeln (fpOut, '          Style="Dash"');
            psDot         : Writeln (fpOut, '          Style="Dot"');
            psDashDot     : Writeln (fpOut, '          Style="DashDot"');
            psDashDotDot  : Writeln (fpOut, '          Style="DashDotDot"');
            psClear       : Writeln (fpOut, '          Style="Clear"');
            psInsideFrame : Writeln (fpOut, '          Style="InsideFrame"');
          end;
          Writeln (fpOut, '          Width="' +
                   IntToStr (FPrintFormats.Items[i].Elements.Items[j].FShape.Pen.Width) +
                   '"/>');
          Writeln (fpOut, '      </Shape>');
        end;

        if FPrintFormats.Items[i].Elements.Items[j].ItemType = itCaption then begin
          Writeln (fpOut, '      <Caption');
          Writeln (fpOut, '        Caption="' +
                   XMLizeString (FPrintFormats.Items[i].Elements.Items[j].Caption.Caption) + '">');

          Writeln (fpOut, '        <Font');
          case FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Charset of
            ANSI_CHARSET        : Writeln (fpOut, '          CharSet="ANSI"');
            DEFAULT_CHARSET     : Writeln (fpOut, '          CharSet="Default"');
            SYMBOL_CHARSET      : Writeln (fpOut, '          CharSet="Symbol"');
            MAC_CHARSET         : Writeln (fpOut, '          CharSet="Mac"');
            SHIFTJIS_CHARSET    : Writeln (fpOut, '          CharSet="ShiftJIS"');
            HANGEUL_CHARSET     : Writeln (fpOut, '          CharSet="Hangeul"');
            JOHAB_CHARSET       : Writeln (fpOut, '          CharSet="Johab"');
            GB2312_CHARSET      : Writeln (fpOut, '          CharSet="GB2312"');
            CHINESEBIG5_CHARSET : Writeln (fpOut, '          CharSet="ChineseBig5"');
            GREEK_CHARSET       : Writeln (fpOut, '          CharSet="Greek"');
            TURKISH_CHARSET     : Writeln (fpOut, '          CharSet="Turkish"');
            VIETNAMESE_CHARSET  : Writeln (fpOut, '          CharSet="Vietnamese"');
            HEBREW_CHARSET      : Writeln (fpOut, '          CharSet="Hebrew"');
            ARABIC_CHARSET      : Writeln (fpOut, '          CharSet="Arabic"');
            BALTIC_CHARSET      : Writeln (fpOut, '          CharSet="Baltic"');
            RUSSIAN_CHARSET     : Writeln (fpOut, '          CharSet="Russian"');
            THAI_CHARSET        : Writeln (fpOut, '          CharSet="Thai"');
            EASTEUROPE_CHARSET  : Writeln (fpOut, '          CharSet="EastEurope"');
            OEM_CHARSET         : Writeln (fpOut, '          CharSet="OEM"');
          end;
          Writeln (fpOut, '          Color="' +
                   IntToStr (FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Color) +
                   '"');
          Writeln (fpOut, '          Height="' +
                   IntToStr (FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Height) +
                   '"');
          Writeln (fpOut, '          Name="' +
                   XMLizeString (FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Name) +
                   '"');
          case FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Pitch of
            fpDefault  : Writeln (fpOut, '          Pitch="Default"');
            fpVariable : Writeln (fpOut, '          Pitch="Variable"');
            fpFixed    : Writeln (fpOut, '          Pitch="Fixed"');
          end;
          if fsBold in FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Style then
            Writeln (fpOut, '          Bold="True"')
          else
            Writeln (fpOut, '          Bold="False"');
          if fsItalic in FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Style then
            Writeln (fpOut, '          Italic="True"')
          else
            Writeln (fpOut, '          Italic="False"');
          if fsUnderline in FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Style then
            Writeln (fpOut, '          Underline="True"')
          else
            Writeln (fpOut, '          Underline="False"');
          if fsStrikeout in FPrintFormats.Items[i].Elements.Items[j].Caption.Font.Style then
            Writeln (fpOut, '          Strikeout="True"/>')
          else
            Writeln (fpOut, '          Strikeout="False"/>');
          Writeln (fpOut, '      </Caption>');
        end;
        Writeln (fpOut, '    </Element>');

      end;
      Writeln (fpOut, '  </PrintFormat>');
    end;
    Writeln (fpOut, '</VpPrintFormats>');
  finally
    CloseFile (fpOut);
  end;
end;
{=====}

procedure TVpPrinter.SetBottomMargin (const v : Extended);
begin
  if v <> FBottomMargin then begin
    FBottomMargin := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetCurFormat (const v : Integer);
begin
  if FPrintFormats.Count = 0 then                                        
    raise EVpPrintFormatError.Create (RSNoPrintFormats);                 

  if v <> FCurFormat then begin
    if (v < 0) or (v >= FPrintFormats.Count) then
      raise EVpPrintFormatError.Create (RSBadPrintFormat + IntToStr (v)); 
    FCurFormat := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetDefaultXMLFileName (const v : string);         
begin                                                                  
  if v <> FDefaultXMLFileName then                                     
    FDefaultXMLFileName := v;                                          
end;                                                                   
{=====}

procedure TVpPrinter.SetLeftMargin (const v : Extended);
begin
  if v <> FLeftMargin then begin
    FLeftMargin := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetMarginUnits (const v : TVpItemMeasurement);
begin
  if v <> FMarginUnits then begin
    FMarginUnits := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetPrintFormats (const v : TVpPrintFormat);
begin
  FPrintFormats.Assign (v);
  NotifyLinked;
end;
{=====}

procedure TVpPrinter.SetRightMargin (const v : Extended);
begin
  if v <> FRightMargin then begin
    FRightMargin := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetTopMargin (const v : Extended);
begin
  if v <> FTopMargin then begin
    FTopMargin := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.SetUseFormComponents (const v : Boolean);
begin
  if v <> FUseFormComponents then begin
    FUseFormComponents := v;
    NotifyLinked;
  end;
end;
{=====}

procedure TVpPrinter.UpdateDateVariables (Date : TDateTime);
begin
  AddVariable ('DayNumber', FormatDateTime ('d', Date));
  AddVariable ('DayNumber0', FormatDateTime ('dd', Date));
  AddVariable ('DayAbbrev', FormatDateTime ('ddd', Date));
  AddVariable ('DayName', FormatDateTime ('dddd', Date));
  AddVariable ('ShortDate', FormatDateTime ('ddddd', Date));
  AddVariable ('LongDate', FormatDateTime ('dddddd', Date));
  AddVariable ('Era', FormatDateTime ('e', Date));
  AddVariable ('Era0', FormatDateTime ('ee', Date));
  AddVariable ('EraAbbrev', FormatDateTime ('g', Date));
  AddVariable ('EraName', FormatDateTime ('gg', Date));
  AddVariable ('Month', FormatDateTime ('m', Date));
  AddVariable ('Month0', FormatDateTime ('mm', Date));
  AddVariable ('MonthAbbv', FormatDateTime ('mmm', Date));
  AddVariable ('MonthName', FormatDateTime ('mmmm', Date));
  AddVariable ('ShortYear', FormatDateTime ('yy', Date));
  AddVariable ('LongYear', FormatDateTime ('yyyy', Date));
  AddVariable ('DateSep', FormatDateTime ('/', Date));
  AddVariable ('d', FormatDateTime ('d', Date));
  AddVariable ('dd', FormatDateTime ('dd', Date));
  AddVariable ('ddd', FormatDateTime ('ddd', Date));
  AddVariable ('dddd', FormatDateTime ('dddd', Date));
  AddVariable ('ddddd', FormatDateTime ('ddddd', Date));
  AddVariable ('dddddd', FormatDateTime ('dddddd', Date));
  AddVariable ('e', FormatDateTime ('e', Date));
  AddVariable ('ee', FormatDateTime ('ee', Date));
  AddVariable ('g', FormatDateTime ('g', Date));
  AddVariable ('gg', FormatDateTime ('gg', Date));
  AddVariable ('m', FormatDateTime ('m', Date));
  AddVariable ('mm', FormatDateTime ('mm', Date));
  AddVariable ('mmm', FormatDateTime ('mmm', Date));
  AddVariable ('mmmm', FormatDateTime ('mmmm', Date));
  AddVariable ('yy', FormatDateTime ('yy', Date));
  AddVariable ('yyyy', FormatDateTime ('yyyy', Date));
  AddVariable ('/', FormatDateTime ('/', Date));
end;
{=====}

function TVpPrinter.ValidFormat (const v : Integer) : Boolean;
begin
  Result := (v >= 0) and (v < FPrintFormats.Count);
end;
{=====}

procedure TVpPrinter.xmlPrintFormatAttribute (oOwner     : TObject;
                                               sName,
                                               sValue     : DOMString;
                                               bSpecified : Boolean);
var
  Item : TVpAttributeItem;
begin
  Item := TVpAttributeItem (FAttributes.Add);
  Item.Name := sName;
  Item.Value := sValue;
end;
{=====}

procedure TVpPrinter.xmlPrintFormatEndElement (oOwner : TObject;
                                                sValue : DOMString);
begin
  if (sValue = 'PrintFormat') or (sValue = 'VpPrintFormats') then begin
    FLoadingIndex := -1;
    FElementIndex := -1;
  end else if sValue = 'Element' then
    FElementIndex := -1;
  FAttributes.Clear;
end;
{=====}

procedure TVpPrinter.xmlPrintFormatStartElement (oOwner : TObject;
                                                  sValue : DOMString);
var
  i          : Integer;
  NewItem    : TVpPrintFormatItem;
  NewElement : TVpPrintFormatElementItem;

begin
  if sValue = 'VpPrintFormats' then begin
    FLoadingIndex := -1;
    FElementIndex := -1;

  end else if sValue = 'PrintFormat' then begin
    { Search for either missing names or duplicate names }
    { Missing names will be replaced with Unknown.  Duplicate names
      are not allowed. }
    for i := 0 to FAttributes.Count - 1 do
      if FAttributes.Items[i].Name = 'Name' then begin
        if FAttributes.Items[i].Value = '' then
          FAttributes.Items[i].Value := 'Unknown'
        else if Find (FAttributes.Items[i].Value) >= 0 then
          Exit;
      end;
    { If we've gotten this far, the name is good.  Add the element }
    NewItem := TVpPrintFormatItem (FPrintFormats.Add);
    for i := 0 to FAttributes.Count - 1 do begin
      if (FAttributes.Items[i].Name = 'Name') and
         (Fattributes.Items[i].Value <> '') then
        NewItem.FormatName := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Description' then
        NewItem.Description := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then begin
        if FAttributes.Items[i].Value = 'True' then
          NewItem.Visible := True
        else if FAttributes.Items[i].Value = 'False' then
          NewItem.Visible := False;
      end else if FAttributes.Items[i].Name = 'DayIncrementUnits' then begin
        if FAttributes.Items[i].Value = 'Day' then
          NewItem.DayIncUnits := duDay
        else if FAttributes.Items[i].Value = 'Week' then
          NewItem.DayIncUnits := duWeek
        else if FAttributes.Items[i].Value = 'Month' then
          NewItem.DayIncUnits := duMonth
        else if FAttributes.Items[i].Value = 'Year' then
          NewItem.DayIncUnits := duYear;
      end;
    end;
    FLoadingIndex := NewItem.Index;

  end else if sValue = 'Element' then begin
    if FLoadingIndex < 0 then
      Exit;
    NewElement := TVpPrintFormatElementItem (FPrintFormats.Items[FLoadingIndex].Elements.Add);
    try
      FElementIndex := NewElement.Index;
      for i := 0 to FAttributes.Count - 1 do begin
        if FAttributes.Items[i].Name = 'Name' then
          NewElement.ElementName := FAttributes.Items[i].Value
        else if FAttributes.Items[i].Name = 'Visible' then begin
          if FAttributes.Items[i].Value = 'False' then
            NewElement.Visible := False
          else if FAttributes.Items[i].Value = 'True' then
            NewElement.Visible := True;
        end else if FAttributes.Items[i].Name = 'Rotation' then begin
          if FAttributes.Items[i].Value = '90' then
            NewElement.Rotation := ra90
          else if FAttributes.Items[i].Value = '180' then
            NewElement.Rotation := ra180
          else if FAttributes.Items[i].Value = '270' then
            NewElement.Rotation := ra270
          else
            NewElement.Rotation := ra0;
        end else if FAttributes.Items[i].Name = 'Item' then begin
          if FAttributes.Items[i].Value = 'DayView' then
            NewElement.ItemType := itDayView
          else if FAttributes.Items[i].Value = 'WeekView' then
            NewElement.ItemType := itWeekView
          else if FAttributes.Items[i].Value = 'MonthView' then
            NewElement.ItemType := itMonthView
          else if FAttributes.Items[i].Value = 'Shape' then
            NewElement.ItemType := itShape
          else if FAttributes.Items[i].Value = 'Caption' then
            NewElement.ItemType := itCaption
          else if FAttributes.Items[i].Value = 'Calendar' then
            NewElement.ItemType := itCalendar
          else if FAttributes.Items[i].Value = 'Tasks' then
            NewElement.ItemType := itTasks
          else if FAttributes.Items[i].Value = 'Contacts' then
            NewElement.ItemType := itContacts
          else
            raise EVpPrintFormatError.Create (RSBadItemType + FAttributes.Items[i].Value); 
        end else if FAttributes.Items[i].Name = 'Measurement' then begin
          if FAttributes.Items[i].Value = 'AbsolutePixel' then
            NewElement.Measurement := imAbsolutePixel
          else if FAttributes.Items[i].Value = 'Percent' then
            NewElement.Measurement := imPercent
          else if FAttributes.Items[i].Value = 'Inches' then
            NewElement.Measurement := imInches
          else
            raise EVpPrintFormatError.Create (RSBadMeasurement + FAttributes.Items[i].Value); 
        end else if FAttributes.Items[i].Name = 'Left' then
          NewElement.Left := StrToFloat (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Top' then
          NewElement.Top := StrToFloat (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Width' then
          NewElement.Width := StrToFloat (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Height' then
          NewElement.Height := StrToFloat (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'DayOffset' then
          NewElement.DayOffset:= StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'DayOffsetUnits' then begin
          if FAttributes.Items[i].Value = 'Day' then
            NewElement.DayOffsetUnits := duDay
          else if FAttributes.Items[i].Value = 'Week' then
            NewElement.DayOffsetUnits := duWeek
          else if FAttributes.Items[i].Value = 'Month' then
            NewElement.DayOffsetUnits := duMonth
          else if FAttributes.Items[i].Value = 'Year' then
            NewElement.DayOffsetUnits := duYear;
        end;
      end;
    except
      on EConvertError do begin
      end;
    end;

  end else if sValue = 'Shape' then begin
    if (FLoadingIndex < 0) or (FElementIndex < 0) then
      Exit;
    for i := 0 to FAttributes.Count - 1 do
      if FAttributes.Items[i].Name = 'Type' then begin
        if FAttributes.Items[i].Value = 'Rectangle' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustRectangle
        else if FAttributes.Items[i].Value = 'TopLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustTopLine
        else if FAttributes.Items[i].Value = 'BottomLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustBottomLine
        else if FAttributes.Items[i].Value = 'LeftLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustLeftLine
        else if FAttributes.Items[i].Value = 'RightLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustRightLine
        else if FAttributes.Items[i].Value = 'TLToBRLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustTLToBRLine
        else if FAttributes.Items[i].Value = 'BLToTRLine' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustBLToTRLine
        else if FAttributes.Items[i].Value = 'Ellipse' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Shape := ustEllipse;
      end;

  end else if sValue = 'Caption' then begin
    if (FLoadingIndex < 0) or (FElementIndex < 0) then
      Exit;
    for i := 0 to FAttributes.Count - 1 do
      if FAttributes.Items[i].Name = 'Caption' then
        FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Caption :=
            FAttributes.Items[i].Value;

  end else if sValue = 'Pen' then begin
    if (FLoadingIndex < 0) or (FElementIndex < 0) then
      Exit;
    try
      for i := 0 to FAttributes.Count - 1 do
        if FAttributes.Items[i].Name = 'Color' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Color :=
            StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Style' then begin
          if FAttributes.Items[i].Value = 'Solid' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psSolid
          else if FAttributes.Items[i].Value = 'Dash' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psDash
          else if FAttributes.Items[i].Value = 'Dot' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psDot
          else if FAttributes.Items[i].Value = 'DashDot' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psDashDot
          else if FAttributes.Items[i].Value = 'DashDotDot' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psDashDotDot
          else if FAttributes.Items[i].Value = 'Clear' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psClear
          else if FAttributes.Items[i].Value = 'InsideFrame' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Style := psInsideFrame;
        end else if FAttributes.Items[i].Name = 'Width' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Pen.Width :=
            StrToInt (FAttributes.Items[i].Value);
    except
      on EConvertError do begin
      end;
    end;

  end else if sValue = 'Brush' then begin
    if (FLoadingIndex < 0) or (FElementIndex < 0) then
      Exit;
    try
      for i := 0 to FAttributes.Count - 1 do
        if FAttributes.Items[i].Name = 'Color' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Color :=
            StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Style' then begin
          if FAttributes.Items[i].Value = 'Solid' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsSolid
          else if FAttributes.Items[i].Value = 'Clear' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsClear
          else if FAttributes.Items[i].Value = 'Horizontal' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsHorizontal
          else if FAttributes.Items[i].Value = 'Vertical' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsVertical
          else if FAttributes.Items[i].Value = 'FDiagonal' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsFDiagonal
          else if FAttributes.Items[i].Value = 'BDiagonal' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsBDiagonal
          else if FAttributes.Items[i].Value = 'Cross' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsCross
          else if FAttributes.Items[i].Value = 'DiagCross' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Shape.Brush.Style := bsDiagCross;
        end;
    except
      on EConvertError do begin
      end;
    end;

  end else if sValue = 'Font' then begin
    if (FLoadingIndex < 0) or (FElementIndex < 0) then
      Exit;
    try
      for i := 0 to FAttributes.Count - 1 do
        if FAttributes.Items[i].Name = 'CharSet' then begin
          if FAttributes.Items[i].Value = 'ANSI' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := ANSI_CHARSET
          else if FAttributes.Items[i].Value = 'Default' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := DEFAULT_CHARSET
          else if FAttributes.Items[i].Value = 'Symbol' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := SYMBOL_CHARSET
          else if FAttributes.Items[i].Value = 'Mac' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := MAC_CHARSET
          else if FAttributes.Items[i].Value = 'ShiftJIS' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := SHIFTJIS_CHARSET
          else if FAttributes.Items[i].Value = 'Hangeul' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := HANGEUL_CHARSET
          else if FAttributes.Items[i].Value = 'Johab' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := JOHAB_CHARSET
          else if FAttributes.Items[i].Value = 'GB2313' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := GB2312_CHARSET
          else if FAttributes.Items[i].Value = 'ChineseBig5' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := CHINESEBIG5_CHARSET
          else if FAttributes.Items[i].Value = 'Greek' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := GREEK_CHARSET
          else if FAttributes.Items[i].Value = 'Turkish' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := TURKISH_CHARSET
          else if FAttributes.Items[i].Value = 'Vietnamese' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := VIETNAMESE_CHARSET
          else if FAttributes.Items[i].Value = 'Hebrew' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := HEBREW_CHARSET
          else if FAttributes.Items[i].Value = 'Arabic' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := ARABIC_CHARSET
          else if FAttributes.Items[i].Value = 'Baltic' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := BALTIC_CHARSET
          else if FAttributes.Items[i].Value = 'Russian' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := RUSSIAN_CHARSET
          else if FAttributes.Items[i].Value = 'Thai' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := THAI_CHARSET
          else if FAttributes.Items[i].Value = 'EastEurope' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := EASTEUROPE_CHARSET
          else if FAttributes.Items[i].Value = 'OEM' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.CharSet := OEM_CHARSET
        end else if FAttributes.Items[i].Name = 'Color' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Color :=
            StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Height' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Height:=
            StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Name' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Name :=
            FAttributes.Items[i].Value
        else if FAttributes.Items[i].Name = 'Color' then
          FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Color :=
            StrToInt (FAttributes.Items[i].Value)
        else if FAttributes.Items[i].Name = 'Pitch' then begin
          if FAttributes.Items[i].Value = 'Default' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Pitch := fpDefault
          else if FAttributes.Items[i].Value = 'Variable' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Pitch := fpVariable
          else if FAttributes.Items[i].Value = 'Fixed' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Pitch := fpFixed;
        end else if FAttributes.Items[i].Name = 'Bold' then begin
          if FAttributes.Items[i].Value = 'True' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style + [fsBold]
          else
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style - [fsBold];
        end else if FAttributes.Items[i].Name = 'Italic' then begin
          if FAttributes.Items[i].Value = 'True' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style + [fsItalic]
          else
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style - [fsItalic];
        end else if FAttributes.Items[i].Name = 'Underline' then begin
          if FAttributes.Items[i].Value = 'True' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style + [fsUnderline]
          else
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style - [fsUnderline];
        end else if FAttributes.Items[i].Name = 'Strikeout' then begin
          if FAttributes.Items[i].Value = 'True' then
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style + [fsStrikeout]
          else
            FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style :=
              FPrintFormats.Items[FLoadingIndex].Elements.Items[FElementIndex].Caption.Font.Style - [fsStrikeout];
        end;
    except
      on EConvertError do begin
      end;
    end;
  end;
  FAttributes.Clear;
end;
{=====}

end.
