// ------------------------------------------------------------------------------
// DPF.iOS.UIPrintPageRenderer Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.UIPrintPageRenderer;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.UIFont,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
{$ENDIF}
  DPF.iOS.Common,
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  // ------------------------------------------------------------------------------

{$IFDEF IOS}
  UIPrintPageRendererClass = interface( NSObjectClass )
    ['{96A50B8F-2F32-4D42-AAF1-29DE9C2F98CD}']
  end;

  UIPrintPageRenderer = interface( NSObject )
    ['{4E75D23D-3D4E-44B3-BF58-CCB68F0AD584}']
    procedure addPrintFormatter( formatter: UIPrintFormatter; startingAtPageAtIndex: NSInteger ); cdecl;
    procedure drawContentForPageAtIndex( pageIndex: NSInteger; inRect: CGRect ); cdecl;
    procedure drawFooterForPageAtIndex( pageIndex: NSInteger; inRect: CGRect ); cdecl;
    procedure drawHeaderForPageAtIndex( pageIndex: NSInteger; inRect: CGRect ); cdecl;
    procedure drawPageAtIndex( pageIndex: NSInteger; inRect: CGRect ); cdecl;
    procedure drawPrintFormatter( printFormatter: UIPrintFormatter; forPageAtIndex: NSInteger ); cdecl;
    function footerHeight: Single; cdecl;
    function headerHeight: Single; cdecl;
    function numberOfPages: NSInteger; cdecl;
    function paperRect: CGRect; cdecl;
    procedure prepareForDrawingPages( range: NSRange ); cdecl;
    function printFormatters: NSArray; cdecl;
    function printFormattersForPageAtIndex( pageIndex: NSInteger ): NSArray; cdecl;
    function printableRect: CGRect; cdecl;
    procedure setFooterHeight( footerHeight: Single ); cdecl;
    procedure setHeaderHeight( headerHeight: Single ); cdecl;
    procedure setPrintFormatters( printFormatters: NSArray ); cdecl;
    procedure setValue( value: NSValue; forKey: NSString ); cdecl;
  end;

  TUIPrintPageRenderer = class( TOCGenericImport<UIPrintPageRendererClass, UIPrintPageRenderer> )
  end;
{$ENDIF}


  // ------------------------------------------------------------------------------

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIPrintPageRenderer = class( TComponent )
  private

{$IFDEF IOS}
    FDPFPrintPageRenderer: UIPrintPageRenderer;
{$ENDIF}
  protected
    FTitle: string;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFDEF IOS}
    procedure PrintToPDF( const html: string; const pdfFile: string );
{$ENDIF}
  published
    property Title: string read FTitle write FTitle;
  end;

  // ------------------------------------------------------------------------------

{$IFDEF IOS}

procedure UIGraphicsBeginPDFContextToData( data: Pointer; bounds: CGRect; documentInfo: NSDictionary ); cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFContextToData';
procedure UIGraphicsBeginPDFPage; cdecl; external libUIKit name _PU + 'UIGraphicsBeginPDFPage';
function UIGraphicsGetPDFContextBounds: CGRect; cdecl; external libUIKit name _PU + 'UIGraphicsGetPDFContextBounds';
procedure UIGraphicsEndPDFContext; cdecl; external libUIKit name _PU + 'UIGraphicsEndPDFContext';
{$ENDIF}

implementation

uses
  Math;

// ------------------------------------------------------------------------------
{ TDPFUIPrintPageRenderer }
// ------------------------------------------------------------------------------
constructor TDPFUIPrintPageRenderer.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  FDPFPrintPageRenderer := TUIPrintPageRenderer.Wrap( TUIPrintPageRenderer.Alloc.init );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIPrintPageRenderer.Destroy;
begin
{$IFDEF IOS}
  FDPFPrintPageRenderer.release;
  FDPFPrintPageRenderer := nil;
{$ENDIF}
  inherited;
end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFUIPrintPageRenderer.PrintToPDF( const html: string; const pdfFile: string );
var
  // PrintInterface: UIPrintInteractionController;
  fmt       : UIMarkupTextPrintFormatter;
  render    : UIPrintPageRenderer;
  page      : CGRect;
  printable : CGRect;
  CGRectZero: CGRect;
  pdfData   : NSMutableData;
  bounds    : CGRect;
  i         : Integer;
begin

  // PrintInterface := TUIPrintInteractionController.Wrap( TUIPrintInteractionController.OCClass.sharedPrintController );

  // -----------------------------------------------
  // Create a Print Formatter
  fmt := TUIMarkupTextPrintFormatter.Wrap( TUIMarkupTextPrintFormatter.Alloc.initWithMarkupText( NSSTR( html ) ) );
  fmt.setMarkupText( NSSTR( html ) );
  fmt.setStartPage( 0 );

  // -----------------------------------------------
  // Assign print formatter to UIPrintPageRenderer
  render := TUIPrintPageRenderer.Wrap( TUIPrintPageRenderer.Alloc.init );
  render.addPrintFormatter( fmt, 0 );

  // PrintInterface.setPrintPageRenderer( render );
  // PrintInterface.setPrintFormatter( fmt );

  // -----------------------------------------------
  // Create PDF Context and draw
  CGRectZero := CGRectMake( 0, 0, 0, 0 );
  pdfData    := TNSMutableData.Wrap( TNSMutableData.OCClass.data );
  UIGraphicsBeginPDFContextToData( ( pdfData as ILocalObject ).GetObjectID, CGRectZero, nil );
  // render.prepareForDrawingPages( NSMakeRange( 0, 1 ) );


  // -----------------------------------------------
  // Assign paperRect and printableRect
  bounds := UIGraphicsGetPDFContextBounds( );
  page.origin.x    := 0;
  page.origin.y    := 0;
  page.size.width  := bounds.size.width; //792;
  page.size.height := bounds.size.height;//612;
  printable        := CGRectInset( page, 0, 0 );

  render.setValue( TNSValue.Wrap( TNSValue.OCClass.valueWithCGRect( page ) ), NSSTR( 'paperRect' ) );
  render.setValue( TNSValue.Wrap( TNSValue.OCClass.valueWithCGRect( printable ) ), NSSTR( 'printableRect' ) );
  // -----------------------------------------------

  for i  := 0 to { render.numberOfPages -1 } 0 do
  begin
    UIGraphicsBeginPDFPage( );
    render.drawPageAtIndex( i, bounds );
  end;

  UIGraphicsEndPDFContext( );

  // -----------------------------------------------
  // Save PDF file
  pdfData.writeToFile( NSSTR( pdfFile ), true );

  fmt.release;
  render.release;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
