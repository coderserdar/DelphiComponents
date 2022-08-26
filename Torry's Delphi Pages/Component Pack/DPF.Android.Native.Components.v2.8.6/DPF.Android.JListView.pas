// ------------------------------------------------------------------------------
// DPF.Android.JListView Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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
unit DPF.Android.JListView;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,
  System.Generics.Collections,

  System.TypInfo,
  DPF.Android.BaseControl,

  DPF.Android.JRelativeLayout,
  DPF.Android.JTextView,

{$IFDEF ANDROID}
  DPF.Android.Widget,
  DPF.Android.R,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

const
  TAG_BASE = 10000;

type

  TDPFJListView = class;

{$IFDEF ANDROID}

  TDPFOnListViewListener = class( TJavaLocal, JDPFOnListViewListener )
  private
    [weak]
    FDPFJListView: TDPFJListView;
  public
    constructor create( ADPFJListView: TDPFJListView );

    function onGetRowCount( view: JListView ): Integer; cdecl;
    function onGetCustomView( view: JListView; cellView: JView; position: Integer ): JView; cdecl;
    procedure onItemSelected( listview: JListView; childView: JView; position: Integer; id: Int64 ); cdecl;
    procedure onNothingSelected( listview: JListView ); cdecl;
    procedure onItemClick( listview: JListView; view: JView; position: integer; id: Int64 ); cdecl;
  end;

{$ENDIF}

  TDPFListViewOnItemClick       = procedure( Sender: TObject; const Position: Integer; const ID: Int64 ) of object;
  TDPFListViewOnItemSelected    = procedure( Sender: TObject; const Position: Integer; const ID: Int64 ) of object;
  TDPFListViewOnNothingSelected = procedure( Sender: TObject ) of object;
  TDPFListViewOnDrawCell        = procedure( Sender: TObject; Position: Integer; var Objects: TArray<TDPFANDBaseControl> ) of object;
  TDPFListViewOnRowCount        = procedure( Sender: TObject; var RowCount: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJListView = class( TDPFANDBaseControl )
  private
    FOnItemClick      : TDPFListViewOnItemClick;
    FOnItemSelected   : TDPFListViewOnItemSelected;
    FOnNothingSelected: TDPFListViewOnNothingSelected;
    FOnDrawCell       : TDPFListViewOnDrawCell;
    FOnRowCount       : TDPFListViewOnRowCount;
    FUniqueID         : Integer;
    FSelectedColor    : TAlphaColor;
    function GetUniqueID: Integer;
    procedure SetSelectedColor( const Value: TAlphaColor );
  protected
{$IFDEF ANDROID}
    FJDPFListView: JDPFListView;

    FDPFOnListViewListener: TDPFOnListViewListener;

    FSelectedLists    : TList<Integer>;
    FLastItemSelected : Integer;
    CellDicCustomViews: TDictionary<Integer, TArray<TDPFANDBaseControl>>;

{$ENDIF}
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF ANDROID}
    procedure Paint; override;
{$ENDIF}
  public
{$IFDEF ANDROID}
    procedure Loaded; override;
    procedure ReLoad;
    function GetRowCustomViews( const Position: Integer; ID: Integer ): TArray<TDPFANDBaseControl>;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure RefreshNeeded;

    property UniqueID: Integer read GetUniqueID write FUniqueID;

  published
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor default TAlphaColors.Null;

    property OnItemClick      : TDPFListViewOnItemClick read FOnItemClick write FOnItemClick;
    property OnItemSelected   : TDPFListViewOnItemSelected read FOnItemSelected write FOnItemSelected;
    property OnNothingSelected: TDPFListViewOnNothingSelected read FOnNothingSelected write FOnNothingSelected;
    property OnDrawCell       : TDPFListViewOnDrawCell read FOnDrawCell write FOnDrawCell;
    property OnRowCount       : TDPFListViewOnRowCount read FOnRowCount write FOnRowCount;

    property Clickable;
    property Focusable;
    property FocusableInTouchMode;
    property BackgroundColor1;
    property BackgroundColor2;
    property BackgroundColor3;
    property BackgroundImage;
    property BorderWidth;
    property BorderColor;
    property BorderCornerRadius;
    property GradientOrientation;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
    property OnClick;
  end;

implementation

// ------------------------------------------------------------------------------
{ TDPFJListView }
constructor TDPFJListView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'ListView';
  BackgroundColor1 := TAlphaColors.Lightsteelblue;
  Visible          := True;
  FSelectedColor   := TAlphaColors.Null;

{$IFDEF ANDROID}
  FUniqueID          := TAG_BASE;
  CellDicCustomViews := TDictionary < Integer, TArray < TDPFANDBaseControl >>.Create;
  CallInUIThreadAndWaitFinishing(
    procedure( )
    var
      FJTextView: JTextView;
    begin
      FDPFOnListViewListener := TDPFOnListViewListener.create( self );
      FJDPFListView := TJDPFListView.JavaClass.init( SharedActivity );
      FJDPFListView.setListViewListener( FDPFOnListViewListener );

      // This line is important if you want to remove this then app will be crash !!!
      // FJTextView := TJTextView.JavaClass.init( SharedActivity );

      FSelectedLists := TList<Integer>.Create;
      FSelectedLists.Sort;
      JControl := FJDPFListView;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJListView.Destroy;
var
  i, j: Integer;
  Arr : TArray<TArray<TDPFANDBaseControl>>;
begin
{$IFDEF ANDROID}
  Arr   := CellDicCustomViews.Values.ToArray;
  for I := 0 to high( Arr ) do
  begin
    for J := 0 to high( Arr[I] ) do
      Arr[I][J].DisposeOf;
    SetLength( Arr[I], 0 );
  end;
  CellDicCustomViews.Clear;
  FDPFOnListViewListener.DisposeOf;
  FSelectedLists.DisposeOf;
  CellDicCustomViews.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFJListView.GetUniqueID: Integer;
begin
  Result := FUniqueID;
  Inc( FUniqueID );
end;

// ------------------------------------------------------------------------------
procedure TDPFJListView.RefreshNeeded;
begin
{$IFDEF ANDROID}
  if Assigned( FJDPFListView ) then
  begin
    CallInUIThread(
      procedure( )
      begin
        FJDPFListView.reLoad;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF ANDROID}

procedure TDPFJListView.ReLoad;
begin
  FUniqueID := TAG_BASE;
  if Assigned( FJDPFListView ) then
    FJDPFListView.reLoad;
end;

// ------------------------------------------------------------------------------
procedure TDPFJListView.Loaded;
begin
  Resize; // Very Important for Embbeded Forms, Frames

  SetSelectedColor( FSelectedColor );
  ReLoad;
  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFJListView.Resize;
begin
{$IFDEF ANDROID}
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJListView.SetSelectedColor( const Value: TAlphaColor );
begin
  FSelectedColor := Value;
{$IFDEF ANDROID}
  if assigned( FJDPFListView ) then
    FJDPFListView.setItemSelectedColor( value );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJListView.Move;
begin
  Resize;
end;

{$IFNDEF ANDROID}

// ------------------------------------------------------------------------------
procedure TDPFJListView.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor1;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}
{$IFDEF ANDROID}

// ------------------------------------------------------------------------------
function TDPFJListView.GetRowCustomViews( const Position: Integer; ID: Integer ): TArray<TDPFANDBaseControl>;
begin
  Result := nil;
  if CellDicCustomViews.ContainsKey( ID ) then
    CellDicCustomViews.TryGetValue( ID, Result );
end;

// ------------------------------------------------------------------------------
{ TDPFOnListViewListener }

constructor TDPFOnListViewListener.create( ADPFJListView: TDPFJListView );
begin
  inherited create;
  FDPFJListView := ADPFJListView;
end;

// ------------------------------------------------------------------------------
function TDPFOnListViewListener.onGetRowCount( view: JListView ): Integer; cdecl;
begin
  result := 0;
  if Assigned( FDPFJListView.FOnRowCount ) then
    FDPFJListView.FOnRowCount( FDPFJListView, Result );
end;

// ------------------------------------------------------------------------------
function TDPFOnListViewListener.onGetCustomView( view: JListView; cellView: JView; position: Integer ): JView; cdecl;
var
  FJTextView : JTextView;
  r          : JLinearLayout;
  v          : JView;
  VV         : TDPFJRelativeLayout;
  TT         : TDPFJTextView;
  CustomViews: TArray<TDPFANDBaseControl>;
  ReUsable   : Boolean;
  I          : Integer;
  mID        : Integer;
begin
  if Assigned( FDPFJListView.FOnDrawCell ) then
  begin
    mID := -1;
    if assigned( cellView ) then
      mID := cellView.getId;

    ReUsable := assigned( cellView ) and FDPFJListView.CellDicCustomViews.ContainsKey( cellView.getId );

    if ReUsable then
      FDPFJListView.CellDicCustomViews.TryGetValue( cellView.getId, CustomViews );

    mID := FDPFJListView.CellDicCustomViews.Count;

    FDPFJListView.FOnDrawCell( FDPFJListView, position, CustomViews );

    if Length( CustomViews ) > 0 then
    begin
      for I := 0 to high( CustomViews ) do
      begin
        if ( CustomViews[i].JControl <> nil ) and not ReUsable then
        begin
          CustomViews[i].JControl.setId( FDPFJListView.UniqueID );
        end;
        if not CustomViews[i].isLoaded then
          CustomViews[i].Loaded;
      end;
      FDPFJListView.FJDPFListView.setCustomView( CustomViews[0].JControl );
      FDPFJListView.CellDicCustomViews.AddOrSetValue( CustomViews[0].JControl.getId, CustomViews );
    end;
  end;

  (* For Test
    exit;
    // ----------------------------------------------------------------------------
    if ( cellView = nil ) then
    begin
    r := TJLinearLayout.JavaClass.init( SharedActivity );
    r.setLayoutParams( TJLinearLayout_LayoutParams.JavaClass.init( -1, 100 ) );
    r.setPadding( 10, 10, 10, 10 );
    FJTextView := TJTextView.JavaClass.init( SharedActivity );
    FJTextView.setTextColor( TAlphaColors.Black );
    FJTextView.setId( 101010 );
    FJTextView.setText( StrToJCharSequence( Position.ToString + ' ) aaaaaaaaaaaaaaaaaa' ) );
    r.addView( FJTextView );
    end
    else
    begin
    r          := TJLinearLayout.Wrap( ( cellView as ILocalObject ).GetObjectID );
    v          := r.findViewById( 101010 );
    FJTextView := TJTextView.Wrap( ( v as ILocalObject ).GetObjectID );
    if FJTextView <> nil then
    begin
    FJTextView.setTextColor( TAlphaColors.Red );
    FJTextView.setText( StrToJCharSequence( Position.ToString + ' ) aaaaaaaaaaaaaaaaaa' ) );
    end;
    end;

    FDPFJListView.FJDPFListView.setCustomView( r );
  *)
end;

// ------------------------------------------------------------------------------
procedure TDPFOnListViewListener.onItemSelected( listview: JListView; childView: JView; position: Integer; id: Int64 ); cdecl;
begin
  if Assigned( FDPFJListView.FOnItemSelected ) then
    FDPFJListView.FOnItemSelected( FDPFJListView, position, id );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnListViewListener.onNothingSelected( listview: JListView ); cdecl;
begin
  if Assigned( FDPFJListView.FOnNothingSelected ) then
    FDPFJListView.FOnNothingSelected( FDPFJListView );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnListViewListener.onItemClick( listview: JListView; view: JView; position: integer; id: Int64 ); cdecl;

begin
  FDPFJListView.FSelectedLists.Remove( position );
  FDPFJListView.FSelectedLists.Add( position );
  FDPFJListView.FLastItemSelected := position;
  id                              := view.getId;
  if Assigned( FDPFJListView.OnItemClick ) then
    FDPFJListView.OnItemClick( FDPFJListView, position, id );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
