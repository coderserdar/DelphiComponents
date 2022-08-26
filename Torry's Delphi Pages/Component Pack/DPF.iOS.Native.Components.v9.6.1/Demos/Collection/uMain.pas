unit uMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UIImageView,
  DPF.iOS.UILabel,
  DPF.iOS.UIFont,
  DPF.iOS.UIView,
  DPF.iOS.UICollectionView,
  DPF.iOS.UIButton,
  DPF.iOS.UISwitch;

type
  TFCollection = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUICollectionView1: TDPFUICollectionView;
    DPFUIView2: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFSwitch1: TDPFSwitch;
    DPFLabel1: TDPFLabel;
<<<<<<< .mine
    procedure DPFUICollectionView1DrawCell( Sender: TObject; Section, ItemNo: Integer; FrameSize: DPFNSize; isSelected: Boolean; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
=======
>>>>>>> .r1202
    procedure DPFUICollectionView1GetNumberOfSections( Sender: TObject; var NumberOfSections: Integer );
<<<<<<< .mine
    procedure DPFUICollectionView1GetNumberOfItemInSection( Sender: TObject; SectionNo: Integer; var numberOfRowsInSection: Integer );
    procedure DPFUICollectionView1ItemSize( Sender: TObject; Section, ItemNo: Integer; var Width, Height: Single );
=======
    procedure DPFUICollectionView1GetNumberOfItemInSection( Sender: TObject; SectionNo: NativeInt; var numberOfRowsInSection: Integer );
    procedure DPFUICollectionView1ItemSize( Sender: TObject; Section, ItemNo: NativeInt; var Width, Height: Single );
>>>>>>> .r1202
    procedure DPFButton1Click( Sender: TObject );
    procedure FormCreate( Sender: TObject );
<<<<<<< .mine
    procedure DPFUICollectionView1SelectCell( Sender: TObject; Section, ItemNo: Integer; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
    procedure DPFUICollectionView1DeSelectCell( Sender: TObject; Section, ItemNo: Integer; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
=======
    procedure DPFUICollectionView1SelectCell( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
    procedure DPFUICollectionView1DeSelectCell( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
>>>>>>> .r1202
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure DPFUICollectionView1DrawCell( Sender: TObject; Section, ItemNo: NativeInt; FrameSize: DPFNSize; isSelected: Boolean; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
  private
    { Private declarations }
    idx: Integer;
  public
    { Public declarations }
  end;

var
  FCollection: TFCollection;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFCollection.DPFButton1Click( Sender: TObject );
begin
  DPFUICollectionView1.SelectItem( 0, idx, true, Integer( UICollectionViewScrollPositionTop ) );
  inc( idx );
end;

<<<<<<< .mine
procedure TFCollection.DPFUICollectionView1DrawCell( Sender: TObject; Section, ItemNo: Integer; FrameSize: DPFNSize; isSelected: Boolean; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
var
  MustBeCreate: Boolean;
  S           : string;
begin
  Handled      := true;
  MustBeCreate := length( Objects ) = 0;

  if MustBeCreate then
    SetLength( Objects, 2 );

  if MustBeCreate then
  begin
    TDPFImageView( Objects[0] )            := TDPFImageView.Create( nil );
    TDPFImageView( Objects[0] ).Position.X := 0;
    TDPFImageView( Objects[0] ).Position.Y := 0;
    TDPFImageView( Objects[0] ).Width      := FrameSize.width;
    TDPFImageView( Objects[0] ).Height     := FrameSize.height;
    TDPFImageView( Objects[0] ).CacheImage := true;

    TDPFLabel( Objects[1] )                           := TDPFLabel.Create( nil );
    TDPFLabel( Objects[1] ).TextColor                 := TAlphaColors.White;
    TDPFLabel( Objects[1] ).Alpha                     := 0.5;
    TDPFLabel( Objects[1] ).BackgroundColor           := TAlphaColors.Black;
    TDPFLabel( Objects[1] ).Width                     := FrameSize.width;
    TDPFLabel( Objects[1] ).Height                    := 20;
    TDPFLabel( Objects[1] ).Position.x                := 0;
    TDPFLabel( Objects[1] ).Position.Y                := 0;
    TDPFLabel( Objects[1] ).AdjustsFontSizeToFitWidth := true;

  end;

  S := Format( '%.*d', [2, ItemNo mod 32] ) + '.jpg';
  TDPFImageView( Objects[0] ).ClearImage;
  TDPFImageView( Objects[0] ).ImageList.text := '/Documents/' + S;
  TDPFImageView( Objects[0] ).ReloadImage;

  TDPFLabel( Objects[1] ).Text := S;

  if isSelected then
  begin
    TDPFImageView( Objects[0] ).BorderWidth := 5;
    TDPFImageView( Objects[0] ).BorderColor := TAlphaColors.Red;
  end
  else
  begin
    TDPFImageView( Objects[0] ).BorderWidth := 0;
    TDPFImageView( Objects[0] ).BorderColor := TAlphaColors.Null;
  end;
  TDPFImageView( Objects[0] ).Loaded ;

end;

=======
>>>>>>> .r1202
// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1GetNumberOfItemInSection( Sender: TObject; SectionNo: NativeInt; var numberOfRowsInSection: Integer );
begin
  numberOfRowsInSection := 1000;
end;

// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1GetNumberOfSections( Sender: TObject; var NumberOfSections: Integer );
begin
  NumberOfSections := 1;
end;

<<<<<<< .mine
procedure TFCollection.DPFUICollectionView1ItemSize( Sender: TObject; Section, ItemNo: Integer; var Width, Height: Single );
=======
// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1ItemSize( Sender: TObject; Section, ItemNo: NativeInt; var Width, Height: Single );
>>>>>>> .r1202
begin
  if IsIPad then
  begin
    Width  := 210;
    Height := 210;
  end
  else
  begin
    Width  := 100;
    Height := 100;
  end;
end;

<<<<<<< .mine
procedure TFCollection.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
=======
// ------------------------------------------------------------------------------
procedure TFCollection.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
>>>>>>> .r1202
begin
<<<<<<< .mine
  DPFUICollectionView1.AllowsMultipleSelection := DPFSwitch1.ISON;
end;

procedure TFCollection.DPFUICollectionView1SelectCell( Sender: TObject; Section, ItemNo: Integer; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
var
  s: string;
begin
=======
  DPFUICollectionView1.AllowsMultipleSelection := DPFSwitch1.ISON;
end;

// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1SelectCell( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
begin
>>>>>>> .r1202
  if Length( Objects ) < 1 then
    exit;

<<<<<<< .mine
  TDPFImageView( Objects[0] ).BorderWidth := 5;
  TDPFImageView( Objects[0] ).BorderColor := TAlphaColors.Red;
  TDPFImageView( Objects[0] ).Loaded;

=======
  TDPFUIView( Objects[0] ).BackgroundColor := TAlphaColors.Red;
  TDPFLabel( Objects[2] ).TextColor        := TAlphaColors.Yellow;
  TDPFLabel( Objects[2] ).TextAlignment    := TDPFTextAlignment.taCenter;
>>>>>>> .r1202
  S := TDPFLabel( Objects[1] ).Text;

<<<<<<< .mine
  TDPFLabel( Objects[1] ).Text := S;
=======
// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1DeSelectCell( Sender: TObject; Section: NativeInt; ItemNo: NativeInt; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
//var s: string;
begin
  if Length( Objects ) < 1 then
    exit;

  TDPFUIView( Objects[0] ).BackgroundColor := TAlphaColors.Null;
  TDPFLabel( Objects[2] ).TextColor        := TAlphaColors.White;
  TDPFLabel( Objects[2] ).TextAlignment    := TDPFTextAlignment.taLeft;
>>>>>>> .r1202
end;

<<<<<<< .mine
procedure TFCollection.DPFUICollectionView1DeSelectCell( Sender: TObject; Section, ItemNo: Integer; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl> );
=======
// ------------------------------------------------------------------------------
procedure TFCollection.DPFUICollectionView1DrawCell( Sender: TObject; Section, ItemNo: NativeInt; FrameSize: DPFNSize; isSelected: Boolean; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
var
  MustBeCreate: Boolean;
  S           : string;
>>>>>>> .r1202
begin
  Handled      := true;
  MustBeCreate := length( Objects ) = 0;

  if MustBeCreate then
    SetLength( Objects, 3 );

  if MustBeCreate then
  begin
    TDPFUIView( Objects[0] )                 := TDPFUIView.Create( nil );
    TDPFUIView( Objects[0] ).Position.X      := 0;
    TDPFUIView( Objects[0] ).Position.Y      := 0;
    TDPFUIView( Objects[0] ).Width           := FrameSize.width;
    TDPFUIView( Objects[0] ).Height          := FrameSize.height;
    TDPFUIView( Objects[0] ).BackgroundColor := TAlphaColors.Null;

    TDPFImageView( Objects[1] )            := TDPFImageView.Create( nil );
    TDPFImageView( Objects[1] ).Parent     := TDPFUIView( Objects[0] );
    TDPFImageView( Objects[1] ).Position.X := 2;
    TDPFImageView( Objects[1] ).Position.Y := 2;
    TDPFImageView( Objects[1] ).Width      := FrameSize.width - 4;
    TDPFImageView( Objects[1] ).Height     := FrameSize.height - 4;
    TDPFImageView( Objects[1] ).CacheImage := true;

    TDPFLabel( Objects[2] )                           := TDPFLabel.Create( nil );
    TDPFLabel( Objects[2] ).Parent                    := TDPFImageView( Objects[1] );
    TDPFLabel( Objects[2] ).TextColor                 := TAlphaColors.White;
    TDPFLabel( Objects[2] ).Alpha                     := 0.5;
    TDPFLabel( Objects[2] ).BackgroundColor           := TAlphaColors.Black;
    TDPFLabel( Objects[2] ).Position.x                := 2;
    TDPFLabel( Objects[2] ).Position.Y                := 2;
    TDPFLabel( Objects[2] ).Width                     := FrameSize.width - 4;
    TDPFLabel( Objects[2] ).Height                    := 20;
    TDPFLabel( Objects[2] ).AdjustsFontSizeToFitWidth := true;

  end;

  S := Format( '%.*d', [2, ItemNo mod 32] ) + '.jpg';
  TDPFImageView( Objects[1] ).ClearImage;
  TDPFImageView( Objects[1] ).ImageList.text := '/Documents/' + S;
  TDPFImageView( Objects[1] ).ReloadImage;

  TDPFLabel( Objects[2] ).Text          := S;
  TDPFLabel( Objects[2] ).TextColor     := TAlphaColors.White;
  TDPFLabel( Objects[2] ).TextAlignment := TDPFTextAlignment.taLeft;

  if isSelected then
  begin
    TDPFUIView( Objects[0] ).BackgroundColor := TAlphaColors.Red;
  end
  else
  begin
    TDPFUIView( Objects[0] ).BackgroundColor := TAlphaColors.Null;
  end;
end;

// ------------------------------------------------------------------------------
procedure TFCollection.FormCreate( Sender: TObject );
begin
  idx := 0;
end;

// ------------------------------------------------------------------------------
end.
