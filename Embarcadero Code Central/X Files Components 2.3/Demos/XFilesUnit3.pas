unit XFilesUnit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, XDBGrids, StdCtrls, ExtCtrls, ComCtrls, Mask
  {$IFDEF VER120}, ImgList{$ENDIF} {$IFNDEF VER110}, JPEG{$ENDIF}, XQRGrids;

type
  TXFilesForm3 = class(TForm)
    SaveDialog: TSaveDialog;
    BioLifeDataSource: TDataSource;
      BioLife: TTable;
        BioLifeSpeciesNo: TFloatField;
        BioLifeCategory: TStringField;
        BioLifeCommon_Name: TStringField;
        BioLifeSpeciesName: TStringField;
        BioLifeLengthcm: TFloatField;
        BioLifeLength_In: TFloatField;
        BioLifeNotes: TMemoField;
        BioLifeGraphic: TGraphicField;
    BioLifePanel: TPanel;
      BioLifeDBGrid: TXDBGrid;
      BioLifeQRGrid: TXQRGrid;
      ImageListBMP: TImageList;
      ImageListICO: TImageList;
    OperatePanel: TPanel;
      CommentsPanel: TPanel;
        ImageOffsetXLabel: TLabel;
        ImageOffsetYLabel: TLabel;
        ImagesGroupBox: TGroupBox;
        ImageJPG1: TImage;
        ImageJPG2: TImage;
        ImageJPG3: TImage;
        ImageJPG4: TImage;
        ImageWMF1: TImage;
        ImageWMF2: TImage;
        ImageWMF3: TImage;
        ImageWMF4: TImage;
        ImageDrawRadioGroup: TRadioGroup;
        AlignmentRadioGroup: TRadioGroup;
        VAlignmentRadioGroup: TRadioGroup;
        OptionsGroupBox: TGroupBox;
        TransparentCheckBox: TCheckBox;
        WallpaperCheckBox: TCheckBox;
        PictureRadioGroup: TRadioGroup;
        WallpapersGroupBox: TGroupBox;
        ImageJPG5: TImage;
        ImageJPG6: TImage;
        ImageJPG7: TImage;
        ImageJPG8: TImage;
        ImageJPG9: TImage;
        ImageJPG10: TImage;
        ImageJPG11: TImage;
        ImageJPG12: TImage;
        ImageJPG13: TImage;
        ImageOffsetXMaskEdit: TMaskEdit;
        ImageOffsetYMaskEdit: TMaskEdit;
        ImageOffsetXUpDown: TUpDown;
        ImageOffsetYUpDown: TUpDown;
      ReportPanel: TPanel;
        ReportLabel: TLabel;
        PreviewButton: TButton;
        ShowReportButton: TButton;
        SaveReportButton: TButton;
        PrintButton: TButton;
        PrintAllButton: TButton;
        AlignLabel: TLabel;
        AlignComboBox: TComboBox;
        PartLabel: TLabel;
        PartMaskEdit: TMaskEdit;
        PartUpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure BioLifeCategoryGetText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure BioLifeLengthcmGetText(Sender: TField; var Text: String; DisplayText: Boolean);
    {OnCalcImageIndex & OnPaintColumnCell}
    procedure BioLifeDBGridCalcImageIndex(Sender: TObject; Column: TXColumn; var Index: Integer);
    procedure BioLifeDBGridPaintColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TXColumn; Highlight: Boolean; Selections: TSelections;
      var Color: TColor; Font: TFont; var Image: TPersistent);
    {Picture & Wallpaper demo modes}
    procedure PictureClick(Sender: TObject);
    procedure WallpaperClick(Sender: TObject);
    {Options or Images changed}
    procedure OptionsChanged(Sender: TObject);
    procedure ImageJPGClick(Sender: TObject);
    procedure ImageWMFClick(Sender: TObject);
    {Print & Preview}
    procedure PreviewButtonClick(Sender: TObject);
    procedure ShowReportButtonClick(Sender: TObject);
    procedure SaveReportButtonClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure PrintAllButtonClick(Sender: TObject);
    procedure ReportChange(Sender: TObject);
  private
    Wallpaper: TImage;
    procedure LoadJPGImages;
  end;

var
  XFilesForm3: TXFilesForm3;

implementation

{$R *.DFM}

{This demo ilustrate using pictures and memos by XDBGrid. Graphic column show  }
{image from ftGraphic field. Notes column retrieve text from ftMemo field.     }
{Pictures that are showing into "Length" column are using only for demonstrate }
{variety graphic formats handled by XDBGrid. You may show images retrived from }
{fields, ImageLists or from resources.                                         }
{XDBGrid can handle any registered graphic format: BMP, ICO, WMF, JPG, GIF,... }
{Procedure LoadJPGImages is defined for Delphi 3 compatibility.                }
{In higher Delphi version you may store JPG graphics into Image components.    }

procedure TXFilesForm3.LoadJPGImages;{$IFNDEF VER110}
var
  Resource: TResourceStream;
  JPGImage: TJPEGImage;
  I: Integer;{$ENDIF}
begin {$IFNDEF VER110}
  JPGImage := TJPEGImage.Create;
  for I := 1 to 13 do
  begin
    Resource := TResourceStream.Create(HInstance, 'JPG'+IntToStr(I), RT_RCDATA);
    JPGImage.LoadFromStream(Resource);
    TImage(FindComponent('ImageJPG'+IntToStr(I))).Picture.Assign(JPGImage);
    Resource.Free;
  end;
  JPGImage.Free;{$ENDIF}
end;

procedure TXFilesForm3.FormCreate(Sender: TObject);
begin
  LoadJPGImages;
  AlignComboBox.ItemIndex := Ord(BioLifeQRGrid.ReportAlign);
  PartUpDown.Position := BioLifeQRGrid.ReportPart;
end;

procedure TXFilesForm3.BioLifeCategoryGetText(Sender: TField; var Text: String;
  DisplayText: Boolean);
const
  Str = '%s'#13'%s'#13'(%s)';
begin
  Text := Format(Str, [BioLifeCategory.AsString, BioLifeCommon_Name.AsString,
    BioLifeSpeciesName.AsString]);
end;

procedure TXFilesForm3.BioLifeLengthcmGetText(Sender: TField; var Text: String;
  DisplayText: Boolean);
const
  Str = '%f "'#13'(%d cm)';
begin
  Text := Format(Str, [BioLifeLength_In.AsFloat, BioLifeLengthcm.AsInteger]);
end;

{OnCalcImageIndex & OnPaintColumnCell}

procedure TXFilesForm3.BioLifeDBGridCalcImageIndex(Sender: TObject; Column: TXColumn; var Index: Integer);
begin
  {Calculate ImageIndex for column "Length". If Images list is defined, image  }
  {is drawing from Images. If Images is nil, you may setup image to draw into  }
  {OnPaintColumnCell event.                                                    }
  if (Column <> nil) and (Column.FieldName = 'Length (cm)') then
  begin
    Index := (BioLifeLengthCm.AsInteger-1) div 50;
    if Index > 3 then Index := 3 else if Index < 0 then Index := 0;
  end;
end;

procedure TXFilesForm3.BioLifeDBGridPaintColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TXColumn; Highlight: Boolean;
  Selections: TSelections; var Color: TColor; Font: TFont; var Image: TPersistent);
var
  ImageIndex: Integer;
begin
  if Wallpaper <> nil then {Handle pictures drawing for "Wallpaper demo mode". }
  begin
    Image := Wallpaper.Picture;
    if not Highlight and (StrToInt(Copy(Wallpaper.Name, 9, 2)) >= 10) then Font.Color := clYellow;
    if Highlight and (StrToInt(Copy(Wallpaper.Name, 9, 2)) < 10) then Font.Color := clNavy;
  end else                 {Handle pictures drawing for "Picture demo mode".   }
  if Column.FieldName = 'Length (cm)' then
  begin
    BioLifeDBGridCalcImageIndex(Self, Column, ImageIndex);
    case PictureRadioGroup.ItemIndex of
      3: Image := TImage(FindComponent('ImageJPG'+IntToStr(ImageIndex+1))).Picture;
      4: Image := TImage(FindComponent('ImageWMF'+IntToStr(ImageIndex+1))).Picture;
    end;
  end else {Pictures are drawing by XDBGrid from Column.Images list.           }
end;

{Picture & Wallpaper demo modes}

procedure TXFilesForm3.PictureClick(Sender: TObject);
var
  I: Integer;
begin
  if Wallpaper <> nil then {Setup columns layout for "Picture demo mode"       }
  begin
    Wallpaper := nil;
    for I := 0 to BioLifeDBGrid.Columns.Count-1 do
    with BioLifeDBGrid.Columns[I] do
    begin
      ImageDraw := idCenter;
      ImageOffsetX := 0;
      ImageOffsetY := 0;
      Alignment := taCenter;
      VAlignment := tvCenter;
      Transparent := False;
      Wallpaper := False;
    end;
    BioLifeDBGrid.ColumnByName('Notes').Alignment := taLeftJustify;
    BioLifeDBGrid.ColumnByName('Graphic').ImageDraw := idStretch;
    BioLifeDBGrid.ColumnByName('Graphic').ImageOffsetX := 155;
    BioLifeDBGrid.FixedCols := 1;
    ImageDrawRadioGroup.ItemIndex := Ord(idCenter);
    WallpaperCheckBox.Checked := False;
  end;
  with BioLifeDBGrid.ColumnByName('Length (cm)') do
  case PictureRadioGroup.ItemIndex of {Setup Images for column "Length".       }
    1: Images := ImageListBMP;
    2: Images := ImageListICO;
  else Images := nil; {Use PaintColumnCell for drawing other graphic formats.  }
  end;
  BioLifeDBGrid.Refresh;
end;

procedure TXFilesForm3.WallpaperClick(Sender: TObject);
begin
  if Wallpaper = nil then {Setup columns layout for "Wallpaper demo mode"      }
  begin
    PictureRadioGroup.ItemIndex := 0;
    ImageDrawRadioGroup.ItemIndex := Ord(idTileGrid);
    BioLifeDBGrid.FixedCols := 0;
    WallpaperCheckBox.Checked := True;
    Wallpaper := TImage(Sender);
    OptionsChanged(nil);
  end
  else Wallpaper := TImage(Sender);
  BioLifeDBGrid.Refresh;
end;

{Options or Images changed}

procedure TXFilesForm3.OptionsChanged(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to BioLifeDBGrid.Columns.Count-1 do
  with BioLifeDBGrid.Columns[I] do
    if (Self.Wallpaper <> nil) or (FieldName = 'Length (cm)') then
    begin
      ImageDraw := TImageDraw(ImageDrawRadioGroup.ItemIndex);
      ImageOffsetX := ImageOffsetXUpDown.Position;
      ImageOffsetY := ImageOffsetYUpDown.Position;
      Alignment := TAlignment(AlignmentRadioGroup.ItemIndex);
      VAlignment := TVAlignment(VAlignmentRadioGroup.ItemIndex);
      Transparent := TransparentCheckBox.Checked;
      Wallpaper := WallpaperCheckBox.Checked;
    end;
end;

procedure TXFilesForm3.ImageJPGClick(Sender: TObject);
begin
  PictureRadioGroup.ItemIndex := 3;
end;

procedure TXFilesForm3.ImageWMFClick(Sender: TObject);
begin
  PictureRadioGroup.ItemIndex := 4;
end;

{Print & Preview}

{In this demo you may show, how many XDBGrid settings may be changed by XQRGrid}

procedure TXFilesForm3.PrintButtonClick(Sender: TObject);
begin
  BioLifeQRGrid.Print;
end;

procedure TXFilesForm3.PrintAllButtonClick(Sender: TObject);
begin
  BioLifeQRGrid.PrintAll;
end;

procedure TXFilesForm3.PreviewButtonClick(Sender: TObject);
begin
  BioLifeQRGrid.Preview;
end;

procedure TXFilesForm3.ShowReportButtonClick(Sender: TObject);
begin
  BioLifeQRGrid.ShowReport;
end;

procedure TXFilesForm3.SaveReportButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then BioLifeQRGrid.SaveReport(SaveDialog.FileName);
end;

procedure TXFilesForm3.ReportChange(Sender: TObject);
begin
  BioLifeQRGrid.ReportAlign := TXReportAlign(AlignComboBox.ItemIndex);
  BioLifeQRGrid.ReportPart := PartUpDown.Position;
end;

end.
