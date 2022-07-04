unit QREDBImage;
{
 TQREDBImage 1.0 (Enhaced TQRDBImage):
  by Sebastián Mayorá - Argentina - DelphiHelper@yahoo.com.ar
 Please read QREDBImage.txt or readme.txt for more information

 Bug in this version (1.2)
 ShinkToFit must ALWAYS be TRUE, if dont some bands are not diplayed

}

{ TODO :
Cargar algunas imágenes para probar nuevamente.
Probar Imprimir también, hacer un mix de los demos }
interface

uses
  Windows,  Classes,{Tstream } Graphics {TGraphic}, Db{field}, Controls{csopaque,csframe}, Forms {screen},
  QuickRpt, uEdbr {main rutines};

type

  TQREDBImage = class(TQRPrintable)
  private
    FField : TField;
    FDataSet : TDataSet;
    FDataField : string;
    FPicture: TPicture;
    FShrinkToFit: boolean;
    FCenter: boolean;
    FPictureLoaded: boolean;
    fOnLoadCustomImage: TLoadCustomImageEvent;
    FZoomToFit: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSet(Value: TDataSet);
    procedure SetPicture(Value: TPicture);
    procedure SetShrinkToFit(Value: Boolean);
    procedure SetZoomToFit(const Value: Boolean);
  protected
    Memoria: TMemoryStream;
    function GetPalette: HPALETTE; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Prepare; override;
    procedure Print(OfsX, OfsY : integer); override;
    procedure UnPrepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPicture;//Si hubiera sido virtual en la clase ancestro...
    property Field: TField read FField;
    property Picture: TPicture read FPicture write SetPicture;
  published
    property Center: boolean read FCenter write SetCenter default True;
    property DataField: string read FDataField write SetDataField;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property ShrinkToFit: boolean read FShrinkToFit write SetShrinkToFit default True;
    property ZoomToFit : Boolean read FZoomToFit write SetZoomToFit default False;
    property OnLoadCustomImage :TLoadCustomImageEvent read fOnLoadCustomImage write fOnLoadCustomImage;
  end;

implementation
uses  JPeg, Dialogs;

constructor TQREDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];
  Width := 105;
  Height := 105;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FCenter := True;
  Memoria := TMemoryStream.Create;
  fShrinkToFit := True;
  fZoomToFit   := False;
end;

destructor TQREDBImage.Destroy;
begin
  FPicture.Free;
  Memoria.Free;
  inherited Destroy;
end;

procedure TQREDBImage.Prepare;
begin
  inherited Prepare;
  if assigned(FDataSet) then
    begin
    FField := DataSet.FindField(FDataField);
    if Field is TBlobField then
      Caption := '';
//    LoadPicture;
    end
  else
    FField := nil;
end;
Type TCrack = Class(TGRaphic);

procedure TQREDBImage.Print(OfsX, OfsY : integer);
var
  H: integer;
  Dest: TRect;
  DrawPict: TPicture;
begin
  with QRPrinter.Canvas do
    begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    DrawPict := TPicture.Create;
    H := 0;
    try
      if Assigned(FField) and (FField is TBlobField) then
        begin
        if not fPictureLoaded then
          LoadPicture;
        DrawPict.Assign(Picture);
        if (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
          begin
          H := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
          RealizePalette(Handle);
          end;
        Dest.Left   := QRPrinter.XPos(OfsX + Size.Left);
        Dest.Top    := QRPrinter.YPos(OfsY + Size.Top);
        Dest.Right  := QRPrinter.XPos(OfsX + Size.Width + Size.Left);
        Dest.Bottom := QRPrinter.YPos(OfsY + Size.Height + Size.Top);
        if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
           FillRect(Dest)
        else
          if (fShrinkToFit and ((DrawPict.Height > Height ) or (DrawPict.width > width)) ) or
              (fZoomToFit and ((DrawPict.Height < Height ) or (DrawPict.width < width))) then
            with QRPrinter.Canvas do
              StretchDraw( Rescale(Dest, DrawPict), DrawPict.Graphic) {1.5}
          else
            begin
            IntersectClipRect(Handle, Dest.Left, Dest.Top, Dest.Right, Dest.Bottom);
            Dest.Right := Dest.Left +
              Round(DrawPict.Width / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.XFactor);
            Dest.Bottom := Dest.Top +
              Round(DrawPict.Height / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.YFactor);
            if Center then OffsetRect(Dest,
              (QRPrinter.XSize(Size.Width) -
                Round(DrawPict.Width / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.XFactor)) div 2,
              (QRPrinter.YSize(Size.Height) -
                Round(DrawPict.Height / Screen.PixelsPerInch * 254 * ParentReport.QRPrinter.YFactor)) div 2);
            QRPrinter.Canvas.StretchDraw(Dest, DrawPict.Graphic);
            SelectClipRgn(Handle, 0);
            end;
        end;
    finally
      if H <> 0 then SelectPalette(Handle, H, True);
      DrawPict.Free;
      fPictureLoaded := False;
    end;
  end;
  inherited Print(OfsX,OfsY);
end;

procedure TQREDBImage.Unprepare;
begin
  FField := nil;
  inherited Unprepare;
end;

procedure TQREDBImage.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TQREDBImage.SetDataField(const Value: string);
begin
  FDataField := Value;
end;

function TQREDBImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

procedure TQREDBImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
    begin
    FCenter := Value;
    Invalidate;
    end;
end;

procedure TQREDBImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TQREDBImage.SetShrinkToFit(Value: Boolean);
begin
  if FShrinkToFit <> Value then
    begin
    FShrinkToFit := Value;
    Invalidate;
    end;
end;

procedure TQREDBImage.Paint;
var
  W, H: Integer;
  R: TRect;
  S: string;
begin
  with Canvas do
    begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Font := Self.Font;
    if Field <> nil then
      S := Field.DisplayLabel
    else
      S := Name;
    S := '(' + S + ')';
    W := TextWidth(S);
    H := TextHeight(S);
    R := ClientRect;
    TextRect(R, (R.Right - W) div 2, (R.Bottom - H) div 2, S);
    end;
  inherited Paint;
end;

procedure TQREDBImage.PictureChanged(Sender: TObject);
begin
  FPictureLoaded := True;
  Invalidate;
end;

procedure TQREDBImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSet) then
    DataSet := nil;
end;

procedure TQREDBImage.LoadPicture;
begin
 try
   LoadPictureEx(Memoria, Picture, Field,fOnLoadCustomImage, False);
 except
     MessageDlg('No puedo cargar la imagen', mtError, [mbOK], 0);
 end;
end;

procedure TQREDBImage.SetZoomToFit(const Value: Boolean);
begin
  if FZoomToFit <> Value then
    begin
    FZoomToFit := Value;
    Invalidate;
    end;
end;

end.

