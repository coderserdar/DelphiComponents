unit uEDBR;

{$I Definitions.inc}

PLEASE, read Readme.txt before continue, it is VERY important. After reading remove this line. Thanks you

//to disable GraphicEx from Mike Lischke comment next line (put // at the beginning)
{$DEFINE GraphicEX}
{$IFDEF GraphicEX}
  {$I GraphicConfiguration.inc}
{$ENDIF}

interface
uses windows , db, {$IFDEF GraphicEX} GraphicEx, {$ENDIF} jpeg, Graphics, classes;

//Evento para cargar imágenes personalizadas desde el stream guardado
type
  TLoadCustomImageEvent = procedure (var B: TGraphic; Stream: TStream)of object;


procedure LoadPictureEx(Memoria: TMemoryStream;Picture: TPicture; Field:TField; Evento:TLoadCustomImageEvent; Loaded: Boolean);
function Rescale(ARect: TRect; DrawPict: TPicture): TRect;

implementation

procedure LoadPictureEx(Memoria: TMemoryStream;Picture: TPicture; Field:TField; Evento:TLoadCustomImageEvent; Loaded: Boolean);
var B: TGraphic;            Buf: word;
    {$IFDEF GraphicEX}
    GraphicClass: TGraphicExGraphicClass;
    {$ELSE}
    GraphicClass: TGraphicClass;
    {$ENDIF}
begin
   if (Field = nil) or not Field.IsBlob or Field.IsNull then
     begin
     Picture.Assign(Nil);
     Exit;
     end;

  if not Loaded and (not Assigned(Field) or Field.IsBlob) then
    try
      Memoria.Clear;
      TBlobField(Field).SaveToStream(Memoria);
      Memoria.Position :=0;
      B:= nil;

    {$IFDEF GraphicEX}
      GraphicClass := FileFormatList.GraphicFromContent(Memoria);
    {$ELSE}
      GraphicClass := nil;
    {$ENDIF}

      if GraphicClass = nil then
        begin
        if Memoria.Size > SizeOf(buf) then
          begin
          Memoria.Read(buf, SizeOf(buf));
          case buf of
            $0000 : B := TIcon.Create;
            $0001 : begin
                    Picture.Assign({ FDataLink. }Field);
                    Exit;
                    end;
            $4D42 : B := TBitmap.Create;
            {$IFDEF TIFFGraphic}
            $4D4D : B := TTIFFGraphic.Create;
            {$ENDIF}
            {$IFDEF GIFGraphic}
            $4947 : B := TGIFGraphic.Create;
            {$ENDIF}
            $CDD7 : B := TMetafile.Create;
            $D8FF : B := TJPEGImage.Create;
          end;
          end
          end
        else
          begin       // GraphicFromContent always returns TGraphicExGraphicClass
          B := GraphicClass.Create;
          end;

      Memoria.Position := 0;
      if B <> nil then
        try
          B.LoadFromStream(Memoria);
        except
          B:= nil;
        end;
  //if the stored image is not ico, bmp, wmf, emf, jpg, jpeg, gif, tiff, tif
  //or I cant load it then fire the event
      if (b = nil) and Assigned(Evento) then
        try
          Memoria.Position := 0;
          Evento(B, Memoria);
        except
          B:= nil;
        end;

        Picture.Assign(B);//B can be NIL, no problem with that
    finally
      B.Free;
      Memoria.Clear;
    end;
end;

function Rescale(ARect: TRect;DrawPict: TPicture): TRect;
var dw, dh: real; cw, ch, x, med: Integer;
begin
  if (DrawPict.Width <=0) or (DrawPict.Height<=0) then
    begin
    Result := arect;
    Exit;
    end;
  cw := (ARect.Right - ARect.Left);
  ch := (ARect.Bottom - ARect.Top);
  dw := DrawPict.Width  / cw  ;
  dh := DrawPict.Height / ch  ;
  if dw > dh then
    begin
    x := Trunc(DrawPict.Height * cw/ DrawPict.width);
    med := Trunc((ch - x)/ 2);
    Result := Rect(ARect.Left, ARect.Top+med, ARect.Left+cw, ARect.Top+med + x );
    end
  else
    begin
    x := Trunc(DrawPict.Width * ch/ DrawPict.Height);
    Med := Trunc((cw - x)/ 2);
    Result := Rect(ARect.Left+med, ARect.Top,  ARect.Left+med + x ,ARect.Top+ ch);
    end;
end;


end.

