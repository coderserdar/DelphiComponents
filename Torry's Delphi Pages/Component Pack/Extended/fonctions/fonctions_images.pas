unit fonctions_images;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\Compilers.inc}
{$I ..\extends.inc}

uses Forms,
{$IFDEF FPC}
        LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  DB, Graphics, Classes, Controls, Dialogs ;

const CST_IMAGE_MAUVAISE_TAILLE = 'La taille de l''image doit être au moins de 32 sur 32.' ;
      CST_IMAGE_DEFORMATION = 'L''image sera déformée, continuer ?' ;
      CST_IMAGE_MAUVAISE_IMAGE = 'Mauvais format d''image.' ;
{$IFDEF VERSIONS}
  gVer_fonctions_images : T_Version = ( Component : 'Gestion des images' ; FileUnit : 'fonctions_images' ;
                        			             Owner : 'Matthieu Giroux' ;
                        			              Comment : 'Chargement des icônes et bitmap ( vérifier des erreurs éventuelles avec Memproof ).' + #13#10 + 'Gestion des images.' ;
                        			              BugsStory : 'Version 1.0.0.4 : Bug couleur transparente en noir dans les imagelist.' + #13#10 +
                        			                	        'Version 1.0.0.3 : Handle à 0 après FreeImage et create des TBitmap.' + #13#10 +
                        			                	        'Version 1.0.0.2 : Suppression du RealeaseHanlde après FreeImage.' + #13#10 +
                        			                	        'Version 1.0.0.1 : Meilleure gestion des images, problèmes de rafraichissement.' + #13#10 +
                        			                	        'Version 1.0.0.0 : La gestion est en place.' + #13#10 + 'Il faut utiliser les fonctions et vérifier les erreurs éventuellement produites avec Memproof.';
                        			              UnitType : 1 ;
                        			              Major : 1 ; Minor : 0 ; Release : 0 ; Build : 4 );


{$ENDIF}

// Charge un icône ou un bitmap dans un champ bitmap si le bitmap est assez grand par rapport à ai_Taille
// aod_ChargerImage : Chargement du fichier image
// aF_FieldImage    : Champ image à enregistrer
// ai_Taille        : La taille à modifier : -1 pour ne pas modifier
// ab_MontreMessage : Interaction avec l'utilisateur
// adxb_Image       : Bitmap de visualisation
// Sortie           : Enregistré ou non

function fb_ChargeIcoBmp ( const aod_ChargerImage : TOpenDialog ;
                           const adat_DataSet     : TDataSet    ;
                           const aF_FieldImage    : TField      ;
                           const ai_Taille        : Integer     ;
                           const ab_MontreMessage : Boolean     ;
                           const adxb_Image       : TBitmap     ) : Boolean ;

procedure p_SetImageFileToField ( const afile: String; const field : TField ; const ab_ShowError : Boolean );
procedure p_SetStreamToField ( const astream: TStream; const field : TField ; const ab_ShowError : Boolean );
procedure p_SetFieldToImage ( const field : TField ; const Image : TPicture  ; const ab_ShowError : Boolean );
procedure p_SetFileToStream ( const afile : String; const Stream : TStream ; const ab_ShowError : Boolean );
procedure p_SetStreamToImage ( const stream: tStream; const Image : TPicture ; const ab_ShowError : Boolean );
procedure p_SetFileToBitmap ( const afile : String; const abmp_Image : TBitmap ; const ab_ShowError : Boolean );
procedure p_SetFileToImage ( const afile : String; const Image : TPicture ; const ab_ShowError : Boolean );

function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const ab_AjouteBitmap      ,
                                        ab_ImageDefaut       : Boolean     ;
                                  const aIma_ImagesMenus     : TImageList  ;
                                  const ai_CompteurImageDef : Integer     ) : Integer ; overload ;
// Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const aIma_ImagesMenus     : TImageList  ) : Integer ; overload ;
// Transforme un bitmap en tout petit bitmap
// Entrée : Le Bitmap source
// Sortie : Le petit bitmap
procedure p_RecuperePetitBitmap ( const abmp_BitmapOrigine : TBitmap );

// Transforme un bitmap en tout petit bitmap
// Entrée : Le Bitmap source
// Sortie : Le petit bitmap
procedure p_ChangeTailleBitmap ( const abmp_BitmapOrigine : TBitmap; const ai_Taille : Integer );

// Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aGra_IconAChanger   : Image à changer
// aGra_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_AssignDBImage ( const aFie_FieldImage     : TField   ;
                        		const aGra_IconAChanger   : TGraphic ;
                        		const aGra_DefaultPicture : TGraphic ) : Boolean ;

function fb_FichierIcoBmpVersBitmap ( const as_Fichier : String; const aBmp_Sortie : TBitmap ) : Boolean ;
// Transformation d'un champ bitmap en TIcon
// abmp_Bitmap        : Image bitmap
// aico_Destination   : Image retour icon

procedure p_BitmapVersIco ( const aBmp_Bitmap : TBitmap ; const aIco_Destination : TIcon );

implementation
uses ImagingTypes, ImagingComponents, Imaging,
{$IFDEF FPC}
     LCLType,
{$ELSE}
     JclGraphics, StrUtils,
{$ENDIF}
     SysUtils, unite_messages ;



// Transformation d'un champ bitmap en TIcon
// abmp_Bitmap        : Image bitmap
// aico_Destination   : Image retour icon

procedure p_BitmapVersIco ( const aBmp_Bitmap : TBitmap ; const aIco_Destination : TIcon );
var
  lii_IconInfo : TIconInfo;
{$IFDEF FPC}
  lpi_IconInfo : PIconInfo;
{$ENDIF}
begin
// Le bitmap doit être transparent
  aBmp_Bitmap.Transparent := True ;
  aIco_Destination.Modified := False ;
  with aIco_Destination do
    if Handle <> 0 Then
      Begin
        ReleaseHandle ;
        Handle := 0 ;
      End ;
 {Crée un icon}
  lii_IconInfo.fIcon := true; // C'est un icône
  lii_IconInfo.xHotspot := 0; // Valeurs par défaut : connaîs pas
  lii_IconInfo.yHotspot := 0;
  lii_IconInfo.hbmMask := aBmp_Bitmap.MaskHandle; // Masque de transparence
  lii_IconInfo.hbmColor := aBmp_Bitmap.Handle; // Bitmap
{$IFDEF FPC}
  lpi_IconInfo := @lii_IconInfo ;
  aIco_Destination.Handle := CreateIconIndirect(lpi_IconInfo); /// Création de l'icône
{$ELSE}
  aIco_Destination.Handle := CreateIconIndirect(lii_IconInfo); /// Création de l'icône
{$ENDIF}
  aIco_Destination.Palette := aBmp_Bitmap.Palette ; // récupère la Palette
// l'icône est transparent
  aIco_Destination.Transparent := True ;
  // Midifications faites
  aIco_Destination.PaletteModified := True ;
  aIco_Destination.Modified := True ;

end;

// Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aGra_IconAChanger   : Image à changer
// aGra_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_AssignDBImage ( const aFie_FieldImage     : TField   ;
                        		const  aGra_IconAChanger    : TGraphic ;
                        		const aGra_DefaultPicture : TGraphic ) : Boolean ;
var lBmp_Bitmap : TBitmap ;
begin
  Result := False ; // Pas d'affectation
  try
//    if not aGra_IconAChanger.Empty Then
      Begin
       if ( aGra_IconAChanger is TIcon )
       and (( aGra_IconAChanger as TIcon ).Handle <> 0 ) Then
         Begin
           ( aGra_IconAChanger as TIcon ).ReleaseHandle ;
           ( aGra_IconAChanger as TIcon ).Handle := 0 ;
         End
       else
         if  ( aGra_IconAChanger is TBitmap )
         and ( not ( aGra_IconAChanger as TBitmap ).Handle <> 0 ) Then
           Begin
{$IFDEF DELPHI}
             ( aGra_IconAChanger as TBitmap ).Dormant ;
{$ENDIF}
             ( aGra_IconAChanger as TBitmap ).FreeImage ;
             try
               ( aGra_IconAChanger as TBitmap ).Canvas.Refresh ;
             Except
             End;
             ( aGra_IconAChanger as TBitmap ).Handle := 0 ;
           End ;
      End ;
    if  Assigned ( aFie_FieldImage )
    and not aFie_FieldImage.IsNull
    and (   aFie_FieldImage.IsBlob )
     then
      Begin
        aGra_IconAChanger.Transparent := True ;
        if aGra_IconAChanger is TIcon
         Then
          begin
            Result := True ;
            lBmp_Bitmap := TBitmap.Create ;
            lBmp_Bitmap.Handle := 0 ;
          // affectation
            try
              lBmp_Bitmap.Transparent := True ;
              lBmp_Bitmap.Assign ( aFie_FieldImage );
              lBmp_Bitmap.Transparent := True ;
{$IFDEF DELPHI}
              p_BitmapVersIco ( lBmp_Bitmap, aGra_IconAChanger as TIcon );
{$ENDIF}
            except
            End;
            with lBmp_Bitmap do
              if Handle <> 0 Then
                Begin
{$IFDEF DELPHI}
                                    Dormant ;
{$ENDIF}
                  FreeImage ;
                  Handle := 0 ;
                End ;
            lBmp_Bitmap.Free;
          end
         else
          begin
          // affectation
            ( aGra_IconAChanger as TBitmap ).Assign ( aFie_FieldImage );
            Result := True ;
          end ;
        aGra_IconAChanger.Transparent := True ;
        aGra_IconAChanger.Modified := True ;
      End
    else
      if assigned ( aGra_DefaultPicture ) // Image par défaut
      and aGra_IconAChanger.Empty
      and ( aGra_IconAChanger.ClassType = aGra_DefaultPicture.ClassType ) // Image par défaut
       Then
        begin
          aGra_IconAChanger.Assign ( aGra_DefaultPicture );
          aGra_IconAChanger.Transparent := True ;
          aGra_IconAChanger.Modified := True ;
          Result := True ;
        end;
  finally
  end;
end ;

  Function Ico2Bmp (Ico : TIcon) : TBitmap;
  Begin
    Result := TBitmap.Create;
    Result.Handle := 0 ;
    Result.Width := Ico.Width;
    Result.Height := Ico.Height;
    Result.Canvas.CopyMode := cmSrcCopy;
    Result.Canvas.Draw(0, 0, Ico);
  end;
  // Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aIco_IconAChanger   : Image à changer
// aIco_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_FichierIcoBmpVersBitmap ( const as_Fichier : String; const aBmp_Sortie : TBitmap ) : Boolean;
var lIco_Icon : TIcon ;
begin
  Result := False ;
  try
    if lowercase ( ExtractFileExt ( as_Fichier )) = '.ico'
     Then
      Begin                     
        Result := True ;
        lIco_Icon := TIcon.Create ;
        lIco_Icon  .LoadFromFile   ( as_Fichier );
        aBmp_Sortie.Width := lIco_Icon.Width;
        aBmp_Sortie.Height := lIco_Icon.Height;
        aBmp_Sortie.Canvas.CopyMode := cmSrcCopy;
        aBmp_Sortie.Canvas.Draw(0, 0, lIco_Icon);
//        aBmp_Sortie.Handle := IconToBitmap ( lIco_Icon.Handle ) ;
        aBmp_Sortie.Modified := True ;
        lIco_Icon.ReleaseHandle ;
        lIco_Icon.Free ;
      End
     Else
      Begin
        aBmp_Sortie.LoadFromFile ( as_Fichier );
      End ;
    aBmp_Sortie.Transparent := True ;
  except
      MessageDlg(CST_IMAGE_MAUVAISE_IMAGE, mtWarning, [mbOk], 0);
  End ;
end ;

// Transforme un bitmap en tout petit bitmap
// Entrée : Le Bitmap source
// Sortie : Le petit bitmap
procedure p_ChangeTailleBitmap ( const abmp_BitmapOrigine : TBitmap; const ai_Taille : Integer );

var
  lrec_Rectangle      : TRect ;  // Nouvelle taille
  lbmp_Tempo          : TBitmap ;
Begin
  lbmp_Tempo := TBitmap.Create ; // Création petit bitmap
  lbmp_Tempo.Handle := 0 ;
  lbmp_Tempo.Width   := ai_Taille ;
  lbmp_Tempo.Height  := ai_Taille ;
  lrec_Rectangle.Left := 0 ;
  lrec_Rectangle.Top  := 0 ;
  lrec_Rectangle.Right  := ai_Taille ;
  lrec_Rectangle.Bottom := ai_Taille ;
  lbmp_Tempo.Canvas.StretchDraw ( lrec_Rectangle, abmp_BitmapOrigine );
  lbmp_Tempo.Width   := ai_Taille ;
  lbmp_Tempo.Height  := ai_Taille ;
  lbmp_Tempo.Transparent := True ;
  lbmp_Tempo.Modified := True ;
  // 2004-10-20 : MAJ destruction bitmap
  with abmp_BitmapOrigine do
    if Handle <> 0 Then
      Begin
{$IFDEF DELPHI}
        Dormant ;
{$ENDIF}
        FreeImage ;
        Handle := 0 ;
      End ;
  abmp_BitmapOrigine.Assign ( lbmp_Tempo );
  try
{$IFDEF DELPHI}
                         lbmp_Tempo.Dormant ;
{$ENDIF}
       lbmp_Tempo.FreeImage ;
       lbmp_Tempo.Handle := 0 ;
  finally
       lbmp_Tempo.Free;
  End ;
end ;
procedure p_RecuperePetitBitmap ( const abmp_BitmapOrigine : TBitmap );

Begin
  p_ChangeTailleBitmap ( abmp_BitmapOrigine, 16 );
end ;
// Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const ab_AjouteBitmap      ,
                                        ab_ImageDefaut       : Boolean     ;
                                  const aIma_ImagesMenus     : TImageList  ;
                                  const ai_CompteurImageDef : Integer     ) : Integer ;
Begin
  Result := -1 ;
  // Peut ajouter un bitmap
  if ab_AjouteBitmap
   Then
    Begin
      // Récupère le bitmap en petit
      p_RecuperePetitBitmap ( aBmp_Picture );
      // La couleur de transparence doit être celle du bitmap
      aIma_ImagesMenus.BkColor := aBmp_Picture.TransparentColor ;
      // Ajoute dans l'image liste
      aIma_ImagesMenus.AddMasked ( aBmp_Picture, aBmp_Picture.TransparentColor );
      // Libère l'image temporaire
{$IFDEF DELPHI}
      aBmp_Picture.Dormant ;
{$ENDIF}
      aBmp_Picture.FreeImage ;
      aBmp_Picture.Handle := 0 ;
      // Numero de l'image ajoutée
      Result := aIma_ImagesMenus.Count - 1 ;
    End
    // Sinon image par défaut de l'image liste
   Else
    if ab_ImageDefaut
     Then
      Result := ai_CompteurImageDef ;
End ;

// Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const aIma_ImagesMenus     : TImageList  ) : Integer ;

//var lIco_Icon : TIcon ;
Begin
  Result := -1 ;
  // Peut ajouter un bitmap
  if aBmp_Picture.Handle <> 0
   Then
    Begin
      // Récupère le bitmap en petit
      p_RecuperePetitBitmap ( aBmp_Picture );
      // La couleur de transparence doit être celle du bitmap
      aIma_ImagesMenus.BkColor := aBmp_Picture.TransparentColor ;
      // Ajoute dans l'image liste
      aIma_ImagesMenus.AddMasked ( aBmp_Picture , aBmp_Picture.TransparentColor );
      // Libère l'image temporaire
{$IFDEF DELPHI}
      aBmp_Picture.Dormant ;
{$ENDIF}
      aBmp_Picture.FreeImage ;
      aBmp_Picture.Handle := 0 ;
      // Spécififique àl'xpbar
      aIma_ImagesMenus.BkColor := clBackground ;
      // Numero de l'image ajoutée
      Result := aIma_ImagesMenus.Count - 1 ;
    End ;
End ;

// Charge un icône ou un bitmap dans un champ bitmap si le bitmap est assez grand par rapport à ai_Taille
// aod_ChargerImage : Chargement du fichier image
// aF_FieldImage    : Champ image à enregistrer
// ai_Taille        : La taille à modifier : -1 pour ne pas modifier
// ab_MontreMessage : Interaction avec l'utilisateur
// adxb_Image       : Bitmap de visualisation
// Sortie           : Enregistré ou non

function fb_ChargeIcoBmp ( const aod_ChargerImage : TOpenDialog ;
                           const adat_DataSet     : TDataSet    ;
                           const aF_FieldImage    : TField      ;
                           const ai_Taille        : Integer     ;
                           const ab_MontreMessage : Boolean     ;
                           const adxb_Image       : TBitmap     ) : Boolean ;
var lRec_Taille32  : TRect ;
    LBmp_Tempo     ,
    LBmp_Tempo2    : TBitmap ;
begin
  Result := False ;
  If aod_ChargerImage.Execute then
    begin
      if assigned ( adxb_Image )
       Then
        adxb_Image.Modified := False ;
      LBmp_Tempo := TBitmap.Create ;
      LBmp_Tempo.Handle := 0 ;
      fb_FichierIcoBmpVersBitmap ( aod_ChargerImage.FileName, LBmp_Tempo );
      // L'image n'est pas à la bonne taille
      if  ( ai_Taille         >  0  )
      and (    ( LBmp_Tempo.Width  <> ai_Taille )
           or  ( LBmp_Tempo.Height <> ai_Taille ))
       Then
        Begin
      // L'image est petite alors erreur
          if ( LBmp_Tempo.Width  < ai_Taille )
          or ( LBmp_Tempo.Height < ai_Taille )
           Then
            Begin
              if ab_MontreMessage
               Then
                MessageDlg ( CST_IMAGE_MAUVAISE_TAILLE, mtWarning, [mbOk], 0);
              Exit ;
            End
           Else
            if  (    ( LBmp_Tempo.Width  = LBmp_Tempo.Height )
      // L'image va être déformée alors avertissement
                 or  ( ab_MontreMessage and ( MessageDlg ( CST_IMAGE_DEFORMATION, mtWarning, [mbOk,mbCancel], 0) = mrOK )))
             Then
              Begin
              // Création du bitmap de conversion
								LBmp_Tempo2 := TBitmap.Create ;
								LBmp_Tempo2.Handle := 0 ;
                LBmp_Tempo2.Assign ( LBmp_Tempo );
                lRec_Taille32.Left   := 0 ;
                lRec_Taille32.Top    := 0 ;
                lRec_Taille32.Right  := ai_Taille ;
                lRec_Taille32.Bottom := ai_Taille ;
                LBmp_Tempo.Transparent := False ;

              // Conversion
                LBmp_Tempo2.Canvas.StretchDraw ( lRec_Taille32, LBmp_Tempo );
                LBmp_Tempo2.Height := ai_Taille ;
                LBmp_Tempo2.Width  := ai_Taille ;
                LBmp_Tempo2.Modified := True ;
//                LBmp_Tempo2.Canvas.Refresh ;
{$IFNDEF FPC}
								LBmp_Tempo.Dormant ;
{$ENDIF}
								LBmp_Tempo.FreeImage ;
								LBmp_Tempo.Handle := 0 ;
								LBmp_Tempo.Assign ( LBmp_Tempo2 );
								LBmp_Tempo.Transparent := True ;
								Result := True ;

								// Libération du bitmap de conversion
{$IFNDEF FPC}
								LBmp_Tempo2.Dormant ;
{$ENDIF}
								LBmp_Tempo2.FreeImage ;
								LBmp_Tempo2.Handle := 0 ;
								LBmp_Tempo2.Free ;

              End ;


        End
       Else
        Begin
         Result := True ;
        End ;
      if Result
       Then
         Begin
          if assigned ( adxb_Image )
           Then
            Begin
            // 2004-10-20 : MAJ destruction bitmap
              with adxb_Image do
                if Handle <> 0 Then
                  Begin
  {$IFNDEF FPC}
                    Dormant ;
  {$ENDIF}
                    FreeImage ;
                    Handle := 0 ;
                  End ;
              adxb_Image.Assign ( aF_FieldImage );
              adxb_Image.Modified := True ;
            End ;
          adat_DataSet.Edit ;
          aF_FieldImage.Assign ( LBmp_Tempo );
          adat_DataSet.Post ;
         end;
{$IFDEF DELPHI}
      LBmp_Tempo.Dormant ;
{$ENDIF}
      LBmp_Tempo.FreeImage ;
      LBmp_Tempo.Handle := 0 ;
      LBmp_Tempo.Free ;
    end;
End ;


// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_SetFieldToImage ( const field : TField ; const Image : TPicture  ; const ab_ShowError : Boolean );
var l_c_memory_stream: tMemoryStream;
begin
  if not ( field.IsNull ) then
    Begin
      l_c_memory_stream:= tMemoryStream.Create;
      try
        ( field as tBlobField ).SaveToStream ( l_c_memory_stream );
      Except
        On E:Exception do
         if ab_ShowError Then
            ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FIELD_IMAGE);
      end;
      p_SetStreamToImage ( l_c_memory_stream, Image, ab_ShowError );
      l_c_memory_stream.Free;
    End;

end;

// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_SetImageFileToField ( const afile: String; const field : TField ; const ab_ShowError : Boolean );
var l_c_memory_stream: tMemoryStream;
begin
  if FileExists ( afile ) then
    Begin
      l_c_memory_stream:= tMemoryStream.Create;
      p_SetFileToStream(afile,l_c_memory_stream, ab_ShowError);
      try
        ( field as tBlobField ).LoadFromStream ( l_c_memory_stream );
      Except
        On E:Exception do
         if ab_ShowError Then
            ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FIELD_IMAGE);
      end;
      p_SetStreamToField ( l_c_memory_stream, field, ab_ShowError );
      l_c_memory_stream.Free;
    End;

end;

// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_SetStreamToField ( const astream: TStream; const field : TField ; const ab_ShowError : Boolean );
begin
  if ( field is tBlobField ) then
    try
      ( field as tBlobField ).LoadFromStream ( astream );
    Except
      On E:Exception do
       if ab_ShowError Then
          ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_STREAM_field);
    end;

end;

procedure p_SetStreamToImage ( const stream: tStream; const Image : TPicture ; const ab_ShowError : Boolean );
var Aimagedata : TImageData;
begin
  try
    stream.Position := 0;
    aimagedata.Width   := 0;
    aimagedata.Height  := 0;
    Aimagedata.Format  := ifUnknown;
    Aimagedata.Size    := 0;
    Aimagedata.Bits    := nil;
    Aimagedata.Palette := nil;
    LoadImageFromStream( stream, aimagedata );
    ConvertDataToBitmap( aimagedata, Image.Bitmap );
    Image.Bitmap.Canvas.Refresh;
  Except
    On E:Exception do
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_STREAM_IMAGE);
  end;
end;

procedure p_SetFileToStream ( const afile : String; const Stream : TStream ; const ab_ShowError : Boolean );
var Aimagedata : TImageData;
begin
  try
    aimagedata.Width   := 0;
    aimagedata.Height  := 0;
    Aimagedata.Format  := ifUnknown;
    Aimagedata.Size    := 0;
    Aimagedata.Bits    := nil;
    Aimagedata.Palette := nil;
    LoadImageFromFile  ( afile, aimagedata );
    SaveImageToStream( 'JPG', Stream, aImageData);
  Except
    On E:Exception do
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_File_IMAGE);
  end;
end;

procedure p_SetFileToBitmap ( const afile : String; const abmp_Image : TBitmap ; const ab_ShowError : Boolean );
var Aimagedata : TImageData;
begin
  try
    aimagedata.Width   := 0;
    aimagedata.Height  := 0;
    Aimagedata.Format  := ifUnknown;
    Aimagedata.Size    := 0;
    Aimagedata.Bits    := nil;
    Aimagedata.Palette := nil;
    LoadImageFromFile  ( afile, aimagedata );
    ConvertDataToBitmap( aimagedata, abmp_Image );
  Except
    On E:Exception do
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_File_IMAGE);
  end;
end;


procedure p_SetFileToImage ( const afile : String; const Image : TPicture ; const ab_ShowError : Boolean );
begin
  p_SetFileToBitmap ( afile, Image.Bitmap, ab_ShowError );
  Image.Bitmap.Canvas.Refresh;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_fonctions_images );
{$ENDIF}
end.
