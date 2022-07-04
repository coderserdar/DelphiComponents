unit fonctions_db;

interface

{$I ..\Compilers.inc}
{$I ..\extends.inc}

uses SysUtils,
  {$IFDEF EADO}
     ADODB,
  {$ENDIF}
  {$IFDEF DELPHI_9_UP}
     WideStrings,
  {$ENDIF}
     DB,
     Graphics,
     fonctions_version,
     Controls,
     Classes ;

const


  CST_SQL_ORDER_BY = 'ORDER BY ' ;
  CST_SQL_ASC  = ' ASC'  ;
  CST_SQL_DESC = ' DESC' ;
  CST_SQL_ORDER_INTERLEAVING = 1 ;
  CST_DELPHI_FIELD_STRING = [ftString, ftFmtMemo, ftMemo, ftFixedChar, ftWideString];
  gVer_fonctions_db : T_Version = ( Component : 'Gestion des données' ; FileUnit : 'fonctions_db' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions gestion des données partagées.' ;
                        			                 BugsStory : 'Version 1.2.0.0 : Fonctions sur les composants de données dans fonctions_dbcomponents.' + #13#10 +
                        			                	        	 'Version 1.1.0.1 : Changement de nom de procédures non utilisées, bug mode synchrone lent.' + #13#10 +
                        			                	        	 'Version 1.1.0.0 : Mode asynchrone.' + #13#10 +
                        			                	        	 'Version 1.0.8.0 : Fonction fb_RefreshDatasetIfEmpty.' + #13#10 +
                        			                	        	 'Version 1.0.7.0 : Toutes les fonctions de déplacement.' + #13#10 +
                        			                	        	 'Version 1.0.6.0 : Procédure p_InitNavigateurBoutonsDeplacement.' + #13#10 +
                        			                	        	 'Version 1.0.5.0 : Fonction fb_MAJTableNumOrdre.' + #13#10 +
                        			                	        	 'Version 1.0.4.0 : Fonction fb_RefreshADODataset correcte et fonction fb_RefreshADORecord.' + #13#10 +
                        			                	        	 'Version 1.0.3.0 : Fonction fb_DatasetFilterLikeRecord pour évènement OnFilterRecord.' + #13#10 +
                        			                	        	 'Version 1.0.2.0 : Fonction fb_RefreshADODataset.' + #13#10 +
                        			                	        	 'Version 1.0.1.1 : Bug texte de recherche à rien.' + #13#10 +
                        			                	        	 'Version 1.0.1.0 : Deuxième fonction fb_Locate.' + #13#10 +
                        			                	        	 'Version 1.0.0.0 : La fonction fb_Locate a des limites sur plusieurs champs recherchés.';
                        			                 UnitType : 1 ;
                        			                 Major : 1 ; Minor : 2 ; Release : 0 ; Build : 0 );

function fs_AjouteRechercheClePrimaire ( const adat_Dataset         : TDataset    ;
                                         const as_ChampsClePrimaire : TStringList ;
                                         const avar_ValeursCle      : Variant     ;
                                         const as_ChampExclu        : String      ): String ;
function fb_ChangeEnregistrement(var avar_EnregistrementCle : Variant ; const adat_Dataset : TDataset ;
  const as_Cle: String; const ab_Sort:Boolean): Boolean;

function fb_MAJTableNumsOrdre ( const aDat_Dataset : TDataset ; const as_NomOrdre : String ; const ai_Intervalle : Longint ; const ab_DisableControls : Boolean ): Boolean ;
function fb_SetMultipleFieldToQuery ( const astl_Champs, sts_SQLQuery : TStrings ; const avar_Variant : Variant ; const adat_Dataset : TDataset ): Boolean ; overload;
function fb_SetMultipleFieldToQuery ( const astl_Champs, astl_SQLQuery : TStrings ; const avar_Variant : Variant ; const adat_Dataset : TDataset ; const ab_AddColma : Boolean ): Boolean ; overload;
function fb_Locate ( const adat_Dataset : TDataset ; const as_Champ : String ; const avar_Recherche : Variant; const alo_Options : TLocateOptions ; const ab_Trie : Boolean ): Boolean ;
function fb_LocateFilter ( const aado_Seeker : TDataset ; const as_oldFilter, as_Fields, as_Condition : String ; const avar_Records : Variant ; const ach_Separator : Char ): Boolean ;

function  fb_MAJTableNumOrdre ( const aDat_Dataset : TDataset ; const avar_Numordre : Variant ; const as_NomOrdre : String ): Boolean;
function fb_IntervertitPositions2Champs   ( const aDat_Dataset : TDataset ; const as_NomOrdre : String ; const ab_Precedent, ab_SortAsc, ab_DisableControls : Boolean ): Boolean;
function  fb_SortADataset ( const aDat_Dataset : TDataset; const as_NomChamp : String ; const ab_Desc : Boolean ) : Boolean;
procedure p_UpdateBatch ( const adat_Dataset: Tdataset);

var ge_DataSetErrorEvent : TDataSetErrorEvent = nil;
    ge_NilEvent : TDataSetErrorEvent = nil;
    // Affiche-t-on un message sur erreur ?
    gb_DBMessageOnError : Boolean =  true;

implementation

uses Variants,  Math, fonctions_erreurs, fonctions_string, unite_messages,
{$IFDEF FPC}
     SQLDB,
{$ELSE}
     DBTables,
{$ENDIF}
{$IFDEF ZEOS}
   ZDataset, ZAbstractRODataset,
 {$ENDIF}
   fonctions_proprietes, TypInfo, ExtCtrls;


function fb_LocateFilter ( const aado_Seeker : TDataset ; const as_oldFilter, as_Fields, as_Condition : String ; const avar_Records : Variant ; const ach_Separator : Char ): Boolean ;
var ls_Filter : String ;
    ls_And : String ;
Begin
  Result := False ;
  if pos ( as_Fields, ach_Separator ) <= 0 Then
    Begin
      if as_oldFilter <> ''  then
        ls_And := ' AND '
       else
        ls_And := '' ;

      ls_Filter := as_Fields + as_Condition ;
      if ( aado_Seeker.FieldByName ( as_Fields ).DataType in CST_DELPHI_FIELD_STRING )
        Then
          ls_Filter := ls_Filter + '''' + fs_stringDbQuote ( avar_Records ) + ''''
        Else
          ls_Filter := ls_Filter + fs_stringDbQuote ( VarToStr ( avar_Records )) ;
      aado_Seeker.Filter := as_oldFilter+ ls_And + ls_Filter;
      aado_Seeker.Refresh ;
      Result := aado_Seeker.RecordCount > 0 ;
    End ;
End ;


function fb_Locate(const adat_Dataset: TDataset;
  const as_Champ: String; const avar_Recherche: Variant;
  const alo_Options: TLocateOptions; const ab_Trie : Boolean ): Boolean;
var lext_Tempo ,
    lext_Recherche : Extended ;
    lb_Continue    : Boolean ;
begin
  Result := False ;
  if not assigned ( adat_Dataset )
  or ( adat_Dataset.IsEmpty )
  or VarIsArray (  avar_Recherche )
  or ( avar_Recherche = Null ) Then
    Exit ;

  lb_Continue := False ;
  lext_Recherche := 0 ;
  if assigned ( adat_Dataset.FindField ( as_Champ ))
  and ( adat_Dataset.FieldByName ( as_Champ ) is TNumericField ) Then
    try
      if VarIsStr ( avar_Recherche ) Then
        Begin
          If  ( Trim ( avar_Recherche ) <> '**' )
          and ( Trim ( avar_Recherche ) <> ''   ) Then
            lext_Recherche := StrToFloat ( avar_Recherche )
        End
      Else
        lext_Recherche := avar_Recherche ;
      If  ( Trim ( avar_Recherche ) <> '**' )
      and ( Trim ( avar_Recherche ) <> ''   ) Then
        lb_Continue := True ;
    Except
    End ;
  try
{$IFDEF EADO}
    if ab_Trie
    and assigned ( adat_Dataset.FindField ( as_Champ ))
    and ( adat_Dataset is TCustomADODataset ) Then
      ( adat_Dataset as TCustomADODataset ).Sort := as_Champ + CST_SQL_ASC ;
{$ENDIF}
    if assigned ( adat_Dataset.FindField ( as_Champ ))
    and not adat_Dataset.IsEmpty
    and ( adat_Dataset.FieldByName ( as_Champ ) is TNumericField )
    and lb_Continue Then
      with adat_Dataset do
        Begin
          First ;
          while not eof do
            Begin
              lExt_Tempo := adat_Dataset.FieldByName ( as_Champ ).AsFloat ;
              if  ( CompareValue ( lext_Tempo, lext_Recherche, 0 ) = 0 )  Then
                Begin
                  Result := True ;
                  Exit ;
                End ;
              if  ( CompareValue ( lext_Tempo, lext_Recherche, 0 ) = 1 )  Then
                Begin
                  Exit ;
                End ;
              adat_Dataset.Next ;
            End ;
        End
    Else
      Result := adat_Dataset.Locate ( as_Champ, avar_Recherche, alo_Options );
  Except
    on e: Exception do
      f_GereExceptionEvent ( E, adat_Dataset, ge_DataSetErrorEvent, False );
  End ;
end;

// Mise à jour du numéro d'ordre ( position  dans une table )
// aDat_Dataset : Dataset à changer
// ai_NumOrdre : Nouveau numéro d'ordre de la table
// as_NomOrdre : Champ d'ordonnancement
// Retour   : Y a - t-il eu une erreur
function  fb_MAJTableNumOrdre ( const aDat_Dataset : TDataset ; const avar_Numordre : Variant ; const as_NomOrdre : String ): Boolean;
Begin
  Result := False ;
  try
    aDat_Dataset.Edit ;
    aDat_Dataset.FieldByName ( as_NomOrdre ).Value := avar_Numordre ;
    aDat_Dataset.Post ;
    Result := True ;
  Except
    on e: Exception do
      f_GereException ( e, aDat_Dataset );
  End ;
End ;


// Mise à jour du numéro d'ordre ( position  dans une table )
// aDat_Dataset : Dataset à changer
// as_NomOrdre : Champ d'ordonnancement
// ab_erreur   : Y a - t-il eu une erreur
function fb_MAJTableNumsOrdre ( const aDat_Dataset : TDataset ; const as_NomOrdre : String ; const ai_Intervalle : Longint ; const ab_DisableControls : Boolean ): Boolean ;
var li_Numordre1    : Longint ;
begin
  Result := True ;
  if ab_DisableControls then
    aDat_Dataset.DisableControls;
  try
    aDat_Dataset.First ;
    li_Numordre1 := 1 ;
    // Début des numéros d'ordonancement
    // Affectation des nouveaux numéros d'ordre
    while li_Numordre1 <= 1 + aDat_Dataset.RecordCount * ai_Intervalle  do
      Begin
        if not fb_MAJTableNumOrdre ( aDat_Dataset, li_Numordre1, as_NomOrdre ) Then
          // Pa smis à jour
          Result := False ;
        aDat_Dataset.Next;
        if aDat_Dataset.Eof Then
          Break;
        inc ( li_Numordre1, ai_Intervalle );
      End;
  Except
   // Erreur = faux
    on e: Exception do
      Begin
        f_GereException ( e, aDat_Dataset );
        Result := False;
      End;
  End;
  if ab_DisableControls then
    aDat_Dataset.EnableControls;
End;

// Intervertit deux enregistrement dans le positionement
// aDat_GroupeFonctions : La dataset associé
// ab_Precedent         : Précédent ou non alors suivant
function fb_IntervertitPositions2Champs   ( const aDat_Dataset : TDataset ; const as_NomOrdre : String ; const ab_Precedent, ab_SortAsc, ab_DisableControls : Boolean ): Boolean;
var lvar_Numordre1      ,
    lvar_Numordre2      : Variant ;
    lbkm_GardeEnr     : TBookmarkStr ;
    lb_continue       : Boolean ;

begin
  Result := False ;
  if not assigned ( aDat_Dataset )
  or not ( aDat_Dataset.Active )
   Then
    Exit ;
    // Enregistement en cours
  lvar_Numordre1      := aDat_Dataset.FieldByName ( as_NomOrdre ).Value ;
  if ab_Precedent
  and aDat_Dataset.Bof
   Then
    Exit ;
  if not ab_Precedent
  and aDat_Dataset.Eof
   Then
    Exit ;
// Bookmark pour revenir à l'enregistrement sélectionné
  lbkm_GardeEnr := aDat_Dataset.Bookmark ;
  if ab_DisableControls Then
    aDat_Dataset.DisableControls ;
  try
    fb_SortADataset ( aDat_Dataset, as_NomOrdre, True );
    try
      aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
    Except
    End ;
    lb_continue := True ;
    if ab_Precedent
     Then
      Begin
      // Enregistement précédent
        aDat_Dataset.Prior ;
        if not aDat_Dataset.Bof
         Then
          Begin
            lvar_Numordre2      := aDat_Dataset.FieldByName ( as_NomOrdre ).Value ;
            if VarCompareValue ( lvar_Numordre1, lvar_Numordre2 ) = vrEqual Then
              Begin
                lb_continue := False ;
                if ( aDat_Dataset.FieldByName ( as_NomOrdre ) is TNumericField ) Then
                  Begin
                    fb_MAJTableNumsOrdre ( aDat_Dataset, as_NomOrdre, CST_SQL_ORDER_INTERLEAVING, ab_DisableControls );
                    aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
                    fb_IntervertitPositions2Champs ( aDat_Dataset, as_NomOrdre, ab_Precedent, ab_SortAsc, ab_DisableControls );
                    Exit;
                  End;
              End ;
            if lb_continue Then
              Begin
                Result := fb_MAJTableNumOrdre ( aDat_Dataset, lvar_Numordre1, as_NomOrdre );
                try
                  aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
                Except
                End ;
                Result := Result and fb_MAJTableNumOrdre ( aDat_Dataset, lvar_Numordre2, as_NomOrdre );
              End ;
          End ;
      End
     Else
      Begin
      // Enregistement précédent
        aDat_Dataset.Next ;
        if not aDat_Dataset.Eof
         Then
           Begin
            lvar_Numordre2      := aDat_Dataset.FieldByName ( as_NomOrdre ).AsInteger ;
            if VarCompareValue ( lvar_Numordre1, lvar_Numordre2 ) = vrEqual Then
              Begin
                lb_continue := False ;
                if ( aDat_Dataset.FieldByName ( as_NomOrdre ) is TNumericField ) Then
                  Begin
                    fb_MAJTableNumsOrdre ( aDat_Dataset, as_NomOrdre, CST_SQL_ORDER_INTERLEAVING, ab_DisableControls);
                    aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
                    fb_IntervertitPositions2Champs ( aDat_Dataset, as_NomOrdre, ab_Precedent, ab_SortAsc, ab_DisableControls );
                    Exit;
                  End;
              End ;
            if lb_continue Then
              Begin
                Result := fb_MAJTableNumOrdre ( aDat_Dataset, lvar_Numordre1, as_NomOrdre );
                try
                  aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
                Except
                End ;
                Result := Result and fb_MAJTableNumOrdre ( aDat_Dataset, lvar_Numordre2, as_NomOrdre );
              End ;
           End;
      End ;
     // Mise à jour de la table liée
  //  aDat_Dataset.Refresh ;
  {  if (aDat_Dataset is TADOTable) then
      TADOTable(aDat_Dataset).Sort   := ls_NomOrdre + CST_SQL_ASC
      else if (aDat_Dataset is TADOQuery) then
         TADOQuery(aDat_Dataset).Sort   := ls_NomOrdre + CST_SQL_ASC ;}
  finally
    if ab_DisableControls Then
      aDat_Dataset.EnableControls ;
    try
      aDat_Dataset.Bookmark := lbkm_GardeEnr  ;
    Except
    End ;
  End ;
end;

function fb_SetMultipleFieldToQuery ( const astl_Champs, sts_SQLQuery : TStrings ; const avar_Variant : Variant ; const adat_Dataset : TDataset ): Boolean ;
Begin
  Result := fb_SetMultipleFieldToQuery ( astl_Champs, sts_SQLQuery, avar_Variant, adat_Dataset, True )
End ;
function fb_SetMultipleFieldToQuery ( const astl_Champs, astl_SQLQuery : TStrings ; const avar_Variant : Variant ; const adat_Dataset : TDataset ; const ab_AddColma : Boolean ): Boolean ;
var li_i : Integer ;
Begin
  Result := False ;
  if astl_Champs.Count <= 1 Then
    try
      if VarIsNull ( avar_Variant ) Then
        astl_SQLQuery.Add( ' ' + astl_Champs [ 0 ] + ' IS NULL'   )
      Else
        if ((adat_Dataset.FindField ( astl_Champs [ 0 ] ) is TStringField )
        or (adat_Dataset.FindField ( astl_Champs [ 0 ] ) is TMemoField )
        or ( adat_Dataset.FindField ( astl_Champs [ 0 ] ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then
          astl_SQLQuery.Add( ' ' + astl_Champs [ 0 ] + '=''' + avar_Variant + '''' )
         Else
          astl_SQLQuery.Add( ' ' + astl_Champs [ 0 ] + '=''' + avar_Variant + '''' );
      Result := True ;
    Except
      On E: Exception do
        Begin
          f_GereException ( E, adat_Dataset );
        End ;
    End ;
  try
    for li_i := 0 to astl_Champs.Count - 1 do
      Begin
        if VarIsNull ( avar_Variant [ li_i ] ) Then
          if li_i = 0 Then
            astl_SQLQuery.Add( ' (' + astl_Champs [ li_i ] + '=''' + avar_Variant [ li_i ] + '''' )
          Else
            astl_SQLQuery.Add ( ' AND ' + astl_Champs [ li_i ] + ' IS NULL' );

        if ((adat_Dataset.FindField ( astl_Champs [ li_i ] ) is TStringField )
        or (adat_Dataset.FindField ( astl_Champs [ li_i ] ) is TMemoField )
        or ( adat_Dataset.FindField ( astl_Champs [ li_i ] ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then
          Begin
            if li_i = 0 Then
              astl_SQLQuery.Add( ' (' + astl_Champs [ li_i ] + '=''' + avar_Variant [ li_i ] + '''' )
            Else
              astl_SQLQuery.Add ( ' AND ' + astl_Champs [ li_i ] + '=''' + avar_Variant [ li_i ] + '''' )
          End
        Else
          Begin
            if li_i = 0 Then
              astl_SQLQuery.Add( ' (' + astl_Champs [ li_i ] + '=' + avar_Variant [ li_i ] )
            Else
              astl_SQLQuery.Add ( ' AND ' + astl_Champs [ li_i ] + '=' + avar_Variant [ li_i ] )
          End ;
        End ;
    If ab_AddColma Then
      astl_SQLQuery.Add( ')' );
    Result := True ;
  Except
    On E: Exception do
      Begin
        f_GereException ( E, adat_Dataset );
      End ;
  End ;
End ;

procedure p_UpdateBatch ( const adat_Dataset: Tdataset);
Begin
  {$IFDEF EADO}
  if ( adat_Dataset is TCustomADODataset ) Then
    Begin
     ( adat_Dataset as TCustomADODataset ).UpdateBatch(arAll);
    End
   else
  {$ENDIF}
  {$IFDEF FPC}
    adat_Dataset.Refresh;
  {$ENDIF}
End ;

function fs_AjouteRechercheClePrimaire ( const adat_Dataset         : TDataset    ;
                                         const as_ChampsClePrimaire : TStringList ;
                                         const avar_ValeursCle      : Variant     ;
                                         const as_ChampExclu        : String      ): String ;

var
    ls_EnrChamp  : String ;
    lvar_Valeur  : Variant ;
    li_i         : Integer ;
    lb_First     : Boolean ;
    ls_SQL       : WideString;
Begin
  Result := '' ;
  lb_First     := True ;
  for li_i := 0 to as_ChampsClePrimaire.Count - 1 do
    if as_ChampExclu <> as_ChampsClePrimaire [ li_i ] Then
      Begin
        if  ( adat_Dataset.State <> dsInsert ) Then
          Begin
            if VarIsArray ( avar_ValeursCle ) Then
              Begin
                if  ( li_i >= varArrayLowBound  ( avar_ValeursCle, 1 ))
                and ( li_i <= varArrayHighBound ( avar_ValeursCle, 1 ))
                and ( avar_ValeursCle [ li_i ] <> Null ) Then
                  lvar_Valeur := avar_ValeursCle [ li_i ]
                Else
                  lvar_Valeur := Null ;
              End
            Else lvar_Valeur := avar_ValeursCle ;
            if  ( lvar_Valeur <> Null )
            and ( adat_Dataset.State = dsEdit )
            and ( Trim ( adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ).AsString ) <> Trim ( VarToStr( lvar_Valeur ))) Then
              Begin
                lvar_Valeur := adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ).Value ;
              End ;
          End
        Else
          Begin
            lvar_Valeur := adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ).Value ;
          End ;
        if lvar_Valeur = Null Then
          ls_EnrChamp := 'NULL'
        Else
          if VarIsFloat ( lvar_Valeur ) Then
            ls_EnrChamp := fs_RemplaceChar ( FloatToStr( lvar_Valeur ), ',', '.' )
        Else
          ls_EnrChamp := fs_RemplaceChar ( VarToStr( lvar_Valeur ), ',', '.' ) ;
        if lb_First then
          ls_SQL := ' WHERE '
         Else
          ls_SQL := ' AND ' ;
        if  ( adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ) <> nil              )
        and ( adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ).DataType = ftString )
        and ( adat_Dataset.FindField ( as_ChampsClePrimaire [ li_i ] ).Value <> Null       )
         Then ls_SQL := ls_SQL + as_ChampsClePrimaire [ li_i ] + '=''' + fs_stringDbQuote ( ls_EnrChamp ) + ''''
         else ls_SQL := ls_SQL + as_ChampsClePrimaire [ li_i ] + '='   +                    ls_EnrChamp ;
       Result := ls_SQL ;
       lb_First     := False ;
      End ;
End ;




// Teste si on change d'enregistrement
// Compare Un enregistrement ( variant ) et l'affecte à nouveau avec l'enregistrement de la clé en renvoyant true
// avar_EnregistrementCle : un vairant à mette dans la form propriétaire et à ne toucher qu'avec cette fonction
// adat_Dataset           : Le dataset de la clé
// as_Cle                 : LA clé
function fb_ChangeEnregistrement(var avar_EnregistrementCle : Variant ; const adat_Dataset : TDataset ;
  const as_Cle: String; const ab_Sort:Boolean): Boolean;
var lvar_NouveauEnregistrementCle : Variant ;
    li_i : Integer ;
begin
  // On n'a pas changé d'enregistrement par défaut
  Result := False ;
  if ab_Sort Then
    Exit ;
  if      adat_Dataset.Active
  and not adat_Dataset.IsEmpty
  and ( as_Cle <> '' ) Then
    Begin
      try
        lvar_NouveauEnregistrementCle := adat_Dataset.FieldValues [ as_Cle ];
    // Compare Un enregistrement ( variant )
    // Tableau de valeurs
      if  VarIsArray ( lvar_NouveauEnregistrementCle ) Then
        Begin
          if not VarIsArray ( avar_EnregistrementCle        ) Then
            Begin
              avar_EnregistrementCle := lvar_NouveauEnregistrementCle ;
              Result := True ;
            End
          Else
            for li_i := VarArrayLowBound ( lvar_NouveauEnregistrementCle, 1 ) to VarArrayHighBound ( lvar_NouveauEnregistrementCle, 1 ) do
              if  ( li_i >= VarArrayLowBound  ( avar_EnregistrementCle, 1 ))
              and ( li_i <= VarArrayHighBound ( avar_EnregistrementCle, 1 )) Then
                if (( avar_EnregistrementCle        [ li_i ] <> Null ) and ( lvar_NouveauEnregistrementCle [ li_i ] =  Null ))
                or (( avar_EnregistrementCle        [ li_i ] =  Null ) and ( lvar_NouveauEnregistrementCle [ li_i ] <> Null ))
                or  (( avar_EnregistrementCle       [ li_i ] <> Null )
                and ( lvar_NouveauEnregistrementCle [ li_i ] <> Null )
                and ( VarCompareValue ( avar_EnregistrementCle [ li_i ], lvar_NouveauEnregistrementCle [ li_i ] ) <> vrEqual )) Then
                  Begin
                    avar_EnregistrementCle        [ li_i ] := lvar_NouveauEnregistrementCle [ li_i ];
                    // Une valeur est différente alors on a changé d'enregistrement
                    Result := True ;
                  End
                Else
                  avar_EnregistrementCle        [ li_i ] := lvar_NouveauEnregistrementCle [ li_i ];

        End
      Else
        if ( as_Cle <> '' ) Then
        // Pas de tableau
          if  ( avar_EnregistrementCle        <> Null )
          and ( lvar_NouveauEnregistrementCle <> Null ) Then
            Begin
              if ( VarCompareValue ( avar_EnregistrementCle, lvar_NouveauEnregistrementCle ) <> vrEqual ) Then
                Begin
                  // et l'affecte à nouveau avec l'enregistrement de la clé en renvoyant true
                  avar_EnregistrementCle := lvar_NouveauEnregistrementCle ;
                  // La valeur est différente alors on a changé d'enregistrement
                  Result := True ;
                End ;
            End
          Else
            if  ( lvar_NouveauEnregistrementCle <> Null ) Then
              Begin
                // et l'affecte à nouveau avec l'enregistrement de la clé en renvoyant true
                avar_EnregistrementCle := lvar_NouveauEnregistrementCle ;
                // La valeur est différente alors on a changé d'enregistrement
                Result := True ;
              End

      except
        On E: Exception do
          Begin
            f_GereExceptionEvent ( E, adat_Dataset, ge_NilEvent, not gb_DBMessageOnError );
          End ;
      End ;
    End
   Else
     avar_EnregistrementCle := Null ;
end;

// Mise à jour de la propriété sort
// aDat_Dataset : Le dataset
// as_NomChamp : Le champ à trier
// ab_Desc  : Descendant ou Montant
function  fb_SortADataset ( const aDat_Dataset : TDataset; const as_NomChamp : String ; const ab_Desc : Boolean ) : Boolean;
{$IFDEF EADO}
var ls_AscDesc: String;
{$ENDIF}
Begin
  Result := False ;
  try
{$IFDEF EADO}
    if  ( aDat_DataSet is TCustomADODataset )
    and (( aDat_DataSet as TCustomADODataset ).CursorLocation = clUseClient )
    and assigned ( aDat_DataSet.FindField ( as_NomChamp )) then
      Begin
        if ab_Desc Then
          ls_AscDesc := CST_SQL_DESC
         Else
          ls_AscDesc := CST_SQL_ASC;

        TCustomADODataset( aDat_DataSet ).Sort   := as_NomChamp + ls_AscDesc ;
        Result := True ;
      End
     Else
{$ENDIF}
    if assigned ( GetPropInfo ( aDat_DataSet, 'SortedFields' )) Then
      Begin
        Result := True;
        p_SetComponentProperty ( aDat_DataSet, 'SortedFields', as_NomChamp);
        if ab_Desc Then
          p_SetComponentProperty ( aDat_DataSet, 'SortType', 1)
         Else
          p_SetComponentProperty ( aDat_DataSet, 'SortType', 0);
      End;
  Except
  End ;
End ;


// récupère la propriété sort
// aDat_Dataset : Le dataset
// Sortie : La propriété sort
{function TF_CustomFrameWork.fs_GetSort ( const aDat_Dataset : TDataset ) : String ;
Begin
  Result := '' ;
  if ( aDat_DataSet is TCustomADODataset ) then
    Result := TCustomADODataset( aDat_DataSet ).Sort
End ;}



initialization
  p_ConcatVersion ( gVer_fonctions_db );
finalization
end.
