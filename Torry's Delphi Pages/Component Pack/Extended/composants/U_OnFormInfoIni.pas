{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TOnFormInfoIni :                                       }
{             Objet de sauvegarde d'informations de Forms             }
{             20 Février 2003                                         }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_OnFormInfoIni;


{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


interface
// Listes des informations sauvegardées dans le fichier ini de l'application :
// Les données objets Edit
// La position des Objets (avec l'utilisation des Panels et des RxSplitters et RbSplitter)
// L'index de la pageactive des PageControls (onglets)
// L'index des objets CheckBoxex, RadioBoutons, RadioGroups ,PopupMenus
// les positions de la fenêtre

{$I ..\Compilers.inc}
{$I ..\extends.inc}

uses
{$IFDEF FPC}
  LCLIntf, MaskEdit, lresources,
{$ELSE}
  Windows, Mask, Consts, ShellAPI, JvToolEdit, U_ExtPageControl,
{$ENDIF}
{$IFDEF RX}
  RxLookup,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ComCtrls, ExtCtrls,
  RTLConsts, Variants, Menus, Buttons,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  ExtDlgs,
  fonctions_init, DBGrids;

{$IFDEF VERSIONS}
  const
    gVer_TSvgFormInfoIni : T_Version = ( Component : 'Composant TOnFormInfoIni' ;
                                               FileUnit : 'U_OnFormInfoIni' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Gestion de l''ini à mettre sur une fiche.' ;
                                               BugsStory : '1.0.0.0 : Gestion de beaucoup de composants.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );

{$ENDIF}
type
  // Liste des objets dont on veut conserver les donner dans le fichier INI
  TSauveEditObjet = (feTEdit, feTCheck, feTComboValue, feTComboBox, feTColorCombo,feTCurrencyEdit,feTDateEdit,
        {$IFDEF DELPHI}
        feTDateTimePicker,
        {$ENDIF}
  feTDirectoryEdit,feTFileNameEdit,feTGrid,feTListBox, feTListView, feTMemo, feTPageControl, feTPopup, feTRadio, feTRadioGroup, feTRichEdit,feTSpinEdit,
  feTVirtualTrees );
  TEventIni = procedure ( const AInifile : TCustomInifile ; var Continue : Boolean ) of object;
  TSauveEditObjets = set of TSauveEditObjet;

  { TOnFormInfoIni }

  TOnFormInfoIni = class(TComponent)
  private
    FSauveEditObjets: TSauveEditObjets;
    FSauvePosObjet,
    FAutoUpdate,
    FSauvePosForm:      Boolean;
    FOnIniLoad, FOnIniWrite : TEventIni;

  protected
    FUpdateAll ,
    FAutoChargeIni: Boolean;
    FormAOwner:     TCustomForm;
    FormOldDestroy: TNotifyEvent;
    FormOldCreate:  TNotifyEvent;
    FormOldShow:    TNotifyEvent;
//    procedure loaded; override;
    function ExisteComposantSvgEditSurLaFiche(Fiche:TCustomForm) :Integer;
    function GetfeSauveEdit(aSauveObjet:TSauveEditObjets;aObjet :TSauveEditObjet):Boolean ;
    // traitement de la position de la af_Form mise dans le create
    procedure p_LecturePositionFenetre(aFiche:TCustomForm);
    procedure p_EcriturePositionFenetre(aFiche:TCustomForm);

  public
    Constructor Create(AOwner:TComponent); override;
    procedure ExecuteLecture(aLocal:Boolean);
    procedure p_ExecuteLecture(const aF_Form: TCustomForm);
    procedure ExecuteEcriture(aLocal: Boolean);
    procedure p_ExecuteEcriture(const aF_Form: TCustomForm);
    procedure p_LectureColonnes(const aF_Form: TCustomForm);

    property AutoUpdate : Boolean read FAutoUpdate write FAutoUpdate;
  published
    // Propriété qui conserve la position des objets d'une form
    property SauvePosObjects: Boolean read FSauvePosObjet write FSauvePosObjet default False;
    // Propriété qui conserve les données des objets d'une form
    property SauveEditObjets: TSauveEditObjets read FSauveEditObjets write FSauveEditObjets nodefault;
    // Propriété qui conserve la position(index) des objets PageControl (onglets)
    property SauvePosForm: Boolean read FSauvePosForm  write FSauvePosForm default False;
    property OnIniLoad  : TEventIni read FOnIniLoad write FOnIniLoad ;
    property OnIniWrite : TEventIni read FOnIniWrite write FOnIniWrite;
    procedure LaFormDestroy(Sender: TObject);
    procedure LaFormShow(Sender: TObject);
    procedure LaFormCreate(Sender: TObject);
  end;

implementation

uses TypInfo, Grids, U_ExtNumEdits,
{$IFDEF FPC}
     richview, EditBtn,
{$ELSE}
{$IFDEF RX}
     rxToolEdit,
{$ENDIF}
{$ENDIF}
{$IFDEF VIRTUALTREES}
     VirtualTrees ,
{$ENDIF}
     JvXPCheckCtrls, DB, unite_messages, fonctions_proprietes;

////////////////////////////////////////////////////////////////////////////////
// retourne le nom de la machine
////////////////////////////////////////////////////////////////////////////////
function FW_Read_Computer_Name : string;
begin
  Result := f_IniFWReadComputerName;
end;

////////////////////////////////////////////////////////////////////////////////
// Retourne le fichier INI
////////////////////////////////////////////////////////////////////////////////
function GetFileIni: string;
begin
  Result := ExtractFilePath(Application.ExeName) + CST_Avant_Fichier + FW_Read_Computer_Name + '.ini';
end;

////////////////////////////////////////////////////////////////////////////////
// permet de sauver dans un ini le contenu d'un mémo, d'un Combobox, d'un ListBox, d'un RichEdit
// et d'un façon générale, le contenu des composants qui le stocke dans des TStrings
////////////////////////////////////////////////////////////////////////////////
procedure SauveTStringsDansIni(FIni:TMemIniFile; SectionIni:string; LeTStrings:TStrings; ItemIndex:integer);
var i: integer;
begin
  Fini.EraseSection(SectionIni); // on efface toute la section décrite par SectionIni
  for i := 0 to LeTStrings.Count - 1 do // pour chaque ligne du Tstrings
  begin
    // on aura ainsi dans le fichier ini et dans la section considéré :
    // L0= suivi du contenu de la première ligne du TStrings. puis L1= etc..
    FIni.WriteString(SectionIni, 'L' + IntToStr(i), LeTStrings[i]);// écrit dans le fichier ini
  end;
  FIni.WriteInteger(SectionIni, 'ItemIndex', ItemIndex);
end;


////////////////////////////////////////////////////////////////////////////////
// permet de lire le contenu d'un ini qui a été sauvé par SauveTStringsDansIni
////////////////////////////////////////////////////////////////////////////////
procedure LitTstringsDeIni(FIni: TMemIniFile; SectionIni: string; LeTStrings: TStrings; var ItemIndex: integer);
var i: integer;
begin
  ItemIndex := -1;
  if FIni.SectionExists(SectionIni) then
    begin
      LeTStrings.Clear;
      i := 0;
      while FIni.ValueExists(SectionIni, 'L' + IntToStr(i)) do
        begin
          LeTStrings.Add(FIni.ReadString(SectionIni, 'L' + IntToStr(i), ''));
          inc(i);
        end;
      ItemIndex := Fini.ReadInteger(SectionIni, 'ItemIndex', 0);
    end;
end;


////////////////////////////////////////////////////////////////////////////////
// Constructeur de l'objet TOnFormInfoIni
////////////////////////////////////////////////////////////////////////////////
Constructor TOnFormInfoIni.Create(AOwner:TComponent);
var lmet_MethodToAdd  : TMethod;
begin
  FAutoChargeIni := True;
  Inherited Create(AOwner);
  FAutoUpdate    := False;
  FSauvePosObjet := False;
  FSauvePosForm  := False;
  FOnIniLoad     := nil;
  FOnIniWrite    := nil;
  if not (csDesigning in ComponentState)  //si on est pas en mode conception
  and ( AOwner is TCustomForm ) then
    begin
      lmet_MethodToAdd.Data := Self;
      lmet_MethodToAdd.Code := MethodAddress('LaFormDestroy' );
      FormAOwner           := TCustomForm(AOwner);        // La forme propriétaire de notre composant
      FormOldDestroy       := TNotifyEvent ( fmet_getComponentMethodProperty ( FormAOwner, 'OnDestroy' )); // Sauvegarde de l'événement OnDestroy
      p_SetComponentMethodProperty ( FormAOwner, 'OnDestroy', lmet_MethodToAdd );        // Idem pour OnDestroy
      FormOldCreate        := TNotifyEvent ( fmet_getComponentMethodProperty ( FormAOwner, 'OnCreate' ));  // Sauvegarde de l'événement OnClose
      lmet_MethodToAdd.Code := MethodAddress('LaFormCreate' );
      p_SetComponentMethodProperty ( FormAOwner, 'OnCreate', lmet_MethodToAdd );         // Idem pour OnClose
      FormOldShow          := TNotifyEvent ( fmet_getComponentMethodProperty ( FormAOwner, 'OnShow' ));  // Sauvegarde de l'événement OnShow
      lmet_MethodToAdd.Code := MethodAddress('LaFormShow' );
      p_SetComponentMethodProperty ( FormAOwner, 'OnShow', lmet_MethodToAdd );     // Idem pour OnShow
    End;
end;


////////////////////////////////////////////////////////////////////////////////
// Au chargement de l'objet TOnFormInfoIni, on lit les données dans le fichier ini
////////////////////////////////////////////////////////////////////////////////
{procedure TOnFormInfoIni.loaded;
begin
  inherited;
  if not Assigned(FormAOwner) then
    Exit;
end;
}
////////////////////////////////////////////////////////////////////////////////
// À la fermeture de la form, on écrit les données dans le fichier ini
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.LaFormDestroy ( Sender: TObject );
begin
  if Assigned(FormAOwner)
   then
    p_ExecuteEcriture(FormAOwner);
  if Assigned(FormOldDestroy) then FormOldDestroy(Sender);
end;

////////////////////////////////////////////////////////////////////////////////
// À la fermeture de la form, on écrit les données dans le fichier ini
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.LaFormCreate ( Sender: TObject );
var
  FIni:TMemIniFile;
  Indice :integer;
  SvgEditDeLaFiche:TOnFormInfoIni;

begin
  FUpdateAll := False ;
  FAutoUpdate := True ;
  if Assigned(FormOldCreate) then FormOldCreate(Sender);
  FIni := f_GetMemIniFile;
  if Assigned(FIni) then
    try
      Self.Updating ;
      Indice := ExisteComposantSvgEditSurLaFiche ( FormAOwner );
      if Indice <> -1 then
        begin
          SvgEditDeLaFiche:= TOnFormInfoIni(FormAOwner.Components[Indice]);

          // Traitement de la position de la af_Form
          if (TFormStyle ( flin_getComponentProperty ( FormAOwner, 'FormStyle' )) <> fsMDIChild) and (SvgEditDeLaFiche.FSauvePosForm) then
            p_LecturePositionFenetre(FormAOwner);
        end;

    finally
      Self.Updated;
    end;
end;

procedure TOnFormInfoIni.LaFormShow(Sender: TObject);

begin
  try
    if Assigned(FormOldShow)
     then FormOldShow(Sender);
  Except

  end;
  if FAutoChargeIni then
    Begin
      p_ExecuteLecture(TForm(Self.Owner));
    end;
end;


////////////////////////////////////////////////////////////////////////////////
// Fonction qui vérifie si l'objet TOnFormInfoIni existe dans la form et retourne son index
////////////////////////////////////////////////////////////////////////////////
function TOnFormInfoIni.ExisteComposantSvgEditSurLaFiche(Fiche:TCustomForm) :Integer;
var j: integer;
begin
  Result:=-1;
  for j := 0 to Fiche.ComponentCount - 1 do //pour chaque composant de la fiche
    if Fiche.Components[j] is TOnFormInfoIni then Result := j;
end;

////////////////////////////////////////////////////////////////////////////////
// Fonction qui regarde dans la propriété TSauveEditObjets de TOnFormInfoIni
// et renvoie la valeur de sauvegarde d'un objet de la form
////////////////////////////////////////////////////////////////////////////////
function TOnFormInfoIni.GetfeSauveEdit(aSauveObjet:TSauveEditObjets;aObjet :TSauveEditObjet):Boolean;
begin
  Result := False;
  if aObjet in aSauveObjet then
    Result := True;
end;

////////////////////////////////////////////////////////////////////////////////
// Lecture des données dans le fichier ini
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.ExecuteLecture ( aLocal:Boolean);
var i: integer;
begin
  if not Assigned(FormAOwner) then Exit;
  for i := 0 to Application.ComponentCount - 1 do //pour chaque fiche de l'application
    begin
      if Application.Components[i] is TForm
         and ((FormAOwner.Name = (TForm(Application.Components[i])).Name)
              and aLocal) or (not aLocal) then
        p_ExecuteLecture(TForm(Application.Components[i]));
    end; //fin pour chaque fiche de l'application
end;

////////////////////////////////////////////////////////////////////////////////
// Lecture des données dans le fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.p_ExecuteLecture(const aF_Form: TCustomForm);
var
  FIni: TMemIniFile;
  mit: TMenuItem;
  j, k, Indice, Rien, valItemIndex, li_Taille : integer;
  SvgEditDeLaFiche: TOnFormInfoIni;
  ls_Temp : String ;
  ab_continue : Boolean;

begin
  FAutoChargeIni := False;
  Rien := 0;
  FIni := f_GetMemIniFile;
 {$IFDEF FPC}
    if FSauvePosObjet
    and ( Owner is TCustomForm ) Then
      Begin
        ( Owner as TCustomForm ).BeginUpdateBounds;
      End;
 {$ENDIF}
  ab_continue := True;

  if Assigned(FIni) then
    try
      Self.Updating;
      Indice := ExisteComposantSvgEditSurLaFiche(af_Form);
      if Indice <> -1 then
        begin
          SvgEditDeLaFiche:= TOnFormInfoIni(af_Form.Components[Indice]);

          If Assigned ( FOnIniLoad ) Then
            FOnIniLoad ( FIni, ab_continue );

          // traitement des composants de la af_Form
          if ab_continue Then
            for j := 0 to af_Form.ComponentCount - 1 do
              begin
                try
                  if (aF_Form.Components[j] is TDBGrid) and
                     GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTGrid) then
                    begin
                      f_IniReadGridFromIni ( FIni, aF_Form.Name, aF_Form.Components[j] as TDBGrid );
                    end;

                  if (aF_Form.Components[j].ClassNameIs ( 'TBaseVirtualTree' ))
                   and   GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTVirtualTrees) then
                      begin
                        {$IFDEF DELPHI}
                        f_IniReadVirtualTreeFromIni ( FIni, aF_Form.Name, aF_Form.Components[j] as TBaseVirtualTree );
                        {$ENDIF}
                    end;

                  if (aF_Form.Components[j] is TListView)
                   and   GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTListView) then
                      begin
                        f_IniReadListViewFromIni ( FIni, aF_Form.Name, aF_Form.Components[j] as TListView );
  {                      for k := 0 to lsv_ListView.Columns.Count - 1 do
                          if FIni.ReadInteger(aF_Form.Name, aF_Form.Components[j].Name + '.' + lsv_ListView.Columns[k].Caption, lsv_ListView.Columns[k].Width) > 0 Then
                            lsv_ListView.Columns[k].Width := FIni.ReadInteger(aF_Form.Name, aF_Form.Components[j].Name + '.' + lsv_ListView.Columns[k].Caption, lsv_ListView.Columns[k].Width);}
                    end;

                  // lecture de la position des objets Panels et Rxsplitters
                  if (   (af_Form.Components[j] Is TPanel )
                      or (af_Form.Components[j] is TCustomListView )
                      {$IFDEF VIRTUALTREES}
                      or (af_Form.Components[j] is TBaseVirtualTree  )
                      {$ENDIF}
                      or (af_Form.Components[j] is TCustomGrid ))
                  and SvgEditDeLaFiche.FSauvePosObjet then
                    begin
                      li_Taille := FIni.ReadInteger (af_Form.Name, af_Form.Components[j].Name +'.Width', TControl (af_Form.Components[j]).Width);
                      if li_Taille > 0 Then
                        TControl(af_Form.Components[j]).Width := li_Taille ;
                      li_Taille := FIni.ReadInteger (af_Form.Name, af_Form.Components[j].Name +'.Height',TControl (af_Form.Components[j]).Height);
                      if li_Taille > 0 Then
                        TControl(af_Form.Components[j]).Height:= li_Taille ;
                      Continue;
                    end;

                  // lecture de la page de contrôle(onglets)
                  if ((af_Form.Components[j] is TPageControl)) and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTPageControl )   then
                    begin
                      TPageControl(af_Form.Components[j]).ActivePageIndex := FIni.ReadInteger (af_Form.Name, af_Form.Components[j].Name , 0);
                      Continue;
                    end;
                  // lecture des CheckBoxes
                  if (af_Form.Components[j] is TCheckBox) and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTCheck ) then
                    begin
                      TCheckBox(af_Form.Components[j]).Checked:= FIni.ReadBool(af_Form.name,af_Form.Components[j].Name,true);
                      Continue;
                    end;
                  if (af_Form.Components[j] is TJvXPCheckBox) and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTCheck ) then
                    begin
                      TJvXPCheckBox(af_Form.Components[j]).Checked:= FIni.ReadBool(af_Form.name,af_Form.Components[j].Name,true);
                      Continue;
                    end;
                  // lecture des RadioBoutons
                  if (af_Form.Components[j] is TRadioButton) and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTRadio ) then
                    begin
                      TRadioButton(af_Form.Components[j]).Checked:= FIni.ReadBool(af_Form.name,af_Form.Components[j].Name,true);
                      Continue;
                    end;
                  // lecture des groupes de RadioBoutons
                  if (af_Form.Components[j] is TRadioGroup)  and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTRadioGroup ) then
                    begin
                      TRadioGroup(af_Form.Components[j]).ItemIndex:= FIni.ReadInteger(af_Form.name,af_Form.Components[j].Name,0);
                      Continue;
                    end;
                  // lecture de PopupMenu
                  if (af_Form.Components[j] is TPopupMenu) and GetfeSauveEdit ( SvgEditDeLaFiche.FSauveEditObjets, feTPopup )  then
                    begin
                      mit := TMenu(af_Form.Components[j]).Items;
                      for k := 0 to mit.Count-1 do
                        begin
                          if mit.Items[k].RadioItem then
                            mit.Items[k].Checked := FIni.ReadBool (af_Form.Name, af_Form.Components[j].Name+'_'+mit.Items[k].Name,true)
                          else
                            mit.Items[k].Checked := FIni.ReadBool (af_Form.Name, af_Form.Components[j].Name+'_'+mit.Items[k].Name,False);
                        end;
                    end;

                  if (af_Form.Components[j] is TEdit) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTedit) then
                    begin
                      ls_Temp := FIni.ReadString(af_Form.name,af_Form.Components[j].Name,'');
                      if ( ls_Temp <> '' ) Then
                        TCustomEdit(af_Form.Components[j]).Text := ls_Temp ;
                      Continue;
                    end;

                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDateEdit) Then
                    if (af_Form.Components[j] is {$IFDEF FPC} TDateEdit {$ELSE} TJvCustomDateEdit {$ENDIF}) then
                      Begin
                        {$IFDEF FPC} TDateEdit {$ELSE} TJvCustomDateEdit {$ENDIF}(af_Form.Components[j]).Date := StrToDateTime(FIni.ReadString (af_Form.name,af_Form.Components[j].Name,DateToStr(Date)));
                        Continue;
                      End;

                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTFileNameEdit) Then
                    Begin
                      {$IFDEF DELPHI}
                    if (af_Form.Components[j] is  TJvFileNameEdit) then
                      begin
                        TJvFileNameEdit (af_Form.Components[j]).Text := FIni.ReadString(af_Form.name, af_Form.Components[j].Name, GetCurrentDir);
                        Continue;
                      end;
                       {$ENDIF}
                   {$IFDEF RX}
                    if (af_Form.Components[j] is TFileNameEdit ) then
                      begin
                        TFileNameEdit (af_Form.Components[j]).Text := FIni.ReadString(af_Form.name, af_Form.Components[j].Name, GetCurrentDir);
                        Continue;
                      end;
                      {$ENDIF}
                    End ;

                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDirectoryEdit) Then
                    Begin
                      if (af_Form.Components[j].ClassNameIs('TJvDirectoryEdit')) then
                        begin
                          p_SetComponentProperty (af_Form.Components[j], 'Text', FIni.ReadString (af_Form.name,af_Form.Components[j].Name, GetCurrentDir ));
                          Continue;
                        end;
                      if (af_Form.Components[j].ClassNameIs('TDirectoryEdit')) then
                        begin
                          p_SetComponentProperty (af_Form.Components[j], {$IFDEF FPC} 'Text' {$ELSE} 'EditText' {$ENDIF}, FIni.ReadString (af_Form.name,af_Form.Components[j].Name, GetCurrentDir));
                          Continue;
                        end;
                    End;


                  {$IFDEF DELPHI}
                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDateTimePicker) Then
                    if (af_Form.Components[j] is TDateTimePicker) then
                      begin
                        if FIni.ReadString(af_Form.name,af_Form.Components[j].Name,'%ù@à*£')<>'%ù@à*£' then TDateTimePicker(af_Form.Components[j]).DateTime:=StrToDateTime( FIni.ReadString(af_Form.name,af_Form.Components[j].Name,''));
                        Continue;
                      end;
                  {$ENDIF}
                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTSpinEdit) Then
                    if (af_Form.Components[j].ClassNameIs( 'TSpinEdit'))
                    or (af_Form.Components[j].ClassNameIs( 'TRxSpinEdit'))
                     then
                      begin
                        p_SetComponentProperty(af_Form.Components[j], 'Value', FIni.ReadInteger(af_Form.name,af_Form.Components[j].Name,flin_getComponentProperty(af_Form.Components[j], 'Value')));
                        Continue;
                      end;
                  if (af_Form.Components[j] is TMemo) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTMemo)        then
                    begin
                      LitTstringsDeIni(FIni, af_Form.name+af_Form.Components[j].Name,TCustomMemo(af_Form.Components[j]).Lines,rien );
                      Continue;
                    end;
                  if (af_Form.Components[j] is {$IFDEF FPC} TRichView {$ELSE} TCustomRichEdit {$ENDIF}) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTRichEdit)    then
                    begin
                      LitTstringsDeIni(FIni, af_Form.name+af_Form.Components[j].Name,{$IFDEF FPC} TRichView {$ELSE} TCustomRichEdit {$ENDIF}(af_Form.Components[j]).Lines,rien);
                      Continue;
                    end;
                  if (af_Form.Components[j] is TCustomComboBox) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboValue)
                  and not assigned ( fobj_getComponentObjectProperty(af_Form.Components[j],'Datasource'))
                   then
                    begin
                        p_SetComponentProperty(af_Form.Components[j], 'Text', FIni.ReadString (af_Form.name,af_Form.Components[j].Name+'.Text',''));
                    End;
                  if (af_Form.Components[j].CLassNameIs( 'TExtColorCombo')) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTColorCombo)
                   then
                    begin
                        p_SetComponentProperty(af_Form.Components[j], 'Value', FIni.ReadString (af_Form.name,af_Form.Components[j].Name+'.Value',''));
                    End;
  {$IFDEF RX}
                  if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboValue)
                  and (af_Form.Components[j] is {$IFDEF FPC}TRxCustomDBLookupCombo{$ELSE}TRxLookupControl{$ENDIF})
                  and not assigned ( fobj_getComponentObjectProperty(af_Form.Components[j],'Datasource'))
                   Then
                    begin
                      {$IFNDEF FPC}
                      if (af_Form.Components[j] is TRxDBLookupList ) Then
                        (af_Form.Components[j] as TRxDBLookupList).DisplayValue := FIni.ReadString (af_Form.name,af_Form.Components[j].Name + '.Value', '')
                       else
                       {$ENDIF}
                        if (af_Form.Components[j] is TRxDBLookupCombo ) Then
                        {$IFDEF FPC}
                          (af_Form.Components[j] as TRxDBLookupCombo).LookupDisplayIndex := FIni.ReadInteger (af_Form.name,af_Form.Components[j].Name + '.Index', -1);
                        {$ELSE}
                         (af_Form.Components[j] as TRxDBLookupCombo).DisplayValue := FIni.ReadString (af_Form.name,af_Form.Components[j].Name + '.Index', '');
                         {$ENDIF}
                    End;
  {$ENDIF}
                  if (af_Form.Components[j] is TCustomComboBox) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboBox)    then
                    begin
                      LitTstringsDeIni(FIni, af_Form.name+af_Form.Components[j].Name,TCustomComboBox(af_Form.Components[j]).Items,valItemIndex);
                      if valItemIndex<=TCustomComboBox(af_Form.Components[j]).Items.Count-1 then
                        TCustomComboBox(af_Form.Components[j]).ItemIndex:=valItemIndex;
                      Continue;
                    end;
                  if (af_Form.Components[j] is TListBox) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTListBox)     then
                    begin
                      LitTstringsDeIni(FIni, af_Form.name+af_Form.Components[j].Name,TCustomListBox(af_Form.Components[j]).Items,valItemIndex);
                      if valItemIndex<=TCustomListBox(af_Form.Components[j]).Items.Count-1 then TCustomListBox(af_Form.Components[j]).ItemIndex:=valItemIndex;
                      Continue;
                    end;
                  if  GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTCurrencyEdit) Then
                    if (af_Form.Components[j] is TExtNumEdit) then
                      begin
                        TExtNumEdit(af_Form.Components[j]).Text := FIni.ReadString (af_Form.name,af_Form.Components[j].Name,' ');
                        Continue;
                      end;
                except
                end;
              end; // Fin for composant de la af_Form
        end;

    finally

   {$IFDEF FPC}
      if ab_continue
      and FSauvePosObjet
      and ( Owner is TCustomForm ) Then
        Begin
          ( Owner as TCustomForm ).EndUpdateBounds;
        End;
   {$ENDIF}
      Self.Updated;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Ecriture des données dans le fichier ini
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.ExecuteEcriture(aLocal:Boolean);
var i : Integer ;
    ab_continue : Boolean ;
    FIni : TMemIniFile;
begin

  if not assigned ( FormAOwner )
   Then
    Exit ;
  FUpdateAll := True ;
  ab_continue := True;
  FIni := f_GetMemIniFile;
  If Assigned ( FOnIniWrite ) Then
    FOnIniWrite ( FIni, ab_continue );
  if ab_continue Then
    try
      For i:=0 to application.ComponentCount-1 do //pour chaque af_Form de l'application
      begin
        if ( application.Components[i] is TCustomForm )
        and ((FormAOwner.Name = ( TForm ( application.Components[i] )).Name) and aLocal) or (Not aLocal)
         Then
          p_ExecuteEcriture ( TCustomForm ( application.Components[i] ));
      end; //fin pour chaque af_Form de l'application
    finally
      FUpdateAll := False ;

      if FAutoUpdate Then
        Begin
          fb_iniWriteFile ( FIni, False );
          Application.ProcessMessages ;

        End ;
    End ;
end;

////////////////////////////////////////////////////////////////////////////////
// Ecriture des données dans le fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.p_ExecuteEcriture ( const af_Form : TCustomForm );
var
  FIni: TMemIniFile;
  mit: TMenuItem;
  j, k, Indice: integer;
//  lvt_EnteteArbre : TVTHeader ;
//  lgd_grid: TDBGrid;
//  lsv_ListView : TListView ;
  SvgEditDeLaFiche: TOnFormInfoIni;

begin
  FIni:= f_GetMemIniFile;
  if not Assigned(FIni) then Exit;

    Indice := ExisteComposantSvgEditSurLaFiche ( af_Form );
    if Indice <> -1 then
    begin
      SvgEditDeLaFiche:= TOnFormInfoIni(af_Form.Components[Indice]);
      // traitement de la position de la af_Form
      if (TFormStyle ( flin_getComponentProperty ( FormAOwner, 'FormStyle' )) <> fsMDIChild) and (SvgEditDeLaFiche.FSauvePosForm)  then
        p_EcriturePositionFenetre(af_Form);

      // Traitement des composants de la af_Form
      For j:=0 to af_Form.ComponentCount-1 do
      begin
        Try
          if (af_Form.Components[j] is TDBGrid) and
             GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTGrid) then
            begin
              p_IniWriteGridToIni ( FIni, af_Form.Name, af_Form.Components[j] as TDBGrid );
              Continue;
            end;

              if (aF_Form.Components[j].ClassNameIs ( 'TBaseVirtualTree' ))
               and   GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTVirtualTrees) then
                begin
                  {$IFDEF DELPHI}
                  p_IniWriteVirtualTreeToIni ( FIni, af_Form.Name, af_Form.Components[j] as TBaseVirtualTree );
                  {$ENDIF}
                  Continue;
                end;

              if (aF_Form.Components[j] is TListView)
               and   GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTListView) then
                begin
                  p_IniWriteListViewToIni ( FIni, af_Form.Name, af_Form.Components[j] as TListView );
                  Continue;
                end;

          // écriture des positions des objets Panels et RxSplitters
          if SvgEditDeLaFiche.FSauvePosObjet then
          begin
           if      (af_Form.Components[j] is TPanel)
                or (af_Form.Components[j].ClassNameIs ( 'TBaseVirtualTree' ))
                or (af_Form.Components[j] is TCustomGrid)
                or (af_Form.Components[j] is TCustomListView) Then
                 begin
                  if TControl(af_Form.Components[j]).Width  > 0 Then
                    FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name+'.Width',TControl(af_Form.Components[j]).Width);
                  if TControl(af_Form.Components[j]).Height > 0 Then
                    FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name+'.Height',TControl(af_Form.Components[j]).Height);
                  Continue;
                end;
          end;

          // Écriture de la position des colonnes des grilles
          // Ecriture de la page de contrôle(onglets)
          if (af_Form.Components[j] is TPageControl)     and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTPageControl )   then
            begin
              FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name,TPageControl(af_Form.Components[j]).ActivePageIndex );
              Continue;
            end;

          if (af_Form.Components[j] is TCheckBox)       and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTCheck )         then
          begin
            FIni.WriteBool(af_Form.name,af_Form.Components[j].Name,TCheckBox(af_Form.Components[j]).Checked);
            Continue;
            end;
          if (af_Form.Components[j] is TJvXpCheckBox)       and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTCheck )         then
          begin
            FIni.WriteBool(af_Form.name,af_Form.Components[j].Name,TJvXPCheckBox(af_Form.Components[j]).Checked);
            Continue;
            end;
          if (af_Form.Components[j] is TRadioButton)    and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTRadio )      then
          begin
            FIni.WriteBool(af_Form.name,af_Form.Components[j].Name,TRadioButton(af_Form.Components[j]).Checked);
            Continue;
            end;
          if (af_Form.Components[j] is TRadioGroup)     and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTRadioGroup )       then
          begin
            FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name,TRadioGroup(af_Form.Components[j]).ItemIndex);
            Continue;
            end;
          // Ecriture de PopupMenu
          if (af_Form.Components[j] is TPopupMenu )     and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets, feTPopup )  then
            begin
              mit := TMenu(af_Form.Components[j]).Items;
              for k := 0 to mit.Count-1 do
                begin
                  FIni.WriteBool (af_Form.Name, af_Form.Components[j].Name+'_'+mit.Items[k].Name , mit.Items[k].Checked);
                end;
              Continue;
            end;
          // écriture des données des objets dans le fichier ini.
          if SvgEditDeLaFiche.FSauveEditObjets <> [] Then
          begin
            if (af_Form.Components[j] is TEdit)           and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTEdit) then
            begin
              FIni.WriteString(af_Form.name,af_Form.Components[j].Name,TCustomEdit(af_Form.Components[j]).Text);
              Continue;
              end;
            if (af_Form.Components[j] is {$IFDEF FPC} TDateEdit {$ELSE} TJvCustomDateEdit {$ENDIF})       and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDateEdit) then
            begin
              FIni.WriteString(af_Form.name,af_Form.Components[j].Name,DateTimeToStr({$IFDEF FPC} TDateEdit {$ELSE} TJvCustomDateEdit {$ENDIF}(af_Form.Components[j]).Date));
              Continue;
              end;
              if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTFileNameEdit) Then
                Begin
                {$IFDEF DELPHI}
                  If (af_Form.Components[j] is TJvFileNameEdit) then
                    begin
                      FIni.WriteString(af_Form.name,af_Form.Components[j].Name,TJvFileNameEdit(af_Form.Components[j]).Text);
                      Continue;
                    end;
                 {$ENDIF}
                {$IFDEF RX}
                  If (af_Form.Components[j] is TFileNameEdit) then
                    begin
                      FIni.WriteString(af_Form.name,af_Form.Components[j].Name,TFileNameEdit(af_Form.Components[j]).Text);
                      Continue;
                    end;
                 {$ENDIF}
                End ;
              if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDirectoryEdit) then
                begin
                  if (af_Form.Components[j].ClassNameIs('TJvDirectoryEdit')) then
                    Begin
                      FIni.WriteString(af_Form.name,af_Form.Components[j].Name,fs_getComponentProperty(af_Form.Components[j], 'Text'));
                      Continue;
                    End;
                  if (af_Form.Components[j].ClassNameIs('TDirectoryEdit')) then
                    begin
                      FIni.WriteString(af_Form.name,af_Form.Components[j].Name,fs_getComponentProperty(af_Form.Components[j], {$IFDEF FPC} 'Text' {$ELSE} 'EditText' {$ENDIF}));
                      Continue;
                    end;
                end;
              if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTSpinEdit)
              and  (   (af_Form.Components[j].ClassNameIs( 'TSpinEdit'))
                    or (af_Form.Components[j].ClassNameIs( 'TRxSpinEdit')))
               then
                begin
                FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name,flin_getComponentProperty(af_Form.Components[j], 'Value' ));
                Continue;
                end;
              if (af_Form.Components[j] is TMemo)           and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTMemo)            then
                begin
                SauveTStringsDansIni(FIni, af_Form.name+af_Form.Components[j].Name,TMemo(af_Form.Components[j]).Lines,0);
                            Continue;
                end;
              if (af_Form.Components[j] is {$IFDEF FPC} TRichView {$ELSE} TCustomRichEdit {$ENDIF})       and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTRichEdit)        then
                begin
                          SauveTStringsDansIni(FIni, af_Form.name+af_Form.Components[j].Name,{$IFDEF FPC} TRichView {$ELSE} TCustomRichEdit {$ENDIF}(af_Form.Components[j]).Lines,0);
                Continue;
                end;
              if (af_Form.Components[j].CLassNameIs( 'TExtColorCombo')) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTColorCombo)
               then
                begin
                    FIni.WriteInteger(af_Form.name,af_Form.Components[j].Name+ '.Value', Flin_getComponentProperty (af_Form.Components[j], 'Value'));
                End;
              if (af_Form.Components[j] is TCustomComboBox) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboValue)
              and not assigned ( fobj_getComponentObjectProperty(af_Form.Components[j],'Datasource'))
               Then
                begin
                  FIni.WriteString(af_Form.name,af_Form.Components[j].Name+'.Text',fs_getComponentProperty(af_Form.Components[j], 'Text'));
                End;
            {$IFDEF RX}
              if GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboValue)
              and (af_Form.Components[j] is {$IFDEF FPC}TRxCustomDBLookupCombo{$ELSE}TRxLookupControl{$ENDIF})
              and not assigned ( fobj_getComponentObjectProperty(af_Form.Components[j],'Datasource'))
               then
                begin
                 {$IFNDEF FPC}
                  if (af_Form.Components[j] is TRxDBLookupList) Then
                    FIni.WriteString( af_Form.name,af_Form.Components[j].Name + '.Value', (af_Form.Components[j] as TRxDBLookupList).DisplayValue)
                   else
                  {$ENDIF}
                    if (af_Form.Components[j] is TRxDBLookupCombo) Then
                    {$IFDEF FPC}
                      FIni.WriteInteger( af_Form.name,af_Form.Components[j].Name + '.Index', (af_Form.Components[j] as TRxDBLookupCombo).LookupDisplayIndex);
                    {$ELSE}
                      FIni.WriteString( af_Form.name,af_Form.Components[j].Name + '.Value', (af_Form.Components[j] as TRxDBLookupCombo).DisplayValue);
                     {$ENDIF}
                End;
            {$ENDIF}
              if (af_Form.Components[j] is TCustomComboBox)       and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTComboBox)        then
                begin
                SauveTStringsDansIni(FIni, af_Form.name+af_Form.Components[j].Name,TCustomComboBox(af_Form.Components[j]).Items,TCustomComboBox(af_Form.Components[j]).ItemIndex);
                Continue;
                end;
              if (af_Form.Components[j] is TListBox)        and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTListBox)         then
                begin
                SauveTStringsDansIni(FIni, af_Form.name+af_Form.Components[j].Name,TCustomListBox(af_Form.Components[j]).Items,TListBox(af_Form.Components[j]).ItemIndex);
                            Continue;
                end;
              if (af_Form.Components[j] is TExtNumEdit)   and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTCurrencyEdit) then
                begin
                FIni.WriteString(af_Form.name,af_Form.Components[j].Name,TExtNumEdit(af_Form.Components[j]).Text);
                Continue;
                end;
            {$IFDEF DELPHI}
              if (af_Form.Components[j] is TDateTimePicker) and GetfeSauveEdit(SvgEditDeLaFiche.FSauveEditObjets ,feTDateTimePicker)  then
                begin
                FIni.WriteString(af_Form.name,af_Form.Components[j].Name,DateTimeToStr(TDateTimePicker(af_Form.Components[j]).DateTime));
                Continue;
                end;
            {$ENDIF}
            end;
          Except
          end;
        end;
      end;
    if not FUpdateAll
    and FAutoUpdate Then
      Begin
        fb_iniWriteFile ( Fini, False );
        Application.ProcessMessages ;
      End;

end;

////////////////////////////////////////////////////////////////////////////////
//  Lecture de la position des colonnes des grilles dans le fichier INI
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.p_LectureColonnes(const aF_Form: TCustomForm);

begin
end;


////////////////////////////////////////////////////////////////////////////////
// Lecture des données dans le fichier INI concernant la fenêtre uniquement
// traitement de la position de la af_Form mise dans le create
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.p_LecturePositionFenetre(aFiche: TCustomForm);
var li_etat, li_ScreenHeight, li_ScreenWidth: integer;
begin
  // Résolution de l'écran
  li_ScreenHeight := f_IniReadSectionInt (aFiche.Name,'Screen.Height',Screen.Height);
  li_ScreenWidth := f_IniReadSectionInt (aFiche.Name,'Screen.Width',Screen.Width);

  li_etat := f_IniReadSectionInt (aFiche.Name,aFiche.name+'.WindowState',0);
  // positionnement de la fenêtre
  p_SetComponentProperty ( aFiche, 'Position', poDesigned );
  if li_etat = 0 then
    aFiche.WindowState := wsNormal
  else
    if li_etat = 1 then
    begin
      p_SetComponentProperty ( aFiche, 'Position', poDefault );
      p_SetComponentProperty ( aFiche, 'WindowState', wsMaximized );
      end
    else
      p_SetComponentProperty ( aFiche, 'WindowState', wsMinimized );

  if li_etat <> 1 then
  begin
    aFiche.Width := f_IniReadSectionInt (aFiche.Name,aFiche.name+'.Width',aFiche.Width) ;
    aFiche.Height:= f_IniReadSectionInt (aFiche.Name,aFiche.name+'.Height',aFiche.Height);
    aFiche.Top   := f_IniReadSectionInt (aFiche.Name,aFiche.name+'.Top',aFiche.Top);
    aFiche.Left  := f_IniReadSectionInt (aFiche.Name,aFiche.name+'.Left',aFiche.Left);

    if Screen.Height <> li_ScreenHeight then
    begin
      aFiche.Width := Round(aFiche.Width * Screen.Width / li_ScreenWidth)  ;
      aFiche.Height:= Round(aFiche.Height * Screen.Height/li_ScreenHeight) ;
      aFiche.Top   := Round(aFiche.Top * Screen.Height/li_ScreenHeight) ;
      aFiche.Left  := Round(aFiche.Left * Screen.Width/li_ScreenWidth);
    end;

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Ecriture des données dans le fichier ini concernant la fenêtre
////////////////////////////////////////////////////////////////////////////////
procedure TOnFormInfoIni.p_EcriturePositionFenetre(aFiche: TCustomForm);
var li_etat: integer;
begin
  p_IniWriteSectionInt(aFiche.Name,'Screen.Height',Screen.Height);
  p_IniWriteSectionInt(aFiche.Name,'Screen.Width',Screen.Width);

  // Etat de la fenêtre
  if aFiche.WindowState = wsNormal then
    li_etat := 0
  else
    if aFiche.WindowState = wsMaximized then
      li_etat := 1
    else
      li_etat := 2;
  // sauvegarde de son état
  p_IniWriteSectionInt(aFiche.Name,aFiche.name+'.WindowState',li_etat);

  // sauvegarde de sa position si la fenêtre n'est pas au Maximun
  if li_etat <> 1 then
    begin
      p_IniWriteSectionInt (aFiche.Name,aFiche.name+'.Width',aFiche.Width);
      p_IniWriteSectionInt (aFiche.Name,aFiche.name+'.Height',aFiche.Height);
      p_IniWriteSectionInt (aFiche.Name,aFiche.name+'.Top',aFiche.Top);
      p_IniWriteSectionInt (aFiche.Name,aFiche.name+'.Left',aFiche.Left);
    end;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TSvgFormInfoIni );
{$ENDIF}
end.
