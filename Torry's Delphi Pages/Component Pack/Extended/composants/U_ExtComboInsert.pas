{*********************************************************************}
{                                                                     }
{                                                                     }
{             TExtDBComboInsert :                               }
{             Objet issu d'un TCustomComboBox qui associe les         }
{             avantages de la DBComoBox et de la DBLookUpComboBox     }
{             Créateur : Matthieu Giroux                          }
{             31 Mars 2005                                            }
{             Version 1.0                                             }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtComboInsert;

{$I ..\Compilers.inc}

interface

uses Variants, Windows, Messages, Controls, Classes,
     Graphics, Menus, Mask, ComCtrls, DB,DBCtrls,
     fonctions_version, JvDBLookup ;

{$IFDEF VERSIONS}
  const
    gVer_TDBLookupComboInsert : T_Version = ( Component : 'Composant TDBComboBoxInsert' ;
                                               FileUnit : 'U_DBComboBoxInsert' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Insertion automatique dans une DBComboLookupEdit.' ;
                                               BugsStory : '1.0.1.2 : Bug validation au post.' +#13#10
                                                         + '1.0.1.1 : Bug rafraîchissement quand pas de focus.' +#13#10
                                                         + '1.0.1.0 : Propriété Modify.' +#13#10
                                                         + '1.0.0.0 : Version bêta inadaptée, réutilisation du code de la TJvDBLookupComboEdit.' +#13#10
                                                         + '0.9.0.0 : En place à tester.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 1 ; Build : 2 );

{$ENDIF}
type

  TExtDBComboInsert = class;

  TPopupDataWindow = class(TJvPopupDataWindow)
  private
  protected
    FComboEditor: TExtDBComboInsert;
    FChanging : Boolean ;
    procedure ListLinkActiveChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TExtDBComboInsert }
  TExtDBComboInsert = class(TJvDBLookupEdit)
  private
    // On est en train d'écrire dans la combo
    FModify : Boolean ;
    // Valeur affichée
    FDisplayValue : Variant ;
    // Lien de données
    FDataLink: TFieldDataLink;
    // Canevas de peinture du composant
    FCanvas: TControlCanvas;
    // Focus sur le composant
    FFocused: Boolean;
    // En train de mettre à jour ou pas
    FUpdate ,
    // Beep sur erreur
    FBeepOnError: Boolean;
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetTextMargins: TPoint;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetDisplayValue: String;
  protected
    OldText : String ;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean); override;
    procedure ActiveChange(Sender: TObject); virtual ;
    procedure DataChange(Sender: TObject); virtual ;
    procedure EditingChange(Sender: TObject); virtual ;
//    procedure ListLinkActiveChanged; override ;
    procedure InsertLookup ( const Update : Boolean ); virtual ;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure DoEnter; override;
    procedure DoExit; override;

    function GetReadOnly: Boolean; override; // suppress the warning
    procedure SetReadOnly(Value: Boolean); override;
    procedure Change; override;
    procedure DoChange; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
  public
    property Modify : Boolean read FModify ;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    // function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
    property DisplayValue : Variant read FDisplayValue write FDisplayValue;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  procedure Register;

implementation

uses
  SysUtils, Forms, StdCtrls, fonctions_erreurs,
  JvConsts, JvToolEdit, unite_messages;

{ TPopupDataWindow }

////////////////////////////////////////////////////////////////////////////////
// Constructeur : create
// description : Création de la liste en popup
////////////////////////////////////////////////////////////////////////////////
constructor TPopupDataWindow.Create(AOwner: TComponent);
begin
  // Initialisation
  FChanging := False ;
  // combo parent
  FComboEditor := AOwner as TExtDBComboInsert ;
  inherited Create(AOwner);
end;

////////////////////////////////////////////////////////////////////////////////
// procedure : ListLinkActiveChanged
// description : Ouverture des données : affectation de datafield
////////////////////////////////////////////////////////////////////////////////
procedure TPopupDataWindow.ListLinkActiveChanged;
begin
  inherited;
  if  assigned ( LookupSource )
  and assigned ( LookupSource.DataSet )
  and LookupSource.DataSet.Active Then
    try
      FComboEditor.Text := FComboEditor.GetDisplayValue ;
    finally
    End ;
end;


{ TExtDBComboInsert }


////////////////////////////////////////////////////////////////////////////////
// procedure : Register
// description : Enregistrement du composant dans la palette des composants
////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS, [TExtDBComboInsert]);
end;


////////////////////////////////////////////////////////////////////////////////
// Constructeur : Create
// description  : Initialisation du composant
////////////////////////////////////////////////////////////////////////////////
constructor TExtDBComboInsert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Pas de modification ni de mise à jour à la création
  FModify := False ;
  FUpdate := False ;
  // style lookup
  ControlStyle := ControlStyle + [csReplicatable];
  // Création du canevas
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  // Création du lien
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  // Changement dans les données
  FDataLink.OnDataChange := DataChange;
  // Edition des données
  FDataLink.OnEditingChange := EditingChange;
  // Mise à jour des données
  FDataLink.OnUpdateData := UpdateData;
  // Activation des données
  FDataLink.OnActiveChange := ActiveChange;
  // Beep sur erreur par défaut
  FBeepOnError := True;
  // Mode création pour informer la popup
  ControlState := ControlState + [csCreating];
  // Création popup
  try
    // Une ancienne popup existe déjà
    FPopup.Free ;
    // Création du descendant de TPopupDataWindow
    FPopup := TPopupDataWindow.Create(Self);
    // evènement d'affectation de donnée
    TJvPopupDataWindow(FPopup).OnCloseUp := PopupCloseUp;
    // Glyph de combo par défaut
    GlyphKind := gkDropDown; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Destructeur : Destroy
// description : Destruction des objets créés au create
////////////////////////////////////////////////////////////////////////////////
destructor TExtDBComboInsert.Destroy;
begin
  // Lien de données
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  // canevas de peinture du composant
  FCanvas.Free;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : ResetMaxLength
// description : Vérifications avant affectation de la taille du texte à rien
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : Loaded
// description : Initialisations après le chargement des autres composants
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if csDesigning in ComponentState then
    DataChange(Self);
end;


////////////////////////////////////////////////////////////////////////////////
// procédure   : Notification
// description : enlève les liens vers les composants supprimés dans la fiche
// paramètres  : AComponent : Le composant testé
//               Operation  : supprimé ou ajouté
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : KeyDown
// description : évènement enfoncement de touche
// paramètres  : Key : La touche appuyée
//               Shift : Touche spéciale
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // new order, because result of inherited KeyDown(...) could be = 0
  // so, first set DataSet in Edit-Mode
  // certaines touches initient l'édition des données
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then // taken from TDBComboBox.KeyDown(...)
    FDataLink.Edit;
  // Auto-insertion dans ce composant
  if Key in [VK_RETURN] Then
    InsertLookup ( True );
  inherited KeyDown(Key, Shift);
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : KeyPress
// description : évènement appuie sur touche
// paramètre   : Key : La touche appuyée
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  // vérifications : Tout est-il renseigné correctement ?
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) Then
    Begin
      FModify := True ;
       if  ( (( LookupDisplay = '' ) and not FDataLink.Field.IsValidChar(Key))
       or  ( ( LookupDisplay <> '' ) and
       ( not assigned ( LookupSource ) or not assigned ( LookupSource.DataSet ) or not assigned ( LookupSource.DataSet.FindField ( LookupDisplay ) ) or not LookupSource.DataSet.FindField ( LookupDisplay ).IsValidChar(Key)))) then
      begin
        if BeepOnError then
          SysUtils.Beep;
        Key := #0;
      end;
    End ;
    // Mode édition sur certaines touches
  case Key of
    CtrlH, CtrlV, CtrlX, #32..#255:
      FDataLink.Edit;
    Esc:
    // annulation
      begin
        FDataLink.Reset;
        SelectAll;
//        Key := #0;
      end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : EditCanModify
// description : Edition puis retour du mode édition
// paramètres  : résultat : en mode édition ou pas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : Reset
// description : Remet les données originelles
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : SetFocused
// description : Attribue la variable focus
// paramètre   : Value : Focus ou pas focus
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    // if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : Change
// description : Evènement sur changement du datasourc principal
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.Change;
begin
  // On avertit le lien de données
  FDataLink.Modified;
  if assigned ( FDataLink.Dataset )
  and not ( FDataLink.Dataset.State in [dsInsert,dsEdit]) Then
    // Les données viennent peut-être d'être validées
    FModify := False ;
  inherited Change;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : DoChange
// description : Evènement sur changement du datasourc principal
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DoChange;
begin
  inherited DoChange;
end;


////////////////////////////////////////////////////////////////////////////////
// fonction    : GetDataSource
// description : Récupère le Datasource principal
// paramètre   : résultat : le Datasource principal
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : SetDataSource
// description : Attribue le Datasource principal
// paramètre   : Value : le futur Datasource principal
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetDataField
// description : Récupère le champ du Datasource principal
// paramètre   : résultat : le champ du Datasource principal
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : SetDataField
// description : Attribue le champ du Datasource principal
// paramètre   : Value : le futur champ du Datasource principal
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetReadOnly
// description : Récupère la lecture seule
// paramètre   : résultat : lecture seule ou pas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly or inherited GetReadOnly;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : SetReadOnly
// description : Attribue la lecture seule
// paramètre   : Value : lecture seule ou pas
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  FDataLink.ReadOnly := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetCanvas
// description : Récupère le canevas de peinture du composant
// paramètre   : résultat : le canevas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetField
// description : Récupère le champ principal
// paramètre   : résultat : le champ
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetField: TField;
begin
  Result := FDataLink.Field;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement   : ActiveChange
// description : Initialisation de la taille du texte
// paramètre   : Sender : pour l'évènement
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetDisplayValue
// description : Récupère la valeur affichée
// paramètre   : résultat : la valeur affichée
////////////////////////////////////////////////////////////////////////////////
function  TExtDBComboInsert.GetDisplayValue : String ;
Begin
  Result := '' ;
    // Tests
  If  assigned ( FDataLink.Field )
  and not FDataLink.Field.IsNull
  and assigned ( LookupSource )
  and assigned ( LookupSource.DataSet )
  and assigned ( LookupSource.DataSet.FindField ( LookupDisplay ))
  and assigned ( LookupSource.DataSet.FindField ( LookupField   )) Then
    Begin
      if ( FPopup as TJvPopupDataWindow ).Locate ( LookupSource.DataSet.FindField ( LookupField   ), FDataLink.Field.AsString, True ) Then
        // récupération à partir de la listes
        Result := LookupSource.DataSet.FindField ( LookupDisplay ).AsString ;
    End ;
End ;

////////////////////////////////////////////////////////////////////////////////
// Evènement   : DataChange
// description : Changement dans les données
// paramètre   : Sender : pour l'évènement
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    // récupération du masque de saisie
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        // Taille maxi
        MaxLength := FDataLink.Field.Size;
    if FFocused and FDataLink.CanModify then
      Begin
        // récupération des données de la liste en mode lecture
        if  ( not ( FDataLink.DataSet.State in [dsEdit, dsInsert]) or FUpdate) Then
          Begin
            Text := GetDisplayValue ;
          End ;
      End
    else
    begin
      // affectation du texte à partir de la liste quand on n'est pas sur la combo
      EditText := GetDisplayValue ;// FDataLink.Field.DisplayText
      // Vérification de l'édition du champ ailleurs
      if FDataLink.Editing then //and FDataLink.FModify || FModified is private in parent of fdatalink
        Modified := True;
    end;
  end
  else
  begin
    EditMask := '';
    if csDesigning in ComponentState then
      // Pas de donnée : on montre le nom de la combo
      EditText := Name
    else
      EditText := '' ;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement   : EditingChange
// description : Changement dans l'édition des données
// paramètre   : Sender : pour l'évènement
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.EditingChange(Sender: TObject);
begin
  //ReadOnly := not FDataLink.Editing;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement   : UpdateData
// description : Mise à jour après l'édition des données
// paramètre   : Sender : pour l'évènement
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.UpdateData(Sender: TObject);
begin
  // auto-insertion spécifique de ce composant
  InsertLookup ( False );
  // Validation de l'édition
  ValidateEdit;
  // affectation
  FDataLink.Field.Value := LookupValue;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement message  : WMUndo
// description : Annulation
// paramètre   : Msg : données du message
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.WMUndo(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement message  : WMPaste
// description : Coller
// paramètre   : Msg : données du message
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement message  : WMCut
// description : Couper
// paramètre   : Msg : données du message
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : DoEnter
// description : Attribue le focus au composant
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DoEnter;
begin
  // Affecte la propriété focused
  SetFocused(True);
  inherited DoEnter;
  // Sélectionne le texte
  SelectAll ;
  // pas de lecture seule ?
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

////////////////////////////////////////////////////////////////////////////////
// procédure   : InsertLookup
// description : Insertion automatique
// paramètre   : Update : validation du champ si pas en train de valider
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.InsertLookup ( const Update : Boolean );
begin
  if ( csDesigning in ComponentState ) Then
    Exit ;
    // vérifications
  if  assigned ( FDataLink.Dataset )
  and assigned ( LookupSource )
  and assigned ( LookupSource.DataSet )
  and assigned ( FDataLink.Field )
  and ( FDataLink.Dataset.State in [ dsEdit,dsInsert ]) Then
    try
      if ( Text <> '' ) Then
        // Si du texte est présent
        Begin
          if not LookupSource.DataSet.Locate ( LookupDisplay, Text, [loCaseInsensitive] ) Then
            // Autoinsertion si pas dans la liste
            Begin
              LookupSource.DataSet.Insert ;
              LookupSource.DataSet.FieldByName ( LookupDisplay ).Value := Text ;
              LookupSource.DataSet.Post ;
              LookupValue := LookupSource.DataSet.FieldByName ( LookupField ).Value ;
              FUpdate := True ;
              if Update Then
                FDataLink.UpdateRecord;
            End
          Else
          // sinon affectation de Datafield uniquement
            Begin
              FUpdate := True ;
              if Update Then
                Begin
                  FDataLink.UpdateRecord;
                End ;
            End ;
        End
      Else
        // pas de texte : on remet le texte originel
        LookupValue := OldText ;
      FModify := False ;
    except
      SelectAll;
      SetFocus;
      raise;
    end;
  FUpdate := False ;
End ;

////////////////////////////////////////////////////////////////////////////////
// procédure   : DoExit
// description : Défocus du composant
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.DoExit;
begin
  // Auto-insertion
  InsertLookup ( True );
  // Focused à False
  SetFocused(False);
  CheckCursor;
  inherited DoExit;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement message  : WMPaint
// description : Peinture de la combo
// paramètre   : Msg : données du message
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.WMPaint(var Msg: TWMPaint);
const
  AlignStyle: array [Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  // destruction : pas besoin de peindre
  if csDestroying in ComponentState then
    Exit;
  // alignement horizontal en cours
  AAlignment := Alignment; //FAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    // alignement horizontal en cours
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then
        ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then
        ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;

{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  // Initialisation de la peinture
  DC := Msg.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    // couleur de police
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      // Mode simple
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      // Couleur de pinceau
      Brush.Color := Color;
      if not Enabled then
        // désactivation
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        //récupération du texte du champ
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase:
            S := AnsiUpperCase(S);
          ecLowerCase:
            S := AnsiLowerCase(S);
        end;
      end
      else
        //récupération du texte d'édition
        S := EditText;
        // mode mot de passe
      if PasswordChar <> #0 then
        FillChar(S[1], Length(S), PasswordChar);
        // Marges
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify:
          Left := Margins.X;
        taRightJustify:
          Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then
        UpdateTextFlags;
        // affiche le texte
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    // libération du canevas
    FCanvas.Handle := 0;
    if Msg.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Evènement message  : CMGetDataLink
// description : Récupération du handle du datalink
// paramètre   : Msg : données du message
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : GetTextMargins
// description : Récupère les marges sur le texte
// paramètre   : résultat : les marges du haut et du bas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  // mode style 3D ou pas
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then
      I := 0
    else
      // mode enfoncement et superposé
    if Ctl3D then
      I := 1
    else
      I := 2;
      // Nouvelles marges : avertir windows
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end
  else
  begin
    // Aucune marge prédéfinie sinon
    if BorderStyle = bsNone then
      I := 0
    else
    begin
      // calculs des marges autour du texte
      DC := GetDC(HWND_DESKTOP);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(HWND_DESKTOP, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : ExecuteAction
// description : Exécute une classe d'action
// paramètre   : Action   : L'action répertoriée
//              résultat : Action exécutée ou pas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

////////////////////////////////////////////////////////////////////////////////
// fonction    : UpdateAction
// description : Exécute une classe d'action de mise à jour
// paramètre   : Action   : L'action répertoriée
//              résultat : Action exécutée ou pas
////////////////////////////////////////////////////////////////////////////////
function TExtDBComboInsert.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

////////////////////////////////////////////////////////////////////////////////
// procédure Evènement : PopupCloseUp
// description : Affectation évenutelle à la fermeture de la liste
// Paramètres  : Sender : La liste
//               Accept : affectation ou pas
////////////////////////////////////////////////////////////////////////////////
procedure TExtDBComboInsert.PopupCloseUp(Sender: TObject;
  Accept: Boolean);
begin
  // vérfications pour affectation
  if Accept
  and  assigned ( LookupSource )
  and assigned ( LookupSource.DataSet )
  and assigned ( LookupSource.DataSet.FindField ( LookupField ))
  and assigned ( FDataLink.Field ) Then
    try
      // affectation
      FDataLink.Dataset.edit ;
      FDataLink.Field.Value := LookupSource.DataSet.FindField ( LookupField ).Value ;
    finally
    End ;

  inherited;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TDBLookupComboInsert );
{$ENDIF}
end.
