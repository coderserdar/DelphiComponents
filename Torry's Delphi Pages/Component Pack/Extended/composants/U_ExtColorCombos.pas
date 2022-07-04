{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtColorCombo :                                        }
{             Objet de choix de couleur                               }
{             qui permet de personnalisé la couleur du titre          }
{             de l'onglet actif                                       }
{             10 Mars 2006                                            }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtColorCombos;

{$I ..\Compilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\Compilers.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, lMessages, lresources,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF TNT}
   TntStdCtrls,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls, fonctions_erreurs, fonctions_version,
  u_extcomponent ;

type
  TColorcomboLanguage = (lgEnglish, lgPortuguese, lgFrench);

const
{$IFDEF VERSIONS}
    gVer_TExtColorCombo : T_Version = ( Component : 'Composant TExtColorCombo' ;
                                               FileUnit : 'U_ExtColorCombo' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Choisir une couleur dans une liste ou avec la palette de couleurs.' ;
                                               BugsStory : '1.0.1.0 : Bug du re-focus enlevé, propriétés Combo.' + #13#10 +
                                                           '1.0.0.0 : OK.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );
    gVer_TDBColorCombo : T_Version = ( Component : 'Composant TExtDBColorCombo' ;
                                               FileUnit : 'U_ExtColorCombo' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Choisir une couleur dans une liste ou avec la palette de couleurs.' + #13#10 + 'Descendant de TExtColorCombo avec lien aux données.' ;
                                               BugsStory : '1.0.1.0 : Améliorations sur la gestion des erreurs' + #13#10
                                                         + '1.0.0.1 : Bug ''pas en mode édition'' enlevé.' + #13#10
                                                         + '1.0.0.0 : OK.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );
{$ENDIF}
    CST_COLOR_COMBO_DEFAULT_COLOR_VALUE = -1 ;
    CST_COLOR_COMBO_DEFAULT_COLOR       = clWhite ;
    CST_COLOR_COMBO_DEFAULT_COLOR_HTML  = '#FFFFFF' ;
    CST_COLOR_COMBO_DEFAULT_LANGUAGE    = lgFrench ;
    CST_COLOR_COMBO_DEFAULT_STYLE       = csOwnerDrawFixed ;
type

  TExtColorCombo = class(TCustomComboBox, IFWComponent, IFWComponentEdit)
    { Private declarations }
      ColorDlg: TColorDialog;
    private
      FBeforeEnter, FBeforeExit : TNotifyEvent;
      FLabel : {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} ;
      FOldColor ,
      FColorFocus ,
      FColorReadOnly,
      FColorEdit ,
      FColorLabel : TColor;
      FReadOnly   ,
      FAlwaysSame : Boolean;
      FNotifyOrder : TNotifyEvent;
      FLanguage : TColorComboLanguage;
      FColorValue : TColor;
      FHTMLColor: shortstring;
      procedure SetHTMLColor(Value: shortstring);
      procedure SetLanguage(Lang: TColorcomboLanguage);
      procedure p_setLabel ( const alab_Label : {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} );
      procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      procedure WMLButtonDown(var Message: {$IFDEF FPC}TLMLButtonDown{$ELSE}TWMLButtonDown{$ENDIF}); message {$IFDEF FPC}LM_LBUTTONDOWN{$ELSE}WM_LBUTTONDOWN{$ENDIF};
      procedure WMRButtonDown(var Message: {$IFDEF FPC}TLMRButtonDown{$ELSE}TWMRButtonDown{$ENDIF}); message {$IFDEF FPC}LM_RBUTTONDOWN{$ELSE}WM_RBUTTONDOWN{$ENDIF};
      procedure WMLButtonDblClk(var Message: {$IFDEF FPC}TLMLButtonDblClk{$ELSE}TWMLButtonDblClk{$ENDIF}); message {$IFDEF FPC}LM_LBUTTONDBLCLK{$ELSE}WM_LBUTTONDBLCLK{$ENDIF};
      procedure WMRButtonDblClk(var Message: {$IFDEF FPC}TLMLButtonDblClk{$ELSE}TWMLButtonDblClk{$ENDIF}); message {$IFDEF FPC}LM_RBUTTONDBLCLK{$ELSE}WM_RBUTTONDBLCLK{$ENDIF};
    protected
    { Protected declarations }
      Function WebColor(AColor:TColor): String;
      procedure p_SetColorValue(AColor: TColor); virtual ;
      procedure MouseDown(Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure WMKeyDown(var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF}); message {$IFDEF FPC}LM_KEYDOWN{$ELSE}WM_KEYDOWN{$ENDIF};
    protected
      function GetReadOnly: Boolean; virtual;
      procedure SetReadOnly(Value: Boolean); virtual;
    public
    { Public declarations }
      constructor Create(AOwner: TComponent); override;
      procedure CreateWnd; override;
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure Loaded; override;
      procedure SetOrder ; virtual;
      procedure Change; override;
      procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
      function Focused : Boolean; override ;
    published
    { Published declarations }
      property Language : TColorcomboLanguage read FLanguage write SetLanguage  default CST_COLOR_COMBO_DEFAULT_LANGUAGE;
      property HTMLcolor : shortString read FHTMLColor write SetHTMLColor stored True ;
      property Value : TColor read FColorValue write p_SetColorValue stored True default CST_COLOR_COMBO_DEFAULT_COLOR_VALUE ;
      property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored True default False;
    // Visuel
      property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
      property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
      property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
      property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
      property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
      property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
      property MyLabel : {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} read FLabel write p_setLabel;
      property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
      property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
    // Propriétés gardées
      property AutoComplete;
      property AutoDropDown;
{$IFDEF DELPHI}
      property AutoCloseUp;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property ImeMode;
      property ImeName;
{$ENDIF}
      property Style default CST_COLOR_COMBO_DEFAULT_STYLE; {Must be published before Items}
      property Anchors;
      property BiDiMode;
      property Color;
      property Constraints;
    {$IFNDEF FPC}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  TExtDBColorCombo  = class( TExtColorCombo )
    private
      FDataLink: TFieldDataLink;
      function GetDataField: string;
      function GetDataSource: TDataSource;
      function GetField: TField;
      procedure SetDataField(const AValue: string);
      procedure SetDataSource(AValue: TDataSource);
      procedure WMCut(var Message: TMessage); message {$IFDEF FPC} LM_CUT {$ELSE} WM_CUT {$ENDIF};
      procedure WMPaste(var Message: TMessage); message {$IFDEF FPC} LM_PASTE {$ELSE} WM_PASTE {$ENDIF};
    {$IFDEF FPC}
    {$ELSE}
      procedure WMUndo(var Message: TMessage); message WM_UNDO;
    {$ENDIF}
      procedure CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF}); message CM_EXIT;
      procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    protected
      procedure ActiveChange(Sender: TObject); virtual;
      procedure DataChange(Sender: TObject); virtual;
      procedure UpdateData(Sender: TObject); virtual;
      function GetReadOnly: Boolean; override;
      procedure SetReadOnly(AValue: Boolean); override;
      procedure p_SetColorValue(AColor: TColor); override ;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure KeyPress(var Key: Char); override;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    public
      procedure Change; override;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function ExecuteAction(AAction: TBasicAction): Boolean; override;
      function UpdateAction(AAction: TBasicAction): Boolean; override;
      property Field: TField read GetField;
    published
      property HTMLcolor  stored False ;
      property Value stored False ;
      property DataField: string read GetDataField write SetDataField stored True;
      property DataSource: TDataSource read GetDataSource write SetDataSource stored True;
    end;


implementation

uses unite_messages, fonctions_proprietes;

const
  CST_COLOR_COMBO_LastDefinedColor = 19;
  CST_COLOR_COMBO_ActiveColors: array [0..CST_COLOR_COMBO_LastDefinedColor] of TColor = (
  clBlack, clMaroon, clGreen, clMoneyGreen, clOlive, clNavy, clPurple, clTeal, clGray,
  clSilver, clRed, clLime, clYellow, clInfoBk, clBlue, clSkyBlue, clFuchsia, clAqua, clWhite, CST_COLOR_COMBO_DEFAULT_COLOR );

  CST_COLOR_COMBO_Colors: array [lgEnglish..lgFrench,0..CST_COLOR_COMBO_LastDefinedColor ] of String=(
  (
  'Black','Maroon','Green','Money green', 'Olive', 'Navy', 'Purple', 'Teal', 'Gray',
  'Silver', 'Red', 'Lime', 'Yellow', 'Pale yellow', 'Blue', 'Sky blue', 'Fuchsia', 'Aqua', 'White', 'None'),
  (
  'Preto','Marron','Verde', 'Dinheiro verde', 'Oliva', 'Azul Escuro', 'Roxo', 'Azul-petróleo', 'Cinza',
  'Prata', 'Vermelho', 'Limão', 'Amarelo', 'Empalideça amarelo', 'Azul', 'Azul celeste', 'Rosa', 'Turquesa', 'Branco', 'Nada'),
  (
  'Noir','Marron','Vert','Vert argent', 'Olivier', 'Bleu marine', 'Pourpre', 'Bleu nuit', 'Gris',
  'Argent', 'Rouge', 'Citron', 'Jaune', 'Jaune pâle', 'Bleu', 'Bleu ciel', 'Fuchsia', 'Eau', 'Blanc', 'Indéfini'));


{ TExtColorCombo }

constructor TExtColorCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := CST_COLOR_COMBO_DEFAULT_STYLE;
  FColorValue := CST_COLOR_COMBO_DEFAULT_COLOR_VALUE ;
  FHTMLColor  := CST_COLOR_COMBO_DEFAULT_COLOR_HTML  ;
  FLanguage:= CST_COLOR_COMBO_DEFAULT_LANGUAGE ;

  //Visuel
  FReadOnly   := False;
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TExtColorCombo.p_setLabel(const alab_Label: {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF});
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      p_SetComponentObjectProperty ( FLabel, 'MyEdit', Self );
    End;
end;

function TExtColorCombo.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TExtColorCombo.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TExtColorCombo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

procedure TExtColorCombo.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} par une couleur
  // de fond bleu et son libellÃ© en marron (sauf si le libellÃ© est sÃ©lectionnÃ©
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TExtColorCombo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TExtColorCombo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TExtColorCombo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;


procedure TExtColorCombo.CreateWnd;
var a :  Integer;
begin
  inherited;
  if ( Items.Count = 0 ) then
    Begin
      Items.BeginUpdate;
      for a:=0 to CST_COLOR_COMBO_LastDefinedColor do Items.add(colortostring(CST_COLOR_COMBO_ActiveColors[a]));
      Items.EndUpdate;
    End;
end;

procedure TExtColorCombo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ( Button = mbRight )
  and not DroppedDown Then
    Begin
      ItemIndex := CST_COLOR_COMBO_LastDefinedColor ;
      Change ;
    End ;
end;


procedure TExtColorCombo.WMLButtonDown(var Message: {$IFDEF FPC}TLMLButtonDown{$ELSE}TWMLButtonDown{$ENDIF});
begin
  if not ReadOnly Then
    Begin

      inherited;
    End;

end;
procedure TExtColorCombo.WMLButtonDblClk(var Message: {$IFDEF FPC}TLMLButtonDblClk{$ELSE}TWMLButtonDblClk{$ENDIF});
begin
  if not ReadOnly Then
    Begin

      inherited;
    End;

end;

procedure TExtColorCombo.WMRButtonDblClk(var Message: {$IFDEF FPC}TLMLButtonDblClk{$ELSE}TWMLButtonDblClk{$ENDIF});
begin
  if not ReadOnly Then
    Begin

      inherited;
    End;

end;

procedure TExtColorCombo.WMRButtonDown(var Message: {$IFDEF FPC}TLMRButtonDown{$ELSE}TWMRButtonDown{$ENDIF});
begin
  if not ReadOnly Then
    Begin

      inherited;
    End;

end;
procedure TExtColorCombo.WMKeyDown(var Message: {$IFDEF FPC}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF});
begin
  if not ReadOnly then
    inherited;
end;

procedure TExtColorCombo.SetLanguage(Lang: TColorcomboLanguage);
begin
    FLanguage:=lang;
    ItemIndex:=ItemIndex;
end;


procedure TExtColorCombo.DrawItem(Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
      novorect: trect;
      Texto: array[0..255] of Char;
      Safer: TColor;
      format : Uint ;
begin
    with Canvas do
    begin
      safer:=Brush.Color;
      FillRect(ARect);
      novorect:= rect(arect.Left+4, arect.Top+1, 24, arect.bottom-1);
      Brush.Color := StringToColor(Items[Index]);
      FillRect(novorect);
      Pen.Color := clblack;
      Rectangle(Novorect.Left, Novorect.Top, Novorect.Right, Novorect.Bottom);
      novoRect := rect(ARect.Left + 30, arect.top, arect.right - 5, arect.bottom);

      // Couleur personnalisée
      if  ( FColorValue > -1 )
      and ( Index = CST_COLOR_COMBO_LastDefinedColor )
       Then
        Begin
          StrPCopy(Texto, FHTMLColor)
        End
        // Couleur non personnalisée ou indéfnie
       else
        StrPCopy(Texto, CST_COLOR_COMBO_Colors[Flanguage,Index]);
      format := DT_SINGLELINE or DT_NOPREFIX;
      if ( BiDiMode = bdLeftToRight )
      or (( BiDiMode = bdRightToLeftReadingOnly ) and not DroppedDown ) Then
        format := format or DT_LEFT
      Else
        format := format or DT_RIGHT ;
      if BiDiMode <> bdRightToLeftNoAlign Then
        format := format or DT_VCENTER ;
      Brush.Color := safer;
      DrawText(Canvas.Handle, texto, StrLen(texto), novoRect, format );
    end;
end;

Function TExtColorCombo.WebColor(AColor:TColor): String;
var
     Temp: String;
begin
 Result := '#'+IntToHex(ColorToRGB(AColor),6);
end;

procedure TExtColorCombo.SetHTMLColor(Value: shortstring);
var
    Temp: shortstring;
begin
   Value := uppercase(Value);
   if (Value = '') then FHTMLColor:= CST_COLOR_COMBO_DEFAULT_COLOR_HTML
   else
     FHTMLColor := value;
end;

procedure TExtColorCombo.Change;
begin
  if ItemIndex=CST_COLOR_COMBO_LastDefinedColor then
   begin
    ColorDlg:=TColorDialog.Create(self);
    {$IFDEF DELPHI}
    ColorDlg.Options:=[cdFullOpen];
    {$ENDIF}
    if FColorValue > -1 Then
      colorDlg.Color:= FColorValue;
    if colorDlg.Execute
     then
      p_SetColorValue(colorDlg.Color);
    ColorDlg.free;
   end
  else
   if itemindex <= CST_COLOR_COMBO_LastDefinedColor  Then
     p_SetColorValue(CST_COLOR_COMBO_ActiveColors[itemindex])
    Else
     p_SetColorValue(-1);
  inherited change;
end;

procedure TExtColorCombo.p_SetColorValue(AColor: TColor);
var
   a, AIndex: integer;
   Temp: TColor;
   StringColor : String;
begin
 if FColorValue <> AColor then
   try
     temp:=FColorValue;
     StringColor := ColorToString(AColor);
     AIndex := CST_COLOR_COMBO_LastDefinedColor ;
     for a:=0 to CST_COLOR_COMBO_LastDefinedColor - 1 do
       if items[a] = StringColor then
        begin
         AIndex:=a;
         Break;
        end;
      if AIndex = CST_COLOR_COMBO_LastDefinedColor
       then
        begin
          if AColor = -1 then
            Items[CST_COLOR_COMBO_LastDefinedColor]:=ColorToString(CST_COLOR_COMBO_DEFAULT_COLOR)
           else
            Items[CST_COLOR_COMBO_LastDefinedColor]:=StringColor;
        end;
      FColorValue:= AColor;
      if FColorValue > -1 Then
        SetHtmlColor(webcolor(AColor))
         Else
          SetHtmlColor(webcolor(CST_COLOR_COMBO_DEFAULT_COLOR));
      ItemIndex:=AIndex;
   except
     FColorValue := temp;
   end;
end;


function TExtColorCombo.Focused: Boolean;
begin
  Result := csFocusing in ControlState ;
end;


{ TExtDBColorCombo }
constructor TExtDBColorCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create ;
  FDataLink.DataSource := nil ;
  FDataLink.FieldName  := '' ;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
end;

destructor TExtDBColorCombo.Destroy;
begin
  inherited Destroy;
  FDataLink.Free ;
end;

procedure TExtDBColorCombo.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    Begin
      DataChange(Self);
    End ;
end;

procedure TExtDBColorCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TExtDBColorCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TExtDBColorCombo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    {$IFDEF DELPHI}
    MessageBeep(0);
    {$ENDIF}
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TExtDBColorCombo.Change;
begin
  inherited Change;
  if assigned ( FDataLink.Field ) Then
    if FDataLink.Field.IsNull then
      p_SetColorValue ( -1 )
    Else
      p_SetColorValue ( FDataLink.Field.AsInteger );
  FDataLink.Modified;
end;

function TExtDBColorCombo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TExtDBColorCombo.SetDataSource(AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := AValue;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

function TExtDBColorCombo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TExtDBColorCombo.SetDataField(const AValue: string);
begin
  if  assigned ( FDataLink.DataSet )
  and FDataLink.DataSet.Active Then
    Begin
      if assigned ( FDataLink.DataSet.FindField ( AValue ))
      and ( FDataLink.DataSet.FindField ( AValue ) is TNumericField ) Then
        FDataLink.FieldName := AValue;
    End
  Else
    FDataLink.FieldName := AValue;
end;

function TExtDBColorCombo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TExtDBColorCombo.SetReadOnly(AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

function TExtDBColorCombo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TExtDBColorCombo.ActiveChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    begin
      p_SetColorValue ( FDataLink.Field.AsInteger );
    end;
end;

procedure TExtDBColorCombo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    begin
      p_SetColorValue ( FDataLink.Field.AsInteger );
    end;
end;


procedure TExtDBColorCombo.UpdateData(Sender: TObject);
begin
  if Value > -1 Then
    Begin
      FDataLink.Edit ;
      FDataLink.Field.Value := Value ;
    End ;
end;

{$IFDEF DELPHI}
procedure TExtDBColorCombo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;
{$ENDIF}

procedure TExtDBColorCombo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TExtDBColorCombo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TExtDBColorCombo.CMExit(var Message: {$IFDEF FPC} TLMExit {$ELSE} TCMExit {$ENDIF});
begin
  try
    FDataLink.UpdateRecord;
  except
    on e: Exception do
      Begin
        SetFocus;
        f_GereException ( e, FDataLink.DataSet, nil , False )
      End ;
  end;
  DoExit;
end;

procedure TExtDBColorCombo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TExtDBColorCombo.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction){$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.ExecuteAction(AAction){$ENDIF};
end;

function TExtDBColorCombo.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) {$IFDEF DELPHI}  or (FDataLink <> nil) and
    FDataLink.UpdateAction(AAction){$ENDIF};
end;

procedure TExtDBColorCombo.p_SetColorValue(AColor: TColor);
begin
 inherited p_SetColorValue ( AColor );
 if assigned ( FDataLink.Field )
 and ( FDataLink.Field.AsInteger <> AColor ) Then
  Begin
    FDataLink.Dataset.Edit ;
    FDataLink.Field.Value := AColor ;
  End
 Else
  if assigned ( FDataLink.Field )
  and FDataLink.Field.IsNull Then
    AColor := -1 ;
end;

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_TExtColorCombo   );
  p_ConcatVersion ( gVer_TDBColorCombo );
{$ENDIF}
end.
