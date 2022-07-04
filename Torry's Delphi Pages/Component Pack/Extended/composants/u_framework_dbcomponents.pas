unit u_framework_dbcomponents;

{$I ..\Compilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
   LCLIntf, LCLType,
   SQLDB, lmessages,
   RxDBGrid,
   dbdateedit,
{$ELSE}
   Windows, Mask, DBTables, ActnMan,
{$ENDIF}
{$IFDEF JEDI}
  JvDBGrid,JvDBLookup,jvDBUltimGrid, jvDBControls, JvDBDateTimePicker,
{$ENDIF}
{$IFDEF RX}
  RxLookup,
{$ENDIF}
{$IFDEF EXRX}
  ExRXDBGrid,
{$ELSE}
  DB,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Graphics, Controls, Classes, ExtCtrls, Dialogs, Messages,
  Buttons, Forms, DBCtrls,
  DBGrids, ComCtrls, StdCtrls, SysUtils,
  TypInfo, Variants, u_extcomponent,
{$IFDEF TNT}
   TntDBGrids, TntStdCtrls, TntDBCtrls,
{$ENDIF}
  fonctions_erreurs, u_framework_components;

{$IFDEF VERSIONS}
const
    gVer_framework_DBcomponents : T_Version = ( Component : 'Composants d''interactivité de données' ;
                                               FileUnit : 'u_framework_dbcomponents' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Composants d''interactivité de U_CustomFrameWork.' ;
                                               BugsStory : '0.9.0.0 : Création à partir de u_framework_components.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 9 ; Release : 0 ; Build : 0 );

{$ENDIF}
type

{ TFWDBEdit }

   TFWDBEdit = class ( {$IFDEF TNT}TTntDBEdit{$ELSE}TDBEdit{$ENDIF}, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TFWLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label : TFWLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
       property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
       property MyLabel : TFWLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
     End;

   TFWDBDateEdit = class ( {$IFDEF FPC}TDBDateEdit{$ELSE}TJvDBDateEdit{$ENDIF}, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TFWLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label : TFWLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
       property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
       property MyLabel : TFWLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
     End;

{$IFNDEF FPC}

   TFWDBDateTimePicker = class ( TJvDBDateTimePicker, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TFWLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label : TFWLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
       property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
       property MyLabel : TFWLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
     End;

{$ENDIF}

   { TFWLabel }
   TFWDBLookupCombo = class ( {$IFDEF JEDI}TJvDBLookupCombo{$ELSE}{$IFDEF FPC}TDBLookupComboBox{$ELSE}{$IFDEF RX}TRxDBLookupCombo{$ELSE}TDBLookupComboBox{$ENDIF}{$ENDIF}{$ENDIF}, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TFWLabel ;
       FOldColor ,
       FColorReadOnly,
       FColorFocus ,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label : TFWLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
       property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
       property MyLabel : TFWLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
     End;

   { TFWDBGrid }

   TFWDBGrid = class ( {$IFDEF TNT}TTntDBGrid{$ELSE}{$IFDEF EXRX}TExRxDBGrid{$ELSE}{$IFDEF JEDI}TJvDBUltimGrid{$ELSE}TRXDBGrid{$ENDIF}{$ENDIF}{$ENDIF}, IFWComponent )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FColorEdit     ,
       FColorFocus    ,
       FOldFixedColor : TColor;
       FAlwaysSame : Boolean;
       FFieldsTags : Array of Integer ;
       function  fi_getFieldTags ( li_i : LongInt ):Integer;
       procedure p_setFieldTags ( li_i : LongInt ; const a_value : Integer );
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure KeyUp(var ach_Key: Word; ashi_Shift: TShiftState); override;
       {$IFDEF EXRX}
       procedure TitleClick(Column: TColumn); override;
       {$ELSE}
       procedure DoTitleClick(ACol: Longint; AField: TField); override;
       {$ENDIF}
       property FieldsTags [ Index : Longint ] : Integer read fi_getFieldTags write p_setFieldTags;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_GRILLE_STD ;
       property FixedColor default CST_GRILLE_STD ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_GRILLE_SELECT ;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
     End;

   TFWDBMemo = class ( TDBMemo, IFWComponent, IFWComponentEdit )
      private
       FBeforeEnter, FBeforeExit : TNotifyEvent;
       FLabel : TFWLabel ;
       FOldColor ,
       FColorFocus ,
       FColorReadOnly,
       FColorEdit ,
       FColorLabel : TColor;
       FAlwaysSame : Boolean;
       FNotifyOrder : TNotifyEvent;
       procedure p_setLabel ( const alab_Label : TFWLabel );
       procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      public

       constructor Create ( AOwner : TComponent ); override;
       procedure DoEnter; override;
       procedure DoExit; override;
       procedure Loaded; override;
       procedure SetOrder ; virtual;
      published
       property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
       property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
       property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
       property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
       property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
       property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
       property MyLabel : TFWLabel read FLabel write p_setLabel;
       property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
       property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
     End;


implementation

uses fonctions_db;



{ TFWDBEdit }

procedure TFWDBEdit.p_setLabel(const alab_Label: TFWLabel);
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      FLabel.MyEdit := Self;
    End;
end;

procedure TFWDBEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDBEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

{$IFNDEF FPC}
{ TFWDBDateTimePicker }

procedure TFWDBDateTimePicker.p_setLabel(const alab_Label: TFWLabel);
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      FLabel.MyEdit := Self;
    End;
end;

procedure TFWDBDateTimePicker.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBDateTimePicker.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDBDateTimePicker.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBDateTimePicker.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBDateTimePicker.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

{$ENDIF}

{ TFWDBDateEdit }

procedure TFWDBDateEdit.p_setLabel(const alab_Label: TFWLabel);
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      FLabel.MyEdit := Self;
    End;
end;

procedure TFWDBDateEdit.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBDateEdit.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDBDateEdit.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBDateEdit.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBDateEdit.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;

{ TFWDBLookupCombo }

procedure TFWDBLookupCombo.p_setLabel(const alab_Label: TFWLabel);
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      FLabel.MyEdit := Self;
    End;
end;

procedure TFWDBLookupCombo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBLookupCombo.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDBLookupCombo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBLookupCombo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBLookupCombo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;


{ TFWDBGrid }

constructor TFWDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorFocus := CST_GRILLE_SELECT;
  FColorEdit  := CST_GRILLE_STD;
end;

procedure TFWDBGrid.Loaded;
begin
  inherited Loaded;
  FOldFixedColor := FixedColor;
  if  FAlwaysSame
   Then
    FixedColor := gCol_Grid ;
End;

procedure TFWDBGrid.p_setFieldTags( li_i:Longint; const a_value: Integer);
begin
  if li_i > high ( FFieldsTags ) then
   Setlength ( FFieldsTags, li_i + 1 );
  FFieldsTags [ li_i ] := a_value;
end;

function TFWDBGrid.fi_getFieldTags( li_i: Longint): Integer;
begin
  Result := -1 ;
  if li_i < high ( FFieldsTags ) then
    Result := FFieldsTags [ li_i ];

end;


procedure TFWDBGrid.KeyUp(var ach_Key: Word; ashi_Shift: TShiftState);
begin
  if  ( ach_Key = VK_DELETE )
  and ( ashi_Shift = [ssCtrl] )
  and assigned ( DataSource )
  and assigned ( Datasource.DataSet ) Then
    Begin
      Datasource.DataSet.Delete;
      ach_Key := 0 ;
    End;
  inherited KeyUp(ach_Key, ashi_Shift);
end;

procedure TFWDBGrid.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Rétablit le focus si on l'a perdu
  if FAlwaysSame
   Then
    FixedColor := gCol_GridSelect
   else
    FixedColor := FColorFocus ;
  inherited DoEnter;
end;

procedure TFWDBGrid.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  // Rétablit la couleur du focus
  if FAlwaysSame
   Then
    FixedColor := gCol_Grid
   else
    FixedColor := FOldFixedColor ;
end;

 // Gestion du click sur le titre
{$IFDEF EXRX}
procedure TFWDBGrid.TitleClick(Column: TColumn);
{$ELSE}
procedure TFWDBGrid.DoTitleClick(ACol: Longint; AField: TField);
{$ENDIF}

var li_Tag , li_i : Integer ;
begin
  // Phase d'initialisation
 li_Tag := FieldsTags [ {$IFDEF EXRX} Column.Index {$ELSE}ACol{$ENDIF} ];
 if li_tag <> -1 then
   for li_i :=0 to ComponentCount -1 do
      if  ( li_Tag = Components [ li_i ].Tag )
      and Supports( Components [ li_i ], IFWComponentEdit )
      and ( Components [ li_i ] as TWinControl ).Visible  Then
        Begin
          ( Components [ li_i ] as IFWComponentEdit).SetOrder;
        End ;
  SetFocus;
end;


{ TFWDBMemo }

procedure TFWDBMemo.p_setLabel(const alab_Label: TFWLabel);
begin
  if alab_Label <> FLabel Then
    Begin
      FLabel := alab_Label;
      FLabel.MyEdit := Self;
    End;
end;

procedure TFWDBMemo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

constructor TFWDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TFWDBMemo.DoEnter;
begin
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son tlabel par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TFWDBMemo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TFWDBMemo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TFWDBMemo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
  inherited;
End;


{$IFDEF VERSIONS}
initialization
  // Gestion de version
  p_ConcatVersion(gVer_framework_DBcomponents);
{$ENDIF}
end.

