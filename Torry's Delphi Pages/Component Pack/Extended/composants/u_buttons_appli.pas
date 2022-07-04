unit u_buttons_appli;

{$I ..\Compilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
   lresources,
{$ELSE}
   Windows,
{$ENDIF}
  Classes,
{$IFDEF VERSIONS}
   fonctions_version,
{$ENDIF}
  JvXPButtons;

const
{$IFDEF VERSIONS}
    gVer_buttons_appli : T_Version = ( Component : 'Boutons personnalisés' ;
                                               FileUnit : 'u_buttons_appli' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Composants boutons personnalisés.' ;
                                               BugsStory : '0.8.0.0 : Gestion à tester.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 8 ; Release : 0 ; Build : 0 );
{$ENDIF}
   CST_FWCLOSE='TFWCLOSE';
   CST_FWCANCEL='TFWCANCEL';
   CST_FWOK='TFWOK';
   CST_FWINSERT='TFWINSERT';
   CST_FWCOPY='TFWCOPY';
   CST_FWQUIT='TFWQUIT';
   CST_FWERASE='TFWERASE';
   CST_FWSAVEAS='TFWSAVEAS';
   CST_FWPRINT='TFWPRINT';
   CST_FWDOCUMENT='TFWDOCUMENT';
   CST_FWPREVIEW='TFWPREVIEW';
   CST_FWINIT='TFWINIT';
{$IFDEF GROUPVIEW}
   CST_FWOUTSELECT='TFWOUTSELECT';
   CST_FWINSELECT='TFWINSELECT';
   CST_FWOUTALL='TFWOUTALL';
   CST_FWINALL='TFWINALL';
{$ENDIF}

type

   IFWButton = interface
       procedure Paint;
   end;
{ TFWClose }

   TFWClose = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public
       constructor Create ( AOwner : TComponent ) ; override;
       procedure Paint; override;
       procedure Click; override;
      published
       property Glyph stored False;
     End;

{ TFWCancel }

   TFWAbort = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWOK }
   TFWOK = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWInsert }
   TFWInsert = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWDocument }
   TFWDocument = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWQuit }
   TFWQuit = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWErase }
   TFWErase = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWSaveAs }
   TFWSaveAs = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWPrint }
   TFWPrint = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWPreview }
   TFWPreview = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWCopy }
   TFWCopy = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWInit }
   TFWInit = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{$IFDEF GROUPVIEW}
{ TFWOutSelect }
   TFWOutSelect = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWOutAll }
   TFWBasket = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
       property Caption stored False;
     End;

   TFWRecord = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
       property Caption stored False;
     End;

   TFWCancel = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
       property Caption stored False;
     End;

   TFWOutAll = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWInSelect }
   TFWInSelect = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{ TFWInAll }
   TFWInAll = class ( TJvXPButton,IFWButton )
      private
      {$IFNDEF FPC}
       ResInstance             : THandle      ;
      {$ENDIF}
      public

       procedure Paint; override;
      published
       property Glyph stored False;
     End;

{$ENDIF}

implementation

uses {$IFNDEF FPC}Consts,{$ENDIF}{$IFDEF GROUPVIEW}unite_messages,{$ENDIF}Forms ;

{$IFDEF DELPHI}
  {$R *.dcr}
{$ENDIF}


{ TFWClose }


procedure TFWClose.Click;
begin
  if not assigned ( OnClick )
  and ( Owner is TCustomForm ) then
    ( Owner as TCustomForm ).Close;
  inherited;

end;

constructor TFWClose.Create(AOwner: TComponent);
begin
  inherited;
  Caption := SCloseButton;
end;

procedure TFWClose.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWCLOSE);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWCLOSE);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWCancel }

procedure TFWCancel.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWCANCEL);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWCANCEL);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWOK }

procedure TFWOK.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWOK);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWOK);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWInsert }

procedure TFWInsert.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWINSERT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWINSERT);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWDocument }

procedure TFWDocument.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWDOCUMENT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWDOCUMENT);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWSaveAs }

procedure TFWSaveAs.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWSAVEAS);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWSAVEAS);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWQuit }

procedure TFWQuit.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWQUIT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWQUIT);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWerase }

procedure TFWErase.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWERASE);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWERASE);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWPrint }

procedure TFWPrint.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWPRINT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWPRINT);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWPreview }

procedure TFWPreview.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWPREVIEW);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWPREVIEW);
    {$ENDIF}
    End;
  inherited Paint;
end;

{ TFWInit }

procedure TFWInit.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWINIT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWINIT);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWCopy }

procedure TFWCopy.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWCOPY);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWCOPY);
    {$ENDIF}
    End;
  inherited;

end;


{$IFDEF GROUPVIEW}

{ TFWOutSelect }

procedure TFWOutSelect.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWOUTSELECT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWOUTSELECT);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWBasket }
procedure TFWBasket.Paint;
begin
  Caption := Gs_GROUPVIEW_Basket;
  inherited;

end;

{ TFWRecord }
procedure TFWRecord.Paint;
begin
  Caption := Gs_GROUPVIEW_Record;
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWOK);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWOK);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWRecord }
procedure TFWAbort.Paint;
begin
  Caption := Gs_GROUPVIEW_Abort;
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWCANCEL);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWCANCEL);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWOutAll }

procedure TFWOutAll.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWOUTALL);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWOUTALL);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWInSelect }

procedure TFWInSelect.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWINSELECT);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWINSELECT);
    {$ENDIF}
    End;
  inherited;

end;

{ TFWInAll }

procedure TFWInAll.Paint;
begin
  if Glyph.Bitmap.Empty then
    Begin
    {$IFDEF FPC}
      Glyph.LoadFromLazarusResource( CST_FWINALL);
    {$ELSE}
      if ( ResInstance = 0 ) Then
        ResInstance:= FindResourceHInstance(HInstance);
      Glyph.Bitmap.LoadFromResourceName(ResInstance, CST_FWINALL);
    {$ENDIF}
    End;
  inherited;

end;

{$ENDIF}

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_buttons_appli  );
{$ENDIF}
{$IFDEF FPC}
  {$I u_buttons_appli.lrs}
{$ENDIF}

end.

