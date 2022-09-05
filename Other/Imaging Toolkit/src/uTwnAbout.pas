unit uTwnAbout;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.Classes, Vcl.Graphics, Vcl.Forms,
     Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons,
     Vcl.ExtCtrls, System.SysUtils, System.Win.Registry, WinApi.ShellAPI,
     {$ELSE}
     WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
     Buttons,  ExtCtrls, SysUtils,
     ShellAPI, Registry,
     {$ENDIF}
     mcmAboutConst;

type
  TURLLabelType = (ulaAuto, ulaPassive, ulaLink);
  TURLLinkType = (uliHttp, uliMailTo);

  TURLLabel = class(TCustomLabel)
  private
    FLinkFont       : TFont;
    FPassiveFont    : TFont;
    FURL            : string;
    FURLCaption     : TCaption;
    FShowPrefix     : Boolean;
    FCaptionChanged : Boolean;
    FLabelType      : TURLLabelType;
    FLinkType       : TURLLinkType;
    FLinkable       : Boolean;
    FURLAsHint      : Boolean;
    procedure SetLinkFont(Value : TFont);
    procedure SetPassiveFont(Value : TFont);
    procedure SetURL(Value : string);
    procedure SetShowPrefix(Value : Boolean);
    procedure SetLabelType(Value : TURLLabelType);
    procedure SetLinkType(Value : TURLLinkType);
    procedure SetURLCaption(Value : TCaption);
    function  GetCaption : TCaption;
    procedure SetCaption(Value : TCaption);
    procedure SetURLAsHint(Value : Boolean);
  protected
    procedure SetAFont(AFont, AValue : TFont);
    procedure CheckLink;
    procedure SetViewFont;
    procedure SetTheCaption;
    procedure UpdateHint;
    procedure Click; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
    property    Linkable : Boolean
      read      FLinkable;
    property    URLCaption : TCaption
      read      FURLCaption;
  published
    { Published declarations - Inherited }
    property ShowHint;
    property Transparent;
    property Color;
    property Align;
    property Alignment;
    property AutoSize;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    { Published declarations }

    property LinkFont : TFont
      read   FLinkFont
      write  SetLinkFont;
    property PassiveFont : TFont
      read   FPassiveFont
      write  SetPassiveFont;

    property Caption
      read   GetCaption
      write  SetCaption;
    property URL : string
      read   FURL
      write  SetURL;
    property URLAsHint : Boolean
      read   FURLAsHint
      write  SetURLAsHint;
    property ShowPrefix : Boolean
      read   FShowPrefix
      write  SetShowPrefix;
    property LabelType : TURLLabelType
      read   FLabelType
      write  SetLabelType;
    property LinkType : TURLLinkType
      read   FLinkType
      write  SetLinkType;
  end;

type
  TmcmAboutBox    = class(TForm)
    Panel         : TPanel;
    ProductName   : TLabel;
    Version       : TLabel;
    Copyright     : TLabel;
    Image1        : TImage;
    OkButton      : TButton;
    bgLicense     : TGroupBox;
    lDemoNote     : TLabel;
    lmcm          : TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDemoVer : boolean;
  public
    URLProd     : TURLLabel;
    URLLabel    : TURLLabel;
    MailtoLabel : TURLLabel;
    property DemoVersion : boolean
      write  FDemoVer;
  end;


var mcmAboutBox : TmcmAboutBox;

implementation

{$R *.DFM}

Uses {$IFDEF GE_DXE2}
     WinApi.MAPI,
     {$ELSE}
     MAPI,
     {$ENDIF}twain;

{$IFDEF VER93} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF VER110} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF VER125} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF VER130} {$IFDEF BCB} {$DEFINE CBUILDER} {$ENDIF} {$ENDIF}
{$IFDEF VER135} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF VER140} {$IFDEF BCB} {$DEFINE CBUILDER} {$ENDIF} {$ENDIF}
{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

procedure TmcmAboutBox.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end; // TmcmAboutBox.OKButtonClick.

procedure TmcmAboutBox.FormCreate(Sender : TObject);
begin
  {$IFDEF ACTIVETWAIN}
    ProductName.AutoSize  := False;
    ProductName.Width     := 190;
    ProductName.Caption   := defActiveToolkit;
    ProductName.Alignment := taCenter;
    Version.Alignment     := taCenter;
  {$ENDIF}
  FDemoVer := False;
end; // TAboutBox.FormCreate.


procedure TmcmAboutBox.FormDestroy(Sender : TObject);
begin
end; // TAboutBox.FormDestroy.


procedure TmcmAboutBox.FormShow(Sender: TObject);
begin
  {$IFDEF MCMDEMO}
    DefaultWait := 60000;
  {$ENDIF}

  {$IFNDEF ACTIVETWAIN}
    URLProd          := TURLLabel.Create(Panel);
    URLProd.SetParent(Panel);
    {$IFDEF CBUILDER}
      ProductName.Caption := defCBuilderToolkit;
      URLProd.Caption   := defCBuilderURL;
      URLProd.URL       := defCBuilderURL;
    {$ELSE}
      ProductName.Caption := defDelphiToolkit;
      URLProd.Caption   := defDelphiURL;
      URLProd.URL       := defDelphiURL;
    {$ENDIF}

    URLProd.AutoSize  := False;
    URLProd.Left      := 8;
    URLProd.Top       := 140;
    URLProd.Width     := 220;
    URLProd.Alignment := taCenter;
  {$ENDIF}

  // web page reference
  URLLabel           := TURLLabel.Create(Panel);
  URLLabel.SetParent(Panel);
  URLLabel.Caption   := defURL;
  URLLabel.URL       := defURL;
  URLLabel.AutoSize  := False;
  URLLabel.Left      := 8;
  URLLabel.Top       := 100;
  URLLabel.Width     := 220;
  URLLabel.Alignment := taCenter;

  // e-mail
  MailtoLabel := TURLLabel.Create(Panel);
  MailtoLabel.SetParent(Panel);
  MailtoLabel.Caption  := defMailto;
  MailtoLabel.URL      := defMailto;
  MailtoLabel.AutoSize := False;
  MailtoLabel.Left     := 8;
  MailtoLabel.Top      := 120;
  MailtoLabel.Width    := 220;
  MailtoLabel.Alignment := taCenter;

  Version.Caption := defVersion;
  Copyright.Caption := defCopyRight;
  lmcm.Caption := defCompanyAddress;

  if FDemoVer
  then begin
       bgLicense.Visible := True;
       Panel.Height := 165;
       Height := 374;
  end
  else begin
       bgLicense.Visible := False;
       Panel.Height := 169;
       OkButton.Top := 184;
       Height := 248;
  end;
end; // TmcmAboutBox.FormShow.


function GetApplicationPathFromExtension(const Extension : string) : string;
var Str : string;
Begin
  Result := '';
  with TRegistry.Create
  do begin
     try
       RootKey := HKEY_CLASSES_ROOT;
       if OpenKey('\' + Extension, False)
       then begin
            Str := ReadString('');
            if (Str <> '')
            then begin
                 if OpenKey('\' + Str + '\shell\open\command', False)
                 then begin
                      Str := ReadString('');
                      if Str <> ''
                      then Result := Str;
                 end;
            end
            else begin
                 if OpenKey('\' + Extension + '\shell\open\command', False)
                 then begin
                      Str := ReadString('');
                      if (Str <> '')
                      then Result := Str;
                 end;
            end;
       end;
     finally
       Free;
     end;
  end;
end; // GetApplicationPathFromExtension.

constructor TURLLabel.Create(AOwner : TComponent);
Begin
  Inherited Create(AOwner);
  CheckLink;

  FLinkFont := TFont.Create;
  FLinkFont.Assign(Font);
  FLinkFont.Color := clBlue;
  FLinkFont.Style := FLinkFont.Style + [fsUnderline];

  FPassiveFont := TFont.Create;
  FPassiveFont.Assign(Font);

  SetURL(defURL);

  SetViewFont;

  ShowAccelChar := False;
  FURLAsHint := True;
end; // TURLLabel.Create.

Destructor TURLLabel.Destroy;
begin
  FLinkFont.Free;
  FPassiveFont.Free;
  Inherited Destroy;
end; // TURLLabel.Destroy.

procedure TURLLabel.Paint;
begin
  Inherited Paint;
  {$IFDEF MCMDEMO}
  if (pCR_LF[0] <> char($0D))
  then pCR_LF[0] := char($0D);
  {$ENDIF}
end; // TURLLabel.Paint.

procedure TURLLabel.SetAFont(AFont, AValue : TFont);
begin
  if (AFont <> NIL)
  then AFont.Assign(AValue);
end; // TURLLabel.SetAFont.

procedure TURLLabel.SetLinkFont(Value : TFont);
begin
  SetAFont(FLinkFont,Value);
end; // TURLLabel.SetLinkFont.

procedure TURLLabel.SetPassiveFont(Value : TFont);
begin
  SetAFont(FPassiveFont,Value);
end; // TURLLabel.SetPassiveFont.

procedure TURLLabel.SetViewFont;
begin
  if (FLabelType = ulaLink) or
     (FLinkable and (FLabelType = ulaAuto))
  then begin
       Font := LinkFont;
       if Not(csDesigning IN ComponentState)
       then Cursor := crHandPoint;
  end
  else begin
       Font := PassiveFont;
       if Not(csDesigning in ComponentState)
       then Cursor := crDefault;
  end;
end; // TURLLabel.SetViewFont.

procedure TURLLabel.SetURL(Value : string);
var Str : string;
begin
  if (FURL = Value)
  then Exit;
  if (Pos('@', Value) <> 0)
  then FLinkType := uliMailTo;

  if (Pos('/', Value) <> 0)
  then FLinkType := uliHttp;

  Str := LowerCase(Copy(Value, 1, 7));
  if (Str = 'mailto:') or (Str = 'http://')
  then FURL := Copy(Value, 8, Length(Value))
  else FURL := Value;
  SetTheCaption;
end; // TURLLabel.SetURL

procedure TURLLabel.SetShowPrefix(Value : Boolean);
begin
  FShowPrefix := Value;
  SetTheCaption;
end; // TURLLabel.SetShowPrefix.

procedure TURLLabel.SetLabelType(Value : TURLLabelType);
begin
  if (Value = FLabelType)
  then Exit;
  FLabelType := Value;
  SetViewFont;
end; // TURLLabel.SetLabelType.

procedure TURLLabel.SetLinkType(Value : TURLLinkType);
Begin
  if (FLinkType = Value)
  then Exit;
  FLinkType := Value;
  CheckLink;
  case FLinkType of
  uliMailTo : if FURL = defURL
              then FURL := defMailto;
  uliHttp   : if FURL = defMailto
              then FURL := defURL;
  end;
  SetTheCaption;
end; // TURLLabel.SetLinkType.

procedure TURLLabel.CheckLink;
var AModule : HModule;
begin
  case FLinkType of
  uliHttp : FLinkable := (GetApplicationPathFromExtension('.html') <> '') and
                         (GetApplicationPathFromExtension('.htm') <> '');
  uliMailTo : begin
                AModule := LoadLibrary(PChar(MAPIDLL));
                FLinkable := (AModule > 32);
                if FLinkable
                then FreeLibrary(AModule);
              end;
  end;
end; // TURLLabel.CheckLink.

procedure TURLLabel.SetTheCaption;
begin
  if FShowPrefix
  then begin
       case FLinkType of
       uliHttp   : SetURLCaption('http://' + FURL);
       uliMailTo : SetURLCaption('mailto:' + FURL);
       end;
  end
  else SetURLCaption(FURL);
end; // TURLLabel.SetTheCaption.

procedure TURLLabel.Click;
var Param : string;
begin
  Inherited Click;
  if (FLabelType = ulaLink) or
     (FLinkable and (FLabelType = ulaAuto))
     then begin
          case FLinkType of
          uliHttp   : Param := 'http://' + FURL;
          uliMailTo : Param := 'mailto:' + FURL;
          end;
          ShellExecute(0, 'open', PChar(Param), nil, nil, SW_SHOWNORMAL);
     end;
end; // TURLLabel.Click.

function TURLLabel.GetCaption : TCaption;
begin
  Result := Inherited Caption;
end; // TURLLabel.GetCaption.

procedure TURLLabel.SetCaption(Value : TCaption);
begin
  FCaptionChanged := True;
  Inherited Caption := Value;
end; // TURLLabel.SetCaption.

Procedure TURLLabel.SetURLCaption(Value : TCaption);
Begin
  if Not(FCaptionChanged)
  then Inherited Caption := Value;
  FURLCaption := Value;
  UpdateHint; // Update the hint values
end; // TURLLabel.SetURLCaption.

procedure TURLLabel.SetURLAsHint(Value : Boolean);
begin
  if (FURLAsHint = Value)
  then Exit;
  FURLAsHint := Value;
  UpdateHint;
end; // TURLLabel.SetURLAsHint.

procedure TURLLabel.UpdateHint;
begin
  if URLAsHint
  then Inherited Hint := URLCaption
  else Inherited Hint := '';
end; // TURLLabel.UpdateHint

{$IFDEF CBUILDER} {$UNDEF CBUILDER} {$ENDIF}
{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

end.

