{ Copyright (C) 1998-2003, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  This component is an extended label that supports the url-processing
  (http/mailto starting).
}
unit URLLbl;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls;

const
  defURL = 'www.scalabium.com';
  defMailto = 'mshkolnik@scalabium.com';

type
  TLabelType = (Auto, Passive, Active);
  TLinkType = (ltFile, ltFTP, ltGopher, ltHTTP, ltMailTo, ltNews, ltTelNet, ltNone);

  TURLLabel = class(TCustomLabel)
  private
    { Private declarations }
    FLinkFont: TFont;
    FPassiveFont: TFont;
    FURL: string;
    FURLCaption: TCaption;
    FShowPrefix: Boolean;
    FCaptionChanged: Boolean;
    FLabelType: TLabelType;
    FLinkType: TLinkType;
    FLinkAble: Boolean;
    FURLAsHint: Boolean;
    procedure SetLinkFont(Value: TFont);
    procedure SetPassiveFont(Value: TFont);
    procedure SetURL(Value: string);
    procedure SetShowPrefix(Value: Boolean);
    procedure SetLabelType(Value: TLabelType);
    procedure SetLinkType(Value: TLinkType);
    procedure SetURLCaption(Value: TCaption);
    function GetCaption: TCaption;
    procedure SetCaption(Value: TCaption);
    procedure SetURLAsHint(Value: Boolean);
    procedure SetHint(Value: TCaption);
    function GetHint: TCaption;
  protected
    { Protected declarations }
    procedure SetAFont(AFont, AValue: TFont);
    procedure CheckLinkAble;
    procedure SetViewFont;
    procedure SetTheCaption;
    procedure UpdateHint;
    procedure Click; override;

    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LinkAble: Boolean read FLinkAble;
    property URLCaption: TCaption read FURLCaption;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  published
    { Published declarations }
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

    property Hint: TCaption read GetHint write SetHint;
    property LinkFont: TFont read FLinkFont write SetLinkFont;
    property PassiveFont: TFont read FPassiveFont write SetPassiveFont;

    property Caption read GetCaption write SetCaption;
    property URL: string read FURL write SetURL;
    property URLAsHint: Boolean read FURLAsHint write SetURLAsHint;
    property ShowPrefix: Boolean read FShowPrefix write SetShowPrefix;

    property LabelType: TLabelType read FLabelType write SetLabelType default Auto;
    property LinkType: TLinkType read FLinkType write SetLinkType default ltHTTP;
  end;

procedure Register;

function GetProgramPathFromExt(const Ext: string): string;

implementation

uses SysUtils, ShellAPI, Registry;

procedure Register;
begin
  RegisterComponents('SMComponents', [TURLLabel]);
end;

// Pick a program from the registry associated with a extension
function GetProgramPathFromExt(const Ext: string): string;
var s: string;
begin
  Result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;

      if OpenKey('\' + Ext, False) then
      begin
        S := ReadString('');
        if S = '' then
          S := Ext;
        if OpenKey('\' + S + '\shell\open\command', False) then
        begin
          S := ReadString('');
          if S <> '' then
            Result := S;
        end;
      end;
  finally
    Free;
  end;
end;

constructor TURLLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CheckLinkAble;
  Cursor := crHandPoint;

  // Default link font is normal font but
  //   in blue, and underlined
  FLinkFont := TFont.Create;
  FLinkFont.Assign(Font);
  FLinkFont.Color := clBlue;
  FLinkFont.Style := FLinkFont.Style + [fsUnderline];

  // Passive is the normal font
  FPassiveFont := TFont.Create;
  FPassiveFont.Assign(Font);

  FLinkType := ltHTTP;
  FShowPrefix := True;

  // Set web page, and update the caption
  SetURL(defURL);

  // Set the font used for view
  SetViewFont;
  // don't show accelerator char (underline a char)
  ShowAccelChar := False;
  // Use the Hint property as full URL notify
  FURLAsHint := True;
end;

destructor TURLLabel.Destroy;
begin
  FLinkFont.Free;
  FPassiveFont.Free;
  inherited Destroy;
end;

procedure TURLLabel.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
    if (FLabelType = Auto) then
      Font.Assign(FPassiveFont);
end;

procedure TURLLabel.CMMouseEnter(var Msg: TMessage);
begin
 //Change color when mouse is over label
 if (FLabelType = Auto) then
   Font.Assign(LinkFont);
end;


procedure TURLLabel.CMMouseLeave(var Msg: TMessage);
begin
 //Change color when mouse leaves label
 if (FLabelType = Auto) then
   Font.Assign(PassiveFont);
end;

procedure TURLLabel.SetAFont(AFont, AValue: TFont);
begin
  if AFont <> nil then
  begin
    AFont.Assign(AValue);
    Invalidate;
  end;
end;

procedure TURLLabel.SetLinkFont(Value: TFont);
begin
  SetAFont(FLinkFont, Value);
end;

procedure TURLLabel.SetPassiveFont(Value: TFont);
begin
  SetAFont(FPassiveFont, Value);
end;

procedure TURLLabel.SetViewFont;
begin
  // if the label should look like a HTML link
  if (FLabelType = Active) {or
     (FLinkAble and (FLabelType = Auto))} then
    Font := LinkFont
  else  // if the label should look like a normal label
    Font := PassiveFont;
end;

procedure TURLLabel.SetURL(Value: string);
var S: string;
begin
  if FURL = Value then Exit;
  if Pos('@', Value) <> 0 then  // can only be a e-mail
    FLinkType := ltMailTo;
{  if Pos('/', Value) <> 0 then  // can only be a URL
    FLinkType := http;
}  S := LowerCase(Copy(Value, 1, 7));
  if (S = 'mailto:') or (S = 'http://') then
    FURL := Copy(Value, 8, Length(Value))
  else
    FURL := Value;
  SetTheCaption;  // update the caption
end;

procedure TURLLabel.SetShowPrefix(Value: Boolean);
begin
  FShowPrefix := Value;
  SetTheCaption; // update the caption
end;

procedure TURLLabel.SetLabelType(Value: TLabelType);
begin
  if Value = FLabelType then
    Exit;
  FLabelType := Value;
  SetViewFont; // update the font (according to the new type)
end;

procedure TURLLabel.SetLinkType(Value: TLinkType);
begin
  if FLinkType = Value then
    Exit;
  FLinkType := Value;
  CheckLinkAble;
  case FLinkType of
    ltHTTP: if FURL = defMailto then FURL := defURL;
    ltMailTo: if FURL = defURL then FURL := defMailto;
  end;
  SetTheCaption;
end;

procedure TURLLabel.CheckLinkAble;
var AModule: HModule;
begin
  case FLinkType of
    // if the .html and the .htm extension is assigned to
    // a program
    ltHTTP: FLinkAble := (GetProgramPathFromExt('.html') <> '') and
                         (GetProgramPathFromExt('.htm') <> '');
    // Check it the MAPI dll is there
    ltMailTo: begin
               AModule := LoadLibrary('MAPI32.DLL');
               FLinkAble := (AModule > 32);
               if FLinkAble then
                 FreeLibrary(AModule);
              end;
  end;
end;

procedure TURLLabel.SetTheCaption;
begin
  if FShowPrefix then
  begin
    case FLinkType of
      ltFile: SetURLCaption('file://' + FURL);
      ltFTP: SetURLCaption('ftp://' + FURL);
      ltGopher: SetURLCaption('gopher://' + FURL);
      ltHTTP: SetURLCaption('http://' + FURL);
      ltMailTo: SetURLCaption('mailto:'+ FURL);
      ltNews: SetURLCaption('news:' + FURL);
      ltTelNet: SetURLCaption('telnet:' + FURL);
    end;
  end
  else
    SetURLCaption(FURL);
end;

procedure TURLLabel.Click;
var
  Param: string;
begin
  inherited Click;
  if (LinkType <> ltNone) and
     ((FLabelType = Active) or
      (FLinkAble and (FLabelType = Auto))) then
  begin
    case FLinkType of
      ltFile: Param := 'file://' + URL;
      ltFTP: Param := 'ftp://' + URL;
      ltGopher: Param := 'gopher://' + URL;
      ltHTTP: Param := 'http://' + URL;
      ltMailTo: Param := 'mailto:'+ URL;
      ltNews: Param := 'news:' + URL;
      ltTelNet: Param := 'telnet:' + URL;
    end;
    // Execute the default web browser on the
    // web page or the mailto window
    ShellExecute(0, 'open', PChar(Param), nil, nil, SW_SHOWNORMAL);
  end;
end;

function TURLLabel.GetCaption: TCaption;
begin
  Result := inherited Caption; // Get the old caption
end;

procedure TURLLabel.SetCaption(Value: TCaption);
begin
  FCaptionChanged := True;     // Set flag that caption is set by user
  inherited Caption := Value;  // Set the "real" caption variabel
end;

procedure TURLLabel.SetURLCaption(Value: TCaption);
begin
  if NOT FCaptionChanged then   // Check if user have changed the caption
    inherited Caption := Value; // if (s)he havent, set it!
  FURLCaption := Value;
  UpdateHint;                   // Update the hint values
end;

procedure TURLLabel.SetURLAsHint(Value: Boolean);
begin
  if FURLAsHint = Value then
    Exit;
  FURLAsHint := Value;
  UpdateHint;                    // Update the hint values
end;

procedure TURLLabel.UpdateHint;
begin
  if URLAsHint then              // if we use URL as hint
    inherited Hint := URLCaption // copy URL caption
  else
    inherited Hint := '';        // delete it...
end;

procedure TURLLabel.SetHint(Value: TCaption);
begin
  inherited Hint := Value;
end;

function TURLLabel.GetHint: TCaption;
begin
  FURLAsHint := False;        // remove the property which "allows" the
                              // the hint to be used as URL notify event!
  Result := inherited Hint;
end;

end.
