{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtLinkLabel                                    }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_LinkLabel;

interface

uses
   Classes
  ,StdCtrls
  ;

type
  TgtLinkType = (ltURL,ltMailAddress);

  TgtLinkLabel = class(TLabel)
  private
    { Private declarations }
    FUrl         : String;
    FMailAddress : String;
    FLinkType    : TgtLinkType;
    procedure SetUrl(const Value:String);
    procedure SetMailAddress(const Value:String);
    procedure SetLinkType(const Value:TgtLinkType);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    procedure   Click;override;
  published
    { Published declarations }
    property URL         : String      read FUrl         write SetUrl;
    property MailAddress : String      read FMailAddress write SetMailAddress;
    property LinkType    : TgtLinkType read FLinkType    write SetLinkType;
  end;

implementation
 uses
  ShellApi
 ,Windows
 ,Graphics
 ,Controls
 ;

{ TLinkLabel }
{------------------------------------------------------------------------------}
procedure TgtLinkLabel.Click;
begin

  inherited;
  case FLinkType of
    ltURL:
      begin
        ShellExecute(HWND(nil),'open',PChar(FUrl),
                     '','',SW_SHOWMAXIMIZED);
      end;
    ltMailAddress:
      begin
        ShellExecute(HWND(nil),'open','iexplore.exe',
                     PChar('mailto:'+FMailAddress),'',SW_HIDE)//Hide the IE window
      end;
  end;
end;
{------------------------------------------------------------------------------}
constructor TgtLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.Color  := clBlue;
  Font.Style  := Font.Style + [fsUnderline];
  Cursor      := crHandPoint;
end;
{------------------------------------------------------------------------------}
procedure TgtLinkLabel.SetLinkType(const Value: TgtLinkType);
begin
  FLinkType := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtLinkLabel.SetMailAddress(const Value: String);
begin
  FMailAddress := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtLinkLabel.SetUrl(const Value: String);
begin
  FUrl := Value;
end;
{------------------------------------------------------------------------------}
end.

