{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtYouTubeVideoPlayer                           }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTYouTubeVideoPlayer;

interface
uses
   Classes
  ,ExtCtrls
  ,Controls
  ,SHDocVw
  ;

type
{------------------------------------------------------------------------------}
  TgtYouTubeVideoPlayer = class(TWinControl)
  private
    FEmbeddedCode: string;
    procedure SetEmbeddedCode(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    FWebBrowser : TWebBrowser;
    procedure CreateWnd;override;
    procedure LoadHTMLToBrowser;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure LoadVideo;
  published
    { Published declarations}
    property EmbeddedCode    : string       read FEmbeddedCode    write SetEmbeddedCode;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   SysUtils
  ,Forms
  ,OleCtrls
  ,ActiveX
//  ,MSHTML_TLB
  ;


const
  HTML_CODE =
'<html>                                                                                                                       '#13+
'<head>                                                                                                                       '#13+
'<title>%s</title>                                                                                                            '#13+
'</head>                                                                                                                      '#13+
'<body>                                                                                                                       '#13+
'%s                                                                                                                           '#13+
'</body>                                                                                                                      '#13+
'</html>                                                                                                                      '#13+
'';
{------------------------------------------------------------------------------}

{ TgtYouTubeVideoPlayer }
{------------------------------------------------------------------------------}
constructor TgtYouTubeVideoPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    FWebBrowser       := TWebBrowser.Create(Self);
  Self.Width        := 475;
  Self.Height       := 394;
end;
{------------------------------------------------------------------------------}
destructor TgtYouTubeVideoPlayer.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtYouTubeVideoPlayer.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FWebBrowser.ParentWindow := Self.Handle;
    FWebBrowser.Visible      := True;
    FWebBrowser.Align        := alClient;
    FWebBrowser.Width        := Self.Width;
    FWebBrowser.Height       := Self.Height;
  end;
end;
{------------------------------------------------------------------------------}
{ Taken from http://delphi.about.com/cs/adptips2004/a/bltip0104_4.htm }
procedure TgtYouTubeVideoPlayer.LoadHTMLToBrowser;
var
   sl : TStringList;
   ms : TMemoryStream;
begin
   FWebBrowser.Navigate('about:blank') ;
   while FWebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;

   if Assigned(FWebBrowser.Document) then
   begin
     sl := TStringList.Create;
     try
       ms := TMemoryStream.Create;
       try
         sl.Text := Format(HTML_CODE,['',FEmbeddedCode]);
         sl.SaveToStream(ms) ;
         ms.Seek(0, 0);
         (FWebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms));
       finally
         ms.Free;
       end;
     finally
      sl.Free;
     end;
   end;
end;
{------------------------------------------------------------------------------}
procedure TgtYouTubeVideoPlayer.LoadVideo;
begin
  LoadHTMLToBrowser;
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
procedure TgtYouTubeVideoPlayer.SetEmbeddedCode(const Value: string);
begin
  FEmbeddedCode := Value;
  if not (csDesigning in ComponentState) then
    LoadVideo;
end;
{------------------------------------------------------------------------------}

end.
