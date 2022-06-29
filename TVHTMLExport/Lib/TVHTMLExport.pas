{
  Name        : TTVHTMLExport for Delphi
  Author      : Oliver Killguss
  e-Mail      : killguss@web.de
  Version     : 1.20
  Date        : 25.10.2001


  Description
  ===========
  TTVHTMLExport exports tree's from the Delphi's TTreeView into a HTML file. Now it's possible to
  auto generate links within nodes and produce title, header and footer to output file. TTVHTMLExport
  develop it's own images for the nodes appearance but you can also select your own images for
  development. All used images will copied to destination path of generated file. Full compiled
  updated demo with sources also included.

  This Component is FREEWARE with full source code. I still hold the copyright.
  If you have any ideas for improvement or bug reports, don't hesitate to e-mail me.
  If you use this component in a commercial product so don't forget to give me some credits


  History:
  ========
  1.20 - Event OnParseURL added. Demo Improved
  1.12 - Style Sheet support. Remember that the <style type="text/css"> and </style> tag is
         automatically added !
  1.11 - Properties changed for header, title and footer
  1.10 - Changes on image handling, now includes owner bitmaps and can generate
         links within nodes
  1.00 - Initial Release
}
unit TVHTMLExport;

interface

uses
  {$ifdef LINUX}
  Types, Variants, QGraphics, QControls, QForms, QDialogs, QComCtrls,
  {$ELSE}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ComCtrls, ShellAPI,
  {$ENDIF}
  Classes, SysUtils;
type
  THeaderSize = (H1,H2,H3,H4,H5,H6);

  TTVHTMLEXPORTParseURL = procedure(URL: String; var UseURL: Boolean) of object;

{---- TTitle ----}
  TTitle = class(TPersistent)
  private
    FSize: THeaderSize;
    FText: String;
    FShow: Boolean;
  published
    property Text: String read FText write FText;
    property Size: THeaderSize read FSize write FSize;
    property Show: Boolean read FShow write FShow;
  end;

{---- TFooter ----}
  TFooter = class(TPersistent)
  private
    FShow: Boolean;
    FBold: Boolean;
    FLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Lines: TStringList read FLines write FLines;
    property Show: Boolean read FShow write FShow;
    property Bold: Boolean read FBold write FBold;
  end;

{---- THeader ----}
  THeader = class(TPersistent)
  private
    FShow: Boolean;
    FBold: Boolean;
    FLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Lines: TStringList read FLines write FLines;
    property Show: Boolean read FShow write FShow;
    property Bold: Boolean read FBold write FBold;
  end;

{---- TTVHTMLExport ----}
  TTVHTMLExport = class(TComponent)
  private
    FShowFolderImage,
    FUseInternalImages,
    FCreateURL,
    FShowBorder         : Boolean;
    FIndent             : Integer;
    FTitle              : TTitle;
    FFolderOpenImage,
    FFolderImage        : TBitmap;
    FHeader             : THeader;
    FFooter             : TFooter;
    FStyleSheet         : TStringList;
    FOnParseURL         : TTVHTMLEXPORTParseURL;
    function GetHighestLevel(Tree: TTreeView): Integer;
    procedure setFolderOpenImage(Value: TBitmap);
    procedure setFolderImage(Value: TBitmap);
    procedure setUseInternalImages(Value: Boolean);
    function CheckURL(value: string): String;
  public
    procedure SaveToHTML(FileName: String; Tree: TTreeView);
    {$IFDEF WIN32}
    procedure Show(Tree: TTreeView);
    {$ENDIF}
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Indent: Integer read FIndent write FIndent;
    property ShowFolderImage: Boolean read FShowFolderImage write FShowFolderImage;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;

    property Title: TTitle read FTitle write FTitle;
    property Footer: TFooter read FFooter write FFooter;
    property Header: THeader read FHeader write FHeader;

    property FolderImage: TBitmap read FFolderImage write setFolderImage;
    property FolderOpenImage: TBitmap read FFolderOpenImage write setFolderOpenImage;
    property UseInternalImages: Boolean read FUseInternalImages write setUseInternalImages;
    property CreateURL: Boolean read FCreateURL write FCreateURL;
    property StyleSheet: TStringList read FStyleSheet write FStyleSheet;
    property OnParseURL: TTVHTMLEXPORTParseURL read FOnParseURL write FOnParseURL;
  end;

procedure Register;

implementation

const Version = 'TreeView HTML Export V1.12';
{$R *.res}

procedure Register;
begin
  RegisterComponents('DDM', [TTVHTMLExport]);
end;

{---- THeader ----}

constructor THeader.Create;
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor THeader.Destroy;
begin
  inherited;
  FLines.Free;
end;

{---- TFooter ----}

constructor TFooter.Create;
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor TFooter.Destroy;
begin
  inherited;
  FLines.Free;
end;

{---- TTVHTMLExport ----}

constructor TTVHTMLExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFolderImage       := TBitmap.Create;
  FFolderOpenImage   := TBitmap.Create;
  Indent             := 2;
  ShowFolderImage    := true;
  ShowBorder         := false;
  UseInternalImages  := true;
  FHeader            := THeader.Create;
  FFooter            := TFooter.Create;
  FTitle             := TTitle.Create;
  FTitle.Text        := '';
  FTitle.Show        := true;
  FHeader.Show       := true;
  FHeader.Lines.Clear;
  FFooter.Show       := true;
  FFooter.Lines.Clear;
  FStyleSheet        := TStringList.Create;
end;

destructor TTVHTMLExport.Destroy;
begin
  FFolderImage.FreeImage;
  FFolderOpenImage.FreeImage;
  inherited;
end;

function TTVHTMLExport.GetHighestLevel(Tree: TTreeView): Integer;
var
  ANode: TTreeNode;
  level : Integer;
  i: Integer;
  function max(a, b: integer): integer;
  begin
    if a > b then result := a else result := b;
  end;
begin
  level := 0;
  for i := 0 to Tree.Items.Count -1 do
  begin
    ANode := Tree.Items[i];
    if ANode <> nil then
      Level := max(ANode.Level, Level);
  end;
  result := level + 1;
end;

procedure TTVHTMLExport.SaveToHTML(FileName: String; Tree: TTreeView);
var
  ANode: TTreeNode;
  level : integer;
  hLevel : Integer;
  i: integer;
  f: TextFile;
  Bitmap : TBitmap;
  hs : String;

  procedure CopyImages(path: String);
  begin
    if UseInternalImages then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.LoadFromResourceName(HInstance,'FOLDER');
      Bitmap.SaveToFile(path+'folder.bmp');
      Bitmap.LoadFromResourceName(HInstance,'OPENFOLDER');
      Bitmap.SaveToFile(path+'openfolder.bmp');
    end
    else
    begin
      FolderImage.SaveToFile(path+'folder.bmp');
      FolderOpenImage.SaveToFile(path+'openfolder.bmp');
    end;
  end;
begin
  if Tree.items.Count > 0 then
  begin
    try
    hLevel := GetHighestLevel(Tree);
    ANode := tree.Items[0];
    if ANode = nil then exit;
    assignfile(f,filename);
    rewrite(f);
    writeln(f,'<html>');
    writeln(f,'<head>');
    writeln(f,'<title>' + title.text + '</title>');
    writeln(f,'<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">');
    writeln(f,'<meta name="GENERATOR" content="'+version+'">');
    writeln(f,'<style type="text/css">');
    writeln(f,'  <!--');

    for i := 0 to StyleSheet.Count -1 do
      writeln(f,'    ' + StyleSheet[i]);
    writeln(f,'  //-->');
    writeln(f,'</style>');

    writeln(f,'</head>');
    writeln(f,'<body>');
    case Title.Size of
      h1 : hs := 'H1';
      h2 : hs := 'H2';
      h3 : hs := 'H3';
      h4 : hs := 'H4';
      h5 : hs := 'H5';
      h6 : hs := 'H6';
    end;

    if (Title.Show) and (Title.text <> '') then
      writeln(f,'<'+hs+'>' + title.text + '</'+hs+'>');

    if Header.Bold then
      write(f,'<b>');
    if (Header.Show) and (Header.Lines.Count <> 0) then
    begin
      write(f,'<p>');
      for i := 0 to Header.lines.count -1 do
        writeln(f,header.lines[i]);
      writeln(f,'</p>');
    end;
    if Header.Bold then
      write(f,'</b>');
    write(f,'<table border=');
    if FShowBorder then
      write (f,'"1"')
    else
      write (f,'"0"');
    writeln(f,' CELLSPACING=1 CELLPADDING=4 WIDTH="100%" align=center>');
    write(f,'  <TR><TD VALIGN="TOP" COLSPAN='+intToStr(hLevel)+'><P>');
    if FShowFolderImage then
    begin
      if ANode.HasChildren then
        write(f,'<IMG SRC="openfolder.bmp">')
      else
        write(f,'<IMG SRC="folder.bmp">')
    end;
    writeln(f,ANode.text+'</TD></TR>');
    ANode := ANode.GetNext;
    while ANode <> nil do
    begin
      writeln(f,'<TR>');
      level := ANode.Level - 1;
      for i := 0 to level do
          writeln(f,'<TD WIDTH="'+inttostr(Indent)+'%" VALIGN="TOP">&nbsp;</TD>');
      write(f,'<TD WIDTH="'+inttoStr(100 - (Indent * Indent))+'%" VALIGN="TOP" colspan='+intToStr(hLevel - ANode.Level)+'><P>');
      if FShowFolderImage then
      begin
        if ANode.HasChildren then
          write(f,'<IMG SRC="openfolder.bmp">')
        else
          write(f,'<IMG SRC="folder.bmp">')
      end;
      writeln(f,CheckURL(ANode.text)+'</TD></TR>');
      ANode := ANode.GetNext;
    end;
    writeln(f,'</TABLE>');

    if Footer.Bold then
      write(f,'<b>');
    if (Footer.Show) and (Footer.Lines.Count <> 0) then
    begin
      write(f,'<p>');
      for i := 0 to Footer.lines.count -1 do
        writeln(f,Footer.lines[i]);
      writeln(f,'</p>');
    end;
    if Footer.Bold then
      write(f,'<b>');

    writeln(f,'</BODY>');
    writeln(f,'</HTML>');
    CopyImages(ExtractFilePath(Filename));
    finally
       Closefile(f);
    end;
  end;
end;

{$IFDEF WIN32}
procedure TTVHTMLExport.Show(Tree: TTreeView);
begin
  try
    saveToHtml('c:\tmp.html',Tree);
    ShellExecute(GetDesktopWindow(), 'open', PChar('c:\tmp.html'), nil, nil, SW_SHOWNORMAL);
  except
  end;
end;
{$ENDIF}

procedure TTVHTMLExport.setFolderOpenImage(Value: TBitmap);
begin
  FFolderOpenImage.Assign(Value);
end;

procedure TTVHTMLExport.setFolderImage(Value: TBitmap);
begin
  FFolderImage.Assign(Value);
end;

procedure TTVHTMLExport.setUseInternalImages(Value: Boolean);
begin
  if (Value = False) and ((FFolderOpenImage.Empty) or (FFolderImage.Empty)) then
  begin
    ShowMessage('To set this value to false you have to import new Bitmaps first !');
    exit;
  end;
  FUseInternalImages := Value;
end;

function TTVHTMLExport.CheckURL(value: string): String;
const
  URLType : array[0..8] of String = ('FILE://','FTP://','HTTP://','HTTPS://','MAILTO:','NEWS:',
                                     'TELNET:','WAIS:', 'WWW.');
var
  p, i : Integer;
  link, s, s2, prelink: String;
  canUse: Boolean;
begin
  canUse := true;
  result := value;
  if CreateURL = false then exit;
  for i := 0 to 8 do
  begin
    p := pos(URLType[i],uppercase(value));
    if p > 0 then { now create link }
    begin
      link := Copy(value, 0 , p - 1);
      s := Copy(value, p, length(value) - p + 1);
      p := pos(' ', s);
      if p > 0 then
        s2 := Copy(s, 0, p - 1)
      else
        s2 := s;
      if i = 8 then
        prelink := 'http://'
      else
        prelink := '';
//       if pos('@',s2) > 0 then

      link := link + '<a href="' + prelink + s2 + '">' + s2 + '</a>';
      if p > 0 then link := link + copy(s, p, length(s) - p + 1);
      if Assigned(FOnParseURL) then
        FOnParseURL(s2, CanUse);
      if CanUse then
        result := link { this is the complete url string }
      else
        result := value;
      exit;
    end;
  end;
end;

end.
