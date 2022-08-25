unit HTMLAbt;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, StdCtrls, Buttons, HtmlGlobals, Htmlview, ExtCtrls, HTMLUn2,
    OverbyteIcsWSocket;

{$INCLUDE htmlcons.inc}

type
    TAboutBox = class(TForm)
        BitBtn1 : TBitBtn;
        Panel1 : TPanel;
        Viewer : THTMLViewer;
    private
        { Private declarations }
    public
        { Public declarations }
        constructor CreateIt(Owner: TComponent; const ProgName, CompName: string); overload;
        constructor CreateIt(Owner: TComponent; const Message: ThtString); overload;
    end;

var
    AboutBox : TAboutBox;

implementation

{$R *.DFM}

function ConfigInfo : String;
begin
    Result := '<ul><li>compiled with ' +
{$IFDEF Ver150}
        'Delphi 7'
{$ENDIF}
{$IFDEF Ver170}
        'Delphi 2005'
{$ENDIF}
{$IFDEF Ver185}
        'Delphi 2007'
{$ELSE}
{$IFDEF Ver180}
        'Delphi 2006'
{$ENDIF}
{$ENDIF}
{$IFDEF Ver200}
        'Delphi 2009'
{$ENDIF}
{$IFDEF Ver210}
        'Delphi 2010'
{$ENDIF}
{$IFDEF Ver220}
        'Delphi XE'
{$ENDIF}
{$IFDEF Ver230}
        'Delphi XE2'
{$ENDIF}
{$IFDEF Ver240}
        'Delphi XE3'
{$ENDIF}
{$IFDEF Ver250}
        'Delphi XE4'
{$ENDIF}
{$IFDEF Ver260}
        'Delphi XE5'
{$ENDIF}
{$IFDEF Ver270}
        'Delphi XE6'
{$ENDIF}
{$IFDEF Ver280}
        'Delphi XE7'
{$ENDIF}
{$IFDEF Ver290}
        'Delphi XE8'
{$ENDIF}
{$IFDEF Ver300}
        'Delphi 10 Seattle'
{$ENDIF}
{$IFDEF Ver310}
        'Delphi 10.1 Berlin'
{$ENDIF}
{$IFDEF Ver320}
        'Delphi 10.2 Tokyo'
{$ENDIF}
{$IFDEF Ver330}
        'Delphi 10.3 Rio'
{$ENDIF}
{$IFDEF Ver340}
        'Delphi 10.4 Sydney'
{$ENDIF}
{$IFDEF Ver350}
        'Delphi 11 Alexandria'
{$ENDIF}
{$IFDEF LCL}
        'Lazarus ' + lcl_version
{$ENDIF}
        ;

{$ifdef win64}
  Result := Result + '<li>Compiled for Win64</li>';
{$endif}
{$ifdef win32}
  Result := Result + '<li>Compiled for Win32</li>';
{$endif}
{$ifdef wince}
  Result := Result + '<li>Compiled for WinCE</li>';
{$endif}
{$ifdef unix}
  Result := Result + '<li>Compiled for Unix';
  {$ifdef LCL}
    Result := Result + ' (' + LCLPlatformDirNames[WidgetSet.LCLPlatform] +')';
  {$endif}
  Result := Result + '</li>';
{$endif}

{$IFDEF UseTNT}
    Result := Result + '<li>Using TNT unicode controls.';
{$ELSE}
{$IFDEF UseElPack}
    Result := Result + '<li>Using ElPack unicode controls.';
{$ELSE}
{$IFDEF UNICODE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL unicode character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL unicode character controls.';
{$ENDIF}
{$ELSE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL single byte character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL single byte character controls.';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
    Result := Result + '<li>ICS  ' + Trim(OverbyteIcsWSocket.CopyRight);   // March 2018

    Result := Result + '</ul>';
end;

constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
var
  S: String;
begin
  inherited Create(Owner);
  inherited Loaded;
  Viewer.DefFontName := 'MS Sans Serif';
  Viewer.DefFontSize := 9;
  Viewer.DefFontColor := clNavy;
  S :='<body text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
    '<h3>Version '+ VersionNo +'</h3>'+
    '</center>'+
    ConfigInfo +
    '</body>';
  Viewer.LoadFromString(S);
end;


constructor TAboutBox.CreateIt(Owner: TComponent;
  const Message: ThtString);
begin
  inherited Create(Owner);
  inherited Loaded;
  if Owner is TCustomForm then
    Caption := TCustomForm(Owner).Caption;
  Viewer.DefFontName := 'Verdana';
  Viewer.DefFontSize := 12;
  Viewer.DefFontColor := clBlack;
  Viewer.LoadFromString('<body>' + Message + '</body>');
end;

end.
