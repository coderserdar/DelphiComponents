unit u_zconnection;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


{$I ..\extends.inc}

// Unit U_ZConnection
// Auto connexion ZEOS with inifile
// Autor : Matthieu GIROUX
// Just have to call the function fb_InitZConnection
// Licence GNU GPL

interface

uses
{$IFDEF FPC}
  LResources,
{$ELSE}
  JvExControls,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  zconnection,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  JvXPButtons, StdCtrls, IniFiles, JvXPCore;

{$IFDEF VERSIONS}
const
  gVer_zconnection : T_Version = ( Component : 'Connexion ZEOS' ; FileUnit : 'u_zconnection' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fenetre de connexion ZEOS si pas dans l''INI.' ;
                        			                 BugsStory : 'Version 0.0.5.0 : Fenetre avec les drivers et le codepage.' + #13#10
                                                         + 'Version 0.0.4.0 : Fenetre sans les drivers.';
                        			                 UnitType : 3 ;
                        			                 Major : 0 ; Minor : 0 ; Release : 5 ; Build : 0 );
{$ENDIF}

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

type

  { TF_ZConnectionWindow }

  TF_ZConnectionWindow = class(TForm)
    cbx_Protocol: TComboBox;
    Label7: TLabel;
    quit: TJvXPButton;
    Save: TJvXPButton;
    quitall: TJvXPButton;
    Test: TJvXPButton;
    ed_Base: TEdit;
    ed_Host: TEdit;
    ed_Password: TEdit;
    ed_User: TEdit;
    ed_Catalog: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ed_Collation: TEdit;
    lb_Collation: TLabel;
    procedure quitallClick(Sender: TObject);
    procedure quitClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure ed_HostEnter(Sender: TObject);
    procedure ed_BaseEnter(Sender: TObject);
    procedure ed_UserEnter(Sender: TObject);
    procedure ed_PasswordEnter(Sender: TObject);
    procedure ed_CatalogEnter(Sender: TObject);
    procedure ed_CollationEnter(Sender: TObject);
  private
    Connexion : TZConnection ;
    Inifile : TCustomInifile;
    { private declarations }
  public
    constructor Create ( AOwner : TComponent ); override;
    procedure DoShow; override;
    { public declarations }
  end; 

var
  F_ZConnectionWindow: TF_ZConnectionWindow = nil;
  gst_TestOk : String = 'Test OK' ;
  gst_TestBad : String = 'Error' ;
  gs_DataSectionIni : String = 'Database' ;
  gs_DataDriverIni : String = 'Driver' ;
  gs_DataBaseNameIni : String = 'Database Name' ;
  gs_DataUserNameIni : String = 'User Name' ;
  gs_DataHostIni : String = 'Host Name' ;
  gs_DataCatalogIni : String = 'Catalog' ;
  gs_DataPasswordIni : String = 'Password' ;
  gs_DataProtocolIni : String = 'Protocol' ;
  gs_DataCollationIni : String = 'Collation Encode' ;

procedure p_SetCaractersZEOSConnector(const azco_Connect : TZConnection ; const as_NonUtfChars : String );
procedure p_ShowZConnectionWindow ( const Connexion : TZConnection ; const Inifile : TCustomInifile );
function  fb_InitZConnection ( const Connexion : TZConnection ; const Inifile : TCustomInifile ; const Test : Boolean ) : String;
procedure p_InitZConnection ( const Connexion : TZConnection ; const Inifile : TCustomInifile ; const Test : Boolean );
function fb_TestZConnection ( const Connexion : TZConnection ; const lb_ShowMessage : Boolean ) : Boolean;
function fs_CollationEncode ( const Connexion : TZConnection ) : String;

implementation

uses fonctions_init,zdbcintfs, zclasses, Types, fonctions_db,
     fonctions_components;

{ TF_ZConnectionWindow }

// Test Mode
procedure TF_ZConnectionWindow.TestClick(Sender: TObject);
begin
  Connexion.Database     := ed_Base     .Text ;
  Connexion.Protocol     := cbx_Protocol.Text ;
  Connexion.HostName     := ed_Host     .Text ;
  Connexion.Password     := ed_Password .Text ;
  Connexion.User         := ed_User     .Text ;
  Connexion.Catalog      := ed_Catalog  .Text ;
  fb_TestZConnection ( Connexion, True );
end;

// Getting Drivers Names
constructor TF_ZConnectionWindow.Create(AOwner: TComponent);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  inherited Create(AOwner);
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      cbx_Protocol.Items.Append(Protocols[J]);
  End;
end;

procedure TF_ZConnectionWindow.DoShow;
begin
  With Connexion do
    Begin
      ed_Base     .Text := Database;
      cbx_Protocol.Text := Protocol;
      ed_Host     .Text := HostName;
      ed_Password .Text := Password;
      ed_User     .Text := User;
      ed_Catalog  .Text := Catalog;
    end;
  inherited DoShow;
end;


// Saving to IniFile
procedure TF_ZConnectionWindow.SaveClick(Sender: TObject);
begin
  if assigned ( IniFile )
    Then
      Begin
        IniFile.WriteString ( gs_DataSectionIni , gs_DataBaseNameIni , ed_Base     .Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataProtocolIni , cbx_Protocol.Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataHostIni     , ed_Host     .Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataPasswordIni , ed_Password .Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataUserNameIni , ed_User     .Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataCatalogIni  , ed_Catalog  .Text  );
        IniFile.WriteString ( gs_DataSectionIni , gs_DataCollationIni, ed_Collation.Text  );

        fb_iniWriteFile( Inifile, True );
      End;
  Close;
end;

// Quit application
procedure TF_ZConnectionWindow.quitallClick(Sender: TObject);
begin
  Application.Terminate;
end;

// Close Window
procedure TF_ZConnectionWindow.quitClick(Sender: TObject);
begin
  Close;
end;

// Procédures and functions

// Open connexion and erros
function fb_TestZConnection ( const Connexion : TZConnection ; const lb_ShowMessage : Boolean ) : Boolean;
Begin
  Result := False ;
  try
    Connexion.Connected:=True;
  Except
    on E: Exception do
      Begin
        if lb_ShowMessage Then
          ShowMessage ( gst_TestBad + ' : ' + #13#10 + E.Message );
        Exit ;
      End ;
  End ;
  if ( Connexion.Connected ) Then
    Begin
      Result := True ;
      if lb_ShowMessage Then
        ShowMessage ( gst_TestOk );
    End ;
End ;

// Init connexion with inifile
function fs_CollationEncode ( const Connexion : TZConnection ) : String;
var ls_Prop   : String ;
    li_i      ,
    li_equal  : Integer ;
Begin
  Result := 'utf8';
  for li_i := 0 to Connexion.Properties.Count - 1 do
    Begin
      ls_Prop := Connexion.Properties [ li_i ];
      li_equal := pos ( '=', ls_Prop);
      if  ( pos ( 'codepage', LowerCase(ls_Prop)) > 0 )
      and ( li_equal > 0 )
      and ( li_equal + 1 < length ( ls_prop ) )
       Then
         Begin
           Result := Copy ( ls_prop, li_equal +1, length ( ls_prop ));
         End;
    End;
End;
// Init connexion with inifile
function fb_InitZConnection ( const Connexion : TZConnection ; const Inifile : TCustomInifile ; const Test : Boolean ) : String;
Begin
  p_InitZConnection ( Connexion, Inifile, Test );
  Result := gs_DataHostIni      + ' = ' +  Connexion.HostName  + #13#10
          + gs_DataProtocolIni  + ' = ' +  Connexion.Protocol  + #13#10
          + gs_DataBaseNameIni  + ' = ' +  Connexion.Database  + #13#10
          + gs_DataUserNameIni  + ' = ' +  Connexion.User      + #13#10
          + gs_DataPasswordIni  + ' = ' +  Connexion.Password  + #13#10
          + gs_DataCatalogIni   + ' = ' +  Connexion.catalog   + #13#10
          + gs_DataCollationIni + ' = ' +  fs_CollationEncode ( Connexion )   + #13#10 ;
End ;

// Init connexion with inifile
procedure p_InitZConnection ( const Connexion : TZConnection ; const Inifile : TCustomInifile ; const Test : Boolean );
Begin
  if assigned ( Inifile )
  and assigned ( Connexion ) Then
    Begin
      Connexion.Database     := Inifile.ReadString ( gs_DataSectionIni, gs_DataBaseNameIni, '' ) ;
      Connexion.Protocol     := Inifile.ReadString ( gs_DataSectionIni, gs_DataProtocolIni , '' ) ;
      Connexion.HostName     := Inifile.ReadString ( gs_DataSectionIni, gs_DataHostIni    , '' ) ;
      Connexion.Password     := Inifile.ReadString ( gs_DataSectionIni, gs_DataPasswordIni, '' ) ;
      Connexion.User         := Inifile.ReadString ( gs_DataSectionIni, gs_DataUserNameIni, '' ) ;
      Connexion.catalog      := Inifile.ReadString ( gs_DataSectionIni, gs_DataCatalogIni    , '' ) ;
      p_SetCaractersZEOSConnector(Connexion, Inifile.ReadString ( gs_DataSectionIni, gs_DataCollationIni    , 'UTF8' ));
    End ;
  if ( Connexion.Database = '' )
  or not ( fb_TestZConnection ( Connexion, Test )) Then
    Begin
      p_ShowZConnectionWindow ( Connexion, Inifile );
    End ;
End ;

// Show The Window ( automatic )
procedure p_ShowZConnectionWindow ( const Connexion : TZConnection ; const Inifile : TCustomInifile );
Begin
  if not assigned ( F_ZConnectionWindow )
    Then
      Application.CreateForm ( TF_ZConnectionWindow, F_ZConnectionWindow );
  F_ZConnectionWindow.Connexion := Connexion;
  F_ZConnectionWindow.Inifile := Inifile;
  F_ZConnectionWindow.ShowModal ;
End ;

procedure TF_ZConnectionWindow.ed_HostEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );
end;

procedure TF_ZConnectionWindow.ed_BaseEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );

end;

procedure TF_ZConnectionWindow.ed_UserEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );

end;

procedure TF_ZConnectionWindow.ed_PasswordEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );

end;

procedure TF_ZConnectionWindow.ed_CatalogEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );

end;

procedure TF_ZConnectionWindow.ed_CollationEnter(Sender: TObject);
begin
  p_ComponentSelectAll ( Sender );

end;

procedure p_SetCaractersZEOSConnector(const azco_Connect : TZConnection ; const as_NonUtfChars : String );
begin
  azco_Connect.Properties.Clear;
  azco_Connect.Properties.Add('codepage='+as_NonUtfChars);
end;

initialization
{$IFDEF FPC}
  {$I u_zconnection.lrs}
{$ENDIF}
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_zconnection );
{$ENDIF}

end.

