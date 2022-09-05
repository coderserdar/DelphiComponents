{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ � um poderoso gerador de relat�rios dispon�vel como um pacote de componentes }
{ para Delphi. Em FortesReport, os relat�rios s�o constitu�dos por bandas que  }
{ t�m fun��es espec�ficas no fluxo de impress�o. Voc� definir agrupamentos     }
{ subn�veis e totais simplesmente pela rela��o hier�rquica entre as bandas.    }
{ Al�m disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright � 1999-2015 Fortes Inform�tica     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              M�rcio Martins                                  }
{                              R�gys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informa��es voc� pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descri��o...
******************************************************************************}
unit SVN_Class;

interface

uses
  SysUtils, Windows, Dialogs, Menus, Registry, ShellApi,
  Classes, Controls, Graphics, ImgList, ExtCtrls, ActnList,
  Comobj, ActiveX, Variants;

var
  TSVNTortoisePath: string;
  TSVNCollabNetPath: string;
  TTortoiseMergePath: string;

const
  SVN_OBJECT_NAME = 'SubWCRev.object';

type
  TWCInfo = record
    Revision: string;
    Author: string;
    Date: string;
    HasModifications: Boolean;
  end;

  TSVN_Class = class
  private class var
    FSVNInstalled: boolean;
    FWCInfo: TWCInfo;
  private
    class procedure SVNCollabNetExec(Params: String); static;
    class procedure SVNTortoiseExec( Params: String ); static;
  public
    class constructor Create;

    // executar programa externo e aguardar o termino
    class function  WinExecAndWait32(CmdLine: AnsiString; Visibility: Integer = SW_SHOW): DWORD; static;

    // M�todos que utilizam o tortoise
//    class function IsTortoiseInstalado: Boolean; static;
    class procedure SVNTortoise_CheckOut(const AUrl, APath: String;
      const AFecharAutomaticamente: Boolean); static;
    class procedure SVNTortoise_Update(const APath: String;
      const AFecharAutomaticamente: Boolean); static;

    // m�todos que utilizam o CollabNet que � o mesmo utilizado pelo delphi XE2
    class function IsCollabNetInstalado: Boolean; static;
    class procedure SVNCollabNet_Checkout(const AUrl, APath: String); static;
    class procedure SVNCollabNet_Update(const AUrl, APath: String); static;

    class function IsOLEObjectInstalled(const Name: string): boolean;
    class function IsSvnDir(const ADir: string): boolean;
    class function GetRevision(const ADir: string): Boolean;
    class property SVNInstalled: boolean read FSVNInstalled;
    class property WCInfo: TWCInfo read FWCInfo;
  end;

implementation

//******************************************************************************
//
//  Executar um aplicativo e aguardar o retorno do mesmo
//
//******************************************************************************

class function TSVN_Class.WinExecAndWait32(CmdLine: AnsiString; Visibility: Integer): DWORD;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, String(CmdLine));
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
           zAppName,               // pointer to command line string }
           nil,                    // pointer to process security attributes }
           nil,                    // pointer to thread security attributes }
           false,                  // handle inheritance flag }
           CREATE_NEW_CONSOLE or   // creation flags }
           NORMAL_PRIORITY_CLASS,
           nil,                    // pointer to new environment block }
           nil,                    // pointer to current directory name }
           StartupInfo,            // pointer to STARTUPINFO }
           ProcessInfo) then
  begin
    Result := 9; { pointer to PROCESS_INF }
  end
  else
  begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);

    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

//******************************************************************************
//
//  Setar paths dos aplicativos utilizados
//
//******************************************************************************

class constructor TSVN_Class.Create;
var
  Reg: TRegistry;
const
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_32KEY = $0200;

  function SetPathTortoise: Boolean;
  begin
    Result := Reg.OpenKeyReadOnly( '\SOFTWARE\TortoiseSVN' ) ;
    if Result then
    begin
      TSVNTortoisePath   := Reg.ReadString( 'ProcPath' );
      TTortoiseMergePath := Reg.ReadString( 'TMergePath' );
    end
    else
    begin
      TSVNTortoisePath   := '';
      TTortoiseMergePath := '';
    end;
  end;

  function SetPathCollabNet: Boolean;
  var
    CollabNetReg: String;
  begin
    Result := Reg.OpenKeyReadOnly( '\SOFTWARE\CollabNet\Subversion' ) ;
    if Result then
    begin
      CollabNetReg := Reg.ReadString( 'Client Version' );
      CollabNetReg := '\SOFTWARE\CollabNet\Subversion\' + CollabNetReg + '\Client';
      if Reg.OpenKeyReadOnly( CollabNetReg ) then
        TSVNCollabNetPath := Reg.ReadString( 'Install Location' ) + '\svn.exe';
    end
    else
    begin
      TSVNCollabNetPath := '';
    end;
  end;
begin
  FSVNInstalled := IsOLEObjectInstalled(SVN_OBJECT_NAME);

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // TORTOISE
    if not SetPathTortoise then
    begin
      //try 64 bit registry
      Reg.Access := Reg.Access or KEY_WOW64_64KEY;
      if not SetPathTortoise then
      begin
        //try WOW64 bit registry
        Reg.Access := Reg.Access or KEY_WOW64_32KEY;
        SetPathTortoise;
      end;
    end;

    // COLLABNET
    if not SetPathCollabNet then
    begin
      //try 64 bit registry
      Reg.Access := Reg.Access or KEY_WOW64_64KEY;
      if not SetPathCollabNet then
      begin
        //try WOW64 bit registry
        Reg.Access := Reg.Access or KEY_WOW64_32KEY;
        SetPathCollabNet;
      end;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

//******************************************************************************
//
//  TORTOISE
//
//******************************************************************************

class procedure TSVN_Class.SVNTortoiseExec( Params: string );
var
  CmdLine: AnsiString;
begin
  CmdLine := AnsiString(TSVNTortoisePath + ' ' + Params );
  WinExecAndWait32(CmdLine, SW_SHOW);
end;

//class function TSVN_Class.IsTortoiseInstalado: Boolean;
//begin
//  Result := FileExists(TSVNTortoisePath);
//end;

class procedure TSVN_Class.SVNTortoise_CheckOut(const AUrl, APath: String;
  const AFecharAutomaticamente: Boolean);
var
  Comando: String;
begin
  Comando := '/command:checkout' +
               ' /blockpathadjustments ' +
               ' /path:' + AnsiQuotedStr( APath, '"' ) +
               ' /url:'  + AnsiQuotedStr( AUrl, '"' );

  if AFecharAutomaticamente then
    Comando := Comando + ' /closeonend:3'
  else
    Comando := Comando + ' /closeonend:0';

  TSVN_Class.SVNTortoiseExec( Comando );
end;

class procedure TSVN_Class.SVNTortoise_Update(const APath: String;
  const AFecharAutomaticamente: Boolean);
var
  Comando: String;
begin
  Comando := '/command:update' +
               ' /notempfile' +
               ' /path:' + AnsiQuotedStr( APath, '"' );

  if AFecharAutomaticamente then
    Comando := Comando + ' /closeonend:3'
  else
    Comando := Comando + ' /closeonend:0';

  TSVN_Class.SVNTortoiseExec( Comando );
end;

//******************************************************************************
//
//  TORTOISE
//
//******************************************************************************

class procedure TSVN_Class.SVNCollabNetExec( Params: string );
var
  CmdLine: AnsiString;
begin
  CmdLine := AnsiString(TSVNCollabNetPath + ' ' + Params );
  TSVN_Class.WinExecAndWait32(CmdLine, SW_SHOW);
end;

class function TSVN_Class.IsCollabNetInstalado: Boolean;
begin
  Result := FileExists(TSVNCollabNetPath);
end;

class procedure TSVN_Class.SVNCollabNet_Checkout(const AUrl, APath: String);
begin
  TSVN_Class.SVNCollabNetExec(
    Format('co %s %s', [
      AnsiQuotedStr( AUrl, '"' ),
      AnsiQuotedStr( APath, '"' )
    ])
  );
end;

class procedure TSVN_Class.SVNCollabNet_Update(const AUrl, APath: String);
begin
  //TSVN_Class.SVNCollabNetExec( 'info ' + AnsiQuotedStr( AUrl, '"' ) );
  TSVN_Class.SVNCollabNetExec(
    'up ' + AnsiQuotedStr( APath, '"' )
  );
end;


//-- Capturar informa��es da �ltima revis�o
class function TSVN_Class.IsOLEObjectInstalled(const Name: string): boolean;
var
  ClassID: TCLSID;
begin
  Result := CLSIDFromProgID(PWideChar(WideString(Name)), ClassID) = S_OK;
end;

class function TSVN_Class.GetRevision(const ADir: string): Boolean;
var
  Svn: OLEVariant;
begin
  Result := False;

  FWCInfo.Revision := '';
  FWCInfo.Date     := '';
  FWCInfo.Author   := '';

  FWCInfo.HasModifications := False;

  if (not FSvnInstalled) or (ADir = '') then
    Exit;

  Svn := CreateOLEObject(SVN_OBJECT_NAME);
  Svn.GetWCInfo(ADir, True, True);

//  if not Svn.IsSvnItem then
//    Exit;

  FWCInfo.Revision := Svn.Revision;
  FWCInfo.Date     := Svn.Date;
  FWCInfo.Author   := Svn.Author;

  FWCInfo.HasModifications := Svn.HasModifications;

  Svn := Unassigned;
  Result := True;
end;

class function TSVN_Class.IsSvnDir(const ADir: string): boolean;
var
  Svn: OLEVariant;
begin
  Result := False;
  if (not FSVNInstalled) or (ADir = '') then
    Exit;

  Svn := CreateOleObject(SVN_OBJECT_NAME);
  Svn.GetWCInfo(ADir, True, True);

  Result := Svn.IsSvnItem;

  Svn := Unassigned;
end;


end.
