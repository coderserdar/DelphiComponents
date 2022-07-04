unit OverbyteIcsReg;

{$I OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I OverbyteIcsSslDefs.inc}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  OverbyteIcsWSocket,
  OverbyteIcsDnsQuery,
  OverbyteIcsEmulVT,
  OverbyteIcsMimeDec,
  OverbyteIcsMultiProgressBar,
  OverbyteIcsTnCnx, OverbyteIcsTnEmulVT, OverbyteIcsTnScript,
  OverbyteIcsFtpCli, OverbyteIcsFtpSrv, OverbyteIcsMultipartFtpDownloader,
  OverbyteIcsHttpProt, OverbyteIcsHttpSrv, OverbyteIcsMultipartHttpDownloader,
  OverbyteIcsHttpAppServer,
  OverbyteIcsTimeList,
  OverbyteIcsCharsetComboBox,
  OverbyteIcsPop3Prot,
  OverbyteIcsSmtpProt,
  OverbyteIcsNntpCli,
  OverbyteIcsFingCli,
  OverbyteIcsPing
{$IFDEF USE_SSL}
  , OverbyteIcsSslSessionCache
  , OverbyteIcsSslThrdLock
{$ENDIF}
{$IFDEF VCL}
  , OverbyteIcsLogger
{$ENDIF}
{$IFDEF WIN32}
  , OverbyteIcsWSocketE
  , OverbyteIcsWSocketS
{$ENDIF}
  ;

procedure Register;

implementation

uses
{$IFDEF WIN32}
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin

  RegisterComponents('Overbyte ICS', [
    TWSocket,
    TDnsQuery, TEmulVT, TFingerCli, TPing,
    TMimeDecode, TMimeDecodeEx, TMimeDecodeW,
    TMultiProgressBar,
    TTimeList,
    THttpAppSrv,
    TTnCnx, TTnEmulVT, TTnScript,
    TFtpClient, TFtpServer, TMultipartFtpDownloader,
    THttpCli, THttpServer, TMultipartHttpDownloader,
    TPop3Cli, TSyncPop3Cli,
    TSmtpCli, TSyncSmtpCli, THtmlSmtpCli,
    TNntpCli, THtmlNntpCli,
    TIcsCharsetComboBox
{$IFDEF VCL}
    ,TIcsLogger
{$ENDIF}
  ]);

{$IFDEF USE_SSL}
  RegisterComponents('Overbyte ICS SSL', [
    TSslWSocket,
    TSslContext,
    TSslFtpClient, TSslFtpServer,
    TSslHttpCli, TSslHttpServer,
    TSslPop3Cli,
    TSslSmtpCli,
    TSslNntpCli,
    TSslAvlSessionCache,
    TSslStaticLock
  {$IFNDEF NO_DYNLOCK}
    ,TSslDynamicLock
  {$ENDIF}
  {$IFNDEF OPENSSL_NO_ENGINE}
    ,TSslEngine
  {$ENDIF}
  ]);
{$ENDIF}

{$IFDEF WIN32}
  RegisterComponents('Overbyte ICS', [
    TWSocketServer
  ]);

{$IFDEF USE_SSL}
  RegisterComponents('Overbyte ICS SSL', [
    TSslWSocketServer
  ]);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TWSocket, 'LineEnd',
    TWSocketLineEndProperty);
{$ENDIF}
end;

end.

