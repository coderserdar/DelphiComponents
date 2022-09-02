{*********************************************************}
{* FlashFiler: TFFEngineManager Expert                   *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffclexpt;

interface

uses
  Windows,
  ExptIntf;

type
  { The TFFEngineManagerWizard represents a Delphi expert that will
    create a new TFFEngineManager module with all the appropriate
    components set up, and appropriate methods overriden. The
    Expert is designed to prompt the user for the specific set
    of protocols that the server will support. This wizard is
    compatible with Delphi 3 - Delphi 5. }

  TFFEngineManagerWizard = class(TIExpert)
    public
      procedure Execute; override;
        { Create a new TFFEngineManager }
      function GetAuthor : string; override;
        { Return the Company Name }
      function GetComment : string; override;
        { Return the long description of this expert }
      function GetGlyph : HICON; override;
        { Return the icon to use for the this wizard }
      function GetIDString : string; override;
        { Return a Unique identifier for this expert }
      function GetMenuText : string; override;
        { Return an empty string, since we don't need a menu entry }
      function GetName : string; override;
        { Return the name of the wizard }
      function GetPage : string; override;
        { Return the default object repository page for the wizard }
      function GetState : TExpertState; override;
        { Return the expert state }
      function GetStyle : TExpertStyle; override;
        { Return the expert style }
  end;

implementation

uses
  Dialogs, Classes, Controls,  Forms, SysUtils,
  {Expert specific units}
  Proxies,
{$WARNINGS OFF}
  ToolIntf,
{$WARNINGS ON}
  IStreams,
  {FlashFiler Units}
  ffclexps, { The protocol selection dialog }
  ffllbase,
  ffllcomm,
  fflllgcy,
  ffllprot,
  fflllog,
  ffllthrd,
  ffsqleng,
  ffsrcmd,
  ffsreng,
  ffsrsec;

{ The TTextStream serves as a convienient method to add lines of
  text to a stream. This class is used to build the source code
  for the TFFEngineManager }
type
  TTextStream = class(TStringStream)
    public
      procedure WriteLn(const Str : string);
        { Add a line of text to a stream}
      procedure FormatLn(const Fmt : string; Args : array of const);
        { Format, then add a line of text to a stream }
      procedure NewLine;
        { Add and empty line of text to a stream }
  end;

{=== TTextStream ==========================================}
procedure TTextStream.NewLine;
begin
  WriteString(#13#10);
end;
{-------}
procedure TTextStream.WriteLn(const Str : string);
begin
  WriteString(Str);
  NewLine;
end;
{-------}
procedure TTextStream.FormatLn(const Fmt : string; Args : array of const);
begin
  WriteLn(Format(Fmt, Args));
end;


{===== TFFEngineManager Expert Implementation =================================}
{ constants specific to the implementation of the expert }
const
  CICON          = 'TFFENGINEMANAGERWIZARD';
  CBaseClassName = 'TffBaseEngineManager';
  CFormName      = 'ffEngineManager';

type
  { A set type used to store the selected protocols the TFFEngineManager
    will support }
  TFFProtocols = set of TFFProtocolType;

type
  { A descendent of TFFThreadPool that we can use to get access to the
    SkipInitial property. This class is only used to typecast against
    an actual TFFThreadPool. The SkipInitial property must be set to
    true with modifying the InitialCount property of a thread pool while
    creating the ProxyModule, since the ComponentState will not include
    csDesigning. If this is not set correctly the Delphi IDE will lock
    up tight! }
    
  THackedFFThreadPool = class(TFFThreadPool)
    public
      property SkipInitial;
  end;

  THackedFFBaseCommandHandler = class(TffBaseCommandHandler)         {NEW !!.01}
    public
      property SkipInitial;
  end;

{ Create the Module Proxy that will be used to stream the persistent
  data to a DFM file}
function CreateModuleProxy(ModuleName : string; aProtocols : TFFProtocols) : TDataModule;
const
  { Constants used for the proper alignment of controls. }
  CLeftStart   = 40;
  CTopStart    = 8;
  CHorSpacing  = 112;
  CVertSpacing = 56;

var
  DesignRect : TRect;                   { The default module position and size }
  EventLog   : TffEventLog;
  SEng       : TFFServerEngine;
  SQLEng     : TffSqlEngine;
  CmdH       : TFFServerCommandHandler;
  Transport  : TFFLegacyTransport;
  ThreadPool : TffThreadPool;
  SecMon     : TffSecurityMonitor;
  Position   : LongRec;                 { Temp var to store the position of a
                                          a non-visual component }
  NextLeft   : Integer;                 { Used to store the left position of
                                          a TFFLegacyTransport component. }
begin
  Result := TDataModule.Create(nil);
  try
    { Change Result to a proxy class}
    CreateSubClass(Result, ModuleName, TDataModule);
    with Result do begin
      { Set the properties for the module }
      Name := CFormName;
      DesignRect := ToolServices.GetFormBounds(btCustomModule);
      DesignOffset := DesignRect.TopLeft;
    end;

    { Create the event log. }
    EventLog := TffEventLog.Create(Result);
    { Set the properties for the event log. }
    with EventLog do begin
      Name := 'EventLog';
      Enabled := True;
      FileName := 'FFServer.log';
    end;
    { Since TComponent doesn't publish top and left properties, we have no
      easy access to arrange non-visual components on the data module. Despite
      this we can type case TComponent.DesignInfo as a LongRec. In this
      scenario LongRec.Lo becomes Left, and LongRec.Hi becomes Top. This is not
      documented anywhere but the source for TComponent, however tests show
      that it works reliably in all versions of Delphi. }
    Position := LongRec(EventLog.DesignInfo);
    Position.Lo := CLeftStart;
    Position.Hi := CTopStart;
    EventLog.DesignInfo := LongInt(Position);

    { Create the server engine component. The owner must be the proxy object! }
    SEng := TFFServerEngine.Create(Result);
    { Set the properties for the server engine }
    SEng.Name := 'ServerEngine';
    SEng.ConfigDir := '';                                              {!!.06}
    Position := LongRec(SEng.DesignInfo);
    Position.Lo := CLeftStart + CHorSpacing;
    Position.Hi := CTopStart;
    SEng.DesignInfo := LongInt(Position);
    SEng.EventLog := EventLog;
    SEng.CollectGarbage := True;

    { Create the SQL engine }
    SQLEng := TffSqlEngine.Create(Result);
    SQLEng.Name := 'SQLEngine';
    Position := LongRec(SQLEng.DesignInfo);
    Position.Lo := CLeftStart + (CHorSpacing * 2);
    Position.Hi := CTopStart;
    SQLEng.DesignInfo := LongInt(Position);
    SQLEng.EventLog := EventLog;
    SQLEng.EventLogEnabled := False;

    { Attach the server engine to the SQL engine. }
    SEng.SQLEngine := SQLEng;

    { Create the command handler }
    CmdH := TFFServerCommandHandler.Create(Result);
    { Set the properties for the command handler }
    CmdH.Name := 'CommandHandler';
    Position := LongRec(CmdH.DesignInfo);
    Position.Lo := CLeftStart + (CHorSpacing * 3);
    Position.HI := CTopStart;
    CmdH.DesignInfo := LongInt(Position);
    CmdH.EventLog := EventLog;
    CmdH.EventLogEnabled := False;
    CmdH.ServerEngine := SEng;
    THackedFFBaseCommandHandler(CmdH).SkipInitial := True;         {BEGIN !!.01}
    CmdH.EngineManager := TffBaseEngineManager(CmdH.Owner);
    { Skip intitial is not reverted to False. If it was the command handler
      would raise an AV when destroyed }                             {END !!.01}

    { Create the security monitor }
    SecMon := TffSecurityMonitor.Create(Result);
    { Set the properties for the command handler }
    SecMon.Name := 'SecurityMonitor';
    Position := LongRec(SecMon.DesignInfo);
    Position.Lo := CLeftStart + (CHorSpacing * 4);
    Position.Hi := CTopStart;
    SecMon.DesignInfo := Longint(Position);
    SecMon.ServerEngine := SEng;

    NextLeft := CLeftStart;

    { Create the thread pool }
    ThreadPool := TFFThreadPool.Create(Result);
    { Set the properties for the thread pool }
    ThreadPool.Name := 'ThreadPool';
    ThreadPool.EventLog := EventLog;
    ThreadPool.EventLogEnabled := false;
    { We need to keep the ThreadPool from starting the InitialCount threads.
      To do this we must set SkipInitial to True. SkipInitial is a protected
      method since we don't want users inadvertantly setting the property. To
      get around normal visibility rules we declare a THackedFFThreadPool class
      to promote the SkipInitial property to public. Then, as the code below
      shows we can typecast ThreadPool as the hacked class to set the property. }
    THackedFFThreadPool(ThreadPool).SkipInitial := True;
    try
      ThreadPool.InitialCount := 5;  { Arbitary number of threads. }

      ThreadPool.MaxCount := 256;
    finally
      THackedFFThreadPool(ThreadPool).SkipInitial := False;
    end;
    Position := LongRec(ThreadPool.DesignInfo);
    Position.Lo := NextLeft;
    inc(NextLeft, CHorSpacing);
    Position.HI := CTopStart + CVertSpacing;
    ThreadPool.DesignInfo := LongInt(Position);

    { Set the NextLeft variable. This variable will be assigned to the "left"
      property of the control. Then incremented by CHorSpacing. This is
      necessary to give the transport components a consistent alignment since
      the actual transports created are decided by the developer when the
      expert starts. }
    if ptSingleUser in aProtocols then begin
      { Create a transport with the SingleExe protocol selected. }
      Transport := TFFLegacyTransport.Create(Result);
      Transport.Name := 'SUPTransport';

      { The transport is ultimately associated with the server. This means that
        the transport must listen for requests. }
      Transport.Mode := fftmListen;
      Transport.Protocol := ptSingleUser;
      Transport.RespondToBroadcasts := True;

      { If multiple transports use the same LogFile, problems will occur.
        We set the property here for completeness.}
      Transport.EventLog := EventLog;
      Transport.EventLogEnabled := false;
      Transport.EventLogOptions := [fftpLogErrors];
      Transport.CommandHandler := CmdH;
      Transport.ThreadPool := ThreadPool;
      Transport.Enabled := True;
      Position := LongRec(Transport.DesignInfo);
      Position.Lo := NextLeft;
      Position.HI := CTopStart + CVertSpacing;
      Inc(NextLeft, CHorSpacing);
      Transport.DesignInfo := LongInt(Position);
    end;

    if ptIPXSPX in aProtocols then begin
      Transport := TFFLegacyTransport.Create(Result);
      Transport.Name := 'IPXSPXTransport';
      Transport.Mode := fftmListen;
      Transport.Protocol := ptIPXSPX;
      Transport.RespondToBroadcasts := True;
      Transport.EventLog := EventLog;
      Transport.EventLogEnabled := false;
      Transport.EventLogOptions := [fftpLogErrors];
      Transport.CommandHandler := CmdH;
      Transport.ThreadPool := ThreadPool;
      Transport.Enabled := True;
      Position := LongRec(Transport.DesignInfo);
      Position.Lo := NextLeft;
      Position.HI := CTopStart + CVertSpacing;
      Inc(NextLeft, CHorSpacing);
      Transport.DesignInfo := LongInt(Position);
    end;

    if ptTCPIP in aProtocols then begin
      Transport := TFFLegacyTransport.Create(Result);
      Transport.Name := 'TCPIPTransport';
      Transport.Mode := fftmListen;
      Transport.Protocol := ptTCPIP;
      Transport.RespondToBroadcasts := True;
      Transport.EventLog := EventLog;
      Transport.EventLogEnabled := false;
      Transport.EventLogOptions := [fftpLogErrors];
      Transport.CommandHandler := CmdH;
      Transport.ThreadPool := ThreadPool;
      Transport.Enabled := True;
      Position := LongRec(Transport.DesignInfo);
      Position.Lo := NextLeft;
      Position.HI := CTopStart + CVertSpacing;
      Transport.DesignInfo := LongInt(Position);
    end;

    with Result do
      { Set the size of the module. This could be dynamic, but 200x100
        represents the size just fine. }
      DesignSize := Point(DesignOffset.X + 400,
                          DesignOffset.Y + 100);
  except
    { Delphi is normally responsible for freeing the proxy class. Since
      an error occured, we need to take care of it locally. }
    Result.Free;
    raise;
  end;
end;
{-------}
function AdaptStream(Stream : TStream) : TIStreamAdapter;
begin
  try
    {$IFDEF DCC4OrLater}
    Result := TIStreamAdapter.Create(Stream, soOwned);
    {$ELSE}
    Result := TIStreamAdapter.Create(Stream, True);
    {$ENDIF}
  except
    Stream.Free;
    raise;
  end;
end;
{-------}
function CreateModuleStream(ModuleName : string; aProtocols : TFFProtocols) : TStream;
{ Build the DFM file for the module }
var
  Module : TDataModule;
begin
  Result := TMemoryStream.Create;
  try
    Module := CreateModuleProxy(ModuleName, aProtocols);
    try
      Result.WriteDescendentRes(Module.ClassName, Module, nil);
      Result.Position := 0;
    finally
      Module.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{Begin !!.06}
{$IFNDEF IsDelphi}
{-------}
function CreateHdrStream(UnitName, ModuleName : string; aProtocols : TFFProtocols): TTextStream;
var
  HeaderDate : string;
begin
  Result := TTextStream.Create('');
  with Result do
    try
      WriteLn('//---------------------------------------------------------');
      WriteLn('// FlashFiler: Engine manager');
      HeaderDate := DateToStr(Now);
      FormatLn('// Generated on %s with Release %5.4f',
               [HeaderDate, ffVersionNumber / 10000.0]);
      WriteLn('//---------------------------------------------------------');
      NewLine;
      WriteLn('//---------------------------------------------------------------------------');
      NewLine;
      FormatLn('#ifndef %sH', [UnitName]);
      FormatLn('#define %sH', [UnitName]);
      WriteLn('//---------------------------------------------------------------------------');
      WriteLn('#include <Classes.hpp>');
      WriteLn('#include <Controls.hpp>');
      WriteLn('#include <StdCtrls.hpp>');
      WriteLn('#include <Forms.hpp>');
      WriteLn('#include "ffllbase.hpp"');
      WriteLn('#include "ffllcomm.hpp"');
      WriteLn('#include "ffllcomp.hpp"');
      WriteLn('#include "fflleng.hpp"');
      WriteLn('#include "fflllgcy.hpp"');
      WriteLn('#include "fflllog.hpp"');
      WriteLn('#include "ffllthrd.hpp"');
      WriteLn('#include "ffsqlbas.hpp"');
      WriteLn('#include "ffsqleng.hpp"');
      WriteLn('#include "ffsrcmd.hpp"');
      WriteLn('#include "ffsreng.hpp"');
      WriteLn('#include "ffsrintm.hpp"');
      WriteLn('#include "ffsrjour.hpp"');
      WriteLn('#include "ffsrsec.hpp"');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('class %s : public %s', [ModuleName, CBaseClassName]);
      WriteLn('{');
      WriteLn('__published:     // IDE-managed Components');
      WriteLn('        TffEventLog *EventLog;');
      WriteLn('        TffServerEngine *ServerEngine;');
      WriteLn('        TffSqlEngine *SQLEngine;');
      WriteLn('        TffServerCommandHandler *CommandHandler;');
      WriteLn('        TffSecurityMonitor *SecurityMonitor;');
      WriteLn('        TffThreadPool *ThreadPool;');
      if ptSingleUser in aProtocols then
        WriteLn('        TffLegacyTransport *SUPTransport;');
      if ptIPXSPX in aProtocols then
        WriteLn('        TffLegacyTransport *IPXSPXTransport;');
      if ptTCPIP in aProtocols then
        WriteLn('        TffLegacyTransport *TCPIPTransport;');
      WriteLn('private: // User declarations');
      WriteLn('        TffFullFileName FScriptFile;');
      WriteLn('        bool __fastcall GetLogEnabled(void);');
      WriteLn('        void __fastcall SetLogEnabled(const bool aEnabled);');
      WriteLn('        void __fastcall SetScriptFile(const TffFullFileName aFileName);');
      WriteLn('public:          // User declarations');
      FormatLn('        __fastcall %s(TComponent* Owner);', [ModuleName]);
      NewLine;
      WriteLn('        void __fastcall GetServerEngines(TffList* &aServerList);');
      WriteLn('        void __fastcall GetTransports(TffIntermediateServerEngine *aServer, TffList* &aTransList);');
      WriteLn('        virtual void __fastcall Process(PffDataMessage Msg, bool &Handled);');
      WriteLn('        virtual void __fastcall Restart(void);');
      WriteLn('        virtual void __fastcall Shutdown(void);');
      WriteLn('        virtual void __fastcall Startup(void);');
      WriteLn('        virtual void __fastcall Stop(void);');
      NewLine;
      WriteLn('        __property bool EventLogEnabled={read=GetLogEnabled, write=SetLogEnabled};');
      WriteLn('        __property TffFullFileName ScriptFile={read=FScriptFile, write=SetScriptFile};');
      WriteLn('};');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('extern PACKAGE %s *%s;', [ModuleName, copy(ModuleName, 2, Length(ModuleName) - 1)]);
      WriteLn('//---------------------------------------------------------------------------');
      WriteLn('#endif');
      Position := 0;
    except
      Free;
      raise;
    end;
end;
{-------}
function CreateCppStream(UnitName, ModuleName : string; aProtocols : TFFProtocols): TTextStream;
var
  HeaderDate : string;
begin
  Result := TTextStream.Create('');
  with Result do
    try
      WriteLn('//---------------------------------------------------------');
      WriteLn('// FlashFiler: Engine manager');
      HeaderDate := DateToStr(Now);
      FormatLn('// Generated on %s with Release %5.4f',
               [HeaderDate, ffVersionNumber / 10000.0]);
      WriteLn('//---------------------------------------------------------');
      NewLine;
      WriteLn('//---------------------------------------------------------------------------');
      NewLine;
      WriteLn('#include <vcl.h>');
      WriteLn('#pragma hdrstop');
      NewLine;
      FormatLn('#include "%s.h"', [UnitName]);
      WriteLn('//---------------------------------------------------------------------------');
      WriteLn('#pragma package(smart_init)');
      WriteLn('#pragma link "ffllbase"');
      WriteLn('#pragma link "ffllcomm"');
      WriteLn('#pragma link "ffllcomp"');
      WriteLn('#pragma link "fflleng"');
      WriteLn('#pragma link "fflllgcy"');
      WriteLn('#pragma link "fflllog"');
      WriteLn('#pragma link "ffllthrd"');
      WriteLn('#pragma link "ffnetmsg"');
      WriteLn('#pragma link "ffsqlbas"');
      WriteLn('#pragma link "ffsqleng"');
      WriteLn('#pragma link "ffsrcmd"');
      WriteLn('#pragma link "ffsreng"');
      WriteLn('#pragma link "ffsrintm"');
      WriteLn('#pragma link "ffsrjour"');
      WriteLn('#pragma link "ffsrsec"');
      WriteLn('#pragma resource "*.dfm"');
      FormatLn('%s *%s;', [ModuleName, copy(ModuleName, 2, Length(ModuleName) - 1)]);
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('__fastcall %s::%s(TComponent* Owner)', [ModuleName, ModuleName]);
      FormatLn('        : %s(Owner)', [CBaseClassName]);
      WriteLn('{');
      WriteLn('  EventLog->FileName = ExtractFilePath(Application->ExeName) + "FFServer.log";');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('bool __fastcall %s::GetLogEnabled(void)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  bool Result;');
      WriteLn('  TffBaseServerEngine* anEngine;');
      NewLine;
      WriteLn('  Result = false;');
      WriteLn('  // Assumption: Event log is enabled if we find a server engine');
      WriteLn('  // that is routing events to the log.');
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffBaseServerEngine*>(Components[Inx]);');
      WriteLn('    if (anEngine != NULL)');
      WriteLn('    {');
      WriteLn('      Result = anEngine->EventLogEnabled;');
      WriteLn('      break;');
      WriteLn('    }');
      WriteLn('  }');
      WriteLn('  return Result;');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::SetLogEnabled(const bool aEnabled)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  TffLoggableComponent* aComponent;');
      WriteLn('  TffBaseTransport* aTransport;');
      NewLine;
      WriteLn('  // Assumption: TffBaseLog is always enabled.  We just control which');
      WriteLn('  // components are issuing messages to the log.');
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    aComponent = dynamic_cast<TffLoggableComponent*>(Components[Inx]);');
      WriteLn('    aTransport = dynamic_cast<TffBaseTransport*>(Components[Inx]);');
      WriteLn('    if ((aComponent != NULL) && (aTransport == NULL))');
      WriteLn('      aComponent->EventLogEnabled = aEnabled;');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::SetScriptFile(const TffFullFileName aFileName)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  TffServerEngine* anEngine;');
      NewLine;
      WriteLn('  FScriptFile = aFileName;');
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffServerEngine*>(Components[Inx]);');
      WriteLn('    if (anEngine != NULL)');
      WriteLn('      anEngine->ScriptFile = aFileName;');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::GetServerEngines(TffList* &aServerList)', [ModuleName]);
      WriteLn('{');
      WriteLn('  TffIntListItem* ServerListItem;');
      WriteLn('  int i;');
      WriteLn('  TffBaseServerEngine* anEngine;');
      NewLine;
      WriteLn('  for (i = 0; i < ComponentCount; i++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffBaseServerEngine*>(Components[i]);');
      WriteLn('    if (anEngine != NULL)');
      WriteLn('    {');
      WriteLn('      ServerListItem = new TffIntListItem(int(Components[i]));');
      WriteLn('      aServerList->Insert(ServerListItem);');
      WriteLn('    }');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::GetTransports(TffIntermediateServerEngine *aServer, TffList* &aTransList)', [ModuleName]);
      WriteLn('{');
      WriteLn('  TffIntListItem* TransportItem;');
      WriteLn('  int i, k;');
      NewLine;
      WriteLn('  for (i = 0; i < aServer->CmdHandlerCount; i++)');
      WriteLn('  {');
      WriteLn('    for (k = 0; k < aServer->CmdHandler[i]->TransportCount; k++)');
      WriteLn('    {');
      WriteLn('      TransportItem = new TffIntListItem(int(aServer->CmdHandler[i]->Transports[k]));');
      WriteLn('      aTransList->Insert(TransportItem);');
      WriteLn('    }');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::Process(PffDataMessage Msg, bool &Handled)', [ModuleName]);
      WriteLn('{');
      WriteLn('  Handled = true;');
      WriteLn('  switch(Msg->dmMsg)');
      WriteLn('  {');
      WriteLn('    case ffnmServerRestart  :');
      WriteLn('    {');
      WriteLn('      Restart();');
      WriteLn('      break;');
      WriteLn('    }');
      WriteLn('    case ffnmServerShutdown :');
      WriteLn('    {');
      WriteLn('      Shutdown();');
      WriteLn('      break;');
      WriteLn('    }');
      WriteLn('    case ffnmServerStartup :');
      WriteLn('    {');
      WriteLn('      Startup();');
      WriteLn('      break;');
      WriteLn('    }');
      WriteLn('    case ffnmServerStop :');
      WriteLn('    {');
      WriteLn('      Stop();');
      WriteLn('      break;');
      WriteLn('    }');
      WriteLn('    default:');
      WriteLn('      Handled = false;');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::Restart(void)', [ModuleName]);
      WriteLn('{');
      WriteLn('  Shutdown();');
      WriteLn('  Startup();');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::Shutdown(void)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  TffBaseServerEngine* anEngine;');
      WriteLn('  TffBasePluginEngine* aPlugin;');
      WriteLn('  TffStateComponent* aStateCmp;');
      NewLine;
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffBaseServerEngine*>(Components[Inx]);');
      WriteLn('    aPlugin = dynamic_cast<TffBasePluginEngine*>(Components[Inx]);');
      WriteLn('    aStateCmp = dynamic_cast<TffStateComponent*>(Components[Inx]);');
      NewLine;
      WriteLn('    if ((anEngine != NULL) | (aPlugin != NULL) &&');
      WriteLn('        ((aStateCmp->State != ffesInactive) && (aStateCmp->State != ffesStopped)))');
      WriteLn('      aStateCmp->Shutdown();');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::Startup(void)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  TffBaseServerEngine* anEngine;');
      WriteLn('  TffBasePluginEngine* aPlugin;');
      WriteLn('  TffStateComponent* aStateCmp;');
      NewLine;
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffBaseServerEngine*>(Components[Inx]);');
      WriteLn('    aPlugin = dynamic_cast<TffBasePluginEngine*>(Components[Inx]);');
      WriteLn('    aStateCmp = dynamic_cast<TffStateComponent*>(Components[Inx]);');
      NewLine;
      WriteLn('    if ((anEngine != NULL) | (aPlugin != NULL))');
      WriteLn('      aStateCmp->Startup();');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      FormatLn('void __fastcall %s::Stop(void)', [ModuleName]);
      WriteLn('{');
      WriteLn('  int Inx;');
      WriteLn('  TffBaseServerEngine* anEngine;');
      WriteLn('  TffBasePluginEngine* aPlugin;');
      WriteLn('  TffStateComponent* aStateCmp;');
      NewLine;
      WriteLn('  for (Inx = 0; Inx < ComponentCount; Inx++)');
      WriteLn('  {');
      WriteLn('    anEngine = dynamic_cast<TffBaseServerEngine*>(Components[Inx]);');
      WriteLn('    aPlugin = dynamic_cast<TffBasePluginEngine*>(Components[Inx]);');
      WriteLn('    aStateCmp = dynamic_cast<TffStateComponent*>(Components[Inx]);');
      NewLine;
      WriteLn('    if ((anEngine != NULL) | (aPlugin != NULL))');
      WriteLn('      aStateCmp->Stop();');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('//---------------------------------------------------------------------------');
      Position := 0;
    except
      Free;
      raise;
    end;
end;
{$ENDIF}
{End !!.06}
{-------}
function CreateDelphiSourceStream(UnitName, ModuleName : string; aProtocols : TFFProtocols): TTextStream;
{ Build the source (.pas) file for the module. }
var
  HeaderDate, HeaderVer : string;
begin
  Result := TTextStream.Create('');
  with Result do
    try
      WriteLn('{*********************************************************}');
      WriteLn('{* FlashFiler: Engine manager                            *}');
      HeaderDate := DateToStr(Now);
      HeaderVer := Format('%5.4f', [ffVersionNumber / 10000.0]);
      FormatLn('{* Generated on %s with Release %s%s*}',
               [HeaderDate, HeaderVer,
                StringOfChar(' ', 27 - (Length(HeaderDate) + Length(HeaderVer)))]);
               { 27 is sum of 9 spaces + space occupied by "mm/dd/yyyy" &
                 "vv.vvvv" for version. }
      WriteLn('{*********************************************************}');
      NewLine;
      WriteLn('{$I ffdefine.inc}');
      NewLine;
      FormatLn('unit %s;', [UnitName]);
      NewLine;
      WriteLn('interface');
      NewLine;
      WriteLn('uses');
      WriteLn('  windows, messages, sysutils, classes, controls, forms, fflleng, ffsreng, ');
      WriteLn('  ffllcomm, fflllgcy, fflllog, ffllthrd, ffnetmsg, ffsrintm, ffsrcmd, ffllbase,');
      WriteLn('  ffsrsec, ffsqlbas, ffsqleng, ffllcomp, ffsrjour;');
      NewLine;
      WriteLn('type');
      FormatLn('  %s = class(' + CBaseClassName + ')', [ModuleName]);
      WriteLn('    ServerEngine : TFFServerEngine;');
      WriteLn('    EventLog : TffEventLog;');
      WriteLn('    CommandHandler : TFFServerCommandHandler;');
      WriteLn('    SecurityMonitor : TFFSecurityMonitor;');
      WriteLn('    ThreadPool : TFFThreadPool;');
      if ptSingleUser in aProtocols then
        WriteLn('    SUPTransport : TFFLegacyTransport;');
      if ptIPXSPX in aProtocols then
        WriteLn('    IPXSPXTransport : TFFLegacyTransport;');
      if ptTCPIP in aProtocols then
        WriteLn('    TCPIPTransport : TFFLegacyTransport;');
      WriteLn('    SQLEngine: TffSqlEngine;');
      WriteLn('  private');
      WriteLn('    { private declarations }');
      WriteLn('  protected');
      WriteLn('    FScriptFile : TffFullFileName;');
      WriteLn('    function GetLogEnabled : boolean;');
      WriteLn('    procedure SetLogEnabled(const aEnabled : boolean);');
      WriteLn('    procedure SetScriptFile(const aFileName : TffFullFileName);');
      WriteLn('  public');
      WriteLn('    constructor Create(Sender: TComponent); override;');
      WriteLn('    procedure GetServerEngines(var aServerList : TffList);');
      WriteLn('    procedure GetTransports(aServer : TffIntermediateServerEngine; var aTransList : TffList);');
      WriteLn('    procedure Process(Msg : PffDataMessage; var Handled : Boolean); override;');
      WriteLn('    procedure Restart; override;');
      WriteLn('    procedure Shutdown; override;');
      WriteLn('    procedure Startup; override;');
      WriteLn('    procedure Stop; override;');
      NewLine;
      WriteLn('    { Properties }');
      WriteLn('    property EventLogEnabled : boolean');
      WriteLn('             read  GetLogEnabled');
      WriteLn('             write SetLogEnabled;');
      NewLine;
      WriteLn('    property ScriptFile : TffFullFileName');
      WriteLn('             read FScriptFile');
      WriteLn('             write SetScriptFile;');
      NewLine;
      WriteLn('  end;');
      NewLine;
      WriteLn('var');
      FormatLn('  %s: %s;',
               [copy(ModuleName, 2, Length(ModuleName) - 1),ModuleName]);
      NewLine;
      WriteLn('implementation');
      NewLine;
      WriteLn('{$R *.DFM}');
      NewLine;
      WriteLn('{====================================================================}');
      FormatLn('constructor %s.Create(Sender: TComponent);', [ModuleName]);
      WriteLn('begin');
      WriteLn('  inherited Create(Sender);');
      WriteLn('  EventLog.FileName := ExtractFilePath(Application.ExeName) + ''FFServer.log'';');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('function %s.GetLogEnabled : boolean;', [ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  Result := False;');
      WriteLn('  { Assumption: Event log is enabled if we find a server engine');
      WriteLn('    that is routing events to the log. }');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[Idx] is TffBaseServerEngine) then begin');
      WriteLn('      Result := TffBaseServerEngine(Components[Idx]).EventLogEnabled;');
      WriteLn('      break;');
      WriteLn('    end;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.GetServerEngines(var aServerList: TffList);', [ModuleName]);
      WriteLn('var');
      WriteLn('  ServerListItem : TffIntListItem;');
      WriteLn('  i              : Integer;');
      WriteLn('begin');
      WriteLn('  for I := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[i] is TffBaseServerEngine) then begin');
      WriteLn('      ServerListItem := TffIntListItem.Create(longint(Components[i]));');
      WriteLn('      aServerList.Insert(ServerListItem);');
      WriteLn('    end;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.GetTransports(aServer    : TffIntermediateServerEngine;', [ModuleName]);
      WriteLn('                                     var aTransList : TffList);');
      WriteLn('var');
      WriteLn('  TransportItem : TffIntListItem;');
      WriteLn('  i, k          : Integer;');
      WriteLn('begin');
      WriteLn('  for i := 0 to Pred(aServer.CmdHandlerCount) do begin');
      WriteLn('    for k := 0 to Pred(aServer.CmdHandler[i].TransportCount) do begin');
      WriteLn('      TransportItem := TffIntListItem.Create(Integer(aServer.CmdHandler[i].Transports[k]));');
      WriteLn('      aTransList.Insert(TransportItem);');
      WriteLn('    end;');
      WriteLn('  end;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.Process(Msg : PffDataMessage; var Handled : Boolean);', [ModuleName]);
      WriteLn('begin');
      WriteLn('  Handled := True;');
      WriteLn('  case Msg.dmMsg of');
      WriteLn('    ffnmServerRestart  : Restart;');
      WriteLn('    ffnmServerShutdown : Shutdown;');
      WriteLn('    ffnmServerStartUp  : Startup;');
      WriteLn('    ffnmServerStop     : Stop;');
      WriteLn('  else');
      WriteLn('    Handled := False;');
      WriteLn('  end;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.Restart;', [ModuleName]);
      WriteLn('begin');
      WriteLn('  Shutdown;');
      WriteLn('  Startup;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.SetLogEnabled(const aEnabled : boolean);',[ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  { Assumption: TffBaseLog is always enabled.  We just control which');
      WriteLn('    components are issuing messages to the log. }');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[Idx] is TffLoggableComponent) and');
      WriteLn('       not (Components[Idx] is TffBaseTransport) then');
      WriteLn('      TffLoggableComponent(Components[Idx]).EventLogEnabled := aEnabled');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.SetScriptFile(const aFileName : TffFullFileName);',[ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  FScriptFile := aFileName;');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[Idx] is TffServerEngine) then');
      WriteLn('      TffServerEngine(Components[Idx]).ScriptFile := aFileName;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.Shutdown;', [ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if ((Components[Idx] is TFFBaseServerEngine) or');
      WriteLn('        (Components[Idx] is TFFBasePluginEngine)) and');
      WriteLn('        not (TffStateComponent(Components[Idx]).State in');
      WriteLn('          [ffesInactive, ffesStopped]) then');
      WriteLn('      TffStateComponent(Components[Idx]).Shutdown;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.Startup;', [ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[Idx] is TFFBaseServerEngine) or');
      WriteLn('       (Components[Idx] is TFFBasePluginEngine) then');
      WriteLn('      TffStateComponent(Components[Idx]).Startup;');
      WriteLn('end;');
      WriteLn('{--------}');
      FormatLn('procedure %s.Stop;', [ModuleName]);
      WriteLn('var');
      WriteLn('  Idx : Integer;');
      WriteLn('begin');
      WriteLn('  for Idx := 0 to Pred(ComponentCount) do');
      WriteLn('    if (Components[Idx] is TFFBaseServerEngine) or');
      WriteLn('       (Components[Idx] is TFFBasePluginEngine) then');
      WriteLn('      TffStateComponent(Components[Idx]).Stop;');
      WriteLn('end;');
      WriteLn('{====================================================================}');
      NewLine;
      WriteLn('end.');
      Position := 0;
    except
      Free;
      raise;
    end;
end;
{-------}
procedure CreateEngineManager(aProtocols : TFFProtocols);
{ Create the new module based on the selected protocols (aProtocols) }
var
  UnitName, ModuleName, FileName : string;
{$IFNDEF IsDelphi}
  HdrAdapter,
{$ENDIF}
  ModuleAdapter, UnitAdapter : TIStreamAdapter;
begin
  ToolServices.GetNewModuleAndClassName('TFFEngineManager', UnitName,
                                        ModuleName, FileName);
  ModuleAdapter := AdaptStream(CreateModuleStream(ModuleName, aProtocols));
  try
{$IFDEF IsDelphi}
    UnitAdapter := AdaptStream(CreateDelphiSourceStream(UnitName, ModuleName, aProtocols));
    try
      ToolServices.CreateModule(FileName, UnitAdapter, ModuleAdapter,
                                  [cmAddToProject, cmShowSource, cmMarkModified,
                                   cmShowForm, cmUnNamed]);
    except
      UnitAdapter.Free;
      raise;
    end;
{$ELSE}
    UnitAdapter := AdaptStream(CreateCppStream(UnitName, ModuleName, aProtocols));
    HdrAdapter := AdaptStream(CreateHdrStream(UnitName, ModuleName, aProtocols));
    try
      ToolServices.CreateCppModule(FileName, 'formName', 'TDataModule', '',
                                   HdrAdapter, UnitAdapter, ModuleAdapter,
                                   [cmAddToProject, cmShowSource, cmMarkModified,
                                    cmShowForm, cmUnNamed]);
    except
      UnitAdapter.Free;
      HdrAdapter.Free;
      raise;
    end;
{$ENDIF}
  except
    ModuleAdapter.Free;
    raise;
  end;
end;
{-------}
procedure StartWizard;
var
  FFProtocols : TFFProtocols;
begin
  { Prompt the user for a set of protocols to support. }
  with TFrmSelectProtocols.Create(nil) do
    try
      ShowModal; {The protocol selection form}

      { Get the list of selected protocols }
      FFProtocols := [];
      if chkSU.Checked then
        FFProtocols := [ptSingleUser];
      if chkIPX.Checked then
        FFProtocols := FFProtocols + [ptIPXSPX];
      if chkTCP.Checked then
        FFProtocols := FFProtocols + [ptTCPIP];

    finally
      Free; {The protocol selection form}
    end;

  { Create the module }
  CreateEngineManager(FFProtocols);
end;

{string constants used to return information to the expert}
resourcestring
  RCompany = 'TurboPower Software Company';
  RComment = 'FlashFiler 2 Engine Manager Module';
  RName    = 'FlashFiler 2 Engine Manager';
  RPage    = 'Data Modules';

{=== TFFEngineManagerWizard ===============================}
procedure TFFEngineManagerWizard.Execute;
begin
  StartWizard;
end;
{-------}
function TFFEngineManagerWizard.GetAuthor : string;
begin
  Result := RCompany;
end;
{-------}
function TFFEngineManagerWizard.GetComment : string;
begin
  Result := RComment;
end;
{-------}
function TFFEngineManagerWizard.GetGlyph : HICON;
begin
  Result := LoadIcon(hInstance, CICON);
end;
{-------}
function TFFEngineManagerWizard.GetIDString : string;
begin
  Result := RCompany + '.' + RName;
end;
{-------}
function TFFEngineManagerWizard.GetMenuText : string;
begin
  Result := '';
end;
{-------}
function TFFEngineManagerWizard.GetName : string;
begin
  Result := RName;
end;
{-------}
function TFFEngineManagerWizard.GetPage : string;
begin
  Result := RPage;
end;
{-------}
function TFFEngineManagerWizard.GetState : TExpertState;
begin
  Result := [esEnabled];
end;
{-------}
function TFFEngineManagerWizard.GetStyle : TExpertStyle;
begin
  Result := esForm;
end;

end.

