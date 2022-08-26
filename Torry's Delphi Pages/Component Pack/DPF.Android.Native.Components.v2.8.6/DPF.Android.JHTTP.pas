// ------------------------------------------------------------------------------
// DPF.Android.JHTTP Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.Android.JHTTP;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,

  System.TypInfo,
  DPF.Android.BaseControl,

{$IFDEF ANDROID}
  DPF.Android.Widget,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
{$IFDEF DELPHIXE6}
  Androidapi.Helpers,
{$ENDIF}
{$ELSE}
  DPF.Android.DesignTime,
{$ENDIF}
  FMX.Forms,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

type

  TDPFJHTTP = class;

{$IFDEF ANDROID}

  // ----------------------------------------------------------------------------
  TDPFOnHTTPListener = class( TJavaLocal, JDPFOnHTTPListener )
  private
    FDPFJHTTP: TDPFJHTTP;
  public
    constructor create( ADPFJHTTP: TDPFJHTTP );

    procedure onCancelled( DPFHttp: JDPFHTTP ); cdecl;
    procedure onStarted( DPFHttp: JDPFHTTP ); cdecl;
    procedure onProgressUpdate( DPFHttp: JDPFHTTP; progress: Integer; downloadSize: Integer; downloaded: Integer ); cdecl;
    procedure onFinished( DPFHttp: JDPFHTTP; returnCode: Integer; httpResponseCode: Integer; httpResponseMessage: JString; error: JString ); cdecl;
  end;
{$ENDIF}

  // ----------------------------------------------------------------------------
  THTTPHeader = record
    Field: string;
    Value: string;
  end;

  THTTPFormField = record
    Field: string;
    Value: string;
  end;

  TDPFHTTPReturnCode = ( OP_FINISHED = 0, OP_CANCELED = 1, OP_INTERNAL_ERROR = 2, OP_HTTPERROR = 3 );

  TDPFHTTPOnCancelled       = procedure( sender: TObject ) of object;
  TDPFHTTPOnStarted         = procedure( sender: TObject ) of object;
  TDPFHTTPOnPregoressUpdate = procedure( sender: TObject; Progress: Int64; DownloadSize: Int64; Donloaded: Int64 ) of object;
  TDPFHTTPOnFinished        = procedure( sender: TObject; ReturnCode: TDPFHTTPReturnCode; httpResponseCode: Integer; httpResponseMessage: string; error: string ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidAndroid )]
  TDPFJHTTP = class( TComponent )
  private
    FOnPregoressUpdate: TDPFHTTPOnPregoressUpdate;
    FOnFinished       : TDPFHTTPOnFinished;
    FOnStarted        : TDPFHTTPOnStarted;
    FOnCancelled      : TDPFHTTPOnCancelled;
  protected
{$IFDEF ANDROID}
    FJDPFHTTP         : JDPFHTTP;
    FDPFOnHTTPListener: TDPFOnHTTPListener;
{$ENDIF}
  public
{$IFDEF ANDROID}
    property GetJHTTP: JDPFHTTP read FJDPFHTTP;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure StartDownload( url: string; SaveFileName: string; UserName: string = ''; Password: string = '' );
    procedure StartUpload( SaveFileName: string; Url: string; const HTTPHeader: array of THTTPHeader; FormFields: array of THTTPFormField );
    procedure CancelTask( );
  published
    property OnPregoressUpdate: TDPFHTTPOnPregoressUpdate read FOnPregoressUpdate write FOnPregoressUpdate;
    property OnCancelled      : TDPFHTTPOnCancelled read FOnCancelled write FOnCancelled;
    property OnStarted        : TDPFHTTPOnStarted read FOnStarted write FOnStarted;
    property OnFinished       : TDPFHTTPOnFinished read FOnFinished write FOnFinished;
  end;

function BuildHeaderRecord( Field: string; Value: string ): THTTPHeader;
function BuildFormFieldRecord( Field: string; Value: string ): THTTPFormField;

implementation

// ------------------------------------------------------------------------------
function BuildHeaderRecord( Field: string; Value: string ): THTTPHeader;
begin
  Result.Field := Field;
  Result.Value := Value;
end;

// ------------------------------------------------------------------------------
function BuildFormFieldRecord( Field: string; Value: string ): THTTPFormField;
begin
  Result.Field := Field;
  Result.Value := Value;
end;

// ------------------------------------------------------------------------------
{ TDPFJHTTP }
constructor TDPFJHTTP.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      FDPFOnHTTPListener := TDPFOnHTTPListener.create( self );
      FJDPFHTTP := TJDPFHTTP.JavaClass.init;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFJHTTP.Destroy;
begin
{$IFDEF ANDROID}
  FJDPFHTTP := nil;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFJHTTP.StartDownload( url: string; SaveFileName: string; UserName: string = ''; Password: string = '' );
begin

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      if assigned( FJDPFHTTP ) then
      begin
        FJDPFHTTP.startDownload( SharedActivity, StringToJString( url ), StringToJString( SaveFileName ), StringToJString( UserName ), StringToJString( Password ), FDPFOnHTTPListener );
      end;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJHTTP.StartUpload( SaveFileName: string; Url: string; const HTTPHeader: array of THTTPHeader; FormFields: array of THTTPFormField );
{$IFDEF ANDROID}
var
  mHeaders, mFields: TJavaObjectArray<JString>;
  i                : Integer;
  aa               : JString;
{$ENDIF}
begin

{$IFDEF ANDROID}
  aa       := TJString.JavaClass.init;
  mHeaders := TJavaObjectArray<JString>.Create( Length( HTTPHeader ) * 2 );
  for I    := 0 to high( HTTPHeader ) do
  begin
    mHeaders.Items[i * 2 + 0] := StringToJString( HTTPHeader[i].Field );
    mHeaders.Items[i * 2 + 1] := StringToJString( HTTPHeader[i].Value );
  end;

  mFields := TJavaObjectArray<JString>.Create( Length( FormFields ) * 2 );
  for I   := 0 to high( FormFields ) do
  begin
    mFields.Items[i * 2 + 0] := StringToJString( FormFields[i].Field );
    mFields.Items[i * 2 + 1] := StringToJString( FormFields[i].Value );
  end;

  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      if assigned( FJDPFHTTP ) then
      begin
        FJDPFHTTP.startUpload( SharedActivity, StringToJString( SaveFileName ), StringToJString( url ), mHeaders, mFields, FDPFOnHTTPListener );
      end;
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFJHTTP.CancelTask( );
begin
{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure( )
    begin
      if assigned( FJDPFHTTP ) then
      begin
        FJDPFHTTP.cancelTask;
      end;
    end );
{$ENDIF}
end;

{$IFDEF ANDROID}
// ------------------------------------------------------------------------------

{ TDPFOnWebViewListener }

constructor TDPFOnHTTPListener.create( ADPFJHTTP: TDPFJHTTP );
begin
  inherited create;
  FDPFJHTTP := ADPFJHTTP;
end;

// ------------------------------------------------------------------------------
procedure TDPFOnHTTPListener.onStarted( DPFHttp: JDPFHTTP ); cdecl;
begin
  if Assigned( FDPFJHTTP.FOnStarted ) then
    FDPFJHTTP.FOnStarted( FDPFJHTTP );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnHTTPListener.onProgressUpdate( DPFHttp: JDPFHTTP; progress: Integer; downloadSize: Integer; downloaded: Integer ); cdecl;
begin
  if Assigned( FDPFJHTTP.FOnPregoressUpdate ) then
    FDPFJHTTP.FOnPregoressUpdate( FDPFJHTTP, progress, downloadSize, downloaded );
end;

// ------------------------------------------------------------------------------
// OP_FINISHED       = 0;
// OP_CANCELED       = 1;
// OP_INTERNAL_ERROR = 2;
// OP_HTTPERROR      = 3;
procedure TDPFOnHTTPListener.onFinished( DPFHttp: JDPFHTTP; returnCode: Integer; httpResponseCode: Integer; httpResponseMessage: JString; error: JString ); cdecl;
begin
  if Assigned( FDPFJHTTP.FOnFinished ) then
    FDPFJHTTP.FOnFinished( FDPFJHTTP, TDPFHTTPReturnCode( returnCode ), httpResponseCode, JStringToString( httpResponseMessage ), JStringToString( error ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFOnHTTPListener.OnCancelled( DPFHttp: JDPFHTTP ); cdecl;
begin
  if Assigned( FDPFJHTTP.FOnCancelled ) then
    FDPFJHTTP.FOnCancelled( FDPFJHTTP );
end;

{$ENDIF}
// ------------------------------------------------------------------------------

end.
