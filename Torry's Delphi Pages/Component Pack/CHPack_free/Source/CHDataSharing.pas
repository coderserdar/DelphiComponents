unit CHDataSharing;

{ ##############################################################################
  TCHDataSharing

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 20.08.2003    - First Release


  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, StdCtrls, ComCtrls;

type
  EMemMapException = class(Exception);

  TStringRecord = record
    Text : string[255];
    end;

type
  TDataReadyEvent = procedure(Sender: TObject) of object;
  TDataRequestEvent = procedure(Sender: TObject; MsgSender: Hwnd; Param: Integer) of object;
  TBeforeSendDataEvent = procedure(Sender: TObject) of object;
  TDataSentEvent = procedure(Sender: TObject) of object;

  TCHCustomDataSharing = class(TComponent)
  private
    FData : TStringRecord;
    FSendFile : THandle;
    FReadFile : THandle;
    FFileName: String;
    FRequestingDataMsgHandle: hWND;
    FSendingDataMsgHandle: hWND;
    FTargetApplication: String;
    FTargetApplicationHandle : HWND;

    FAfterDataSent: TDataSentEvent;
    FBeforeSendData: TBeforeSendDataEvent;
    FDataReady: TDataReadyEvent;
    FDataRequest: TDataRequestEvent;
    FDataStrings: TStrings;
    Procedure SetFFileName (Value: String);
    procedure SetDataStrings(const Value: TStrings);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RequestingDataMsgHandle: hWND Read FRequestingDataMsgHandle;
    property SendingDataMsgHandle: hWND Read FSendingDataMsgHandle;
    procedure HandleMessages(var Msg: TMsg; var Handled: Boolean);
    procedure RequestData(Param :Integer);

    property MemoryName: String Read FFileName Write SetFFileName;
    property TargetApplication: String Read FTargetApplication Write FTargetApplication;
    property DataStrings : TStrings read FDataStrings Write SetDataStrings;


    function ReadData: String;
    function SendData(Data : string): Boolean;
    function ReadDataStrings: Boolean;
    function SendDataStrings: Boolean;

    // Events
    property AfterDataSent: TDataSentEvent read FAfterDataSent write FAfterDataSent;
    property BeforeSendData: TBeforeSendDataEvent read FBeforeSendData write FBeforeSendData;
    property OnDataReady: TDataReadyEvent read FDataReady write FDataReady;
    property OnDataRequest: TDataRequestEvent read FDataRequest write FDataRequest;
  published
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
 TCHDataSharing = class(TCHCustomDataSharing)
  private
  protected
  public
  published
    Property MemoryName;
    property TargetApplication;
    property DataStrings;

    property AfterDataSent;
    property BeforeSendData;
    property OnDataReady;
    property OnDataRequest;
  end;

Const
  FILENAME_RESET = 1000;
  FILENAME_ADDCHAR = 1001;
  FILENAME_READY = 1002;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHDataSharing]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomDataSharing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MemoryName := 'DataFile_' + Application.Title;
  TargetApplication := '';
  FDataStrings := TStringList.Create;
  FSendingDataMsgHandle := registerWindowMessage('WM_SendingData');
  FRequestingDataMsgHandle := registerWindowMessage('WM_RequestingData');
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomDataSharing.Destroy;
begin
  FDataStrings.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
Procedure TCHCustomDataSharing.SetFFileName (Value: String);
begin
  if Value <> FFileName then
  begin
    if pos('\', Value) = 0 then
      FFileName := Value
    else
      Application.MessageBox('Das Zeichen <\> ist nicht erlaubt!', 'Hinweis', MB_OK);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomDataSharing.SendData(Data: string): Boolean;
var
  ShareFileView: Pointer;
  FileSize, i : Integer;
  Msg: String;
begin
  Result := True;
  ShareFileView := nil;

  Try
    Try
      if assigned(FBeforeSendData) then
        FBeforeSendData(self);

      //  FILEMAPPING  CREATION IN MEMORY
      FData.Text := Data;
      FileSize := SizeOf(FData.Text);
      FSendFile := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, FileSize, PChar(MemoryName));
      ShareFileView := MapViewOfFileEx(FSendFile, FILE_MAP_WRITE, 0, 0, 0, nil);
      System.FillChar(ShareFileView^, FileSize, $FF);
      System.Move(FData, ShareFileView^, SizeOf(FData));
      FlushViewOfFile(ShareFileView,0);


      //  NOTIFICATION MESSAGE
      // Windows calls to send a notification message to target application
      // warning the application something is ready for reading in memory
      // Get application handle  = Application.title of searched main window
      FTargetApplicationHandle := FindWindow('TApplication',PChar(FTargetApplication));

      if FTargetApplicationHandle <> 0 then
      begin
        PostMessage(FTargetApplicationHandle,SendingDataMsgHandle,FILENAME_RESET,0);
        For i := 1 to Length(MemoryName) do
          PostMessage(FTargetApplicationHandle,SendingDataMsgHandle,FILENAME_ADDCHAR,Ord(MemoryName[i]));
          PostMessage(FTargetApplicationHandle,SendingDataMsgHandle,FILENAME_READY,0);
        end
        else
        begin
          Msg := 'Die Anwendung: [' + TargetApplication + '] ist nicht gestartet. Es konnten keine Daten gesendet werden.';
          Application.MessageBox(PChar(Msg), 'Fehler', MB_OK);
        end;

      if assigned(FAfterDataSent) then
        FAfterDataSent(self);

    except
      Result := False;
    end;
  finally
    UnmapViewOfFile(ShareFileView);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomDataSharing.ReadData: String;
var
  ShareFileView: Pointer;
begin
  Result := '';
  ShareFileView := nil;
  FReadFile := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(MemoryName));
  Try
    if FReadFile = 0 then
      raise Exception.Create('Der Speicherblock ['+ MemoryName +'] konnte nicht gelesen werden.');
    ShareFileView := MapViewOfFile(FReadFile, FILE_MAP_ALL_ACCESS, 0, 0, 0);

    //  DATA READING
    System.Move(ShareFileView^, FData, SizeOf(FData.Text));

    Result := FData.Text;
  finally
    UnmapViewOfFile(ShareFileView);
    CloseHandle(FReadFile);
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomDataSharing.ReadDataStrings: Boolean;
var
  sRead, sVar : string;
begin
  Result := True;
  try
    FDataStrings.Clear;
    sRead := ReadData;
    while Pos('$\$', sRead) > 0 do
    begin
      sVar := Copy(sRead, 1, Pos('$\$', sRead) -1);
      FDataStrings.Add(sVar);
      Delete(sRead, 1, Length(sVar) + 3);
    end;
  except
    Result := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomDataSharing.SendDataStrings: Boolean;
var
  I : Integer;
  sSendString : string;
begin
  Result := True;
  try
    for I := 0 to FDataStrings.Count -1 do
    begin
        sSendString := sSendString + FDataStrings.Strings[I] + '$\$';
    end;
    SendData(sSendString);
  except
    Result := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomDataSharing.RequestData(Param: Integer);
var
  Msg: String;
begin
  FTargetApplicationHandle := FindWindow('TApplication',PChar(FTargetApplication));
  if FTargetApplicationHandle <> 0 then
    PostMessage(FTargetApplicationHandle,RequestingDataMsgHandle,Application.Handle,Param)
  else
  begin
    Msg := 'Die Anwendung: [' + TargetApplication + '] ist nicht gestartet. Es konnten keine Daten gesendet werden.';
    Application.MessageBox(PChar(Msg), 'Fehler', MB_OK);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomDataSharing.HandleMessages(var Msg: TMsg; var Handled: Boolean);
begin
  If Msg.message = SendingDataMsgHandle then
  begin
    case Msg.wParam of
      FILENAME_RESET   : MemoryName := '';
      FILENAME_ADDCHAR : MemoryName := MemoryName + Chr(Msg.lParam);
      FILENAME_READY   : if assigned(FDataReady) then
                           FDataReady(self);
    end;
    Handled := True;
  end;

  If Msg.message = RequestingDataMsgHandle then
  begin
    if assigned(FDataReady) then
      FDataRequest(self, Msg.wParam, Msg.lParam);
    Handled := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomDataSharing.SetDataStrings(const Value: TStrings);
begin
  FDataStrings.Assign(Value);
end;



end.
