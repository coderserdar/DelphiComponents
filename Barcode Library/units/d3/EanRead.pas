unit EanRead;

interface

uses
  Messages, Windows, SysUtils, Classes, Controls,Dialogs;

resourcestring rsErrorOpeningComPort='Error opening com port.';

type

  TpsComPort      = (psCom1, psCom2, psCom3, psCom4, psCom5, psCom6, psCom7, psCom8);
  TpsComBitRate   = (br300, br600, br1200, br2400, br4800, br9600, br14400, br19200, br38400, br57600, br115200 );
  TpsComParity    = (parityNone, parityOdd, paritySpace, parityEven, parityMark);
  TpsComDataBits  = (db7Bits, db8Bits);
  TpsComStopBits  = (sbStop1, sbStop15, sbStop2);

  TpsOnBarcodeReady = procedure (Sender:TObject; Barcode:String) of Object;

  TpsBarcodeReaderOption = (boRemoveCR, boRemoveLF);
  TpsBarcodeReaderOptions = set of TpsBarcodeReaderOption;
  TBarcodeReader = class;

  // thread for  reading from port
  TEanReadThread = class(TThread)
  private
        FReader     : TBarcodeReader;
        FBarcode    : String;
        FEventMask  : DWORD;
        FComHandle  : THandle;
        FTimeOut    : Integer;
        procedure HandleComPortEvent;
  public
        CloseEvent  : THandle;
        procedure   Execute; override;
        constructor CreateEan(Reader:TBarcodeReader);
        procedure   DataReceived;
  end;

  // basic com port component
  TBarcodeReader = class(TComponent)
  private
    FActive      : Boolean;
    FComPort     : TpsComPort;
    FComParity   : TpsComParity;
    FComBitRate  : TpsComBitRate;
    FComdataBits : TpsComDataBits;
    FComStopBits : TpsComStopBits;

    FReadThread  : TEanReadThread;
    FOnBarcodeReady: TpsOnBarcodeReady;
    FTimeOut: Integer;
    FLastBarcode: String;
    FOptions: TpsBarcodeReaderOptions;

    procedure SetActive(const Value: Boolean);
    procedure InternalClose;
    procedure InternalOpen;
  protected
  public
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     procedure   Loaded; override;
     procedure   ReConnect;
  published
     property Active    :Boolean        Read FActive       Write SetActive;
     property Port      :TpsComPort     Read FComPort      Write FComPort;
     property BitRate   :TpsComBitRate  Read FComBitrate   Write FComBitRate;
     property Parity    :TpsComParity   Read FComParity    Write FComParity;
     property DataBits  :TpsComDataBits Read FComDataBits  Write FComDataBits;
     property StopBits  :TpsComStopBits Read FComStopBits  Write FComStopBits;
     property TimeOut   :Integer        Read FTimeOut      Write FTimeOut Default 100;
     property LastBarcode:String        Read FLastBarcode  Write FLastBarcode;
     property OnBarcodeReady:TpsOnBarcodeReady Read FOnBarcodeReady Write FOnBarcodeReady;
     property Options:TpsBarcodeReaderOptions  Read FOptions Write FOptions;
  end;

implementation

constructor TBarcodeReader.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);

     FComPort     := psCom2;
     FComParity   := parityEven;
     FComBitRate  := br9600;
     FComdataBits := db8Bits;
     FComStopBits := sbStop1;
     FTimeOut     := 100;

     FReadThread  := nil;
     FActive      := False;
end;

destructor TBarcodeReader.Destroy;
begin
     if FActive then
        InternalClose;
     inherited Destroy;
end;

procedure TBarcodeReader.Loaded;
begin
     inherited Loaded;
     if Active then InternalOpen;
end;

procedure TBarcodeReader.InternalOpen;
begin
        if FActive then InternalClose;
        FReadThread := TEanReadThread.CreateEan(Self);
        FActive := True;
end;

procedure TBarcodeReader.InternalClose;
begin
    if FActive then begin
        SetEvent(FReadThread.CloseEvent);
        FReadThread.WaitFor;
    end;
    FActive := False;
end;


procedure TbarcodeReader.SetActive(const Value: Boolean);
begin
    if FActive<>Value then begin
        if Value then InternalOpen
        else          InternalClose;
    end;
end;

constructor TEanReadThread.CreateEan(Reader: TBarcodeReader);
var PortName:String;
    lpdcb:Tdcb;
begin
        inherited create(False);

        FReader  := Reader;
        PortName := '\\.\COM'+IntToStr(Integer(Reader.Port));
        FTimeOut := Reader.TimeOut;
        SetLength(FBarcode,20);
        FBarcode := '';

        FComHandle:=CreateFile(PChar(PortName),
                GENERIC_READ,
                0,
                nil,
                OPEN_EXISTING,
                FILE_FLAG_OVERLAPPED,
                0);

        if FComHandle=INVALID_HANDLE_VALUE then
                raise Exception.Create(rsErrorOpeningComPort);

        // set serial port parameters here
        SetupComm(FComHandle, 8192, 0);
        GetCommState(FComHandle,lpdcb);
        case Reader.BitRate of
                br115200 : lpdcb.baudrate:= 115200;
                br57600  : lpdcb.baudrate:=  57600;
                br38400  : lpdcb.baudrate:=  38400;
                br19200  : lpdcb.baudrate:=  19200;
                br14400  : lpdcb.baudrate:=  14400;
                br9600   : lpdcb.baudrate:=   9600;
                br4800   : lpdcb.baudrate:=   4800;
                br2400   : lpdcb.baudrate:=   2400;
                br1200   : lpdcb.baudrate:=   1200;
                br600    : lpdcb.baudrate:=    600;
                br300    : lpdcb.baudrate:=    300;
        end;
        case Reader.StopBits of
                sbStop1  : lpdcb.StopBits:=ONESTOPBIT;
                sbStop15 : lpdcb.StopBits:=ONE5STOPBITS;
                sbStop2  : lpdcb.StopBits:=TWOSTOPBITS;
        end;
        case Reader.DataBits of
                db8Bits  : lpdcb.ByteSize:=8;
                db7Bits  : lpdcb.ByteSize:=7;
        end;
        case Reader.Parity of
                parityNone      : lpdcb.Parity:= NoParity;
                parityOdd       : lpdcb.Parity:= OddParity;
                paritySpace     : lpdcb.Parity:= NoParity;
                parityEven      : lpdcb.Parity:= EvenParity;
                parityMark      : lpdcb.Parity:= MarkParity;
        end;


        SetCommState(FComHandle,lpdcb);
        SetCommMask(FComHandle,ev_rxchar);

        FreeOnTerminate := True;
end;


procedure TEanReadThread.DataReceived;
    procedure BarcodeRemove(ch:Char);
    var l,k:Integer;
    begin
         k:=Pos(ch,FBarcode);
         l:=Length(FBarcode);
         while k>0 do begin
               FBarcode:=Copy(FBarcode,1,k-1)+Copy(FBarcode,k+1,l);
               k:=Pos(ch,FBarcode);
         end;
    end;
begin
        if boRemoveCR in FReader.Options then
           BarcodeRemove(#13);
        if boRemoveLF in FReader.Options then
           BarcodeRemove(#10);

        FReader.Lastbarcode := FBarcode;
        if Assigned(FReader.OnBarcodeReady) then
                FReader.OnBarcodeReady(FReader, FBarcode);
        FBarcode := '';
end;

procedure TEanReadThread.Execute;
var WaitHandles : array[0..1] of THandle;
    Overlap     : TOverlapped;
    WaitEvent   : Cardinal;
begin
    ZeroMemory(@Overlap, SizeOf(Overlap));
    CloseEvent     := CreateEvent(nil, True, False, nil);
    Overlap.hEvent := CreateEvent(nil, True, True,  nil);

    WaitHandles[0] := CloseEvent;
    WaitHandles[1] := Overlap.hEvent;

    while not Terminated do begin
        WaitCommEvent(FComHandle, FEventMask, @Overlap);
        WaitEvent := WaitForMultipleObjects(2, @WaitHandles, False, FTimeOut);
        case WaitEvent of
                WAIT_OBJECT_0     : Terminate;
                WAIT_OBJECT_0 + 1 : HandleComPortEvent;
                WAIT_TIMEOUT      : if FBarcode<>'' then Synchronize(DataReceived);
        end;
    end;

    CloseHandle(Overlap.hEvent);
    CloseHandle(CloseEvent);
    CloseHandle(FComHandle);
end;

procedure TEanReadThread.HandleComPortEvent;
var ErrorMask : DWORD;
    ComStat   : TComStat;
    Overlapped: TOverlapped;
    BytesRead : DWORD;
    Buffer    : array[0..4095] of Char;
    Received  : Integer;
    i,ofs     : Integer;
begin
        ClearCommError(FComHandle, ErrorMask, @ComStat);
        if (FEventMask and EV_RXCHAR)<>0 then begin
                Received := ComStat.cbInQue;
                if Received>4096 then Received:=4096;
                if Received>0 then begin
                        ZeroMemory(@OverLapped, SizeOf(OverLapped));
                        Readfile(FComHandle, Buffer, Received, BytesRead, @OverLapped);
                        ofs := Length(FBarcode);
                        SetLength(FBarcode, ofs+Received);
                        for i:=1 to Received do
                                FBarcode[ofs+i] := Buffer[i-1];
                end;
        end;
end;


procedure TBarcodeReader.ReConnect;
begin
        Active := False;
        Active := True;
end;

end.
