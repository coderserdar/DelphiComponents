// TWSocket extensions by Chris Barber, Lucas Controls, Inc.
// This component is based on works by Francois Piette, whose tireless efforts
// are greatly appreciated.

// Revisions:
//    1.0 initial. This one did not have any revision information in it.
//    1.1   added virtual method DoReceiveBuffer as the event-firing method for
//          the OnReceiveBuffer event. This allows decendant classes to override or
//          capture this event internally without "eating" the event. Also,
//          programmers should note that if the DataAvailable event is used
//          there is the possibility that its event handler will empty the
//          socket's receive buffer leaving no data for the ReceiveBuffer event to process.
//    1.2   Added property PreserveSettings which preserves the values of the
//          Addr, Port, and Proto properties when true. Override of virtual method
//          AssignDefaultValue implements this feature.

unit WSocketExt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WSocket;

type
  TReceiveBufferEvent = procedure(Sender:TObject; Buf:pointer; Count:integer; FromAddr:string; FromPort:word) of object;

  TWSocketExt = class(TWSocket)
  private
    { Private declarations }
    FOnReceiveBuffer:TReceiveBufferEvent;
    FPreserveSettings:boolean;

  protected
    { Protected declarations }
    function  TriggerDataAvailable(Error : Word) : Boolean; override;
    procedure BuildReceiveBuffer(var Buf:pointer; var Count:integer; var PeerAddr:string; var PeerPort:word);
    procedure DoReceiveBuffer(Buf:pointer; Count:integer; PeerAddr:string; PeerPort:word);virtual;
    procedure SetPortNumber(p:word);
    function  GetPortNumber:word;
    procedure AssignDefaultValue; override;
  public
    { Public declarations }
    function  SendTo(Data : Pointer; Len : Integer; ToAddr:string; ToPort:word) : integer; virtual;
    procedure Close; override;
  published
    { Published declarations }
    constructor Create(AComponent:TComponent);override;
    property  PortNumber:word read GetPortNumber write SetPortNumber;
    property  OnReceiveBuffer:TReceiveBufferEvent read FOnReceiveBuffer write FOnReceiveBuffer;
    property  PreserveSettings:boolean read FPreserveSettings write FPreserveSettings;
  end;

procedure Register;

implementation

uses  WinSock;

constructor TWSocketExt.Create(AComponent:TComponent);
begin
   inherited Create(AComponent);
   FPreserveSettings := false; // to maintain base class default behavior
end;

procedure TWSocketExt.Close;
begin
   inherited;
   // nothing here yet - just a place holder for some future stuff.
end;

procedure TWSocketExt.AssignDefaultValue;
var
   Lproto:string;
   Laddr:string;
   Lport:string;
begin
   if FPreserveSettings then
   begin
      Lproto := Proto;
      Laddr := Addr;
      Lport := Port;
      inherited;
      Proto := Lproto;
      Addr := Laddr;
      Port := Lport;
   end
   else
      inherited;
end;

procedure TWSocketExt.SetPortNumber(p:word);
begin
   // I've really got a problem with the Port property being a string.
   // Obviously, I am not on the same wavelength as Francois when it comes
   // to designing property interfaces.
   Port := IntToStr(p);
end;

function TWSocketExt.GetPortNumber:word;
var
   code:integer;
begin
   val(Port, result, code);
   if code > 0 then
      result := 0;
end;

function  TWSocketExt.SendTo(Data : Pointer; Len : Integer; ToAddr:string; ToPort:word) : integer;
begin
   // This method transmits the buffer to the network peer specified by the
   // ToAddr and ToPort parameters, regardless of the values of the
   // Addr and Port properties.

   // Judging from the TWSocket source, I suspect that this method is effective
   // only when running UDP.

   // setting ToAddr to '' will reset sin_addr and sin_port back to the current values
   // of the Addr and Port properties, making the call equivalent to Send().
   // Note that once having called SendTo(), subsequent calls to Send()
   // will behave identically to the last call to SendTo() because TryToSend()
   // assumes that everything is set correctly to begin with. In fact, this
   // method depends on that fact.

   if ToAddr <> '' then
   begin
      sin.sin_addr.s_addr := WSocketResolveHost(ToAddr).s_addr;
      sin.sin_port := ToPort;
   end
   else
   begin
      sin.sin_addr.s_addr := WSocketResolveHost(Addr).s_addr;
      sin.sin_port := PortNumber;
   end;

   result := Send(Data, Len);
end;

function  TWSocketExt.TriggerDataAvailable(Error : Word) : Boolean;
var
   Buf:pointer;
   Count:integer;
   PeerAddr:string;
   PeerPort:word;
begin
   // if this handler empties the receive buffer then
   // OnReceiveBuffer will not get any data
   result := inherited TriggerDataAvailable(Error);

   try
      BuildReceiveBuffer(Buf, Count, PeerAddr, PeerPort);
      DoReceiveBuffer(Buf, Count, PeerAddr, PeerPort);
      result := true;
   finally
      if Buf <> nil then
         FreeMem(Buf);
   end;
end;

procedure TWSocketExt.BuildReceiveBuffer(var buf:pointer; var count:integer; var PeerAddr:string; var PeerPort:word);
var
   PeerInfo:TSockAddrIn;
   PeerInfoSize:integer;
begin
   PeerAddr := '';
   PeerPort := 0;
   Count := RcvdCount;
   Buf := nil;
   if Count > 0 then
   begin
      Buf := AllocMem(Count);
      PeerInfoSize := Sizeof(PeerInfo);
      ReceiveFrom(Buf, Count, PeerInfo, PeerInfoSize);
      with PeerInfo do
      begin
         PeerAddr := Format('%d.%d.%d.%d',
                                    [byte(sin_addr.s_un_b.s_b1),
                                     byte(sin_addr.s_un_b.s_b2),
                                     byte(sin_addr.s_un_b.s_b3),
                                     byte(sin_addr.s_un_b.s_b4)]);

         PeerPort := sin_port;
      end;
   end;
end;

procedure TWSocketExt.DoReceiveBuffer(Buf:pointer; Count:integer; PeerAddr:string; PeerPort:word);
begin
   if assigned(FOnReceiveBuffer) then
      FOnReceiveBuffer(self, Buf, Count, PeerAddr, PeerPort);
end;

procedure Register;
begin
  RegisterComponents('Internet', [TWSocketExt]);
end;

end.
