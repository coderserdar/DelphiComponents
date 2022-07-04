// This demo shows how to use threading with NCOCI8. Here are 2 approaches:
// - to use real threading - "run threads" button do that. To use real
//   threading, you should set: TOCIDatabase.InitModes = [dmThreaded].
// - to use nonblocking mode - this allows to run query in main (UI) thread,
//   and it will not block execution. You can use TOCIDatabase.OnIdle event
//   to do something while query is executing. To use that, you should set
//   TOCIDatabase.NonBlockingMode = True
// It is not possible to mix both approaches in single connection ! Because
// that do not press both buttons.

// 1) You can change query, marked by comment: CHANGE THIS QUERY to something
//    more resource consumable.
// 2) For break functionality You must connect to server using Oracle8 client
//    version >= 8.0.5.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, NCOci, NCOciWrapper, NCOciDB;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OCIDatabase1: TOCIDatabase;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FThreadsCount: Integer;
    procedure ThreadTerminated;
    procedure ThreadStarted;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

type
    TOCIThread = Class(TThread)
    private
        FOwner: TForm1;
    protected
        constructor Create(AOwner: TForm1);
        procedure Execute; override;
    end;

constructor TOCIThread.Create(AOwner: TForm1);
begin
    inherited Create(False);
    FOwner := AOwner;
    FreeOnTerminate := True;
    Priority := tpIdle;
    Resume;
end;

procedure TOCIThread.Execute;
var
    FQuery: TOCIQuery;
begin
    Synchronize(FOwner.ThreadStarted);
    FQuery := TOCIQuery.Create(nil);
    // CHANGE THIS QUERY
    FQuery.SQL.Add('select count(*) from sales s1, sales s2, sales s3');
    try
        try
            FQuery.Open;
        except on E: EOCINativeError do
             // object canceled current operation
            if (E.Errors[0].ErrorCode <> 1013) then
                raise;
        end;
    finally
        FQuery.Free;
        Synchronize(FOwner.ThreadTerminated);
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    i: Integer;
begin
    for i := 0 to 10 do
        TOCIThread.Create(Self);
end;

procedure TForm1.ThreadStarted;
begin
    Inc(FThreadsCount);
    Label2.Caption := IntToStr(FThreadsCount);
    Update;
end;

procedure TForm1.ThreadTerminated;
begin
    Dec(FThreadsCount);
    Label2.Caption := IntToStr(FThreadsCount);
    Update;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
    FQuery: TOCIQuery;
begin
    FQuery := TOCIQuery.Create(nil);
    // CHANGE THIS QUERY
    FQuery.SQL.Add('select count(*) from sales s1, sales s2, sales s3');
    try
        try
            FQuery.Open;
        except on E: EOCINativeError do
             // object canceled current operation
            if (E.Errors[0].ErrorCode <> 1013) then
                raise;
        end;
    finally
        FQuery.Free;
    end;
end;

end.
