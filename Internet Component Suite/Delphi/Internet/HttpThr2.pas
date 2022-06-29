{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (From a work done by Ed Hochman <ed@mbhsys.com>)
Creation:     Jan 13, 1998
Version:      1.00
Description:  HttpThrd is a demo program showing how to use THttpCli component
              in a multi-threaded program.
EMail:        francois.piette@pophost.eunet.be    
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
  	      implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpThr2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HttpProt;

type
  THTTPThread = class(TThread)
  private
    FProgress : String;
    procedure UpdateStatus;
    procedure ShowProgress;
    procedure Progress(Msg : String);
    procedure DocBegin(Sender : TObject);
    procedure DocData(Sender : TObject; Buffer : Pointer; Len : Integer);
    procedure DocEnd(Sender : TObject);
  published
    procedure Setup(i: Integer);
    procedure Execute; override;
  public
    FURL          : String;
    FProxy        : String;
    FThreadNumber : Integer;
    FHttpCli      : THTTPCli;
    Success       : Boolean;
  end;

implementation

uses
    HttpThr1;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Setup(i: Integer);
begin
    FThreadNumber := i;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.UpdateStatus;
begin
    HttpThreadForm.ProcessResults(FThreadNumber, Success);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.ShowProgress;
begin
    HttpThreadForm.ProgressListBox.Items.Add(FProgress);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Progress(Msg : String);
begin
    FProgress := Msg;
    SynChronize(ShowProgress);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Execute;
begin
    FHttpCli               := THTTPCli.Create(Nil);
    FHttpCli.MultiThreaded := TRUE;
    FHttpCli.RcvdStream    := TMemoryStream.Create;
    FHttpCli.OnDocBegin    := DocBegin;
    FHttpCli.OnDocEnd      := DocEnd;
    FHttpCli.OnDocData     := DocData;
    while not Terminated do begin
        Progress(IntToStr(FThreadNumber) + ' Start get');
        with FHttpCli do begin
            URL   := FURL;
            Proxy := FProxy;
            (RcvdStream as TMemoryStream).Clear;
            try
                Get;   // Get page from internet
                Success := TRUE;
            except
                Success := FALSE;
            end;
        end;

        if not Terminated then
            Synchronize(UpdateStatus);
    end;

    if FHttpCli.RcvdStream <> nil then
         FHttpCli.RcvdStream.Free;

    FHttpCli.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocBegin(Sender : TObject);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc begin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocData(Sender : TObject; Buffer : Pointer; Len : Integer);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc data');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocEnd(Sender : TObject);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc end');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

