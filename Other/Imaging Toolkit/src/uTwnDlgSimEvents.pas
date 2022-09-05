// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  15930: uTwnDlgSimEvents.pas 
//
//    Rev 1.3    2014-01-15 13:42:06  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.2    2013-12-04 23:16:20  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.1    14-06-2003 10:35:28  mcm    Version: DT 2.4
// Fixed returning the correct device event when not all device events were
// enabled.

//
//   Rev 1.0    04-12-2001 16:49:14  mcm    Version: DT 2.0

unit uTwnDlgSimEvents;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, WinApi.Messages, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls,
     {$ENDIF}
     mcmTWAINContainer;

type
  TFormEvents = class(TForm)
    gbEvents  : TGroupBox;
    lEvents   : TLabel;
    cbEvents  : TComboBox;
    btnSend   : TButton;
    btnClose  : TButton;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure cbEventsChange(Sender : TObject);
    procedure btnSendClick(Sender : TObject);
  private
    { Private declarations }
    FContainer : TtwnContainer;
    FCurDevice : integer;
  public
    { Public declarations }
    property DeviceEventContainer : TtwnContainer
      read   FContainer
      write  FContainer;
    property Event : integer
      read   FCurDevice;
  end;

var FormEvents : TFormEvents;

implementation

{$R *.DFM}

procedure TFormEvents.FormCreate(Sender : TObject);
begin
  cbEvents.ItemIndex := -1;
  FCurDevice := -1;
end; { End TFormEvents.FormCreate.                                             }


procedure TFormEvents.FormShow(Sender : TObject);
begin
  if (FContainer <> Nil)
  then cbEvents.Enabled := True
  else cbEvents.Enabled := False;
  cbEvents.ItemIndex := 0;
  cbEventsChange(Sender);
end; { End TFormEvents.FormShow.                                               }


procedure TFormEvents.cbEventsChange(Sender : TObject);
var i : integer;
begin
  if (FContainer <> Nil)
  then begin
       FCurDevice := -1;
       i := 0;
       while (FCurDevice = -1) and (i < FContainer.NumItems)
       do begin
          if (FContainer.Items[i] = cbEvents.ItemIndex)
          then FCurDevice := cbEvents.ItemIndex;
          inc(i);
       end;
       if (FCurDevice >= 0)
       then btnSend.Enabled := True
       else btnSend.Enabled := False;
  end
  else btnSend.Enabled := False;
end; { End TFormEvents.cbEventsChange.                                         }


procedure TFormEvents.btnSendClick(Sender : TObject);
begin
;
end; { End TFormEvents.btnSendClick.                                           }

end.
