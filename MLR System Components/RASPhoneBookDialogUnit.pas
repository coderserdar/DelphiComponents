unit RASPhoneBookDialogUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TRASPhoneBookDialogOption = (rpoUsePosition, rpoForceCloseOnDial,
    rpoNoUser, rpoUpdateDefaults);
  TRASPhoneBookDialogOptions = set of TRASPhoneBookDialogOption;
  TRASPhoneBookDialogCallback = procedure (Sender :TObject; Event :DWord;
    Text :PChar; Data :Pointer) of object;

  TRASPhoneBookDialog = class(TComponent)
  private
    FPositionY: Integer;
    FPoxitionX: Integer;
    FPhoneBook: string;
    FEntry: string;
    FOptions: TRASPhoneBookDialogOptions;
    FOnCallback: TRASPhoneBookDialogCallback;
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoCallback(Event :DWord; Text :PChar; Data :Pointer);
  public
    { Public declarations }
    procedure Execute;
  published
    { Published declarations }
    property Options    :TRASPhoneBookDialogOptions read FOptions write FOptions default [];
    property PhoneBook  :string read FPhoneBook write FPhoneBook;
    property Entry      :string read FEntry write FEntry;
    property PositionX  :Integer read FPoxitionX write FPositionX default 0;
    property PositionY  :Integer read FPositionY write FPositionY default 0;
    property OnCallback :TRASPhoneBookDialogCallback read FOnCallback write FOnCallback;
  end;

procedure Register;

implementation


procedure DlgCallback(CallbackID :DWord; Event :DWord; Text :PChar; Data :Pointer); export;
begin
  TRASPhoneBookDialog(CallbackID).DoCallback(Event, Text, Data);
end;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TRASPhoneBookDialog]);
end;

{ TRASPhoneBookDialog }

procedure TRASPhoneBookDialog.DoCallback(Event: DWord; Text: PChar;
  Data: Pointer);
begin
  if Assigned(FOnCallback) FOnCallback(Self, Event, Text, Data);
end;

procedure TRASPhoneBookDialog.Execute;
begin

end;

end.
