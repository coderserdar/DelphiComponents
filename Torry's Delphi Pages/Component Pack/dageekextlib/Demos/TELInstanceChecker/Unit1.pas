{*******************************************************}
{                                                       }
{       Extension Library example of                    }
{       TELInstanceChecker                              }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELControls, ImgList, Menus, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Bevel1: TBevel;
    Label4: TLabel;
    Memo2: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure InstanceCheckerResieveData(Sender: TObject;
      const AData: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  InstanceChecker: TELInstanceChecker;

implementation

uses Unit2;

{$R *.dfm}

{$IFNDEF VER140}
function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Windows.GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;
{$ENDIF}

{ TForm1 }

procedure TForm1.InstanceCheckerResieveData(Sender: TObject;
  const AData: string);
begin
  { This procedure will be executed when other instance of this
    application post data with InstanceChecker.PostData method }

  Memo2.Lines.Add(AData);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Set InstanceCheckerResieveData as OnResieveData event handler
    of InstanceChecker }
  InstanceChecker.OnResieveData := InstanceCheckerResieveData;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LSI: _STARTUPINFOA;
  LPI: _PROCESS_INFORMATION;
begin
  with LSI do
  begin
    cb := SizeOf(LSI);
    lpReserved := nil;
    lpDesktop := nil;
    lpTitle := nil;
    dwFlags := 0;
    cbReserved2 := 0;
    lpReserved2 := nil;
  end;
  CreateProcess(PChar(GetModuleName(HInstance)), nil, nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, nil, LSI, LPI);
end;

initialization
  { Create InstanceChecker }
  InstanceChecker := TELInstanceChecker.Create('TELInstanceChecker demo');

  { Try to register instance BEFORE creating any forms to save time
    and resources (Components dropped on forms at design time and
    OnCreate event handlers may execute unneeded (if application can not
    register instance) code while creating forms. Some times this code may
    be very slow, for example opening DB connections). }
  if not InstanceChecker.RegisterInstance then
  begin
    { If instance was not registered, then post data to registered instance and
      close this instance. You can post diferent kind of information,
      for examlpe CD Player demo post all startup params to registered
      instance. }
    with TForm2.Create(nil) do
    try
      if ShowModal = mrOk then
      begin
        if InstanceChecker.PostData(Edit1.Text) then
           Halt
        else
          { Method Post data return False if registered instance was closed.
            In this case this instance can be registered. }
          if InstanceChecker.RegisterInstance then
            ShowMessage('While you typing post data, registered instance' +
              ' of this application was closed, ' + #13 +
              'so this instance was registered')
          else
            Halt;
      end else Halt;
    finally
      Free;
    end;
  end;

finalization
  { Free InstanceChecker }
  InstanceChecker.Free;

end.
