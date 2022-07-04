{***************************************************}
{File:      NCOciCompNamerSetupDlg.pas              }
{Revision:  1.02 / 06.02.2000                       }
{Comment:   Component naming expert options dialog  }
{Copyright: (c) 1997-2000, Dmitry Arefiev           }
{Author:    Dmitry Arefiev, darefiev@da-soft.com    }
{***************************************************}
{$I NCOciDef.inc}

unit NCOciCompNamerSetupDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, NCOciCompNamer, Menus, ComCtrls;

type
  TOciCompNamerSetupFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PopupMenu1: TPopupMenu;
    C1: TMenuItem;
    T1: TMenuItem;
    V1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    U1: TMenuItem;
    L1: TMenuItem;
    P1: TMenuItem;
    S1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    TabSheet3: TTabSheet;
    HotKey1: THotKey;
    Label2: TLabel;
    Label5: TLabel;
    HotKey2: THotKey;
    N4: TMenuItem;
    N5: TMenuItem;
    T2: TMenuItem;
    C2: TMenuItem;
    CheckBox6: TCheckBox;
    procedure MIClick(Sender: TObject);
  private
    { Private declarations }
    FCompNamer: TOciCompNamer;
    FNamingHotKey: TShortCut;
    FOptHotKey: TShortCut;
    procedure LoadData;
    procedure SaveData;
  public
    { Public declarations }
    class function Execute(ACompNamer: TOciCompNamer;
      var ANamingHotKey: TShortCut; var AOptHotKey: TShortCut): Boolean;
  end;

var
  OciCompNamerSetupFrm: TOciCompNamerSetupFrm;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{$R *.DFM}

procedure TOciCompNamerSetupFrm.LoadData;
begin
    with FCompNamer do begin
        Edit1.Text := StripPrefixs;
        CheckBox1.Checked := nsRemoveUCa in Options;
        CheckBox2.Checked := nsRemoveLCa in Options;
        CheckBox3.Checked := nsRemoveUCb in Options;
        CheckBox4.Checked := nsRemoveLCb in Options;
        CheckBox6.Checked := nsRemoveOver in Options;
        CheckBox5.Checked := nsPrefixsNoCase in Options;
        Memo1.Lines := NameFormats;
        HotKey1.HotKey := FNamingHotKey;
        HotKey2.HotKey := FOptHotKey;
    end;
end;

procedure TOciCompNamerSetupFrm.SaveData;
begin
    with FCompNamer do begin
        StripPrefixs := Edit1.Text;
        Options := [];
        if CheckBox1.Checked then
            Options := Options + [nsRemoveUCa];
        if CheckBox2.Checked then
            Options := Options + [nsRemoveLCa];
        if CheckBox3.Checked then
            Options := Options + [nsRemoveUCb];
        if CheckBox4.Checked then
            Options := Options + [nsRemoveLCb];
        if CheckBox6.Checked then
            Options := Options + [nsRemoveOver];
        if CheckBox5.Checked then
            Options := Options + [nsPrefixsNoCase];
        NameFormats := Memo1.Lines;
        FNamingHotKey := HotKey1.HotKey;
        FOptHotKey := HotKey2.HotKey;
    end;
end;

procedure TOciCompNamerSetupFrm.MIClick(Sender: TObject);
var
    s: String;
    i: Integer;

    procedure SendChar(ctrl: TControl; ch: Char);
    var
        VkCode: Word;

        procedure PostVk(AMsg: Word; AVKey: Word);
        begin
            Ctrl.Perform(AMsg, AVKey, MapVirtualKey(AVKey, 0));
        end;

    begin
        VkCode := VkKeyScan(ch);
        if (HiByte(VkCode) and 1) = 1 then
            PostVk(WM_KEYDOWN, VK_SHIFT);
        PostVk(WM_KEYDOWN, VkCode);
        PostVk(WM_CHAR, Word(ch));
        PostVk(WM_KEYUP, VkCode);
        if (HiByte(VkCode) and 1) = 1 then
            PostVk(WM_KEYUP, VK_SHIFT);
    end;

begin
    s := TMenuItem(Sender).Hint;
    for i := 1 to Length(s) do
        SendChar(Memo1, s[i]);
end;

class function TOciCompNamerSetupFrm.Execute(ACompNamer: TOciCompNamer;
  var ANamingHotKey: TShortCut; var AOptHotKey: TShortCut): Boolean;
begin
    with TOciCompNamerSetupFrm.Create(nil) do
    try
        FCompNamer := ACompNamer;
        FNamingHotKey := ANamingHotKey;
        FOptHotKey := AOptHotKey;
        LoadData;
        Result := (ShowModal = mrOK);
        if Result then begin
            SaveData;
            ANamingHotKey := FNamingHotKey;
            AOptHotKey := FOptHotKey;
        end;
    finally
        Free;
    end;
end;

end.
