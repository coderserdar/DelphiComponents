{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo show how to use the SysLog client.
Creation:     September 2009
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSysLogClientDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls,
  OverbyteIcsWSocket, OverbyteIcsSysLogClient, OverbyteIcsSysLogDefs;

type
  TSysLogClientForm = class(TForm)
    ToolPanel: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TextEdit: TEdit;
    FacilityComboBox: TComboBox;
    SeverityComboBox: TComboBox;
    ProcessNameEdit: TEdit;
    ProcessIDEdit: TEdit;
    ServerEdit: TEdit;
    ProcessMySelfCheckBox: TCheckBox;
    SendButton: TButton;
    HostnameEdit: TEdit;
    Label7: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape4: TShape;
    Shape3: TShape;
    DefaultPRICheckBox: TCheckBox;
    Label8: TLabel;
    Shape5: TShape;
    Shape6: TShape;
    NowCheckBox: TCheckBox;
    MonthEdit: TEdit;
    Label9: TLabel;
    DayEdit: TEdit;
    Label10: TLabel;
    HourEdit: TEdit;
    MinEdit: TEdit;
    SecEdit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    YearEdit: TEdit;
    MsgIDEdit: TEdit;
    StructDataEdit: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Shape7: TShape;
    Shape8: TShape;
    RFCPanel: TPanel;
    RFC3164RadioButton: TRadioButton;
    RFC5424RadioButton: TRadioButton;
    SetDefaultButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ProcessMySelfCheckBoxClick(Sender: TObject);
    procedure DefaultPRICheckBoxClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure NowCheckBoxClick(Sender: TObject);
    procedure RFC5424RadioButtonClick(Sender: TObject);
    procedure RFC3164RadioButtonClick(Sender: TObject);
    procedure SetDefaultButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FSysLogClient: TSysLogClient;
    procedure Display(const Msg : String);
  public
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SysLogClientForm: TSysLogClientForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyText            = 'Text';
    KeyFacility        = 'Facility';
    KeySeverity        = 'Severity';
    KeyHostName        = 'HostName';
    KeyProcess         = 'ProcessName';
    KeyPID             = 'ProcessID';
    KeyMySelf          = 'ProcessMySelf';
    KeyServer          = 'Server';
    KeyDftPri          = 'DefaultPRI';
    KeyYear            = 'Year';
    KeyMonth           = 'Month';
    KeyDay             = 'Day';
    KeyHour            = 'Hour';
    KeyMin             = 'Minute';
    KeySec             = 'Second';
    KeyNow             = 'Now';
    KeyMsgID           = 'MsgID';
    KeyStructData      = 'StructuredData';
    KeyRFC5424         = 'RFC5424';

    DftServer          = '127.0.0.1';
    DftFacility        = SYSLOG_FACILITY_USER;
    DftSeverity        = SYSLOG_SEVERITY_NOTICE;
    DftHostName        = 'MyHostName';
    DftProcess         = 'MyProcessName';
    DftPID             = '1234';
    DftMySelf          = TRUE;
    DftNow             = TRUE;
    DftPRI             = TRUE;
    DftYear            = '2009';
    DftMonth           = '9';
    DftDay             = '13';
    DftHour            = '14';
    DftMin             = '34';
    DftSec             = '48';
    DftText            = 'Hello http://www.OverByte.be';
    DftMsgID           = 'ID47';
    DftRFC5424         = TRUE;
    DftStructData      = '[exampleSDID@32473 iut="3" eventSource=' +
                         '"Application" eventID="1011"]';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.FormCreate(Sender: TObject);
var
    Facility : TSysLogFacility;
    Severity : TSysLogSeverity;
begin
    FSysLogClient := TSysLogClient.Create(Self);
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
    DisplayMemo.Clear;

    FacilityCombobox.Clear;
    for Facility := Low(TSysLogFacility) to High(TSysLogFacility) do
        FacilityCombobox.Items.Add(IntToStr(Ord(Facility)) + ') ' +
                                   SysLogFacilityString[Facility]);

    SeverityCombobox.Clear;
    for Severity := Low(TSysLogSeverity) to High(TSysLogSeverity) do
        SeverityCombobox.Items.Add(IntToStr(Ord(Severity)) + ') ' +
                                   SysLogSeverityString[Severity]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
            TextEdit.Text                 := IniFile.ReadString(SectionData,
                                                KeyText, DftText);
            FacilityCombobox.ItemIndex    := IniFile.ReadInteger(SectionData,
                                                KeyFacility, Ord(DftFacility));
            SeverityCombobox.ItemIndex    := IniFile.ReadInteger(SectionData,
                                                KeySeverity, Ord(DftSeverity));
            HostNameEdit.Text             := IniFile.ReadString(SectionData,
                                                KeyHostName, DftHostName);
            ProcessNameEdit.Text          := IniFile.ReadString(SectionData,
                                                KeyProcess, DftProcess);
            ProcessIDEdit.Text            := IniFile.ReadString(SectionData,
                                                KeyPID,     DftPID);
            ProcessMySelfCheckbox.Checked := IniFile.ReadBool(SectionData,
                                                KeyMySelf, DftMySelf);
            ServerEdit.Text               := IniFile.ReadString(SectionData,
                                                KeyServer, DftServer);
            DefaultPRICheckBox.Checked    := IniFile.ReadBool(SectionData,
                                                KeyDftPri, DftPRI);
            NowCheckBox.Checked           := IniFile.ReadBool(SectionData,
                                                KeyNow, DftNow);
            YearEdit.Text                 := IniFile.ReadString(SectionData,
                                                KeyYear, DftYear);
            MonthEdit.Text                := IniFile.ReadString(SectionData,
                                                KeyMonth, DftMonth);
            DayEdit.Text                  := IniFile.ReadString(SectionData,
                                                KeyDay, DftDay);
            HourEdit.Text                 := IniFile.ReadString(SectionData,
                                                KeyHour, DftHour);
            MinEdit.Text                  := IniFile.ReadString(SectionData,
                                                KeyMin, DftMin);
            SecEdit.Text                  := IniFile.ReadString(SectionData,
                                                KeySec, DftSec);
            MsgIDEdit.Text                := IniFile.ReadString(SectionData,
                                                KeyMsgID, DftMsgID);
            StructDataEdit.Text           := IniFile.ReadString(SectionData,
                                                KeyStructData, DftStructData);
            RFC5424RadioButton.Checked    := IniFile.ReadBool(SectionData,
                                                KeyRFC5424, DftRFC5424);
            RFC3164RadioButton.Checked    := not RFC5424RadioButton.Checked;
        finally
            IniFile.Free;
        end;
        ProcessMySelfCheckBoxClick(nil);
        DefaultPRICheckBoxClick(nil);
        NowCheckBoxClick(nil);
        RFC5424RadioButtonClick(nil);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    try
        IniFile := TIcsIniFile.Create(FIniFileName);
        try
            IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
            IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
            IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
            IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
            IniFile.WriteString(SectionData,
                                KeyText,     TextEdit.Text);
            IniFile.WriteInteger(SectionData,
                                 KeyFacility, FacilityCombobox.ItemIndex);
            IniFile.WriteInteger(SectionData,
                                 KeySeverity, SeverityCombobox.ItemIndex);
            IniFile.WriteString(SectionData,
                                KeyHostName, HostNameEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyProcess,  ProcessNameEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyPID,      ProcessIDEdit.Text);
            IniFile.WriteBool(SectionData,
                              KeyMySelf,     ProcessMySelfCheckbox.Checked);
            IniFile.WriteString(SectionData,
                                KeyServer,   ServerEdit.Text);
            IniFile.WriteBool(SectionData,
                              KeyDftPri,     DefaultPRICheckBox.Checked);
            IniFile.WriteBool(SectionData,
                              KeyNow,        NowCheckBox.Checked);
            IniFile.WriteString(SectionData,
                                KeyYear,     YearEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyMonth,    MonthEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyDay,      DayEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyHour,     HourEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyMin,      MinEdit.Text);
            IniFile.WriteString(SectionData,
                                KeySec,      SecEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyMsgID,    MsgIDEdit.Text);
            IniFile.WriteString(SectionData,
                                KeyStructData, StructDataEdit.Text);
            IniFile.WriteBool(SectionData,
                              KeyRFC5424, RFC5424RadioButton.Checked);
            IniFile.UpdateFile;
        finally
            IniFile.Free;
        end;
    except
        // Ignore any exception writing INI file (Usally permission problem)
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.Display(const Msg : String);
var
    I : Integer;
begin
    if not Assigned(DisplayMemo) then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.ProcessMySelfCheckBoxClick(Sender: TObject);
var
    MySelf  : String;
begin
    if ProcessMySelfCheckbox.Checked then begin
        ProcessIDEdit.Text := IntToStr(GetCurrentProcessID);
        SetLength(MySelf, 256);
        SetLength(MySelf, GetModuleFileName(0, @MySelf[1], Length(MySelf)));
        ProcessNameEdit.Text := ChangeFileExt(ExtractFileName(MySelf), '');
        HostNameEdit.Text    := String(LocalHostName);
    end;
    HostNameEdit.Enabled    := not ProcessMySelfCheckbox.Checked;;
    ProcessNameEdit.Enabled := not ProcessMySelfCheckbox.Checked;;
    ProcessIDEdit.Enabled   := not ProcessMySelfCheckbox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.RFC3164RadioButtonClick(Sender: TObject);
begin
    RFC5424RadioButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.RFC5424RadioButtonClick(Sender: TObject);
begin
     MsgIDEdit.Enabled      := RFC5424RadioButton.Checked;
     StructDataEdit.Enabled := RFC5424RadioButton.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.DefaultPRICheckBoxClick(Sender: TObject);
begin
    if DefaultPRICheckBox.Checked then begin
        FacilityCombobox.ItemIndex := Ord(SYSLOG_FACILITY_USER);
        SeverityCombobox.ItemIndex := Ord(SYSLOG_SEVERITY_NOTICE);
    end;
    FacilityComboBox.Enabled := not DefaultPRICheckBox.Checked;
    SeverityComboBox.Enabled := not DefaultPRICheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.NowCheckBoxClick(Sender: TObject);
var
    TS               : TDateTime;
    Day, Month, Year : Word;
    Hour, Minute     : Word;
    Second, MilliSec : Word;
begin
    if NowCheckBox.Checked then begin
        TS := Now;
        DecodeDate(TS, Year, Month, Day);
        DecodeTime(TS, Hour, Minute, Second, MilliSec);
        YearEdit.Text  := IntToStr(Year);
        MonthEdit.Text := IntToStr(Month);
        DayEdit.Text   := IntToStr(Day);
        HourEdit.Text  := IntToStr(Hour);
        MinEdit.Text   := IntToStr(Minute);
        SecEdit.Text   := IntToStr(Second);
    end;
    YearEdit.Enabled  := not NowCheckBox.Checked;
    MonthEdit.Enabled := not NowCheckBox.Checked;
    DayEdit.Enabled   := not NowCheckBox.Checked;
    HourEdit.Enabled  := not NowCheckBox.Checked;
    MinEdit.Enabled   := not NowCheckBox.Checked;
    SecEdit.Enabled   := not NowCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.SendButtonClick(Sender: TObject);
var
    Day, Month, Year : Word;
    Hour, Minute     : Word;
    Second           : Word;
begin
    NowCheckBoxClick(nil);
    Year   := StrToInt(YearEdit.Text);
    Month  := StrToInt(MonthEdit.Text);
    Day    := StrToInt(DayEdit.Text);
    Hour   := StrToInt(HourEdit.Text);
    Minute := StrToInt(MinEdit.Text);
    Second := StrToInt(SecEdit.Text);
    try
        FSysLogClient.TimeStamp := EncodeDate(Year, Month, Day) +
                                   EncodeTime(Hour, Minute, Second, 0);
    except
        Beep;
        ShowMessage('Please check the date/time you entered');
        Exit;
    end;
    FSysLogClient.Server     := Trim(ServerEdit.Text);
    FSysLogClient.Text       := Trim(TextEdit.Text);
    FSysLogClient.Facility   := TSysLogFacility(FacilityCombobox.ItemIndex);
    FSysLogClient.Severity   := TSysLogSeverity(SeverityCombobox.ItemIndex);
    FSysLogClient.HostName   := Trim(HostNameEdit.Text);
    FSysLogClient.Process    := Trim(ProcessNameEdit.Text);
    FSysLogClient.PID        := StrToIntDef(ProcessIDEdit.Text, 0);
    FSysLogClient.MsgID      := Trim(MsgIDEdit.Text);
    FSysLogClient.StructData := Trim(StructDataEdit.Text);
    FSysLogClient.RFC5424    := RFC5424RadioButton.Checked;
    FSysLogClient.Send;
    FSysLogClient.Close;
    Display(FormatDateTime('YYYYMMDD HHNNSS', Now) + '|' +
            FSysLogClient.RawMessage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClientForm.SetDefaultButtonClick(Sender: TObject);
begin
    TextEdit.Text                 := DftText;
    FacilityCombobox.ItemIndex    := Ord(DftFacility);
    SeverityCombobox.ItemIndex    := Ord(DftSeverity);
    HostNameEdit.Text             := DftHostName;
    ProcessNameEdit.Text          := DftProcess;
    ProcessIDEdit.Text            := DftPID;
    ProcessMySelfCheckbox.Checked := DftMySelf;
    ServerEdit.Text               := DftServer;
    DefaultPRICheckBox.Checked    := DftPRI;
    NowCheckBox.Checked           := DftNow;
    YearEdit.Text                 := DftYear;
    MonthEdit.Text                := DftMonth;
    DayEdit.Text                  := DftDay;
    HourEdit.Text                 := DftHour;
    MinEdit.Text                  := DftMin;
    SecEdit.Text                  := DftSec;
    MsgIDEdit.Text                := DftMsgID;
    StructDataEdit.Text           := DftStructData;
    RFC5424RadioButton.Checked    := DftRFC5424;
    RFC3164RadioButton.Checked    := not RFC5424RadioButton.Checked;
    ProcessMySelfCheckBoxClick(nil);
    DefaultPRICheckBoxClick(nil);
    NowCheckBoxClick(nil);
    RFC5424RadioButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
