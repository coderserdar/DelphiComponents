{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 09, 2010
Description:  Demo program showing how to use REST API from Google and
              Yahoo. REST stands for "Representational State Transfer".
              For an introduction to REST, see wikipedia article:
              http://en.wikipedia.org/wiki/Representational_State_Transfer
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2010 by François PIETTE
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
unit OverbyteIcsRestDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, Grids, Math, PngImage,
  OverbyteIcsWndControl,
  OverbyteIcsUrl,
  OverbyteIcsHttpProt, ComCtrls, xmldom, XMLIntf, msxmldom, XMLDoc;

type
  TRestDemoForm = class(TForm)
    DisplayMemo: TMemo;
    Splitter1: TSplitter;
    HttpCli1: THttpCli;
    PageControl1: TPageControl;
    GoogleTabSheet: TTabSheet;
    ToolsGooglePanel: TPanel;
    GoogleGetButton: TButton;
    AddressListBox: TListBox;
    DataStringGrid: TStringGrid;
    YahooTabSheet: TTabSheet;
    ToolsYahooPanel: TPanel;
    YahooGetButton: TButton;
    XMLDocument1: TXMLDocument;
    Image1: TImage;
    LocationEdit: TEdit;
    Label1: TLabel;
    ZoomComboBox: TComboBox;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GoogleGetButtonClick(Sender: TObject);
    procedure YahooGetButtonClick(Sender: TObject);
    procedure ZoomComboBoxChange(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FGoogleApiKey : String;
    FYahooApiKey  : String;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  RestDemoForm: TRestDemoForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyGoogleApiKey    = 'GoogleApiKey';
    KeyYahooApiKey     = 'YahooApiKey';
    KeyYahooLocation   = 'YahooLocation';
    KeyYahooZoom       = 'YahooZoom';
    KeyDisplayHeight   = 'DisplayHeight';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.FormCreate(Sender: TObject);
begin
    FIniFileName  := ChangeFileExt(Application.ExeName, '.ini');
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        FGoogleApiKey := Trim(IniFile.ReadString(SectionData, KeyGoogleApiKey, ''));
        FYahooApiKey  := Trim(IniFile.ReadString(SectionData, KeyYahooApiKey, ''));
        LocationEdit.Text := IniFile.ReadString(SectionData, KeyYahooLocation, 'Embourg,Belgium');
        ZoomComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyYahooZoom, 0);
        DisplayMemo.Height := IniFile.ReadInteger(SectionData, KeyDisplayHeight, 50);
        IniFile.Destroy;
        DisplayMemo.Clear;
        // We need an API key to access Google services.
        // It is free of charge, just ask one at Google web site.
        if FGoogleApiKey = '' then begin
            Display('Google API key not found in INI file');
            FGoogleApiKey := Trim(InputBox('Enter your Google API key',
                                     'Google API key', ''));
            if FGoogleApiKey = '' then begin
                Application.MessageBox(
                    'To get an API key, visit' + #10 +
                    'http://code.google.com/intl/fr-BE/apis/base/signup.html',
                    'Missing Google API key',
                    MB_OK);
                Application.Terminate;
            end;
            IniFile      := TIniFile.Create(FIniFileName);
            IniFile.WriteString(SectionData, KeyGoogleApiKey, FGoogleApiKey);
            IniFile.Destroy;
            DisplayMemo.Clear;
            Display('Google API key is "' + FGoogleApiKey + '"');
        end;
        // We need an API key to access Yahoo services.
        // It is free of charge, just ask one at Yahoo web site.
        if FYahooApiKey = '' then begin
            Display('Yahoo API key not found in INI file');
            FYahooApiKey := Trim(InputBox('Enter your Yahoo API key',
                                     'Yahoo API key', ''));
            if FYahooApiKey = '' then begin
                Application.MessageBox(
                    'To get an API key, visit' + #10 +
                    'http://developer.yahoo.com/maps/',
                    'Missing Yahoo API key',
                    MB_OK);
                Application.Terminate;
            end;
            IniFile      := TIniFile.Create(FIniFileName);
            IniFile.WriteString(SectionData, KeyYahooApiKey, FYahooApiKey);
            IniFile.Destroy;
            DisplayMemo.Clear;
            Display('Yahoo API key is "' + FYahooApiKey + '"');
        end;
        DataStringGrid.RowCount     := 2;
        DataStringGrid.FixedRows    := 1;
        DataStringGrid.ColCount     := 3;
        DataStringGrid.FixedCols    := 0;
        DataStringGrid.ColWidths[0] := 300;
        DataStringGrid.ColWidths[1] := 100;
        DataStringGrid.ColWidths[2] := 100;
        DataStringGrid.Cells[0, 0]  := 'Location';
        DataStringGrid.Cells[1, 0]  := 'Latitude';
        DataStringGrid.Cells[2, 0]  := 'Longitude';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.WriteString(SectionData, KeyYahooLocation, LocationEdit.Text);
        IniFile.WriteInteger(SectionData, KeyYahooZoom, ZoomComboBox.ItemIndex);
        IniFile.WriteInteger(SectionData, KeyDisplayHeight, DisplayMemo.Height);
    finally
        IniFile.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.GoogleGetButtonClick(Sender: TObject);
const
    Zero : byte = 0;
var
    DataStream  : TMemoryStream;
    DataList    : TStringList;
    StrResponse : String;
    Item        : Integer;
begin
    GoogleGetButton.Enabled := FALSE;
    DataStream              := TMemoryStream.Create;
    DataList                := TStringList.Create;
    try
        DataStringGrid.RowCount := Max(AddressListBox.Items.Count + 1, 2);
        for Item := 0 to AddressListBox.Items.Count - 1 do begin
            DataStream.Clear;
            Display(AddressListBox.Items[Item]);
            // Doc: http://code.google.com/intl/fr-BE/apis/maps/documentation/geocoding/
            HttpCli1.URL := 'http://maps.google.com/maps/geo?' +
                            'q=' + UrlEncode(AddressListBox.Items[Item]) +
                            '&output=csv&key=' + UrlEncode(FGoogleApiKey);
            HttpCli1.RcvdStream := DataStream;
            try
                HttpCli1.Get;
            except
                on E:Exception do Display('Failed. ' +
                                          E.ClassName + ': ' + E.Message);
            end;
            if HttpCli1.StatusCode <> 200 then begin
                Display('  => Failed ' + IntToStr(HttpCli1.StatusCode) + ' ' +
                        HttpCli1.ReasonPhrase);
                DataStringGrid.Cells[0, Item + 1] := AddressListBox.Items[Item];
                DataStringGrid.Cells[1, Item + 1] := 'failed';
                DataStringGrid.Cells[2, Item + 1] := HttpCli1.ReasonPhrase;
            end
            else begin
                DataStream.Write(Zero, 1);
                StrResponse := String(PAnsiChar(DataStream.Memory));
                DataList.Clear;
                DataList.LineBreak := ',';
                DataList.Text      := StrResponse;
                Display(DataList.Strings[2] + ', ' + DataList.Strings[3]);
                DataStringGrid.Cells[0, Item + 1] := AddressListBox.Items[Item];
                DataStringGrid.Cells[1, Item + 1] := DataList.Strings[2];
                DataStringGrid.Cells[2, Item + 1] := DataList.Strings[3];
            end;
        end;
    finally
        FreeAndNil(DataList);
        FreeAndNil(DataStream);
        GoogleGetButton.Enabled := TRUE;
        Display('Done');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.YahooGetButtonClick(Sender: TObject);
const
    Zero : byte = 0;
var
    DataStream   : TMemoryStream;
    ImageStream  : TFileStream;
    StrResponse  : String;
    TempFileName : String;
begin
    YahooGetButton.Enabled := FALSE;
    DataStream              := TMemoryStream.Create;
    try
        Image1.Picture := nil;
        DataStream.Clear;
        Display('Searching ' + LocationEdit.Text);
        // Documentation: http://developer.yahoo.com/maps/rest/V1/
        HttpCli1.URL := 'http://api.local.yahoo.com/MapsService/V1/mapImage?' +
                        '&location=' + UrlEncode(LocationEdit.Text) +
                        '&zoom=' + ZoomComboBox.Text +
                        '&image_height=' + IntToStr(Image1.Height) +
                        '&image_width=' + IntToStr(Image1.width) +
                        '&appid=' + UrlEncode(FYahooApiKey);
        HttpCli1.RcvdStream := DataStream;
        try
            HttpCli1.Get;
        except
            on E:Exception do Display('Failed. ' +
                                      E.ClassName + ': ' + E.Message);
        end;
        if HttpCli1.StatusCode <> 200 then begin
            Display('  => Failed ' + IntToStr(HttpCli1.StatusCode) + ' ' +
                    HttpCli1.ReasonPhrase);
        end
        else begin
            DataStream.Write(Zero, 1);
            StrResponse := String(PAnsiChar(DataStream.Memory));
            XMLDocument1.Active := False;
            XMLDocument1.XML.Text := StrResponse;
            XMLDocument1.Active := True;
            StrResponse := XMLDocument1.DocumentElement.NodeValue;
            XMLDocument1.Active := False;

            TempFileName := 'Temp.png';
            ImageStream := TFileStream.Create(TempFileName, fmCreate);
            try
                HttpCli1.RcvdStream := ImageStream;
                HttpCli1.URL := StrResponse;
                HttpCli1.Get;
            finally
                FreeAndNil(ImageStream);
            end;
            if HttpCli1.StatusCode <> 200 then begin
                Display('  => Failed ' + IntToStr(HttpCli1.StatusCode) +
                        HttpCli1.ReasonPhrase);
            end
            else begin
                Image1.Picture.LoadFromFile(TempFileName);
            end;
        end;
    finally
        FreeAndNil(DataStream);
        YahooGetButton.Enabled := TRUE;
        Display('Done');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestDemoForm.ZoomComboBoxChange(Sender: TObject);
begin
    YahooGetButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
