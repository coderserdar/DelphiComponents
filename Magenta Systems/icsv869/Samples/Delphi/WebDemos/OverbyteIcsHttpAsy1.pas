{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     January 31, 1998
Version:      6.00
Description:  This program shows how to use the THttpCli component to execute
              a list of request sequentialy or simultaneously.
              Simultaneous request are possible without using threads because
              the THttpCli component is fully event-driven and asynchronous.
              Data is not stored. See the HttpTst sample program for an
              example of data store. We keep this sample as simple as possible.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1998-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. 

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

Updates:
Feb 01, 1998 V1.01 Adedpted to be compatible with Delphi 1
             Save window position and size to ini file.
Jan 10, 2004 V1.02 Steve Endicott <Endi@pacbell.net> added a feature to
             execute head of get at will. Just add 'head ' in front of the URL
             to execute a head request. Just the URL will result in a get.
Jul 16, 2008 V6.00 A. Garrels made it ICS V6 compatible.			 
Apr 6, 2021  V8.67 Must select something to edit it.


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpAsy1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsHttpProt, OverbyteIcsIniFiles,
  OverbyteIcsWndControl;

const
  HttpAsyVersion         = 600;
  CopyRight : String     = 'OverbyteIcsHttpTst (c) 1998-2010 Francois Piette  V6.00 ';

type
  THttpAsyForm = class(TForm)
    URLListBox: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    URLEdit: TEdit;
    AddButton: TButton;
    Panel2: TPanel;
    ExecButton: TButton;
    RemoveButton: TButton;
    DisplayMemo: TMemo;
    HttpCli1: THttpCli;
    ReplaceButton: TButton;
    ClearDisplayButton: TButton;
    HeaderCheckBox: TCheckBox;
    DataCheckBox: TCheckBox;
    SimultCheckBox: TCheckBox;
    AbortButton: TButton;
    Label7: TLabel;
    Label8: TLabel;
    HttpVersionComboBox: TComboBox;
    procedure AddButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RemoveButtonClick(Sender: TObject);
    procedure ExecButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      Error: Word);
    procedure URLListBoxClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure ClearDisplayButtonClick(Sender: TObject);
    procedure HttpCli1HeaderData(Sender: TObject);
    procedure HttpCli1DocData(Sender: TObject; Buffer: Pointer;
      Len: Integer);
    procedure AbortButtonClick(Sender: TObject);
  private
    FInitialized  : Boolean;
    FIniFileName  : String;
    FCurrentItem  : Integer;
    FHttpCliList  : TList;      { For simultaneous requests }
    FFlagAbort    : Boolean;
    procedure StartNext;        { For sequential requests }
    procedure ExecSimultaneous;
    procedure ExecSequential;
    procedure HttpCliItemRequestDone(Sender: TObject; { For simult. requests }
                  RqType: THttpRequest; Error: Word);
  end;

var
  HttpAsyForm: THttpAsyForm;

implementation

{$R *.DFM}

const
    SectionData   = 'Data';
    KeyURL        = 'URLEdit';
    KeyCount      = 'Count';
    KeyList       = 'List';
    KeySimult     = 'Simultaneous';
    KeyHeader     = 'DisplayHeader';
    KeyData       = 'DisplayData';
    KeyHttpVer    = 'HttpVer';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.FormCreate(Sender: TObject);
begin
    DisplayMemo.Clear;
    FCurrentItem := -1;
    FIniFileName := GetIcsIniFileName;
    FHttpCliList := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    Count   : Integer;
    I       : Integer;
    URL     : String;
begin
    if not FInitialized then begin
        FInitialized           := TRUE;
        IniFile                := TIcsIniFile.Create(FIniFileName);
        URLEdit.Text           := IniFile.ReadString(SectionData, KeyURL, '');
        HeaderCheckBox.Checked := (IniFile.ReadInteger(SectionData, KeyHeader, 0) <> 0);
        DataCheckBox.Checked   := (IniFile.ReadInteger(SectionData, KeyData,   0) <> 0);
        SimultCheckBox.Checked := (IniFile.ReadInteger(SectionData, KeySimult,   0) <> 0);
        Count                  := IniFile.ReadInteger(SectionData, KeyCount, 0);
        for I := 1 to Count do begin
            URL := IniFile.ReadString(SectionData, KeyList + IntToStr(I), '');
            if URL <> '' then
                URLListBox.Items.Add(URL);
        end;
        HttpVersionComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyHttpVer, 0);
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    I       : Integer;
    Count   : Integer;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyURL,  URLEdit.Text);
    IniFile.WriteInteger(SectionData, KeyHeader, Ord(HeaderCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyData,   Ord(DataCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeySimult, Ord(SimultCheckBox.Checked));
    Count := URLListBox.Items.Count;
    IniFile.WriteInteger(SectionData, KeyCount, Count);
    for I := 1 to Count do
        IniFile.WriteString(SectionData, KeyList + IntToStr(I),
                            URLListBox.Items[I - 1]);
    IniFile.WriteInteger(SectionData,   KeyHttpVer,   HttpVersionComboBox.ItemIndex);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Add an URL to the URL list box                                            }
procedure THttpAsyForm.AddButtonClick(Sender: TObject);
begin
    if Trim(URLEdit.Text) <> '' then
        URLListBox.Items.Add(URLEdit.Text);
    ActiveControl := URLEdit;
    URLEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Remove the selected URL from hte URL listbox                              }
procedure THttpAsyForm.RemoveButtonClick(Sender: TObject);
var
    Item : Integer;
begin
    Item := URLListBox.ItemIndex;
    if Item >= 0 then
        URLListBox.Items.Delete(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Execute button click handler. Start sequential or simultaneous execution  }
procedure THttpAsyForm.ExecButtonClick(Sender: TObject);
begin
    ExecButton.Enabled := FALSE;
    FFLagAbort         := FALSE;
    if SimultCheckBox.Checked then
        ExecSimultaneous
    else
        ExecSequential;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Start simultaneous execution by creating as much THttpCli components as   }
{ there are URL in the list box. (We could alternatively limit the number   }
{ of THttpCli components and use them sequentially. It would be a           }
{ combination of the sequential and simultaneous implementations.)          }
{ A TList is used to keep track of all the component created. This is only  }
{ needed to abort each one if requested by the user.                        }
{ The Tag property is used to store the item number, just for display.      }
procedure THttpAsyForm.ExecSimultaneous;
var
    Count    : Integer;
    Item     : Integer;
    AHttpCli : THttpCli;
    DoHead   : Boolean;
begin
    { Check if the list if empty }
    if FHttpCliList.Count > 0 then begin
        MessageBeep(MB_OK);
        Exit;
    end;

    { Get the URL count }
    Count := URLListBox.Items.Count;
    if Count <= 0 then
        Exit;  { Nothing to do ! }

    { Create a new HTTP component for each URL, }
    { add it to the list and start the request  }
    for Item := 1 to Count do begin
        AHttpCli := THttpCli.Create(Self);
        FHttpCliList.Add(AHttpCli);
        AHttpCli.Tag           := Item;
        AHttpCli.RequestVer    := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        DoHead                 := LowerCase(Copy(URLListBox.Items[Item - 1], 1, 5)) = 'head ';
        if DoHead then
            AHttpCli.Url := Trim(Copy(URLListBox.Items[Item - 1], 6, 999))
        else
            AHttpCli.URL := URLListBox.Items[Item - 1];
        AHttpCli.OnRequestDone := HttpCliItemRequestDone;
        AHttpCli.OnHeaderData  := HttpCli1HeaderData;
        AHttpCli.OnDocData     := HttpCli1DocData;
        DisplayMemo.Lines.Add('Start item ' + IntToStr(Item) + ': ' +
                              AHttpCli.Url);
        if DoHead then
            AHttpCli.HeadAsync
        else
            AHttpCli.GetAsync;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This OnRequestDone handler is used for the simultaneous request model.    }
{ It search the THttpCli component in the list and remove it.               }
procedure THttpAsyForm.HttpCliItemRequestDone(
  Sender : TObject;
  RqType : THttpRequest;
  Error  : Word);
var
    Item     : Integer;
    AHttpCli : THttpCli;
    Count    : Integer;
begin
    AHttpCli := Sender as THttpCli;
    Item     := AHttpCli.Tag;
    DisplayMemo.Lines.Add(
        'Finished Item ' + IntToStr(Item) +
        ' StatusCode = ' + IntToStr(AHttpCli.StatusCode) +
        ' ' + AHttpCli.URL +
        ' Error = ' + IntToStr(Error));
    DisplayMemo.Lines.Add('');

    { Remove the item form the list }
    Count := FHttpCliList.Count;
    for Item := 1 to Count do begin
        if AHttpCli = FHttpCliList.Items[Item - 1] then begin
            FHttpCliList.Delete(Item - 1);
            break;
        end;
    end;

    { Free the item }
    AHttpCli.Free;

    { Check if the list is empty. If yes, we have all requests finished. }
    if FHttpCliList.Count <= 0 then begin
        ExecButton.Enabled := TRUE;
        DisplayMemo.Lines.Add('All Finished');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.ExecSequential;
begin
    if FCurrentItem >= 0 then begin
        MessageBeep(MB_OK);
        Exit;
    end;
    FCurrentItem := 0;
    StartNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Start the next request (this could also be the first).                    }
procedure THttpAsyForm.StartNext;
Var DoHead : Boolean;
begin
    if FFlagAbort then begin
        DisplayMemo.Lines.Add('Abort requested');
        FCurrentItem       := -1;
        ExecButton.Enabled := TRUE;
        Exit;
    end;

    Inc(FCurrentItem);
    if FCurrentItem > URLListBox.Items.Count then begin
        DisplayMemo.Lines.Add('All Finished');
        FCurrentItem       := -1;
        ExecButton.Enabled := TRUE;
        Exit;
    end;
    HttpCli1.RequestVer := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
    HttpCli1.Tag        := FCurrentItem;
    DoHead              := LowerCase(Copy(URLListBox.Items[FCurrentItem - 1], 1, 5)) = 'head ';
    if DoHead then
        HttpCli1.Url := Trim(Copy(URLListBox.Items[FCurrentItem - 1], 6, 999))
    else
        HttpCli1.Url        := URLListBox.Items[FCurrentItem - 1];
    DisplayMemo.Lines.Add('Start item ' + IntToStr(FCurrentItem) + ': ' +
                          HttpCli1.Url);
    if DoHead then
        HttpCli1.HeadASync
    else
        HttpCli1.GetASync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This OnRequestDone event handler is called during the sequential model.   }
{ Just start the next request.                                              }
procedure THttpAsyForm.HttpCli1RequestDone(Sender: TObject;
  RqType: THttpRequest; Error: Word);
begin
    DisplayMemo.Lines.Add('Finished item ' + IntToStr(FCurrentItem) +
                          ' StatusCode = ' + IntToStr(HttpCli1.StatusCode) +
                          ' Error = ' + IntToStr(Error));
    DisplayMemo.Lines.Add('');
    StartNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.URLListBoxClick(Sender: TObject);
var
    Item : Integer;
begin
    Item := URLListBox.ItemIndex;    { V8.67 must select something }
    if Item < 0 then
        Exit;
    UrlEdit.Text := UrlListBox.Items[Item];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.ReplaceButtonClick(Sender: TObject);
var
    Item : Integer;
begin
    Item := URLListBox.ItemIndex;
    if Item < 0 then
        Exit;
    URLListBox.Items.Delete(Item);
    URLListBox.Items.Insert(Item, UrlEdit.Text);
    URLListBox.ItemIndex := Item;
    ActiveControl := URLEdit;
    URLEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpAsyForm.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This OnHeaderData event handler is used in both sequential and            }
{ simultaneous requests models.                                             }
procedure THttpAsyForm.HttpCli1HeaderData(Sender: TObject);
var
    AHttpCli : THttpCli;
begin
    if not HeaderCheckBox.Checked then
        Exit;
    AHttpCli := Sender as THttpCli;
    DisplayMemo.Lines.Add('Item ' + IntToStr(AHttpCli.Tag) + ': ' +
                          AHttpCli.LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This OnDocData event handler is used in both sequential and               }
{ simultaneous requests models.                                             }
procedure THttpAsyForm.HttpCli1DocData(Sender: TObject; Buffer: Pointer;
  Len: Integer);
var
    AHttpCli : THttpCli;
begin
    if not DataCheckBox.Checked then
        Exit;

    AHttpCli := Sender as THttpCli;
    { Display a message stating that data is available }
    DisplayMemo.Lines.Add('Item ' + IntToStr(AHttpCli.Tag) + ' Data');

    { We could display the data, but it use a huge space in the display }
    { DisplayMemo.Lines.Add(StrPas(Buffer)); }

    { We could also store the data somewhere (with the help of OnDocBegin }
    { and OnDocEnd events. Or using the RcvdStream property.              }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Abort all the running request.                                            }
{ In the simultaneous model, we use the list to abort all.                  }
{ We just need to call abort. We will get the OnRequestDone event with an   }
{ error code stating that the request has been aborted.                     }
procedure THttpAsyForm.AbortButtonClick(Sender: TObject);
var
    Count    : Integer;
    Item     : Integer;
    AHttpCli : THttpCli;
begin
    FFLagAbort := TRUE;
    if SimultCheckBox.Checked then begin
        Count := FHttpCliList.Count;
        for Item := 1 to Count do begin
            AHttpCli := THttpCli(FHttpCliList.Items[Item - 1]);
            AHttpCli.Abort;
        end;
    end
    else
        HttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

