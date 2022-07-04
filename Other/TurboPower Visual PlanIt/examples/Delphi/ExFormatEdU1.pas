{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit ExFormatEdU1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, ExtCtrls,

  VpPrtFmt, VpBase, VpBaseDS, VpDBDS, VpBDEDS, VpPrtPrv;

const
  BaseCaption = 'Print Format Designer';
  FileCaption = BaseCaption + ' - %s';
  UnnamedFile = '<Unnamed>';

type
  TfrmPrnFormat = class(TForm)
    btnClose: TButton;
    btnDeleteElement: TButton;
    btnDeleteFormat: TButton;
    btnEditElement: TButton;
    btnEditFormat: TButton;
    btnLoadFile: TButton;
    btnNewElement: TButton;
    btnNewFile: TButton;
    btnNewFormat: TButton;
    btnSaveFile: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lbElements: TListBox;
    lbFormats: TListBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PrintPreview: TVpPrintPreview;
    SaveDialog1: TSaveDialog;
    VpControlLink: TVpControlLink;
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteElementClick(Sender: TObject);
    procedure btnDeleteFormatClick(Sender: TObject);
    procedure btnEditElementClick(Sender: TObject);
    procedure btnEditFormatClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnNewElementClick(Sender: TObject);
    procedure btnNewFileClick(Sender: TObject);
    procedure btnNewFormatClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbFormatsClick(Sender: TObject);
  private
    FileName : string;
    FLocalControlLink : TVpControlLink;
    IsDirty : Boolean;

    function DirtyPrompt: Integer;
    procedure DoEditElement;
    procedure DoEditFormat;
    procedure DoNewElement;
    procedure DoNewFile;
    procedure DoNewFormat;
    procedure DoSave;
    procedure EnableElementButtons(Enable: Boolean);
    procedure EnableFormatButtons(Enable: Boolean);
    procedure UpdateCaption;

    function GetControlLink: TVpControlLink;
    procedure SetControlLink(const Value: TVpControlLink);
    { Private declarations }
  public
    property LocalControlLink : TVpControlLink
      read GetControlLink write SetControlLink;

    procedure Execute;
    { Public declarations }
  end;

var
  frmPrnFormat: TfrmPrnFormat;

implementation

uses
  EdFormat, EdElement;

{$R *.DFM}

{TfrmPrnFormat}

procedure TfrmPrnFormat.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  IsDirty := False;
  DoNewFile;
  UpdateCaption;
end;
{=====}
procedure TfrmPrnFormat.btnCloseClick(Sender: TObject);
begin
  Close;
end;
{=====}
procedure TfrmPrnFormat.btnDeleteElementClick(Sender: TObject);
var
  Format : TVpPrintFormatItem;
  Idx : Integer;
  Item : string;
begin
  Format := TVpPrintFormatItem(lbFormats.Items.Objects[lbFormats.ItemIndex]);
  Item := '';
  if lbElements.ItemIndex > -1 then
    Item := lbElements.Items[lbElements.ItemIndex];

  if Item <> '' then begin
    for Idx := Pred(Format.Elements.Count) downto 0 do begin
      if Format.Elements.Items[Idx].ElementName = Item then begin
        Format.Elements.Items[Idx].Free;
        lbElements.Items.Delete(lbElements.ItemIndex);
        IsDirty := True;
      end;
    end;
  end;
end;
{=====}
procedure TfrmPrnFormat.btnDeleteFormatClick(Sender: TObject);
var
  Prn : TVpPrinter;
  Idx : Integer;
begin
  Prn := LocalControlLink.Printer;
  Idx := Prn.Find(lbFormats.Items[lbFormats.ItemIndex]);
  Prn.PrintFormats.Items[Idx].Free;
  lbFormats.Items.Delete(lbFormats.ItemIndex);
  IsDirty := True;
end;
{=====}
procedure TfrmPrnFormat.btnEditElementClick(Sender: TObject);
begin
  DoEditElement;
end;
{=====}
procedure TfrmPrnFormat.btnEditFormatClick(Sender: TObject);
begin
  DoEditFormat;
end;
{=====}
procedure TfrmPrnFormat.btnLoadFileClick(Sender: TObject);
var
  Prn : TVpPrinter;
  i, Rslt : Integer;
begin
  if IsDirty then begin
    Rslt := DirtyPrompt;

    case Rslt of
      ID_YES: begin
        DoSave;
      end;

      ID_NO: begin
        // nothing
      end;

      ID_CANCEL: Exit;
    end;
  end;

  if OpenDialog1.Execute then begin
    FileName := OpenDialog1.FileName;
    lbFormats.Items.Clear;
    Prn := LocalControlLink.Printer;
    Prn.LoadFromFile(FileName, False);
    for i := 0 to Pred(Prn.PrintFormats.Count) do
      lbFormats.Items.AddObject(Prn.PrintFormats.Items[i].FormatName, Prn.PrintFormats.Items[i]);
    UpdateCaption;
  end;
end;
{=====}
procedure TfrmPrnFormat.btnNewElementClick(Sender: TObject);
begin
  DoNewElement;
end;
{=====}
procedure TfrmPrnFormat.btnNewFormatClick(Sender: TObject);
begin
  DoNewFormat;
end;
{=====}
procedure TfrmPrnFormat.btnNewFileClick(Sender: TObject);
var
  Rslt : Integer;
begin
  if IsDirty then begin
    Rslt := DirtyPrompt;

    case Rslt of
      ID_YES: begin
        DoSave;
        DoNewFile;
      end;

      ID_NO: begin
        DoNewFile;
      end;

      ID_CANCEL: Exit;
    end;
  end
  else
    DoNewFile;
end;
{=====}
procedure TfrmPrnFormat.btnSaveFileClick(Sender: TObject);
begin
  DoSave;
end;
{=====}
function TfrmPrnFormat.DirtyPrompt : Integer;
begin
  Result := Application.MessageBox(
      PChar('Save changes to ' + FileName + '?'),
      PChar('Inquiry'),
      MB_YESNOCANCEL or MB_ICONQUESTION);
end;
{=====}
procedure TfrmPrnFormat.DoEditElement;
var
  E : TVpPrintFormatElementItem;
begin
  if lbElements.ItemIndex > -1 then begin
    E := TVpPrintFormatElementItem(lbElements.Items.Objects[lbElements.ItemIndex]);
    if frmEditElement.Execute(E) then begin
      IsDirty := True;
    end;
  end
  else begin
    DoNewElement;
  end;
end;
{=====}
procedure TfrmPrnFormat.DoEditFormat;
var
  AFormat : TVpPrintFormatItem;
begin
  if lbFormats.ItemIndex > -1 then begin
    AFormat := TVpPrintFormatItem(lbFormats.Items.Objects[lbFormats.ItemIndex]);
    if frmEditFormat.Execute(AFormat) then begin
      IsDirty := True;
    end;
  end
  else begin
    DoNewFormat;
  end;
end;
{=====}
procedure TfrmPrnFormat.DoNewElement;
var
  Format : TVpPrintFormatItem;
  E : TVpPrintFormatElementItem;
  Unique, Cancelled : Boolean;
begin
  Format := TVpPrintFormatItem(lbFormats.Items.Objects[lbFormats.ItemIndex]);


  Unique := False;
  repeat
    E := TVpPrintFormatElementItem.Create(Format.Elements);

    { edit Element }
    Cancelled := not frmEditElement.Execute(E);

    if not Cancelled then begin
      if lbElements.Items.IndexOf(E.ElementName) > -1 then begin
        ShowMessage('An Element named ' + E.ElementName + ' already exists.' +
          #13#10 + 'Please use another name.');
        { dump empty element }
        Format.Elements.Items[E.Index].Free;
        Unique := False;
      end
      else begin
        lbElements.Items.AddObject(E.ElementName, E);
        IsDirty := True;
        Unique := True;
      end;
    end else
      { dump empty element }
      Format.Elements.Items[E.Index].Free;
  { until element name is Unique or operation Cancelled }
  until Unique or Cancelled;
end;
{=====}
procedure TfrmPrnFormat.DoNewFile;
var
  Prn : TVpPrinter;
begin
  Prn := LocalControlLink.Printer;
  Prn.PrintFormats.Clear;
  lbFormats.Clear;
  lbElements.Clear;
  FileName := UnnamedFile;
  IsDirty := False;
  PrintPreview.ControlLink := nil;
  EnableFormatButtons(False);
  btnNewFormat.Enabled := True;
  EnableElementButtons(False);
end;
{=====}
procedure TfrmPrnFormat.DoNewFormat;
var
  AFormat : TVpPrintFormatItem;
  Prn : TVpPrinter;
  Unique, Cancelled : Boolean;
begin
  Prn := LocalControlLink.Printer;

  Unique := False;
  repeat
    AFormat := TVpPrintFormatItem.Create(Prn.PrintFormats);
    { edit format }
    Cancelled := not frmEditFormat.Execute(AFormat);

    if not Cancelled then begin
      if lbFormats.Items.IndexOf(AFormat.FormatName) > -1 then begin
        ShowMessage('A format named ' + AFormat.FormatName + ' already exists.' +
          #13#10 + 'Please use another name.');
        { dump empty format }
        Prn.PrintFormats.Items[AFormat.Index].Free;
        Unique := False;
      end
      else begin
        lbFormats.Items.AddObject(AFormat.FormatName, AFormat);
        IsDirty := True;
        Unique := True;
      end;
    end else
      { dump empty format }
      Prn.PrintFormats.Items[AFormat.Index].Free;
  { until format name is Unique or operation Cancelled }
  until Unique or Cancelled;
end;
{=====}
procedure TfrmPrnFormat.DoSave;
begin
  if FileName <> UnnamedFile then
    SaveDialog1.FileName := FileName
  else
    SaveDialog1.FileName := 'Unnamed.xml';
  if SaveDialog1.Execute then begin
    FileName := SaveDialog1.FileName;
    LocalControlLink.Printer.SaveToFile(FileName);
    IsDirty := False;
    UpdateCaption;
  end;
end;
{=====}
procedure TfrmPrnFormat.EnableElementButtons(Enable : Boolean);
begin
  btnNewElement.Enabled := Enable;
  btnEditElement.Enabled := Enable;
  btnDeleteElement.Enabled := Enable;
end;
{=====}
procedure TfrmPrnFormat.EnableFormatButtons(Enable : Boolean);
begin
  btnNewFormat.Enabled := Enable;
  btnEditFormat.Enabled := Enable;
  btnDeleteFormat.Enabled := Enable;
end;
{=====}
procedure TfrmPrnFormat.Execute;
begin
  ShowModal;
end;
{=====}
procedure TfrmPrnFormat.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Rslt : Integer;
begin
  if IsDirty then begin
    Rslt := DirtyPrompt;

    case Rslt of
      ID_YES: begin
        DoSave;
        CanClose := True;
      end;

      ID_NO: begin
        CanClose := True;
      end;

      ID_CANCEL: begin
        CanClose := False;
        Exit;
      end;
    end;
  end
  else
    CanClose := True;
end;
{=====}
function TfrmPrnFormat.GetControlLink: TVpControlLink;
begin
  if not Assigned(FLocalControlLink) then begin
    FLocalControlLink := VpControlLink;
  end;
  Result := FLocalControlLink;
end;
{=====}
procedure TfrmPrnFormat.lbFormatsClick(Sender: TObject);
var
  E : TVpPrintFormatElementItem;
  Prn : TVpPrinter;
  i, Idx : Integer;
begin
  lbElements.Items.Clear;
  Prn := LocalControlLink.Printer;
  Idx := Prn.Find(lbFormats.Items[lbFormats.ItemIndex]);

  Prn.CurFormat := Idx;

  PrintPreview.ControlLink := LocalControlLink;

  for i := 0 to Pred(Prn.PrintFormats.Items[Idx].Elements.Count) do begin
    E := Prn.PrintFormats.Items[Idx].Elements.Items[i];
    lbElements.Items.AddObject(E.ElementName, E);
  end;

  EnableElementButtons(False);
  btnNewElement.Enabled := True;
  EnableFormatButtons(True);
end;
{=====}
procedure TfrmPrnFormat.lbElementsClick(Sender: TObject);
begin
  EnableElementButtons(True);
end;
{=====}
procedure TfrmPrnFormat.SetControlLink(const Value: TVpControlLink);
begin
  if LocalControlLink <> Value then begin
    if not Assigned(Value) then
      FLocalControlLink := VpControlLink
    else
      FLocalControlLink := Value;
  end;
end;
{=====}
procedure TfrmPrnFormat.UpdateCaption;
begin
  Caption := Format(FileCaption, [FileName]);
end;
{=====}

end.


 
