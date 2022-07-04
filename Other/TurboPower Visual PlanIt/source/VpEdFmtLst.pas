{*********************************************************}
{*                VPEDFMTLST.PAS 1.03                    *}
{*********************************************************}

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

{$I Vp.INC}

unit VpEdFmtLst;

interface

uses     
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, ExtCtrls,

  VpPrtFmt, VpBase, VpBaseDS, VpDBDS, VpBDEDS, VpPrtPrv, Buttons,
  VpException, VpSR;

const
  BaseCaption = 'Print Format Designer';
  FileCaption = BaseCaption + ' - %s';
  UnnamedFile = '<Unnamed>';

type
  TfrmPrnFormat = class(TForm)
    btnDeleteElement: TButton;
    btnDeleteFormat: TButton;
    btnEditElement: TButton;
    btnEditFormat: TButton;
    btnLoadFile: TButton;
    btnMoveElementDn: TSpeedButton;
    btnMoveElementUp: TSpeedButton;
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
    btnOk: TButton;
    Label3: TLabel;
    procedure btnDeleteElementClick(Sender: TObject);
    procedure btnDeleteFormatClick(Sender: TObject);
    procedure btnEditElementClick(Sender: TObject);
    procedure btnEditFormatClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnMoveElementDnClick(Sender: TObject);
    procedure btnMoveElementUpClick(Sender: TObject);
    procedure btnNewElementClick(Sender: TObject);
    procedure btnNewFileClick(Sender: TObject);
    procedure btnNewFormatClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbFormatsClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbElementsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbElementsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbElementsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FFormatFileName : string;                                          
    FControlLink : TVpControlLink;
    IsDirty : Boolean;
    LastX, LastY: Integer;
    DragItem : Integer;
  protected
    function DirtyPrompt: Integer;
    procedure DoEditElement;
    procedure DoEditFormat;
    procedure DoNewElement;
    procedure DoNewFile;
    function DoNewFormat : Integer;                                                                                 
    procedure DoSave;
    procedure EnableElementButtons(Enable: Boolean);
    procedure EnableFormatButtons(Enable: Boolean);
    procedure EnableMoveButtons;                                       
    procedure SetFormatFileName (const v : string);                    
    procedure UpdateFormats;
    procedure UpdateCaption;
    procedure UpdatePreview;

    function GetControlLink: TVpControlLink;
    procedure SetControlLink(const Value: TVpControlLink);
    { Private declarations }
  public

    property ControlLink : TVpControlLink
      read FControlLink write SetControlLink;

    function Execute : Boolean;
    { Public declarations }
  published
    property FormatFileName : string                                   
             read FFormatFileName write SetFormatFileName;             
  end;

var
  frmPrnFormat: TfrmPrnFormat;

implementation

uses
  VpEdFmt, VpEdElem;

{$R *.DFM}

{TfrmPrnFormat}
procedure TfrmPrnFormat.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  IsDirty := False;
  FormatFileName := UnnamedFile;



  EnableFormatButtons(False);
  EnableElementButtons(False);
end;
{=====}
procedure TfrmPrnFormat.EnableMoveButtons;                             
begin                                                                  
  btnMoveElementUp.Enabled := lbElements.ItemIndex > 0;                
  btnMoveElementDn.Enabled :=                                          
      lbElements.ItemIndex < lbElements.Items.Count - 1;               
end;                                                                   
{=====}
procedure TfrmPrnFormat.FormShow(Sender: TObject);
begin
  PrintPreview.Parent := Panel1;

  if ControlLink.Printer.PrintFormats.Count > 0 then begin
    UpdateFormats;
  end
  else begin
    DoNewFile;
    UpdateCaption;
  end;
  btnNewFormat.Enabled := True;
  lbFormats.SetFocus;
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
        UpdatePreview;
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
  Prn := ControlLink.Printer;
  Idx := Prn.Find(lbFormats.Items[lbFormats.ItemIndex]);
  if (Idx < 0) or (Idx >= Prn.PrintFormats.Count) then                   
    ShowMessage ('Invalid print format: ' +                              
                 lbFormats.Items[lbFormats.ItemIndex]);                  
  Prn.PrintFormats.Items[Idx].Free;
  lbFormats.Items.Delete(lbFormats.ItemIndex);
  IsDirty := True;
  UpdatePreview;
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
  Rslt : Integer;
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
    FormatFileName := OpenDialog1.FileName;
    lbFormats.Items.Clear;
    Prn := ControlLink.Printer;
    Prn.LoadFromFile(FormatFileName, False);
    UpdateFormats;
    UpdateCaption;
  end;
end;
{=====}
procedure TfrmPrnFormat.btnMoveElementDnClick(Sender: TObject);
var
  E : TVpPrintFormatElementItem;
begin
  if lbElements.ItemIndex > -1 then begin
    E := TVpPrintFormatElementItem(lbElements.Items.Objects[lbElements.ItemIndex]);
    E.Index := E.Index + 1;
    lbElements.Items.Move(lbElements.ItemIndex, lbElements.ItemIndex + 1);
  end;
end;
{=====}
procedure TfrmPrnFormat.btnMoveElementUpClick(Sender: TObject);
var
  E : TVpPrintFormatElementItem;
begin
  if lbElements.ItemIndex > -1 then begin
    E := TVpPrintFormatElementItem(lbElements.Items.Objects[lbElements.ItemIndex]);
    E.Index := E.Index - 1;            
    lbElements.Items.Move(lbElements.ItemIndex, lbElements.ItemIndex - 1);
  end;
end;
{=====}
procedure TfrmPrnFormat.btnNewElementClick(Sender: TObject);
begin
  DoNewElement;
end;
{=====}
procedure TfrmPrnFormat.btnNewFormatClick(Sender: TObject);
var                                                                      
  NewFormatIdx : Integer;                                                
  i            : Integer;                                                

begin
  NewFormatIdx := DoNewFormat;                                           
  if (NewFormatIdx > 0) and                                              
     (Assigned (ControlLink)) and                                        
     (NewFormatIdx < ControlLink.Printer.PrintFormats.Count) then        
    for i := 0 to lbFormats.Items.Count - 1 do                           
      if lbFormats.Items[i] = ControlLink.Printer.PrintFormats.          
             Items[NewFormatIdx].FormatName then begin                   
        lbFormats.ItemIndex := i;                                        
        lbFormatsClick (Self);                                           
        Break;                                                           
      end;                                                               
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
procedure TfrmPrnFormat.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
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
      PChar('Save changes to ' + FormatFileName + '?'),
      PChar('Inquiry'),
      MB_YESNOCANCEL or MB_ICONQUESTION);
end;
{=====}
procedure TfrmPrnFormat.DoEditElement;
var
  E : TVpPrintFormatElementItem;
  frmEditElement: TfrmEditElement;
begin
  if lbElements.ItemIndex > -1 then begin
    Application.CreateForm(TfrmEditElement, frmEditElement);

    E := TVpPrintFormatElementItem(lbElements.Items.Objects[lbElements.ItemIndex]);
    if frmEditElement.Execute(E) then begin
      IsDirty := True;
    end;

    frmEditElement.Free;

    UpdatePreview;
  end
  else begin
    DoNewElement;
  end;
end;
{=====}
procedure TfrmPrnFormat.DoEditFormat;
var
  AFormat : TVpPrintFormatItem;
  frmEditFormat: TfrmEditFormat;
begin
  if lbFormats.ItemIndex > -1 then begin
    Application.CreateForm(TfrmEditFormat, frmEditFormat);
    AFormat := TVpPrintFormatItem(lbFormats.Items.Objects[lbFormats.ItemIndex]);
    if frmEditFormat.Execute(AFormat) then begin
      IsDirty := True;
    end;
    frmEditFormat.Free;

    UpdatePreview;
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
  frmEditElement: TfrmEditElement;
begin
  Format := TVpPrintFormatItem(lbFormats.Items.Objects[lbFormats.ItemIndex]);

  Unique := False;

  Application.CreateForm(TfrmEditElement, frmEditElement);

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
        lbElements.ItemIndex := E.Index;
        IsDirty := True;
        Unique := True;
        UpdatePreview;
      end;
    end else
      { dump empty element }
      Format.Elements.Items[E.Index].Free;
  { until element name is Unique or operation Cancelled }
  until Unique or Cancelled;

  frmEditElement.Free;
end;
{=====}
procedure TfrmPrnFormat.DoNewFile;
var
  Prn : TVpPrinter;
begin
  Prn := ControlLink.Printer;
  Prn.PrintFormats.Clear;
  lbFormats.Clear;
  lbElements.Clear;
  FormatFileName := UnnamedFile;
  IsDirty := False;
  PrintPreview.ControlLink := nil;
  EnableFormatButtons(False);
  btnNewFormat.Enabled := True;
  EnableElementButtons(False);
end;
{=====}
function TfrmPrnFormat.DoNewFormat : Integer;                            
var
  AFormat : TVpPrintFormatItem;
  Prn : TVpPrinter;
  Unique, Cancelled : Boolean;
  frmEditFormat: TfrmEditFormat;
begin
  Result := -1;                                                          
  Application.CreateForm(TfrmEditFormat, frmEditFormat);

  Prn := ControlLink.Printer;
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
        lbFormats.ItemIndex := AFormat.Index;
        UpdatePreview;
        IsDirty := True;
        Unique := True;
      end;
    end else
      { dump empty format }
      Prn.PrintFormats.Items[AFormat.Index].Free;
  { until format name is Unique or operation Cancelled }
  until Unique or Cancelled;

  if not Cancelled then                                                  
    Result := AFormat.Index;                                             

  frmEditFormat.Free;
end;
{=====}
procedure TfrmPrnFormat.DoSave;
begin
  if FormatFileName <> UnnamedFile then
    SaveDialog1.FileName := FormatFileName
  else
    SaveDialog1.FileName := 'Unnamed.xml';
  if SaveDialog1.Execute then begin
    FormatFileName := SaveDialog1.FileName;
    ControlLink.Printer.SaveToFile(FormatFileName);
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
//  btnMoveElementUp.Enabled := Enable;                                
//  btnMoveElementDn.Enabled := Enable;                                
  EnableMoveButtons;                                                   
end;
{=====}
procedure TfrmPrnFormat.EnableFormatButtons(Enable : Boolean);
begin
  btnNewFormat.Enabled := Enable;
  btnEditFormat.Enabled := Enable;
  btnDeleteFormat.Enabled := Enable;
end;
{=====}
function TfrmPrnFormat.Execute : Boolean;
begin
  if not Assigned (ControlLink) then
    raise EVpPrintFormatEditorError.Create (RSNoControlLink);

  Result := ShowModal = mrOk;
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
  Result := FControlLink;
end;
{=====}
procedure TfrmPrnFormat.lbFormatsClick(Sender: TObject);
var
  E : TVpPrintFormatElementItem;
  Prn : TVpPrinter;
  i, Idx : Integer;
begin
  lbElements.Items.Clear;
  Prn := ControlLink.Printer;
  Idx := Prn.Find(lbFormats.Items[lbFormats.ItemIndex]);

  Prn.CurFormat := Idx;

  PrintPreview.ControlLink := ControlLink;

  for i := 0 to Pred(Prn.PrintFormats.Items[Idx].Elements.Count) do begin
    E := Prn.PrintFormats.Items[Idx].Elements.Items[i];
    lbElements.Items.AddObject(E.ElementName, E);
  end;

  UpdatePreview;

  EnableElementButtons(False);
  btnNewElement.Enabled := True;
  EnableFormatButtons(True);
  EnableMoveButtons;                                                   
end;
{=====}
procedure TfrmPrnFormat.lbElementsClick(Sender: TObject);
begin
  EnableElementButtons(True);
end;
{=====}
procedure TfrmPrnFormat.lbElementsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LastX:=X;
  LastY:=Y;
  DragItem := (Sender as TListBox).ItemAtPos(Point(LastX, LastY),True);
end;
{=====}
procedure TfrmPrnFormat.lbElementsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  lb : TListBox;
  Dest: Integer;
  E : TVpPrintFormatElementItem;
begin
  lb := Source as TListBox;
  Dest:=lb.ItemAtPos(Point(X, Y),True);
  lb.Items.Move(DragItem, Dest);
  E := TVpPrintFormatElementItem(lbElements.Items.Objects[Dest]);
  E.Index := Dest;
  lb.ItemIndex := Dest;
  EnableMoveButtons;                                                   
end;
{=====}
procedure TfrmPrnFormat.lbElementsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  lb : TListBox;
begin
  lb := (Source as TListBox);
  lb.Canvas.DrawFocusRect(lb.ItemRect(lb.ItemAtPos(Point(LastX, LastY), True)));
  lb.Canvas.DrawFocusRect(lb.ItemRect(lb.ItemAtPos(Point(X, Y), True)));
  LastX := X;
  LastY := Y;
  Accept:=True;
end;
{=====}
procedure TfrmPrnFormat.SetControlLink(const Value: TVpControlLink);
begin
  if FControlLink <> Value then begin                                  
    FControlLink := Value;
    if Assigned (FControlLink) then                                    
      FFormatFileName := FControlLink.Printer.DefaultXMLFileName;      
  end;                                                                 
end;
{=====}
procedure TfrmPrnFormat.SetFormatFileName (const v : string);          
begin                                                                  
  if v <> FFormatFileName then begin                                   
    FFormatFileName := v;                                              
    if Assigned (FControlLink) then                                    
      FControlLink.Printer.DefaultXMLFileName := v;                    
  end;                                                                 
end;                                                                   
{=====}
procedure TfrmPrnFormat.UpdateCaption;
begin
  Caption := Format(FileCaption, [FormatFileName]);
end;
{=====}
procedure TfrmPrnFormat.UpdateFormats;
var
  i : Integer;
  Prn : TVpPrinter;
begin
  Prn := ControlLink.Printer;
  for i := 0 to Pred(Prn.PrintFormats.Count) do
    lbFormats.Items.AddObject(Prn.PrintFormats.Items[i].FormatName, Prn.PrintFormats.Items[i]);
  EnableMoveButtons;                                                   
end;
{=====}
procedure TfrmPrnFormat.UpdatePreview;
var
  Prn : TVpPrinter;
  Idx : Integer;                                                         
       
begin
  Prn := ControlLink.Printer;
  if lbFormats.ItemIndex > -1 then begin                                 
    Idx := Prn.Find (lbFormats.Items[lbFormats.ItemIndex]);              
    if Idx > - 1 then                                                    
    Prn.CurFormat := Idx;                                                
    {Prn.CurFormat := lbFormats.ItemIndex;                             }   
  end;                                                                                                                                                              
  Prn.NotifyLinked;
  EnableMoveButtons;                                                   
end;
{=====}


end.
  
