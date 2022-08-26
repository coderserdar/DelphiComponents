//----------------------------------------------------------------------------
// Unit Name: pb_Config
// Author:    Helli
// Date:      06.06.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_config;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Registry, ComCtrls, ImgList, CheckLst, ExtCtrls;

type
  TPaletteConfig = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    ConfigList: TTreeView;
    RadioGroup: TRadioGroup;
    BtnClear: TButton;
    procedure FormShow(Sender: TObject);
    procedure ConfigListClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
   procedure SetOptions;
  end;

var Config: TPaletteConfig;

implementation

{$R *.dfm}

uses pb_master, pb_Common;


const cCheckIcon = 22;

type TTrick = record
               case integer of
                0: (b1, b2, b3, b4: Byte);
                1: (lo, hi: SmallInt);
                2: (int: Integer;);
               end;

//------------------------------------------------------------------------------
procedure TPaletteConfig.FormShow(Sender: TObject);
var s, lang, s2, short: string;
    i, j: Integer;
    tn, tnc: TTreeNode;

 function Add (Node: TTreeNode; code, icon: Integer; Checked: Boolean; const s: string): TTreenode;
 begin
  if Assigned (Node) then Result:= Configlist.Items.AddChild (Node, s)
                     else Result:= Configlist.Items.Add (nil, s);
  Result.ImageIndex:= icon;
  if Checked then Result.ImageIndex:= cCheckIcon;
  Result.SelectedIndex:= Result.ImageIndex;
  Result.StateIndex:= code;
 end;

begin
 // Sprache anpassen
 Caption:= GetLangStr (ltCfgPaletteBar);
 BtnOk.Caption:= GetLangStr (ltOKCaption);
 BtnCancel.Caption:= GetLangStr (ltCancelCaption);
 BtnClear.Caption:= GetLangStr (ltDelCaption);
 RadioGroup.Caption:= GetLangStr (ltDelete);
 RadioGroup.Items.Clear;
 RadioGroup.Items.Add(GetLangStr(ltHistory));
 RadioGroup.Items.Add(GetLangStr(ltFavorites));
 RadioGroup.Items.Add(GetLangStr(ltSearchText));

 ConfigList.Items.Clear;

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltCompList));
 tnc:= Add (tn,  $00000000, 21, False, GetLangStr (ltPalView));
       Add (tnc, $01010000, -1, PB_Options.ShowMode = smKachel, GetLangStr (ltPalViewKachel));
       Add (tnc, $02010000, -1, PB_Options.ShowMode = smSymbol, GetLangStr (ltPalViewSymbol));
       Add (tnc, $03010000, -1, PB_Options.ShowMode = smList,   GetLangStr (ltPalViewList));
       Add (tnc, $04010000, -1, PB_Options.ShowMode = smText,   GetLangStr (ltPalViewText));
 tnc:= Add (tn,  $00000000, 21, False, GetLangStr (ltPalText));
       Add (tnc, $01020000, -1, PB_Options.TextSize = tsSmall,  GetLangStr (ltSmall));
       Add (tnc, $02020000, -1, PB_Options.TextSize = tsMedium, GetLangStr (ltMedium));
       Add (tnc, $03020000, -1, PB_Options.TextSize = tsBig,    GetLangStr (ltBig));
       Add (tn,  $00030000, -1, PB_Options.Hottrack, GetLangStr (ltPalHottrack));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltToolbar));
 tnc:= Add (tn,  $00000000, 21, False, GetLangStr (ltShowButtons));
       Add (tnc, $01040000, -1, Bit0  in PB_Options.Buttons, GetLangStr (ltActCategory));
       Add (tnc, $02040000, -1, Bit1  in PB_Options.Buttons, GetLangStr (ltAllComps));
       Add (tnc, $03040000, -1, Bit2  in PB_Options.Buttons, GetLangStr (ltTreeview));
       Add (tnc, $04040000, -1, Bit3  in PB_Options.Buttons, GetLangStr (ltActForm));
       Add (tnc, $05040000, -1, Bit4  in PB_Options.Buttons, GetLangStr (ltHistory));
       Add (tnc, $06040000, -1, Bit5  in PB_Options.Buttons, GetLangStr (ltFavorites));
       Add (tnc, $07040000, -1, Bit6  in PB_Options.Buttons, GetLangStr (ltEnterExtInfo));
       Add (tnc, $08040000, -1, Bit7  in PB_Options.Buttons, GetLangStr (ltShowExtInfo));
       Add (tnc, $09040000, -1, Bit8  in PB_Options.Buttons, GetLangStr (ltShowHelp));
       Add (tnc, $0A040000, -1, Bit9  in PB_Options.Buttons, GetLangStr (ltFavAdd));
       Add (tnc, $0B040000, -1, Bit10 in PB_Options.Buttons, GetLangStr (ltFavRemove));
       Add (tnc, $0C040000, -1, Bit11 in PB_Options.Buttons, GetLangStr (ltCfgCategory));
       Add (tnc, $0D040000, -1, Bit12 in PB_Options.Buttons, GetLangStr (ltCfgPaletteBar));
       Add (tnc, $0E040000, -1, Bit13 in PB_Options.Buttons, GetLangStr (ltAbout));

 tnc:= Add (tn,  $00000000, 21, False, GetLangStr (ltToolbarPos));
       Add (tnc, $01050000, -1, PB_Options.ButtonPos = alTop,   GetLangStr (ltTop));
       Add (tnc, $02050000, -1, PB_Options.ButtonPos = alLeft,  GetLangStr (ltLeft));
       Add (tnc, $03050000, -1, PB_Options.ButtonPos = alRight, GetLangStr (ltRight));
 tnc:= Add (tn,  $04050000, -1, PB_Options.ToolBarViz, GetLangStr (ltShow));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltA2ZBar));
       Add (tn,  $01060000, -1, PB_Options.A2ZFilter, GetLangStr (ltFilterMode));
       Add (tn,  $02060000, -1, PB_Options.A2ZPos = alLeft,  GetLangStr (ltLeft));
       Add (tn,  $03060000, -1, PB_Options.A2ZPos = alRight, GetLangStr (ltRight));
       Add (tn,  $04060000, -1, PB_Options.A2ZViz, GetLangStr (ltVisible));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltHistory));
       Add (tn,  $01070000, -1, PB_Options.SaveHistory, GetLangStr (ltSave));
      // Add (tn,  $02070000, -1, False, GetLangStr (ltHistClear));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltSearchbar));
       Add (tn,  $01090000, -1, PB_Options.SearchBar = alTop, GetLangStr (ltTop));
       Add (tn,  $02090000, -1, PB_Options.SearchBar = alBottom, GetLangStr (ltBottom));
       Add (tn,  $03090000, -1, PB_Options.SearchBarViz, GetLangStr (ltVisible));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltMenu));
       Add (tn,  $010A0000, -1, PB_Options.Menu, GetLangStr (ltShow));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltStatusbar));
       Add (tn,  $010B0000, -1, PB_Options.Menu, GetLangStr (ltShow));

 tn:=  Add (nil, $00000000, 19, False, GetLangStr (ltLanguage));
 j:= 1;
 if Assigned (gLangList) then begin
  for i:= 0 to pred (gLangList.Count) do begin
   s:= gLangList[i];
   lang:= GetStrPart (s, '_');
   s2:=  GetStrPart (s, '_');
   short:= s;
   if lang = PB_options.Language then begin
    Add (tn, TextLong (short+#8+Chr(j)), -1, short = PB_options.Language, s2);
    Inc(j);
   end;
  end;
 end;
 ConfigList.Selected:= nil;
end;

//------------------------------------------------------------------------------
procedure TPaletteConfig.ConfigListClick(Sender: TObject);
var Node, tn: TTreeNode;
    trick: TTrick;
begin
 ConfigList.Items.BeginUpdate;
 Node:= ConfigList.Selected;
 if Assigned (node) then begin
  if Node.StateIndex > 0 then begin
   trick.int:= Node.StateIndex;
   case trick.b3 of
     3,
     4,
    10,
    11:  begin
          if Node.ImageIndex = cCheckIcon then Node.ImageIndex:= -1
                                          else Node.ImageIndex:= cCheckIcon;
         end;
     6:  case trick.b4 of
          2:   begin
                Node.ImageIndex:= cCheckIcon;
                tn:= Node.GetNext;
                tn.ImageIndex:= -1;
               end;
          3:   begin
                Node.ImageIndex:= cCheckIcon;
                tn:= Node.GetPrev;
                tn.ImageIndex:= -1;
               end;
          else if Node.ImageIndex = cCheckIcon then Node.ImageIndex:= -1
                                               else Node.ImageIndex:= cCheckIcon;
         end;
     7:  ;
     9:  case trick.b4 of
          1:   begin
                Node.ImageIndex:= cCheckIcon;
                tn:= Node.GetNext;
                tn.ImageIndex:= -1;
               end;
          2:   begin
                Node.ImageIndex:= cCheckIcon;
                tn:= Node.GetPrev;
                tn.ImageIndex:= -1;
               end;
          else if Node.ImageIndex = cCheckIcon then Node.ImageIndex:= -1
                                               else Node.ImageIndex:= cCheckIcon;
         end;
    else begin
          tn:= Node.Parent.getFirstChild;
          repeat
           tn.ImageIndex:= -1;
           tn:= tn.getNextSibling;
          until tn = nil;
          Node.ImageIndex:= cCheckIcon;
         end;
   end;
   Node.SelectedIndex:= Node.ImageIndex;
  end; // Node.StateIndex
 end; // Assigend
 ConfigList.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TPaletteConfig.SetOptions;
var Node: TTreenode;
    trick: TTrick;
    Checked: Boolean;
begin
 Node:= ConfigList.Items.GetFirstNode;
 if Assigned (node) then begin
  PB_Options.Buttons:= [];
  repeat
   if Node.StateIndex > 0 then begin
    trick.int:= Node.StateIndex;
    Checked:= Node.ImageIndex = cCheckIcon;
    case trick.b3 of
      1: case trick.b4 of
          1: if Checked then PB_Options.ShowMode:= smKachel;
          2: if Checked then PB_Options.ShowMode:= smSymbol;
          3: if Checked then PB_Options.ShowMode:= smList;
          4: if Checked then PB_Options.ShowMode:= smText;
         end;
      2: case trick.b4 of
          1: if Checked then PB_Options.TextSize:= tsSmall;
          2: if Checked then PB_Options.TextSize:= tsMedium;
          3: if Checked then PB_Options.TextSize:= tsBig;
         end;
      3: PB_Options.Hottrack:= Checked;
      4: if Checked then Include (PB_Options.Buttons, TBit(trick.b4-1));
      5: case trick.b4 of
          1: if Checked then PB_Options.ButtonPos:= alTop;
          2: if Checked then PB_Options.ButtonPos:= alLeft;
          3: if Checked then PB_Options.ButtonPos:= alRight;
          4: PB_options.ToolBarViz:= Checked;
         end;
      6: case trick.b4 of
          1: PB_Options.A2ZFilter:= Checked;
          2: if Checked then PB_Options.A2ZPos:= alLeft;
          3: if Checked then PB_Options.A2ZPos:= alRight;
          4: PB_Options.A2ZViz:= Checked;
         end;
      7: PB_Options.SaveHistory:= Checked;
      8: if Checked then PB_Options.Language:= Chr (trick.b1) + Chr (trick.b2);
      9: case trick.b4 of
          1: if Checked then PB_Options.SearchBar:= alTop;
          2: if Checked then PB_Options.SearchBar:= alBottom;
          3: PB_Options.SearchBarViz:= Checked;
         end;
     10: PB_Options.Menu:= Checked;
     11: PB_Options.StatusBarViz:= Checked;
    end;
   end; // Node.StateIndex
   Node:= Node.GetNext;
  until Node = nil;
 end; // Assigend
end;

//------------------------------------------------------------------------------
procedure TPaletteConfig.BtnClearClick(Sender: TObject);
begin
 case RadioGroup.ItemIndex of
  0: PBar.ClearList (rmHistory);
  1: PBar.ClearList (rmFavorites);
  2: PBar.ClearList (rmSearch);
 end;
end;

//------------------------------------------------------------------------------
procedure TPaletteConfig.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 SaveFormPos (Config, cRegConfig);
end;

//------------------------------------------------------------------------------
end.

