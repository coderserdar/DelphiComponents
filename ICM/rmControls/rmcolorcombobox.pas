{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmColorComboBox
Purpose  : Standard Combobox with Colors being displayed.  Both standard 16 and
           windows system colors.
Date     : 01-01-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmColorComboBox;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TColorSet = (csEndUser, csProgrammer);
  TrmColorComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    fBoxRect : TRect;
    fColorSet : TColorSet;
    function GetColorIndex:TColor;
    procedure SetColorSet(value : TColorSet);
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; WRect: TRect; State: TOwnerDrawState); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
    property ColorIndex:TColor read GetColorIndex;
  published
    { Published declarations }
    property ColorSet:TColorSet read fColorSet write SetColorSet default csEndUser;
    property OnChange;
  end;

implementation

const
     maxcolors = 43;
     DefaultColors = 18;

     ColorValues : array[0..42] of TColor =
       (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
        clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clLtGray,
        clDkGray, clWhite, clScrollBar, clBackground, clActiveCaption, clInactiveCaption,
        clMenu, clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText,
        clActiveBorder, clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText,
        clBtnFace, clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
        cl3DDkShadow, cl3DLight, clInfoText, clInfoBk);

     ColorStrings : array[0..42] of String =
        ('clBlack','clMaroon','clGreen','clOlive','clNavy','clPurple','clTeal','clGray',
         'clSilver','clRed','clLime','clYellow','clBlue','clFuchsia','clAqua','clLtGray',
         'clDkGray','clWhite','clScrollBar','clBackground','clActiveCaption','clInactiveCaption',
         'clMenu','clWindow','clWindowFrame','clMenuText','clWindowText','clCaptionText',
         'clActiveBorder','clInactiveBorder','clAppWorkSpace','clHighlight','clHighlightText',
         'clBtnFace','clBtnShadow','clGrayText','clBtnText','clInactiveCaptionText','clBtnHighlight',
         'cl3DDkShadow','cl3DLight','clInfoText','clInfoBk');


constructor TrmColorComboBox.create(AOwner:TComponent);
begin
     inherited create(AOwner);
     style := csOwnerDrawFixed;
     ColorSet := csEndUser;
end;

function TrmColorComboBox.GetColorIndex:TColor;
begin
   result := ColorValues[itemindex];
end;

procedure TrmColorComboBox.DrawItem(Index: Integer; WRect: TRect; State: TOwnerDrawState);
var
   fillcolor, textcolor : TColor;
   ColorName : string;
begin
     if (odSelected in state) then
     begin
          fillcolor := clHighLight;
          TextColor := clHighLightText;
     end
     else
     begin
          fillcolor := clWindow;
          TextColor := clWindowText;
     end;
     fBoxRect := rect(WRect.Left + 2,WRect.Top + 2, WRect.Left + 15, WRect.Bottom - 2);
     with canvas do
     begin
          brush.color := FillColor;
          fillrect(WRect);
          brush.color := ColorValues[index];
          fillrect(fboxrect);
          brush.color := clactiveborder;
          framerect(fboxrect);
          brush.color := fillColor;
          pen.color := TextColor;
          WRect.left := WRect.left + 17;
          colorname := ColorStrings[Index];
          if ColorSet = csEndUser then delete(colorname,1,2);
          TextRect(WRect, Wrect.Left, WRect.Top, colorname);
     end;
end;

procedure TrmColorComboBox.SetColorSet(value : TColorSet);
var
   loop:integer;
begin
     fColorSet := value;

     if (not self.HandleAllocated) or (csdesigning in componentstate) then exit;
     items.clear;
     if ColorSet = csProgrammer then
        for loop := 0 to maxColors-1 do items.add(ColorStrings[loop])
     else
        for loop := 0 to DefaultColors-1 do items.add(ColorStrings[loop])
end;

procedure TrmColorComboBox.CreateWnd;
var
   loop:integer;
begin
     inherited;
     items.clear;
     if ColorSet = csProgrammer then
        for loop := 0 to maxColors-1 do items.add(ColorStrings[loop])
     else
        for loop := 0 to DefaultColors-1 do items.add(ColorStrings[loop])
end;

end.
