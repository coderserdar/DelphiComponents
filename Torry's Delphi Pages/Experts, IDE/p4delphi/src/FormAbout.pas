{**************************************************************************************************}
{                                                                                                  }
{ Perforce for Delphi plugin (P4Delphi)                                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall. Portions created by                 }
{ Chris Fairall are Copyright (C) Chris Fairall (cfairall at bigpond dot net dot au)               }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit FormAbout;
{----------------------------------------------------------------------------

   Unit Name     :  FormAbout
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  About box and display of results of "p4 info" command.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormAbout.pas $
   $Revision: #6 $
   $DateTime: 2006/03/06 18:16:41 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmAbout = class(TForm)
    btnClose: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lvInfo: TListView;
    Label3: TLabel;
    Image2: TImage;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  UnitP4Engine;

{$R *.dfm}

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  slInfo     : TStringList;
  iCounter,
  iPos       : Integer;
  sItem,
  sDesc      : String;
begin
  Screen.Cursor := crHourGlass;
  try
    slInfo := TStringList.Create;
    try
      P4Engine.Info(slInfo, false);
      for iCounter := 0 to slInfo.Count - 1 do
        begin
          iPos := Pos(': ', slInfo[iCounter]);
          if iPos > 0 then
            begin
              sItem := Copy(slInfo[iCounter], 1, iPos - 1);
              sDesc := Copy(slInfo[iCounter], iPos + 2, Length(slInfo[iCounter]));
              if CompareText(sItem, 'Current directory') <> 0 then
                with lvInfo.Items.Add do
                  begin
                    Caption := sItem;
                    SubItems.Add(sDesc);
                  end;
            end;
        end;
      lvInfo.Columns[0].Width := -1;
    finally
      slInfo.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
