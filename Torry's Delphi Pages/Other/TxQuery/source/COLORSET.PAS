{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is ColorSet.pas                                      }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

unit ColorSet;

{$I XQ_FLAG.INC}
interface

uses Windows,
     SysUtils,
     Classes,
     Graphics,
     Forms,
     Controls,
     StdCtrls,
     Buttons,
     ExtCtrls,
     ColorGrd,
     XQLex,
     XQYacc,
     syntaxhi;

type
  TfrmColorSettings = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    ChkBold: TCheckBox;
    ChkItalic: TCheckBox;
    ChkUnderline: TCheckBox;
    ColorGrid1: TColorGrid;
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    FOriginalColorConfig: TColorConfig;
    FColorConfig: TColorConfig;
    FIgnoreChanges: Boolean;
    procedure RefreshInfo;
  public
    { Public declarations }
    function Enter(ColorConfig: TColorConfig):Word;
  end;

implementation

{$R *.DFM}

function TfrmColorSettings.Enter(ColorConfig: TColorConfig): Word;
begin
   FColorConfig:= TColorConfig.Create;
   FColorConfig.Assign(ColorConfig);
   FOriginalColorConfig:= ColorConfig;

   ListBox1.ItemIndex:= 0;
   ListBox1Click(nil);

   Result:= ShowModal;
end;


procedure TfrmColorSettings.RefreshInfo;
var
   Index, I: Integer;
   ColorElement: PColorElement;
begin
   FIgnoreChanges:= True;
   Index:= ListBox1.ItemIndex; if Index < 0 then Exit;
   I:= FColorConfig.IndexOfGroup(TElementGroup(Index));
   ColorElement:= PColorElement(Fcolorconfig.ColorSettings[I]);
   with ColorElement^ do
   begin
      ColorGrid1.ForeGroundIndex:= ColorGrid1.ColorToIndex(ForeColor);
      ColorGrid1.BackGroundIndex:= ColorGrid1.ColorToIndex(BackColor);
      ChkBold.Checked := fsBold in FontStyle;
      ChkItalic.Checked := fsItalic in FontStyle;
      ChkUnderline.Checked := fsUnderline in FontStyle;
   end;
   FIgnoreChanges:= False;
end;

procedure TfrmColorSettings.ListBox1Click(Sender: TObject);
begin
   RefreshInfo;
end;

procedure TfrmColorSettings.FormDestroy(Sender: TObject);
begin
   FColorConfig.Free;
end;

procedure TfrmColorSettings.OKBtnClick(Sender: TObject);
begin
   FOriginalColorConfig.Assign(FColorConfig);
end;

end.
