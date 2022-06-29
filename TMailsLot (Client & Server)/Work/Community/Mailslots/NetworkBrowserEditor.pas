{------------------------------------------------------------------------------}
{ NetworkBrowserEditor                                                         }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/09/2001                                                       }
{ Version   : 1.000 (Delphi 5)                                                 }
{------------------------------------------------------------------------------}
 Unit NetworkBrowserEditor;

{==============================================================================}
{ Copyright (c) 1987-2002 Franck Musson                                        }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.                        }
{ IN NO EVENT SHALL FRANCK MUSSON BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER    }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS       }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{ Except as contained in this notice, the name of Franck Musson shall not be   }
{ used in advertising or otherwise to promote the sale, use or other dealings  }
{ in this Software without prior written authorization from Franck Musson.     }
{==============================================================================}
 
{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Clauses Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ComCtrls, ExtCtrls, NetworkBrowser, ImgList;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TTNetworkBrowserEditor = Class(TForm)
      Panel1: TPanel;
      Panel2: TPanel;
      TreeView: TTreeView;
      OkButton: TButton;
      ImageList: TImageList;
      Procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; Var AllowExpansion: Boolean);
    Private
      FBrowser: TNetworkBrowser;
    Public
      Procedure Load(Abrowser: TNetworkBrowser);
    End;

    Procedure ShowNetworkBrowser(ABrowser: TNetworkBrowser);

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{$R *.DFM}

{------------------------------------------------------------------------------}
{ ShowNetworkBrowser(ABrowser: TNetworkBrowser);                               }
{------------------------------------------------------------------------------}
 Procedure ShowNetworkBrowser(ABrowser: TNetworkBrowser);
 Var
   Net: TTNetworkBrowserEditor;
 Begin
    Net := TTNetworkBrowserEditor.Create(Application);
    Try
       Net.Load(Abrowser);
       Net.ShowModal;
    Finally
       Net.Free;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TTNetworkBrowserEditor                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TTNetworkBrowserEditor.Load(ABrowser: TNetworkBrowser);                      }
{------------------------------------------------------------------------------}
 Procedure TTNetworkBrowserEditor.Load(ABrowser: TNetworkBrowser);
 Var
    I, J: Integer;
    Root, Grp: TTreenode;
 Begin
    FBrowser := ABrowser;
    FBrowser.Active := True;
    For I := 0 To FBrowser.Networks.Count-1 Do Begin
       FBrowser.Networks[I].Active := True;
       Root := Treeview.Items.AddChildObject(Nil, FBrowser.Networks[I].RemoteName, FBrowser.Networks[I]);
       Root.HasChildren := True;
       Root.ImageIndex := 0;
       Root.SelectedIndex := 0;       
       For J := 0 To FBrowser.Networks[I].Workgroups.Count-1 Do Begin
         Grp := Treeview.Items.AddChildObject(Root, FBrowser.Networks[I].Workgroups[J].RemoteName, FBrowser.Networks[I].Workgroups[J]);
         Grp.HasChildren := True;
         Grp.ImageIndex := 1;
         Grp.SelectedIndex := 1;
       End;
    End;
    Treeview.Items[0].Expand(False);
 End;

{------------------------------------------------------------------------------}
{ TTNetworkBrowserEditor.TreeViewExpanding(...);                               }
{------------------------------------------------------------------------------}
 Procedure TTNetworkBrowserEditor.TreeViewExpanding(Sender: TObject; Node: TTreeNode; Var AllowExpansion: Boolean);
 Var
    Grp: TNetworkBrowserGroup;
    Tmp: TtreeNode;
    I: Integer;
 Begin
    If (Node.Count>0) Then Exit;
    Screen.Cursor := crHourglass;
    Try
       Node.HasChildren := False;
       If TObject(Node.Data) Is TNetworkBrowserGroup Then Begin
          Grp := (TObject(Node.Data) As TNetworkBrowserGroup);
          Grp.Active := True;
          For I := 0 To Grp.Computers.Count-1 Do Begin
             Tmp := Treeview.Items.AddChildObject(Node, Grp.Computers[I].RemoteName, Grp.Computers[I]);
             Tmp.ImageIndex := 2;
             Tmp.SelectedIndex := 2;
          End;
       End;
    Finally
       Screen.Cursor := crDefault;
    End;
 End;

{------------------------------------------------------------------------------}
{ End of NetworkBrowserEditor.pas                                              }
{------------------------------------------------------------------------------}
 End.
