{------------------------------------------------------------------------------}
{ NetworkBrowserEditor3                                                        }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 21/09/2001                                                       }
{ Version   : 1.000 (Delphi 3)                                                 }
{------------------------------------------------------------------------------}
 Unit NetworkBrowserEditor3;

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
    NetworkBrowser, StdCtrls, ComCtrls, ExtCtrls;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TTNetworkBrowserEditor = Class(TForm)
      Panel1: TPanel;
      TreeView: TTreeView;
      Panel2: TPanel;
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
{ Fin de NetworkBrowserEditor3.pas                                             }
{------------------------------------------------------------------------------}
 End.
