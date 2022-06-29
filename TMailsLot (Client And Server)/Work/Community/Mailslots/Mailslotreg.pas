{------------------------------------------------------------------------------}
{ Mailslotreg                                                                  }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/09/2001                                                       }
{ Version   : 1.000 (Delphi 3/5)                                               }
{------------------------------------------------------------------------------}
 Unit Mailslotreg;

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

 Procedure Register;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ Clauses Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Classes, Mailslots, NetworkBrowser, DsnConst, dsgnintf,
    {$IFDEF VER100}
    NetworkBrowserEditor3
    {$ELSE}
    NetworkBrowserEditor
    {$ENDIF};

{------------------------------------------------------------------------------}
{ Constantes                                                                   }
{------------------------------------------------------------------------------}
 Resourcestring
    SExecuter = '&Executer...';

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserEditor                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
 Type
    TNetworkBrowserEditor = Class(TComponentEditor)
    Public
      Procedure ExecuteVerb(Index: Integer);                           Override;
      Function GetVerb(Index: Integer): String;                        Override;
      Function GetVerbCount: Integer;                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserEditor.ExecuteVerb(Index: Integer);                           }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserEditor.ExecuteVerb(Index: Integer);
 Begin
    Case Index Of
       0: ShowNetworkBrowser(TNetworkBrowser(Component));
    Else
       Inherited ExecuteVerb(Index);
    End;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserEditor.GetVerb(Index: Integer): String;                       }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserEditor.GetVerb(Index: Integer): String;
 Begin
    Case Index Of
       0: Result := SExecuter;
       1: Result := '-';
    Else
       Result := Inherited GetVerb(Index);
    End;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserEditor.GetVerbCount: Integer;                                 }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserEditor.GetVerbCount: Integer;
 Begin
    Result := 2;
 End;

{------------------------------------------------------------------------------}
{ Register Procedure                                                           }
{------------------------------------------------------------------------------}
 Procedure Register;
 Begin
    RegisterComponents(srSystem, [TMailslotServer, TMailslotClient, TNetworkBrowser]);
    RegisterComponentEditor(TNetworkBrowser, TNetworkBrowserEditor);
 End;

{------------------------------------------------------------------------------}
{ End of mailslotreg.pas                                                       }
{------------------------------------------------------------------------------}
 End.
