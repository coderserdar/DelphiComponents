{------------------------------------------------------------------------------}
{ NetworkBrowser                                                               }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 21/09/2001                                                       }
{ Version   : 1.000 (Delphi 3/5)                                               }
{------------------------------------------------------------------------------}
 Unit NetworkBrowser;

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
    Windows, Messages, SysUtils, Classes, Stdctrls, Dialogs;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TNetworkBrowserNet = Class;
    TNetworkBrowserNets = Class;
    TNetworkBrowserGroup = Class;
    TNetworkBrowserGroups = Class;
    TNetworkBrowserComputer = Class;
    TNetworkBrowserComputers = Class;

    TNetworkBrowser = Class(TComponent)
    Private
      FActive: Boolean;
      FNetworks: TNetworkBrowserNets;
      Function  GetActive: Boolean;
      Procedure SetActive(Value: Boolean);
      Function  GetNetworkList: TNetworkBrowserNets;
      Procedure SetNetworkList(Const Value: TNetworkBrowserNets);
      Function  GetNetworks: String;
    Protected
      Procedure BuildNetworkList;                                               Virtual;
      Procedure ClearNetworkList;                                               Virtual;
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
      Property NetworksText: String            Read GetNetworks;
      Property Networks: TNetworkBrowserNets   Read GetNetworkList Write SetNetworkList;
      Property Active: Boolean                 Read GetActive      Write SetActive;
    End;

    TCustomBrowserItem = Class(TCollectionItem)
    Private
      FdwScope: Integer;
      FdwType: Integer;
      FdwDisplayType: Integer;
      FdwUsage: Integer;
      FLocalName: String;
      FRemoteName: String;
      FComment: String;
      FProvider: String;
    Public
      Property Scope: Integer        Read FdwScope       Write FdwScope;
      Property NodeType: Integer     Read FdwType        Write FdwType;
      Property DisplayType: Integer  Read FdwDisplayType Write FdwDisplayType;
      Property Usage: Integer        Read FdwUsage       Write FdwUsage;
    Published
      Property LocalName: String     Read FLocalName     Write FLocalName;
      Property RemoteName: String    Read FRemoteName    Write FRemoteName;
      Property Comment: String       Read FComment       Write FComment;
      Property Provider: String      Read FProvider      Write FProvider;
    End;

    TNetworkBrowserNets = Class(TCollection)
    Private
      FBrowser: TNetworkBrowser;
      Function GetItem(Index: Integer): TNetworkBrowserNet;
      Function GetCount: Integer;
    Public
      {$IFDEF VER100}
      Constructor Create(AOwner: TNetworkBrowser);                              Virtual;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserNet;                                         Virtual;
      {$ELSE}
      Constructor Create(AOwner: TNetworkBrowser);                              Overload;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserNet;                                         Overload;
      {$ENDIF}
      Property Count: Integer                               Read GetCount;
      Property Items[Index: Integer]: TNetworkBrowserNet    Read GetItem;       Default;
      Property NetworkBrowser: TNetworkBrowser              Read FBrowser;
    End;

    TNetworkBrowserNet = Class(TCustomBrowserItem)
    Private
      FActive: Boolean;
      FWorkgroups: TNetworkBrowserGroups;
      Function  GetActive: Boolean;
      Procedure SetActive(Value: Boolean);
      Function  GetWorkGroupList: TNetworkBrowserGroups;
      Procedure SetWorkGroupList(Const Value: TNetworkBrowserGroups);
      Function  GetWorkGroups: String;
    Protected
      Procedure LoadWorkGroups;                                                 Virtual;
      Procedure ClearWorkGroups;                                                Virtual;
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Property WorkgroupsText: String            Read GetWorkGroups;
      Property WorkGroups: TNetworkBrowserGroups Read GetWorkGroupList Write SetWorkGroupList;
      Property Active: Boolean                   Read GetActive        Write SetActive;
    End;

    TNetworkBrowserGroups = Class(TCollection)
    Private
      FNetwork: TNetworkBrowserNet;
      Function GetItem(Index: Integer): TNetworkBrowserGroup;
      Function GetCount: Integer;
    Public
      {$IFDEF VER100}
      Constructor Create(AOwner: TNetworkBrowserNet);                           Virtual;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserGroup;                                       Virtual;
      {$ELSE}
      Constructor Create(AOwner: TNetworkBrowserNet);                           Overload;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserGroup;                                       Overload;
      {$ENDIF}
      Property Count: Integer                               Read GetCount;
      Property Items[Index: Integer]: TNetworkBrowserGroup  Read GetItem;       Default;
      Property Network: TNetworkBrowserNet                  Read FNetwork;
    End;

    TNetworkBrowserGroup = Class(TCustomBrowserItem)
    Private
      FActive: Boolean;
      FComputers: TNetworkBrowserComputers;
      Function  GetComputerList: TNetworkBrowserComputers;
      Procedure SetComputerList(Const Value: TNetworkBrowserComputers);
      Function  GetActive: Boolean;
      Procedure SetActive(Value: Boolean);
      Function  GetComputers: String;
    Protected
      Procedure LoadComputers;                                                  Virtual;
      Procedure ClearComputers;                                                 Virtual;
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Property  ComputersText: String              Read GetComputers;
      Property Computers: TNetworkBrowserComputers Read GetComputerList Write SetComputerList;
      Property Active: Boolean                     Read GetActive       Write SetActive;
    End;

    TNetworkBrowserComputers = Class(TCollection)
    Private
      FWorkGroup: TNetworkBrowserGroup;
      Function GetItem(Index: Integer): TNetworkBrowserComputer;
      Function GetCount: Integer;
    Public
      {$IFDEF VER100}
      Constructor Create(AOwner: TNetworkBrowserGroup);                         Virtual;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserComputer;                                    Virtual;
      {$ELSE}
      Constructor Create(AOwner: TNetworkBrowserGroup);                         Overload;
      Destructor Destroy;                                                       Override;
      Function Add: TNetworkBrowserComputer;                                    Overload;
      {$ENDIF}
      Property Count: Integer                                  Read GetCount;
      Property Items[Index: Integer]: TNetworkBrowserComputer  Read GetItem;    Default;
    End;

    TNetworkBrowserComputer = Class(TCustomBrowserItem);

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ Constants                                                                    }
{------------------------------------------------------------------------------}
 ResourceString
    SBrowserNotActive = 'Network Browser not active';

{------------------------------------------------------------------------------}
{ Private Types                                                                }
{------------------------------------------------------------------------------}
 Type
    PNetResourceRec = ^TNetResourceRec;
    TNetResourceRec = Record
      dwScope       : Integer;
      dwType        : Integer;
      dwDisplayType : Integer;
      dwUsage       : Integer;
      LocalName     : String;
      RemoteName    : String;
      Comment       : String;
      Provider      : String;
    End;

{------------------------------------------------------------------------------}
{ Function Propercase(Const Value: String): String;                            }
{------------------------------------------------------------------------------}
 Function Propercase(Const Value: String): String;
 Begin
    Result := Uppercase(Copy(Value, 1,1))+Lowercase(Copy(Value, 2, Length(Value)));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowser                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowser.Create(AOwner: TComponent);                                  }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowser.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FNetworks := TNetworkBrowserNets.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.Destroy;                                                     }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowser.Destroy;
 Begin
    Active := False;
    FNetworks.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.GetActive: Boolean;                                          }
{ Read method property Active                                                  }
{------------------------------------------------------------------------------}
 Function TNetworkBrowser.GetActive: Boolean;
 Begin
    Result := FActive;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.SetActive(Value: Boolean);                                   }
{ Write method property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowser.SetActive(Value: Boolean);
 Begin
    If (FActive=Value) Then Exit;
    FActive := Value;
    If Value Then
       BuildNetworkList
    Else
       ClearNetworkList;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.GetNetworkList: TNetworkBrowserNets;                         }
{ Read method property Networks                                                }
{------------------------------------------------------------------------------}
 Function TNetworkBrowser.GetNetworkList: TNetworkBrowserNets;
 Begin
    Result := FNetworks;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.SetNetworkList(Const Value: TNetworkBrowserNets);            }
{ Write method property Networks                                               }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowser.SetNetworkList(Const Value: TNetworkBrowserNets);
 Begin
    FNetworks.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.ClearNetworkList;                                            }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowser.ClearNetworkList;
 Begin
    FNetworks.Clear;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.BuildNetworkList;                                            }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowser.BuildNetworkList;
 Var
   PtrResource : PNetResource;
   Buffer      : Pointer;
   BuffSize    : DWORD;
   Count       : DWORD;
   Res         : DWORD;
   FHandle     : THandle;
   I           : SmallInt;
 Begin
    ClearNetworkList;
    GetMem(Buffer, 16385);
    Try
       res := WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK, RESOURCEUSAGE_CONTAINER, Nil, FHandle);
       If (res<>0) Then Raise Exception(Res);
       Try
          Count := $FFFFFFFF;
          BuffSize := 16385;
          res := WNetEnumResource(FHandle, Count, Buffer, BuffSize);
          If (res=ERROR_NO_MORE_ITEMS) Then Exit;
          If (res<>0) Then Raise Exception(res);
          PtrResource := PNetResource(Buffer);
          For I := 0 To Count - 1 Do
             With FNetworks.Add Do Begin
                Scope       := PtrResource^.dwScope;
                NodeType    := PtrResource^.dwType ;
                DisplayType := PtrResource^.dwDisplayType ;
                Usage       := PtrResource^.dwUsage ;
                LocalName   := Propercase(StrPas(PtrResource^.lpLocalName));
                RemoteName  := Propercase(StrPas(PtrResource^.lpRemoteName));
                Comment     := Propercase(StrPas(PtrResource^.lpComment));
                Provider    := Propercase(StrPas(PtrResource^.lpProvider));
                Inc(PtrResource);
             End;
       Finally
          res := WNetCloseEnum(FHandle);
          If (res<>0) Then Raise Exception(Res);
       End;
    Finally
       FreeMem(Buffer);
    End;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowser.GetNetworks: String;                                         }
{ Read Method Property NetworksText                                            }
{------------------------------------------------------------------------------}
 Function TNetworkBrowser.GetNetworks: String;
 Var
    I: Integer;
 Begin
    Result := '';
    If Not Active Then Raise Exception.Create(SBrowserNotActive);
    For I := 0 To Networks.Count-1 Do
       If (I=0) Then
          Result := Networks[I].RemoteName
       Else
          Result := Result+','+Networks[I].RemoteName;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserNets                                           }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowserNets.Create(AOwner: TNetworkBrowser);                         }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowserNets.Create(AOwner: TNetworkBrowser);
 Begin
    Inherited Create(TNetworkBrowserNet);
    FBrowser := AOwner;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNets.Destroy;                                                 }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowserNets.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNets.Add: TNetworkBrowserNet;                                 }
{ Add method Overload                                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNets.Add: TNetworkBrowserNet;
 Begin
    Result := TNetworkBrowserNet(Inherited Add);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNets.GetItem(Index: Integer): TNetworkBrowserNet;             }
{ Read Method property Items Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNets.GetItem(Index: Integer): TNetworkBrowserNet;
 Begin
    FBrowser.Active := True;
    Result := TNetworkBrowserNet(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNets.GetCount: Integer;                                       }
{ Read Method property Count Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNets.GetCount: Integer;
 Begin
    FBrowser.Active := True;
    Result := Inherited Count;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserNet                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.Create(Collection: TCollection);                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowserNet.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FActive := False;
    FWorkgroups := TNetworkBrowserGroups.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.Destroy;                                                  }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowserNet.Destroy;
 Begin
    Active := False;
    FWorkgroups.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.GetWorkgroupList: TNetworkBrowserGroups;                  }
{ Read Method property Workgroups                                              }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNet.GetWorkgroupList: TNetworkBrowserGroups;
 Begin
    Result := FWorkgroups;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.SetWorkgroupList(Const Value: TNetworkBrowserGroups);     }
{ Write Method property Computers                                              }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserNet.SetWorkgroupList(Const Value: TNetworkBrowserGroups);
 Begin
    FWorkgroups.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.GetActive: Boolean;                                       }
{ Read Method property Active                                                  }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNet.GetActive: Boolean;
 Begin
    Result := FActive;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.SetActive(Value: Boolean);                                }
{ Write Method property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserNet.SetActive(Value: Boolean);
 Begin
    If (FActive=Value) Then Exit;
    FActive := Value;
    If FActive Then
       LoadWorkgroups
    Else
       ClearWorkgroups;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.ClearWorkgroups;                                          }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserNet.ClearWorkgroups;
 Begin
    FWorkgroups.Clear;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.LoadWorkgroups;                                           }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserNet.LoadWorkgroups;
 Var
   NetResource : TNetResource;
   PtrResource : PNetResource;
   Buffer      : Pointer;
   BuffSize    : DWORD;
   Count       : DWORD;
   res         : DWORD;
   I           : Integer;
   FHandle     : THandle;
 Begin
    ClearWorkgroups;
    GetMem(Buffer, 16385);
    Try
       Zeromemory(@NetResource, SizeOf(NetResource));
       NetResource.dwScope       := FdwScope;
       NetResource.dwDisplayType := FdwDisplayType;
       NetResource.dwUsage       := FdwUsage;
       NetResource.dwScope       := FdwScope;
       NetResource.lpLocalName   := @LocalName[1];
       NetResource.lpRemoteName  := @RemoteName[1];
       NetResource.lpComment     := @Comment[1];
       NetResource.lpProvider    := @Provider[1];
       res := WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK, RESOURCEUSAGE_CONTAINER, @NetResource, FHandle);
       If (res<>0) Then Exit;
       Try
          While True Do Begin
             Count := $FFFFFFFF;
             BuffSize := 16385;
             res := WNetEnumResource(FHandle, Count, Buffer, BuffSize);
             If (res=ERROR_NO_MORE_ITEMS) Then Exit;
             If (res<>0) Then Exit;
             PtrResource := PNetResource(Buffer);
             For I := 0 To Count-1 Do Begin
                With FWorkgroups.Add  Do Begin
                   Scope       := PtrResource^.dwScope;
                   NodeType    := PtrResource^.dwType ;
                   DisplayType := PtrResource^.dwDisplayType ;
                   Usage       := PtrResource^.dwUsage ;
                   LocalName   := Propercase(StrPas(PtrResource^.lpLocalName));
                   RemoteName  := Propercase(StrPas(PtrResource^.lpRemoteName));
                   Comment     := Propercase(StrPas(PtrResource^.lpComment));
                   Provider    := Propercase(StrPas(PtrResource^.lpProvider));
                End;
                Inc(PtrResource);
             End;
          End;
       Finally
          res := WNetCloseEnum(FHandle);
          If (res<>0) Then Raise Exception(res);
       End;
    Finally
       FreeMem(Buffer);
    End;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserNet.GetWorkGroups: String;                                    }
{ Read Method Property WorkgroupsText                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserNet.GetWorkGroups: String;
 Var
    I: Integer;
 Begin
    Result := '';
    If Not Active Then Raise Exception.Create(SBrowserNotActive);
    For I := 0 To Workgroups.Count-1 Do
       If (I=0) Then
          Result := Workgroups[I].RemoteName
       Else
          Result := Result+','+Workgroups[I].RemoteName;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserGroups                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowserGroups.Create(AOwner: TNetworkBrowserNet);                    }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowserGroups.Create(AOwner: TNetworkBrowserNet);
 Begin
    Inherited Create(TNetworkBrowserGroup);
    FNetwork := AOwner;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroups.Destroy;                                               }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowserGroups.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroups.Add: TNetworkBrowserGroup;                             }
{ Add method Overload                                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroups.Add: TNetworkBrowserGroup;
 Begin
    Result := TNetworkBrowserGroup(Inherited Add);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroups.GetItem(Index: Integer): TNetworkBrowserGroup;         }
{ Read Method property Items Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroups.GetItem(Index: Integer): TNetworkBrowserGroup;
 Begin
    FNetwork.Active := True;
    Result := TNetworkBrowserGroup(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroups.GetCount: Integer;                                     }
{ Read Method property Count Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroups.GetCount: Integer;
 Begin
    FNetwork.Active := True;
    Result := Inherited Count;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserGroup                                          }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.Create(Collection: TCollection);                        }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowserGroup.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FActive := False;
    FComputers := TNetworkBrowserComputers.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.Destroy;                                                }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowserGroup.Destroy;
 Begin
    Active := False;
    FComputers.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.GetComputerList: TNetworkBrowserComputers;              }
{ Read Method property Computers                                               }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroup.GetComputerList: TNetworkBrowserComputers;
 Begin
    Result := FComputers;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.SetComputerList(Const Value: TNetworkBrowserComputers); }
{ Write Method property Computers                                              }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserGroup.SetComputerList(Const Value: TNetworkBrowserComputers);
 Begin
    FComputers.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.GetActive: Boolean;                                     }
{ Read Method property Active                                                  }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroup.GetActive: Boolean;
 Begin
    Result := FActive;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.SetActive(Value: Boolean);                              }
{ Write Method property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserGroup.SetActive(Value: Boolean);
 Begin
    If (FActive=Value) Then Exit;
    FActive := Value;
    If FActive Then
       LoadComputers
    Else
       ClearComputers;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.ClearComputers;                                         }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserGroup.ClearComputers;
 Begin
    FComputers.Clear;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.LoadComputers;                                          }
{------------------------------------------------------------------------------}
 Procedure TNetworkBrowserGroup.LoadComputers;
 Var
   NetResource : TNetResource;
   PtrResource : PNetResource;
   Buffer      : Pointer;
   BuffSize    : DWORD;
   Count       : DWORD;
   res         : DWORD;
   I           : Integer;
   FHandle     : THandle;
 Begin
    ClearComputers;
    GetMem(Buffer, 16385);
    Try
       Zeromemory(@NetResource, SizeOf(NetResource));
       NetResource.lpRemoteName  := @RemoteName[1];
       NetResource.dwDisplayType := RESOURCEDISPLAYTYPE_SERVER;
       NetResource.dwUsage       := RESOURCEUSAGE_CONTAINER;
       NetResource.dwScope       := RESOURCETYPE_DISK;
       res := WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK, RESOURCEUSAGE_CONTAINER, @NetResource, FHandle);
       If (res<>0) Then Exit;
       Try
          While True Do Begin
             Count := $FFFFFFFF;
             BuffSize := 16385;
             res := WNetEnumResource(FHandle, Count, Buffer, BuffSize);
             If (res=ERROR_NO_MORE_ITEMS) Then Exit;
             If (res<>0) Then Exit;
             PtrResource := PNetResource(Buffer);
             For I := 0 To Count-1 Do Begin
                With FComputers.Add  Do Begin
                   Scope       := PtrResource^.dwScope;
                   NodeType    := PtrResource^.dwType ;
                   DisplayType := PtrResource^.dwDisplayType ;
                   Usage       := PtrResource^.dwUsage ;
                   LocalName   := Propercase(Copy(StrPas(PtrResource^.lpLocalName), 3, Length(StrPas(PtrResource^.lpLocalName))-2));
                   RemoteName  := Propercase(Copy(StrPas(PtrResource^.lpRemoteName), 3, Length(StrPas(PtrResource^.lpRemoteName))-2));
                   Comment     := Propercase(StrPas(PtrResource^.lpComment));
                   Provider    := Propercase(StrPas(PtrResource^.lpProvider));
                End;
                Inc(PtrResource);
             End;
          End;
       Finally
          res := WNetCloseEnum(FHandle);
          If (res<>0) Then Raise Exception(res);
       End;
    Finally
       FreeMem(Buffer);
    End;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserGroup.GetComputers: String;                                   }
{ Read Method Property ComputersText                                           }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserGroup.GetComputers: String;
 Var
    I: Integer;
 Begin
    Result := '';
    If Not Active Then Raise Exception.Create(SBrowserNotActive);
    For I := 0 To Computers.Count-1 Do
       If (I=0) Then
          Result := Computers[I].RemoteName
       Else
          Result := Result+','+Computers[I].RemoteName;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNetworkBrowserComputers                                      }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNetworkBrowserComputers.Create(AOwner: TNetworkBrowserGroup);               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNetworkBrowserComputers.Create(AOwner: TNetworkBrowserGroup);
 Begin
    Inherited Create(TNetworkBrowserComputer);
    FWorkGroup := AOwner;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserComputers.Destroy;                                            }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNetworkBrowserComputers.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserComputers.Add: TNetworkBrowserComputer;                       }
{ Add method Overload                                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserComputers.Add: TNetworkBrowserComputer;
 Begin
    Result := TNetworkBrowserComputer(Inherited Add);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserComputers.GetItem(Index: Integer): TNetworkBrowserComputer;   }
{ Read Method property Items Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserComputers.GetItem(Index: Integer): TNetworkBrowserComputer;
 Begin
    FWorkGroup.Active := True;
    Result := TNetworkBrowserComputer(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TNetworkBrowserComputers.GetCount: Integer;                                  }
{ Read Method property Count Override                                          }
{------------------------------------------------------------------------------}
 Function TNetworkBrowserComputers.GetCount: Integer;
 Begin
    FWorkGroup.Active := True;
    Result := Inherited Count;
 End;

{------------------------------------------------------------------------------}
{ End of NetworkBrowser.pas                                                    }
{------------------------------------------------------------------------------}
 End.

