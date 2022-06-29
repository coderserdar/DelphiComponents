{------------------------------------------------------------------------------}
{ Mailslots                                                                    }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/09/2001                                                       }
{ Version   : 1.002 (Delphi 3/5)                                               }
{------------------------------------------------------------------------------}
 Unit Mailslots;

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
    Windows, SysUtils, Classes;

{------------------------------------------------------------------------------}
{ Constants                                                                    }
{------------------------------------------------------------------------------}
 Const
    MAILSLOT_NO_MESSAGE = -1;
    MAILSLOT_WAIT_FOREVER = -1;
    DEFMAXSIZE = 400;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TCustomMailslotServer = Class;
    TMailslotClient = Class;

    TMailslotKind = (mkLocal, mkComputer, mkDomain, mkBroadcast);
    TMailslotSendEvent = Procedure(Sender: TObject; Var Message: String) Of Object;
    TMailslotReceiveEvent = Procedure(Sender: TObject; Const Message: String) Of Object;

    TCustomMailslotServer = Class(TComponent)
    Private
      FHandle        : THandle;
      FName          : String;
      FTimeOut       : Integer;
      FMaxSize       : WORD;
      FOnReceive     : TMailslotReceiveEvent;
      FMessages      : TStrings;
      FMessageSize   : DWORD;
      FMessageCount  : DWORD;
      FCheckDelay    : Word;
      FLastItem      : String;
      FNewItem       : String;
      FStreamedActive: Boolean;
      FThread        : TThread;
      Function  GetActive: Boolean;
      Procedure SetActive(Const Value: Boolean);
      Procedure SetMailslotName(Const Value: String);
      Procedure SetMaxSize(Const Value: WORD);
      Procedure SetTimeMout(Const Value: Integer);
      Procedure SetMessages(Value: TStrings);
      Function  GetMailslotName: String;
      Procedure SetCheckDelay(Const Value: Word);
      Procedure ReadMailSlot;
    Protected
      Procedure DoConnect;                                                      Virtual;
      Procedure DoDisconnect;                                                   Virtual;
      Property CheckDelay: Word        Read FCheckDelay   Write SetCheckDelay;
      Property Active: Boolean         Read GetActive     Write SetActive;
      Property MailslotName: String    Read FName         Write SetMailslotName;
      Property TimeOut: Integer        Read FTimeOut      Write SetTimeMout     Default MAILSLOT_WAIT_FOREVER;
      Property MaxSize: WORD           Read FMaxSize      Write SetMaxSize      Default DEFMAXSIZE;
      Property Messages: TStrings      Read FMessages     Write SetMessages;
      Property OnMessageReceived: TMailslotReceiveEvent
                                       Read FOnReceive    Write FOnReceive;
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
      Procedure Loaded;                                                         Override;
    End;

    TMailslotServer = Class(TCustomMailslotServer)
    Published
      Property Active;
      Property MailslotName;
      Property TimeOut;
      Property MaxSize;
      Property Messages;
      Property OnMessageReceived;
    End;

    TCustomMailslotClient = Class(TComponent)
    Private
      FHandle : THandle;
      FName   : String;
      FTarget : String;
      FMessage: String;
      FKind   : TMailslotKind;
      FStreamedActive: Boolean;
      FOnSendMessage : TMailslotSendEvent;
      Function GetActive: Boolean;
      Procedure SetActive(Const Value: Boolean);
      Procedure SetMailslotName(Const Value: String);
      Procedure SetMailslotKind(Const Value: TMailslotKind);
      Procedure SetMailslotTarget(Const Value: String);
      Procedure SetMailslotMessage(Const Value: String);
      Function  GetMailslotName: String;
      Procedure PushMessage;
    Protected
      Procedure DoConnect;                                                      Virtual;
      Procedure DoDisconnect;                                                   Virtual;
      Property Active: Boolean             Read GetActive      Write SetActive;
      Property MailslotName: String        Read FName          Write SetMailslotName;
      Property MailslotKind: TMailslotKind Read FKind          Write SetMailslotKind;
      Property MailslotTarget: String      Read FTarget        Write SetMailslotTarget;
      Property MessageString: String       Read FMessage       Write SetMailslotMessage;
      Property OnSendMessage: TMailslotSendEvent
                                           Read FOnSendMessage Write FOnSendMessage;
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
      Procedure Loaded;                                                         Override;
    End;

    TMailslotClient = Class(TCustomMailslotClient)
    Published
      Property Active;
      Property MailslotName;
      Property MailslotKind;
      Property MailslotTarget;
      Property MessageString;
      Property OnSendMessage;
    End;

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
   SInvalidTimeOut = 'Invalid TimeOut value !';
   SComputer       = 'Computer';
   SDomain         = 'Domain';
   SInvalidDelay   = 'Invalid Check Delay (must be between 1 and 1000';

{------------------------------------------------------------------------------}
{ Private Types                                                                }
{------------------------------------------------------------------------------}
 Type
    TMailslotServerThread = Class(TThread)
    Private
      FMailslotServer: TCustomMailslotServer;
    Protected
      Procedure Execute; Override;
    Public
      Constructor Create(AMailslotServer: TCustomMailslotServer);
    End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomMailslotServer                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomMailslotServer.Create(AMailslot: TMailslot);                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomMailslotServer.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FHandle     := 0;
    FCheckDelay := 1;
    FName       := 'mailslotname';
    FTimeOut    := MAILSLOT_WAIT_FOREVER;
    FMaxSize    := DEFMAXSIZE;
    FMessages   := TStringList.Create;
    FThread     := TMailslotServerThread.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.Destroy;                                               }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomMailslotServer.Destroy;
 Begin
    Active := False;
    FMessages.Free;
    FThread.Terminate;
    FThread.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.Loaded;                                                }
{ Loaded procedure Override                                                    }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.Loaded;
 Begin
    Inherited Loaded;
    Active := FStreamedActive;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.GetActive: Boolean;                                    }
{ Read method Property Active                                                  }
{------------------------------------------------------------------------------}
 Function TCustomMailslotServer.GetActive: Boolean;
 Begin
    Result := (FHandle<>0);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetActive(Const Value: Boolean);                       }
{ Write method Property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetActive(Const Value: Boolean);
 Begin
    If Value Then
       DoConnect
    Else
       DoDisconnect;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetName(Const Value: String);                          }
{ Write method Property Name                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetMailslotName(Const Value: String);
 Begin
    Active := False;
    FName := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetTimeMout(Const Value: Integer);                     }
{ Write method Property TimeOut                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetTimeMout(Const Value: Integer);
 Begin
    If (FTimeOut<0) And (FTimeOut<>MAILSLOT_WAIT_FOREVER) Then
       Raise Exception.Create(SInvalidTimeOut);
    FTimeOut := Value;
    If Active Then
       SetMailSlotInfo(FHandle, FTimeOut);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetMessages(Value: TStrings);                          }
{ Write method Property Messages                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetMessages(Value: TStrings);
 Begin
    FMessages.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetMaxSize(Const Value: WORD);                         }
{ Write method Property MaxSize                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetMaxSize(Const Value: WORD);
 Begin
    Active := False;
    FMaxSize := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.GetMailslotName: String;                               }
{ Read method Property MailSlotName                                            }
{------------------------------------------------------------------------------}
 Function TCustomMailslotServer.GetMailslotName: String;
 Begin
    Result := Lowercase('\\.\mailslot\'+FName);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.DoConnect;                                             }
{ Connection of the server mailslot                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.DoConnect;
 Begin
    If Active Then Exit;
    If (csLoading In ComponentState) Then Begin
       FStreamedActive := True;
       Exit;
    End;
    FHandle := CreateMailslot(PChar(GetMailslotName), FMaxSize+9, DWORD(FTimeOut), Nil);
    If (FHandle=INVALID_HANDLE_VALUE) Then Begin
       FHandle := 0;
       Raise Exception.Create(SysErrorMessage(GetLastError));
    End;
    FThread.Resume;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.DoDisconnect;                                          }
{ DisConnection of the server mailslot                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.DoDisconnect;
 Begin
    If Not Active Then Exit;
    FThread.Suspend;
    CloseHandle(FHandle);
    FHandle := 0;
    FMessages.Clear;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetCheckDelay(Const Value: Word);                      }
{ Write method property CheckDelay                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.SetCheckDelay(Const Value: Word);
 Begin
    If (Value<=0) Or (Value>1000) Then
       Raise Exception.Create(SInvalidDelay);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.ReadMailSlot;                                          }
{ Synchronize method                                                           }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotServer.ReadMailSlot;
 Var
    {$IFDEF VER100}
    BytesRead: Integer;
    {$ELSE}
    BytesRead: Cardinal;
    {$ENDIF}
    ReadValue: String;
    TrueValue: String;
    SpacePos : Integer;
 Begin
    SetLength(ReadValue, FMessageSize);
    If Not ReadFile(FHandle, PChar(ReadValue)^, FMessageSize, BytesRead, Nil) Then
       Raise Exception.Create(SysErrorMessage(GetLastError));
    SpacePos := Pos(' ', ReadValue);
    FNewItem := Copy(ReadValue, 1, SpacePos);
    If AnsiCompareText(FLastItem, FNewItem)=0 Then Exit;
    FLastItem := FNewItem;
    TrueValue := Copy(ReadValue, SpacePos+1, Length(ReadValue)-SpacePos);
    FMessages.Add(TrueValue);
    If Assigned(FOnReceive) Then
       Self.FOnReceive(Self, TrueValue);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomMailslotClient                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomMailslotClient.Create(AOwner: TComponent);                            }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomMailslotClient.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FHandle   := 0;
    FName     := 'mailslotname';
    FTarget   := '*';
    FKind     := mkBroadcast;
    FMessage  := '';
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.Destroy;                                               }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomMailslotClient.Destroy;
 Begin
    Destroying;
    Active := False;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.Loaded;                                                }
{ Loaded procedure Override                                                    }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.Loaded;
 Begin
    Inherited Loaded;
    Active := FStreamedActive;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.GetActive: Boolean;                                    }
{ Read method Property Active                                                  }
{------------------------------------------------------------------------------}
 Function TCustomMailslotClient.GetActive: Boolean;
 Begin
    Result := (FHandle<>0);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.SetActive(Const Value: Boolean);                       }
{ Write method Property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.SetActive(Const Value: Boolean);
 Begin
    If Value Then
       DoConnect
    Else
       DoDisconnect;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotServer.SetName(Const Value: String);                          }
{ Write method Property Name                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.SetMailslotName(Const Value: String);
 Begin
    Active := False;
    FName := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.SetMailslotKind(Const Value: TMailslotKind);           }
{ Write method Property Kind                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.SetMailslotKind(Const Value: TMailslotKind);
 Begin
    Active := False;
    FKind  := Value;
    Case FKind Of
       mkLocal:
         FTarget := '.';
       mkComputer:
         If (FTarget='.') Or (FTarget='*') Or (AnsiComparetext(FTarget, SDomain)=0) Then
            FTarget := SComputer;
       mkDomain:
         If (FTarget='.') Or (FTarget='*') Or (AnsiComparetext(FTarget, SComputer)=0) Then
            FTarget := SDomain;
       mkBroadcast:
         FTarget := '*';
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.SetMailslotTarget(Const Value: String);                }
{ Write method Property Target                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.SetMailslotTarget(Const Value: String);
 Begin
    Active := False;
    Case FKind Of
       mkLocal:
         FTarget := '.';
       mkComputer:
         FTarget := Value;
       mkDomain:
         FTarget := Value;
       mkBroadcast:
         FTarget := '*';
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.SetMailslotMessage(Const Value: String);               }
{ Write method Property Message                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.SetMailslotMessage(Const Value: String);
 Begin
    FMessage := Value;
    PushMessage;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.GetMailslotName: String;                               }
{ Read method Property MailSlotName                                            }
{------------------------------------------------------------------------------}
 Function TCustomMailslotClient.GetMailslotName: String;
 Begin
    Result := Lowercase('\\'+FTarget+'\mailslot\'+FName);
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.DoConnect;                                             }
{ Connection of the server mailslot                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.DoConnect;
 Begin
    If Active Then Exit;
    If (csLoading In ComponentState) Then Begin
       FStreamedActive := True;
       Exit;
    End;
    FHandle := CreateFile(PChar(GetMailslotName), GENERIC_WRITE, FILE_SHARE_READ Or FILE_SHARE_WRITE, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    If (FHandle=INVALID_HANDLE_VALUE) Then Begin
       FHandle := 0;
       If Not (csDesigning In ComponentState) Then
          Raise Exception.Create(SysErrorMessage(GetLastError))
       Else
          Exit;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.DoDisconnect;                                          }
{ DisConnection of the server mailslot                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.DoDisconnect;
 Begin
    If Not Active Then Exit;
    CloseHandle(FHandle);
    FHandle := 0;
 End;

{------------------------------------------------------------------------------}
{ TCustomMailslotClient.PushMessage;                                           }
{ Send Message                                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomMailslotClient.PushMessage;
 Var
    {$IFDEF VER100}
    BytesWritten: Integer;
    {$ELSE}
    BytesWritten: Cardinal;
    {$ENDIF}
    Tmp: String;
 Begin
    If Not Active Or (FMessage='') Then Exit;
    If Assigned(FOnSendMessage) Then
       FOnSendMessage(Self, FMessage);
    Tmp := IntToHex(GetTickCount, 8)+' '+FMessage;
    BytesWritten := 0;
    If Not WriteFile(FHandle, PChar(Tmp)^, Length(Tmp), BytesWritten, Nil) Then
       Raise Exception.Create(SysErrorMessage(GetLastError));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TMailslotServerThread                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TMailslotServerThread.Create(AMailslotServer: TCustomMailslotServer);        }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TMailslotServerThread.Create(AMailslotServer: TCustomMailslotServer);
 Begin
    Inherited Create(True);
    FMailslotServer := AMailslotServer;
 End;

{------------------------------------------------------------------------------}
{ TMailslotServerThread.Execute;                                               }
{ Execute method Override                                                      }
{------------------------------------------------------------------------------}
 Procedure TMailslotServerThread.Execute;
 Begin
    While Not Terminated Do
       With FMailslotServer Do Begin
          If Not GetMailslotInfo(FHandle, Nil, FMessageSize, @FMessageCount, Nil) Then
             Raise Exception.Create(SysErrorMessage(GetLastError));
          If (FMessageCount>0) Then
             Synchronize(ReadMailSlot)
          Else
             Sleep(CheckDelay);
       End;
 End;

{------------------------------------------------------------------------------}
{ End of mailslots.pas                                                         }
{------------------------------------------------------------------------------}
 End.
