// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  15890: mcmTWAINDSEntry.pas 
//
//    Rev 1.3    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.2    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.1    06-03-2003 11:39:24  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.0    04-12-2001 16:49:08  mcm    Version: DT 2.0

unit mcmTWAINDSEntry;

//------------------------------------------------------------------------------
{$IFDEF WIN32}
  {$A-} // Align data Byte = $A-, Word = $A+.
{$ELSE}
  {$A+} // Align data Byte = $A-, Word = $A+.
{$ENDIF}
{$F+} // Force FAR call.
{$J+}
{$N-} // CoProcessor code generation.
{$R-} // Range check.
{$W-} // Protected mode only $W-, Real mode stack frame support $W+.
{$X+} // Extended syntax (When on, function can be used as procedures).
//------------------------------------------------------------------------------

{$C PRELOAD MOVEABLE DISCARDABLE}

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, WinApi.Messages, Vcl.Dialogs,
     {$ELSE}
     Windows, Dialogs, Messages,
     {$ENDIF}
     twain;

//------------------------------------------------------------------------------
// Extern Function Prototypes
//------------------------------------------------------------------------------

procedure LibMain(ulReasonCalled : integer);

function DS_Entry(pSrc  : pTW_IDENTITY;
                  DG    : TW_UINT32;
		  DAT   : TW_UINT16;
                  MSG   : TW_UINT16;
		  pData : TW_MEMREF) : TW_UINT16; stdcall; export;


// Global variables
const TimerOut      = 999;      // timeout value for message boxes.

//var   SourceControl : array[0..MAX_TWPATH] of char;

const twStatus      : TW_STATUS
                    = (ConditionCode : TWCC_SUCCESS;
                       Reserved      : (0));

implementation

uses mcmTWAINDSLayer, uTwnDlgHandle;

//------------------------------------------------------------------------------
// FUNCTION: LibMain
//
// ARGS:     hLibModule      handle to current instance
//           ulReasonCalled
//           lpReserved
//
// RETURNS:  1
//
// NOTES:    This function intializes the Source DLL
//------------------------------------------------------------------------------

procedure LibMain(ulReasonCalled : integer);
begin
  if (hInstance <> 0)
  then begin
       case ulReasonCalled of
       DLL_PROCESS_DETACH : begin
                              try
                                if Assigned(TWAINSource)
                                then TWAINSource.Free;
                              except
                              end;
                              TWAINSource := Nil;
                            end;
       DLL_PROCESS_ATTACH : begin
   			      // State 3
                              TWAINSource := TTWAINSource.Create;
                            end;
       DLL_THREAD_ATTACH  : begin
                            end;
       DLL_THREAD_DETACH  : begin
                            end;
       end;
  end;
end; { End LibMain.                                                            }


//------------------------------------------------------------------------------
// FUNCTION: DS_Entry
//
// ARGS:    pSrc    ptr to caller, source of message
//          DG      data group ID, DG_IMAGE, DG_xxxx
//          DAT     data argument type, DAT_xxxx
//          MSG     message ID, MSG_xxxx
//          pData   pointer to the data
//
// RETURNS: twRC    status code
//
// NOTES:   This function is declared as the single entry point to the Source.
//          It should be exported as Ordinal function 1 (@1).
//------------------------------------------------------------------------------

function DS_Entry(pSrc  : pTW_IDENTITY;
                  DG    : TW_UINT32;
                  DAT   : TW_UINT16;
                  MSG   : TW_UINT16;
                  pData : TW_MEMREF) : TW_UINT16; stdcall;
var twMsg    : TTWMessage;
    twRc     : TW_UINT16;
    i        : integer;
    DlgFound : boolean;
begin
//------------------------------------------------------------------------------
// TW apps are sending down all their messages prior to process them
// to give us a chance to extract messages such as dialog keyboard
// handling, accelerator translation or MDI processing in a Source
// while the Source is enabled. We are doing this filtering prior to process
// real Source messages to minimize overhead.
// Relay windows messages down to the Source via the SM.  The SM will send
// a MSG_PROCESSEVENT to the Source to flag the arrival of a message relayed
// from the APP that was originally a windows message.
//
// Valid states: 5 -- 7.  As soon as the application has enabled the Source it
// must being sending the Source events.  This allows the Source to recieve
// events to update it's user interface and to return messages to the
// application.
// The app sends down ALL message, the Source decides which ones apply to it.
// added to keep stop Apps from talking to us until properly connected
// NOTE: Since SM peeks into Sources pSrc may be invalid (NULL) on some calls
// so the order of evaluation is key.
//------------------------------------------------------------------------------
  if Assigned(TWAINSource)
  then begin
       if ((TWAINSource.FappIdentity.Id = 0) or (TWAINSource.FappIdentity.Id = pSrc^.Id))
       then begin
            if (MSG = MSG_PROCESSEVENT)
            then begin
                 DlgFound := False;
                 twRc := TWRC_NOTDSEVENT;
                 i := 0;
                 while (i < DlgCount) and Not(DlgFound)
                 do begin
                    if (hDlg[i] <> 0)
                    then begin
                         if (IsDialogMessage(hDlg[i], TMsg(pTW_EVENT(pData)^.pEvent^)))
                         then begin
                              DlgFound := True;
                              twRc := TWRC_DSEVENT;
                              // You should for proper message handling return
                              // a MSG_NULL for all windows messages processed by the
                              // Data Source.
                              // NOTE: could slip by with a default case in the App
                              // and no MSG_NULL but that would be cheating!!
                              pTW_EVENT(pData)^.TWMessage := MSG_NULL;
                         end;

                         if (TMsg(pTW_EVENT(pData)^.pEvent^).hwnd = hDlg[i])
                         then begin
                              TMessage(pTW_EVENT(pData)^.pEvent^).Result := 0;
                              twRc := TWRC_DSEVENT;
                              pTW_EVENT(pData)^.TWMessage := MSG_NULL;
                         end;
                    end;
                    inc(i);
                 end;

                 if Not(DlgFound)
                 then begin
                      twRc := TWRC_NOTDSEVENT;
                      pTW_EVENT(pData)^.TWMessage := MSG_NULL;
                 end;
            end
            else begin
                 // Reset the condition code.
                 if (DG  <> DG_CONTROL) and
                    (DAT <> DAT_STATUS)
                 then twStatus.ConditionCode := TWCC_SUCCESS;

                 // Build the message for the dispatch routine.
                 twMsg.pSrc  := pSrc;
                 twMsg.DG    := DG;
                 twMsg.DAT   := DAT;
                 twMsg.MSG   := MSG;
                 twMsg.pData := pData;

                 // Route the Source message.
                 if (DG = DG_CONTROL)
                 then twRc := TWAINSource.DispatchMsg(@twMsg, DG, DAT)
                 else begin
                      if (DG = DG_IMAGE)
                      then twRc := TWAINSource.DispatchMsg(@twMsg, DG, DAT)
                      else begin
                           twStatus.ConditionCode := TWCC_BADPROTOCOL;
                           twRc := TWRC_FAILURE;
                      end;
                 end;
            end;
       end
       else twRc := TWRC_NOTDSEVENT;
  end
  else twRc := TWRC_NOTDSEVENT;
  DS_Entry := twRc;
end; { End DS_Entry.                                                           }

end.

