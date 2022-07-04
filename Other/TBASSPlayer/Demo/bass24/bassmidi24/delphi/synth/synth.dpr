program synth;
{$APPTYPE CONSOLE}
uses
  Windows, Bass, BassMidi;

function IntToStr(Value : integer) : string;
begin
  Result  := '';
  while (abs(Value) > 9) do begin
    Result  := chr(abs(Value) mod 10 + ord('0')) + Result;
    Value   := Value div 10;
  end; {while}
  Result  := chr(abs(Value) mod 10 + ord('0')) + Result;
  if (Value < 0) then
    Result  := '-' + Result;
end; {IntToStr}

// display error messages
procedure Error(const text : string);
begin
  WriteLn('Error(' + IntToStr(BASS_ErrorGetCode) + '): ' + text);
	BASS_Free;
  ReadLn;
end;

const
  cNoKeys = 20;
  cKeys     : array[1..cNoKeys] of char =
	              ('Q', '2', 'W', '3', 'E', 'R', '5', 'T', '6', 'Y', '7', 'U', 'I', '9', 'O', '0', 'P', #219, #187, #221);
	cFXNames  : array[0..8] of string =
                ('CHORUS', 'COMPRESSOR', 'DISTORTION', 'ECHO', 'FLANGER', 'GARGLE', 'I3DL2REVERB', 'PARAMEQ', 'REVERB');
var
	font    :	BASS_MIDI_FONT;
  info    : BASS_INFO;
  str     : HSTREAM;
  fx      : array[0..8] of HFX;
  buflen  : DWORD;
  keyin   : INPUT_RECORD;
  r       : DWORD;
  key     : integer;
  drums   : integer;
  pressed : array[1..cNoKeys] of boolean;
  preset  : byte;
begin

  WriteLn('BASSMIDI Simple Synth'#13#10+
	        '---------------------');
	
	if (HIWORD(BASS_GetVersion) <> BASSVERSION) then begin
    WriteLn('An incorrect version of BASS.DLL was loaded');                     // check the correct BASS was loaded
    ReadLn;
		Exit;
	end; {if}

  if (ParamCount > 0) then begin
    Write('loading soundfont... ');
		font.font := BASS_MIDI_FontInit(PChar(ParamStr(1)), 0);                     // initialized font
    if (font.font <> 0) then begin
			font.preset := -1;                                                        // all presets
			font.bank   := 0;                                                         // default bank(s)
			BASS_MIDI_StreamSetFonts(0, font, 1);                                     // make it the default
			WriteLn('ok');
		end else
      Error('failed');
  end else begin
		WriteLn('no soundfont provided (using Creative font if available)');
	  if (BASS_MIDI_StreamGetFonts(0, font, 0) <> DWORD(-1)) then begin           // no soundfont loaded...
      WriteLn('no soundfont loaded, please provide one in the command-line');
      ReadLn;
      Exit;
    end; {if}
  end; {if}

	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);                                 // 10ms update period

	if not BASS_Init(-1, 44100, BASS_DEVICE_LATENCY, 0, nil) then begin           // setup output - get latency
		Error('Can''t initialize device');
    Exit;
  end; {if}

	BASS_GetInfo(info);
	WriteLn('device latency: ' + IntToStr(info.latency) + 'ms');
	WriteLn('device minbuf: '  + IntToStr(info.minbuf)  + 'ms');
  if (info.dsver < 8) then
	  WriteLn('ds version: ' + IntToStr(info.dsver) + '(effects disabled)')
  else
	  WriteLn('ds version: ' + IntToStr(info.dsver) + '(effects enabled)' );

	
	BASS_SetConfig(BASS_CONFIG_BUFFER, 10 + info.minbuf);                         // default buffer size = update period + 'minbuf'
	buflen  := BASS_GetConfig(BASS_CONFIG_BUFFER);

	str     := BASS_MIDI_StreamCreate(1, BASS_MIDI_NOFX, 44100);                  // create a MIDI stream not using reverb / chorus effects
	WriteLn('press these keys to play:'#13#10#13#10 +
			    '  2 3  5 6 7  9 0  ='#13#10 +
			    ' Q W ER T Y UI O P[ ]'#13#10#13#10 +
			    'press -/+ to de/increase the buffer'#13#10 +
			    'press F11/F12 to change preset'#13#10 +
			    'press enter to toggle drums'#13#10 +
			    'press spacebar to quit'#13#10);

	if (info.dsver >= 8) then                                                     // DX8 effects available
		WriteLn('press F1-F9 to toggle effects'#13#10);

	Write('using a ' + IntToStr(buflen) + 'ms buffer'#13);

	BASS_ChannelPlay(str, false);

  drums   := 0;
  preset  := 0;

	while (ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), keyin, 1, r)) do begin

    if (keyin.EventType <> KEY_EVENT) then
      continue;

    if (keyin.Event.KeyEvent.wVirtualKeyCode = VK_SPACE) then
      break;

		if keyin.Event.KeyEvent.bKeyDown then begin
      case keyin.Event.KeyEvent.wVirtualKeyCode of
        VK_SUBTRACT,
        VK_ADD        : begin // recreate stream with smaller/larger buffer
                          BASS_StreamFree(str);
                          if (keyin.Event.KeyEvent.wVirtualKeyCode = VK_SUBTRACT) then
                            BASS_SetConfig(BASS_CONFIG_BUFFER, pred(buflen))        // smaller buffer
                          else
                            BASS_SetConfig(BASS_CONFIG_BUFFER, succ(buflen));       // larger buffer
                          buflen  := BASS_GetConfig(BASS_CONFIG_BUFFER);
	                        Write('using a ' + IntToStr(buflen) + 'ms buffer'#9#9#13);
                          str := BASS_MIDI_StreamCreate(1, BASS_MIDI_NOFX, 44100);
                          // set preset/drums/effects on the new stream
                          BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_PROGRAM, preset);
                          BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_DRUMS, drums);
                          for r := 0 to 8 do begin
                            if (fx[r] <> 0) then
                              fx[r] := BASS_ChannelSetFX(str, BASS_FX_DX8_CHORUS + r, 0);
                          end; {for}
                          BASS_ChannelPlay(str, false);
                        end;

        VK_F11        : if (preset > 0) then begin                                  // previous preset
                          dec(preset);
                          BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_PROGRAM, preset);
                          BASS_MIDI_FontCompact(0);                                 // unload unused samples
                          Write('preset = ' + IntToStr(preset) + #9#9#13);
                        end; {if}

        VK_F12        : if (preset < 127) then begin                                // next preset
                          inc(preset);
                          BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_PROGRAM, preset);
                          BASS_MIDI_FontCompact(0);                                 // unload unused samples
                          Write('preset = ' + IntToStr(preset) + #9#9#13);
                        end; {if}

        VK_RETURN     : begin                                                       // toggle drums
                          drums := (drums + 1) mod 2;
                          BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_DRUMS, drums);
                          BASS_MIDI_FontCompact(0);                                 // unload unused samples
                          if (drums = 0) then
                            Write('drums = OFF'#9#9#13)
                          else
                            Write('drums = ON'#9#9#13);
                        end;

			  VK_F1..VK_F9  : begin
                          r := keyin.Event.KeyEvent.wVirtualKeyCode - VK_F1;
                          if (fx[r] <> 0) then begin
                            BASS_ChannelRemoveFX(str, fx[r]);
                            fx[r] := 0;
                            Write('effect ' + cFXNames[r] + ' = OFF'#9#9#13);
                          end else begin  // set the effect, not bothering with parameters (use defaults)
                            fx[r] := BASS_ChannelSetFX(str, BASS_FX_DX8_CHORUS + r, 0);
                            if (fx[r] <> 0) then
                            Write('effect ' + cFXNames[r] + ' = ON'#9#9#13);
                          end; {if}
                        end;
      end; {case}
    end; {if}

		for key := 1 to cNoKeys do begin
			if (keyin.Event.KeyEvent.wVirtualKeyCode = ord(cKeys[key])) then begin
				if (keyin.Event.KeyEvent.bKeyDown and not pressed[key]) then
					pressed[key]  := BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_NOTE, MAKEWORD(60 - drums * 24 + key, 100))
			  else if ((not keyin.Event.KeyEvent.bKeyDown) and pressed[key]) then
					pressed[key]  := not BASS_MIDI_StreamEvent(str, 0, MIDI_EVENT_NOTE, 60 - drums * 24 + key);
      end; {if}
    end; {for}

  end; {while}

  Bass_Free;

end.
