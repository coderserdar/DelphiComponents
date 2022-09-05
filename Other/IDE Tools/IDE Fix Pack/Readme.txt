IDE Fix Pack for Delphi - Version 6.4.3 (2019-07-24)
(C) 2008-2019 Andreas Hausladen
Andreas.Hausladen@gmx.de


Installation:
=============
1. Start IDEFixPackReg.exe under your user account
2. Select the IDE registry keys for which you want to install the bugfix
3. Press the "Install" button


Uninstallation:
===============
1. Start IDEFixPackReg.exe under your user account
2. Press the "Uninstall" button. This will uninstall the bugfix for all
   (not only the selected) IDE registry keys.


Compiler Speed Pack x86 - compiler options:
===========================================
  -x-O1              Enable options -x-fvs   -x-fpr
  -x-O2              Enable options -x-fvs   -x-fpr -x-orc
  -x-O3              Enable options -x-fvs=2 -x-fpr -x-orc
  -x-Ox              Enable ABI changing optimizations -O3 -x-orc=2 -x-ff
  -x-Oe              Enable experimental options -O3 -x-cgo

  -x-fdi             Fold duplicate dll import sections and names (default: on)
  -x-ff              Fast floating point (removes FWAIT)
  -x-fvs             Generate faster interface virtual method stub (same as -x-fvs=1)
  -x-fvs=n           Generate faster interface virtual method stub
                       n=0  disable
                       n=1  Replaces XCHG, uses ECX if available, uses final and sealed information
                       n=2  Like n=1 + replaces RET with JMP (return stack buffer)
  -x-fpr             Enable fast function prolog (replaces XCHG and slow memory page probing)
  -x-orc             Omit temporary (managed) records for function results (same as -x-orc=1)
  -x-orc=n           Omit temporary (managed) records for function results
                       n=0  Disable
                       n=1  Omit temporary result record if used in last statement
                       n=2  Omit all temporary result records (treat as var parameter)
  -x-cgo             Code generator emits better optimized code

  -x--compileonly    Output intermediate files, skip linking (same as -x--compileonly=1)
  -x--compileonly=n  Output intermediate files, skip linking
                       n=0  Disable (default)
                       n=1  Output only DCU and DCP
                       n=2  Output only DCU
  -x--reslist        Output used resource file names (Same as -x--reslist=1)
  -x--reslist=n      Output used resource file names
                       n=0  Disable (default)
                       n=1  <target>.resfilelist (UTF8)
                       n=2  <target>.resfilelist (ANSI)
                       n=3  <target>.map (ANSI)
  -x--depfile        Output all file dependencies to a DEP file (same as -x--depfile=3)
  -x--depfile=n      Output all file dependencies to a DEP file
                       n=0  Disable (default)
                       n=1  <target>.dep file (UTF-8)
                       n=2  <target>.dep.json file
                       n=3  <target>.dep.json file with dllimport information
  -x--unitstats      Output the unit tree to a file (same as -x--unitstats=1)
  -x--unitstats=n    Output unit information to a file
                       n=0  Disable (default)
                       n=1  Uses lists per unit: <target>.unitstats (UTF-8)
                       n=2  Uses lists per unit: <target>.unitstats.json
                       n=3  Uses lists and statistics: <target>.unitstats.json

-x-fdi
------
Delphi generates multiple DLL import sections for the same DLL. Windows 10 Creators Update 1703
changed how DLLs are loaded and now multiple sections for one DLL cause performance issues and
can crash the debugger. This is fixed with Windows 1709 but it is still better to have only one
import section per DLL. Delphi 10.2 Update 1 fixed this issue for the static dll imports but not
for the delay dll imports.
This option is enabled by default, if you want to disable it you have to turn it off by appending a
minus symbol to the option: "-x-fdi-".
  
-x-ff
-----
With this option the Win32 compiler doesn't emit the "FWAIT" instruction after floating point
operations. This also means that FPU exceptions (div by zero, ...) may not be thrown at the
source line that caused them.

-x-fvs / -x-fvs=1
-----------------
If a virtual method is used through an interface the compiler emits helper code that translates
the interface call into a virtual method call. This code uses the XCHG instruction that implies
a CPU lock. With -x-fvs=1 the compiler will emit an alternative code for XCHG and if the
ECX register can be freely used (function with less than 3 parameters) a much faster helper code
is emitted.
Furthermore if the interface method is a virtual method that is marked as final or is in a sealed
class, the compiler will emit helper code as if it is a static method.

-x-fvs=2
--------
Same as -x-fvs=1 with the addition that if the ECX register is not available the helper's RET
instruction is replaced by a JMP what helps the CPU to keep the "return stack cache" valid. This
is done by accessing stack memory below ESP.

-x-fpr
------
For some functions that meet special conditions the compiler emits stack frame code that fills
the stack with zeros to clear variables with managed types. If there are too many of those the compiler
uses the XCHG instruction to restore the ECX register. With this option the XCHG is replaced with an
alternative code.
If the size of local variables exceed a memory page (4KB) the compiler generates memory page probing
code that is a lot faster than the original compiler's code.

-x-orc / -x-orc=1
-----------------
In functions that return a (managed) record, this option allows the compiler to remove the temporary
record copy if "Result := Func();" is the last statement in the function, as the caller already has
a copy of the record or has the result record assignment also as the last statement.

-x-orc=2
--------
All temporary function return record buffers with managed fields will be eliminated.
Records whos Size(Rec)<=Size(Pointer) with managed fields will always call the function with the
original record pointer. If an exception is thrown in the callee, the caller's target record may
be altered. This is a break in the ABI that is already happening for Size(Rec)>Size(Pointer) records
within the original compiler.

-x-cgo
------
Enables an Integer division code generator optimization where the compiler emitted a push/pop where
it could use the target EDX cpu register instead.

Unoptimized code:
   push dword ptr [eax+$1c]
   mov eax,ecx
   pop edx
   sub eax,edx
   mov ecx,$00000014
   cdq 
   idiv ecx

Optimized code (no push/pop):
   lea edx,[eax+$1c]
   mov eax,ecx
   sub eax,[edx]
   mov ecx,$00000014
   cdq 
   idiv ecx


Changelog:
==========
2019-07-31: (6.4.4)
  - Fixed: CodeGenMod.Win32.VirtFinalSealed was broken for Delphi 10.3.2
  - Removed: IDE Insight Listbox drop shadow patch isn't necessary for Delphi 10.3.2 anymore

2019-07-24 (6.4.3)
  - Added: Delphi 10.3.2 (Rio Update 2) support

2019-03-23 (6.4.2)
  - Fixed: Unit-Name HashMap could cause an internal compiler error if the debugger removed System/SysInit from the unit list without IDEFixPack's knowing.
  - Fixed: TCustomListBox.ResetContent patch caused an EInvalidPointerOp exception if the IDE or a component designer used TCheckListBox. 

2019-03-03 (6.4.1)
  - Fixed: StyleUtils.inc patch contained a hard coded address causing the patch to fail if the DLL's base address was relocated. (10.3 only)
  - Fixed: TCustomListBox.ResetContent patch crashed with an access violation. (2010 only)

2019-03-01 (6.4)
  - Fixed: Packages with duplicate units may not have caused a fatal compiler error.
  - Added: Support for Delphi 10.3 Rio
  - Added: StyleUtils.inc performance optimizations for faster UI rendering (D10.3)
  - Added: Infinite loop detection in TDebugger.UpdateEventLog
  - Added: Fix for TStringList.IndexOfName bug (RSP-21633)
  - Added: Fix for access violation in the Welcomepage JScript9.dll binding
  - Added: TCustomListBox.ResetContent is skipped if the handle isn't created yet
  - Added: More STRINGCHECKS-free RTL code (2009/2010)
  - Added: More DFM Streaming optimizations
  - Added: RTL and DFM streaming patches to remove STRINGCHECKS (2009/2010)
  - Added: Removed VclFixPack OutputDebugString calls (2009)
  - Added: FillChar uses Enhanced REP MOVSB/STOSB cpu feature if available for large sizes.
  - Added: Enabled CPU LOCK string assignment optimization for local variables
  - Added: -x-Oe (experimental optimizations) and -x-cgo compiler option extension
  - Added: CodeGen: Remove of some unnecessary push/pop operations
  - Added: Expression Evaluator allows array access to pointers even if the type wasn't declared with {$POINTERMATH ON}
  - Added: Max number of constants in a function was increased from 64K to 16M (2009-XE3)
  - Added: New compiler option extensions: -x--compileonly, -x--reslist, -x--depfile, -x--unitstats
  - Added: More performance optimization for the DCC64 compiler
  - Added: TStringBuilder.SetLength optimization [RSP-19178] (XE+)
  - Added: TStrings.GetDelimitedText optimization
  - Improved: Slight faster TStringList.IndexOfName optimization for sorted string lists.

2018-06-17 (6.3.1)
  - Fixed: Incompatiblity with CodeInsightPlus (UTF8ToUnicode appends a #0)

2018-06-17 (6.3)
  - Added: Patch to remove IDE flickering when WM_SETTINGCHANGE is broadcasted
  - Added: Fix for RSP-20700: Tooltip Help Insight is blinking if Structure View is scrolled
  - Added: Undo XE3+ TFiler/TReader/TParser/TStream TBytes usage, replace SetLength with SetLengthUninit for special cases
  - Added: -x-fpr generates 3 times faster stack memory page probing code (RSP-19826)
  - Added: Options -x-O1, -x-O2, -x-O3, -x-Ox that enable other optimization options

2018-01-25 (6.2)
  - Fixed: Directory search cache failed if project units had "..\" in it (Delphi 10.2 only)
  - Fixed: Some VirtualProtect calls specified nil as last argument what Windows 10 1709 doesn't like if a debugger is attached.
  - Fixed: Faster IndexOfName for sorted TStringList used inverted CaseInsentive (isn't used by the IDE)
  - Added: Option -x-orc and -x-orc=n to remove temporary record variables for function results (n=1: only if the assignment is the last statement, n=2 for all)

2017-12-14 (6.1.2)
  - Added: Support for Delphi 10.2 Update 2
  - Added: Fix for unnecessary temporary variable if an empty open array argument is part of a function call (Delphi 2009-10.1)

2017-10-30 (6.1.1)
  - Fixed: -x-fvs (fast interface virtual stub) ECX usage detection failed for some record return types.

2017-10-29 (6.1)
  - Added: Option -x-ff to enable "fast floating point" (like Borland C++'s -ff command line option)
  - Added: Option -x-fvs and -x-fvs=n to enable fast interface virtual stub (n=1: replace XCHG, n=2: keep the CPU's return stack buffer in order)
  - Added: Option -x-fpr to remove XCHG from the function prolog code.
  - Added: DLL import table section folding and duplicate name/ordinal elimination, also for delay dll imports
  - Changed: Split "Compiler64.X86" patch into multiple smaller patches and removed the "Compiler64.X86" patch name
  - Changed: EditorFocusFix now skips the SetActiveWindow call if the mainform (undocked) is not the active window

2017-09-28 (6.0)
  - Added: Support for RAD Studio 10.2 Tokyo Update 1
  - Added: Editor Block Completion UTF8 fix (Delphi 2009 only)
  - Added: Win64 compile speed optimizations (Delphi XE2 and newer)

2017-04-09 (5.96)
  - Fixed: Disable DynArraySetLength patch if 10.1 Berlin Update 2 is detected (Delphi 10.1 Berlin only)
  - Fixed: "clang template debug symbol bloat" disabled for 10 Seattle and newer. (Delphi 10 Seattle and newer)
  - Added: Support for RAD Studio 10.2 Tokyo
  - Added: IDE minimize doesn't shrink main window to width and height zero.

2016-05-30 (5.95)
  - Added: Support for RAD Studio 10.1 Berlin (disabled Compiler generics performance patch)
  - Added: CloseHandle for created DCU files is delegated to a background thread. Windows Defender "workaround"
  - Added: Fix for RSP-13116: TCustomImageList.BeginUpdate/EndUpdate (Delphi XE8-10 Seattle)
  - Added: Fix for RSP-14557: DynArraySetLength - resizing an array of managed type is causing entire copy instead of realloc (Delphi 10.1 Berlin only)

2015-11-01 (5.94)
  - Added: Support for RAD Studio 10 Seattle (Removed unnecessary patches)
  - Added: Patch for Clipboard History exception from 10 Seattle Castalia integration (Delphi 10 Seattle and newer)
  - Added: timeBeginPeriod/timeEndPeriod calls from IDEVirtualTrees disabled (battery drain)
  - Added: CodeInsight popup window border fix for Windows 10.
  - Added: Removed unnecessary memory reallocations for 64bit and AARM compiler (Delphi XE4 and newer)

2015-08-03 (5.93)
  - Added: Windows 10 support

2015-05-16 (5.92)
  - Fixed: TStringList.IndexOfName optimization broke UniDAC's designtime editor
  - Fixed: "Find References" shortcut was assigned to "Find Local References" unless the search menu was opened
  - Added: "Find Local References" shortcut Shift+Ctrl+Alt+Enter

2015-04-23 (5.91)
  - Fixed: IDEFixPack didn't take into account that DB.TFlatList uses late loading through GetCount
  - Fixed: fastdcc32/64/aarm in XE7 and XE8 didn't load the CompilerSpeedPack into the compiler's process

2015-04-21 (5.9)
  - Fixed: FieldAddress optimization could fail if a published field name was also used in an ancestor
  - Added: Support for XE8 (removed unnecessary patches)
  - Added: Castalia XE8 startup optimization (Delphi XE8 only)
  - Added: Fix for QC 71270, 76338 and 76379. "CodeInsight doesn't like the underscore."
  - Added: TStringList.IndexOfName optimization

2015-02-15 (5.8)
  - Fixed: Windows 8.1 compatibility with .NET code patch (Delphi 2009 only)
  - Fixed: IDE Fix Pack tried to patch the fastdcc32/64/aarm.exe if it was renamed to dcc32/64/aarm.exe
  - Added: IDE Insight [F6]-Key calls SelectAll (Delphi XE5 and newer)
  - Added: Compiler cleanup code performance optimization for large packages
  - Added: If XE7's rlink32.dll is detected in older Delphi versions the "too many resources to handle" patch is skipped
  - Changed: Installer/Uninstaller doesn't require rtl.bpl/vcl.bpl in the PATH anymore
  - Improved: IDE Fix Pack StringLists use CompareStringOrdinal instead of CompareString

2014-09-16 (5.71)
  - Improved: EditorLineBreak.ttr patch not only prevents the file loading but also deletes/renames the existing file

2014-08-25 (5.7)
  - Fixed: CompilerSpeedPack to external compiler execution dead locked when debugging DCC32/64/AARM.EXE
  - Added: Support for XE7 (removed unnecessary patches)
  - Added: TComponent.Remove optimization (Delphi 2009 only)
  - Added: TObject.FieldAddress optimization for DFM loading
  - Added: String function replacements without STRINGCHECKS (Delphi 2009-XE)
  - Added: Fix for QC 111917: RLINK32: Too many resources to handle. Instead of 3626 it can now handle 65534 resource items
  - Added: Fix for "class operator In" class completion
  - Added: IDEFixPackReg*.exe supports command line parameter /EXTRACT that extracts the files instead of installing them
  - Added: Disable creation of EditorLineEnd.ttr (Delphi 2009/2010 only)
  - Changed: Disabled ".NET Windows\Globalization search I/O optimization" patch for Windows 8 (Delphi 2009 only)

2014-05-10 (5.6)
  - Added: Support for XE6 (removed unnecessary patches)
  - Added: Apply CompilerSpeedPack to external compiler execution (dcc32, dcc64, dccaarm)
  - Added: CompilerSpeedPack for Android

2014-05-04 (5.5)
  - Fixed: Debug symbols are not always loaded for runtime packages
  - Fixed: Work around access violation in TComponentRoot.IsRootSelected
  - Added: Fix for QC 111998: $TDW macro used in Tools causes crash (Delphi XE2-XE5)
  - Added: CLANG BCC64 debuginfo bloat reduction for nested templates that caused the linker to run out of memory (Delphi XE3-XE8)
  - Improved: Splash screen progressbar accuracy

2013-12-11 (5.4.1)
  - Added: XE5 Update 2 support

2013-11-02 (5.4)
  - Fixed: Switching between C++64 and C++32 platform caused "String list does not allow duplicates"
  - Fixed: Possible access violation in PageControlTabSheetCompDropFix.DeleteSelection
  - Fixed: Possible infinite loop in compiler patch
  - Added: Support for XE5 (removed all unnecessary patches)
  - Added: Faster inline function compilation (Delphi 2009-XE)
  - Added: Much better compiler performance if a unit instantiates many Generics
  - Added: Fix for *.tds symbols aren't loaded if the debugger is started with a relative HostApp
  - Added: Hashtable for Compiler directives lookup (improves jedi.inc compilation speed) (Delphi 2009/2010 only)
  - Added: Hashtable for symbol lookup in used units
  - Added: Optimization for: DCC64 was busy allocating and zero-ing memory for units with many functions (e.g. Generics) (Delphi XE2 and newer)
  - Added: Optimization for: DCC64 was busy searching a linear list for units with many Generics. (Delphi XE2-10.0 Berlin)
  - Removed: Increased Linker I/O buffer

2013-06-11 (5.31)
  - Fixed: XE3 Win64 compiler memory leak patch crashed the compiler with an internal error. (Delphi XE3 only)

2013-06-07 (5.3)
  - Added: Support for XE4 (Removed all unnecessary patches)
  - Added: Fix for QC 111500: Debugger truncates command line
  - Added: HelpInsight performance optimization (Delphi XE/XE2 only)
  - Added: HelpInsight background WaitFor bugfix
  - Added: Compiler performance optimization for many Generics (Delphi XE-XE4 only)
  - Added: Fix for QC 116290: Code Completion in class declaration doesn't find message handlers if Messages is not included by using WinApi.Messages (Delphi XE2 and newer)
  - Added: XP SelectDirectory dialog replaced by Vista version if available
  - Added: Some slight C++Builder Kibitz/Compiler performance optimization
  - Added: Faster Unicode Pos function
  - Added: Performance optimization for IOTAxxxx.AddNotifier where Add/RemoveNotifier is called for many items (e.g. Local Variables/Watches)
  - Added: Fix for QC 116008: IDE always does the compile->link cycle even if there where no modifications made to the project
  - Added: Fix for not disappearing Designer Guidelines
  - Added: Editor Buffer now always disables the automatic/implicit changes to the buffer if undo is disabled.
  - Added: Fix for QC 105642: Ctrl+C causes premature line cut and garbage in the clipboard
  - Added: Fix for QC 115248: Huge memory leak in BDS.EXE with large local variables or watches (Delphi XE to XE6)
  - Added: Directory selection dialogs in IDE Options and Project Options use the $(Platform) and $(Config) from the option dialog (Delphi XE2 and newer)
  - Added: Improved startup performance (Delphi XE3 and newer)
  - Added: Memory leak fix for Win64 compiler (Delphi XE3 and newer)
  - Added: TStream.WriteBuffer/ReadBuffer performance optimization (DFM streaming) (Delphi XE3 and newer)
  - Added: Fix for QC 105668: Last line of the multi-line comment is not repainted (Delphi XE3 and newer)
  - Added: Fix for QC 116169: Slow opening forms in IDE that don't use LiveBindings (Delphi XE3-XE7)
  - Added: Fix for QC 116373: VCL Designer Guidelines spanning to the IDE's main form instead of the designer form. Revert to X3 behavior. (Delphi XE4 only)
  - Rewritten: Linker I/O buffer memory handling
  - Improved: comp32x.dll isn't loaded at IDE startup anymore
  - Improved: IDEFixPackReg*.exe supports command line parameters (see "IDEFixPackReg*.exe /?")

2012-12-18 (5.2)
  - Fixed: IDEFixPack crashed the IDE under Windows 8. (Delphi 2009 only)

2012-12-11 (5.1)
  - Added: Support for XE3 (removed all unnecessary patches)
  - Added: Disable Package Cache option in the installer
  - Added: Bug fix for find in files Umlauts issue
  - Added: Replacement for slow Compiler C-RTL functions (memcpy, memmove, memset) (Delphi 2009 only)
  - Added: No LoadStringRes calls for disabled warnings (UnsafeCode, Assertions...)
  - Added: Special memory allocator for lookahead tokens
  - Added: Fix for TLabel.AutoSize is ignored when a new TLabel is dropped onto the form (Delphi 2009 only)
  - Added: Fix for Right click in the editor switch block selection flag but did not reset it (Delphi 2009 only)
  - Added: Fix for ToDo List could cause an access violation when the IDE terminates (Delphi 2009 only)
  - Added: Fix for Watch window sometimes lost the PopupMenu shortcuts (Delphi 2009 only)
  - Added: Fix for CompileProgress PopupMode
  - Added: DFM read/inheritance error dialogs are sizeable
  - Added: Designer DFM Loading Form Modified-Flag patch (Delphi 2009 only)
  - Added: Fix for QC 7270. Invisible TabSheet was activated
  - Added: Replaced slow inlined strcpy for file name copy (Delphi XE2 Update 4 + Hotfix 1 and newer)

2012-08-01 (5.0)
  - Fixed: RC files couldn't be opened by double clicking in the ProjectManager
  - Fixed: Random order of BuildConfiguration tags in the dproj file
  - Added: Much improved linker performance if generics are used
  - Added: Parser for Find Unit (for Symbol) can be canceled by pressing ESC
  - Added: Refactor actions are only available after the parser has finished
  - Added: Refactor actions don't start the parser if the user pressed an unrelated shortcut
  - Added: ObjectTextToBinary performance optimization for Linker and the FormDesigner
  - Added: Compile Dialog accepts the cancel button when only up to date DCU files are loaded but no actual unit compilation is done
  - Added: Protect the editor from CnWizard's access from Application.OnIdle (Random AVs when closing tabs)
  - Added: Fix for QC#82199: Saving from the DFM as text editor truncates the PAS file by the number of Unicode chars (Delphi 2009/2010 only)
  - Added: Fix for compiler bug where a memory allocator returned an invalid address due to an overflow
  - Added: Fix for IDE removing form-unit from project when switching from DFM Text editor to FormDesigner with errors in the DFM
  - Added: Enabled "Ignore errors when kibitzing" patch for XE+
  - Added: comp32x.dll keeps file handles open that shouldn't belong to this debugger instance (Delphi XE2 and newer)
  - Removed: DisablePackageCache patch (too many component vendors now depend on not loading all packages)

2012-04-24 (4.9.1)
  - Fixed: The compiler didn't find or compile not saved files when AutoSave=off

2012-04-24 (4.9)
  - Added: HelpInsight (Ctrl+Shift+H) doesn't invoke the ErrorInsight parser in its OnUpdate in the main thread anymore
  - Improved: The Directory Cache isn't reset for every DCU the ErrorInsight parser wants to read.
  - Fixed: Patch for QC83992 (TImageList) didn't work when Windows was in classic style.

2012-04-11 (4.8)
  - Added: Fix for QC83992 (TImageList is duplicated in inherited forms)
  - Fixed: Missing Patch: Improved IDE ProjectManager performance
  - Fixed: Missing Patch: Vista compatible main icon resource

2012-04-07 (4.7.1)
  - Fixed: Background Compiler can be used again (Delphi 2010 only)
  - Fixed: IDE lost the debugger state and disabled all Debugging functions after terminating a process when Error/HelpInsight started parsing the code

2012-02-29 (4.7)
  - Added: Improved DataModule Code to Design switch performance (Delphi XE2 only)
  - Added: Improved Component Toolbar performance when switching between Code and Design (Delphi 2010-XE2 only)
  - Added: Package and Palette Caches are disabled
  - Added: Code editor parenthesis highlighting knows about string literals
  - Added: Closing files/projects cancels the background parser
  - Added: Debugger now calls CreateProcess with CREATE_DEFAULT_ERROR_MODE
  - Added: Improved IDE ProjectManager performance
  - Added: Fix for QC #103388: Include System Variables isn't working anymore (Delphi XE2 only)

2012-01-04 (4.6.6)
  - Fixed: Last directory cache fix broke the debugger's source file search if you use relative paths

2012-01-02 (4.6.5)
  - Fixed: Directory cache didn't find units in the DPR's directory anymore
  - Added: Editor undo for a large undo operation is a lot faster

2011-12-27 (4.6.1):
  - Fixed: Directory cache didn't find files if CodeInsight was invoked in a uses-clause
  - Fixed: Possible access violation during in memory unit clean up

2011-12-15 (4.6):
  - Fixed: XML Serializer cache race condition
  - Added: Memory leak fix for Debugger's GetModuleFileNameEx calls (Delphi 2009/2010 only)
  - Added: Improved the performance when debugging the IDE
  - Added: After compile/debug/closeall unused compiler memory is released
  - Added: Compiler Package optimization
  - Added: Delphi-Debugger GetFileIndex optimization
  - Added: LiveBindings slowed down the IDE (Delphi XE2 and newer)
  - Added: Debugger memory leak fix (XE2 and newer)
  - Improved C++ Debugger symbol reader performance

2011-11-11 (4.51):
  - Added: Fix for linker's memory leak (Delphi 2010 only)

2011-11-02 (4.5):
  - Added: Support for Delphi XE2 (removed unnecessary patches)
  - Added: Improved Compile Progress dialog responsiveness and update interval
  - Added: GetUnitOf is replaced by a function with a much much faster algorithm
  - Added: Optimized compiler string functions
  - Added: Delphi Linker initial write buffer size increased to 5 MB
  - Added: Disables FileSystem cache to prevent double caching (directory cache) (Delphi XE2 and newer)
  - Added: Disables GetUnitOf cache and replaces the function by a much much faster algorithm (Delphi XE2 and newer)
  - Added: Disables UnitFindByAlias cache as IDE Fix Pack already has a faster unitname hash for it (Delphi XE2 and newer)
  - Fixed: Some access violations due to the unitname hash
  - Fixed: Directory cache caused problems with the debugger not finding the source files

2011-08-28 (4.4):
  - Fixed: Directory cache problem with packages
  - Added: Replaced LocalAlloc/ReAlloc/Free calls from the compiler with FastMM functions (Delphi 2009 only)
  - Added: Compiler callback used inlined AnsiCompareFileName, that is now also replaced by a faster version
  - Added: SameFileName uses the faster AnsiCompareFileName version
  - Added: Directory cache for $(WINDIR)\Globalization file search on IDE start (Delphi 2009 only)
  - Improved: LogView update tweaks (Delphi 2009 only)

2011-08-20 (4.3):
  - Added: Compiler directory cache
  - Added: 64kB I/O buffer for map/drc file creation
  - Added: Performance boost for LogView
  - Added fix for: Welcome page problems with "Save Desktop" (Delphi 2009 only)
  - Improved: .NET XMLSerializer's CreateProcess call is now cached without the need to call CreateProcess.
  - Fixed: Cleaning up the compiler's units let some units in memory.

2011-06-20 (4.2):
  - Fixed: Background compiler couldn't find file with non-ascii chars in the path. (Delphi XE only)
  - Added: Prevent IDE deadlocks
  - Added: HelpInsight parsing is done in the background when moving the mouse over an identifier
  - Added: Much improved IDE startup performance
  - Added: Some old Debugger C-RTL functions are replaced by much faster versions
  - Removed: No search for non-existing $(WINDIR)\Globalization files on IDE start (caused problems) (Delphi 2009 only)

2011-04-22 (4.1):
  - Fixed: Closing editor tab could raise access violation
  - Fixed: XML Serializer cache wasn't compiled into IDE Fix Pack 2009 anymore (Delphi 2009 only)
  - Added: IDE Fix Pack is now loaded earlier
  - Added: msxmldom, XMLDoc and Variants optimization for the IDE's usage pattern
  - Added: IDE start disk I/O optimizations
  - Added: Masks.pas optimization (UpperCase => UpCase)
  - Added: No search for non-existing $(WINDIR)\Globalization files on IDE start (Delphi 2009 only)
  - Added: No write access to $(BDS)\bin\_refactoring.log for Together.Refactoring (Delphi 2009 only)
  - Removed: QC #50564: Long/Unicode string assignments could be made 20% to 30% faster (caused problems with the options dialog)

2011-04-18 (4.0):
  - Added: Extensive Code Completion/Code Insight optimizations
  - Added: Compiler file search cache
  - Added: fix for: Compilation takes longer and longer when compiling multiple times
  - Added: TXmlIniFile optimization for the IDE's usage pattern / dproj files are now better formated.
  - Added: Structure View updates in the main thread are now faster
  - Added: Another Error Insight "Cannot resolve unit" fix
  - Added: Reduces the time that the background compiler needs to compile (Delphi XE only)
  - Added fix for: QC #88038: Delphi always maximizes itself on taskbar change
  - Added fix for: QC #89148: TListView ItemData streaming error
  - Added fix for: QC #89149: Saving a form with a ListView can kill the IDE
  - Added fix for: QC #93152: TBitBtn.DoubleBuffered default doesn't match constructor's default
  - Fixed: The Editor Focus fix caused the IDE's menu bar to shrink if "Minimize on start" is set
  - Fixed: Debugger sometimes stopped working (Start, Pause, Stop buttons are disabled)

2010-09-12: (3.5):
  - Added: Support for Delphi XE (removed unnecessary patches)
  - Added fix for: QC #86428: Form looses ancestor components after unloading packages
  - Added fix for: QC #23580: Object Inspector shows incomplete / mangled event handler list
  - Added fix for: Code Insight doesn''t show matching methods for event assignments
  - Fixed: "Local variable view" optimization caused debugger to crash after execution.
  - Optimization: QC #50564: Long/Unicode string assignments could be made 20% to 30% faster
  - Optimization: Optimized TObject.ClassNameIs implementation
  - Optimization: GetDynaMethod uses binary search in DMT
  - Optimization: LoadResString cache (improves the speed of switching between Code and Design editor)
  - Optimization: Call stack with IInterface parameters are resolved much faster

2010-03-15: (3.0/3.01)
  - Added fix for: QC #80822: ObjectInspecor: Properties are duplicated after scrolling
  - Added fix for: QC #80776: ObjectInspector shows "EditControl" instead of the real content
  - Added fix for: QC #79776: Clicking on object Inspector rejects focus
  - Added fix for: QC #29732: Class Completion adds published section
  - Added fix for: Step-Out doesn't recognize the return address at ESP. (Delphi 2010 only)
  - Removed broken fix for QC #47242: Possible AV when shutting down the IDE

2009-12-22 (2.9):
  - Bugfix: CallStack optimization could cause access violations in RAD Studio 2010
  - Added fix for: QC #75738: Debugging extremly slow
  - Added fix for: QC #68493: Switching away and back to Delphi orphans focus on Code Editor

2009-12-05 (2.8):
  - Added fix for: Vista compatible main icon resource doesn't work
  - Added fix for: F1 key doesn't invoke help from ObjectInspector (Delphi 2010 only)
  - Added Debugger optimization (Callstack, Local variable view, StepInto)
  - Added Startup .NET XML-Serializer cache

2009-09-28 (2.71):
  - Fixed: Compatiblity issue. IDEFixPack caused an IDE localization bug if OraDeveloperTools was loaded.

2009-09-03 (2.7):
  - Added: Support for Delphi 2010 (removed unnecessary patches)
  - Added fix for: 64 bit Debugger assertion
  - Added fix for: Undo destroyed editor buffer
  - Added fix for: Vista 64 IDE startup delay
  - Added: QC #74646: Buffer overflow in TCustomClientDataSet.DataConvert with ftWideString
  - Fixed: TTabSheet looked strange if used with SilverThemes

2009-05-30 (2.6):
  - Added: RAD Studio 2009 Update 3 (removed fixed patches, update patches to new machine code)
  - Fixed: QC #47242: Possible AV when shutting down the IDE
  - Fixed: QC #71575: Delphi 2009 literal string assigment
  - Fixed: ToolsAPI IOTAProjectOptions.GetOptionNames destroys options.

2009-03-03 (2.5):
  - Fixed: The AppDeActivateZOrder patch now fixes the cause instead of the symptoms
  - Added fix for: Error Insight fails to find TObject class
  - Added fix for: Possible deadlock when Error Insight calls ProcessMessages

2009-02-18 (2.4):
  - Added fix for: Error Insight defines VER190 instead of VER200
  - Added fix for: IDE may select the wrong file when performing a ctrl+click on a filename
    in the editor
  - Added faster AnsiCompareFileName replacement function which speeds up the "Install Packages..." dialog

2009-02-05 (2.3):
  - Added: Fix for "Cannot resolve unit name" Error Insight bug.

2009-01-25 (2.2):
  - Fixed: C++Builder compilation slow down caused by the ReadWrite mode fix
  - Fixed: DBGrid ScrollBar gab wasn't painted correctly in BiDiMode <> bdLeftToRight
  - Fixed: TTabSheet could throw an access violation if no PageControl was assigned to it
  - Added: RtlVclOptimize unit is compiled into the IDE Fix Pack, speeding up the IDE
      * WStrCmp, StrScan(PAnsiChar), StrScan(PWideChar), WideSameStr, InOpSet, TList,
        TObjectList, TComponent.Notification, TComponent.Destroying, IsLeapYear,
        Dynamic method call (GetDynaMethod), Set-Equality (SetEq),
        LoadResourceModule, Resource string cache (LoadResString).

2008-11-30 (2.1):
  - Added fix for Show Component Caption IDE bug
  - Added fix for Debugger invokes anonymous method by itself
  - Added fix for IDE Compiler opens all files in ReadWrite mode and blocks command line compiler
  - Added fix for IDE dead lock when updating the editors

2008-11-19 (2.0):
  - Added: Fix for QC #68647: Infinite loop in Forms.GetNonToolWindowPopupParent (2006-2009)
  - Added: Fix for QC #68740: Lost focus after TOpenDialog when MainFormOnTaskBar is set (2007-2009)
  - Added: Fix for QC #59963: Closing non-modal forms after a task switch can deactivate the application (2007-2009)
  - Added: Fix for QC #66892: Closing forms deactivates the application (missing “stdcall”) (2009)
  - Added: Fix for Classes.MakeObjectInstance memory leak fix (for usage in a DLL) (6-2009)
  - Added: Fix for QC #64484: SysUtils.Abort can raise an AccessViolation (6-2009)
  - Added: Fix for QC #35001: MDIChild’s active control focus is not set correctly (6-2009)
  - Added: Fix for QC #56252: TPageControl flickers a lot with active theming (7-2009)
  - Added: Fix for QC #68730: TLabel is not painted on a themed, double-buffered TTabSheet in Vista (7-2009)
  - Added: Fix for TLabels on TTabSheet are not painted (themes) if a TWinControl like TMemo is on the TTabSheet (TWinControl.PaintWindow bug) (7-2009)
  - Added: Fix for Grid flickers with active theming (DBGrid, StringGrid and DrawGrid only, no derived classes) (7-2009)
  - Added: Fix for QC #69112: TSpeedButton is painted as a black rectangle on a double buffered panel on a sheet of glass. (2009)
  - Added: Fix for Workaround for Windows Vista CompareString bug (Workaround is disabled by default, define “VistaCompareStringFix” to activate it) (Vista)

2008-11-07 (1.0):
  - Initial release
