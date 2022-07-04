--------------------------------------------------------------------
	GT Component Pack Version 1.0.70 Component Count : 70 2009-09-18
+ New Components:
        TgtFormFader :
                is a non visual component that can apply a fade in and/or a fade out effect
                on any form.
        TgtDBGridColumnManager :
                is a non visual component that can be attached into a dbgrid and manage at runtime all properties
                of any column of the associated grid.
                It can also save/load automatically the defined columns code free.
        TgtDBLookUpEdit :
                is as the name implies a db-aware lookupedit control.
                It is written all from scratch.
                It can display 2 fields in separate editors.
                It also has a dropdown list with values of the fields from
                the dataset assosiated to the ListSource property.

+ BugFixes:
+ Enchancements:
--------------------------------------------------------------------
--------------------------------------------------------------------
	GT Component Pack Version 1.0.65 Component Count : 67 2009-08-27
+ New Components:
        TgtSplashScreen :
                is a non visual component that can display a splash screen at the begging
                of the application.
                You can define the display duration of the splash.
                You can define if the screen will be automatically closed after the duration has passed
                or it will be closed manually by code.
                You can determine the position and size of the splash screen.
                You can load a picture to be displayed.
        TgtYouTubeVideoPlayer :
                is a visual component that will display a video from youtube(currently is supported but it will propably work with other sites)
                using the embedded code that is provided from youtube and other youtube like sites.

+ BugFixes:
+ Enchancements:
--------------------------------------------------------------------

--------------------------------------------------------------------
	GT Component Pack Version 1.0.60 Component Count : 65 2009-08-20
+ New Components:
+ BugFixes:
        Fixed color and width in IPEdit.
        Fixed IP validation in IPEdit now it also validates IP given
        not only when enter key is pressed but also when the last editor
        fires up the OnExit event.
+ Enchancements:
--------------------------------------------------------------------

--------------------------------------------------------------------
	GT Component Pack Version 1.0.55 Component Count : 65 2009-08-12
+ New Components:
        TgtDBGridExporter
                A component that "hooks" to a TDBGrid or a descentant of it and can export the data of the grid into a file.
                It has a feature to add a popupmenu to the grid if the grid has none and fully customizable menu captions.
                If the grid has already a popupmenu then it adds the needed menus to the bottom of the popupmenu.

        TgtDBEdit
        TgtDBMemo
        TgtDBImage
        TgtDBListBox
        TgtDBComboBox
        TgtDBLookUpListBox
        TgtDBLookUpComboBox
        TgtDBRichEdit
                All the above components are descentants of the standard Delphi DB Aware controls, the difference is that all have
                a bound label that is created when the component is created(like LabeledEdit).
                The bound label can be positioned in the following positions :
                        blpLeft
                        blpRight
                        blpTopLeft
                        blpTopRight
                        blpBottomLeft
                        blpBottomRight

        TgtDBProgressBar
        TgtDBTrackBar
        TgtDBUpDown
        TgtDBShape
        TgtDBGauge
                All the above controls are DB Aware versions of the controls they descent from.
                ProgressBar,TrackBar and Gauge can be used to show in a graphical way the value of an integer field.
                The shape descentant changes the brush color depending on the value of the field.It is actually a color display field.
        TgtDBDateTimePicker
                Is another DBDateTimePicker but it is all written from scratch.
+ BugFixes:

+ Enchancements:
--------------------------------------------------------------------

--------------------------------------------------------------------
	GT Component Pack Version 1.0.50 Component Count : 50 2009-07-30
+ New Components:
	TgtMenuStyler a component that can be attached to one menu or hook all available menus
	and override the menus default draw procedures to mimic the menus seen in MS Office and other 
	applications.ColorScheme is fully customizable.
	It works both with TMainMenu and TPopUpMenu as well.
+ BugFixes:
	The release of the packs where wrong.
+ Enchancements:
--------------------------------------------------------------------

--------------------------------------------------------------------
	GT Component Pack Version 1.0.45 Component Count : 49 2009-07-11
+ New Components:
+ BugFixes:
+ Enchancements:
	TgtInfoTrayDialog did not display properly on Windows Vista.
	TgtFileDownLoad  some string conversions issues where solved.
--------------------------------------------------------------------
--------------------------------------------------------------------
	GT Component Pack Version 1.0.40 Component Count : 49 2009-07-08
+ New Components :
+ BugFixes :
	TgtStatusBar did destroy all panel related controls.
	TgtCustomDialogButton ActionResult was not created on all occasions
	and an AV was raised.
	TgtStringStore Count property was missing.
+ Enchancements  :
	TgtGroupBox
	 OnCheckBoxClick event added
	TgtIPEdit 
	 IP edits accepts now only numeric values.
	 IP edits now cannot accept more than 3 characters
--------------------------------------------------------------------
--------------------------------------------------------------------
	GT Component Pack Version 1.0.35 Component Count : 49 2009-07-06
+ New Components :
	TgtInfoTrayDialog
		A form dialog component which has the same visual result as
		a Windows Messenger dialog.
--------------------------------------------------------------------
--------------------------------------------------------------------
	GT Component Pack Version 1.0.30 Component Count : 48 2009-06-12
--------------------------------------------------------------------
+ New Components :
	TgtPrinterCombo     (ComboBox  that lists the available printers)
	TgtFontCombo        (ComboBox  that lists the available fonts)
	TgtSQLServerCombo   (ComboBox  that lists the available SQL Servers)
	TgtNetResourceCombo (ComboBox  that lists the available NetWork Resources)
	TgtFileDownload     (Component that can download files from the Internet)
+ BugFixes :
	Minor BugFixes at o_GTButtons.
+ Enchancements :
	TgtFormEvents now works even the related form is the main form.
--------------------------------------------------------------------
		GT Component Pack Version 1.0.25 Component Count : 43
--------------------------------------------------------------------
+ New Components :
      TgtDialogButton
      TgtControlPanelDialog

+ BugFixes at :
      DateControlsManager when date was set it throwed a conversion exception.
      FileInfo when file version as numeric was requested it throwed a conversion error.
--------------------------------------------------------------------
		GT Component Pack Version 1.0.20 Component Count : 41
--------------------------------------------------------------------
 + New Components :
      TgtStatusBar
      TgtIPEdit
      TgtDayCombo
      TgtMonthCombo
      TgtYearCombo
      TgtDateCtrlManager
--------------------------------------------------------------------
		GT Component Pack Version 1.0.15
--------------------------------------------------------------------
 + Major bug fix in TgtSettingsManager in Setting Controls Suite.
   It crashed the IDE of Delphi 2009 due to a memory leak from the component.

--------------------------------------------------------------------
		GT Component Pack Version 1.0.10 Component Count : 35
--------------------------------------------------------------------
 + New components :
      TgtFileInfoListView
      TgtPackageManager
--------------------------------------------------------------------
		GT Component Pack Version 1.0.0
--------------------------------------------------------------------
 + Initial Version consisting of 33 components which are :
      TgtLoginDialog
      TgtSettingsManager
      TgtSettingsManagerStorageOptions
      TgtCustomSettingControl
      TgtCSCCheckBox
      TgtCSCColorBox
      TgtCSCComboBox
      TgtCSCDateTimePicker
      TgtCSCEdit
      TgtCSCLabeledEdit
      TgtCSCListBox
      TgtCSCMemo
      TgtCSCRadioGroup
      TgtCSCRadioButton
      TgtCSCTrackBar
      TgtMenuTreeView
      TgtBenchMark
      TgtRegisteredClasses
      TgtFileInfo
      TgtFormEvents
      TgtExplorerSysObjs
      TgtShellTreeView
      TgtShellComboBox
      TgtShellListView
      TgtListBox
      TgtMessageBox
      TgtTimer
      TgtWindowsCPL
      TgtLinkLabel
      TgtProcessListView
      TgtProcessManager
      TgtRegionalSettings
      TgtStringStore