// ------------------------------------------------------------------------------
// DPF.Android.R Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.Android.R;

interface

{$I DPF.ANDROID.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  System.TypInfo,
{$IFDEF ANDROID}
  Androidapi.JNI.App,
  Androidapi.JNI.Widget,
  Androidapi.JNI.JavaTypes,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.Log,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$ELSE}
  DPF.ANDROID.DesignTime,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types;

{$IFDEF ANDROID}

type

  JR_layout   = interface; // android.R.layout
  JR_id       = interface; // android.R.id
  JR_drawable = interface; // android.R.drawable

  // ----------------------------------------------------------------------------
  // JR_layout Class
  // ----------------------------------------------------------------------------
  JR_layoutClass = interface( JObjectClass )
    ['{39E4043E-BA6C-4AA3-8E32-8D06E3A8C3F3}']

    function init: JR_layout; cdecl;

    // --------------------------------------------------------------------------
    function _Getactivity_list_item: Integer;
    function _Getbrowser_link_context_header: Integer;
    function _Getexpandable_list_content: Integer;
    function _Getlist_content: Integer;
    function _Getpreference_category: Integer;
    function _Getselect_dialog_item: Integer;
    function _Getselect_dialog_multichoice: Integer;
    function _Getselect_dialog_singlechoice: Integer;
    function _Getsimple_dropdown_item_1line: Integer;
    function _Getsimple_expandable_list_item_1: Integer;
    function _Getsimple_expandable_list_item_2: Integer;
    function _Getsimple_gallery_item: Integer;
    function _Getsimple_list_item_1: Integer;
    function _Getsimple_list_item_2: Integer;
    function _Getsimple_list_item_activated_1: Integer;
    function _Getsimple_list_item_activated_2: Integer;
    function _Getsimple_list_item_checked: Integer;
    function _Getsimple_list_item_multiple_choice: Integer;
    function _Getsimple_list_item_single_choice: Integer;
    function _Getsimple_selectable_list_item: Integer;
    function _Getsimple_spinner_dropdown_item: Integer;
    function _Getsimple_spinner_item: Integer;
    function _Gettest_list_item: Integer;
    function _Gettwo_line_list_item: Integer;

    // --------------------------------------------------------------------------

    function my_spinner: Integer;
//    function _Getmy_spinner: Integer;
//    property my_spinner: Integer read _Getmy_spinner;
    // --------------------------------------------------------------------------

    property activity_list_item: Integer read _Getactivity_list_item;
    property browser_link_context_header: Integer read _Getbrowser_link_context_header;
    property expandable_list_content: Integer read _Getexpandable_list_content;
    property list_content: Integer read _Getlist_content;
    property preference_category: Integer read _Getpreference_category;
    property select_dialog_item: Integer read _Getselect_dialog_item;
    property select_dialog_multichoice: Integer read _Getselect_dialog_multichoice;
    property select_dialog_singlechoice: Integer read _Getselect_dialog_singlechoice;
    property simple_dropdown_item_1line: Integer read _Getsimple_dropdown_item_1line;
    property simple_expandable_list_item_1: Integer read _Getsimple_expandable_list_item_1;
    property simple_expandable_list_item_2: Integer read _Getsimple_expandable_list_item_2;
    property simple_gallery_item: Integer read _Getsimple_gallery_item;
    property simple_list_item_1: Integer read _Getsimple_list_item_1;
    property simple_list_item_2: Integer read _Getsimple_list_item_2;
    property simple_list_item_activated_1: Integer read _Getsimple_list_item_activated_1;
    property simple_list_item_activated_2: Integer read _Getsimple_list_item_activated_2;
    property simple_list_item_checked: Integer read _Getsimple_list_item_checked;
    property simple_list_item_multiple_choice: Integer read _Getsimple_list_item_multiple_choice;
    property simple_list_item_single_choice: Integer read _Getsimple_list_item_single_choice;
    property simple_selectable_list_item: Integer read _Getsimple_selectable_list_item;
    property simple_spinner_dropdown_item: Integer read _Getsimple_spinner_dropdown_item;
    property simple_spinner_item: Integer read _Getsimple_spinner_item;
    property test_list_item: Integer read _Gettest_list_item;
    property two_line_list_item: Integer read _Gettwo_line_list_item;
  end;

  [JavaSignature( 'android/R$layout' )]
  JR_layout = interface( JObject )
    ['{0C93153B-83D7-4BC2-A527-FF056AA6D96E}']
  end;

  TJR_layout = class( TJavaGenericImport<JR_layoutClass, JR_layout> )
  end;

  // ----------------------------------------------------------------------------
  // Jid Class
  // ----------------------------------------------------------------------------
  JR_idClass = interface( JObjectClass )
    ['{378B59DA-8844-4738-B1FA-5905B50C5F41}']

    function init: JR_id; cdecl;
    // ------------------------------------------------

    function _GetaddToDictionary: integer;
    function _Getbackground: integer;
    function _Getbutton1: integer;
    function _Getbutton2: integer;
    function _Getbutton3: integer;
    function _GetcandidatesArea: integer;
    function _Getcheckbox: integer;
    function _GetcloseButton: integer;
    function _Getcontent: integer;
    function _Getcopy: integer;
    function _GetcopyUrl: integer;
    function _Getcustom: integer;
    function _Getcut: integer;
    function _Getedit: integer;
    function _Getempty: integer;
    function _GetextractArea: integer;
    function _Gethint: integer;
    function _Gethome: integer;
    function _Geticon: integer;
    function _Geticon1: integer;
    function _Geticon2: integer;
    function _Getinput: integer;
    function _GetinputArea: integer;
    function _GetinputExtractEditText: integer;
    function _GetkeyboardView: integer;
    function _Getlist: integer;
    function _Getmessage: integer;
    function _Getpaste: integer;
    function _Getprimary: integer;
    function _Getprogress: integer;
    function _GetsecondaryProgress: integer;
    function _GetselectAll: integer;
    function _GetselectTextMode: integer;
    function _GetselectedIcon: integer;
    function _GetstartSelectingText: integer;
    function _GetstopSelectingText: integer;
    function _Getsummary: integer;
    function _GetswitchInputMethod: integer;
    function _Gettabcontent: integer;
    function _Gettabhost: integer;
    function _Gettabs: integer;
    function _Gettext1: integer;
    function _Gettext2: integer;
    function _Gettitle: integer;
    function _Gettoggle: integer;
    function _Getwidget_frame: integer;

    // ------------------------------------------------
    function inclusionlayout: Integer ; cdecl ;
    // ------------------------------------------------

    property addToDictionary: Integer read _GetaddToDictionary;
    property background: Integer read _Getbackground;
    property button1: Integer read _Getbutton1;
    property button2: Integer read _Getbutton2;
    property button3: Integer read _Getbutton3;
    property candidatesArea: Integer read _GetcandidatesArea;
    property checkbox: Integer read _Getcheckbox;
    property closeButton: Integer read _GetcloseButton;
    property content: Integer read _Getcontent;
    property copy: Integer read _Getcopy;
    property copyUrl: Integer read _GetcopyUrl;
    property custom: Integer read _Getcustom;
    property cut: Integer read _Getcut;
    property edit: Integer read _Getedit;
    property empty: Integer read _Getempty;
    property extractArea: Integer read _GetextractArea;
    property hint: Integer read _Gethint;
    property home: Integer read _Gethome;
    property icon: Integer read _Geticon;
    property icon1: Integer read _Geticon1;
    property icon2: Integer read _Geticon2;
    property input: Integer read _Getinput;
    property inputArea: Integer read _GetinputArea;
    property inputExtractEditText: Integer read _GetinputExtractEditText;
    property keyboardView: Integer read _GetkeyboardView;
    property list: Integer read _Getlist;
    property &message: Integer read _Getmessage;
    property paste: Integer read _Getpaste;
    property primary: Integer read _Getprimary;
    property progress: Integer read _Getprogress;
    property secondaryProgress: Integer read _GetsecondaryProgress;
    property selectAll: Integer read _GetselectAll;
    property selectTextMode: Integer read _GetselectTextMode;
    property selectedIcon: Integer read _GetselectedIcon;
    property startSelectingText: Integer read _GetstartSelectingText;
    property stopSelectingText: Integer read _GetstopSelectingText;
    property summary: Integer read _Getsummary;
    property switchInputMethod: Integer read _GetswitchInputMethod;
    property tabcontent: Integer read _Gettabcontent;
    property tabhost: Integer read _Gettabhost;
    property tabs: Integer read _Gettabs;
    property text1: Integer read _Gettext1;
    property text2: Integer read _Gettext2;
    property title: Integer read _Gettitle;
    property toggle: Integer read _Gettoggle;
    property widget_frame: Integer read _Getwidget_frame;
  end;

  [JavaSignature( 'android/R$id' )]
  JR_id = interface( JObject )
    ['{B6805E34-2566-4D64-B334-1687E8BB1503}']
  end;

  TJR_id = class( TJavaGenericImport<JR_idClass, JR_id> )
  end;

  // ----------------------------------------------------------------------------
  // JR_drawable Class
  // ----------------------------------------------------------------------------
  JR_drawableClass = interface( JObjectClass )
    ['{2A242688-8233-43F8-A0F7-A4E8CC96CCA7}']

    function init: JR_drawable; cdecl;
    // ------------------------------------------------

    function _Getalert_dark_frame: Integer;
    function _Getalert_light_frame: Integer;
    function _Getarrow_down_float: Integer;
    function _Getarrow_up_float: Integer;
    function _Getbottom_bar: Integer;
    function _Getbtn_default: Integer;
    function _Getbtn_default_small: Integer;
    function _Getbtn_dialog: Integer;
    function _Getbtn_dropdown: Integer;
    function _Getbtn_minus: Integer;
    function _Getbtn_plus: Integer;
    function _Getbtn_radio: Integer;
    function _Getbtn_star: Integer;
    function _Getbtn_star_big_off: Integer;
    function _Getbtn_star_big_on: Integer;
    function _Getbutton_onoff_indicator_off: Integer;
    function _Getbutton_onoff_indicator_on: Integer;
    function _Getcheckbox_off_background: Integer;
    function _Getcheckbox_on_background: Integer;
    function _Getdark_header: Integer;
    function _Getdialog_frame: Integer;
    function _Getdialog_holo_dark_frame: Integer;
    function _Getdialog_holo_light_frame: Integer;
    function _Getdivider_horizontal_bright: Integer;
    function _Getdivider_horizontal_dark: Integer;
    function _Getdivider_horizontal_dim_dark: Integer;
    function _Getdivider_horizontal_textfield: Integer;
    function _Getedit_text: Integer;
    function _Geteditbox_background: Integer;
    function _Geteditbox_background_normal: Integer;
    function _Geteditbox_dropdown_dark_frame: Integer;
    function _Geteditbox_dropdown_light_frame: Integer;
    function _Getgallery_thumb: Integer;
    function _Getic_btn_speak_now: Integer;
    function _Getic_delete: Integer;
    function _Getic_dialog_alert: Integer;
    function _Getic_dialog_dialer: Integer;
    function _Getic_dialog_email: Integer;
    function _Getic_dialog_info: Integer;
    function _Getic_dialog_map: Integer;
    function _Getic_input_add: Integer;
    function _Getic_input_delete: Integer;
    function _Getic_input_get: Integer;
    function _Getic_lock_idle_alarm: Integer;
    function _Getic_lock_idle_charging: Integer;
    function _Getic_lock_idle_lock: Integer;
    function _Getic_lock_idle_low_battery: Integer;
    function _Getic_lock_lock: Integer;
    function _Getic_lock_power_off: Integer;
    function _Getic_lock_silent_mode: Integer;
    function _Getic_lock_silent_mode_off: Integer;
    function _Getic_media_ff: Integer;
    function _Getic_media_next: Integer;
    function _Getic_media_pause: Integer;
    function _Getic_media_play: Integer;
    function _Getic_media_previous: Integer;
    function _Getic_media_rew: Integer;
    function _Getic_menu_add: Integer;
    function _Getic_menu_agenda: Integer;
    function _Getic_menu_always_landscape_portrait: Integer;
    function _Getic_menu_call: Integer;
    function _Getic_menu_camera: Integer;
    function _Getic_menu_close_clear_cancel: Integer;
    function _Getic_menu_compass: Integer;
    function _Getic_menu_crop: Integer;
    function _Getic_menu_day: Integer;
    function _Getic_menu_delete: Integer;
    function _Getic_menu_directions: Integer;
    function _Getic_menu_edit: Integer;
    function _Getic_menu_gallery: Integer;
    function _Getic_menu_help: Integer;
    function _Getic_menu_info_details: Integer;
    function _Getic_menu_manage: Integer;
    function _Getic_menu_mapmode: Integer;
    function _Getic_menu_month: Integer;
    function _Getic_menu_more: Integer;
    function _Getic_menu_my_calendar: Integer;
    function _Getic_menu_mylocation: Integer;
    function _Getic_menu_myplaces: Integer;
    function _Getic_menu_preferences: Integer;
    function _Getic_menu_recent_history: Integer;
    function _Getic_menu_report_image: Integer;
    function _Getic_menu_revert: Integer;
    function _Getic_menu_rotate: Integer;
    function _Getic_menu_save: Integer;
    function _Getic_menu_search: Integer;
    function _Getic_menu_send: Integer;
    function _Getic_menu_set_as: Integer;
    function _Getic_menu_share: Integer;
    function _Getic_menu_slideshow: Integer;
    function _Getic_menu_sort_alphabetically: Integer;
    function _Getic_menu_sort_by_size: Integer;
    function _Getic_menu_today: Integer;
    function _Getic_menu_upload: Integer;
    function _Getic_menu_upload_you_tube: Integer;
    function _Getic_menu_view: Integer;
    function _Getic_menu_week: Integer;
    function _Getic_menu_zoom: Integer;
    function _Getic_notification_clear_all: Integer;
    function _Getic_notification_overlay: Integer;
    function _Getic_partial_secure: Integer;
    function _Getic_popup_disk_full: Integer;
    function _Getic_popup_reminder: Integer;
    function _Getic_popup_sync: Integer;
    function _Getic_search_category_default: Integer;
    function _Getic_secure: Integer;
    function _Getlist_selector_background: Integer;
    function _Getmenu_frame: Integer;
    function _Getmenu_full_frame: Integer;
    function _Getmenuitem_background: Integer;
    function _Getpicture_frame: Integer;
    function _Getpresence_audio_away: Integer;
    function _Getpresence_audio_busy: Integer;
    function _Getpresence_audio_online: Integer;
    function _Getpresence_away: Integer;
    function _Getpresence_busy: Integer;
    function _Getpresence_invisible: Integer;
    function _Getpresence_offline: Integer;
    function _Getpresence_online: Integer;
    function _Getpresence_video_away: Integer;
    function _Getpresence_video_busy: Integer;
    function _Getpresence_video_online: Integer;
    function _Getprogress_horizontal: Integer;
    function _Getprogress_indeterminate_horizontal: Integer;
    function _Getradiobutton_off_background: Integer;
    function _Getradiobutton_on_background: Integer;
    function _Getscreen_background_dark: Integer;
    function _Getscreen_background_dark_transparent: Integer;
    function _Getscreen_background_light: Integer;
    function _Getscreen_background_light_transparent: Integer;
    function _Getspinner_background: Integer;
    function _Getspinner_dropdown_background: Integer;
    function _Getstar_big_off: Integer;
    function _Getstar_big_on: Integer;
    function _Getstar_off: Integer;
    function _Getstar_on: Integer;
    function _Getstat_notify_call_mute: Integer;
    function _Getstat_notify_chat: Integer;
    function _Getstat_notify_error: Integer;
    function _Getstat_notify_missed_call: Integer;
    function _Getstat_notify_more: Integer;
    function _Getstat_notify_sdcard: Integer;
    function _Getstat_notify_sdcard_prepare: Integer;
    function _Getstat_notify_sdcard_usb: Integer;
    function _Getstat_notify_sync: Integer;
    function _Getstat_notify_sync_noanim: Integer;
    function _Getstat_notify_voicemail: Integer;
    function _Getstat_sys_data_bluetooth: Integer;
    function _Getstat_sys_download: Integer;
    function _Getstat_sys_download_done: Integer;
    function _Getstat_sys_headset: Integer;
    function _Getstat_sys_phone_call: Integer;
    function _Getstat_sys_phone_call_forward: Integer;
    function _Getstat_sys_phone_call_on_hold: Integer;
    function _Getstat_sys_speakerphone: Integer;
    function _Getstat_sys_upload: Integer;
    function _Getstat_sys_upload_done: Integer;
    function _Getstat_sys_vp_phone_call: Integer;
    function _Getstat_sys_vp_phone_call_on_hold: Integer;
    function _Getstat_sys_warning: Integer;
    function _Getstatus_bar_item_app_background: Integer;
    function _Getstatus_bar_item_background: Integer;
    function _Getsym_action_call: Integer;
    function _Getsym_action_chat: Integer;
    function _Getsym_action_email: Integer;
    function _Getsym_call_incoming: Integer;
    function _Getsym_call_missed: Integer;
    function _Getsym_call_outgoing: Integer;
    function _Getsym_contact_card: Integer;
    function _Getsym_def_app_icon: Integer;
    function _Gettitle_bar: Integer;
    function _Gettitle_bar_tall: Integer;
    function _Gettoast_frame: Integer;
    function _Getzoom_plate: Integer;

    // ------------------------------------------------

    property alert_dark_frame: integer read _Getalert_dark_frame;
    property alert_light_frame: integer read _Getalert_light_frame;
    property arrow_down_float: integer read _Getarrow_down_float;
    property arrow_up_float: integer read _Getarrow_up_float;
    property bottom_bar: integer read _Getbottom_bar;
    property btn_default: integer read _Getbtn_default;
    property btn_default_small: integer read _Getbtn_default_small;
    property btn_dialog: integer read _Getbtn_dialog;
    property btn_dropdown: integer read _Getbtn_dropdown;
    property btn_minus: integer read _Getbtn_minus;
    property btn_plus: integer read _Getbtn_plus;
    property btn_radio: integer read _Getbtn_radio;
    property btn_star: integer read _Getbtn_star;
    property btn_star_big_off: integer read _Getbtn_star_big_off;
    property btn_star_big_on: integer read _Getbtn_star_big_on;
    property button_onoff_indicator_off: integer read _Getbutton_onoff_indicator_off;
    property button_onoff_indicator_on: integer read _Getbutton_onoff_indicator_on;
    property checkbox_off_background: integer read _Getcheckbox_off_background;
    property checkbox_on_background: integer read _Getcheckbox_on_background;
    property dark_header: integer read _Getdark_header;
    property dialog_frame: integer read _Getdialog_frame;
    property dialog_holo_dark_frame: integer read _Getdialog_holo_dark_frame;
    property dialog_holo_light_frame: integer read _Getdialog_holo_light_frame;
    property divider_horizontal_bright: integer read _Getdivider_horizontal_bright;
    property divider_horizontal_dark: integer read _Getdivider_horizontal_dark;
    property divider_horizontal_dim_dark: integer read _Getdivider_horizontal_dim_dark;
    property divider_horizontal_textfield: integer read _Getdivider_horizontal_textfield;
    property edit_text: integer read _Getedit_text;
    property editbox_background: integer read _Geteditbox_background;
    property editbox_background_normal: integer read _Geteditbox_background_normal;
    property editbox_dropdown_dark_frame: integer read _Geteditbox_dropdown_dark_frame;
    property editbox_dropdown_light_frame: integer read _Geteditbox_dropdown_light_frame;
    property gallery_thumb: integer read _Getgallery_thumb;
    property ic_btn_speak_now: integer read _Getic_btn_speak_now;
    property ic_delete: integer read _Getic_delete;
    property ic_dialog_alert: integer read _Getic_dialog_alert;
    property ic_dialog_dialer: integer read _Getic_dialog_dialer;
    property ic_dialog_email: integer read _Getic_dialog_email;
    property ic_dialog_info: integer read _Getic_dialog_info;
    property ic_dialog_map: integer read _Getic_dialog_map;
    property ic_input_add: integer read _Getic_input_add;
    property ic_input_delete: integer read _Getic_input_delete;
    property ic_input_get: integer read _Getic_input_get;
    property ic_lock_idle_alarm: integer read _Getic_lock_idle_alarm;
    property ic_lock_idle_charging: integer read _Getic_lock_idle_charging;
    property ic_lock_idle_lock: integer read _Getic_lock_idle_lock;
    property ic_lock_idle_low_battery: integer read _Getic_lock_idle_low_battery;
    property ic_lock_lock: integer read _Getic_lock_lock;
    property ic_lock_power_off: integer read _Getic_lock_power_off;
    property ic_lock_silent_mode: integer read _Getic_lock_silent_mode;
    property ic_lock_silent_mode_off: integer read _Getic_lock_silent_mode_off;
    property ic_media_ff: integer read _Getic_media_ff;
    property ic_media_next: integer read _Getic_media_next;
    property ic_media_pause: integer read _Getic_media_pause;
    property ic_media_play: integer read _Getic_media_play;
    property ic_media_previous: integer read _Getic_media_previous;
    property ic_media_rew: integer read _Getic_media_rew;
    property ic_menu_add: integer read _Getic_menu_add;
    property ic_menu_agenda: integer read _Getic_menu_agenda;
    property ic_menu_always_landscape_portrait: integer read _Getic_menu_always_landscape_portrait;
    property ic_menu_call: integer read _Getic_menu_call;
    property ic_menu_camera: integer read _Getic_menu_camera;
    property ic_menu_close_clear_cancel: integer read _Getic_menu_close_clear_cancel;
    property ic_menu_compass: integer read _Getic_menu_compass;
    property ic_menu_crop: integer read _Getic_menu_crop;
    property ic_menu_day: integer read _Getic_menu_day;
    property ic_menu_delete: integer read _Getic_menu_delete;
    property ic_menu_directions: integer read _Getic_menu_directions;
    property ic_menu_edit: integer read _Getic_menu_edit;
    property ic_menu_gallery: integer read _Getic_menu_gallery;
    property ic_menu_help: integer read _Getic_menu_help;
    property ic_menu_info_details: integer read _Getic_menu_info_details;
    property ic_menu_manage: integer read _Getic_menu_manage;
    property ic_menu_mapmode: integer read _Getic_menu_mapmode;
    property ic_menu_month: integer read _Getic_menu_month;
    property ic_menu_more: integer read _Getic_menu_more;
    property ic_menu_my_calendar: integer read _Getic_menu_my_calendar;
    property ic_menu_mylocation: integer read _Getic_menu_mylocation;
    property ic_menu_myplaces: integer read _Getic_menu_myplaces;
    property ic_menu_preferences: integer read _Getic_menu_preferences;
    property ic_menu_recent_history: integer read _Getic_menu_recent_history;
    property ic_menu_report_image: integer read _Getic_menu_report_image;
    property ic_menu_revert: integer read _Getic_menu_revert;
    property ic_menu_rotate: integer read _Getic_menu_rotate;
    property ic_menu_save: integer read _Getic_menu_save;
    property ic_menu_search: integer read _Getic_menu_search;
    property ic_menu_send: integer read _Getic_menu_send;
    property ic_menu_set_as: integer read _Getic_menu_set_as;
    property ic_menu_share: integer read _Getic_menu_share;
    property ic_menu_slideshow: integer read _Getic_menu_slideshow;
    property ic_menu_sort_alphabetically: integer read _Getic_menu_sort_alphabetically;
    property ic_menu_sort_by_size: integer read _Getic_menu_sort_by_size;
    property ic_menu_today: integer read _Getic_menu_today;
    property ic_menu_upload: integer read _Getic_menu_upload;
    property ic_menu_upload_you_tube: integer read _Getic_menu_upload_you_tube;
    property ic_menu_view: integer read _Getic_menu_view;
    property ic_menu_week: integer read _Getic_menu_week;
    property ic_menu_zoom: integer read _Getic_menu_zoom;
    property ic_notification_clear_all: integer read _Getic_notification_clear_all;
    property ic_notification_overlay: integer read _Getic_notification_overlay;
    property ic_partial_secure: integer read _Getic_partial_secure;
    property ic_popup_disk_full: integer read _Getic_popup_disk_full;
    property ic_popup_reminder: integer read _Getic_popup_reminder;
    property ic_popup_sync: integer read _Getic_popup_sync;
    property ic_search_category_default: integer read _Getic_search_category_default;
    property ic_secure: integer read _Getic_secure;
    property list_selector_background: integer read _Getlist_selector_background;
    property menu_frame: integer read _Getmenu_frame;
    property menu_full_frame: integer read _Getmenu_full_frame;
    property menuitem_background: integer read _Getmenuitem_background;
    property picture_frame: integer read _Getpicture_frame;
    property presence_audio_away: integer read _Getpresence_audio_away;
    property presence_audio_busy: integer read _Getpresence_audio_busy;
    property presence_audio_online: integer read _Getpresence_audio_online;
    property presence_away: integer read _Getpresence_away;
    property presence_busy: integer read _Getpresence_busy;
    property presence_invisible: integer read _Getpresence_invisible;
    property presence_offline: integer read _Getpresence_offline;
    property presence_online: integer read _Getpresence_online;
    property presence_video_away: integer read _Getpresence_video_away;
    property presence_video_busy: integer read _Getpresence_video_busy;
    property presence_video_online: integer read _Getpresence_video_online;
    property progress_horizontal: integer read _Getprogress_horizontal;
    property progress_indeterminate_horizontal: integer read _Getprogress_indeterminate_horizontal;
    property radiobutton_off_background: integer read _Getradiobutton_off_background;
    property radiobutton_on_background: integer read _Getradiobutton_on_background;
    property screen_background_dark: integer read _Getscreen_background_dark;
    property screen_background_dark_transparent: integer read _Getscreen_background_dark_transparent;
    property screen_background_light: integer read _Getscreen_background_light;
    property screen_background_light_transparent: integer read _Getscreen_background_light_transparent;
    property spinner_background: integer read _Getspinner_background;
    property spinner_dropdown_background: integer read _Getspinner_dropdown_background;
    property star_big_off: integer read _Getstar_big_off;
    property star_big_on: integer read _Getstar_big_on;
    property star_off: integer read _Getstar_off;
    property star_on: integer read _Getstar_on;
    property stat_notify_call_mute: integer read _Getstat_notify_call_mute;
    property stat_notify_chat: integer read _Getstat_notify_chat;
    property stat_notify_error: integer read _Getstat_notify_error;
    property stat_notify_missed_call: integer read _Getstat_notify_missed_call;
    property stat_notify_more: integer read _Getstat_notify_more;
    property stat_notify_sdcard: integer read _Getstat_notify_sdcard;
    property stat_notify_sdcard_prepare: integer read _Getstat_notify_sdcard_prepare;
    property stat_notify_sdcard_usb: integer read _Getstat_notify_sdcard_usb;
    property stat_notify_sync: integer read _Getstat_notify_sync;
    property stat_notify_sync_noanim: integer read _Getstat_notify_sync_noanim;
    property stat_notify_voicemail: integer read _Getstat_notify_voicemail;
    property stat_sys_data_bluetooth: integer read _Getstat_sys_data_bluetooth;
    property stat_sys_download: integer read _Getstat_sys_download;
    property stat_sys_download_done: integer read _Getstat_sys_download_done;
    property stat_sys_headset: integer read _Getstat_sys_headset;
    property stat_sys_phone_call: integer read _Getstat_sys_phone_call;
    property stat_sys_phone_call_forward: integer read _Getstat_sys_phone_call_forward;
    property stat_sys_phone_call_on_hold: integer read _Getstat_sys_phone_call_on_hold;
    property stat_sys_speakerphone: integer read _Getstat_sys_speakerphone;
    property stat_sys_upload: integer read _Getstat_sys_upload;
    property stat_sys_upload_done: integer read _Getstat_sys_upload_done;
    property stat_sys_vp_phone_call: integer read _Getstat_sys_vp_phone_call;
    property stat_sys_vp_phone_call_on_hold: integer read _Getstat_sys_vp_phone_call_on_hold;
    property stat_sys_warning: integer read _Getstat_sys_warning;
    property status_bar_item_app_background: integer read _Getstatus_bar_item_app_background;
    property status_bar_item_background: integer read _Getstatus_bar_item_background;
    property sym_action_call: integer read _Getsym_action_call;
    property sym_action_chat: integer read _Getsym_action_chat;
    property sym_action_email: integer read _Getsym_action_email;
    property sym_call_incoming: integer read _Getsym_call_incoming;
    property sym_call_missed: integer read _Getsym_call_missed;
    property sym_call_outgoing: integer read _Getsym_call_outgoing;
    property sym_contact_card: integer read _Getsym_contact_card;
    property sym_def_app_icon: integer read _Getsym_def_app_icon;
    property title_bar: integer read _Gettitle_bar;
    property title_bar_tall: integer read _Gettitle_bar_tall;
    property toast_frame: integer read _Gettoast_frame;
    property zoom_plate: integer read _Getzoom_plate;
  end;

  [JavaSignature( 'android/R$drawable' )]
  JR_drawable = interface( JObject )
    ['{16AE4035-C298-4C70-A114-E6D205E1B3EF}']
  end;

  TJR_drawable = class( TJavaGenericImport<JR_drawableClass, JR_drawable> )
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
end.
