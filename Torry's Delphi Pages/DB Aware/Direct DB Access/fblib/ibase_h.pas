{********************************************************}
{ This unit originally part of the Zeos Database Objects }
{ It has been imported and renamed only to make it       }
{ consistant with the rest of the library.               }
{                                                        }
{    Modifications                                       }
{                  Alessandro Batisti                    }
{                  fblib@altervista.org                  }
{                  http://fblib.altervista.org           }
{    Original Authors                                    }
{    Copyright (c) 1999-2001 Sergey Seroukhov            }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}
{$I fbl.inc}
unit ibase_h;

interface

uses
  SysUtils;

type
  {$IFDEF UNIX}
  TLibHandle = Pointer;
  {$ELSE}
  {$IFDEF D6M}
  TLibHandle = cardinal;
  PWord = ^word;
  PPChar = ^PChar;
  {$ELSE}
  TLibHandle = HModule;
  {$ENDIF}
  {$ENDIF}


  {***************** Plain API Constants definition ****************}

const
  {$IFNDEF UNIX}
  DEFAULT_DLL_LOCATION = 'gds32.dll';
  DEFAULT_DLL_LOCATION2 = 'fbclient.dll';
  LibHandleNil = 0;
  {$ELSE}
  {$IFDEF LINUX}
  DEFAULT_DLL_LOCATION = 'libgds.so.0';
  DEFAULT_DLL_LOCATION2 = 'libfbclient.so';
  {$ENDIF}
  {$IFDEF BSD}
  DEFAULT_DLL_LOCATION = 'libgds.so.0';
  DEFAULT_DLL_LOCATION2 = 'libgds.so';
  {$ENDIF}
  LibHandleNil = nil;
  {$ENDIF}
  
  ISC_NULL = -1;
  ISC_NOTNULL = 0;
  ISC_TRUE = 1;
  ISC_FALSE = 0;
  DSQL_CLOSE = 1;
  DSQL_DROP = 2;

  SQLDA_VERSION1 = 1;
  SQLDA_VERSION2 = 2;
  SQL_DIALECT_V5 = 1;
  SQL_DIALECT_V6 = 2;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)

  { Actions to pass to the blob filter (ctl_source) }
  isc_blob_filter_open = 0;
  isc_blob_filter_get_segment = 1;
  isc_blob_filter_close = 2;
  isc_blob_filter_create = 3;
  isc_blob_filter_put_segment = 4;
  isc_blob_filter_alloc = 5;
  isc_blob_filter_free = 6;
  isc_blob_filter_seek = 7;

  { Database parameter block stuff }
  isc_dpb_version1 = 1;
  isc_dpb_cdd_pathname = 1;
  isc_dpb_allocation = 2;
  isc_dpb_journal = 3;
  isc_dpb_page_size = 4;
  isc_dpb_num_buffers = 5;
  isc_dpb_buffer_length = 6;
  isc_dpb_debug = 7;
  isc_dpb_garbage_collect = 8;
  isc_dpb_verify = 9;
  isc_dpb_sweep = 10;
  isc_dpb_enable_journal = 11;
  isc_dpb_disable_journal = 12;
  isc_dpb_dbkey_scope = 13;
  isc_dpb_number_of_users = 14;
  isc_dpb_trace = 15;
  isc_dpb_no_garbage_collect = 16;
  isc_dpb_damaged = 17;
  isc_dpb_license = 18;
  isc_dpb_sys_user_name = 19;
  isc_dpb_encrypt_key = 20;
  isc_dpb_activate_shadow = 21;
  isc_dpb_sweep_interval = 22;
  isc_dpb_delete_shadow = 23;
  isc_dpb_force_write = 24;
  isc_dpb_begin_log = 25;
  isc_dpb_quit_log = 26;
  isc_dpb_no_reserve = 27;
  isc_dpb_user_name = 28;
  isc_dpb_password = 29;
  isc_dpb_password_enc = 30;
  isc_dpb_sys_user_name_enc = 31;
  isc_dpb_interp = 32;
  isc_dpb_online_dump = 33;
  isc_dpb_old_file_size = 34;
  isc_dpb_old_num_files = 35;
  isc_dpb_old_file = 36;
  isc_dpb_old_start_page = 37;
  isc_dpb_old_start_seqno = 38;
  isc_dpb_old_start_file = 39;
  isc_dpb_drop_walfile = 40;
  isc_dpb_old_dump_id = 41;
  isc_dpb_wal_backup_dir = 42;
  isc_dpb_wal_chkptlen = 43;
  isc_dpb_wal_numbufs = 44;
  isc_dpb_wal_bufsize = 45;
  isc_dpb_wal_grp_cmt_wait = 46;
  isc_dpb_lc_messages = 47;
  isc_dpb_lc_ctype = 48;
  isc_dpb_cache_manager = 49;
  isc_dpb_shutdown = 50;
  isc_dpb_online = 51;
  isc_dpb_shutdown_delay = 52;
  isc_dpb_reserved = 53;
  isc_dpb_overwrite = 54;
  isc_dpb_sec_attach = 55;
  isc_dpb_disable_wal = 56;
  isc_dpb_connect_timeout = 57;
  isc_dpb_dummy_packet_interval = 58;
  isc_dpb_gbak_attach = 59;
  isc_dpb_sql_role_name = 60;
  isc_dpb_set_page_buffers = 61;
  isc_dpb_working_directory = 62;
  isc_dpb_SQL_dialect = 63;
  isc_dpb_set_db_readonly = 64;
  isc_dpb_set_db_SQL_dialect = 65;
  isc_dpb_gfix_attach = 66;
  isc_dpb_gstat_attach = 67;
  isc_dpb_last_dpb_constant = isc_dpb_gstat_attach;

  { isc_dpb_verify specific flags }
  isc_dpb_pages = 1;
  isc_dpb_records = 2;
  isc_dpb_indices = 4;
  isc_dpb_transactions = 8;
  isc_dpb_no_update = 16;
  isc_dpb_repair = 32;
  isc_dpb_ignore = 64;

  { isc_dpb_shutdown specific flags }
  isc_dpb_shut_cache = 1;
  isc_dpb_shut_attachment = 2;
  isc_dpb_shut_transaction = 4;
  isc_dpb_shut_force = 8;

  (* Parameters for isc_spb_prp_access_mode *)

  isc_spb_prp_am_readonly = 39;
  isc_spb_prp_am_readwrite = 40;

  { Transaction parameter block stuff }
  isc_tpb_version1 = 1;
  isc_tpb_version3 = 3;
  isc_tpb_consistency = 1;
  isc_tpb_concurrency = 2;
  isc_tpb_shared = 3;
  isc_tpb_protected = 4;
  isc_tpb_exclusive = 5;
  isc_tpb_wait = 6;
  isc_tpb_nowait = 7;
  isc_tpb_read = 8;
  isc_tpb_write = 9;
  isc_tpb_lock_read = 10;
  isc_tpb_lock_write = 11;
  isc_tpb_verb_time = 12;
  isc_tpb_commit_time = 13;
  isc_tpb_ignore_limbo = 14;
  isc_tpb_read_committed = 15;
  isc_tpb_autocommit = 16;
  isc_tpb_rec_version = 17;
  isc_tpb_no_rec_version = 18;
  isc_tpb_restart_requests = 19;
  isc_tpb_no_auto_undo = 20;
  isc_tpb_last_tpb_constant = isc_tpb_no_auto_undo;

  { Blob Parameter Block }
  isc_bpb_version1 = 1;
  isc_bpb_source_type = 1;
  isc_bpb_target_type = 2;
  isc_bpb_type = 3;
  isc_bpb_source_interp = 4;
  isc_bpb_target_interp = 5;
  isc_bpb_filter_parameter = 6;

  (* Service parameter block stuff *)
  isc_spb_version1 = 1;
  isc_spb_current_version = 2;
  isc_spb_version = isc_spb_current_version;
  isc_spb_user_name = isc_dpb_user_name;
  isc_spb_sys_user_name = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc = isc_dpb_sys_user_name_enc;
  isc_spb_password = isc_dpb_password;
  isc_spb_password_enc = isc_dpb_password_enc;
  isc_spb_command_line = 105;
  isc_spb_dbname = 106;
  isc_spb_verbose = 107;
  isc_spb_options = 108;
  isc_spb_connect_timeout = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name = isc_dpb_sql_role_name;

  (* service action block stuff *)
  isc_action_svc_backup = 1;  // Starts database backup process on the server
  isc_action_svc_restore = 2;  // Starts database restore process on the server
  isc_action_svc_repair = 3;  // Starts database repair process on the server
  isc_action_svc_add_user = 4;  // Adds a new user to the security database
  isc_action_svc_delete_user = 5;  // Deletes a user record from the security database
  isc_action_svc_modify_user = 6;  // Modifies a user record in the security database
  isc_action_svc_display_user = 7;  // Displays a user record from the security database
  isc_action_svc_properties = 8;  // Sets database properties
  isc_action_svc_add_license = 9;  // Adds a license to the license file
  isc_action_svc_remove_license = 10;  // Removes a license from the license file
  isc_action_svc_db_stats = 11;  // Retrieves database statistics
  isc_action_svc_get_ib_log = 12;  // Retrieves the InterBase log file from the server

  (* Parameters for isc_action_svc_backup *)
  isc_spb_bkp_file = 5;
  isc_spb_bkp_factor = 6;
  isc_spb_bkp_length = 7;
  isc_spb_bkp_ignore_checksums = $01;
  isc_spb_bkp_ignore_limbo = $02;
  isc_spb_bkp_metadata_only = $04;
  isc_spb_bkp_no_garbage_collect = $08;
  isc_spb_bkp_old_descriptions = $10;
  isc_spb_bkp_non_transportable = $20;
  isc_spb_bkp_convert = $40;
  isc_spb_bkp_expand = $80;

  (*Parameters for isc_action_svc_restore *)

  isc_spb_res_buffers = 9;
  isc_spb_res_page_size = 10;
  isc_spb_res_length = 11;
  isc_spb_res_access_mode = 12;
  isc_spb_res_deactivate_idx = $0100;
  isc_spb_res_no_shadow = $0200;
  isc_spb_res_no_validity = $0400;
  isc_spb_res_one_at_a_time = $0800;
  isc_spb_res_replace = $1000;
  isc_spb_res_create = $2000;
  isc_spb_res_use_all_space = $4000;

  (* Parameters for isc_spb_res_access_mode  *)
  isc_spb_res_am_readonly = isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite = isc_spb_prp_am_readwrite;

  (* Parameters for isc_info_svc_db_stats  *)
  isc_spb_sts_data_pages = $01;
  isc_spb_sts_db_log = $02;
  isc_spb_sts_hdr_pages = $04;
  isc_spb_sts_idx_pages = $08;
  isc_spb_sts_sys_relations = $10;
  isc_spb_sts_record_versions = $20;
  isc_spb_sts_table = $40;

  (* Parameters for isc_action_{add|delete|modify)_user *)

  isc_spb_sec_userid = 5;
  isc_spb_sec_groupid = 6;
  isc_spb_sec_username = 7;
  isc_spb_sec_password = 8;
  isc_spb_sec_groupname = 9;
  isc_spb_sec_firstname = 10;
  isc_spb_sec_middlename = 11;
  isc_spb_sec_lastname = 12;

 {******************************************
 * Parameters for isc_info_svc_svr_db_info *
 *******************************************}

  isc_spb_num_att = 5;
  isc_spb_num_db = 6;

  {*****************************
  *  Service information items *
  *****************************}

  isc_info_svc_svr_db_info = 50;  // Retrieves the number of attachments and databases
  isc_info_svc_get_license = 51;
  // Retrieves all license keys and IDs from the license file
  isc_info_svc_get_license_mask = 52;
  // Retrieves a bitmask representing licensed options on the server
  isc_info_svc_get_config = 53;  // Retrieves the parameters and values for IB_CONFIG
  isc_info_svc_version = 54;  // Retrieves the version of the services manager
  isc_info_svc_server_version = 55;  // Retrieves the version of the InterBase server
  isc_info_svc_implementation = 56;
  // Retrieves the implementation of the InterBase server
  isc_info_svc_capabilities = 57;
  // Retrieves a bitmask representing the server's capabilities
  isc_info_svc_user_dbpath = 58;
  // Retrieves the path to the security database in use by the server
  isc_info_svc_get_env = 59;  // Retrieves the setting of $INTERBASE
  isc_info_svc_get_env_lock = 60;  // Retrieves the setting of $INTERBASE_LCK
  isc_info_svc_get_env_msg = 61;  // Retrieves the setting of $INTERBASE_MSG
  isc_info_svc_line = 62;  // Retrieves 1 line of service output per call
  isc_info_svc_to_eof = 63;
  // Retrieves as much of the server output as will fit in the supplied buffer
  isc_info_svc_timeout = 64;
  // Sets / signifies a timeout value for reading service information
  isc_info_svc_get_licensed_users = 65;
  // Retrieves the number of users licensed for accessing the server
  isc_info_svc_limbo_trans = 66;  // Retrieve the limbo transactions
  isc_info_svc_running = 67;  // Checks to see if a service is running on an attachment
  isc_info_svc_get_users = 68;
  // Returns the user information from isc_action_svc_display_users

 {********************************************
 * Parameters for isc_action_svc_properties *
 ********************************************}

  isc_spb_prp_page_buffers = 5;
  isc_spb_prp_sweep_interval = 6;
  isc_spb_prp_shutdown_db = 7;
  isc_spb_prp_deny_new_attachments = 9;
  isc_spb_prp_deny_new_transactions = 10;
  isc_spb_prp_reserve_space = 11;
  isc_spb_prp_write_mode = 12;
  isc_spb_prp_access_mode = 13;
  isc_spb_prp_set_sql_dialect = 14;
  isc_spb_prp_activate = $0100;
  isc_spb_prp_db_online = $0200;

 {*******************************************
 * Parameters for isc_spb_prp_reserve_space *
 *******************************************}

  isc_spb_prp_res_use_full = 35;
  isc_spb_prp_res = 36;

 {*****************************************
 * Parameters for isc_spb_prp_write_mode  *
 *****************************************}

  isc_spb_prp_wm_async = 37;
  isc_spb_prp_wm_sync = 38;

 {****************************************
 * Parameters for isc_action_svc_repair  *
 ****************************************}

  isc_spb_rpr_commit_trans = 15;
  isc_spb_rpr_rollback_trans = 34;
  isc_spb_rpr_recover_two_phase = 17;
  isc_spb_tra_id = 18;
  isc_spb_single_tra_id = 19;
  isc_spb_multi_tra_id = 20;
  isc_spb_tra_state = 21;
  isc_spb_tra_state_limbo = 22;
  isc_spb_tra_state_commit = 23;
  isc_spb_tra_state_rollback = 24;
  isc_spb_tra_state_unknown = 25;
  isc_spb_tra_host_site = 26;
  isc_spb_tra_remote_site = 27;
  isc_spb_tra_db_path = 28;
  isc_spb_tra_advise = 29;
  isc_spb_tra_advise_commit = 30;
  isc_spb_tra_advise_rollback = 31;
  isc_spb_tra_advise_unknown = 33;

  isc_spb_rpr_validate_db = $01;
  isc_spb_rpr_sweep_db = $02;
  isc_spb_rpr_mend_db = $04;
  isc_spb_rpr_list_limbo_trans = $08;
  isc_spb_rpr_check_db = $10;
  isc_spb_rpr_ignore_checksum = $20;
  isc_spb_rpr_kill_shadows = $40;
  isc_spb_rpr_full = $80;


  { SQL information items }
  isc_info_sql_select = 4;
  isc_info_sql_bind = 5;
  isc_info_sql_num_variables = 6;
  isc_info_sql_describe_vars = 7;
  isc_info_sql_describe_end = 8;
  isc_info_sql_sqlda_seq = 9;
  isc_info_sql_message_seq = 10;
  isc_info_sql_type = 11;
  isc_info_sql_sub_type = 12;
  isc_info_sql_scale = 13;
  isc_info_sql_length = 14;
  isc_info_sql_null_ind = 15;
  isc_info_sql_field = 16;
  isc_info_sql_relation = 17;
  isc_info_sql_owner = 18;
  isc_info_sql_alias = 19;
  isc_info_sql_sqlda_start = 20;
  isc_info_sql_stmt_type = 21;
  isc_info_sql_get_plan = 22;
  isc_info_sql_records = 23;
  isc_info_sql_batch_fetch = 24;

  { SQL information return values }
  isc_info_sql_stmt_select = 1;
  isc_info_sql_stmt_insert = 2;
  isc_info_sql_stmt_update = 3;
  isc_info_sql_stmt_delete = 4;
  isc_info_sql_stmt_ddl = 5;
  isc_info_sql_stmt_get_segment = 6;
  isc_info_sql_stmt_put_segment = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans = 9;
  isc_info_sql_stmt_commit = 10;
  isc_info_sql_stmt_rollback = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator = 13;

  isc_bpb_type_segmented = 0;
  isc_bpb_type_stream = 1;

  {************** Information call declarations **************}

  { Common, structural codes }
  isc_info_end = 1;
  isc_info_truncated = 2;
  isc_info_error = 3;
  isc_info_data_not_ready = 4;
  isc_info_flag_end = 127;

  { Database information items }

  isc_info_db_id = 4;
  isc_info_reads = 5;
  isc_info_writes = 6;
  isc_info_fetches = 7;
  isc_info_marks = 8;
  isc_info_implementation = 11;
  isc_info_version = 12;
  isc_info_base_level = 13;
  isc_info_page_size = 14;
  isc_info_num_buffers = 15;
  isc_info_limbo = 16;
  isc_info_current_memory = 17;
  isc_info_max_memory = 18;
  isc_info_window_turns = 19;
  isc_info_license = 20;
  isc_info_allocation = 21;
  isc_info_attachment_id = 22;
  isc_info_read_seq_count = 23;
  isc_info_read_idx_count = 24;
  isc_info_insert_count = 25;
  isc_info_update_count = 26;
  isc_info_delete_count = 27;
  isc_info_backout_count = 28;
  isc_info_purge_count = 29;
  isc_info_expunge_count = 30;
  isc_info_sweep_interval = 31;
  isc_info_ods_version = 32;
  isc_info_ods_minor_version = 33;
  isc_info_no_reserve = 34;
  isc_info_logfile = 35;
  isc_info_cur_logfile_name = 36;
  isc_info_cur_log_part_offset = 37;
  isc_info_num_wal_buffers = 38;
  isc_info_wal_buffer_size = 39;
  isc_info_wal_ckpt_length = 40;
  isc_info_wal_cur_ckpt_interval = 41;
  isc_info_wal_prv_ckpt_fname = 42;
  isc_info_wal_prv_ckpt_poffset = 43;
  isc_info_wal_recv_ckpt_fname = 44;
  isc_info_wal_recv_ckpt_poffset = 45;
  isc_info_wal_grpc_wait_usecs = 47;
  isc_info_wal_num_io = 48;
  isc_info_wal_avg_io_size = 49;
  isc_info_wal_num_commits = 50;
  isc_info_wal_avg_grpc_size = 51;
  isc_info_forced_writes = 52;
  isc_info_user_names = 53;
  isc_info_page_errors = 54;
  isc_info_record_errors = 55;
  isc_info_bpage_errors = 56;
  isc_info_dpage_errors = 57;
  isc_info_ipage_errors = 58;
  isc_info_ppage_errors = 59;
  isc_info_tpage_errors = 60;
  isc_info_set_page_buffers = 61;
  isc_info_db_SQL_dialect = 62;
  isc_info_db_read_only = 63;
  isc_info_db_size_in_pages = 64;

  // firebird 1.0
  (* Values 65 -100 unused to avoid conflict with InterBase *)

  frb_info_att_charset = 101;
  isc_info_db_class = 102;
  isc_info_firebird_version = 103;
  isc_info_oldest_transaction = 104;
  isc_info_oldest_active = 105;
  isc_info_oldest_snapshot = 106;
  isc_info_next_transaction = 107;
  isc_info_db_provider = 108;
  isc_info_db_last_value = isc_info_db_provider;

  (* Leave this LAST! *)

  isc_info_isc_version = isc_info_version;


  { Database information return values }

  isc_info_db_impl_rdb_vms = 1;
  isc_info_db_impl_rdb_eln = 2;
  isc_info_db_impl_rdb_eln_dev = 3;
  isc_info_db_impl_rdb_vms_y = 4;
  isc_info_db_impl_rdb_eln_y = 5;
  isc_info_db_impl_jri = 6;
  isc_info_db_impl_jsv = 7;

  isc_info_db_impl_isc_apl_68K = 25;
  isc_info_db_impl_isc_vax_ultr = 26;
  isc_info_db_impl_isc_vms = 27;
  isc_info_db_impl_isc_sun_68k = 28;
  isc_info_db_impl_isc_os2 = 29;
  isc_info_db_impl_isc_sun4 = 30;   (* 30 *)

  isc_info_db_impl_isc_hp_ux = 31;
  isc_info_db_impl_isc_sun_386i = 32;
  isc_info_db_impl_isc_vms_orcl = 33;
  isc_info_db_impl_isc_mac_aux = 34;
  isc_info_db_impl_isc_rt_aix = 35;
  isc_info_db_impl_isc_mips_ult = 36;
  isc_info_db_impl_isc_xenix = 37;
  isc_info_db_impl_isc_dg = 38;
  isc_info_db_impl_isc_hp_mpexl = 39;
  isc_info_db_impl_isc_hp_ux68K = 40;  (* 40 *)

  isc_info_db_impl_isc_sgi = 41;
  isc_info_db_impl_isc_sco_unix = 42;
  isc_info_db_impl_isc_cray = 43;
  isc_info_db_impl_isc_imp = 44;
  isc_info_db_impl_isc_delta = 45;
  isc_info_db_impl_isc_next = 46;
  isc_info_db_impl_isc_dos = 47;
  isc_info_db_impl_m88K = 48;
  isc_info_db_impl_unixware = 49;
  isc_info_db_impl_isc_winnt_x86 = 50;

  isc_info_db_impl_isc_epson = 51;
  isc_info_db_impl_alpha_osf = 52;
  isc_info_db_impl_alpha_vms = 53;
  isc_info_db_impl_netware_386 = 54;
  isc_info_db_impl_win_only = 55;
  isc_info_db_impl_ncr_3000 = 56;
  isc_info_db_impl_winnt_ppc = 57;
  isc_info_db_impl_dg_x86 = 58;
  isc_info_db_impl_sco_ev = 59;
  isc_info_db_impl_i386 = 60;

  isc_info_db_impl_freebsd = 61;
  isc_info_db_impl_netbsd = 62;
  isc_info_db_impl_darwin = 63;
  isc_info_db_impl_last_value = isc_info_db_impl_darwin;
  (* Leave this LAST!*)


  isc_info_db_impl_isc_a = isc_info_db_impl_isc_apl_68K;
  isc_info_db_impl_isc_u = isc_info_db_impl_isc_vax_ultr;
  isc_info_db_impl_isc_v = isc_info_db_impl_isc_vms;
  isc_info_db_impl_isc_s = isc_info_db_impl_isc_sun_68k;


  isc_info_db_class_access = 1;
  isc_info_db_class_y_valve = 2;
  isc_info_db_class_rem_int = 3;
  isc_info_db_class_rem_srvr = 4;
  isc_info_db_class_pipe_int = 7;
  isc_info_db_class_pipe_srvr = 8;
  isc_info_db_class_sam_int = 9;
  isc_info_db_class_sam_srvr = 10;
  isc_info_db_class_gateway = 11;
  isc_info_db_class_cache = 12;
  // firebird 1.0
  isc_info_db_class_classic_access = 13;
  isc_info_db_class_server_access = 14;
  isc_info_db_class_last_value = isc_info_db_class_server_access;
  (* Leave this LAST! *)


  // firebird 1.0
  isc_info_db_code_rdb_eln = 1;
  isc_info_db_code_rdb_vms = 2;
  isc_info_db_code_interbase = 3;
  isc_info_db_code_firebird = 4;
  isc_info_db_code_last_value = isc_info_db_code_firebird;
  (* Leave this LAST! *)


  { Request information items }
  isc_info_number_messages = 4;
  isc_info_max_message = 5;
  isc_info_max_send = 6;
  isc_info_max_receive = 7;
  isc_info_state = 8;
  isc_info_message_number = 9;
  isc_info_message_size = 10;
  isc_info_request_cost = 11;
  isc_info_access_path = 12;
  isc_info_req_select_count = 13;
  isc_info_req_insert_count = 14;
  isc_info_req_update_count = 15;
  isc_info_req_delete_count = 16;

  // blob info

  isc_info_blob_num_segments = 4;
  isc_info_blob_max_segment = 5;
  isc_info_blob_total_length = 6;
  isc_info_blob_type = 7;

  { SQL definitions }
  SQL_VARYING = 448;
  SQL_TEXT = 452;
  SQL_DOUBLE = 480;
  SQL_FLOAT = 482;
  SQL_LONG = 496;
  SQL_SHORT = 500;
  SQL_TIMESTAMP = 510;
  SQL_BLOB = 520;
  SQL_D_FLOAT = 530;
  SQL_ARRAY = 540;
  SQL_QUAD = 550;
  SQL_TYPE_TIME = 560;
  SQL_TYPE_DATE = 570;
  SQL_INT64 = 580;
  SQL_DATE = SQL_TIMESTAMP;

  { Blob Subtypes }
  { types less than zero are reserved for customer use }
  isc_blob_untyped = 0;

  { internal subtypes }
  isc_blob_text = 1;
  isc_blob_blr = 2;
  isc_blob_acl = 3;
  isc_blob_ranges = 4;
  isc_blob_summary = 5;
  isc_blob_format = 6;
  isc_blob_tra = 7;
  isc_blob_extfile = 8;

  { the range 20-30 is reserved for dBASE and Paradox types }
  isc_blob_formatted_memo = 20;
  isc_blob_paradox_ole = 21;
  isc_blob_graphic = 22;
  isc_blob_dbase_ole = 23;
  isc_blob_typed_binary = 24;

  { Blr definitions }
  blr_text = 14;
  blr_text2 = 15;
  blr_short = 7;
  blr_long = 8;
  blr_quad = 9;
  blr_float = 10;
  blr_double = 27;
  blr_d_float = 11;
  blr_timestamp = 35;
  blr_varying = 37;
  blr_varying2 = 38;
  blr_blob = 261;
  blr_cstring = 40;
  blr_cstring2 = 41;
  blr_blob_id = 45;
  blr_sql_date = 12;
  blr_sql_time = 13;
  blr_int64 = 16;
  blr_date = blr_timestamp;

  blr_inner = 0;
  blr_left = 1;
  blr_right = 2;
  blr_full = 3;

  blr_gds_code = 0;
  blr_sql_code = 1;
  blr_exception = 2;
  blr_trigger_code = 3;
  blr_default_code = 4;

  blr_version4 = 4;
  blr_version5 = 5;
  blr_eoc = 76;
  blr_end = -1;

  blr_assignment = 1;
  blr_begin = 2;
  blr_dcl_variable = 3;
  blr_message = 4;
  blr_erase = 5;
  blr_fetch = 6;
  blr_for = 7;
  blr_if = 8;
  blr_loop = 9;
  blr_modify = 10;
  blr_handler = 11;
  blr_receive = 12;
  blr_select = 13;
  blr_send = 14;
  blr_store = 15;
  blr_label = 17;
  blr_leave = 18;
  blr_store2 = 19;
  blr_post = 20;

  blr_literal = 21;
  blr_dbkey = 22;
  blr_field = 23;
  blr_fid = 24;
  blr_parameter = 25;
  blr_variable = 26;
  blr_average = 27;
  blr_count = 28;
  blr_maximum = 29;
  blr_minimum = 30;
  blr_total = 31;
  blr_add = 34;
  blr_subtract = 35;
  blr_multiply = 36;
  blr_divide = 37;
  blr_negate = 38;
  blr_concatenate = 39;
  blr_substring = 40;
  blr_parameter2 = 41;
  blr_from = 42;
  blr_via = 43;
  blr_user_name = 44;
  blr_null = 45;
 
  blr_eql = 47;
  blr_neq = 48;
  blr_gtr = 49;
  blr_geq = 50;
  blr_lss = 51;
  blr_leq = 52;
  blr_containing = 53;
  blr_matching = 54;
  blr_starting = 55;
  blr_between = 56;
  blr_or = 57;
  blr_and = 58;
  blr_not = 59;
  blr_any = 60;
  blr_missing = 61;
  blr_unique = 62;
  blr_like = 63;
 
  blr_stream = 65;
  blr_set_index = 66;
  blr_rse = 67;
  blr_first = 68;
  blr_project = 69;
  blr_sort = 70;
  blr_boolean = 71;
  blr_ascending = 72;
  blr_descending = 73;
  blr_relation = 74;
  blr_rid = 75;
  blr_union = 76;
  blr_map = 77;
  blr_group_by = 78;
  blr_aggregate = 79;
  blr_join_type = 80;

  blr_agg_count = 83;
  blr_agg_max = 84;
  blr_agg_min = 85;
  blr_agg_total = 86;
  blr_agg_average = 87;
  blr_parameter3 = 88;
  blr_run_count = 118;
  blr_run_max = 89;
  blr_run_min = 90;
  blr_run_total = 91;
  blr_run_average = 92;
  blr_agg_count2 = 93;
  blr_agg_count_distinct = 94;
  blr_agg_total_distinct = 95;
  blr_agg_average_distinct = 96;

  blr_function = 100;
  blr_gen_id = 101;
  blr_prot_mask = 102;
  blr_upcase = 103;
  blr_lock_state = 104;
  blr_value_if = 105;
  blr_matching2 = 106;
  blr_index = 107;
  blr_ansi_like = 108;
  blr_bookmark = 109;
  blr_crack = 110;
  blr_force_crack = 111;
  blr_seek = 112;
  blr_find = 113;

  blr_continue = 0;
  blr_forward = 1;
  blr_backward = 2;
  blr_bof_forward = 3;
  blr_eof_backward = 4;

  blr_lock_relation = 114;
  blr_lock_record = 115;
  blr_set_bookmark = 116;
  blr_get_bookmark = 117;
  blr_rs_stream = 119;
  blr_exec_proc = 120;
  blr_begin_range = 121;
  blr_end_range = 122;
  blr_delete_range = 123;
  blr_procedure = 124;
  blr_pid = 125;
  blr_exec_pid = 126;
  blr_singular = 127;
  blr_abort = 128;
  blr_block = 129;
  blr_error_handler = 130;
  blr_cast = 131;
  blr_release_lock = 132;
  blr_release_locks = 133;
  blr_start_savepoint = 134;
  blr_end_savepoint = 135;
  blr_find_dbkey = 136;
  blr_range_relation = 137;
  blr_delete_ranges = 138;

  blr_plan = 139;
  blr_merge = 140;
  blr_join = 141;
  blr_sequential = 142;
  blr_navigational = 143;
  blr_indices = 144;
  blr_retrieve = 145;

  blr_relation2 = 146;
  blr_rid2 = 147;
  blr_reset_stream = 148;
  blr_release_bookmark = 149;
  blr_set_generator = 150;
  blr_ansi_any = 151;
  blr_exists = 152;
  blr_cardinality = 153;

  blr_record_version = 154; // get tid of record
  blr_stall = 155; // fake server stall
  blr_seek_no_warn = 156;
  blr_find_dbkey_version = 157;
  blr_ansi_all = 158;
  blr_extract = 159;

  { sub parameters for blr_extract }
  blr_extract_year = 0;
  blr_extract_month = 1;
  blr_extract_day = 2;
  blr_extract_hour = 3;
  blr_extract_minute = 4;
  blr_extract_second = 5;
  blr_extract_weekday = 6;
  blr_extract_yearday = 7;

  blr_current_date = 160;
  blr_current_timestamp = 161;
  blr_current_time = 162;

  {******* These verbs were added in 6.0, primarily to support 64-bit integers *********}

  blr_add2 = 163;
  blr_subtract2 = 164;
  blr_multiply2 = 165;
  blr_divide2 = 166;
  blr_agg_total2 = 167;
  blr_agg_total_distinct2 = 168;
  blr_agg_average2 = 169;
  blr_agg_average_distinct2 = 170;
  blr_average2 = 171;
  blr_gen_id2 = 172;
  blr_set_generator2 = 173;

  { for security function }

  sec_uid_spec = $01;
  sec_gid_spec = $02;
  sec_server_spec = $04;
  sec_password_spec = $08;
  sec_group_name_spec = $10;
  sec_first_name_spec = $20;
  sec_middle_name_spec = $40;
  sec_last_name_spec = $80;
  sec_dba_user_name_spec = $100;
  sec_dba_password_spec = $200;

  sec_protocol_tcpip = 1;
  sec_protocol_netbeui = 2;
  sec_protocol_spx = 3;
  sec_protocol_local = 4;
  
  {****************** Plain API Types definition *****************}

  {$IFDEF FPC}
  //{$PACKRECORDS C}
  {$ENDIF}

type
  Int = longint;
  Long = longint;
  Short = smallint;
  ULong = cardinal;
  UChar = char;
  UShort = word;
  Pint = ^Int;
  PLong = ^Long;
  PVoid = Pointer;
  PShort = ^Short;
  //PPChar = ^PChar;
  Float = single;
  ISC_LONG = longint;
  UISC_LONG = ULong;
  ISC_INT64 = int64;
  ISC_STATUS = longint;
  UISC_STATUS = ULong;
  PISC_LONG = ^ISC_LONG;
  PUISC_LONG = ^UISC_LONG;
  PISC_STATUS = ^ISC_STATUS;
  PPISC_STATUS = ^PISC_STATUS;
  PUISC_STATUS = ^UISC_STATUS;
  PFloat = ^Float;
  (*Delphi 4/5 *)
  {$IFDEF D6M}
  PDouble = ^double;
  PSingle = ^single;
  {$ENDIF}

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec: integer;   { Seconds }
    tm_min: integer;   { Minutes }
    tm_hour: integer;   { Hour (0--23) }
    tm_mday: integer;   { Day of month (1--31) }
    tm_mon: integer;   { Month (0--11) }
    tm_year: integer;   { Year (calendar year minus 1900) }
    tm_wday: integer;   { Weekday (0--6) Sunday = 0) }
    tm_yday: integer;   { Day of year (0--365) }
    tm_isdst: integer;   { 0 if daylight savings time is not in effect) }
  end;
  PCTimeStructure = ^TCTimeStructure;
  TM = TCTimeStructure;
  PTM = ^TM;

  { InterBase Handle Definitions }
  TISC_BLOB_HANDLE = PVoid;
  PISC_BLOB_HANDLE = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE = PVoid;
  PISC_DB_HANDLE = ^TISC_DB_HANDLE;
  TISC_STMT_HANDLE = PVoid;
  PISC_STMT_HANDLE = ^TISC_STMT_HANDLE;
  TISC_TR_HANDLE = PVoid;
  PISC_TR_HANDLE = ^TISC_TR_HANDLE;
  TISC_CALLBACK = procedure;
  TISC_SVC_HANDLE = PVoid;
  PISC_SVC_HANDLE = ^TISC_SVC_HANDLE;

  { Time & Date Support }
  ISC_DATE = longint;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;

  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  { Blob id structure }
  TGDS_QUAD = record
    gds_quad_high: ISC_LONG;
    gds_quad_low: UISC_LONG;
  end;
  PGDS_QUAD = ^TGDS_QUAD;

  TISC_QUAD = TGDS_QUAD;
  PISC_QUAD = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower: Short;
    array_bound_upper: Short;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype: UChar;
    array_desc_scale: char;
    array_desc_length: Short;
    array_desc_field_name: array[0..31] of char;
    array_desc_relation_name: array[0..31] of char;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype: Short;
    blob_desc_charset: Short;
    blob_desc_segment_size: Short;
    blob_desc_field_name: array[0..31] of UChar;
    blob_desc_relation_name: array[0..31] of UChar;
  end;
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  { varying type }

  TISC_VARYING = record
    strlen: Short;
    str: array[0..0] of char;
  end;
  PISC_VARYING = ^TISC_VARYING;

  { Declare the extended SQLDA }

  TXSQLVAR = record
    sqltype: Short;     { datatype of field }
    sqlscale: Short;     { scale factor }
    sqlsubtype: Short;     { datatype subtype - BLOBs }
    { & text types only }
    sqllen: Short;     { length of data area }
    sqldata: PChar;     { address of data }
    sqlind: PShort;    { address of indicator }
    { variable }
    sqlname_length: Short;     { length of sqlname field }
    { name of field, name length + space for NULL }
    sqlname: array[0..31] of char;
    relname_length: Short;     { length of relation name }
    { field's relation name + space for NULL }
    relname: array[0..31] of char;
    ownname_length: Short;     { length of owner name }
    { relation's owner name + space for NULL }
    ownname: array[0..31] of char;
    aliasname_length: Short;     { length of alias name }
    { relation's alias name + space for NULL }
    aliasname: array[0..31] of char;
  end;
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version: Short;     { version of this XSQLDA }
    { XSQLDA name field }
    sqldaid: array[0..7] of char;
    sqldabc: ISC_LONG;  { length in bytes of SQLDA }
    sqln: Short;     { number of fields allocated }
    sqld: Short;     { actual number of fields }
    { first field address }
    sqlvar: array[0..0] of TXSQLVAR;
  end;
  PXSQLDA = ^TXSQLDA;

  {****************************************************}
  { This record type is for passing arguments to       }
  { isc_start_transaction (See docs)                   }
  {****************************************************}
  TISC_START_TRANS = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: word;
    tpb_address: PChar;
  end;

  {****************************************************}
  { This record type is for passing arguments to       }
  { isc_start_multiple (see docs)                      }
  {****************************************************}
  TISC_TEB = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: longint;
    tpb_address: PChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

  (* Security Functions *)

  TUSER_SEC_DATA = record
    sec_flags: Short;              (** which fields are specified **)
    uid: Int;                      (** the user's id **)
    gid: Int;                      (** the user's group id **)
    protocol: Int;                 (** protocol to use for connection **)
    server: PChar;                 (** server to administer **)
    user_name: PChar;              (** the user's name **)
    password: PChar;               (** the user's password **)
    group_name: PChar;             (** the group name **)
    first_name: PChar;             (** the user's first name **)
    middle_name: PChar;            (** the user's middle name **)
    last_name: PChar;              (** the user's last name **)
    dba_user_name: PChar;          (** the dba user name **)
    dba_password: PChar;           (** the dba password **)
  end;
  PUSER_SEC_DATA = ^TUSER_SEC_DATA;

  {$IFDEF FPC}
  //{$PACKRECORDS DEFAULT}
  {$ENDIF}


  (** security function **)

  Tisc_add_user = function(status_vector: PISC_STATUS;
    user_sec_data: PUSER_SEC_DATA): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  Tisc_delete_user = function(status_vector: PISC_STATUS;
    user_sec_data: PUSER_SEC_DATA): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  Tisc_modify_user = function(status_vector: PISC_STATUS;
    user_sec_data: PUSER_SEC_DATA): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  (** Service manager functions  **)

  Tisc_service_attach = function(status_vector: PISC_STATUS;
    service_length: UShort;
    service: PChar;
    svc_handle: PISC_SVC_HANDLE;
    spb_length: UShort;
    spb: PChar): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  Tisc_service_detach = function(status_vector: PISC_STATUS;
    svc_handle: PISC_SVC_HANDLE): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  Tisc_service_query = function(status_vector: PISC_STATUS;
    service_handle: PISC_SVC_HANDLE;
    recv_handle: PISC_SVC_HANDLE;
    send_spb_length: UShort;
    send_spb: PChar;
    request_spb_length: UShort;
    request_spb: PChar;
    buffer_length: UShort;
    buffer: PChar): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

  Tisc_service_start = function(status_vector: PISC_STATUS;
    service_handle: PISC_SVC_HANDLE;
    recv_handle: PISC_SVC_HANDLE;
    spb_length: UShort;
    spb: PChar): ISC_STATUS;
  {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}


  {************** Plain API Function types definition *************}

  { General database routines }

  Tisc_attach_database = function(status_vector: PISC_STATUS;
    db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
    parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_detach_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_drop_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_database_info = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short;
    result_buffer: PChar): ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Array processing routines }
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS;
    isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
    isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_get_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar; 
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar; 
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_set_desc = function(status_vector: PISC_STATUS;
    table_name: PChar; column_name: PChar;
    sql_dtype, sql_length, sql_dimensions: PShort;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_put_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_free = function(isc_arg1: PChar): ISC_LONG;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PChar;
    buffer_length: Short); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_interprete = function(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Transaction support routines }

  Tisc_start_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    db_handle: PISC_DB_HANDLE; tpb_length: UShort; tpb_address: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_start_multiple = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    teb_vector_address: PISC_TEB): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Dynamic SQL routines }

  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
    xsqlda: PXSQLDA): ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort;
    in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort;
    statement: PChar; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_fetch = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; options: UShort): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_prepare = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
    length: UShort; statement: PChar; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: UShort): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
    buffer_length: Short; buffer: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Blob processing routines }

  Tisc_open_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_buffer: PChar): ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_create_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_address: PChar): ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_blob_info = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short;
    result_buffer: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_close_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_get_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
    seg_buffer_length: UShort; seg_buffer: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_put_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort;
    seg_buffer: PChar): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Event processing routines }

  {$IFDEF FPC}
  Tisc_event_block = function(event_buffer: PPChar; result_buffer: PPChar;
    id_count: UShort; event1, event2, event3, event4, event5, event6,
    event7, event8, event9,
    event10, event11, event12, event13, event14, event15: PChar): ISC_LONG;
  cdecl;
  {$ELSE}
  Tisc_event_block = function(event_buffer: PPChar; result_buffer: PPChar;
    id_count: UShort; event_list: array of PChar): ISC_LONG;
  cdecl;
  {$ENDIF}

  Tisc_event_counts = procedure(status_vector: PISC_STATUS;
    buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_que_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
    event_buffer: PChar; event_function: TISC_CALLBACK;
    event_function_arg: PVoid): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Types convertion routines }

  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Interbase Version 6 routines }
  Tisc_decode_sql_date = procedure(ib_date: PISC_DATE;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure;
    ib_date: PISC_DATE); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure;
    ib_time: PISC_TIME); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure;
    ib_timestamp: PISC_TIMESTAMP); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_vax_integer = function(buffer: PChar; length: Short): ISC_LONG;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  {*******************************}
  { Client information functions  }
  { Only Firebird 1.5             }
  {*******************************}

  Tisc_get_client_version = procedure(Buffer: PChar);
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tisc_get_client_major_version = function: integer;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tisc_get_client_minor_version = function: integer;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  {************* Plain API Function variables definition ************}

var
  { General database routines }
  isc_attach_database: Tisc_attach_database;
  isc_detach_database: Tisc_detach_database;
  isc_drop_database: Tisc_drop_database;
  isc_database_info: Tisc_database_info;
  isc_free: Tisc_free;
  isc_sqlcode: Tisc_sqlcode;
  isc_sql_interprete: Tisc_sql_interprete;
  isc_interprete: Tisc_interprete;

  { Transaction support routines }
  isc_start_transaction: Tisc_start_transaction;
  isc_start_multiple: Tisc_start_multiple;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;

  { Dynamic SQL routines }
  isc_dsql_allocate_statement: Tisc_dsql_allocate_statement;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_describe: Tisc_dsql_describe;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_execute: Tisc_dsql_execute;
  isc_dsql_execute2: Tisc_dsql_execute2;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_dsql_fetch: Tisc_dsql_fetch;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_prepare: Tisc_dsql_prepare;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_sql_info: Tisc_dsql_sql_info;

  { Array processing routines }
  isc_array_gen_sdl: Tisc_array_gen_sdl;
  isc_array_get_slice: Tisc_array_get_slice;
  isc_array_lookup_bounds: Tisc_array_lookup_bounds;
  isc_array_lookup_desc: Tisc_array_lookup_desc;
  isc_array_set_desc: Tisc_array_set_desc;
  isc_array_put_slice: Tisc_array_put_slice;

  { Blob processing routines }
  isc_open_blob2: Tisc_open_blob2;
  isc_create_blob2: Tisc_create_blob2;
  isc_blob_info: Tisc_blob_info;
  isc_close_blob: Tisc_close_blob;
  isc_cancel_blob: Tisc_cancel_blob;
  isc_get_segment: Tisc_get_segment;
  isc_put_segment: Tisc_put_segment;

  { Event processing routines }
  isc_que_events: Tisc_que_events;
  isc_event_counts: Tisc_event_counts;
  isc_event_block: Tisc_event_block;
  isc_cancel_events: Tisc_cancel_events;

  { Types convertion routines }
  isc_encode_date: Tisc_encode_date;
  isc_decode_date: Tisc_decode_date;
  isc_vax_integer: Tisc_vax_integer;

  isc_encode_sql_date: Tisc_encode_sql_date;
  isc_decode_sql_date: Tisc_decode_sql_date;

  isc_encode_sql_time: Tisc_encode_sql_time;
  isc_decode_sql_time: Tisc_decode_sql_time;

  isc_encode_timestamp: Tisc_encode_timestamp;        
  isc_decode_timestamp: Tisc_decode_timestamp;

  (* Security Functions *) //add by Alessandro Batisti
  isc_add_user: Tisc_add_user;
  isc_delete_user: Tisc_delete_user;
  isc_modify_user: Tisc_modify_user;

  (*service function *)
  isc_service_attach: Tisc_service_attach;
  isc_service_detach: Tisc_service_detach;
  isc_service_query: Tisc_service_query;
  isc_service_start: Tisc_service_start;

  (* client Inormation function only Firebird 1.5.X *)
  isc_get_client_version: Tisc_get_client_version;
  isc_get_client_major_version: Tisc_get_client_major_version;
  isc_get_client_minor_version: Tisc_get_client_minor_version;

  { Library Initialization }

function LoadFBClientLibrary: boolean;
procedure CheckFbClientLoaded;
function FBClientLibraryExists: boolean;


{ XSQLDA_LENGTH is defined in C as a macro, but in Pascal we must defined it
   as a function... }
function XSQLDA_LENGTH(Value: longint): longint;

  {Service manager function}
procedure add_spb_length(var p: PChar; ALength: integer);
procedure add_spb_numeric(var p: PChar; AData: integer);

function GetFbClientVersion: integer;

var
  DLL: string;
  hDLL: TLibHandle;
  {$IFDEF UNIX}
  hCryptDLL: TLibHandle;
  {$ENDIF}
  _ClientVersion: integer;
  LibLoaded: boolean;

implementation

{$IFDEF UNIX}
 {$IFNDEF FPC}
uses Libc;
 {$ELSE}
   
uses dl;
 {$ENDIF}
{$ELSE}
  
uses Windows;
{$ENDIF}


function FBClientLibraryExists: boolean;
begin
  if hDLL = LibHandleNil then
    LoadFBClientLibrary;
  Result := (hDLL <> LibHandleNil);
end;

//------------------------------------------------------------------------------

function GetLibraryHandle(const ALibName: string): TLibHandle;
begin
  {$IFDEF UNIX}
  Result := dlopen(PChar(ALibName), RTLD_LAZY);
  {$ELSE}
  Result := LoadLibrary(PChar(ALibName));
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function GetProcedurePointer(ALib: TLibHandle; const AProcName: string): Pointer;
var
  bErr: boolean;
begin
  {$IFDEF UNIX}
  Result := dlsym(Alib, PChar(AProcName));
  bErr := (Result = nil);
  {$ELSE}
  Result := GetProcAddress(ALib, PChar(AProcName));
  bErr := (Result = nil);
  {$ENDIF}
  if bErr then
    raise Exception.CreateFmt('Unable to locate symbol "%s"', [AProcName]);
end;

//------------------------------------------------------------------------------

function TryGetProcedurePointer(ALib: TLibHandle; const AProcName: string): Pointer;
begin
  {$IFDEF UNIX}
  Result := dlsym(Alib, PChar(AProcName));
  //if (dlerror <> nil) then Result := nil;
  {$ELSE}
  Result := GetProcAddress(Alib, PChar(AprocName));
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function GetFbClientVersion: integer;
begin
  CheckFbClientLoaded;
  Result := _ClientVersion;
end;

//------------------------------------------------------------------------------


procedure CheckFbClientLoaded;
begin
  if hDLL = LibHandleNil then
    LoadFBClientLibrary;
  if hDLL = LibHandleNil then
    raise Exception.Create('Library ' + DLL + ' not found');
end;

//------------------------------------------------------------------------------

{ Initialize Firebird Dynamic library }

function LoadFBClientLibrary: boolean;

  procedure TryToLoadLibrary;
  begin
    if hDLL = LibHandleNil then
    begin
      LibLoaded := False;
      if hDLL = LibHandleNil then
      begin
        hDLL := GetLibraryHandle(DLL);
        LibLoaded := True;
      end;
    end;
  end;
begin
  // Firebird needs libcryp.so symbols
  {$IFDEF UNIX}
  {$IFNDEF FPC}
  hCryptDLL := dlopen('libcrypt.so', RTLD_GLOBAL);
  if hCryptDLL = nil then
  begin
    hCryptDLL := dlopen('libcrypt.so.1', RTLD_GLOBAL);
    if hCryptDLL = nil then raise Exception.Create('Error loading libcrypt.so');
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF UNIX}
  DLL := 'fbembed.dll';
  TryToLoadLibrary;
  if hDLL = LibHandleNil then
  begin
    {$ENDIF}
    DLL := DEFAULT_DLL_LOCATION2;
    TryToLoadLibrary;
    if hDLL = LibHandleNil then
    begin
      DLL := DEFAULT_DLL_LOCATION;
      TryToLoadLibrary;
    end;
    {$IFNDEF UNIX}
  end;
  {$ENDIF}

  if hDLL <> LibHandleNil then
  begin
    isc_sqlcode := Tisc_sqlcode(GetProcedurePointer(hDLL, 'isc_sqlcode'));
    isc_sql_interprete := Tisc_sql_interprete(GetProcedurePointer(hDLL, 'isc_sql_interprete'));
    isc_interprete := Tisc_interprete(GetProcedurePointer(hDLL, 'isc_interprete'));
    isc_vax_integer := Tisc_vax_integer(GetProcedurePointer(hDLL, 'isc_vax_integer'));
    isc_array_gen_sdl := Tisc_array_gen_sdl(GetProcedurePointer(hDLL, 'isc_array_gen_sdl'));
    isc_array_get_slice := Tisc_array_get_slice(GetProcedurePointer(hDLL, 'isc_array_get_slice'));
    isc_array_lookup_bounds := Tisc_array_lookup_bounds(GetProcedurePointer(hDLL, 'isc_array_lookup_bounds'));
    isc_array_lookup_desc := Tisc_array_lookup_desc(GetProcedurePointer(hDLL, 'isc_array_lookup_desc'));
    isc_array_set_desc := Tisc_array_set_desc(GetProcedurePointer(hDLL, 'isc_array_set_desc'));
    isc_array_put_slice := Tisc_array_put_slice(GetProcedurePointer(hDLL, 'isc_array_put_slice'));
    isc_blob_info := Tisc_blob_info (GetProcedurePointer(hDLL, 'isc_blob_info'));
    isc_open_blob2 := Tisc_open_blob2(GetProcedurePointer(hDLL, 'isc_open_blob2'));
    isc_close_blob := Tisc_close_blob(GetProcedurePointer(hDLL, 'isc_close_blob'));
    isc_cancel_blob := Tisc_cancel_blob(GetProcedurePointer(hDLL, 'isc_cancel_blob'));
    isc_get_segment := Tisc_get_segment(GetProcedurePointer(hDLL, 'isc_get_segment'));
    isc_put_segment := Tisc_put_segment(GetProcedurePointer(hDLL, 'isc_put_segment'));
    isc_create_blob2 := Tisc_create_blob2(GetProcedurePointer(hDLL, 'isc_create_blob2'));
    isc_decode_date := Tisc_decode_date(GetProcedurePointer(hDLL, 'isc_decode_date'));
    isc_encode_date := Tisc_encode_date(GetProcedurePointer(hDLL, 'isc_encode_date'));
    isc_dsql_free_statement := Tisc_dsql_free_statement(GetProcedurePointer(hDLL, 'isc_dsql_free_statement'));
    isc_dsql_execute2 := Tisc_dsql_execute2(GetProcedurePointer(hDLL, 'isc_dsql_execute2'));
    isc_dsql_execute := Tisc_dsql_execute(GetProcedurePointer(hDLL, 'isc_dsql_execute'));
    isc_dsql_set_cursor_name := Tisc_dsql_set_cursor_name(GetProcedurePointer(hDLL, 'isc_dsql_set_cursor_name'));
    isc_dsql_fetch := Tisc_dsql_fetch(GetProcedurePointer(hDLL, 'isc_dsql_fetch'));
    isc_dsql_sql_info := Tisc_dsql_sql_info(GetProcedurePointer(hDLL, 'isc_dsql_sql_info'));
    isc_dsql_allocate_statement := Tisc_dsql_allocate_statement(GetProcedurePointer(hDLL, 'isc_dsql_allocate_statement'));
    isc_dsql_alloc_statement2 := Tisc_dsql_alloc_statement2(GetProcedurePointer(hDLL,'isc_dsql_alloc_statement2'));
    isc_dsql_prepare := Tisc_dsql_prepare(GetProcedurePointer(hDLL, 'isc_dsql_prepare'));
    isc_dsql_describe_bind := Tisc_dsql_describe_bind(GetProcedurePointer(hDLL, 'isc_dsql_describe_bind'));
    isc_dsql_describe := Tisc_dsql_describe(GetProcedurePointer(hDLL, 'isc_dsql_describe'));
    isc_dsql_execute_immediate := Tisc_dsql_execute_immediate(GetProcedurePointer(hDLL, 'isc_dsql_execute_immediate'));
    isc_drop_database := Tisc_drop_database(GetProcedurePointer(hDLL, 'isc_drop_database'));
    isc_detach_database := Tisc_detach_database(GetProcedurePointer(hDLL, 'isc_detach_database'));
    isc_attach_database := Tisc_attach_database(GetProcedurePointer(hDLL, 'isc_attach_database'));
    isc_database_info := Tisc_database_info(GetProcedurePointer(hDLL, 'isc_database_info'));
    isc_start_multiple := Tisc_start_multiple(GetProcedurePointer(hDLL, 'isc_start_multiple'));
    isc_start_transaction := Tisc_start_transaction(GetProcedurePointer(hDLL, 'isc_start_transaction'));
    isc_commit_transaction := Tisc_commit_transaction(GetProcedurePointer(hDLL, 'isc_commit_transaction'));
    isc_commit_retaining := Tisc_commit_retaining(GetProcedurePointer(hDLL, 'isc_commit_retaining'));
    isc_rollback_transaction := Tisc_rollback_transaction(GetProcedurePointer(hDLL, 'isc_rollback_transaction'));
    isc_cancel_events := Tisc_cancel_events(GetProcedurePointer(hDLL, 'isc_cancel_events'));
    isc_que_events := Tisc_que_events(GetProcedurePointer(hDLL, 'isc_que_events'));
    isc_event_counts := Tisc_event_counts(GetProcedurePointer(hDLL, 'isc_event_counts'));
    isc_event_block := Tisc_event_block(GetProcedurePointer(hDLL, 'isc_event_block'));
    isc_free := Tisc_free(GetProcedurePointer(hDLL, 'isc_free'));
    // func add  by Alessandro Batisti
    isc_add_user := Tisc_add_user(GetProcedurePointer(hDLL, 'isc_add_user'));
    isc_delete_user := Tisc_delete_user(GetProcedurePointer(hDLL, 'isc_delete_user'));
    isc_modify_user := Tisc_modify_user(GetProcedurePointer(hDLL, 'isc_modify_user'));
    isc_rollback_retaining := Tisc_rollback_retaining(TryGetProcedurePointer(hDLL, 'isc_rollback_retaining'));
    if Assigned(isc_rollback_retaining) then
    begin
      _ClientVersion := 6;
      isc_decode_sql_date := Tisc_decode_sql_date(GetProcedurePointer(hDLL, 'isc_decode_sql_date'));
      isc_decode_sql_time := Tisc_decode_sql_time(GetProcedurePointer(hDLL, 'isc_decode_sql_time'));
      isc_decode_timestamp := Tisc_decode_timestamp(GetProcedurePointer(hDLL, 'isc_decode_timestamp'));
      isc_encode_sql_date := Tisc_encode_sql_date(GetProcedurePointer(hDLL, 'isc_encode_sql_date'));
      isc_encode_sql_time := Tisc_encode_sql_time(GetProcedurePointer(hDLL, 'isc_encode_sql_time'));
      isc_encode_timestamp := Tisc_encode_timestamp(GetProcedurePointer(hDLL, 'isc_encode_timestamp'));
      isc_service_attach := Tisc_service_attach(GetProcedurePointer(hDLL, 'isc_service_attach'));
      isc_service_detach := Tisc_service_detach(GetProcedurePointer(hDLL, 'isc_service_detach'));
      isc_service_query := Tisc_service_query(GetProcedurePointer(hDLL, 'isc_service_query'));
      isc_service_start := Tisc_service_start(GetProcedurePointer(hDLL, 'isc_service_start'));
    end
    else
    begin
      _ClientVersion := 5;
      {$IFDEF FBL_IB5}
      isc_rollback_retaining := isc_rollback_retaining_stub;
      isc_decode_sql_date := isc_decode_sql_date_stub;
      isc_decode_sql_time := isc_decode_sql_time_stub;
      isc_decode_timestamp := isc_decode_timestamp_stub;
      isc_encode_sql_date := isc_encode_sql_date_stub;
      isc_encode_sql_time := isc_encode_sql_time_stub;
      isc_encode_timestamp := isc_encode_timestamp_stub;
      {$ENDIF}
    end;

    isc_get_client_version := Tisc_get_client_version(TryGetProcedurePointer(hDLL, 'isc_get_client_version'));
    if Assigned(isc_get_client_version) then
    begin
      _ClientVersion := 7;
      isc_get_client_major_version := Tisc_get_client_major_version(GetProcedurePointer(hDLL,
        'isc_get_client_major_version'));
      isc_get_client_minor_version := Tisc_get_client_minor_version(GetProcedurePointer(hDLL,
        'isc_get_client_minor_version'));
    end;
    Result := True;
  end
  else
    //raise Exception.Create('Library ' + DLL + ' not found');
    Result := False;
end;

//------------------------------------------------------------------------------

function XSQLDA_LENGTH(Value: longint): longint;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

//------------------------------------------------------------------------------

{Service manager function}

procedure add_spb_length(var p: PChar; ALength: integer);
begin
  p^ := char(ALength);
  Inc(p);
  p^ := char(ALength shr 8);
  Inc(p);
end;

//------------------------------------------------------------------------------

procedure add_spb_numeric(var p: PChar; AData: integer);
begin
  p^ := char(AData);
  Inc(p);
  p^ := char(AData shr 8);
  Inc(p);
  p^ := char(AData shr 16);
  Inc(p);
  p^ := char(AData shr 24);
  Inc(p);
end;


{$IFDEF FBL_IB5}
{***************** Stub Functions ***************}

function isc_rollback_retaining_stub(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS;
  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_rollback_retaining']);
end;

procedure isc_encode_sql_date_stub(tm_date: PCTimeStructure;
  ib_date: PISC_DATE); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_date']);
end;

procedure isc_encode_sql_time_stub(tm_date: PCTimeStructure;
  ib_time: PISC_TIME); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_time']);
end;

procedure isc_encode_timestamp_stub(tm_date: PCTimeStructure;
  ib_timestamp: PISC_TIMESTAMP); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_timestamp']);
end;

procedure isc_decode_sql_date_stub(ib_date: PISC_DATE;
  tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_sql_date']);
end;

procedure isc_decode_sql_time_stub(ib_time: PISC_TIME;
  tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_sql_time']);
end;

procedure isc_decode_timestamp_stub(ib_timestamp: PISC_TIMESTAMP;
  tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_timestamp']);
end;
{$ENDIF}

initialization
  {$IFDEF UNIX}
  hCryptDll := LibHandleNil;
  {$ENDIF}
  hDLL := LibHandleNil;
  _ClientVersion := 0;

finalization
  {$IFDEF UNIX}
  {$IFNDEF FPC}
  if hCryptDLL <> LibHandleNil then
  begin
    dlclose(hCryptDLL);
    hCRyptDll := LibHandleNil;
  end;
  {$ENDIF}
  {$ELSE}
  if (hDLL <> LibHandleNil) and LibLoaded then
    FreeLibrary(hDLL);
  {$ENDIF}
end.
