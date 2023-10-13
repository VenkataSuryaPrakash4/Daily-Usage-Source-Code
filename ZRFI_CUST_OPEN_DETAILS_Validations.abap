*&---------------------------------------------------------------------*
*& Include          ZRFI_OPEN_CUSTO_BALNCE_CD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form hide_buket
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM hide_buket .
  LOOP AT SCREEN INTO DATA(wa_screen).
    IF wa_screen-group1 = 'BUC' AND
       rb_sum = abap_true.
      wa_screen-active = 0.
    ENDIF.
    MODIFY SCREEN FROM wa_screen.
    CLEAR:screen.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data.
  SELECT rclnt,rldnr,rbukrs,gjahr,belnr,docln,tsl,
           budat,bldat,kunnr
      FROM acdoca
      INTO TABLE @lt_acdoca
      WHERE rbukrs = @p_bukrs AND
            gjahr = @p_gjahr AND
            belnr IN @s_belnr AND
            kunnr IN @s_kunnr AND
            koart = 'D'.
  IF sy-subrc = 0.
    DELETE lt_acdoca WHERE kunnr IS INITIAL.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data .
  LOOP AT lt_acdoca INTO DATA(wa_acdoca).
    lv_bukrs = wa_acdoca-rbukrs.
    lv_kunnr = wa_acdoca-kunnr.
    IF p_key IS NOT INITIAL.
      lv_key = p_key.
    ELSE.
      lv_key = sy-datum.
    ENDIF.
    CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
      EXPORTING
        companycode = lv_bukrs
        customer    = lv_kunnr
        keydate     = lv_key
      TABLES
        lineitems   = lt_items.

    READ TABLE lt_items WITH KEY comp_code = wa_acdoca-rbukrs
                                 customer = wa_acdoca-kunnr
                                 fisc_year = wa_acdoca-gjahr
                                 doc_no = wa_acdoca-belnr
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CLEAR:lv_bukrs,lv_kunnr,lv_key,wa_acdoca.
      CONTINUE.
    ELSE.
      wa_final-bukrs = wa_acdoca-rbukrs.
      wa_final-gjahr = wa_acdoca-gjahr.
      wa_final-belnr = wa_acdoca-belnr.
      wa_final-kunnr = wa_acdoca-kunnr.

      lv_date = wa_acdoca-budat.
      PERFORM convert_dates USING lv_date
                            CHANGING lv_ext_date.
      wa_final-budat = lv_ext_date.
      CLEAR:lv_date,lv_ext_date.

      lv_date = wa_acdoca-bldat.
      PERFORM convert_dates USING lv_date
                    CHANGING lv_ext_date.
      wa_final-bldat = lv_ext_date.
      wa_final-tsl = wa_acdoca-tsl.

      IF wa_final-tsl LE 0.
        wa_final-icon = 1.
      ELSEIF wa_final-tsl GT 0 AND wa_final-tsl LE 2000.
        wa_final-icon = 2.
      ELSE.
        wa_final-icon = 3.
      ENDIF.

      APPEND wa_final TO lt_final.
    ENDIF.
    CLEAR:lv_bukrs,lv_kunnr,lv_key,wa_acdoca,wa_final,lv_date,lv_ext_date.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display .
*---Layout
  wa_layout-zebra = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  wa_layout-lights_fieldname = 'ICON'.

*---Field Cat
  lt_fcat = VALUE #( ( col_pos = 1 fieldname = 'BUKRS' outputlen = 5 seltext_s = 'Comp Code' )
                     ( col_pos = 2 fieldname = 'BELNR' outputlen = 10 seltext_s = 'Doc Num' )
                     ( col_pos = 3 fieldname = 'GJAHR' outputlen = 5 seltext_s = 'Year' )
                     ( col_pos = 4 fieldname = 'KUNNR' outputlen = 10 seltext_s = 'Cust Num' no_zero = 'X' )
                     ( col_pos = 5 fieldname = 'BUDAT' outputlen = 10 seltext_s = 'Post Date' )
                     ( col_pos = 6 fieldname = 'BLDAT' outputlen = 10 seltext_s = 'Doc Date' )
                     ( col_pos = 7 fieldname = 'TSL' outputlen = 15 seltext_s = 'Amount' do_sum = 'X' ) ).

*---Sort
*  APPEND VALUE #( fieldname = 'BUKRS' tabname = 'LT_FINAL' up = 'X' ) TO lt_sort.subtot
  lt_sort = VALUE #( ( fieldname = 'BUKRS' tabname = 'LT_FINAL' up = 'X' )
                     ( fieldname = 'BELNR' tabname = 'LT_FINAL' subtot = 'X' ) ).

  DATA:lt_events TYPE slis_t_event.
*---Events
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 4
    IMPORTING
      et_events       = lt_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    DATA(wa_events) = lt_events[ 3 ].
    wa_events-form = 'TOP_OF_PAGE'.
    MODIFY lt_events FROM wa_events INDEX 3.
  ENDIF.

*---Filter
  APPEND VALUE #( fieldname = 'BUDAT' tabname = 'LT_FINAL' sign0 = 'I'
                  optio = 'EQ' valuf_int = '25.09.2021' ) TO lt_filter.

*---Calling FM to display output
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZPF_STAT_PDF'
      i_callback_user_command  = 'USR_CMD'
      is_layout                = wa_layout
      it_fieldcat              = lt_fcat
      it_sort                  = lt_sort
      it_filter                = lt_filter
      i_save                   = 'A'
      it_events                = lt_events
    TABLES
      t_outtab                 = lt_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_dates
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      <-- LV_EXT_DATE
*&---------------------------------------------------------------------*
FORM convert_dates  USING    VALUE(p_lv_date)
                    CHANGING p_lv_ext_date.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = p_lv_date
    IMPORTING
      date_external            = p_lv_ext_date
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
ENDFORM.

FORM top_of_page.
  DATA:lt_head TYPE slis_t_listheader.

  lv_date = sy-datum.
  PERFORM convert_dates USING lv_date
                CHANGING lv_ext_date.

  lt_head = VALUE #( ( typ = 'H' info = 'Customer open balance' )
                     ( typ = 'S' key = 'Program name :' info = sy-repid )
                     ( typ = 'S' key = 'Date :' info = lv_ext_date )
                     ( typ = 'S' key = 'Company code :' info = p_bukrs )
                     ( typ = 'S' key = 'Year :' info = p_gjahr )
                     ( typ = 'A' info = sy-uname ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_head
      i_logo             = 'ALVLOG'.
  CLEAR:lv_date.
ENDFORM.

FORM zpf_stat_pdf USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZPF_STAT_PDF'.
ENDFORM.

FORM usr_cmd USING r_ucomm LIKE sy-ucomm
                   rs_selfield TYPE slis_selfield.
  CONSTANTS:c_slash TYPE c VALUE '\'.
  DATA:lt_params   TYPE TABLE OF rsparams,
       wa_params   TYPE rsparams,
       lv_desktop  TYPE string,
       lv_spoolid  TYPE rspoid,
       lv_sym      TYPE char10,
       lv_name     TYPE tbtcjob-jobname VALUE sy-repid,
       lv_count    TYPE tbtcjob-jobcount,
       lv_status   TYPE tbtco-status,
       lv_authname TYPE tbtcjob-authcknam.

  IF r_ucomm = '&PDF'.
    lv_authname  = sy-uname.
*Create BG programatically

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_name
      IMPORTING
        jobcount         = lv_count
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
      CALL FUNCTION 'JOB_SUBMIT'
        EXPORTING
          authcknam               = lv_authname
          jobcount                = lv_count
          jobname                 = lv_name
          language                = sy-langu
          report                  = sy-repid
        EXCEPTIONS
          bad_priparams           = 1
          bad_xpgflags            = 2
          invalid_jobdata         = 3
          jobname_missing         = 4
          job_notex               = 5
          job_submit_failed       = 6
          lock_failed             = 7
          program_missing         = 8
          prog_abap_and_extpg_set = 9
          OTHERS                  = 10.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_count
          jobname              = lv_name
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.
    ENDIF.
    DO 100 TIMES.
      IF lv_status NE 'F'.
        CALL FUNCTION 'BDL_READ_JOB_STATUS'
          EXPORTING
            jobname       = lv_name
            jobnumber     = lv_count
          IMPORTING
            jobstatus     = lv_status
          EXCEPTIONS
            job_not_found = 1
            OTHERS        = 2.
      ELSE. "STATUS = 'F'
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_status = 'F'.
*---Find spool number
      DATA(lv_lng) = strlen( sy-repid ).

      IF lv_lng GE 9.
        DATA(lv_sufix) = |{ sy-repid+0(9) }| & |{ sy-uname+0(3) }|.
        CONDENSE lv_sufix NO-GAPS.
      ELSE.
        DATA(lv_diff) = 9 - lv_lng.
        DO lv_diff TIMES."______
          lv_sym = |{ lv_sym }| & |_| .
        ENDDO.
        lv_sufix = |{ sy-repid }| & |{ lv_sym }| & |{ sy-uname+0(3) }|.
        CONDENSE lv_sufix NO-GAPS.
      ENDIF.

      IF lv_sufix IS NOT INITIAL.
        SELECT rqident
          FROM tsp01
          INTO TABLE @DATA(lt_sp01)
          WHERE rq2name = @lv_sufix.
        IF sy-subrc = 0.
          SORT lt_sp01 BY rqident DESCENDING.
          DATA(wa_spool) = lt_sp01[ 1 ].
          lv_spoolid = wa_spool-rqident.
        ENDIF.
      ENDIF.

*---Find desktop path name
      CALL METHOD cl_gui_frontend_services=>get_desktop_directory
        CHANGING
          desktop_directory    = lv_desktop
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc = 0.
        CALL METHOD cl_gui_cfw=>update_view
          EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2
            OTHERS            = 3.
      ENDIF.
*C:\Users\S20abaph54\Desktop\cust_bal_20221226.pdf
      DATA(lv_path) =  |{ lv_desktop }| & |{ c_slash }| & |cust_bal_| & |{ sy-datum }| & |.pdf|.
      CONDENSE lv_path NO-GAPS.
*---Filling report selection screen inputs via interna table
      lt_params =  VALUE #( ( selname = 'SPOOLNO' sign = 'I' option = 'EQ' low = lv_spoolid high = ' ' )
                            ( selname = 'P_FILE' sign = 'I' option = 'EQ' low = lv_path high = ' ' ) ).

*---Calling report to download pdf
      SUBMIT rstxpdft4 WITH SELECTION-TABLE lt_params AND RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_inputs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_inputs .
  DATA:lv_msg TYPE char30.
  SELECT SINGLE bukrs
         FROM t001
         INTO @DATA(wa_bukrs)
    WHERE bukrs = @p_bukrs.
  IF sy-subrc NE 0.
    lv_msg = TEXT-e01.
    REPLACE ALL OCCURRENCES OF '&' IN lv_msg WITH p_bukrs.
    MESSAGE lv_msg TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear .
Clear:lt_acdoca.
ENDFORM.
