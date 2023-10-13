*&---------------------------------------------------------------------*
*& Include          ZRFI_OPEN_CUSTO_BALNCE_GD
*&---------------------------------------------------------------------*
DATA:lv_date     TYPE sy-datum,
     lv_ext_date TYPE char10.

TYPES:BEGIN OF ty_final,
        icon  TYPE c,
        bukrs TYPE bukrs,
        belnr TYPE belnr,
        gjahr TYPE gjahr,
        kunnr TYPE kunnr,
        budat TYPE char10,
        bldat TYPE char10,
        tsl   TYPE dmbtr,
      END OF ty_final,

      BEGIN OF ty_acdoca,
        rclnt type MANDT,
        rldnr type FINS_LEDGER,
        rbukrs type bukrs,
        gjahr type gjahr,
        belnr type BELNR_D,
        docln type DOCLN6,
        tsl TYPE FINS_VTCUR12,
        budat type budat,
        bldat type bldat,
        kunnr type kunnr,
      END OF ty_acdoca.

DATA:lv_bukrs  TYPE bapi3007_1-comp_code,
     lv_kunnr  TYPE bapi3007_1-customer,
     lv_key    TYPE bapi3007-key_date,
     lt_acdoca type TABLE of ty_acdoca,
     lt_items  TYPE TABLE OF bapi3007_2,
     lt_final  TYPE TABLE OF ty_final,
     lt_fcat   TYPE slis_t_fieldcat_alv,
     lt_sort   TYPE slis_t_sortinfo_alv,
     lt_filter TYPE slis_t_filter_alv,
     wa_layout TYPE slis_layout_alv,
     wa_final  TYPE ty_final.
