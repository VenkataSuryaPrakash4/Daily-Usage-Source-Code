*&---------------------------------------------------------------------*
*& Include          ZRFI_OPEN_CUSTO_BALNCE_SEL
*&---------------------------------------------------------------------*
TABLES:acdoca.
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:s_kunnr FOR acdoca-kunnr NO-EXTENSION,
                 s_belnr FOR acdoca-belnr.

  PARAMETERS:p_bukrs TYPE acdoca-rbukrs DEFAULT 'S4HA',
             p_gjahr TYPE acdoca-gjahr DEFAULT '2021',
             p_key   TYPE acdoca-budat.
SELECTION-SCREEN:END OF BLOCK b1.

SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:ch_edit AS CHECKBOX.

  SELECTION-SCREEN SKIP 1.
  PARAMETERS:rb_alv   RADIOBUTTON GROUP rep,
             rb_class RADIOBUTTON GROUP rep,
             rb_block RADIOBUTTON GROUP rep,
             rb_hier  RADIOBUTTON GROUP rep.

  SELECTION-SCREEN SKIP 1.
  PARAMETERS: rb_det RADIOBUTTON GROUP typ USER-COMMAND usr,
              rb_sum RADIOBUTTON GROUP typ.
SELECTION-SCREEN:END OF BLOCK b2.

SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN:BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(10) TEXT-003 FOR FIELD p_buc1 MODIF ID buc.
    PARAMETERS:p_buc1 TYPE n LENGTH 3 MODIF ID buc.
    SELECTION-SCREEN POSITION 18.
    PARAMETERS:p_buc2 TYPE n LENGTH 3 MODIF ID buc.
    SELECTION-SCREEN POSITION 25.
    PARAMETERS:p_buc3 TYPE n LENGTH 3 MODIF ID buc.
  SELECTION-SCREEN:END OF LINE.
SELECTION-SCREEN:END OF BLOCK b3.
