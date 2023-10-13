*&---------------------------------------------------------------------*
*& Report ZRFI_CUST_OPEN_DETAILS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrfi_cust_open_details.
TYPE-POOLS:slis.
TABLES:acdoca.

INCLUDE zrfi_cust_open_details_gd.  "Global Declarations
INCLUDE zrfi_cust_open_details_ss.  "Selection Screen
INCLUDE zrfi_cust_open_details_cd.  "Business Logic

INITIALIZATION.
  PERFORM clear.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_validation.

AT SELECTION-SCREEN.
  PERFORM validate_inputs.

AT SELECTION-SCREEN ON p_bukrs.
  PERFORM validate_inputs.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_help.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM display_data.

END-OF-SELECTION.

TOP-OF-PAGE.
  WRITE:/ 'Program :', sy-repid.
  WRITE:/ 'Date :', sy-datum.
  ULINE.

END-OF-PAGE.
  WRITE:/ '*---------- End of Report ----------*'.
