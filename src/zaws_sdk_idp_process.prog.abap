*&---------------------------------------------------------------------*
*& Include          ZSDK_DEMO_INVOICE_PROCESS
*&---------------------------------------------------------------------*

INITIALIZATION.
  PERFORM initialize_fkey.
  PERFORM initialize_profiles.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_file USING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_key.
  PERFORM list_bucket_objects USING p_bucket.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bucket.
  PERFORM list_buckets USING p_bucket.

AT SELECTION-SCREEN ON p_pfl.
  PERFORM validate_profile.

AT SELECTION-SCREEN.
  PERFORM handle_ucomm.

START-OF-SELECTION.
