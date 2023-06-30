*&---------------------------------------------------------------------*
*& Include          ZSDK_DEMO_TEXTRACT_F01
*&---------------------------------------------------------------------*
FORM get_file USING pv_file.
  DATA: lv_file TYPE ibipparms-path.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = lv_file.

  pv_file = lv_file.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form upload_pdf_file
*&---------------------------------------------------------------------*
*& upload PDF file into local table
*&---------------------------------------------------------------------*
*&      --> P_FILE
*&---------------------------------------------------------------------*
FORM upload_pdf_file  USING  pv_file TYPE string.

  DATA: lv_len     TYPE i.

  CLEAR: gt_rawtab, gv_xstring.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = pv_file
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_len
    CHANGING
      data_tab                = gt_rawtab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv2 sy-msgv4.
  ENDIF.

* convert binary data PDF-stream
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_len
    IMPORTING
      buffer       = gv_xstring
    TABLES
      binary_tab   = gt_rawtab
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form load_pdf_file_s3
*&---------------------------------------------------------------------*
*& load pdf content to S3 for textract processing
*&---------------------------------------------------------------------*
FORM load_pdf_file_s3 .
  PERFORM get_key USING p_file
                  CHANGING p_key.

  IF p_bucket IS INITIAL OR p_bucket EQ TEXT-003.
    MESSAGE e016(rp) WITH |Enter a valid bucket name|.
  ENDIF.

  DATA(lo_s3) = NEW zcl_aws_s3_helper( im_profile = p_pfl ).

  TRY.
      lo_s3->go_s3_client->putobject(
                 iv_bucket = p_bucket
                 iv_key =  p_key
                 iv_body = gv_xstring  " pdf content as xstring
             ).

      MESSAGE s016(rp) WITH TEXT-m06.

    CATCH /aws1/cx_s3_nosuchbucket.
      MESSAGE i016(rp) WITH TEXT-m05.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form textanalyze_using_textract
*&---------------------------------------------------------------------*
*& Perfrom text analysis using textract
*&---------------------------------------------------------------------*
FORM textanalyze_using_textract USING p_post_fi.

  DATA: lt_tables TYPE  zcl_aws_textract_helper=>tt_table.
  DATA(lo_textract) = NEW zcl_aws_textract_helper( iv_profile = p_pfl ).

  TRY.
      DATA(lv_jobid) = lo_textract->analyze_document_asynchronous(
          iv_bucket = p_bucket
          iv_key    = p_key
      ).

      MESSAGE i016(rp) WITH |Document Analysis started| |Mode Asynchronous|.

    CATCH /aws1/cx_texaccessdeniedex.
    CATCH /aws1/cx_texbaddocumentex.
    CATCH /aws1/cx_texdocumenttoolargeex.
    CATCH /aws1/cx_texidempotentprmmis00.
    CATCH /aws1/cx_texinternalservererr.
    CATCH /aws1/cx_texinvalidkmskeyex.
    CATCH /aws1/cx_texinvalids3objectex.
    CATCH /aws1/cx_texlimitexceededex.
    CATCH /aws1/cx_texprovthruputexcdex.
    CATCH /aws1/cx_texthrottlingex.
    CATCH /aws1/cx_texunsupporteddocex.
    CATCH /aws1/cx_texclientexc.
    CATCH /aws1/cx_texserverexc.
    CATCH /aws1/cx_rt_technical_generic.
    CATCH /aws1/cx_rt_service_generic.
  ENDTRY.

  DATA(lv_status) = lo_textract->get_document_results_asynch( im_jobid = lv_jobid ).

  CHECK lv_status = lo_textract->co_status.

  lt_tables = lo_textract->get_table_data( ).

*--- business logic / mapping example customize to your needs ---------------------*
  PERFORM map_table_data_bapi USING lt_tables
                                    'table_1'
                                     abap_true.

  PERFORM map_table_data_bapi USING lt_tables
                                    'table_2'
                                     abap_false.

* Post Invoice document via FB60
  PERFORM post_fb60_invoice.
ENDFORM.

FORM map_table_data_bapi USING pt_tables TYPE zcl_aws_textract_helper=>tt_table
                                  pv_tabid TYPE string
                                  pv_no_header.

  DATA:  lt_table      TYPE zcl_aws_textract_helper=>tt_table.
  DATA:  lo_transposer TYPE REF TO zif_transposer.
  DATA:  lt_data       TYPE zif_transposer=>tt_rows.

  FIELD-SYMBOLS: <fs_tab> TYPE STANDARD TABLE.

  lt_table = VALUE #( FOR ls_table IN pt_tables
                        WHERE ( table_id = pv_tabid ) ( ls_table ) ).


  lt_data = CORRESPONDING #( lt_table ).

  CREATE OBJECT lo_transposer TYPE zcl_transpose_columns
    EXPORTING
      it_data = lt_data.

  DATA(lt_trans_data) = lo_transposer->transpose( iv_no_header  = conv boole( pv_no_header ) ).

  ASSIGN lt_trans_data->* TO <fs_tab>.

  CASE  pv_tabid .
    WHEN 'table_1'.
      gt_invmap = CORRESPONDING #( <fs_tab> ).
    WHEN 'table_2'.
      gt_invitems = CORRESPONDING #( <fs_tab> ).
    WHEN OTHERS.
  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*

*& Form list_buckets

*&---------------------------------------------------------------------*

FORM list_buckets USING p_bucket TYPE string.


  TYPES: BEGIN OF ty_bucketname,

           bucket TYPE  /aws1/s3_bucketname,
         END OF ty_bucketname.

  DATA: lt_bucketnames TYPE TABLE OF ty_bucketname.
  DATA: lt_return      TYPE TABLE OF ddshretval.
  DATA(lo_s3) = NEW zcl_aws_s3_helper( im_profile = p_pfl ).

  TRY.
      lt_bucketnames = lo_s3->list_buckets( ).
    CATCH /aws1/cx_s3_clientexc INTO DATA(lo_client_exception).
  ENDTRY.

* F4 help list buckets
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'BUCKET'
      window_title = 'Buckets'
      value_org    = 'S'
    TABLES
      value_tab    = lt_bucketnames
      return_tab   = lt_return.

  IF lt_return[] IS NOT INITIAL.
    p_bucket = lt_return[ 1 ]-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form initialize_fkey
*&---------------------------------------------------------------------*
FORM initialize_fkey .

  MOVE: TEXT-012 TO bt2,
        TEXT-013 TO bt3,
*        TEXT-022 TO bt5,
        TEXT-033 TO bt6,
        TEXT-037 TO bt7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form list_bucket_objects
*&---------------------------------------------------------------------*
FORM list_bucket_objects  USING    p_bucket TYPE string.

  TYPES: BEGIN OF ty_content,
           key          TYPE /aws1/s3_objectkey,
           lastmodified TYPE /aws1/s3_lastmodified,
           size         TYPE /aws1/s3_size,
         END OF ty_content.

  DATA: lt_objects TYPE zcl_aws_s3_helper=>tt_content.
  DATA: lt_return  TYPE TABLE OF ddshretval.


  IF p_bucket IS INITIAL.
    MESSAGE i016(rp) WITH | Enter a bucket name |.
  ENDIF.

  DATA(lo_s3) = NEW  zcl_aws_s3_helper( im_profile = p_pfl ).

  TRY.
      CALL METHOD lo_s3->list_bucket_objects
        EXPORTING
          im_bucket   = p_bucket
        RECEIVING
          re_contents = lt_objects.

    CATCH /aws1/cx_s3_nosuchbucket.
      MESSAGE i016(rp) WITH | Bucket not found |.
    CATCH /aws1/cx_s3_clientexc INTO DATA(lo_client_ex).
      MESSAGE i016(rp) WITH |{ lo_client_ex->av_err_msg }|.
  ENDTRY.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'KEY'
      window_title = 'Objects'
      value_org    = 'S'
    TABLES
      value_tab    = lt_objects
      return_tab   = lt_return.

  IF lt_return[] IS NOT INITIAL.
    p_key = lt_return[ 1 ]-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_ucomm
*&---------------------------------------------------------------------*
*& Handle user commands
*&---------------------------------------------------------------------*
FORM handle_ucomm .
  CASE sy-ucomm.
    WHEN 'UC1'.
      PERFORM list_bucket_objects USING p_bucket.
    WHEN 'UC2'.
      PERFORM upload_pdf_file USING p_file.
      PERFORM load_pdf_file_s3.
    WHEN 'UC3'.
      PERFORM create_s3_bucket.
    WHEN 'CRET'.
      PERFORM textanalyze_using_textract USING abap_true.
    WHEN 'UC6'.
      CALL SELECTION-SCREEN 100.
    WHEN 'UC7'.
      PERFORM subscribe_to_sns.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form subscribe to sns
*&---------------------------------------------------------------------*
FORM subscribe_to_sns.

  DATA: lv_pfl TYPE /aws1/rt_pflh-profile_id.

  PERFORM get_profile_id CHANGING lv_pfl.

  DATA(lo_session) = /aws1/cl_rt_session_aws=>create(
                                             iv_profile_id =  lv_pfl  ).
  DATA(lo_sns)     = /aws1/cl_sns_factory=>create( lo_session ).
  DATA(lt_topics) = lo_sns->listtopics( )->get_topics( ).

  LOOP AT lt_topics INTO DATA(ls_topic).

    DATA(lt_attributes) =  lo_sns->gettopicattributes(
          iv_topicarn =  ls_topic->get_topicarn( )
      )->get_attributes( ).

* Create a subscription to the topic
    READ TABLE lt_attributes WITH KEY key = 'TopicArn'
           INTO DATA(ls_attributes).
    IF sy-subrc = 0.
      IF ls_attributes-value->get_value( ) CS 'sap-ap-notifications'.
        lo_sns->subscribe(
          EXPORTING
            iv_topicarn              = ls_attributes-value->get_value( )
             iv_protocol              = 'Email'
             iv_endpoint              = p_email

        ).
        MESSAGE s016(rp) WITH |Subscription was created sucessfully|.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_s3_bucket
*&---------------------------------------------------------------------*
FORM create_s3_bucket.

  DATA(lo_s3) = NEW zcl_aws_s3_helper( im_profile = p_pfl ).

  TRY.
      lo_s3->go_s3_client->createbucket(
        EXPORTING
            iv_bucket                    = p_bucket
       RECEIVING
          oo_output                    = DATA(lo_bucket)
      ).

      MESSAGE s016(rp) WITH TEXT-m04.
    CATCH /aws1/cx_s3_bucketalrdyexists.  " BucketAlreadyExists
      MESSAGE i016(rp) WITH TEXT-m01.
    CATCH /aws1/cx_s3_bktalrdyownedbyyou. " BucketAlreadyOwnedByYou
      MESSAGE i016(rp) WITH TEXT-m02.
    CATCH /aws1/cx_s3_clientexc INTO DATA(lo_client_exception).
      " Exception /AWS1/CX_S3_CLIENTEXC
      MESSAGE i016(rp) WITH TEXT-m03 lo_client_exception->av_http_code.
    CATCH /aws1/cx_s3_serverexc.
      " Exception /AWS1/CX_S3_SERVEREXC
    CATCH /aws1/cx_rt_technical_generic.  " Technical errors
    CATCH /aws1/cx_rt_service_generic.    " Generic Service call error
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_key
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_key  USING    pv_file
              CHANGING cv_key.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = pv_file
    IMPORTING
      stripped_name = cv_key
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.

ENDFORM.

FORM invoice_notification USING lv_msg TYPE string
                                lv_sub TYPE string.

  DATA(lo_session) = /aws1/cl_rt_session_aws=>create(
                                             iv_profile_id =  p_pfl  ).

  DATA(lo_sns)     = /aws1/cl_sns_factory=>create( lo_session ).
  DATA(lt_topics) = lo_sns->listtopics( )->get_topics( ).

  LOOP AT lt_topics INTO DATA(ls_topic).

    DATA(lt_attributes) =  lo_sns->gettopicattributes(
          iv_topicarn =  ls_topic->get_topicarn( )
      )->get_attributes( ).
* Create a subscription to the topic
    READ TABLE lt_attributes WITH KEY key = 'TopicArn'
           INTO DATA(ls_attributes).
    IF sy-subrc = 0.
      IF ls_attributes-value->get_value( ) CS 'sap-ap-notifications'.

        lo_sns->publish(
            iv_topicarn               = ls_attributes-value->get_value( )
*           iv_targetarn              =
*           iv_phonenumber            =
            iv_message                 = lv_msg
            iv_subject                 = lv_sub
        ).

      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM save_item_text USING     pv_key TYPE bapiache09-obj_key
                               pv_item TYPE n
                               pv_desc TYPE string
                               pv_langu TYPE string.

  DATA: ls_head TYPE thead.
  DATA: lv_desc TYPE string.
  DATA: lt_lines TYPE TABLE OF tline.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input            = pv_langu
    IMPORTING
      output           = ls_head-tdspras
    EXCEPTIONS
      unknown_language = 1
      OTHERS           = 2.

  ls_head-tdname = |{ p_bukrs }|    &&
                   |{ pv_key(10) }| &&
                   |{ p_gjahr }|    &&
                   |{ pv_item }|.

  ls_head-tdobject = 'DOC_ITEM'.
  ls_head-tdid     = '0001'.

  lv_desc = pv_desc.



  IF strlen( lv_desc ) <= 132.
    APPEND INITIAL LINE TO lt_lines
        ASSIGNING FIELD-SYMBOL(<fs_line>).
    <fs_line>-tdformat = '*'.
    <fs_line>-tdline = lv_desc.
  ELSE.
    DO.
      APPEND INITIAL LINE TO lt_lines
      ASSIGNING <fs_line>.
      IF sy-index = 1.
        <fs_line>-tdformat = '*'.
      ELSE.
        <fs_line>-tdformat = '='.
      ENDIF.

      IF strlen( lv_desc ) > 132.
        <fs_line>-tdline = lv_desc(132).
        lv_desc = lv_desc+132.
      ELSE.
        <fs_line>-tdline = lv_desc.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.



  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = ls_head
      insert          = 'I'
      savemode_direct = 'X'
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.
  .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_fb60_invoice
*&---------------------------------------------------------------------*
*& Post FB60 invoice
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM post_fb60_invoice .
  DATA: ls_header       TYPE           bapiache09.
  DATA: lt_accountgl    TYPE TABLE OF  bapiacgl09.
  DATA: lt_vendoritem   TYPE TABLE OF  bapiacap09.
  DATA: lt_amount       TYPE TABLE OF  bapiaccr09.

  DATA: lv_vendor_total TYPE bapidoccur.
  DATA: lv_posnr        TYPE posnr_acc.
  DATA: lv_key          TYPE bapiache09-obj_key.
  DATA: lt_return       TYPE TABLE OF bapiret2.

  DATA: lv_trans_desc   TYPE string.
  DATA: lv_item(3)      TYPE n.
  DATA: lv_msg     TYPE string,
        lv_subject TYPE string.

  ls_header-comp_code = p_bukrs.
  ls_header-doc_type  = p_docty.
  ls_header-username  = sy-uname.
  ls_header-ref_doc_no = gt_invmap[ key = 'Invoice Number' ]-value.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = gt_invmap[ key = 'Invoice Date' ]-value
    IMPORTING
      date_internal            = ls_header-doc_date
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.

  ls_header-pstng_date = sy-datum.
*--------------------------------------------------------------------------
  DELETE gt_invitems WHERE description IS INITIAL.

  LOOP AT gt_invitems ASSIGNING FIELD-SYMBOL(<fs_item>).
    REPLACE ALL OCCURRENCES OF '$' IN <fs_item>-vat WITH space.
    CONDENSE  <fs_item>-vat.
    REPLACE ALL OCCURRENCES OF '$' IN <fs_item>-total WITH space.
    CONDENSE  <fs_item>-total.

    lv_posnr = lv_posnr + 1.

    SPLIT <fs_item>-description AT '-' INTO TABLE DATA(lt_desc).

    APPEND INITIAL LINE TO  lt_accountgl ASSIGNING FIELD-SYMBOL(<fs_gl>).
    <fs_gl>-itemno_acc = lv_posnr.
    <fs_gl>-gl_account = p_hkont.
    <fs_gl>-item_text  = lt_desc[ 1 ].

    APPEND INITIAL LINE TO  lt_amount ASSIGNING FIELD-SYMBOL(<fs_amt>).
    <fs_amt>-itemno_acc =   lv_posnr.
    <fs_amt>-currency   = 'USD'.
    <fs_amt>-amt_doccur = CONV bapidoccur( <fs_item>-vat ).
    <fs_amt>-amt_doccur = <fs_amt>-amt_doccur + CONV bapidoccur( <fs_item>-total ).
    lv_vendor_total = lv_vendor_total + <fs_amt>-amt_doccur.


  ENDLOOP.


* Vendor item
  lv_posnr = lv_posnr + 1.

  APPEND INITIAL LINE TO  lt_vendoritem ASSIGNING FIELD-SYMBOL(<fs_vendor>).
  <fs_vendor>-itemno_acc = lv_posnr.
  <fs_vendor>-vendor_no  = gt_invmap[ key = 'Account Number' ]-value.

* Credit entry vendor
  APPEND INITIAL LINE TO  lt_amount ASSIGNING <fs_amt>.
  <fs_amt>-itemno_acc =   lv_posnr.
  <fs_amt>-currency   =   'USD'.
  <fs_amt>-amt_doccur =   lv_vendor_total.
  <fs_amt>-amt_doccur =   <fs_amt>-amt_doccur * -1.

* Invoice Init
  CALL FUNCTION 'FI_DUPLICATE_INVOICE_INIT'.

* check duplicate invoices before posting
  CALL FUNCTION 'FI_DUPLICATE_INVOICE_CHECK'
    EXPORTING
      i_bukrs       = p_bukrs
      i_lifnr       = <fs_vendor>-vendor_no
      i_waers       = 'USD'
      i_bldat       = ls_header-doc_date
      i_xblnr       = CONV bkpf-xblnr( ls_header-ref_doc_no )
      i_wrbtr       = lv_vendor_total
      i_koart       = 'K'
      i_reprf       = 'X'
      i_shkzg       = 'H'
      i_xumsw       = 'X'
      i_bstat       = ''
    EXCEPTIONS
      missing_data  = 1
      error_message = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    lv_msg = |Duplicate posting found for the invoice:{ ls_header-ref_doc_no }|.
    lv_subject = |Duplicate invoice detected|.
    PERFORM invoice_notification USING lv_msg lv_subject.

    MESSAGE e016(rp) WITH |Duplicate posting found|
                          |for invoice:{ ls_header-ref_doc_no }|.
  ENDIF.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_header
    IMPORTING
      obj_key        = lv_key               " Reference key
    TABLES
      accountgl      = lt_accountgl         " G/L account item
      accountpayable = lt_vendoritem        " Vendor Item
      currencyamount = lt_amount
      return         = lt_return.           " Return parameter



  IF line_exists( lt_return[ type = 'E' ] ).
    MESSAGE e016(rp) WITH lt_return[ type = 'E' ]-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    MESSAGE i016(rp) WITH |Document { lv_key(10) }|
                          |was posted successfully|.

    lv_msg = |Document { lv_key(10) } was posted successfully|.
    lv_subject = |Vendor Invoice posting was successful |.
    PERFORM invoice_notification USING lv_msg lv_subject.
  ENDIF.

  LOOP AT gt_invitems ASSIGNING <fs_item>.

    lv_item = lv_item + 1.

    PERFORM translate_description USING <fs_item>-description
                                        lv_key
                                        lv_item.


  ENDLOOP.
ENDFORM.

FORM translate_description    USING  pv_desc  TYPE string
                                     pv_key   TYPE bapiache09-obj_key
                                     pv_item  TYPE n.

* Session
  DATA(lo_session) = /aws1/cl_rt_session_aws=>create( iv_profile_id = p_pfl ).
* Translate
  DATA(lo_xl8)     = /aws1/cl_xl8_factory=>create( lo_session ).
* Translate
  CALL METHOD lo_xl8->translatetext
    EXPORTING
      iv_text               = pv_desc
      iv_sourcelanguagecode = 'auto'    " will use comprehend to do lang detection
      iv_targetlanguagecode = 'de'
    RECEIVING
      oo_output             = DATA(lo_output).

  DATA(lv_trans_desc) = lo_output->get_translatedtext( ).

  PERFORM save_item_text USING pv_key
                               pv_item
                               pv_desc
                               'en'.

  PERFORM save_item_text USING pv_key
                               pv_item
                               lv_trans_desc
                               'de'.


ENDFORM.

FORM get_profile_id CHANGING pv_pfl TYPE /aws1/rt_pflh-profile_id.

  READ TABLE gt_profiles INTO DATA(ls_profile)
              WITH KEY profile_id = p_pfl.
  IF sy-subrc = 0.
    pv_pfl = ls_profile-profile_id.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form initialize_profiles
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialize_profiles .
  CLEAR: gt_profiles.
  SELECT * FROM /aws1/rt_pflh INTO TABLE gt_profiles.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_profile
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM validate_profile .

  DATA: lv_pfl TYPE /aws1/rt_pflh-profile_id.

  PERFORM get_profile_id CHANGING lv_pfl.
  IF lv_pfl IS INITIAL.
    MESSAGE e016(rp) WITH 'Enter a valid profile'.
  ENDIF.

ENDFORM.
