class ZCL_AWS_TEXTRACT_HELPER definition
  public
  final
  create public .

public section.

  types:
    begin of ty_word,
           page type string,
           word type string,
         end   of ty_word .
  types:
    tt_word type standard table of ty_word with non-unique key word .
  types:
    BEGIN OF ty_form,
           key   TYPE string,
           value TYPE string,
           page  TYPE string,
         end of ty_form .
  types:
    tt_form type STANDARD TABLE OF ty_form with NON-UNIQUE key key .
  types:
    begin of ty_table,
         table_id type string,
         page     type string,
         row_indx type sy-tabix,
         col_indx type sy-tabix,
         cell_val type string,
         end of ty_table .
  types:
    tt_table type standard table of ty_table with non-unique key table_id .

  data GO_TEXTRACT type ref to /AWS1/IF_TEX .
  data GO_SESSION type ref to /AWS1/CL_RT_SESSION_BASE .
  data AT_FEATURETYPES type /AWS1/CL_TEXFEATURETYPES_W=>TT_FEATURETYPES .
  constants CO_STATUS type STRING value 'SUCCEEDED' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_PROFILE type /AWS1/RT_PROFILE_ID
    raising
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_NO_AUTH_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods SET_FEATURE_TYPES
    importing
      !IM_TT_FEATURES type STRINGTAB optional .
  methods ANALYZE_DOCUMENT_ASYNCHRONOUS
    importing
      !IV_BUCKET type STRING
      !IV_KEY type STRING
    returning
      value(RE_JOBID) type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXBADDOCUMENTEX
      /AWS1/CX_TEXDOCUMENTTOOLARGEEX
      /AWS1/CX_TEXIDEMPOTENTPRMMIS00
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXLIMITEXCEEDEDEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXUNSUPPORTEDDOCEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_DOCUMENT_RESULTS_ASYNCH
    importing
      !IM_JOBID type STRING
    returning
      value(RE_STATUS) type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_TABLE_DATA
    returning
      value(RE_DATA) type TT_TABLE .
  methods GET_FORM_DATA
    returning
      value(RE_DATA) type TT_FORM .
  methods GET_WORDS
    returning
      value(RE_DATA) type TT_WORD .
  methods GET_LINES
    returning
      value(RE_DATA) type TT_WORD .
protected section.
private section.

  types:
    BEGIN OF ty_block,
           block_id TYPE string,
           block    TYPE REF TO /aws1/cl_texblock,
         END   OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block .

  data AT_BLOCKS type /AWS1/CL_TEXBLOCK=>TT_BLOCKLIST .
  data AT_KEY_BLOCK type TT_BLOCK .
  data AT_VAL_BLOCK type TT_BLOCK .
  data AT_BLOCK_MAP type TT_BLOCK .
  data AT_TAB_BLOCK type TT_BLOCK .

  methods GET_KEY_VALUE_BLOCK_MAPS .
  methods GET_TABLE_BLOCKS .
  methods GET_BLOCKS
    importing
      !IM_JOBID type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_VALUEBLOCK_FOR_KEY
    importing
      !IM_KEY_BLOCK type TY_BLOCK
    returning
      value(RE_VAL_BLOCK) type TY_BLOCK .
  methods GET_TEXT_BLOCK
    importing
      !IM_BLOCK type TY_BLOCK
    returning
      value(RE_VALUE) type STRING .
  methods GET_ROWS_TABLE_BLOCK
    importing
      !IM_TABLEID type STRING
      !IM_BLOCK type TY_BLOCK
    returning
      value(RE_TABLE) type TT_TABLE .
ENDCLASS.



CLASS ZCL_AWS_TEXTRACT_HELPER IMPLEMENTATION.


  METHOD ANALYZE_DOCUMENT_ASYNCHRONOUS.

    DATA(lo_s3object) = NEW /aws1/cl_texs3object( iv_bucket = iv_bucket
                                                iv_name   = iv_key  ).

    DATA(lo_documentlocation) = NEW /aws1/cl_texdocumentlocation( io_s3object = lo_s3object ).

* Start document analysis
   re_jobid = go_textract->startdocumentanalysis(
        EXPORTING
          io_documentlocation    =  lo_documentlocation
          it_featuretypes        =  at_featuretypes
      )->get_jobid( ).


  ENDMETHOD.


  METHOD constructor.
    go_session  =  /aws1/cl_rt_session_aws=>create( iv_profile_id =  iv_profile ).
    go_textract =  /aws1/cl_tex_factory=>create( go_session ).
    set_feature_types( ).
  ENDMETHOD.


  METHOD get_blocks.


    IF ( go_textract->getdocumentanalysis( EXPORTING
            iv_jobid = im_jobid )->has_blocks( ) = abap_true ).
      at_blocks = go_textract->getdocumentanalysis(
                     EXPORTING iv_jobid = im_jobid )->get_blocks( ).

      IF go_textract->getdocumentanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        )->has_nexttoken( ) = abap_true.

        DATA(lv_token) = go_textract->getdocumentanalysis(
                                         EXPORTING iv_jobid = im_jobid
                                         )->get_nexttoken( ).
      ENDIF.

      WHILE lv_token IS NOT INITIAL.
        DATA(lt_next_blocks) =   go_textract->getdocumentanalysis(
                                      EXPORTING iv_jobid = im_jobid
                                                iv_nexttoken = lv_token
                                                )->get_blocks(  ).
        APPEND LINES OF lt_next_blocks TO at_blocks.

        IF go_textract->getdocumentanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->has_nexttoken( ) = abap_true.

          lv_token = go_textract->getdocumentanalysis(
                                       EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->get_nexttoken( ).
        ELSE.

          EXIT.
        ENDIF.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


  METHOD get_document_results_asynch.

    WHILE go_textract->getdocumentanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ) <> 'SUCCEEDED'.

      cl_progress_indicator=>progress_indicate(
       i_text = |Watiing for Textract to finish Processing Job|
       i_output_immediately = abap_true ).
*
      WAIT UP TO 10 SECONDS.

      IF sy-index = 10.
        DATA(lv_status) = go_textract->getdocumentanalysis( EXPORTING
                             iv_jobid = im_jobid )->get_jobstatus( ).

        EXIT.
      ENDIF.
    ENDWHILE.

    IF lv_status IS NOT INITIAL.
      re_status = lv_status.
      EXIT.
    ENDIF.

    get_blocks( im_jobid ).

    get_key_value_block_maps( ).

    get_table_blocks( ).

    re_status = go_textract->getdocumentanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ).

  ENDMETHOD.


  method GET_FORM_DATA.

 data: ls_form type ty_form.

      LOOP AT at_key_block INTO DATA(ls_key_block).

       data(ls_val_block) = get_valueblock_for_key( im_key_block = ls_key_block ).

       ls_form-key = get_text_block( im_block = ls_key_block ).
       ls_form-value = get_text_block( im_block = ls_val_block ).
       ls_form-page = ls_val_block-block->get_page( ).

       append ls_form to re_data.

      endloop.

  endmethod.


  METHOD get_key_value_block_maps.

    CLEAR: at_block_map, at_key_block, at_val_block.

    LOOP AT AT_blocks INTO DATA(ls_blocks).
* get key, value, block maps
      AT_block_map = VALUE #( BASE At_block_map ( block_id =
      ls_blocks->get_id( ) block = ls_blocks ) ).

      IF ls_blocks->get_blocktype( ) EQ 'KEY_VALUE_SET'.
        DATA(lt_entitytypes) = ls_blocks->get_entitytypes( ).
        IF lt_entitytypes[] IS NOT INITIAL.
          LOOP AT lt_entitytypes INTO DATA(ls_entitytypes).
            IF ls_entitytypes->get_value( ) EQ 'KEY'.
              At_key_block = VALUE #( BASE At_key_block ( block_id =
              ls_blocks->get_id( ) block = ls_blocks ) ).
            ELSE.
              At_val_block = VALUE #( BASE At_val_block ( block_id =
              ls_blocks->get_id( ) block = ls_blocks ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method GET_LINES.

    LOOP AT at_block_map INTO DATA(ls_block_map).

      IF ls_block_map-block->get_blocktype( ) EQ 'LINE'.

        re_data = VALUE #( BASE re_data (
        page = ls_block_map-block->get_page( )
        word = ls_block_map-block->get_text( ) ) ).
      ENDIF.

    ENDLOOP.


  endmethod.


  METHOD get_rows_table_block.

    DATA: ls_table TYPE ty_table.
    DATA: lt_table TYPE tt_table.

    DATA(lt_relationships) = im_block-block->get_relationships( ).

    LOOP AT lt_relationships INTO DATA(ls_relationship).
      IF ls_relationship->get_type( ) EQ 'CHILD'.
        LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
          READ TABLE at_block_map WITH KEY block_id = ls_id->get_value( )
                                 INTO DATA(ls_cell_block).


          IF sy-subrc = 0 AND
             ls_cell_block-block->get_blocktype( ) EQ 'CELL'.

            CLEAR: ls_table.

            ls_table-cell_val = get_text_block( im_block = ls_cell_block  ).
            ls_table-row_indx = ls_cell_block-block->get_rowindex( ).
            ls_table-col_indx = ls_cell_block-block->get_columnindex( ).
            ls_table-page = ls_cell_block-block->get_page( ).
            ls_table-table_id = im_tableid.
            APPEND ls_table TO lt_table.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    re_table = lt_table.

  ENDMETHOD.


  method GET_TABLE_BLOCKS.


  LOOP AT at_block_map INTO DATA(ls_block_map).
    IF ls_block_map-block->get_blocktype( ) EQ 'TABLE'.
      APPEND ls_block_map TO at_tab_block.
    ENDIF.
  ENDLOOP.

  endmethod.


  METHOD get_table_data.

    DATA lt_table TYPE tt_table.

    LOOP AT at_tab_block INTO DATA(ls_table_block).

      DATA(lv_tableid) = |table_{ sy-tabix }|.

      CLEAR: lt_table.
      get_rows_table_block(
        EXPORTING
          im_tableid =  lv_tableid
          im_block   = ls_table_block
        RECEIVING
          re_table   = lt_table
      ).

      APPEND LINES OF lt_table TO re_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_text_block.

    DATA(lt_relationships) = im_block-block->get_relationships( ).

    LOOP AT lt_relationships INTO DATA(ls_relationship).
      IF ls_relationship->get_type( ) EQ 'CHILD'.
        LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
          READ TABLE at_block_map WITH KEY block_id = ls_id->get_value( )
                                 INTO DATA(ls_word_block).
          IF sy-subrc = 0.
            CASE ls_word_block-block->get_blocktype( ).

              WHEN 'WORD'.
                DATA(lv_text) = ls_word_block-block->get_text( ).
                CONCATENATE re_value lv_text INTO re_value SEPARATED BY space.
              WHEN 'SELECTION_ELEMENT'.
                IF ls_word_block-block->get_selectionstatus( ) EQ
                'SELECTED'.

                  CONCATENATE re_value 'X ' INTO re_value.
                ENDIF.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  method GET_VALUEBLOCK_FOR_KEY.

  DATA(lt_relationships) = im_key_block-block->get_relationships( ).


  LOOP AT lt_relationships INTO DATA(ls_relationship).
    IF ls_relationship->get_type( ) EQ 'VALUE'.
      LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
        READ TABLE at_val_block WITH KEY block_id = ls_id->get_value( )
                                INTO re_val_block.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  endmethod.


  method GET_WORDS.

    LOOP AT at_block_map INTO DATA(ls_block_map).

      IF ls_block_map-block->get_blocktype( ) EQ 'WORD'.

        re_data = VALUE #( BASE re_data (
        page = ls_block_map-block->get_page( )
        word = ls_block_map-block->get_text( ) ) ).
      ENDIF.

    ENDLOOP.


  endmethod.


  METHOD set_feature_types.

    CLEAR: at_featuretypes.

    IF im_tt_features IS INITIAL.
      at_featuretypes = VALUE #( (  NEW /aws1/cl_texfeaturetypes_w( iv_value = 'FORMS'  ) )
                                 (  NEW /aws1/cl_texfeaturetypes_w( iv_value = 'TABLES' ) )
                                ).
    ELSE.
      LOOP AT im_tt_features INTO DATA(ls_feature).
        at_featuretypes = VALUE #( BASE at_featuretypes
                                  ( NEW /aws1/cl_texfeaturetypes_w( iv_value = ls_feature ) )
                                ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
