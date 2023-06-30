class ZCL_TRANSPOSE_COLUMNS definition
  public
  final
  create public .

public section.

  interfaces ZIF_TRANSPOSER .

  methods CONSTRUCTOR
    importing
      !IT_DATA type ZIF_TRANSPOSER=>TT_ROWS .
protected section.
private section.

  aliases O_DYNA_TABLE
    for ZIF_TRANSPOSER~O_DYNA_TABLE .
  aliases O_DYNA_WA
    for ZIF_TRANSPOSER~O_DYNA_WA .
  aliases T_DATA
    for ZIF_TRANSPOSER~T_DATA .
  aliases T_FIELDS
    for ZIF_TRANSPOSER~T_FIELDS .
ENDCLASS.



CLASS ZCL_TRANSPOSE_COLUMNS IMPLEMENTATION.


  method CONSTRUCTOR.

    t_data = it_data.
  endmethod.


  METHOD zif_transposer~collect_fields.


    CHECK t_fields IS INITIAL.

    DATA: ls_field LIKE LINE OF me->t_fields.

    IF iv_no_header = abap_true.
      me->t_fields = VALUE #( ( field = 'Key') ( field = 'Value') ).
    ELSE.
      LOOP AT me->t_data INTO DATA(ls_data).
        IF ls_data-row_indx > 1.
          EXIT.
        ENDIF.

        MOVE ls_data-cell_val TO ls_field-field.
        APPEND ls_field TO me->t_fields.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_transposer~create_dynamic_table.
    DATA lo_element  TYPE REF TO cl_abap_elemdescr.
    DATA lt_tot_comp TYPE cl_abap_structdescr=>component_table.


    DATA lo_new_type TYPE REF TO cl_abap_structdescr.

    LOOP AT me->t_fields INTO DATA(ls_field).
      REPLACE ALL OCCURENCES OF '.' IN ls_field-field WITH space.
      REPLACE ALL OCCURENCES OF '%' IN ls_field-field WITH 'P'.
      CONDENSE ls_field-field NO-GAPS.

      READ TABLE lt_tot_comp WITH KEY name = ls_field-field TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_field-field = |{ ls_field-field }| && |_| && |{ sy-tabix }|.
        CONDENSE ls_field-field NO-GAPS.
      ENDIF.
      lo_element ?= cl_abap_elemdescr=>get_string( ).
      lt_tot_comp = VALUE #( BASE lt_tot_comp ( name = ls_field-field type = lo_element ) ).
    ENDLOOP.

    lo_new_type = cl_abap_structdescr=>create( p_components = lt_tot_comp ).

    DATA(lo_new_tab) = cl_abap_tabledescr=>create(
                    p_line_type  = lo_new_type
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).

    CREATE DATA o_dyna_table TYPE HANDLE lo_new_tab.
    CREATE DATA o_dyna_wa    TYPE HANDLE lo_new_type.

  ENDMETHOD.


  METHOD zif_transposer~transpose.

    FIELD-SYMBOLS: <fs_tab> TYPE STANDARD TABLE.


    me->zif_transposer~collect_fields( iv_no_header = iv_no_header ).
    me->zif_transposer~create_dynamic_table( ).

    IF iv_no_header = abap_false.
      DELETE me->t_data WHERE row_indx = 1.  " delete header row
    ENDIF.

    ASSIGN o_dyna_table->* TO <fs_tab>.
    ASSIGN o_dyna_wa->*    TO FIELD-SYMBOL(<fs_wa>).

    LOOP AT me->t_data INTO DATA(ls_data).
      ASSIGN COMPONENT ls_data-col_indx OF STRUCTURE
             <fs_wa> TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc = 0.
        <fs_field> = ls_data-cell_val.
        SHIFT <fs_field> LEFT DELETING LEADING space.
      ENDIF.

      AT END OF row_indx.
        APPEND <fs_wa> TO <fs_tab>.
        CLEAR  <fs_wa>.
      ENDAT.
    ENDLOOP.

    ro_data = o_dyna_table.

  ENDMETHOD.
ENDCLASS.
