interface ZIF_TRANSPOSER
  public .


  types:
    begin of  TY_ROWS,
         row_indx    type sy-tabix,
         col_indx    type sy-tabix,
         cell_val    type string,
         end of ty_rows .
  types:
    tt_rows type standard table of ty_rows .
  types:
    begin of ty_fields,
         field type string,
         end of ty_fields .
  types:
    tt_fields type standard table of ty_fields .

  data T_FIELDS type TT_FIELDS .
  data T_DATA type TT_ROWS .
  data O_DYNA_TABLE type ref to DATA .
  data O_DYNA_WA type ref to DATA .

  methods TRANSPOSE
    importing
      !IV_NO_HEADER type BOOLE optional
    returning
      value(RO_DATA) type ref to DATA .
  methods CREATE_DYNAMIC_TABLE .
  methods COLLECT_FIELDS
    importing
      !IV_NO_HEADER type BOOLE optional .
endinterface.
