*&---------------------------------------------------------------------*
*& Include          ZSDK_DEMO_TEXTRACT_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields.

TYPES: ty_raw(255) TYPE x.
TYPES: tt_raw TYPE STANDARD TABLE OF ty_raw.


TYPES: BEGIN OF ty_block,
         block_id TYPE string,
         block    TYPE REF TO /aws1/cl_texblock,
       END   OF ty_block.

TYPES: BEGIN OF ty_word,
         page TYPE string,
         word TYPE string,
       END   OF ty_word.

TYPES: tt_block TYPE STANDARD TABLE OF ty_block.

TYPES: BEGIN OF ty_table,
         table_id TYPE string,
         page     TYPE string,
         row_indx TYPE sy-tabix,
         col_indx TYPE sy-tabix,
         cell_val TYPE string,
       END OF ty_table.

TYPES: tt_table TYPE STANDARD TABLE OF ty_table.
TYPES: tt_word TYPE STANDARD TABLE OF ty_word.
** Invoice headers
  types: begin of ty_invmap,
         key type string,
         value type string,
         end   of ty_invmap.

* Invoice items
 types: begin of ty_invitems,
         description type string,
         qty         type string,
         unit        type string,
         unitprice   type string,
         vatp        type string,
         vat         type string,
         total       type string,
         end   of ty_invitems.
*-----------------------------------------------------------------------*
*   C O N S T A N T S
*-----------------------------------------------------------------------*
CONSTANTS: gc_qm TYPE /aws1/rt_profile_id VALUE 'QM'.

*-----------------------------------------------------------------------*
*   G L O B A L  V A R I A B L E S
*-----------------------------------------------------------------------*
DATA: gv_xstring    TYPE xstring.
DATA: gv_bucketname TYPE /aws1/s3_bucketname.

DATA: gs_message    TYPE bapiret2.
*-----------------------------------------------------------------------*
*   G L O B A L  I N T E R N A L  T A B L E S
*-----------------------------------------------------------------------*
DATA: gt_rawtab TYPE tt_raw.



  DATA: gt_invmap type table of ty_invmap.
  DATA: gt_invitems type table of ty_invitems.
  DATA: gt_profiles  type table of /aws1/rt_pflh.


*-----------------------------------------------------------------------*
*   G L O B A L  O B J E C T S
*-----------------------------------------------------------------------*
* AWS SDK session object
DATA: go_session  TYPE REF TO /aws1/cl_rt_session_base.
DATA: go_s3       TYPE REF TO /aws1/if_s3.
