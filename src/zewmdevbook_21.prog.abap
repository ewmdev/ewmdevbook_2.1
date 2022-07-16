*&---------------------------------------------------------------------*
*& Report ZEWMDEVBOOK_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE zewmdevbook_21_top                      .    " Global Data

* INCLUDE ZEWMDEVBOOK_21_O01                      .  " PBO-Modules
* INCLUDE ZEWMDEVBOOK_21_I01                      .  " PAI-Modules
* INCLUDE ZEWMDEVBOOK_21_F01                      .  " FORM-Routines

BREAK-POINT ID zewmdevbook_21.

PARAMETERS: docno  TYPE /scdl/dl_docno_int,
            itemno TYPE /scdl/dl_itemno.

"2.1 Object Instances BOPF
DATA: lo_sp          TYPE REF TO /scdl/cl_sp_prd_out,
      lo_message_box TYPE REF TO /scdl/cl_sp_message_box.

TRY.
    lo_message_box = NEW /scdl/cl_sp_message_box( ).
    lo_sp          = NEW /scdl/cl_sp_prd_out(
      io_message_box = lo_message_box
      iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_out_prd
      iv_mode        = /scdl/cl_sp=>sc_mode_classic ).
  CATCH /scdl/cx_delivery.
ENDTRY.

"2.2 Example Delivery Query BOPF
DATA: ls_options      TYPE /scdl/s_sp_query_options,
      ls_inparam      TYPE /scdl/s_sp_q_head,
      lt_selections   TYPE /scdl/t_sp_selection,
      lt_a_item       TYPE /scdl/t_sp_a_item,
      lt_return_codes TYPE /scdl/t_sp_return_code,
      lt_messages     TYPE /scdl/dm_message_tab.

DATA(ls_selections) = VALUE /scdl/s_sp_selection(
  fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
  sign      = wmegc_sign_inclusive
  option    = wmegc_option_eq
  low       = |{ docno ALPHA = IN }| ).
APPEND ls_selections TO lt_selections.
CLEAR ls_selections.
ls_selections = VALUE #(
  fieldname = /scdl/if_dl_logfname_c=>sc_itemno_i
  sign      = wmegc_sign_inclusive
  option    = wmegc_option_eq
  low       = |{ itemno ALPHA = IN }| ).
APPEND ls_selections TO lt_selections.

lo_sp->query(
  EXPORTING
    query      = /scdl/if_sp_c=>sc_qry_item
    selections = lt_selections
  IMPORTING
    outrecords = lt_a_item
    rejected   = DATA(lv_rejected) ).
IF lv_rejected = abap_true.
  lt_messages = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ENDIF.

"2.3 Example Delivery Aspect Select BOPF
DATA: lt_sp_k_head       TYPE /scdl/t_sp_k_head,
      lt_sp_k_item       TYPE /scdl/t_sp_k_item,
      lt_a_item_delterms TYPE /scdl/t_sp_a_item_delterm.

ASSIGN lt_a_item[ 1 ] TO FIELD-SYMBOL(<a_item>).
IF sy-subrc = 0.
  DATA(ls_sp_k_item) = CORRESPONDING /scdl/s_sp_k_item( <a_item> ).
  APPEND ls_sp_k_item TO lt_sp_k_item.
ELSE.
  EXIT.
ENDIF.
lo_sp->select(
  EXPORTING
    inkeys       = lt_sp_k_item
    aspect       = /scdl/if_sp_c=>sc_asp_item_delterm
  IMPORTING
    outrecords   = lt_a_item_delterms
    rejected     = lv_rejected
    return_codes = lt_return_codes ).

IF lv_rejected = abap_true.
  lt_messages = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ELSEIF line_exists( lt_return_codes[ failed = abap_true ] ).
  lt_messages = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ENDIF.

"2.4 Example Delivery Aspect Select by Relation BOPF
lo_sp->select_by_relation(
  EXPORTING
    relation  = /scdl/if_sp_c=>sc_rel_head_to_item
    inrecords = lt_sp_k_head
    aspect    = /scdl/if_sp_c=>sc_asp_head
  IMPORTING
    outrecords   = lt_a_item
    rejected     = lv_rejected
    return_codes = lt_return_codes ).

"2.5 Example Delivery Query EWM Service Provider
DATA:
  lt_message        TYPE /scdl/dm_message_tab,
  lt_selection      TYPE TABLE OF /scwm/dlv_selection_str,
  ls_addmeas_detail TYPE /scdl/dl_addmeas_key_str,
  ls_include        TYPE /scwm/dlv_query_incl_str_prd.

"Get instance
DATA(lo_delivery) = NEW /scwm/cl_dlv_management_prd( ).
"Build up range of select options
DATA(ls_selection) = VALUE /scwm/dlv_selection_str(
  fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
  sign      = wmegc_sign_inclusive
  option    = wmegc_option_eq
  low       = |{ docno ALPHA = IN }| ).
APPEND ls_selection TO lt_selection.
"Set read options
DATA(ls_read_options) = VALUE /scwm/dlv_query_contr_str(
  data_retrival_only      = abap_true
  mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance ).
"Call DLV query
TRY.
    CALL METHOD lo_delivery->query
      EXPORTING
        iv_doccat       = /scdl/if_dl_c=>sc_doccat_out_prd
        it_selection    = lt_selection
        is_read_options = ls_read_options
        is_include_data = ls_include
      IMPORTING
        et_headers      = DATA(lt_headers)
        et_items        = DATA(lt_items)
        eo_message      = DATA(lo_message).
    IF lo_message IS BOUND.
      lt_message = lo_message->get_messages( ).
    ENDIF.
  CATCH /scdl/cx_delivery INTO DATA(lx_delivery).
    IF lx_delivery->mo_message IS BOUND.
      lo_message->add( lx_delivery->mo_message ).
    ENDIF.
ENDTRY.

"2.6 Example Delivery Hierarchy
DATA: ls_addmeas   TYPE /scdl/dl_addmeas_str,
      ls_status    TYPE /scdl/dl_status_str,
      ls_hierarchy TYPE /scdl/dl_hierarchy_str.

LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<item>).
  "Check hierarchy
  DATA(lo_corr) = /scwm/cl_dlv_correlation=>get_instance( ).
  LOOP AT <item>-hierarchy INTO ls_hierarchy.
    TRY.
        CALL METHOD lo_corr->get_hier_cat
          EXPORTING
            iv_hierarchy_type = ls_hierarchy-hierarchy_type
          IMPORTING
            ev_hierarchy_cat  = DATA(lv_cat).
      CATCH /bopf/cx_frw .
    ENDTRY.
    "Skip split items
    IF  lv_cat = /scdl/if_dl_hierarchy_c=>sc_cat_ssp
    AND ls_hierarchy-parent_object IS NOT INITIAL.
      DATA(lv_skip) = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF lv_skip = abap_true.
    DELETE lt_items.
    CONTINUE.
  ENDIF.

  READ TABLE <item>-status INTO ls_status
  WITH KEY status_type = /scdl/if_dl_status_c=>sc_t_picking.

  IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_not_relevant.
    DELETE lt_items.
    CONTINUE.
  ELSEIF ls_status-status_value NE /scdl/if_dl_status_c=>sc_v_finished.
    "Item & not yet completely picked.
    MESSAGE e001(zewmdevbook_21) WITH <item>-itemno.
    CONTINUE.
  ENDIF.

  READ TABLE <item>-addmeas INTO ls_addmeas
  WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_pack
           qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open.
  IF sy-subrc = 0 AND ls_addmeas-qty NE 0.
    "Item & not yet completely packed.
    MESSAGE e002(zewmdevbook_21) WITH <item>-itemno.
  ENDIF.
ENDLOOP.
