*&---------------------------------------------------------------------*
*& Report ZEWMDEVBOOK_21
*&---------------------------------------------------------------------*
*& Example Report for Warehouse Request Processing
*&---------------------------------------------------------------------*
REPORT zewmdevbook_21.

PARAMETERS: p_lgnum  TYPE /scwm/lgnum     OBLIGATORY,
            p_docno  TYPE /scdl/dl_docno  OBLIGATORY,
            p_itemno TYPE /scdl/dl_itemno OBLIGATORY,
            p_wt_fm  RADIOBUTTON GROUP wtcr,
            p_wt_api RADIOBUTTON GROUP wtcr.


"2.1 Object Instances BOPF
BREAK-POINT ID zewmdevbook_21.
TRY.
    DATA(lo_message_box) = NEW /scdl/cl_sp_message_box( ).
    DATA(lo_sp)          = NEW /scdl/cl_sp_prd_out(
      io_message_box = lo_message_box
      iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_out_prd
      iv_mode        = /scdl/cl_sp=>sc_mode_classic ).
  CATCH /scdl/cx_delivery.
ENDTRY.

"2.2 Example Delivery BOPF based Query
DATA: lt_bopf_items TYPE /scdl/t_sp_a_item.

lo_sp->query(
  EXPORTING
    query      = /scdl/if_sp_c=>sc_qry_item
    selections = VALUE /scdl/t_sp_selection(
   ( fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
     sign      = wmegc_sign_inclusive
     option    = wmegc_option_eq
     low       = |{ p_docno ALPHA = IN }| )
   ( fieldname = /scdl/if_dl_logfname_c=>sc_itemno_i
     sign      = wmegc_sign_inclusive
     option    = wmegc_option_eq
     low       = |{ p_itemno ALPHA = IN }| ) )
  IMPORTING
    outrecords = lt_bopf_items
    rejected   = DATA(lv_rejected) ).
IF lv_rejected = abap_true.
  DATA(lt_messages) = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ENDIF.

"2.3 Example Delivery BOPF based Aspect Select
DATA: lt_a_item_delterms TYPE /scdl/t_sp_a_item_delterm.

lo_sp->select(
  EXPORTING
    inkeys       = CORRESPONDING /scdl/t_sp_k_item( lt_bopf_items )
    aspect       = /scdl/if_sp_c=>sc_asp_item_delterm
  IMPORTING
    outrecords   = lt_a_item_delterms
    rejected     = lv_rejected
    return_codes = DATA(lt_return_codes) ).

IF lv_rejected = abap_true.
  lt_messages = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ELSEIF line_exists( lt_return_codes[ failed = abap_true ] ).
  lt_messages = lo_message_box->get_messages( ).
  CALL METHOD /scwm/cl_tm=>cleanup( ).
  EXIT.
ENDIF.

"2.4 Example Delivery BOPF base Aspect Select by Relation
lo_sp->select_by_relation(
  EXPORTING
    relation  = /scdl/if_sp_c=>sc_rel_head_to_item
    inrecords = CORRESPONDING /scdl/t_sp_k_head( lt_bopf_items )
    aspect    = /scdl/if_sp_c=>sc_asp_head
  IMPORTING
    outrecords   = lt_bopf_items
    rejected     = lv_rejected
    return_codes = lt_return_codes ).

SORT lt_bopf_items ASCENDING.
DELETE ADJACENT DUPLICATES FROM lt_bopf_items.
LOOP AT lt_bopf_items ASSIGNING FIELD-SYMBOL(<bopf_item>).
  WRITE: / 'BOPF',
           <bopf_item>-itemno,
           <bopf_item>-itemcat,
           <bopf_item>-itemtype.
ENDLOOP.


"2.5 Example Delivery EWM Service Provider based Query
BREAK-POINT ID zewmdevbook_21.
"Get instance of service provider class
DATA(lo_delivery) = NEW /scwm/cl_dlv_management_prd( ).
"Call query method of service provider class
TRY.
    CALL METHOD lo_delivery->query
      EXPORTING
        iv_doccat       = /scdl/if_dl_c=>sc_doccat_out_prd
        it_selection    = VALUE /scwm/dlv_selection_tab(
            ( fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
              sign      = wmegc_sign_inclusive
              option    = wmegc_option_eq
              low       = |{ p_docno ALPHA = IN }| ) )
        is_read_options = VALUE #(
data_retrival_only      = abap_true
mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance )
      IMPORTING
        et_headers      = DATA(lt_srv_headers)
        et_items        = DATA(lt_srv_items)
        eo_message      = DATA(lo_message).
    IF lo_message IS BOUND.
      DATA(lt_message) = lo_message->get_messages( ).
    ENDIF.
  CATCH /scdl/cx_delivery INTO DATA(lx_delivery).
    IF lx_delivery->mo_message IS BOUND.
      lo_message->add( lx_delivery->mo_message ).
    ENDIF.
ENDTRY.

LOOP AT lt_srv_headers ASSIGNING FIELD-SYMBOL(<srv_header>).
  WRITE: / 'SRV ',
           <srv_header>-docno,
           <srv_header>-doccat,
           <srv_header>-doctype.
ENDLOOP.

"2.6 Example Delivery Hierarchy EWM Service Provider
LOOP AT lt_srv_items ASSIGNING FIELD-SYMBOL(<srv_item>).
  "Check hierarchy
  DATA(lo_corr) = /scwm/cl_dlv_correlation=>get_instance( ).
  LOOP AT <srv_item>-hierarchy ASSIGNING FIELD-SYMBOL(<srv_item_hierarchy>).
    TRY.
        CALL METHOD lo_corr->get_hier_cat
          EXPORTING
            iv_hierarchy_type = <srv_item_hierarchy>-hierarchy_type
          IMPORTING
            ev_hierarchy_cat  = DATA(lv_cat).
      CATCH /bopf/cx_frw .
    ENDTRY.
    "Skip split items
    IF  lv_cat = /scdl/if_dl_hierarchy_c=>sc_cat_ssp
    AND <srv_item_hierarchy>-parent_object IS NOT INITIAL.
      DATA(lv_skip) = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF lv_skip = abap_true.
    DELETE lt_srv_items.
    CONTINUE.
  ENDIF.

  TRY.
      DATA(ls_status) = <srv_item>-status[
        status_type = /scdl/if_dl_status_c=>sc_t_picking ].

      IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_not_relevant.
        DELETE lt_srv_items.
        CONTINUE.
      ELSEIF ls_status-status_value NE /scdl/if_dl_status_c=>sc_v_finished.
        "Item & not yet completely picked.
        MESSAGE i001(zewmdevbook_21) WITH <srv_item>-itemno.
        CONTINUE.
      ENDIF.
    CATCH cx_sy_itab_line_not_found.
      "Item & not relevant for picking.
      MESSAGE i003(zewmdevbook_21) WITH <srv_item>-itemno.
  ENDTRY.

  TRY.
      DATA(ls_addmeas) = <srv_item>-addmeas[
       qty_role = /scdl/if_dl_addmeas_c=>sc_qtyrole_pack
       qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_open ].
      IF ls_addmeas-qty NE 0.
        "Item & not yet completely packed.
        MESSAGE i002(zewmdevbook_21) WITH <srv_item>-itemno.
      ENDIF.
    CATCH cx_sy_itab_line_not_found.
      "Item & not relevant for packing.
      MESSAGE i004(zewmdevbook_21) WITH <srv_item>-itemno.
  ENDTRY.
ENDLOOP.


"2.7 Example Delivery API based Query
BREAK-POINT ID zewmdevbook_21.
"Set warehouse request of type Outbound Delivery Order
DATA: lo_whr_api_outb TYPE REF TO /scwm/if_api_whr_outbound.
TRY.
    /scwm/cl_api_factory=>get_service( IMPORTING eo_api = lo_whr_api_outb ).
    "Set warehouse number obligatory for API
    /scwm/cl_tm=>set_lgnum( p_lgnum ).
    "Map business keys to warehouse request keys and read ODOs
    lo_whr_api_outb->/scwm/if_api_warehouse_request~get_keys_for_bus_keys(
          EXPORTING
            it_whr_bus_keys = VALUE /scwm/if_api_warehouse_request=>yt_whr_bus_key(
              ( docno = |{ p_docno ALPHA = IN }| ) )
          IMPORTING
            et_whr_keymap   = DATA(et_keys_map) ).

    lo_whr_api_outb->read_outbound_dlv_order(
      EXPORTING
        it_whr_key      = CORRESPONDING #( et_keys_map )
        is_include      = VALUE #( head_refdoc = abap_true )
        is_read_options = VALUE #( fast_for_display = abap_true
                                   include_deleted  = abap_true )
        is_locking      = VALUE #( lock_result = abap_false )
      IMPORTING
        et_headers      = DATA(lt_whr_api_headers) ).
  CATCH /scwm/cx_api_faulty_call.
ENDTRY.

LOOP AT lt_whr_api_headers ASSIGNING FIELD-SYMBOL(<whr_api_header>).
  WRITE: / 'API ',
           <whr_api_header>-docno,
           <whr_api_header>-doccat,
           <whr_api_header>-doctype.
ENDLOOP.


"2.8 Sample Call of Function Module /SCWM/TO_CREATE_WHR
DATA: lt_create_whr TYPE /scwm/tt_to_prep_whr_int,
      lv_tanum      TYPE /scwm/tanum,
      lt_ltap_vb    TYPE /scwm/tt_ltap_vb,
      lt_bapiret    TYPE bapirettab,
      lv_severity   TYPE bapi_mtype.

BREAK-POINT ID zewmdevbook_21.
IF p_wt_fm = abap_true.
  "Call Central Cleanup
  /scwm/cl_tm=>cleanup( EXPORTING iv_lgnum = p_lgnum ).
  "Transfer DLV Keys
  LOOP AT lt_srv_items ASSIGNING <srv_item>.
    DATA(ls_create_whr) = VALUE /scwm/s_to_prepare_whr_int(
      rdocid  = <srv_item>-docid
      ritmid  = <srv_item>-itemid
      rdoccat = <srv_item>-doccat ).
    APPEND ls_create_whr TO lt_create_whr.
    CLEAR ls_create_whr.
  ENDLOOP.
  "Trigger WT Creation for Warehouse Request per Function Module
  CALL FUNCTION '/SCWM/TO_CREATE_WHR'
    EXPORTING
      iv_lgnum       = p_lgnum
      iv_bname       = sy-uname
      it_create_whr  = lt_create_whr
      iv_update_task = abap_false
      iv_commit_work = abap_true
    IMPORTING
      et_ltap_vb     = lt_ltap_vb
      et_bapiret     = lt_bapiret
      ev_severity    = lv_severity.

  "2.9 Sample Call of API /SCWM/TO_CREATE_WHR
  DATA: lo_wt_api TYPE REF TO /scwm/if_api_whse_task.
ELSEIF p_wt_api = abap_true.
  TRY.
      /scwm/cl_api_factory=>get_service( IMPORTING eo_api = lo_wt_api ).
      "Trigger WT Creation for Warehouse Request per API
      lo_wt_api->create_for_whr(
          EXPORTING
            iv_whno        = p_lgnum
            it_create      = CORRESPONDING #( lt_srv_items )
          IMPORTING
            et_created_wht = DATA(lt_created_whr_wht)
            eo_message     = DATA(lo_error_msg) ).

      lo_error_msg->get_messages( IMPORTING et_message = DATA(lt_error_msg)
                                            et_bapiret = lt_bapiret ).
    CATCH /scwm/cx_api_whse_task.
  ENDTRY.
ENDIF.
WRITE: / 'Number of messages from WT creation:', lines( lt_bapiret ).
