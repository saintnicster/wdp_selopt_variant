CLASS zcl_wdc__variant_maint DEFINITION
  PUBLIC
  INHERITING FROM cl_wd_component_assistance
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_t_variant_fields TYPE STANDARD TABLE OF zdv_wdc__varaint_fields WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_sel_texts,
        paramid       TYPE if_wd_select_options=>t_selection_screen_item-m_id,
        kind          TYPE if_wd_select_options=>t_selection_screen_item-m_type,
        m_description TYPE if_wd_select_options=>t_selection_screen_item-m_description,
      END OF ty_sel_texts .
    TYPES:
      ty_t_sel_texts TYPE STANDARD TABLE OF ty_sel_texts WITH DEFAULT KEY .
    TYPES:
      ty_t_varivar TYPE TABLE OF rsvarivar WITH DEFAULT KEY .

    METHODS delete_current_default .
    METHODS set_variant_as_default
      IMPORTING
        !i_variant   TYPE variant
        !i_is_global TYPE wdy_boolean .
    METHODS selopt_field_is_date
      IMPORTING
        !i_paramid    TYPE memoryid
        !i_kind       TYPE rsscr_kind
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    METHODS varaints_enabled_for_user
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    METHODS user_can_create_self_variants
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    METHODS user_can_create_glbl_variants
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    METHODS pull_variant_list
      RETURNING
        VALUE(r_list) TYPE zcl_wdc__variant=>ty_t_variant_list .
    METHODS clear_variant .
    METHODS delete_variant
      IMPORTING
        !i_variant_user TYPE uname
        !i_variant      TYPE variant .
    METHODS load_variant
      IMPORTING
        !i_variant_user TYPE uname
        !i_variant      TYPE variant .
    METHODS save_variant_to_db
      IMPORTING
        !i_variant     TYPE variant
        !i_description TYPE text25
        !i_is_global   TYPE wdy_boolean
        !it_field_info TYPE ty_t_variant_fields .
    METHODS transform_screen_to_db
      CHANGING
        !ct_variant_fields TYPE zcl_wdc__variant=>ty_t_db_import_struct .
    METHODS populate_screen_from_db
      IMPORTING
        VALUE(io_variant) TYPE REF TO zcl_wdc__variant OPTIONAL .
    METHODS variant_can_be_deleted_by_user
      IMPORTING
        !i_variant      TYPE variant
        !i_variant_user TYPE uname
      RETURNING
        VALUE(retval)   TYPE wdy_boolean .
    METHODS pull_select_option_texts
      EXPORTING
        !et_fields TYPE ty_t_sel_texts .
    METHODS set_variant_helper
      IMPORTING
        !i_helper TYPE REF TO zif_wdc__variant_helper .
    METHODS set_selopt_helper
      IMPORTING
        !i_helper TYPE REF TO if_wd_select_options .
    METHODS set_header_data
      IMPORTING
        !i_parent_component    TYPE wdy_component_name
        !i_componentusage_name TYPE wdy_wb_componentusage_name
        !i_uname               TYPE uname .
    METHODS check_existance
      IMPORTING
        !i_variant        TYPE variant
        !i_is_global      TYPE wdy_boolean
      EXPORTING
        !e_is_current     TYPE wdy_boolean
        !e_already_exists TYPE wdy_boolean .
    METHODS get_varivar_table
      RETURNING
        VALUE(retval) TYPE ty_t_varivar .
    METHODS get_single_varivar
      IMPORTING
        !i_vname      TYPE raldb_vnam
      RETURNING
        VALUE(retval) TYPE rsvarivar .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_variant_cache,
        list       TYPE zcl_wdc__variant=>ty_t_variant_list,
        cache_date TYPE sydatum,
        cache_time TYPE syuzeit,
      END OF ty_variant_cache .

  data GO_CURRENT_VARIANT type ref to ZCL_WDC__VARIANT .
  constants GC_TYPENAME_FIELD_ID type STRING value 'M_ID' ##NO_TEXT.
  constants GC_TYPENAME_FIELD_TYPE type STRING value 'M_TYPE' ##NO_TEXT.
  constants GC_TYPENAME_FIELD_RANGE type STRING value 'MT_RANGE_TABLE' ##NO_TEXT.
  constants GC_TYPENAME_PARAM_VALUE type STRING value 'M_VALUE' ##NO_TEXT.
  data GO_SEL_HELPER type ref to IF_WD_SELECT_OPTIONS .
  data GS_VARIANT_KEY type ZWDC__VARIANT_KEY .
  data GS_VARIANT_CACHE type TY_VARIANT_CACHE .
  data GO_VARIANT_HELPER type ref to ZIF_WDC__VARIANT_HELPER .
  data GT_VARIVAR type TY_T_VARIVAR .

  methods POPULATE_DYN_DATE
    importing
      !IS_VALUE type ZDV_WDC__VARAINT_DB
    returning
      value(RETVAL) type RSDATRANGE .
ENDCLASS.



CLASS ZCL_WDC__VARIANT_MAINT IMPLEMENTATION.


  METHOD check_existance.
    DATA(ls_key)   = gs_variant_key.
    ls_key-variant = i_variant.

    IF zcl_wdc__variant=>check_existence( is_key = ls_key i_is_global = i_is_global ).
      e_already_exists = abap_true.

      IF go_current_variant IS BOUND.
        DATA(ls_current) = go_current_variant->get_header( ).
        IF  ls_current-variant                        EQ ls_key-variant AND
            go_current_variant->is_global_variant( )  EQ i_is_global.
          e_is_current = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD clear_variant.
    CLEAR go_current_variant.

    "Explicitly not passing a variant object so that it will
    " clear out the fields and not repopulate with anything
    me->populate_screen_from_db(  ).

  ENDMETHOD.


  METHOD delete_current_default.
    CALL METHOD zcl_wdc__variant=>default_remove
      EXPORTING
        is_variant = gs_variant_key.

  ENDMETHOD.


  METHOD delete_variant.
    READ TABLE gs_variant_cache-list  WITH KEY  uname    = i_variant_user
                                                variant  = i_variant
                                      BINARY SEARCH ASSIGNING FIELD-SYMBOL(<fs_cache>).
    IF sy-subrc EQ 0.
      go_current_variant = <fs_cache>-object.
      IF go_current_variant IS NOT BOUND.
        RETURN.
      ENDIF.
    ENDIF.

    go_current_variant->delete( ).

    IF go_variant_helper IS BOUND.
      TRY.
          CALL METHOD go_variant_helper->post_save
            EXPORTING
              i_variant_key = CORRESPONDING #( <fs_cache> )
              i_was_deleted = abap_true.
        CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
          "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
          "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
          "If they didn't implement, they want default behaviours
          IF lo_error->textid NE cx_sy_dyn_call_illegal_method=>method_not_implemented.
            RAISE EXCEPTION lo_error.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_single_varivar.
    READ TABLE gt_varivar WITH KEY runt_fb = i_vname ASSIGNING FIELD-SYMBOL(<fs_varivar>).
    IF sy-subrc EQ 0.
      retval = <fs_varivar>.
    ENDIF.
  ENDMETHOD.


  METHOD get_varivar_table.
    IF gt_varivar IS INITIAL.
      CALL FUNCTION 'RS_VARI_V_INIT'
        TABLES
          p_varivar = gt_varivar.

      DELETE gt_varivar WHERE runt_fb = 'RS_VARI_V_TODAY_XWD'.
      DELETE gt_varivar WHERE runt_fb = 'RS_VARI_V_WDAYS_UP_TO_NOW'.
      DELETE gt_varivar WHERE runt_fb = 'RS_VARI_V_XWD_ACTUAL_MONTH'.
    ENDIF.
    retval = gt_varivar.
  ENDMETHOD.


  METHOD load_variant.
    READ TABLE gs_variant_cache-list  WITH KEY  uname    = i_variant_user
                                                variant  = i_variant
                                      BINARY SEARCH ASSIGNING FIELD-SYMBOL(<fs_cache>).
    IF sy-subrc EQ 0.
      go_current_variant = <fs_cache>-object.
    ENDIF.

    IF go_current_variant IS NOT BOUND.
      RETURN.
    ENDIF.

    populate_screen_from_db( go_current_variant ).

  ENDMETHOD.


  METHOD populate_dyn_date.
    TYPES ty_t_rsdatrange TYPE STANDARD TABLE OF rsdatrange WITH DEFAULT KEY.

    DATA(lv_date)       = VALUE sydatum(  ).
    DATA(lt_daterange)  = VALUE ty_t_rsdatrange( ).
    DATA lt_intrange TYPE STANDARD TABLE OF rsintrange WITH DEFAULT KEY.
    lt_intrange = VALUE #( (  low   = is_value-low_dy
                              high  = is_value-high_dy  ) ).

    CLEAR retval.

    CALL FUNCTION is_value-vname
      IMPORTING
        p_date     = lv_date
      TABLES
        p_datetab  = lt_daterange
        p_intrange = lt_intrange.

    IF lines( lt_daterange ) > 0.
      READ TABLE lt_daterange INTO retval INDEX 1.
    ELSE.
      retval-low = lv_date.
    ENDIF.

    retval-sign    = is_value-sign_dy.
    retval-option  = is_value-option_dy.

  ENDMETHOD.


  METHOD populate_screen_from_db.
    DATA(ls_rng_date) = VALUE rsdatrange(  ).
    DATA(lt_fields) = VALUE if_wd_select_options=>tt_selection_screen_item(  ).
    FIELD-SYMBOLS <fs_field_data> LIKE LINE OF lt_fields.

    IF io_variant IS SUPPLIED AND io_variant IS BOUND.
      DATA(lt_db_fields) = io_variant->get_variant_fields(  ).
      FIELD-SYMBOLS <fs_db_field> LIKE LINE OF lt_db_fields.
      SORT lt_db_fields BY paramid ASCENDING.
    ENDIF.

    go_sel_helper->get_parameter_fields( IMPORTING et_fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING <fs_field_data>.
      CHECK <fs_field_data>-m_read_only EQ abap_false.

      DATA(lo_value) = go_sel_helper->get_value_of_parameter_field( <fs_field_data>-m_id ).
      IF lo_value IS NOT BOUND.
        CREATE DATA lo_value TYPE tvarv_low.
      ENDIF.
      ASSIGN lo_value->* TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0 AND <fs_value> IS ASSIGNED.
        CLEAR <fs_value>.
      ENDIF.

      READ TABLE lt_db_fields WITH KEY paramid = <fs_field_data>-m_id
                              BINARY SEARCH ASSIGNING <fs_db_field>.
      IF sy-subrc EQ 0.
        IF <fs_db_field>-vname IS NOT INITIAL.
          ls_rng_date = populate_dyn_date( <fs_db_field> ).
          <fs_value> = ls_rng_date-low.
        ELSE.
          <fs_value> = <fs_db_field>-low.
        ENDIF.

        CALL METHOD go_sel_helper->set_value_of_parameter_field
          EXPORTING
            i_id    = <fs_field_data>-m_id
            i_value = lo_value.
      ENDIF.
      UNASSIGN <fs_db_field>.


    ENDLOOP.
    UNASSIGN <fs_field_data>.

    go_sel_helper->get_selection_fields( IMPORTING et_fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING <fs_field_data>.
      CHECK <fs_field_data>-m_read_only EQ abap_false.

      DATA(lo_rng_table) = go_sel_helper->get_range_table_of_sel_field( <fs_field_data>-m_id ).
      FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE .
      ASSIGN lo_rng_table->* TO <fs_table>.
      CLEAR <fs_table>.

      READ TABLE lt_db_fields WITH KEY paramid = <fs_field_data>-m_id
                              BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        LOOP AT lt_db_fields ASSIGNING <fs_db_field> FROM sy-tabix.
          IF <fs_db_field>-paramid NE <fs_field_data>-m_id.
            EXIT.
          ENDIF.

          IF <fs_db_field>-vname IS NOT INITIAL.
            ls_rng_date = populate_dyn_date( <fs_db_field> ).
            MOVE-CORRESPONDING ls_rng_date TO <fs_db_field>.
          ENDIF.

          APPEND INITIAL LINE TO <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row>).
          MOVE-CORRESPONDING <fs_db_field> TO <fs_row>.
        ENDLOOP.
        UNASSIGN <fs_db_field>.

        CALL METHOD go_sel_helper->set_range_table_of_sel_field
          EXPORTING
            i_id           = <fs_field_data>-m_id
            it_range_table = lo_rng_table.
      ELSE.
        CALL METHOD go_sel_helper->reset_selection_field
          EXPORTING
            i_id = <fs_field_data>-m_id.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_field_data>.

  ENDMETHOD.


  METHOD pull_select_option_texts.
    DATA lt_fields TYPE if_wd_select_options=>tt_selection_screen_item.
    FIELD-SYMBOLS <fs_field> LIKE LINE OF lt_fields.

    go_sel_helper->get_parameter_fields( IMPORTING et_fields = lt_fields ).
    go_sel_helper->get_selection_fields( IMPORTING et_fields = DATA(lt_sel_fields) ).
    APPEND LINES OF lt_sel_fields TO lt_fields.

    et_fields = CORRESPONDING #( lt_fields MAPPING paramid = m_id kind = m_type ).

    SORT et_fields BY paramid kind.

  ENDMETHOD.


  METHOD pull_variant_list.
    IF gs_variant_cache-cache_date NE sy-datum OR gs_variant_cache-cache_time NE sy-uzeit.
      CLEAR gs_variant_cache .
    ENDIF.

    IF gs_variant_cache IS INITIAL.
      gs_variant_cache-cache_date = sy-datum.
      gs_variant_cache-cache_time = sy-uzeit.

      DATA(lt_list_buffer) = zcl_wdc__variant=>get_variant_list( gs_variant_key ).
      SORT lt_list_buffer BY uname variant.

      IF go_variant_helper IS BOUND.
        DATA(lt_keys) = CORRESPONDING zcl_wdc__variant=>ty_t_variant_keys( lt_list_buffer ).

        TRY .
            go_variant_helper->before_display( CHANGING c_variant_list = lt_keys ).
          CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
            "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
            "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
            "If they didn't implement, they want default behaviours
            IF lo_error->textid NE cx_sy_dyn_call_illegal_method=>method_not_implemented.
              RAISE EXCEPTION lo_error.
            ENDIF.
        ENDTRY.

        IF lt_keys IS NOT INITIAL.
          LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<fs_key>).
            READ TABLE lt_list_buffer WITH KEY  parent_component = <fs_key>-parent_component
                                                component_usage  = <fs_key>-component_usage
                                                uname            = <fs_key>-uname
                                                variant          = <fs_key>-variant
                                      BINARY SEARCH ASSIGNING FIELD-SYMBOL(<fs_cache>).
            IF sy-subrc EQ 0.
              APPEND <fs_cache> TO gs_variant_cache-list.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fs_key>.
        ENDIF.
      ELSE.
        APPEND LINES OF lt_list_buffer TO gs_variant_cache-list.
      ENDIF.
    ENDIF.

    APPEND LINES OF gs_variant_cache-list TO r_list.

  ENDMETHOD.


  METHOD save_variant_to_db.
    DATA(ls_variant_key) = gs_variant_key.
    ls_variant_key-variant = i_variant.
    DATA(lv_current_user) = ls_variant_key-uname.

    DATA lt_fields TYPE zcl_wdc__variant=>ty_t_db_import_struct.
    lt_fields = CORRESPONDING #( it_field_info ).

    DATA(lo_variant) = zcl_wdc__variant=>get_variant( ls_variant_key ).
    IF lo_variant IS NOT BOUND.
      lo_variant = zcl_wdc__variant=>new_variant( is_key   = ls_variant_key
                                                 i_uname  = lv_current_user ).
    ENDIF.
    lo_variant->set_description( i_description ).
    lo_variant->set_variant_fields( lt_fields ).

    lo_variant->save_to_db( i_uname           = lv_current_user
                            i_save_as_global  = i_is_global ).

    go_current_variant = lo_variant.

    IF go_variant_helper IS BOUND.
      TRY.
          go_variant_helper->post_save( i_variant_key = CORRESPONDING #( go_current_variant->get_header(  ) )
                                        i_was_deleted = abap_false ).
        CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
          "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
          "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
          "If they didn't implement, they want default behaviours
          IF lo_error->textid NE cx_sy_dyn_call_illegal_method=>method_not_implemented.
            RAISE EXCEPTION lo_error.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD selopt_field_is_date.
    DATA(lv_kind) = VALUE abap_typekind(  ).

    IF i_kind EQ if_wd_select_options=>mc_sscreen_item_type_parameter.
      DATA(lo_data) = go_sel_helper->get_value_of_parameter_field( |{ i_paramid }| ).
      IF lo_data IS BOUND.
        lv_kind = cl_abap_elemdescr=>get_data_type_kind( lo_data ).
      ENDIF.

    ELSEIF i_kind EQ if_wd_select_options=>mc_sscreen_item_type_field.
      DATA(lo_range) = go_sel_helper->get_range_table_of_sel_field( |{ i_paramid }| ).

      FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE .
      ASSIGN lo_range->* TO <fs_table>.

      READ TABLE <fs_table> INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_value>).
      ENDIF.

      IF <fs_value> IS ASSIGNED.
        lv_kind = cl_abap_elemdescr=>get_data_type_kind( <fs_value> ).
      ENDIF.

    ENDIF.

    retval = COND #( WHEN lv_kind EQ cl_abap_elemdescr=>typekind_date THEN abap_true ELSE abap_false ).
  ENDMETHOD.


  METHOD set_header_data.
    gs_variant_key-parent_component = i_parent_component.
    gs_variant_key-component_usage  = i_componentusage_name.
    gs_variant_key-uname            = i_uname.

  ENDMETHOD.


  METHOD set_selopt_helper.
    go_sel_helper = i_helper.
  ENDMETHOD.


  METHOD set_variant_as_default.
    delete_current_default(  ).

    IF i_variant IS NOT INITIAL.
      zcl_wdc__variant=>default_set(
          is_variant            = VALUE #( parent_component  = gs_variant_key-parent_component
                                           component_usage   = gs_variant_key-component_usage
                                           uname             = gs_variant_key-uname
                                           variant           = i_variant )
          iv_variant_is_global  = i_is_global ).
    ENDIF.
  ENDMETHOD.


  METHOD set_variant_helper.
    go_variant_helper = i_helper.
  ENDMETHOD.


  METHOD transform_screen_to_db.
    DATA:
      lt_insert_buffer TYPE zcl_wdc__variant=>ty_t_db_import_struct,
      ls_insert_buffer LIKE LINE OF lt_insert_buffer,
      lt_fields        TYPE if_wd_select_options=>tt_selection_screen_item
      .

    go_sel_helper->get_parameter_fields( IMPORTING et_fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field_data>).
      FIELD-SYMBOLS <fs_value> TYPE any.
      ASSIGN <fs_field_data>-m_value->* TO <fs_value>.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO lt_insert_buffer ASSIGNING FIELD-SYMBOL(<fs_insert>).
        <fs_insert>-paramid = <fs_field_data>-m_id.
        <fs_insert>-kind    = <fs_field_data>-m_type.
        <fs_insert>-low     = <fs_value>.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_field_data>.

    CLEAR lt_fields.
    go_sel_helper->get_selection_fields( IMPORTING et_fields = lt_fields ).
    LOOP AT lt_fields ASSIGNING <fs_field_data>.
      ls_insert_buffer-paramid  = <fs_field_data>-m_id.
      ls_insert_buffer-kind     = <fs_field_data>-m_type.

      FIELD-SYMBOLS <fs_range_table> TYPE ANY TABLE.
      UNASSIGN <fs_range_table>.

      ASSIGN <fs_field_data>-mt_range_table->* TO <fs_range_table>.
      IF sy-subrc EQ 0.
        LOOP AT <fs_range_table> ASSIGNING FIELD-SYMBOL(<fs_range_row>).
          APPEND INITIAL LINE TO lt_insert_buffer ASSIGNING <fs_insert>.
          <fs_insert>-paramid = <fs_field_data>-m_id.
          <fs_insert>-kind    = <fs_field_data>-m_type.
          MOVE-CORRESPONDING <fs_range_row> TO <fs_insert>.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_field_data>.

    APPEND LINES OF lt_insert_buffer TO ct_variant_fields.

  ENDMETHOD.


  METHOD user_can_create_glbl_variants.
    IF go_variant_helper IS BOUND.
      TRY.
          retval = go_variant_helper->user_can_create_glbl_variants( i_current_user = gs_variant_key-uname ).
        CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
          "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
          "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
          "If they didn't implement, they want default behaviours
          IF lo_error->textid EQ cx_sy_dyn_call_illegal_method=>method_not_implemented.
            retval = abap_true.
          ELSE.
            RAISE EXCEPTION lo_error.
          ENDIF.
      ENDTRY.
    ELSE.
      retval = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD user_can_create_self_variants.
    IF go_variant_helper IS BOUND.
      TRY.
          retval = go_variant_helper->user_can_create_self_variants( i_current_user = gs_variant_key-uname ).
        CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
          "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
          "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
          "If they didn't implement, they want default behaviours
          IF lo_error->textid EQ cx_sy_dyn_call_illegal_method=>method_not_implemented.
            retval = abap_true.
          ELSE.
            RAISE EXCEPTION lo_error.
          ENDIF.
      ENDTRY.
    ELSE.
      retval = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD varaints_enabled_for_user.
    IF go_variant_helper IS BOUND.
      TRY .
          retval = go_variant_helper->varaints_enabled_for_user( i_current_user = gs_variant_key-uname ).
        CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error) .
          "Catch and release because of ATC.  9.9999999 out of 10 times, METHOD_NOT_IMPLEMENTED will be thrown
          "This means that the interface didn't implement the method (new with 7.4ish), so just process as normal.
          "If they didn't implement, they want default behaviours
          IF lo_error->textid EQ cx_sy_dyn_call_illegal_method=>method_not_implemented.
            retval = abap_true.
          ELSE.
            RAISE EXCEPTION lo_error.
          ENDIF.
      ENDTRY.
    ELSE.
      retval = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD variant_can_be_deleted_by_user.
    CHECK i_variant_user IS NOT INITIAL.

    READ TABLE gs_variant_cache-list  WITH KEY  uname    = i_variant_user
                                                variant  = i_variant
                                      BINARY SEARCH ASSIGNING FIELD-SYMBOL(<fs_cache>).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    retval = abap_true.

    IF  <fs_cache>-object->is_global_variant( ) EQ abap_true  AND
        user_can_create_glbl_variants( )        EQ abap_false.
      retval = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
