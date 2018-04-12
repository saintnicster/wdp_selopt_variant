CLASS zcl_wdc__variant DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_db_import_struct TYPE zdv_wdc__varaint_db .
    TYPES:
*        m_id   TYPE if_wd_select_options=>t_selection_screen_item-m_id,
*        m_type TYPE if_wd_select_options=>t_selection_screen_item-m_type,
*        sign   TYPE tvarv_sign,
*        option TYPE tvarv_opti,
*        low    TYPE tvarv_low,
*        high   TYPE tvarv_hgh,
*        vtype  TYPE rsvar_svar,
*        sign_dy   TYPE tvarv_sign,
*        option_dy TYPE tvarv_opti,
*        low_dy    TYPE tvarv_low,
*        high_dy   TYPE tvarv_hgh,
*        vname     TYPE raldb_vnam,
*      END OF ty_db_import_struct .
      ty_t_db_import_struct TYPE STANDARD TABLE OF ty_db_import_struct WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_variant_list.
            INCLUDE TYPE zwdc__variant_key.
    TYPES:
      object     TYPE REF TO zcl_wdc__variant,
      is_default TYPE abap_bool,
      END OF ty_variant_list .
    TYPES:
      ty_t_variant_list TYPE STANDARD TABLE OF ty_variant_list WITH DEFAULT KEY .
    TYPES:
      ty_t_variant_keys TYPE STANDARD TABLE OF zwdc__variant_key WITH DEFAULT KEY .

    CONSTANTS c_all_users TYPE syuname VALUE '********' ##NO_TEXT.

    CLASS-METHODS default_set
      IMPORTING
        !is_variant           TYPE zwdc__variant_key
        !iv_variant_is_global TYPE wdy_boolean .
    CLASS-METHODS default_remove
      IMPORTING
        !is_variant TYPE zwdc__variant_key .
    CLASS-METHODS default_get
      IMPORTING
        !iv_parent_component TYPE wdy_component_name
        !iv_component_usage  TYPE wdy_wb_componentusage_name
        !iv_current_user     TYPE uname
      EXPORTING
        !ev_variant          TYPE variant
        !ev_is_global        TYPE wdy_boolean .
    CLASS-METHODS check_existence
      IMPORTING
        !is_key       TYPE zwdc__variant_key
        !i_is_global  TYPE wdy_boolean OPTIONAL
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    CLASS-METHODS get_variant_list
      IMPORTING
        !i_variant_key TYPE zwdc__variant_key
      RETURNING
        VALUE(r_list)  TYPE zcl_wdc__variant=>ty_t_variant_list .
    CLASS-METHODS get_variant
      IMPORTING
        !is_key       TYPE zwdc__variant_key
      RETURNING
        VALUE(retval) TYPE REF TO zcl_wdc__variant .
    CLASS-METHODS new_variant
      IMPORTING
        !is_key       TYPE zwdc__variant_key
        !i_uname      TYPE uname
      RETURNING
        VALUE(retval) TYPE REF TO zcl_wdc__variant .
    METHODS save_to_db
      IMPORTING
        !i_uname          TYPE syuname DEFAULT sy-uname
        !i_save_as_global TYPE wdy_boolean DEFAULT abap_false .
    METHODS get_variant_fields
      IMPORTING
        !i_bypass_cache TYPE wdy_boolean DEFAULT abap_false
      RETURNING
        VALUE(retval)   TYPE ty_t_db_import_struct .
    METHODS set_description
      IMPORTING
        !i_description TYPE text25 .
    METHODS set_variant_fields
      IMPORTING
        !it_fields TYPE zcl_wdc__variant=>ty_t_db_import_struct .
    METHODS is_global_variant
      RETURNING
        VALUE(retval) TYPE wdy_boolean .
    METHODS get_header
      RETURNING
        VALUE(retval) TYPE zwdc__variant .
    METHODS delete .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_variant_hd TYPE zwdc__variant .
    DATA gt_variant_fields TYPE ty_t_db_import_struct .

    METHODS constructor
      IMPORTING
        !is_variant TYPE zwdc__variant .
ENDCLASS.



CLASS ZCL_WDC__VARIANT IMPLEMENTATION.


  METHOD check_existence.
    CLEAR retval .

    DATA(ls_key) = is_key.

    IF i_is_global IS SUPPLIED AND i_is_global EQ abap_true.
      ls_key-uname = c_all_users.
    ENDIF.

    SELECT COUNT(*) FROM zwdc__variant
      WHERE
        parent_component  = ls_key-parent_component AND
        component_usage   = ls_key-component_usage  AND
        uname             = ls_key-uname            AND
        variant           = ls_key-variant.
    IF sy-subrc EQ 0 AND sy-dbcnt NE 0.
      retval = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    gs_variant_hd = is_variant.

  ENDMETHOD.


  METHOD default_get.
    SELECT SINGLE variant is_global FROM zwdc__varintdflt INTO (ev_variant, ev_is_global)
      WHERE
        parent_component  = iv_parent_component AND
        component_usage   = iv_component_usage  AND
        uname             = iv_current_user.
    IF sy-subrc NE 0 .
      CLEAR: ev_variant, ev_is_global.
    ENDIF.

  ENDMETHOD.


  METHOD default_remove.
    SELECT COUNT(*) FROM zwdc__varintdflt
      WHERE
        parent_component  = is_variant-parent_component AND
        component_usage   = is_variant-component_usage  AND
        uname             = is_variant-uname.
    IF sy-subrc EQ 0 AND sy-dbcnt EQ 1.
      UPDATE zwdc__varintdflt SET variant = space is_global = space
        WHERE
          parent_component  = is_variant-parent_component AND
          component_usage   = is_variant-component_usage  AND
          uname             = is_variant-uname.
      IF sy-subrc EQ 0.
        MESSAGE text-s01 TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD default_set.
    DATA(ls_default) = CORRESPONDING zwdc__varintdflt( is_variant ).
    ls_default-is_global  = iv_variant_is_global.

    SELECT COUNT(*) FROM zwdc__varintdflt
      WHERE
        parent_component  = is_variant-parent_component AND
        component_usage   = is_variant-component_usage  AND
        uname             = is_variant-uname.
    IF sy-subrc EQ 0 AND sy-dbcnt EQ 1.
      UPDATE zwdc__varintdflt
        FROM ls_default.
    ELSE.
      INSERT zwdc__varintdflt FROM ls_default.
    ENDIF.

    IF sy-subrc EQ 0 AND sy-dbcnt EQ 1.
      MESSAGE text-s02 TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD delete.
    DELETE zwdc__variant FROM gs_variant_hd.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA ls_variant_key TYPE zwdc__variant_key.
    MOVE-CORRESPONDING gs_variant_hd TO ls_variant_key.

    DELETE FROM DATABASE zwdc__variantraw(wd)
                ID ls_variant_key.

  ENDMETHOD.


  METHOD get_header.
    retval = gs_variant_hd.

  ENDMETHOD.


  METHOD get_variant.
    DATA ls_variant_buffer TYPE zwdc__variant.

    SELECT SINGLE * FROM zwdc__variant
      INTO ls_variant_buffer
      WHERE
        parent_component  = is_key-parent_component AND
        component_usage   = is_key-component_usage  AND
        uname             = is_key-uname            AND
        variant           = is_key-variant.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.


    CREATE OBJECT retval
      EXPORTING
        is_variant = ls_variant_buffer.

    retval->get_variant_fields( ).


  ENDMETHOD.


  METHOD get_variant_fields.
    IF gt_variant_fields IS INITIAL OR i_bypass_cache EQ abap_true.
      DATA ls_variant_key TYPE zwdc__variant_key.

      MOVE-CORRESPONDING gs_variant_hd TO ls_variant_key.

      IMPORT gt_variant_fields = gt_variant_fields
         FROM DATABASE zwdc__variantraw(wd)
                    ID ls_variant_key.
    ENDIF.

    retval = gt_variant_fields.

  ENDMETHOD.


  METHOD get_variant_list.
    DATA:
      lr_user   TYPE RANGE OF sy-uname,
      lt_buffer TYPE STANDARD TABLE OF zwdc__variant WITH DEFAULT KEY
      .

    APPEND VALUE #( sign = 'I' option = 'EQ' low = c_all_users ) TO lr_user.
    IF i_variant_key-uname IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_variant_key-uname ) TO lr_user.
    ENDIF.

    SELECT * FROM zwdc__variant
      INTO TABLE lt_buffer
      WHERE
        parent_component = i_variant_key-parent_component AND
        component_usage  = i_variant_key-component_usage  AND
        uname           IN lr_user.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL METHOD zcl_wdc__variant=>default_get
      EXPORTING
        iv_parent_component = i_variant_key-parent_component
        iv_component_usage  = i_variant_key-component_usage
        iv_current_user     = i_variant_key-uname
      IMPORTING
        ev_variant          = DATA(lv_default_variant)
        ev_is_global        = DATA(lv_is_global).


    LOOP AT lt_buffer ASSIGNING FIELD-SYMBOL(<fs_buffer>).
      APPEND CORRESPONDING #( <fs_buffer> ) TO r_list ASSIGNING FIELD-SYMBOL(<fs_r_list>).

      IF <fs_r_list>-variant EQ lv_default_variant.
        IF ( <fs_r_list>-uname EQ c_all_users AND lv_is_global EQ abap_true ) OR
           ( <fs_r_list>-uname EQ i_variant_key-uname AND lv_is_global EQ abap_false ).
          <fs_r_list>-is_default = abap_true.
        ENDIF.
      ENDIF.

      CREATE OBJECT <fs_r_list>-object
        EXPORTING
          is_variant = <fs_buffer>.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_global_variant.
    retval = COND #( WHEN gs_variant_hd-uname EQ c_all_users THEN abap_true
                                                             ELSE abap_false ).

  ENDMETHOD.


  METHOD new_variant.
    IF zcl_wdc__variant=>check_existence( is_key ) NE abap_true.
      DATA ls_variant_buffer LIKE gs_variant_hd.
      MOVE-CORRESPONDING is_key TO ls_variant_buffer.

      CREATE OBJECT retval
        EXPORTING
          is_variant = ls_variant_buffer.

      retval->gs_variant_hd-erdat = sy-datum.
      retval->gs_variant_hd-ernam = i_uname.
    ENDIF.
  ENDMETHOD.


  METHOD save_to_db.
    DATA ls_variant_key TYPE zwdc__variant_key.

    IF i_save_as_global EQ abap_true.
      gs_variant_hd-uname = c_all_users.
    ENDIF.

    gs_variant_hd-aedat = sy-datum.
    gs_variant_hd-aenam = i_uname.

    MODIFY zwdc__variant FROM gs_variant_hd.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING gs_variant_hd TO ls_variant_key.

    IF gt_variant_fields IS NOT INITIAL.
      EXPORT gt_variant_fields = gt_variant_fields
         TO DATABASE zwdc__variantraw(wd)
                  ID ls_variant_key.
    ELSE.
      DELETE FROM DATABASE zwdc__variantraw(wd)
                  ID ls_variant_key.
    ENDIF.

  ENDMETHOD.


  METHOD set_description.
    gs_variant_hd-description = i_description.

  ENDMETHOD.


  METHOD set_variant_fields.
    gt_variant_fields = it_fields.

  ENDMETHOD.
ENDCLASS.
