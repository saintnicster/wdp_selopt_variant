CLASS zcl_wdc__util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_WDC__UTIL
*"* do not include other source files here!!!
  PUBLIC SECTION.

    CONSTANTS enabled TYPE zwdc__guiprop VALUE '0011' ##NO_TEXT.
    CONSTANTS invisible TYPE zwdc__guiprop VALUE '0001' ##NO_TEXT.
    CONSTANTS required TYPE zwdc__guiprop VALUE '1011' ##NO_TEXT.
    CONSTANTS readonly TYPE zwdc__guiprop VALUE '0111' ##NO_TEXT.
    CONSTANTS disabled TYPE zwdc__guiprop VALUE '0010' ##NO_TEXT.
    CONSTANTS required_readonly TYPE zwdc__guiprop VALUE '1111' ##NO_TEXT.

    CLASS-METHODS gui_prop_fill_table
      IMPORTING
        !i_struct         TYPE any
      RETURNING
        VALUE(r_prop_tab) TYPE wdr_context_properties_tab .
    CLASS-METHODS gui_prop_set_all_attr
      IMPORTING
        !i_prop   TYPE zwdc__guiprop
      CHANGING
        !c_struct TYPE any .
    CLASS-METHODS gui_prop_read_table
      IMPORTING
        !i_prop_tab TYPE wdr_context_properties_tab
      CHANGING
        !c_struct   TYPE any .
    CLASS-METHODS gui_prop_set_to_readonly
      CHANGING
        !c_struct TYPE any .
    CLASS-METHODS set_read_only
      IMPORTING
        !i_root TYPE REF TO if_wd_view_element .
    CLASS-METHODS get_gui_property_for_element
      IMPORTING
        !i_prop_tab   TYPE wdr_context_properties_tab
        !i_comp       TYPE string
      RETURNING
        VALUE(r_prop) TYPE zwdc__guiprop .
  PROTECTED SECTION.
*"* protected components of class ZCL_WDC__UTIL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_WDC__UTIL
*"* do not include other source files here!!!

    TYPE-POOLS abap .
    CLASS-METHODS gui_prop_get_attr_name
      IMPORTING
        !i_comp       TYPE abap_compdescr_tab
        !i_index      TYPE sy-index
      RETURNING
        VALUE(r_name) TYPE abap_compname .
ENDCLASS.



CLASS ZCL_WDC__UTIL IMPLEMENTATION.


  METHOD get_gui_property_for_element.

    DATA: ls_prop TYPE wdr_context_properties,
          lv_prop TYPE zwdc__guiprop.

    READ TABLE i_prop_tab INTO ls_prop WITH KEY attribute_name = i_comp.
    IF sy-subrc = 0.
      lv_prop(1)   = ls_prop-required.
      lv_prop+1(1) = ls_prop-read_only.
      lv_prop+2(1) = ls_prop-visible.
      lv_prop+3(1) = ls_prop-enabled.
      TRANSLATE lv_prop USING 'X1 0'.
      r_prop = lv_prop.
    ENDIF.

  ENDMETHOD.


  METHOD gui_prop_fill_table.

    FIELD-SYMBOLS: <fs_comp>  TYPE any,
                   <fs_struc> TYPE any.

    DATA: lo_ref      TYPE REF TO cl_abap_structdescr,
          ls_property TYPE wdr_context_properties,
          ls_comp     TYPE zwdc__guiprop.

    ASSIGN i_struct TO <fs_struc>.
    lo_ref ?= cl_abap_typedescr=>describe_by_data( <fs_struc> ).

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_struc> TO <fs_comp>.
      IF sy-subrc <> 0. EXIT. ENDIF.
      ls_comp = <fs_comp>.
      TRANSLATE ls_comp USING '1X0 '.
      ls_property-attribute_name = gui_prop_get_attr_name( i_comp  = lo_ref->components
                                                  i_index = sy-index ).
      ls_property-required      = ls_comp(1).
      ls_property-read_only     = ls_comp+1(1).
      ls_property-visible       = ls_comp+2(1).
      ls_property-enabled       = ls_comp+3(1).
      APPEND ls_property TO r_prop_tab.
    ENDDO.

  ENDMETHOD.


  METHOD gui_prop_get_attr_name.

    DATA: ls_comp TYPE abap_compdescr.

    READ TABLE i_comp INDEX i_index INTO ls_comp.
    r_name = ls_comp-name.

  ENDMETHOD.


  METHOD gui_prop_read_table.

    FIELD-SYMBOLS: <fs_comp>  TYPE any,
                   <fs_struc> TYPE any.

    DATA: ls_prop         TYPE wdr_context_properties.

    ASSIGN c_struct TO <fs_struc>.

    LOOP AT i_prop_tab INTO ls_prop.
      ASSIGN COMPONENT ls_prop-attribute_name OF STRUCTURE <fs_struc> TO <fs_comp>.
      IF sy-subrc <> 0. CONTINUE. ENDIF.
      <fs_comp>(1)   = ls_prop-required.
      <fs_comp>+1(1) = ls_prop-read_only.
      <fs_comp>+2(1) = ls_prop-visible.
      <fs_comp>+3(1) = ls_prop-enabled.
      TRANSLATE <fs_comp> USING 'X1 0'.
    ENDLOOP.

  ENDMETHOD.


  METHOD gui_prop_set_all_attr.

    FIELD-SYMBOLS: <fs_comp>  TYPE any,
                   <fs_struc> TYPE any.

    ASSIGN c_struct TO <fs_struc>.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_struc> TO <fs_comp>.
      IF sy-subrc <> 0. EXIT. ENDIF.
      <fs_comp> = i_prop.
    ENDDO.

  ENDMETHOD.


  METHOD gui_prop_set_to_readonly.

    FIELD-SYMBOLS: <fs_comp>  TYPE any,
                   <fs_struc> TYPE any.

    ASSIGN c_struct TO <fs_struc>.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_struc> TO <fs_comp>.
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF <fs_comp> IS INITIAL OR <fs_comp> = enabled.
        <fs_comp> = readonly.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD set_read_only.

    DATA: lo_element      TYPE REF TO cl_wdr_view_element,
          lo_ui_container TYPE REF TO cl_wd_uielement_container,
          lt_children     TYPE cl_wd_uielement=>tt_uielement,
          lo_ui_element   TYPE REF TO cl_wd_uielement.


    lo_element ?= i_root.
    IF lo_element->_is_ui_element_container = abap_true.         "Is this a container?
      lo_ui_container  ?= i_root.
      lt_children      = lo_ui_container->get_children( ).
      LOOP AT lt_children INTO lo_ui_element.
        set_read_only( lo_ui_element ).
      ENDLOOP.

    ELSE.                                                       "... or is it a UI element
      CHECK lo_element->_has_read_only = abap_true.             "Does this UI element have a read only property?
      CASE lo_element->_definition_name.
        WHEN 'INPUT_FIELD'.
          DATA: lo_ui_input_field  TYPE REF TO cl_wd_abstract_input_field.
          lo_ui_input_field ?= i_root.
          lo_ui_input_field->set_read_only( abap_true ).
        WHEN 'DROPDOWN_BY_IDX' OR 'DROPDOWN_BY_KEY'.
          DATA: lo_dropdown TYPE REF TO cl_wd_abstract_dropdown.
          lo_dropdown ?= i_root.
          lo_dropdown->set_read_only( abap_true ).
        WHEN 'TEXT_EDIT'.
          DATA: lo_text_edit TYPE REF TO cl_wd_text_edit.
          lo_text_edit ?= i_root.
          lo_text_edit->set_read_only( abap_true ).
      ENDCASE.
    ENDIF.





  ENDMETHOD.
ENDCLASS.
