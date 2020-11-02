*---------------------------------------------------------------------------------*
*  INDEVO.PL                                                                      *
*                                                                                 *
*  abapDOC                                                                        *
*  Documentation generator for repository objects                                 *
*                                                                                 *
*  Version: 1.0.1                                                                   *
*  Date: 04.06.2020                                                               *
*---------------------------------------------------------------------------------*
*  MIT License                                                                    *
*                                                                                 *
*  Copyright (c) 2020 Jacek Kopcinski (indevo.pl)                                 *
*                                                                                 *
*  Permission is hereby granted, free of charge, to any person obtaining a copy   *
*  of this software and associated documentation files (the "Software"), to deal  *
*  in the Software without restriction, including without limitation the rights   *
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
*  copies of the Software, and to permit persons to whom the Software is          *
*  furnished to do so, subject to the following conditions:                       *
*                                                                                 *
*  The above copyright notice and this permission notice shall be included in all *
*  copies or substantial portions of the Software.                                *
*                                                                                 *
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
*  SOFTWARE.                                                                      *
*---------------------------------------------------------------------------------*

REPORT  zid_abap_doc.

TABLES:
  sci_dynp.

"$. Region UTILITIES

CLASS lcl_config DEFINITION.
  PUBLIC SECTION.
    METHODS add_param IMPORTING i_name TYPE clike i_value TYPE any.
    METHODS get_param IMPORTING i_name TYPE clike
      RETURNING value(r_value) TYPE REF TO data.
    METHODS get_param_boolean IMPORTING i_name TYPE clike
      RETURNING value(r_value) TYPE abap_bool.
    METHODS get_param_string IMPORTING i_name TYPE clike
      RETURNING value(r_value) TYPE string.
    METHODS has_param IMPORTING i_name TYPE clike
      RETURNING value(r_value) TYPE abap_bool.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_entry,
        name TYPE string,
        value TYPE REF TO data,
      END OF ty_entry,
      ty_entries_ts TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY name.
    DATA:
      entries TYPE ty_entries_ts.
ENDCLASS.

CLASS lcl_config IMPLEMENTATION.
  METHOD add_param.
    DATA:
      ls_entry TYPE ty_entry.

    ls_entry-name = i_name.
    GET REFERENCE OF i_value INTO ls_entry-value.
    INSERT ls_entry INTO TABLE entries.
  ENDMETHOD.

  METHOD get_param.
    DATA:
      ls_entry TYPE ty_entry.

    READ TABLE entries INTO ls_entry WITH TABLE KEY name = i_name.
    CHECK sy-subrc = 0.
    r_value = ls_entry-value.
  ENDMETHOD.

  METHOD get_param_boolean.
    DATA:
      ref TYPE REF TO abap_bool.

    ref ?= get_param( i_name ).
    CHECK ref IS BOUND.
    r_value = ref->*.
  ENDMETHOD.

  METHOD get_param_string.
    DATA:
      ref TYPE REF TO data.
    FIELD-SYMBOLS:
      <text> TYPE any.

    ref ?= get_param( i_name ).
    CHECK ref IS BOUND.
    ASSIGN ref->* TO <text>.
    r_value = <text>.
  ENDMETHOD.

  METHOD has_param.
    DATA:
      ls_entry TYPE ty_entry.

    READ TABLE entries INTO ls_entry WITH TABLE KEY name = i_name.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml_doc DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: create RETURNING value(r_doc) TYPE REF TO lcl_xml_doc.
    CLASS-METHODS: create_from_data IMPORTING i_root TYPE clike i_data TYPE any
      RETURNING value(r_doc) TYPE REF TO lcl_xml_doc.
    METHODS add_doc IMPORTING i_xml_doc TYPE REF TO lcl_xml_doc.
    METHODS append_data_element IMPORTING i_name TYPE clike i_data TYPE any
      RETURNING value(r_element) TYPE REF TO if_ixml_element.
    METHODS append_element IMPORTING i_element TYPE REF TO if_ixml_element.
    METHODS append_simple IMPORTING i_name TYPE clike i_value TYPE clike OPTIONAL
      RETURNING value(r_element) TYPE REF TO if_ixml_element.
    METHODS create_data_element IMPORTING i_name TYPE clike i_data TYPE any
      RETURNING value(r_element) TYPE REF TO if_ixml_element.
    METHODS create_simple IMPORTING i_name TYPE clike i_value TYPE clike OPTIONAL
      RETURNING value(r_element) TYPE REF TO if_ixml_element.
    METHODS is_empty RETURNING value(r_value) TYPE abap_bool.
    METHODS to_string RETURNING value(r_xml) TYPE string.
    METHODS to_xstring RETURNING value(r_xml) TYPE xstring.

  PRIVATE SECTION.
    METHODS constructor.
    DATA xml TYPE REF TO if_ixml.
    DATA doc TYPE REF TO if_ixml_document.
    DATA last_element TYPE REF TO if_ixml_element.
ENDCLASS.

CLASS lcl_xml_doc IMPLEMENTATION.
  METHOD constructor.
    xml = cl_ixml=>create( ).
    doc = xml->create_document( ).
  ENDMETHOD.

  METHOD create.
    CREATE OBJECT r_doc.
  ENDMETHOD.

  METHOD create_from_data.
    ASSERT i_root IS NOT INITIAL.
    CHECK i_data IS NOT INITIAL.

    r_doc = create( ).
    r_doc->append_data_element( i_name = i_root i_data = i_data ).
  ENDMETHOD.

  METHOD append_data_element.
    append_element( create_data_element( i_name = i_name i_data = i_data ) ).
  ENDMETHOD.

  METHOD create_data_element.
    DATA:
      lo_doc TYPE REF TO if_ixml_document,
      ls_stab TYPE abap_trans_srcbind,
      lt_stab TYPE abap_trans_srcbind_tab.

    ASSERT i_name IS NOT INITIAL.
    CHECK i_data IS NOT INITIAL.

    ls_stab-name = i_name.
    GET REFERENCE OF i_data INTO ls_stab-value.
    APPEND ls_stab TO lt_stab.

    lo_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML lo_doc.

    r_element = lo_doc->find_from_name( ls_stab-name ).
  ENDMETHOD.

  METHOD add_doc.
    doc->get_root_element( )->append_child( i_xml_doc->doc->get_root_element( ) ).
  ENDMETHOD.

  METHOD append_element.
    ASSERT i_element IS BOUND.
    IF last_element IS BOUND.
      last_element->append_child( i_element ).
    ELSE.
      doc->append_child( i_element ).
    ENDIF.
    last_element = i_element.
  ENDMETHOD.

  METHOD append_simple.
    DATA:
     lo_elem TYPE REF TO if_ixml_element.

    r_element = create_simple( i_name = i_name i_value = i_value ).
    append_element( r_element ).
  ENDMETHOD.

  METHOD create_simple.
    DATA:
      lv_value TYPE string.

    r_element = doc->create_element( i_name ).
    IF i_value IS NOT INITIAL.
      lv_value = i_value.
      r_element->set_value( lv_value ).
    ENDIF.
  ENDMETHOD.

  METHOD is_empty.
    IF doc->get_root_element( ) IS NOT BOUND.
      r_value = abap_true.
    ELSE.
      r_value = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD to_string.
    DATA:
      lo_ostream TYPE REF TO if_ixml_ostream,
      lo_renderer TYPE REF TO if_ixml_renderer,
      lv_mark TYPE string,
      lo_streamfactory TYPE REF TO if_ixml_stream_factory.

    lo_streamfactory = xml->create_stream_factory( ).
    lo_ostream = lo_streamfactory->create_ostream_cstring( r_xml ).
    lo_renderer = xml->create_renderer( ostream  = lo_ostream document = doc ).
    lo_renderer->set_normalizing( abap_true ).
    lo_renderer->render( ).
  ENDMETHOD.

  METHOD to_xstring.
    DATA:
      lo_ostream TYPE REF TO if_ixml_ostream,
      lo_renderer TYPE REF TO if_ixml_renderer,
      lv_mark TYPE string,
      lo_streamfactory TYPE REF TO if_ixml_stream_factory.

    lo_streamfactory = xml->create_stream_factory( ).
    lo_ostream = lo_streamfactory->create_ostream_xstring( r_xml ).
    lo_renderer = xml->create_renderer( ostream  = lo_ostream document = doc ).
    lo_renderer->set_normalizing( abap_true ).
    lo_renderer->render( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcx_abap_doc_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_msg TYPE csequence,
      if_message~get_text REDEFINITION,
      if_message~get_longtext REDEFINITION.
    CLASS-METHODS:
      raise IMPORTING i_msg TYPE csequence RAISING lcx_abap_doc_exception,
      raise_from_sy RAISING lcx_abap_doc_exception.
  PRIVATE SECTION.
    DATA:
       msg TYPE string.
ENDCLASS.

CLASS lcx_abap_doc_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    msg = i_msg.
  ENDMETHOD.

  METHOD raise.
    RAISE EXCEPTION TYPE lcx_abap_doc_exception
      EXPORTING
        i_msg = i_msg.
  ENDMETHOD.

  METHOD raise_from_sy.
    DATA:
      lv_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
    raise( lv_msg ).
  ENDMETHOD.

  METHOD if_message~get_text.
    result = msg.
  ENDMETHOD.

  METHOD if_message~get_longtext.
    result = msg.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_frontend DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      file_download IMPORTING i_path TYPE string i_xstring TYPE xstring,
      select_directory IMPORTING i_title TYPE clike RETURNING value(r_directory) TYPE string,
      show_file_save_dialog IMPORTING i_title TYPE string
          i_extension TYPE string
          i_default_filename TYPE string
        RETURNING value(r_path) TYPE string.
    CLASS-DATA:
      dir_sep.
ENDCLASS.

CLASS lcl_frontend IMPLEMENTATION.

  METHOD class_constructor.
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = dir_sep ).
  ENDMETHOD.

  METHOD file_download.
    DATA:
      lt_rawdata  TYPE solix_tab.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( i_xstring ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( i_xstring )
        filename                  = i_path
        filetype                  = 'BIN'
        show_transfer_status      = abap_false
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        OTHERS                    = 1 ).
    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.

  METHOD select_directory.
    DATA:
      lv_desktop_dir TYPE string.

    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING desktop_directory = lv_desktop_dir
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    cl_gui_cfw=>flush( ). "needed - communicates with the frontend to read desktop dir.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title = i_title
        initial_folder = lv_desktop_dir
      CHANGING
        selected_folder = r_directory
      EXCEPTIONS
        OTHERS = 0 ).
  ENDMETHOD.

  METHOD show_file_save_dialog.
    DATA:
      lv_action   TYPE i,
      lv_filename TYPE string,
      lv_path     TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = i_title
        default_extension    = i_extension
        default_file_name    = i_default_filename
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = r_path
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_type_info DEFINITION ABSTRACT FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_domain_value_text IMPORTING i_domain TYPE clike i_value TYPE clike
      RETURNING value(r_text) TYPE string.
    CLASS-METHODS get_type_descr IMPORTING i_obj_type TYPE clike RETURNING value(r_text) TYPE string.
    CLASS-METHODS visibility IMPORTING i_code TYPE clike RETURNING value(r_value) TYPE string.
    CLASS-METHODS level IMPORTING i_code TYPE clike RETURNING value(r_value) TYPE string.
    CLASS-METHODS param_kind IMPORTING i_code TYPE clike RETURNING value(r_value) TYPE string.
    CLASS-METHODS typing IMPORTING i_code TYPE clike RETURNING value(r_value) TYPE string.

    TYPES:
      BEGIN OF ty_attribute,
        name TYPE string,
        source TYPE string,
        inherited TYPE xsdboolean,
        description TYPE string,
        exposure TYPE i, "visibility code
        visibility TYPE string,
        typing TYPE string,
        type TYPE string,
        level TYPE string,"Static/Instance
        read_only TYPE xsdboolean,
        value TYPE string,
      END OF ty_attribute,
      ty_attributes_tt TYPE STANDARD TABLE OF ty_attribute WITH DEFAULT KEY,

      BEGIN OF ty_parameter,
        name TYPE string,
        description TYPE string,
        kind TYPE string,
        optional TYPE xsdboolean,
        default_value TYPE string,
        pass_by_value TYPE xsdboolean,
        typing TYPE string,
        type TYPE string,
      END OF ty_parameter,
      ty_parameters_tt TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY,

      BEGIN OF ty_exception,
        name TYPE string,
        description TYPE string,
        class_based TYPE xsdboolean,
      END OF ty_exception,
      ty_exceptions_tt TYPE STANDARD TABLE OF ty_exception WITH DEFAULT KEY,

      BEGIN OF ty_event,
        name TYPE string,
        source TYPE string,
        inherited TYPE xsdboolean,
        description TYPE string,
        level TYPE string,
        exposure TYPE i,
        visibility TYPE string,
        parameters TYPE ty_parameters_tt,
      END OF ty_event,
      ty_events_tt TYPE STANDARD TABLE OF ty_event WITH DEFAULT KEY,

      BEGIN OF ty_method,
        name TYPE string,
        source TYPE string,
        inherited TYPE xsdboolean,
        description TYPE string,
        exposure TYPE i, "visibility code
        visibility TYPE string,
        abstract TYPE xsdboolean,
        redefined TYPE xsdboolean,
        final TYPE xsdboolean,
        level TYPE string, "Static/Instance
        parameters TYPE ty_parameters_tt,
        exceptions TYPE ty_exceptions_tt,
      END OF ty_method,
      ty_methods_tt TYPE STANDARD TABLE OF ty_method WITH DEFAULT KEY,

      BEGIN OF ty_ddic_field,
        name TYPE string,
        description TYPE string,
        is_key TYPE xsdboolean,
        not_null TYPE xsdboolean,
        include TYPE xsdboolean,
        typing TYPE string,
        type TYPE string,
      END OF ty_ddic_field,
      ty_ddic_fields_tt TYPE STANDARD TABLE OF ty_ddic_field WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_type_info IMPLEMENTATION.
  METHOD get_domain_value_text.
    DATA:
      lv_domain TYPE dd07l-domname,
      lv_value TYPE dd07l-domvalue_l,
      ls_dd07v TYPE dd07v.

    lv_domain = i_domain.
    lv_value = i_value.
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = lv_domain
        value    = lv_value
      IMPORTING
        dd07v_wa = ls_dd07v.

    r_text = ls_dd07v-ddtext.
  ENDMETHOD.

  METHOD get_type_descr.
    DATA:
      ls_type_in TYPE ko105,
      ls_type_out TYPE ko100,
      lt_types_in TYPE STANDARD TABLE OF ko105,
      lt_types_out TYPE STANDARD TABLE OF ko100.

    ls_type_in-pgmid = 'R3TR'.
    ls_type_in-object = i_obj_type.
    APPEND ls_type_in TO lt_types_in.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      TABLES
        tt_types_in  = lt_types_in
        tt_types_out = lt_types_out.

    READ TABLE lt_types_out INTO ls_type_out INDEX 1.
    IF sy-subrc = 0.
      r_text = ls_type_out-text.
    ELSE.
      r_text = i_obj_type.
    ENDIF.
  ENDMETHOD.

  METHOD level.
    CASE i_code.
      WHEN '0'. r_value = 'Instance'.
      WHEN '1'. r_value = 'Static'.
      WHEN '2'. r_value = 'Constant'.
    ENDCASE.
  ENDMETHOD.

  METHOD param_kind.
    CASE i_code.
      WHEN '0'. r_value = 'Importing'.
      WHEN '1'. r_value = 'Exporting'.
      WHEN '2'. r_value = 'Changing'.
      WHEN '3'. r_value = 'Returning'.
    ENDCASE.
  ENDMETHOD.

  METHOD typing.
    CASE i_code.
      WHEN '0'.	r_value = 'LIKE'.
      WHEN '1'.	r_value = 'TYPE'.
      WHEN '3'.	r_value = 'TYPE REF TO'.
    ENDCASE.
  ENDMETHOD.

  METHOD visibility.
    CASE i_code.
      WHEN '0'. r_value = 'Private'.
      WHEN '1'. r_value = 'Protected'.
      WHEN '2'. r_value = 'Public'.
      WHEN '3'. r_value = 'Package'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
"$. EndRegion UTILITIES

"$. Region REPO OBJECTS

CLASS lcl_repo_object DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_header,
        name TYPE string,
        obj_type TYPE string,
        obj_type_name TYPE string,
        package TYPE string,
        description TYPE string,
      END OF ty_header,

    ty_type_ts TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line,
    ty_object_tt TYPE STANDARD TABLE OF REF TO lcl_repo_object
      WITH DEFAULT KEY.

    TYPES:
        BEGIN OF ty_obj_key,
          pgmid TYPE tadir-pgmid,
          object TYPE tadir-object,
          obj_name TYPE tadir-obj_name,
          devclass TYPE tadir-devclass,
        END OF ty_obj_key.

    METHODS constructor IMPORTING i_key TYPE ty_obj_key.
    METHODS exists ABSTRACT RETURNING value(r_value) TYPE abap_bool.
    METHODS fill_header IMPORTING i_description TYPE clike
      RETURNING value(r_header) TYPE ty_header.
    METHODS get_header RETURNING value(r_header) TYPE ty_header.
    METHODS get_id RETURNING value(r_id) TYPE string.
    METHODS to_xml ABSTRACT RETURNING value(r_doc) TYPE REF TO lcl_xml_doc
      RAISING lcx_abap_doc_exception.
    METHODS get_key RETURNING value(r_value) TYPE ty_obj_key.
    METHODS get_name RETURNING value(r_value) TYPE e071-obj_name.
    METHODS get_type RETURNING value(r_value) TYPE e071-object.
    METHODS get_package_name RETURNING value(r_value) TYPE tadir-devclass.
    METHODS set_config IMPORTING i_config TYPE REF TO lcl_config.
  PROTECTED SECTION.
    DATA:
      header TYPE ty_header,
      config TYPE REF TO lcl_config,
      obj_key TYPE ty_obj_key.

    METHODS process_config.
ENDCLASS.

CLASS lcl_repo_object IMPLEMENTATION.
  METHOD constructor.
    obj_key = i_key.
    CREATE OBJECT config.
  ENDMETHOD.

  METHOD fill_header.
    me->header-name = obj_key-obj_name.
    me->header-obj_type = obj_key-object.
    me->header-description = i_description.
    me->header-obj_type_name = lcl_type_info=>get_type_descr( obj_key-object ).
    me->header-package = obj_key-devclass.
    r_header = me->header.
  ENDMETHOD.

  METHOD get_header.
    r_header = me->header.
  ENDMETHOD.

  METHOD get_id.
    r_id = |{ get_type( ) }.{ get_name( ) }|.
  ENDMETHOD.

  METHOD get_key.
    r_value = obj_key.
  ENDMETHOD.

  METHOD get_name.
    r_value = obj_key-obj_name.
  ENDMETHOD.

  METHOD get_type.
    r_value = obj_key-object.
  ENDMETHOD.

  METHOD get_package_name.
    r_value = obj_key-devclass.
  ENDMETHOD.


  METHOD set_config.
    ASSERT i_config IS BOUND.
    me->config = i_config.
    process_config( ).
  ENDMETHOD.

  METHOD process_config. "NEEDED
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_fugr DEFINITION INHERITING FROM lcl_repo_object.
  PUBLIC SECTION.
    METHODS exists REDEFINITION.
    METHODS to_xml REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      ty_params_docu_tt TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY,
      BEGIN OF ty_signature,
        import TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY,
        changing TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY,
        export TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY,
        tables TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY,
        exception TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY,
        docu TYPE ty_params_docu_tt,
      END OF ty_signature,
      ty_rs38l_incl_tt TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_function,
        name TYPE string,
        description TYPE rs38l_ftxt,
        remote TYPE xsdboolean,
        bas_xml TYPE xsdboolean,
        update TYPE xsdboolean,
        parameters TYPE lcl_type_info=>ty_parameters_tt,
        exceptions TYPE lcl_type_info=>ty_exceptions_tt,
      END OF ty_function,
      ty_functions_tt TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY,

      BEGIN OF ty_function_group,
        header TYPE ty_header,
        functions TYPE ty_functions_tt,
      END OF ty_function_group.

    METHODS get_param_descr IMPORTING i_param TYPE clike i_docu TYPE ty_params_docu_tt
      RETURNING value(r_descr) TYPE string.
    METHODS not IMPORTING i_value TYPE abap_bool RETURNING value(r_value) TYPE abap_bool.
    METHODS read_fugr_text RETURNING value(r_text) TYPE string.
    METHODS read_functions RETURNING value(r_functab) TYPE ty_rs38l_incl_tt.
    METHODS get_functions RETURNING value(r_functions) TYPE ty_functions_tt
      RAISING lcx_abap_doc_exception.
    METHODS signature_to_exceptions IMPORTING i_signature TYPE ty_signature
      RETURNING value(r_exceptions) TYPE lcl_type_info=>ty_exceptions_tt.
    METHODS signature_to_parameters IMPORTING i_signature TYPE ty_signature
      RETURNING value(r_parameters) TYPE lcl_type_info=>ty_parameters_tt.

ENDCLASS.

CLASS lcl_object_fugr IMPLEMENTATION.
  METHOD exists.
    DATA: lv_pool  TYPE tlibg-area.

    lv_pool = obj_key-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      ls_fugr TYPE ty_function_group,
      lt_functions TYPE ty_functions_tt.

    ls_fugr-header = fill_header( read_fugr_text( ) ).
    ls_fugr-functions = get_functions( ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_fugr ).
  ENDMETHOD.

  METHOD read_fugr_text.
    SELECT SINGLE areat FROM tlibt INTO r_text
      WHERE spras = sy-langu AND area  = obj_key-obj_name.
  ENDMETHOD.
  METHOD read_functions.

    DATA: lv_area TYPE rs38l-area.
    lv_area = obj_key-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = r_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    CHECK sy-subrc = 0.
    SORT r_functab BY funcname ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_functab COMPARING funcname.

  ENDMETHOD.

  METHOD get_functions.
    DATA:
      ls_function   TYPE ty_function,
      ls_signature TYPE ty_signature,
      lt_source TYPE STANDARD TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS:
      <ls_func> TYPE rs38l_incl.

    lt_functab = read_functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
      CLEAR ls_function.
      ls_function-name = <ls_func>-funcname.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          remote_call             = ls_function-remote
          update_task             = ls_function-update
          short_text              = ls_function-description
          remote_basxml_supported = ls_function-bas_xml
        TABLES
          import_parameter        = ls_signature-import
          changing_parameter      = ls_signature-changing
          export_parameter        = ls_signature-export
          tables_parameter        = ls_signature-tables
          exception_list          = ls_signature-exception
          documentation           = ls_signature-docu
          source                  = lt_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc = 2.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        lcx_abap_doc_exception=>raise_from_sy( ).
      ENDIF.
      IF ls_function-remote IS NOT INITIAL. "convert to true/false
        ls_function-remote = abap_true.
      ENDIF.
      IF ls_function-update IS NOT INITIAL. "convert to true/false
        ls_function-update = abap_true.
      ENDIF.

      ls_function-parameters = signature_to_parameters( ls_signature ).
      ls_function-exceptions = signature_to_exceptions( ls_signature ).
      APPEND ls_function TO r_functions.
    ENDLOOP.

  ENDMETHOD.

  METHOD signature_to_parameters.
    DATA:
      ls_parameter TYPE lcl_type_info=>ty_parameter.

    FIELD-SYMBOLS:
      <import> TYPE rsimp,
      <changing> TYPE rscha,
      <export> TYPE rsexp,
      <tables> TYPE rstbl.

    LOOP AT i_signature-import ASSIGNING <import>.
      CLEAR ls_parameter.
      ls_parameter-name = <import>-parameter.
      ls_parameter-description = get_param_descr( i_param = <import>-parameter i_docu = i_signature-docu ).
      ls_parameter-kind = 'Importing'.
      ls_parameter-optional = <import>-optional.
      ls_parameter-default_value = <import>-default.
      ls_parameter-pass_by_value = not( <import>-reference ).
      IF <import>-dbfield IS NOT INITIAL.
        ls_parameter-type = <import>-dbfield.
        ls_parameter-typing = 'LIKE'.
      ELSEIF <import>-ref_class = abap_true.
        ls_parameter-type = replace( val = <import>-typ sub = `REF TO ` with = '').
        ls_parameter-typing = 'TYPE REF TO'.
      ELSE.
        ls_parameter-type = <import>-typ.
        ls_parameter-typing = 'TYPE'.
      ENDIF.
      APPEND ls_parameter TO r_parameters.
    ENDLOOP.

    LOOP AT i_signature-export ASSIGNING <export>.
      CLEAR ls_parameter.
      ls_parameter-name = <export>-parameter.
      ls_parameter-description = get_param_descr( i_param = <export>-parameter i_docu = i_signature-docu ).
      ls_parameter-kind = 'Exporting'.
      ls_parameter-pass_by_value = not( <export>-reference ).
      IF <export>-dbfield IS NOT INITIAL.
        ls_parameter-type = <export>-dbfield.
        ls_parameter-typing = 'LIKE'.
      ELSEIF <export>-ref_class = abap_true.
        ls_parameter-type = replace( val = <export>-typ sub = `REF TO ` with = '').
        ls_parameter-typing = 'TYPE REF TO'.
      ELSE.
        ls_parameter-type = <export>-typ.
        ls_parameter-typing = 'TYPE'.
      ENDIF.
      APPEND ls_parameter TO r_parameters.
    ENDLOOP.

    LOOP AT i_signature-tables ASSIGNING <tables>.
      CLEAR ls_parameter.
      ls_parameter-name = <tables>-parameter.
      ls_parameter-description = get_param_descr( i_param = <tables>-parameter i_docu = i_signature-docu ).
      ls_parameter-kind = 'Tables'.
      ls_parameter-optional = <tables>-optional.
      IF <tables>-dbstruct IS NOT INITIAL.
        ls_parameter-type = <tables>-dbstruct.
        ls_parameter-typing = 'LIKE'.
      ELSEIF <tables>-ref_class = abap_true.
        ls_parameter-type = replace( val = <tables>-typ sub = `REF TO ` with = '').
        ls_parameter-typing = 'TYPE REF TO'.
      ELSE.
        ls_parameter-type = <tables>-typ.
        ls_parameter-typing = 'TYPE'.
      ENDIF.
      APPEND ls_parameter TO r_parameters.
    ENDLOOP.

    LOOP AT i_signature-changing ASSIGNING <changing>.
      CLEAR ls_parameter.
      ls_parameter-name = <changing>-parameter.
      ls_parameter-description = get_param_descr( i_param = <changing>-parameter i_docu = i_signature-docu ).
      ls_parameter-kind = 'Changing'.
      ls_parameter-optional = <changing>-optional.
      ls_parameter-default_value = <changing>-default.
      ls_parameter-pass_by_value = not( <changing>-reference ).
      IF <changing>-dbfield IS NOT INITIAL.
        ls_parameter-type = <changing>-dbfield.
        ls_parameter-typing = 'LIKE'.
      ELSEIF <changing>-ref_class = abap_true.
        ls_parameter-type = replace( val = <changing>-typ sub = `REF TO ` with = '').
        ls_parameter-typing = 'TYPE REF TO'.
      ELSE.
        ls_parameter-type = <changing>-typ.
        ls_parameter-typing = 'TYPE'.
      ENDIF.
      APPEND ls_parameter TO r_parameters.
    ENDLOOP.

  ENDMETHOD.

  METHOD not.
    IF i_value = abap_true.
      r_value = abap_false.
    ELSE.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_param_descr.
    FIELD-SYMBOLS:
      <docu> TYPE rsfdo.

    READ TABLE i_docu ASSIGNING <docu>
     WITH KEY parameter = i_param.
    IF sy-subrc = 0.
      r_descr = <docu>-stext.
    ENDIF.
  ENDMETHOD.

  METHOD signature_to_exceptions.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_clif DEFINITION ABSTRACT INHERITING FROM lcl_repo_object.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_key TYPE ty_obj_key.
    METHODS exists REDEFINITION.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_interface_impl,
        name TYPE string,
        impl_class TYPE string,
      END OF ty_interface_impl,
      ty_interfaces_tt TYPE STANDARD TABLE OF ty_interface_impl WITH DEFAULT KEY,

      BEGIN OF ty_seo_clif,
        class TYPE vseoclass,
        interface TYPE vseointerf,
        refclsname TYPE seoclsname,
        attributes TYPE seoo_attributes_r,
        methods TYPE seoo_methods_r,
        events TYPE seoo_events_r,
        types TYPE seoo_types_r,
        parameters TYPE seos_parameters_r,
        exceptions TYPE seos_exceptions_r,
        implementings TYPE seor_implementing_keys,
        inheritance TYPE seor_inheritance_keys,
        redefinitions TYPE seor_redefinitions_r,
        friendships TYPE seof_friendships_r,
        aliases TYPE seoo_aliases_r,
        comprisings TYPE seor_comprisings_r,
        comprisings_detail TYPE seok_int_typeinfos,
      END OF ty_seo_clif.
    TYPES:
      ty_exposure_ra TYPE RANGE OF seoexpose.
    TYPES:
      BEGIN OF ty_components,
        methods TYPE lcl_type_info=>ty_methods_tt,
        attributes TYPE lcl_type_info=>ty_attributes_tt,
        events TYPE lcl_type_info=>ty_events_tt,
      END OF ty_components.

    METHODS add_components IMPORTING i_seo TYPE ty_seo_clif
      CHANGING c_components TYPE ty_components.
    METHODS get_attributes IMPORTING i_seo TYPE ty_seo_clif
      RETURNING value(r_attributes) TYPE lcl_type_info=>ty_attributes_tt.
    METHODS get_events IMPORTING i_seo TYPE ty_seo_clif
      RETURNING value(r_events) TYPE lcl_type_info=>ty_events_tt.
    METHODS get_methods IMPORTING i_seo TYPE ty_seo_clif
      RETURNING value(r_methods) TYPE lcl_type_info=>ty_methods_tt.
    METHODS process_config REDEFINITION.
    METHODS read_interface IMPORTING i_name TYPE clike
      RETURNING value(r_seo) TYPE ty_seo_clif
      RAISING lcx_abap_doc_exception.

    DATA:
      clif_key TYPE seoclskey,
      exposure_range TYPE ty_exposure_ra.

  PRIVATE SECTION.
    METHODS fill_type_from_src IMPORTING i_src TYPE clike
      CHANGING c_attribute TYPE lcl_type_info=>ty_attribute.

ENDCLASS.

CLASS lcl_object_clif IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_key ).
    clif_key-clsname = i_key-obj_name.
  ENDMETHOD.

  METHOD exists.
    CALL FUNCTION 'SEO_CLIF_EXISTENCE_CHECK'
      EXPORTING
        cifkey        = clif_key
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD process_config.
    DATA:
      ls_exp_ra LIKE LINE OF me->exposure_range.

    ls_exp_ra-sign = 'I'.
    ls_exp_ra-option = 'EQ'.

    IF config->get_param_boolean( 'CLIF-PRIVATE' ) = abap_true.
      ls_exp_ra-low = '0'.
      APPEND ls_exp_ra TO me->exposure_range.
    ENDIF.
    IF config->get_param_boolean( 'CLIF-PROTECTED' ) = abap_true.
      ls_exp_ra-low = '1'.
      APPEND ls_exp_ra TO me->exposure_range.
    ENDIF.
    IF config->get_param_boolean( 'CLIF-PUBLIC' ) = abap_true.
      ls_exp_ra-low = '2'.
      APPEND ls_exp_ra TO me->exposure_range.
    ENDIF.
  ENDMETHOD.

  METHOD get_attributes.
    DATA:
      ls_attrib TYPE lcl_type_info=>ty_attribute,
      ls_seo_attrib TYPE seoo_attribute_r.

    LOOP AT i_seo-attributes INTO ls_seo_attrib
      WHERE exposure IN exposure_range.

      CLEAR ls_attrib.
      "Add interface prefix
      IF i_seo-interface-clsname IS NOT INITIAL AND clif_key-clsname <> i_seo-interface-clsname.
        ls_attrib-name = i_seo-interface-clsname && '~' && ls_seo_attrib-cmpname.
      ELSE.
        ls_attrib-name = ls_seo_attrib-cmpname.
      ENDIF.
      ls_attrib-description = ls_seo_attrib-descript.
      ls_attrib-level = lcl_type_info=>level( ls_seo_attrib-attdecltyp ).
      ls_attrib-exposure = ls_seo_attrib-exposure.
      ls_attrib-visibility = lcl_type_info=>visibility( ls_seo_attrib-exposure ).
      ls_attrib-read_only = ls_seo_attrib-attrdonly.
      IF ls_seo_attrib-typtype = '4'. "internal type
        fill_type_from_src( EXPORTING i_src = ls_seo_attrib-typesrc
          CHANGING c_attribute = ls_attrib ).
      ELSE.
        ls_attrib-typing = lcl_type_info=>typing( ls_seo_attrib-typtype ).
        ls_attrib-type = ls_seo_attrib-type.
      ENDIF.
      ls_attrib-value = ls_seo_attrib-attvalue.
      IF i_seo-refclsname IS NOT INITIAL.
        ls_attrib-source = i_seo-refclsname.
      ELSE.
        ls_attrib-source = ls_seo_attrib-clsname.
      ENDIF.
      IF ls_seo_attrib-clsname <> clif_key-clsname..
        ls_attrib-inherited = abap_true.
      ENDIF.
      APPEND ls_attrib TO r_attributes.
    ENDLOOP.
    SORT r_attributes BY exposure DESCENDING name.
  ENDMETHOD.

  METHOD fill_type_from_src.
    CONSTANTS:
      c_type_ref_to TYPE string VALUE 'TYPE REF TO',
      c_type TYPE string VALUE 'TYPE',
      c_like TYPE string VALUE 'LIKE'.
    DATA:
      lv_definition TYPE string.

    "remove new lines
    lv_definition = replace( val = i_src regex = '[\r\n]' with = '' occ = 0 ).
    "remove attribute name (first identifier)
    lv_definition = replace( val = lv_definition regex = '^[A-Za-z_][A-Za-z_0-9]*\s*' with = '' ).
    lv_definition = to_upper( lv_definition ).
    IF contains( val = i_src sub = c_type_ref_to ).
      c_attribute-typing = c_type_ref_to.
      c_attribute-type = replace( val = lv_definition regex = |{ c_type_ref_to }\\s*| with = '' ).
    ELSEIF contains( val = i_src sub = c_like ).
      c_attribute-typing = 'LIKE'.
      c_attribute-type = replace( val = lv_definition regex = |{ c_like }\\s*| with = '' ).
    ELSEIF contains( val = i_src sub = 'TYPE').
      c_attribute-typing = c_type.
      c_attribute-type = replace( val = lv_definition regex = |{ c_type }\\s*| with  = '' ).
    ENDIF.

  ENDMETHOD.

  METHOD get_events.
    DATA:
      ls_event TYPE lcl_type_info=>ty_event,
      ls_param TYPE lcl_type_info=>ty_parameter,
      ls_seo_param TYPE seos_parameter_r,
      ls_seo_event TYPE seoo_event_r.

    LOOP AT i_seo-events INTO ls_seo_event
      WHERE exposure IN exposure_range.

      CLEAR ls_event.
      "Add interface prefix
      IF i_seo-interface-clsname IS NOT INITIAL AND clif_key-clsname <> i_seo-interface-clsname.
        ls_event-name = i_seo-interface-clsname && '~' && ls_seo_event-cmpname.
      ELSE.
        ls_event-name = ls_seo_event-cmpname.
      ENDIF.
      ls_event-description = ls_seo_event-descript.
      ls_event-level = lcl_type_info=>level( ls_seo_event-evtdecltyp ).
      ls_event-exposure = ls_seo_event-exposure.
      ls_event-visibility = lcl_type_info=>visibility( ls_seo_event-exposure ).
      IF i_seo-refclsname IS NOT INITIAL.
        ls_event-source = i_seo-refclsname.
      ELSE.
        ls_event-source = ls_seo_event-clsname.
      ENDIF.
      IF ls_seo_event-clsname <> clif_key-clsname.
        ls_event-inherited = abap_true.
      ENDIF.
      LOOP AT i_seo-parameters INTO ls_seo_param WHERE cmpname = ls_seo_event-cmpname.
        CLEAR ls_param.
        ls_param-name = ls_seo_param-sconame.
        ls_param-description = ls_seo_param-descript.
        ls_param-optional = ls_seo_param-paroptionl.
        ls_param-default_value = ls_seo_param-parvalue.
        ls_param-kind = lcl_type_info=>param_kind( ls_seo_param-pardecltyp ).
        IF ls_seo_param-parpasstyp = '0'.
          ls_param-pass_by_value = abap_true.
        ENDIF.
        ls_param-typing = lcl_type_info=>typing( ls_seo_param-typtype ).
        ls_param-type = ls_seo_param-type.
        APPEND ls_param TO ls_event-parameters.
      ENDLOOP.

      APPEND ls_event TO r_events.
    ENDLOOP.

    SORT r_events BY exposure DESCENDING name.
  ENDMETHOD.

  METHOD get_methods.
    DATA:
      ls_param TYPE lcl_type_info=>ty_parameter,
      ls_excep TYPE lcl_type_info=>ty_exception,
      ls_method TYPE lcl_type_info=>ty_method,
      ls_seo_param TYPE seos_parameter_r,
      ls_seo_excep TYPE seos_exception_r,
      ls_seo_method TYPE seoo_method_r.

    LOOP AT i_seo-methods INTO ls_seo_method
      WHERE exposure IN exposure_range.

      CLEAR ls_method.
      "Add interface prefix
      IF i_seo-interface-clsname IS NOT INITIAL AND clif_key-clsname <> i_seo-interface-clsname.
        ls_method-name = i_seo-interface-clsname && '~' && ls_seo_method-cmpname.
      ELSE.
        ls_method-name = ls_seo_method-cmpname.
      ENDIF.
      ls_method-description = ls_seo_method-descript.
      ls_method-abstract = ls_seo_method-mtdabstrct.
      ls_method-redefined = ls_seo_method-redefin.
      ls_method-final = ls_seo_method-mtdfinal.
      ls_method-source = ls_seo_method-refclsname.
      ls_method-level = lcl_type_info=>level( ls_seo_method-mtddecltyp ).
      ls_method-exposure = ls_seo_method-exposure.
      ls_method-visibility = lcl_type_info=>visibility( ls_seo_method-exposure ).
      IF i_seo-refclsname IS NOT INITIAL.
        ls_method-source = i_seo-refclsname.
      ELSE.
        ls_method-source = ls_seo_method-clsname.
      ENDIF.
      IF ls_seo_method-clsname <> clif_key-clsname.
        ls_method-inherited = abap_true.
      ENDIF.
      LOOP AT i_seo-parameters INTO ls_seo_param WHERE cmpname = ls_seo_method-cmpname.
        CLEAR ls_param.
        ls_param-name = ls_seo_param-sconame.
        ls_param-description = ls_seo_param-descript.
        ls_param-optional = ls_seo_param-paroptionl.
        ls_param-default_value = ls_seo_param-parvalue.
        ls_param-kind = lcl_type_info=>param_kind( ls_seo_param-pardecltyp ).
        IF ls_seo_param-parpasstyp = '0'.
          ls_param-pass_by_value = abap_true.
        ENDIF.
        ls_param-typing = lcl_type_info=>typing( ls_seo_param-typtype ).
        ls_param-type = ls_seo_param-type.
        APPEND ls_param TO ls_method-parameters.
      ENDLOOP.

      LOOP AT i_seo-exceptions INTO ls_seo_excep WHERE cmpname = ls_seo_method-cmpname.
        CLEAR ls_excep.
        ls_excep-name = ls_seo_excep-sconame.
        ls_excep-description = ls_seo_excep-descript.
        APPEND ls_excep TO ls_method-exceptions.
      ENDLOOP.
      APPEND ls_method TO r_methods.
    ENDLOOP.
    SORT r_methods BY exposure DESCENDING name.
  ENDMETHOD.

  METHOD add_components.
    APPEND LINES OF get_attributes( i_seo ) TO c_components-attributes.
    APPEND LINES OF get_methods( i_seo ) TO c_components-methods.
    APPEND LINES OF get_events( i_seo ) TO c_components-events.
  ENDMETHOD.

  METHOD read_interface.
    DATA:
      ls_intf_key TYPE seoclskey.

    ls_intf_key-clsname = i_name.
    CALL FUNCTION 'SEO_INTERFACE_TYPEINFO_GET'
      EXPORTING
        intkey              = ls_intf_key
        version             = seoc_version_active
      IMPORTING
        interface           = r_seo-interface
        attributes          = r_seo-attributes
        methods             = r_seo-methods
        events              = r_seo-events
        parameters          = r_seo-parameters
        exceps              = r_seo-exceptions
        comprisings         = r_seo-comprisings
        explore_comprisings = r_seo-comprisings_detail
        aliases             = r_seo-aliases
      EXCEPTIONS
        not_existing        = 1
        is_class            = 2
        model_only          = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_object_clif.
  PUBLIC SECTION.
    METHODS to_xml REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_inheritance,
        level TYPE i,
        super_class TYPE string,
      END OF ty_inheritance,
      ty_inheritance_tt TYPE STANDARD TABLE OF ty_inheritance WITH DEFAULT KEY,

      BEGIN OF ty_class,
        header TYPE ty_header,
        exposure TYPE i, "visibility code
        visibility TYPE string,
        inheritance TYPE ty_inheritance_tt,
        interfaces TYPE ty_interfaces_tt.
            INCLUDE TYPE ty_components AS components.
    TYPES:
    END OF ty_class.

    METHODS add_intf_components IMPORTING i_seo TYPE ty_seo_clif
      CHANGING c_components TYPE ty_components
      RAISING lcx_abap_doc_exception.
    METHODS add_inh_components  IMPORTING i_seo TYPE ty_seo_clif
      CHANGING c_components TYPE ty_components
      RAISING lcx_abap_doc_exception.
    METHODS read_class
      RETURNING value(r_seo) TYPE ty_seo_clif
      RAISING lcx_abap_doc_exception.
    METHODS resolve_inheritance
      RETURNING value(r_seo) TYPE ty_seo_clif
      RAISING lcx_abap_doc_exception.
    METHODS get_interfaces IMPORTING i_seo TYPE ty_seo_clif
      RETURNING value(r_interfaces) TYPE ty_interfaces_tt.
    METHODS get_inheritance IMPORTING i_inh_keys TYPE seor_inheritance_keys
      RETURNING value(r_inheritance) TYPE ty_inheritance_tt.
ENDCLASS.

CLASS lcl_object_clas IMPLEMENTATION.

  METHOD to_xml.
    DATA:
      ls_class TYPE ty_class,
      ls_seo TYPE ty_seo_clif,
      lo_class TYPE REF TO cl_oo_class_components_flat.

    ls_seo = read_class( ).
    ls_class-header = fill_header( ls_seo-class-descript ).
    ls_class-inheritance = get_inheritance( ls_seo-inheritance ).
    ls_class-interfaces = get_interfaces( ls_seo ).
    ls_class-exposure = ls_seo-class-exposure.
    ls_class-visibility = lcl_type_info=>visibility( ls_seo-class-exposure ).

    add_components( EXPORTING i_seo = ls_seo CHANGING c_components = ls_class-components ).
    add_intf_components( EXPORTING i_seo = ls_seo CHANGING c_components = ls_class-components ).
    add_inh_components( EXPORTING i_seo = ls_seo CHANGING c_components = ls_class-components ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_class ).
  ENDMETHOD.

  METHOD add_intf_components.
    DATA:
      ls_impl_key TYPE seor_implementing_key,
      ls_intf_seo TYPE ty_seo_clif.

    LOOP AT i_seo-implementings INTO ls_impl_key.
      ls_intf_seo = read_interface( ls_impl_key-refclsname ).
      ls_intf_seo-refclsname = ls_impl_key-clsname. "set implementing class name
      add_components( EXPORTING i_seo = ls_intf_seo CHANGING c_components = c_components ).
    ENDLOOP.

  ENDMETHOD.

  METHOD add_inh_components.
    DATA:
      ls_super_seo TYPE ty_seo_clif.

    ls_super_seo = resolve_inheritance( ).
    add_components( EXPORTING i_seo = ls_super_seo CHANGING c_components = c_components ).
  ENDMETHOD.

  METHOD get_interfaces.
    DATA:
      ls_interface TYPE ty_interface_impl.

    FIELD-SYMBOLS:
      <impl_key> TYPE seor_implementing_key.

    "Interaces implemented in the current class
    LOOP AT i_seo-implementings ASSIGNING <impl_key>.
      CLEAR ls_interface.
      ls_interface-name = <impl_key>-refclsname.
      ls_interface-impl_class = <impl_key>-clsname.
      APPEND ls_interface TO r_interfaces.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_inheritance.
    DATA:
      ls_inheritance TYPE ty_inheritance.
    FIELD-SYMBOLS:
      <super> TYPE seor_inheritance_key.

    LOOP AT i_inh_keys ASSIGNING <super>.
      CLEAR ls_inheritance.
      ls_inheritance-level = sy-tabix.
      ls_inheritance-super_class = <super>-refclsname.
      APPEND ls_inheritance TO r_inheritance.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_class.
    CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
      EXPORTING
        clskey            = clif_key
        version           = seoc_version_active
        with_descriptions = seox_true
      IMPORTING
        class             = r_seo-class
        attributes        = r_seo-attributes
        methods           = r_seo-methods
        events            = r_seo-events
        types             = r_seo-types
        parameters        = r_seo-parameters
        exceps            = r_seo-exceptions
        redefinitions     = r_seo-redefinitions
        friendships       = r_seo-friendships
        aliases           = r_seo-aliases
      EXCEPTIONS
        not_existing      = 1
        is_interface      = 2
        model_only        = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.

    CALL FUNCTION 'SEO_CLASS_INHERITANCE_PATH_GET'
      EXPORTING
        clskey       = clif_key
        version      = seoc_version_active
      IMPORTING
        path         = r_seo-inheritance
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        model_only   = 3
        OTHERS       = 4.

    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.

    CALL FUNCTION 'SEO_CLASS_ALL_IMPLEMENTG_GET'
      EXPORTING
        clskey       = clif_key
        version      = seoc_version_active
      IMPORTING
        set          = r_seo-implementings
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        model_only   = 3
        OTHERS       = 4.

    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.

  METHOD resolve_inheritance.
    CALL FUNCTION 'SEO_CLASS_RESOLVE_INHERITANCE'
      EXPORTING
        clskey       = clif_key
        version      = seoc_version_active
      IMPORTING
        attributes   = r_seo-attributes
        methods      = r_seo-methods
        events       = r_seo-events
        types        = r_seo-types
        parameters   = r_seo-parameters
        exceps       = r_seo-exceptions
        aliases      = r_seo-aliases
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        model_only   = 3
        OTHERS       = 4.

    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_intf DEFINITION INHERITING FROM lcl_object_clif.
  PUBLIC SECTION.
    METHODS to_xml REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_interface,
        header TYPE ty_header,
        exposure TYPE i, "visibility code
        visibility TYPE string,
        interfaces TYPE ty_interfaces_tt.
            INCLUDE TYPE ty_components AS components.
    TYPES:
     END OF ty_interface.

    METHODS add_intf_components IMPORTING i_seo TYPE ty_seo_clif
      CHANGING c_components TYPE ty_components
      RAISING lcx_abap_doc_exception.
    METHODS get_interfaces IMPORTING i_seo TYPE ty_seo_clif
      RETURNING value(r_interfaces) TYPE ty_interfaces_tt.

ENDCLASS.

CLASS lcl_object_intf IMPLEMENTATION.

  METHOD to_xml.
    DATA:
      ls_seo TYPE ty_seo_clif,
      ls_intf TYPE ty_interface,
      lo_intf TYPE REF TO cl_oo_interf_components_flat.

    ls_seo = read_interface( clif_key-clsname ).

    ls_intf-header = fill_header( ls_seo-interface-descript ).
    ls_intf-visibility = lcl_type_info=>visibility( ls_seo-interface-exposure ).

    ls_intf-interfaces = get_interfaces( ls_seo ).
    add_components( EXPORTING i_seo = ls_seo CHANGING c_components = ls_intf-components ).
    add_intf_components( EXPORTING i_seo = ls_seo CHANGING c_components = ls_intf-components ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_intf ).
  ENDMETHOD.

  METHOD get_interfaces.
    DATA:
      ls_interface TYPE ty_interface_impl,
      ls_seo_comprising TYPE seor_comprising_r.

    LOOP AT i_seo-comprisings INTO ls_seo_comprising.
      CLEAR ls_interface.
      ls_interface-name = ls_seo_comprising-refclsname.
      APPEND ls_interface TO r_interfaces.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_intf_components.
    DATA:
      ls_super_seo TYPE ty_seo_clif.

    FIELD-SYMBOLS:
      <compr> TYPE seok_int_typeinfo.

    LOOP AT i_seo-comprisings_detail ASSIGNING <compr>.
      CLEAR ls_super_seo.
      ls_super_seo-interface-clsname = <compr>-interface-clsname.
      ls_super_seo-methods = <compr>-methods.
      ls_super_seo-attributes = <compr>-attributes.
      ls_super_seo-events = <compr>-events.
      ls_super_seo-types = <compr>-types.
      ls_super_seo-aliases = <compr>-aliases.
      add_components( EXPORTING i_seo = ls_super_seo CHANGING c_components = c_components ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_tabl DEFINITION INHERITING FROM lcl_repo_object.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_key TYPE ty_obj_key.
    METHODS exists REDEFINITION.
    METHODS to_xml REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_index_field,
        name TYPE string,
        description TYPE string,
      END OF ty_index_field,
      ty_index_fields_tt TYPE STANDARD TABLE OF ty_index_field WITH DEFAULT KEY,
      BEGIN OF ty_index,
        name TYPE string,
        description TYPE string,
        status TYPE as4local,
        status_text TYPE string,
        unique TYPE xsdboolean,
        db_index_name TYPE string,
        db_status TYPE ddixdbstat,
        db_status_text TYPE string,
        fields TYPE ty_index_fields_tt,
      END OF ty_index,
      ty_indexes_tt TYPE STANDARD TABLE OF ty_index WITH DEFAULT KEY,

      BEGIN OF ty_technical,
        buffering TYPE bufallow,
        buffering_text TYPE string,
        change_log TYPE xsdboolean,
        data_class TYPE string,
        size_category TYPE i,
      END OF ty_technical,
      BEGIN OF ty_table,
        header TYPE ty_header,
        kind TYPE string, "Transparent/Structure
        maintenance TYPE dd02v-mainflag,
        maintenance_text TYPE string,
        technical TYPE ty_technical,
        fields TYPE lcl_type_info=>ty_ddic_fields_tt,
        indexes TYPE ty_indexes_tt,
      END OF ty_table,
      BEGIN OF ty_sap_table,
        dd02v TYPE dd02v,
        dd09l TYPE dd09l,
        t_dd03p TYPE dd03ttyp,
        t_dd05m TYPE dd05mttyp,
        t_dd08v TYPE dd08vttyp,
        t_dd12v TYPE dd12vtab,
        t_dd17v TYPE dd17vtab,
        t_dd35v TYPE dd35vttyp,
        t_dd36m TYPE dd36mttyp,
      END OF ty_sap_table.
    METHODS get_fields IMPORTING i_dd03p TYPE dd03ttyp
      RETURNING value(r_fields) TYPE lcl_type_info=>ty_ddic_fields_tt.
    METHODS get_field_description
      IMPORTING i_dd03p TYPE dd03ttyp
        i_fieldname TYPE clike
      RETURNING value(r_text) TYPE string.
    METHODS get_indexes IMPORTING i_sap_table TYPE ty_sap_table
      RETURNING value(r_indexes) TYPE ty_indexes_tt.
    METHODS get_technical IMPORTING i_dd09l TYPE dd09l
      RETURNING value(r_technical) TYPE ty_technical.
    METHODS read_table RETURNING value(r_table) TYPE ty_sap_table
      RAISING lcx_abap_doc_exception.
    DATA:
      tabname TYPE ddobjname.
ENDCLASS.

CLASS lcl_object_tabl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_key ).
    tabname = i_key-obj_name.
  ENDMETHOD.

  METHOD exists.
    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = obj_key-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      ls_table TYPE ty_table,
      ls_sap_table TYPE ty_sap_table.

    ls_sap_table = read_table( ).

    ls_table-header = fill_header( ls_sap_table-dd02v-ddtext ).
    ls_table-maintenance = ls_sap_table-dd02v-mainflag.
    ls_table-maintenance_text = lcl_type_info=>get_domain_value_text( i_domain = 'MAINTFLAG' i_value = ls_sap_table-dd02v-mainflag ).
    CASE ls_sap_table-dd02v-tabclass.
      WHEN 'TRANSP'.
        ls_table-kind = 'Transparent'.
      WHEN 'INTTAB'.
        ls_table-kind = 'Structure'.
    ENDCASE.
    ls_table-technical = get_technical( ls_sap_table-dd09l ).
    ls_table-fields = get_fields( ls_sap_table-t_dd03p ).
    ls_table-indexes = get_indexes( ls_sap_table ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_table ).
  ENDMETHOD.

  METHOD get_fields.
    DATA:
      ls_field TYPE lcl_type_info=>ty_ddic_field.

    FIELD-SYMBOLS:
      <dd03p> TYPE dd03p.

    LOOP AT i_dd03p ASSIGNING <dd03p>.
      CLEAR ls_field.
      IF <dd03p>-fieldname = '.INCLU--AP'.
        ls_field-name = '.APPEND'.
      ELSE.
        ls_field-name = <dd03p>-fieldname.
      ENDIF.
      ls_field-description = <dd03p>-ddtext.
      ls_field-is_key = <dd03p>-keyflag.
      ls_field-not_null = <dd03p>-notnull.
      IF <dd03p>-precfield IS NOT INITIAL.
        ls_field-type = <dd03p>-precfield.
      ELSEIF <dd03p>-rollname IS NOT INITIAL.
        ls_field-type = <dd03p>-rollname.
      ELSE.
        ls_field-type = <dd03p>-datatype.
      ENDIF.
      IF <dd03p>-adminfield > '0'.
        ls_field-include = abap_true.
      ENDIF.
      APPEND ls_field TO r_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_indexes.
    DATA:
      ls_index TYPE ty_index,
      ls_index_field TYPE ty_index_field.
    FIELD-SYMBOLS:
      <dd12v> TYPE dd12v,
      <dd17v> TYPE dd17v.

    LOOP AT i_sap_table-t_dd12v ASSIGNING <dd12v>.
      CLEAR ls_index.
      ls_index-name = <dd12v>-indexname.
      ls_index-description = <dd12v>-ddtext.
      ls_index-db_index_name = <dd12v>-dbindex.
      ls_index-db_status = <dd12v>-dbstate.
      ls_index-db_status_text = lcl_type_info=>get_domain_value_text( i_domain = 'DDIXDBSTAT' i_value = <dd12v>-dbstate ).
      ls_index-status = <dd12v>-as4local.
      IF <dd12v>-as4local = 'A'.
        ls_index-status_text = 'Active'.
      ELSE.
        ls_index-status_text = 'Inactive'.
      ENDIF.
      ls_index-unique = <dd12v>-uniqueflag.
      LOOP AT i_sap_table-t_dd17v ASSIGNING <dd17v> WHERE indexname = <dd12v>-indexname.
        CLEAR ls_index_field.
        ls_index_field-name = <dd17v>-fieldname.
        ls_index_field-description = get_field_description( i_dd03p = i_sap_table-t_dd03p i_fieldname = <dd17v>-fieldname ).
        APPEND ls_index_field TO ls_index-fields.
      ENDLOOP.
      APPEND ls_index TO r_indexes.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_field_description.
    DATA:
      ls_dd03p TYPE dd03p.

    READ TABLE i_dd03p WITH KEY fieldname = i_fieldname INTO ls_dd03p.
    IF sy-subrc = 0.
      r_text = ls_dd03p-ddtext.
    ENDIF.
  ENDMETHOD.

  METHOD get_technical.
    r_technical-buffering = i_dd09l-bufallow.
    r_technical-buffering_text = lcl_type_info=>get_domain_value_text( i_domain = 'BUFALLOW' i_value = i_dd09l-bufallow ).
    r_technical-change_log = i_dd09l-protokoll.
    r_technical-data_class = i_dd09l-tabart.
    r_technical-size_category = i_dd09l-tabkat.
  ENDMETHOD.

  METHOD read_table.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = tabname
        langu         = sy-langu
      IMPORTING
        dd02v_wa      = r_table-dd02v
        dd09l_wa      = r_table-dd09l
      TABLES
        dd03p_tab     = r_table-t_dd03p
        dd05m_tab     = r_table-t_dd05m
        dd08v_tab     = r_table-t_dd08v
        dd12v_tab     = r_table-t_dd12v
        dd17v_tab     = r_table-t_dd17v
        dd35v_tab     = r_table-t_dd35v
        dd36m_tab     = r_table-t_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    CHECK sy-subrc = 0.
    IF r_table-dd02v IS INITIAL.
      lcx_abap_doc_exception=>raise( |Table/Structure { tabname } does not exist| ).
    ENDIF.
    DELETE r_table-t_dd03p WHERE depth > 0. "delete fields from nested structures
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_repo_object.
  PUBLIC SECTION.
    METHODS exists REDEFINITION.
    METHODS to_xml REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_label,
        text TYPE string,
        max_length TYPE i,
      END OF ty_label,
      BEGIN OF ty_labels,
        short TYPE ty_label,
        medium TYPE ty_label,
        long TYPE ty_label,
        heading TYPE ty_label,
      END OF ty_labels,
      BEGIN OF ty_data_element,
        header TYPE ty_header,
        type TYPE string,
        labels TYPE ty_labels,
      END OF ty_data_element,
      BEGIN OF ty_sap_dtel,
        dd04l TYPE dd04l,
        dd04t TYPE dd04t,
      END OF ty_sap_dtel.
    METHODS get_labels IMPORTING i_sap_dtel TYPE ty_sap_dtel
      RETURNING value(r_labels) TYPE ty_labels.
    METHODS read_dtel RETURNING value(r_dtel) TYPE ty_sap_dtel
     RAISING lcx_abap_doc_exception.
ENDCLASS.

CLASS lcl_object_dtel IMPLEMENTATION.
  METHOD exists.
    DATA: lv_rollname TYPE dd04l-rollname.
    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = obj_key-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      ls_dtel TYPE ty_data_element,
      ls_sap_dtel TYPE ty_sap_dtel.

    ls_sap_dtel = read_dtel( ).

    ls_dtel-header = fill_header( ls_sap_dtel-dd04t-ddtext ).
    IF ls_sap_dtel-dd04l-domname IS NOT INITIAL.
      ls_dtel-type = ls_sap_dtel-dd04l-domname.
    ELSE.
      ls_dtel-type = ls_sap_dtel-dd04l-datatype.
    ENDIF.
    ls_dtel-labels = get_labels( ls_sap_dtel ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_dtel ).
  ENDMETHOD.

  METHOD get_labels.
    r_labels-short-text = i_sap_dtel-dd04t-scrtext_s.
    r_labels-short-max_length = i_sap_dtel-dd04l-scrlen1.
    r_labels-medium-text = i_sap_dtel-dd04t-scrtext_m.
    r_labels-medium-max_length = i_sap_dtel-dd04l-scrlen2.
    r_labels-long-text = i_sap_dtel-dd04t-scrtext_l.
    r_labels-long-max_length = i_sap_dtel-dd04l-scrlen3.
    r_labels-heading-text = i_sap_dtel-dd04t-reptext.
    r_labels-heading-max_length = i_sap_dtel-dd04l-headlen.
  ENDMETHOD.

  METHOD read_dtel.
    SELECT SINGLE * FROM dd04l
      INTO r_dtel-dd04l
      WHERE rollname = obj_key-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0 OR r_dtel-dd04l IS INITIAL.
      lcx_abap_doc_exception=>raise( 'DTEL not found in DD04L' ) ##no_text.
    ENDIF.

    SELECT SINGLE * FROM dd04t
      INTO r_dtel-dd04t
      WHERE rollname = obj_key-obj_name
      AND ddlanguage = sy-langu
      AND as4local = 'A'
      AND as4vers = '0000'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_repo_object.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_key TYPE ty_obj_key.
    METHODS exists REDEFINITION.
    METHODS to_xml REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_value,
        pos TYPE i,
        low TYPE string,
        high TYPE string,
        description TYPE string,
      END OF ty_value,
      ty_values_tt TYPE STANDARD TABLE OF ty_value WITH DEFAULT KEY,
      BEGIN OF ty_domain,
        header TYPE ty_header,
        type TYPE string,
        length TYPE i,
        output_length TYPE i,
        conv_exit TYPE string,
        sign TYPE xsdboolean,
        lowercase TYPE xsdboolean,
        values TYPE ty_values_tt,
      END OF ty_domain,
      BEGIN OF ty_sap_domain,
        dd01v TYPE dd01v,
        t_dd07v TYPE dd07v_tab,
      END OF ty_sap_domain.
    METHODS read_domain RETURNING value(r_doma) TYPE ty_sap_domain
      RAISING lcx_abap_doc_exception.
    METHODS get_values IMPORTING i_dd07v TYPE dd07v_tab
      RETURNING value(r_values) TYPE ty_values_tt.
    DATA:
      domname TYPE ddobjname.
ENDCLASS.

CLASS lcl_object_doma IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_key ).
    domname = i_key-obj_name.
  ENDMETHOD.

  METHOD exists.
    DATA: lv_domname TYPE dd01l-domname.
    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = obj_key-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      ls_domain TYPE ty_domain,
      ls_sap_doma TYPE ty_sap_domain.

    ls_sap_doma = read_domain( ).

    ls_domain-header = fill_header( ls_sap_doma-dd01v-ddtext ).
    ls_domain-conv_exit = ls_sap_doma-dd01v-convexit.
    ls_domain-type = ls_sap_doma-dd01v-datatype.
    ls_domain-length = ls_sap_doma-dd01v-leng.
    ls_domain-output_length = ls_sap_doma-dd01v-outputlen.
    ls_domain-sign = ls_sap_doma-dd01v-signflag.
    ls_domain-lowercase = ls_sap_doma-dd01v-lowercase.
    ls_domain-values = get_values( ls_sap_doma-t_dd07v ).

    r_doc = lcl_xml_doc=>create_from_data( i_root = obj_key-object i_data = ls_domain ).
  ENDMETHOD.

  METHOD read_domain.
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = domname
        langu         = sy-langu
      IMPORTING
        dd01v_wa      = r_doma-dd01v
      TABLES
        dd07v_tab     = r_doma-t_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR r_doma-dd01v IS INITIAL.
      lcx_abap_doc_exception=>raise( 'Domain - Error from DDIF_DOMA_GET' ).
    ENDIF.

    DELETE r_doma-t_dd07v WHERE appval = abap_true.
    SORT r_doma-t_dd07v BY valpos ASCENDING ddlanguage ASCENDING.
  ENDMETHOD.

  METHOD get_values.
    DATA:
      ls_value TYPE ty_value.

    FIELD-SYMBOLS:
      <dd07v> TYPE dd07v.

    LOOP AT i_dd07v ASSIGNING <dd07v> WHERE ddlanguage = sy-langu.
      CLEAR ls_value.
      ls_value-pos = <dd07v>-valpos.
      ls_value-low = <dd07v>-domvalue_l.
      ls_value-high = <dd07v>-domvalue_h.
      ls_value-description = <dd07v>-ddtext.
      APPEND ls_value TO r_values.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

"$. Endregion REPO OBJECTS

CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      status_success TYPE symsgty VALUE 'S',
      status_error TYPE symsgty VALUE 'E',
      status_unknown TYPE symsgty VALUE 'U'.
    CLASS-METHODS:
      get_instance RETURNING value(r_log) TYPE REF TO lcl_log.
    METHODS:
      add_error IMPORTING i_object TYPE clike i_msg TYPE clike,
      add_from_ex IMPORTING i_ex TYPE REF TO cx_root,
      add_success IMPORTING i_object TYPE clike i_msg TYPE clike,
      add_unknown IMPORTING i_object TYPE clike,
      set_show_unknown IMPORTING i_value TYPE abap_bool,
      show.
  PRIVATE SECTION.
    METHODS:
      adjust_columns IMPORTING i_salv_table TYPE REF TO cl_salv_table,
      get_icon IMPORTING i_status TYPE symsgty RETURNING value(r_icon) TYPE icon_l2.
    TYPES:
      BEGIN OF ty_entry,
        object TYPE string,
        status TYPE alstate,
        status_icon TYPE balimsgty,
        status_txt TYPE baltmsg.
    TYPES:
      END OF ty_entry.
    TYPES:
      ty_entry_tt TYPE STANDARD TABLE OF ty_entry.
    DATA:
      entries TYPE ty_entry_tt,
      show_unknown TYPE abap_bool.
    CLASS-DATA:
      instance TYPE REF TO lcl_log.
ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS BOUND.
      r_log = instance.
    ELSE.
      CREATE OBJECT instance.
      r_log = instance.
    ENDIF.
  ENDMETHOD.

  METHOD add_success.
    DATA:
      ls_entry TYPE ty_entry.

    ls_entry-object = i_object.
    ls_entry-status = status_success.
    ls_entry-status_icon = get_icon( ls_entry-status ).
    ls_entry-status_txt = i_msg.
    APPEND ls_entry TO entries.
  ENDMETHOD.

  METHOD add_error.
    DATA:
      ls_entry TYPE ty_entry.

    ls_entry-object = i_object.
    ls_entry-status = status_error.
    ls_entry-status_icon = get_icon( ls_entry-status ).
    ls_entry-status_txt = i_msg.
    APPEND ls_entry TO entries.
  ENDMETHOD.

  METHOD add_unknown.
    DATA:
      ls_entry TYPE ty_entry.

    CHECK show_unknown = abap_true.
    ls_entry-object = i_object.
    ls_entry-status = status_unknown.
    ls_entry-status_icon = get_icon( ls_entry-status ).
    ls_entry-status_txt = |Object type { substring_before( val = i_object sub = '.' ) } is not supported|.
    APPEND ls_entry TO entries.
  ENDMETHOD.

  METHOD add_from_ex.
    add_error( i_object = '' i_msg = i_ex->get_text( ) ).
  ENDMETHOD.

  METHOD set_show_unknown.
    me->show_unknown = i_value.
  ENDMETHOD.

  METHOD show.
    DATA:
      lo_salv_table TYPE REF TO cl_salv_table.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_salv_table
      CHANGING
        t_table      = entries ).

    adjust_columns( lo_salv_table ).
    lo_salv_table->display( ).
  ENDMETHOD.

  METHOD adjust_columns.
    DATA:
      lo_columns TYPE REF TO cl_salv_columns_table.

    lo_columns = i_salv_table->get_columns( ).
    lo_columns->set_optimize( ).
  ENDMETHOD.

  METHOD get_icon.
    CASE i_status.
      WHEN status_unknown.
        r_icon = icon_status.
      WHEN status_success.
        r_icon = icon_led_green.
      WHEN status_error.
        r_icon = icon_led_red.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_set DEFINITION.
  PUBLIC SECTION.
    TYPES:
      ty_object_set_tt TYPE STANDARD TABLE OF REF TO lcl_object_set WITH DEFAULT KEY.

    METHODS add IMPORTING i_object TYPE REF TO lcl_repo_object.
    METHODS clear.
    METHODS constructor IMPORTING i_package_name TYPE devclass.
    METHODS get_by_type IMPORTING i_type TYPE tadir-object
      RETURNING value(r_set) TYPE REF TO lcl_object_set.
    METHODS get_package_name RETURNING value(r_value) TYPE devclass.
    METHODS get_type RETURNING value(r_value) TYPE string.
    METHODS size RETURNING value(r_value) TYPE i.
    METHODS split_by_type
      RETURNING value(r_sets) TYPE ty_object_set_tt.
    METHODS to_table
      RETURNING value(r_objects) TYPE lcl_repo_object=>ty_object_tt.
    METHODS to_xml
      RETURNING value(r_doc) TYPE REF TO lcl_xml_doc.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_object.
            INCLUDE TYPE lcl_repo_object=>ty_obj_key AS obj_key.
    TYPES:
         ref TYPE REF TO lcl_repo_object,
     END OF ty_object,
     ty_objects_ts TYPE SORTED TABLE OF ty_object WITH UNIQUE KEY obj_key.

    DATA:
      package_name TYPE devclass,
      objects TYPE ty_objects_ts.
ENDCLASS.

CLASS lcl_object_set IMPLEMENTATION.
  METHOD constructor.
    ASSERT i_package_name IS NOT INITIAL.
    me->package_name = i_package_name.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      ls_object TYPE ty_object.

    r_doc = lcl_xml_doc=>create( ).
    CHECK lines( objects ) <> 0.
    r_doc->append_simple( 'OBJECTS' ).
    LOOP AT objects INTO ls_object.
      r_doc->add_doc( ls_object-ref->to_xml(  ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add.
    DATA:
      ls_object TYPE ty_object.

    ASSERT i_object IS BOUND.
    ASSERT i_object->get_package_name( ) = me->package_name.
    ls_object-obj_key = i_object->get_key( ).
    ls_object-ref = i_object.
    INSERT ls_object INTO TABLE objects.
  ENDMETHOD.

  METHOD clear.
    CLEAR objects.
  ENDMETHOD.

  METHOD get_by_type.
    DATA:
      ls_object TYPE ty_object.

    CREATE OBJECT r_set
      EXPORTING
        i_package_name = me->package_name.

    LOOP AT objects INTO ls_object WHERE object = i_type.
      r_set->add( ls_object-ref ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type.
    DATA:
      ls_object TYPE ty_object.

    r_value = 'EMPTY'.
    LOOP AT objects INTO ls_object.
      IF r_value = 'EMPTY'.
        r_value = ls_object-object.
      ELSEIF r_value <> ls_object-object.
        r_value = 'MIXED'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD size.
    r_value = lines( objects ).
  ENDMETHOD.

  METHOD split_by_type.
    DATA:
      ls_object TYPE ty_object,
      lo_set TYPE REF TO lcl_object_set.

    LOOP AT objects INTO ls_object.
      AT NEW object.
        CREATE OBJECT lo_set
          EXPORTING
            i_package_name = me->package_name.
      ENDAT.
      lo_set->add( ls_object-ref ).
      AT END OF object.
        APPEND lo_set TO r_sets.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD to_table.
    DATA:
      ls_object TYPE ty_object.

    LOOP AT objects INTO ls_object.
      APPEND ls_object-ref TO r_objects.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_package_name.
    r_value = me->package_name.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_package DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_name TYPE devclass.
    METHODS add_object IMPORTING i_object TYPE REF TO lcl_repo_object.
    METHODS get_name RETURNING value(r_name) TYPE devclass.
    METHODS get_objects
      RETURNING value(r_set) TYPE REF TO lcl_object_set.
  PRIVATE SECTION.
    DATA:
      objects_set TYPE REF TO lcl_object_set,
      name TYPE devclass.
ENDCLASS.

CLASS lcl_package IMPLEMENTATION.
  METHOD constructor.
    ASSERT i_name IS NOT INITIAL.
    name = i_name.
    CREATE OBJECT objects_set
      EXPORTING
        i_package_name = i_name.
  ENDMETHOD.

  METHOD add_object.
    ASSERT i_object->get_package_name( ) = name.
    objects_set->add( i_object ).
  ENDMETHOD.

  METHOD get_name.
    r_name = name.
  ENDMETHOD.

  METHOD get_objects.
    r_set = objects_set.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_repository DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_package,
        name TYPE devclass,
        ref TYPE REF TO lcl_package,
      END OF ty_package,
      ty_package_ts TYPE SORTED TABLE OF ty_package WITH UNIQUE KEY name.

    METHODS add_object IMPORTING i_obj_key TYPE lcl_repo_object=>ty_obj_key.
    METHODS clear.
    METHODS constructor.
    METHODS get_package IMPORTING i_name TYPE devclass
      RETURNING value(r_package) TYPE REF TO lcl_package.
    METHODS get_packages RETURNING value(r_packages) TYPE ty_package_ts.
    METHODS get_supported_types RETURNING value(r_types) TYPE devtyrange.
    METHODS get_objects_count RETURNING value(r_value) TYPE i.
    METHODS is_registered IMPORTING i_type TYPE tadir-object
      RETURNING value(r_value) TYPE abap_bool.
    METHODS register_type IMPORTING i_obj_type TYPE tadir-object i_class_name TYPE seoclsname.
    METHODS set_config IMPORTING i_config TYPE REF TO lcl_config.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_obj_type,
        obj_type TYPE tadir-object,
        class_name TYPE seoclsname,
      END OF ty_obj_type,
      ty_obj_types_ts TYPE SORTED TABLE OF ty_obj_type
        WITH UNIQUE KEY obj_type.

    DATA:
      config TYPE REF TO lcl_config,
      packages TYPE ty_package_ts,
      obj_types TYPE ty_obj_types_ts.

    METHODS create_object IMPORTING i_obj_key TYPE lcl_repo_object=>ty_obj_key
      RETURNING value(r_object) TYPE REF TO lcl_repo_object.
ENDCLASS.

CLASS lcl_repository IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT config.
  ENDMETHOD.

  METHOD add_object.
    DATA:
      lo_object TYPE REF TO lcl_repo_object.

    lo_object = create_object( i_obj_key ).
    CHECK lo_object IS BOUND AND lo_object->exists( ) = abap_true.
    get_package( lo_object->get_package_name( ) )->add_object( lo_object ).
  ENDMETHOD.

  METHOD clear.
    CLEAR packages.
  ENDMETHOD.

  METHOD create_object.
    DATA:
      ls_obj_type TYPE ty_obj_type.

    CLEAR r_object.

    READ TABLE obj_types INTO ls_obj_type
      WITH TABLE KEY obj_type = i_obj_key-object.

    CHECK sy-subrc = 0.
    CREATE OBJECT r_object TYPE (ls_obj_type-class_name)
      EXPORTING
        i_key = i_obj_key.
    r_object->set_config( config ).
  ENDMETHOD.

  METHOD get_objects_count.
    DATA:
      lv_package_count TYPE i,
      lo_package TYPE REF TO lcl_package,
      ls_package TYPE ty_package.

    LOOP AT me->packages INTO ls_package.
      lo_package = ls_package-ref.
      lv_package_count = lo_package->get_objects( )->size( ).
      ADD lv_package_count TO r_value.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_supported_types.
    DATA:
      ls_type_range TYPE devtyrgln,
      ls_obj_type TYPE ty_obj_type.

    CLEAR r_types.

    ls_type_range-sign = 'I'.
    ls_type_range-option = 'EQ'.

    LOOP AT obj_types INTO ls_obj_type.
      ls_type_range-low = ls_obj_type-obj_type.
      APPEND ls_type_range TO r_types.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_package.
    DATA:
      ls_package TYPE ty_package.

    READ TABLE packages INTO ls_package WITH TABLE KEY name = i_name.
    IF sy-subrc = 0.
      r_package = ls_package-ref.
    ELSE.
      CREATE OBJECT r_package
        EXPORTING
          i_name = i_name.
      ls_package-name = i_name.
      ls_package-ref = r_package.
      INSERT ls_package INTO TABLE packages.
    ENDIF.
  ENDMETHOD.

  METHOD get_packages.
    r_packages = packages.
  ENDMETHOD.

  METHOD is_registered.
    READ TABLE obj_types TRANSPORTING NO FIELDS
      WITH TABLE KEY obj_type = i_type.
    IF sy-subrc = 0.
      r_value = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD register_type.
    DATA:
      ls_obj_type TYPE ty_obj_type.

    ls_obj_type-obj_type = i_obj_type.
    ls_obj_type-class_name = i_class_name.
    INSERT ls_obj_type INTO TABLE me->obj_types.
  ENDMETHOD.

  METHOD set_config.
    ASSERT i_config IS BOUND.
    me->config = i_config.
  ENDMETHOD.

ENDCLASS.
"$. Region Documentation
CLASS lcl_index DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_index_item,
        obj_name TYPE string,
        description TYPE string,
        obj_type TYPE string,
        obj_type_name TYPE string,
        doc_name TYPE string,
        doc_section TYPE string,
      END OF ty_index_item.

    METHODS constructor IMPORTING
      i_name TYPE clike
      i_title TYPE clike
      i_package_name TYPE clike.
    METHODS add_item IMPORTING i_item TYPE ty_index_item.
    METHODS get_name RETURNING value(r_value) TYPE string.
    METHODS get_package_name RETURNING value(r_value) TYPE devclass.
    METHODS get_title RETURNING value(r_value) TYPE string.
    METHODS to_xml RETURNING value(r_doc) TYPE REF TO lcl_xml_doc
      RAISING lcx_abap_doc_exception.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_index_obj_type,
        name TYPE string,
        description TYPE string,
      END OF ty_index_obj_type,
      ty_index_obj_types_ts TYPE SORTED TABLE OF ty_index_obj_type WITH UNIQUE KEY name,

      BEGIN OF ty_index_object,
        obj_type TYPE string,
        name TYPE string,
        description TYPE string,
        doc_name TYPE string,
        doc_section TYPE string,
      END OF ty_index_object,
      ty_index_objects_ts TYPE SORTED TABLE OF ty_index_object WITH UNIQUE KEY obj_type name.

    DATA:
      name TYPE string,
      title TYPE string,
      package_name TYPE devclass,
      obj_types TYPE ty_index_obj_types_ts,
      objects TYPE ty_index_objects_ts.
ENDCLASS.

CLASS lcl_index IMPLEMENTATION.
  METHOD constructor.
    ASSERT i_name IS NOT INITIAL.
    ASSERT i_title IS NOT INITIAL.
    ASSERT i_package_name IS NOT INITIAL.
    me->name = i_name.
    me->title = i_title.
    me->package_name = i_package_name.
  ENDMETHOD.

  METHOD add_item.
    DATA:
      ls_object TYPE ty_index_object,
      ls_obj_type TYPE ty_index_obj_type.

    ls_object-name = i_item-obj_name.
    ls_object-obj_type = i_item-obj_type.
    ls_object-description = i_item-description.
    ls_object-doc_name = i_item-doc_name.
    ls_object-doc_section = i_item-doc_section.
    INSERT ls_object INTO TABLE objects.

    ls_obj_type-name = i_item-obj_type.
    ls_obj_type-description = i_item-obj_type_name.
    INSERT ls_obj_type INTO TABLE obj_types.
  ENDMETHOD.

  METHOD to_xml.
    DATA:
      lo_doc_elem TYPE REF TO if_ixml_element,
      lo_index_elem TYPE REF TO if_ixml_element.

    r_doc = lcl_xml_doc=>create( ).
    IF objects IS NOT INITIAL.
      lo_index_elem = r_doc->append_simple( 'INDEX' ).
      lo_index_elem->append_child( r_doc->create_data_element( i_name = 'OBJECTS' i_data = objects ) ).
      lo_index_elem->append_child( r_doc->create_data_element( i_name = 'TYPES' i_data = obj_types ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_name.
    r_value = me->name.
  ENDMETHOD.

  METHOD get_title.
    r_value = me->title.
  ENDMETHOD.

  METHOD get_package_name.
    r_value = me->package_name.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_doc DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create_for_index IMPORTING i_index TYPE REF TO lcl_index
      RETURNING value(r_unit) TYPE REF TO lcl_abap_doc.
    CLASS-METHODS create_for_object IMPORTING i_object TYPE REF TO lcl_repo_object
      RETURNING value(r_unit) TYPE REF TO lcl_abap_doc.
    CLASS-METHODS create_for_set IMPORTING i_set TYPE REF TO lcl_object_set
      RETURNING value(r_unit) TYPE REF TO lcl_abap_doc.
    METHODS get_name RETURNING value(r_value) TYPE string.
    METHODS get_title RETURNING value(r_value) TYPE string.
    METHODS get_timestamp RETURNING value(r_value) TYPE string.
    METHODS get_package_name RETURNING value(r_value) TYPE devclass.
    METHODS get_type RETURNING value(r_value) TYPE string.
    METHODS to_xml IMPORTING i_index TYPE REF TO lcl_index
      RETURNING value(r_doc) TYPE REF TO lcl_xml_doc
      RAISING lcx_abap_doc_exception.
  PRIVATE SECTION.
    METHODS constructor IMPORTING
      i_name TYPE clike
      i_type TYPE clike
      i_package_name TYPE clike
      i_title TYPE clike.
    METHODS update_index IMPORTING i_index TYPE REF TO lcl_index.
    DATA:
      name TYPE string,
      type TYPE string,
      title TYPE string,
      package_name TYPE devclass,
      index TYPE REF TO lcl_index,
      objects TYPE REF TO lcl_object_set.
ENDCLASS.

CLASS lcl_abap_doc IMPLEMENTATION.

  METHOD to_xml.
    DATA:
      lo_doc_elem TYPE REF TO if_ixml_element.

    r_doc = lcl_xml_doc=>create( ).

    lo_doc_elem = r_doc->append_simple( 'ABAP_DOC' ).
    lo_doc_elem->append_child( r_doc->create_simple( i_name = 'TYPE' i_value = get_type( ) ) ).
    lo_doc_elem->append_child( r_doc->create_simple( i_name = 'TITLE' i_value = get_title( ) ) ).
    lo_doc_elem->append_child( r_doc->create_simple( i_name = 'PACKAGE' i_value = get_package_name( ) ) ).
    lo_doc_elem->append_child( r_doc->create_simple( i_name = 'TIMESTAMP' i_value = get_timestamp( ) ) ).

    IF objects->size( ) > 0.
      r_doc->add_doc( objects->to_xml( ) ).
      update_index( i_index ).
    ELSEIF me->index IS BOUND.
      r_doc->add_doc( index->to_xml( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    ASSERT i_name IS NOT INITIAL.
    ASSERT i_package_name IS NOT INITIAL.
    ASSERT i_type IS NOT INITIAL.
    ASSERT i_title IS NOT INITIAL.

    me->name = i_name.
    me->type = i_type.
    me->package_name = i_package_name.
    me->title = i_title.
    CREATE OBJECT objects
      EXPORTING
        i_package_name = i_package_name.
  ENDMETHOD.

  METHOD create_for_index.
    CREATE OBJECT r_unit
      EXPORTING
        i_name         = i_index->get_name( )
        i_type         = 'INDEX'
        i_package_name = i_index->get_package_name( )
        i_title        = |Summary for package { i_index->get_package_name( ) }|.

    r_unit->index = i_index.
  ENDMETHOD.

  METHOD create_for_object.
    CREATE OBJECT r_unit
      EXPORTING
        i_name         = |{ i_object->get_type( ) }.{ i_object->get_name( ) }|
        i_type         = |{ i_object->get_type( ) }.SINGLE|
        i_package_name = i_object->get_package_name( )
        i_title        = |{ lcl_type_info=>get_type_descr( i_object->get_type( ) ) } { i_object->get_name( ) }|.

    r_unit->objects->add( i_object ).
  ENDMETHOD.

  METHOD create_for_set.
    ASSERT i_set IS BOUND.

    CREATE OBJECT r_unit
      EXPORTING
        i_name         = |{ i_set->get_type( ) }.SET|
        i_type         = |{ i_set->get_type( ) }.MULTI|
        i_package_name = i_set->get_package_name( )
        i_title = |List of objects: { lcl_type_info=>get_type_descr( i_set->get_type( ) ) }|.

    r_unit->objects = i_set.
  ENDMETHOD.

  METHOD get_name.
    r_value = me->name.
  ENDMETHOD.

  METHOD get_title.
    r_value = me->title.
  ENDMETHOD.

  METHOD get_package_name.
    r_value = me->package_name.
  ENDMETHOD.

  METHOD get_type.
    r_value = me->type.
  ENDMETHOD.

  METHOD get_timestamp.
    r_value = |{ sy-datum(4) }-{ sy-datum+4(2) }-{ sy-datum+6(2) }T{ sy-uzeit(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }|.
  ENDMETHOD.

  METHOD update_index.
    DATA:
      ls_item TYPE lcl_index=>ty_index_item,
      ls_header TYPE lcl_repo_object=>ty_header,
      lo_object TYPE REF TO lcl_repo_object,
      lt_objects TYPE lcl_repo_object=>ty_object_tt.

    lt_objects = objects->to_table( ).
    LOOP AT lt_objects INTO lo_object.
      CLEAR ls_item.
      ls_header = lo_object->get_header( ).
      ls_item-obj_name = ls_header-name.
      ls_item-obj_type = ls_header-obj_type.
      ls_item-description = ls_header-description.
      ls_item-obj_type_name = ls_header-obj_type_name.
      ls_item-doc_name = get_name( ).
      IF objects->size( ) > 1.
        ls_item-doc_section = lo_object->get_name( ).
      ENDIF.
      i_index->add_item( ls_item ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

"$. Endregion Documentation

"$. Region DOC BUILDERS

CLASS lcl_doc_builder DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_progress,
        value TYPE i,
        max TYPE i,
        percent TYPE i,
        object TYPE string,
        action TYPE string,
      END OF ty_progress.

    METHODS constructor.
    METHODS generate_docs ABSTRACT IMPORTING i_repository TYPE REF TO lcl_repository
      RAISING lcx_abap_doc_exception.
    METHODS set_config IMPORTING i_config TYPE REF TO lcl_config.
    EVENTS progress EXPORTING value(info) TYPE ty_progress.
  PROTECTED SECTION.
    DATA:
      config TYPE REF TO lcl_config,
      log TYPE REF TO lcl_log.
    METHODS on_progress IMPORTING
      i_object TYPE clike
      i_action TYPE clike
      i_value TYPE i
      i_max TYPE i.
    METHODS process_config.
ENDCLASS.

CLASS lcl_doc_builder IMPLEMENTATION.
  METHOD constructor.
    log = lcl_log=>get_instance( ).
    CREATE OBJECT config.
  ENDMETHOD.

  METHOD set_config.
    ASSERT i_config IS BOUND.
    me->config = i_config.
    process_config( ).
  ENDMETHOD.

  METHOD on_progress.
    DATA:
      ls_info TYPE ty_progress.

    ls_info-object = i_object.
    ls_info-action = i_action.
    ls_info-value = i_value.
    ls_info-max = i_max.
    IF i_max <> 0.
      ls_info-percent = 100 * i_value / i_max.
    ENDIF.
    RAISE EVENT progress EXPORTING info = ls_info.
  ENDMETHOD.

  METHOD process_config.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_html_doc_builder DEFINITION INHERITING FROM lcl_doc_builder.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS generate_docs REDEFINITION.
  PROTECTED SECTION.
    METHODS process_config REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      generate_doc IMPORTING
        i_object TYPE REF TO lcl_repo_object OPTIONAL
        i_set TYPE REF TO lcl_object_set OPTIONAL
        i_index TYPE REF TO lcl_index,
      generate_docs_aggregated IMPORTING i_set TYPE REF TO lcl_object_set
        i_index TYPE REF TO lcl_index,
      generate_docs_for_package IMPORTING i_package TYPE REF TO lcl_package,
      get_root_path RETURNING value(r_path) TYPE string,
      get_doc_path IMPORTING
          i_doc TYPE REF TO lcl_abap_doc
          i_ext TYPE string
        RETURNING value(r_path) TYPE string,
      save_stylesheet RAISING lcx_abap_doc_exception,
      should_be_aggregated IMPORTING i_repo_object TYPE REF TO lcl_repo_object
        RETURNING value(r_value) TYPE abap_bool.
    DATA:
      xslt TYPE cxsltdesc,
      save_xml TYPE abap_bool,
      package_index TYPE abap_bool,
      aggregated_types TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line,
      dir_sep TYPE c,
      directory TYPE string,
      objects_processed TYPE i,
      objects_total TYPE i.
ENDCLASS.

CLASS lcl_html_doc_builder IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    dir_sep = lcl_frontend=>dir_sep.
    INSERT 'DTEL' INTO TABLE aggregated_types.
    INSERT 'DOMA' INTO TABLE aggregated_types.
    "Standard XSLT transformation
    xslt = 'ZID_OBJECTS_2_HTML'.
  ENDMETHOD.

  METHOD process_config.
    DATA:
      lv_custom_xslt TYPE cxsltdesc.

    save_xml = config->get_param_boolean( 'SAVE_XML' ).
    lv_custom_xslt = config->get_param_string( 'XSLT' ).
    IF lv_custom_xslt IS NOT INITIAL.
      xslt = lv_custom_xslt.
    ENDIF.
    package_index = config->get_param_boolean( 'PACKAGE_INDEX' ).
  ENDMETHOD.

  METHOD generate_docs.
    DATA:
      ls_package TYPE lcl_repository=>ty_package,
      lt_packages TYPE lcl_repository=>ty_package_ts.

    me->objects_total = i_repository->get_objects_count( ).
    directory = lcl_frontend=>select_directory( 'HTML Documentation' ).
    CHECK directory IS NOT INITIAL.
    lt_packages = i_repository->get_packages( ).
    LOOP AT lt_packages INTO ls_package.
      generate_docs_for_package( ls_package-ref ).
    ENDLOOP.
    save_stylesheet( ).
  ENDMETHOD.

  METHOD generate_docs_for_package.
    DATA:
      lo_index TYPE REF TO lcl_index,
      lt_objects TYPE lcl_repo_object=>ty_object_tt,
      lo_set TYPE REF TO lcl_object_set,
      lo_repo_object TYPE REF TO lcl_repo_object.

    lt_objects = i_package->get_objects( )->to_table( ).

    CREATE OBJECT lo_index
      EXPORTING
        i_name         = 'index'
        i_package_name = i_package->get_name( )
        i_title        = |List of objects for package { i_package->get_name( ) }|.

    CREATE OBJECT lo_set
      EXPORTING
        i_package_name = i_package->get_name( ).

    LOOP AT lt_objects INTO lo_repo_object.
      ADD 1 TO me->objects_processed.
      on_progress( i_object = lo_repo_object->get_id( ) i_action = 'Processing' i_value = me->objects_processed i_max = me->objects_total ).
      IF should_be_aggregated( lo_repo_object ) = abap_true.
        lo_set->add( lo_repo_object ).
      ELSE.
        generate_doc( i_object = lo_repo_object i_index = lo_index ).
      ENDIF.

    ENDLOOP.
    generate_docs_aggregated( i_set = lo_set i_index = lo_index ).
    IF package_index = abap_true.
      generate_doc( i_index = lo_index ).
    ENDIF.
  ENDMETHOD.

  METHOD generate_doc.
    DATA:
      lo_doc TYPE REF TO lcl_abap_doc,
      lo_xml TYPE REF TO lcl_xml_doc,
      lo_ex TYPE REF TO lcx_abap_doc_exception,
      lo_xslt_ex TYPE REF TO cx_transformation_error,
      lv_path TYPE string,
      lv_xmlx TYPE xstring,
      lv_htmlx TYPE xstring.

    IF i_object IS SUPPLIED.
      lo_doc = lcl_abap_doc=>create_for_object( i_object ).
    ELSEIF i_set IS SUPPLIED.
      lo_doc = lcl_abap_doc=>create_for_set( i_set ).
    ELSE.
      lo_doc = lcl_abap_doc=>create_for_index( i_index ).
    ENDIF.

    TRY.
        lo_xml = lo_doc->to_xml( i_index = i_index ).
      CATCH lcx_abap_doc_exception INTO lo_ex.
        log->add_error( i_object = lo_doc->get_name( ) i_msg = |XML generation error: { lo_ex->get_text( ) }| ).
        RETURN.
    ENDTRY.

    CHECK lo_xml->is_empty( ) = abap_false.
    lv_xmlx = lo_xml->to_xstring( ).

    "XML Export
    IF save_xml = abap_true.
      lv_path = get_doc_path( i_doc = lo_doc i_ext = '.xml' ).
      lcl_frontend=>file_download( EXPORTING i_path = lv_path i_xstring = lv_xmlx ).
    ENDIF.

    TRY.
        CALL TRANSFORMATION (xslt)
          SOURCE XML lv_xmlx
          RESULT XML lv_htmlx.
        "HTML
        lv_path = get_doc_path( i_doc = lo_doc i_ext = '.html' ).
        lcl_frontend=>file_download( EXPORTING i_path = lv_path i_xstring = lv_htmlx ).
        log->add_success( i_object = lo_doc->get_name( ) i_msg = |Exported| ).
      CATCH cx_transformation_error INTO lo_xslt_ex.
        log->add_error( i_object = lo_doc->get_name( ) i_msg = |Transformation error: { lo_xslt_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD generate_docs_aggregated.
    DATA:
      lo_set TYPE REF TO lcl_object_set,
      lt_sets TYPE lcl_object_set=>ty_object_set_tt.

    lt_sets = i_set->split_by_type( ).
    LOOP AT lt_sets INTO lo_set.
      generate_doc( i_set = lo_set i_index = i_index ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_root_path.
    r_path = |{ directory }{ dir_sep }AbapDocs|.
  ENDMETHOD.

  METHOD get_doc_path.
    r_path = |{ get_root_path( ) }{ dir_sep }{ i_doc->get_package_name( ) }{ dir_sep }{ i_doc->get_name( ) }{ i_ext }|.
  ENDMETHOD.

  METHOD save_stylesheet.
    CONSTANTS:
      c_stylesheet TYPE string VALUE 'abapdoc-j.css'.
    DATA:
      lv_path TYPE string,
      lv_cssx TYPE xstring,
      lo_mime TYPE REF TO if_mr_api.

    lo_mime = cl_mime_repository_api=>get_api( ).
    lo_mime->get(
      EXPORTING i_url = '/SAP/PUBLIC/CSS/' && c_stylesheet
        i_check_authority = abap_false
      IMPORTING e_content = lv_cssx
      EXCEPTIONS
        parameter_missing = 1
        error_occured = 2
        not_found = 3
        permission_failure = 4
        OTHERS = 5 ).

    IF sy-subrc <> 0.
      lcx_abap_doc_exception=>raise_from_sy( ).
    ENDIF.

    lv_path = |{ get_root_path( ) }{ dir_sep }{ c_stylesheet }|.
    lcl_frontend=>file_download( EXPORTING i_path = lv_path i_xstring = lv_cssx ).
    log->add_success( i_object = 'CSS' i_msg = |Exported stylesheet ({ c_stylesheet })| ).
  ENDMETHOD.

  METHOD should_be_aggregated.
    READ TABLE aggregated_types TRANSPORTING NO FIELDS
      WITH TABLE KEY table_line = i_repo_object->get_type( ).
    IF sy-subrc = 0.
      r_value = abap_true.
    ELSE.
      r_value = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

"$. Endregion DOC BUILDERS

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      add_free_obj IMPORTING i_type TYPE clike ir_range TYPE STANDARD TABLE,
      get_transport_request RETURNING value(r_request) TYPE e070-trkorr,
      set_config IMPORTING i_config TYPE REF TO lcl_config,
      start.
    DATA:
      r_progs TYPE scit_prgnm,
      r_fugrs TYPE scit_fugr,
      r_pckg TYPE scit_devc,
      r_clas TYPE scit_clas,
      r_fpckg TYPE scit_devc,
      r_fresp TYPE scit_resp,
      r_tran TYPE RANGE OF trkorr,
      r_ddics TYPE scit_tabl,
      r_typps TYPE scit_typp,
      p_show_unsupported TYPE boolean.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_object.
            INCLUDE TYPE lcl_repo_object=>ty_obj_key AS key.
    TYPES:
      author TYPE tadir-author,
      parent_object TYPE e071-object,
      parent_obj_name TYPE e071-obj_name,
      END OF ty_object,
      BEGIN OF ty_free_obj,
        type TYPE sci_trobj,
        range TYPE scit_objn,
      END OF ty_free_obj,

     tty_free_objs TYPE SORTED TABLE OF ty_free_obj WITH UNIQUE KEY type,
     ty_request_task_tt TYPE STANDARD TABLE OF e070,
     ty_tr_objects_tt TYPE STANDARD TABLE OF e071,
     ty_objects_tt TYPE STANDARD TABLE OF ty_object.

    METHODS:
      build_object_set,
      build_dictionary_set,
      build_obj_set_for_type_group,
      build_obj_set_for_program,
      build_obj_set_for_package,
      build_obj_set_for_func_group,
      build_obj_set_for_class,
      build_obj_set_for_request,
      build_obj_set_for_free_objs,
      fill_prog_responsible
        CHANGING ct_progs TYPE ty_objects_tt,
      get_transport_tasks
        EXPORTING ot_tasks TYPE ty_request_task_tt,
      get_task_objects
        IMPORTING it_tasks TYPE ty_request_task_tt
        EXPORTING ot_tr_objects TYPE ty_tr_objects_tt,
      get_responsible
        IMPORTING i_author TYPE tadir-author i_cnam TYPE trdir-cnam i_unam TYPE trdir-unam
        RETURNING value(ov_resp) TYPE tadir-author,
      on_progress FOR EVENT progress OF lcl_doc_builder IMPORTING info,
      register_repo_types,
      process_objects,
      show_progress IMPORTING i_percent TYPE i i_text TYPE string.
    DATA:
      log TYPE REF TO lcl_log,
      config TYPE REF TO lcl_config,
      objects TYPE ty_objects_tt,
      free_objs TYPE tty_free_objs,
      repository TYPE REF TO lcl_repository.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT repository.
  ENDMETHOD.

  METHOD register_repo_types.
    repository->register_type( i_obj_type = 'FUGR' i_class_name = 'LCL_OBJECT_FUGR' ).
    repository->register_type( i_obj_type = 'CLAS' i_class_name = 'LCL_OBJECT_CLAS' ).
    repository->register_type( i_obj_type = 'INTF' i_class_name = 'LCL_OBJECT_INTF' ).
    repository->register_type( i_obj_type = 'TABL' i_class_name = 'LCL_OBJECT_TABL' ).
    repository->register_type( i_obj_type = 'DTEL' i_class_name = 'LCL_OBJECT_DTEL' ).
    repository->register_type( i_obj_type = 'DOMA' i_class_name = 'LCL_OBJECT_DOMA' ).
  ENDMETHOD.

  METHOD set_config.
    me->config = i_config.
  ENDMETHOD.

  METHOD start.
    log = lcl_log=>get_instance( ).
    log->set_show_unknown( me->config->get_param_boolean( 'SHOW_NOT_SUPPORTED' ) ).
    register_repo_types( ).
    build_object_set( ).
    process_objects( ).
  ENDMETHOD.

  METHOD process_objects.
    DATA:
      lo_builder TYPE REF TO lcl_doc_builder,
      lo_repo_object TYPE REF TO lcl_repo_object,
      ls_object TYPE ty_object,
      lo_ex TYPE REF TO lcx_abap_doc_exception.

    CHECK objects IS NOT INITIAL.

    repository->clear( ).
    repository->set_config( config ).

    LOOP AT objects INTO ls_object.
      IF repository->is_registered( ls_object-key-object ) = abap_true.
        repository->add_object( ls_object-key ).
      ELSE.
        log->add_unknown( |{ ls_object-object }.{ ls_object-obj_name }| ).
      ENDIF.
    ENDLOOP.

    CREATE OBJECT lo_builder TYPE lcl_html_doc_builder.
    SET HANDLER me->on_progress FOR lo_builder.
    lo_builder->set_config( config ).
    TRY.
        lo_builder->generate_docs( repository ).
      CATCH lcx_abap_doc_exception INTO lo_ex.
        log->add_from_ex( lo_ex ).
    ENDTRY.
    log->show( ).

  ENDMETHOD.

  METHOD build_object_set.
    build_dictionary_set( ).
    build_obj_set_for_type_group( ).
    build_obj_set_for_program( ).
    build_obj_set_for_package( ).
    build_obj_set_for_func_group( ).
    build_obj_set_for_class( ).
    build_obj_set_for_request( ).
    build_obj_set_for_free_objs( ).
  ENDMETHOD.

  METHOD build_obj_set_for_program.

    DATA:
      ls_tadir TYPE tadir,
      lt_progs TYPE ty_objects_tt.

    CHECK r_progs IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
      FROM tadir INNER JOIN trdir ON obj_name = trdir~name
      INTO TABLE lt_progs
      WHERE pgmid = 'R3TR'
        AND obj_name IN r_progs
        AND object = 'PROG'
        AND devclass IN r_fpckg
        AND delflag = space.

    fill_prog_responsible( CHANGING ct_progs = lt_progs ).
    DELETE lt_progs WHERE author NOT IN r_fresp.
    APPEND LINES OF lt_progs TO objects.

  ENDMETHOD.

  METHOD build_dictionary_set.

    DATA:
      dict_objs TYPE ty_objects_tt.

    CHECK r_ddics IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE dict_objs
      WHERE pgmid  = 'R3TR'
        AND object IN ('DOMA', 'DTEL','TABL','VIEW','SQLT','TTYP')
        AND obj_name  IN r_ddics
        AND devclass IN r_fpckg
        AND author IN r_fresp
        AND delflag = space.

    APPEND LINES OF dict_objs TO objects.

  ENDMETHOD.

  METHOD build_obj_set_for_type_group.
    DATA:
      type_grp_objs TYPE ty_objects_tt.

    CHECK r_typps IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE type_grp_objs
      WHERE pgmid  = 'R3TR'
        AND object = 'TYPE'
        AND obj_name  IN r_typps
        AND devclass IN r_fpckg
        AND author IN r_fresp
        AND delflag = space.

    APPEND LINES OF type_grp_objs TO objects.

  ENDMETHOD.

  METHOD build_obj_set_for_package.

    DATA:
      pckg_objects TYPE ty_objects_tt.

    CHECK r_pckg IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
      FROM tadir INTO TABLE pckg_objects
      WHERE devclass IN r_pckg
        AND author IN r_fresp
        AND delflag = space.

    APPEND LINES OF pckg_objects TO objects.

  ENDMETHOD.

  METHOD build_obj_set_for_func_group.
    DATA:
      object TYPE ty_object,
      e071 TYPE e071,
      sub_obj TYPE vrso,
      sub_objs TYPE STANDARD TABLE OF vrso,
      fugr TYPE ty_object,
      fugrs TYPE ty_objects_tt.

    CHECK r_fugrs IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE fugrs
      WHERE pgmid  = 'R3TR'
        AND object IN ('FUGR','FUGS','FUGX')
        AND obj_name  IN r_fugrs
        AND devclass IN r_fpckg
        AND author IN r_fresp
        AND delflag = space.

    APPEND LINES OF fugrs TO objects.

  ENDMETHOD.

  METHOD build_obj_set_for_class.
    DATA:
      object TYPE ty_object,
      e071 TYPE e071,
      sub_obj TYPE vrso,
      sub_objs TYPE STANDARD TABLE OF vrso,
      class TYPE ty_object,
      classes TYPE ty_objects_tt.

    CHECK r_clas IS NOT INITIAL.

    SELECT pgmid object obj_name devclass author
    FROM tadir INTO TABLE classes
      WHERE pgmid  = 'R3TR'
        AND object IN ('CLAS','INTF')
        AND obj_name  IN r_clas
        AND devclass IN r_fpckg
        AND author IN r_fresp
        AND delflag = space.

    APPEND LINES OF classes TO objects.

  ENDMETHOD.

  METHOD build_obj_set_for_request.
    DATA:
      tasks TYPE ty_request_task_tt,
      tr_object TYPE e071,
      tr_objects TYPE ty_tr_objects_tt,
      object TYPE ty_object,
      objects TYPE ty_objects_tt,
      tadir TYPE tadir.

    FIELD-SYMBOLS:
      <tr_object> TYPE e071.

    get_transport_tasks( IMPORTING ot_tasks = tasks ).
    get_task_objects( EXPORTING it_tasks = tasks
                      IMPORTING ot_tr_objects = tr_objects ).

    LOOP AT tr_objects ASSIGNING <tr_object>.
      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071  = <tr_object>
        IMPORTING
          we_tadir = tadir.

      SELECT SINGLE * FROM tadir INTO tadir
        WHERE pgmid = tadir-pgmid
          AND object = tadir-object
          AND obj_name = tadir-obj_name
          AND delflag = space.

      CHECK tadir-author IN r_fresp.

      object-pgmid = <tr_object>-pgmid.
      object-object = <tr_object>-object.
      object-obj_name = <tr_object>-obj_name.
      object-parent_object = tadir-object.
      object-parent_obj_name = tadir-obj_name.
      object-devclass = tadir-devclass.
      object-author = tadir-author.
      APPEND object TO objects.

    ENDLOOP.

  ENDMETHOD.

  METHOD build_obj_set_for_free_objs.
    DATA:
      free_obj TYPE ty_free_obj,
      objs TYPE ty_objects_tt.

    CHECK free_objs IS NOT INITIAL.

    LOOP AT free_objs INTO free_obj.
      SELECT pgmid object obj_name devclass author
      FROM tadir INTO TABLE objs
        WHERE pgmid  = 'R3TR'
          AND object = free_obj-type
          AND obj_name  IN free_obj-range
          AND devclass IN r_fpckg
          AND author IN r_fresp
          AND delflag = space.

      APPEND LINES OF objs TO objects.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_transport_tasks.
    CHECK r_tran IS NOT INITIAL.

    CLEAR ot_tasks[].

    SELECT * FROM e070
      INTO TABLE ot_tasks
      WHERE strkorr IN r_tran OR trkorr IN r_tran.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Gets objects from given task
*----------------------------------------------------------------------*
  METHOD get_task_objects.
    CHECK it_tasks[] IS NOT INITIAL.

    CLEAR ot_tr_objects[].

    SELECT * FROM e071
      INTO TABLE ot_tr_objects
      FOR ALL ENTRIES IN it_tasks
      WHERE trkorr = it_tasks-trkorr.

    SORT ot_tr_objects BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM ot_tr_objects COMPARING pgmid object obj_name.

  ENDMETHOD.

  METHOD get_responsible.

    " TADIR-Author
    IF i_author <> space  AND
       i_author <> 'SAP*' AND
       i_author <> 'DDIC'.
      ov_resp = i_author.
    ELSE.
      " TRDIR-Author
      IF i_cnam <> space  AND
         i_cnam <> 'SAP*' AND
         i_cnam <> 'DDIC'.
        ov_resp = i_cnam.
      ELSE.
        " TRDIR-Modifier
        IF i_unam <> space.
          ov_resp = i_unam.
        ELSE.
          ov_resp = '???'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD fill_prog_responsible.
    TYPES:
      BEGIN OF ty_prog_responsible,
        name TYPE trdir-name,
        cnam TYPE trdir-cnam,
        unam TYPE trdir-unam,
      END OF ty_prog_responsible.

    DATA:
      ls_prog_resp TYPE ty_prog_responsible,
      lt_prog_resps TYPE SORTED TABLE OF ty_prog_responsible
        WITH UNIQUE KEY name.

    FIELD-SYMBOLS:
      <prog> TYPE ty_object.

    CHECK ct_progs IS NOT INITIAL.

    SELECT name cnam unam INTO TABLE lt_prog_resps
      FROM trdir
      FOR ALL ENTRIES IN ct_progs
      WHERE name = ct_progs-obj_name(40).

    LOOP AT ct_progs ASSIGNING <prog>.
      CLEAR ls_prog_resp.
      READ TABLE lt_prog_resps INTO ls_prog_resp
        WITH TABLE KEY name = <prog>-obj_name.

      <prog>-author = get_responsible(
          i_author = <prog>-author
          i_cnam = ls_prog_resp-cnam
          i_unam = ls_prog_resp-unam ).

    ENDLOOP.

  ENDMETHOD.

  METHOD on_progress.
    DATA:
      lv_text TYPE string.

    lv_text = |{ info-action } ({ info-value } of { info-max }): { info-object }|.
    show_progress( i_percent = info-percent i_text = lv_text ).
  ENDMETHOD.

  METHOD show_progress.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = i_percent
        text       = i_text.

  ENDMETHOD.

  METHOD add_free_obj.
    DATA:
      ls_free_obj TYPE ty_free_obj.

    CHECK ir_range IS NOT INITIAL.
    ls_free_obj-type = i_type.
    ls_free_obj-range = ir_range.
    INSERT ls_free_obj INTO TABLE free_objs.
  ENDMETHOD.

  METHOD get_transport_request.
    DATA:
      lv_from_date TYPE d,
      lv_trkorr    TYPE e070-trkorr.

    lv_from_date = sy-datum - 90.
    CALL FUNCTION 'TR_F4_REQUESTS'
      EXPORTING
        i_trfunctions       = 'K'
        i_trstatus          = 'RDL'
        i_from_date         = lv_from_date
      IMPORTING
        ev_selected_request = r_request.
  ENDMETHOD.

ENDCLASS.

DATA main TYPE REF TO lcl_main.
DATA config TYPE REF TO lcl_config.

INITIALIZATION.
  CREATE OBJECT main.
  CREATE OBJECT config.

  "$. Region SELECTION SCREEN

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-bl2.
  SELECT-OPTIONS:
    so_ddic FOR sci_dynp-o_ddic MEMORY ID ddic, "ddic type
    so_ddty FOR sci_dynp-o_ddty MEMORY ID ddty. "type group
  SELECTION-SCREEN END  OF  BLOCK bl2.

  SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-bl3.
  SELECT-OPTIONS:
    so_prog FOR sci_dynp-o_repo MEMORY ID prog, "complex program (with includes)
    so_fugr FOR sci_dynp-o_fugr MEMORY ID fugr, "function group
    so_pckg FOR sci_dynp-o_tadir_p MEMORY ID pckg, "package
    so_clas FOR sci_dynp-o_clas MEMORY ID clas. "class
  SELECTION-SCREEN END  OF  BLOCK bl3.

  SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-bl4.
  SELECT-OPTIONS:
    so_tran FOR sci_dynp-o_order MEMORY ID tran.
  SELECTION-SCREEN END  OF  BLOCK bl4.

  SELECTION-SCREEN BEGIN OF BLOCK bl5 WITH FRAME TITLE text-bl5.
  SELECT-OPTIONS:
    so_fpckg FOR sci_dynp-o_tadir_p MEMORY ID fpckg, "filter for package
    so_fresp FOR sci_dynp-o_tadir_r MEMORY ID fresp. "filter for person resonsible
  SELECTION-SCREEN END  OF  BLOCK bl5.

  SELECTION-SCREEN BEGIN OF BLOCK bl7 WITH FRAME TITLE text-bl7.

  SELECTION-SCREEN: BEGIN OF TABBED BLOCK options FOR 10 LINES,
                    TAB (20) text-b20 USER-COMMAND gene
                                     DEFAULT SCREEN 101,
                    TAB (20) text-b19 USER-COMMAND clif
                                     DEFAULT SCREEN 102,
                    END OF BLOCK options.

  SELECTION-SCREEN END OF BLOCK bl7.

  "General Options
  SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
  PARAMETERS:
    p_transf TYPE cxsltdesc MEMORY ID xsltname.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_pckidx  AS CHECKBOX TYPE abap_bool,
    p_expxml AS CHECKBOX TYPE abap_bool,
    p_notsup AS CHECKBOX TYPE abap_bool.
  SELECTION-SCREEN END OF SCREEN 101.

  "Options for Classes/Interfaces
  SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.
  SELECTION-SCREEN COMMENT /1(30) text-bl0.
  PARAMETERS:
    p_publ AS CHECKBOX TYPE abap_bool DEFAULT 'X',
    p_prot AS CHECKBOX TYPE abap_bool DEFAULT 'X',
    p_priv AS CHECKBOX TYPE abap_bool DEFAULT 'X'.
  SELECTION-SCREEN END OF SCREEN 102. "Classes/Interfaces

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tran-low.
  so_tran-low = main->get_transport_request( ).

  "$. Endregion SELECTION SCREEN

START-OF-SELECTION.
  "Dictionary
  main->r_ddics = so_ddic[].
  main->r_typps = so_ddty[].
  "Complex
  main->r_progs = so_prog[].
  main->r_fugrs = so_fugr[].
  main->r_pckg = so_pckg[].
  main->r_clas = so_clas[].
  "Transports
  main->r_tran = so_tran[].
  "Filters
  main->r_fpckg = so_fpckg[].
  main->r_fresp = so_fresp[].
  "Set configuration from parameters
  config->add_param( i_name = 'SAVE_XML' i_value = p_expxml ).
  config->add_param( i_name = 'SHOW_NOT_SUPPORTED' i_value = p_notsup ).
  config->add_param( i_name = 'XSLT' i_value = p_transf ).
  config->add_param( i_name = 'PACKAGE_INDEX' i_value = p_pckidx ).
  "Object-specific configuration
  config->add_param( i_name = 'CLIF-PUBLIC' i_value = p_publ ).
  config->add_param( i_name = 'CLIF-PROTECTED' i_value = p_prot ).
  config->add_param( i_name = 'CLIF-PRIVATE' i_value = p_priv ).
  main->set_config( config ).
  main->start( ).

END-OF-SELECTION.
