CLASS zcl_abap_file DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_extension
      IMPORTING
        !iv_path            TYPE zif_abap_file=>ty_filepath
      RETURNING
        VALUE(rv_extension) TYPE zif_abap_file=>ty_extension .
    METHODS set_fileattr
      IMPORTING
        !iv_path TYPE zif_abap_file=>ty_filepath
        !iv_syst TYPE zif_abap_file=>ty_system DEFAULT 'LOCL'
        !iv_hdri TYPE zif_abap_file=>ty_hdrind DEFAULT 'X'
        !iv_sepa TYPE zif_abap_file=>ty_separator DEFAULT ';'
      RAISING
        zcx_abap_file .
  PROTECTED SECTION.

    DATA:
      BEGIN OF s_fileattr,
        v_exte TYPE zif_abap_file=>ty_extension,
        v_path TYPE zif_abap_file=>ty_filepath,
        v_syst TYPE zif_abap_file=>ty_system,
        v_hdri TYPE zif_abap_file=>ty_hdrind,
        v_sepa TYPE zif_abap_file=>ty_separator,
      END OF s_fileattr .
    DATA:
      BEGIN OF s_itabattr,
        t_flds TYPE abap_component_tab,
        t_keys TYPE abap_keydescr_tab,
      END OF s_itabattr .
    DATA o_data TYPE REF TO data .

    METHODS check_fileattr
      RAISING
        zcx_abap_file .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAP_FILE IMPLEMENTATION.


  METHOD check_fileattr.

*      RAISING zcx_abap_file.

    "Valida caminho do arquivo
    IF me->s_fileattr-v_path IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Caminho do arquivo é inválido'.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE me->s_fileattr-v_exte.
      WHEN zif_abap_file=>c_extension-csv OR zif_abap_file=>c_extension-txt.
        IF me->s_fileattr-v_sepa IS INITIAL.
          MESSAGE e001(00) INTO lv_dummy
            WITH 'Separador obrigatório'.
          RAISE EXCEPTION TYPE zcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.


  METHOD get_extension.

*      IMPORTING iv_path        TYPE ty_filepath
*      RETURNING VALUE(rv_extension) TYPE ty_extension.

    DATA:
      lv_path   TYPE string,
      lv_offset TYPE i,
*      lv_length TYPE i,
      lv_size   TYPE i.

    CLEAR rv_extension.
    CHECK iv_path IS NOT INITIAL.

    lv_path = iv_path.

    CLEAR lv_offset.
    FIND ALL OCCURRENCES OF '\' IN lv_path IN CHARACTER MODE MATCH OFFSET lv_offset.
    IF lv_offset <> 0.
      ADD 1 TO lv_offset.
      SHIFT lv_path BY lv_offset PLACES LEFT.
    ENDIF.

    CLEAR lv_offset.
    FIND ALL OCCURRENCES OF '.' IN lv_path IN CHARACTER MODE MATCH OFFSET lv_offset.
    IF lv_offset <> 0.
      ADD 1 TO lv_offset.
      lv_size = strlen( lv_path ) - lv_offset.
      rv_extension = lv_path+lv_offset(lv_size).
    ENDIF.

    rv_extension = to_upper( condense( replace( val = rv_extension sub = '.' with = '' ) ) ).


  ENDMETHOD.


  METHOD set_fileattr.

*      IMPORTING iv_path TYPE lif_abap_file=>ty_filepath
*                iv_syst TYPE lif_abap_file=>ty_system DEFAULT 'LOCL'
*                iv_hdri TYPE lif_abap_file=>ty_hdrind DEFAULT 'X'
*                iv_sepa TYPE lif_abap_file=>ty_separator DEFAULT ';'
*      RAISING   zcx_abap_file.

    "Salva atributos do arquivo
    me->s_fileattr-v_path = iv_path.
    me->s_fileattr-v_syst = iv_syst.
    me->s_fileattr-v_hdri = iv_hdri.
    me->s_fileattr-v_sepa = iv_sepa.

    IF me->s_fileattr-v_exte IS INITIAL.
      me->s_fileattr-v_exte = get_extension( me->s_fileattr-v_path ).
    ENDIF.

    "Seta separador para arq txt
    IF me->s_fileattr-v_exte = 'TXT'.
      me->s_fileattr-v_sepa = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
