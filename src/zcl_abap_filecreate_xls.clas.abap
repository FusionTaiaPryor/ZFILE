CLASS zcl_abap_filecreate_xls DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_filecreate
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS save_locl
         REDEFINITION .
    METHODS save_serv
         REDEFINITION .
  PRIVATE SECTION.

    METHODS get_binary
      EXPORTING
        !et_bintab TYPE solix_tab
        !ev_size   TYPE i
      RAISING
        zcx_abap_file .
ENDCLASS.



CLASS ZCL_ABAP_FILECREATE_XLS IMPLEMENTATION.


  METHOD get_binary.

*      EXPORTING et_bintab TYPE solix_tab
*                ev_size   TYPE i
*      RAISING   ZCX_ABAP_FILE.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: et_bintab[], ev_size.

    "Recupera tabela
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar tabela interna'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY .
        "Cria instancia da salv
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <ft_data>
        ).

        "Recupera XSTRING do excel
        DATA(lv_xstring) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx  ).

        "Converte para binario
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = ev_size
          TABLES
            binary_tab    = et_bintab[]
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

      CATCH cx_salv_msg INTO DATA(lo_except).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lo_except.
    ENDTRY.


  ENDMETHOD.


  METHOD save_locl.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lt_solix TYPE solix_tab,
      lv_size  TYPE i.

    "Recupera binario
    me->get_binary( IMPORTING et_bintab = lt_solix[]
                              ev_size   = lv_size ).
    CHECK lt_solix[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = me->s_fileattr-v_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_solix[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.


  METHOD save_serv.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lt_solix TYPE solix_tab,
      lv_size  TYPE i.

    "Recupera binario
    me->get_binary( IMPORTING et_bintab = lt_solix[]
                              ev_size   = lv_size ).
    CHECK lt_solix[] IS NOT INITIAL.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar o arquivo'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Insere dados no arquivo
    LOOP AT lt_solix[] ASSIGNING FIELD-SYMBOL(<fs_solix>).
      TRANSFER <fs_solix> TO me->s_fileattr-v_path.
    ENDLOOP.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao fechar o arquivo'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
