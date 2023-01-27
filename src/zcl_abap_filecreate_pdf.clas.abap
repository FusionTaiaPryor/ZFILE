CLASS zcl_abap_filecreate_pdf DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_filecreate
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_data
         REDEFINITION .
  PROTECTED SECTION.

    METHODS save_locl
         REDEFINITION .
    METHODS save_serv
         REDEFINITION .
  PRIVATE SECTION.

    DATA t_binary TYPE solix_tab .
ENDCLASS.



CLASS ZCL_ABAP_FILECREATE_PDF IMPLEMENTATION.


  METHOD save_locl.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lv_size TYPE i.

    CHECK me->t_binary[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = me->s_fileattr-v_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = me->t_binary[]
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

    CHECK me->t_binary[] IS NOT INITIAL.

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
    LOOP AT me->t_binary[] ASSIGNING FIELD-SYMBOL(<fs_binary>).
      TRANSFER <fs_binary> TO me->s_fileattr-v_path.
    ENDLOOP.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao salvar o arquivo'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.


  METHOD set_data.

*      IMPORTING it_data TYPE ANY TABLE
*      RAISING   ZCX_ABAP_FILE.

    CLEAR me->t_binary[].
    me->t_binary[] = it_data[].


  ENDMETHOD.
ENDCLASS.
