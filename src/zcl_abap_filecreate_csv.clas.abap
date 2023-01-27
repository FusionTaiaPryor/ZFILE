CLASS zcl_abap_filecreate_csv DEFINITION
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

    METHODS get_txttab
      RETURNING
        VALUE(rt_txttab) TYPE stringtab
      RAISING
        zcx_abap_file .
    METHODS get_header
      RETURNING
        VALUE(rv_header) TYPE string
      RAISING
        zcx_abap_file .
ENDCLASS.



CLASS ZCL_ABAP_FILECREATE_CSV IMPLEMENTATION.


  METHOD get_header.

*      RETURNING VALUE(rv_header) TYPE string
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: rv_header.

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

    TRY.
        "Recupera campos
        lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data( <ft_data> ).
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
        DATA(lt_fields) = lo_structdescr->get_ddic_field_list( ).

        "Monta linha de cabecalho
        LOOP AT lt_fields[] ASSIGNING FIELD-SYMBOL(<fs_field>).
          IF rv_header IS INITIAL.
            rv_header = <fs_field>-scrtext_l.
          ELSE.
            rv_header = |{ rv_header }{ me->s_fileattr-v_sepa }{ <fs_field>-scrtext_l }|.
          ENDIF.
        ENDLOOP.

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD get_txttab.

*      RETURNING VALUE(rt_txttab) TYPE stringtab
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lv_txt TYPE string.
    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: rt_txttab[].

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

    IF me->s_fileattr-v_hdri = abap_true.
      "Insere cabecalho
      APPEND me->get_header( ) TO rt_txttab[].
    ENDIF.

    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
      CLEAR lv_txt.

      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fv_data>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF lv_txt IS INITIAL.
          lv_txt = <fv_data>.
        ELSE.
          lv_txt = |{ lv_txt }{ me->s_fileattr-v_sepa }{ <fv_data> }|.
        ENDIF.
      ENDDO.
      CHECK lv_txt IS NOT INITIAL.

      APPEND lv_txt TO rt_txttab[].
    ENDLOOP.


  ENDMETHOD.


  METHOD save_locl.

*      RAISING ZCX_ABAP_FILE.

    "Recupera tabela de string
    DATA(lt_txttab) = me->get_txttab( ).
    CHECK lt_txttab[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = me->s_fileattr-v_path
        filetype                  = 'ASC'
      CHANGING
        data_tab                  = lt_txttab[]
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

    "Recupera tabela de string
    DATA(lt_txttab) = me->get_txttab( ).
    CHECK lt_txttab[] IS NOT INITIAL.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR OUTPUT IN TEXT MODE
      ENCODING DEFAULT IGNORING CONVERSION ERRORS
      WITH SMART LINEFEED.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar o arquivo'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Insere dados no arquivo
    LOOP AT lt_txttab[] ASSIGNING FIELD-SYMBOL(<fs_txt>).
      TRANSFER <fs_txt> TO me->s_fileattr-v_path.
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
ENDCLASS.
