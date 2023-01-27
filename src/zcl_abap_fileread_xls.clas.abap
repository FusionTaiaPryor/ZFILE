CLASS zcl_abap_fileread_xls DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_fileread
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_dataxstr
         REDEFINITION .
  PROTECTED SECTION.

    METHODS read_locl
         REDEFINITION .
    METHODS read_serv
         REDEFINITION .
  PRIVATE SECTION.

    DATA v_xstring TYPE xstring .

    METHODS parse
      RAISING
        zcx_abap_file .
ENDCLASS.



CLASS ZCL_ABAP_FILEREAD_XLS IMPLEMENTATION.


  METHOD get_dataxstr.

*      RETURNING VALUE(rv_data) TYPE xstring
*      RAISING   ZCX_ABAP_FILE.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR rv_data.

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao recuperar tabela interna'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <ft_data>
        ).

        "Recupera catalogo de campos
        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lo_salv->get_columns( )
          r_aggregations = lo_salv->get_aggregations( )
        ).

        "Gera XSTRING
        cl_salv_bs_lex=>export_from_result_data_table(
          EXPORTING
            is_format            = if_salv_bs_lex_format=>mc_format_xlsx
            ir_result_data_table = cl_salv_ex_util=>factory_result_data_table( r_data         = me->o_data
                                                                               t_fieldcatalog = lt_fcat[] )
          IMPORTING
            er_result_file       = rv_data
        ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_salv_msg.

      CATCH cx_salv_unexpected_param_value INTO DATA(lx_unexpected_param_value).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_unexpected_param_value.
    ENDTRY.


  ENDMETHOD.


  METHOD parse.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lo_dataline TYPE REF TO data.

    FIELD-SYMBOLS:
      <ft_xls> TYPE ANY TABLE,
      <ft_itb> TYPE ANY TABLE,
      <fs_itb> TYPE any.

    CHECK me->v_xstring IS NOT INITIAL.

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_itb>.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao recuperar tabela interna'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria linha do tipo da tabela interna
    CREATE DATA lo_dataline LIKE LINE OF <ft_itb>.
    IF sy-subrc IS INITIAL.
      ASSIGN lo_dataline->* TO <fs_itb>.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar linha da tabela interna'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY .
        "Instancia classe de tratamento de excel
        DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
          document_name = 'Excel_Document'
          xdocument     = me->v_xstring
        ).

        "Le sheets
        lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
          IMPORTING
            worksheet_names = DATA(lt_worksheets)
        ).

        "Le dados da sheet
        IF lt_worksheets[] IS NOT INITIAL.
          DATA(lo_filelist) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheets[ 1 ] ).
          ASSIGN lo_filelist->* TO <ft_xls>.
        ENDIF.

        IF <ft_xls> IS ASSIGNED.
          "Move dados
          LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<fs_xls>).
            IF me->s_fileattr-v_hdri = abap_true.
              CHECK sy-tabix <> 1.
            ENDIF.

            CLEAR <fs_itb>.

            DO.
              DATA(lv_index) = sy-index.
              ASSIGN COMPONENT lv_index OF STRUCTURE <fs_xls> TO FIELD-SYMBOL(<fv_xls>).
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              ASSIGN COMPONENT lv_index OF STRUCTURE <fs_itb> TO FIELD-SYMBOL(<fv_itb>).
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              <fv_itb> = <fv_xls>.
            ENDDO.

            IF <fs_itb> IS NOT INITIAL.
              INSERT <fs_itb> INTO TABLE <ft_itb>.
            ENDIF.
          ENDLOOP.
        ENDIF.

      CATCH cx_fdt_excel_core INTO DATA(lx_excel_core).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_excel_core.
      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD read_locl.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lt_solix  TYPE solix_tab,
      lv_length TYPE i.

    CLEAR: me->v_xstring.

    "Importa arquivo em binario
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = me->s_fileattr-v_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
        header                  = me->v_xstring
      CHANGING
        data_tab                = lt_solix[]
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    IF lt_solix[] IS NOT INITIAL.

      "Transforma binario em xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_length
        IMPORTING
          buffer       = me->v_xstring
        TABLES
          binary_tab   = lt_solix[]
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
      ENDIF.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.


  ENDMETHOD.


  METHOD read_serv.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lt_solix  TYPE solix_tab,
      ls_solix  TYPE solix,
      lv_totlen TYPE i,
      lv_length TYPE i.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao abrir o arquivo'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Le arquivo
    DO.
      READ DATASET me->s_fileattr-v_path INTO ls_solix
        MAXIMUM LENGTH 255 LENGTH lv_length.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      "Insere na tabela
      APPEND ls_solix TO lt_solix[].

      "Calcula tamanho total
      lv_totlen = lv_totlen + lv_length.
    ENDDO.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.

    IF lt_solix[] IS NOT INITIAL.

      "Transforma binario em xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_totlen
        IMPORTING
          buffer       = me->v_xstring
        TABLES
          binary_tab   = lt_solix[]
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
      ENDIF.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
