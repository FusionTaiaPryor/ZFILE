CLASS zcl_abap_fileread_csv DEFINITION
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

    DATA t_csvtab TYPE stringtab .

    METHODS parse
      RAISING
        zcx_abap_file .
    METHODS conversion_decimal
      IMPORTING
        !input  TYPE string
      EXPORTING
        !output TYPE any
      RAISING
        zcx_abap_file .
    METHODS conversion_date
      IMPORTING
        !input  TYPE string
      EXPORTING
        !output TYPE d
      RAISING
        zcx_abap_file .
    METHODS conversion_time
      IMPORTING
        !input  TYPE string
      EXPORTING
        !output TYPE t
      RAISING
        zcx_abap_file .
    METHODS conversion_alpha_exit
      IMPORTING
        !input  TYPE string
        !outtyp TYPE REF TO cl_abap_typedescr OPTIONAL
      EXPORTING
        !output TYPE any
      RAISING
        zcx_abap_file .
ENDCLASS.



CLASS ZCL_ABAP_FILEREAD_CSV IMPLEMENTATION.


  METHOD conversion_alpha_exit.

*      IMPORTING input  TYPE string
*                outtyp TYPE REF TO cl_abap_typedescr OPTIONAL
*      EXPORTING output TYPE any
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lo_outtyp TYPE REF TO cl_abap_elemdescr,
      lv_input  TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Recupera instancia do tipo
    lo_outtyp ?= outtyp.
    IF lo_outtyp IS INITIAL.
      lo_outtyp ?= cl_abap_typedescr=>describe_by_data( output ).
    ENDIF.

    "Recupera exit de conversao
    lo_outtyp->get_ddic_field(
      RECEIVING
        p_flddescr   = DATA(ls_fldattr)
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF ls_fldattr-convexit IS NOT INITIAL.
      "Define nome da funcao da rotina de conversao
      DATA(lv_fmname) = |CONVERSION_EXIT_{ condense( ls_fldattr-convexit ) }_INPUT|.

      "Executa rotina de conversao
      CALL FUNCTION lv_fmname
        EXPORTING
          input         = lv_input
        IMPORTING
          output        = lv_input
        EXCEPTIONS
          length_error  = 1
          error_message = 2
          OTHERS        = 3 ##FM_SUBRC_OK.
    ENDIF.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD conversion_date.

*      IMPORTING input  TYPE string
*      EXPORTING output TYPE d
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS '/'.
      TRANSLATE lv_input USING '/.'.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    "Converte data (DD.MM.AAAA -> AAAAMMDD)
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_input
        accept_initial_date      = abap_true
      IMPORTING
        date_internal            = lv_input
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD conversion_decimal.

*      IMPORTING input  TYPE string
*      EXPORTING output TYPE any
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS ','.
      TRANSLATE lv_input USING ',.'.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD conversion_time.

*      IMPORTING input  TYPE string
*      EXPORTING output TYPE t
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS ':'.
      TRANSLATE lv_input USING ': '.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD get_dataxstr.

*      RETURNING VALUE(rv_data) TYPE xstring
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lv_csvline TYPE string.
    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: rv_data, lv_csvline.

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
        "Cria instancia da classe de conversao
        DATA(lo_convout) = cl_abap_conv_out_ce=>create( ignore_cerr = abap_true ).

        "Insere linha de cabecalho
        IF me->s_fileattr-v_hdri = abap_true.
          CLEAR lv_csvline.

          "Monta header
          LOOP AT me->s_itabattr-t_flds[] ASSIGNING FIELD-SYMBOL(<fs_flds>).
            IF lv_csvline IS INITIAL.
              lv_csvline = <fs_flds>-name.
            ELSE.
              lv_csvline = |{ lv_csvline }{ me->s_fileattr-v_sepa }{ <fs_flds>-name }|.
            ENDIF.
          ENDLOOP.

          "Converte para xstring
          lo_convout->write( lv_csvline ).

          "Insere no xstring de retorno
          rv_data = |{ rv_data }{ lo_convout->get_buffer( ) }|.
        ENDIF.

        "Insere dados da tabela no CSV
        LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
          CLEAR lv_csvline.

          DO.
            "Recupera valor do campo
            ASSIGN COMPONENT sy-index OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fv_data>).
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            IF lv_csvline IS INITIAL.
              lv_csvline = <fv_data>.
            ELSE.
              lv_csvline = |{ lv_csvline }{ me->s_fileattr-v_sepa }{ <fv_data> }|.
            ENDIF.
          ENDDO.

          IF lv_csvline IS NOT INITIAL.
            IF rv_data IS NOT INITIAL.
              "Insere quebra de linha
              lv_csvline = |{ cl_abap_char_utilities=>newline }{ lv_csvline }|.
            ENDIF.

            "Converte para xstring
            lo_convout->write( lv_csvline ).

            "Insere no xstring de retorno
            rv_data = |{ rv_data }{ lo_convout->get_buffer( ) }|.
          ENDIF.
        ENDLOOP.

      CATCH cx_parameter_invalid_range INTO DATA(lx_parame_invalid_range).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_parame_invalid_range.
      CATCH cx_sy_codepage_converter_init INTO DATA(lx_codepage_convinit).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_codepage_convinit.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD parse.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lo_dataline TYPE REF TO data.

    FIELD-SYMBOLS:
      <ft_itb> TYPE ANY TABLE,
      <fs_itb> TYPE any.

    CHECK me->t_csvtab[] IS NOT INITIAL.

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

    "Move dados
    LOOP AT me->t_csvtab[] ASSIGNING FIELD-SYMBOL(<fs_csv>).
      IF me->s_fileattr-v_hdri = abap_true.
        CHECK sy-tabix <> 1.
      ENDIF.

      "Recupera valores da linha do CSV
      SPLIT <fs_csv> AT me->s_fileattr-v_sepa INTO TABLE DATA(lt_fieldtab).
      CHECK lt_fieldtab[] IS NOT INITIAL.

      CLEAR <fs_itb>.

      "Move dados da linha do CSV para linha da ITAB
      LOOP AT lt_fieldtab[] ASSIGNING FIELD-SYMBOL(<fv_csv>).
        "Recupera campo da tab interna
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_itb> TO FIELD-SYMBOL(<fv_itb>).
        CHECK sy-subrc IS INITIAL.

        "Converte string para tipo interno
        DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data( <fv_itb> ).
        CASE lo_typedescr->type_kind.
          WHEN 'D'.
            "Data
            me->conversion_date( EXPORTING input  = <fv_csv>
                                 IMPORTING output = <fv_itb> ).
          WHEN 'T'.
            "Hora
            me->conversion_time( EXPORTING input  = <fv_csv>
                                 IMPORTING output = <fv_itb> ).
          WHEN 'P'.
            "Valor decimal
            me->conversion_decimal( EXPORTING input  = <fv_csv>
                                    IMPORTING output = <fv_itb> ).
          WHEN OTHERS.
            "Exit de conversao ALPHA
            me->conversion_alpha_exit( EXPORTING input  = <fv_csv>
                                                 outtyp = lo_typedescr
                                       IMPORTING output = <fv_itb> ).
        ENDCASE.

      ENDLOOP.

      IF <fs_itb> IS NOT INITIAL.
        "Insere linha na ITAB
        INSERT <fs_itb> INTO TABLE <ft_itb>.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD read_locl.

*      RAISING ZCX_ABAP_FILE.

    CLEAR me->t_csvtab[].

    "Importa arquivo
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = me->s_fileattr-v_path
        filetype                = 'ASC'
      CHANGING
        data_tab                = me->t_csvtab[]
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

    "Remove linhas vazias
    DELETE me->t_csvtab[] WHERE table_line IS INITIAL.
    IF me->t_csvtab[] IS NOT INITIAL.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.


  ENDMETHOD.


  METHOD read_serv.


    DATA:
      lv_csvlin TYPE string.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR INPUT IN TEXT MODE
      ENCODING DEFAULT IGNORING CONVERSION ERRORS
      WITH SMART LINEFEED.
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
      CLEAR lv_csvlin.
      READ DATASET me->s_fileattr-v_path INTO lv_csvlin.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      "Insere na tabela
      APPEND lv_csvlin TO me->t_csvtab[].
    ENDDO.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.

    "Remove linhas vazias
    DELETE me->t_csvtab[] WHERE table_line IS INITIAL.
    IF me->t_csvtab[] IS NOT INITIAL.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
