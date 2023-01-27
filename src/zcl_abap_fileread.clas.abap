CLASS zcl_abap_fileread DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_file
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_components_from_ddic
      IMPORTING
        !iv_ddic TYPE tabname
      EXPORTING
        !et_flds TYPE abap_component_tab
        !et_keys TYPE abap_keydescr_tab
      RAISING
        zcx_abap_file .
    CLASS-METHODS get_components_from_itab
      IMPORTING
        !it_itab TYPE ANY TABLE
      EXPORTING
        !et_flds TYPE abap_component_tab
        !et_keys TYPE abap_keydescr_tab
      RAISING
        zcx_abap_file .
    CLASS-METHODS get_components_from_list
      IMPORTING
        !it_list TYPE zif_abap_file=>tt_fldslist
      EXPORTING
        !et_flds TYPE abap_component_tab
        !et_keys TYPE abap_keydescr_tab
      RAISING
        zcx_abap_file .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_fileexte       TYPE zif_abap_file=>ty_extension
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abap_fileread
      RAISING
        zcx_abap_file .
    METHODS create_itab
      IMPORTING
        !it_flds TYPE abap_component_tab
        !it_keys TYPE abap_keydescr_tab OPTIONAL
      RAISING
        zcx_abap_file .
    METHODS read
      RAISING
        zcx_abap_file .
    METHODS get_dataref
      RETURNING
        VALUE(ro_data) TYPE REF TO data
      RAISING
        zcx_abap_file .
    METHODS get_datatab
      EXPORTING
        !et_data TYPE ANY TABLE
      RAISING
        zcx_abap_file .
    METHODS download
      RAISING
        zcx_abap_file .
    METHODS preview
      RAISING
        zcx_abap_file .
    METHODS get_dataxstr
          ABSTRACT
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abap_file .
  PROTECTED SECTION.

    METHODS check_filepath
      RAISING
        zcx_abap_file .
    METHODS read_locl
          ABSTRACT
      RAISING
        zcx_abap_file .
    METHODS read_serv
          ABSTRACT
      RAISING
        zcx_abap_file .
  PRIVATE SECTION.

    METHODS create_data
      RAISING
        zcx_abap_file .
ENDCLASS.



CLASS ZCL_ABAP_FILEREAD IMPLEMENTATION.


  METHOD check_filepath.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lv_exists TYPE c LENGTH 1.

    "Valida caminho do arquivo
    IF me->s_fileattr-v_path IS INITIAL.
      MESSAGE e001(00)
        WITH 'Caminho do arquivo é inválido'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Valida existencia do arquivo
    CASE me->s_fileattr-v_syst.
      WHEN zif_abap_file=>c_system-locl.
        "Busca arq na maquina local
        cl_gui_frontend_services=>file_exist(
          EXPORTING
            file                 = me->s_fileattr-v_path
          RECEIVING
            result               = lv_exists
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

      WHEN zif_abap_file=>c_system-serv.
        "Busca arq no servidor (AL11)
        CALL FUNCTION 'PFL_CHECK_OS_FILE_EXISTENCE'
          EXPORTING
            long_filename         = CONV pfebackuppro( me->s_fileattr-v_path )
          IMPORTING
            file_exists           = lv_exists
          EXCEPTIONS
            authorization_missing = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

    ENDCASE.

    IF lv_exists = abap_false.
      MESSAGE e001(00)
        WITH 'Arquivo não existente'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.


  METHOD create_data.

*      RAISING ZCX_ABAP_FILE.

    DATA:
      lo_tabdescr TYPE REF TO cl_abap_tabledescr.

    TRY.
        "Cria tipo de tabela
        lo_tabdescr = cl_abap_tabledescr=>create(
          p_line_type  = cl_abap_structdescr=>create( me->s_itabattr-t_flds[] )
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false
          p_key        = me->s_itabattr-t_keys[]
          p_key_kind   = cl_abap_tabledescr=>keydefkind_user
        ).

      CATCH cx_sy_table_creation INTO DATA(lx_tabcreate).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_tabcreate.
      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

    "Cria tabela interna
    CREATE DATA me->o_data TYPE HANDLE lo_tabdescr.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar tabela interna'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.


  METHOD create_itab.

*      IMPORTING it_flds TYPE abap_component_tab
*                it_keys TYPE abap_keydescr_tab OPTIONAL
*      RAISING   ZCX_ABAP_FILE.

    "Salva atributos da tab interna
    me->s_itabattr-t_flds[] = it_flds[].
    me->s_itabattr-t_keys[] = it_keys[].

    "Remove linhas vazias
    DELETE me->s_itabattr-t_flds[] WHERE name IS INITIAL.
    DELETE me->s_itabattr-t_keys[] WHERE name IS INITIAL.

    "Remove linhas duplicadas
    DELETE ADJACENT DUPLICATES FROM me->s_itabattr-t_flds[] COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM me->s_itabattr-t_keys[] COMPARING ALL FIELDS.

    "Valida campos
    IF me->s_itabattr-t_flds[] IS INITIAL.
      MESSAGE e001(00)
        WITH 'Estrutura do arquivo não definida'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Valida chaves
    IF me->s_itabattr-t_keys[] IS INITIAL.
      MESSAGE e001(00)
        WITH 'Estrutura de chaves do arquivo não definida'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria tabela interna
    me->create_data( ).


  ENDMETHOD.


  METHOD download.


    DATA:
      lt_solix TYPE solix_tab,
      lv_size  TYPE i.

    CLEAR: lt_solix[], lv_size.

    "Gera xstring ta tabela interna
    DATA(lv_xstring) = me->get_dataxstr( ).
    CHECK lv_xstring IS NOT INITIAL.

    "Converte XSTRING para binario
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_solix[].

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

    "Abre arquivo
    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = me->s_fileattr-v_path
        synchronous            = ' '
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.


  ENDMETHOD.


  METHOD get_components_from_ddic.

*      IMPORTING iv_ddic TYPE tabname
*      EXPORTING et_flds TYPE abap_component_tab
*                et_keys TYPE abap_keydescr_tab
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lo_typedescr   TYPE REF TO cl_abap_typedescr,
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    CLEAR: et_flds[], et_keys[].

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_ddic
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Tipo ' iv_ddic ' desconhecido'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE lo_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        "Realiza cast do objeto
        lo_structdescr ?= lo_typedescr.

        "Recupera chaves
        DATA(lt_ddic) = lo_structdescr->get_ddic_field_list( ).
        DELETE lt_ddic WHERE keyflag = abap_false.
        et_keys[] = CORRESPONDING #( lt_ddic[] MAPPING name = fieldname ).

      WHEN cl_abap_typedescr=>kind_table.
        "Realiza cast do objeto
        lo_tabledescr  ?= lo_typedescr.

        "Recupera tipo da linha
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

        "Recupera chaves
        et_keys[] = lo_tabledescr->key[].

      WHEN OTHERS.
        MESSAGE e001(00)
          WITH 'Tipo ' iv_ddic ' desconhecido'
          INTO lv_dummy.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.

    "Recupera campos
    et_flds[] = lo_structdescr->get_components( ).

    IF et_keys[] IS INITIAL.
      "Recupera chaves
      et_keys[] = CORRESPONDING #( et_flds[] MAPPING name = name ).
    ENDIF.


  ENDMETHOD.


  METHOD get_components_from_itab.

*      IMPORTING it_itab TYPE ANY TABLE
*      EXPORTING et_flds TYPE abap_component_tab
*                et_keys TYPE abap_keydescr_tab
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    CLEAR: et_flds[], et_keys[].

    TRY.
        "Recupera chaves
        lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data( it_itab ).
        et_keys[] = lo_tabledescr->key[].

        "Recupera campos
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
        et_flds[] = lo_structdescr->get_components( ).

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD get_components_from_list.

*      IMPORTING it_list TYPE tt_fldslist
*      EXPORTING et_flds TYPE abap_component_tab
*                et_keys TYPE abap_keydescr_tab
*      RAISING   ZCX_ABAP_FILE.

    DATA:
      lo_typedescr TYPE REF TO cl_abap_typedescr.

    CLEAR: et_flds[], et_keys[].
    CHECK it_list[] IS NOT INITIAL.

    DATA(lt_fieldlist) = it_list[].
    SORT lt_fieldlist BY posn.

    LOOP AT lt_fieldlist[] ASSIGNING FIELD-SYMBOL(<fs_field>).
      "Insere componente
      APPEND INITIAL LINE TO et_flds[] ASSIGNING FIELD-SYMBOL(<fs_component>).
      <fs_component>-name = <fs_field>-name.

      IF <fs_field>-ddic IS NOT INITIAL.
        CASE <fs_field>-ddic.
          WHEN 'STRING'.
            <fs_component>-type  = cl_abap_elemdescr=>get_string( ).
          WHEN 'XSTRING'.
            <fs_component>-type  = cl_abap_elemdescr=>get_xstring( ).
          WHEN OTHERS.
            "Elemento de dados
            cl_abap_elemdescr=>describe_by_name(
              EXPORTING
                p_name         = <fs_field>-ddic
              RECEIVING
                p_descr_ref    = lo_typedescr
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2
            ).
            IF sy-subrc <> 0.
              MESSAGE e001(00)
                WITH 'Tipo ' <fs_field>-ddic ' desconhecido'
                INTO DATA(lv_dummy).
              RAISE EXCEPTION TYPE zcx_abap_file
                EXPORTING
                  symsg = CORRESPONDING #( sy ).
            ENDIF.

            "Seta tipo do campo
            <fs_component>-type ?= lo_typedescr.
        ENDCASE.

      ELSE.
        "Tipos basicos
        CASE <fs_field>-predefined-type.
          WHEN 'C'. <fs_component>-type = cl_abap_elemdescr=>get_c( CONV #( <fs_field>-predefined-leng ) ).
          WHEN 'D'. <fs_component>-type = cl_abap_elemdescr=>get_d( ).
          WHEN 'F'. <fs_component>-type = cl_abap_elemdescr=>get_f( ).
          WHEN 'I'. <fs_component>-type = cl_abap_elemdescr=>get_i( ).
          WHEN 'N'. <fs_component>-type = cl_abap_elemdescr=>get_n( CONV #( <fs_field>-predefined-leng ) ).
          WHEN 'P'.
            <fs_component>-type = cl_abap_elemdescr=>get_p( p_length   = CONV #( <fs_field>-predefined-leng )
                                                            p_decimals = CONV #( <fs_field>-predefined-deci ) ).
          WHEN 'T'. <fs_component>-type = cl_abap_elemdescr=>get_t( ).
          WHEN 'X'. <fs_component>-type = cl_abap_elemdescr=>get_x( CONV #( <fs_field>-predefined-leng ) ).
          WHEN OTHERS.
            MESSAGE e001(00)
              WITH 'Tipo ' <fs_field>-predefined-type ' desconhecido'
              INTO lv_dummy.
            RAISE EXCEPTION TYPE zcx_abap_file
              EXPORTING
                symsg = CORRESPONDING #( sy ).
        ENDCASE.

      ENDIF.

      IF <fs_field>-ispk = abap_true.
        APPEND VALUE #( name = <fs_field>-name ) TO et_keys[].
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_dataref.

*      RETURNING VALUE(ro_data) TYPE REF TO data
*      RAISING   ZCX_ABAP_FILE.

    CLEAR ro_data.
    ro_data = me->o_data.


  ENDMETHOD.


  METHOD get_datatab.

*      EXPORTING et_data TYPE ANY TABLE
*      RAISING   ZCX_ABAP_FILE.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR et_data[].

    TRY.
        "Recupera tabela interna
        ASSIGN me->o_data->* TO <ft_data>.
        IF sy-subrc IS INITIAL.
          et_data[] = <ft_data>.
        ENDIF.

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD get_instance.

*      IMPORTING iv_fileexte        TYPE ty_extension
*      RETURNING VALUE(ro_instance) TYPE REF TO lcl_abap_fileread
*      RAISING   ZCX_ABAP_FILE.

    CLEAR ro_instance.

    "Valida tipo do arquivo
    IF iv_fileexte IS INITIAL.
      MESSAGE e001(00)
        WITH 'Tipo do arquivo é inválido'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE iv_fileexte.
      WHEN zif_abap_file=>c_extension-xls
        OR zif_abap_file=>c_extension-xlsx.
        "Instancia classe de leitura de excel
        ro_instance = NEW zcl_abap_fileread_xls( ).

      WHEN zif_abap_file=>c_extension-csv
        OR zif_abap_file=>c_extension-txt.
        "Instancia classe de leitura de csv
        ro_instance = NEW zcl_abap_fileread_csv( ).

      WHEN OTHERS.
        MESSAGE e001(00)
          WITH 'Tipo ' iv_fileexte ' desconhecido'
          INTO lv_dummy.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
    ENDCASE.

    "Seta extensao
    ro_instance->s_fileattr-v_exte = iv_fileexte.


  ENDMETHOD.


  METHOD preview.


    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

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

        "Habilita funcoes
        lo_salv->get_functions( )->set_all( if_salv_c_bool_sap=>true ).

        "Seta todas colunas como otimizada
        lo_salv->get_columns( )->set_optimize( ).

        "Seta ZEBRA
        lo_salv->get_display_settings( )->set_striped_pattern( if_salv_c_bool_sap=>true ).

        "Seta linha selecionavel
        lo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        "Exibe ALV
        lo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            previous = lx_salv_msg.
    ENDTRY.


  ENDMETHOD.


  METHOD read.

*      RAISING ZCX_ABAP_FILE.

    "Valida arquivo
    me->check_filepath( ).

    CASE me->s_fileattr-v_syst.
      WHEN zif_abap_file=>c_system-locl.
        "Le arquivo local
        me->read_locl( ).

      WHEN zif_abap_file=>c_system-serv.
        "Le arquivo do servidor (AL11)
        me->read_serv( ).

      WHEN OTHERS.
        MESSAGE e001(00)
          WITH 'Origem do arquivo desconhecida'
          INTO DATA(lv_dummy).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.


  ENDMETHOD.
ENDCLASS.
