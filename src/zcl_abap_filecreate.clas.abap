CLASS zcl_abap_filecreate DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_file
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_fileexte       TYPE zif_abap_file=>ty_extension
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abap_filecreate
      RAISING
        zcx_abap_file .
    METHODS set_data
      IMPORTING
        !it_data TYPE ANY TABLE
      RAISING
        zcx_abap_file .
    METHODS save
      RAISING
        zcx_abap_file .
  PROTECTED SECTION.

    METHODS save_locl
          ABSTRACT
      RAISING
        zcx_abap_file .
    METHODS save_serv
          ABSTRACT
      RAISING
        zcx_abap_file .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAP_FILECREATE IMPLEMENTATION.


  METHOD get_instance.

*      IMPORTING iv_fileexte        TYPE ty_extension
*      RETURNING VALUE(ro_instance) TYPE REF TO lcl_abap_filecreate
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
      WHEN zif_abap_file=>c_extension-csv
        OR zif_abap_file=>c_extension-txt.
        "Instancia classe para salvar csv
        ro_instance = NEW zcl_abap_filecreate_csv( ).

      WHEN zif_abap_file=>c_extension-xls
        OR zif_abap_file=>c_extension-xlsx.
        "Instancia classe para salvar excel
        ro_instance = NEW zcl_abap_filecreate_xls( ).

      WHEN zif_abap_file=>c_extension-pdf.
        "Instancia classe para salvar pdf
        ro_instance = NEW zcl_abap_filecreate_pdf( ).

      WHEN OTHERS.
        MESSAGE e001(00)
          WITH 'Tipo ' iv_fileexte ' desconhecido'
          INTO lv_dummy.
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
    ENDCASE.

    ro_instance->s_fileattr-v_exte = iv_fileexte.


  ENDMETHOD.


  METHOD save.

*      RAISING ZCX_ABAP_FILE.

    CASE me->s_fileattr-v_syst.
      WHEN zif_abap_file=>c_system-locl.
        "Salva arquivo local
        me->save_locl( ).

      WHEN zif_abap_file=>c_system-serv.
        "Salva arquivo no servidor (AL11)
        me->save_serv( ).

      WHEN OTHERS.
        MESSAGE e001(00)
          WITH 'Destino do arquivo desconhecido'
          INTO DATA(lv_dummy).
        RAISE EXCEPTION TYPE zcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.


  ENDMETHOD.


  METHOD set_data.

*      IMPORTING it_data TYPE ANY TABLE
*      RAISING   ZCX_ABAP_FILE.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    "Cria tabela interna
    CREATE DATA me->o_data LIKE it_data[].
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar tabela interna'
        INTO DATA(lv_dummy).
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Recupera tabela
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00)
        WITH 'Erro ao criar tabela interna'
        INTO lv_dummy.
      RAISE EXCEPTION TYPE zcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Salva informacoes
    <ft_data> = it_data[].


  ENDMETHOD.
ENDCLASS.
