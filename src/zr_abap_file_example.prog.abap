*&---------------------------------------------------------------------*
*& Report  ZR_ABAP_FILE_EXAMPLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zr_abap_file_example.

"Parametros do arquivo
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_path TYPE zif_abap_file=>ty_filepath LOWER CASE,
  p_syst TYPE zif_abap_file=>ty_system DEFAULT zif_abap_file=>c_system-serv,
  p_hdri TYPE zif_abap_file=>ty_hdrind DEFAULT 'X',
  p_sepa TYPE zif_abap_file=>ty_separator DEFAULT ';'.
SELECTION-SCREEN END OF BLOCK b1.

"Leitura
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:
  p_read RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_prev AS CHECKBOX DEFAULT 'X',
  p_down AS CHECKBOX.
SELECTION-SCREEN SKIP.

"Criacao
PARAMETERS:
  p_save  RADIOBUTTON GROUP g1,
  p_opath TYPE zif_abap_file=>ty_filepath LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  TRY .
      CASE abap_true.
        WHEN p_read. PERFORM f_read.
        WHEN p_save. PERFORM f_save.
      ENDCASE.
    CATCH zcx_abap_file INTO DATA(lx_exception).
      MESSAGE lx_exception->get_text( ) TYPE 'E'.
  ENDTRY.

FORM f_read RAISING zcx_abap_file.

  DATA:
    lo_fileread TYPE REF TO zcl_abap_fileread,
    lt_tbfields TYPE abap_component_tab,
    lt_tbkeys   TYPE abap_keydescr_tab,
    lt_bsegkey  TYPE STANDARD TABLE OF bsegkey.

  "Recupera instancia da classe correspondente
  lo_fileread = zcl_abap_fileread=>get_instance( zcl_abap_file=>get_extension( p_path ) ).


  "Busca estrutura da tab interna a partir da tabela interna
  zcl_abap_fileread=>get_components_from_itab( EXPORTING it_itab = lt_bsegkey[]
                                          IMPORTING et_flds = lt_tbfields[]
                                                    et_keys = lt_tbkeys[] ).

  "Seta atributos da tabela interna
  lo_fileread->create_itab( it_flds = lt_tbfields[]   "campos
                            it_keys = lt_tbkeys[] ).  "chaves

  "Seta atributos do arquivo
  lo_fileread->set_fileattr( iv_path = p_path     "caminho do arquivo
                             iv_syst = p_syst     "origem do arquivo (SERV / LOCL)
                             iv_hdri = p_hdri     "indicador de cabecalho
                             iv_sepa = p_sepa ).  "separador (para CSV)

  "Le arquivo
  lo_fileread->read( ).

*  "Recupera dados
*  lo_fileread->get_datatab( IMPORTING et_data = lt_bsegkey[] ).
*  DATA(lo_data) = lo_fileread->get_dataref( ).

  CASE abap_true.
    WHEN p_prev.
      "Visualiza arquivo
      lo_fileread->preview( ).
    WHEN p_down.
      "Realiza download
      lo_fileread->download( ).
  ENDCASE.

ENDFORM.

FORM f_save RAISING zcx_abap_file.

  DATA:
    lt_bintab TYPE solix_tab,
    lv_size   TYPE i.

  "importa PDF para teste
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = p_opath
      filetype                = 'BIN'
    CHANGING
      data_tab                =  lt_bintab[]
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
      OTHERS                  = 19
  ).

  DATA(lo_filecreate) = zcl_abap_filecreate=>get_instance( zcl_abap_file=>get_extension( p_opath ) ).

  lo_filecreate->set_fileattr( iv_path = p_path
                               iv_syst = p_syst
                               iv_hdri = p_hdri
                               iv_sepa = p_sepa ).

  lo_filecreate->set_data( lt_bintab[] ).

  lo_filecreate->save( ).

ENDFORM.
