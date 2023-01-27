class ZCX_ABAP_FILE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !PREVIOUS like PREVIOUS optional
      !TEXTID like TEXTID optional
      !SYMSG type SYMSG optional
      !MSGTXT type STRING optional .
  methods GET_SYMSG
    returning
      value(SYMSG) type SYMSG .

  methods GET_TEXT
    redefinition .
protected section.
private section.

  data SYMSG type SYMSG .
  data TXMSG type STRING .
ENDCLASS.



CLASS ZCX_ABAP_FILE IMPLEMENTATION.


  method CONSTRUCTOR.

    super->constructor( previous = previous textid = textid ).
    me->symsg = symsg.
    me->txmsg = msgtxt.

  endmethod.


  method GET_SYMSG.

    CLEAR symsg.
    symsg = me->symsg.

  endmethod.


  method GET_TEXT.

    CLEAR result.
    result = me->txmsg.
    CHECK result IS INITIAL.
    IF me->symsg-msgty IS NOT INITIAL.
      MESSAGE ID me->symsg-msgid TYPE me->symsg-msgty NUMBER me->symsg-msgno
        WITH me->symsg-msgv1 me->symsg-msgv2 me->symsg-msgv3 me->symsg-msgv4
        INTO result.
    ENDIF.
    CHECK result IS INITIAL.
    IF me->previous IS BOUND.
      result = me->previous->get_text( ).
    ENDIF.

  endmethod.
ENDCLASS.
