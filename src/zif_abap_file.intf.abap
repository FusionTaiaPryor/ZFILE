interface ZIF_ABAP_FILE
  public .


  types:
    ty_extension TYPE c LENGTH 4 .
  types TY_FILEPATH type STRING .
  types:
    ty_system    TYPE c LENGTH 4 .
  types TY_HDRIND type ABAP_BOOL .
  types:
    ty_separator TYPE c LENGTH 1 .
  types:
    BEGIN OF ty_fldslist,
      name TYPE name_feld,
      posn TYPE i,
      ispk TYPE keyflag,
      ddic TYPE rollname,
      BEGIN OF predefined,
        type TYPE name_feld,
        leng TYPE ddleng,
        deci TYPE decimals,
      END OF predefined,
    END OF ty_fldslist .
  types:
    tt_fldslist TYPE STANDARD TABLE OF ty_fldslist WITH KEY name .

  constants:
    BEGIN OF c_extension,
      xlsx TYPE ty_extension VALUE 'XLSX',
      xls  TYPE ty_extension VALUE 'XLS',
      csv  TYPE ty_extension VALUE 'CSV',
      txt  TYPE ty_extension VALUE 'TXT',
      pdf  TYPE ty_extension VALUE 'PDF',
    END OF c_extension .
  constants:
    BEGIN OF c_system,
      locl TYPE ty_system VALUE 'LOCL',
      serv TYPE ty_system VALUE 'SERV',
    END OF c_system .
endinterface.
