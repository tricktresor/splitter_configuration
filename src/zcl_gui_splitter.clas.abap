class ZCL_GUI_SPLITTER definition
  public
  inheriting from CL_GUI_SPLITTER_CONTAINER
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(LINK_DYNNR) type SY-DYNNR optional
      value(LINK_REPID) type SY-REPID optional
      value(SHELLSTYLE) type I optional
      value(LEFT) type I optional
      value(TOP) type I optional
      value(WIDTH) type I optional
      value(HEIGHT) type I optional
      value(METRIC) type CNTL_METRIC default cntl_metric_dynpro
      value(ALIGN) type I default 15
      value(PARENT) type ref to CL_GUI_CONTAINER optional
      value(ROWS) type I optional
      value(COLUMNS) type I optional
      value(NO_AUTODEF_PROGID_DYNNR) type C optional
      value(NAME) type STRING optional
    exceptions
      CNTL_ERROR
      CNTL_SYSTEM_ERROR .

  methods GET_NAME
    redefinition .
protected section.
private section.

  data MY_NAME type STRING .
ENDCLASS.



CLASS ZCL_GUI_SPLITTER IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      EXPORTING
        link_dynnr              = link_dynnr
        link_repid              = link_repid
        shellstyle              = shellstyle
        left                    = left
        top                     = top
        width                   = width
        height                  = height
        metric                  = cntl_metric_dynpro
        align                   = 15
        parent                  = parent
        rows                    = rows
        columns                 = columns
        no_autodef_progid_dynnr = no_autodef_progid_dynnr
        name                    = name
      EXCEPTIONS
        cntl_error              = 1
        cntl_system_error       = 2
        OTHERS                  = 3  ).
    my_name = name.

  ENDMETHOD.


  METHOD get_name.

    name = my_name.

  ENDMETHOD.
ENDCLASS.
