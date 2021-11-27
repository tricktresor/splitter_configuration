REPORT ztrcktrsr_splitter_config_03.

PARAMETERS dummy.

SELECTION-SCREEN PUSHBUTTON /1(10) load  USER-COMMAND zload.
SELECTION-SCREEN PUSHBUTTON /1(10) save  USER-COMMAND zsave.
SELECTION-SCREEN PUSHBUTTON /1(10) reset USER-COMMAND zreset.

CLASS lcl_screen_config DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF _info,
        name   TYPE string,
        object TYPE REF TO object,
        parent TYPE REF TO object,
        rows   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
        cols   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
      END OF _info .
    TYPES:
      _info_list TYPE STANDARD TABLE OF _info WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        start_container TYPE REF TO cl_gui_container.
    METHODS get_current
      RETURNING
        VALUE(configuration) TYPE _info_list.

    METHODS set
      IMPORTING
        !configuration TYPE _info_list.
    METHODS save.
    METHODS load.
    METHODS reset.

  PROTECTED SECTION.
    DATA list TYPE _info_list.   "current config with objects
    DATA start_container TYPE REF TO cl_gui_container.
    METHODS add_container
      IMPORTING
        control TYPE REF TO cl_gui_control.

ENDCLASS.

CLASS lcl_screen_config IMPLEMENTATION.
  METHOD constructor.

    me->start_container = start_container.

  ENDMETHOD.

  METHOD get_current.

    CLEAR list.
    add_container( me->start_container ).
    configuration = list.

  ENDMETHOD.

  METHOD add_container.

    DATA container TYPE REF TO cl_gui_container.
    DATA splitter TYPE REF TO zcl_gui_splitter.
    DATA info TYPE _info.
    DATA size TYPE i.

    CHECK control IS INSTANCE OF cl_gui_container.

    container ?= control.

    LOOP AT container->children INTO DATA(child).
      IF child IS INSTANCE OF zcl_gui_splitter.
        CLEAR info.
        splitter ?= child.
        splitter->get_rows( IMPORTING result = DATA(number_of_rows) ).
        splitter->get_columns( IMPORTING result = DATA(number_of_cols) ).
        DO number_of_rows TIMES.
          splitter->get_row_height(
            EXPORTING
              id                = sy-index
            IMPORTING
              result            = size ).
          cl_gui_cfw=>flush( ).
          APPEND size TO info-rows.
        ENDDO.

        DO number_of_cols TIMES.
          splitter->get_column_width(
            EXPORTING
              id                = sy-index
            IMPORTING
              result            = size ).
          cl_gui_cfw=>flush( ).
          APPEND size TO info-cols.
        ENDDO.

        info-name   = splitter->get_name( ).
        info-object = child.
        info-parent = control.
        APPEND info TO list.
      ENDIF.
      add_container( control = child ).
    ENDLOOP.
  ENDMETHOD.

  METHOD reset.

    DATA settings_xml TYPE xstring.
    DATA id TYPE c LENGTH 22.
    id = 'SPL_' && sy-uname.
    IMPORT settings_xml TO settings_xml FROM DATABASE indx(z2) ID id.
    IF sy-subrc > 0.
      MESSAGE 'configuration did not exist for user' TYPE 'I'.
    ELSE.
      DELETE FROM DATABASE indx(z2) ID id.
      IF sy-subrc = 0.
        MESSAGE 'configuration for user deleted' TYPE 'I'.
      ELSE.
        MESSAGE 'Error deleting user configuration' TYPE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD set.

    LOOP AT configuration INTO DATA(line).

      TRY.
          DATA(splitter) = CAST zcl_gui_splitter( list[ name = line-name ]-object ).
          "set row heights
          LOOP AT line-rows INTO DATA(row_size).
            splitter->set_row_height( id = sy-tabix height = row_size ).
          ENDLOOP.
          "set column widths
          LOOP AT line-cols INTO DATA(col_size).
            splitter->set_column_width( id = sy-tabix width = col_size ).
          ENDLOOP.

        CATCH cx_sy_move_cast_error.
          CONTINUE.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD load.

    "load screen configuration
    DATA settings_xml TYPE xstring.
    DATA configuration TYPE _info_list.
    DATA id TYPE c LENGTH 22.
    id = 'SPL_' && sy-uname.
    IMPORT settings_xml TO settings_xml FROM DATABASE indx(z2) ID id.
    IF sy-subrc > 0.
      MESSAGE 'configuration did not exist for user' TYPE 'I'.
    ELSE.
      CALL TRANSFORMATION id SOURCE XML settings_xml RESULT root = configuration.
      IF configuration IS INITIAL.
        MESSAGE 'no user configuration exists' TYPE 'I'.
      ELSE.
        get_current( ).
        set( configuration ).
        MESSAGE 'screen configuration loaded' TYPE 'I'.
        cl_gui_cfw=>flush( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD save.

    DATA configuration TYPE _info_list.
    DATA splitter TYPE REF TO zcl_gui_splitter.

    get_current( ).

    LOOP AT list INTO DATA(control).
      CHECK control-object IS INSTANCE OF zcl_gui_splitter.
      splitter ?= control-object.
      INSERT VALUE #(
        name = splitter->get_name( )
        rows = control-rows
        cols = control-cols ) INTO TABLE configuration.
    ENDLOOP.

    CHECK configuration IS NOT INITIAL.

    CALL TRANSFORMATION id SOURCE root = configuration RESULT XML DATA(settings_xml).

    DATA id TYPE c LENGTH 22.
    id = |SPL_{ sy-uname }|.
    EXPORT settings_xml FROM settings_xml TO DATABASE indx(z2) ID id.
    IF sy-subrc = 0.
      MESSAGE 'configuration saved' TYPE 'I'.
    ELSE.
      MESSAGE 'Failed to save configuration' TYPE 'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

  DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 80 ).
  DATA(splitter_main)   = NEW zcl_gui_splitter( name = 'Main'   parent = docker columns = 3 rows = 1 ).
  DATA(splitter_left)   = NEW zcl_gui_splitter( name = 'Left'   parent = splitter_main->get_container( row = 1 column = 1 ) rows = 3 columns = 1 ).
  DATA(splitter_middle) = NEW zcl_gui_splitter( name = 'Middle' parent = splitter_main->get_container( row = 1 column = 2 ) rows = 2 columns = 1 ).
  DATA(splitter_right)  = NEW zcl_gui_splitter( name = 'Right'  parent = splitter_main->get_container( row = 1 column = 3 ) rows = 1 columns = 2 ).

  DATA(configuration)   = NEW lcl_screen_config( docker ).

  load = 'Load'.
  save = 'Save'.
  reset = 'Reset'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ZLOAD'.
      configuration->load( ).
    WHEN 'ZSAVE'.
      configuration->save( ).
    WHEN 'ZRESET'.
      configuration->reset( ).
  ENDCASE.

START-OF-SELECTION.
  DATA(config) = configuration->get_current( ).
  LOOP AT config INTO DATA(config_obj).
    WRITE: / 'object', sy-tabix, config_obj-name COLOR COL_POSITIVE.
    WRITE: / ' - columns:', lines( config_obj-cols ), 'sizes:'.
    LOOP AT config_obj-cols INTO DATA(width).
      WRITE width.
    ENDLOOP.
    WRITE: / ' - rows:   ', lines( config_obj-rows ), 'sizes:'.
    LOOP AT config_obj-rows INTO DATA(height).
      WRITE height.
    ENDLOOP.
  ENDLOOP.
