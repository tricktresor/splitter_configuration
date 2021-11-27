REPORT ztrcktrsr_splitter_config_01.

PARAMETERS dummy.

CLASS lcl_screen_config DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF _info,
        object TYPE REF TO object,
        parent TYPE REF TO object,
        rows   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
        cols   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
      END OF _info .
    TYPES:
      _info_list TYPE STANDARD TABLE OF _info WITH DEFAULT KEY.

    METHODS get_configuration
      IMPORTING
        start_container      TYPE REF TO cl_gui_container
      RETURNING
        VALUE(configuration) TYPE _info_list.

  PROTECTED SECTION.
    DATA list TYPE _info_list.
    METHODS add_container
      IMPORTING
        control TYPE REF TO cl_gui_control.

ENDCLASS.

CLASS lcl_screen_config IMPLEMENTATION.
  METHOD get_configuration.

    CLEAR list.

    LOOP AT start_container->children INTO DATA(child).
      add_container( control = child ).
    ENDLOOP.

    configuration = list.

  ENDMETHOD.

  METHOD add_container.

    DATA container TYPE REF TO cl_gui_container.
    DATA splitter TYPE REF TO cl_gui_splitter_container.
    DATA info TYPE _info.
    DATA size TYPE i.

    CHECK control IS INSTANCE OF cl_gui_container.

    container ?= control.

    LOOP AT container->children INTO DATA(child).
      IF child IS INSTANCE OF cl_gui_splitter_container.
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

        info-object = child.
        info-parent = control.
        APPEND info TO list.
      ENDIF.
      add_container( control = child ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

  DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 80 ).
  DATA(splitter_main)   = NEW cl_gui_splitter_container( parent = docker columns = 3 rows = 1 ).
  DATA(splitter_left)   = NEW cl_gui_splitter_container( parent = splitter_main->get_container( row = 1 column = 1 ) rows = 3 columns = 1 ).
  DATA(splitter_middle) = NEW cl_gui_splitter_container( parent = splitter_main->get_container( row = 1 column = 2 ) rows = 2 columns = 1 ).
  DATA(splitter_right)  = NEW cl_gui_splitter_container( parent = splitter_main->get_container( row = 1 column = 3 ) rows = 1 columns = 2 ).

  DATA(configuration)   = NEW lcl_screen_config( ).

START-OF-SELECTION.
  DATA(config) = configuration->get_configuration( docker ).
  LOOP AT config INTO DATA(config_obj).
    WRITE: / 'object', sy-tabix.
    WRITE: / ' - columns:', lines( config_obj-cols ), 'sizes:'.
    LOOP AT config_obj-cols INTO DATA(width).
      WRITE width.
    ENDLOOP.
    WRITE: / ' - rows:   ', lines( config_obj-rows ), 'sizes:'.
    LOOP AT config_obj-rows INTO DATA(height).
      WRITE height.
    ENDLOOP.
  ENDLOOP.
