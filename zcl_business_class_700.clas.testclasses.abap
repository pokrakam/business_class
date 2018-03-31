*"* use this source file for your ABAP unit test classes

CLASS ltc_wf_super_test DEFINITION DEFERRED.
CLASS zcl_business_class_700 DEFINITION LOCAL FRIENDS ltc_wf_super_test.

*----------------------------------------------------------------------*
*       CLASS lcl_wf_super DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_wf_super DEFINITION FINAL INHERITING FROM zcl_business_class_700 FRIENDS ltc_wf_super_test.
  PUBLIC SECTION.
ENDCLASS.                    "lcl_wf_super DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_wf_super_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_wf_super_test DEFINITION FINAL
                        FOR TESTING
                        DURATION SHORT
                        RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_lpor,
                 instid TYPE sibfinstid VALUE 'TEST',
                 typeid TYPE sibftypeid VALUE 'LCL_WF_SUPER',
                 catid  TYPE sibfcatid VALUE 'CL',
               END OF c_lpor.

    DATA: f_cut TYPE REF TO lcl_wf_super.

    METHODS: setup.
    METHODS: teardown.

    METHODS: get_default_attribute_value FOR TESTING.
    METHODS: find_by_lpor FOR TESTING.
    METHODS: lpor FOR TESTING.

ENDCLASS.                    "ltc_wf_super_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_wf_super_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_wf_super_test IMPLEMENTATION.

  METHOD setup.
    TRY.
        CREATE OBJECT f_cut
          EXPORTING
            i_lpor = c_lpor.
      CATCH cx_bo_instance_not_found.
        cl_abap_unit_assert=>fail( msg = 'Could not instantiate test object' ).
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.
    CLEAR f_cut.
  ENDMETHOD.                    "teardown

  METHOD get_default_attribute_value.
    DATA: act TYPE REF TO data.

    FIELD-SYMBOLS <val> TYPE clike.

    act = f_cut->bi_object~default_attribute_value(  ).
    ASSIGN act->* TO <val>.
    cl_abap_unit_assert=>assert_equals( msg = 'Default attribute wrong' exp = 'TEST' act = <val> ).
  ENDMETHOD.                    "get_default_attribute_value


  METHOD find_by_lpor.
    DATA: inst1 TYPE REF TO zcl_business_class_700,
          inst2 TYPE REF TO zcl_business_class_700,
          inst3 TYPE REF TO zcl_business_class_700,
          lpor  TYPE sibflpor.

    lpor = c_lpor.

    "Test 1: Should not reference the un-managed instance of this test
    inst1 ?= lcl_wf_super=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst1 ).
    "Should reference different instance, as first was created without instance management
    "(assert_differs cannot use reference variables)
    IF f_cut = inst1.
      cl_abap_unit_assert=>fail( msg = 'Should not be same instance' ).
    ENDIF.

    "Test 2: A second class with same key should have same instance
    lpor-instid = 'TEST'.
    inst2 ?= zcl_business_class_700=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst2 ).
    "Note, not using ASSERT_EQUALS to keep consistent with previous test
    "Should reference same instance
    IF inst1 <> inst2.
      cl_abap_unit_assert=>fail( msg = 'Duplicate instance with same key' ).
    ENDIF.

    "Test 3: A third class with same key should have a different instance
    lpor-instid = 'TEST2'.
    inst3 ?= zcl_business_class_700=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst3 ).
    "Should reference new instance
    IF inst2 = inst3.
      cl_abap_unit_assert=>fail( msg = 'Different key same instance' ).
    ENDIF.

  ENDMETHOD.                    "find_by_lpor


  METHOD lpor.
    DATA: lpor TYPE sibflpor.
    lpor = f_cut->bi_persistent~lpor( ).
    cl_abap_unit_assert=>assert_equals( msg = 'LPOR incorrect' exp = c_lpor act = lpor ).
  ENDMETHOD.                    "lpor

ENDCLASS.                    "ltc_wf_super_test IMPLEMENTATION
