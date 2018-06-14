*"* use this source file for your ABAP unit test classes

CLASS ltc_wf_super_test DEFINITION DEFERRED.
CLASS zcl_business_class DEFINITION LOCAL FRIENDS ltc_wf_super_test.

CLASS lcl_implementing_subclass DEFINITION
                                FINAL
                                INHERITING FROM zcl_business_class
                                CREATE PUBLIC
                                FRIENDS ltc_wf_super_test.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_lpor TYPE sibflpor
                        RAISING   cx_bo_instance_not_found .
ENDCLASS.

CLASS lcl_implementing_subclass IMPLEMENTATION.

  METHOD constructor.
    super->constructor( i_lpor = i_lpor ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_wf_super_test DEFINITION
                        FINAL
                        FOR TESTING
                        DURATION SHORT
                        RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_lpor,
                 instid TYPE sibfinstid VALUE 'TEST',
                 typeid TYPE sibftypeid VALUE 'LCL_IMPLEMENTING_SUBCLASS',
                 catid  TYPE sibfcatid VALUE 'CL',
               END OF c_lpor.

    DATA: cut TYPE REF TO lcl_implementing_subclass.

    METHODS setup RAISING cx_static_check.
    METHODS teardown.

    METHODS get_default_attribute_value FOR TESTING.
    METHODS find_by_lpor FOR TESTING.
    METHODS lpor FOR TESTING.
    METHODS dummy FOR TESTING RAISING cx_static_check.
    METHODS release FOR TESTING RAISING cx_static_check.
    METHODS invalid_objtype_return_empty FOR TESTING RAISING cx_static_check.

ENDCLASS.                    "ltc_wf_super_test DEFINITION

CLASS ltc_wf_super_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT cut
      EXPORTING
        i_lpor = c_lpor.

  ENDMETHOD.                    "setup

  METHOD teardown.
    CLEAR cut.
    CLEAR zcl_business_class=>instances.
  ENDMETHOD.                    "teardown


  METHOD get_default_attribute_value.

    DATA(act) = cut->bi_object~default_attribute_value(  ).
    ASSIGN act->* TO FIELD-SYMBOL(<val>).
    cl_abap_unit_assert=>assert_equals( act = <val>
                                        exp = 'TEST' ).

  ENDMETHOD.


  METHOD find_by_lpor.
    DATA: inst1 TYPE REF TO zcl_business_class,
          inst2 TYPE REF TO zcl_business_class,
          inst3 TYPE REF TO zcl_business_class,
          lpor  TYPE sibflpor.

    lpor = c_lpor.

    "Test 1: Should not reference the un-managed instance of this test
    inst1 ?= lcl_implementing_subclass=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst1 ).
    "Should reference different instance, as first was created without instance management
    "(assert_differs cannot use reference variables)
    IF cut = inst1.
      cl_abap_unit_assert=>fail( msg = 'Should not be same instance' ).
    ENDIF.

    "Test 2: A second class with same key should have same instance
    lpor-instid = 'TEST'.
    inst2 ?= zcl_business_class=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst2 ).
    "Note, not using ASSERT_EQUALS to keep consistent with previous test
    "Should reference same instance
    IF inst1 <> inst2.
      cl_abap_unit_assert=>fail( msg = 'Duplicate instance with same key' ).
    ENDIF.

    "Test 3: A third class with same key should have a different instance
    lpor-instid = 'TEST2'.
    inst3 ?= zcl_business_class=>bi_persistent~find_by_lpor( lpor ).
    cl_abap_unit_assert=>assert_bound( inst3 ).
    "Should reference new instance
    IF inst2 = inst3.
      cl_abap_unit_assert=>fail( msg = 'Different key same instance' ).
    ENDIF.

  ENDMETHOD.


  METHOD lpor.
    DATA: lpor TYPE sibflpor.
    lpor = cut->bi_persistent~lpor( ).
    cl_abap_unit_assert=>assert_equals( act = lpor
                                        exp = c_lpor
                                        msg = 'LPOR incorrect' ).
  ENDMETHOD.


  METHOD release.
    cl_abap_unit_assert=>assert_equals( act = lines( zcl_business_class=>instances )
                                        exp = 0 ).
    DATA(instance) = lcl_implementing_subclass=>bi_persistent~find_by_lpor( c_lpor ).
    cl_abap_unit_assert=>assert_equals( act = lines( zcl_business_class=>instances )
                                        exp = 1 ).
    instance->bi_object~release( ).
    cl_abap_unit_assert=>assert_equals( act = lines( zcl_business_class=>instances )
                                        exp = 0 ).
  ENDMETHOD.


  METHOD dummy.
    "For coverage / sanity check - run through empty method
    cut->execute_default_method( ).
    cut->supply_instance( ).
    cut->refresh( ).
  ENDMETHOD.


  METHOD invalid_objtype_return_empty.
    DATA(test) = zcl_business_class=>find_by_lpor(
                    VALUE #( catid  = 'CL'
                             typeid = 'BLAH'
                             instid = '123' ) ).
    cl_abap_unit_assert=>assert_not_bound( test ).
  ENDMETHOD.


ENDCLASS.                    "ltc_wf_super_test IMPLEMENTATION
