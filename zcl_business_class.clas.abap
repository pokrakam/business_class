CLASS zcl_business_class DEFINITION
                         PUBLIC
                         ABSTRACT
                         CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES bi_object.
    INTERFACES bi_persistent.

    ALIASES: find_by_lpor FOR bi_persistent~find_by_lpor,
             refresh      FOR bi_persistent~refresh.

    ALIASES: default_attribute_value FOR bi_object~default_attribute_value,
             execute_default_method  FOR bi_object~execute_default_method,
             release                 FOR bi_object~release.


  PROTECTED SECTION.

    DATA lpor TYPE sibflpor.

    METHODS constructor     IMPORTING i_lpor TYPE sibflpor
                            RAISING   cx_bo_instance_not_found .
    METHODS supply_instance RAISING cx_bo_instance_not_found.

  PRIVATE SECTION.

    TYPES: BEGIN OF t_instance_with_key,
             typeid TYPE sibftypeid,
             instid TYPE sibfinstid,
             objref TYPE REF TO zcl_business_class,
           END OF t_instance_with_key .
    TYPES:
      t_instances TYPE STANDARD TABLE OF t_instance_with_key .

    CLASS-DATA instances TYPE t_instances .
ENDCLASS.



CLASS zcl_business_class IMPLEMENTATION.


  METHOD bi_object~default_attribute_value.
    GET REFERENCE OF me->lpor-instid INTO result.
  ENDMETHOD.


  METHOD bi_object~execute_default_method  ##needed.
    "To be redefined
  ENDMETHOD.


  METHOD bi_object~release.
    "Remove from instance table
    DELETE TABLE instances WITH TABLE KEY typeid = lpor-typeid instid = lpor-instid.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD bi_persistent~find_by_lpor.
    "Called to request an object instance

    CHECK lpor-instid IS NOT INITIAL.

    "Buffered instantiation
    DATA(instance) = VALUE #(
        zcl_business_class=>instances[ typeid = lpor-typeid
                                       instid = lpor-instid ]
        OPTIONAL ).

    "If not found, instantiate
    IF instance IS INITIAL.
      instance-typeid = lpor-typeid.
      instance-instid = lpor-instid.
      TRY.
          CREATE OBJECT instance-objref TYPE (lpor-typeid)
            EXPORTING
              i_lpor = lpor.
        CATCH CX_SY_CREATE_OBJECT_ERROR.  "Catch all to also include invalid object types
          RETURN.   "Caller must check existence
      ENDTRY.
      "Add new object to the instance table
      APPEND instance TO zcl_business_class=>instances.
    ENDIF.

    result = instance-objref.

  ENDMETHOD.


  METHOD bi_persistent~lpor.
    "Called to request an instance's key
    result = me->lpor.
  ENDMETHOD.


  METHOD bi_persistent~refresh  ##needed.
    "me->supply_instance( ).
  ENDMETHOD.


  METHOD constructor.

    me->lpor = i_lpor.

    "Subclasses should do instance validation and/or call
    "me->supply_instance(  ).

  ENDMETHOD.


  METHOD supply_instance  ##needed.
    "To be redefined
  ENDMETHOD.

ENDCLASS.
