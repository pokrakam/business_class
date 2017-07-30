CLASS zcl_business_class DEFINITION
                         PUBLIC
                         ABSTRACT
                         CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES bi_object.
    INTERFACES bi_persistent.

    ALIASES refresh FOR bi_persistent~refresh.
    ALIASES release FOR bi_object~release.

    METHODS constructor      IMPORTING i_lpor TYPE sibflpor
                             RAISING   cx_bo_instance_not_found .

  PROTECTED SECTION.

    DATA lpor TYPE sibflpor.

    METHODS supply_instance RAISING cx_bo_instance_not_found.

  PRIVATE SECTION.

    TYPES: BEGIN OF t_instance,
             typeid   TYPE sibftypeid,
             instid   TYPE sibfinstid,
             instance TYPE REF TO zcl_business_class,
           END OF t_instance .
    TYPES:
      t_instances TYPE STANDARD TABLE OF t_instance .

    CLASS-DATA instances TYPE t_instances .
ENDCLASS.



CLASS zcl_business_class IMPLEMENTATION.


  METHOD bi_object~default_attribute_value.
    GET REFERENCE OF me->lpor-instid INTO result.
  ENDMETHOD.


  METHOD bi_object~execute_default_method  ##needed.
  ENDMETHOD.


  METHOD bi_object~release.
    "If we have an instance
    READ TABLE instances INTO DATA(instance)
         WITH TABLE KEY typeid = lpor-typeid instid = lpor-instid.
    CHECK sy-subrc = 0.

    "Remove from instance table
    DELETE TABLE instances WITH TABLE KEY typeid = lpor-typeid instid = lpor-instid.
  ENDMETHOD.


  METHOD bi_persistent~find_by_lpor.
    "Called to request an object instance

    DATA: instance TYPE t_instance.

    CHECK lpor-instid IS NOT INITIAL.

    "Buffered instantiation
    READ TABLE zcl_business_class=>instances
       WITH KEY typeid = lpor-typeid
                instid = lpor-instid
       INTO instance.

    "If not found, instantiate
    IF sy-subrc <> 0.
      instance-typeid = lpor-typeid.
      instance-instid = lpor-instid.
      TRY.
          CREATE OBJECT instance-instance TYPE (lpor-typeid)
            EXPORTING
              i_lpor = lpor.
        CATCH cx_static_check.  "Catch all to also include invalid object types
          RETURN.   "Caller must check existence
      ENDTRY.
      "Add new object to the instance table
      APPEND instance TO zcl_business_class=>instances.
    ENDIF.

    result = instance-instance.

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
