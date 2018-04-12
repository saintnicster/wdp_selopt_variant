interface ZIF_WDC__VARIANT_HELPER
  public .


  methods VARAINTS_ENABLED_FOR_USER
    importing
      !I_CURRENT_USER type UNAME
    returning
      value(RETVAL) type WDY_BOOLEAN .
  methods USER_CAN_CREATE_SELF_VARIANTS
    importing
      !I_CURRENT_USER type UNAME
    returning
      value(RETVAL) type WDY_BOOLEAN .
  methods USER_CAN_CREATE_GLBL_VARIANTS
    importing
      !I_CURRENT_USER type UNAME
    returning
      value(RETVAL) type WDY_BOOLEAN .
  methods POST_SAVE
    importing
      !I_VARIANT_KEY type ZWDC__VARIANT_KEY
      !I_WAS_DELETED type WDY_BOOLEAN .
  methods BEFORE_DISPLAY
    changing
      !C_VARIANT_LIST type ZCL_WDC__VARIANT=>TY_T_VARIANT_KEYS .
endinterface.
