CLASS zcl_pr_modify_item DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_mm_pur_s4_pr_modify_item .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PR_MODIFY_ITEM IMPLEMENTATION.


  METHOD if_mm_pur_s4_pr_modify_item~modify_item.

    DATA: ls_message LIKE LINE OF messages.

*    SELECT SINGLE description
*        FROM yy1_message_mm_check_002
*  WHERE code = '001' INTO @DATA(lv_desc).
*text-001
    IF purchaserequisition-purchasingdocumenttype EQ 'NB'(t01).
*
**      MOVE-CORRESPONDING purchaserequisitionitem TO PURREQNITEMS.
*
*      ls_message-messageid        = 'DUMMY'.             "Message ID
*      ls_message-messagetype      = 'I'.                 "Type of Message
*      ls_message-messagenumber    = '001'.               "Message Number
*      ls_message-messagevariable1 = 'Message from BADI'.  "Place holder
*      APPEND ls_message TO messages.
*
*      purchaserequisitionitemchange-purchaserequisitionitemtext = ' this was changed via ths badi'.
*      purchaserequisitionitemchange-overalllimitamount          = '8888'.
*
*      INSERT VALUE #( ) INTO TABLE purchaserequisitionitemaccchng.
*
**      !PURCHASEREQUISITIONITEMCHANGE type MMPUR_S_PR_ITM_CHANGE
**      !PURCHASEREQUISITIONITEMACCCHNG type MMPUR_T_PR_ITMACC_CHANGE optional
*
    ENDIF.

  ENDMETHOD.
ENDCLASS.
