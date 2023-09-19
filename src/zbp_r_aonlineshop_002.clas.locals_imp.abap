CLASS lsc_zr_onlineshoptp_002 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

*    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_zr_onlineshoptp_002 IMPLEMENTATION.

  METHOD save_modified.

    DATA : lt_online_shop_as TYPE STANDARD TABLE OF zaonlineshop_002,
           ls_online_shop_as TYPE                   zaonlineshop_002.

    IF create-onlineshop IS NOT INITIAL.
      lt_online_shop_as = CORRESPONDING #( create-onlineshop MAPPING FROM ENTITY ).
      INSERT zaonlineshop_002 FROM TABLE @lt_online_shop_as.
    ENDIF.
    IF update IS NOT INITIAL.
      CLEAR lt_online_shop_as.
      lt_online_shop_as = CORRESPONDING #( update-onlineshop MAPPING FROM ENTITY ).
      LOOP AT update-onlineshop  INTO DATA(onlineshop) WHERE orderuuid IS NOT INITIAL.
*           select * from zaonlineshop_002 where order_uuid = @onlineshop-OrderUUID into @data(ls_onlineshop) .
*                      lt_online_shop_as = CORRESPONDING #( create-onlineshop MAPPING FROM ENTITY ).

        MODIFY zaonlineshop_002 FROM TABLE @lt_online_shop_as.
*           ENDSELECT.
      ENDLOOP.
    ENDIF.

    LOOP AT delete-onlineshop INTO DATA(onlineshop_delete) WHERE orderuuid IS NOT INITIAL.
      DELETE FROM zaonlineshop_002 WHERE order_uuid = @onlineshop_delete-orderuuid.
      DELETE FROM zaonlineshop_00d WHERE orderuuid = @onlineshop_delete-orderuuid.
    ENDLOOP.

  ENDMETHOD.

  METHOD cleanup_finalize.

  ENDMETHOD.

*  METHOD adjust_numbers.

*    DATA lv_bldg_num TYPE n LENGTH 5.
*
*    LOOP AT mapped-building ASSIGNING FIELD-SYMBOL(<map_building>) WHERE %key-buildingid IS INITIAL .
*      TRY.
**          using number range to generate the building id
*          cl_numberrange_runtime=>number_get(
*            EXPORTING
*              nr_range_nr       = 'N1'
*              object            = 'ZNR_BLD_NO'
*              quantity          = 1
*            IMPORTING
*              number            = DATA(number)
*              returncode        = DATA(ret_code)
*              returned_quantity = DATA(ret_qty)
*          ).
*          lv_bldg_num = number.
*          <map_building>-%key-buildingid = |B{ lv_bldg_num }|.
*        CATCH cx_nr_object_not_found cx_number_ranges INTO DATA(lox_exp).
*          APPEND VALUE #(
*              %key = <map_building>-%key
*              %msg = lox_exp
*          ) TO reported-building.
*      ENDTRY.
*    ENDLOOP.

*  ENDMETHOD.

ENDCLASS.

CLASS lhc_onlineshop DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF is_draft,
        false TYPE abp_behv_flag VALUE '00', " active (not draft)
        true  TYPE abp_behv_flag VALUE '01', " draft
      END OF is_draft.
    CONSTANTS:
      BEGIN OF c_overall_status,
        new       TYPE string VALUE 'New / Composing',
*        composing TYPE string VALUE 'Composing...',
        submitted TYPE string VALUE 'Submitted / Approved',
        cancelled TYPE string VALUE 'Cancelled',
      END OF c_overall_status.

    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR onlineshop
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR onlineshop RESULT result.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR onlineshop~calculatetotalprice.

    METHODS setinitialordervalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR onlineshop~setinitialordervalues.

    METHODS checkdeliverydate FOR VALIDATE ON SAVE
      IMPORTING keys FOR onlineshop~checkdeliverydate.

    METHODS checkorderedquantity FOR VALIDATE ON SAVE
      IMPORTING keys FOR onlineshop~checkorderedquantity.

    METHODS upload_file FOR MODIFY
      IMPORTING keys FOR ACTION onlineshop~upload_file.

    METHODS createpurchaserequisitionitem FOR MODIFY
      IMPORTING keys FOR ACTION onlineshop~createpurchaserequisitionitem RESULT result.

ENDCLASS.

CLASS lhc_onlineshop IMPLEMENTATION.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD get_instance_features.

    " read relevant olineShop instance data
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        FIELDS ( overallstatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders)
      FAILED failed.

    "ToDo: dynamic feature control is currently not working for the action cancel order

    " evaluate condition, set operation state, and set result parameter
    " update and checkout shall not be allowed as soon as purchase requisition has been created
    result = VALUE #( FOR onlineorder IN onlineorders
                      ( %tky                   = onlineorder-%tky
                        %features-%update
                          = COND #( WHEN onlineorder-overallstatus = c_overall_status-submitted  THEN if_abap_behv=>fc-o-disabled
                                    WHEN onlineorder-overallstatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled   )
*                         %features-%delete
*                           = COND #( WHEN OnlineOrder-PurchaseRequisition IS NOT INITIAL THEN if_abap_behv=>fc-o-disabled
*                                     WHEN OnlineOrder-OverallStatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
*                                     ELSE if_abap_behv=>fc-o-enabled   )
                        %action-edit
                          = COND #( WHEN onlineorder-overallstatus = c_overall_status-submitted THEN if_abap_behv=>fc-o-disabled
                                    WHEN onlineorder-overallstatus = c_overall_status-cancelled THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled   )

                        ) ).
  ENDMETHOD.

  METHOD calculatetotalprice.
    DATA total_price TYPE zr_aonlineshop_002-totalprice.

    " read transfered instances
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        FIELDS ( orderid totalprice )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    LOOP AT onlineorders ASSIGNING FIELD-SYMBOL(<onlineorder>).
      " calculate total value
      <onlineorder>-totalprice = <onlineorder>-price * <onlineorder>-orderquantity.
    ENDLOOP.

    "update instances
    MODIFY ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        UPDATE FIELDS ( totalprice )
        WITH VALUE #( FOR onlineorder IN onlineorders (
                          %tky       = onlineorder-%tky
                          totalprice = <onlineorder>-totalprice
                        ) ).
  ENDMETHOD.

  METHOD setinitialordervalues.

    DATA delivery_date TYPE i_purchasereqnitemtp-deliverydate.
    DATA(creation_date) = cl_abap_context_info=>get_system_date(  ).
    "set delivery date proposal
    delivery_date = cl_abap_context_info=>get_system_date(  ) + 14.
    "read transfered instances
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        FIELDS ( orderid overallstatus  deliverydate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    "delete entries with assigned order ID
    DELETE onlineorders WHERE orderid IS NOT INITIAL.
    CHECK onlineorders IS NOT INITIAL.

    " **Dummy logic to determine order IDs**
    " get max order ID from the relevant active and draft table entries
    SELECT MAX( orderid ) FROM zr_aonlineshop_002 INTO @DATA(max_order_id). "active table
    SELECT SINGLE FROM zr_aonlineshop_002 FIELDS MAX( orderid ) INTO @DATA(max_orderid_draft). "draft table
    IF max_orderid_draft > max_order_id.
      max_order_id = max_orderid_draft.
    ENDIF.

    "set initial values of new instances
    MODIFY ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        UPDATE FIELDS ( orderid overallstatus  deliverydate price  )
        WITH VALUE #( FOR order IN onlineorders INDEX INTO i (
                          %tky          = order-%tky
                          orderid       = max_order_id + i
                          overallstatus = c_overall_status-new  "'New / Composing'
                          deliverydate  = delivery_date
                          createdat     = creation_date
                        ) ).
    .
  ENDMETHOD.

  METHOD checkdeliverydate.

*   " read transfered instances
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        FIELDS ( deliverydate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    DATA(creation_date) = cl_abap_context_info=>get_system_date(  ).
    "raise msg if 0 > qty <= 10
    LOOP AT onlineorders INTO DATA(online_order).


      IF online_order-deliverydate IS INITIAL OR online_order-deliverydate = ' '.
        APPEND VALUE #( %tky = online_order-%tky ) TO failed-onlineshop.
        APPEND VALUE #( %tky         = online_order-%tky
                        %state_area   = 'VALIDATE_DELIVERYDATE'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Delivery Date cannot be initial' )
                      ) TO reported-onlineshop.

      ELSEIF  ( ( online_order-deliverydate ) - creation_date ) < 14.
        APPEND VALUE #(  %tky = online_order-%tky ) TO failed-onlineshop.
        APPEND VALUE #(  %tky          = online_order-%tky
                        %state_area   = 'VALIDATE_DELIVERYDATE'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Delivery Date should be atleast 14 days after the creation date'  )

                        %element-orderquantity  = if_abap_behv=>mk-on
                      ) TO reported-onlineshop.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkorderedquantity.

    "read relevant order instance data
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
    ENTITY onlineshop
    FIELDS ( orderid ordereditem orderquantity )
    WITH CORRESPONDING #( keys )
    RESULT DATA(onlineorders).

    "raise msg if 0 > qty <= 10
    LOOP AT onlineorders INTO DATA(onlineorder).
      APPEND VALUE #(  %tky           = onlineorder-%tky
                      %state_area    = 'VALIDATE_QUANTITY'
                    ) TO reported-onlineshop.

      IF onlineorder-orderquantity IS INITIAL OR onlineorder-orderquantity = ' '.
        APPEND VALUE #( %tky = onlineorder-%tky ) TO failed-onlineshop.
        APPEND VALUE #( %tky          = onlineorder-%tky
                        %state_area   = 'VALIDATE_QUANTITY'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Quantity cannot be empty' )
                        %element-orderquantity = if_abap_behv=>mk-on
                      ) TO reported-onlineshop.

      ELSEIF onlineorder-orderquantity > 10.
        APPEND VALUE #(  %tky = onlineorder-%tky ) TO failed-onlineshop.
        APPEND VALUE #(  %tky          = onlineorder-%tky
                        %state_area   = 'VALIDATE_QUANTITY'
                        %msg          = new_message_with_text(
                                severity = if_abap_behv_message=>severity-error
                                text     = 'Quantity should be below 10' )

                        %element-orderquantity  = if_abap_behv=>mk-on
                      ) TO reported-onlineshop.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD upload_file.

  ENDMETHOD.

  METHOD createpurchaserequisitionitem.

    DATA: purchase_requisitions      TYPE TABLE FOR CREATE i_purchaserequisitiontp,
          purchase_requisition       TYPE STRUCTURE FOR CREATE i_purchaserequisitiontp,
          purchase_requisition_items TYPE TABLE FOR CREATE i_purchaserequisitiontp\_purchaserequisitionitem,
          purchase_requisition_item  TYPE STRUCTURE FOR CREATE i_purchaserequisitiontp\\purchaserequisition\_purchaserequisitionitem,
          purchase_reqn_acct_assgmts TYPE TABLE FOR CREATE i_purchasereqnitemtp\_purchasereqnacctassgmt,
          purchase_reqn_acct_assgmt  TYPE STRUCTURE FOR CREATE i_purchasereqnitemtp\_purchasereqnacctassgmt,
          purchase_reqn_item_texts   TYPE TABLE FOR CREATE i_purchasereqnitemtp\_purchasereqnitemtext,
          purchase_reqn_item_text    TYPE STRUCTURE FOR CREATE i_purchasereqnitemtp\_purchasereqnitemtext,
          update_lines               TYPE TABLE FOR UPDATE zr_aonlineshop_002\\onlineshop,
          update_line                TYPE STRUCTURE FOR UPDATE zr_aonlineshop_002\\onlineshop,
          delivery_date              TYPE i_purchasereqnitemtp-deliverydate,
          requested_quantity         TYPE i_purchasereqnitemtp-requestedquantity.

*    delivery_date = cl_abap_context_info=>get_system_date(  ) + 14.

    "read transfered order instances
    READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(onlineorders).

    DATA n TYPE i.

    LOOP AT onlineorders INTO DATA(onlineorder).
      n += 1.
      "purchase requisition
      purchase_requisition = VALUE #(  %cid                   = |My%CID_{ n }|
                                        purchaserequisitiontype  = 'NB'  ) .
      APPEND purchase_requisition TO purchase_requisitions.

      "purchase requisition item
      purchase_requisition_item = VALUE #(
                                        %cid_ref = |My%CID_{ n }|
                                        %target  = VALUE #(  (
                                                      %cid                         = |My%ItemCID_{ n }|
                                                      plant                        = '1010'  "Plant 01 (DE)
                                                      accountassignmentcategory    = 'U'  "unknown
*                                                       PurchaseRequisitionItemText  =  . "retrieved automatically from maintained MaterialInfo
                                                      requestedquantity            = onlineorder-orderquantity
                                                      purchaserequisitionprice     = onlineorder-price
                                                      purreqnitemcurrency          = onlineorder-currency
                                                      material                     = 'D001'
                                                      materialgroup                = 'A001'
                                                      purchasinggroup              = '001'
                                                      purchasingorganization       = '1010'
                                                      deliverydate                 = onlineorder-deliverydate   "delivery_date  "yyyy-mm-dd (at least 10 days)
                                                      createdbyuser                = onlineorder-createdby
                                                      ) ) ).
      APPEND purchase_requisition_item TO purchase_requisition_items.

      "purchase requisition account assignment
      purchase_reqn_acct_assgmt = VALUE #(
                                          %cid_ref = |My%ItemCID_{ n }|
                                          %target  = VALUE #( (
                                                        %cid       = |My%AccntCID_{ n }|
                                                        costcenter = 'JMW-COST'
                                                        glaccount  = '0000400000' ) ) ) .
      APPEND purchase_reqn_acct_assgmt TO purchase_reqn_acct_assgmts .

      "purchase requisition item text
      purchase_reqn_item_text    =  VALUE #(
                                          %cid_ref = |My%ItemCID_{ n }|
                                          %target  = VALUE #( (
                                                        %cid           = |My%TextCID_{ n }|
                                                        textobjecttype = 'B01'
                                                        language       = 'E'
                                                        plainlongtext  = onlineorder-notes
                                                    )  )  ) .
      APPEND purchase_reqn_item_text TO purchase_reqn_item_texts.
    ENDLOOP.

    "EML deep create statement
    IF keys IS NOT INITIAL.
      "purchase reqn
      MODIFY ENTITIES OF i_purchaserequisitiontp
        ENTITY purchaserequisition
          CREATE FIELDS ( purchaserequisitiontype )
          WITH purchase_requisitions
        "purchase reqn item
        CREATE BY \_purchaserequisitionitem
          FIELDS ( plant
*                   purchaserequisitionitemtext
                  accountassignmentcategory
                  requestedquantity
                  baseunit
                  purchaserequisitionprice
                  purreqnitemcurrency
                  material
                  materialgroup
                  purchasinggroup
                  purchasingorganization
                  deliverydate
                )
        WITH purchase_requisition_items
      "purchase reqn account assignment
      ENTITY purchaserequisitionitem
        CREATE BY \_purchasereqnacctassgmt
            FIELDS ( costcenter
                    glaccount
                    quantity
                    baseunit )
            WITH purchase_reqn_acct_assgmts
        "purchase reqn item text
        CREATE BY \_purchasereqnitemtext
            FIELDS ( plainlongtext )
            WITH purchase_reqn_item_texts
      REPORTED DATA(reported_create_pr)
      MAPPED DATA(mapped_create_pr)
      FAILED DATA(failed_create_pr).
    ENDIF.
    "retrieve the generated
    zbp_r_aonlineshop_002=>mapped_purchase_requisition-purchaserequisition = mapped_create_pr-purchaserequisition.

    "set a flag to check in the save sequence that purchase requisition has been created
    "the correct value for PurchaseRequisition has to be calculated in the save sequence using convert key
    LOOP AT keys INTO DATA(key).
      update_line-%tky           = key-%tky.
      update_line-overallstatus  = c_overall_status-submitted. "'Submitted / Approved'.
      APPEND update_line TO update_lines.
    ENDLOOP.

    MODIFY ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
      ENTITY onlineshop
        UPDATE
        FIELDS (  overallstatus  )
        WITH update_lines
      REPORTED reported
      FAILED failed
      MAPPED mapped.

    IF failed IS INITIAL.
      "Read the changed data for action result
      READ ENTITIES OF zr_aonlineshop_002 IN LOCAL MODE
        ENTITY onlineshop
          ALL FIELDS WITH
          CORRESPONDING #( keys )
        RESULT DATA(result_read).
      "return result entities
      result = VALUE #( FOR order_2 IN result_read ( %tky   = order_2-%tky
                                                    %param = order_2 ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
