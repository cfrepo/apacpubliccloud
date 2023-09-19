@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_AONLINESHOP_002'
define root view entity ZC_AONLINESHOP_002
  provider contract transactional_query
  as projection on ZR_AONLINESHOP_002
{
  key OrderUUID,
      OrderID,
      OrderedItem,
      Price,
      TotalPrice,
      Currency,
      OrderQuantity,
      DeliveryDate,
      OverallStatus,
      Notes,
      LocalLastChangedAt,
      PurchaseRequisition,
      PrCreationDate

}
