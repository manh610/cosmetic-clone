package com.cosmetic.gg.model.order;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.order.orderProductItemResponse;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class OrderModel extends BaseModel {

	private String addressId;
	
	private EDeliveryType deliveryType;

	private String paymentId;

	private LocalDateTime orderDate;

	private LocalDateTime deliveryDate;

	private LocalDateTime receiptDate;

	private String censor;
	
	private String shipper;

	private boolean isPayment;

	private String note;

	private EStatus status;

	private Float totalPrice;

	private String deliveryUnitId;

	private String userId;

	private String discountId;
	
	private List<orderProductItemResponse> productItem = new ArrayList<>();
}
