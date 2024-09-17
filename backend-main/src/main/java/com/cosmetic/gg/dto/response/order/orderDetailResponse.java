package com.cosmetic.gg.dto.response.order;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class orderDetailResponse extends BaseModel{

	private String addressId;
	
	private EDeliveryType deliveryType;

	private String paymentId;

	private LocalDateTime orderDate;

	private LocalDateTime deliveryDate;

	private LocalDateTime receiptDate;

	private String censorId;
	
	private String shipperId;

	private boolean isPayment;

	private String note;

	private EStatus status;

	private Float totalPrice;

	private String deliveryUnitId;

	private String userId;

	private String discountId;
	
	
	
	private String provinceName;
	
	private String districtName;
	
	private String wardName;
	
	private String addressDetail;
	
	
	List<ProductItemResponse> productItem = new ArrayList<>();
}
