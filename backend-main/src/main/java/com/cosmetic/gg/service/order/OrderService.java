package com.cosmetic.gg.service.order;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.order.orderDetailResponse;
import com.cosmetic.gg.entity.order.OrderItem;
import com.cosmetic.gg.model.order.OrderModel;

public interface OrderService {

	Map<String, Object> search(
			String userId, String discountId, String deliveryUnitId, String censor, String shipper,
			EDeliveryType deliveryType, List<EStatus> status, Boolean isPayment, 
			LocalDateTime orderDateFrom, LocalDateTime orderDateTo, 
			String provinceId, String districtId, String wardId,
			Integer pageIndex, Integer pageSize);
	
	List<Error> validator(OrderModel orderModel);
	
	OrderModel create(OrderModel orderModel);
	
	OrderModel update(OrderModel orderModel);
	
	orderDetailResponse detail(String id);
	
	
	List<Error> validatorItem(OrderItem orderItem);
	
	OrderItem addProduct(OrderItem orderItem);
	
	OrderItem updateProduct(OrderItem orderItem);
	
	List<orderDetailResponse> getOrderByUser(String id);
}
