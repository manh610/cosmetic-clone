package com.cosmetic.gg.dto.request.order;

import java.util.List;

import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class OrderItemRequest extends BaseModel{
	
	private String orderId;

	private List<OrderItemProductRequest> productItem;
}
