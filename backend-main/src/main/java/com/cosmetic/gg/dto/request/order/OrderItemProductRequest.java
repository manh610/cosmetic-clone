package com.cosmetic.gg.dto.request.order;

import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class OrderItemProductRequest extends BaseModel{

	private String productItemId;

	private Integer quantity;
}
