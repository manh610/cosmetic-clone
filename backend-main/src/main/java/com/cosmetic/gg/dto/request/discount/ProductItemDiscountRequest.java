package com.cosmetic.gg.dto.request.discount;

import java.util.List;

import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ProductItemDiscountRequest extends BaseModel{

	private String discountId;

	private Integer quantity;
	
	private List<String> productItemIds;
}
