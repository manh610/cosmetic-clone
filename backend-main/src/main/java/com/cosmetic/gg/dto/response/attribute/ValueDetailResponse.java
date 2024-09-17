package com.cosmetic.gg.dto.response.attribute;

import java.util.ArrayList;
import java.util.List;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.discount.ProductDiscountDetailResponse;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ValueDetailResponse extends BaseModel {
	
	private String productItemId;
	
	private String value;

	private Float importPrice;
	
	private Float sellPrice;

	private Integer importQuantity;

	private Integer sellQuantity;

	private EStatus status;

	private String unit;

	private String description;

    private byte[] image;
    
    List<ProductDiscountDetailResponse> productItemDiscounts = new ArrayList<>();
}
