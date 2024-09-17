package com.cosmetic.gg.model.attribute;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ValueDetailModel extends BaseModel{

	private Float importPrice;

	private Float sellPrice;

	private Integer importQuantity;
	
	private Integer sellQuantity;

	private EStatus status;

	private String unit;

	private String description;

    private byte[] image;

	private String value;

	private String productId;
}
