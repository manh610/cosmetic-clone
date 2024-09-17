package com.cosmetic.gg.dto.response.discount;

import java.time.LocalDateTime;

import javax.persistence.Lob;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ProductDiscountResponse extends BaseModel {

	private String code;

	private String name;

	@Lob
	private byte[] photo;

	private String madeIn;
	
	private EStatus status;
	
	private String description;

	private LocalDateTime productionDate;

	private LocalDateTime expirationDate;

	private String categoryId;

	private String brandId;
	
	private Integer quantityDiscount;
}
