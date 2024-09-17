package com.cosmetic.gg.dto.response.supply;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ImportDetailResponse extends BaseModel {

	private String code;
	
	private LocalDateTime importDate;

	private int vat;

	private String representativeName;

	private String representativePhone;

	private String representativeEmail;

	private String supplierId;

	private String supplierAddressId;
	
	List<ProductDetailResponse> productId = new ArrayList<>();
	
	private EStatus status;
	
	private String description;
	
	private LocalDateTime createdAt;
	
	private String createdBy;
	
	private LocalDateTime updatedAt;
	
	private String updatedBy;
}
