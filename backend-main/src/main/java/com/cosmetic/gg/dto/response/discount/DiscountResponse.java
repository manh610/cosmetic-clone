package com.cosmetic.gg.dto.response.discount;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Lob;

import com.cosmetic.gg.common.enums.EDiscountType;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class DiscountResponse extends BaseModel {

	private String code;

	private String name;

	private LocalDateTime startDate;
	
	private LocalDateTime endDate;
	
	private Integer value;
	
	private String path;
	
	private EDiscountType discountType;
	
	private boolean isShow;
	
	@Lob
	private byte[] image;
	
	private String description;
	
	private Integer totalUse;
	
	List<User> users = new ArrayList<>();
	
	List<ProductItemResponse> productItems = new ArrayList<>();
	
	List<ProductDiscountResponse> products = new ArrayList<>();
}
