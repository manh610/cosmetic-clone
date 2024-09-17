package com.cosmetic.gg.dto.response.product;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Lob;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.attribute.ValueDetailResponse;
import com.cosmetic.gg.dto.response.discount.ProductDiscountDetailResponse;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ProductDetailResponse extends BaseModel {

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
	
	private String categoryName;
	
	private String brandId;
	
	private String brandName;
	
	private Float MinPrice;
	
	private Float MaxPrice;
	
	private Integer totalQuantity;
	
	private Integer totalFavorite;
	
	private Integer totalSell;
	
	private Double totalStar;
	
	private Integer totalReview;
	
	List<SkinTypeResponse> skinTypes = new ArrayList<>();
	
//	List<byte[]> images = new ArrayList<>();
	List<ProductImage> images = new ArrayList<>();
	
	List<ValueDetailResponse> valueDetails = new ArrayList<>();
	
	List<ProductDiscountDetailResponse> productDiscounts = new ArrayList<>();
}
