package com.cosmetic.gg.dto.response.product;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.Lob;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ProductResponse extends BaseModel{
	
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
	
	List<SkinTypeResponse> skinTypes = new ArrayList<>();
	
	private Integer totalFavorite;
	
	private Integer totalSell;
	
	private Double totalStar;
	
	private Integer totalReview;
}
